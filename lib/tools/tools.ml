
let max_read_bytes = Workspace_path.max_read_bytes
let max_write_bytes = Workspace_path.max_write_bytes
let max_command_bytes = 65_536
let max_walk_entries = 10_000
let max_search_bytes = 16_777_216
let max_matches = 100
let max_regex_line = 4096
let regex_scan_seconds = 3.0

exception Tool_error of string

type mobile_discovery = {
  stack : string;
  root : string;
  subroot : string;
  manifest_hash : string;
  choices : string list;
}

type session_context = {
  owner : string;
  process_manager : Workspace_process.manager;
  read_artifact : string -> string option;
  lsp_manager : Workspace_lsp.manager;
  dap_manager : Workspace_dap.manager;
  dap_granted_effect : Workspace_dap.authorization_effect option ref;
  eval_lock : Mutex.t;
  mutable python_kernel : Workspace_eval.t option;
  mutable javascript_kernel : Workspace_eval.t option;
  ssh_lock : Mutex.t;
  ssh_sessions : (string, Workspace_ssh.session) Hashtbl.t;
  browser_manager : Workspace_browser.manager;
  hub_port : (unit -> int) option;
  xcode_lock : Mutex.t;
  mutable xcode_discovery : Workspace_xcode.discovery option;
  mobile_lock : Mutex.t;
  mutable mobile_discovery : mobile_discovery option;
  record_file_change : path:string -> before:string -> after:string -> unit;
  mutable closed : bool;
}

type file_location = {
  root : string;
  path : string;
  worktree_id : string option;
}

let starts_with text prefix =
  String.length text >= String.length prefix &&
  String.sub text 0 (String.length prefix) = prefix

let resolve_file_location ?cancel ?context ~root path =
  let lower = String.lowercase_ascii path in
  if starts_with lower "local://" then
    let path = String.sub path 8 (String.length path - 8) in
    if path = "" then raise (Tool_error "local URI requires a workspace-relative path");
    Some { root; path; worktree_id = None }
  else if starts_with lower "worktree://" then (
    let uri = String.sub path 11 (String.length path - 11) in
    let id, relative = match String.index_opt uri '/' with
      | None -> uri, "."
      | Some slash ->
          String.sub uri 0 slash,
          String.sub uri (slash + 1) (String.length uri - slash - 1) in
    let context = match context with
      | Some context -> context
      | None -> raise (Tool_error "worktree paths require a private session owner") in
    if id = "" then raise (Tool_error "worktree URI requires a managed worktree ID");
    let worktree = Workspace_git.find_worktree ?cancel ~base:root
      ~owner:context.owner ~id () in
    Some { root = worktree.path; path = relative; worktree_id = Some id })
  else if Workspace_reader.is_scheme_uri path then None
  else Some { root; path; worktree_id = None }

let require_session_context = function
  | Some context when not context.closed -> context
  | Some _ -> raise (Tool_error "session tools are closed")
  | None -> raise (Tool_error "this tool requires a private saved session")

let create_session_context ?lsp_manager ?hub_port ~owner ~root ~process_manager ~read_artifact
    ~record_file_change () =
  let lsp_manager = match lsp_manager with
    | Some manager -> manager
    | None -> Workspace_lsp.create_manager () in
  let dap_granted_effect = ref None in
  let dap_manager = Workspace_dap.create_manager ~owner ~workspace_root:root
    ~authorize:(fun authorization ->
      if !dap_granted_effect <> Some authorization then
        raise (Workspace_dap.Not_approved
          "effect-specific interactive DAP approval is required")) in
  { owner; process_manager; read_artifact;
    lsp_manager;
    dap_manager; dap_granted_effect;
    eval_lock = Mutex.create (); python_kernel = None; javascript_kernel = None;
    ssh_lock = Mutex.create (); ssh_sessions = Hashtbl.create 8;
    browser_manager = Workspace_browser.create_manager ~owner;
    hub_port;
    xcode_lock = Mutex.create (); xcode_discovery = None;
    mobile_lock = Mutex.create (); mobile_discovery = None;
    record_file_change; closed = false }

let close_session_context context =
  if not context.closed then (
    context.closed <- true;
    context.dap_granted_effect := None;
    Mutex.lock context.xcode_lock;
    context.xcode_discovery <- None;
    Mutex.unlock context.xcode_lock;
    Mutex.lock context.mobile_lock;
    context.mobile_discovery <- None;
    Mutex.unlock context.mobile_lock;
    let ignore_failure action = try action () with _ -> () in
    ignore_failure (fun () -> Workspace_lsp.close_manager context.lsp_manager);
    ignore_failure (fun () -> Workspace_dap.close_manager context.dap_manager);
    ignore_failure (fun () -> Workspace_browser.close_manager context.browser_manager);
    let python_kernel, javascript_kernel =
      Mutex.lock context.eval_lock;
      let kernels = context.python_kernel, context.javascript_kernel in
      context.python_kernel <- None;
      context.javascript_kernel <- None;
      Mutex.unlock context.eval_lock;
      kernels in
    Option.iter (fun kernel -> ignore_failure (fun () -> Workspace_eval.close kernel))
      python_kernel;
    Option.iter (fun kernel -> ignore_failure (fun () -> Workspace_eval.close kernel))
      javascript_kernel;
    let ssh_sessions =
      Mutex.lock context.ssh_lock;
      let sessions = Hashtbl.fold (fun _ session rows -> session :: rows)
        context.ssh_sessions [] in
      Hashtbl.clear context.ssh_sessions;
      Mutex.unlock context.ssh_lock;
      sessions in
    List.iter (fun session ->
      ignore_failure (fun () ->
        Workspace_ssh.close_session ~owner:context.owner session))
      ssh_sessions)

let check_session_context context =
  if context.closed then raise (Tool_error "session tools are closed")

let bounded_text text limit =
  if String.length text <= limit then text
  else
    let rec boundary index =
      if index > 0 && index < String.length text &&
         (Char.code text.[index] land 0xc0) = 0x80 then boundary (index - 1)
      else index in
    let length = boundary limit in
    String.sub text 0 length ^
      Printf.sprintf "\n[%d bytes omitted]" (String.length text - length)
exception Cancelled

let fail message = raise (Tool_error message)

let field name = function
  | `Assoc fields -> (match List.assoc_opt name fields with Some value -> value | None -> `Null)
  | _ -> fail "arguments must be a JSON object"
let replace_string_field name value = function
  | `Assoc fields ->
      `Assoc (List.map (fun (key, current) ->
        if key = name then key, `String value else key, current) fields)
  | _ -> fail "arguments must be a JSON object"

let resolve_path_arguments ?cancel ?context ~root args =
  match field "path" args with
  | `String path ->
      (match resolve_file_location ?cancel ?context ~root path with
       | Some location when starts_with (String.lowercase_ascii path) "local://" ||
                            starts_with (String.lowercase_ascii path) "worktree://" ->
           location.root, replace_string_field "path" location.path args
       | _ -> root, args)
  | _ -> root, args

let required_string name args =
  match field name args with
  | `String value -> value
  | `Null -> fail ("missing required string argument: " ^ name)
  | _ -> fail (name ^ " must be a string")

let optional_string name default args =
  match field name args with
  | `Null -> default
  | `String value -> value
  | _ -> fail (name ^ " must be a string")

let optional_int name default ~minimum ~maximum args =
  match field name args with
  | `Null -> default
  | `Int n when n >= minimum && n <= maximum -> n
  | _ -> fail (Printf.sprintf "%s must be an integer between %d and %d" name minimum maximum)

let optional_bool name default args =
  match field name args with
  | `Null -> default
  | `Bool value -> value
  | _ -> fail (name ^ " must be a boolean")

(* Boyer-Moore-Horspool: skip ahead by the shift of the window's last byte. *)
let find_from text needle start =
  let text_len = String.length text and needle_len = String.length needle in
  if needle_len >= 3 then (
    let shift = Array.make 256 needle_len in
    for i = 0 to needle_len - 2 do
      shift.(Char.code needle.[i]) <- needle_len - 1 - i
    done;
    let last = needle.[needle_len - 1] in
    let rec scan i =
      if i + needle_len > text_len then None
      else
        let tail = text.[i + needle_len - 1] in
        let rec same k = k < 0 || (text.[i + k] = needle.[k] && same (k - 1)) in
        if tail = last && same (needle_len - 2) then Some i
        else scan (i + shift.(Char.code tail)) in
    scan start)
  else
  let rec matches i j =
    j = needle_len || (text.[i + j] = needle.[j] && matches i (j + 1))
  in
  let rec scan i =
    if i + needle_len > text_len then None
    else if text.[i] = needle.[0] && matches i 0 then Some i
    else scan (i + 1)
  in
  scan start
(* A segment cannot cross a separator; memoization also bounds repeated stars. *)
let glob_metacharacter = function '*' | '?' | '[' | '\\' -> true | _ -> false

let rec glob_segment pattern name =
  let plen = String.length pattern in
  let literal from = not (String.exists glob_metacharacter
    (String.sub pattern from (plen - from))) in
  (* Most ignore rules and globs are literal names or `*.ext`; match those
     without allocating a memo table per candidate. *)
  if not (String.exists glob_metacharacter pattern) then String.equal pattern name
  else if plen > 0 && pattern.[0] = '*' && literal 1 then
    String.length name >= plen - 1 &&
    String.ends_with ~suffix:(String.sub pattern 1 (plen - 1)) name
  else glob_segment_memo pattern name

and glob_segment_memo pattern name =
  let plen = String.length pattern and nlen = String.length name in
  let memo = Hashtbl.create 32 in
  let rec matches pi ni =
    match Hashtbl.find_opt memo (pi, ni) with
    | Some result -> result
    | None ->
        let result =
          if pi = plen then ni = nlen
          else match pattern.[pi] with
            | '*' -> matches (pi + 1) ni || (ni < nlen && matches pi (ni + 1))
            | '?' -> ni < nlen && matches (pi + 1) (ni + 1)
            | '\\' when pi + 1 < plen ->
                ni < nlen && pattern.[pi + 1] = name.[ni] && matches (pi + 2) (ni + 1)
            | '[' ->
                let close = try String.index_from pattern (pi + 1) ']' with Not_found -> plen in
                if close = plen then ni < nlen && name.[ni] = '[' && matches (pi + 1) (ni + 1)
                else if ni >= nlen then false
                else
                  let first = pi + 1 in
                  let invert = first < close && (pattern.[first] = '!' || pattern.[first] = '^') in
                  let start = if invert then first + 1 else first in
                  let rec member k =
                    if k >= close then false
                    else if k + 2 < close && pattern.[k + 1] = '-' then
                      (name.[ni] >= pattern.[k] && name.[ni] <= pattern.[k + 2]) ||
                      member (k + 3)
                    else name.[ni] = pattern.[k] || member (k + 1) in
                  start < close && (member start <> invert) && matches (close + 1) (ni + 1)
            | char -> ni < nlen && char = name.[ni] && matches (pi + 1) (ni + 1) in
        Hashtbl.add memo (pi, ni) result;
        result
  in
  matches 0 0

(* Compile a segment glob once per rule rather than per candidate path. *)
let segment_matcher pattern =
  let plen = String.length pattern in
  if not (String.exists glob_metacharacter pattern) then String.equal pattern
  else if plen > 0 && pattern.[0] = '*' &&
          not (String.exists glob_metacharacter (String.sub pattern 1 (plen - 1))) then
    let suffix = String.sub pattern 1 (plen - 1) in
    String.ends_with ~suffix
  else glob_segment_memo pattern

(* Split the pattern once; `**`-free patterns compare segment by segment and
   only `**` needs the memoized search. *)
let glob_parts pattern =
  let patterns = Array.of_list (String.split_on_char '/' pattern) in
  let segments = Array.map (fun segment ->
    if segment = "**" then None else Some (segment_matcher segment)) patterns in
  let recursive = Array.exists Option.is_none segments in
  fun path ->
    let names = Array.of_list (String.split_on_char '/' path) in
    if not recursive then
      Array.length names = Array.length segments &&
      (let rec all index = index = Array.length names ||
         ((Option.get segments.(index)) names.(index) && all (index + 1)) in all 0)
    else
      let memo = Hashtbl.create 32 in
      let rec matches i j =
        match Hashtbl.find_opt memo (i, j) with
        | Some answer -> answer
        | None ->
            let answer =
              if i = Array.length segments then j = Array.length names
              else match segments.(i) with
                | None -> matches (i + 1) j ||
                    (j < Array.length names && matches i (j + 1))
                | Some segment -> j < Array.length names &&
                    segment names.(j) && matches (i + 1) (j + 1) in
            Hashtbl.add memo (i, j) answer;
            answer
      in
      matches 0 0

let valid_glob pattern =
  if pattern = "" || String.length pattern > 512 ||
     String.contains pattern '\000' ||
     List.exists (fun part -> part = ".." || part = ".")
       (String.split_on_char '/' pattern) ||
     not (Filename.is_relative pattern) then
    fail "glob must be a nonempty relative pattern (up to 512 bytes) without '.' or '..'"

type ignore_rule = {
  base : string;
  base_prefix : string;
  directory_only : bool;
  negated : bool;
  basename_only : bool;
  matches : string -> bool;
}

let ignore_rules absolute base =
  let path = Filename.concat absolute ".gitignore" in
  try
    if (Unix.lstat path).Unix.st_kind <> Unix.S_REG then []
    else
      let text = Workspace_path.read_bounded path max_read_bytes in
      String.split_on_char '\n' text |> List.filter_map (fun line ->
        let line =
          if String.ends_with ~suffix:"\r" line then
            String.sub line 0 (String.length line - 1) else line in
        (* Only unescaped trailing spaces are insignificant in gitignore. *)
        let rec end_of_pattern n =
          if n > 0 && line.[n - 1] = ' ' &&
             (n < 2 || line.[n - 2] <> '\\') then end_of_pattern (n - 1)
          else n in
        let line = String.sub line 0 (end_of_pattern (String.length line)) in
        if line = "" || line.[0] = '#' then None
        else
          let negated = line.[0] = '!' in
          let pattern = if negated then String.sub line 1 (String.length line - 1) else line in
          if pattern = "" then None
          else
            let directory_only = pattern.[String.length pattern - 1] = '/' in
            let pattern = if directory_only then String.sub pattern 0 (String.length pattern - 1) else pattern in
            let anchored = String.length pattern > 0 && pattern.[0] = '/' in
            let pattern = if anchored then
              String.sub pattern 1 (String.length pattern - 1) else pattern in
            if String.length pattern > 512 then fail ("gitignore rule exceeds 512 bytes: " ^ path);
            if pattern = "" then None
            else
              let basename_only = not anchored && not (String.contains pattern '/') in
              Some { base; base_prefix = (if base = "" then "" else base ^ "/");
                     directory_only; negated; basename_only;
                     matches = if basename_only then segment_matcher pattern
                       else glob_parts pattern })
  with Unix.Unix_error (Unix.ENOENT, _, _) -> []

let ignored rules relative is_directory =
  let basename = lazy (Filename.basename relative) in
  List.fold_left (fun excluded rule ->
    (* Only a rule that could flip the current decision needs matching. *)
    if rule.negated <> excluded || (rule.directory_only && not is_directory) then excluded
    else if rule.base_prefix <> "" &&
            not (String.length relative > String.length rule.base_prefix &&
                 String.starts_with ~prefix:rule.base_prefix relative) then excluded
    else
      let matches =
        if rule.basename_only then rule.matches (Lazy.force basename)
        else if rule.base_prefix = "" then rule.matches relative
        else rule.matches (String.sub relative (String.length rule.base_prefix)
          (String.length relative - String.length rule.base_prefix)) in
      if matches then not rule.negated else excluded) false rules

let matching_glob pattern =
  if String.contains pattern '/' then glob_parts pattern
  else let segment = segment_matcher pattern in
    fun relative -> segment (Filename.basename relative)

let fuzzy_separator character =
  match Uchar.to_int character with
  | 0x2f | 0x5c | 0x2d | 0x5f | 0x2e | 0x20 -> true
  | _ -> false

let fuzzy_query text =
  if String.length text > 256 then fail "query exceeds 256-byte limit";
  let malformed = ref false in
  let reversed = Uutf.String.fold_utf_8 (fun acc _ -> function
    | `Malformed _ -> malformed := true; acc
    | `Uchar character ->
        (match Uucp.Case.Fold.fold character with
         | `Self -> character :: acc
         | `Uchars characters -> List.rev_append characters acc))
      [] text in
  if !malformed then fail "query must be valid UTF-8";
  let query = Array.of_list (List.rev reversed) in
  if Array.length query = 0 then fail "query must not be empty";
  query

let fuzzy_score query path =
  let query_index = ref 0 and path_index = ref 0 in
  let previous_match = ref (-1) and first_match = ref 0 in
  let previous_separator = ref true and score = ref 0 in
  let feed character =
    let position = !path_index in
    if !query_index < Array.length query &&
       Uchar.equal character query.(!query_index) then (
      let gap = if !previous_match < 0 then 0 else position - !previous_match in
      if !query_index = 0 then first_match := position;
      score := !score + 10 +
        (if !previous_separator then 12 else 0) +
        (if !query_index > 0 && gap = 1 then 8 else 0) -
        (if !query_index > 0 then min 20 (max 0 (gap - 1)) else 0);
      previous_match := position;
      incr query_index);
    previous_separator := fuzzy_separator character;
    incr path_index in
  let malformed = ref false in
  ignore (Uutf.String.fold_utf_8 (fun () _ -> function
    | `Malformed _ -> malformed := true
    | `Uchar character ->
        (match Uucp.Case.Fold.fold character with
         | `Self -> feed character
         | `Uchars characters -> List.iter feed characters))
    () path);
  if not !malformed && !query_index = Array.length query then
    Some (!score - !first_match)
  else None

let skip_directory = function
  | ".git" | ".hg" | ".svn" | "_build" | "build" | ".build" | "dist"
  | "node_modules" | "DerivedData" | ".gradle" | ".dart_tool" | "Pods" -> true
  | _ -> false

(* [stop] ends a walk whose caller already has all the output it can return;
   [descend] prunes directories that cannot contain a wanted path. *)
let walk ?(hidden = true) ?cancel ?(stop = fun () -> false) ?(descend = fun _ -> true)
    ?visit_directory root relative visit =
  let check_cancel () =
    match cancel with Some cancelled when cancelled () -> raise Cancelled | _ -> () in
  check_cancel ();
  let starting = Workspace_path.checked_path root relative in
  let relative =
    match List.filter (fun part -> part <> "" && part <> ".")
            (String.split_on_char '/' relative) with
    | [] -> "."
    | parts -> String.concat "/" parts in
  if (Unix.lstat starting).Unix.st_kind <> Unix.S_DIR then fail ("not a directory: " ^ relative);
  let entries = ref 0 and truncated = ref false in
  let rec ancestors absolute prefix rules = function
    | [] -> rules
    | name :: rest ->
        let child = if prefix = "" then name else prefix ^ "/" ^ name in
        if skip_directory name || (not hidden && name.[0] = '.') ||
           ignored rules child true then
          fail ("directory is excluded from listing/search: " ^ child);
        let absolute = Filename.concat absolute name in
        if (Unix.lstat absolute).Unix.st_kind <> Unix.S_DIR then
          fail ("not a directory: " ^ child);
        ancestors absolute child (rules @ ignore_rules absolute child) rest in
  let root_rules = ignore_rules root "" in
  let rules = if relative = "." then root_rules else
    ancestors root "" root_rules (String.split_on_char '/' relative) in
  let rec directory absolute prefix rules =
    check_cancel ();
    let dir = Unix.opendir absolute in
    let names = Fun.protect ~finally:(fun () -> Unix.closedir dir) (fun () ->
      let rec collect acc =
        check_cancel ();
        if !entries >= max_walk_entries then (truncated := true; acc)
        else match Unix.readdir dir with
          | exception End_of_file -> acc
          | "." | ".." -> collect acc
          | name -> incr entries; collect (name :: acc)
      in List.sort String.compare (collect [])) in
    List.iter (fun name ->
      check_cancel ();
      if stop () then truncated := true else
      let child = Filename.concat absolute name in
      let relative = if prefix = "" then name else prefix ^ "/" ^ name in
      try
        match (Unix.lstat child).Unix.st_kind with
        | Unix.S_DIR when not (skip_directory name) &&
                          (hidden || name.[0] <> '.') && descend relative &&
                          not (ignored rules relative true) ->
            Option.iter (fun visit -> visit relative child) visit_directory;
            directory child relative (rules @ ignore_rules child relative)
        | Unix.S_REG when (hidden || name.[0] <> '.') &&
                          not (ignored rules relative false) ->
            visit relative child
        | _ -> ()
      with Unix.Unix_error (Unix.ENOENT, _, _) -> ()) names
  in
  directory starting (if relative = "." then "" else relative) rules;
  !truncated

type fuzzy_match = { path : string; is_directory : bool; score : int }

let compare_fuzzy_match left right =
  let by_score = compare right.score left.score in
  if by_score <> 0 then by_score else String.compare left.path right.path

(* Concurrent tree walks hand the OCaml runtime lock back and forth on every
   readdir/stat/read, which made a parallel batch slower than running it
   serially. Walks take turns; waiting releases the runtime lock, so cheap
   shared calls such as read_file still overlap. Reentrant per thread. *)
let scan_lock = Mutex.create ()
let scan_owner = Atomic.make (-1)

let scanning work =
  let self = Thread.id (Thread.self ()) in
  if Atomic.get scan_owner = self then work ()
  else (
    Mutex.lock scan_lock;
    Atomic.set scan_owner self;
    Fun.protect work ~finally:(fun () ->
      Atomic.set scan_owner (-1);
      Mutex.unlock scan_lock))

let fuzzy_file_search ?cancel root args =
  let query = fuzzy_query (required_string "query" args) in
  let relative = optional_string "path" "." args in
  let hidden = optional_bool "hidden" false args in
  let max_results = optional_int "max_results" 100 ~minimum:1 ~maximum:100 args in
  let total_matches = ref 0 and result_count = ref 0 in
  let results = Array.make max_results None in
  let add_match path is_directory =
    match fuzzy_score query path with
    | None -> ()
    | Some score ->
        incr total_matches;
        let candidate = Some { path; is_directory; score } in
        let position = ref 0 in
        while !position < !result_count &&
              compare_fuzzy_match (Option.get results.(!position))
                (Option.get candidate) <= 0 do
          incr position
        done;
        if !position < max_results then (
          let next_count = min max_results (!result_count + 1) in
          for index = next_count - 1 downto !position + 1 do
            results.(index) <- results.(index - 1)
          done;
          results.(!position) <- candidate;
          result_count := next_count) in
  let walk_truncated = walk ~hidden ?cancel
    ~visit_directory:(fun path _ -> add_match path true)
    root relative (fun path _ -> add_match path false) in
  let matches = List.init !result_count (fun index ->
    let result = Option.get results.(index) in
    `Assoc [
      "path", `String result.path;
      "is_directory", `Bool result.is_directory;
      "score", `Int result.score
    ]) in
  Yojson.Basic.to_string (`Assoc [
    "matches", `List matches;
    "total_matches", `Int !total_matches;
    "truncated", `Bool (walk_truncated || !total_matches > !result_count)
  ])

let fuzzy_file_search ?cancel root args =
  scanning (fun () -> fuzzy_file_search ?cancel root args)

let append_bounded output text limit =
  if Buffer.length output + String.length text <= limit then (Buffer.add_string output text; true)
  else false

let list_files root args =
  let relative = optional_string "path" "." args in
  let output = Buffer.create 4096 in
  let count = ref 0 and overflow = ref false in
  let walk_limit = walk ~stop:(fun () -> !overflow) root relative (fun name _ ->
    if !count < 500 && not !overflow then
      if append_bounded output (name ^ "\n") (max_read_bytes - 128) then incr count
      else overflow := true
    else overflow := true) in
  if walk_limit || !overflow then Buffer.add_string output "[truncated; narrow the path]\n";
  if !count = 0 && not (walk_limit || !overflow) then "No files found" else Buffer.contents output
(* Str's backtracking is not time-bounded. Limit candidate lines and allow
   only one repetition operator; reject quantified groups and backreferences. *)
let list_files root args = scanning (fun () -> list_files root args)

let validate_regex pattern =
  if String.length pattern > 512 then fail "regex exceeds 512-byte limit";
  let length = String.length pattern in
  let repetitions = ref 0 in
  let rec scan i in_class =
    if i >= length then (
      if in_class then fail "invalid regex: unterminated character class")
    else match pattern.[i] with
      | '\\' ->
          if i + 1 = length then fail "invalid regex: trailing escape";
          (match pattern.[i + 1] with
           | '0' .. '9' | '{' -> fail "regex backreferences and interval repetition are not supported"
           | _ -> ());
          scan (i + 2) in_class
      | '[' when not in_class -> scan (i + 1) true
      | ']' when in_class -> scan (i + 1) false
      | ('*' | '+' | '?') when not in_class ->
          incr repetitions;
          if !repetitions > 1 ||
             (i >= 2 && pattern.[i - 1] = ')' && pattern.[i - 2] = '\\') then
            fail "regex has an unsafe repeated expression";
          scan (i + 1) in_class
      | _ -> scan (i + 1) in_class
  in
  scan 0 false


let glob root args =
  let pattern = required_string "pattern" args in
  valid_glob pattern;
  let relative = optional_string "path" "." args in
  let limit = optional_int "limit" 100 ~minimum:1 ~maximum:500 args in
  let hidden = optional_bool "hidden" false args in
  let output = Buffer.create 4096 in
  let count = ref 0 and overflow = ref false in
  let segments = Array.of_list (String.split_on_char '/' pattern) in
  let descend directory =
    (* Compare the directory with the pattern's leading segments up to `**`. *)
    let parts = Array.of_list (String.split_on_char '/' directory) in
    let rec fits index =
      if index >= Array.length parts then true
      else if index >= Array.length segments - 1 then false
      else segments.(index) = "**" ||
        (glob_segment segments.(index) parts.(index) && fits (index + 1)) in
    Array.length segments = 1 || fits 0 in
  let wanted = matching_glob pattern in
  let walk_limit = walk ~hidden ~stop:(fun () -> !overflow) ~descend root relative (fun name _ ->
    if wanted name then
      if !count < limit && not !overflow then
        if append_bounded output (name ^ "\n") (max_read_bytes - 128) then incr count
        else overflow := true
      else overflow := true) in
  if walk_limit || !overflow then Buffer.add_string output "[truncated; narrow the glob or path]\n";
  if !count = 0 && not (walk_limit || !overflow) then "No files found" else Buffer.contents output

let glob root args = scanning (fun () -> glob root args)

let search_matches root args ~regex =
  let query = required_string "pattern" args in
  if query = "" || String.length query > 4096 then fail "pattern must contain 1 to 4096 bytes";
  let case_sensitive = optional_bool "case_sensitive" true args in
  let match_query = if case_sensitive then query else String.lowercase_ascii query in
  let compiled = if regex then (
    validate_regex match_query;
    Some (try Str.regexp match_query with Failure reason -> fail ("invalid regex: " ^ reason)))
    else None in
  let relative = optional_string "path" "." args in
  let file_glob = optional_string "glob" "" args in
  if file_glob <> "" then valid_glob file_glob;
  let limit = optional_int "limit" max_matches ~minimum:1 ~maximum:max_matches args in
  let hidden = optional_bool "hidden" false args in
  let output = Buffer.create 4096 in
  let matches = ref 0 and scanned = ref 0 and truncated = ref false in
  (* Str backtracking is bounded per line; a deadline bounds the whole scan. *)
  let deadline = Unix.gettimeofday () +. regex_scan_seconds in
  let wanted = if file_glob = "" then fun _ -> true else matching_glob file_glob in
  let walk_limit = walk ~hidden ~stop:(fun () -> !truncated) root relative (fun name path ->
    if file_glob <> "" && not (wanted name) then ()
    else
      let size = (Unix.stat path).Unix.st_size in
      if size > max_write_bytes then ()
      else if !scanned + size > max_search_bytes then truncated := true
      else if not !truncated then (
        scanned := !scanned + size;
        let contents = Workspace_path.read_bounded path max_write_bytes in
        (* Lowercasing once keeps offsets aligned with the original contents. *)
        let haystack = if case_sensitive then contents else String.lowercase_ascii contents in
        let length = String.length contents in
        let emit number start finish =
          if !matches >= limit then truncated := true
          else (
            let line = String.sub contents start (finish - start) in
            let preview = if String.length line > 240 then String.sub line 0 240 ^ "..." else line in
            if append_bounded output (Printf.sprintf "%s:%d:%s\n" name number preview)
                (max_read_bytes - 128) then incr matches
            else truncated := true) in
        let line_end start = try String.index_from contents start '\n' with Not_found -> length in
        let binary () = String.contains contents '\000' in
        match compiled with
          | None ->
              (* Jump between literal hits instead of splitting every line. *)
              if not (String.contains match_query '\n') &&
                 find_from haystack match_query 0 <> None && not (binary ()) then (
                let rec hits from line_start number =
                  if not !truncated then match find_from haystack match_query from with
                    | None -> ()
                    | Some position ->
                        let rec advance start number =
                          let finish = line_end start in
                          if finish < position then advance (finish + 1) (number + 1)
                          else start, finish, number in
                        let start, finish, number = advance line_start number in
                        emit number start finish;
                        if finish < length then hits (finish + 1) (finish + 1) (number + 1) in
                hits 0 0 1)
          | Some _ when binary () -> ()
          | Some expression ->
              let rec lines start number =
                if Unix.gettimeofday () > deadline then truncated := true
                else if start < length && not !truncated then (
                  let finish = line_end start in
                  if finish - start > max_regex_line then truncated := true
                  else (
                    let matched =
                      try ignore (Str.search_forward expression
                            (String.sub haystack start (finish - start)) 0); true
                      with Not_found -> false in
                    if matched then emit number start finish);
                  lines (finish + 1) (number + 1))
              in lines 0 1)) in
  if walk_limit || !truncated then Buffer.add_string output "[truncated; narrow the path, glob or query]\n";
  if !matches = 0 && not (walk_limit || !truncated) then "No matches found" else Buffer.contents output

let search root args = scanning (fun () -> search_matches root args ~regex:false)
let grep root args = scanning (fun () -> search_matches root args ~regex:true)
let read_text_page root relative args =
  let path = Workspace_path.regular_path root relative in
  let requested_offset = optional_int "offset" 0 ~minimum:0 ~maximum:max_int args in
  let line = optional_int "line" 0 ~minimum:1 ~maximum:max_int args in
  (* Models often echo defaults (offset 0, line 1); only distinct positions conflict. *)
  let line = if line = 1 && requested_offset > 0 then 0 else line in
  if line > 0 && requested_offset > 0 then
    fail "use either line or offset, not both; retry with only one of them";
  let count = optional_int "max_bytes" 16_384 ~minimum:1 ~maximum:max_read_bytes args in
  let max_lines = optional_int "max_lines" 1000 ~minimum:1 ~maximum:1000 args in
  Workspace_path.with_fd path [Unix.O_RDONLY] 0 (fun fd ->
    let size = (Unix.fstat fd).Unix.st_size in
    if requested_offset > size then
      fail (Printf.sprintf "offset %d exceeds file size %d" requested_offset size);
    let buffer = Bytes.create 8192 in
    let position = ref 0 and current_line = ref 1 in
    while !position < size &&
          (if line > 0 then !current_line < line else !position < requested_offset) do
      let available = if line > 0 then size - !position
                      else requested_offset - !position in
      let n = Unix.read fd buffer 0 (min 8192 available) in
      if n = 0 then fail "file changed while reading";
      let consumed = ref n in
      for i = 0 to n - 1 do
        if (line = 0 || !current_line < line) && Bytes.get buffer i = '\n' then (
          incr current_line;
          if line > 0 && !current_line = line then consumed := i + 1)
      done;
      position := !position + !consumed
    done;
    if line > 0 && !current_line <> line then
      fail (Printf.sprintf "line %d exceeds file line count" line);
    let offset = if line > 0 then !position else requested_offset in
    ignore (Unix.lseek fd offset Unix.SEEK_SET);
    let requested = min (max_read_bytes - 512) (min count (size - offset)) in
    let bytes = Bytes.create requested in
    let rec read_at position =
      if position < requested then (
        let n = Unix.read fd bytes position (requested - position) in
        if n <> 0 then read_at (position + n) else position)
      else position in
    let actual = read_at 0 in
    let newlines = ref 0 and selected = ref actual in
    for i = 0 to actual - 1 do
      if i < !selected && Bytes.get bytes i = '\000' then
        fail "binary file; read_file supports text only";
      if !selected = actual && Bytes.get bytes i = '\n' then (
        incr newlines;
        if !newlines = max_lines then selected := i + 1)
    done;
    let text = Bytes.sub_string bytes 0 !selected in
    let end_offset = offset + !selected in
    let end_line = if !selected = 0 then !current_line
                   else !current_line + !newlines -
                     (if text.[!selected - 1] = '\n' then 1 else 0) in
    Printf.sprintf "%s\n[page: offset: %d; bytes: %d; lines: %d-%d; next offset: %d; next line: %d; file size: %d; %s]"
      text offset !selected !current_line end_line end_offset
      (!current_line + !newlines) size (if end_offset < size then "truncated" else "end of file"))

let reader_extensions =
  Workspace_reader.archive_extensions @
  [".sqlite"; ".sqlite3"; ".db"; ".pdf"; ".docx"; ".odt"; ".rtf";
   ".doc"; ".ppt"; ".pptx"; ".xls"; ".xlsx"; ".ods"; ".ipynb"]

let limit_lines text count =
  let rec scan index seen =
    if index >= String.length text then text
    else if text.[index] = '\n' then
      let seen = seen + 1 in
      if seen >= count && index + 1 < String.length text then
        String.sub text 0 (index + 1) ^ "\n[more lines omitted]"
      else scan (index + 1) seen
    else scan (index + 1) seen
  in
  scan 0 0

let read_file ?cancel ?context root args =
  let input = required_string "path" args in
  if String.length input > 4096 then fail "workspace read path exceeds the 4096-byte limit";
  let location = resolve_file_location ?cancel ?context ~root input in
  let reader_root, reader_path = match location with
    | Some location -> location.root, location.path
    | None -> root, input in
  let input_is_uri = Workspace_reader.is_scheme_uri input in
  let special =
    input_is_uri ||
    Workspace_reader.archive_spec reader_path <> None ||
    Workspace_reader.selector_suffix reader_path <> None ||
    Workspace_reader.sqlite_spec reader_path <> None ||
    Workspace_reader.suffix_extension reader_path reader_extensions <> None ||
    (match location with
     | Some location ->
         (try (Unix.stat (Workspace_path.checked_path location.root location.path)).Unix.st_kind = Unix.S_DIR
          with Unix.Unix_error _ | Workspace_path.Error _ -> false)
     | None -> false) in
  if not special then (
    match location with
    | Some location -> read_text_page location.root location.path args
    | None -> fail "unsupported workspace URI"
  ) else (
    let offset = field "offset" args and line = field "line" args in
    if offset <> `Null || line <> `Null then
      fail "offset and line pagination apply to local text paths; use a :line-range selector for structured reads";
    let limit = optional_int "max_bytes" 16_384 ~minimum:1
      ~maximum:max_read_bytes args in
    let max_lines = optional_int "max_lines" 1000 ~minimum:1 ~maximum:1000 args in
    let reader_path = if input_is_uri && location = None then input else reader_path in
    let text = Workspace_reader.read ?cancel
      ?read_artifact:(Option.map (fun context -> context.read_artifact) context)
      ~root:reader_root ~path:reader_path () in
    bounded_text (limit_lines text max_lines) (min limit (max_read_bytes - 128)))
let write_file root args =
  let relative = required_string "path" args in
  let content = required_string "content" args in
  let path = Workspace_path.writable_path root relative in
  Workspace_path.atomic_write path content;
  Printf.sprintf "Wrote %d bytes to %s" (String.length content) relative

let edit_file root args =
  let relative = required_string "path" args in
  let old_text = required_string "old_string" args in
  let new_text = required_string "new_string" args in
  if old_text = "" then fail "old_string must not be empty";
  let path = Workspace_path.writable_path root relative in
  let contents = Workspace_path.read_bounded path max_write_bytes in
  let index = match find_from contents old_text 0 with
    | Some index -> index
    | None -> fail "old_string was not found; read_file to check the exact text" in
  (match find_from contents old_text (index + 1) with
   | Some _ -> fail "old_string matches more than once; provide a longer unique excerpt"
   | None -> ());
  let result = String.sub contents 0 index ^ new_text ^
    String.sub contents (index + String.length old_text)
      (String.length contents - index - String.length old_text) in
  Workspace_path.atomic_write path result;
  Printf.sprintf "Edited %s" relative

let shell_quote text =
  "'" ^ String.concat "'\\''" (String.split_on_char '\'' text) ^ "'"

type gradle_token =
  | Gradle_word of string
  | Gradle_string of string option
  | Gradle_symbol of char
  | Gradle_newline

let gradle_tokens source =
  let length = String.length source in
  let starts index value =
    index + String.length value <= length &&
    String.sub source index (String.length value) = value in
  let is_word_start = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '$' -> true
    | _ -> false in
  let is_word_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '$' -> true
    | _ -> false in
  let quoted start quote triple =
    let delimiter = if triple then String.make 3 quote else String.make 1 quote in
    let rec find index interpolated =
      if index >= length then (length, None)
      else if starts index delimiter then
        index + String.length delimiter,
        (if interpolated then None
         else Some (String.sub source start (index - start)))
      else if source.[index] = '\\' then
        find (min length (index + 2)) interpolated
      else if source.[index] = '$' then find (index + 1) true
      else find (index + 1) interpolated in
    find start false in
  let rec skip_block index depth =
    if index >= length then length
    else if starts index "/*" then skip_block (index + 2) (depth + 1)
    else if starts index "*/" then
      if depth = 1 then index + 2 else skip_block (index + 2) (depth - 1)
    else skip_block (index + 1) depth in
  let rec scan index acc =
    if index >= length then List.rev acc
    else match source.[index] with
    | ' ' | '\t' | '\012' -> scan (index + 1) acc
    | '\n' -> scan (index + 1) (Gradle_newline :: acc)
    | '\r' ->
        let next = if index + 1 < length && source.[index + 1] = '\n'
          then index + 2 else index + 1 in
        scan next (Gradle_newline :: acc)
    | '/' when starts index "//" ->
        let rec line_end cursor =
          if cursor >= length || source.[cursor] = '\n' ||
             source.[cursor] = '\r' then cursor
          else line_end (cursor + 1) in
        scan (line_end (index + 2)) acc
    | '/' when starts index "/*" ->
        let stop = skip_block (index + 2) 1 in
        let newlines = ref 0 in
        for cursor = index to stop - 1 do
          if source.[cursor] = '\n' then incr newlines
        done;
        scan stop (List.init !newlines (fun _ -> Gradle_newline) @ acc)
    | ('"' | '\'') as quote ->
        let triple = starts index (String.make 3 quote) in
        let begin_content = index + (if triple then 3 else 1) in
        let stop, value = quoted begin_content quote triple in
        scan stop (Gradle_string value :: acc)
    | char when is_word_start char ->
        let stop = ref (index + 1) in
        while !stop < length && is_word_char source.[!stop] do
          incr stop
        done;
        scan !stop
          (Gradle_word (String.sub source index (!stop - index)) :: acc)
    | symbol -> scan (index + 1) (Gradle_symbol symbol :: acc)
  in
  scan 0 []

let gradle_module_path value =
  if value = ":" then Some ""
  else
    let value = if String.starts_with ~prefix:":" value then
        String.sub value 1 (String.length value - 1)
      else value in
    let parts = String.split_on_char ':' value in
    let safe part =
      part <> "" && part <> "." && part <> ".." &&
      String.for_all (function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' -> true
        | _ -> false) part in
    if value <> "" && List.for_all safe parts then
      Some (String.concat "/" parts)
    else None

let gradle_included_modules source =
  let tokens = gradle_tokens source in
  let modules = ref [] and unresolved = ref false in
  let add_arguments arguments =
    let arguments = List.filter (function [] -> false | _ -> true) arguments in
    if arguments = [] then unresolved := true;
    List.iter (function
      | [Gradle_string (Some value)] ->
          (match gradle_module_path value with
          | Some module_path -> modules := module_path :: !modules
          | None -> unresolved := true)
      | _ -> unresolved := true) arguments in
  let split_arguments tokens =
    let rec loop parens brackets braces current arguments = function
      | [] -> List.rev (List.rev current :: arguments), [], false
      | Gradle_symbol ')' :: rest when parens = 0 && brackets = 0 &&
                                      braces = 0 ->
          List.rev (List.rev current :: arguments), rest, true
      | Gradle_newline :: rest when parens = 0 && brackets = 0 &&
                                   braces = 0 ->
          List.rev (List.rev current :: arguments),
          Gradle_newline :: rest, false
      | Gradle_symbol ',' :: rest when parens = 0 && brackets = 0 &&
                                      braces = 0 ->
          loop parens brackets braces [] (List.rev current :: arguments) rest
      | (Gradle_symbol '(' as token) :: rest ->
          loop (parens + 1) brackets braces (token :: current) arguments rest
      | (Gradle_symbol ')' as token) :: rest ->
          loop (parens - 1) brackets braces (token :: current) arguments rest
      | (Gradle_symbol '[' as token) :: rest ->
          loop parens (brackets + 1) braces (token :: current) arguments rest
      | (Gradle_symbol ']' as token) :: rest ->
          loop parens (brackets - 1) braces (token :: current) arguments rest
      | (Gradle_symbol '{' as token) :: rest ->
          loop parens brackets (braces + 1) (token :: current) arguments rest
      | (Gradle_symbol '}' as token) :: rest ->
          loop parens brackets (braces - 1) (token :: current) arguments rest
      | token :: rest ->
          loop parens brackets braces (token :: current) arguments rest in
    loop 0 0 0 [] [] tokens in
  let rec scan parens brackets braces statement_start previous guarded = function
    | [] -> ()
    | Gradle_newline :: rest when parens = 0 && brackets = 0 &&
                                  braces = 0 ->
        scan 0 0 0 (not guarded) None guarded rest
    | Gradle_newline :: rest ->
        scan parens brackets braces false (Some Gradle_newline) guarded rest
    | Gradle_symbol ';' :: rest when parens = 0 && brackets = 0 &&
                                    braces = 0 ->
        scan 0 0 0 true None false rest
    | Gradle_word "include" :: Gradle_symbol '(' :: rest
      when parens = 0 && brackets = 0 && braces = 0 &&
           statement_start && not guarded ->
        let arguments, rest, closed = split_arguments rest in
        if not closed then unresolved := true;
        add_arguments arguments;
        scan 0 0 0 false (Some (Gradle_symbol ')')) false rest
    | Gradle_word "include" ::
        ((Gradle_string _ | Gradle_word _ | Gradle_symbol '*') as first) :: rest
      when parens = 0 && brackets = 0 && braces = 0 &&
           statement_start && not guarded ->
        let rec statement acc = function
          | [] -> List.rev acc, []
          | (Gradle_newline | Gradle_symbol ';') :: rest ->
              List.rev acc, rest
          | token :: rest -> statement (token :: acc) rest in
        let arguments, rest = statement [first] rest in
        let rec split current acc = function
          | [] -> List.rev (List.rev current :: acc)
          | Gradle_symbol ',' :: rest ->
              split [] (List.rev current :: acc) rest
          | token :: rest -> split (token :: current) acc rest in
        add_arguments (split [] [] arguments);
        scan 0 0 0 true None false rest
    | Gradle_word "include" :: rest ->
        (match previous with
        | Some (Gradle_symbol '.') -> ()
        | _ -> unresolved := true);
        scan parens brackets braces false (Some (Gradle_word "include"))
          false rest
    | (Gradle_word ("if" | "else" | "for" | "while" | "when") as token) ::
      rest ->
        scan parens brackets braces false (Some token) true rest
    | (Gradle_symbol '(' as token) :: rest ->
        scan (parens + 1) brackets braces false (Some token) guarded rest
    | (Gradle_symbol ')' as token) :: rest ->
        scan (max 0 (parens - 1)) brackets braces false (Some token)
          guarded rest
    | (Gradle_symbol '[' as token) :: rest ->
        scan parens (brackets + 1) braces false (Some token) guarded rest
    | (Gradle_symbol ']' as token) :: rest ->
        scan parens (max 0 (brackets - 1)) braces false (Some token)
          guarded rest
    | (Gradle_symbol '{' as token) :: rest ->
        scan parens brackets (braces + 1) false (Some token) false rest
    | (Gradle_symbol '}' as token) :: rest ->
        scan parens brackets (max 0 (braces - 1)) false (Some token)
          false rest
    | token :: rest ->
        scan parens brackets braces false (Some token)
          (guarded && parens > 0) rest in
  scan 0 0 0 true None false tokens;
  List.sort_uniq String.compare !modules, !unresolved

let join_relative directory name =
  if directory = "" || directory = "." then name
  else Filename.concat directory name

let relative_label path = if path = "" then "." else path

let source_roots = ["src/main"; "src/test"; "src/androidTest"]

type mobile_candidate =
  | Xcode_workspace of string * string
  | Xcode_project of string * string
  | Swift_package of string
  | Gradle_settings of string
  | Gradle_wrapper of string
  | Pubspec_manifest of string
  | Node_manifest of string
  | Oversized_manifest of string

let xcode_scheme_bundle path =
  let is_xcode_bundle name =
    Filename.check_suffix name ".xcworkspace" ||
    Filename.check_suffix name ".xcodeproj" in
  let rec find prefix = function
    | bundle :: "xcshareddata" :: "xcschemes" :: scheme :: []
      when is_xcode_bundle bundle &&
           String.length scheme > String.length ".xcscheme" &&
           Filename.check_suffix scheme ".xcscheme" ->
        Some (String.concat "/" (prefix @ [bundle]))
    | part :: rest -> find (prefix @ [part]) rest
    | [] -> None in
  find [] (String.split_on_char '/' path)


let mobile_project ?cancel root args =
  let subroot = optional_string "subroot" "" args
  and platform = optional_string "platform" "" args in
  if (subroot = "") <> (platform = "") then
    fail "select both an exact mobile subroot and platform";
  if subroot <> "" then (
    let checked = Workspace_path.checked_path root subroot in
    if (Unix.stat checked).Unix.st_kind <> Unix.S_DIR then
      fail "selected mobile subroot must be a workspace directory");
  if platform <> "" && platform <> "ios" && platform <> "android" then
    fail "mobile platform must be ios or android";
  let max_candidates = 100 in
  let candidates = ref [] and candidate_count = ref 0
  and truncated = ref false in
  let directories = Hashtbl.create 256 in
  let lockfiles = Hashtbl.create 32 in
  Hashtbl.replace directories "." ();
  let add_candidate candidate =
    if !candidate_count < max_candidates then (
      incr candidate_count;
      candidates := candidate :: !candidates
    ) else truncated := true in
  let add_regular path candidate =
    try
      let absolute = Workspace_path.checked_path root path in
      let stat = Unix.lstat absolute in
      if stat.Unix.st_kind = Unix.S_REG then
        if stat.Unix.st_size > max_write_bytes then
          add_candidate (Oversized_manifest path)
        else add_candidate (candidate path)
    with Unix.Unix_error _ | Workspace_path.Error _ -> () in
  let schemes_by_bundle = Hashtbl.create 8 in
  let oversized_schemes = ref [] and scheme_count = ref 0 in
  let bundle_registered bundle =
    List.exists (function
      | Xcode_workspace (path, _) -> path = bundle
      | Xcode_project (path, _) -> path = bundle
      | _ -> false) !candidates in
  let add_shared_scheme path =
    match xcode_scheme_bundle path with
    | Some bundle when bundle_registered bundle ->
        (try
           let absolute = Workspace_path.checked_path root path in
           let stat = Unix.lstat absolute in
           if stat.Unix.st_kind = Unix.S_REG then
             if !scheme_count >= max_candidates then truncated := true
             else (
               incr scheme_count;
               if stat.Unix.st_size > max_write_bytes then
                 oversized_schemes := path :: !oversized_schemes
               else
                 let previous = Option.value
                   (Hashtbl.find_opt schemes_by_bundle bundle) ~default:[] in
                 Hashtbl.replace schemes_by_bundle bundle
                   (path :: previous))
         with Unix.Unix_error _ | Workspace_path.Error _ -> ())
    | _ -> () in

  let visit_directory relative _absolute =
    Hashtbl.replace directories relative ();
    let name = Filename.basename relative in
    if Filename.check_suffix name ".xcworkspace" then (
      let manifest = Filename.concat relative "contents.xcworkspacedata" in
      add_regular manifest (fun _ -> Xcode_workspace (relative, manifest))
    ) else if Filename.check_suffix name ".xcodeproj" then (
      let manifest = Filename.concat relative "project.pbxproj" in
      add_regular manifest (fun _ -> Xcode_project (relative, manifest))) in
  let visit_file relative _absolute =
    match Filename.basename relative with
    | "Package.swift" -> add_regular relative (fun _ -> Swift_package relative)
    | "settings.gradle" | "settings.gradle.kts" ->
        add_regular relative (fun _ -> Gradle_settings relative)
    | "gradlew" | "gradlew.bat" ->
        add_regular relative (fun _ -> Gradle_wrapper relative)
    | "pubspec.yaml" -> add_regular relative (fun _ -> Pubspec_manifest relative)
    | "package.json" -> add_regular relative (fun _ -> Node_manifest relative)
    | "package-lock.json" | "npm-shrinkwrap.json" | "yarn.lock"
    | "pnpm-lock.yaml" as name ->
        (try
           let absolute = Workspace_path.checked_path root relative in
           if (Unix.lstat absolute).Unix.st_kind = Unix.S_REG then
             Hashtbl.replace lockfiles relative name
         with Unix.Unix_error _ | Workspace_path.Error _ -> ())
    | name when Filename.check_suffix name ".xcscheme" ->
        add_shared_scheme relative
    | _ -> () in

  let walk_truncated = walk ~hidden:true ?cancel ~visit_directory
    root "." visit_file in

  if walk_truncated then truncated := true;
  let candidates = List.rev !candidates in
  let directory path =
    match Filename.dirname path with "." -> "" | parent -> parent in
  let candidate_root = function
    | Xcode_workspace (bundle, _) | Xcode_project (bundle, _) -> bundle
    | Swift_package path | Gradle_settings path | Pubspec_manifest path
    | Node_manifest path -> relative_label (directory path)
    | Gradle_wrapper _ | Oversized_manifest _ -> "" in
  let candidate_platform candidate =
    match candidate with
    | Xcode_workspace _ | Xcode_project _ | Swift_package _ -> "ios"
    | Gradle_settings _ -> "android"
    | Pubspec_manifest _ | Node_manifest _ -> platform
    | _ -> "" in
  let selectable candidate = candidate_root candidate <> "" in
  let selection = List.filter (fun candidate ->
    selectable candidate && candidate_root candidate = subroot &&
    candidate_platform candidate = platform) candidates in
  let current = ref None in
  let host_owner candidate =
    let candidate_root = candidate_root candidate in
    List.exists (function
      | Pubspec_manifest path | Node_manifest path ->
          let parent = directory path in
          let host = if platform = "ios" then "ios" else "android" in
          platform <> "" &&
          (candidate_root = join_relative parent host ||
           let prefix = join_relative parent host ^ "/" in
           String.length candidate_root > String.length prefix &&
           String.sub candidate_root 0 (String.length prefix) = prefix)
      | _ -> false) candidates in
  let can_suggest () =
    not !truncated && List.length selection = 1 &&
    match !current with
    | Some candidate ->
        List.mem candidate selection && not (host_owner candidate)
    | None -> false in
  let output = Buffer.create 1024 in
  let truncation_notice = "[truncated; narrow the workspace and retry]\n" in
  let output_limit = max_read_bytes - String.length truncation_notice in
  let output_truncated = ref false in
  let append text =
    if not !output_truncated then
      if not (append_bounded output text output_limit) then (
        truncated := true;
        output_truncated := true) in
  let stack_count = ref 0 in
  let add_stack name commands =
    incr stack_count;
    if !stack_count = 1 then
      append "Detected mobile project evidence (commands are previews, never executed):\n";
    let selected = can_suggest () in
    append (name ^ "\n" ^
      (if commands <> [] && not selected then
        "  Focused commands withheld: select an exact subroot and platform; nested native hosts belong to their owning framework.\n"
       else "") ^
      String.concat "" (List.map (fun command -> "  " ^ command ^ "\n")
        (if selected then commands else []))) in
  let add_diagnostic text = append (text ^ "\n") in
  List.sort String.compare !oversized_schemes |> List.iter (fun path ->
    add_diagnostic
      (Printf.sprintf
        "Ignored oversized Xcode shared scheme: %s (exceeds %d-byte limit; no scheme commands suggested)."
        path max_write_bytes));

  let command_in path command =
    let parent = directory path in
    if parent = "" then command
    else "cd " ^ shell_quote parent ^ " && " ^ command in
  let read_manifest path =
    try Some (Workspace_path.read_bounded
      (Workspace_path.checked_path root path) max_write_bytes)
    with Unix.Unix_error _ | Workspace_path.Error _ -> None in
  let gradle_settings = List.filter_map (function
    | Gradle_settings path -> Some path
    | _ -> None) candidates in
  let gradle_wrappers = List.filter_map (function
    | Gradle_wrapper path -> Some path
    | _ -> None) candidates in
  let render_xcode kind bundle manifest =
    let schemes = Option.value (Hashtbl.find_opt schemes_by_bundle bundle)
      ~default:[] |> List.sort String.compare in
    let scheme_name path =
      let filename = Filename.basename path in
      String.sub filename 0
        (String.length filename - String.length ".xcscheme") in
    let provenance = match schemes with
      | [] ->
          "  Candidate shared schemes: none found; private/user schemes remain unknown."
      | paths ->
          String.concat "\n" (List.map (fun path ->
            "  Candidate shared scheme: " ^ scheme_name path ^
            " (" ^ path ^ ")") paths) in
    add_stack
      ("Xcode " ^ kind ^ ": " ^ manifest ^ "\n" ^ provenance ^
       "\n  Use separately approved xcode_preflight schemes, destinations, then build/test; candidate filenames are not verified schemes.")
      [] in

  let render_gradle settings_path =
    let parent = directory settings_path in
    let wrappers = List.filter (fun path -> directory path = parent)
      gradle_wrappers in
    let wrapper_text = match wrappers with
      | [] ->
          "  Gradle wrapper: not found beside settings; system Gradle availability is unknown."
      | paths ->
          String.concat "\n" (List.map (fun path ->
            "  Gradle wrapper script: " ^ path ^
            " (regular-file evidence only; not executed)") paths) in
    let module_lines source =
      let modules, unresolved = gradle_included_modules source in
      let modules = "" :: modules in
      let module_lines = List.concat_map (fun module_path ->
        let gradle_path = if module_path = "" then ":"
          else ":" ^ String.concat ":"
            (String.split_on_char '/' module_path) in
        let module_directory = if module_path = "" then parent
          else join_relative parent module_path in
        let heading = if module_path = "" then
            "  Declared module: : (settings root)"
          else "  Declared module: " ^ gradle_path in
        if not (Hashtbl.mem directories
          (if module_directory = "" then "." else module_directory)) then
          [heading;
           "    Conventional module directory not found: " ^
             relative_label module_directory ^
             " (projectDir mapping remains unknown)."]
        else
          let roots = List.filter_map (fun source_root ->
            let path = join_relative module_directory source_root in
            if Hashtbl.mem directories path then Some path else None)
            source_roots in
          [heading;
           "    Candidate module directory: " ^
             relative_label module_directory ^
             " (conventional path; projectDir mapping is not evaluated)."] @
          (match roots with
           | [] -> ["    Candidate source roots: none found under conventional src/."]
           | roots -> List.map (fun path ->
               "    Candidate source root: " ^ path) roots)) modules in
      module_lines @
      (if unresolved then
        ["  Unresolved dynamic or unsupported module include; additional modules remain unknown."]
       else []) @
      ["  Task names, variants, projectDir remapping and SDK readiness remain unknown."] in
    match read_manifest settings_path with
    | None ->
        add_stack
          ("Android Gradle settings: " ^ settings_path ^
           "\n  Settings file is unreadable; no modules inferred.\n" ^
           wrapper_text) []
    | Some settings ->
        let lines = module_lines settings in
        add_stack
          ("Android Gradle settings: " ^ settings_path ^ "\n" ^
           wrapper_text ^ "\n" ^ String.concat "\n" lines)
          [] in
  List.iter (fun candidate ->
    current := Some candidate;
    match candidate with
    | Xcode_workspace (bundle, manifest) ->
        render_xcode "workspace" bundle manifest
    | Xcode_project (bundle, manifest) ->
        render_xcode "project" bundle manifest
    | Swift_package path ->
        (match read_manifest path with
        | None -> add_diagnostic
            ("Ignored unreadable Swift package manifest: " ^ path ^
             " (no test roots suggested).")
        | Some _ ->
            let package_root = directory path in
            let tests_root = join_relative package_root "Tests" in
            let child_roots = Hashtbl.fold (fun candidate () found ->
              if directory candidate = tests_root then candidate :: found
              else found) directories []
              |> List.sort String.compare in
            let test_roots =
              if Hashtbl.mem directories tests_root then
                tests_root :: child_roots
              else [] in
            let root_lines = match test_roots with
              | [] -> ["  Candidate test roots: none found under conventional Tests/."]
              | roots -> List.map (fun candidate ->
                  "  Candidate test root: " ^ candidate ^
                  " (filesystem convention only; target mapping unknown)") roots in
            add_stack
              (String.concat "\n"
                (("Swift Package Manager: " ^ path) ::
                 ("  Package root: " ^ relative_label package_root) ::
                 root_lines @
                 ["  Package.swift was read as bounded text, never evaluated.";
                  "  Computed/unsupported test targets and SDK requirements remain unknown."]))
              [])
    | Gradle_settings path -> render_gradle path
    | Gradle_wrapper path ->
        let parent = directory path in
        if not (List.exists (fun settings -> directory settings = parent)
            gradle_settings) then
          add_stack
            ("Gradle wrapper script: " ^ path ^
             "\n  No settings file was discovered beside the wrapper; modules and tasks remain unknown.")
            []
    | Pubspec_manifest path ->
        (match read_manifest path with
         | None -> add_diagnostic
             ("Ignored unreadable mobile manifest: " ^ path ^
              " (no commands suggested).")
         | Some pubspec ->
             let lines = String.split_on_char '\n' pubspec in
             let section = ref "" and subsection = ref "" in
             let flutter_sdk = ref false and plugin = ref false in
             List.iter (fun line ->
               let trimmed = String.trim line in
               if trimmed <> "" && trimmed.[0] <> '#' then (
                 let indent =
                   let rec count n = if n < String.length line &&
                     (line.[n] = ' ' || line.[n] = '\t') then count (n + 1)
                     else n in count 0 in
                 if indent = 0 then (
                   section := trimmed; subsection := "")
                 else if indent = 2 then (
                   subsection := trimmed;
                   if !section = "flutter:" && trimmed = "plugin:"
                   then plugin := true)
                 else if indent = 4 && !section = "dependencies:" &&
                   !subsection = "flutter:" && trimmed = "sdk: flutter"
                 then flutter_sdk := true)) lines;
             let package_root = directory path in
             let host name =
               let location = join_relative package_root name in
               if Hashtbl.mem directories location then
                 ["  Existing " ^ name ^ " host root: " ^ location]
               else [] in
             let kind = if !plugin && !flutter_sdk then "Flutter plugin"
               else if !flutter_sdk then
                 if host "ios" <> [] || host "android" <> [] then "Flutter app"
                 else "Flutter package"
               else "Dart package" in
             add_stack (String.concat "\n"
               (("  " ^ kind ^ ": " ^ path) ::
                ("  Package root: " ^ relative_label package_root) ::
                (if !flutter_sdk then host "ios" @ host "android" else []) @
                ["  Pubspec was read as bounded text; SDK and build readiness remain unknown."]))
               [])
    | Node_manifest path ->
        (match read_manifest path with
         | None -> add_diagnostic
             ("Ignored unreadable mobile manifest: " ^ path ^
              " (no commands suggested).")
         | Some text ->
             (match Yojson.Basic.from_string text with
              | `Assoc _ as json ->
                  let has_dependency name =
                    List.exists (fun section ->
                      match field section json with
                      | `Assoc entries ->
                          (match List.assoc_opt name entries with
                           | Some (`String _) -> true | _ -> false)
                      | _ -> false) ["dependencies"; "devDependencies"] in
                  let package_root = directory path in
                  let locks = Hashtbl.fold (fun file name found ->
                    if directory file = package_root then (file, name) :: found
                    else found) lockfiles [] |> List.sort compare in
                  let manager = function
                    | "package-lock.json" | "npm-shrinkwrap.json" -> "npm"
                    | "yarn.lock" -> "yarn"
                    | "pnpm-lock.yaml" -> "pnpm"
                    | _ -> assert false in
                  let choices = List.sort_uniq String.compare
                    (List.map (fun (_, name) -> manager name) locks) in
                  let selection = match choices with
                    | [choice] -> Some choice | _ -> None in
                  let scripts = match field "scripts" json with
                    | `Assoc entries -> List.filter_map (function
                        | name, `String _ -> Some name | _ -> None) entries
                    | _ -> [] in
                  let commands = match selection with
                    | None -> []
                    | Some manager ->
                        List.filter_map (fun name ->
                          if List.mem name scripts then
                            Some (command_in path
                              (if manager = "npm" then
                                 "npm run " ^ shell_quote name
                               else manager ^ " " ^ shell_quote name))
                          else None) ["test"; "lint"; "build"] in
                  let hosts = List.filter_map (fun name ->
                    let host = join_relative package_root name in
                    if Hashtbl.mem directories host then
                      Some ("  Existing " ^ name ^ " host root: " ^ host)
                    else None) ["ios"; "android"] in
                  let kind = if has_dependency "expo" then Some "Expo"
                    else if has_dependency "react-native" then
                      Some "React Native" else None in
                  (match kind with
                   | None -> ()
                   | Some kind ->
                       let lock_lines = match locks with
                         | [] -> ["  Package manager unknown: no lockfile; choose explicitly."]
                         | locks ->
                             List.map (fun (file, _) -> "  Lockfile: " ^ file)
                               locks @
                             (match selection with
                              | Some name -> ["  Package manager: " ^ name]
                              | None -> ["  Conflicting lockfiles: choose a package manager explicitly; no commands suggested."]) in
                       let script_lines = if scripts = [] then
                           ["  Declared scripts: none."]
                         else List.map (fun name ->
                           "  Declared script: " ^ name) scripts in
                       add_stack
                         (String.concat "\n"
                           (("  " ^ kind ^ ": " ^ path) ::
                            ("  Package root: " ^ relative_label package_root) ::
                            lock_lines @ hosts @ script_lines @
                            ["  Native build, SDK and script effects remain unverified."]))
                         commands)
              | _ -> add_diagnostic
                  ("Ignored invalid mobile manifest: " ^ path ^
                   " (no commands suggested).")
              | exception Yojson.Json_error _ -> add_diagnostic
                  ("Ignored invalid mobile manifest: " ^ path ^
                   " (no commands suggested).")))
    | Oversized_manifest path ->
        add_diagnostic
          (Printf.sprintf "Ignored oversized mobile manifest: %s (exceeds %d-byte limit; no commands suggested)."
             path max_write_bytes))
    candidates;
  if subroot <> "" && selection = [] then
    add_diagnostic ("No mobile stack matched selected subroot " ^ subroot ^
      " and platform " ^ platform ^ "; no commands suggested.");
  if !stack_count = 0 then
    add_diagnostic "No supported mobile project manifests found under workspace.";
  let result = Buffer.contents output in
  if !output_truncated && subroot <> "" then
    "Mobile inventory output exceeded its limit; narrow workspace and retry. No commands suggested.\n"
  else if !truncated then result ^ truncation_notice else result

let mobile_project ?cancel root args =
  scanning (fun () -> mobile_project ?cancel root args)

let run_command ?cancel ?on_progress root args =
  let command = required_string "command" args in
  if command = "" then fail "command must not be empty";
  let timeout = optional_int "timeout_seconds" 60 ~minimum:1 ~maximum:300 args in
  let result = Workspace_process.run_shell ?cancel ?on_progress
    ~timeout_seconds:timeout ~output_limit:max_command_bytes
    ~cwd:(Some root) ~command () in
  let status = match result.termination with
    | Workspace_process.Exited code -> Printf.sprintf "exit %d" code
    | Workspace_process.Signaled signal -> Printf.sprintf "signal %d" signal
    | Workspace_process.Timed_out -> "timed out"
    | Workspace_process.Cancelled -> raise Cancelled in
  Printf.sprintf "Status: %s%s\n%s" status
    (if result.truncated then " (output truncated to last 65536 bytes)" else "")
    result.output
let xcode_command args =
  let bundle = required_string "subroot" args in
  let action = required_string "action" args in
  let flag = if Filename.check_suffix bundle ".xcworkspace" then "-workspace"
    else if Filename.check_suffix bundle ".xcodeproj" then "-project"
    else fail "select an exact Xcode workspace or project bundle" in
  let prefix = "xcodebuild " ^ flag ^ " " ^
    shell_quote (Filename.basename bundle) in
  let scheme = optional_string "scheme" "" args in
  match action with
  | "schemes" -> prefix ^ " -list -json"
  | "destinations" ->
      if scheme = "" then fail "select a discovered scheme";
      prefix ^ " -scheme " ^ shell_quote scheme ^ " -showdestinations"
  | "simulators" ->
      if scheme = "" then fail "select a discovered scheme";
      "xcrun simctl list devices available -j"
  | "build" | "test" ->
      let destination = required_string "destination" args in
      if scheme = "" || destination = "" then
        fail "select a discovered scheme and simulator destination";
      prefix ^ " -scheme " ^ shell_quote scheme ^
      " -destination " ^
      shell_quote ("platform=iOS Simulator,id=" ^ destination) ^
      " CODE_SIGNING_ALLOWED=NO " ^ action
  | _ -> fail "unsupported Xcode preflight action"

let xcode_preflight ~approved ?cancel ?on_progress ?context root args =
  if not approved then fail "this action requires explicit interactive approval";
  let context = require_session_context context in
  let bundle = required_string "subroot" args in
  let action = required_string "action" args in
  let command = xcode_command args in
  let manifest = Filename.concat bundle
    (if Filename.check_suffix bundle ".xcworkspace" then
      "contents.xcworkspacedata" else "project.pbxproj") in
  let file = Workspace_path.checked_path root manifest in
  let stat = Unix.lstat file in
  if stat.Unix.st_kind <> Unix.S_REG || stat.Unix.st_size > max_write_bytes then
    fail "selected Xcode bundle has no bounded regular manifest";
  let manifest_hash = Digestif.SHA256.(
    to_hex (digest_string (Workspace_path.read_bounded file max_write_bytes))) in
  let observed = ref false in
  let truncated = walk ?cancel ~hidden:true ~visit_directory:(fun path _ ->
    if path = bundle then observed := true) root "." (fun _ _ -> ()) in
  if truncated || not !observed then
    fail "selected Xcode bundle is not a scanned workspace project";
  let scheme = optional_string "scheme" "" args in
  Mutex.lock context.xcode_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.xcode_lock) (fun () ->
    check_session_context context;
    if action = "schemes" then context.xcode_discovery <- None
    else (
      let discovery = match context.xcode_discovery with
        | Some state when state.bundle = bundle && state.root = root -> state
        | _ -> fail "approve scheme discovery for this exact Xcode bundle first" in
      if discovery.manifest_hash <> manifest_hash then (
        context.xcode_discovery <- None;
        fail "Xcode manifest changed since discovery; discover schemes again");
      if not (List.mem scheme discovery.schemes) then
        fail "scheme was not discovered for this Xcode bundle";
      if action = "build" || action = "test" || action = "simulators" then (
        let destinations = Option.value ~default:[]
          (List.assoc_opt scheme discovery.destinations) in
        if action = "simulators" then (
          if destinations = [] then
            fail "approve destination discovery with a compatible simulator for this scheme first")
        else
          let destination = required_string "destination" args in
          if not (List.mem destination destinations) then
            fail "simulator destination was not discovered for this scheme"));
    let cwd = Filename.dirname (Workspace_path.checked_path root bundle) in
    let result = Workspace_process.run_shell ?cancel ?on_progress
      ~timeout_seconds:(optional_int "timeout_seconds" 120
        ~minimum:1 ~maximum:300 args)
      ~output_limit:max_command_bytes ~cwd:(Some cwd) ~command () in
    let outcome = match result.termination with
      | Workspace_process.Exited code -> Printf.sprintf "exit %d" code
      | Workspace_process.Signaled signal -> Printf.sprintf "signal %d" signal
      | Workspace_process.Timed_out -> "timed out"
      | Workspace_process.Cancelled -> raise Cancelled in
    let successful = result.termination = Workspace_process.Exited 0 &&
      not result.truncated in
    if action = "schemes" then (
      if successful then (
        let schemes = Workspace_xcode.schemes result.output in
        context.xcode_discovery <- Some
          { Workspace_xcode.root; bundle; manifest_hash; schemes; destinations = [] };
        "Xcode scheme discovery: " ^ outcome ^ "\nVerified schemes: " ^
        (if schemes = [] then
           "none; select another project/workspace or configure a shared scheme"
         else String.concat ", " schemes))
      else "Xcode scheme discovery: " ^ outcome ^
        (if result.truncated then " (output truncated)" else "") ^
        "\n" ^ result.output)
    else if action = "destinations" then (
      let discovery = Option.get context.xcode_discovery in
      discovery.destinations <- List.remove_assoc scheme discovery.destinations;
      if successful then (
        let destinations = Workspace_xcode.destinations result.output in
        discovery.destinations <- (scheme, destinations) ::
          discovery.destinations;
        "Xcode destination discovery: " ^ outcome ^
        "\nAvailable iOS Simulator IDs: " ^
        (if destinations = [] then
           "none; select another scheme or make a compatible simulator runtime available"
         else String.concat ", " destinations))
      else "Xcode destination discovery: " ^ outcome ^
        (if result.truncated then " (output truncated)" else "") ^
        "\n" ^ result.output)
    else if action = "simulators" then
      if successful then (
        let discovery = Option.get context.xcode_discovery in
        let destinations = Option.get
          (List.assoc_opt scheme discovery.destinations) in
        let devices = Workspace_xcode.compatible_simulators
          ~destinations result.output in
        "Apple simulator inventory: " ^ outcome ^ " (scheme " ^ scheme ^ ")" ^
        (if devices = [] then
           "\nNo available compatible iOS Simulator devices; no device was booted."
         else "\nAvailable compatible iOS Simulator devices:\n" ^
           (List.map (fun (device : Workspace_xcode.simulator) ->
             Printf.sprintf "iOS %s | %s | %s | %s" device.runtime
               device.id device.state device.name) devices
            |> String.concat "\n")))
      else
        "Apple simulator inventory: " ^ outcome ^
        (if result.truncated then " (output truncated; no devices accepted)"
         else "; no devices accepted")
    else
      let locations = if result.termination = Workspace_process.Exited 0 then []
        else Workspace_swift_diagnostics.locations ~root ~cwd result.output in
      "Xcode " ^ action ^ ": " ^ outcome ^ " (scheme " ^ scheme ^ ")" ^
      (if result.truncated then " (output truncated; incomplete result)" else "") ^
      (if locations = [] then "" else
         "\nChecked Swift errors:\n" ^ String.concat "\n" locations) ^
      "\n" ^ result.output)

let mobile_command ~root args =
  let stack = required_string "stack" args in
  let action = required_string "action" args in
  let subroot = required_string "subroot" args in
  let target = optional_string "target" "" args in
  let manager = optional_string "manager" "" args in
  match stack with
  | "swiftpm" ->
      Workspace_swiftpm_focus.command ~root ~subroot ~action ~target
  | "gradle" ->
      Workspace_gradle_focus.command ~root ~subroot ~action ~task:target
  | "flutter" ->
      Workspace_flutter_focus.command ~root ~subroot ~action ~target
  | "node" ->
      Workspace_node_scripts.command ~root ~subroot ~action ~manager
  | _ -> fail "mobile check stack must be swiftpm, gradle, flutter or node"

let mobile_manifest ~root ~stack ~subroot =
  let relative = match stack with
    | "swiftpm" -> Filename.concat subroot "Package.swift"
    | "gradle" ->
        let settings = Filename.concat subroot "settings.gradle.kts" in
        let checked = Workspace_path.checked_path root settings in
        (try if (Unix.lstat checked).Unix.st_kind = Unix.S_REG then settings
          else fail "Gradle settings must be a regular file"
         with Unix.Unix_error (Unix.ENOENT, _, _) ->
           Filename.concat subroot "settings.gradle")
    | _ -> fail "no discovery manifest for this mobile stack" in
  let path = Workspace_path.checked_path root relative in
  Digestif.SHA256.(
    to_hex (digest_string (Workspace_path.read_bounded path max_write_bytes)))

let mobile_check ~approved ?cancel ?on_progress ?context root args =
  if not approved then fail "mobile project code requires explicit interactive approval";
  let context = require_session_context context in
  let root = Workspace_path.root_path root in
  let stack = required_string "stack" args
  and action = required_string "action" args
  and subroot = required_string "subroot" args in
  let command, cwd = mobile_command ~root args in
  let discovery_action = (stack = "swiftpm" && action = "discover") ||
    (stack = "gradle" && action = "tasks") in
  let needs_discovery = (stack = "swiftpm" || stack = "gradle") &&
    action = "run" in
  let manifest_hash = if discovery_action || needs_discovery then
      Some (mobile_manifest ~root ~stack ~subroot)
    else None in
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    check_session_context context;
    if discovery_action then context.mobile_discovery <- None;
    if needs_discovery then (
      let state = match context.mobile_discovery with
        | Some state when state.stack = stack && state.root = root &&
                          state.subroot = subroot -> state
        | _ -> fail "approve focused task discovery for this project first" in
      if Some state.manifest_hash <> manifest_hash then (
        context.mobile_discovery <- None;
        fail "mobile project manifest changed since task discovery; discover again");
      let target = required_string "target" args in
      if not (List.mem target state.choices) then
        fail "focused task was not in the approved discovery result");
    let result = Workspace_process.run_shell ?cancel ?on_progress
      ~timeout_seconds:(optional_int "timeout_seconds" 120
        ~minimum:1 ~maximum:300 args)
      ~output_limit:max_command_bytes ~cwd:(Some cwd) ~command () in
    let outcome = match result.termination with
      | Workspace_process.Exited code -> Printf.sprintf "exit %d" code
      | Workspace_process.Signaled signal -> Printf.sprintf "signal %d" signal
      | Workspace_process.Timed_out -> "timed out"
      | Workspace_process.Cancelled -> raise Cancelled in
    let successful = result.termination = Workspace_process.Exited 0 &&
      not result.truncated in
    if discovery_action && successful then (
      let choices = if stack = "swiftpm" then
          Workspace_swiftpm_focus.tests result.output
        else Workspace_gradle_focus.tasks result.output in
      context.mobile_discovery <- Some {
        stack; root; subroot; manifest_hash = Option.get manifest_hash;
        choices };
      "Mobile " ^ stack ^ " discovery: " ^ outcome ^ "\nTasks:\n" ^
        String.concat "\n" choices)
    else
      let note =
        if stack = "swiftpm" && action = "run" &&
           Workspace_swiftpm_focus.no_tests result.output then
          " (zero matching tests executed; not a pass)"
        else if result.truncated then " (output truncated; incomplete result)"
        else "" in
      let locations =
        if result.termination = Workspace_process.Exited 0 then []
        else if stack = "swiftpm" && action = "run" then
          Workspace_swift_diagnostics.locations ~within_cwd:true
            ~root ~cwd result.output
        else if stack = "gradle" && action = "run" then
          Workspace_android_diagnostics.locations ~root ~cwd ~subroot
            ~task:(required_string "target" args) result.output
        else if stack = "flutter" then
          Workspace_flutter_diagnostics.locations ~root ~cwd ~subroot result.output
        else if stack = "node" then
          Workspace_node_diagnostics.locations ~root ~cwd ~subroot result.output
        else [] in
      "Mobile " ^ stack ^ " " ^ action ^ ": " ^ outcome ^ note ^
      (if stack = "gradle" && action = "run" then
         " (selected task " ^ required_string "target" args ^ ")" else "") ^
      (if locations = [] then "" else
         "\nChecked " ^ (if stack = "flutter" then "Dart"
           else if stack = "node" then "JS/TS"
           else if stack = "gradle" then "Kotlin/Java" else "Swift") ^
         " errors:\n" ^ String.concat "\n" locations) ^
      "\n" ^ result.output)

let android_device_command ~root args =
  let subroot = required_string "subroot" args in
  let action = required_string "action" args in
  let _, cwd = Workspace_gradle_focus.command ~root ~subroot
    ~action:"tasks" ~task:"" in
  let command = match action with
    | "avds" -> "emulator -list-avds"
    | "devices" -> "adb devices"
    | _ -> fail "Android inventory action must be avds or devices" in
  command, cwd

let android_devices ~approved ?cancel ?on_progress ?context root args =
  if not approved then fail "Android inventory requires explicit interactive approval";
  let context = require_session_context context in
  check_session_context context;
  let command, cwd = android_device_command ~root args in
  let action = required_string "action" args in
  let result = Workspace_process.run_shell ?cancel ?on_progress
    ~timeout_seconds:30 ~output_limit:16_384 ~cwd:(Some cwd) ~command () in
  let outcome = match result.termination with
    | Workspace_process.Exited code -> Printf.sprintf "exit %d" code
    | Workspace_process.Signaled signal -> Printf.sprintf "signal %d" signal
    | Workspace_process.Timed_out -> "timed out"
    | Workspace_process.Cancelled -> raise Cancelled in
  let label = if action = "avds" then "AVD" else "ADB" in
  let prefix = "Android " ^ label ^ " inventory: " ^ outcome in
  if result.termination <> Workspace_process.Exited 0 || result.truncated then
    prefix ^ " (no device choices; command failed or output was truncated)"
  else if action = "avds" then
    let names = Workspace_android_devices.avds result.output in
    prefix ^ "\nConfigured AVDs (not running; SDK image readiness unknown): " ^
    (if names = [] then "none"
     else String.concat ", " (List.map (Printf.sprintf "%S") names))
  else
    let devices = Workspace_android_devices.adb_devices result.output in
    let emulators, other = List.partition
      (fun device -> device.Workspace_android_devices.emulator) devices in
    let ready = List.filter
      (fun device -> device.Workspace_android_devices.state =
        Workspace_android_devices.Ready) emulators in
    let unavailable = List.filter
      (fun device -> device.Workspace_android_devices.state <>
        Workspace_android_devices.Ready) emulators in
    let state = function
      | Workspace_android_devices.Ready -> "ready"
      | Workspace_android_devices.Offline -> "offline"
      | Workspace_android_devices.Unauthorized -> "unauthorized"
      | Workspace_android_devices.Unavailable -> "unavailable" in
    let row device = Printf.sprintf "%s (%s)"
      device.Workspace_android_devices.serial
      (state device.Workspace_android_devices.state) in
    let count wanted = List.fold_left (fun n device ->
      if device.Workspace_android_devices.state = wanted then n + 1 else n)
      0 other in
    prefix ^ "\nReady emulators: " ^
    (if ready = [] then "none" else String.concat ", " (List.map row ready)) ^
    "\nUnavailable emulators: " ^
    (if unavailable = [] then "none"
     else String.concat ", " (List.map row unavailable)) ^
    Printf.sprintf
      "\nPhysical/unclassified devices (not selectable): ready=%d offline=%d unauthorized=%d unavailable=%d"
      (count Workspace_android_devices.Ready)
      (count Workspace_android_devices.Offline)
      (count Workspace_android_devices.Unauthorized)
      (count Workspace_android_devices.Unavailable)

let string_list name args =
  match field name args with
  | `List values ->
      List.map (function `String value -> value | _ -> fail (name ^ " must contain only strings")) values
  | `Null -> []
  | _ -> fail (name ^ " must be an array")

let environment_overrides args =
  match field "environment" args with
  | `Null -> []
  | `Assoc fields ->
      let names = List.map fst fields in
      if List.length names <> List.length (List.sort_uniq String.compare names) then
        fail "duplicate environment override";
      List.map (function
        | name, `String value -> name, value
        | name, _ -> fail (name ^ " environment value must be a string")) fields
  | _ -> fail "environment must be an object of string values"

let optional_timeout name maximum args =
  match field name args with
  | `Null -> None
  | `Int seconds when seconds >= 1 && seconds <= maximum -> Some seconds
  | _ -> fail (Printf.sprintf "%s must be between 1 and %d seconds" name maximum)

let process_cwd ?cancel ?context ~root args =
  let requested = optional_string "cwd" "." args in
  let location = match resolve_file_location ?cancel ?context ~root requested with
    | Some location -> location
    | None -> fail "process working directory must be workspace-relative or an owned worktree URI" in
  let workspace_root = Workspace_path.root_path location.root in
  Workspace_path.root_path
    (Workspace_path.checked_path workspace_root location.path)

let process_manager context = context.process_manager
let require_explicit_approval approved =
  if not approved then fail "this action requires explicit interactive approval"


let start_process ~approved ?cancel ?context root args =
  require_explicit_approval approved;
  let context = require_session_context context in
  let id = required_string "id" args in
  let program = required_string "program" args in
  let arguments = string_list "arguments" args in
  let cwd = process_cwd ?cancel ~context ~root args in
  let environment = environment_overrides args in
  ignore (Workspace_process.environment_with_overrides environment);
  let timeout_seconds = optional_timeout "timeout_seconds" 86_400 args in
  let output_limit = optional_int "output_limit" 65_536 ~minimum:1
    ~maximum:Workspace_process.max_output_limit args in
  let pty = optional_bool "pty" false args in
  Workspace_process.start (process_manager context) ~id ~cwd:(Some cwd) ~environment
    ?timeout_seconds ~output_limit ~pty ~program ~arguments ();
  Printf.sprintf "Started process job %S: %s"
    id (Filename.quote_command program arguments)

let start_shell ~approved ?cancel ?context root args =
  require_explicit_approval approved;
  let context = require_session_context context in
  let id = required_string "id" args in
  let command = required_string "command" args in
  let cwd = process_cwd ?cancel ~context ~root args in
  let environment = environment_overrides args in
  ignore (Workspace_process.environment_with_overrides environment);
  let timeout_seconds = optional_timeout "timeout_seconds" 86_400 args in
  let output_limit = optional_int "output_limit" 65_536 ~minimum:1
    ~maximum:Workspace_process.max_output_limit args in
  let pty = optional_bool "pty" false args in
  Workspace_process.start_shell (process_manager context) ~id ~cwd:(Some cwd)
    ~environment ?timeout_seconds ~output_limit ~pty ~command ();
  Printf.sprintf "Started shell job %S." id

let process_termination = function
  | Workspace_process.Exited code -> Printf.sprintf "exit %d" code
  | Workspace_process.Signaled signal -> Printf.sprintf "signal %d" signal
  | Workspace_process.Timed_out -> "timed out"
  | Workspace_process.Cancelled -> "cancelled"

let process_status = function
  | Workspace_process.Running -> "running"
  | Workspace_process.Completed termination -> process_termination termination

let process_list ?context _root _args =
  let manager = process_manager (require_session_context context) in
  match Workspace_process.jobs manager with
  | [] -> "No managed process jobs."
  | jobs ->
      bounded_text
        (String.concat "\n" (List.map (fun (job : Workspace_process.job_summary) ->
          Printf.sprintf "%s · %s · %d bytes%s%s\n%s"
            job.id (process_status job.status) job.bytes_received
            (if job.truncated then " · output truncated" else "")
            (if job.ready then " · ready" else "")
            job.command) jobs))
        (max_read_bytes - 128)

let process_output ?context _root args =
  let manager = process_manager (require_session_context context) in
  let id = required_string "id" args in
  let offset = optional_int "offset" 0 ~minimum:0 ~maximum:max_int args in
  let max_bytes = optional_int "max_bytes" 16_384 ~minimum:1
    ~maximum:(max_read_bytes - 256) args in
  let page = Workspace_process.read_output manager ~id ~offset ~max_bytes () in
  Printf.sprintf
    "[output page offset %d; earliest retained %d; next %d%s]\n%s"
    page.offset page.first_offset page.next_offset
    (if page.truncated then "; earlier output was truncated" else "")
    page.output

let process_wait ?cancel ?context _root args =
  let manager = process_manager (require_session_context context) in
  let id = required_string "id" args in
  let timeout = optional_int "timeout_seconds" 10 ~minimum:1 ~maximum:300 args in
  let status = Workspace_process.wait_job manager ~id
    ~timeout_seconds:timeout ?cancel () in
  "Process job " ^ id ^ ": " ^ process_status status

let process_ready ?cancel ?context _root args =
  let manager = process_manager (require_session_context context) in
  let id = required_string "id" args in
  let regex = optional_string "log_regex" "" args in
  let regex = if regex = "" then None else Some regex in
  Option.iter validate_regex regex;
  let port = optional_int "port" 0 ~minimum:1 ~maximum:65_535 args in
  let port = if port = 0 then None else Some port in
  if regex = None && port = None then fail "readiness requires log_regex or port";
  let timeout_seconds = optional_int "timeout_seconds" 10 ~minimum:1 ~maximum:300 args in
  let ready = Workspace_process.wait_ready manager ~id ~timeout_seconds
    ?cancel ?log_regex:regex ?port () in
  if ready then Printf.sprintf "Process job %s is ready." id
  else Printf.sprintf "Process job %s is not ready." id

let process_stdin ~approved ?context _root args =
  require_explicit_approval approved;
  let manager = process_manager (require_session_context context) in
  let id = required_string "id" args in
  let data = required_string "data" args in
  Workspace_process.write_stdin manager ~id ~data;
  Printf.sprintf "Wrote %d bytes to process job %s." (String.length data) id

let process_close_stdin ~approved ?context _root args =
  require_explicit_approval approved;
  let manager = process_manager (require_session_context context) in
  let id = required_string "id" args in
  Workspace_process.close_stdin manager ~id;
  Printf.sprintf "Closed stdin for process job %s." id

let process_kill ~approved ?context _root args =
  require_explicit_approval approved;
  let manager = process_manager (require_session_context context) in
  let id = required_string "id" args in
  Workspace_process.kill_job manager ~id;
  Printf.sprintf "Process job %s: %s"
    id (process_status (Workspace_process.job_status manager ~id))

let git_result_text label (result : Workspace_git.process_result) =
  if result.Workspace_process.termination = Workspace_process.Cancelled then
    raise Cancelled;
  let status = match result.Workspace_process.termination with
    | Workspace_process.Exited code -> Printf.sprintf "exit %d" code
    | Workspace_process.Signaled signal -> Printf.sprintf "signal %d" signal
    | Workspace_process.Timed_out -> "timed out"
    | Workspace_process.Cancelled -> assert false in
  Printf.sprintf "%s · %s%s\n%s" label status
    (if result.truncated then " · output truncated" else "")
    result.output

let owned_worktrees ?cancel ?context ~root () =
  let context = require_session_context context in
  Workspace_git.list_worktrees ?cancel ~base:root ~owner:context.owner ()

let worktree_list ?cancel ?context root _args =
  match owned_worktrees ?cancel ?context ~root () with
  | [] -> "No managed worktrees belong to this session."
  | worktrees ->
      String.concat "\n" (List.map (fun (item : Workspace_git.managed_worktree) ->
        Printf.sprintf "%s · %s · %s" item.id item.branch item.path) worktrees)

let worktree_status ?cancel ?context root args =
  let context = require_session_context context in
  let id = required_string "id" args in
  git_result_text ("Worktree " ^ id ^ " status")
    (Workspace_git.status ?cancel ~base:root ~owner:context.owner ~id ())

let worktree_diff ?cancel ?context root args =
  let context = require_session_context context in
  let id = required_string "id" args in
  git_result_text ("Worktree " ^ id ^ " diff")
    (Workspace_git.diff ?cancel ~base:root ~owner:context.owner ~id ())

let worktree_history ?cancel ?context root args =
  let context = require_session_context context in
  let id = required_string "id" args in
  let count = optional_int "count" 20 ~minimum:1 ~maximum:100 args in
  let result = Workspace_git.history ?cancel ~base:root ~owner:context.owner ~id ~count () in
  if result.truncated then fail "Git history exceeded the output limit";
  match result.Workspace_process.termination with
  | Workspace_process.Exited 0 when String.trim result.output = "" ->
      "No commits in this worktree."
  | Workspace_process.Exited 0 ->
      Printf.sprintf "Worktree %s recent commits:\n%s" id result.output
  | _ -> git_result_text ("Worktree " ^ id ^ " history") result

let worktree_create ~approved ?cancel ?context root args =
  let context = require_session_context context in
  let id = required_string "id" args in
  let path = required_string "path" args in
  let branch = required_string "branch" args in
  let result = Workspace_git.create_worktree ?cancel ~base:root ~path
    ~branch ~id ~owner:context.owner ~approved () in
  git_result_text (Printf.sprintf "Worktree %s at %s on branch %s" id path branch) result

let worktree_commit ~approved ?cancel ?context root args =
  let context = require_session_context context in
  let id = required_string "id" args in
  let paths = string_list "paths" args in
  let message = required_string "message" args in
  let result = Workspace_git.commit ?cancel ~base:root ~owner:context.owner
    ~id ~approved ~paths ~message () in
  let commit_id = Option.fold ~none:"" ~some:(fun hash -> "\nCommit: " ^ hash)
    result.commit_id in
  let files = if result.files = [] then "" else
    "\nCommitted paths:\n" ^ String.concat "\n" result.files in
  git_result_text ("Worktree " ^ id ^ " commit") result.process ^ commit_id ^ files

let worktree_remove ~approved ?cancel ?context root args =
  require_explicit_approval approved;
  let context = require_session_context context in
  let id = required_string "id" args in
  git_result_text ("Worktree " ^ id ^ " removal")
    (Workspace_git.remove_worktree ?cancel ~base:root
      ~owner:context.owner ~id ())

let schema name description properties required =
  `Assoc ["type", `String "function";
          "function", `Assoc ["name", `String name; "description", `String description;
                              "parameters", `Assoc ["type", `String "object";
                                                    "properties", `Assoc properties;
                                                    "required", `List (List.map (fun s -> `String s) required);
                                                    "additionalProperties", `Bool false]]]

let string_field description = `Assoc ["type", `String "string"; "description", `String description]
let bounded_string_field description maximum =
  `Assoc ["type", `String "string"; "description", `String description;
          "maxLength", `Int maximum]
let integer_field description minimum maximum =
  `Assoc ["type", `String "integer"; "description", `String description;
          "minimum", `Int minimum; "maximum", `Int maximum]
let boolean_field description = `Assoc ["type", `String "boolean"; "description", `String description]
let enum_string_field description values =
  `Assoc ["type", `String "string"; "description", `String description;
          "enum", `List (List.map (fun value -> `String value) values)]

let replacement_hunk_field =
  `Assoc ["type", `String "object";
          "properties", `Assoc [
            "old_text", string_field "Exact original text; must occur once";
            "new_text", string_field "Replacement text";
          ];
          "required", `List [`String "old_text"; `String "new_text"];
          "additionalProperties", `Bool false]

let replacement_hunks_field =
  `Assoc ["type", `String "array";
          "description", `String "Non-overlapping exact-text replacements against one source snapshot";
          "items", replacement_hunk_field]

let string_array_field description =
  `Assoc ["type", `String "array"; "description", `String description;
          "items", `Assoc ["type", `String "string"]]

let object_field properties required =
  `Assoc ["type", `String "object";
          "properties", `Assoc properties;
          "required", `List (List.map (fun name -> `String name) required);
          "additionalProperties", `Bool false]

let lsp_position_field =
  object_field [
    "line", integer_field "Zero-based document line" 0 max_int;
    "character", integer_field "Zero-based UTF-16 character offset" 0 max_int
  ] ["line"; "character"]

let lsp_range_field =
  object_field ["start", lsp_position_field; "end", lsp_position_field]
    ["start"; "end"]

let dap_breakpoint_field =
  object_field [
    "line", integer_field "One-based source line" 1 max_int;
    "condition", bounded_string_field "Optional breakpoint condition" 1024;
    "hit_condition", bounded_string_field "Optional hit condition" 1024;
    "log_message", bounded_string_field "Optional logpoint message" 1024
  ] ["line"]

let dap_breakpoints_field =
  `Assoc ["type", `String "array";
          "items", dap_breakpoint_field;
          "description", `String "Source breakpoints (maximum 128)"]

let environment_field =
  `Assoc ["type", `String "object";
          "description", `String "Child-only environment overrides";
          "additionalProperties", string_field "Environment variable value"]

let web_search ~approved ?cancel args =
  require_explicit_approval approved;
  let query = required_string "query" args in
  let page = optional_int "page" 0 ~minimum:0 ~maximum:Web_search.max_page args in
  let count = optional_int "count" 5 ~minimum:1 ~maximum:Web_search.max_results args in
  let response = Web_search.search ?cancel ~page ~count ~query () in
  let result result =
    `Assoc [
      "title", `String result.Web_search.title;
      "url", `String result.url;
      "snippet", `String result.snippet;
      "provider", `String result.provider;
      "citation", `String result.citation
    ] in
  Yojson.Basic.to_string (`Assoc ([
    "provider", `String response.provider;
    "query", `String response.query;
    "page", `Int response.page;
    "citations", `List (List.map (fun citation -> `String citation) response.citations);
    "results", `List (List.map result response.results)
  ] @ (if response.failed = [] then [] else [
    "fallback_from", `List (List.map (fun (provider, error) ->
      `Assoc ["provider", `String provider; "error", `String error]) response.failed)])))

let web_fetch ~approved ?cancel args =
  require_explicit_approval approved;
  let url = required_string "url" args in
  let max_bytes = optional_int "max_bytes" Web_search.max_content_bytes
    ~minimum:1 ~maximum:Web_search.max_content_bytes args in
  let page = Web_search.fetch_url ?cancel ~max_bytes url () in
  Yojson.Basic.to_string (`Assoc [
    "source_url", `String page.source_url;
    "markdown", `String page.markdown;
    "truncated", `Bool page.truncated
  ])
let image_ocr ~approved ?cancel root args =
  require_explicit_approval approved;
  let path = required_string "path" args in
  let mime = required_string "mime" args in
  let absolute = Workspace_path.checked_path root path in
  let image = Workspace_path.read_bounded absolute Native_services.max_image_bytes in
  match Native_services.recognize ?cancel ~mime image with
  | Native_services.Available text ->
      Yojson.Basic.to_string (`Assoc [
        "status", `String "available";
        "text", `String text
      ])
  | Native_services.Unavailable message ->
      Yojson.Basic.to_string (`Assoc [
        "status", `String "unavailable";
        "message", `String message
      ])

let clipboard_read ~approved ?cancel () =
  require_explicit_approval approved;
  match Native_services.clipboard ?cancel () with
  | Native_services.Available text ->
      Yojson.Basic.to_string (`Assoc [
        "status", `String "available";
        "text", `String text
      ])
  | Native_services.Unavailable message ->
      Yojson.Basic.to_string (`Assoc [
        "status", `String "unavailable";
        "message", `String message
      ])

let clipboard_write ~approved ?cancel args =
  require_explicit_approval approved;
  let text = required_string "text" args in
  match Native_services.clipboard_write ?cancel text with
  | Native_services.Available () ->
      Yojson.Basic.to_string (`Assoc ["status", `String "available"])
  | Native_services.Unavailable message ->
      Yojson.Basic.to_string (`Assoc [
        "status", `String "unavailable";
        "message", `String message
      ])

let browser_tool ~approved ?cancel ?context args =
  let context = require_session_context context in
  check_session_context context;
  let action = required_string "action" args in
  let id = match action with
    | "open" -> optional_string "id" "browser" args
    | _ -> required_string "id" args in
  let manager = context.browser_manager in
  let timeout_seconds = float_of_int
    (optional_int "timeout_seconds" 30 ~minimum:1
      ~maximum:(int_of_float Workspace_browser.max_operation_seconds) args) in
  let json = match action with
    | "open" ->
        require_explicit_approval approved;
        ignore (Workspace_browser.open_session ?cancel manager ~id);
        `Assoc ["status", `String "open"; "id", `String id]
    | "navigate" ->
        require_explicit_approval approved;
        Workspace_browser.navigate ?cancel manager ~id
          ~url:(required_string "url" args) ~timeout_seconds
    | "evaluate" ->
        require_explicit_approval approved;
        Workspace_browser.evaluate ?cancel manager ~id
          ~expression:(required_string "expression" args) ~timeout_seconds
    | "observe" -> Workspace_browser.observe ?cancel manager ~id
    | "screenshot" ->
        require_explicit_approval approved;
        Workspace_browser.screenshot ?cancel manager ~id
    | "list_tools" ->
        Workspace_browser.list_tools ?cancel manager ~id
          ?name:(match Protocol.member "name" args with
            | `String value -> Some value | _ -> None)
          ?frame:(match Protocol.member "frame" args with
            | `String value -> Some value | _ -> None)
    | "tool_events" ->
        if optional_bool "clear" false args then
          require_explicit_approval approved;
        Workspace_browser.tool_events ?cancel manager ~id
          ?since:(match Protocol.member "since" args with
            | `Int value -> Some value | _ -> None)
          ?clear:(match Protocol.member "clear" args with
            | `Bool value -> Some value | _ -> None)
    | "call_tool" ->
        require_explicit_approval approved;
        Workspace_browser.call_tool ?cancel manager ~id
          ?frame:(match Protocol.member "frame" args with
            | `String value -> Some value | _ -> None)
          ~name:(required_string "name" args)
          ~arguments:(match Protocol.member "arguments" args with
            | `Null -> `Assoc [] | value -> value)
          ~timeout_seconds
    | "close" ->
        Workspace_browser.close_session manager ~id;
        `Assoc ["status", `String "closed"; "id", `String id]
    | _ -> fail "unsupported browser action" in
  match action with
  | "screenshot" ->
      let mime_type = match Protocol.member "mime_type" json with
        | `String value -> value | _ -> "image/png" in
      let data = match Protocol.member "data" json with
        | `String value -> value | _ -> "" in
      [Protocol.Text (Yojson.Basic.to_string (`Assoc [
        "session", Protocol.member "session" json;
        "mime_type", `String mime_type;
        "bytes", Protocol.member "bytes" json ]));
       Protocol.Image { mime_type; data }]
  | _ -> [Protocol.Text (Yojson.Basic.to_string json)]
let publish_web_tool ~approved ?cancel ?context args =
  let context = require_session_context context in
  check_session_context context;
  let action = required_string "action" args in
  let manager = context.process_manager in
  let relay = match optional_string "relay" "" args with
    | "" -> None | value -> Some (Workspace_portal.https_origin value) in
  let tunnel_id = Workspace_portal.job_id in
  let json = match action with
    | "publish" ->
        require_explicit_approval approved;
        let port = optional_int "port" 0 ~minimum:1 ~maximum:65535 args in
        if port = 0 then fail "publish requires a loopback port";
        let name = match optional_string "name" "" args with
          | "" -> Workspace_portal.fresh_name ()
          | value -> Workspace_portal.publish_name value in
        Workspace_portal.publish ?cancel manager ~id:(tunnel_id name)
          ?relay ~port ~name
    | "stop" ->
        require_explicit_approval approved;
        let name = match optional_string "name" "" args with
          | "" -> fail "stop requires a tunnel name"
          | value -> Workspace_portal.publish_name value in
        Workspace_portal.stop manager ~id:(tunnel_id name)
    | "list" -> Workspace_portal.list manager
    | "attach" ->
        require_explicit_approval approved;
        (match context.hub_port with
         | None -> fail "attach requires an interactive session hub"
         | Some resolve ->
             let port = try resolve () with
               | exn -> fail (Printexc.to_string exn) in
             let name = match optional_string "name" "" args with
               | "" -> Workspace_portal.fresh_name ()
               | value -> Workspace_portal.publish_name value in
             let json = Workspace_portal.publish ?cancel manager
               ~id:(tunnel_id name) ?relay ~port ~name in
             (match json with
              | `Assoc fields -> `Assoc (("attach", `Bool true) :: fields)
              | other -> other))
    | _ -> fail "unsupported publish_web action" in
  [Protocol.Text (Yojson.Basic.to_string json)]



let memory_tool ~approved ~root args =
  let action = required_string "action" args in
  match action with
  | "list" ->
      let memory, issues = Project_memory.scan ~root in
      `Assoc [
        "entries", `List (List.map (fun (item : Project_memory.entry) ->
          `Assoc ["name", `String item.name;
                  "bytes", `Int item.bytes;
                  "summary", `String item.summary])
          (Project_memory.entries memory));
        "diagnostics", `List (List.map (fun (code, message) ->
          `Assoc ["code", `String code; "message", `String message]) issues)]
  | "get" ->
      let name = required_string "name" args in
      let memory, _ = Project_memory.scan ~root in
      (match Project_memory.get memory ~name with
       | Ok text -> `Assoc ["name", `String name; "text", `String text]
       | Error message -> fail message)
  | "put" ->
      require_explicit_approval approved;
      let name = required_string "name" args in
      let text = required_string "text" args in
      let path = Workspace_memory.put ~root ~name text in
      `Assoc ["status", `String "saved"; "path", `String path]
  | "forget" ->
      require_explicit_approval approved;
      let name = required_string "name" args in
      if Workspace_memory.forget ~root ~name then
        `Assoc ["status", `String "forgotten"; "name", `String name]
      else `Assoc ["status", `String "absent"; "name", `String name]
  | _ -> fail "unsupported memory action (list|get|put|forget)"

let lsp_start ~approved ?context root args =
  require_explicit_approval approved;
  let context = require_session_context context in
  check_session_context context;
  let program = required_string "program" args in
  let arguments = string_list "arguments" args in
  try Workspace_lsp.start context.lsp_manager ~owner:context.owner ~root
    ~program ~args:arguments ~execution_approved:true;
    Yojson.Basic.to_string (`Assoc ["status", `String "started"])
  with Workspace_lsp.Error message -> fail message

let lsp_preview ?context ~root args =
  let context = require_session_context context in
  check_session_context context;
  let program = required_string "program" args in
  let arguments = string_list "arguments" args in
  let preview_id = required_string "preview_id" args in
  try
    Workspace_lsp.preview_details context.lsp_manager ~owner:context.owner ~root
      ~program ~arguments preview_id
  with Workspace_lsp.Error message -> fail message

let lsp_preview_paths ?context ~root args =
  let _, files = lsp_preview ?context ~root args in
  Workspace_lsp.preview_paths files

let lsp_execute ~approved ?cancel ?context root args =
  let context = require_session_context context in
  check_session_context context;
  let program = required_string "program" args in
  let arguments = string_list "arguments" args in
  let apply_requested = optional_string "action" "" args = "apply_preview" in
  if apply_requested then require_explicit_approval approved;
  let apply_approved = apply_requested && approved in
  try
    Workspace_lsp.execute context.lsp_manager ~owner:context.owner ~root
      ~program ~args:arguments ?cancel ~apply_approved
      ~on_file_change:context.record_file_change args
    |> Yojson.Basic.to_string
  with Workspace_lsp.Error message -> fail message

let ssh_session context id =
  check_session_context context;
  Mutex.lock context.ssh_lock;
  let session = Hashtbl.find_opt context.ssh_sessions id in
  Mutex.unlock context.ssh_lock;
  match session with
  | Some session -> session
  | None -> fail "unknown SSH session"

let ssh_open ~approved ?cancel ?context args =
  require_explicit_approval approved;
  let context = require_session_context context in
  check_session_context context;
  let id = required_string "id" args in
  Workspace_process.validate_id id;
  let home = Option.value ~default:"" (Sys.getenv_opt "HOME") in
  if home = "" then fail "HOME is unavailable; configure an owned known_hosts file";
  let endpoint = {
    Workspace_ssh.host = required_string "host" args;
    user = required_string "user" args;
    remote_root = required_string "remote_root" args;
    known_hosts = optional_string "known_hosts"
      (Filename.concat (Filename.concat home ".ssh") "known_hosts") args
  } in
  Mutex.lock context.ssh_lock;
  let already_open = Hashtbl.mem context.ssh_sessions id in
  Mutex.unlock context.ssh_lock;
  if already_open then fail "SSH session id is already open";
  let session =
    try Workspace_ssh.open_session ?cancel ~owner:context.owner ~endpoint
      ~host_trusted:true ~network_approved:true ()
    with Workspace_ssh.Error message -> fail message in
  Mutex.lock context.ssh_lock;
  Hashtbl.add context.ssh_sessions id session;
  Mutex.unlock context.ssh_lock;
  Yojson.Basic.to_string (`Assoc [
    "status", `String "connected";
    "id", `String id;
    "host", `String endpoint.host;
    "user", `String endpoint.user;
    "remote_root", `String endpoint.remote_root;
    "authentication", `String "system SSH identity; no Pave OAuth/API credential"
  ])

let ssh_close ?context args =
  let context = require_session_context context in
  check_session_context context;
  let id = required_string "id" args in
  Mutex.lock context.ssh_lock;
  let session = Hashtbl.find_opt context.ssh_sessions id in
  Option.iter (fun _ -> Hashtbl.remove context.ssh_sessions id) session;
  Mutex.unlock context.ssh_lock;
  (match session with
   | None -> fail "unknown SSH session"
   | Some session ->
       (try Workspace_ssh.close_session ~owner:context.owner session
        with Workspace_ssh.Error message -> fail message));
  Yojson.Basic.to_string (`Assoc ["status", `String "closed"; "id", `String id])

let ssh_read ~approved ?cancel ?context args =
  require_explicit_approval approved;
  let context = require_session_context context in
  let session = ssh_session context (required_string "id" args) in
  let path = required_string "path" args in
  try
    let contents = Workspace_ssh.read_file ?cancel ~owner:context.owner
      ~read_approved:true ~network_approved:true session ~path () in
    Yojson.Basic.to_string (`Assoc ["path", `String path; "contents", `String contents])
  with Workspace_ssh.Error message -> fail message

let ssh_write ~approved ?cancel ?context args =
  require_explicit_approval approved;
  let context = require_session_context context in
  let session = ssh_session context (required_string "id" args) in
  let path = required_string "path" args in
  let contents = required_string "contents" args in
  try
    let bytes = Workspace_ssh.write_file ?cancel ~owner:context.owner
      ~network_approved:true ~write_approved:true session ~path ~contents () in
    Yojson.Basic.to_string (`Assoc [
      "path", `String path; "bytes_written", `Int bytes
    ])
  with Workspace_ssh.Error message -> fail message

let ssh_command ~approved ?cancel ?context args =
  require_explicit_approval approved;
  let context = require_session_context context in
  let session = ssh_session context (required_string "id" args) in
  let program = required_string "program" args in
  let arguments = string_list "arguments" args in
  let timeout = optional_int "timeout_seconds"
    Workspace_ssh.default_timeout_seconds ~minimum:1
    ~maximum:Workspace_ssh.max_timeout_seconds args in
  try
    let output = Workspace_ssh.run_command ?cancel ~timeout_seconds:timeout
      ~owner:context.owner ~network_approved:true ~execution_approved:true
      session ~program ~arguments () in
    Yojson.Basic.to_string (`Assoc ["output", `String output])
  with Workspace_ssh.Error message -> fail message

let workspace_snapshot ?cancel ?context root args =
  let root, args = resolve_path_arguments ?cancel ?context ~root args in
  let path = required_string "path" args in
  let limit = optional_int "max_bytes" 16_384 ~minimum:1
    ~maximum:(max_read_bytes - 512) args in
  let snapshot = Workspace_edit.read_snapshot ~root ~path in
  let size = String.length snapshot.contents in
  let length = min limit size in
  let content = String.sub snapshot.contents 0 length in
  if String.contains content '\000' then fail "binary file; workspace snapshots support text only";
  Printf.sprintf
    "SHA-256: %s\n%s\n[page: offset 0; bytes: %d; file size: %d; next offset: %d; %s]"
    snapshot.sha256 content length size length
    (if length < size then
       "truncated; continue with read_file offset/line, and apply this hash only if unchanged"
     else "end of file")

let workspace_eval_bridge ?cancel ?context ~root ~deadline name arguments =
  let bounded_job_timeout arguments =
    let requested = optional_int "timeout_seconds" 10
      ~minimum:1 ~maximum:300 arguments in
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0. then fail "workspace evaluation timed out";
    let timeout = min requested (max 1 (int_of_float (ceil remaining))) in
    match arguments with
    | `Assoc fields ->
        `Assoc (("timeout_seconds", `Int timeout) ::
          List.remove_assoc "timeout_seconds" fields)
    | _ -> fail "workspace tool arguments must be an object" in
  match name with
  | "read_file" ->
      let path = required_string "path" arguments in
      let lower = String.lowercase_ascii path in
      if not (Filename.is_relative path) ||
         Workspace_reader.is_scheme_uri lower ||
         starts_with lower "worktree://" then
        fail "workspace evaluation bridge only reads workspace-relative files";
      read_file ?cancel ?context root arguments
  | "workspace_snapshot" -> workspace_snapshot ?cancel ?context root arguments
  | "fuzzy_file_search" -> fuzzy_file_search ?cancel root arguments
  | "list_files" -> list_files root arguments
  | "search" -> search root arguments
  | "glob" -> glob root arguments
  | "grep" -> grep root arguments
  | "mobile_project" -> mobile_project ?cancel root arguments
  | "process_list" -> process_list ?context root arguments
  | "process_output" -> process_output ?context root arguments
  | "process_wait" ->
      process_wait ?cancel ?context root (bounded_job_timeout arguments)
  | "process_ready" ->
      process_ready ?cancel ?context root (bounded_job_timeout arguments)
  | _ -> fail "workspace evaluation bridge tool is not allowlisted"

let workspace_eval ~approved ?cancel ?context root args =
  require_explicit_approval approved;
  let context = require_session_context context in
  check_session_context context;
  let language = match required_string "language" args with
    | "python" -> Workspace_eval.Python
    | "javascript" -> Workspace_eval.JavaScript
    | _ -> fail "language must be python or javascript" in
  let action = optional_string "action" "run" args in
  if action <> "run" && action <> "reset" then
    fail "action must be run or reset";
  let timeout_seconds = optional_int "timeout_seconds" 10
    ~minimum:1 ~maximum:Workspace_eval.max_timeout_seconds args in
  let source = if action = "run" then Some (required_string "code" args) else None in
  let kernel, created =
    Mutex.lock context.eval_lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock context.eval_lock) (fun () ->
      check_session_context context;
      let current = match language with
        | Workspace_eval.Python -> context.python_kernel
        | Workspace_eval.JavaScript -> context.javascript_kernel in
      match current with
      | Some kernel -> kernel, false
      | None ->
          let kernel = Workspace_eval.create ~owner:context.owner language in
          (match language with
           | Workspace_eval.Python -> context.python_kernel <- Some kernel
           | Workspace_eval.JavaScript -> context.javascript_kernel <- Some kernel);
          kernel, true) in
  try
    match action with
    | "reset" ->
        if not created then Workspace_eval.reset kernel;
        Yojson.Basic.to_string (`Assoc ["status", `String "reset"])
    | "run" ->
        let deadline = Unix.gettimeofday () +. float_of_int timeout_seconds in
        let cancelled () =
          Option.fold ~none:false ~some:(fun cancel -> cancel ()) cancel ||
          Unix.gettimeofday () >= deadline in
        let bridge ~name ~arguments =
          workspace_eval_bridge ~cancel:cancelled ~context ~root ~deadline
            name arguments in
        let result = Workspace_eval.evaluate ~cancel:cancelled ~timeout_seconds
          ~tool_bridge:bridge kernel (Option.get source) in
        Yojson.Basic.to_string (`Assoc [
          "output", `String result.output;
          "error", (match result.error with None -> `Null | Some text -> `String text);
          "truncated", `Bool result.truncated
        ])
    | _ -> assert false
  with Workspace_eval.Error message -> fail message


let with_dap_effect context ~approved authorization action =
  require_explicit_approval approved;
  context.dap_granted_effect := Some authorization;
  Fun.protect
    ~finally:(fun () -> context.dap_granted_effect := None)
    action

let dap_start ~approved ?cancel ?context root args =
  let context = require_session_context context in
  check_session_context context;
  let id = required_string "id" args in
  Workspace_process.validate_id id;
  let program = required_string "program" args in
  let arguments = string_list "arguments" args in
  let transport =
    with_dap_effect context ~approved Workspace_dap.Adapter_process (fun () ->
      Workspace_dap.stdio_transport ?cancel context.dap_manager
        ~program ~arguments ~cwd:root) in
  (try ignore (Workspace_dap.create_session context.dap_manager ~id ~transport)
   with exn -> (try transport.close () with _ -> ()); raise exn);
  Yojson.Basic.to_string (`Assoc [
    "status", `String "adapter started";
    "id", `String id
  ])

let dap_required_int name args =
  match field name args with
  | `Int value -> value
  | `Null -> fail ("missing required integer argument: " ^ name)
  | _ -> fail (name ^ " must be an integer")

let dap_optional_string name args =
  match field name args with
  | `Null -> None
  | `String value -> Some value
  | _ -> fail (name ^ " must be a string")

let dap_breakpoints args =
  match field "breakpoints" args with
  | `List rows ->
      List.map (fun row ->
        let line = match field "line" row with
          | `Int line -> line
          | _ -> fail "each breakpoint requires an integer line" in
        { Workspace_dap.line;
          condition = dap_optional_string "condition" row;
          hit_condition = dap_optional_string "hit_condition" row;
          log_message = dap_optional_string "log_message" row })
        rows
  | _ -> fail "breakpoints must be an array"

let dap_execute ~approved ?cancel ?context _root args =
  let context = require_session_context context in
  check_session_context context;
  let id = required_string "id" args in
  let action = required_string "action" args in
  let manager = context.dap_manager in
  let with_effect authorization call =
    with_dap_effect context ~approved authorization call in
  let output = match action with
    | "initialize" ->
        Workspace_dap.initialize ?cancel manager ~id
          ~adapter_id:(required_string "adapter_id" args)
    | "launch" ->
        with_effect Workspace_dap.Launch (fun () ->
          Workspace_dap.launch ?cancel manager ~id
            ~target:(required_string "target" args)
            ~arguments:(string_list "arguments" args))
    | "trust_host" ->
        let host = Workspace_dap.normalize_host (required_string "host" args) in
        with_effect (Workspace_dap.Remote_host host) (fun () ->
          Workspace_dap.trust_attach_host manager ~id ~host);
        `Assoc ["status", `String "remote host trusted"; "host", `String host]
    | "attach" ->
        let host = Workspace_dap.normalize_host (required_string "host" args) in
        with_effect (Workspace_dap.Remote_host host) (fun () ->
          Workspace_dap.attach ?cancel manager ~id ~host
            ~port:(dap_required_int "port" args))
    | "configuration_done" ->
        with_effect Workspace_dap.Debug_execution (fun () ->
          Workspace_dap.configuration_done ?cancel manager ~id)
    | "set_breakpoints" ->
        with_effect Workspace_dap.Breakpoints (fun () ->
          Workspace_dap.set_breakpoints ?cancel manager ~id
            ~source:(required_string "source" args)
            ~breakpoints:(dap_breakpoints args))
    | "threads" -> Workspace_dap.threads ?cancel manager ~id
    | "stack_trace" ->
        Workspace_dap.stack_trace ?cancel
          ~start_frame:(optional_int "start_frame" 0 ~minimum:0 ~maximum:1_000_000 args)
          ~levels:(optional_int "levels" 50 ~minimum:1 ~maximum:100 args)
          manager ~id ~thread_id:(dap_required_int "thread_id" args) ()
    | "scopes" ->
        Workspace_dap.scopes ?cancel manager ~id
          ~frame_id:(dap_required_int "frame_id" args)
    | "variables" ->
        Workspace_dap.variables ?cancel
          ?filter:(dap_optional_string "filter" args)
          ~start:(optional_int "start" 0 ~minimum:0 ~maximum:1_000_000 args)
          ~count:(optional_int "count" 100 ~minimum:1 ~maximum:1000 args)
          manager ~id ~reference:(dap_required_int "reference" args) ()
    | "continue" | "next" | "step_in" | "step_out" ->
        with_effect Workspace_dap.Debug_execution (fun () ->
          let thread_id = dap_required_int "thread_id" args in
          let single_thread = optional_bool "single_thread" false args in
          match action with
          | "continue" ->
              Workspace_dap.continue_ ?cancel ~single_thread manager ~id ~thread_id ()
          | "next" ->
              Workspace_dap.next ?cancel ~single_thread manager ~id ~thread_id ()
          | "step_in" ->
              Workspace_dap.step_in ?cancel ~single_thread manager ~id ~thread_id ()
          | _ ->
              Workspace_dap.step_out ?cancel ~single_thread manager ~id ~thread_id ())
    | "evaluate" ->
        with_effect Workspace_dap.Evaluate (fun () ->
          let frame_id = match field "frame_id" args with
            | `Null -> None
            | `Int value -> Some value
            | _ -> fail "frame_id must be an integer" in
          Workspace_dap.evaluate ?cancel
            ~context:(optional_string "context" "watch" args)
            manager ~id ~expression:(required_string "expression" args) ~frame_id)
    | "disconnect" ->
        let terminate_debuggee = optional_bool "terminate_debuggee" false args in
        if terminate_debuggee then
          with_effect Workspace_dap.Debug_execution (fun () ->
            Workspace_dap.disconnect ?cancel ~terminate_debuggee manager ~id)
        else Workspace_dap.disconnect ?cancel manager ~id
    | "events" ->
        `List (Workspace_dap.take_events manager ~id)
    | "close" ->
        Workspace_dap.close_session manager ~id;
        `Assoc ["status", `String "closed"; "id", `String id]
    | _ -> fail "unsupported DAP action" in
  Yojson.Basic.to_string output

let token_count args =
  let encoding = required_string "encoding" args in
  let text = required_string "text" args in
  let result = Native_tokenizer.count_tokens ~encoding text in
  Yojson.Basic.to_string (`Assoc [
    "encoding", `String result.encoding;
    "token_count", `Int result.token_count;
    "input_bytes", `Int (String.length text)
  ])

let repository_security_scan ?cancel root args =
  let file_limit = optional_int "file_limit"
    Repository_security.max_files ~minimum:0
    ~maximum:Repository_security.max_files args in
  let finding_limit = optional_int "finding_limit" 100 ~minimum:0
    ~maximum:100 args in
  let format = match field "format" args with
    | `Null -> "summary"
    | `String value -> value
    | _ -> fail "format must be a string" in
  let result =
    try Repository_security.scan ?cancel ~file_limit ~finding_limit ~root ()
    with Repository_security.Error message -> fail message in
  match format with
  | "sarif" -> Repository_security.sarif ~root result
  | "summary" ->
      let findings = List.filter
        (Repository_security.validate_finding ~root) result.findings in
      let finding finding =
        `Assoc [
          "rule_id", `String finding.Repository_security.rule_id;
          "path", `String finding.path;
          "start_line", `Int finding.start_line;
          "start_column", `Int finding.start_column;
          "end_line", `Int finding.end_line;
          "end_column", `Int finding.end_column;
          "message", `String finding.message;
          "provenance", `String "deterministic-rule";
          "validated", `Bool true
        ] in
      Yojson.Basic.to_string (`Assoc [
        "files_scanned", `Int result.files_scanned;
        "truncated", `Bool (result.truncated ||
          List.length findings <> List.length result.findings);
        "findings", `List (List.map finding findings)
      ])
  | _ -> fail "format must be summary or sarif"

let repository_security_scan ?cancel root args =
  scanning (fun () -> repository_security_scan ?cancel root args)

let is_shell_tool = function
  | "run_command" | "start_process" | "start_shell" | "xcode_preflight"
  | "mobile_check" | "android_devices" -> true
  | _ -> false

let requires_explicit_approval ~name ~args =
  match name with
  | "run_command" | "start_process" | "start_shell" | "xcode_preflight"
  | "mobile_check" | "android_devices"
  | "process_stdin" | "process_close_stdin" | "process_kill"
  | "worktree_create" | "worktree_commit" | "worktree_remove"
  | "web_search" | "web_fetch" | "image_ocr"
  | "clipboard_read" | "clipboard_write"
  | "lsp_start" | "workspace_eval"
  | "ssh_open" | "ssh_read" | "ssh_write" | "ssh_command"
  | "dap_start" -> true
  | "lsp" -> optional_string "action" "" args = "apply_preview"
  | "dap" ->
      (match field "action" args with
       | `String ("launch" | "trust_host" | "attach" | "configuration_done"
         | "set_breakpoints" | "continue" | "next" | "step_in" | "step_out"
         | "evaluate") -> true
       | `String "disconnect" -> optional_bool "terminate_debuggee" false args
       | _ -> false)
  | "browser" ->
      (match field "action" args with
       | `String ("open" | "navigate" | "evaluate" | "screenshot" | "call_tool") -> true
       | `String "tool_events" -> optional_bool "clear" false args
       | _ -> false)
  | "publish_web" ->
      (match field "action" args with
       | `String ("publish" | "attach" | "stop") -> true
       | _ -> false)
  | "read_file" ->
      (match field "path" args with
       | `String path -> starts_with (String.lowercase_ascii path) "https://"
       | _ -> false)
  | _ -> false

let non_reversible_tool ~name ~args =
  match name with
  | "run_command" | "start_process" | "start_shell" | "xcode_preflight"
  | "mobile_check" | "android_devices"
  | "process_stdin" | "process_close_stdin" | "process_kill"
  | "worktree_create" | "worktree_commit" | "worktree_remove"
  | "lsp_start" | "dap_start" | "ssh_open" | "ssh_read"
  | "ssh_write" | "ssh_command" | "web_search" | "web_fetch"
  | "clipboard_write" -> true
  | "workspace_eval" -> optional_string "action" "run" args = "run"
  | "dap" ->
      (match field "action" args with
       | `String ("launch" | "attach" | "configuration_done"
         | "set_breakpoints" | "continue" | "next" | "step_in" | "step_out"
         | "evaluate") -> true
       | `String "disconnect" -> optional_bool "terminate_debuggee" false args
       | _ -> false)
  | "browser" ->
      (match field "action" args with
       | `String ("open" | "navigate" | "evaluate" | "call_tool") -> true
       | _ -> false)
  | "publish_web" ->
      (match field "action" args with
       | `String ("publish" | "attach" | "stop") -> true
       | _ -> false)
  | _ -> false

let definitions = [
  schema "lsp_start" "Start one private workspace language server with the exact executable and arguments; server startup is NOT SANDBOXED and requires explicit approval."
    ["program", string_field "Absolute language-server executable";
     "arguments", string_array_field "Exact argument vector; no shell parsing"] ["program"];
  schema "lsp" "Use the already approved, session-owned language server for workspace definitions, references, hover, diagnostics, rename previews, and code-action previews. Apply an exact cached workspace-edit preview only after separate explicit approval."
    ["action", enum_string_field "LSP request" [
       "definition"; "references"; "hover"; "diagnostics"; "rename";
       "code_actions"; "apply_preview"; "shutdown"; "cancel"];
     "program", string_field "Exact executable used by lsp_start";
     "arguments", string_array_field "Exact arguments used by lsp_start";
     "path", bounded_string_field "Workspace-relative document path" 4096;
     "language_id", bounded_string_field "Document language identifier" 128;
     "position", lsp_position_field;
     "range", lsp_range_field;
     "new_name", bounded_string_field "Proposed symbol name" 4096;
     "preview_id", bounded_string_field "Exact cached rename/code-action preview to apply" 80;
     "request_id", integer_field "Active request ID to cancel" 1 max_int]
    ["action"; "program"];
  schema "workspace_eval" "Run or reset one persistent per-session Python or JavaScript kernel. Execution is UNSANDBOXED; every call requires explicit approval. pave.tool exposes bounded read-only workspace tools and inspection of existing session-owned process jobs; it cannot start or mutate jobs."
    ["action", enum_string_field "Kernel action (default run)" ["run"; "reset"];
     "language", enum_string_field "Kernel runtime" ["python"; "javascript"];
     "code", bounded_string_field "Exact code to review and execute (maximum 2000 bytes)" 2000;
     "timeout_seconds", integer_field "Evaluation deadline (default 10 seconds)" 1 Workspace_eval.max_timeout_seconds]
    ["language"];
  schema "ssh_open" "Open an owner-bound SSH workspace session using system SSH identities and owned known_hosts; no Pave OAuth/API credential is forwarded. Network access requires explicit approval."
    ["id", string_field "Session-local SSH session ID";
     "host", bounded_string_field "Pinned SSH host name or address" 253;
     "user", bounded_string_field "SSH user name" 128;
     "remote_root", bounded_string_field "Absolute remote workspace root" 4096;
     "known_hosts", bounded_string_field "Owned known_hosts file (default ~/.ssh/known_hosts)" 4096]
    ["id"; "host"; "user"; "remote_root"];
  schema "ssh_close" "Close a private SSH workspace session."
    ["id", string_field "Session-local SSH session ID"] ["id"];
  schema "ssh_read" "Read a bounded workspace-relative file from a pinned SSH host. Each remote read requires explicit approval."
    ["id", string_field "Session-local SSH session ID";
     "path", bounded_string_field "Path under the configured remote root" 4096]
    ["id"; "path"];
  schema "ssh_write" "Atomically write a bounded workspace-relative file to a pinned SSH host. Requires explicit approval of the exact contents."
    ["id", string_field "Session-local SSH session ID";
     "path", bounded_string_field "Path under the configured remote root" 4096;
     "contents", bounded_string_field "Exact file contents (maximum 2000 bytes)" 2000]
    ["id"; "path"; "contents"];
  schema "ssh_command" "Execute one remote program directly without a shell on a pinned SSH host. Requires explicit approval of the exact program and arguments."
    ["id", string_field "Session-local SSH session ID";
     "program", bounded_string_field "Remote executable name or absolute path" 4096;
     "arguments", string_array_field "Exact argument vector";
     "timeout_seconds", integer_field "Remote command deadline" 1 Workspace_ssh.max_timeout_seconds]
    ["id"; "program"];
  schema "dap_start" "Start one private workspace DAP adapter as a direct child process with a minimal environment and bounded stdio framing; requires explicit approval."
    ["id", string_field "Session-local DAP session ID";
     "program", bounded_string_field "Absolute executable path" 4096;
     "arguments", string_array_field "Exact adapter argument vector"]
    ["id"; "program"];
  schema "dap" "Control one private DAP session. Launch/attach, breakpoints, execution, evaluation, and debuggee termination require effect-specific explicit approval; inspection is read-only."
    ["id", string_field "Session-local DAP session ID";
     "action", enum_string_field "DAP operation" [
       "initialize"; "launch"; "trust_host"; "attach"; "configuration_done";
       "set_breakpoints"; "threads"; "stack_trace"; "scopes"; "variables";
       "continue"; "next"; "step_in"; "step_out"; "evaluate";
       "disconnect"; "events"; "close"];
     "adapter_id", bounded_string_field "DAP adapter identifier" 128;
     "target", bounded_string_field "Workspace-relative launch target" 4096;
     "arguments", string_array_field "Exact debuggee or adapter arguments";
     "host", bounded_string_field "Remote debuggee host name or address" 253;
     "port", integer_field "Remote debuggee port" 1 65_535;
     "source", bounded_string_field "Workspace-relative breakpoint source file" 4096;
     "breakpoints", dap_breakpoints_field;
     "thread_id", integer_field "DAP thread identifier" 1 max_int;
     "start_frame", integer_field "First stack frame index" 0 1_000_000;
     "levels", integer_field "Maximum stack frames to return" 1 100;
     "frame_id", integer_field "DAP stack frame identifier" 1 max_int;
     "reference", integer_field "DAP variables reference" 1 max_int;
     "filter", enum_string_field "Optional variable filter" ["named"; "indexed"];
     "start", integer_field "First variable index" 0 1_000_000;
     "count", integer_field "Maximum variables to return" 1 1000;
     "single_thread", boolean_field "Limit execution control to the selected thread";
     "expression", bounded_string_field "Exact watch/hover expression (maximum 2000 bytes)" 2000;
     "context", enum_string_field "DAP evaluation context" ["watch"; "hover"];
     "terminate_debuggee", boolean_field "Terminate the debuggee on disconnect (requires explicit approval)"]
    ["id"; "action"];
  schema "browser" "Drive one session-owned isolated headless browser page over the Chrome DevTools Protocol. open launches a pinned Chromium executable with a fresh throwaway profile and loopback-only debugging; navigate, evaluate, screenshot and call_tool require separate explicit approval; observe, list_tools, tool_events and close are read-only; close releases the session. list_tools reads the page-declared modelContext catalog across all frames (name or frame filters; schemas only for exact-name reads); tool_events returns catalog transitions since a cursor; call_tool invokes one page-declared tool in its owning frame and returns a bounded result or a structured error. Page content and page-declared tools are untrusted."
    ["id", string_field "Session-local browser session ID (default \"browser\" for open)";
     "action", enum_string_field "Browser operation" [
       "open"; "navigate"; "evaluate"; "observe"; "screenshot";
       "list_tools"; "tool_events"; "call_tool"; "close"];
     "url", bounded_string_field "Exact http:// or https:// URL to navigate to" 4096;
     "expression", bounded_string_field "Exact JavaScript expression evaluated in the owned page (maximum 16384 bytes)" 16384;
     "name", bounded_string_field "Exact page-declared tool name" 128;
     "frame", bounded_string_field "Restrict discovery or invocation to one CDP frame id" 128;
     "since", integer_field "tool_events cursor: only transitions newer than this sequence" 0 max_int;
     "clear", boolean_field "tool_events: clear the retained transition log after reading";
     "arguments", `Assoc ["type", `String "object";
       "description", `String "JSON arguments passed to the page-declared tool"];
     "timeout_seconds", integer_field "Per-operation deadline (default 30 seconds)" 1 120]
    ["action"];
  schema "token_count" "Count tokens using exact vendored cl100k_base or o200k_base ranks; does not contact a provider or infer an upstream tokenizer."
    ["encoding", enum_string_field "Exact tiktoken encoding" ["cl100k_base"; "o200k_base"];
     "text", bounded_string_field "Text to count (maximum 1 MiB)" Native_tokenizer.max_input_bytes]
    ["encoding"; "text"];
  schema "mobile_project" "Inventory bounded mobile stacks; choose an exact subroot and ios/android platform before suggesting a focused inert command. Nested framework hosts cannot select a neighboring native build."
    ["subroot", string_field "Exact workspace-relative project root (Xcode bundle path for Xcode); supply with platform";
     "platform", enum_string_field "Chosen host platform" ["ios"; "android"]] [];
  schema "xcode_preflight" "Run one explicitly approved Xcode scheme or destination discovery, compatible Apple simulator inventory, or selected non-signing build/test. Each phase requires separate interactive approval; inventory never boots a simulator or installs an SDK."
    ["action", enum_string_field "One approved phase" ["schemes"; "destinations"; "simulators"; "build"; "test"];
     "subroot", string_field "Exact scanned Xcode .xcworkspace or .xcodeproj bundle";
     "scheme", string_field "Exact scheme returned by approved discovery";
     "destination", string_field "Exact available iOS Simulator ID returned for this scheme";
     "timeout_seconds", integer_field "Per-command deadline (default 120 seconds)" 1 300]
    ["action"; "subroot"];
  schema "mobile_check" "Run one explicitly approved focused SwiftPM test, offline system-Gradle task, Flutter analysis/test, or local React Native/Expo script. Discovery and selected execution require separate approvals; project code runs as your user. No dependency installation or implicit SDK setup."
    ["stack", enum_string_field "Selected mobile stack" ["swiftpm"; "gradle"; "flutter"; "node"];
     "action", enum_string_field "SwiftPM discover/run, Gradle tasks/run, Flutter analyze/test, or Node test/lint" ["discover"; "tasks"; "run"; "analyze"; "test"; "lint"];
     "subroot", string_field "Exact workspace-relative package/settings/project root";
     "target", string_field "Exact discovered Swift test or Gradle task; for Flutter test, exact workspace-relative .dart test file";
     "manager", enum_string_field "Node script runner when multiple lockfiles exist" ["npm"; "pnpm"; "yarn"];
     "timeout_seconds", integer_field "Per-command deadline (default 120 seconds)" 1 300]
    ["stack"; "action"; "subroot"];
  schema "android_devices" "Inventory configured Android AVDs or attached ADB devices with one separately approved command per phase; never boot, install, select a physical serial or run a test."
    ["action", enum_string_field "AVD configuration or ADB transport listing" ["avds"; "devices"];
     "subroot", string_field "Exact workspace-relative Gradle settings directory"]
    ["action"; "subroot"];
  schema "read_file" "Read bounded workspace files, directories, documents, archives, notebooks, SQLite, owned artifacts, managed worktrees, or public HTTPS URLs. HTTPS fetches send no credentials and require approval."
    ["path", string_field "Workspace-relative path or supported local://, artifact://, worktree://, or HTTPS source";
     "offset", integer_field "Byte offset for ordinary local text files (default 0; do not combine with a line other than 1)" 0 max_int;
     "line", integer_field "One-based line for ordinary local text files (default 1; do not combine with a nonzero offset)" 1 max_int;
     "max_lines", integer_field "Maximum lines returned (default 1000)" 1 1000;
     "max_bytes", integer_field "Maximum output bytes (default 16384)" 1 max_read_bytes] ["path"];
  schema "workspace_snapshot" "Read a bounded page and SHA-256 for one text file snapshot (maximum 1 MiB); pass the hash to conflict-aware edit tools."
    ["path", string_field "Workspace-relative file path or owned worktree URI";
     "max_bytes", integer_field "Snapshot page bytes (default 16384)" 1 (max_read_bytes - 512)] ["path"];
  schema "list_files" "Recursively list workspace files; respects .gitignore and excludes build/dependency/Git directories. Bounded output."
    ["path", string_field "Workspace-relative or owned worktree directory (default .)"] [];
  schema "glob" "Discover files with *, ?, character classes and ** directory segments; respects nested .gitignore. Bounded output."
    ["pattern", string_field "Workspace-relative glob, for example **/*.swift";
     "path", string_field "Workspace-relative or owned worktree directory (default .)";
     "hidden", boolean_field "Include dotfiles and hidden directories (default false)";
     "limit", integer_field "Maximum matching paths (default 100)" 1 500] ["pattern"];
  schema "fuzzy_file_search" "Rank workspace files and directories by case-insensitive fuzzy path match; respects .gitignore, hidden-path policy and excluded dependency/build/Git directories. Bounded traversal and result count."
    ["query", bounded_string_field "Non-empty UTF-8 subsequence query (maximum 256 bytes)" 256;
     "path", string_field "Workspace-relative or owned worktree directory (default .)";
     "hidden", boolean_field "Include dotfiles and hidden directories (default false)";
     "max_results", integer_field "Maximum ranked matches (default 100)" 1 100] ["query"];
  schema "search" "Find bounded text matches in non-binary workspace files, respecting nested .gitignore; hidden paths are excluded by default."
    ["pattern", string_field "Literal text to search for";
     "path", string_field "Workspace-relative or owned worktree directory (default .)";
     "glob", string_field "Optional file glob relative to the search directory";
     "hidden", boolean_field "Include dotfiles and hidden directories (default false)";
     "case_sensitive", boolean_field "Use ASCII-only case matching (default true)";
     "limit", integer_field "Maximum matching lines (default 100)" 1 max_matches] ["pattern"];
  schema "web_search" "Search the public web. Uses the configured provider order (Exa, Firecrawl, Brave, Tavily, Kagi, Jina with API keys; credential-free DuckDuckGo otherwise), falling back to the next provider on failure. Requires network approval; returns source URLs, citations, and provider provenance."
    ["query", bounded_string_field "Search query sent to the selected provider" Web_search.max_query_bytes;
     "page", integer_field "Brave-only page/offset (default 0; nonzero pages exclude other providers)" 0 Web_search.max_page;
     "count", integer_field "Maximum results (default 5)" 1 Web_search.max_results] ["query"];
  schema "web_fetch" "Fetch one public HTTPS document without credentials or redirects (up to 1 MiB downloaded). Convert HTML to Markdown and preserve JSON text. Output is a bounded preview; truncated=true means content is incomplete, not a complete document or valid complete JSON. Requires separate network approval."
    ["url", bounded_string_field "Credential-free public HTTPS URL on port 443" 4096;
     "max_bytes", integer_field "Maximum returned content bytes, not download size (default 65536); inspect truncated"
       1 Web_search.max_content_bytes] ["url"];
  schema "image_ocr" "Run the fixed local Tesseract helper on a workspace image passed through stdin. Requires explicit approval; never invokes a shell or network."
    ["path", string_field "Workspace-relative image path";
     "mime", enum_string_field "Declared image type, checked against bytes"
       ["image/png"; "image/jpeg"; "image/gif"; "image/tiff"; "image/bmp"]]
    ["path"; "mime"];
  schema "clipboard_read" "Read the operating-system clipboard through a fixed platform helper. Clipboard text may contain private credentials; always requires explicit approval."
    [] [];
  schema "clipboard_write" "Replace the operating-system clipboard with this exact text. Always requires explicit approval; tool input is capped at 2000 bytes for complete review."
    ["text", bounded_string_field "Exact clipboard text, maximum 2000 bytes" 2000]
    ["text"];
  schema "grep" "Find bounded OCaml Str regex matches per line, respecting nested .gitignore; hidden paths are excluded by default. One repetition operator maximum; no backreferences."
    ["pattern", string_field "Regex (up to 512 bytes; at most one repetition; no backreferences)";
     "path", string_field "Workspace-relative or owned worktree directory (default .)";
     "glob", string_field "Optional file glob relative to the search directory";
     "hidden", boolean_field "Include dotfiles and hidden directories (default false)";
     "case_sensitive", boolean_field "Use ASCII-only case matching (default true)";
     "limit", integer_field "Maximum matching lines (default 100)" 1 max_matches] ["pattern"];
  schema "write_file" "Atomically create or replace a workspace or owned worktree file (maximum 1 MiB); parent directory must exist."
    ["path", string_field "Workspace-relative or owned worktree file path";
     "content", string_field "Complete replacement file contents"] ["path"; "content"];
  schema "edit_file" "Atomically replace exactly one occurrence of old_string in a workspace or owned worktree file (maximum 1 MiB)."
    ["path", string_field "Workspace-relative or owned worktree file path";
     "old_string", string_field "Exact, unique original text";
     "new_string", string_field "Replacement text"] ["path"; "old_string"; "new_string"];
  schema "apply_edits" "Atomically apply exact, unique non-overlapping hunks only if the workspace file still matches the supplied SHA-256 snapshot."
    ["path", string_field "Workspace-relative or owned worktree file path";
     "expected_sha256", string_field "SHA-256 returned by workspace_snapshot";
     "hunks", replacement_hunks_field] ["path"; "expected_sha256"; "hunks"];
  schema "ast_edit" "Preview or apply one OCaml implementation AST edit. Supports syntactic unqualified value rename or unique structural expression replacement; no text fallback."
    ["path", string_field "Workspace-relative or owned worktree .ml implementation file";
     "language", enum_string_field "Supported AST language" ["ocaml"];
     "operation", enum_string_field "AST operation" ["rename_identifier"; "replace_expression"];
     "expected_sha256", string_field "SHA-256 returned by workspace_snapshot";
     "old_name", string_field "Old unqualified OCaml value identifier";
     "new_name", string_field "New unqualified OCaml value identifier";
     "target", string_field "OCaml expression shape to match uniquely";
     "replacement", string_field "Replacement OCaml expression";
     "dry_run", boolean_field "Preview only; defaults to true"] ["path"; "language"; "operation"; "expected_sha256"];
  schema "run_command" "Run a shell command with workspace as cwd, returning exit status and bounded output/time. NOT SANDBOXED; every command requires interactive approval."
    ["command", string_field "Shell command to execute (not sandboxed)";
     "timeout_seconds", integer_field "Deadline in seconds (default 60, maximum 300)" 1 300] ["command"];
  schema "start_process" "Start an owned background process from an executable and argv. Bounded jobs/output; NOT SANDBOXED and requires explicit approval."
    ["id", string_field "Session-local job ID";
     "program", string_field "Executable path or command name";
     "arguments", string_array_field "Argument vector; no shell parsing";
     "cwd", string_field "Workspace-relative or owned worktree working directory (default workspace root)";
     "environment", environment_field;
     "timeout_seconds", integer_field "Optional process deadline (maximum 86400)" 1 86_400;
     "output_limit", integer_field "Retained output bytes (default 65536)" 1 Workspace_process.max_output_limit;
     "pty", boolean_field "Attach a PTY where the supported runtime is available"] ["id"; "program"];
  schema "start_shell" "Start a managed background shell job with bounded output/time. NOT SANDBOXED; each shell command requires explicit approval."
    ["id", string_field "Session-local job ID";
     "command", string_field "Shell command (not sandboxed)";
     "cwd", string_field "Workspace-relative or owned worktree working directory";
     "environment", environment_field;
     "timeout_seconds", integer_field "Optional process deadline (maximum 86400)" 1 86_400;
     "output_limit", integer_field "Retained output bytes (default 65536)" 1 Workspace_process.max_output_limit;
     "pty", boolean_field "Attach a PTY where the supported runtime is available"] ["id"; "command"];
  schema "process_list" "List bounded managed processes owned by the current private session."
    [] [];
  schema "process_output" "Read a bounded page of merged stdout/stderr by absolute byte offset."
    ["id", string_field "Session-local process job ID";
     "offset", integer_field "Absolute output byte offset (default 0)" 0 max_int;
     "max_bytes", integer_field "Maximum output bytes (default 16384)" 1 (max_read_bytes - 256)] ["id"];
  schema "process_wait" "Wait up to 300 seconds for a managed process to exit."
    ["id", string_field "Session-local process job ID";
     "timeout_seconds", integer_field "Wait duration (default 10, maximum 300)" 1 300] ["id"];
  schema "process_ready" "Wait for a managed process log regex and/or loopback port to become ready."
    ["id", string_field "Session-local process job ID";
     "log_regex", string_field "Bounded safe regex matched against retained output";
     "port", integer_field "Loopback TCP port to probe" 1 65_535;
     "timeout_seconds", integer_field "Readiness deadline (default 10, maximum 300)" 1 300] ["id"];
  schema "process_stdin" "Write bounded input bytes to a running managed process; requires explicit approval."
    ["id", string_field "Session-local process job ID";
     "data", string_field "Input bytes to write"] ["id"; "data"];
  schema "process_close_stdin" "Close stdin for a running managed process; requires explicit approval."
    ["id", string_field "Session-local process job ID"] ["id"];
  schema "process_kill" "Cancel a managed process and its process group; requires explicit approval."
    ["id", string_field "Session-local process job ID"] ["id"];
  schema "worktree_list" "List only Pave-managed worktrees owned by the current private session."
    [] [];
  schema "worktree_status" "Inspect status in a worktree managed by the current private session."
    ["id", string_field "Managed worktree ID"] ["id"];
  schema "worktree_diff" "Read a bounded no-color diff in a worktree managed by the current private session."
    ["id", string_field "Managed worktree ID"] ["id"];
  schema "worktree_history" "Read bounded recent commit metadata from a session-owned managed worktree."
    ["id", string_field "Managed worktree ID";
     "count", integer_field "Number of commits (default 20, maximum 100)" 1 100] ["id"];
  schema "worktree_create" "Create an isolated Git worktree outside the base repository. Always requires explicit approval."
    ["id", string_field "Session-local worktree ID";
     "path", string_field "Absolute new worktree path outside the base repository";
     "branch", string_field "New branch name"] ["id"; "path"; "branch"];
  schema "worktree_commit" "Commit only the explicitly listed paths in an owned worktree; unrelated staged changes are preserved. Always requires explicit approval."
    ["id", string_field "Managed worktree ID";
     "paths", string_array_field "Exact relative paths to stage and commit";
     "message", string_field "Single-line commit message"] ["id"; "paths"; "message"];
  schema "worktree_remove" "Remove only a clean managed worktree owned by the current private session. Always requires explicit approval."
    ["id", string_field "Managed worktree ID"] ["id"];
  schema "repository_security_scan" "Opt-in bounded deterministic workspace credential scan. Reports only rule metadata, workspace-relative locations, and validated provenance; never includes matched secret text."
    ["format", enum_string_field "Result format (default summary)" ["summary"; "sarif"];
     "file_limit", integer_field "Maximum files scanned (default 2000)" 0 Repository_security.max_files;
     "finding_limit", integer_field "Maximum findings returned (default 100)" 0 100] [];
  schema "memory" "Read and update the project memory store under .pave/memory. list shows saved knowledge entries; get returns one entry's text; put saves or replaces a <name>.md note (requires approval); forget deletes it (requires approval). Memory persists across sessions and is project-shared knowledge."
    ["action", enum_string_field "Store operation" ["list"; "get"; "put"; "forget"];
     "name", `Assoc ["type", `String "string"; "maxLength", `Int 48;
       "description", `String "Entry name [a-z][a-z0-9_-]{0,47}; required by get/put/forget"];
     "text", `Assoc ["type", `String "string"; "maxLength", `Int 32768;
       "description", `String "UTF-8 note body for put"]]
    ["action"];
  schema "publish_web" "Publish an already-running localhost web server through gosuda/portal-tunnel relays. publish starts a session-owned portal expose process; name is an optional hostname prefix (random when omitted). stop ends one tunnel; list inspects owned tunnels; attach publishes the session hub. No other tunnel service is used. The local server and tunnel must remain running; the hostname is publicly relay-listed."
    ["action", enum_string_field "Tunnel operation" ["publish"; "stop"; "list"; "attach"];
     "port", integer_field "Loopback port to publish (required for publish)" 1 65535;
     "name", bounded_string_field "Lowercase DNS hostname prefix; randomly generated when omitted; required for stop" 63;
     "relay", bounded_string_field "Optional HTTPS relay origin; pins this relay and disables discovery when provided" 2048]
    ["action"];
]

let function_name json =
  Protocol.member "name" (Protocol.member "function" json)

let definitions_without_shell = List.filter (fun json ->
  match function_name json with
  | `String name -> not (is_shell_tool name)
  | _ -> true) definitions

let available ~allow_shell =
  if allow_shell then definitions else definitions_without_shell

let available_for ~allow_shell ~enabled =
  List.filter (fun json ->
    match function_name json with
    | `String name when is_shell_tool name && not allow_shell -> false
    | `String name -> enabled name
    | _ -> false) definitions

let execution_mode = function
  | "mobile_project" | "read_file" | "workspace_snapshot"
  | "list_files" | "glob" | "search" | "grep" | "fuzzy_file_search"
  | "token_count" | "repository_security_scan" ->
      Tool_scheduler.Shared
  | _ -> Tool_scheduler.Exclusive

let approval_decision ~command_patterns ~name ~args =
  let tier value = {
    Approval.tier = value; policy = None; override = false; reason = None
  } in
  match name with
  | "mobile_project" | "read_file" | "workspace_snapshot"
  | "list_files" | "glob" | "search" | "grep" | "fuzzy_file_search"
  | "repository_security_scan" | "token_count" | "process_list" | "process_output"
  | "process_wait" | "process_ready" | "worktree_list" | "worktree_status"
  | "worktree_diff" | "worktree_history" | "clipboard_read" | "ssh_read"
  | "ssh_close" ->
      tier Approval.Read
  | "lsp" when optional_string "action" "" args = "apply_preview" ->
      tier Approval.Write
  | "lsp" -> tier Approval.Read
  | "dap" ->
      (match field "action" args with
       | `String ("initialize" | "threads" | "stack_trace" | "scopes" |
         "variables" | "events" | "close") -> tier Approval.Read
       | `String "disconnect" when
           not (optional_bool "terminate_debuggee" false args) -> tier Approval.Read
       | _ -> tier Approval.Exec)
  | "browser" ->
      (match field "action" args with
       | `String ("observe" | "list_tools" | "close") -> tier Approval.Read
       | `String "tool_events" when
           not (optional_bool "clear" false args) -> tier Approval.Read
       | _ -> tier Approval.Exec)
  | "publish_web" ->
      (match field "action" args with
       | `String "list" -> tier Approval.Read
       | _ -> tier Approval.Exec)
  | "memory" ->
      (match field "action" args with
       | `String ("list" | "get") -> tier Approval.Read
       | _ -> tier Approval.Write)
  | "ssh_write" | "write_file" | "edit_file" | "apply_edits" | "ast_edit"
  | "worktree_create" | "worktree_remove" | "clipboard_write" ->
      tier Approval.Write
  | "run_command" | "start_shell" ->
      (match Protocol.member "command" args with
       | `String command -> Approval.command_decision command_patterns command
       | _ -> tier Approval.Exec)
  | _ -> tier Approval.Exec
let preview_text text =
  let limit = 2000 in
  if String.length text <= limit then text
  else
    let rec boundary index =
      if index > 0 && index < String.length text &&
         (Char.code text.[index] land 0xc0) = 0x80 then boundary (index - 1)
      else index in
    let length = boundary limit in
    String.sub text 0 length ^
      Printf.sprintf "\n[%d bytes omitted]" (String.length text - length)
let bounded_tool_text text limit =
  if String.length text <= limit then text
  else
    let rec boundary index =
      if index > 0 && index < String.length text &&
         (Char.code text.[index] land 0xc0) = 0x80 then boundary (index - 1)
      else index in
    let length = boundary limit in
    String.sub text 0 length ^
      Printf.sprintf "\n[%d bytes omitted]" (String.length text - length)

let parse_hunks args =
  match field "hunks" args with
  | `List values ->
      List.map (function
        | `Assoc fields ->
            let names = List.map fst fields in
            if List.length names <> 2 ||
               List.sort String.compare names <> ["new_text"; "old_text"] then
              fail "each hunk must contain exactly old_text and new_text";
            let text name = match List.assoc_opt name fields with
              | Some (`String value) -> value
              | _ -> fail ("hunk " ^ name ^ " must be a string") in
            { Workspace_edit.old_text = text "old_text";
              new_text = text "new_text" }
        | _ -> fail "each hunk must be an object") values
  | _ -> fail "hunks must be an array"

let apply_edits ?cancel ?context root args =
  let root, args = resolve_path_arguments ?cancel ?context ~root args in
  let path = required_string "path" args in
  let expected_sha256 = required_string "expected_sha256" args in
  let hunks = parse_hunks args in
  let preview = Workspace_edit.apply_hunks ~root ~path ~expected_sha256 ~hunks in
  Printf.sprintf "%s %s; SHA-256: %s"
    (if preview.changed then "Applied" else "No changes to")
    path preview.result_sha256

let ast_operation args =
  let operation = required_string "operation" args in
  let present name = match field name args with `Null | `String "" -> false | _ -> true in
  match operation with
  | "rename_identifier" ->
      if present "target" || present "replacement" then
        fail "rename_identifier does not accept target or replacement";
      Workspace_edit.Rename_identifier {
        old_name = required_string "old_name" args;
        new_name = required_string "new_name" args }
  | "replace_expression" ->
      if present "old_name" || present "new_name" then
        fail "replace_expression does not accept old_name or new_name";
      Workspace_edit.Replace_expression {
        target = required_string "target" args;
        replacement = required_string "replacement" args }
  | _ -> fail "operation must be rename_identifier or replace_expression"

let ast_operation_text = function
  | Workspace_edit.Rename_identifier { old_name; new_name } ->
      Printf.sprintf "rename unqualified value identifier %S to %S" old_name new_name
  | Workspace_edit.Replace_expression { target; replacement } ->
      Printf.sprintf "replace unique expression %S with %S" target replacement

let ast_edit ?cancel ?context root args =
  let root, args = resolve_path_arguments ?cancel ?context ~root args in
  let path = required_string "path" args in
  let language = required_string "language" args in
  let edit = {
    Workspace_edit.path = path;
    expected_sha256 = required_string "expected_sha256" args;
    operation = ast_operation args;
  } in
  let dry_run = optional_bool "dry_run" true args in
  if dry_run then
    match Workspace_edit.preview_ast ~root ~language ~edits:[edit] with
    | [preview] ->
        Printf.sprintf
          "AST preview for %s (%s)\nOriginal SHA-256: %s\nResult SHA-256: %s\nChanged: %b\n%s\n%s"
          path language preview.original_sha256 preview.result_sha256 preview.changed
          (ast_operation_text edit.operation)
          (bounded_tool_text preview.content (max_read_bytes - 512))
    | _ -> assert false
  else
    let preview = Workspace_edit.apply_ast ~root ~language ~edit in
    Printf.sprintf "Applied AST edit to %s; SHA-256: %s; changed: %b"
      path preview.result_sha256 preview.changed


let approval_request ?cancel ?context ~root ~name ~args (decision : Approval.decision) =
  let base_root = root in
  let preview_root, preview_args =
    if List.mem name ["workspace_snapshot"; "write_file"; "edit_file";
        "apply_edits"; "ast_edit"] then
      resolve_path_arguments ?cancel ?context ~root args
    else root, args in
  let value name fallback args = match Protocol.member name args with
    | `String text -> text | _ -> fallback in
  let quoted name fallback args = Printf.sprintf "%S" (value name fallback args) in
  let cwd_detail () =
    let cwd = process_cwd ?cancel ?context ~root:base_root args in
    ["Working directory: " ^ Printf.sprintf "%S" cwd] in
  let environment_details () =
    let inherited =
      "Inherited environment allowlist: " ^
      String.concat ", " Workspace_process.inherited_environment_names ^
      "; provider, OAuth, cloud, search, and custom credentials are excluded." in
    let overrides = environment_overrides args in
    if overrides = [] then [inherited; "Explicit child overrides: none"]
    else [inherited; "Explicit child overrides:"] @
      List.map (fun (key, value) ->
        key ^ "=" ^ Printf.sprintf "%S" value) overrides in
  let impact, details = match name with
    | "mobile_project" ->
        "Reads project manifests and suggests commands; it executes nothing.",
        []
    | "read_file" ->
        (match value "path" "" args with
         | path when starts_with (String.lowercase_ascii path) "https://" ->
             "Fetches this public HTTPS URL without credentials or redirects; returned content is untrusted.",
             ["URL: " ^ Printf.sprintf "%S" path;
              "The pinned fetcher rejects private/local addresses and sends no authentication."]
         | _ ->
             "Reads a bounded workspace source; it makes no changes.",
             ["Path: " ^ quoted "path" "(missing)" args])
    | "web_search" ->
        let page = optional_int "page" 0 ~minimum:0 ~maximum:Web_search.max_page args in
        let order, credentialed, rendered = match Web_search.plan_summary ~page () with
          | explicit, engines ->
              (String.concat " → " (List.map (fun (name, _, browser) ->
                   if browser then name ^ " (local browser)" else name) engines) ^
                 (if explicit then " (from " ^ Web_search.priority_variable ^ ")"
                  else " (automatic)")),
              List.filter_map (fun (name, keyed, _) -> if keyed then Some name else None) engines,
              List.exists (fun (_, _, browser) -> browser) engines
          | exception Web_search.Error message -> "(unavailable: " ^ message ^ ")", [], false in
        "Sends this query to the first search provider in the order below; the next is tried only if one fails or finds nothing. Each API credential goes only to its own provider; DuckDuckGo and Ecosia receive the query without credentials.",
        ["Query: " ^ quoted "query" "(missing)" args;
         "Providers: " ^ order] @
        (if rendered then
           ["A local headless browser opens the provider's results page with a fresh, temporary profile; that page's scripts run in the browser sandbox."]
         else []) @
        [
         "Credentials sent to: " ^
           (if credentialed = [] then "none" else String.concat ", " credentialed);
         "Credentials are never shown in this approval."]
    | "web_fetch" ->
        "Fetches one public HTTPS page without authentication or redirects; the returned page is untrusted.",
        ["URL: " ^ quoted "url" "(missing)" args;
         "Only public addresses on port 443 are allowed."]
    | "image_ocr" ->
        "Runs the fixed local OCR helper on this workspace image; image bytes are sent to the helper through stdin, with no shell or network.",
        ["Path: " ^ quoted "path" "(missing)" args;
         "Declared MIME type: " ^ quoted "mime" "(missing)" args;
         "Maximum image size: 10 MiB."]
    | "clipboard_read" ->
        "Reads the current operating-system clipboard through a fixed platform helper; clipboard contents may include private credentials or personal data.",
        ["No clipboard content is read before approval."]
    | "clipboard_write" ->
        let text = value "text" "" args in
        "Replaces the current operating-system clipboard with this exact text; the previous clipboard value cannot be restored by /rewind.",
        [Printf.sprintf "Text (%d bytes):" (String.length text);
         Printf.sprintf "%S" (preview_text text)]
    | "browser" ->
        (match value "action" "" args with
         | "open" ->
             "Launches a pinned local Chromium-family browser headless with a fresh throwaway profile and a loopback-only debugging endpoint; the process is owned by this session.",
             ["Session: " ^ quoted "id" "browser" args;
              "Browser: " ^ (try Option.value (Web_search.detect_browser ())
                ~default:"(not found)"
                with _ -> "(invalid or missing " ^ Web_search.browser_variable ^ ")")]
         | "navigate" ->
             "Navigates the owned page to this exact URL; page scripts run inside the browser sandbox.",
             ["URL: " ^ quoted "url" "(missing)" args;
              "Session: " ^ quoted "id" "browser" args]
         | "evaluate" ->
             "Runs this exact JavaScript expression inside the owned page; page code is untrusted.",
             ["Expression: " ^ quoted "expression" "(missing)" args;
              "Session: " ^ quoted "id" "browser" args]
         | "screenshot" ->
             "Captures the owned page's rendered pixels; the returned image is untrusted content.",
             ["Session: " ^ quoted "id" "browser" args]
         | "call_tool" ->
             "Invokes a tool the page declared through its modelContext; the page's code runs the call and the result is untrusted.",
             ["Tool: " ^ quoted "name" "(missing)" args;
              "Arguments: " ^ quoted "arguments" "{}" args;
              "Session: " ^ quoted "id" "browser" args]
         | _ ->
             "Manages an isolated owned browser session; read actions expose page state.",
             ["Action: " ^ quoted "action" "(missing)" args;
              "Session: " ^ quoted "id" "browser" args])
    | "publish_web" ->
        (match value "action" "" args with
         | "publish" | "attach" as action ->
             "Publishes this localhost service through gosuda Portal; anyone can discover its hostname in the relay listing and reach it until stopped or session exit.",
             ["Local service: " ^ (if action = "attach" then "session control hub (token required)"
               else "127.0.0.1:" ^ value "port" "(missing)" args);
              "Public hostname prefix: " ^
                (match value "name" "" args with
                 | "" -> "randomly generated" | name -> name);
              "Relay: " ^ value "relay" "Portal public discovery" args;
              "Process: portal expose (PAVE_PORTAL or PATH); private identity outside the workspace"]
         | "stop" ->
             "Stops only the named session-owned Portal tunnel.",
             ["Prefix: " ^ value "name" "(missing)" args]
         | _ ->
             "Lists session-owned gosuda Portal tunnels.",
             ["Action: " ^ value "action" "(missing)" args])
    | "workspace_snapshot" ->
        "Reads a bounded text page and the file's SHA-256 snapshot; it makes no changes.",
        ["Path: " ^ quoted "path" "(missing)" args]
    | "list_files" ->
        "Lists workspace paths; it makes no changes.",
        ["Directory: " ^ quoted "path" "." args]
    | "glob" ->
        "Searches workspace paths; it makes no changes.",
        ["Pattern: " ^ quoted "pattern" "(missing)" args;
         "Directory: " ^ quoted "path" "." args]
    | "fuzzy_file_search" ->
        "Ranks workspace file and directory paths against this query; it makes no changes.",
        ["Query: " ^ quoted "query" "(missing)" args;
         "Directory: " ^ quoted "path" "." args;
         "Hidden paths: " ^ (if optional_bool "hidden" false args then "included" else "excluded");
         "Maximum results: " ^ string_of_int
           (optional_int "max_results" 100 ~minimum:1 ~maximum:100 args)]
    | "search" | "grep" ->
        "Searches workspace file contents; it makes no changes.",
        ["Pattern: " ^ quoted "pattern" "(missing)" args;
         "Directory: " ^ quoted "path" "." args]
    | "write_file" ->
        let content = value "content" "" args in
        "Creates or replaces a workspace or session-owned worktree file.",
        ["Path: " ^ quoted "path" "(missing)" args;
         Printf.sprintf "Content (%d bytes):" (String.length content);
         Printf.sprintf "%S" (preview_text content)]
    | "edit_file" ->
        "Replaces one exact, unique text range in a workspace or session-owned worktree file.",
        ["Path: " ^ quoted "path" "(missing)" args;
         "Find: " ^ Printf.sprintf "%S" (preview_text (value "old_string" "(missing)" args));
         "Replace with: " ^ Printf.sprintf "%S" (preview_text (value "new_string" "(missing)" args))]
    | "apply_edits" ->
        let path = value "path" "(missing)" preview_args in
        let expected = value "expected_sha256" "(missing)" preview_args in
        let hunks = parse_hunks preview_args in
        let preview = Workspace_edit.preview_hunks ~root:preview_root ~path
          ~expected_sha256:expected ~hunks in
        "Applies a conflict-checked atomic multi-hunk edit.",
        ["Path: " ^ quoted "path" "(missing)" args;
         "Original SHA-256: " ^ preview.original_sha256;
         "Result SHA-256: " ^ preview.result_sha256;
         Printf.sprintf "Hunks: %d" (List.length hunks)] @
        List.concat_map (fun (index, hunk) ->
          [Printf.sprintf "Hunk %d find: %S" index (preview_text hunk.Workspace_edit.old_text);
           Printf.sprintf "Hunk %d replace: %S" index (preview_text hunk.Workspace_edit.new_text)])
          (List.mapi (fun index hunk -> index + 1, hunk) hunks)
    | "ast_edit" ->
        let path = value "path" "(missing)" preview_args in
        let language = value "language" "(missing)" preview_args in
        let operation = ast_operation args in
        let edit = { Workspace_edit.path = path;
          expected_sha256 = value "expected_sha256" "(missing)" preview_args;
          operation } in
        let preview = match Workspace_edit.preview_ast ~root:preview_root
            ~language ~edits:[edit] with
          | [preview] -> preview | _ -> assert false in
        let dry_run = optional_bool "dry_run" true args in
        (if dry_run then "Previews a syntax-aware AST edit without writing."
         else "Applies one syntax-aware AST edit after approval."),
        ["Path: " ^ quoted "path" "(missing)" args;
         "Language: " ^ language;
         "Original SHA-256: " ^ preview.original_sha256;
         "Result SHA-256: " ^ preview.result_sha256;
         "Operation: " ^ ast_operation_text operation;
         "Proposed content: " ^ Printf.sprintf "%S" (preview_text preview.content)]
    | "xcode_preflight" ->
        let command = xcode_command args in
        (if optional_string "action" "" args = "simulators" then
           "Lists installed CoreSimulator devices as your user, intersected with this session's approved Xcode scheme destinations. No device is booted, installed to or launched."
         else
           "Executes one Xcode command against the selected project as your user; project configuration is untrusted executable code. Discovery and build/test require separate approvals."),
        ["Working directory: parent of " ^ quoted "subroot" "(missing)" args;
         "Exact command: " ^ command;
         (if optional_string "action" "" args = "simulators" then
            "Only installed available iOS Simulator devices compatible with this scheme are shown; no physical devices or local device paths."
          else
            "Build/test may write derived data; simulator tests may launch a simulator. No signing or physical-device destination is selected.")]
    | "mobile_check" ->
        let command, cwd = mobile_command ~root:base_root args in
        "Executes selected mobile project code as your user; discovery and execution each need approval. Commands do not install dependencies, provision SDKs, or sandbox project code.",
        ["Working directory: " ^ Printf.sprintf "%S" cwd;
         "Exact command: " ^ command;
         "The selected toolchain may write local build artifacts or invoke project-defined code."]
    | "android_devices" ->
        let command, cwd = android_device_command ~root:base_root args in
        "Lists Android devices as your user; this inventory is not authorization to boot, install, launch or test. Each phase requires separate interactive approval.",
        ["Working directory: " ^ Printf.sprintf "%S" cwd;
         "Exact command: " ^ command;
         (if optional_string "action" "" args = "devices" then
            "ADB may start its local server and access your configured ADB identity. Physical serials are withheld; offline and unauthorized transports are not ready."
          else
            "Reads locally configured AVD names; no SDK or system image is installed, and no emulator is booted.")]
    | "run_command" ->
        "Runs /bin/sh as your user from the workspace root. It is not sandboxed and may access or modify files outside the workspace or use the network.",
        ["Working directory: " ^ Printf.sprintf "%S" base_root;
         "Command: " ^ value "command" "(missing)" args]
    | "start_process" ->
        let program = value "program" "(missing)" args in
        let arguments = string_list "arguments" args in
        "Starts an unsandboxed background executable under your account; it may access files and the network.",
        (["Job ID: " ^ quoted "id" "(missing)" args;
          "Program and arguments: " ^ Filename.quote_command program arguments] @
         cwd_detail () @ environment_details ())
    | "start_shell" ->
        "Starts an unsandboxed background shell under your account; it may access files and the network.",
        (["Job ID: " ^ quoted "id" "(missing)" args;
          "Command: " ^ value "command" "(missing)" args] @
         cwd_detail () @ environment_details ())
    | "process_stdin" ->
        let data = value "data" "" args in
        "Writes input to an existing session-owned process; the process may perform side effects.",
        ["Job ID: " ^ quoted "id" "(missing)" args;
         Printf.sprintf "Input (%d bytes): %S" (String.length data) data]
    | "process_close_stdin" ->
        "Closes stdin for an existing session-owned process.",
        ["Job ID: " ^ quoted "id" "(missing)" args]
    | "process_kill" ->
        "Terminates an existing session-owned process and its process group.",
        ["Job ID: " ^ quoted "id" "(missing)" args]
    | "worktree_create" ->
        "Creates a new Git branch and worktree outside the selected repository.",
        ["Worktree ID: " ^ quoted "id" "(missing)" args;
         "Path: " ^ quoted "path" "(missing)" args;
         "Branch: " ^ quoted "branch" "(missing)" args]
    | "worktree_commit" ->
        let paths = string_list "paths" args in
        "Stages and commits only these exact paths in a session-owned worktree; unrelated staged changes are preserved.",
        ["Worktree ID: " ^ quoted "id" "(missing)" args;
         "Paths: " ^ String.concat ", " (List.map (Printf.sprintf "%S") paths);
         "Commit message: " ^ quoted "message" "(missing)" args]
    | "worktree_remove" ->
        "Removes a clean session-owned worktree; dirty or ignored content makes removal fail.",
        ["Worktree ID: " ^ quoted "id" "(missing)" args]
    | "lsp_start" ->
        let program = value "program" "(missing)" args in
        let arguments = string_list "arguments" args in
        "Starts an unsandboxed language-server process under your account. Server code may access files and the network.",
        ["Workspace: " ^ Printf.sprintf "%S" base_root;
         "Program and arguments: " ^ Filename.quote_command program arguments;
         "The server receives only PATH, LANG, LC_ALL, and TMPDIR."]
    | "lsp" ->
        let action = value "action" "(missing)" args in
        let details =
          if action = "apply_preview" then (
            let preview_id = required_string "preview_id" args in
            let title, files = lsp_preview ?context ~root:base_root args in
            let rows = Workspace_lsp.preview_file_rows files in
            let file_details = List.concat_map (fun row ->
              let path = required_string "path" row in
              let original = required_string "original_sha256" row in
              let result = required_string "result_sha256" row in
              let content = required_string "content" row in
              ["File: " ^ Printf.sprintf "%S" path;
               "Original SHA-256: " ^ original;
               "Proposed SHA-256: " ^ result;
               "Exact proposed contents:\n" ^ content]) rows in
            let details =
              ["Action: apply_preview"; "Preview ID: " ^ preview_id;
               "Edit: " ^ title] @ file_details in
            let size = List.fold_left (fun total detail ->
              String.length detail + total + 1) 0 details in
            if size > 8_192 then
              fail "LSP edit preview exceeds the interactive approval limit";
            details)
          else [
            "Action: " ^ action;
            "Document: " ^ Printf.sprintf "%S"
              (value "path" "(not required for this action)" args);
            "Language ID: " ^ quoted "language_id" "(missing)" args;
            "Position: " ^ Yojson.Basic.to_string (field "position" args);
            "Range: " ^ Yojson.Basic.to_string (field "range" args);
            "New name: " ^ quoted "new_name" "(not applicable)" args
          ] in
        (if action = "apply_preview" then
           "Applies this exact cached rename/code-action preview. All target paths, original file hashes, proposed file hashes, and complete proposed contents are shown above; files are revalidated before the one-shot apply."
         else
           "Sends a read-only request to the already approved workspace language server."),
        details
    | "workspace_eval" ->
        let action = value "action" "run" args in
        let code = value "code" "" args in
        "Runs unsandboxed persistent " ^ value "language" "(missing)" args ^
          " code or resets that language's session kernel. Kernel code may access files and the network.",
        ["Action: " ^ action;
         "Code (" ^ string_of_int (String.length code) ^ " bytes):";
         Printf.sprintf "%S" code;
         "Timeout: " ^ string_of_int (optional_int "timeout_seconds" 10
           ~minimum:1 ~maximum:Workspace_eval.max_timeout_seconds args) ^ " seconds";
         "pave.tool bridge: read-only workspace_snapshot/read_file/list_files/search/glob/grep/mobile_project and process_list/process_output/process_wait/process_ready; no job start, stdin, close, or kill."]
    | "ssh_open" ->
        "Opens an SSH connection to the requested host using system SSH identities and known_hosts; the connection receives no Pave OAuth/API credentials.",
        ["Session ID: " ^ quoted "id" "(missing)" args;
         "Host: " ^ quoted "host" "(missing)" args;
         "User: " ^ quoted "user" "(missing)" args;
         "Remote root: " ^ quoted "remote_root" "(missing)" args;
        "Known hosts: " ^ quoted "known_hosts"
          (match Sys.getenv_opt "HOME" with
           | Some home when home <> "" ->
               Filename.concat (Filename.concat home ".ssh") "known_hosts"
           | _ -> "(unavailable: HOME is not set)") args]
    | "ssh_close" ->
        "Closes this session-owned SSH connection.",
        ["Session ID: " ^ quoted "id" "(missing)" args]
    | "ssh_read" | "ssh_write" | "ssh_command" ->
        let context = require_session_context context in
        let session = ssh_session context (value "id" "(missing)" args) in
        let endpoint = Workspace_ssh.session_endpoint session in
        let common = [
          "Session ID: " ^ quoted "id" "(missing)" args;
          "SSH endpoint: " ^ endpoint.user ^ "@" ^ endpoint.host ^ ":" ^ endpoint.remote_root
        ] in
        (match name with
         | "ssh_read" ->
             "Reads remote data from the pinned SSH workspace; no remote file is changed.",
             common @ ["Path: " ^ quoted "path" "(missing)" args]
         | "ssh_write" ->
             let contents = value "contents" "" args in
             "Writes these exact contents to the pinned SSH workspace.",
             common @ ["Path: " ^ quoted "path" "(missing)" args;
                       Printf.sprintf "Contents (%d bytes):" (String.length contents);
                       Printf.sprintf "%S" contents]
         | _ ->
             let program = value "program" "(missing)" args in
             let arguments = string_list "arguments" args in
             "Executes this exact remote program and argument vector without a shell.",
             common @ ["Program and arguments: " ^
               Filename.quote_command program arguments;
               "Timeout: " ^ string_of_int (optional_int "timeout_seconds"
                 Workspace_ssh.default_timeout_seconds ~minimum:1
                 ~maximum:Workspace_ssh.max_timeout_seconds args) ^ " seconds"])
    | "dap_start" ->
        let program = value "program" "(missing)" args in
        let arguments = string_list "arguments" args in
        "Starts an unsandboxed DAP adapter as a direct child process; the adapter may access files or the network.",
        ["Session ID: " ^ quoted "id" "(missing)" args;
         "Workspace: " ^ Printf.sprintf "%S" base_root;
         "Program and arguments: " ^ Filename.quote_command program arguments;
         "Adapter environment contains only PATH and LANG; stderr is discarded from the DAP protocol."]
    | "dap" ->
        let action = value "action" "(missing)" args in
        let impact = match action with
          | "launch" -> "Launches a workspace debuggee."
          | "trust_host" -> "Pins the requested remote debug host for this DAP session."
          | "attach" -> "Attaches the workspace adapter to the approved remote host and port."
          | "set_breakpoints" -> "Changes active breakpoints in the debuggee."
          | "configuration_done" -> "Allows the debuggee to begin execution."
          | "continue" | "next" | "step_in" | "step_out" -> "Controls debuggee execution."
          | "evaluate" -> "Evaluates an expression in the stopped debuggee."
          | "disconnect" when optional_bool "terminate_debuggee" false args ->
              "Disconnects and terminates the debuggee."
          | _ -> "Performs the requested DAP session operation." in
        impact,
        ["Session ID: " ^ quoted "id" "(missing)" args;
         "Action: " ^ action;
         "Exact request parameters: " ^ Yojson.Basic.to_string args;
         "Workspace-confined launch/source paths and effect-specific authorization are enforced."]
    | "token_count" ->
        "Counts text locally with exact vendored tokenizer ranks; no network or provider identity is used.",
        ["Encoding: " ^ quoted "encoding" "(missing)" args;
         "Input bytes: " ^ string_of_int (String.length (value "text" "" args))]
    | _ ->
        "Performs a tool action that has no safe preview.",
        ["No argument preview is available."] in
  let trigger = match name with
    | "run_command" | "start_shell" -> Approval.Dangerous_command
    | "web_search" | "web_fetch" | "browser" | "ssh_command" | "publish_web" ->
        Approval.Network
    | "write_file" | "edit_file" | "apply_edits" | "ast_edit" | "workspace_rewind" ->
        Approval.File_access
    | _ -> Approval.Tool_call in
  { Approval.tool_name = name; tier = decision.tier; trigger; impact; details;
    reason = decision.reason }

let parameters_schema definitions name =
  match List.find_opt (fun json -> function_name json = `String name) definitions with
  | Some json -> Some (Protocol.member "parameters" (Protocol.member "function" json))
  | None -> None

(* Models differ in how they spell optional or scalar values: null for an
   omitted field, "5" or 5.0 for an integer, "true" for a boolean, a lone string
   for a one-item array, or a differently cased enum value. Rewrite only these
   unambiguous spellings; schema validation still decides what is accepted. *)
let rec normalize_value schema value =
  let type_name = match Protocol.member "type" schema with `String value -> value | _ -> "" in
  let enum = match Protocol.member "enum" schema with `List values -> values | _ -> [] in
  let integer_text text =
    let text = String.trim text in
    if text <> "" && String.for_all (fun c -> (c >= '0' && c <= '9') || c = '-') text
    then int_of_string_opt text else None in
  let value = match type_name, value with
    | "integer", `Float number when Float.is_integer number &&
        Float.abs number < 4.0e15 -> `Int (int_of_float number)
    | "integer", `String text ->
        (match integer_text text with Some number -> `Int number | None -> value)
    | "number", `String text ->
        (match integer_text text, float_of_string_opt (String.trim text) with
         | Some number, _ -> `Int number
         | None, Some number when Float.is_finite number -> `Float number
         | _ -> value)
    | "boolean", `String text ->
        (match String.lowercase_ascii (String.trim text) with
         | "true" -> `Bool true | "false" -> `Bool false | _ -> value)
    | "string", `Int number -> `String (string_of_int number)
    | "string", `Bool flag -> `String (string_of_bool flag)
    | ("object" | "array"), `String text ->
        (match (try Some (Yojson.Basic.from_string text) with Yojson.Json_error _ -> None),
               type_name with
         | Some (`Assoc _ as parsed), "object" | Some (`List _ as parsed), "array" -> parsed
         | _, "array" when Protocol.member "type" (Protocol.member "items" schema) = `String "string" ->
             `List [value]
         | _ -> value)
    | _ -> value in
  let value = match value with
    | `String text when enum <> [] && not (List.mem value enum) ->
        let folded = String.lowercase_ascii (String.trim text) in
        (match List.filter (function
            | `String candidate -> String.lowercase_ascii candidate = folded
            | _ -> false) enum with
         | [unique] -> unique
         | _ -> value)
    | _ -> value in
  match value with
  | `Assoc fields ->
      let properties = match Protocol.member "properties" schema with
        | `Assoc properties -> properties | _ -> [] in
      let required = match Protocol.member "required" schema with
        | `List names -> List.filter_map (function `String name -> Some name | _ -> None) names
        | _ -> [] in
      `Assoc (List.filter_map (fun (field, field_value) ->
        match List.assoc_opt field properties, field_value with
        | Some field_schema, `Null
          when not (List.mem field required) &&
               Protocol.member "type" field_schema <> `String "null" -> None
        | Some field_schema, _ -> Some (field, normalize_value field_schema field_value)
        | None, _ -> Some (field, field_value)) fields)
  | `List items when type_name = "array" ->
      let item_schema = Protocol.member "items" schema in
      if item_schema = `Null then value
      else `List (List.map (normalize_value item_schema) items)
  | _ -> value

let normalize_arguments ~schema args =
  match args with `Assoc _ -> normalize_value schema args | _ -> args

let normalize_tool_arguments ~name ~args =
  match parameters_schema definitions name with
  | Some schema -> normalize_arguments ~schema args
  | None -> args

let describe_parameters schema =
  let properties = match Protocol.member "properties" schema with
    | `Assoc properties -> properties | _ -> [] in
  let required = match Protocol.member "required" schema with
    | `List names -> names | _ -> [] in
  String.concat ", " (List.map (fun (field, field_schema) ->
    let type_name = match Protocol.member "type" field_schema with
      | `String value -> value | _ -> "value" in
    let range = match Protocol.member "minimum" field_schema,
                      Protocol.member "maximum" field_schema with
      | `Int low, `Int high when high < max_int -> Printf.sprintf " %d..%d" low high
      | `Int low, _ -> Printf.sprintf " >=%d" low
      | _ -> "" in
    let values = match Protocol.member "enum" field_schema with
      | `List values -> " one of " ^ String.concat "|" (List.filter_map (function
          | `String value -> Some value | _ -> None) values)
      | _ -> "" in
    Printf.sprintf "%s (%s%s%s%s)" field type_name range values
      (if List.mem (`String field) required then ", required" else ""))
    properties)

let validate_arguments ~name ~args =
  let schema = match parameters_schema definitions name with
    | Some schema -> schema
    | None -> fail ("unknown tool: " ^ name) in
  (match Protocol.member Protocol.invalid_arguments_key args with
   | `String received ->
       fail ("arguments were not a valid JSON object; resend the call with one JSON object. Received: " ^ received)
   | _ -> ());
  let schema_fields key schema =
    match Protocol.member key schema with
    | `Assoc fields -> fields
    | `Null -> []
    | _ -> fail "invalid tool parameter schema" in
  let names key schema =
    match Protocol.member key schema with
    | `List values -> List.map (function
        | `String value -> value
        | _ -> fail "invalid tool parameter schema") values
    | `Null -> []
    | _ -> fail "invalid tool parameter schema" in
  let bound key schema =
    match Protocol.member key schema with
    | `Int number -> Some number
    | `Null -> None
    | _ -> fail "invalid tool parameter schema" in
  let rec validate_value label schema value =
    let type_name = match Protocol.member "type" schema with
      | `String value -> value
      | _ -> fail "invalid tool parameter schema" in
    let valid = match type_name, value with
      | "string", `String _ | "integer", `Int _ | "boolean", `Bool _
      | "object", `Assoc _ | "array", `List _ | "null", `Null
      | "number", (`Int _ | `Float _) -> true
      | _ -> false in
    if not valid then fail (label ^ " must have JSON type " ^ type_name);
    (match Protocol.member "enum" schema with
     | `List allowed when not (List.mem value allowed) ->
         fail (label ^ " must be one of the advertised values")
     | `List _ | `Null -> ()
     | _ -> fail "invalid tool parameter schema");
    (match value with
     | `String text ->
         (match bound "maxLength" schema with
          | Some maximum when String.length text > maximum ->
              fail (label ^ " exceeds its maximum length")
          | Some _ | None -> ())
     | `Int number ->
         let minimum = bound "minimum" schema in
         let maximum = bound "maximum" schema in
         if (match minimum with Some limit -> number < limit | None -> false) ||
            (match maximum with Some limit -> number > limit | None -> false)
         then fail (Printf.sprintf "%s=%d is outside its allowed range%s" label number
           (match minimum, maximum with
            | Some low, Some high -> Printf.sprintf " %d..%d" low high
            | Some low, None -> Printf.sprintf " >=%d" low
            | None, Some high -> Printf.sprintf " <=%d" high
            | None, None -> ""))
     | `Assoc fields ->
         let field_names = List.map fst fields in
         if List.length field_names <> List.length (List.sort_uniq String.compare field_names) then
           fail (label ^ " contains a duplicate field");
         let properties = schema_fields "properties" schema in
         List.iter (fun required ->
           if not (List.mem_assoc required fields) then
             fail (label ^ " is missing required field " ^ required))
           (names "required" schema);
         let additional = Protocol.member "additionalProperties" schema in
         List.iter (fun (field, field_value) ->
           match List.assoc_opt field properties with
           | Some field_schema -> validate_value (label ^ "." ^ field) field_schema field_value
           | None ->
               (match additional with
                | `Bool false ->
                    fail (Printf.sprintf "unexpected argument: %s.%s" label field)
                | `Assoc _ as field_schema ->
                    validate_value (label ^ "." ^ field) field_schema field_value
                | `Bool true | `Null -> ()
                | _ -> fail "invalid tool parameter schema")) fields
     | `List values ->
         let item_schema = Protocol.member "items" schema in
         if item_schema = `Null then fail "invalid tool parameter schema";
         List.iteri (fun index item -> validate_value
           (Printf.sprintf "%s[%d]" label index) item_schema item) values
     | _ -> ())
  in
  try validate_value "arguments" schema args
  with Tool_error message ->
    fail (Printf.sprintf "%s. Accepted arguments: %s" message (describe_parameters schema))

type prepared_execution =
  ?cancel:(unit -> bool) -> ?on_progress:(int -> unit) -> ?approved:bool -> unit ->
  (Protocol.content_block list, string) result

let session_tool_names = [
  "start_process"; "start_shell"; "process_list"; "process_output";
  "process_wait"; "process_ready"; "process_stdin"; "process_close_stdin";
  "process_kill"; "worktree_list"; "worktree_status"; "worktree_diff";
  "worktree_history"; "worktree_create"; "worktree_commit"; "worktree_remove";
  "lsp_start"; "lsp"; "workspace_eval";
  "ssh_open"; "ssh_close"; "ssh_read"; "ssh_write"; "ssh_command";
  "dap_start"; "dap"; "xcode_preflight"; "mobile_check"; "android_devices";
  "browser"; "publish_web"
]

let path_tool_names = [
  "workspace_snapshot"; "list_files"; "search"; "glob"; "grep";
  "fuzzy_file_search"; "image_ocr";
  "write_file"; "edit_file"; "apply_edits"; "ast_edit"
]

let error_message = function
  | Tool_error message | Workspace_edit.Error message | Workspace_path.Error message
  | Workspace_process.Error message | Workspace_git.Error message
  | Workspace_reader.Error message | Repository_security.Error message
  | Web_search.Error message | Native_services.Error message
  | Workspace_lsp.Error message | Workspace_dap.Error message
  | Workspace_dap.Not_approved message | Workspace_eval.Error message
  | Workspace_ssh.Error message | Native_tokenizer.Error message
  | Workspace_xcode.Error message | Workspace_swiftpm_focus.Error message
  | Workspace_gradle_focus.Error message | Workspace_flutter_focus.Error message
  | Workspace_node_scripts.Error message
  | Workspace_android_devices.Error message
  | Workspace_browser.Error message | Workspace_portal.Error message ->
      "Error: " ^ message
  | Workspace_dap.Cancelled -> "Error: DAP operation cancelled"
  | Unix.Unix_error (code, operation, path) ->
      Printf.sprintf "Error: %s %s: %s" operation path (Unix.error_message code)
  | Sys_error message -> "Error: " ^ message
  | exn -> "Error: " ^ Printexc.to_string exn

let prepare ?cancel ?context ~root ~name ~args () =
  try
    let root = Workspace_path.root_path root in
    validate_arguments ~name ~args;
    if List.mem name session_tool_names then
      ignore (require_session_context context);
    let tool_root, tool_args =
      if List.mem name path_tool_names then
        resolve_path_arguments ?cancel ?context ~root args
      else root, args in
    if name = "web_search" then (
      ignore (Web_search.valid_query (required_string "query" args));
      let page = optional_int "page" 0 ~minimum:0 ~maximum:Web_search.max_page args in
      Web_search.check_configuration ~page ());
    if name = "browser" then (
      match optional_string "action" "" args with
      | "open" -> ignore (Workspace_browser.detect_browser ())
      | "navigate" ->
          ignore (Workspace_browser.validate_navigation_url
            (required_string "url" args))
      | _ -> ());
    if name = "publish_web" then (
      match optional_string "action" "" args with
      | "publish" | "attach" ->
          ignore (Workspace_portal.detect_portal ());
          (if optional_string "action" "" args = "publish" then (
            let port = optional_int "port" 0 ~minimum:1 ~maximum:65535 args in
            if port = 0 then fail "publish requires a loopback port";
            if not (Workspace_process.port_accepting port) then
              fail "localhost web server is not listening on the requested 127.0.0.1 port"));
          (match optional_string "name" "" args with
           | "" -> () | value -> ignore (Workspace_portal.publish_name value));
          (match optional_string "relay" "" args with
           | "" -> () | value -> ignore (Workspace_portal.https_origin value));
          (if optional_string "action" "" args = "attach" then
            let context = require_session_context context in
            if context.hub_port = None then fail "attach requires an interactive session hub")
      | "stop" -> ignore (Workspace_portal.publish_name (required_string "name" args))
      | _ -> ());
    if name = "start_process" then (
      Workspace_process.validate_id (required_string "id" args);
      Workspace_process.validate_program
        (required_string "program" args) (string_list "arguments" args);
      ignore (process_cwd ?cancel ?context ~root args);
      ignore (Workspace_process.environment_with_overrides (environment_overrides args));
      ignore (optional_timeout "timeout_seconds" 86_400 args);
      ignore (optional_int "output_limit" 65_536 ~minimum:1
        ~maximum:Workspace_process.max_output_limit args);
      ignore (optional_bool "pty" false args))
    else if name = "start_shell" then (
      Workspace_process.validate_id (required_string "id" args);
      Workspace_process.validate_text "shell command" 65_536
        (required_string "command" args);
      ignore (process_cwd ?cancel ?context ~root args);
      ignore (Workspace_process.environment_with_overrides (environment_overrides args));
      ignore (optional_timeout "timeout_seconds" 86_400 args);
      ignore (optional_int "output_limit" 65_536 ~minimum:1
        ~maximum:Workspace_process.max_output_limit args);
      ignore (optional_bool "pty" false args));
    let execute ?cancel ?on_progress ?(approved = false) () =
      try
        let result = match name with
          | "browser" -> Ok (browser_tool ~approved ?cancel ?context args)
          | "publish_web" -> Ok (publish_web_tool ~approved ?cancel ?context args)
          | "memory" -> Ok [Protocol.Text (Yojson.Basic.to_string
              (try memory_tool ~approved ~root args
               with Workspace_memory.Error message -> fail message))]
          | _ -> Ok [Protocol.Text (match name with
          | "read_file" -> read_file ?cancel ?context root args
          | "workspace_snapshot" -> workspace_snapshot ?cancel ?context tool_root tool_args
          | "list_files" -> list_files tool_root tool_args
          | "fuzzy_file_search" -> fuzzy_file_search ?cancel tool_root tool_args
          | "search" -> search tool_root tool_args
          | "glob" -> glob tool_root tool_args
          | "grep" -> grep tool_root tool_args
          | "write_file" -> write_file tool_root tool_args
          | "edit_file" -> edit_file tool_root tool_args
          | "apply_edits" -> apply_edits ?cancel ?context tool_root tool_args
          | "ast_edit" -> ast_edit ?cancel ?context tool_root tool_args
          | "run_command" -> run_command ?cancel ?on_progress root args
          | "xcode_preflight" ->
              xcode_preflight ~approved ?cancel ?on_progress ?context root args
          | "mobile_check" ->
              mobile_check ~approved ?cancel ?on_progress ?context root args
          | "android_devices" ->
              android_devices ~approved ?cancel ?on_progress ?context root args
          | "start_process" -> start_process ~approved ?cancel ?context root args
          | "start_shell" -> start_shell ~approved ?cancel ?context root args
          | "process_list" -> process_list ?context root args
          | "process_output" -> process_output ?context root args
          | "process_wait" -> process_wait ?cancel ?context root args
          | "process_ready" -> process_ready ?cancel ?context root args
          | "process_stdin" -> process_stdin ~approved ?context root args
          | "process_close_stdin" -> process_close_stdin ~approved ?context root args
          | "process_kill" -> process_kill ~approved ?context root args
          | "worktree_list" -> worktree_list ?cancel ?context root args
          | "worktree_status" -> worktree_status ?cancel ?context root args
          | "worktree_diff" -> worktree_diff ?cancel ?context root args
          | "worktree_history" -> worktree_history ?cancel ?context root args
          | "worktree_create" -> worktree_create ~approved ?cancel ?context root args
          | "worktree_commit" -> worktree_commit ~approved ?cancel ?context root args
          | "worktree_remove" -> worktree_remove ~approved ?cancel ?context root args
          | "repository_security_scan" ->
              repository_security_scan ?cancel root args
          | "web_search" -> web_search ~approved ?cancel args
          | "web_fetch" -> web_fetch ~approved ?cancel args
          | "image_ocr" -> image_ocr ~approved ?cancel tool_root tool_args
          | "clipboard_read" -> clipboard_read ~approved ?cancel ()
          | "clipboard_write" -> clipboard_write ~approved ?cancel args
          | "lsp_start" -> lsp_start ~approved ?context root args
          | "lsp" -> lsp_execute ~approved ?cancel ?context root args
          | "workspace_eval" -> workspace_eval ~approved ?cancel ?context root args
          | "ssh_open" -> ssh_open ~approved ?cancel ?context args
          | "ssh_close" -> ssh_close ?context args
          | "ssh_read" -> ssh_read ~approved ?cancel ?context args
          | "ssh_write" -> ssh_write ~approved ?cancel ?context args
          | "ssh_command" -> ssh_command ~approved ?cancel ?context args
          | "dap_start" -> dap_start ~approved ?cancel ?context root args
          | "dap" -> dap_execute ~approved ?cancel ?context root args
          | "mobile_project" -> mobile_project ?cancel root args
          | "token_count" -> token_count args
          | _ -> assert false) ] in
        result
      with
      | Cancelled | Workspace_dap.Cancelled | Workspace_browser.Cancelled -> raise Cancelled
      | (Workspace_process.Error _ | Workspace_git.Error _ |
         Workspace_reader.Error _ | Repository_security.Error _
         | Web_search.Error _ | Native_services.Error _
         | Workspace_lsp.Error _ | Workspace_dap.Error _
         | Workspace_dap.Not_approved _ | Workspace_eval.Error _
         | Workspace_browser.Error _ | Workspace_portal.Error _
         | Workspace_ssh.Error _ | Native_tokenizer.Error _) as exn ->
          (match cancel with
           | Some cancelled when cancelled () -> raise Cancelled
           | _ -> Error (error_message exn))
      | exn ->
          (match cancel with
           | Some cancelled when cancelled () -> raise Cancelled
           | _ -> Error (error_message exn)) in
    Ok execute
  with
  | Cancelled | Workspace_dap.Cancelled | Workspace_browser.Cancelled -> raise Cancelled
  | (Workspace_process.Error _ | Workspace_git.Error _ |
     Workspace_reader.Error _ | Workspace_lsp.Error _ | Workspace_dap.Error _
     | Workspace_dap.Not_approved _ | Workspace_eval.Error _
     | Workspace_ssh.Error _ | Web_search.Error _ | Workspace_browser.Error _
     | Workspace_portal.Error _) as exn ->
      (match cancel with
       | Some cancelled when cancelled () -> raise Cancelled
       | _ -> Error (error_message exn))
  | exn ->
      (match cancel with
       | Some cancelled when cancelled () -> raise Cancelled
       | _ -> Error (error_message exn))


let execute ?cancel ?on_progress ?preflight ?context ?(approved = false)
    ~root ~name ~args () =
  match prepare ?cancel ?context ~root ~name ~args () with
  | Error result -> Error result
  | Ok execute ->
      try
        match preflight with
        | Some check ->
            (match check () with
             | Some message -> Error message
             | None -> execute ?cancel ?on_progress ~approved ())
        | None -> execute ?cancel ?on_progress ~approved ()
      with
      | Cancelled -> raise Cancelled
      | exn -> Error (error_message exn)

