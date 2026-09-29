
let max_read_bytes = Workspace_path.max_read_bytes
let max_write_bytes = Workspace_path.max_write_bytes
let max_command_bytes = 65_536
let max_walk_entries = 10_000
let max_search_bytes = 16_777_216
let max_matches = 100
let max_regex_line = 4096

exception Tool_error of string
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

let create_session_context ?lsp_manager ~owner ~root ~process_manager ~read_artifact
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
    record_file_change; closed = false }

let close_session_context context =
  if not context.closed then (
    context.closed <- true;
    context.dap_granted_effect := None;
    let ignore_failure action = try action () with _ -> () in
    ignore_failure (fun () -> Workspace_lsp.close_manager context.lsp_manager);
    ignore_failure (fun () -> Workspace_dap.close_manager context.dap_manager);
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

let find_from text needle start =
  let text_len = String.length text and needle_len = String.length needle in
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
let glob_segment pattern name =
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

let glob_parts pattern path =
  let patterns = Array.of_list (String.split_on_char '/' pattern) in
  let names = Array.of_list (String.split_on_char '/' path) in
  let memo = Hashtbl.create 32 in
  let rec matches i j =
    match Hashtbl.find_opt memo (i, j) with
    | Some answer -> answer
    | None ->
        let answer =
          if i = Array.length patterns then j = Array.length names
          else if patterns.(i) = "**" then
            matches (i + 1) j ||
            (j < Array.length names && matches i (j + 1))
          else j < Array.length names &&
            glob_segment patterns.(i) names.(j) && matches (i + 1) (j + 1) in
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
  pattern : string;
  directory_only : bool;
  negated : bool;
  basename_only : bool;
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
            else Some { base; pattern; directory_only; negated;
                        basename_only = not anchored && not (String.contains pattern '/') })
  with Unix.Unix_error (Unix.ENOENT, _, _) -> []

let ignored rules relative is_directory =
  List.fold_left (fun excluded rule ->
    let local =
      if rule.base = "" then Some relative
      else
        let prefix = rule.base ^ "/" in
        if String.length relative > String.length prefix &&
           String.sub relative 0 (String.length prefix) = prefix then
          Some (String.sub relative (String.length prefix)
                  (String.length relative - String.length prefix))
        else None in
    match local with
    | None -> excluded
    | Some local ->
        if rule.directory_only && not is_directory then excluded
        else
          let matches =
            if rule.basename_only then glob_segment rule.pattern (Filename.basename local)
            else glob_parts rule.pattern local in
          if matches then not rule.negated else excluded) false rules

let matching_glob pattern relative =
  if String.contains pattern '/' then glob_parts pattern relative
  else glob_segment pattern (Filename.basename relative)

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

let walk ?(hidden = true) ?cancel ?visit_directory root relative visit =
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
      let child = Filename.concat absolute name in
      let relative = if prefix = "" then name else prefix ^ "/" ^ name in
      try
        match (Unix.lstat child).Unix.st_kind with
        | Unix.S_DIR when not (skip_directory name) &&
                          (hidden || name.[0] <> '.') &&
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

let append_bounded output text limit =
  if Buffer.length output + String.length text <= limit then (Buffer.add_string output text; true)
  else false

let list_files root args =
  let relative = optional_string "path" "." args in
  let output = Buffer.create 4096 in
  let count = ref 0 and overflow = ref false in
  let walk_limit = walk root relative (fun name _ ->
    if !count < 500 && not !overflow then
      if append_bounded output (name ^ "\n") (max_read_bytes - 128) then incr count
      else overflow := true
    else overflow := true) in
  if walk_limit || !overflow then Buffer.add_string output "[truncated; narrow the path]\n";
  if !count = 0 && not (walk_limit || !overflow) then "No files found" else Buffer.contents output
(* Str's backtracking is not time-bounded. Limit candidate lines and allow
   only one repetition operator; reject quantified groups and backreferences. *)
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
  let walk_limit = walk ~hidden root relative (fun name _ ->
    if matching_glob pattern name then
      if !count < limit && not !overflow then
        if append_bounded output (name ^ "\n") (max_read_bytes - 128) then incr count
        else overflow := true
      else overflow := true) in
  if walk_limit || !overflow then Buffer.add_string output "[truncated; narrow the glob or path]\n";
  if !count = 0 && not (walk_limit || !overflow) then "No files found" else Buffer.contents output

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
  let walk_limit = walk ~hidden root relative (fun name path ->
    if file_glob <> "" && not (matching_glob file_glob name) then ()
    else
      let size = (Unix.stat path).Unix.st_size in
      if size > max_write_bytes then ()
      else if !scanned + size > (if regex then 262_144 else max_search_bytes) then truncated := true
      else if not !truncated then (
        scanned := !scanned + size;
        let contents = Workspace_path.read_bounded path max_write_bytes in
        if not (String.contains contents '\000') then (
          let length = String.length contents in
          let rec lines start number =
            if start < length && not !truncated then (
              let finish = try String.index_from contents start '\n' with Not_found -> length in
              if regex && finish - start > max_regex_line then truncated := true
              else (
                let line = String.sub contents start (finish - start) in
                let match_line = if case_sensitive then line else String.lowercase_ascii line in
                let matched = match compiled with
                  | None -> find_from match_line match_query 0 <> None
                  | Some expression ->
                      (try ignore (Str.search_forward expression match_line 0); true
                       with Not_found -> false) in
                if matched then (
                  if !matches >= limit then truncated := true
                  else (
                    let preview = if String.length line > 240 then String.sub line 0 240 ^ "..." else line in
                    if append_bounded output (Printf.sprintf "%s:%d:%s\n" name number preview)
                        (max_read_bytes - 128) then incr matches
                    else truncated := true)));
              lines (finish + 1) (number + 1))
          in lines 0 1))) in
  if walk_limit || !truncated then Buffer.add_string output "[truncated; narrow the path, glob or query]\n";
  if !matches = 0 && not (walk_limit || !truncated) then "No matches found" else Buffer.contents output

let search root args = search_matches root args ~regex:false
let grep root args = search_matches root args ~regex:true
let read_text_page root relative args =
  let path = Workspace_path.regular_path root relative in
  let requested_offset = optional_int "offset" 0 ~minimum:0 ~maximum:max_int args in
  let line = optional_int "line" 0 ~minimum:1 ~maximum:max_int args in
  if line > 0 && field "offset" args <> `Null then fail "use either line or offset, not both";
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


let mobile_project ?cancel root =
  let max_candidates = 100 in
  let candidates = ref [] and candidate_count = ref 0
  and truncated = ref false in
  let directories = Hashtbl.create 256 in
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
    | name when Filename.check_suffix name ".xcscheme" ->
        add_shared_scheme relative
    | _ -> () in

  let walk_truncated = walk ~hidden:true ?cancel ~visit_directory
    root "." visit_file in

  if walk_truncated then truncated := true;
  let candidates = List.rev !candidates in
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
      append "Detected mobile project evidence and suggested commands (not executed):\n";
    append (name ^ "\n" ^
      String.concat "" (List.map (fun command -> "  " ^ command ^ "\n") commands)) in
  let add_diagnostic text = append (text ^ "\n") in
  List.sort String.compare !oversized_schemes |> List.iter (fun path ->
    add_diagnostic
      (Printf.sprintf
        "Ignored oversized Xcode shared scheme: %s (exceeds %d-byte limit; no scheme commands suggested)."
        path max_write_bytes));

  let directory path =
    match Filename.dirname path with "." -> "" | parent -> parent in
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
  let render_xcode kind flag bundle manifest =
    let name = Filename.basename bundle in
    let target = flag ^ " " ^ shell_quote name in
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
    let scheme_commands = List.concat_map (fun path ->
      let scheme = shell_quote (scheme_name path) in
      let target = target ^ " -scheme " ^ scheme in
      [command_in bundle ("xcodebuild " ^ target ^ " build");
       command_in bundle ("xcodebuild " ^ target ^ " test")]) schemes in
    add_stack ("Xcode " ^ kind ^ ": " ^ manifest ^ "\n" ^ provenance)
      (command_in bundle ("xcodebuild -list " ^ target) :: scheme_commands) in

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
  List.iter (function
    | Xcode_workspace (bundle, manifest) ->
        render_xcode "workspace" "-workspace" bundle manifest
    | Xcode_project (bundle, manifest) ->
        render_xcode "project" "-project" bundle manifest
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
                      | `Assoc entries -> List.mem_assoc name entries
                      | _ -> false) ["dependencies"; "devDependencies"] in
                  let scripts = field "scripts" json in
                  let has_script name =
                    match scripts with
                    | `Assoc entries -> List.mem_assoc name entries
                    | _ -> false in
                  let script_commands =
                    (if has_script "build" then ["npm run build"] else []) @
                    (if has_script "test" then ["npm test"] else []) in
                  if has_dependency "expo" then
                    add_stack ("Expo: " ^ path)
                      (List.map (command_in path)
                        ("npx expo export" :: script_commands))
                  else if has_dependency "react-native" then
                    add_stack ("React Native: " ^ path ^
                      " (use Xcode/Gradle above for native builds)")
                      (List.map (command_in path) script_commands)
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
  if !stack_count = 0 then
    add_diagnostic "No supported mobile project manifests found under workspace.";
  let result = Buffer.contents output in
  if !truncated then result ^ truncation_notice else result

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
  Yojson.Basic.to_string (`Assoc [
    "provider", `String response.provider;
    "query", `String response.query;
    "page", `Int response.page;
    "citations", `List (List.map (fun citation -> `String citation) response.citations);
    "results", `List (List.map result response.results)
  ])

let web_fetch ~approved ?cancel args =
  require_explicit_approval approved;
  let url = required_string "url" args in
  let max_bytes = optional_int "max_bytes" Web_search.max_content_bytes
    ~minimum:1 ~maximum:Web_search.max_content_bytes args in
  let page = Web_search.fetch_url ?cancel ~max_bytes url () in
  Yojson.Basic.to_string (`Assoc [
    "source_url", `String page.source_url;
    "markdown", `String page.markdown
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
  | "mobile_project" -> mobile_project ?cancel root
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

let is_shell_tool = function
  | "run_command" | "start_process" | "start_shell" -> true
  | _ -> false

let requires_explicit_approval ~name ~args =
  match name with
  | "run_command" | "start_process" | "start_shell"
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
  | "read_file" ->
      (match field "path" args with
       | `String path -> starts_with (String.lowercase_ascii path) "https://"
       | _ -> false)
  | _ -> false

let non_reversible_tool ~name ~args =
  match name with
  | "run_command" | "start_process" | "start_shell"
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
  schema "token_count" "Count tokens using exact vendored cl100k_base or o200k_base ranks; does not contact a provider or infer an upstream tokenizer."
    ["encoding", enum_string_field "Exact tiktoken encoding" ["cl100k_base"; "o200k_base"];
     "text", bounded_string_field "Text to count (maximum 1 MiB)" Native_tokenizer.max_input_bytes]
    ["encoding"; "text"];
  schema "mobile_project" "Inventory bounded mobile manifests and map SwiftPM test roots, literal Gradle modules and conventional source roots, and Flutter pubspec kind and observed native host roots; never execute project code or infer build readiness."
    [] [];
  schema "read_file" "Read bounded workspace files, directories, documents, archives, notebooks, SQLite, owned artifacts, managed worktrees, or public HTTPS URLs. HTTPS fetches send no credentials and require approval."
    ["path", string_field "Workspace-relative path or supported local://, artifact://, worktree://, or HTTPS source";
     "offset", integer_field "Byte offset for ordinary local text files (default 0; exclusive with line)" 0 max_int;
     "line", integer_field "One-based line for ordinary local text files (exclusive with offset)" 1 max_int;
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
  schema "web_search" "Search with the explicitly configured pinned Brave or Tavily provider. Requires network approval; returns source URLs, citations, and provider provenance."
    ["query", bounded_string_field "Search query sent to the selected provider" Web_search.max_query_bytes;
     "page", integer_field "Provider page/offset (default 0)" 0 Web_search.max_page;
     "count", integer_field "Maximum results (default 5)" 1 Web_search.max_results] ["query"];
  schema "web_fetch" "Fetch a public HTTPS page without credentials or redirects and convert bounded HTML to Markdown. Requires separate network approval."
    ["url", bounded_string_field "Credential-free public HTTPS URL on port 443" 4096;
     "max_bytes", integer_field "Maximum HTML and converted output bytes (default 65536)"
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
  let present name = field name args <> `Null in
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
        let priority = Option.value ~default:"(unset)"
          (Sys.getenv_opt "PAVE_WEB_SEARCH_PROVIDER_PRIORITY") in
        let configured = [
          "brave", "BRAVE_SEARCH_API_KEY";
          "tavily", "TAVILY_API_KEY"
        ] |> List.filter_map (fun (provider, variable) ->
          match Sys.getenv_opt variable with
          | Some value when value <> "" -> Some provider
          | _ -> None) in
        "Sends this query to the first configured public search provider in the explicit priority order; its API credential is sent only to that provider.",
        ["Query: " ^ quoted "query" "(missing)" args;
         "Provider priority: " ^ priority;
         "Credentials present for: " ^
           (if configured = [] then "none" else String.concat ", " configured);
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
  { Approval.tool_name = name; tier = decision.tier; impact; details;
    reason = decision.reason }

let validate_arguments ~name ~args =
  let schema =
    match List.find_opt (fun json -> function_name json = `String name) definitions with
    | Some json -> Protocol.member "parameters" (Protocol.member "function" json)
    | None -> fail ("unknown tool: " ^ name) in
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
         then fail (label ^ " is outside its allowed range")
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
  validate_value "arguments" schema args

type prepared_execution =
  ?cancel:(unit -> bool) -> ?on_progress:(int -> unit) -> ?approved:bool -> unit ->
  Protocol.content_block list

let session_tool_names = [
  "start_process"; "start_shell"; "process_list"; "process_output";
  "process_wait"; "process_ready"; "process_stdin"; "process_close_stdin";
  "process_kill"; "worktree_list"; "worktree_status"; "worktree_diff";
  "worktree_history"; "worktree_create"; "worktree_commit"; "worktree_remove";
  "lsp_start"; "lsp"; "workspace_eval";
  "ssh_open"; "ssh_close"; "ssh_read"; "ssh_write"; "ssh_command";
  "dap_start"; "dap"
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
  | Workspace_ssh.Error message | Native_tokenizer.Error message ->
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
          | "token_count" -> token_count args
          | "mobile_project" -> mobile_project ?cancel root
          | _ -> assert false in
        [Protocol.Text result]
      with
      | Cancelled | Workspace_dap.Cancelled -> raise Cancelled
      | (Workspace_process.Error _ | Workspace_git.Error _ |
         Workspace_reader.Error _ | Repository_security.Error _
         | Web_search.Error _ | Native_services.Error _
         | Workspace_lsp.Error _ | Workspace_dap.Error _
         | Workspace_dap.Not_approved _ | Workspace_eval.Error _
         | Workspace_ssh.Error _ | Native_tokenizer.Error _) as exn ->
          (match cancel with
           | Some cancelled when cancelled () -> raise Cancelled
           | _ -> [Protocol.Text (error_message exn)])
      | exn ->
          (match cancel with
           | Some cancelled when cancelled () -> raise Cancelled
           | _ -> [Protocol.Text (error_message exn)]) in
    Ok execute
  with
  | Cancelled | Workspace_dap.Cancelled -> raise Cancelled
  | (Workspace_process.Error _ | Workspace_git.Error _ |
     Workspace_reader.Error _ | Workspace_lsp.Error _ | Workspace_dap.Error _
     | Workspace_dap.Not_approved _ | Workspace_eval.Error _
     | Workspace_ssh.Error _) as exn ->
      (match cancel with
       | Some cancelled when cancelled () -> raise Cancelled
       | _ -> Error (error_message exn))
  | exn ->
      (match cancel with
       | Some cancelled when cancelled () -> raise Cancelled
       | _ -> Error (error_message exn))


let execute ?cancel ?on_progress ?preflight ?context ?(approved = false)
    ~root ~name ~args () =
  let display execute = Protocol.display_content_blocks
    (execute ?cancel ?on_progress ?approved:(Some approved) ()) in
  match prepare ?cancel ?context ~root ~name ~args () with
  | Error result -> result
  | Ok execute ->
      try
        match preflight with
        | Some check ->
            (match check () with
             | Some message -> message
             | None -> display execute)
        | None -> display execute
      with
      | Cancelled -> raise Cancelled
      | exn -> error_message exn

