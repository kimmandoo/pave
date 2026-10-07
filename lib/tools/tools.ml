
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

type android_inventory = {
  root : string;
  subroot : string;
  devices : Workspace_android_devices.device list;
}
type android_avd_inventory = { avd_root : string; avd_subroot : string; avd_names : string list }


type guarded_edit_evidence = { before_sha256 : string; after_sha256 : string }
type mobile_lifecycle_handler_cache = {
  build_hash : string;
  handlers : Workspace_mobile_app_lifecycle.handler list;
}
type mobile_performance_target = { build_hash : string; pid : int }
type mobile_performance_templates = { template_build_hash : string; names : string list }
type mobile_device_inventory_cache = {
  device_root : string;
  device_platform : Workspace_mobile_run.platform;
  device_subroot : string;
  device_scheme : string;
  device_inventory : Workspace_mobile_device_lifecycle.inventory;
}
type mobile_xctest_plans = (string, string) Hashtbl.t





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
  mutable android_inventory : android_inventory option;
  mutable android_avds : android_avd_inventory option;
  mobile_device_inventories : (string, mobile_device_inventory_cache) Hashtbl.t;
  mobile_device_booting : (string, Workspace_mobile_device_lifecycle.booting) Hashtbl.t;
  mobile_device_managed : (string, Workspace_mobile_device_lifecycle.managed) Hashtbl.t;
  mobile_xctest_plans : mobile_xctest_plans;
  mutable next_mobile_device_session : int;
  mutable next_mobile_device_inventory : int;
  mutable next_mobile_device_process : int;

  mobile_run_manager : Workspace_mobile_run.manager;
  node_server_manager : Workspace_node_server.manager;
  mobile_environment_plans : (string, Workspace_mobile_environment.plan) Hashtbl.t;
  mutable next_mobile_environment_plan : int;
  mobile_lifecycle_handlers : (string, mobile_lifecycle_handler_cache) Hashtbl.t;
  mobile_lifecycle_observations : (string, Workspace_mobile_app_lifecycle.observation) Hashtbl.t;
  mobile_lifecycle_generations : (string, int) Hashtbl.t;
  mobile_performance_targets : (string, mobile_performance_target) Hashtbl.t;
  mobile_performance_templates : (string, mobile_performance_templates) Hashtbl.t;
  mutable next_mobile_trace : int;

  guarded_edit_evidence : (string, guarded_edit_evidence) Hashtbl.t;
  record_file_change : path:string -> before:string -> after:string -> unit;
  mutable closed : bool;
}

type file_location = {
  root : string;
  path : string;
  worktree_id : string option;
}
let guarded_evidence_key ~root ~path =
  let absolute = try Workspace_path.regular_path root path
    with Workspace_path.Error message -> raise (Tool_error message) in
  root ^ "\000" ^ absolute

let max_guarded_edit_evidence = 256

let starts_with text prefix =
  String.length text >= String.length prefix &&
  String.sub text 0 (String.length prefix) = prefix

let mobile_verify_evidence_available context ~root =
  let root = Workspace_path.root_path root in
  let prefix = root ^ "\000" in
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.fold (fun key _ found -> found || starts_with key prefix)
      context.guarded_edit_evidence false)

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
    android_inventory = None;
    android_avds = None;
    mobile_device_inventories = Hashtbl.create 8;
    mobile_device_booting = Hashtbl.create 8;
    mobile_device_managed = Hashtbl.create 8;
    mobile_xctest_plans = Hashtbl.create 16;
    next_mobile_device_session = 0;
    next_mobile_device_inventory = 0;
    next_mobile_device_process = 0;

    mobile_run_manager = Workspace_mobile_run.create_manager ();
    node_server_manager = Workspace_node_server.create_manager ~process_manager;
    mobile_environment_plans = Hashtbl.create 8;
    next_mobile_environment_plan = 0;
    mobile_lifecycle_handlers = Hashtbl.create 8;
    mobile_lifecycle_observations = Hashtbl.create 8;
    mobile_lifecycle_generations = Hashtbl.create 8;
    mobile_performance_targets = Hashtbl.create 8;
    mobile_performance_templates = Hashtbl.create 8;
    next_mobile_trace = 0;

    guarded_edit_evidence = Hashtbl.create 16;
    record_file_change; closed = false }

let close_session_context context =
  if not context.closed then (
    context.closed <- true;
    Mutex.lock context.xcode_lock;
    context.xcode_discovery <- None;
    Mutex.unlock context.xcode_lock;
    Mutex.lock context.mobile_lock;
    context.mobile_discovery <- None;
    context.android_inventory <- None;
    context.android_avds <- None;
    Hashtbl.clear context.mobile_device_inventories;
    Hashtbl.clear context.mobile_device_booting;
    Hashtbl.clear context.mobile_device_managed;
    Hashtbl.clear context.mobile_xctest_plans;
    Hashtbl.clear context.mobile_environment_plans;
    Hashtbl.clear context.mobile_lifecycle_handlers;
    Hashtbl.clear context.mobile_lifecycle_observations;
    Hashtbl.clear context.mobile_lifecycle_generations;
    Hashtbl.clear context.mobile_performance_targets;
    Hashtbl.clear context.mobile_performance_templates;
    Hashtbl.clear context.guarded_edit_evidence;
    Mutex.unlock context.mobile_lock;
    let ignore_failure action = try action () with _ -> () in
    ignore_failure (fun () -> Workspace_lsp.close_manager context.lsp_manager);
    ignore_failure (fun () -> Workspace_dap.close_manager context.dap_manager);
    ignore_failure (fun () -> Workspace_browser.close_manager context.browser_manager);
    ignore_failure (fun () -> Workspace_mobile_run.close_manager context.mobile_run_manager);
    ignore_failure (fun () -> Workspace_node_server.close_manager context.node_server_manager);
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
let required_bool name args =
  match field name args with
  | `Bool value -> value
  | `Null -> fail ("missing required boolean argument: " ^ name)
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
(* Sensitive-mutation review plumbing (M21b/M22b). A call that will write
   files computes the proposed target set before publishing; when the
   classifier flags the change it must match the exact review the user
   approved, otherwise nothing is written. *)
let file_state absolute =
  match (try Some (Unix.lstat absolute) with Unix.Unix_error _ -> None) with
  | Some stat when stat.Unix.st_kind = Unix.S_REG ->
      (match (try Some (Workspace_path.read_bounded absolute max_write_bytes)
              with
              | Workspace_path.Error _ | Sys_error _ | Unix.Unix_error _ -> None)
       with
       | Some contents -> Workspace_edit.sha256 contents, Some contents
       | None -> Sensitive_mutation.unreadable_sha256, None)
  | Some _ -> Sensitive_mutation.unreadable_sha256, None
  | None -> Sensitive_mutation.new_file_sha256, None

let confirmed_before ~root ~path original_sha256 =
  match (try Some (Workspace_edit.read_snapshot ~root ~path)
         with Workspace_edit.Error _ -> None) with
  | Some snapshot when snapshot.Workspace_edit.sha256 = original_sha256 ->
      Some snapshot.Workspace_edit.contents
  | _ -> None

let proposed_of_prepared (prepared : Workspace_edit.prepared) =
  { Sensitive_mutation.path = prepared.preview.path;
    original_sha256 = prepared.before.sha256;
    before = Some prepared.before.contents;
    after = prepared.after }

let write_proposal ~root ~path ~content =
  let absolute = Workspace_path.writable_path root path in
  let original_sha256, before = file_state absolute in
  absolute, { Sensitive_mutation.path; original_sha256; before; after = content }

(* LSP previews record only hashes plus proposed content; the confirmed
   original bytes are recovered through the snapshot check. Files marked
   unchanged never reach the disk, so they are excluded. *)
let lsp_apply_proposals ~root files =
  Workspace_lsp.preview_file_rows files
  |> List.filter_map (fun row ->
    if Protocol.member "changed" row = `Bool true then
      let path = required_string "path" row in
      let original_sha256 = required_string "original_sha256" row in
      Some { Sensitive_mutation.path;
             original_sha256;
             before = confirmed_before ~root ~path original_sha256;
             after = required_string "content" row }
    else None)

(* Gate one prepared write on the supplied exact-content review. A supplied
   review always binds: the current proposed bytes must match its targets
   exactly, or nothing is written. Without a review, ordinary diffs keep the
   existing behavior while sensitive or unresolved diffs fail closed. *)
let require_sensitive_review ~approved ~sensitive_review ~root proposals =
  let targets =
    List.map Sensitive_mutation.target_of_proposed proposals in
  match sensitive_review with
  | Some approved_review ->
      if not approved then
        fail "sensitive workspace change requires explicit interactive approval";
      if not (Sensitive_mutation.targets_match
          approved_review.Approval.targets targets) then
        fail "the approved sensitive change no longer matches the workspace; request a fresh review";
      true
  | None ->
      (match Sensitive_mutation.review ~root proposals with
       | None -> false
       | Some _ ->
           if not approved then
             fail "sensitive workspace change requires explicit interactive approval";
           fail "sensitive workspace change requires exact-content approval")
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

let scanning ?cancel work =
  let self = Thread.id (Thread.self ()) in
  if Atomic.get scan_owner = self then work ()
  else (
    let rec acquire () =
      (match cancel with Some cancelled when cancelled () -> raise Cancelled | _ -> ());
      if not (Mutex.try_lock scan_lock) then (Thread.delay 0.01; acquire ()) in
    acquire ();
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
  scanning ?cancel (fun () -> fuzzy_file_search ?cancel root args)

let append_bounded output text limit =
  if Buffer.length output + String.length text <= limit then (Buffer.add_string output text; true)
  else false

let list_files ?cancel root args =
  let relative = optional_string "path" "." args in
  let output = Buffer.create 4096 in
  let count = ref 0 and overflow = ref false in
  let walk_limit = walk ?cancel ~stop:(fun () -> !overflow) root relative (fun name _ ->
    if !count < 500 && not !overflow then
      if append_bounded output (name ^ "\n") (max_read_bytes - 128) then incr count
      else overflow := true
    else overflow := true) in
  if walk_limit || !overflow then Buffer.add_string output "[truncated; narrow the path]\n";
  if !count = 0 && not (walk_limit || !overflow) then "No files found" else Buffer.contents output
(* Str's backtracking is not time-bounded. Limit candidate lines and allow
   only one repetition operator; reject quantified groups and backreferences. *)
let list_files ?cancel root args = scanning ?cancel (fun () -> list_files ?cancel root args)

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


let glob ?cancel root args =
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
  let walk_limit = walk ?cancel ~hidden ~stop:(fun () -> !overflow) ~descend root relative (fun name _ ->
    if wanted name then
      if !count < limit && not !overflow then
        if append_bounded output (name ^ "\n") (max_read_bytes - 128) then incr count
        else overflow := true
      else overflow := true) in
  if walk_limit || !overflow then Buffer.add_string output "[truncated; narrow the glob or path]\n";
  if !count = 0 && not (walk_limit || !overflow) then "No files found" else Buffer.contents output

let glob ?cancel root args = scanning ?cancel (fun () -> glob ?cancel root args)

let search_matches ?cancel root args ~regex =
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
  let scope = List.filter (fun part -> part <> "" && part <> ".")
    (String.split_on_char '/' relative) |> String.concat "/" in
  let prefix = if scope = "" then "" else scope ^ "/" in
  let wanted = if file_glob = "" then fun _ -> true else
    let matches = matching_glob file_glob in
    fun name ->
      let scoped = if prefix <> "" && starts_with name prefix then
        String.sub name (String.length prefix) (String.length name - String.length prefix)
        else name in
      matches scoped in
  let walk_limit = walk ?cancel ~hidden ~stop:(fun () -> !truncated) root relative (fun name path ->
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
                  (match cancel with Some cancelled when cancelled () -> raise Cancelled | _ -> ());
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
                (match cancel with Some cancelled when cancelled () -> raise Cancelled | _ -> ());
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

let search ?cancel root args = scanning ?cancel (fun () -> search_matches ?cancel root args ~regex:false)
let grep ?cancel root args = scanning ?cancel (fun () -> search_matches ?cancel root args ~regex:true)
let read_text_page ?cancel root relative args =
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
    let check_cancel () =
      match cancel with Some cancelled when cancelled () -> raise Cancelled | _ -> () in
    check_cancel ();
    if requested_offset > size then
      fail (Printf.sprintf "offset %d exceeds file size %d" requested_offset size);
    let buffer = Bytes.create 8192 in
    let position = ref 0 and current_line = ref 1 in
    while !position < size &&
          (if line > 0 then !current_line < line else !position < requested_offset) do
      check_cancel ();
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
        check_cancel ();
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
    | Some location -> read_text_page ?cancel location.root location.path args
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
let write_file ~approved ~sensitive_review root args =
  let relative = required_string "path" args in
  let content = required_string "content" args in
  let absolute, proposal = write_proposal ~root ~path:relative ~content in
  let reviewed = require_sensitive_review ~approved ~sensitive_review ~root
      [proposal] in
  (* A reviewed write republishes only while the target still matches the
     approved original: existing bytes keep their hash and an absent file
     stays absent. *)
  if reviewed then (
    let current_sha256, _ = file_state absolute in
    if current_sha256 <> proposal.Sensitive_mutation.original_sha256 then
      fail "workspace file changed since the sensitive-change review");
  Workspace_path.atomic_write absolute content;
  Printf.sprintf "Wrote %d bytes to %s" (String.length content) relative

let edit_file ~approved ~sensitive_review root args =
  let relative = required_string "path" args in
  let old_text = required_string "old_string" args in
  let new_text = required_string "new_string" args in
  if old_text = "" then fail "old_string must not be empty";
  let prepared =
    try Workspace_edit.prepare_unique ~root ~path:relative
        ~old_text ~new_text
    with Workspace_edit.Error message -> fail message in
  ignore (require_sensitive_review ~approved ~sensitive_review ~root
    [proposed_of_prepared prepared]);
  (try Workspace_edit.write_prepared prepared
   with Workspace_edit.Error message -> fail message);
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
  let consistency_files = ref [] in
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
    | _ when Workspace_rn_consistency.relevant relative ->
        consistency_files := relative :: !consistency_files
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
              [];
            if !flutter_sdk then
              (try
                Workspace_flutter_channels.report_lines ~root
                  ~subroot:package_root
                |> List.iter add_diagnostic
              with
              | Workspace_flutter_channels.Error message ->
                  add_diagnostic
                    ("Flutter channel pairing unavailable: " ^ message)
              | Unix.Unix_error _ ->
                  add_diagnostic
                    "Flutter channel pairing unavailable: workspace scan failed"))
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
                       let consistency = if not (can_suggest ()) then [] else
                         let package_dirs = List.filter_map (function
                           | Node_manifest manifest -> Some (directory manifest)
                           | _ -> None) candidates in
                         let test_hint =
                           if List.mem "test" scripts then
                             (match selection with
                              | Some manager ->
                                  "Observed declared test script: " ^
                                    command_in path
                                      (if manager = "npm" then
                                         "npm run 'test'"
                                       else manager ^ " 'test'") ^
                                    " (preview only; run via approved mobile_check)"
                              | None ->
                                  "Declared 'test' script exists but the package manager is ambiguous; choose one explicitly before running it.")
                           else
                             "No declared 'test' script in " ^ path ^
                               "; no test suggested." in
                         List.map (fun line -> "  " ^ line)
                           (Workspace_rn_consistency.report ~root
                              ~subroot:package_root ~platform
                              ~files:!consistency_files ~directories
                              ~package_dirs ~expo:(kind = "Expo") ~test_hint) in
                       add_stack
                         (String.concat "\n"
                           (("  " ^ kind ^ ": " ^ path) ::
                            ("  Package root: " ^ relative_label package_root) ::
                            lock_lines @ hosts @ script_lines @ consistency @
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
  scanning ?cancel (fun () -> mobile_project ?cancel root args)

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
      let derived = optional_string "derived_data_path" "" args in
      prefix ^ " -scheme " ^ shell_quote scheme ^
      " -destination " ^
      shell_quote ("platform=iOS Simulator,id=" ^ destination) ^
      (if derived = "" then "" else
        " -derivedDataPath " ^ shell_quote derived) ^
      " CODE_SIGNING_ALLOWED=NO " ^ action
  | _ -> fail "unsupported Xcode preflight action"

let xcode_preflight ~approved ?cancel ?on_progress ?context root args =
  if not approved then fail "this action requires explicit interactive approval";
  let context = require_session_context context in
  let root = Workspace_path.root_path root in
  let bundle = required_string "subroot" args in
  let action = required_string "action" args in
  let derived_data_path = optional_string "derived_data_path" "" args in
  if derived_data_path <> "" then (
    let relative = if Filename.is_relative derived_data_path then
        derived_data_path
      else
        let prefix = root ^ Filename.dir_sep in
        if not (starts_with derived_data_path prefix) then
          fail "Xcode derived data path must be inside the workspace";
        String.sub derived_data_path (String.length prefix)
          (String.length derived_data_path - String.length prefix) in
    ignore (Workspace_path.checked_path root relative));
  let command = xcode_command args in
  let manifest = Filename.concat bundle
    (if Filename.check_suffix bundle ".xcworkspace" then
      "contents.xcworkspacedata" else "project.pbxproj") in
  let file = Workspace_path.checked_path root manifest in
  let stat = Unix.lstat file in
  if stat.Unix.st_kind <> Unix.S_REG || stat.Unix.st_size > max_write_bytes then
    fail "selected Xcode bundle has no bounded regular manifest";
  let fingerprint = Workspace_xcode.fingerprint ~root ~bundle in
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
      if discovery.fingerprint <> fingerprint then (
        context.xcode_discovery <- None;
        fail "Xcode project inputs changed since discovery; discover schemes again");
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
            fail "simulator destination was not discovered for this scheme";
          if action = "test" then
            match List.assoc_opt scheme discovery.simulators with
            | None ->
                fail "approve compatible simulator inventory for this scheme before testing"
            | Some inventoried ->
                if not (List.mem destination inventoried) then
                  fail "selected simulator was absent from the approved inventory; refresh simulators"));
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
        match fingerprint with
        | Workspace_xcode.Complete _ ->
            context.xcode_discovery <- Some
              { Workspace_xcode.root; bundle; fingerprint; schemes;
                destinations = []; simulators = [] };
            "Xcode scheme discovery: " ^ outcome ^ "\nVerified schemes: " ^
            (if schemes = [] then
               "none; select another project/workspace or configure a shared scheme"
             else String.concat ", " schemes)
        | Workspace_xcode.Unresolved ->
            "Xcode scheme discovery: " ^ outcome ^
            "\nNo verified schemes: shared scheme files or directly " ^
            "referenced .xcodeproj manifests are missing, escaping the " ^
            "workspace, not regular, or oversized. Resolve them and approve " ^
            "scheme discovery again.")
      else "Xcode scheme discovery: " ^ outcome ^
        (if result.truncated then " (output truncated)" else "") ^
        "\n" ^ result.output)
    else if action = "destinations" then (
      let discovery = Option.get context.xcode_discovery in
      discovery.destinations <- List.remove_assoc scheme discovery.destinations;
      discovery.simulators <- List.remove_assoc scheme discovery.simulators;
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
    else if action = "simulators" then (
      let discovery = Option.get context.xcode_discovery in
      discovery.simulators <- List.remove_assoc scheme discovery.simulators;
      if successful then (
        let destinations = Option.get
          (List.assoc_opt scheme discovery.destinations) in
        let devices = Workspace_xcode.compatible_simulators
          ~destinations result.output in
        discovery.simulators <- (scheme,
          List.map (fun (device : Workspace_xcode.simulator) -> device.id)
            devices) :: discovery.simulators;
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
         else "; no devices accepted"))
    else
      let locations = if result.termination = Workspace_process.Exited 0 then []
        else Workspace_swift_diagnostics.locations ~root ~cwd result.output in
      "Xcode " ^ action ^ ": " ^ outcome ^ " (scheme " ^ scheme ^ ")" ^
      (if result.truncated then " (output truncated; incomplete result)" else "") ^
      (if locations = [] then "" else
         "\nChecked Swift errors:\n" ^ String.concat "\n" locations) ^
      "\n" ^ result.output)

let flutter_integration_device ~context ~root args =
  if optional_string "stack" "" args <> "flutter" ||
     optional_string "action" "" args <> "integration_test" then None
  else
    let root = Workspace_path.root_path root in
    let subroot = required_string "subroot" args in
    let session = Workspace_mobile_run.get context.mobile_run_manager
      (required_string "session_id" args) in
    if session.root <> root then
      fail "Flutter integration test session belongs to a different workspace";
    if session.platform <> Workspace_mobile_run.Android then
      fail "Flutter integration tests require the selected Android app session";
    let expected_android_root =
      if subroot = "." then "android" else subroot ^ "/android" in
    if session.subroot <> expected_android_root then
      fail "Flutter integration test package does not match the selected Android app's host project";
    let prefix = if subroot = "." then "" else subroot ^ "/" in
    if not (starts_with session.app_path prefix) then
      fail "selected Flutter app artifact is outside the integration-test package";
    if not (List.mem session.state
        [Workspace_mobile_run.Installed; Workspace_mobile_run.Running]) then
      fail "Flutter integration tests require the selected app to be installed on its session device";
    Mutex.lock context.mobile_lock;
    let ready = Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock)
      (fun () -> match context.android_inventory with
        | Some inventory when inventory.root = root &&
            inventory.subroot = session.subroot ->
            List.exists (fun (device : Workspace_android_devices.device) ->
              device.serial = session.device && device.emulator &&
              device.state = Workspace_android_devices.Ready) inventory.devices
        | _ -> false) in
    if not ready then
      fail "Flutter integration test device is absent from the current approved ready-emulator inventory";
    Some session.device
let mobile_command ?ready_device_id ~root args =
  let stack = required_string "stack" args in
  let action = required_string "action" args in
  let subroot = required_string "subroot" args in
  let target = optional_string "target" "" args in
  let manager = optional_string "manager" "" args in
  if stack <> "gradle" && optional_string "serial" "" args <> "" then
    fail "an emulator serial applies only to Gradle instrumented runs";
  match stack with
  | "swiftpm" ->
      Workspace_swiftpm_focus.command ~root ~subroot ~action ~target
  | "gradle" ->
      Workspace_gradle_focus.command ~root ~subroot ~action ~task:target
        ~serial:(optional_string "serial" "" args)
  | "flutter" ->
      Workspace_flutter_focus.command ?ready_device_id ~root ~subroot ~action ~target ()
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
    | "flutter" ->
        (if subroot = "" || subroot = "." then "" else subroot ^ "/") ^
        "pubspec.yaml"
    | _ -> fail "no discovery manifest for this mobile stack" in
  let path = Workspace_path.checked_path root relative in
  let stat = Unix.lstat path in
  if stat.Unix.st_kind <> Unix.S_REG then
    fail "mobile discovery manifest is not a regular file";
  Digestif.SHA256.(
    to_hex (digest_string (Workspace_path.read_bounded path max_write_bytes)))

let mobile_discovery_require context ~stack ~root ~subroot ~manifest_hash ~target =
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    match context.mobile_discovery with
    | Some state when state.stack = stack && state.root = root &&
        state.subroot = subroot && state.manifest_hash = manifest_hash &&
        List.mem target state.choices -> ()
    | _ -> fail "approve fresh discovery for this exact mobile project and target first")

let mobile_check ~approved ?cancel ?on_progress ?context root args =
  if not approved then fail "mobile project code requires explicit interactive approval";
  let context = require_session_context context in
  let root = Workspace_path.root_path root in
  let stack = required_string "stack" args
  and action = required_string "action" args
  and subroot = required_string "subroot" args in
  let flutter_discovery = stack = "flutter" && action = "discover" in
  let ready_device_id = flutter_integration_device ~context ~root args in
  let command, cwd =
    if flutter_discovery then ("", if subroot = "" || subroot = "." then root
      else Workspace_path.checked_path root subroot)
    else mobile_command ?ready_device_id ~root args in
  let discovery_action = (stack = "swiftpm" && action = "discover") ||
    (stack = "gradle" && action = "tasks") || flutter_discovery in
  let needs_discovery = (stack = "swiftpm" && action = "run") ||
    (stack = "gradle" && (action = "run" || action = "instrumented")) ||
    (stack = "flutter" && action = "integration_test") in
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
    if stack = "gradle" && action = "instrumented" then (
      let serial = optional_string "serial" "" args in
      let state_label = function
        | Workspace_android_devices.Ready -> "ready"
        | Workspace_android_devices.Offline -> "offline"
        | Workspace_android_devices.Unauthorized -> "unauthorized"
        | Workspace_android_devices.Unavailable -> "unavailable" in
      let inventory = match context.android_inventory with
        | Some inventory when inventory.root = root &&
                              inventory.subroot = subroot -> inventory
        | _ -> fail ("approve an Android device inventory for this project " ^
            "first; no emulator is selected implicitly") in
      match List.find_opt (fun (device : Workspace_android_devices.device) ->
          device.serial = serial) inventory.devices with
      | Some device when not device.emulator ->
          fail ("selected serial " ^ serial ^
            " is not an emulator; no install or test was attempted")
      | Some device when device.state <> Workspace_android_devices.Ready ->
          fail ("selected emulator " ^ serial ^ " is " ^
            state_label device.state ^
            " in the current inventory; refresh inventory and select a ready emulator")
      | Some _ -> ()
      | None ->
          fail ("emulator " ^ serial ^
            " was absent from the approved inventory; refresh android_devices"));
    if flutter_discovery then (
      let discovery = try Workspace_flutter_focus.discover_integration_tests
          ~root ~subroot
        with Workspace_flutter_focus.Error message -> fail message in
      let choices = List.map (fun (target : Workspace_flutter_focus.integration_target) ->
        target.path) discovery.targets in
      if Some (mobile_manifest ~root ~stack ~subroot) <> manifest_hash then
        fail "Flutter pubspec changed during integration-test discovery; discover again";
      context.mobile_discovery <- Some {
        stack; root; subroot; manifest_hash = Option.get manifest_hash; choices };
      "Mobile Flutter integration-test discovery: available\nTargets:\n" ^
      String.concat "\n" choices
    ) else (
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
          else if stack = "gradle" &&
              (action = "run" || action = "instrumented") then
            Workspace_android_diagnostics.locations ~root ~cwd ~subroot
              ~task:(required_string "target" args) result.output
          else if stack = "flutter" then
            Workspace_flutter_diagnostics.locations ~root ~cwd ~subroot result.output
          else if stack = "node" then
            Workspace_node_diagnostics.locations ~root ~cwd ~subroot result.output
          else [] in
        "Mobile " ^ stack ^ " " ^ action ^ ": " ^ outcome ^ note ^
        (if stack = "gradle" && action = "run" then
           " (selected task " ^ required_string "target" args ^ ")"
         else if stack = "gradle" && action = "instrumented" then
           " (selected task " ^ required_string "target" args ^
           " on " ^ required_string "serial" args ^ ")"
         else if stack = "flutter" && action = "integration_test" then
           " (selected integration test " ^ required_string "target" args ^
           " on " ^ Option.value ~default:"" ready_device_id ^ ")"
         else "") ^
        (if locations = [] then "" else
           "\nChecked " ^ (if stack = "flutter" then "Dart"
             else if stack = "node" then "JS/TS"
             else if stack = "gradle" then "Kotlin/Java" else "Swift") ^
           " errors:\n" ^ String.concat "\n" locations) ^
        "\n" ^ result.output))

let android_device_command ~root args =
  let subroot = required_string "subroot" args in
  let action = required_string "action" args in
  let _, cwd = Workspace_gradle_focus.command ~root ~subroot
    ~action:"tasks" ~task:"" ~serial:"" in
  let command = match action with
    | "avds" -> "emulator -list-avds"
    | "devices" -> "adb devices"
    | _ -> fail "Android inventory action must be avds or devices" in
  command, cwd

let android_devices ~approved ?cancel ?on_progress ?context root args =
  if not approved then fail "Android inventory requires explicit interactive approval";
  let context = require_session_context context in
  let root = Workspace_path.root_path root in
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
  if result.termination <> Workspace_process.Exited 0 || result.truncated then (
    if action = "devices" then (
      Mutex.lock context.mobile_lock;
      Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock)
        (fun () -> context.android_inventory <- None));
    if action = "avds" then (
      Mutex.lock context.mobile_lock;
      Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock)
        (fun () -> context.android_avds <- None));
    prefix ^ " (no device choices; command failed or output was truncated)")
  else if action = "avds" then (
    let names = try Workspace_android_devices.avds result.output
      with Workspace_android_devices.Error message ->
        Mutex.lock context.mobile_lock;
        Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock)
          (fun () -> context.android_avds <- None);
        fail message in
    Mutex.lock context.mobile_lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
      context.android_avds <- Some {
        avd_root = root; avd_subroot = required_string "subroot" args;
        avd_names = names });
    prefix ^ "\nConfigured AVDs (not running; SDK image readiness unknown): " ^
    (if names = [] then "none"
     else String.concat ", " (List.map (Printf.sprintf "%S") names)))
  else
    let devices =
      try Workspace_android_devices.adb_devices result.output
      with Workspace_android_devices.Error _ as error ->
        Mutex.lock context.mobile_lock;
        Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock)
          (fun () -> context.android_inventory <- None);
        raise error in
    Mutex.lock context.mobile_lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock)
      (fun () ->
        context.android_inventory <- Some {
          root; subroot = required_string "subroot" args; devices });
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

let mobile_xctest_require_owned_lifecycle context ~root ~device_session_id
    ~inventory_id ~simulator_id ~target_id =
  if not (Workspace_mobile_device_lifecycle.valid_uuid simulator_id) then
    fail "Native XCTest requires an exact iOS Simulator UUID";
  if target_id <> "ios:" ^ simulator_id then
    fail "Native XCTest lifecycle target does not match the Simulator UUID";
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    let cache = match Hashtbl.find_opt context.mobile_device_inventories inventory_id with
      | Some cache when cache.device_root = root &&
          cache.device_platform = Workspace_mobile_run.Ios &&
          Workspace_mobile_device_lifecycle.inventory_id cache.device_inventory = inventory_id &&
          Workspace_mobile_device_lifecycle.inventory_session_id cache.device_inventory =
            device_session_id -> cache
      | _ -> fail "Native XCTest requires the current exact iOS lifecycle inventory" in
    let inventory = cache.device_inventory in
    if not (List.mem simulator_id inventory.configured_simulator_ids &&
            List.mem simulator_id inventory.ios_destinations) then
      fail "selected Simulator is absent from the current compatible lifecycle inventory";
    (match List.filter (fun (simulator : Workspace_xcode.simulator) ->
        simulator.id = simulator_id) inventory.compatible_simulators with
     | [{ state = "Booted"; _ }] -> ()
     | _ -> fail "selected Simulator is not uniquely booted in the current lifecycle inventory");
    let key = inventory_id ^ "\000" ^ target_id in
    let managed = match Hashtbl.find_opt context.mobile_device_managed key with
      | Some managed -> managed
      | None -> fail "Native XCTest requires an owned lifecycle record for this Simulator" in
    if Workspace_mobile_device_lifecycle.managed_session_id managed <> device_session_id ||
       Workspace_mobile_device_lifecycle.managed_inventory_id managed <> inventory_id ||
       Workspace_mobile_device_lifecycle.target_id
         (Workspace_mobile_device_lifecycle.managed_target managed) <> target_id then
      fail "Native XCTest lifecycle ownership record does not match the selected session";
    (match Workspace_mobile_device_lifecycle.managed_target managed with
     | Workspace_mobile_device_lifecycle.Ios_simulator { id }
       when id = simulator_id -> ()
     | _ -> fail "Native XCTest ownership is not for the exact selected Simulator");
    match Workspace_mobile_device_lifecycle.ownership managed with
    | Workspace_mobile_device_lifecycle.Owned { owner_session_id; _ }
      when owner_session_id = device_session_id -> ()
    | Workspace_mobile_device_lifecycle.Owned _ ->
        fail "Native XCTest lifecycle ownership belongs to a different device session"
    | Workspace_mobile_device_lifecycle.Preexisting ->
        fail "Native XCTest is unavailable for a pre-existing Simulator")

let mobile_xctest_lifecycle_guard context ~root
    (session : Workspace_mobile_run.session) =
  if session.platform <> Workspace_mobile_run.Ios then
    fail "Native XCTest backend is available only for iOS Simulator sessions";
  if session.root <> root then
    fail "selected mobile app session belongs to a different workspace root";
  let binding = match session.ios_device_binding with
    | Some binding -> binding
    | None -> fail "iOS app session has no bound device lifecycle identity" in
  if binding.simulator_id <> session.device then
    fail "iOS app session Simulator UUID changed after selection";
  mobile_xctest_require_owned_lifecycle context ~root
    ~device_session_id:binding.device_session_id
    ~inventory_id:binding.inventory_id
    ~simulator_id:binding.simulator_id ~target_id:binding.target_id

let mobile_xctest_capability context ~root
    (session : Workspace_mobile_run.session) =
  try
    if session.state <> Workspace_mobile_run.Running then
      fail "selected app session is not running";
    mobile_xctest_lifecycle_guard context ~root session;
    None
  with Tool_error message -> Some message

let mobile_session_select ~context ~root args =
  let subroot = required_string "subroot" args in
  let platform = required_string "platform" args in
  let device = required_string "device" args in
  let scheme = optional_string "scheme" "" args in
  let device_ready, scheme_ready =
    if platform = "android" then (
      Mutex.lock context.mobile_lock;
      Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock)
        (fun () ->
          let inventory = match context.android_inventory with
            | Some inventory when inventory.root = root &&
                inventory.subroot = subroot -> inventory
            | _ -> fail "approve Android device inventory for this exact project first" in
          let ready = List.exists (fun candidate ->
            candidate.Workspace_android_devices.serial = device &&
            candidate.emulator &&
            candidate.state = Workspace_android_devices.Ready)
            inventory.devices in
          ready, false))
    else if platform = "ios" then (
      let discovery = match context.xcode_discovery with
        | Some discovery when discovery.root = root &&
            discovery.bundle = subroot -> discovery
        | _ -> fail "approve Xcode scheme and simulator inventory for this bundle first" in
      let fingerprint = Workspace_xcode.fingerprint ~root ~bundle:subroot in
      if discovery.fingerprint <> fingerprint then (
        context.xcode_discovery <- None;
        fail "Xcode project inputs changed since discovery; discover again");
      let destinations = Option.value ~default:[]
        (List.assoc_opt scheme discovery.destinations) in
      let simulators = Option.value ~default:[]
        (List.assoc_opt scheme discovery.simulators) in
      let scheme_ready = List.mem scheme discovery.schemes &&
        List.mem device destinations && List.mem device simulators in
      scheme_ready, scheme_ready)
    else fail "mobile session platform must be android or ios" in
  let ios_device_binding =
    if platform = "ios" then (
      let device_session_id = optional_string "device_session_id" "" args in
      let inventory_id = optional_string "inventory_id" "" args in
      match device_session_id, inventory_id with
      | "", "" -> None
      | "", _ | _, "" ->
          fail "iOS lifecycle binding requires both device_session_id and inventory_id"
      | device_session_id, inventory_id ->
          let simulator_id = device in
          let target_id = "ios:" ^ simulator_id in
          mobile_xctest_require_owned_lifecycle context ~root ~device_session_id
            ~inventory_id ~simulator_id ~target_id;
          Some { Workspace_mobile_run.device_session_id;
            inventory_id; simulator_id; target_id })
    else None in
  Workspace_mobile_run.select context.mobile_run_manager ~root ~subroot
    ~platform ~device ~app_id:(required_string "app_id" args)
    ~app_path:(required_string "app_path" args)
    ~scheme:(if scheme = "" then None else Some scheme)
    ~variant:(match field "variant" args with
      | `Null -> None | `String value -> Some value
      | _ -> fail "variant must be a string")
    ~activity:(match optional_string "activity" "" args with
      | "" -> None | value -> Some value)
    ~device_ready ~scheme_ready
    ~ios_device_binding
let xcode_derived_data_path ~root ~app_path =
  let parts = String.split_on_char '/' app_path in
  let rec locate prefix = function
    | "Build" :: "Products" :: configuration :: product :: _
      when (String.starts_with ~prefix:"Debug-iphonesimulator" configuration ||
            String.starts_with ~prefix:"Release-iphonesimulator" configuration) &&
           product = Filename.basename app_path ->
        let relative = match prefix with
          | [] -> "." | _ -> String.concat "/" prefix in
        Workspace_path.checked_path root relative
    | part :: rest -> locate (prefix @ [part]) rest
    | [] -> fail "iOS app artifact must be under DerivedData Build/Products/<configuration>-iphonesimulator"
  in
  locate [] parts
let mobile_session_build_request ~context ~root session args =
  if session.Workspace_mobile_run.state = Workspace_mobile_run.Running then
    fail "cannot build a running mobile app session";
  let timeout_seconds = optional_int "timeout_seconds" 120
    ~minimum:1 ~maximum:300 args in
  match session.Workspace_mobile_run.platform with
  | Workspace_mobile_run.Android ->
      let task = required_string "task" args in
      let variant = Option.value ~default:"" session.variant in
      if variant = "" ||
         not (String.ends_with ~suffix:("assemble" ^
           String.capitalize_ascii variant) task) then
        fail "Android session build task must assemble the selected variant";
      let discovery = match context.mobile_discovery with
        | Some discovery when discovery.stack = "gradle" &&
            discovery.root = session.root &&
            discovery.subroot = session.subroot -> discovery
        | _ -> fail "approve Gradle task discovery for this exact project first" in
      if discovery.manifest_hash <>
          mobile_manifest ~root:session.root ~stack:"gradle"
            ~subroot:session.subroot then (
        context.mobile_discovery <- None;
        fail "Gradle settings changed since task discovery; discover tasks again");
      if not (List.mem task discovery.choices) then
        fail "selected variant task was not in the approved Gradle discovery";
      let fields = [
        "stack", `String "gradle"; "action", `String "run";
        "subroot", `String session.subroot; "target", `String task;
        "timeout_seconds", `Int timeout_seconds] in
      let command, cwd = mobile_command ~root:session.root
        (`Assoc fields) in
      ("gradle", `Assoc fields, command, cwd, "Mobile gradle run: exit 0")
  | Workspace_mobile_run.Ios ->
      let scheme = Option.value ~default:"" session.scheme in
      let derived_data_path = xcode_derived_data_path ~root:session.root
        ~app_path:session.app_path in
      let fields = [
        "action", `String "build"; "subroot", `String session.subroot;
        "scheme", `String scheme; "destination", `String session.device;
        "derived_data_path", `String derived_data_path;
        "timeout_seconds", `Int timeout_seconds] in
      let discovery = match context.xcode_discovery with
        | Some discovery when discovery.root = session.root &&
            discovery.bundle = session.subroot &&
            discovery.fingerprint =
              Workspace_xcode.fingerprint ~root:session.root ~bundle:session.subroot &&
            List.mem scheme discovery.schemes &&
            List.mem session.device
              (Option.value ~default:[] (List.assoc_opt scheme discovery.destinations)) ->
            discovery
        | _ -> fail "approve unchanged Xcode scheme and destination discovery for this app session first" in
      ignore discovery;
      let command = xcode_command (`Assoc fields) in
      let cwd = Filename.dirname
        (Workspace_path.checked_path root session.subroot) in
      ("xcode", `Assoc fields, command, cwd, "Xcode build: exit 0")

let includes text fragment =
  let text_length = String.length text and fragment_length = String.length fragment in
  let rec search offset =
    offset + fragment_length <= text_length &&
    (String.sub text offset fragment_length = fragment || search (offset + 1)) in
  search 0

let mobile_session_build ~approved ?cancel ?on_progress ?(mark_built = true)
    ~context ~root args =
  if not approved then
    fail "mobile app build requires explicit interactive approval";
  let id = required_string "session_id" args in
  let session = Workspace_mobile_run.get context.mobile_run_manager id in
  if session.root <> root then
    fail "mobile app session belongs to a different workspace root";
  let stack, build_args, _command, _cwd, success =
    mobile_session_build_request ~context ~root session args in
  let output = if stack = "gradle" then
      mobile_check ~approved ?cancel ?on_progress ~context session.root build_args
    else
      xcode_preflight ~approved ?cancel ?on_progress ~context session.root build_args in
  if not (String.starts_with ~prefix:success output) ||
     includes output "incomplete result" then
    fail ("Mobile app build did not complete successfully:\n" ^ output);
  if mark_built then
    ignore (Workspace_mobile_run.mark_built context.mobile_run_manager ~id);
  "Mobile build completed for " ^ id ^ ".\n" ^ output

let valid_sha256 value =
  String.length value = 64 &&
  String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false) value

let mobile_verify_source ~context ~root args =
  let path = required_string "source_path" args in
  let before = required_string "before_sha256" args
  and after = required_string "after_sha256" args in
  if not (valid_sha256 before && valid_sha256 after) then
    fail "source snapshot hashes must be lowercase SHA-256 values";
  let snapshot = try Workspace_edit.read_snapshot ~root ~path
    with Workspace_edit.Error message -> fail message in
  let evidence_key = guarded_evidence_key ~root ~path in
  Mutex.lock context.mobile_lock;
  let evidence = Fun.protect ~finally:(fun () ->
      Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.find_opt context.guarded_edit_evidence evidence_key) in
  if snapshot.sha256 <> after then
    fail "edited source changed since the supplied post-edit snapshot";
  (match evidence with
   | Some evidence when evidence.before_sha256 = before &&
                        evidence.after_sha256 = after -> ()
   | _ -> fail "source hashes are not bound to a guarded apply_edits change in this session");
  path, before, after

let mobile_verify_android_task session args =
  let artifact = session.Workspace_mobile_run.app_path in
  let subroot = session.subroot in
  let prefix = if subroot = "." then "" else subroot ^ "/" in
  if not (String.starts_with ~prefix artifact) then
    fail "Android app artifact is outside the selected Gradle project";
  let project_artifact =
    String.sub artifact (String.length prefix)
      (String.length artifact - String.length prefix) in
  let components = String.split_on_char '/' project_artifact in
  let rec module_parts prefix = function
    | "build" :: _ when prefix <> [] -> List.rev prefix
    | part :: rest -> module_parts (part :: prefix) rest
    | [] -> fail "Android app artifact must be inside a module build directory" in
  let module_name = String.concat ":" (module_parts [] components) in
  let task = required_string "target" args in
  if not (String.starts_with ~prefix:(":" ^ module_name ^ ":") task) then
    fail "Android test task must belong to the selected app artifact's Gradle module";
  task


let mobile_test_executed output =
  let text = String.lowercase_ascii output in
  let has fragment = includes text fragment in
  let count_before marker predicate =
    let rec find from =
      match Str.search_forward (Str.regexp_string marker) text from with
      | position ->
          let rec digits index =
            if index >= 0 then match text.[index] with
              | '0'..'9' -> digits (index - 1)
              | _ -> index + 1
            else 0 in
          let first = digits (position - 1) in
          if first < position &&
             (try predicate (int_of_string
                (String.sub text first (position - first)))
              with _ -> false)
          then true
          else find (position + String.length marker)
      | exception Not_found -> false in
    find 0 in
  let positive_count_before marker =
    count_before marker (fun count -> count > 0) in
  let zero_count_before marker =
    count_before marker (fun count -> count = 0) in
  let positive_count_after marker =
    let rec find from =
      match Str.search_forward (Str.regexp_string marker) text from with
      | position ->
          let first = position + String.length marker in
          let rec skip index =
            if index < String.length text && text.[index] = ' ' then skip (index + 1)
            else index in
          let first = skip first in
          let rec digits index =
            if index < String.length text then match text.[index] with
              | '0'..'9' -> digits (index + 1)
              | _ -> index
            else index in
          let stop = digits first in
          if stop > first &&
             (try int_of_string (String.sub text first (stop - first)) > 0
              with _ -> false)
          then true
          else find (position + String.length marker)
      | exception Not_found -> false in
    find 0 in
  not (has "zero tests" || has "executed 0" ||
       has "tests run: 0" || has "no tests found" ||
       has "no matching tests" || zero_count_before " test") &&
  (List.exists positive_count_before
     [" test"; " tests completed"; " tests passed"] ||
   List.exists positive_count_after ["tests run:"; "tests found:"])

let mobile_verify_preview ~context ~root args =
  let session = Workspace_mobile_run.get context.mobile_run_manager
    (required_string "session_id" args) in
  if session.root <> root then
    fail "mobile app session belongs to a different workspace root";
  let path, before, after = mobile_verify_source ~context ~root args in
  let action = required_string "action" args in
  let command, cwd =
    match action, session.platform with
    | "build", _ ->
        let _stack, _build_args, command, cwd, _ =
          mobile_session_build_request ~context ~root:session.root session args in
        command, cwd
    | "test", Workspace_mobile_run.Ios ->
        let test_args = `Assoc [
          "action", `String "test"; "subroot", `String session.subroot;
          "scheme", `String (Option.value ~default:"" session.scheme);
          "destination", `String session.device;
          "timeout_seconds", `Int (optional_int "timeout_seconds" 120
            ~minimum:1 ~maximum:300 args)] in
        xcode_command test_args,
        Filename.dirname (Workspace_path.checked_path root session.subroot)
    | "test", Workspace_mobile_run.Android ->
        let test_args = `Assoc [
          "stack", `String "gradle"; "action", `String "instrumented";
          "subroot", `String session.subroot;
          "target", `String (mobile_verify_android_task session args);
          "serial", `String session.device;
          "timeout_seconds", `Int (optional_int "timeout_seconds" 120
            ~minimum:1 ~maximum:300 args)] in
        mobile_command ~root:session.root test_args
    | _ -> fail "mobile verification action must be build or test" in
  "Runs only the selected app-session's focused " ^ action ^
  " command after rechecking the exact edited source snapshot.",
  ["Session/app/device: " ^ session.id ^ " · " ^ session.app_id ^
      " · " ^ session.device;
   "Project: " ^ session.subroot;
   "Source: " ^ Printf.sprintf "%S" path;
   "Pre-edit SHA-256: " ^ before;
   "Post-edit SHA-256: " ^ after;
   "Working directory: " ^ Printf.sprintf "%S" cwd;
   "Exact command: " ^ command]

let mobile_verify ~approved ?cancel ?on_progress ~context ~root args =
  if not approved then
    fail "mobile source verification requires explicit interactive approval";
  let root = Workspace_path.root_path root in
  let session = Workspace_mobile_run.get context.mobile_run_manager
    (required_string "session_id" args) in
  if session.root <> root then
    fail "mobile app session belongs to a different workspace root";
  let path, _before, expected =
    mobile_verify_source ~context ~root args in
  let action = required_string "action" args in
  let output, success_prefix = match action with
    | "build" ->
        mobile_session_build ~approved ?cancel ?on_progress ~mark_built:false
          ~context ~root args, "Mobile build completed for " ^ session.id ^ "."
    | "test" ->
        (match session.platform with
         | Workspace_mobile_run.Ios ->
             let test_args = `Assoc [
               "action", `String "test"; "subroot", `String session.subroot;
               "scheme", `String (Option.value ~default:"" session.scheme);
               "destination", `String session.device;
               "timeout_seconds", `Int (optional_int "timeout_seconds" 120
                 ~minimum:1 ~maximum:300 args)] in
             let result = xcode_preflight ~approved ?cancel ?on_progress
               ~context session.root test_args in
             result, "Xcode test: exit 0 (scheme "
               ^ Option.value ~default:"" session.scheme ^ ")"
         | Workspace_mobile_run.Android ->
             let test_args = `Assoc [
               "stack", `String "gradle"; "action", `String "instrumented";
               "subroot", `String session.subroot;
               "target", `String (mobile_verify_android_task session args);
               "serial", `String session.device;
               "timeout_seconds", `Int (optional_int "timeout_seconds" 120
                 ~minimum:1 ~maximum:300 args)] in
             mobile_check ~approved ?cancel ?on_progress ~context
               session.root test_args,
             "Mobile gradle instrumented: exit 0")
    | _ -> fail "mobile verification action must be build or test" in
  if not (String.starts_with ~prefix:success_prefix output) ||
     includes output "incomplete result" ||
     (action = "test" && not (mobile_test_executed output)) then
    fail ("Mobile source verification did not complete successfully for " ^
      session.id ^ " (" ^ session.app_id ^ " on " ^ session.device ^
      "):\n" ^ output);
  let current = try Workspace_edit.read_snapshot ~root ~path
    with Workspace_edit.Error message -> fail message in
  if current.sha256 <> expected then
    fail "edited source changed while the mobile command was running";
  if action = "build" then
    ignore (Workspace_mobile_run.mark_built context.mobile_run_manager
      ~id:session.id);
  "VERIFIED " ^ action ^ " for " ^ session.id ^ " (" ^ session.app_id ^
  " on " ^ session.device ^ "), source " ^ path ^ " at SHA-256 " ^
  expected ^ ".\n" ^ output


let mobile_session ~approved ?cancel ?on_progress ?context root args =
  let context = require_session_context context in
  let root = Workspace_path.root_path root in
  check_session_context context;
  match required_string "action" args with
  | "list" ->
      let sessions = Workspace_mobile_run.sessions context.mobile_run_manager in
      if sessions = [] then "No mobile app sessions are selected."
      else String.concat "\n" (List.map Workspace_mobile_run.render sessions)
  | "select" ->
      let session = mobile_session_select ~context ~root args in
      "Selected mobile app session:\n" ^ Workspace_mobile_run.render session
  | "status" ->
      let session = Workspace_mobile_run.get context.mobile_run_manager
        (required_string "session_id" args) in
      Workspace_mobile_run.render session
  | "build" ->
      mobile_session_build ~approved ?cancel ?on_progress ~context ~root args
  | ("install" | "launch" | "stop") as action ->
      if not approved then fail "mobile device action requires explicit interactive approval";
      let id = required_string "session_id" args in
      let run ~root:working_root ~command =
        let result = Workspace_process.run_shell ?cancel ?on_progress
          ~timeout_seconds:(optional_int "timeout_seconds" 60 ~minimum:1
            ~maximum:300 args)
          ~output_limit:max_command_bytes ~cwd:(Some working_root) ~command () in
        let outcome = match result.termination with
          | Workspace_process.Exited 0 when not result.truncated -> "exit 0"
          | Workspace_process.Exited code -> Printf.sprintf "exit %d" code
          | Workspace_process.Signaled signal -> Printf.sprintf "signal %d" signal
          | Workspace_process.Timed_out -> "timed out"
          | Workspace_process.Cancelled -> raise Cancelled in
        if outcome <> "exit 0" then
          fail ("Mobile " ^ action ^ " failed: " ^ outcome ^ "\n" ^ result.output);
        result.output in
      let output = Workspace_mobile_run.execute context.mobile_run_manager
        ~approved ~run ~action ~id in
      "Mobile " ^ action ^ " completed for " ^ id ^ ".\n" ^ output
  | _ -> fail "mobile session action must be list, select, status, build, install, launch or stop"

let mobile_xctest_build_hash root session =
  try (Workspace_mobile_report.build_identity root session).build_hash
  with Workspace_mobile_report.Error message -> fail message

let mobile_xctest_plan_key (session : Workspace_mobile_run.session)
    ~build_hash ~source_hash ~project_hash =
  let binding = match session.ios_device_binding with
    | Some binding -> binding
    | None -> fail "iOS app session has no bound device lifecycle identity" in
  Workspace_edit.sha256 (String.concat "\000" [
    session.id; session.root; session.subroot; session.app_id; session.app_path;
    session.device; binding.device_session_id; binding.inventory_id;
    binding.simulator_id; binding.target_id; build_hash; source_hash; project_hash])

let mobile_xctest_temporary_directory () =
  let path = Filename.temp_file "pave-native-xctest-" "" in
  Unix.unlink path;
  path

let mobile_xctest_preview_directory context key =
  let path = mobile_xctest_temporary_directory () in
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    if Hashtbl.length context.mobile_xctest_plans >= 128 &&
       not (Hashtbl.mem context.mobile_xctest_plans key) then
      fail "too many pending Native XCTest approval previews";
    Hashtbl.replace context.mobile_xctest_plans key path);
  path

let mobile_xctest_execution_directory context key =
  Mutex.lock context.mobile_lock;
  let path = Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock)
    (fun () ->
      match Hashtbl.find_opt context.mobile_xctest_plans key with
      | Some path ->
          Hashtbl.remove context.mobile_xctest_plans key;
          Some path
      | None -> None) in
  match path with
  | Some path -> path
  | None -> fail "Native XCTest approval preview is missing or stale; request a new preview"

let mobile_xctest_preview ~context ~root ~session =
  if session.Workspace_mobile_run.state <> Workspace_mobile_run.Running then
    fail "Native XCTest requires a running selected app session";
  mobile_xctest_lifecycle_guard context ~root session;
  let binding = Option.get session.Workspace_mobile_run.ios_device_binding in
  let source = try Workspace_mobile_observe.xctest_runner_source
      ~bundle_id:session.app_id
    with Workspace_mobile_observe.Error message -> fail message in
  let source_hash = Workspace_edit.sha256 source in
  let project_hash = try Workspace_mobile_observe.xctest_project_fingerprint
      ~bundle_id:session.app_id
    with Workspace_mobile_observe.Error message -> fail message in
  let build_hash = mobile_xctest_build_hash root session in
  let key = mobile_xctest_plan_key session ~build_hash ~source_hash ~project_hash in
  let directory = mobile_xctest_preview_directory context key in
  let shell, project, derived, result, export, booted_log, log, export_log =
    try Workspace_mobile_observe.xctest_command ~directory
      ~simulator_id:binding.simulator_id
    with Workspace_mobile_observe.Error message -> fail message in
  let target_id = binding.target_id in
  let details = [
    "Selected session/app/device: " ^ session.id ^ " · " ^ session.app_id ^
      " · " ^ binding.simulator_id;
    "Lifecycle binding: " ^ binding.device_session_id ^ " · " ^
      binding.inventory_id ^ " · " ^ target_id ^ " · ownership=Owned";
    "Selected app artifact/build SHA-256: " ^ session.app_path ^ " · " ^ build_hash;
    "XCTest helper source version/provenance: " ^
      Workspace_mobile_observe.xctest_version ^ " · generated in-repository from fixed public XCTest/XCUIApplication templates";
    "Generated UI-test source: " ^ Filename.concat (Filename.concat directory "Tests")
      "PaveXCTestRunner.swift";
    Printf.sprintf "Generated Swift source SHA-256: %s (%d bytes)"
      source_hash (String.length source);
    "Generated project/host/scheme template SHA-256: " ^ project_hash;
    "Exact generated test source follows:\n" ^ source;
    "Temporary project: " ^ Printf.sprintf "%S" project;
    "Derived data: " ^ Printf.sprintf "%S" derived;
    "Result bundle: " ^ Printf.sprintf "%S" result;
    "Attachment export: " ^ Printf.sprintf "%S" export;
    "Booted Simulator preflight log: " ^ Printf.sprintf "%S" booted_log;
    "Build log: " ^ Printf.sprintf "%S" log;
    "Attachment export log: " ^ Printf.sprintf "%S" export_log;
    "xcodebuild will build, install and launch only the generated helper host/test runner on this exact Simulator. The selected app must already be running; XCTest calls activate(), which may foreground it and could relaunch it if it exits between the state check and activation. It never installs or reinstalls the selected app.";
    "No physical device, network download, xcodegen, private API, or project-source mutation is used.";
    "Exact approved host command (xcodebuild test for the selected Simulator, followed by xcresulttool attachment export):";
    shell;
    Printf.sprintf "Maximum request/response: %d/%d bytes; maximum accessibility nodes: %d."
      Workspace_mobile_observe.max_xctest_request_bytes
      Workspace_mobile_observe.max_xctest_response_bytes
      Workspace_mobile_observe.max_nodes] in
  "Runs one separately approved Native XCTest accessibility observation against only the selected running app and Simulator.",
  details

let mobile_xctest_read_private ~maximum path =
  let before = try Unix.lstat path with Unix.Unix_error _ ->
    fail "XCTest result file is missing" in
  if before.Unix.st_kind <> Unix.S_REG || before.Unix.st_uid <> Unix.geteuid () ||
     before.Unix.st_perm land 0o077 <> 0 || before.Unix.st_size > maximum then
    fail "XCTest result file is unsafe or exceeds its byte limit";
  let fd = try Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0
    with Unix.Unix_error _ -> fail "XCTest result file could not be opened safely" in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let opened = Unix.fstat fd in
    if opened.Unix.st_kind <> Unix.S_REG ||
       opened.Unix.st_uid <> Unix.geteuid () ||
       opened.Unix.st_dev <> before.Unix.st_dev ||
       opened.Unix.st_ino <> before.Unix.st_ino ||
       opened.Unix.st_perm land 0o077 <> 0 ||
       opened.Unix.st_size > maximum then
      fail "XCTest result file changed or became unsafe while being read";
    let buffer = Buffer.create (min maximum (max 0 opened.Unix.st_size)) in
    let chunk = Bytes.create 8192 in
    let rec read total =
      let count = Unix.read fd chunk 0 (min (Bytes.length chunk) (maximum + 1 - total)) in
      if count = 0 then Buffer.contents buffer
      else if total + count > maximum then
        fail "XCTest result file exceeds its byte limit"
      else (
        Buffer.add_subbytes buffer chunk 0 count;
        read (total + count)) in
    read 0)

let mobile_xctest_log path =
  try mobile_xctest_read_private ~maximum:8192 path
  with Tool_error _ -> ""

let mobile_xctest_status label output =
  let prefix = label ^ "=" in
  match String.split_on_char '\n' output
      |> List.filter (fun line -> String.starts_with ~prefix line) with
  | [line] ->
      let text = String.sub line (String.length prefix)
          (String.length line - String.length prefix) in
      let status = try int_of_string text
        with _ -> fail ("XCTest host returned invalid " ^ label) in
      if status < 0 then fail ("XCTest host returned invalid " ^ label);
      status
  | [] -> fail ("XCTest host returned no " ^ label)
  | _ -> fail ("XCTest host returned ambiguous " ^ label)

let rec mobile_xctest_remove_tree path =
  let stat = Unix.lstat path in
  if stat.Unix.st_uid <> Unix.geteuid () then
    fail "refusing to remove a temporary XCTest path owned by another user";
  match stat.Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.iter (fun name ->
        mobile_xctest_remove_tree (Filename.concat path name));
      Unix.rmdir path
  | Unix.S_REG | Unix.S_LNK -> Unix.unlink path
  | _ -> fail "refusing to remove an unexpected temporary XCTest file type"

let mobile_xctest_run ?cancel ?on_progress ~timeout_seconds ~context ~root
    ~session () =
  if session.Workspace_mobile_run.state <> Workspace_mobile_run.Running then
    fail "Native XCTest requires a running selected app session";
  mobile_xctest_lifecycle_guard context ~root session;
  (match cancel with Some cancelled when cancelled () -> raise Cancelled | _ -> ());
  let binding = Option.get session.Workspace_mobile_run.ios_device_binding in
  let source = try Workspace_mobile_observe.xctest_runner_source
      ~bundle_id:session.app_id
    with Workspace_mobile_observe.Error message -> fail message in
  let source_hash = Workspace_edit.sha256 source in
  let project_hash = try Workspace_mobile_observe.xctest_project_fingerprint
      ~bundle_id:session.app_id
    with Workspace_mobile_observe.Error message -> fail message in
  let build_hash = mobile_xctest_build_hash root session in
  let key = mobile_xctest_plan_key session ~build_hash ~source_hash ~project_hash in
  let directory = mobile_xctest_execution_directory context key in
  let cleanup () =
    try mobile_xctest_remove_tree directory with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> () in
  Fun.protect ~finally:cleanup (fun () ->
    let generated_hash = try Workspace_mobile_observe.create_xctest_project
        ~directory ~bundle_id:session.app_id
      with Workspace_mobile_observe.Error message -> fail message in
    if generated_hash <> project_hash then
      fail "generated Native XCTest project does not match the approved source fingerprint";
    let shell, _project, _derived, _result_bundle, export_directory,
        booted_log, log, export_log =
      try Workspace_mobile_observe.xctest_command ~directory
        ~simulator_id:binding.simulator_id
      with Workspace_mobile_observe.Error message -> fail message in
    (match cancel with Some cancelled when cancelled () -> raise Cancelled | _ -> ());
    let result = Workspace_process.run_shell ?cancel ?on_progress
      ~timeout_seconds ~output_limit:2048 ~cwd:(Some root) ~command:shell () in
    (match result.termination with
     | Workspace_process.Exited 0 when not result.truncated -> ()
     | Workspace_process.Cancelled ->
         fail "Native XCTest outcome is ambiguous: cancellation arrived after xcodebuild may have started; the selected app may already have been observed or acted on. No retry was attempted."
     | Workspace_process.Timed_out ->
         fail "Native XCTest outcome is ambiguous: xcodebuild exceeded its deadline and may already have observed or acted on the selected app. No retry was attempted."
     | Workspace_process.Exited code ->
         fail (Printf.sprintf "Native XCTest host wrapper exited %d%s"
           code (if result.truncated then " with truncated output" else ""))
     | Workspace_process.Signaled signal ->
         fail (Printf.sprintf "Native XCTest host wrapper received signal %d; app outcome may be ambiguous"
           signal));
    let preflight_status =
      mobile_xctest_status "PAVE_XCTEST_PREFLIGHT_STATUS" result.output in
    if preflight_status <> 0 then
      fail (Printf.sprintf
        "Native XCTest did not start because the selected Simulator was not confirmed booted (preflight exit %d); xcodebuild was not run.\n%s"
        preflight_status (mobile_xctest_log booted_log));
    let build_status = mobile_xctest_status "PAVE_XCTEST_BUILD_STATUS" result.output in
    let export_status = mobile_xctest_status "PAVE_XCTEST_EXPORT_STATUS" result.output in
    if export_status <> 0 then
      fail (Printf.sprintf
        "xcresulttool attachment export failed with exit %d (xcodebuild exit %d)\n%s\n%s"
        export_status build_status (mobile_xctest_log log)
        (mobile_xctest_log export_log));
    let manifest = mobile_xctest_read_private ~maximum:
      Workspace_mobile_observe.max_xctest_manifest_bytes
      (Filename.concat export_directory "manifest.json") in
    let filename = try Workspace_mobile_observe.xctest_manifest_attachment manifest
      with Workspace_mobile_observe.Error message -> fail message in
    let payload = mobile_xctest_read_private
        ~maximum:Workspace_mobile_observe.max_xctest_response_bytes
        (Filename.concat export_directory filename) in
    let payload = try Workspace_mobile_observe.xctest_export_payload
        ~manifest ~files:[filename, payload]
      with Workspace_mobile_observe.Error message -> fail message in
    let response = try Workspace_mobile_observe.parse_xctest_response
        ~bundle_id:session.app_id payload
      with Workspace_mobile_observe.Error message -> fail message in
    if build_status <> 0 then
      fail (Printf.sprintf "xcodebuild test exited %d after its XCTest response; no retry was attempted.\n%s"
        build_status (mobile_xctest_log log));
    Yojson.Basic.to_string response)

let mobile_observe_tool ~approved ?cancel ?on_progress ~context ~root args =
  if not approved then fail "mobile screen observation requires explicit interactive approval";
  let root = Workspace_path.root_path root in
  check_session_context context;
  let id = required_string "session_id" args in
  let session = Workspace_mobile_run.get context.mobile_run_manager id in
  if session.Workspace_mobile_run.root <> root then
    fail "mobile app session belongs to a different workspace root";
  if session.state <> Workspace_mobile_run.Running then
    fail "mobile screen observation requires a running app session";
  let action = required_string "action" args in
  if action = "accessibility" && session.platform = Workspace_mobile_run.Ios then
    [Protocol.Text (mobile_xctest_run ?cancel ?on_progress
      ~timeout_seconds:(optional_int "timeout_seconds" 120 ~minimum:1 ~maximum:120 args)
      ~context ~root ~session ())]
  else (
    let command = Workspace_mobile_observe.command action session in
    let timeout_seconds = optional_int "timeout_seconds" 30
      ~minimum:1 ~maximum:120 args in
    let result = Workspace_process.run_shell ?cancel ?on_progress
      ~timeout_seconds ~output_limit:Workspace_mobile_observe.max_screenshot_bytes
      ~cwd:(Some root) ~command () in
    let status = match result.termination with
      | Workspace_process.Exited 0 when not result.truncated -> None
      | Workspace_process.Exited code -> Some (Printf.sprintf "exit %d" code)
      | Workspace_process.Signaled signal -> Some (Printf.sprintf "signal %d" signal)
      | Workspace_process.Timed_out -> Some "timed out"
      | Workspace_process.Cancelled -> raise Cancelled in
    (match status with
     | Some reason ->
         fail ("Mobile " ^ action ^ " failed: " ^ reason ^
           (if result.truncated then " (output truncated)" else "") ^
           "\n" ^ result.output)
     | None -> ());
    match action with
    | "screenshot" ->
        let screenshot = Workspace_mobile_observe.validate_png result.output in
        Workspace_mobile_run.set_screen_size context.mobile_run_manager ~id
          ~width:screenshot.width ~height:screenshot.height;
        [Protocol.Text (Yojson.Basic.to_string (`Assoc [
           "session_id", `String id;
           "status", `String "available";
           "mime_type", `String "image/png";
           "width", `Int screenshot.width;
           "height", `Int screenshot.height;
           "bytes", `Int (String.length screenshot.png)]));
         Protocol.Image { mime_type = "image/png";
           data = Workspace_mobile_observe.base64_encode screenshot.png }]
    | "accessibility" ->
        let nodes = Workspace_mobile_observe.parse_accessibility result.output in
        [Protocol.Text (Workspace_mobile_observe.accessibility_json nodes)]
    | _ -> fail "mobile observation action must be screenshot or accessibility"
  )

let mobile_accessibility_audit_tool ~approved ?cancel ?on_progress ~context ~root args =
  if not approved then fail "mobile accessibility audit requires explicit interactive approval";
  let root = Workspace_path.root_path root in
  check_session_context context;
  let id = required_string "session_id" args in
  let session = Workspace_mobile_run.get context.mobile_run_manager id in
  if session.root <> root then
    fail "mobile app session belongs to a different workspace root";
  if session.state <> Workspace_mobile_run.Running then
    fail "mobile accessibility audit requires a running app session";
  if session.platform <> Workspace_mobile_run.Android then
    fail "rule-based mobile accessibility audit is currently Android-only";
  let command = Workspace_mobile_observe.accessibility_audit_command session in
  let result = Workspace_process.run_shell ?cancel ?on_progress
    ~timeout_seconds:(optional_int "timeout_seconds" 30 ~minimum:1 ~maximum:120 args)
    ~output_limit:Workspace_mobile_observe.max_accessibility_bytes
    ~cwd:(Some root) ~command () in
  (match result.termination with
   | Workspace_process.Exited 0 when not result.truncated -> ()
   | Workspace_process.Exited code ->
       fail (Printf.sprintf "Mobile accessibility observation failed (exit %d)%s"
         code (if result.truncated then "; output truncated" else ""))
   | Workspace_process.Signaled signal ->
       fail (Printf.sprintf "Mobile accessibility observation failed (signal %d)" signal)
   | Workspace_process.Timed_out -> fail "Mobile accessibility observation timed out"
   | Workspace_process.Cancelled -> raise Cancelled);
  let capture = try Workspace_mobile_accessibility_audit.parse_capture result.output
    with Workspace_mobile_accessibility_audit.Error message -> fail message in
  let report = try Workspace_mobile_accessibility_audit.analyze
      ~observation_id:capture.observation_id ?density:capture.density capture.nodes
    with Workspace_mobile_accessibility_audit.Error message -> fail message in
  Workspace_mobile_accessibility_audit.report_json report

let mobile_dev_server_tool ~approved ?cancel ~context ~root args =
  let context = require_session_context (Some context) in
  let root = Workspace_path.root_path root in
  let action = required_string "action" args in
  let id = required_string "id" args in
  (match action with
   | "status" ->
       let session = try Workspace_node_server.get context.node_server_manager ~id
         with Workspace_node_server.Error message -> fail message in
       if session.root <> root then
         fail "Node development server belongs to a different workspace root";
       Workspace_node_server.render session
   | "stop" ->
       if not approved then fail "stopping a mobile development server requires explicit approval";
       let session = try Workspace_node_server.get context.node_server_manager ~id
         with Workspace_node_server.Error message -> fail message in
       if session.root <> root then
         fail "Node development server belongs to a different workspace root";
       let stopped = try Workspace_node_server.stop context.node_server_manager ~id
         with Workspace_node_server.Error message -> fail message in
       Workspace_node_server.render stopped
   | "start" ->
       if not approved then fail "starting a mobile development server requires explicit approval";
       Workspace_process.validate_id id;
       let subroot = required_string "subroot" args in
       let script = required_string "script" args in
       let package_manager = optional_string "manager" "" args in
       let server = try Workspace_node_server.start context.node_server_manager
           ~id ~root ~subroot ~script ~package_manager
           ~host:(required_string "host" args)
           ~port:(match field "port" args with
             | `Int port when port >= 1 && port <= 65_535 -> port
             | _ -> fail "port must be an integer between 1 and 65535")
           ?cancel
           ~readiness_timeout_seconds:(optional_int "readiness_timeout_seconds" 45
             ~minimum:1 ~maximum:300 args) ()
         with Workspace_node_server.Error message -> fail message in
       Workspace_node_server.render server
   | _ -> fail "mobile development server action must be start, status or stop")

let mobile_environment_setting args =
  let setting_name = required_string "effect" args in
  match setting_name with
  | "locale" ->
      Workspace_mobile_environment.Locale (required_string "locale" args)
  | "theme" ->
      Workspace_mobile_environment.Theme
        (match required_string "theme" args with
         | "light" -> Workspace_mobile_environment.Light
         | "dark" -> Workspace_mobile_environment.Dark
         | _ -> fail "theme must be light or dark")
  | "orientation" ->
      Workspace_mobile_environment.Orientation
        (match required_string "orientation" args with
         | "portrait" -> Workspace_mobile_environment.Portrait
         | "landscape" -> Workspace_mobile_environment.Landscape
         | _ -> fail "orientation must be portrait or landscape")
  | _ -> fail "unsupported mobile environment effect"

let mobile_environment_value = function
  | Workspace_mobile_environment.Locale_value "" -> "app locale: default"
  | Workspace_mobile_environment.Locale_value locale -> "app locale: " ^ locale
  | Workspace_mobile_environment.Theme_value Workspace_mobile_environment.Light ->
      "theme: light"
  | Workspace_mobile_environment.Theme_value Workspace_mobile_environment.Dark ->
      "theme: dark"
  | Workspace_mobile_environment.Orientation_value (auto, rotation) ->
      Printf.sprintf "orientation: auto=%b rotation=%d" auto rotation

let mobile_environment_build_hash root session =
  try (Workspace_mobile_report.build_identity root session).build_hash
  with Workspace_mobile_report.Error message -> fail message

let mobile_environment_plan context plan_id =
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    match Hashtbl.find_opt context.mobile_environment_plans plan_id with
    | Some plan -> plan
    | None -> fail "mobile environment plan is absent or belongs to a closed session")

let mobile_environment_session ~context ~root args =
  let id = required_string "session_id" args in
  let session = Workspace_mobile_run.get context.mobile_run_manager id in
  if session.root <> root then
    fail "mobile environment session belongs to a different workspace root";
  if session.platform <> Workspace_mobile_run.Android ||
     session.state <> Workspace_mobile_run.Running then
    fail "mobile environment experiments require the exact running Android app session";
  session

let mobile_environment_plan_preview ~context ~root args =
  let action = required_string "action" args in
  if action = "preview" then (
    let session = mobile_environment_session ~context ~root args in
    let build_hash = mobile_environment_build_hash root session in
    let setting = mobile_environment_setting args in
    let command = try Workspace_mobile_environment.observation_command session ~setting
      with Workspace_mobile_environment.Error message -> fail message in
    let target = match setting with
      | Workspace_mobile_environment.Locale locale -> "locale target: " ^ locale
      | Workspace_mobile_environment.Theme theme ->
          "theme target: " ^ (if theme = Workspace_mobile_environment.Dark then "dark" else "light")
      | Workspace_mobile_environment.Orientation orientation ->
          "orientation target: " ^ (if orientation = Workspace_mobile_environment.Portrait then "portrait" else "landscape") in
    ["Observes the exact pre-state before creating a one-session, one-build preview; no device state changes.";
     "Session/app/device: " ^ session.id ^ " · " ^ session.app_id ^ " · " ^ session.device;
     "Selected build SHA-256: " ^ build_hash;
     "Theme and orientation changes affect the whole selected emulator; locale is app-specific.";
     target;
     "Read command: " ^ command;
     Printf.sprintf "Maximum output: %d bytes; deadline: %d ms."
       Workspace_mobile_environment.max_output_bytes Workspace_mobile_environment.timeout_ms]
  ) else (
    let plan = mobile_environment_plan context (required_string "plan_id" args) in
    let session = mobile_environment_session ~context ~root args in
    let build_hash = mobile_environment_build_hash root session in
    if not (Workspace_mobile_environment.same_session session ~build_hash plan) then
      fail "mobile environment plan belongs to a different app session or build";
    match action with
    | "apply" ->
        ["Applies one approved reversible emulator-only state change after rechecking the exact preview pre-state.";
         "Session/app/device: " ^ session.id ^ " · " ^ session.app_id ^ " · " ^ session.device;
         "Selected build SHA-256: " ^ build_hash;
         "Observed before: " ^ mobile_environment_value plan.before;
         "Approved target: " ^ mobile_environment_value plan.target;
         "Pre-state recheck: " ^ plan.observe_command;
         "Exact change command: " ^ plan.change_command;
         "A partial/cancelled transition leaves this plan available for separately approved restore.";
         Printf.sprintf "Each command is bounded to %d ms and %d output bytes."
           Workspace_mobile_environment.timeout_ms Workspace_mobile_environment.max_output_bytes]
    | "restore" ->
        ["Restores only if the exact current state still equals this plan's target; no automatic restore.";
         "Session/app/device: " ^ session.id ^ " · " ^ session.app_id ^ " · " ^ session.device;
         "Selected build SHA-256: " ^ build_hash;
         "Original state: " ^ mobile_environment_value plan.before;
         "Approved target: " ^ mobile_environment_value plan.target;
         "Exact state observation: " ^ plan.observe_command;
         "Exact restore command: " ^ plan.restore_command;
         "Restore requires this separate explicit approval."]
    | _ -> fail "mobile environment action must be preview, apply or restore"
  )

let mobile_environment_tool ~approved ?cancel ~context ~root args =
  let context = require_session_context (Some context) in
  let root = Workspace_path.root_path root in
  let action = required_string "action" args in
  if not approved then
    fail "mobile environment observation or state change requires explicit interactive approval";
  let process ~command ~timeout_ms ~max_output_bytes =
    let result = Workspace_process.run_shell ?cancel
      ~timeout_seconds:(max 1 (timeout_ms / 1000))
      ~output_limit:max_output_bytes ~cwd:(Some root) ~command () in
    match result.termination with
    | Workspace_process.Exited 0 when not result.truncated -> result.output
    | Workspace_process.Exited code ->
        fail (Printf.sprintf "mobile environment command exited %d%s"
          code (if result.truncated then " with truncated output" else ""))
    | Workspace_process.Signaled signal ->
        fail (Printf.sprintf "mobile environment command received signal %d" signal)
    | Workspace_process.Timed_out -> fail "mobile environment command timed out"
    | Workspace_process.Cancelled -> raise Cancelled in
  match action with
  | "preview" ->
      let session = mobile_environment_session ~context ~root args in
      let build_hash = mobile_environment_build_hash root session in
      let setting = mobile_environment_setting args in
      let command = try Workspace_mobile_environment.observation_command session ~setting
        with Workspace_mobile_environment.Error message -> fail message in
      let before_output = process ~command
        ~timeout_ms:Workspace_mobile_environment.timeout_ms
        ~max_output_bytes:Workspace_mobile_environment.max_output_bytes in
      let after_hash = mobile_environment_build_hash root session in
      if build_hash <> after_hash then
        fail "selected app build changed during environment-state observation";
      let plan = try Workspace_mobile_environment.preview session ~build_hash
          ~setting ~before_output
        with Workspace_mobile_environment.Error message -> fail message in
      Mutex.lock context.mobile_lock;
      let plan_id = Fun.protect
        ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
          if Hashtbl.length context.mobile_environment_plans >= 16 then
            fail "mobile environment plan limit reached; restore or discard an existing plan";
          context.next_mobile_environment_plan <- context.next_mobile_environment_plan + 1;
          let id = "env-" ^ string_of_int context.next_mobile_environment_plan in
          Hashtbl.add context.mobile_environment_plans id plan;
          id) in
      Yojson.Basic.to_string (`Assoc [
        "plan_id", `String plan_id;
        "session_id", `String plan.session_id;
        "device", `String plan.device;
        "build_sha256", `String plan.build_hash;
        "observed_before", `String (mobile_environment_value plan.before);
        "target", `String (mobile_environment_value plan.target);
        "apply_required", `Bool true;
        "restore_required", `Bool true])
  | "apply" ->
      let plan = mobile_environment_plan context (required_string "plan_id" args) in
      let session = mobile_environment_session ~context ~root args in
      let build_hash = mobile_environment_build_hash root session in
      let cancelled = ref false in
      let run ~command ~timeout_ms ~max_output_bytes ~cancelled =
        try process ~command ~timeout_ms ~max_output_bytes
        with Cancelled -> cancelled := true; raise Cancelled in
      let observe ~command ~timeout_ms ~max_output_bytes ~cancelled:_ =
        process ~command ~timeout_ms ~max_output_bytes in
      (try ignore (Workspace_mobile_environment.execute session ~build_hash
        ~approved:true ~cancelled ~run ~observe plan)
       with Workspace_mobile_environment.Error message -> fail message);
      if mobile_environment_build_hash root session <> build_hash then
        fail "selected app build changed during environment transition; restoration remains available";
      Yojson.Basic.to_string (`Assoc [
        "plan_id", `String (required_string "plan_id" args);
        "status", `String "applied";
        "build_sha256", `String build_hash;
        "target", `String (mobile_environment_value plan.target);
        "restore_action", `String "restore"])
  | "restore" ->
      let plan_id = required_string "plan_id" args in
      let plan = mobile_environment_plan context plan_id in
      let session = mobile_environment_session ~context ~root args in
      let build_hash = mobile_environment_build_hash root session in
      let cancelled = ref false in
      let run ~command ~timeout_ms ~max_output_bytes ~cancelled =
        try process ~command ~timeout_ms ~max_output_bytes
        with Cancelled -> cancelled := true; raise Cancelled in
      let observe ~command ~timeout_ms ~max_output_bytes ~cancelled:_ =
        process ~command ~timeout_ms ~max_output_bytes in
      let ownership : Workspace_mobile_environment.ownership = { plan } in
      let result = try Workspace_mobile_environment.restore session ~build_hash
          ~approved:true ~cancelled ~run ~observe ownership
        with Workspace_mobile_environment.Error message -> fail message in
      (match result with
       | Workspace_mobile_environment.Restored
       | Workspace_mobile_environment.Already_changed ->
           Mutex.lock context.mobile_lock;
           Hashtbl.remove context.mobile_environment_plans plan_id;
           Mutex.unlock context.mobile_lock
       | Workspace_mobile_environment.Restore_failed _ -> ());
      let status = match result with
        | Workspace_mobile_environment.Restored -> "restored"
        | Workspace_mobile_environment.Already_changed -> "state_changed_not_restored"
        | Workspace_mobile_environment.Restore_failed message -> "restore_failed: " ^ message in
      Yojson.Basic.to_string (`Assoc [
        "plan_id", `String plan_id;
        "status", `String status;
        "build_sha256", `String build_hash;
        "original", `String (mobile_environment_value plan.before)])
  | _ -> fail "mobile environment action must be preview, apply or restore"

let mobile_lifecycle_session ~context ~root args =
  let id = required_string "session_id" args in
  let session = try Workspace_mobile_run.get context.mobile_run_manager id
    with Workspace_mobile_run.Error message -> fail message in
  if session.root <> root then
    fail "mobile lifecycle session belongs to a different workspace root";
  if session.platform <> Workspace_mobile_run.Android then
    fail "live selected-app lifecycle operations are currently Android-only";
  if session.state = Workspace_mobile_run.Selected then
    fail "mobile lifecycle operations require a built selected app";
  session

let mobile_lifecycle_identity root session =
  try
    let build = Workspace_mobile_report.build_identity root session in
    Workspace_mobile_app_lifecycle.identity_of_session session
      ~build_id:build.Workspace_mobile_report.build_hash
  with
  | Workspace_mobile_report.Error message
  | Workspace_mobile_app_lifecycle.Error message -> fail message

let mobile_lifecycle_next_generation context session_id =
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    let previous = Option.value ~default:0
      (Hashtbl.find_opt context.mobile_lifecycle_generations session_id) in
    if previous = max_int then fail "mobile lifecycle observation generation exhausted";
    let generation = previous + 1 in
    Hashtbl.replace context.mobile_lifecycle_generations session_id generation;
    generation)

let mobile_lifecycle_observation context session_id =
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.find_opt context.mobile_lifecycle_observations session_id)

let mobile_lifecycle_cache_handlers context session_id =
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.find_opt context.mobile_lifecycle_handlers session_id)

let mobile_lifecycle_process ?cancel ?on_progress ~root command =
  let result = Workspace_process.run_shell ?cancel ?on_progress
    ~timeout_seconds:20
    ~output_limit:Workspace_mobile_app_lifecycle.max_manifest_bytes
    ~cwd:(Some root) ~command () in
  match result.termination with
  | Workspace_process.Exited 0 when not result.truncated -> result.output
  | Workspace_process.Exited code ->
      fail (Printf.sprintf "mobile lifecycle command exited %d%s%s" code
        (if result.truncated then " with truncated output" else "")
        (if result.output = "" then "" else ": " ^ result.output))
  | Workspace_process.Signaled signal ->
      fail (Printf.sprintf "mobile lifecycle command received signal %d" signal)
  | Workspace_process.Timed_out -> fail "mobile lifecycle command timed out"
  | Workspace_process.Cancelled -> raise Cancelled

let mobile_lifecycle_accessibility ?cancel ?on_progress ~root session =
  let command = try Workspace_mobile_observe.command "accessibility" session
    with Workspace_mobile_observe.Error message -> fail message in
  let output = mobile_lifecycle_process ?cancel ?on_progress ~root command in
  try Workspace_mobile_observe.parse_accessibility output
  with Workspace_mobile_observe.Error message -> fail message

let mobile_lifecycle_observe ?cancel ?on_progress ~context ~root
    (session : Workspace_mobile_run.session) =
  if session.state <> Workspace_mobile_run.Running then
    fail "fresh lifecycle observation requires a running selected-app session";
  let before = mobile_lifecycle_identity root session in
  let generation = mobile_lifecycle_next_generation context session.id in
  let command = try Workspace_mobile_app_lifecycle.observation_command session
    with Workspace_mobile_app_lifecycle.Error message -> fail message in
  let output = mobile_lifecycle_process ?cancel ?on_progress ~root command in
  let after = mobile_lifecycle_identity root session in
  if not (Workspace_mobile_app_lifecycle.same_identity before after) then
    fail "selected app build changed during lifecycle observation";
  let observation = try Workspace_mobile_app_lifecycle.parse_observation
      ~identity:after ~generation output
    with Workspace_mobile_app_lifecycle.Error message -> fail message in
  Mutex.lock context.mobile_lock;
  Hashtbl.replace context.mobile_lifecycle_observations session.id observation;
  Mutex.unlock context.mobile_lock;
  observation

let mobile_lifecycle_transition = function
  | "background" -> Workspace_mobile_app_lifecycle.Background
  | "resume" -> Workspace_mobile_app_lifecycle.Resume
  | "recreate_process" -> Workspace_mobile_app_lifecycle.Recreate_process
  | _ -> fail "transition must be background, resume or recreate_process"

let mobile_app_lifecycle_tool ~approved ?cancel ?on_progress ~context ~root args =
  let action = required_string "action" args in
  if not approved && not (List.mem action ["list_scenarios"; "show_scenario"]) then
    fail "mobile app lifecycle actions require explicit interactive approval";
  let context = require_session_context (Some context) in
  let root = Workspace_path.root_path root in
  let get_scenario name =
    try Workspace_mobile_app_lifecycle.load ~root name
    with Workspace_mobile_app_lifecycle.Error message -> fail message in
  match action with
  | "inspect_handlers" ->
      let session = mobile_lifecycle_session ~context ~root args in
      Mutex.lock context.mobile_lock;
      Hashtbl.remove context.mobile_lifecycle_handlers session.id;
      Mutex.unlock context.mobile_lock;
      let before = mobile_lifecycle_identity root session in
      let apk = Workspace_path.checked_path root session.app_path in
      let command = "apkanalyzer manifest print " ^ Filename.quote apk in
      let output = mobile_lifecycle_process ?cancel ?on_progress ~root command in
      let handlers = try Workspace_mobile_app_lifecycle.manifest_handlers
          ~app_id:session.app_id output
        with Workspace_mobile_app_lifecycle.Error message -> fail message in
      let after = mobile_lifecycle_identity root session in
      if not (Workspace_mobile_app_lifecycle.same_identity before after) then
        fail "selected APK changed during URL-handler inspection";
      Mutex.lock context.mobile_lock;
      Hashtbl.replace context.mobile_lifecycle_handlers session.id
        { build_hash = after.build_id; handlers };
      Mutex.unlock context.mobile_lock;
      let indexed = List.mapi (fun index
          (handler : Workspace_mobile_app_lifecycle.handler) ->
        `Assoc [
          "handler_id", `String ("handler-" ^ string_of_int (index + 1));
          "activity", `String handler.Workspace_mobile_app_lifecycle.activity;
          "scheme", `String handler.scheme; "host", `String handler.host;
          "path", `String handler.path]) handlers in
      Yojson.Basic.to_string (`Assoc [
        "session_id", `String session.id; "app_id", `String session.app_id;
        "build_sha256", `String after.build_id;
        "handler_evidence", `List indexed])
  | "open_link" ->
      let session = mobile_lifecycle_session ~context ~root args in
      if session.state <> Workspace_mobile_run.Running then
        fail "selected-app deep links require the selected app to be running";
      let identity = mobile_lifecycle_identity root session in
      let cache = match mobile_lifecycle_cache_handlers context session.id with
        | Some cache when cache.build_hash = identity.build_id -> cache
        | _ ->
            Mutex.lock context.mobile_lock;
            Hashtbl.remove context.mobile_lifecycle_handlers session.id;
            Mutex.unlock context.mobile_lock;
            fail "inspect URL handlers again; approved manifest evidence is missing or stale" in
      let handler_id = required_string "handler_id" args in
      let handler_index =
        if String.starts_with ~prefix:"handler-" handler_id then
          int_of_string_opt (String.sub handler_id 8 (String.length handler_id - 8))
        else None in
      let handler = match handler_index with
        | Some index when index > 0 ->
            (try List.nth cache.handlers (index - 1) with _ ->
              fail "handler_id is not in the inspected selected-app manifest")
        | _ -> fail "handler_id is not an inspected handler" in
      let command, link = try Workspace_mobile_app_lifecycle.deep_link_preview
          ~approved_evidence:true session ~handler
          ~url:(required_string "url" args)
        with Workspace_mobile_app_lifecycle.Error message -> fail message in
      let output = mobile_lifecycle_process ?cancel ?on_progress ~root command in
      let before = mobile_lifecycle_observe ?cancel ?on_progress
        ~context ~root session in
      let require_handler
          (observation : Workspace_mobile_app_lifecycle.observation) =
        if not (Workspace_mobile_app_lifecycle.same_identity identity observation.identity) then
          fail "selected build changed while verifying the deep-link dispatch";
        if observation.state <> Workspace_mobile_app_lifecycle.Foreground ||
           observation.resumed_activity <> Some handler.activity then
          fail ("deep-link dispatch did not verify the exact handler activity; observed " ^
            Option.value ~default:"no selected-app resumed activity"
              observation.resumed_activity) in
      require_handler before;
      let nodes = mobile_lifecycle_accessibility ?cancel ?on_progress ~root session in
      let after = mobile_lifecycle_observe ?cancel ?on_progress
        ~context ~root session in
      require_handler after;
      let assertion = required_string "destination_assertion" args in
      let verified = try Workspace_mobile_app_lifecycle.verify_deep_link_destination
          ~identity ~generation:before.generation ~link
          ~observation:{ Workspace_mobile_app_lifecycle.identity = identity;
            generation = before.generation; nodes } ~assertion
        with Workspace_mobile_app_lifecycle.Error message -> fail message in
      Yojson.Basic.to_string (`Assoc [
        "session_id", `String session.id; "handler_id", `String handler_id;
        "url", `String link.url; "command_status", `String "exited_zero";
        "output", `String output; "handler_verified", `Bool true;
        "observed_resumed_activity", `String handler.activity;
        "destination_content_verified", `Bool true;
        "destination_assertion", `String verified.assertion;
        "destination_observation_generation", `Int verified.generation;
        "accessibility_node_count", `Int (List.length nodes)])
  | "observe" ->
      let session = mobile_lifecycle_session ~context ~root args in
      let observation = mobile_lifecycle_observe ?cancel ?on_progress
        ~context ~root session in
      Yojson.Basic.to_string (`Assoc [
        "session_id", `String session.id;
        "state", `String (Workspace_mobile_app_lifecycle.state_name observation.state);
        "generation", `Int observation.generation;
        "process_id", (match observation.process_id with None -> `Null | Some pid -> `Int pid);
        "resumed_activity", (match observation.resumed_activity with
          | None -> `Null | Some activity -> `String activity);
        "build_sha256", `String observation.identity.build_id])
  | "create_scenario" ->
      let session = mobile_lifecycle_session ~context ~root args in
      if session.state <> Workspace_mobile_run.Running then
        fail "lifecycle scenario creation requires a running selected app session";
      let identity = mobile_lifecycle_identity root session in
      let observation = match mobile_lifecycle_observation context session.id with
        | Some observation when Workspace_mobile_app_lifecycle.same_identity
            identity observation.identity -> observation
        | _ -> fail "observe the exact selected app after build before creating a lifecycle scenario" in
      if observation.state <> Workspace_mobile_app_lifecycle.Foreground ||
         observation.process_id = None then
        fail "lifecycle scenario creation requires a foreground selected app with a verified PID";
      let name = required_string "name" args in
      let record = try Workspace_mobile_app_lifecycle.create_record ~name ~identity
        with Workspace_mobile_app_lifecycle.Error message -> fail message in
      record.state <- observation.state;
      record.generation <- observation.generation;
      record.process_id <- observation.process_id;
      (try Workspace_mobile_app_lifecycle.save ~root record
       with Workspace_mobile_app_lifecycle.Error message -> fail message);
      Yojson.Basic.to_string (`Assoc [
        "scenario", Workspace_mobile_app_lifecycle.record_json record;
        "device_state_changed", `Bool false])
  | "transition" ->
      let session = mobile_lifecycle_session ~context ~root args in
      if session.state <> Workspace_mobile_run.Running then
        fail "lifecycle transitions require a running selected-app session";
      let identity = mobile_lifecycle_identity root session in
      let record = get_scenario (required_string "name" args) in
      if not (Workspace_mobile_app_lifecycle.same_identity identity record.identity) then
        fail "scenario belongs to a different selected build, app or device";
      let observation = match mobile_lifecycle_observation context session.id with
        | Some observation -> observation
        | None -> fail "take a fresh lifecycle observation before transitioning" in
      let activity = match session.activity with
        | Some activity -> activity
        | None -> fail "selected app session has no approved launch activity" in
      let transition = mobile_lifecycle_transition (required_string "transition" args) in
      let run command = ignore (mobile_lifecycle_process ?cancel ?on_progress ~root command) in
      let observe () = mobile_lifecycle_observe ?cancel ?on_progress
        ~context ~root session in
      let after = try Workspace_mobile_app_lifecycle.run_transition record
          ~approved:true ~observation ~activity ~run ~observe transition
        with exn ->
          (match record.state with
           | Workspace_mobile_app_lifecycle.Failed _ ->
               (try Workspace_mobile_app_lifecycle.update ~root record
                with Workspace_mobile_app_lifecycle.Error message ->
                  fail ("transition failed and its scenario could not be persisted: " ^ message))
           | _ -> ());
          raise exn in
      (try Workspace_mobile_app_lifecycle.update ~root record
       with Workspace_mobile_app_lifecycle.Error message -> fail message);
      Yojson.Basic.to_string (`Assoc [
        "scenario", Workspace_mobile_app_lifecycle.record_json record;
        "observed_state", `String (Workspace_mobile_app_lifecycle.state_name after.state);
        "generation", `Int after.generation;
        "process_id", (match after.process_id with None -> `Null | Some pid -> `Int pid)])
  | "list_scenarios" ->
      let records = try Workspace_mobile_app_lifecycle.list ~root
        with Workspace_mobile_app_lifecycle.Error message -> fail message in
      Yojson.Basic.to_string (`List
        (List.map Workspace_mobile_app_lifecycle.record_json records))
  | "show_scenario" ->
      Workspace_mobile_app_lifecycle.record_json
        (get_scenario (required_string "name" args))
      |> Yojson.Basic.to_string
  | "delete_scenario" ->
      let name = required_string "name" args in
      (try Workspace_mobile_app_lifecycle.delete ~root name
       with Workspace_mobile_app_lifecycle.Error message -> fail message);
      Yojson.Basic.to_string (`Assoc ["deleted_scenario", `String name])
  | _ ->
      fail "mobile_app_lifecycle action must be inspect_handlers, open_link, observe, create_scenario, transition, list_scenarios, show_scenario or delete_scenario"

let mobile_performance_session ~context ~root args =
  let id = required_string "session_id" args in
  let session = try Workspace_mobile_run.get context.mobile_run_manager id
    with Workspace_mobile_run.Error message -> fail message in
  if session.root <> root then
    fail "mobile performance session belongs to a different workspace root";
  if session.state <> Workspace_mobile_run.Running then
    fail "mobile performance requires a running selected app session";
  session

let mobile_performance_identity root session =
  try Workspace_mobile_report.build_identity root session
  with Workspace_mobile_report.Error message -> fail message

let mobile_performance_cached_target context session_id =
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.find_opt context.mobile_performance_targets session_id)

let mobile_performance_cache_target context session_id target =
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.replace context.mobile_performance_targets session_id target)

let mobile_performance_clear_target context session_id =
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.remove context.mobile_performance_targets session_id)

let mobile_performance_cached_templates context session_id =
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.find_opt context.mobile_performance_templates session_id)

let mobile_performance_cache_templates context session_id templates =
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.replace context.mobile_performance_templates session_id templates)

let mobile_performance_ios_process_command
    (session : Workspace_mobile_run.session) =
  if session.platform <> Workspace_mobile_run.Ios then
    fail "iOS process inspection requires an iOS Simulator session";
  "xcrun simctl spawn " ^ Filename.quote session.device ^ " launchctl list"

let mobile_performance_ios_pid app_id output =
  let marker = "UIKitApplication:" ^ app_id ^ "[" in
  let rows = String.split_on_char '\n' output |> List.filter_map (fun line ->
    let fields = String.split_on_char '\t' line
      |> List.concat_map (String.split_on_char ' ')
      |> List.filter (fun value -> value <> "") in
    match fields with
    | pid_text :: _status :: label :: _
      when starts_with label marker && String.ends_with ~suffix:"]" label ->
        (match int_of_string_opt pid_text with
         | Some pid when pid > 0 && pid <= 4_194_304 -> Some pid
         | _ -> fail "simulator launchctl returned an invalid selected-app PID")
    | _ -> None) in
  match List.sort_uniq Int.compare rows with
  | [pid] -> pid
  | [] -> fail "simulator launchctl did not expose one exact UIKitApplication process for the selected app"
  | _ -> fail "simulator launchctl exposed multiple selected-app processes"

let mobile_performance_run ?cancel ?on_progress ~root command =
  Workspace_process.run_shell ?cancel ?on_progress
    ~timeout_seconds:(Workspace_mobile_performance.max_capture_seconds + 15)
    ~output_limit:Workspace_mobile_performance.max_output_bytes
    ~cwd:(Some root) ~command ()


let mobile_performance_exit_code result =
  match result.Workspace_process.termination with
  | Workspace_process.Exited code -> code
  | Workspace_process.Signaled _ | Workspace_process.Timed_out
  | Workspace_process.Cancelled -> -1

let mobile_performance_require_result result =
  match result.Workspace_process.termination with
  | Workspace_process.Cancelled -> raise Cancelled
  | Workspace_process.Exited _ | Workspace_process.Signaled _
  | Workspace_process.Timed_out -> ()

let mobile_performance_revalidate_android_pid ?cancel ?on_progress ~root session pid =
  let before = mobile_performance_identity root session in
  let command = try Workspace_mobile_performance.android_revalidate_pid_command session ~pid
    with Workspace_mobile_performance.Error message -> fail message in
  let result = mobile_performance_run ?cancel ?on_progress ~root command in
  (match result.Workspace_process.termination with
   | Workspace_process.Cancelled -> raise Cancelled
   | Workspace_process.Exited 0 when not result.truncated -> ()
   | _ -> fail "selected Android app PID changed or could not be revalidated immediately before capture");
  let expected = "PAVE_SELECTED_PID=" ^ string_of_int pid in
  let markers = String.split_on_char '\n' result.output |> List.map String.trim
    |> List.filter (String.starts_with ~prefix:"PAVE_SELECTED_PID=") in
  if markers <> [expected] then
    fail "fresh Android PID revalidation did not identify exactly the selected process";
  let after = mobile_performance_identity root session in
  if before <> after then
    fail "selected build changed during immediate Android PID revalidation"

let mobile_performance_revalidate_ios_pid ?cancel ?on_progress ~root session pid =
  let before = mobile_performance_identity root session in
  let result = mobile_performance_run ?cancel ?on_progress ~root
    (mobile_performance_ios_process_command session) in
  (match result.Workspace_process.termination with
   | Workspace_process.Cancelled -> raise Cancelled
   | Workspace_process.Exited 0 when not result.truncated -> ()
   | _ -> fail "selected simulator process list failed immediately before trace capture");
  let current = try mobile_performance_ios_pid session.app_id result.output
    with Tool_error message -> fail message in
  if current <> pid then fail "selected simulator app PID changed immediately before trace capture";
  let after = mobile_performance_identity root session in
  if before <> after then
    fail "selected build changed during immediate simulator PID revalidation"

let mobile_performance_trace_path ~root
    (session : Workspace_mobile_run.session) name =
  if String.length name < 1 || String.length name > 80 ||
     not (String.for_all (function
       | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' -> true
       | _ -> false) name) then
    fail "trace name must contain 1..80 ASCII letters, digits, underscores or hyphens";
  let relative = ".pave/mobile-performance/" ^ session.id ^ "-" ^ name ^ ".trace" in
  let absolute = Filename.concat root relative in
  (try
     ignore (Unix.lstat absolute);
     fail "trace output already exists; choose a new trace name"
   with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  relative, absolute

let mobile_performance_prepare_trace_directory root =
  let root = try Unix.realpath root with _ -> fail "workspace root is unavailable" in
  let pave = Filename.concat root ".pave" in
  (try Unix.mkdir pave 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let pave_stat = try Unix.lstat pave with _ -> fail "workspace .pave directory is unavailable" in
  if pave_stat.Unix.st_kind <> Unix.S_DIR || pave_stat.Unix.st_uid <> Unix.geteuid () then
    fail "workspace .pave path must be an owner-controlled directory";
  let directory = Filename.concat pave "mobile-performance" in
  (try Unix.mkdir directory 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let stat = try Unix.lstat directory with _ -> fail "mobile performance directory is unavailable" in
  if stat.Unix.st_kind <> Unix.S_DIR || stat.Unix.st_uid <> Unix.geteuid () ||
     stat.Unix.st_perm land 0o077 <> 0 then
    fail "mobile performance output directory must be a private owner-controlled directory"

let mobile_performance_android_target context session identity =
  match mobile_performance_cached_target context session.Workspace_mobile_run.id with
  | Some target when target.build_hash = identity.Workspace_mobile_report.build_hash -> target
  | _ ->
      (match mobile_lifecycle_observation context session.id with
       | Some observation
         when observation.identity.build_id = identity.build_hash &&
              observation.state = Workspace_mobile_app_lifecycle.Foreground ->
           (match observation.process_id with
            | Some pid ->
                let target = { build_hash = identity.build_hash; pid } in
                mobile_performance_cache_target context session.id target;
                target
            | None -> fail "fresh selected-app lifecycle observation has no verified PID")
       | _ -> fail "run an approved launch measurement or fresh lifecycle observation for this exact build first")

let mobile_performance_preview ~context ~root args =
  let session = mobile_performance_session ~context ~root args in
  let identity = mobile_performance_identity root session in
  let action = required_string "action" args in
  let command, extra =
    match action, session.platform with
    | "launch", Workspace_mobile_run.Android ->
        let condition = required_string "condition" args in
        Workspace_mobile_performance.android_command ~action session ~condition (),
        [if condition = "cold" then
           "Cold launch force-stops only the selected app; app data is not cleared."
         else
           "Warm launch requires a pre-existing app PID and verifies that the PID survives ActivityManager startup.";
         "The report includes exact condition, one complete ActivityManager sample and the selected PID."]
    | ("frames" | "memory"), Workspace_mobile_run.Android ->
        let condition = required_string "condition" args in
        let target = mobile_performance_android_target context session identity in
        let revalidate = Workspace_mobile_performance.android_revalidate_pid_command
          session ~pid:target.pid in
        Workspace_mobile_performance.android_command ~action session
          ~pid:target.pid ~condition (),
        ["Condition: " ^ condition;
         "Exact selected process ID: " ^ string_of_int target.pid;
         "Run immediately before capture: " ^ revalidate;
         "The capture command independently rechecks the package PID; frames/PSS become available only for the exact PID and a complete known-unit sample."]
    | "ios_templates", Workspace_mobile_run.Ios ->
        Workspace_mobile_performance.ios_templates_command,
        ["Only installed allowlisted templates are cached; no trace is started."]
    | "ios_process", Workspace_mobile_run.Ios ->
        mobile_performance_ios_process_command session,
        ["Reads simulator launchctl process labels and accepts only one exact UIKitApplication bundle-ID match."]
    | "ios_capture", Workspace_mobile_run.Ios ->
        let condition = required_string "condition" args in
        if condition <> "warm" then
          fail "iOS trace capture currently accepts only a verified warm process; cold-start measurement is unavailable";
        let target = match mobile_performance_cached_target context session.id with
          | Some target when target.build_hash = identity.build_hash -> target
          | _ -> fail "inspect the exact selected simulator process again for this build first" in
        let installed = match mobile_performance_cached_templates context session.id with
          | Some cache when cache.template_build_hash = identity.build_hash -> cache.names
          | _ -> fail "list xctrace templates again for this exact build first" in
        let template = required_string "template" args in
        let _, output_path = mobile_performance_trace_path ~root session
            (required_string "name" args) in
        let command = Workspace_mobile_performance.ios_command ~template
          ~installed_templates:installed ~pid:target.pid ~output_path session in
        let revalidate = mobile_performance_ios_process_command session in
        command,
        ["Condition: verified warm process; raw trace only, not a parsed measurement.";
         "Immediately before capture, require this process-list command to return the exact cached PID: " ^ revalidate;
         "Trace bundle: " ^ output_path;
         Printf.sprintf "Capture limit: %d seconds; process output limit: %d bytes."
           Workspace_mobile_performance.max_capture_seconds
           Workspace_mobile_performance.max_output_bytes;
         "No energy or performance counter is inferred from the raw bundle."]
    | _ -> fail "mobile performance action is unsupported for the selected platform" in
  ("Captures one explicitly approved measurement from the exact running selected app; output may contain private runtime data.",
   ["Session/app/device: " ^ session.id ^ " · " ^ session.app_id ^ " · " ^ session.device;
    "Selected build SHA-256: " ^ identity.build_hash;
    "Exact command: " ^ command;
    Printf.sprintf "Deadline: %d seconds; captured output limit: %d bytes."
      (Workspace_mobile_performance.max_capture_seconds + 15)
      Workspace_mobile_performance.max_output_bytes] @ extra)

let mobile_performance_tool ~approved ?cancel ?on_progress ~context ~root args =
  if not approved then fail "mobile performance measurements require explicit interactive approval";
  let context = require_session_context (Some context) in
  let root = Workspace_path.root_path root in
  let session = mobile_performance_session ~context ~root args in
  let identity = mobile_performance_identity root session in
  let action = required_string "action" args in
  let run command =
    let result = mobile_performance_run ?cancel ?on_progress ~root command in
    mobile_performance_require_result result;
    result in
  let result_json report = Workspace_mobile_performance.report_json report in
  match action, session.platform with
  | "launch", Workspace_mobile_run.Android ->
      mobile_performance_clear_target context session.id;
      let condition = required_string "condition" args in
      let command = Workspace_mobile_performance.android_command ~action session ~condition () in
      let result = run command in
      let after = mobile_performance_identity root session in
      if identity <> after then
        fail "selected build changed during Android launch measurement";
      let report = try Workspace_mobile_performance.parse_android_launch session
          ~condition ~output:result.output ~truncated:result.truncated
          ~exit_code:(mobile_performance_exit_code result)
        with Workspace_mobile_performance.Error message -> fail message in
      (match report.pid with
       | Some pid when report.status = "available" ->
           mobile_performance_revalidate_android_pid ?cancel ?on_progress
             ~root session pid;
           mobile_performance_cache_target context session.id
             { build_hash = identity.build_hash; pid }
       | _ -> ());
      result_json { report with build = identity.build_hash }
  | ("frames" | "memory"), Workspace_mobile_run.Android ->
      let condition = required_string "condition" args in
      let target = mobile_performance_android_target context session identity in
      mobile_performance_revalidate_android_pid ?cancel ?on_progress
        ~root session target.pid;
      let command = try Workspace_mobile_performance.android_command
          ~action session ~pid:target.pid ~condition ()
        with Workspace_mobile_performance.Error message -> fail message in
      let result = run command in
      let after = mobile_performance_identity root session in
      if identity <> after then
        fail "selected build changed during Android measurement";
      let report = try Workspace_mobile_performance.parse_android ~action session
          ~pid:target.pid ~condition ~output:result.output ~truncated:result.truncated
          ~exit_code:(mobile_performance_exit_code result)
        with Workspace_mobile_performance.Error message -> fail message in
      result_json { report with build = identity.build_hash }
  | "ios_templates", Workspace_mobile_run.Ios ->
      mobile_performance_clear_target context session.id;
      let result = run Workspace_mobile_performance.ios_templates_command in
      let after = mobile_performance_identity root session in
      if identity <> after then
        fail "selected iOS build changed while listing Instruments templates";
      let templates = try Workspace_mobile_performance.ios_templates
          result.output ~truncated:result.truncated
        with Workspace_mobile_performance.Error message -> fail message in
      let templates = if mobile_performance_exit_code result = 0 then templates else [] in
      mobile_performance_cache_templates context session.id
        { template_build_hash = identity.build_hash; names = templates };
      Yojson.Basic.to_string (`Assoc [
        "session_id", `String session.id; "status",
        `String (if templates = [] then "unavailable" else "available");
        "installed_templates", `List (List.map (fun name -> `String name) templates);
        "reason", `String (if templates = [] then
          "no allowlisted xctrace template was confirmed" else "listed by xctrace")])
  | "ios_process", Workspace_mobile_run.Ios ->
      mobile_performance_clear_target context session.id;
      let result = run (mobile_performance_ios_process_command session) in
      if mobile_performance_exit_code result <> 0 || result.truncated then
        fail "simulator launchctl process listing did not complete successfully";
      let after = mobile_performance_identity root session in
      if identity <> after then
        fail "selected iOS build changed during process inspection";
      let pid = try mobile_performance_ios_pid session.app_id result.output
        with Tool_error message -> fail message in
      mobile_performance_cache_target context session.id
        { build_hash = identity.build_hash; pid };
      Yojson.Basic.to_string (`Assoc [
        "session_id", `String session.id; "app_id", `String session.app_id;
        "device", `String session.device; "process_id", `Int pid;
        "source", `String "xcrun simctl spawn launchctl list";
        "status", `String "available"])
  | "ios_capture", Workspace_mobile_run.Ios ->
      let condition = required_string "condition" args in
      if condition <> "warm" then
        fail "iOS trace capture currently accepts only a verified warm process; cold-start measurement is unavailable";
      let target = match mobile_performance_cached_target context session.id with
        | Some target when target.build_hash = identity.build_hash -> target
        | _ -> fail "inspect the exact selected simulator process again for this build first" in
      let installed = match mobile_performance_cached_templates context session.id with
        | Some cache when cache.template_build_hash = identity.build_hash -> cache.names
        | _ -> fail "list xctrace templates again for this exact build first" in
      let template = required_string "template" args in
      let relative, output_path = mobile_performance_trace_path ~root session
          (required_string "name" args) in
      let command = try Workspace_mobile_performance.ios_command
          ~template ~installed_templates:installed ~pid:target.pid ~output_path session
        with Workspace_mobile_performance.Error message -> fail message in
      mobile_performance_prepare_trace_directory root;
      mobile_performance_revalidate_ios_pid ?cancel ?on_progress
        ~root session target.pid;
      let result = run command in
      let after = mobile_performance_identity root session in
      if identity <> after then
        fail "selected iOS build changed during Instruments capture";
      let stat = try Unix.lstat output_path with _ -> fail "xctrace completed without creating its trace bundle" in
      if stat.Unix.st_kind <> Unix.S_DIR || stat.Unix.st_uid <> Unix.geteuid () then
        fail "xctrace output must be an owner-controlled trace directory";
      (try Unix.chmod output_path 0o700 with _ -> fail "could not make the trace bundle private");
      let trace_hash = try Workspace_mobile_report.hash_build root relative
        with Workspace_mobile_report.Error message -> fail message in
      let report = try Workspace_mobile_performance.parse_ios_capture session
          ~template ~pid:target.pid ~condition ~output_path ~output:result.output
          ~truncated:result.truncated ~exit_code:(mobile_performance_exit_code result)
        with Workspace_mobile_performance.Error message -> fail message in
      let report = { report with build = identity.build_hash;
        status = if report.status = "available" then "trace_captured" else report.status } in
      Yojson.Basic.to_string (`Assoc [
        "report", Yojson.Basic.from_string (result_json report);
        "trace_sha256", `String trace_hash;
        "counters_parsed", `Bool false;
        "counter_status_reason", `String "raw xctrace bundle captured; supported counter export is not implemented"])
  | _ -> fail "mobile performance action is unsupported for the selected platform"

let mobile_device_cache context inventory_id =
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.find_opt context.mobile_device_inventories inventory_id)

let mobile_device_state_key inventory target =
  Workspace_mobile_device_lifecycle.inventory_id inventory ^ "\000" ^
  Workspace_mobile_device_lifecycle.target_id target

let mobile_device_run ?cancel ?on_progress ~root ?(timeout_seconds = 15) command =
  let result = Workspace_process.run_shell ?cancel ?on_progress
    ~timeout_seconds ~output_limit:Workspace_mobile_device_lifecycle.max_output_bytes
    ~cwd:(Some root) ~command () in
  match result.termination with
  | Workspace_process.Exited 0 when not result.truncated -> result.output
  | Workspace_process.Exited code ->
      fail (Printf.sprintf "mobile device command exited %d%s%s" code
        (if result.truncated then " with truncated output" else "")
        (if result.output = "" then "" else ": " ^ result.output))
  | Workspace_process.Signaled signal ->
      fail (Printf.sprintf "mobile device command received signal %d" signal)
  | Workspace_process.Timed_out -> fail "mobile device command timed out"
  | Workspace_process.Cancelled -> raise Cancelled

let mobile_device_inventory_ids context =
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    if context.next_mobile_device_session = max_int ||
       context.next_mobile_device_inventory = max_int then
      fail "mobile device inventory ID space exhausted";
    context.next_mobile_device_session <- context.next_mobile_device_session + 1;
    context.next_mobile_device_inventory <- context.next_mobile_device_inventory + 1;
    "device-session-" ^ string_of_int context.next_mobile_device_session,
    "device-inventory-" ^ string_of_int context.next_mobile_device_inventory)

let mobile_device_store_inventory context cache =
  let id = Workspace_mobile_device_lifecycle.inventory_id cache.device_inventory in
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    if Hashtbl.length context.mobile_device_inventories >= 32 then
      fail "mobile device inventory limit reached; use a new private session";
    Hashtbl.replace context.mobile_device_inventories id cache)

let mobile_device_require_cache context ~root args =
  let inventory_id = required_string "inventory_id" args in
  match mobile_device_cache context inventory_id with
  | Some cache when cache.device_root = root -> cache
  | Some _ -> fail "mobile device inventory belongs to a different workspace root"
  | None -> fail "mobile device inventory is absent or stale; take a fresh approved inventory"

let mobile_device_target cache args =
  match cache.device_platform with
  | Workspace_mobile_run.Android ->
      let name = required_string "target_name" args in
      let port = match field "port" args with
        | `Int value -> value
        | _ -> fail "Android AVD boot requires an exact console port" in
      Workspace_mobile_device_lifecycle.Android_avd { name; port }
  | Workspace_mobile_run.Ios ->
      Workspace_mobile_device_lifecycle.Ios_simulator {
        id = required_string "simulator_id" args }

let mobile_device_approval inventory action target =
  Workspace_mobile_device_lifecycle.approval
    ~marker:"interactive-mobile-device-effect"
    ~action ~session_id:(Workspace_mobile_device_lifecycle.inventory_session_id inventory)
    ~inventory_id:(Workspace_mobile_device_lifecycle.inventory_id inventory)
    ~target_id:(Workspace_mobile_device_lifecycle.target_id target)

let mobile_device_android_snapshot ?cancel ?on_progress ~root ~session_id
    ~inventory_id ~subroot ~configured_avds ?expected_devices () =
  let inventory_args action = `Assoc [
    "action", `String action; "subroot", `String subroot] in
  let list_command, cwd = android_device_command ~root (inventory_args "avds") in
  let device_command, _ = android_device_command ~root (inventory_args "devices") in
  let names = mobile_device_run ?cancel ?on_progress ~root list_command in
  let names = try Workspace_android_devices.avds names
    with Workspace_android_devices.Error message -> fail message in
  if List.sort String.compare names <>
     List.sort String.compare configured_avds then
    fail "configured AVD inventory changed; refresh android_devices before this action";
  let output = mobile_device_run ?cancel ?on_progress ~root device_command in
  let devices = try Workspace_android_devices.adb_devices output
    with Workspace_android_devices.Error message -> fail message in
  (match expected_devices with
   | Some expected when
       List.sort (fun (a : Workspace_android_devices.device) b ->
         compare (a.serial, a.state, a.emulator) (b.serial, b.state, b.emulator)) devices <>
       List.sort (fun (a : Workspace_android_devices.device) b ->
         compare (a.serial, a.state, a.emulator) (b.serial, b.state, b.emulator)) expected ->
       fail "Android device transports changed; refresh android_devices before this action"
   | Some _ | None -> ());
  let bindings = ref [] and complete = ref true in
  List.iter (fun (device : Workspace_android_devices.device) ->
    if device.emulator then
      if device.state <> Workspace_android_devices.Ready then complete := false
      else
        let command = Workspace_mobile_device_lifecycle.avd_name_command device.serial in
        let output = mobile_device_run ?cancel ?on_progress ~root command in
        let name = try Workspace_mobile_device_lifecycle.parse_avd_name output
          with Workspace_mobile_device_lifecycle.Error message -> fail message in
        if not (List.mem name names) then
          fail "running emulator AVD name is absent from the configured inventory";
        bindings := (name, device.serial) :: !bindings) devices;
  let bindings = List.rev !bindings in
  let avd_names = List.map fst bindings in
  if List.length avd_names <> List.length (List.sort_uniq String.compare avd_names) then
    fail "multiple emulator serials identify the same AVD";
  let inventory = try Workspace_mobile_device_lifecycle.create_inventory
      ~session_id ~inventory_id ~configured_avds:names
      ~android_devices:devices ~avd_bindings:bindings
      ~android_bindings_complete:!complete
      ~configured_simulator_ids:[] ~ios_destinations:[] ~compatible_simulators:[]
    with Workspace_mobile_device_lifecycle.Error message -> fail message in
  inventory, cwd

let mobile_device_xcode_discovery context ~root ~bundle ~scheme =
  Mutex.lock context.xcode_lock;
  let discovery = context.xcode_discovery in
  Mutex.unlock context.xcode_lock;
  let discovery = match discovery with
    | Some discovery when discovery.root = root && discovery.bundle = bundle -> discovery
    | _ -> fail "approve Xcode scheme, destination and simulator discovery for this exact bundle first" in
  let fingerprint = try Workspace_xcode.fingerprint ~root ~bundle
    with Workspace_xcode.Error message -> fail message in
  if discovery.fingerprint <> fingerprint then
    fail "Xcode project changed; refresh scheme and simulator discovery";
  let destinations = match List.assoc_opt scheme discovery.destinations with
    | Some destinations when List.mem scheme discovery.schemes -> destinations
    | _ -> fail "scheme was not discovered for this Xcode bundle" in
  if not (List.mem_assoc scheme discovery.simulators) then
    fail "approve compatible simulator inventory for this scheme first";
  destinations

let mobile_device_ios_snapshot ?cancel ?on_progress ~root ~session_id
    ~inventory_id ~destinations () =
  let output = mobile_device_run ?cancel ?on_progress ~root
      "xcrun simctl list devices available -j" in
  let configured = try Workspace_mobile_device_lifecycle.configured_simulator_ids output
    with Workspace_mobile_device_lifecycle.Error message -> fail message in
  let compatible = try Workspace_xcode.compatible_simulators ~destinations output
    with Workspace_xcode.Error message -> fail message in
  let inventory = try Workspace_mobile_device_lifecycle.create_inventory
      ~session_id ~inventory_id ~configured_avds:[] ~android_devices:[]
      ~avd_bindings:[] ~android_bindings_complete:true
      ~configured_simulator_ids:configured ~ios_destinations:destinations
      ~compatible_simulators:compatible
    with Workspace_mobile_device_lifecycle.Error message -> fail message in
  inventory, compatible, output


let mobile_device_inventory_tool ?cancel ?on_progress ~context ~root args =
  let platform = match required_string "platform" args with
    | "android" -> Workspace_mobile_run.Android
    | "ios" -> Workspace_mobile_run.Ios
    | _ -> fail "platform must be android or ios" in
  let session_id, inventory_id = mobile_device_inventory_ids context in
  let cache =
    match platform with
    | Workspace_mobile_run.Android ->
        let subroot = required_string "subroot" args in
        let android, avds =
          Mutex.lock context.mobile_lock;
          Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
            match context.android_inventory, context.android_avds with
            | Some android, Some avds
              when android.root = root && android.subroot = subroot &&
                   avds.avd_root = root && avds.avd_subroot = subroot ->
                android, avds
            | _ -> fail "approve Android AVD and device inventories for this exact project first") in
        let inventory, _ = mobile_device_android_snapshot ?cancel ?on_progress ~root
            ~session_id ~inventory_id ~subroot ~configured_avds:avds.avd_names
            ~expected_devices:android.devices () in
        { device_root = root; device_platform = platform; device_subroot = subroot;
          device_scheme = ""; device_inventory = inventory }
    | Workspace_mobile_run.Ios ->
        let bundle = required_string "subroot" args in
        let scheme = required_string "scheme" args in
        let destinations = mobile_device_xcode_discovery context ~root ~bundle ~scheme in
        let inventory, _, _ = mobile_device_ios_snapshot ?cancel ?on_progress ~root
            ~session_id ~inventory_id ~destinations () in

        { device_root = root; device_platform = platform; device_subroot = bundle;
          device_scheme = scheme; device_inventory = inventory } in
  mobile_device_store_inventory context cache;
  let inventory = cache.device_inventory in
  let targets = match platform with
    | Workspace_mobile_run.Android ->
        List.map (fun name ->
          `Assoc ["target_name", `String name; "requires_console_port", `Bool true])
          inventory.configured_avds
    | Workspace_mobile_run.Ios ->
        List.map (fun (device : Workspace_xcode.simulator) ->
          `Assoc ["simulator_id", `String device.id; "name", `String device.name;
            "runtime", `String device.runtime; "state", `String device.state])
          inventory.compatible_simulators in
  Yojson.Basic.to_string (`Assoc [
    "device_session_id", `String session_id; "inventory_id", `String inventory_id;
    "platform", `String (Workspace_mobile_run.platform_name platform);
    "targets", `List targets;
    "physical_devices_selectable", `Bool false;
    "image_or_runtime_readiness", `String "unknown until boot readiness observation"])

let mobile_device_job_id context =
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    if context.next_mobile_device_process = max_int then
      fail "mobile device process ID space exhausted";
    context.next_mobile_device_process <- context.next_mobile_device_process + 1;
    "pave-device-" ^ string_of_int context.next_mobile_device_process)

let mobile_device_current_android ?cancel ?on_progress ~root cache =
  let inventory = cache.device_inventory in
  let fresh, cwd = mobile_device_android_snapshot ?cancel ?on_progress ~root
      ~session_id:(Workspace_mobile_device_lifecycle.inventory_session_id inventory)
      ~inventory_id:(Workspace_mobile_device_lifecycle.inventory_id inventory)
      ~subroot:cache.device_subroot ~configured_avds:inventory.configured_avds
      ~expected_devices:inventory.android_devices () in
  { cache with device_inventory = fresh }, cwd

let mobile_device_current_ios ?cancel ?on_progress ~context ~root cache =
  let inventory = cache.device_inventory in
  let destinations = mobile_device_xcode_discovery context ~root
      ~bundle:cache.device_subroot ~scheme:cache.device_scheme in
  let fresh, simulators, output = mobile_device_ios_snapshot ?cancel ?on_progress ~root
      ~session_id:(Workspace_mobile_device_lifecycle.inventory_session_id inventory)
      ~inventory_id:(Workspace_mobile_device_lifecycle.inventory_id inventory)
      ~destinations () in
  { cache with device_inventory = fresh }, simulators, output
let mobile_device_lookup_target_state table context cache target =
  let key = mobile_device_state_key cache.device_inventory target in
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.find_opt table key)

let mobile_device_store_target_state table context cache target value =
  let key = mobile_device_state_key cache.device_inventory target in
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.replace table key value)

let mobile_device_remove_target_state table context cache target =
  let key = mobile_device_state_key cache.device_inventory target in
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.remove table key)

let mobile_device_replace_inventory context cache =
  let id = Workspace_mobile_device_lifecycle.inventory_id cache.device_inventory in
  Mutex.lock context.mobile_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.replace context.mobile_device_inventories id cache)

let mobile_device_check_approved approved =
  if not approved then
    fail "mobile device lifecycle effects require exact explicit interactive approval"

let mobile_device_process_job_status context job_id =
  try Workspace_process.job_status context.process_manager ~id:job_id
  with Workspace_process.Error message -> fail message

let mobile_device_wait_job ?cancel context job_id seconds =
  let rec loop remaining =
    (match cancel with Some cancelled when cancelled () -> raise Cancelled | _ -> ());
    match mobile_device_process_job_status context job_id with
    | Workspace_process.Completed result -> Some result
    | Workspace_process.Running when remaining <= 0 -> None
    | Workspace_process.Running ->
        ignore (try Workspace_process.wait_job context.process_manager
            ~id:job_id ~timeout_seconds:1 ()
          with Workspace_process.Error message -> fail message);
        loop (remaining - 1) in
  loop seconds

let mobile_device_process_output context job_id =
  try Workspace_process.read_output context.process_manager ~id:job_id
      ~max_bytes:Workspace_mobile_device_lifecycle.max_output_bytes ()
  with Workspace_process.Error message -> fail message

let mobile_device_android_live_inventory ?cancel ?on_progress ~root cache target =
  let previous = cache.device_inventory in
  let output = mobile_device_run ?cancel ?on_progress ~root "adb devices" in
  let devices = try Workspace_android_devices.adb_devices output
    with Workspace_android_devices.Error message -> fail message in
  let serial = Workspace_mobile_device_lifecycle.target_serial target in
  let target_device = List.find_opt (fun (device : Workspace_android_devices.device) ->
    device.serial = serial) devices in
  let avd_name_output, bindings = match target_device with
    | Some { emulator = true; state = Workspace_android_devices.Ready; _ } ->
        let avd_name_output = mobile_device_run ?cancel ?on_progress ~root
            (Workspace_mobile_device_lifecycle.avd_name_command serial) in
        let name = try Workspace_mobile_device_lifecycle.parse_avd_name avd_name_output
          with Workspace_mobile_device_lifecycle.Error message -> fail message in
        if not (List.mem name previous.configured_avds) then
          fail "running emulator AVD identity changed outside the configured inventory";
        Some avd_name_output, [name, serial]
    | _ -> None, [] in
  let fresh = try Workspace_mobile_device_lifecycle.create_inventory
      ~session_id:(Workspace_mobile_device_lifecycle.inventory_session_id previous)
      ~inventory_id:(Workspace_mobile_device_lifecycle.inventory_id previous)
      ~configured_avds:previous.configured_avds ~android_devices:devices
      ~avd_bindings:bindings ~android_bindings_complete:false
      ~configured_simulator_ids:[] ~ios_destinations:[] ~compatible_simulators:[]
    with Workspace_mobile_device_lifecycle.Error message -> fail message in
  { cache with device_inventory = fresh }, output, avd_name_output

let mobile_device_status_tool ~context ~root args =
  let cache = mobile_device_require_cache context ~root args in
  let inventory = cache.device_inventory in
  let session_id = Workspace_mobile_device_lifecycle.inventory_session_id inventory in
  if required_string "device_session_id" args <> session_id then
    fail "device session ID does not match this inventory";
  let prefix = Workspace_mobile_device_lifecycle.inventory_id inventory ^ "\000" in
  Mutex.lock context.mobile_lock;
  let booting = Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.fold (fun key _ rows ->
      if starts_with key prefix then
        let target = String.sub key (String.length prefix)
            (String.length key - String.length prefix) in
        `Assoc ["target_id", `String target; "state", `String "booting"] :: rows
      else rows) context.mobile_device_booting []) in
  Mutex.lock context.mobile_lock;
  let managed = Fun.protect ~finally:(fun () -> Mutex.unlock context.mobile_lock) (fun () ->
    Hashtbl.fold (fun key state rows ->
      if starts_with key prefix then
        let target = String.sub key (String.length prefix)
            (String.length key - String.length prefix) in
        let ownership = match Workspace_mobile_device_lifecycle.ownership state with
          | Workspace_mobile_device_lifecycle.Preexisting -> "preexisting"
          | Workspace_mobile_device_lifecycle.Owned _ -> "owned" in
        `Assoc ["target_id", `String target; "state", `String ownership] :: rows
      else rows) context.mobile_device_managed []) in
  Yojson.Basic.to_string (`Assoc [
    "device_session_id", `String session_id;
    "inventory_id", `String inventory.inventory_id;
    "booting", `List (List.rev booting);
    "managed", `List (List.rev managed)])

let mobile_device_execution_cache context ~root args =
  let cache = mobile_device_require_cache context ~root args in
  if required_string "device_session_id" args <>
     Workspace_mobile_device_lifecycle.inventory_session_id cache.device_inventory then
    fail "device session ID does not match this inventory";
  cache

let mobile_device_error result =
  match result with
  | Workspace_process.Exited 0 -> ()
  | Workspace_process.Exited code ->
      fail (Printf.sprintf "owned emulator launcher exited %d" code)
  | Workspace_process.Signaled signal ->
      fail (Printf.sprintf "owned emulator launcher received signal %d" signal)
  | Workspace_process.Timed_out -> fail "owned simulator boot command timed out"
  | Workspace_process.Cancelled -> raise Cancelled

let mobile_device_store_ready context cache target managed =
  mobile_device_store_target_state context.mobile_device_managed context cache target managed;
  mobile_device_remove_target_state context.mobile_device_booting context cache target

let mobile_device_boot_tool ~approved ?cancel ?on_progress ~context ~root args =
  mobile_device_check_approved approved;
  let cache = mobile_device_execution_cache context ~root args in
  let target = mobile_device_target cache args in
  let inventory_id = Workspace_mobile_device_lifecycle.inventory_id cache.device_inventory in
  let existing = mobile_device_lookup_target_state context.mobile_device_managed
      context cache target in
  if Option.is_some existing then fail "this exact device already has a lifecycle ownership record";
  if Option.is_some (mobile_device_lookup_target_state context.mobile_device_booting
      context cache target) then
    fail "this exact device already has a pending boot; use readiness or abort_boot";
  let cache, cwd = match cache.device_platform with
    | Workspace_mobile_run.Android ->
        mobile_device_current_android ?cancel ?on_progress ~root cache
    | Workspace_mobile_run.Ios ->
        let fresh, _, _ = mobile_device_current_ios ?cancel ?on_progress
            ~context ~root cache in
        fresh, root in

  mobile_device_replace_inventory context cache;
  let ownership_id = "ownership-" ^ mobile_device_job_id context in
  let approval = mobile_device_approval cache.device_inventory
      Workspace_mobile_device_lifecycle.Boot target in
  let plan = try Workspace_mobile_device_lifecycle.boot_command
      ~inventory:cache.device_inventory ~approval ~target ~ownership_id
    with Workspace_mobile_device_lifecycle.Error message -> fail message in
  match plan with
  | Workspace_mobile_device_lifecycle.Already_booted managed ->
      mobile_device_store_target_state context.mobile_device_managed
        context cache (Workspace_mobile_device_lifecycle.managed_target managed) managed;
      Yojson.Basic.to_string (`Assoc [
        "status", `String "preexisting";
        "target_id", `String (Workspace_mobile_device_lifecycle.target_id
          (Workspace_mobile_device_lifecycle.managed_target managed));
        "device_session_id", `String
          (Workspace_mobile_device_lifecycle.inventory_session_id cache.device_inventory);
        "inventory_id", `String inventory_id;
        "shutdown_owned_device", `Bool false])
  | Workspace_mobile_device_lifecycle.Start { command; pending } ->
      (match cancel with Some cancelled when cancelled () -> raise Cancelled | _ -> ());
      let job_id = mobile_device_job_id context in
      (try Workspace_process.start_shell context.process_manager ~id:job_id
          ~cwd:(Some cwd) ~output_limit:Workspace_mobile_device_lifecycle.max_output_bytes
          ~command ()
       with Workspace_process.Error message -> fail message);
      let booting = try Workspace_mobile_device_lifecycle.settle_launch pending
          (`Started job_id)
        with Workspace_mobile_device_lifecycle.Error message -> fail message in
      let booting = match booting with
        | Some booting -> booting
        | None -> fail "owned device launch did not create a pending boot" in
      mobile_device_store_target_state context.mobile_device_booting
        context cache target booting;
      (match cache.device_platform with
       | Workspace_mobile_run.Android ->
           (match mobile_device_process_job_status context job_id with
            | Workspace_process.Completed termination -> mobile_device_error termination
            | Workspace_process.Running -> ())
       | Workspace_mobile_run.Ios ->
           let result =
             try mobile_device_wait_job ?cancel context job_id 45
             with Cancelled ->
               (try Workspace_process.kill_job context.process_manager ~id:job_id
                with Workspace_process.Error message ->
                  fail ("cancelled simulator boot launcher could not be reaped: " ^ message));
               raise Cancelled in
           (match result with
            | None ->
                (try Workspace_process.kill_job context.process_manager ~id:job_id
                 with Workspace_process.Error message ->
                   fail ("simulator boot timed out and its owned launcher could not be reaped: " ^
                     message));
                fail "simctl boot command did not complete before its deadline; pending ownership retained"
            | Some termination -> mobile_device_error termination));
      Yojson.Basic.to_string (`Assoc [
        "status", `String "booting";
        "target_id", `String (Workspace_mobile_device_lifecycle.target_id target);
        "device_session_id", `String
          (Workspace_mobile_device_lifecycle.inventory_session_id cache.device_inventory);
        "inventory_id", `String inventory_id;
        "launcher_job_id", `String job_id;
        "readiness_action", `String "readiness";
        "abort_action", `String "abort_boot"])

let mobile_device_readiness_tool ~approved ?cancel ?on_progress ~context ~root args =
  mobile_device_check_approved approved;
  let cache = mobile_device_execution_cache context ~root args in
  let target = mobile_device_target cache args in
  let booting = mobile_device_lookup_target_state context.mobile_device_booting
      context cache target in
  match booting with
  | None ->
      (match mobile_device_lookup_target_state context.mobile_device_managed
          context cache target with
       | Some managed ->
           Yojson.Basic.to_string (`Assoc [
             "status", `String "already_managed";
             "target_id", `String (Workspace_mobile_device_lifecycle.target_id target);
             "ownership", `String (match Workspace_mobile_device_lifecycle.ownership managed with
               | Workspace_mobile_device_lifecycle.Preexisting -> "preexisting"
               | Workspace_mobile_device_lifecycle.Owned _ -> "owned")])
       | None -> fail "boot this exact device or refresh its lifecycle inventory first")
  | Some booting ->
      let launcher_id = Workspace_mobile_device_lifecycle.launcher_identity booting in
      (match mobile_device_process_job_status context launcher_id with
       | Workspace_process.Completed (Workspace_process.Exited 0)
       | Workspace_process.Running -> ()
       | Workspace_process.Completed termination -> mobile_device_error termination);
      let timeout = optional_int "readiness_timeout_seconds" 60
          ~minimum:1 ~maximum:300 args in
      let deadline = Unix.gettimeofday () +. float timeout in
      let cancelled = match cancel with Some check -> check | None -> (fun () -> false) in
      let interrupted () = cancelled () || Unix.gettimeofday () >= deadline in
      let inventory = cache.device_inventory in
      let approval = mobile_device_approval inventory
          Workspace_mobile_device_lifecycle.Readiness target in
      let rec wait () =
        if cancelled () then raise Cancelled;
        if Unix.gettimeofday () >= deadline then None else
        let current, ready =
          match cache.device_platform with
          | Workspace_mobile_run.Android ->
              let current, devices_output, avd_name_output =
                mobile_device_android_live_inventory ~cancel:interrupted ?on_progress ~root cache target in
              mobile_device_replace_inventory context current;
              (match avd_name_output with
               | None -> current, Workspace_mobile_device_lifecycle.Waiting
               | Some avd_name_output ->
                   let boot_status_output = mobile_device_run ~cancel:interrupted ?on_progress
                       ~root ~timeout_seconds:5
                       (Workspace_mobile_device_lifecycle.readiness_command
                         ~booting ~approval) in
                   let ready = try Workspace_mobile_device_lifecycle.android_readiness
                       ~booting ~inventory:current.device_inventory ~approval
                       ~avd_name_output ~devices_output ~boot_status_output
                     with Workspace_mobile_device_lifecycle.Error message -> fail message in
                   current, ready)
          | Workspace_mobile_run.Ios ->
              let current, _, devices_json = mobile_device_current_ios ~cancel:interrupted ?on_progress
                  ~context ~root cache in
              mobile_device_replace_inventory context current;
              let ready = try Workspace_mobile_device_lifecycle.ios_readiness
                  ~booting ~inventory:current.device_inventory ~approval ~devices_json
                with Workspace_mobile_device_lifecycle.Error message -> fail message in
              current, ready in
        match ready with
        | Workspace_mobile_device_lifecycle.Ready managed ->
            mobile_device_store_ready context current target managed;
            Some managed
        | Workspace_mobile_device_lifecycle.Waiting ->
            if Unix.gettimeofday () >= deadline then None
            else (Thread.delay (min 0.5 (max 0. (deadline -. Unix.gettimeofday ()))); wait ()) in
      let result =
        try wait () with Cancelled ->
          if cancelled () then (
            (* Reap only this retained boot's launcher. Device identity stays
               pending until separately approved abort confirms it stopped. *)
            (try Workspace_process.kill_job context.process_manager ~id:launcher_id
             with Workspace_process.Error message ->
               fail ("cancelled readiness launcher could not be reaped: " ^ message));
            raise Cancelled)
          else if Unix.gettimeofday () >= deadline then None
          else raise Cancelled in
      match result with
      | None ->
          Yojson.Basic.to_string (`Assoc [
            "status", `String "waiting";
            "target_id", `String (Workspace_mobile_device_lifecycle.target_id target);
            "device_session_id", `String
              (Workspace_mobile_device_lifecycle.inventory_session_id inventory);
            "inventory_id", `String inventory.inventory_id;
            "readiness_timeout_seconds", `Int timeout;
            "next_action", `String "readiness or abort_boot"])
      | Some managed ->
          Yojson.Basic.to_string (`Assoc [
            "status", `String "ready";
            "target_id", `String (Workspace_mobile_device_lifecycle.target_id target);
            "ownership", `String (match Workspace_mobile_device_lifecycle.ownership managed with
              | Workspace_mobile_device_lifecycle.Preexisting -> "preexisting"
              | Workspace_mobile_device_lifecycle.Owned _ -> "owned");
            "device_session_id", `String
              (Workspace_mobile_device_lifecycle.inventory_session_id inventory);
            "inventory_id", `String inventory.inventory_id])

let mobile_device_android_stopped serial devices =
  not (List.exists (fun (device : Workspace_android_devices.device) ->
    device.serial = serial) devices)

let mobile_device_shutdown_tool ~approved ?cancel ?on_progress ~context ~root args =
  mobile_device_check_approved approved;
  let cache = mobile_device_execution_cache context ~root args in
  let target = mobile_device_target cache args in
  let managed = match mobile_device_lookup_target_state context.mobile_device_managed
      context cache target with
    | Some managed -> managed
    | None -> fail "take fresh readiness for this exact device before shutdown" in
  let cache = match cache.device_platform with
    | Workspace_mobile_run.Android ->
        let fresh, _ = mobile_device_current_android ?cancel ?on_progress ~root cache in
        fresh
    | Workspace_mobile_run.Ios ->
        let fresh, _, _ = mobile_device_current_ios ?cancel ?on_progress
            ~context ~root cache in
        fresh in
  mobile_device_replace_inventory context cache;
  let approval = mobile_device_approval cache.device_inventory
      Workspace_mobile_device_lifecycle.Shutdown target in
  let command = try Workspace_mobile_device_lifecycle.shutdown_command
      ~inventory:cache.device_inventory ~approval ~managed
    with Workspace_mobile_device_lifecycle.Error message -> fail message in
  match command with
  | None ->
      Yojson.Basic.to_string (`Assoc [
        "status", `String "preexisting_not_shutdown";
        "target_id", `String (Workspace_mobile_device_lifecycle.target_id target);
        "ownership", `String "preexisting";
        "device_state_changed", `Bool false])
  | Some command ->
      ignore (mobile_device_run ?cancel ?on_progress ~root command);
      let stopped =
        match cache.device_platform, Workspace_mobile_device_lifecycle.ownership managed with
        | Workspace_mobile_run.Android,
          Workspace_mobile_device_lifecycle.Owned { launcher_id; _ } ->
            (match mobile_device_wait_job ?cancel context launcher_id 45 with
             | Some (Workspace_process.Exited 0) ->
                 let output = mobile_device_run ?cancel ?on_progress ~root "adb devices" in
                 let devices = try Workspace_android_devices.adb_devices output
                   with Workspace_android_devices.Error message -> fail message in
                 let serial = Workspace_mobile_device_lifecycle.target_serial target in
                 mobile_device_android_stopped serial devices
             | Some _ | None -> false)
        | Workspace_mobile_run.Ios, _ ->
            let fresh, simulators, _ = mobile_device_current_ios ?cancel ?on_progress
                ~context ~root cache in
            mobile_device_replace_inventory context fresh;
            let id = match target with
              | Workspace_mobile_device_lifecycle.Ios_simulator { id } -> id
              | Workspace_mobile_device_lifecycle.Android_avd _ -> assert false in
            List.exists (fun (simulator : Workspace_xcode.simulator) ->
              simulator.id = id && simulator.state = "Shutdown") simulators
        | Workspace_mobile_run.Android, _ -> false in
      if not stopped then
        fail "shutdown has not been verified; device ownership remains active";
      ignore (Workspace_mobile_device_lifecycle.complete_shutdown managed `Succeeded);
      mobile_device_remove_target_state context.mobile_device_managed
        context cache target;
      Yojson.Basic.to_string (`Assoc [
        "status", `String "shutdown";
        "target_id", `String (Workspace_mobile_device_lifecycle.target_id target);
        "ownership", `String "owned";
        "device_state_changed", `Bool true])

let mobile_device_abort_tool ~approved ?cancel ?on_progress ~context ~root args =
  mobile_device_check_approved approved;
  let cache = mobile_device_execution_cache context ~root args in
  let target = mobile_device_target cache args in
  let booting = match mobile_device_lookup_target_state context.mobile_device_booting
      context cache target with
    | Some booting -> booting
    | None -> fail "no pending owned boot exists for this exact device" in
  let current = match cache.device_platform with
    | Workspace_mobile_run.Android ->
        let fresh, _, _ = mobile_device_android_live_inventory ?cancel ?on_progress
            ~root cache target in
        fresh
    | Workspace_mobile_run.Ios ->
        let fresh, _, _ = mobile_device_current_ios ?cancel ?on_progress
            ~context ~root cache in
        fresh in
  mobile_device_replace_inventory context current;
  let approval = mobile_device_approval current.device_inventory
      Workspace_mobile_device_lifecycle.Shutdown target in
  let command = try Workspace_mobile_device_lifecycle.abort_command
      ~inventory:current.device_inventory ~approval ~booting
    with Workspace_mobile_device_lifecycle.Error message -> fail message in
  (match command with
   | Some command -> ignore (mobile_device_run ?cancel ?on_progress ~root command)
   | None -> ());
  let job_id = Workspace_mobile_device_lifecycle.launcher_identity booting in
  (match mobile_device_process_job_status context job_id with
   | Workspace_process.Running ->
       (try Workspace_process.kill_job context.process_manager ~id:job_id
        with Workspace_process.Error message ->
          fail ("owned device launcher could not be reaped; abort ownership retained: " ^ message));
       (match mobile_device_wait_job ?cancel context job_id 10 with
        | Some _ -> ()
        | None -> fail "owned device launcher did not stop; abort ownership retained")
   | Workspace_process.Completed _ -> ());
  let stopped =
    match cache.device_platform with
    | Workspace_mobile_run.Android ->
        let output = mobile_device_run ?cancel ?on_progress ~root "adb devices" in
        let devices = try Workspace_android_devices.adb_devices output
          with Workspace_android_devices.Error message -> fail message in
        let serial = Workspace_mobile_device_lifecycle.target_serial target in
        mobile_device_android_stopped serial devices
    | Workspace_mobile_run.Ios ->
        let fresh, simulators, _ = mobile_device_current_ios ?cancel ?on_progress
            ~context ~root cache in
        mobile_device_replace_inventory context fresh;
        let id = match target with
          | Workspace_mobile_device_lifecycle.Ios_simulator { id } -> id
          | Workspace_mobile_device_lifecycle.Android_avd _ -> assert false in
        List.exists (fun (simulator : Workspace_xcode.simulator) ->
          simulator.id = id && simulator.state = "Shutdown") simulators in
  if not stopped then
    fail "abort has not verified the exact device is stopped; boot ownership retained";
  ignore (Workspace_mobile_device_lifecycle.complete_abort booting `Succeeded);
  mobile_device_remove_target_state context.mobile_device_booting context cache target;
  Yojson.Basic.to_string (`Assoc [
    "status", `String "aborted";
    "target_id", `String (Workspace_mobile_device_lifecycle.target_id target);
    "owned_process_job_id", `String job_id])

let mobile_device_lifecycle_tool ~approved ?cancel ?on_progress ~context ~root args =
  let context = require_session_context (Some context) in
  let root = Workspace_path.root_path root in
  match required_string "action" args with
  | "inventory" ->
      mobile_device_check_approved approved;
      mobile_device_inventory_tool ?cancel ?on_progress ~context ~root args
  | "status" -> mobile_device_status_tool ~context ~root args
  | "boot" ->
      mobile_device_boot_tool ~approved ?cancel ?on_progress ~context ~root args
  | "readiness" ->
      mobile_device_readiness_tool ~approved ?cancel ?on_progress ~context ~root args
  | "shutdown" ->
      mobile_device_shutdown_tool ~approved ?cancel ?on_progress ~context ~root args
  | "abort_boot" ->
      mobile_device_abort_tool ~approved ?cancel ?on_progress ~context ~root args
  | _ -> fail "mobile_device_lifecycle action must be inventory, status, boot, readiness, shutdown or abort_boot"
let mobile_device_lifecycle_preview ~context ~root args =
  let action = required_string "action" args in
  let command_details commands cwd =
    ["Working directory: " ^ Printf.sprintf "%S" cwd] @
    List.map (fun (label, command) -> label ^ ": " ^ command) commands in
  let android_commands subroot =
    let command action =
      android_device_command ~root (`Assoc [
        "action", `String action; "subroot", `String subroot]) in
    let avds, cwd = command "avds" in
    let devices, _ = command "devices" in
    cwd, avds, devices in
  if action = "inventory" then
    let platform = match required_string "platform" args with
      | "android" -> Workspace_mobile_run.Android
      | "ios" -> Workspace_mobile_run.Ios
      | _ -> fail "platform must be android or ios" in
    (match platform with
     | Workspace_mobile_run.Android ->
         let subroot = required_string "subroot" args in
         let cwd, avds, devices = android_commands subroot in
         ("Reads configured Android AVDs and ADB transport state for one explicit inventory; no device is booted.",
          ["Platform: Android";
           "Exact target scope: configured AVDs only; attached physical devices are not selectable."] @
          command_details ["AVD inventory command", avds; "ADB inventory command", devices] cwd @
          ["The boot action separately revalidates this inventory before launching one selected AVD.";
           "No SDK or system image is downloaded."])
     | Workspace_mobile_run.Ios ->
         let bundle = required_string "subroot" args in
         let scheme = required_string "scheme" args in
         ignore (mobile_device_xcode_discovery context ~root ~bundle ~scheme);
         let command = "xcrun simctl list devices available -j" in
         ("Reads compatible iOS Simulator state for a previously discovered exact Xcode bundle and scheme; no simulator is booted.",
          ["Platform: iOS Simulator";
           "Xcode bundle/scheme: " ^ bundle ^ " · " ^ scheme;
           "Exact command: " ^ command;
           "Physical devices, runtime downloads and erasure are unsupported."]))
  else if action = "status" then
    let cache = mobile_device_execution_cache context ~root args in
    let inventory = cache.device_inventory in
    ("Inspects cached lifecycle ownership for this private session; no device command is run.",
     ["Device session/inventory: " ^
        Workspace_mobile_device_lifecycle.inventory_session_id inventory ^ " · " ^
        Workspace_mobile_device_lifecycle.inventory_id inventory;
      "Cached records are not a fresh readiness or device-state observation."])
  else
    let cache = mobile_device_execution_cache context ~root args in
    let inventory = cache.device_inventory in
    let session_id = Workspace_mobile_device_lifecycle.inventory_session_id inventory in
    let target = mobile_device_target cache args in
    let target_id = Workspace_mobile_device_lifecycle.target_id target in
    let binding = [
      "Device session/inventory: " ^ session_id ^ " · " ^
        Workspace_mobile_device_lifecycle.inventory_id inventory;
      "Exact target: " ^ target_id;
      "Physical devices are never selectable; no runtime/image download or erase is performed."] in
    let android_live_commands () =
      let serial = Workspace_mobile_device_lifecycle.target_serial target in
      ["ADB transport recheck", "adb devices";
       "Exact selected AVD identity recheck (if ready)",
       Workspace_mobile_device_lifecycle.avd_name_command serial] in
    let platform_refresh action =
      match cache.device_platform with
      | Workspace_mobile_run.Android when action = "readiness" || action = "abort_boot" ->
          root, android_live_commands ()
      | Workspace_mobile_run.Android ->
          let _, avds, devices = android_commands cache.device_subroot in
          let bindings = List.filter_map
            (fun (device : Workspace_android_devices.device) ->
              if device.emulator && device.state = Workspace_android_devices.Ready then
                Some ("Current emulator identity recheck",
                  Workspace_mobile_device_lifecycle.avd_name_command device.serial)
              else None) inventory.android_devices in
          root, (["AVD inventory recheck", avds; "ADB transport recheck", devices] @ bindings)
      | Workspace_mobile_run.Ios ->
          root, ["Compatible simulator state recheck",
            "xcrun simctl list devices available -j"] in
    let approval action =
      mobile_device_approval inventory action target in
    let refresh_and action commands =
      let cwd, refresh = platform_refresh action in
      command_details (refresh @ commands) cwd in
    match action with
    | "boot" ->
        let plan = try Workspace_mobile_device_lifecycle.boot_command
            ~inventory ~approval:(approval Workspace_mobile_device_lifecycle.Boot)
            ~target ~ownership_id:"approval-preview"
          with Workspace_mobile_device_lifecycle.Error message -> fail message in
        let preview_details = match plan with
          | Workspace_mobile_device_lifecycle.Already_booted _ ->
              ["Observed in the approved inventory as pre-existing; no boot or shutdown command will run."]
          | Workspace_mobile_device_lifecycle.Start { command; _ } ->
              let launcher_cwd = match cache.device_platform with
                | Workspace_mobile_run.Android ->
                    let cwd, _, _ = android_commands cache.device_subroot in cwd
                | Workspace_mobile_run.Ios -> root in
              ["Exact boot command: " ^ command;
               "Owned launcher working directory: " ^ Printf.sprintf "%S" launcher_cwd;
               "Boot ownership is recorded only after this session starts the launcher."] in
        ("Boots only the exact configured AVD or compatible simulator after fresh inventory revalidation; pre-existing devices are never owned or shut down.",
         binding @ refresh_and "boot" [] @ preview_details)
    | "readiness" ->
        let booting = mobile_device_lookup_target_state context.mobile_device_booting
            context cache target in
        (match booting with
         | None ->
             ("Reports lifecycle state without issuing a device command.",
              binding @ ["No pending owned boot exists for this exact target."])
         | Some booting ->
             let command = Workspace_mobile_device_lifecycle.readiness_command
                 ~booting ~approval:(approval Workspace_mobile_device_lifecycle.Readiness) in
             let platform_commands = match cache.device_platform with
               | Workspace_mobile_run.Android ->
                   ["ADB transport recheck", "adb devices";
                    "Exact selected AVD identity recheck (when ready)",
                    Workspace_mobile_device_lifecycle.avd_name_command
                      (Workspace_mobile_device_lifecycle.target_serial target);
                    "Boot-complete readiness command", command]
               | Workspace_mobile_run.Ios -> ["Simulator readiness command", command] in
             ("Checks readiness only for this session-owned boot. Timeout preserves the boot; cancellation reaps its owned launcher and may stop its emulator.",
              binding @ command_details platform_commands root @
              [Printf.sprintf "Readiness deadline: %d seconds."
                (optional_int "readiness_timeout_seconds" 60 ~minimum:1 ~maximum:300 args)]))
    | "shutdown" ->
        let managed = match mobile_device_lookup_target_state context.mobile_device_managed
            context cache target with
          | Some managed -> managed
          | None -> fail "take fresh readiness for this exact device before shutdown" in
        let command = try Workspace_mobile_device_lifecycle.shutdown_command
            ~inventory ~approval:(approval Workspace_mobile_device_lifecycle.Shutdown)
            ~managed
          with Workspace_mobile_device_lifecycle.Error message -> fail message in
        let preview_details = match command with
          | None -> ["This target is pre-existing; shutdown will not run."]
          | Some command -> ["Exact shutdown command: " ^ command] in
        let verification = match cache.device_platform with
          | Workspace_mobile_run.Android -> ["Exact post-shutdown verification", "adb devices"]
          | Workspace_mobile_run.Ios -> ["Exact post-shutdown verification",
              "xcrun simctl list devices available -j"] in
        ("Shuts down only a device booted and owned by this session; pre-existing devices are preserved.",
         binding @ refresh_and "shutdown" verification @ preview_details @
         ["The exact target must be observed in stopped state before ownership is cleared."])
    | "abort_boot" ->
        let booting = match mobile_device_lookup_target_state context.mobile_device_booting
            context cache target with
          | Some booting -> booting
          | None -> fail "no pending owned boot exists for this exact device" in
        let command = try Workspace_mobile_device_lifecycle.abort_command
            ~inventory ~approval:(approval Workspace_mobile_device_lifecycle.Shutdown)
            ~booting
          with Workspace_mobile_device_lifecycle.Error message -> fail message in
        let preview_details = (match command with
          | Some command -> ["Exact target shutdown command: " ^ command]
          | None -> ["No target shutdown command is needed because this exact device is not ready."]) @
          ["Stops only the launcher process owned by this session; failed stop retains ownership."] in
        let verification = match cache.device_platform with
          | Workspace_mobile_run.Android -> ["Exact post-abort verification", "adb devices"]
          | Workspace_mobile_run.Ios -> ["Exact post-abort verification",
              "xcrun simctl list devices available -j"] in
        ("Cancels one pending owned boot, shutting down only its exact target and reaping only its owned launcher.",
         binding @ refresh_and "abort_boot" verification @ preview_details)
    | _ -> fail "unsupported mobile device lifecycle action"


let mobile_diagnostics_tool ~approved ?cancel ?on_progress ~context ~root args =
  if not approved then fail "mobile runtime diagnostics require explicit interactive approval";
  let root = Workspace_path.root_path root in
  check_session_context context;
  let id = required_string "session_id" args in
  let session = Workspace_mobile_run.get context.mobile_run_manager id in
  if session.root <> root then
    fail "mobile app session belongs to a different workspace root";
  if session.state = Workspace_mobile_run.Selected then
    fail "mobile runtime diagnostics require an app that has been built";
  let action = required_string "action" args in
  if action = "logs" && session.state <> Workspace_mobile_run.Running then
    fail "selected-app log capture requires a running app session";
  let command = Workspace_mobile_diagnostics.command action session in
  let result = Workspace_process.run_shell ?cancel ?on_progress
    ~timeout_seconds:(optional_int "timeout_seconds" 30 ~minimum:1 ~maximum:120 args)
    ~output_limit:Workspace_mobile_diagnostics.max_output_bytes
    ~cwd:(Some root) ~command () in
  (match result.termination with
   | Workspace_process.Exited 0 -> ()
   | Workspace_process.Exited code ->
       fail (Printf.sprintf "Mobile %s diagnostics failed (exit %d)%s\\n%s"
         action code (if result.truncated then ", output truncated" else "") result.output)
   | Workspace_process.Signaled signal ->
       fail (Printf.sprintf "Mobile %s diagnostics failed (signal %d)" action signal)
   | Workspace_process.Timed_out ->
       fail ("Mobile " ^ action ^ " diagnostics timed out")
   | Workspace_process.Cancelled -> raise Cancelled);
  let report = Workspace_mobile_diagnostics.result ~action session
    ~output:result.output ~truncated:result.truncated in
  [Protocol.Text report]

let mobile_visual_regions args =
  match field "dynamic_regions" args with
  | `List regions when List.length regions <= 1024 ->
      List.map (function
        | `Assoc fields ->
            let keys = List.map fst fields |> List.sort String.compare in
            if keys <> ["height"; "width"; "x"; "y"] then
              fail "each dynamic region must contain exactly x, y, width and height";
            let integer key =
              match List.assoc_opt key fields with
              | Some (`Int value) -> value
              | _ -> fail ("dynamic region " ^ key ^ " must be an integer") in
            { Workspace_mobile_visual.x = integer "x";
              y = integer "y"; width = integer "width";
              height = integer "height" }
        | _ -> fail "each dynamic region must be an object") regions
  | `List _ -> fail "too many dynamic regions"
  | _ -> fail "dynamic_regions must be an array"

let mobile_visual_request ~context ~root args =
  let id = required_string "session_id" args in
  let session = Workspace_mobile_run.get context.mobile_run_manager id in
  if session.root <> root then
    fail "mobile app session belongs to a different workspace root";
  if session.state <> Workspace_mobile_run.Running then
    fail "screenshot comparison requires a running app session";
  let action = required_string "action" args in
  if action <> "save" && action <> "compare" then
    fail "mobile visual action must be save or compare";
  let name = required_string "name" args in
  (try Workspace_mobile_visual.validate_key name with
   | Workspace_mobile_visual.Error message -> fail message);
  let os = required_string "os" args
  and locale = required_string "locale" args
  and theme = required_string "theme" args in
  let masks = mobile_visual_regions args in
  let command = Workspace_mobile_observe.command "screenshot" session in
  session, action, name, os, locale, theme, masks, command

let mobile_visual_compare_settings args =
  let bounded_integer name minimum maximum =
    match field name args with
    | `Int value when value >= minimum && value <= maximum -> value
    | `Null -> fail ("comparison requires explicit " ^ name)
    | _ -> fail (Printf.sprintf "%s must be between %d and %d"
        name minimum maximum) in
  bounded_integer "threshold" 0 255,
  bounded_integer "max_differing_pixels" 0 Workspace_mobile_visual.max_pixels

let mobile_visual_preview ~context ~root args =
  let session, action, name, os, locale, theme, masks, command =
    mobile_visual_request ~context ~root args in
  let identity = try Workspace_mobile_report.build_identity root session
    with Workspace_mobile_report.Error message -> fail message in
  let threshold, max_differing_pixels =
    if action = "compare" then mobile_visual_compare_settings args
    else 0, 0 in
  (if action = "save" then
     "Captures one screenshot and writes a private versioned pixel-comparison baseline."
   else "Captures one screenshot and returns baseline/current/difference PNG artifacts plus bounded changed regions."),
  ["Session/app/device: " ^ session.id ^ " · " ^ session.app_id ^
     " · " ^ session.device;
   "Selected build SHA-256: " ^ identity.build_hash;
   "Baseline: " ^ name ^ " · action: " ^ action;
   "Operator-declared OS/locale/theme: " ^ os ^ " · " ^ locale ^ " · " ^ theme;
   Printf.sprintf "Dynamic regions: %d" (List.length masks);
   (if action = "compare" then
      Printf.sprintf "Comparison settings v%d: per-channel threshold=%d; maximum differing pixels=%d."
        Workspace_mobile_visual.version threshold max_differing_pixels
    else "Baseline settings are versioned; no tolerance is applied during save.");
   "Working directory: " ^ Printf.sprintf "%S" session.root;
   "Exact command: " ^ command;
   Printf.sprintf "Maximum screenshot: %d bytes."
     Workspace_mobile_observe.max_screenshot_bytes]

let mobile_visual_tool ~approved ?cancel ?on_progress ~context ~root args =
  if not approved then
    fail "mobile screenshot comparison requires explicit interactive approval";
  let root = Workspace_path.root_path root in
  check_session_context context;
  let session, action, name, os, locale, theme, masks, command =
    mobile_visual_request ~context ~root args in
  let threshold, max_differing_pixels =
    if action = "compare" then mobile_visual_compare_settings args else 0, 0 in
  let build_identity = try Workspace_mobile_report.build_identity root session
    with Workspace_mobile_report.Error message -> fail message in
  let result = Workspace_process.run_shell ?cancel ?on_progress
    ~timeout_seconds:(optional_int "timeout_seconds" 30 ~minimum:1
      ~maximum:120 args)
    ~output_limit:Workspace_mobile_observe.max_screenshot_bytes
    ~cwd:(Some root) ~command () in
  (match result.termination with
   | Workspace_process.Exited 0 when not result.truncated -> ()
   | Workspace_process.Exited code ->
       fail (Printf.sprintf "mobile screenshot failed (exit %d)%s"
         code (if result.truncated then "; output truncated" else ""))
   | Workspace_process.Signaled signal ->
       fail (Printf.sprintf "mobile screenshot failed (signal %d)" signal)
   | Workspace_process.Timed_out -> fail "mobile screenshot timed out"
   | Workspace_process.Cancelled -> raise Cancelled);
  let screenshot = try Workspace_mobile_observe.validate_png result.output
    with Workspace_mobile_observe.Error message -> fail message in
  Workspace_mobile_run.set_screen_size context.mobile_run_manager
    ~id:session.id ~width:screenshot.width ~height:screenshot.height;
  let metadata = {
    Workspace_mobile_visual.app = session.app_id;
    platform = Workspace_mobile_run.platform_name session.platform;
    device = session.device; build_hash = build_identity.build_hash;
    os; locale; theme;
    width = screenshot.width; height = screenshot.height; masks
  } in
  let capture = {
    Workspace_mobile_visual.png = screenshot.png;
    complete = not result.truncated; metadata
  } in
  let output, images = try
    if action = "save" then (
      Workspace_mobile_visual.save ~workspace:root ~name capture;
      let text = Yojson.Basic.to_string (`Assoc [
        "session_id", `String session.id;
        "app_id", `String session.app_id;
        "device", `String session.device;
        "baseline", `String name;
        "status", `String "baseline_saved";
        "settings_version", `Int Workspace_mobile_visual.version;
        "environment_metadata", `String "operator_declared";
        "width", `Int screenshot.width; "height", `Int screenshot.height;
        "dynamic_regions", `Int (List.length masks)]) in
      text, [screenshot.png])
    else
      let report = Workspace_mobile_visual.compare ~workspace:root ~name
        ~threshold ~max_differing_pixels capture in
      let first = match report.comparison.first_difference with
        | None -> `Null
        | Some (x, y) -> `Assoc ["x", `Int x; "y", `Int y] in
      let regions = `List (List.map (fun rect -> `Assoc [
        "x", `Int rect.Workspace_mobile_visual.x;
        "y", `Int rect.y; "width", `Int rect.width;
        "height", `Int rect.height]) report.regions) in
      let text = Yojson.Basic.to_string (`Assoc [
        "session_id", `String session.id;
        "app_id", `String session.app_id;
        "device", `String session.device;
        "baseline", `String name;
        "status", `String (if report.comparison.equal then "within_tolerance" else "different");
        "differing_pixels", `Int report.comparison.differing_pixels;
        "first_difference", first; "regions", regions;
        "settings_version", `Int report.settings_version;
        "threshold", `Int report.threshold;
        "max_differing_pixels", `Int report.max_differing_pixels;
        "image_order", `List (List.map (fun name -> `String name)
          ["baseline"; "current"; "difference"]);
        "environment_metadata", `String "operator_declared";
        "width", `Int screenshot.width; "height", `Int screenshot.height;
        "dynamic_regions", `Int (List.length masks)]) in
      text, [report.baseline_png; report.current_png; report.difference_png]
  with Workspace_mobile_visual.Error message -> fail message in
  [Protocol.Text output] @ List.map (fun png ->
    Protocol.Image { mime_type = "image/png";
      data = Workspace_mobile_observe.base64_encode png }) images

let mobile_control_action action args =
  let integer name minimum maximum =
    match field name args with
    | `Int value when value >= minimum && value <= maximum -> value
    | _ -> fail (Printf.sprintf "mobile %s requires %s between %d and %d"
        action name minimum maximum) in
  match action with
  | "tap" ->
      Workspace_mobile_control.Tap {
        x = integer "x" 0 max_int; y = integer "y" 0 max_int }
  | "swipe" ->
      Workspace_mobile_control.Swipe {
        x1 = integer "x1" 0 max_int; y1 = integer "y1" 0 max_int;
        x2 = integer "x2" 0 max_int; y2 = integer "y2" 0 max_int;
        duration_ms = optional_int "duration_ms" 500 ~minimum:1 ~maximum:10_000 args }
  | "text" ->
      Workspace_mobile_control.Text (required_string "text" args)
  | "back" -> Workspace_mobile_control.Back
  | _ -> fail "mobile control action must be tap, swipe, text or back"

let mobile_control_tool ~approved ?cancel ?on_progress ~context ~root args =
  if not approved then fail "mobile UI control requires explicit interactive approval";
  let root = Workspace_path.root_path root in
  check_session_context context;
  let id = required_string "session_id" args in
  let session = Workspace_mobile_run.get context.mobile_run_manager id in
  if session.Workspace_mobile_run.root <> root then
    fail "mobile app session belongs to a different workspace root";
  let action = required_string "action" args in
  let command = Workspace_mobile_control.command session
    ~screen_size:session.screen_size (mobile_control_action action args) in
  Workspace_mobile_run.clear_screen_size context.mobile_run_manager ~id;
  let timeout_seconds = optional_int "timeout_seconds" 30
    ~minimum:1 ~maximum:120 args in
  let result = Workspace_process.run_shell ?cancel ?on_progress
    ~timeout_seconds ~output_limit:Workspace_mobile_observe.max_screenshot_bytes
    ~cwd:(Some root) ~command () in
  let outcome = match result.termination with
    | Workspace_process.Exited 0 when not result.truncated -> None
    | Workspace_process.Exited code -> Some (Printf.sprintf "exit %d" code)
    | Workspace_process.Signaled signal -> Some (Printf.sprintf "signal %d" signal)
    | Workspace_process.Timed_out -> Some "timed out"
    | Workspace_process.Cancelled -> raise Cancelled in
  (match outcome with
   | Some reason ->
       fail ("Mobile " ^ action ^ " failed: " ^ reason ^
         (if result.truncated then " (output truncated)" else "") ^
         "\n" ^ result.output)
   | None -> ());
  Printf.sprintf "Mobile %s completed for %s; capture a fresh screenshot and accessibility tree to verify the resulting UI state."
    action id

let mobile_scenario_tool ~approved ?cancel ?on_progress ~context ~root args =
  let root = Workspace_path.root_path root in
  check_session_context context;
  let action = required_string "action" args in
  let name = if action = "list" then optional_string "name" "" args
    else required_string "name" args in
  let needs_approval = not (List.mem action ["list"; "status"]) in
  if needs_approval && not approved then
    fail "mobile scenario mutation or replay requires explicit interactive approval";
  let selected_session () =
    let id = required_string "session_id" args in
    let session = Workspace_mobile_run.get context.mobile_run_manager id in
    if session.root <> root then fail "mobile app session belongs to a different workspace root";
    if session.state <> Workspace_mobile_run.Running then
      fail "mobile scenario operations require a running app session";
    session in
  let require_identity (record : Workspace_mobile_scenario.record) session =
    if not (Workspace_mobile_scenario.same_identity record.identity
        (Workspace_mobile_scenario.identity_of_session session)) then
      fail "mobile scenario app/device identity does not match the selected running session" in
  match action with
  | "list" ->
      let records = Workspace_mobile_scenario.list ~root in
      Yojson.Basic.to_string (`Assoc [
        "scenarios", `List (List.map (fun record ->
          `Assoc ["name", `String record.Workspace_mobile_scenario.name;
            "app_id", `String record.Workspace_mobile_scenario.identity.app_id;
            "device", `String record.Workspace_mobile_scenario.identity.device;
            "phase", `String (Workspace_mobile_scenario.phase_name record.Workspace_mobile_scenario.phase);
            "detail", `String (Workspace_mobile_scenario.phase_detail record.Workspace_mobile_scenario.phase)])
          records)])
  | "status" ->
      let record = Workspace_mobile_scenario.load ~root name in
      Workspace_mobile_scenario.render record
  | "save" ->
      let session = selected_session () in
      let steps = match field "steps" args with
        | `List steps -> List.map Workspace_mobile_scenario.step_of_json steps
        | _ -> fail "mobile scenario steps must be an array" in
      let record = Workspace_mobile_scenario.save ~root ~name ~session steps in
      Printf.sprintf "Saved mobile scenario %s for %s on %s (%d steps)."
        name record.Workspace_mobile_scenario.identity.app_id
          record.Workspace_mobile_scenario.identity.device
          (List.length record.Workspace_mobile_scenario.steps)
  | "start" ->
      let record = Workspace_mobile_scenario.load ~root name in
      let session = selected_session () in
      require_identity record session;
      Workspace_mobile_scenario.reset record;
      Workspace_mobile_scenario.update ~root record;
      "Started explicit replay of mobile scenario " ^ name ^ "; next: " ^
      Workspace_mobile_scenario.phase_detail record.Workspace_mobile_scenario.phase
  | "step" ->
      let record = Workspace_mobile_scenario.load ~root name in
      let session = selected_session () in
      require_identity record session;
      let step = Workspace_mobile_scenario.current_step record in
      let command = Workspace_mobile_control.command session
        ~screen_size:session.screen_size step.action in
      Workspace_mobile_run.clear_screen_size context.mobile_run_manager
        ~id:session.id;
      let result = try
        Workspace_process.run_shell ?cancel ?on_progress
          ~timeout_seconds:(optional_int "timeout_seconds" 30
            ~minimum:1 ~maximum:120 args)
          ~output_limit:Workspace_mobile_observe.max_screenshot_bytes
          ~cwd:(Some root) ~command ()
      with exn ->
        Workspace_mobile_scenario.mark_failed record "device action was cancelled or failed";
        Workspace_mobile_scenario.update ~root record;
        raise exn in
      let outcome = match result.termination with
        | Workspace_process.Exited 0 when not result.truncated -> None
        | Workspace_process.Exited code -> Some (Printf.sprintf "exit %d" code)
        | Workspace_process.Signaled signal -> Some (Printf.sprintf "signal %d" signal)
        | Workspace_process.Timed_out -> Some "timed out"
        | Workspace_process.Cancelled -> Some "cancelled" in
      (match outcome with
       | Some reason ->
           Workspace_mobile_scenario.mark_failed record ("device action " ^ reason);
           Workspace_mobile_scenario.update ~root record;
           fail ("Mobile scenario " ^ name ^ " stopped at " ^
             Workspace_mobile_scenario.phase_detail record.Workspace_mobile_scenario.phase ^ "\n" ^ result.output)
       | None -> ());
      Workspace_mobile_scenario.mark_awaiting record;
      Workspace_mobile_scenario.update ~root record;
      let assertion = List.nth record.Workspace_mobile_scenario.steps
        (match record.Workspace_mobile_scenario.phase with Workspace_mobile_scenario.Awaiting index -> index | _ -> assert false) in
      "Scenario step completed; call action=verify for a separately approved fresh accessibility observation. Expected " ^
      Workspace_mobile_scenario.assertion_field_name assertion.assertion_field ^
      "=" ^ assertion.expected
  | "verify" ->
      let record = Workspace_mobile_scenario.load ~root name in
      let session = selected_session () in
      require_identity record session;
      let index = match record.Workspace_mobile_scenario.phase with
        | Workspace_mobile_scenario.Awaiting index -> index
        | _ -> fail "mobile scenario has no step awaiting an accessibility assertion" in
      let step = List.nth record.Workspace_mobile_scenario.steps index in
      let command = Workspace_mobile_observe.command "accessibility" session in
      let result = try
        Workspace_process.run_shell ?cancel ?on_progress
          ~timeout_seconds:(optional_int "timeout_seconds" 30
            ~minimum:1 ~maximum:120 args)
          ~output_limit:Workspace_mobile_observe.max_accessibility_bytes
          ~cwd:(Some root) ~command ()
      with exn ->
        Workspace_mobile_scenario.mark_failed record "accessibility observation was cancelled or failed";
        Workspace_mobile_scenario.update ~root record;
        raise exn in
      let outcome = match result.termination with
        | Workspace_process.Exited 0 when not result.truncated -> None
        | Workspace_process.Exited code -> Some (Printf.sprintf "exit %d" code)
        | Workspace_process.Signaled signal -> Some (Printf.sprintf "signal %d" signal)
        | Workspace_process.Timed_out -> Some "timed out"
        | Workspace_process.Cancelled -> Some "cancelled" in
      (match outcome with
       | Some reason ->
           Workspace_mobile_scenario.mark_failed record ("accessibility observation " ^ reason);
           Workspace_mobile_scenario.update ~root record;
           fail ("Mobile scenario stopped before its first assertion: " ^
             Workspace_mobile_scenario.phase_detail record.Workspace_mobile_scenario.phase ^
             (if result.truncated then " (observation truncated)" else "") ^
             "\n" ^ result.output)
       | None -> ());
      let observed_tree = try
        Workspace_mobile_observe.parse_accessibility result.output
        |> Workspace_mobile_observe.accessibility_json
      with Workspace_mobile_observe.Error message ->
        Workspace_mobile_scenario.mark_failed record ("invalid accessibility observation: " ^ message);
        Workspace_mobile_scenario.update ~root record;
        fail ("Mobile scenario stopped before its first assertion: " ^ message) in
      let verified =
        try Workspace_mobile_scenario.verify_tree step observed_tree
        with Workspace_mobile_scenario.Error message ->
          Workspace_mobile_scenario.mark_failed record ("invalid observation: " ^ message);
          Workspace_mobile_scenario.update ~root record;
          fail ("Mobile scenario stopped before its first assertion: " ^ message) in
      if not verified then (
        Workspace_mobile_scenario.mark_failed record ("expected " ^
          Workspace_mobile_scenario.assertion_field_name step.assertion_field ^
          "=" ^ step.expected);
        Workspace_mobile_scenario.update ~root record;
        fail ("Mobile scenario stopped at first failed assertion: " ^
          Workspace_mobile_scenario.phase_detail record.Workspace_mobile_scenario.phase));
      Workspace_mobile_scenario.advance record;
      Workspace_mobile_scenario.update ~root record;
      "Verified scenario step " ^ string_of_int (index + 1) ^ "; assertion " ^
      Workspace_mobile_scenario.assertion_field_name step.assertion_field ^
      "=" ^ step.expected ^ "; " ^
      Workspace_mobile_scenario.phase_detail record.Workspace_mobile_scenario.phase
  | "delete" ->
      Workspace_mobile_scenario.delete ~root name;
      "Deleted mobile scenario " ^ name
  | _ -> fail "mobile scenario action must be list, save, status, start, step, verify or delete"

let mobile_scenario_preview ~context ~root args =
  let root = Workspace_path.root_path root in
  let action = required_string "action" args in
  let name = if action = "list" then optional_string "name" "" args
    else required_string "name" args in
  if action <> "list" then Workspace_mobile_scenario.check_name name;
  let path = Filename.concat root (".pave/mobile-scenarios/" ^ name ^ ".json") in
  let session () =
    let session = Workspace_mobile_run.get context.mobile_run_manager
      (required_string "session_id" args) in
    if session.root <> root then fail "mobile app session belongs to a different workspace root";
    if session.state <> Workspace_mobile_run.Running then
      fail "mobile scenario operations require a running app session";
    session in
  match action with
  | "list" ->
      ("Lists saved mobile scenarios from the caller-private workspace store.",
       ["Store: " ^ Printf.sprintf "%S" (Filename.dirname path)])
  | "status" ->
      let record = Workspace_mobile_scenario.load ~root name in
      ("Reads a saved mobile scenario and its persistent replay state.",
       ["Scenario: " ^ Workspace_mobile_scenario.render record;
        "Record: " ^ Printf.sprintf "%S" path])
  | "save" ->
      let selected = session () in
      let steps = match field "steps" args with
        | `List values -> List.map Workspace_mobile_scenario.step_of_json values
        | _ -> fail "mobile scenario steps must be an array" in
      List.iter (fun step -> ignore (Workspace_mobile_control.command selected
        ~screen_size:selected.screen_size step.Workspace_mobile_scenario.action)) steps;
      ("Persists an exact app/device-bound bug scenario and accessibility assertions.",
       ["Store: " ^ Printf.sprintf "%S" path;
        "App/device: " ^ selected.app_id ^ " · " ^ selected.device;
        Printf.sprintf "Steps: %d" (List.length steps);
        "Permissions: private scenario directory and 0600 JSON record"])
  | "start" ->
      let record = Workspace_mobile_scenario.load ~root name in
      let selected = session () in
      if not (Workspace_mobile_scenario.same_identity record.Workspace_mobile_scenario.identity
          (Workspace_mobile_scenario.identity_of_session selected)) then
        fail "mobile scenario app/device identity does not match the selected running session";
      ("Starts a new explicit replay; prior failed/complete progress is retained until this approval.",
       ["Scenario: " ^ name ^ " · app/device: " ^ selected.app_id ^ " · " ^ selected.device;
        "Record: " ^ Printf.sprintf "%S" path])
  | "step" ->
      let record = Workspace_mobile_scenario.load ~root name in
      let selected = session () in
      if not (Workspace_mobile_scenario.same_identity record.Workspace_mobile_scenario.identity
          (Workspace_mobile_scenario.identity_of_session selected)) then
        fail "mobile scenario app/device identity does not match the selected running session";
      let step = Workspace_mobile_scenario.current_step record in
      let command = Workspace_mobile_control.command selected
        ~screen_size:selected.screen_size step.action in
      ("Performs exactly the next stored UI action on its bound device; no implicit retry. The result awaits a separately approved accessibility observation and assertion.",
       ["Working directory: " ^ Printf.sprintf "%S" selected.root;
        "Scenario: " ^ name ^ " · " ^ Workspace_mobile_scenario.phase_detail record.Workspace_mobile_scenario.phase;
        "App/device: " ^ selected.app_id ^ " · " ^ selected.device;
        "Assertion: " ^ Workspace_mobile_scenario.assertion_field_name step.assertion_field ^
          "=" ^ step.expected;
        "Exact command: " ^ command])
  | "verify" ->
      let record = Workspace_mobile_scenario.load ~root name in
      let selected = session () in
      if not (Workspace_mobile_scenario.same_identity record.Workspace_mobile_scenario.identity
          (Workspace_mobile_scenario.identity_of_session selected)) then
        fail "mobile scenario app/device identity does not match the selected running session";
      let index = match record.Workspace_mobile_scenario.phase with
        | Workspace_mobile_scenario.Awaiting index -> index
        | _ -> fail "mobile scenario has no step awaiting an accessibility assertion" in
      let step = List.nth record.Workspace_mobile_scenario.steps index in
      let command = Workspace_mobile_observe.command "accessibility" selected in
      ("Captures a fresh accessibility tree from the bound app and persists the exact pending assertion result.",
       ["Scenario step: " ^ string_of_int (index + 1);
        "Expected: " ^ Workspace_mobile_scenario.assertion_field_name step.assertion_field ^
          "=" ^ step.expected;
        "App/device: " ^ selected.app_id ^ " · " ^ selected.device;
        "Exact command: " ^ command;
        Printf.sprintf "Maximum captured output: %d bytes."
          Workspace_mobile_observe.max_accessibility_bytes])
  | "delete" ->
      ("Deletes one caller-private saved mobile scenario.",
       ["Record: " ^ Printf.sprintf "%S" path])
  | _ -> fail "mobile scenario action must be list, save, status, start, step, verify or delete"

let mobile_session_preview ~context ~root args =
  match optional_string "action" "" args with
  | "build" ->
      let session = Workspace_mobile_run.get context.mobile_run_manager
        (required_string "session_id" args) in
      let stack, _, command, cwd, _ =
        mobile_session_build_request ~context ~root session args in
      ("Builds the selected " ^ session.app_id ^ " app for " ^
       Workspace_mobile_run.platform_name session.platform ^
       " using the chosen scheme/variant and approved task.",
       ["Working directory: " ^ Printf.sprintf "%S" cwd;
        "Selected app: " ^ session.app_id ^ " · artifact: " ^ session.app_path;
        "Build path: " ^ stack;
        "Exact command: " ^ command])
  | ("install" | "launch" | "stop") as action ->
      let session = Workspace_mobile_run.get context.mobile_run_manager
        (required_string "session_id" args) in
      let command = Workspace_mobile_run.command action session in
      (("Performs one " ^ action ^ " action for the selected " ^
        Workspace_mobile_run.platform_name session.platform ^ " app on " ^
        session.device ^ "; this command may change device state."),
       ["Working directory: " ^ Printf.sprintf "%S" session.root;
        "Exact command: " ^ command;
        "App: " ^ session.app_id ^ " · artifact: " ^ session.app_path])
  | "select" ->
      let subroot = required_string "subroot" args in
      let platform = required_string "platform" args in
      let device = required_string "device" args in
      let app_id = required_string "app_id" args in
      let app_path = required_string "app_path" args in
      let activity = optional_string "activity" "" args in
      let binding_details =
        if platform = "ios" then (
          let device_session_id = optional_string "device_session_id" "" args in
          let inventory_id = optional_string "inventory_id" "" args in
          match device_session_id, inventory_id with
          | "", "" ->
              ["Native XCTest accessibility is unavailable until both device_session_id and inventory_id bind this app session to an Owned Simulator lifecycle record."]
          | "", _ | _, "" ->
              fail "iOS lifecycle binding requires both device_session_id and inventory_id"
          | device_session_id, inventory_id ->
              let target_id = "ios:" ^ device in
              mobile_xctest_require_owned_lifecycle context ~root
                ~device_session_id ~inventory_id ~simulator_id:device ~target_id;
              ["Device lifecycle session: " ^ device_session_id;
               "Device inventory: " ^ inventory_id;
               "Exact target: " ^ target_id ^ " · ownership=Owned";
               "Native XCTest accessibility observations are scoped to this Simulator.";
               "iOS semantic control and scenario replay remain unavailable."]
        ) else if platform = "android" then [] else
          fail "mobile session platform must be android or ios" in
      ("Stores a mobile app/device selection in this private session; executes no project or device command.",
       ["Project: " ^ subroot; "Platform: " ^ platform;
        "Device: " ^ device; "App: " ^ app_id;
        "Activity: " ^ if activity = "" then "(launcher intent)" else activity;
        "Artifact: " ^ app_path] @ binding_details)
  | "list" | "status" ->
      ("Reads session-owned mobile app state; no device command is executed.", [])
  | _ -> fail "mobile session action must be list, select, status, build, install, launch or stop"

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

(* Called only for a prepared built-in request, never an external tool that
   happens to use the same name. Lookup and expected identity keep a replaced
   or unrelated browser session outside this cleanup. *)
let cleanup_denied_call ?context ~name ~args () =
  if name <> "browser" then Ok () else
  try
    match context with
    | None -> Ok ()
    | Some context ->
        if Workspace_browser.owner context.browser_manager <> context.owner then
          Error "Denied browser cleanup owner mismatch"
        else
          let id = match required_string "action" args with
            | "open" -> optional_string "id" "browser" args
            | _ -> required_string "id" args in
          let id = Workspace_browser.valid_token "browser session id" 128 id in
          let session = try Some (Workspace_browser.lookup context.browser_manager ~id)
            with Workspace_browser.Error _ -> None in
          Option.iter (fun expected ->
            Workspace_browser.close_session ~expected context.browser_manager ~id) session;
          Ok ()
  with _ -> Error "Denied browser cleanup failed"

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
  let publish_relay () = match optional_string "relay" "" args with
    | "" -> None | value -> Some (Workspace_portal.https_origin value) in
  let tunnel_id = Workspace_portal.job_id in
  let json = match action with
    | "publish" ->
        require_explicit_approval approved;
        let relay = publish_relay () in
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
        let relay = publish_relay () in
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

let lsp_start ~approved ?cancel ?context root args =
  require_explicit_approval approved;
  let context = require_session_context context in
  check_session_context context;
  let program = required_string "program" args in
  let arguments = string_list "arguments" args in
  try Workspace_lsp.start ?cancel context.lsp_manager ~owner:context.owner ~root
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

let lsp_execute ~approved ~sensitive_review ?cancel ?context root args =
  let context = require_session_context context in
  check_session_context context;
  let program = required_string "program" args in
  let arguments = string_list "arguments" args in
  let apply_requested = optional_string "action" "" args = "apply_preview" in
  if apply_requested then require_explicit_approval approved;
  if apply_requested && Sensitive_mutation.installed () then (
    let _, files = lsp_preview ~context ~root args in
    ignore (require_sensitive_review ~approved ~sensitive_review ~root
      (lsp_apply_proposals ~root files)));
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
  | "list_files" -> list_files ?cancel root arguments
  | "search" -> search ?cancel root arguments
  | "glob" -> glob ?cancel root arguments
  | "grep" -> grep ?cancel root arguments
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
  scanning ?cancel (fun () -> repository_security_scan ?cancel root args)

let is_shell_tool = function
  | "run_command" | "start_process" | "start_shell" | "xcode_preflight"
  | "mobile_check" | "android_devices" | "mobile_session" | "mobile_verify"
  | "mobile_observe" | "mobile_control" | "mobile_scenario"
  | "mobile_diagnostics" | "mobile_visual" | "mobile_accessibility_audit"
  | "mobile_performance" | "mobile_device_lifecycle" | "mobile_environment"
  | "mobile_app_lifecycle" | "mobile_dev_server" -> true
  | _ -> false



let requires_explicit_approval ~name ~args =
  match name with
  | "start_process" | "start_shell" | "process_stdin" | "process_close_stdin"
  | "process_kill" | "xcode_preflight" | "mobile_check" | "android_devices" | "mobile_verify"
  | "mobile_visual" | "mobile_observe" | "mobile_control" | "mobile_diagnostics"
  | "mobile_accessibility_audit" | "mobile_environment"
  | "mobile_performance" -> true
  | "mobile_device_lifecycle" ->
      List.mem (optional_string "action" "" args)
        ["inventory"; "boot"; "readiness"; "shutdown"; "abort_boot"]
  | "mobile_app_lifecycle" ->
      not (List.mem (optional_string "action" "" args)
        ["list_scenarios"; "show_scenario"])
  | "mobile_dev_server" ->
      List.mem (optional_string "action" "" args) ["start"; "stop"]
  | "worktree_create" | "worktree_commit" | "worktree_remove"
  | "web_search" | "web_fetch" | "image_ocr"
  | "clipboard_read" | "clipboard_write"
  | "lsp_start" | "workspace_eval"
  | "ssh_open" | "ssh_read" | "ssh_write" | "ssh_command" | "dap_start" -> true
  | "mobile_session" ->
      List.mem (optional_string "action" "" args)
        ["build"; "install"; "launch"; "stop"]
  | "mobile_scenario" ->
      not (List.mem (optional_string "action" "" args) ["list"; "status"])
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
  | "mobile_check" | "android_devices" | "mobile_verify"
  | "worktree_create" | "worktree_commit" | "worktree_remove"
  | "lsp_start" | "dap_start" | "ssh_open" | "ssh_read"
  | "ssh_write" | "ssh_command" | "web_search" | "web_fetch"
  | "clipboard_write" -> true
  | "mobile_session" ->
      List.mem (optional_string "action" "" args)
        ["build"; "install"; "launch"; "stop"]
  | "mobile_control" -> true
  | "mobile_environment" ->
      List.mem (optional_string "action" "" args) ["apply"; "restore"]
  | "mobile_scenario" ->
      not (List.mem (optional_string "action" "" args) ["list"; "status"])
  | "mobile_dev_server" ->
      List.mem (optional_string "action" "" args) ["start"; "stop"]
  | "mobile_visual" ->
      optional_string "action" "" args = "save"
  | "mobile_app_lifecycle" ->
      List.mem (optional_string "action" "" args) ["open_link"; "transition"]
  | "mobile_performance" ->
      List.mem (optional_string "action" "" args) ["launch"; "ios_capture"]
  | "mobile_device_lifecycle" ->
      List.mem (optional_string "action" "" args)
        ["boot"; "shutdown"; "abort_boot"]
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
  schema "ssh_write" "Write a bounded workspace-relative file to a pinned SSH host using SFTP; transfer failure can leave a partial remote file. Requires explicit approval of the exact contents."
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
  schema "dap" "Control one private DAP session. Launch/attach, breakpoints, execution, evaluation, and debuggee termination require effect-specific explicit approval; inspection is read-only. Launch/attach may return startup_pending=true while awaiting separately approved breakpoint/configuration_done requests; configuration_done confirms both configuration and the deferred startup response."
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
  schema "browser" "Drive one session-owned isolated headless browser page over the Chrome DevTools Protocol. open launches a pinned Chromium executable with a fresh throwaway profile and loopback-only debugging; navigate, evaluate, screenshot and call_tool require separate explicit approval; observe, list_tools, tool_events (without clear) and close are read-only; clearing tool_events requires approval; close releases the session. list_tools reads the page-declared modelContext catalog across all frames (name or frame filters; schemas only for exact-name reads); tool_events returns catalog transitions since a cursor; call_tool invokes one page-declared tool in its owning frame and returns a bounded result or a structured error. Page content and page-declared tools are untrusted."
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
     "derived_data_path", string_field "Optional workspace-relative output directory for Xcode build artifacts";
     "timeout_seconds", integer_field "Per-command deadline (default 120 seconds)" 1 300]
    ["action"; "subroot"];
  schema "mobile_check" "Run focused SwiftPM tests, offline system-Gradle tasks, Gradle instrumentation tests bound to one inventoried ready emulator, Flutter analysis/tests or discovered device integration_test bound to an exact installed Android app session, or local React Native/Expo scripts. Discovery, device inventory and every execution need separate approvals; project code runs as your user. Flutter integration tests may deploy/install a test runner and app but never run pub get, install dependencies/SDKs, or boot a device."
    ["stack", enum_string_field "Selected mobile stack" ["swiftpm"; "gradle"; "flutter"; "node"];
     "action", enum_string_field "Focused check action" ["discover"; "tasks"; "run"; "instrumented"; "analyze"; "test"; "integration_test"; "lint"];
     "subroot", string_field "Exact workspace-relative package/settings/project root";
     "target", string_field "Exact discovered Swift/Gradle target, Flutter test/*.dart or integration_test/*.dart target";
     "serial", string_field "Gradle instrumented only: exact ready emulator serial reported by the approved android_devices inventory";
     "session_id", string_field "Flutter integration_test only: exact installed Android app session";
     "manager", enum_string_field "Node script runner when multiple lockfiles exist" ["npm"; "pnpm"; "yarn"];
     "timeout_seconds", integer_field "Per-command deadline (default 120 seconds)" 1 300]
    ["stack"; "action"; "subroot"];
  schema "mobile_dev_server" "Start, inspect or stop one private-session-owned React Native/Expo development server using an already declared package.json script. Start/stop require exact approval, readiness and owned-process cleanup; LAN exposure must be selected explicitly. No npx, package installation or prebuild."
    ["action", enum_string_field "Owned development server operation" ["start"; "status"; "stop"];
     "id", bounded_string_field "Unique process-session ID" 100;
     "subroot", bounded_string_field "Exact workspace-relative RN/Expo package root" 4096;
     "script", bounded_string_field "Exact declared package.json development script" 128;
     "manager", enum_string_field "Package manager when multiple lockfiles exist" ["npm"; "pnpm"; "yarn"];
     "host", enum_string_field "Required bind exposure" ["localhost"; "lan"];
     "port", integer_field "Selected listen port" 1 65535;
     "readiness_timeout_seconds", integer_field "Listen readiness deadline (default 45 seconds)" 1 300]
    ["action"; "id"];
  schema "mobile_environment" "Preview, apply or explicitly restore one bounded reversible Android-emulator app locale, global theme or orientation effect. Preview binds the exact running app, device and build; apply and restore each require separate approval and restore only when current state still equals the approved target. Theme/orientation changes are emulator-wide. Permission transitions and airplane-mode controls remain unavailable until their design/capability contracts are reviewed."
    ["action", enum_string_field "Environment operation" ["preview"; "apply"; "restore"];
     "session_id", string_field "Exact running Android app session";
     "effect", enum_string_field "Preview-only supported effect kind" ["locale"; "theme"; "orientation"];
     "locale", bounded_string_field "App locale tag or empty default" 64;
     "theme", enum_string_field "Global emulator theme target" ["light"; "dark"];
     "orientation", enum_string_field "Emulator orientation target" ["portrait"; "landscape"];
     "plan_id", bounded_string_field "Opaque plan ID returned by preview" 32]
    ["action"; "session_id"];
  schema "mobile_app_lifecycle" "Inspect exact selected Android APK URL-handler evidence, open only an exactly matched URL, and verify one exact accessible destination text/description between fresh selected-app observations. open_link requires destination_assertion and exact foreground handler identity. Scenarios bind build, app, device, generation and PID; process recreation requires a changed verified PID. No app-data clear, iOS URL dispatch or device boot."
    ["action", enum_string_field "Lifecycle operation" [
       "inspect_handlers"; "open_link"; "observe"; "create_scenario";
       "transition"; "list_scenarios"; "show_scenario"; "delete_scenario"];
     "session_id", string_field "Exact built/installed Android app session";
     "name", bounded_string_field "Private scenario name [a-z0-9_-]{1,48}" 48;
     "handler_id", bounded_string_field "handler_id from exact APK inspection" 16;
     "url", bounded_string_field "Exact URL matching inspected scheme, host and path" 2048;
     "destination_assertion", bounded_string_field "Required for open_link: exact accessibility text or content description" 512;
     "transition", enum_string_field "Explicit lifecycle effect" [
       "background"; "resume"; "recreate_process"]]
    ["action"];
  schema "mobile_performance" "Capture separately approved Android launch, warm frame/PSS samples with exact build and PID provenance, or an explicitly warm raw iOS Simulator Instruments trace. Android launch condition is verified by force-stop or pre-existing PID continuity; frames/PSS require immediate exact-PID revalidation and complete known-unit samples. iOS xctrace remains raw and does not provide measured counters or energy."
    ["action", enum_string_field "Selected app measurement" [
       "launch"; "frames"; "memory"; "ios_templates"; "ios_process"; "ios_capture"];
     "session_id", string_field "Exact running selected app session";
     "condition", enum_string_field "Required for measured launch/frame/memory and iOS trace operations" ["cold"; "warm"];
     "template", enum_string_field "Installed xctrace template" [
       "Time Profiler"; "Allocations"; "Leaks"; "Activity Monitor"];
     "name", bounded_string_field "Unique private iOS trace name [A-Za-z0-9_-]{1,80}" 80]
    ["action"; "session_id"];
  schema "android_devices" "Inventory configured Android AVDs or attached ADB devices with one separately approved command per phase; never boot, install, select a physical serial or run a test."
    ["action", enum_string_field "AVD configuration or ADB transport listing" ["avds"; "devices"];
     "subroot", string_field "Exact workspace-relative Gradle settings directory"]
    ["action"; "subroot"];
  schema "mobile_device_lifecycle" "Inventory one selected session's existing configured AVDs or compatible iOS simulators, then separately approve owned boot, readiness, shutdown or boot cancellation. Status inspects only cached ownership using device_session_id and inventory_id, with no target or device command. Readiness uses one deadline for all probes; timeout preserves the boot, while cancellation reaps only its owned launcher. Every effect binds the exact device session, inventory and target. Physical devices, runtime/image downloads and erasure are unsupported; pre-existing devices are never shut down."
    ["action", enum_string_field "Device lifecycle operation" [
       "inventory"; "status"; "boot"; "readiness"; "shutdown"; "abort_boot"];
     "platform", enum_string_field "Selected target platform" ["android"; "ios"];
     "subroot", bounded_string_field "Exact workspace-relative Gradle settings directory or Xcode bundle" 4096;
     "scheme", bounded_string_field "Exact Xcode scheme already discovered for this bundle" 256;
     "device_session_id", bounded_string_field "Exact returned device session ID" 64;
     "inventory_id", bounded_string_field "Exact returned device inventory ID" 64;
     "target_name", bounded_string_field "Exact configured Android AVD name from this inventory; required for Android device effects" 128;
     "port", integer_field "Exact Android emulator console port (even, 5554..5682); required for Android device effects" 5554 5682;
     "simulator_id", bounded_string_field "Exact compatible iOS Simulator UUID" 36;
     "readiness_timeout_seconds", integer_field "Complete readiness probe deadline; timeout preserves the owned boot (default 60 seconds)" 1 300]
    ["action"];
  schema "mobile_session" "Select an app and optionally bind an iOS accessibility session to the exact Owned Simulator lifecycle record, then separately build, install, launch or stop. Unbound iOS sessions retain existing behavior but Native XCTest accessibility remains unavailable. Every device/build effect has exact interactive approval; no physical device is selectable."
    ["action", enum_string_field "Session action" ["list"; "select"; "status"; "build"; "install"; "launch"; "stop"];
     "session_id", string_field "Session ID returned by select";
     "platform", enum_string_field "Verified app platform" ["ios"; "android"];
     "subroot", string_field "Exact workspace-relative project root; Xcode bundle path for iOS";
     "device", string_field "Exact iOS Simulator UUID or ready emulator serial from approved inventory";
     "device_session_id", bounded_string_field "Optional exact Owned iOS Simulator lifecycle session ID; supply with inventory_id to enable Native XCTest accessibility" 64;
     "inventory_id", bounded_string_field "Optional exact approved iOS device inventory ID; supply with device_session_id to enable Native XCTest accessibility" 64;
     "app_id", string_field "Exact bundle identifier or Android application ID";
     "app_path", string_field "Workspace-relative expected .app directory or .apk build artifact";
     "scheme", string_field "Exact approved Xcode scheme for iOS";
     "variant", string_field "Selected Android build variant for session";
     "activity", string_field "Optional exact Android component package/activity, for example com.example.app/.MainActivity";
     "task", string_field "Exact discovered Gradle assemble task for the selected variant";
     "timeout_seconds", integer_field "Per-command deadline (default 120 seconds)" 1 300]
    ["action"];
  schema "mobile_verify" "Verify an edited workspace source snapshot against one focused build or nonempty test result for the exact selected mobile app session. Source hashes are checked before and after execution; each command requires explicit approval."
    ["action", enum_string_field "Focused verification command" ["build"; "test"];
     "session_id", string_field "Exact selected mobile app session";
     "source_path", bounded_string_field "Workspace-relative edited source path" 4096;
     "before_sha256", bounded_string_field "Pre-edit source SHA-256" 64;
     "after_sha256", bounded_string_field "Post-edit snapshot SHA-256" 64;
     "task", bounded_string_field "Exact Android assemble task; required for Android build" 512;
     "target", bounded_string_field "Android discovered instrumentation task; required for Android test" 512;
     "timeout_seconds", integer_field "Per-command deadline (default 120 seconds)" 1 300]
    ["action"; "session_id"; "source_path"; "before_sha256"; "after_sha256"];
  schema "mobile_observe" "Capture a bounded screenshot or fresh accessibility tree from a running selected app. Every read is separately approved; iOS accessibility uses a temporary native XCTest runner bound to the selected app, exact Simulator and an Owned lifecycle record. Output is bounded and untrusted."
    ["action", enum_string_field "Read one selected-screen representation" ["screenshot"; "accessibility"];
     "session_id", string_field "Running mobile app session ID";
     "timeout_seconds", integer_field "Device/XCTest read deadline (default 30 seconds Android, 120 seconds iOS)" 1 120]
    ["action"; "session_id"];
  schema "mobile_accessibility_audit" "Capture a fresh bounded Android accessibility tree from the exact running selected app session and report only rule-based findings supported by observed nodes. This does not certify screen-reader, contrast, focus-order or unknown-density touch-target behavior; each private tree capture requires explicit approval."
    ["session_id", string_field "Exact running Android app session";
     "timeout_seconds", integer_field "Device read deadline (default 30 seconds)" 1 120]
    ["session_id"];
  schema "mobile_diagnostics" "Read bounded runtime logs, Android crash-buffer or last-ANR evidence, and selected iOS Simulator process/crash logs. Every capture requires exact explicit approval. Reports preserve truncation and identify local mapping/dSYM artifacts without claiming automatic symbolication."
    ["action", enum_string_field "Diagnostic capture" ["logs"; "crashes"; "anr"];
     "session_id", string_field "Selected built mobile app session ID";
     "timeout_seconds", integer_field "Capture deadline (default 30 seconds)" 1 120]
    ["action"; "session_id"];
  schema "mobile_visual" "Save a bounded screenshot as a private version-2 pixel baseline or compare it with a fresh capture. Metadata binds exact app/platform/device, operator-declared OS/locale/theme and dynamic-region masks. Compare requires explicit per-channel threshold and differing-pixel tolerance, returns baseline/current/difference artifacts plus at most 128 regions, and rejects incomplete images or metadata mismatches."
    ["action", enum_string_field "Save or compare a screenshot baseline" ["save"; "compare"];
     "session_id", string_field "Exact running mobile app session";
     "name", bounded_string_field "Baseline name [A-Za-z0-9_-]{1,80}" 80;
     "os", bounded_string_field "Operator-declared OS/runtime version" 512;
     "locale", bounded_string_field "Operator-declared device locale" 512;
     "theme", bounded_string_field "Operator-declared light/dark or theme identifier" 512;
     "dynamic_regions", `Assoc ["type", `String "array";
       "maxItems", `Int 1024;
       "description", `String "Exact screenshot rectangles ignored during pixel comparison";
       "items", object_field [
         "x", integer_field "Left pixel coordinate" 0 max_int;
         "y", integer_field "Top pixel coordinate" 0 max_int;
         "width", integer_field "Region width" 1 max_int;
         "height", integer_field "Region height" 1 max_int]
         ["x"; "y"; "width"; "height"]];
     "threshold", integer_field "Compare only: explicit maximum channel delta (0..255)" 0 255;
     "max_differing_pixels", integer_field "Compare only: explicit allowed differing-pixel count" 0 Workspace_mobile_visual.max_pixels;
     "timeout_seconds", integer_field "Screenshot deadline (default 30 seconds)" 1 120]
    ["action"; "session_id"; "name"; "os"; "locale"; "theme"; "dynamic_regions"];


  schema "mobile_control" "Perform one explicit Android coordinate/back action on the selected running app. Coordinates are bounded by a recent screenshot, and each action requires separate approval."
    ["action", enum_string_field "One UI action" ["tap"; "swipe"; "text"; "back"];
     "session_id", string_field "Running mobile app session ID";
     "x", integer_field "Tap x coordinate in the most recent screenshot" 0 max_int;
     "y", integer_field "Tap y coordinate in the most recent screenshot" 0 max_int;
     "x1", integer_field "Swipe starting x coordinate" 0 max_int;
     "y1", integer_field "Swipe starting y coordinate" 0 max_int;
     "x2", integer_field "Swipe ending x coordinate" 0 max_int;
     "y2", integer_field "Swipe ending y coordinate" 0 max_int;
     "duration_ms", integer_field "Swipe duration (default 500 milliseconds)" 1 10_000;
     "text", bounded_string_field "Text to enter (maximum 512 bytes)" 512;
     "timeout_seconds", integer_field "Device action deadline (default 30 seconds)" 1 120]
    ["action"; "session_id"];

  schema "mobile_scenario" "Save and explicitly replay Android bug scenarios with fresh accessibility assertions. Version-2 records bind exact project, build bytes, app and device; version-1 records require re-saving. Every action, observation and state transition is separately approved, and failures stop without implicit retries."
    ["action", enum_string_field "Scenario operation" ["list"; "save"; "status"; "start"; "step"; "verify"; "delete"];
     "name", bounded_string_field "Scenario identifier [a-z][a-z0-9_-]{0,47}" 48;
     "session_id", string_field "Exact running app session ID";
     "steps", `Assoc ["type", `String "array";
       "maxItems", `Int Workspace_mobile_scenario.max_steps;
       "description", `String "Ordered Android UI actions and expected accessibility values";
       "items", object_field [
         "action", enum_string_field "Tap, swipe, text or Back" ["tap"; "swipe"; "text"; "back"];
         "x", integer_field "Tap x coordinate" 0 max_int;
         "y", integer_field "Tap y coordinate" 0 max_int;
         "x1", integer_field "Swipe start x" 0 max_int;
         "y1", integer_field "Swipe start y" 0 max_int;
         "x2", integer_field "Swipe end x" 0 max_int;
         "y2", integer_field "Swipe end y" 0 max_int;
         "duration_ms", integer_field "Swipe duration (default 500 milliseconds)" 1 10_000;
         "text", bounded_string_field "Text input (maximum 512 bytes)" 512;
         "expected_field", enum_string_field "Exact accessibility field to assert" ["role"; "text"; "description"; "identifier"];
         "expected_value", bounded_string_field "Expected exact value (maximum 1024 bytes)" 1024]
         ["action"; "expected_field"; "expected_value"]];
     "timeout_seconds", integer_field "Per-step device deadline (default 30 seconds)" 1 120]
    ["action"];


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
  schema "web_search" "Search the public web. Uses the configured provider order, or automatically Exa, Firecrawl, Brave, Tavily, Kagi and Jina with keys, then credential-free DuckDuckGo, then Ecosia when a local Chromium-family browser is detected. Falls back to the next approved provider on failure or an empty answer. Requires network approval; returns source URLs, citations, provider provenance and earlier provider failures."
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
  schema "publish_web" "Publish an already-running localhost web server through gosuda/portal-tunnel relays. publish starts a session-owned portal expose process; name is an optional hostname prefix (random when omitted). If Portal is unavailable, the official CLI is downloaded, SHA-256-verified, and installed privately after publication approval. stop ends one tunnel; list inspects owned tunnels; attach publishes the session hub. No other tunnel service is used. The local server and tunnel must remain running; the hostname is publicly relay-listed."
    ["action", enum_string_field "Tunnel operation" ["publish"; "stop"; "list"; "attach"];
     "port", integer_field "Loopback port to publish (required for publish)" 1 65535;
     "name", bounded_string_field "Lowercase DNS hostname prefix (1–22 bytes, alphanumeric ends); randomly generated when omitted; required for stop" Workspace_portal.max_name_length;
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
  | "mobile_session" when
      List.mem (optional_string "action" "" args) ["list"; "status"] ->
      tier Approval.Read
  | "mobile_observe" -> tier Approval.Read
  | "mobile_accessibility_audit" -> tier Approval.Read
  | "mobile_diagnostics" -> tier Approval.Read
  | "mobile_environment" when optional_string "action" "" args = "preview" ->
      tier Approval.Read
  | "mobile_app_lifecycle" ->
      (match optional_string "action" "" args with
       | "create_scenario" | "delete_scenario" -> tier Approval.Write
       | "open_link" | "transition" -> tier Approval.Exec
       | _ -> tier Approval.Read)
  | "mobile_performance" ->
      (match optional_string "action" "" args with
       | "launch" | "ios_capture" -> tier Approval.Exec
       | _ -> tier Approval.Read)
  | "mobile_device_lifecycle" ->
      (match optional_string "action" "" args with
       | "boot" | "shutdown" | "abort_boot" -> tier Approval.Exec
       | _ -> tier Approval.Read)
  | "mobile_dev_server" when optional_string "action" "" args = "status" ->
      tier Approval.Read
  | "mobile_dev_server" -> tier Approval.Exec
  | "mobile_visual" when optional_string "action" "" args = "save" ->
      tier Approval.Write
  | "mobile_visual" -> tier Approval.Read
  | "mobile_scenario" ->
      (match optional_string "action" "" args with
       | "list" | "status" -> tier Approval.Read
       | "step" -> tier Approval.Exec
       | _ -> tier Approval.Write)
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

let apply_edits ~approved ~sensitive_review ?cancel ?context root args =
  let root, args = resolve_path_arguments ?cancel ?context ~root args in
  let path = required_string "path" args in
  let expected_sha256 = required_string "expected_sha256" args in
  let hunks = parse_hunks args in
  let prepared =
    try Workspace_edit.prepare_hunks ~root ~path ~expected_sha256 ~hunks
    with Workspace_edit.Error message -> fail message in
  ignore (require_sensitive_review ~approved ~sensitive_review ~root
    [proposed_of_prepared prepared]);
  let preview = prepared.preview in
  (try Workspace_edit.write_prepared prepared
   with Workspace_edit.Error message -> fail message);
  Option.iter (fun context ->
    if preview.changed then (
      let evidence_key = guarded_evidence_key ~root ~path in
      Mutex.lock context.mobile_lock;
      Fun.protect ~finally:(fun () ->
        Mutex.unlock context.mobile_lock) (fun () ->
          let before_sha256 = match Hashtbl.find_opt
              context.guarded_edit_evidence evidence_key with
            | Some previous when previous.after_sha256 = prepared.before.sha256 ->
                previous.before_sha256
            | _ -> prepared.before.sha256 in
          if not (Hashtbl.mem context.guarded_edit_evidence evidence_key) &&
             Hashtbl.length context.guarded_edit_evidence >=
               max_guarded_edit_evidence then
            Hashtbl.clear context.guarded_edit_evidence;
          Hashtbl.replace context.guarded_edit_evidence evidence_key
            { before_sha256; after_sha256 = preview.result_sha256 })))
    context;
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

let ast_edit ~approved ~sensitive_review ?cancel ?context root args =
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
  else (
    let prepared =
      try Workspace_edit.prepare_ast ~root ~language edit
      with Workspace_edit.Error message -> fail message in
    ignore (require_sensitive_review ~approved ~sensitive_review ~root
      [proposed_of_prepared prepared]);
    (try Workspace_edit.write_prepared prepared
     with Workspace_edit.Error message -> fail message);
    let preview = prepared.Workspace_edit.preview in
    Printf.sprintf "Applied AST edit to %s; SHA-256: %s; changed: %b"
      path preview.result_sha256 preview.changed)


let approval_request ?cancel ?context ?(env = Sys.getenv_opt)
    ~root ~name ~args (decision : Approval.decision) =
  let base_root = root in
  let preview_root, preview_args =
    if List.mem name ["workspace_snapshot"; "write_file"; "edit_file";
        "apply_edits"; "ast_edit"] then
      resolve_path_arguments ?cancel ?context ~root args
    else root, args in
  let value name fallback args = match Protocol.member name args with
    | `String text -> text
    | `Int number -> string_of_int number
    | _ -> fallback in
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
              "Process: portal expose (PAVE_PORTAL or PATH); private identity outside the workspace"] @
             Option.to_list (Workspace_portal.setup_description ~env ())
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
        let context = require_session_context context in
        let stack = required_string "stack" args
        and action = required_string "action" args
        and subroot = required_string "subroot" args in
        let ready_device_id = flutter_integration_device ~context ~root:base_root args in
        let flutter_discovery = stack = "flutter" && action = "discover" in
        let discovery = if flutter_discovery then
            Some (try Workspace_flutter_focus.discover_integration_tests
              ~root:base_root ~subroot
             with Workspace_flutter_focus.Error message -> fail message)
          else None in
        let command, cwd = if flutter_discovery then
            ("bounded filesystem discovery only; no subprocess is started",
             if subroot = "" || subroot = "." then base_root
             else Workspace_path.checked_path base_root subroot)
          else mobile_command ?ready_device_id ~root:base_root args in
        let integration = match ready_device_id with
          | None ->
              (match discovery with
               | Some discovery ->
                   ["Discovered integration tests:\n" ^
                    String.concat "\n" (List.map
                      (fun (target : Workspace_flutter_focus.integration_target) ->
                        target.path) discovery.targets)]
               | None -> [])
          | Some serial ->
              let session = Workspace_mobile_run.get context.mobile_run_manager
                (required_string "session_id" args) in
              mobile_discovery_require context ~stack ~root:base_root ~subroot
                ~manifest_hash:(mobile_manifest ~root:base_root ~stack ~subroot)
                ~target:(required_string "target" args);
              ["Selected app session: " ^ session.id ^ " · " ^ session.app_id;
               "Exact installed emulator: " ^ serial;
               "Flutter integration_test runs with --no-pub and may compile/deploy or install the test runner and app on this emulator.";
               "No pub get, dependency install, SDK/image install or device boot is performed."] in
        ("Executes selected mobile project code as your user; discovery and execution each need approval. Commands do not provision SDKs or sandbox project code.",
         ["Working directory: " ^ Printf.sprintf "%S" cwd;
          "Exact command: " ^ command] @ integration @
         [if stack = "gradle" && action = "instrumented" then
            "Runs the selected Gradle task with ANDROID_SERIAL bound to one emulator; the task may install/run test APKs. Boot and standalone install remain separately approved."
          else
            "The selected toolchain may write local build artifacts or invoke project-defined code."])
    | "android_devices" ->
        let command, cwd = android_device_command ~root:base_root args in
        "Lists Android devices as your user; this inventory is not authorization to boot, install, launch or test. Each phase requires separate interactive approval.",
        ["Working directory: " ^ Printf.sprintf "%S" cwd;
         "Exact command: " ^ command;
         (if optional_string "action" "" args = "devices" then
            "ADB may start its local server and access your configured ADB identity. Physical serials are withheld; offline and unauthorized transports are not ready."
          else
            "Reads locally configured AVD names; no SDK or system image is installed, and no emulator is booted.")]
    | "mobile_device_lifecycle" ->
        let context = require_session_context context in
        mobile_device_lifecycle_preview ~context ~root:base_root args
    | "mobile_verify" ->
        let context = require_session_context context in
        mobile_verify_preview ~context ~root:base_root args
    | "mobile_session" ->
        let context = require_session_context context in
        mobile_session_preview ~context ~root:base_root args
    | "mobile_observe" ->
        let context = require_session_context context in
        let session = Workspace_mobile_run.get context.mobile_run_manager
          (required_string "session_id" args) in
        if session.root <> base_root then
          fail "mobile app session belongs to a different workspace root";
        if session.state <> Workspace_mobile_run.Running then
          fail "mobile screen observation requires a running app session";
        let action = required_string "action" args in
        if action = "accessibility" &&
           session.platform = Workspace_mobile_run.Ios then
          mobile_xctest_preview ~context ~root:base_root ~session
        else
          let command = Workspace_mobile_observe.command action session in
          ("Reads screen pixels or accessibility content from the selected app; output is untrusted and may contain private user data.",
           ["Working directory: " ^ Printf.sprintf "%S" session.root;
            "Device: " ^ session.device ^ " · app: " ^ session.app_id;
            "Exact command: " ^ command;
            Printf.sprintf "Maximum captured output: %d bytes."
              Workspace_mobile_observe.max_screenshot_bytes])
    | "mobile_accessibility_audit" ->
        let context = require_session_context context in
        let session = Workspace_mobile_run.get context.mobile_run_manager
          (required_string "session_id" args) in
        if session.root <> base_root then
          fail "mobile app session belongs to a different workspace root";
        if session.state <> Workspace_mobile_run.Running then
          fail "mobile accessibility audit requires a running app session";
        if session.platform <> Workspace_mobile_run.Android then
          fail "rule-based mobile accessibility audit is currently Android-only";
        let command = Workspace_mobile_observe.accessibility_audit_command session in
        ("Captures one fresh selected-app accessibility tree and reports only evidenced Android accessibility rules; output may contain private app labels.",
         ["Working directory: " ^ Printf.sprintf "%S" session.root;
          "Session/app/device: " ^ session.id ^ " · " ^ session.app_id ^ " · " ^ session.device;
          "Exact command: " ^ command;
          Printf.sprintf "Maximum captured output: %d bytes."
            Workspace_mobile_observe.max_accessibility_bytes])
    | "mobile_environment" ->
        let context = require_session_context context in
        let lines = mobile_environment_plan_preview ~context ~root:base_root args in
        ("Reads or changes only the exact approved Android emulator state for the selected running app; preview, apply and restore are separate approvals.",
         lines)
    | "mobile_app_lifecycle" ->
        let context = require_session_context context in
        let action = required_string "action" args in
        let detail lines = "Performs the exact selected-app lifecycle operation shown; it never clears app data or boots a device.", lines in
        (match action with
         | "list_scenarios" ->
             detail ["Lists only private lifecycle records under .pave/mobile-app-lifecycle."]
         | "show_scenario" | "delete_scenario" ->
             let name = required_string "name" args in
             let path = Filename.concat base_root
               (".pave/mobile-app-lifecycle/" ^ name ^ ".json") in
             detail ["Scenario: " ^ name; "Exact private record: " ^ path;
               if action = "delete_scenario" then "Deletes only this local scenario record; device state is unchanged."
               else "Reads this local scenario record; device state is unchanged."]
         | "inspect_handlers" | "open_link" | "observe" | "create_scenario" | "transition" ->
             let session = mobile_lifecycle_session ~context ~root:base_root args in
             let identity = mobile_lifecycle_identity base_root session in
             let binding = [
               "Session/app/device: " ^ session.id ^ " · " ^ session.app_id ^ " · " ^ session.device;
               "Selected APK: " ^ session.app_path;
               "Selected build SHA-256: " ^ identity.build_id] in
             (match action with
              | "inspect_handlers" ->
                  let command = "apkanalyzer manifest print " ^
                    Filename.quote (Workspace_path.checked_path base_root session.app_path) in
                  detail (binding @ ["Exact read command: " ^ command;
                    "Only exact VIEW+BROWSABLE+DEFAULT scheme/host/path handlers are retained."])
              | "open_link" ->
                  let cache = match mobile_lifecycle_cache_handlers context session.id with
                    | Some cache when cache.build_hash = identity.build_id -> cache
                    | _ -> fail "inspect URL handlers again; approved manifest evidence is missing or stale" in
                  let handler_id = required_string "handler_id" args in
                  let index = if String.starts_with ~prefix:"handler-" handler_id then
                      int_of_string_opt (String.sub handler_id 8 (String.length handler_id - 8))
                    else None in
                  let handler = match index with
                    | Some index when index > 0 ->
                        (try List.nth cache.handlers (index - 1) with _ ->
                          fail "handler_id is not in the inspected selected-app manifest")
                    | _ -> fail "handler_id is not an inspected handler" in
                  let command, link = try Workspace_mobile_app_lifecycle.deep_link_preview
                      ~approved_evidence:true session ~handler
                      ~url:(required_string "url" args)
                    with Workspace_mobile_app_lifecycle.Error message -> fail message in
                  let observation_command =
                    Workspace_mobile_app_lifecycle.observation_command session in
                  let accessibility_command = Workspace_mobile_observe.command
                    "accessibility" session in
                  let assertion = required_string "destination_assertion" args in
                  detail (binding @ ["Exact inspected handler: " ^ handler_id;
                    "Exact URL: " ^ link.url; "Exact explicit-component command: " ^ command;
                    "Exact destination text/content description: " ^ assertion;
                    "Fresh selected-app lifecycle observation before and after accessibility capture: " ^ observation_command;
                    "Fresh accessibility tree command: " ^ accessibility_command;
                    "The action fails unless the exact selected-app handler is foreground both before and after, and exactly one accessibility node has the requested text or content description."])
              | "observe" ->
                  let command = try Workspace_mobile_app_lifecycle.observation_command session
                    with Workspace_mobile_app_lifecycle.Error message -> fail message in
                  detail (binding @ ["Exact bounded observation command: " ^ command;
                    "Output is parsed for resumed component and selected-app PID."])
              | "create_scenario" ->
                  if session.state <> Workspace_mobile_run.Running then
                    fail "scenario creation requires a running selected app session";
                  let observation = match mobile_lifecycle_observation context session.id with
                    | Some observation when Workspace_mobile_app_lifecycle.same_identity
                        identity observation.identity -> observation
                    | _ -> fail "observe the exact selected app after build before creating a lifecycle scenario" in
                  if observation.state <> Workspace_mobile_app_lifecycle.Foreground ||
                     observation.process_id = None then
                    fail "scenario creation requires a foreground selected app and verified PID";
                  let name = required_string "name" args in
                  let path = Filename.concat base_root
                    (".pave/mobile-app-lifecycle/" ^ name ^ ".json") in
                  detail (binding @ ["Scenario name: " ^ name;
                    "Scenario file: " ^ path;
                    "Fresh generation/PID: " ^ string_of_int observation.generation ^
                      "/" ^ string_of_int (Option.get observation.process_id);
                    "Writes a private record only; device state is unchanged."])
              | "transition" ->
                  let name = required_string "name" args in
                  let record = try Workspace_mobile_app_lifecycle.load
                      ~root:base_root name
                    with Workspace_mobile_app_lifecycle.Error message -> fail message in
                  if not (Workspace_mobile_app_lifecycle.same_identity
                      identity record.identity) then
                    fail "scenario belongs to a different selected build, app or device";
                  let observation = match mobile_lifecycle_observation context session.id with
                    | Some observation -> observation
                    | None -> fail "take a fresh lifecycle observation before transitioning" in
                  let activity = match session.activity with Some value -> value
                    | None -> fail "selected app session has no approved launch activity" in
                  let transition = mobile_lifecycle_transition
                    (required_string "transition" args) in
                  let commands = try Workspace_mobile_app_lifecycle.prepare_transition
                      record ~approved:true ~observation ~activity transition
                    with Workspace_mobile_app_lifecycle.Error message -> fail message in
                  let verify = Workspace_mobile_app_lifecycle.observation_command session in
                  detail (binding @ [
                    "Scenario: " ^ name;
                    "Expected state: " ^
                      Workspace_mobile_app_lifecycle.state_name
                        (Workspace_mobile_app_lifecycle.expected_state transition);
                    "State-loss warning: " ^
                      Workspace_mobile_app_lifecycle.data_loss_description transition;
                    "Exact commands: " ^ String.concat " ; " commands;
                    "Exact post-transition observation: " ^ verify;
                    "No app data is cleared; failed verification is recorded as failed."])
              | _ -> assert false)
         | _ -> fail "unsupported mobile app lifecycle action")
    | "mobile_performance" ->
        let context = require_session_context context in
        mobile_performance_preview ~context ~root:base_root args
    | "mobile_dev_server" ->
        let context = require_session_context context in
        let action = required_string "action" args in
        let id = required_string "id" args in
        if action = "start" then (
          let root = Workspace_path.root_path base_root in
          let subroot = required_string "subroot" args in
          let script = required_string "script" args in
          let package_manager = optional_string "manager" "" args in
          let manager, cwd = try Workspace_node_server.command ~root ~subroot
              ~script ~package_manager
            with Workspace_node_server.Error message -> fail message in
          let exposure = Workspace_node_server.parse_exposure
            (required_string "host" args) in
          let executable, arguments = Workspace_node_server.package_manager_and_args
            ~manager ~script ~host:exposure
            ~port:(match field "port" args with
              | `Int port when port >= 1 && port <= 65_535 -> port
              | _ -> fail "port must be an integer between 1 and 65535") in
          ("Runs declared project code as a session-owned process and waits for the selected port; no dependency install or implicit restart.",
           ["Process session: " ^ id;
            "Package/script: " ^ subroot ^ " · " ^ script;
            "Working directory: " ^ Printf.sprintf "%S" cwd;
            "Exact argv: " ^ String.concat " "
              (List.map (Printf.sprintf "%S") (executable :: arguments));
            "Exposure: " ^ Workspace_node_server.display_host exposure;
            Printf.sprintf "Readiness deadline: %d seconds."
              (optional_int "readiness_timeout_seconds" 45 ~minimum:1 ~maximum:300 args)])
        ) else if action = "stop" then (
          let server = try Workspace_node_server.get context.node_server_manager ~id
            with Workspace_node_server.Error message -> fail message in
          if server.root <> base_root then
            fail "Node development server belongs to a different workspace root";
          ("Stops only the owned Node development server process for this private session.",
           ["Process session: " ^ server.id;
            "Owned process: " ^ server.process_id;
            "Current state: " ^ Workspace_node_server.render server])
        ) else
          fail "only start and stop require development-server effect approval"
    | "mobile_diagnostics" ->
        let context = require_session_context context in
        let session = Workspace_mobile_run.get context.mobile_run_manager
          (required_string "session_id" args) in
        if session.root <> base_root then
          fail "mobile app session belongs to a different workspace root";
        let action = required_string "action" args in
        let command = Workspace_mobile_diagnostics.command action session in
        ("Reads bounded app-scoped runtime logs, Android package-specific process-exit/last-ANR evidence, or iOS Simulator process/crash logs. The exact command and private-data risk are shown before approval. Local mapping/dSYM files are reported only when present and are never applied automatically.",
         ["Working directory: " ^ Printf.sprintf "%S" session.root;
          "Session/app/device: " ^ session.id ^ " · " ^ session.app_id ^ " · " ^ session.device;
          "Exact command: " ^ command;
          Printf.sprintf "Maximum captured output: %d bytes."
            Workspace_mobile_diagnostics.max_output_bytes])
    | "mobile_visual" ->
        let context = require_session_context context in
        mobile_visual_preview ~context ~root:base_root args
    | "mobile_control" ->
        let context = require_session_context context in
        let session = Workspace_mobile_run.get context.mobile_run_manager
          (required_string "session_id" args) in
        if session.root <> base_root then
          fail "mobile app session belongs to a different workspace root";
        let action = required_string "action" args in
        let value = mobile_control_action action args in
        let command = Workspace_mobile_control.command session
          ~screen_size:session.screen_size value in
        ("Performs one device-side UI state change; screen coordinates are bounded by the last screenshot. Device content may be private.",
         ["Working directory: " ^ Printf.sprintf "%S" session.root;
          "Device: " ^ session.device ^ " · app: " ^ session.app_id;
          "Exact command: " ^ command;
          "The screenshot coordinate reference is invalidated before execution, even if the command fails. Capture a new screenshot and accessibility tree to verify the UI transition."])
    | "mobile_scenario" ->
        let context = require_session_context context in
        mobile_scenario_preview ~context ~root:base_root args
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
             "Writes these exact contents to the pinned SSH workspace using SFTP; transfer failure can leave a partial remote file.",
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
  (* Sensitive-diff authorization (M21b/M22b): when a classifier is
     installed, review the exact proposed bytes for every mutation path and
     carry the binding target set in the request so the write can be
     re-verified at apply time. Unresolvable previews fail closed as
     unresolved reviews; the write then still needs a matching approval and
     revalidates on its own. *)
  let sensitive : Approval.sensitive_review option =
    if not (Sensitive_mutation.installed ()) then None
    else
      let review proposals = Sensitive_mutation.review ~root:preview_root proposals in
      (try
         match name with
         | "write_file" ->
             let _, proposal = write_proposal ~root:preview_root
                 ~path:(required_string "path" preview_args)
                 ~content:(required_string "content" preview_args) in
             review [proposal]
         | "edit_file" ->
             let prepared = Workspace_edit.prepare_unique ~root:preview_root
                 ~path:(required_string "path" preview_args)
                 ~old_text:(required_string "old_string" preview_args)
                 ~new_text:(required_string "new_string" preview_args) in
             review [proposed_of_prepared prepared]
         | "apply_edits" ->
             let prepared = Workspace_edit.prepare_hunks ~root:preview_root
                 ~path:(required_string "path" preview_args)
                 ~expected_sha256:(required_string "expected_sha256" preview_args)
                 ~hunks:(parse_hunks preview_args) in
             review [proposed_of_prepared prepared]
         | "ast_edit" when not (optional_bool "dry_run" true args) ->
             let prepared = Workspace_edit.prepare_ast ~root:preview_root
                 ~language:(required_string "language" preview_args)
                 { Workspace_edit.path = required_string "path" preview_args;
                   expected_sha256 =
                     required_string "expected_sha256" preview_args;
                   operation = ast_operation preview_args } in
             review [proposed_of_prepared prepared]
         | "lsp" when optional_string "action" "" args = "apply_preview" ->
             let _, files = lsp_preview ?context ~root:base_root args in
             Sensitive_mutation.review ~root:base_root
               (lsp_apply_proposals ~root:base_root files)
         | _ -> None
       with
       | Cancelled | Provider.Cancelled -> raise Cancelled
       | Workspace_edit.Error message | Workspace_path.Error message
       | Workspace_lsp.Error message -> fail message) in
  let details = match sensitive with
    | None -> details
    | Some review ->
        let lines = List.map (fun (item : Approval.sensitive_effect) ->
            "Sensitive change: " ^ item.effect_path ^ " — " ^
            item.effect_summary) review.effects in
        let lines = lines @ List.map (fun reason ->
            "Unresolved sensitive classification: " ^ reason)
            review.unresolved in
        let lines = lines @
          ["This change needs its own exact-content approval; it is checked again against the reviewed hashes before writing."] in
        let lines = lines @ List.concat_map
          (fun (target : Approval.sensitive_target) ->
            ["Reviewed file: " ^ Printf.sprintf "%S" target.target_path;
             "Reviewed original SHA-256: " ^ target.original_sha256;
             "Reviewed result SHA-256: " ^ target.result_sha256])
          review.targets in
        details @ lines in
  let trigger = match name with
    | "run_command" | "start_shell" -> Approval.Dangerous_command
    | "web_search" | "web_fetch" | "browser" | "ssh_command" | "publish_web" ->
        Approval.Network
    | "write_file" | "edit_file" | "apply_edits" | "ast_edit" | "workspace_rewind" ->
        Approval.File_access
    | _ -> Approval.Tool_call in
  { Approval.tool_name = name; tier = decision.tier; trigger; impact; details;
    reason = (match sensitive with
      | Some _ ->
          Some "The proposed change touches sensitive mobile configuration or cannot be classified; it requires separate exact-content approval."
      | None -> decision.reason);
    sensitive }

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
         (match bound "maxItems" schema with
          | Some maximum when List.length values > maximum ->
              fail (label ^ " exceeds its maximum item count")
          | Some _ | None -> ());
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
  "mobile_session"; "mobile_verify"; "mobile_observe"; "mobile_accessibility_audit";
  "mobile_diagnostics";
  "mobile_visual"; "mobile_control"; "mobile_scenario"; "mobile_dev_server";
  "mobile_environment"; "mobile_app_lifecycle"; "mobile_performance";
  "mobile_device_lifecycle";

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
  | Workspace_mobile_run.Error message
  | Workspace_mobile_control.Error message
  | Workspace_mobile_scenario.Error message
  | Workspace_mobile_diagnostics.Error message
  | Workspace_mobile_visual.Error message
  | Workspace_mobile_environment.Error message
  | Workspace_mobile_app_lifecycle.Error message
  | Workspace_mobile_performance.Error message
  | Workspace_mobile_device_lifecycle.Error message

  | Workspace_android_devices.Error message
  | Workspace_mobile_observe.Error message
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
      | "stop" ->
          (match optional_string "name" "" args with
           | "" -> fail "stop requires a tunnel name"
           | value -> ignore (Workspace_portal.publish_name value))
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
    let execute ?cancel ?on_progress ?(approved = false)
        ?(sensitive_review = None) () =
      try
        let result = match name with
          | "browser" -> Ok (browser_tool ~approved ?cancel ?context args)
          | "mobile_observe" ->
              Ok (mobile_observe_tool ~approved ?cancel ?on_progress
                ~context:(require_session_context context) ~root args)
          | "mobile_accessibility_audit" ->
              Ok [Protocol.Text (mobile_accessibility_audit_tool ~approved
                ?cancel ?on_progress ~context:(require_session_context context)
                ~root args)]
          | "mobile_dev_server" ->
              Ok [Protocol.Text (mobile_dev_server_tool ~approved ?cancel
                ~context:(require_session_context context) ~root args)]
          | "mobile_environment" ->
              Ok [Protocol.Text (mobile_environment_tool ~approved ?cancel
                ~context:(require_session_context context) ~root args)]
          | "mobile_app_lifecycle" ->
              Ok [Protocol.Text (mobile_app_lifecycle_tool ~approved ?cancel
                ?on_progress ~context:(require_session_context context) ~root args)]
          | "mobile_performance" ->
              Ok [Protocol.Text (mobile_performance_tool ~approved ?cancel
                ?on_progress ~context:(require_session_context context) ~root args)]
          | "mobile_diagnostics" ->
              Ok (mobile_diagnostics_tool ~approved ?cancel ?on_progress
                ~context:(require_session_context context) ~root args)
          | "mobile_visual" ->
              Ok (mobile_visual_tool ~approved ?cancel ?on_progress
                ~context:(require_session_context context) ~root args)
          | "mobile_control" ->
              Ok [Protocol.Text (mobile_control_tool ~approved ?cancel ?on_progress
                ~context:(require_session_context context) ~root args)]
          | "mobile_scenario" ->
              Ok [Protocol.Text (mobile_scenario_tool ~approved ?cancel ?on_progress
                ~context:(require_session_context context) ~root args)]
          | "publish_web" -> Ok (publish_web_tool ~approved ?cancel ?context args)
          | "memory" -> Ok [Protocol.Text (Yojson.Basic.to_string
              (try memory_tool ~approved ~root args
               with Workspace_memory.Error message -> fail message))]
          | "mobile_device_lifecycle" ->
              Ok [Protocol.Text (mobile_device_lifecycle_tool ~approved
                ?cancel ?on_progress ~context:(require_session_context context)
                ~root args)]
          | _ -> Ok [Protocol.Text (match name with
          | "read_file" -> read_file ?cancel ?context root args
          | "workspace_snapshot" -> workspace_snapshot ?cancel ?context tool_root tool_args
          | "list_files" -> list_files ?cancel tool_root tool_args
          | "fuzzy_file_search" -> fuzzy_file_search ?cancel tool_root tool_args
          | "search" -> search ?cancel tool_root tool_args
          | "glob" -> glob ?cancel tool_root tool_args
          | "grep" -> grep ?cancel tool_root tool_args
          | "write_file" -> write_file ~approved ~sensitive_review tool_root tool_args
          | "edit_file" -> edit_file ~approved ~sensitive_review tool_root tool_args
          | "apply_edits" -> apply_edits ~approved ~sensitive_review ?cancel ?context tool_root tool_args
          | "ast_edit" -> ast_edit ~approved ~sensitive_review ?cancel ?context tool_root tool_args
          | "run_command" -> run_command ?cancel ?on_progress root args
          | "xcode_preflight" ->
              xcode_preflight ~approved ?cancel ?on_progress ?context root args
          | "mobile_check" ->
              mobile_check ~approved ?cancel ?on_progress ?context root args
          | "android_devices" ->
              android_devices ~approved ?cancel ?on_progress ?context root args
          | "mobile_session" ->
              mobile_session ~approved ?cancel ?on_progress ?context root args
          | "mobile_verify" ->
              mobile_verify ~approved ?cancel ?on_progress
                ~context:(require_session_context context) ~root args
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
          | "lsp_start" -> lsp_start ~approved ?cancel ?context root args
          | "lsp" -> lsp_execute ~approved ~sensitive_review ?cancel ?context root args
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
    ?(sensitive_review = None) ~root ~name ~args () =
  match prepare ?cancel ?context ~root ~name ~args () with
  | Error result -> Error result
  | Ok execute ->
      try
        match preflight with
        | Some check ->
            (match check () with
             | Some message -> Error message
             | None -> execute ?cancel ?on_progress ~approved ~sensitive_review ())
        | None -> execute ?cancel ?on_progress ~approved ~sensitive_review ()
      with
      | Cancelled -> raise Cancelled
      | exn -> Error (error_message exn)


(* Install the real iOS/Android sensitive-change classifier as the
   mutation-review source. The adapter is pure and bounded by
   Workspace_sensitive; unresolved proposals fail closed in
   require_sensitive_review. Callers may still replace the classifier in
   tests via Sensitive_mutation.set_classifier. *)
let () =
  Sensitive_mutation.set_classifier
    (Some (fun ~root:_ (change : Sensitive_mutation.change) ->
      match Workspace_sensitive.classify_proposal ~path:change.path
          ~before:change.before ~after:change.after with
      | Workspace_sensitive.Ordinary -> Sensitive_mutation.Ordinary
      | Workspace_sensitive.Literal report ->
          Sensitive_mutation.Sensitive
            (List.map (fun (finding : Workspace_sensitive.finding) ->
              { Approval.effect_path = report.file;
                effect_summary = finding.detail }) report.findings)
      | Workspace_sensitive.Unresolved report ->
          Sensitive_mutation.Unresolved
            (Workspace_sensitive.describe (Workspace_sensitive.Unresolved report))))
