exception Error of string

let fail message = raise (Error message)

let valid_segment segment =
  segment <> "" && segment <> "." && segment <> ".." &&
  String.length segment <= 128 &&
  String.for_all (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' -> true
    | _ -> false) segment

let qualified_task value =
  String.length value <= 512 && String.starts_with ~prefix:":" value &&
  match String.split_on_char ':' value with
  | "" :: (_ :: _ as components) -> List.for_all valid_segment components
  | _ -> false

let task_name value = valid_segment value

let manifest_limit = Workspace_path.max_write_bytes
let output_limit = Workspace_path.max_read_bytes

let existing_kind path =
  try Some (Unix.lstat path).Unix.st_kind with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> None

let validate_directory root subroot =
  if subroot = "" || String.contains subroot '\000' ||
     not (Filename.is_relative subroot) ||
     (subroot <> "." &&
      List.exists (fun part -> part = "" || part = "." || part = "..")
        (String.split_on_char '/' subroot)) then
    fail "select an exact workspace-relative Gradle settings directory";
  let parts = if subroot = "." then [] else String.split_on_char '/' subroot in
  let _, directory = List.fold_left (fun (relative, _) part ->
    let relative = if relative = "" then part else relative ^ "/" ^ part in
    let path = Workspace_path.checked_path root relative in
    if existing_kind path <> Some Unix.S_DIR then
      fail "Gradle settings directory is absent or contains a symlink";
    relative, path) ("", root) parts in
  directory

(* Only a manifest made of literal include declarations and simple project-name
   assignments provides a complete static module set. Any other construct can
   add projects at configuration time, so it cannot justify rejecting a task. *)
let static_modules source =
  let modules = ref [] and complete = ref true in
  let parse_include text =
    let length = String.length text in
    let cursor = ref 0 in
    let skip () =
      while !cursor < length && (text.[!cursor] = ' ' || text.[!cursor] = '\t') do
        incr cursor
      done in
    let quoted () =
      skip ();
      if !cursor >= length || (text.[!cursor] <> '\'' && text.[!cursor] <> '"')
      then None
      else
        let quote = text.[!cursor] in
        incr cursor;
        let start = !cursor in
        while !cursor < length && text.[!cursor] <> quote &&
          text.[!cursor] <> '\\' && text.[!cursor] <> '$' do
          incr cursor
        done;
        if !cursor = length || text.[!cursor] <> quote then None
        else
          let value = String.sub text start (!cursor - start) in
          incr cursor;
          Some value in
    skip ();
    let parenthesized = !cursor < length && text.[!cursor] = '(' in
    if parenthesized then incr cursor;
    let rec values acc =
      match quoted () with
      | None -> None
      | Some value ->
          skip ();
          if !cursor < length && text.[!cursor] = ',' then (
            incr cursor;
            values (value :: acc))
          else (
            if parenthesized then (
              if !cursor < length && text.[!cursor] = ')' then incr cursor
              else cursor := length + 1);
            skip ();
            if !cursor < length && text.[!cursor] = ';' then incr cursor;
            skip ();
            if !cursor = length then Some (List.rev (value :: acc)) else None) in
    values [] in
  String.split_on_char '\n' source |> List.iter (fun original ->
    let line = String.trim original in
    let len = String.length line in
    if line = "" || String.starts_with ~prefix:"//" line then ()
    else if len >= 7 && String.sub line 0 7 = "include" &&
            (len = 7 || List.mem line.[7] [' '; '\t'; '('; '\''; '"']) then (
      match parse_include (String.sub line 7 (len - 7)) with
      | None -> complete := false
      | Some names -> List.iter (fun name ->
          match String.split_on_char ':' name with
          | "" :: (_ :: _ as parts) when List.for_all valid_segment parts ->
              modules := (":" ^ String.concat ":" parts) :: !modules
          | _ -> complete := false) names)
    else if String.starts_with ~prefix:"rootProject.name" line then (
      let assignment = String.sub line 16 (len - 16) |> String.trim in
      let length = String.length assignment in
      if length < 4 || assignment.[0] <> '=' then complete := false
      else
        let value = String.trim (String.sub assignment 1 (length - 1)) in
        let length = String.length value in
        if length < 2 || not (List.mem value.[0] ['\''; '"']) ||
           value.[length - 1] <> value.[0] ||
           String.contains value '$' || String.contains value '\\' ||
           String.contains (String.sub value 1 (length - 2)) value.[0]
        then complete := false)
    else complete := false);
  if !complete then Some (List.sort_uniq String.compare !modules) else None

let command ~root ~subroot ~action ~task =
  try
    let root = Workspace_path.root_path root in
    let cwd = validate_directory root subroot in
    let manifests = List.filter_map (fun name ->
      let relative = if subroot = "." then name else subroot ^ "/" ^ name in
      let path = Workspace_path.checked_path root relative in
      match existing_kind path with
      | None -> None
      | Some Unix.S_REG -> Some path
      | Some _ -> fail "Gradle settings must be a regular file, not a symlink")
      ["settings.gradle"; "settings.gradle.kts"] in
    let manifest = match manifests with
      | [path] -> path
      | [] -> fail "selected directory has no Gradle settings manifest"
      | _ -> fail "selected directory has ambiguous Gradle settings manifests" in
    if (Unix.lstat manifest).Unix.st_size > manifest_limit then
      fail "Gradle settings manifest exceeds the read limit";
    let settings = Workspace_path.read_bounded manifest manifest_limit in
    match action with
    | "tasks" ->
        if task <> "" then fail "Gradle task discovery does not accept a task";
        "gradle --offline tasks --all", cwd
    | "run" ->
        if not (qualified_task task) then
          fail "select an exact qualified Gradle task (:task or :module:task)";
        let components = String.split_on_char ':' task in
        let module_path = match components with
          | "" :: _ :: [] -> None
          | "" :: rest -> Some (":" ^ String.concat ":"
              (List.rev (List.tl (List.rev rest))))
          | _ -> assert false in
        (match module_path, static_modules settings with
        | Some module_path, Some observed when not (List.mem module_path observed) ->
            fail "Gradle task targets a module not declared in settings"
        | _ -> ());
        "gradle --offline '" ^ task ^ "'", cwd
    | _ -> fail "unsupported Gradle action (expected tasks or run)"
  with
  | Workspace_path.Error message -> fail message
  | Unix.Unix_error (error, _, _) -> fail ("Gradle settings unavailable: " ^ Unix.error_message error)

let tasks output =
  if String.length output > output_limit || output = "" ||
     output.[String.length output - 1] <> '\n' ||
     String.contains output '\000' then
    fail "Gradle task output is missing, truncated, or exceeds the read limit";
  let lines = String.split_on_char '\n' output in
  let section = ref false and banner = ref false and success = ref false in
  let names = ref [] in
  let dash_line line = String.length line >= 3 &&
    String.for_all (fun c -> c = '-') line in
  let rejects line =
    List.exists (fun prefix -> String.starts_with ~prefix line)
      ["FAILURE:"; "BUILD FAILED"; "* Exception is:"; "* What went wrong:";
       "Exception in thread"; "\tat "; "at "; "[truncated";
       "... more"; "> Task :tasks FAILED"] in
  let rec scan = function
    | [] -> ()
    | raw :: rest ->
        let line = String.trim raw in
        if rejects line || String.contains line '\027' then
          fail "Gradle task output contains an error or truncation";
        if String.starts_with ~prefix:"Tasks runnable from root project " line ||
           String.starts_with ~prefix:"All tasks runnable from root project " line
        then banner := true;
        if String.starts_with ~prefix:"BUILD SUCCESSFUL in " line then (
          if not !banner then fail "Gradle task output lacks a task listing";
          success := true;
          section := false)
        else if !banner && not !success then (
          match rest with
          | underline :: _ when line <> "" && dash_line (String.trim underline) &&
              not (dash_line line) -> section := line <> "Rules"
          | _ when !section && line <> "" && not (dash_line line) &&
                   not (String.starts_with ~prefix:"Pattern: " line) ->
              let name = match String.index_opt line ' ' with
                | None -> line
                | Some index ->
                    let suffix = String.sub line index (String.length line - index) in
                    if not (String.starts_with ~prefix:" - " suffix) then
                      fail "Gradle task listing contains a malformed row";
                    String.sub line 0 index in
              let name = if String.starts_with ~prefix:":" name then name
                else ":" ^ name in
              if not (qualified_task name) then
                fail "Gradle task listing contains an unsafe task name";
              names := name :: !names
          | _ -> ());
        scan rest in
  scan lines;
  if not !banner || not !success || !names = [] then
    fail "Gradle task output has no complete task listing";
  List.sort_uniq String.compare !names
