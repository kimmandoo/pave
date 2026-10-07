exception Error of string
let fail message = raise (Error message)

let max_output_bytes = 1_048_576

let symbol_artifact (session : Workspace_mobile_run.session) =
  let relative = match session.platform, session.variant with
    | Workspace_mobile_run.Android, Some variant ->
        let module_root = List.fold_left
          (fun path _ -> Filename.dirname path) session.app_path [1; 2; 3; 4; 5] in
        Filename.concat module_root
          (Filename.concat "build"
            (Filename.concat "outputs/mapping" (Filename.concat variant "mapping.txt")))
    | Workspace_mobile_run.Android, None -> ""
    | Workspace_mobile_run.Ios, _ ->
        let app = session.app_path in
        let stem = if Filename.check_suffix app ".app"
          then Filename.chop_suffix app ".app" else app in
        Filename.concat (stem ^ ".app.dSYM/Contents/Resources/DWARF")
          (Filename.basename stem)
  in
  if relative = "" then `Assoc ["status", `String "unavailable"]
  else
    try
      let path = Workspace_path.checked_path session.root relative in
      let kind = (Unix.lstat path).Unix.st_kind in
      if kind = Unix.S_REG then `Assoc [
        "status", `String "available"; "path", `String relative]
      else `Assoc ["status", `String "unavailable"]
    with Unix.Unix_error _ | Workspace_path.Error _ ->
      `Assoc ["status", `String "unavailable"]
let max_lines = 2_000

let command action (session : Workspace_mobile_run.session) =
  let quote = Filename.quote in
  match action, session.platform with
  | "logs", Workspace_mobile_run.Android ->
      let remote = Printf.sprintf
        "pid=$(pidof -s %s); case \"$pid\" in ''|*[!0-9]*) echo 'selected app is not running' >&2; exit 3;; esac; logcat -d -v threadtime --pid=\"$pid\" -t %d"
        (quote session.app_id) max_lines in
      "adb -s " ^ quote session.device ^ " shell " ^ quote remote
  | "crashes", Workspace_mobile_run.Android ->
      "adb -s " ^ quote session.device ^ " shell dumpsys activity exit-info " ^
      quote session.app_id
  | "anr", Workspace_mobile_run.Android ->
      "adb -s " ^ quote session.device ^ " shell dumpsys activity lastanr"
  | ("logs" | "crashes"), Workspace_mobile_run.Ios ->
      let predicate = if action = "crashes" then
        "process == \"ReportCrash\" AND eventMessage CONTAINS[c] \"" ^ session.app_id ^ "\""
        else "process == \"" ^ session.app_id ^ "\" OR eventMessage CONTAINS[c] \"" ^ session.app_id ^ "\"" in
      "xcrun simctl spawn " ^ quote session.device ^ " log show --last 30m --style compact --predicate " ^ quote predicate
  | "anr", Workspace_mobile_run.Ios ->
      fail "ANR diagnostics are Android-only"
  | _ -> fail "mobile diagnostics action must be logs, crashes or anr"

let clean_output output =
  let buffer = Buffer.create (String.length output) in
  String.iter (fun ch ->
    let code = Char.code ch in
    if ch = '\n' || ch = '\r' || ch = '\t' || code >= 32 && code <> 127 then
      Buffer.add_char buffer ch) output;
  Buffer.contents buffer

let selected_android_report action app_id output =
  let lines = String.split_on_char '\n' (clean_output output) in
  let marker = match action with
    | "crashes" -> "Process: "
    | "anr" -> "ANR in "
    | _ -> fail "report filter requires crashes or anr" in
  let contains_at text needle =
    let rec find index =
      if index + String.length needle > String.length text then None
      else if String.sub text index (String.length needle) = needle then Some index
      else find (index + 1) in
    find 0 in
  let report_matches line =
    match contains_at line (marker ^ app_id) with
    | None -> false
    | Some start ->
        let after = start + String.length marker + String.length app_id in
        after = String.length line ||
        List.mem line.[after] [' '; ','; '('; ':'] in
  let selected = ref false and rows = ref [] and count = ref 0
  and preceding_crash = ref [] in
  List.iter (fun line ->
    let fatal = contains_at line "FATAL EXCEPTION" <> None in
    let next_report = contains_at line "Process: " <> None ||
      contains_at line "ANR in " <> None in
    if action = "crashes" && fatal then (
      selected := false;
      preceding_crash := [line]);
    let matching = report_matches line in
    if matching then (
      selected := true;
      if action = "crashes" then (
        rows := List.rev_append !preceding_crash !rows;
        preceding_crash := []));
    if next_report && not matching then selected := false;
    if !selected && !count < max_lines then (
      rows := line :: !rows;
      incr count)) lines;
  String.concat "\n" (List.rev !rows)

let contains text needle =
  let rec find index =
    if index + String.length needle > String.length text then false
    else if String.sub text index (String.length needle) = needle then true
    else find (index + 1) in
  find 0

let process_exit_crashes ~app_id output =
  let selected = ref [] and current = ref None in
  let crash_identity line =
    let line = String.trim line in
    if not (String.starts_with ~prefix:"process=" line) then false
    else
      match String.index_opt line ' ' with
      | None -> false
      | Some ending ->
          let process = String.sub line 8 (ending - 8) in
          let reason = String.sub line (ending + 1)
            (String.length line - ending - 1) in
          (process = app_id || String.starts_with ~prefix:(app_id ^ ":") process) &&
          (String.starts_with ~prefix:"reason=4 (" reason ||
           String.starts_with ~prefix:"reason=5 (" reason) in
  let finish () = match !current with
    | None -> ()
    | Some lines ->
        if List.length (List.filter crash_identity lines) = 1 then
          selected := String.concat "\n" (List.rev lines) :: !selected;
        current := None in
  String.split_on_char '\n' (clean_output output)
  |> List.iter (fun line ->
       if String.starts_with ~prefix:"ApplicationExitInfo #" (String.trim line) then (
         finish ();
         current := Some [line])
       else if String.starts_with ~prefix:"Historical Process Exit " (String.trim line) then
         finish ()
       else match !current with
         | None -> ()
         | Some lines -> current := Some (line :: lines));
  finish ();
  String.concat "\n" (List.rev !selected)

let result ~action (session : Workspace_mobile_run.session) ~output ~truncated =
  if String.length output > max_output_bytes then fail "mobile diagnostic output exceeds its byte limit";
  let report = match session.platform, action with
    | Workspace_mobile_run.Android, "crashes" ->
        let selected = selected_android_report action session.app_id output in
        if selected <> "" then selected else process_exit_crashes ~app_id:session.app_id output
    | Workspace_mobile_run.Android, "anr" ->
        selected_android_report action session.app_id output
    | _, _ -> clean_output output in
  let has_evidence = String.trim report <> "" in
  let status = if truncated then "incomplete"
    else if has_evidence then "evidence_available"
    else "no_matching_evidence" in
  Yojson.Basic.to_string (`Assoc [
    "session_id", `String session.id;
    "platform", `String (Workspace_mobile_run.platform_name session.platform);
    "app_id", `String session.app_id;
    "action", `String action;
    "status", `String status;
    "truncated", `Bool truncated;
    "symbol_artifact", symbol_artifact session;
    "symbolication", `String "No automatic symbolication is performed; captured frames remain unsymbolicated";
    "evidence", `String (if truncated then "" else report)
  ])

let read_bounded_file path limit =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    let length = in_channel_length channel in
    if length < 0 || length > limit then fail "mobile crash artifact exceeds its byte limit";
    really_input_string channel length)

let android_mapping_path (session : Workspace_mobile_run.session) =
  match session.platform, session.variant with
  | Workspace_mobile_run.Android, Some variant ->
      let module_root = List.fold_left (fun path _ -> Filename.dirname path)
        session.app_path [1; 2; 3; 4; 5] in
      Filename.concat module_root
        (Filename.concat "build"
          (Filename.concat "outputs/mapping" (Filename.concat variant "mapping.txt")))
  | _ -> fail "Android retrace requires a selected Android build variant"

let installed_retrace () =
  let path = Option.value (Sys.getenv_opt "PATH") ~default:"" in
  let rec find = function
    | [] -> None
    | directory :: rest when not (Filename.is_relative directory) ->
        let candidate = Filename.concat directory "retrace" in
        (try
           if (Unix.stat candidate).Unix.st_kind = Unix.S_REG &&
              Unix.access candidate [Unix.X_OK] = () then Some candidate
           else find rest
         with Unix.Unix_error _ -> find rest)
    | _ :: rest -> find rest in
  find (String.split_on_char ':' path)

let android_deobfuscate ~approved ~(run : program:string -> arguments:string list ->
    stdin:string -> string) (session : Workspace_mobile_run.session) ~crash =
  if not approved then fail "Android crash deobfuscation requires separate explicit approval";
  if session.platform <> Workspace_mobile_run.Android then
    fail "Android crash deobfuscation is Android-only";
  if String.length crash > max_output_bytes then fail "Android crash exceeds its byte limit";
  let relative = android_mapping_path session in
  let mapping =
    try Workspace_path.checked_path session.root relative
    with Workspace_path.Error _ -> fail "selected build mapping is outside the workspace" in
  (try
     if (Unix.lstat mapping).Unix.st_kind <> Unix.S_REG then
       fail "selected build mapping is not a regular file"
   with Unix.Unix_error _ -> fail "selected build mapping is unavailable");
  ignore (read_bounded_file mapping max_output_bytes);
  let program = match installed_retrace () with
    | Some program -> program
    | None -> fail "no installed allowlisted retrace executable is available; crash remains unsymbolicated" in
  let transformed =
    try run ~program ~arguments:[mapping; "-"] ~stdin:crash
    with _ -> fail "installed retrace failed; original crash remains available" in
  if String.length transformed > max_output_bytes then
    fail "retrace output exceeds its byte limit; original crash remains available";
  `Assoc [
    "status", `String "symbolicated";
    "mapping", `String relative;
    "tool", `String (Filename.basename program);
    "raw_evidence", `String crash;
    "transformed_evidence", `String transformed
  ]

let ios_crash_directory device =
  if device = "" || not (String.for_all (function
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' -> true | _ -> false) device) then
    fail "invalid selected simulator identity";
  let home = match Sys.getenv_opt "HOME" with
    | Some home when Filename.is_relative home |> not -> home
    | _ -> fail "simulator crash retrieval requires an absolute user home" in
  Filename.concat home
    (Filename.concat "Library/Developer/CoreSimulator/Devices"
      (Filename.concat device "data/Library/Logs/CrashReporter"))

let ios_retrieve_crashes ~approved (session : Workspace_mobile_run.session) =
  if not approved then fail "iOS Simulator crash retrieval requires separate explicit approval";
  if session.platform <> Workspace_mobile_run.Ios then
    fail "iOS crash retrieval is iOS-only";
  let directory = ios_crash_directory session.device in
  let entries =
    try Sys.readdir directory |> Array.to_list
    with Unix.Unix_error _ -> fail "selected Simulator crash-report directory is unavailable" in
  let matching = entries
    |> List.filter (fun name ->
         Filename.check_suffix name ".ips" || Filename.check_suffix name ".crash")
    |> List.sort (fun left right ->
         let mtime name =
           try (Unix.stat (Filename.concat directory name)).Unix.st_mtime
           with Unix.Unix_error _ -> 0. in
         Float.compare (mtime right) (mtime left))
    |> (fun names -> List.filteri (fun index _ -> index < 20) names) in
  let remaining = ref max_output_bytes in
  `List (List.map (fun name ->
    let path = Filename.concat directory name in
    try
      if (Unix.lstat path).Unix.st_kind <> Unix.S_REG then
        `Assoc ["name", `String name; "status", `String "unavailable"]
      else
        let report = read_bounded_file path !remaining in
        remaining := !remaining - String.length report;
        let selected = contains report session.app_id &&
          (contains report ("\"" ^ session.app_id ^ "\"") ||
           contains report ("Identifier: " ^ session.app_id) ||
           contains report ("Bundle Identifier: " ^ session.app_id)) in
        `Assoc ["name", `String name;
          "status", `String (if selected then "available" else "foreign_report");
          "raw_evidence", `String report]
    with Unix.Unix_error _ | Error _ ->
      `Assoc ["name", `String name; "status", `String "unavailable"])
    matching)

let ios_symbolicate ~approved ~(run : program:string -> arguments:string list ->
    stdin:string -> string) (session : Workspace_mobile_run.session) ~report
    ~binary_uuid ~dsym_uuid ~binary_architecture ~dsym_architecture =
  if not approved then fail "iOS crash symbolication requires separate explicit approval";
  if session.platform <> Workspace_mobile_run.Ios then fail "iOS symbolication is iOS-only";
  if String.length report > max_output_bytes then fail "iOS crash report exceeds its byte limit";
  let is_selected = contains report ("\"" ^ session.app_id ^ "\"") ||
    contains report ("Identifier: " ^ session.app_id) ||
    contains report ("Bundle Identifier: " ^ session.app_id) in
  if not is_selected then fail "foreign or unverified iOS crash report remains unsymbolicated";
  let normalize value = String.lowercase_ascii (String.trim value) in
  if normalize binary_uuid = "" || normalize binary_uuid <> normalize dsym_uuid ||
     not (contains (normalize report) (normalize binary_uuid)) then
    fail "iOS binary, report and dSYM UUID mismatch; crash remains unsymbolicated";
  if normalize binary_architecture = "" ||
     normalize binary_architecture <> normalize dsym_architecture ||
     not (contains (normalize report) (normalize binary_architecture)) then
    fail "iOS binary, report and dSYM architecture mismatch; crash remains unsymbolicated";
  let path = Option.value (Sys.getenv_opt "PATH") ~default:"" in
  let rec find = function
    | [] -> None
    | directory :: rest when not (Filename.is_relative directory) ->
        let candidate = Filename.concat directory "symbolicatecrash" in
        (try if (Unix.stat candidate).Unix.st_kind = Unix.S_REG &&
                Unix.access candidate [Unix.X_OK] = () then Some candidate
          else find rest
         with Unix.Unix_error _ -> find rest)
    | _ :: rest -> find rest in
  let program = match find (String.split_on_char ':' path) with
    | Some program -> program
    | None -> fail "no installed allowlisted symbolicatecrash executable; crash remains unsymbolicated" in
  let dsym = symbol_artifact session in
  let dsym_path = match dsym with
    | `Assoc fields when List.assoc_opt "status" fields = Some (`String "available") ->
        Filename.concat session.root
          (Filename.chop_suffix session.app_path ".app" ^ ".app.dSYM")
    | _ -> fail "matching iOS dSYM is unavailable; crash remains unsymbolicated" in
  let transformed =
    let crash_file = Filename.temp_file "pave-ios-crash-" ".ips" in
    Fun.protect ~finally:(fun () -> try Sys.remove crash_file with Sys_error _ -> ())
      (fun () ->
        let channel = open_out_bin crash_file in
        Fun.protect ~finally:(fun () -> close_out_noerr channel)
          (fun () -> output_string channel report);
        try run ~program ~arguments:[crash_file; dsym_path] ~stdin:""
        with _ -> fail "installed iOS symbolicator failed; original crash remains available") in
  if String.length transformed > max_output_bytes then
    fail "iOS symbolication output exceeds its byte limit";
  `Assoc ["status", `String "symbolicated"; "raw_evidence", `String report;
    "transformed_evidence", `String transformed;
    "binary_uuid", `String binary_uuid; "architecture", `String binary_architecture]
