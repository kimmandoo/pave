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
        "process == \"ReportCrash\" OR eventMessage CONTAINS[c] \"" ^ session.app_id ^ "\""
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

let process_exit_crashes output =
  let selected = ref [] and current = ref None in
  let finish () = match !current with
    | None -> ()
    | Some lines ->
        let block = String.concat "\n" (List.rev lines) in
        if contains block "reason=4 (" || contains block "reason=5 (" then
          selected := block :: !selected;
        current := None in
  String.split_on_char '\n' (clean_output output)
  |> List.iter (fun line ->
       if String.starts_with ~prefix:"        ApplicationExitInfo #" line then (
         finish ();
         current := Some [line])
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
        if selected <> "" then selected else process_exit_crashes output
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
