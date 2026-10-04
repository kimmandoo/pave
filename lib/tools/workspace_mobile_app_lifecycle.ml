exception Error of string
let fail message = raise (Error message)

type identity = {
  platform : Workspace_mobile_run.platform;
  device : string;
  app_id : string;
  app_path : string;
  build_id : string;
}

type handler = {
  app_id : string;
  activity : string;
  scheme : string;
  host : string;
  path : string;
}
type deep_link = { scheme : string; host : string; path : string; url : string }
type app_state = Foreground | Backgrounded | Process_recreated | Failed of string
type transition = Background | Resume | Recreate_process

type deep_link_accessibility_observation = {
  identity : identity;
  generation : int;
  nodes : Workspace_mobile_observe.node list;
}

type verified_deep_link_destination = {
  identity : identity;
  generation : int;
  link : deep_link;
  assertion : string;
}

type record = {
  name : string;
  identity : identity;
  mutable state : app_state;
  mutable generation : int;
  mutable process_id : int option;
}
type observation = {
  identity : identity;
  state : app_state;
  generation : int;
  process_id : int option;
  resumed_activity : string option;
}

let identity_of_session (session : Workspace_mobile_run.session) ~build_id =
  if build_id = "" || String.length build_id > 256 then fail "build identity must contain 1..256 bytes";
  { platform = session.platform; device = session.device; app_id = session.app_id;
    app_path = session.app_path; build_id }

let same_identity left right =
  left.platform = right.platform && left.device = right.device &&
  left.app_id = right.app_id && left.app_path = right.app_path &&
  left.build_id = right.build_id

let observation_command (session : Workspace_mobile_run.session) =
  if session.platform <> Workspace_mobile_run.Android then
    fail "live app lifecycle observation is currently Android-only";
  if session.state <> Workspace_mobile_run.Running then
    fail "live app lifecycle observation requires a running selected app";
  let remote = "if ! dumpsys activity activities; then printf '\\nPAVE_LIFECYCLE_STATUS=failed\\n'; exit 1; fi; " ^
    "if ! pid=$(pidof -s " ^ Filename.quote session.app_id ^
    ") || [ -z \"$pid\" ]; then printf '\\nPAVE_LIFECYCLE_STATUS=failed\\n'; exit 1; fi; " ^
    "printf '\\nPAVE_LIFECYCLE_PID=%s\\nPAVE_LIFECYCLE_STATUS=ok\\n' \"$pid\"" in
  "adb -s " ^ Filename.quote session.device ^ " shell " ^ Filename.quote remote

let parse_observation ~(identity : identity) ~generation output =
  if String.length output > 1_048_576 || String.contains output '\000' then
    fail "app lifecycle observation exceeds its output limit";
  let lines = String.split_on_char '\n' output |> List.map String.trim in
  let statuses = List.filter (String.starts_with ~prefix:"PAVE_LIFECYCLE_STATUS=") lines in
  (match statuses with
   | ["PAVE_LIFECYCLE_STATUS=ok"] -> ()
   | ["PAVE_LIFECYCLE_STATUS=failed"] -> fail "lifecycle observation command failed"
   | _ -> fail "lifecycle observation is missing its successful command-status marker");
  let markers = List.filter (String.starts_with ~prefix:"PAVE_LIFECYCLE_PID=") lines in
  let pid_marker = "PAVE_LIFECYCLE_PID=" in
  let process_id = match markers with
    | [line] ->
        let value = String.sub line (String.length pid_marker)
            (String.length line - String.length pid_marker) in
        if value = "" then fail "lifecycle observation has no selected-app PID";
        (match int_of_string_opt value with
         | Some pid when pid > 0 && pid <= 4_194_304 -> Some pid
         | _ -> fail "lifecycle observation contains an invalid selected-app PID")
    | _ -> fail "lifecycle observation is missing its exact selected-app PID marker" in
  let component_on_line line =
    let is_resumed =
      String.starts_with ~prefix:"mResumedActivity:" line ||
      String.starts_with ~prefix:"topResumedActivity=" line ||
      String.starts_with ~prefix:"ResumedActivity:" line in
    if not is_resumed then None
    else
      let tokens = List.concat_map (fun part ->
        List.fold_left (fun pieces separator ->
          List.concat_map (String.split_on_char separator) pieces)
          [part] ['{'; '}'; ','; '='])
        (String.split_on_char ' ' line) in
      List.find_map (fun token ->
        match String.split_on_char '/' token with
        | [package; class_name] when package = identity.app_id && class_name <> "" ->
            let class_name = if String.starts_with ~prefix:"." class_name then
                package ^ class_name
              else if String.contains class_name '.' then class_name
              else package ^ "." ^ class_name in
            if String.starts_with ~prefix:(package ^ ".") class_name &&
               String.for_all
                 (function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' | '$' -> true | _ -> false)
                 class_name then Some (package ^ "/" ^ class_name)
            else None
        | _ -> None) tokens in
  let resumed_activity = List.find_map component_on_line lines in
  let state = match process_id, resumed_activity with
    | None, _ -> Failed "selected app process is not running"
    | Some _, Some _ -> Foreground
    | Some _, None -> Backgrounded in
  { identity; state; generation; process_id; resumed_activity }

let check_token label value maximum allow =
  if value = "" || String.length value > maximum || not (String.for_all allow value) then
    fail ("invalid " ^ label)

let valid_scheme value =
  check_token "deep-link scheme" value 64 (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '+' | '.' | '-' -> true | _ -> false);
  let first = value.[0] in
  if not ((first >= 'a' && first <= 'z') || (first >= 'A' && first <= 'Z')) then
    fail "deep-link scheme must start with a letter";
  String.lowercase_ascii value

let normalize_host value =
  check_token "deep-link host" value 253 (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '-' -> true | _ -> false);
  let host = String.lowercase_ascii value in
  if String.starts_with ~prefix:"." host || String.ends_with ~suffix:"." host ||
     String.contains host '/' || String.contains host ':' then fail "invalid deep-link host";
  host

let valid_path value =
  if value = "" || value.[0] <> '/' || String.length value > 1024 ||
     String.contains value '?' || String.contains value '#' ||
     String.contains value '\\' || String.contains value '\000' ||
     String.exists (fun c -> Char.code c <= 32 || Char.code c = 127) value then
    fail "deep-link path must be an exact absolute path without query or fragment";
  value

let parse_url url =
  if String.length url > 2048 then fail "deep-link URL exceeds 2048 bytes";
  let separator = match String.index_opt url ':' with
    | Some index when index + 2 < String.length url &&
        String.sub url index 3 = "://" -> index
    | _ -> fail "deep-link URL must use an explicit scheme://host/path form" in
  let scheme = valid_scheme (String.sub url 0 separator) in
  let rest = String.sub url (separator + 3) (String.length url - separator - 3) in
  let slash = match String.index_opt rest '/' with Some index -> index
    | None -> fail "deep-link URL must include an exact path" in
  let host = normalize_host (String.sub rest 0 slash) in
  let path = valid_path (String.sub rest slash (String.length rest - slash)) in
  { scheme; host; path; url }
let verify_deep_link_destination ~identity ~generation ~link ~observation ~assertion =
  check_token "deep-link destination assertion" assertion 512 (fun c ->
    Char.code c > 32 && Char.code c <> 127);
  if List.length observation.nodes > Workspace_mobile_observe.max_nodes then
    fail "accessibility observation exceeds its node limit";
  if not (same_identity identity observation.identity) then
    fail "accessibility observation belongs to a different selected app, device, or build";
  if observation.generation <> generation then
    fail "accessibility observation is stale";
  let matching_nodes = List.filter (fun (node : Workspace_mobile_observe.node) ->
    node.package = identity.app_id &&
    (node.text = assertion || node.description = assertion)) observation.nodes in
  match matching_nodes with
  | [] -> fail "accessibility observation has no exact destination text or description"
  | [_] -> { identity; generation; link; assertion }
  | _ -> fail "accessibility observation has ambiguous exact destination nodes"

let manifest_component app_id class_name =
  check_token "Android manifest activity name" class_name 256
    (function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' | '$' -> true | _ -> false);
  let class_name =
    if String.starts_with ~prefix:"." class_name then app_id ^ class_name
    else if String.contains class_name '.' then class_name
    else app_id ^ "." ^ class_name in
  if not (String.starts_with ~prefix:(app_id ^ ".") class_name) then
    fail "Android manifest URL handler is outside the selected app";
  app_id ^ "/" ^ class_name

let max_manifest_handlers = 128
let max_manifest_bytes = 1_048_576


type manifest_filter = {
  activity : string;
  exported : bool;
  mutable actions : string list;
  mutable categories : string list;
  mutable schemes : string list;
  mutable hosts : string list;
  mutable paths : string list;
  mutable unsupported_path_match : bool;
}

let xml_name_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | ':' | '_' | '-' | '.' -> true
  | _ -> false

let parse_manifest_tag text =
  let length = String.length text and cursor = ref 0 in
  let skip_space () = while !cursor < length &&
    List.mem text.[!cursor] [' '; '\t'; '\r'; '\n'] do incr cursor done in
  skip_space ();
  let closing = !cursor < length && text.[!cursor] = '/' in
  if closing then incr cursor;
  skip_space ();
  let start = !cursor in
  while !cursor < length && xml_name_char text.[!cursor] do incr cursor done;
  if start = !cursor then fail "Android manifest contains a malformed XML tag";
  let name = String.sub text start (!cursor - start) in
  let attrs = ref [] and self_closing = ref false in
  let rec attributes () =
    skip_space ();
    if !cursor = length then ()
    else if text.[!cursor] = '/' then (
      incr cursor; skip_space ();
      if !cursor <> length || closing then fail "malformed Android manifest empty tag";
      self_closing := true)
    else if closing then fail "Android manifest closing tag has attributes"
    else begin
      if List.length !attrs >= 64 then
        fail "Android manifest tag contains too many attributes";
      let key_start = !cursor in
      while !cursor < length && xml_name_char text.[!cursor] do incr cursor done;
      if key_start = !cursor then fail "Android manifest contains a malformed attribute";
      let key = String.sub text key_start (!cursor - key_start) in
      skip_space ();
      if !cursor = length || text.[!cursor] <> '=' then
        fail "Android manifest attribute is missing its value";
      incr cursor; skip_space ();
      if !cursor = length || (text.[!cursor] <> '"' && text.[!cursor] <> '\'') then
        fail "Android manifest attribute is not quoted";
      let quote = text.[!cursor] in
      incr cursor;
      let value_start = !cursor in
      while !cursor < length && text.[!cursor] <> quote do incr cursor done;
      if !cursor = length then fail "Android manifest attribute is unterminated";
      let value = String.sub text value_start (!cursor - value_start) in
      if String.length value > 4096 then
        fail "Android manifest attribute exceeds its size limit";
      incr cursor;
      if List.mem_assoc key !attrs then fail "Android manifest has a duplicate attribute";
      attrs := (key, value) :: !attrs;
      attributes ()
    end in
attributes ();
if closing && !self_closing then fail "invalid Android manifest closing tag";
name, closing, !self_closing, !attrs
let manifest_handlers ~app_id output =
  if output = "" || String.length output > max_manifest_bytes ||
     String.contains output '\000' then
    fail "Android manifest output is empty or exceeds its size limit";
  let package = ref None and root_seen = ref false and root_closed = ref false
  and application_seen = ref false and in_application = ref false
  and activity = ref None and activity_exported = ref false
  and filter = ref None and stack = ref [] and handlers = ref [] and cursor = ref 0 in
  let attribute name attrs = List.assoc_opt name attrs in
  let add values value = if List.mem value values then values else value :: values in
  let finish_filter filter =
    if filter.exported &&
       List.mem "android.intent.action.VIEW" filter.actions &&
       List.mem "android.intent.category.BROWSABLE" filter.categories &&
       List.mem "android.intent.category.DEFAULT" filter.categories &&
       not filter.unsupported_path_match then
      match List.rev filter.schemes, List.rev filter.hosts, List.rev filter.paths with
      | [scheme], [host], [path] ->
          let handler = {
            app_id; activity = filter.activity;
            scheme = valid_scheme scheme;
            host = normalize_host host;
            path = valid_path path;
          } in
          if not (List.mem handler !handlers) then (
            if List.length !handlers >= max_manifest_handlers then
              fail "Android manifest contains too many exact URL handlers";
            handlers := handler :: !handlers)
      | _ -> () in
  let valid_filter_parent () =
    !filter = None &&
    match !stack with
    | ("activity" | "activity-alias") :: "application" :: "manifest" :: []
      when !activity <> None -> true
    | _ -> false in
  let valid_filter_child () =
    !filter <> None &&
    match !stack with
    | "intent-filter" :: ("activity" | "activity-alias") :: "application" :: "manifest" :: [] -> true
    | _ -> false in
  let cleanup tag =
    if tag = "intent-filter" then (
      Option.iter finish_filter !filter;
      filter := None);
    if tag = "activity" || tag = "activity-alias" then (
      activity := None;
      activity_exported := false);
    if tag = "application" then in_application := false;
    if tag = "manifest" then root_closed := true in
  let close_tag tag =
    match !stack with
    | current :: rest when current = tag ->
        if (tag = "activity" || tag = "activity-alias") &&
           (!filter <> None || !activity = None) then
          fail "Android manifest activity scope is malformed";
        if tag = "application" && (!activity <> None || !filter <> None) then
          fail "Android manifest application scope is malformed";
        if tag = "manifest" &&
           (!in_application || !activity <> None || !filter <> None) then
          fail "Android manifest root closed before its children";
        stack := rest;
        cleanup tag
    | _ -> fail "Android manifest tags are malformed or mismatched" in
  let start_tag tag self_closing attrs =
    if !root_closed then fail "Android manifest contains content after its root";
    if tag = "manifest" then (
      if !root_seen || !stack <> [] then fail "Android manifest has multiple roots";
      root_seen := true;
      package := attribute "package" attrs)
    else if not !root_seen || !stack = [] then
      fail "Android manifest element is outside its root"
    else if tag = "application" then (
      if !stack <> ["manifest"] || !in_application || !application_seen then
        fail "Android manifest application is outside its root or duplicated";
      application_seen := true;
      in_application := true)
    else if tag = "activity" || tag = "activity-alias" then (
      if !stack <> ["application"; "manifest"] || not !in_application ||
         !activity <> None then
        fail "Android manifest activity is outside its application";
      let name = match attribute "android:name" attrs with
        | Some value -> value | None -> fail "Android manifest activity has no name" in
      let component = manifest_component app_id name in
      if tag = "activity-alias" then
        (match attribute "android:targetActivity" attrs with
         | Some target -> ignore (manifest_component app_id target)
         | None -> fail "Android manifest activity alias has no target");
      activity := Some component;
      activity_exported := attribute "android:exported" attrs = Some "true")
    else if tag = "intent-filter" then (
      if not (valid_filter_parent ()) then
        fail "Android manifest intent filter is outside an activity";
      filter := Some {activity = Option.get !activity;
        exported = !activity_exported; actions=[]; categories=[];
        schemes=[]; hosts=[]; paths=[]; unsupported_path_match=false})
    else if List.mem tag ["action"; "category"; "data"] then (
      if not (valid_filter_child ()) then
        fail "Android manifest intent-filter member is misplaced";
      let active = Option.get !filter in
      if tag = "action" then
        Option.iter (fun value -> active.actions <- add active.actions value)
          (attribute "android:name" attrs)
      else if tag = "category" then
        Option.iter (fun value -> active.categories <- add active.categories value)
          (attribute "android:name" attrs)
      else (
        Option.iter (fun value -> active.schemes <- add active.schemes value)
          (attribute "android:scheme" attrs);
        Option.iter (fun value -> active.hosts <- add active.hosts value)
          (attribute "android:host" attrs);
        Option.iter (fun value -> active.paths <- add active.paths value)
          (attribute "android:path" attrs);
        if List.exists (fun (key, _) ->
            List.mem key ["android:pathPrefix"; "android:pathPattern";
              "android:pathAdvancedPattern"; "android:pathSuffix"])
            attrs then active.unsupported_path_match <- true));
    if self_closing then cleanup tag else stack := tag :: !stack in
  let find_tag_end start =
    let quote = ref None and index = ref start in
    while !index < String.length output &&
      (match !quote with
       | Some q -> if output.[!index] = q then quote := None; true
       | None ->
           if output.[!index] = '"' || output.[!index] = '\'' then
             (quote := Some output.[!index]; true)
           else output.[!index] <> '>') do
      incr index
    done;
    if !index = String.length output then
      fail "Android manifest contains an unterminated XML tag";
    !index in
  let find_comment_end start =
    let index = ref start in
    while !index + 3 <= String.length output &&
      not (output.[!index] = '-' && output.[!index + 1] = '-' &&
           output.[!index + 2] = '>') do
      incr index
    done;
    if !index + 3 > String.length output then
      fail "Android manifest contains an unterminated XML comment";
    !index in
  while !cursor < String.length output do
    match String.index_from_opt output !cursor '<' with
    | None -> cursor := String.length output
    | Some opening ->
        if opening + 4 <= String.length output &&
           String.sub output opening 4 = "<!--" then (
          let ending = find_comment_end (opening + 4) in
          cursor := ending + 3)
        else if opening + 2 <= String.length output &&
                String.sub output opening 2 = "<?" then (
          match String.index_from_opt output (opening + 2) '>' with
          | Some ending -> cursor := ending + 1
          | None -> fail "Android manifest contains an unterminated XML declaration")
        else if opening + 2 <= String.length output &&
                String.sub output opening 2 = "<!" then
          fail "Android manifest uses an unsupported declaration"
        else begin
          let ending = find_tag_end (opening + 1) in
          let text = String.sub output (opening + 1) (ending - opening - 1) in
          let tag, closing, self_closing, attrs = parse_manifest_tag text in
          if closing then close_tag tag else start_tag tag self_closing attrs;
          cursor := ending + 1
        end
  done;
  if not !root_seen || not !root_closed || not !application_seen ||
     !stack <> [] || !filter <> None || !in_application || !activity <> None then
    fail "Android manifest is incomplete";
  if !package <> Some app_id then
    fail "built APK manifest package does not match the selected app";
  List.sort (fun (left : handler) (right : handler) ->
    compare (left.activity, left.scheme, left.host, left.path)
      (right.activity, right.scheme, right.host, right.path)) !handlers
(* [approved_evidence] is backed by exact APK manifest inspection. A URL is not
   evidence merely because the selected session declares a scheme. *)
let deep_link_preview ~approved_evidence (session : Workspace_mobile_run.session)
    ~handler ~url =
  if not approved_evidence then fail "approved project URL-handler evidence is required";
  if session.platform <> Workspace_mobile_run.Android then
    fail "selected-app deep links are unavailable on iOS because simctl URL opening delegates to the system handler";
  if session.state <> Workspace_mobile_run.Running then
    fail "deep-link preview requires the selected app to be running";
  let link = parse_url url in
  if handler.app_id <> session.app_id then fail "URL handler evidence belongs to a different app";
  if valid_scheme handler.scheme <> link.scheme ||
     normalize_host handler.host <> link.host || valid_path handler.path <> link.path then
    fail "deep link does not exactly match approved app URL-handler evidence";
  let activity = handler.activity in
  check_token "selected-app handler activity" activity 256
    (function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' | '$' | '/' -> true | _ -> false);
  if not (String.starts_with ~prefix:(session.app_id ^ "/") activity) then
    fail "URL handler activity is outside the selected app";
  let command = "adb -s " ^ Filename.quote session.device ^ " shell " ^
    Filename.quote ("am start -W -n " ^ activity ^
      " -a android.intent.action.VIEW -d " ^ Filename.quote link.url) in
  command, link

let create_record ~name ~identity =
  check_token "mobile lifecycle scenario name" name 48 (function
    | 'a'..'z' | '0'..'9' | '_' | '-' -> true | _ -> false);
  { name; identity; state = Foreground; generation = -1; process_id = None }

let state_name = function
  | Foreground -> "foreground" | Backgrounded -> "backgrounded"
  | Process_recreated -> "process_recreated" | Failed _ -> "failed"

let data_loss_description = function
  | Background -> "Background/resume may lose volatile UI state if the OS reclaims the process; app data is not cleared."
  | Resume -> "Resume may reveal OS-reclaimed volatile UI state; app data is not cleared."
  | Recreate_process -> "Process recreation loses in-memory UI/process state; persistent app data is not cleared."

let commands (record : record) ~activity = function
  | Background when record.identity.platform = Workspace_mobile_run.Android ->
      ["adb -s " ^ Filename.quote record.identity.device ^ " shell input keyevent KEYCODE_HOME"]
  | Resume when record.identity.platform = Workspace_mobile_run.Android ->
      ["adb -s " ^ Filename.quote record.identity.device ^
       " shell am start -W -n " ^ activity]
  | Recreate_process when record.identity.platform = Workspace_mobile_run.Android ->
      ["adb -s " ^ Filename.quote record.identity.device ^ " shell am force-stop " ^
       Filename.quote record.identity.app_id;
       "adb -s " ^ Filename.quote record.identity.device ^
       " shell am start -W -n " ^ activity]
  | _ -> fail "this lifecycle transition is unsupported on the selected platform"

let expected_state = function
  | Background -> Backgrounded | Resume -> Foreground
  | Recreate_process -> Process_recreated

let prepare_transition (record : record) ~approved ~observation ~activity transition =
  if not approved then fail "mobile lifecycle transition requires exact explicit approval";
  if not (same_identity record.identity observation.identity) then
    fail "fresh observation does not match the recorded build/app/device identity";
  if observation.generation <= record.generation then
    fail "lifecycle transition requires a fresh observation newer than the last recorded observation";
  let state_matches = observation.state = record.state ||
    (record.state = Process_recreated && observation.state = Foreground &&
     record.process_id = observation.process_id) in
  if not state_matches then fail "fresh observation does not match the recorded lifecycle state";
  if observation.process_id = None then
    fail "lifecycle transition requires the selected app process to be running";
  let allowed = match transition, record.state with
    | Background, (Foreground | Process_recreated) | Resume, Backgrounded
    | Recreate_process, (Foreground | Backgrounded | Process_recreated) -> true
    | _ -> false in
  if not allowed then fail "lifecycle transition is invalid for the observed selected-app state";
  check_token "selected-app activity" activity 256 (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' | '$' | '/' -> true | _ -> false);
  if not (String.starts_with ~prefix:(record.identity.app_id ^ "/") activity) then
    fail "lifecycle activity must belong to the selected app";
  commands record ~activity transition

let run_transition (record : record) ~approved ~observation ~activity ~run ~observe transition =
  let commands = prepare_transition record ~approved ~observation ~activity transition in
  try
    List.iter (fun command -> run command) commands;
    let observed = observe () in
    let state = match transition, observation.process_id, observed.process_id, observed.state with
      | Recreate_process, Some before, Some after, Foreground when before <> after ->
          Process_recreated
      | Recreate_process, _, _, _ ->
          fail "process recreation did not produce a new selected-app process ID"
      | _, _, _, state -> state in
    let after = { observed with state } in
    if not (same_identity record.identity after.identity) ||
       after.generation <= observation.generation ||
       after.state <> expected_state transition then
      fail "post-transition observation did not verify the selected app's expected state and identity";
    record.state <- after.state;
    record.generation <- after.generation;
    record.process_id <- after.process_id;
    after
  with exn ->
    let message = Printexc.to_string exn in
    record.state <- Failed
      (if String.length message <= 1024 then message else String.sub message 0 1024);
    raise exn

let max_record_bytes = 16_384
let store_lock = Mutex.create ()

let state_json = function
  | Foreground -> `Assoc ["status", `String "foreground"]
  | Backgrounded -> `Assoc ["status", `String "backgrounded"]
  | Process_recreated -> `Assoc ["status", `String "process_recreated"]
  | Failed message -> `Assoc ["status", `String "failed"; "error", `String message]

let platform_json = function
  | Workspace_mobile_run.Android -> `String "android"
  | Workspace_mobile_run.Ios -> `String "ios"

let record_json record = `Assoc [
  "version", `Int 2;
  "name", `String record.name;
  "identity", `Assoc [
    "platform", platform_json record.identity.platform;
    "device", `String record.identity.device;
    "app_id", `String record.identity.app_id;
    "app_path", `String record.identity.app_path;
    "build_id", `String record.identity.build_id];
  "state", state_json record.state;
  "generation", `Int record.generation;
  "process_id", (match record.process_id with None -> `Null | Some pid -> `Int pid)]

let member label json = match json with
  | `Assoc fields -> (match List.assoc_opt label fields with
      | Some value -> value | None -> fail ("lifecycle record is missing " ^ label))
  | _ -> fail "lifecycle record must be a JSON object"

let json_string label max_length = function
  | `String value when value <> "" && String.length value <= max_length -> value
  | _ -> fail ("invalid lifecycle record " ^ label)

let record_of_json expected_name text =
  if String.length text > max_record_bytes then fail "mobile lifecycle record exceeds its size limit";
  let json = try Yojson.Basic.from_string text with Yojson.Json_error _ ->
    fail "mobile lifecycle record is malformed JSON" in
  (match member "version" json with `Int 2 -> () | _ -> fail "unsupported mobile lifecycle record version");
  let name = json_string "name" 48 (member "name" json) in
  if name <> expected_name then fail "mobile lifecycle record name does not match its path";
  check_token "mobile lifecycle scenario name" name 48 (function
    | 'a'..'z' | '0'..'9' | '_' | '-' -> true | _ -> false);
  let encoded_identity = member "identity" json in
  let platform = match member "platform" encoded_identity with
    | `String "android" -> Workspace_mobile_run.Android
    | `String "ios" -> Workspace_mobile_run.Ios
    | _ -> fail "invalid lifecycle record platform" in
  let identity = {
    platform;
    device = json_string "device" 256 (member "device" encoded_identity);
    app_id = json_string "app_id" 256 (member "app_id" encoded_identity);
    app_path = json_string "app_path" 4096 (member "app_path" encoded_identity);
    build_id = json_string "build_id" 256 (member "build_id" encoded_identity);
  } in
  let encoded_state = member "state" json in
  let state = match member "status" encoded_state with
    | `String "foreground" -> Foreground
    | `String "backgrounded" -> Backgrounded
    | `String "process_recreated" -> Process_recreated
    | `String "failed" -> Failed (json_string "error" 1024 (member "error" encoded_state))
    | _ -> fail "invalid lifecycle record state" in
  let generation = match member "generation" json with
    | `Int value when value >= -1 -> value
    | _ -> fail "invalid lifecycle observation generation" in
  let process_id = match member "process_id" json with
    | `Null -> None
    | `Int pid when pid > 0 && pid <= 4_194_304 -> Some pid
    | _ -> fail "invalid lifecycle record process ID" in
  { name; identity; state; generation; process_id }

let scenario_directory ~root =
  let root = Workspace_path.root_path root in
  let pave = Filename.concat root ".pave" in
  (try Unix.mkdir pave 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let check_dir path =
    let stat = Unix.lstat path in
    if stat.Unix.st_kind <> Unix.S_DIR || stat.Unix.st_uid <> Unix.geteuid () ||
       stat.Unix.st_perm land 0o077 <> 0 then
      fail "mobile lifecycle scenario directory must be caller-owned and private" in
  check_dir pave;
  let directory = Filename.concat pave "mobile-app-lifecycle" in
  (try Unix.mkdir directory 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  check_dir directory;
  directory
let existing_scenario_directory ~root =
  let root = Workspace_path.root_path root in
  let pave = Filename.concat root ".pave" in
  let directory = Filename.concat pave "mobile-app-lifecycle" in
  let check path =
    let stat = Unix.lstat path in
    if stat.Unix.st_kind <> Unix.S_DIR || stat.Unix.st_uid <> Unix.geteuid () ||
       stat.Unix.st_perm land 0o077 <> 0 then
      fail "mobile lifecycle scenario directory must be caller-owned and private" in
  try check pave; check directory; Some directory
  with Unix.Unix_error (Unix.ENOENT, _, _) -> None


let scenario_path directory name = Filename.concat directory (name ^ ".json")

let checked_record_file path =
  match Unix.lstat path with
  | stat when stat.Unix.st_kind = Unix.S_REG && stat.Unix.st_uid = Unix.geteuid () &&
      stat.Unix.st_nlink = 1 && stat.Unix.st_perm land 0o077 = 0 &&
      stat.Unix.st_size <= max_record_bytes -> ()
  | _ -> fail "unsafe mobile lifecycle scenario record"
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> fail "mobile lifecycle scenario not found"

let save ~root record = Mutex.protect store_lock (fun () ->
  check_token "mobile lifecycle scenario name" record.name 48 (function
    | 'a'..'z' | '0'..'9' | '_' | '-' -> true | _ -> false);
  let directory = scenario_directory ~root in
  let path = scenario_path directory record.name in
  (match Unix.lstat path with
   | _ -> fail ("mobile lifecycle scenario already exists: " ^ record.name)
   | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
   | exception Unix.Unix_error (error, operation, _) ->
       fail (operation ^ ": " ^ Unix.error_message error));
  let text = Yojson.Basic.to_string (record_json record) in
  if String.length text > max_record_bytes then fail "mobile lifecycle record exceeds its size limit";
  Workspace_path.atomic_write path text;
  checked_record_file path)

let load ~root name = Mutex.protect store_lock (fun () ->
  check_token "mobile lifecycle scenario name" name 48 (function
    | 'a'..'z' | '0'..'9' | '_' | '-' -> true | _ -> false);
  let directory = match existing_scenario_directory ~root with
    | Some directory -> directory | None -> fail "mobile lifecycle scenario not found" in
  let path = scenario_path directory name in
  checked_record_file path;
  record_of_json name (Workspace_path.read_bounded path max_record_bytes))

let update ~root record = Mutex.protect store_lock (fun () ->
  check_token "mobile lifecycle scenario name" record.name 48 (function
    | 'a'..'z' | '0'..'9' | '_' | '-' -> true | _ -> false);
  let directory = match existing_scenario_directory ~root with
    | Some directory -> directory | None -> fail "mobile lifecycle scenario not found" in
  let path = scenario_path directory record.name in
  checked_record_file path;
  let stored = record_of_json record.name
    (Workspace_path.read_bounded path max_record_bytes) in
  if not (same_identity stored.identity record.identity) then
    fail "mobile lifecycle scenario identity is immutable";
  if record.generation < stored.generation ||
     (record.generation = stored.generation &&
      (match record.state with Failed _ -> false | _ -> true)) then
    fail "mobile lifecycle scenario changed since its observation";
  let text = Yojson.Basic.to_string (record_json record) in
  if String.length text > max_record_bytes then fail "mobile lifecycle record exceeds its size limit";
  Workspace_path.atomic_write path text;
  checked_record_file path)

let list ~root = Mutex.protect store_lock (fun () ->
  match existing_scenario_directory ~root with
  | None -> []
  | Some directory ->
      let entries = Sys.readdir directory |> Array.to_list in
      if List.length entries > 100 then
        fail "too many mobile lifecycle scenarios";
      List.map (fun file ->
        if not (Filename.check_suffix file ".json") then
          fail "mobile lifecycle store contains an unexpected entry";
        let name = String.sub file 0 (String.length file - 5) in
        check_token "mobile lifecycle scenario name" name 48 (function
          | 'a'..'z' | '0'..'9' | '_' | '-' -> true | _ -> false);
        let path = Filename.concat directory file in
        checked_record_file path;
        record_of_json name (Workspace_path.read_bounded path max_record_bytes))
        entries |> List.sort (fun left right -> String.compare left.name right.name))

let delete ~root name = Mutex.protect store_lock (fun () ->
  check_token "mobile lifecycle scenario name" name 48 (function
    | 'a'..'z' | '0'..'9' | '_' | '-' -> true | _ -> false);
  let directory = match existing_scenario_directory ~root with
    | Some directory -> directory | None -> fail "mobile lifecycle scenario not found" in
  let path = scenario_path directory name in
  checked_record_file path;
  Unix.unlink path)
