type assertion_field = Role | Text | Description | Identifier

type step = {
  action : Workspace_mobile_control.action;
  assertion_field : assertion_field;
  expected : string;
}

type identity = {
  platform : string;
  device : string;
  app_id : string;
  app_path : string;
  root : string;
  subroot : string;
  build_hash : string;
  scheme : string option;
  variant : string option;
}

type phase = Idle | Running of int | Awaiting of int | Failed of int * string | Complete

type record = {
  name : string;
  identity : identity;
  steps : step list;
  mutable phase : phase;
}

exception Error of string
let fail message = raise (Error message)
let max_steps = 50
let max_record_bytes = 65_536
let max_tree_bytes = 1_048_576
let max_nodes = 10_000
let guard = Mutex.create ()

let field name = function
  | `Assoc fields -> (match List.assoc_opt name fields with
      | Some value -> value | None -> fail ("missing scenario field " ^ name))
  | _ -> fail "mobile scenario value must be an object"
let string label maximum = function
  | `String value when String.length value <= maximum -> value
  | `String _ -> fail (label ^ " exceeds its size limit")
  | _ -> fail (label ^ " must be a string")
let integer label minimum maximum = function
  | `Int value when value >= minimum && value <= maximum -> value
  | _ -> fail (Printf.sprintf "%s must be between %d and %d" label minimum maximum)
let optional_string label maximum = function
  | `Null -> None
  | value -> Some (string label maximum value)

let assertion_field = function
  | "role" -> Role | "text" -> Text | "description" -> Description
  | "identifier" -> Identifier
  | _ -> fail "scenario assertion field must be role, text, description or identifier"

let step_of_json json =
  let action_name = string "scenario action" 16 (field "action" json) in
  let int key maximum = integer ("scenario " ^ key) 0 maximum (field key json) in
  let action = match action_name with
    | "tap" -> Workspace_mobile_control.Tap {
        x = int "x" max_int; y = int "y" max_int }
    | "swipe" -> Workspace_mobile_control.Swipe {
        x1 = int "x1" max_int; y1 = int "y1" max_int;
        x2 = int "x2" max_int; y2 = int "y2" max_int;
        duration_ms = (match List.assoc_opt "duration_ms"
            (match json with `Assoc fields -> fields | _ -> []) with
          | None -> 500 | Some value -> integer "scenario duration_ms" 1 10_000 value) }
    | "text" -> Workspace_mobile_control.Text
        (string "scenario text" 512 (field "text" json))
    | "back" -> Workspace_mobile_control.Back
    | _ -> fail "scenario step action must be tap, swipe, text or back" in
  let assertion_field = assertion_field (string "scenario assertion field" 16
      (field "expected_field" json)) in
  let expected = string "scenario assertion value" 1024
      (field "expected_value" json) in
  if expected = "" then fail "scenario assertion value cannot be empty";
  { action; assertion_field; expected }

let identity_of_session (session : Workspace_mobile_run.session) =
  let build_hash =
    try (Workspace_mobile_report.build_identity session.root session).build_hash
    with Workspace_mobile_report.Error message -> fail message in
  { platform = Workspace_mobile_run.platform_name session.platform;
    device = session.device; app_id = session.app_id; app_path = session.app_path;
    root = session.root; subroot = session.subroot; build_hash;
    scheme = session.scheme; variant = session.variant }

let same_identity left right =
  left.platform = right.platform && left.device = right.device &&
  left.app_id = right.app_id && left.app_path = right.app_path &&
  left.scheme = right.scheme && left.variant = right.variant &&
  left.root = right.root && left.subroot = right.subroot &&
  left.build_hash = right.build_hash

let identity_json identity = `Assoc [
  "platform", `String identity.platform;
  "device", `String identity.device;
  "app_id", `String identity.app_id;
  "app_path", `String identity.app_path;
  "root", `String identity.root;
  "subroot", `String identity.subroot;
  "build_hash", `String identity.build_hash;
  "scheme", (match identity.scheme with None -> `Null | Some value -> `String value);
  "variant", (match identity.variant with None -> `Null | Some value -> `String value);
]

let action_json = function
  | Workspace_mobile_control.Tap { x; y } -> `Assoc [
      "action", `String "tap"; "x", `Int x; "y", `Int y]
  | Workspace_mobile_control.Swipe { x1; y1; x2; y2; duration_ms } -> `Assoc [
      "action", `String "swipe"; "x1", `Int x1; "y1", `Int y1;
      "x2", `Int x2; "y2", `Int y2; "duration_ms", `Int duration_ms]
  | Workspace_mobile_control.Text text -> `Assoc [
      "action", `String "text"; "text", `String text]
  | Workspace_mobile_control.Back -> `Assoc ["action", `String "back"]

let assertion_field_name = function
  | Role -> "role" | Text -> "text" | Description -> "description"
  | Identifier -> "identifier"

let step_json step = match action_json step.action with
  | `Assoc fields -> `Assoc (fields @ [
      "expected_field", `String (assertion_field_name step.assertion_field);
      "expected_value", `String step.expected])
  | _ -> assert false

let phase_json = function
  | Idle -> `Assoc ["status", `String "idle"]
  | Running index -> `Assoc ["status", `String "running"; "index", `Int index]
  | Awaiting index -> `Assoc ["status", `String "awaiting_assertion"; "index", `Int index]
  | Failed (index, message) -> `Assoc ["status", `String "failed";
      "index", `Int index; "error", `String message]
  | Complete -> `Assoc ["status", `String "complete"]

let to_json record = `Assoc [
  "version", `Int 2;
  "name", `String record.name;
  "identity", identity_json record.identity;
  "steps", `List (List.map step_json record.steps);
  "replay", phase_json record.phase;
]

let option_string_json name json = optional_string name 512 (field name json)

let identity_of_json json = {
  platform = string "scenario platform" 16 (field "platform" json);
  device = string "scenario device" 256 (field "device" json);
  app_id = string "scenario app_id" 256 (field "app_id" json);
  app_path = string "scenario app_path" 4096 (field "app_path" json);
  root = string "scenario root" 4096 (field "root" json);
  subroot = string "scenario subroot" 4096 (field "subroot" json);
  build_hash = string "scenario build_hash" 64 (field "build_hash" json);
  scheme = option_string_json "scheme" json;
  variant = option_string_json "variant" json;
}

let phase_of_json json =
  let status = string "scenario replay status" 32 (field "status" json) in
  let index () = integer "scenario replay index" 0 max_steps (field "index" json) in
  match status with
  | "idle" -> Idle
  | "running" -> Running (index ())
  | "awaiting_assertion" -> Awaiting (index ())
  | "failed" -> Failed (index (), string "scenario replay error" 1024 (field "error" json))
  | "complete" -> Complete
  | _ -> fail "unknown mobile scenario replay status"

let from_json name text =
  if String.length text > max_record_bytes then fail "mobile scenario record exceeds its size limit";
  let json = try Yojson.Basic.from_string text
    with Yojson.Json_error _ -> fail "mobile scenario record is malformed JSON" in
  let version = integer "scenario version (re-save legacy records with exact build identity)" 2 2 (field "version" json) in
  ignore version;
  let stored_name = string "scenario name" 48 (field "name" json) in
  if stored_name <> name then fail "mobile scenario record name does not match its path";
  let identity = identity_of_json (field "identity" json) in
  let steps = match field "steps" json with
    | `List values when List.length values >= 1 && List.length values <= max_steps ->
        List.map step_of_json values
    | `List _ -> fail (Printf.sprintf "scenario must contain 1..%d steps" max_steps)
    | _ -> fail "scenario steps must be an array" in
  let phase = phase_of_json (field "replay" json) in
  let index = match phase with Running index | Awaiting index | Failed (index, _) -> Some index
    | Idle | Complete -> None in
  (match index with
   | Some value when value >= List.length steps -> fail "scenario replay index is outside its steps"
   | _ -> ());
  { name = stored_name; identity; steps; phase }

let check_name name =
  if not (Local_content.name_ok name) then
    fail "scenario name must match [a-z][a-z0-9_-]{0,47}"

let private_dir ~root =
  let check path =
    let stat = try Unix.lstat path with Unix.Unix_error (error, operation, _) ->
      fail (operation ^ ": " ^ Unix.error_message error) in
    if stat.Unix.st_kind <> Unix.S_DIR || stat.Unix.st_uid <> Unix.geteuid () ||
       stat.Unix.st_perm land 0o077 <> 0 then
      fail (path ^ ": scenario directory must be a caller-owned private directory") in
  let root = Workspace_path.root_path root in
  let pave = Filename.concat root ".pave" in
  (try Unix.mkdir pave 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  check pave;
  let directory = Filename.concat pave "mobile-scenarios" in
  (try Unix.mkdir directory 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  check directory;
  pave, directory

let scenario_path directory name = Filename.concat directory (name ^ ".json")

let check_file path =
  match Unix.lstat path with
  | stat when stat.Unix.st_kind = Unix.S_REG && stat.Unix.st_uid = Unix.geteuid () &&
      stat.Unix.st_nlink = 1 && stat.Unix.st_perm land 0o077 = 0 &&
      stat.Unix.st_size <= max_record_bytes -> stat
  | _ -> fail (path ^ ": unsafe mobile scenario file")
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> fail "mobile scenario not found"
  | exception Unix.Unix_error (error, operation, _) ->
      fail (operation ^ ": " ^ Unix.error_message error)

let with_store ~root action = Mutex.protect guard (fun () ->
  let pave, directory = private_dir ~root in
  let lock_path = Filename.concat pave "mobile-scenarios.lock" in
  let before = try Some (Unix.lstat lock_path)
    with Unix.Unix_error (Unix.ENOENT, _, _) -> None in
  (match before with
   | Some stat when stat.Unix.st_kind = Unix.S_REG &&
                    stat.Unix.st_uid = Unix.geteuid () &&
                    stat.Unix.st_nlink = 1 &&
                    stat.Unix.st_perm land 0o077 = 0 -> ()
   | Some _ -> fail "unsafe mobile scenario lock"
   | None -> ());
  let fd = Unix.openfile lock_path
    [Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC; Unix.O_NONBLOCK] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let opened = Unix.fstat fd in
    let current = Unix.lstat lock_path in
    if opened.Unix.st_kind <> Unix.S_REG ||
       opened.Unix.st_uid <> Unix.geteuid () || opened.Unix.st_nlink <> 1 ||
       opened.Unix.st_perm land 0o077 <> 0 ||
       opened.Unix.st_ino <> current.Unix.st_ino ||
       opened.Unix.st_dev <> current.Unix.st_dev ||
       current.Unix.st_kind <> Unix.S_REG then
      fail "unsafe mobile scenario lock";
    Unix.fchmod fd 0o600;
    Unix.lockf fd Unix.F_LOCK 0;
    Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0)
      (fun () -> action directory)))

let load_path directory name =
  check_name name;
  let path = scenario_path directory name in
  let before = check_file path in
  let contents = Workspace_path.read_bounded path max_record_bytes in
  let after = check_file path in
  if before.Unix.st_ino <> after.Unix.st_ino || before.Unix.st_dev <> after.Unix.st_dev then
    fail "mobile scenario file changed while reading";
  from_json name contents

let load ~root name = with_store ~root (fun directory -> load_path directory name)

let write_path directory record =
  let path = scenario_path directory record.name in
  (try ignore (check_file path) with Error "mobile scenario not found" -> ());
  let text = Yojson.Basic.to_string (to_json record) in
  if String.length text > max_record_bytes then fail "mobile scenario record exceeds its size limit";
  Workspace_path.atomic_write path text;
  let stat = check_file path in
  if stat.Unix.st_perm land 0o077 <> 0 then fail "mobile scenario file is not private"

let save ~root ~name ~session steps =
  check_name name;
  if session.Workspace_mobile_run.state <> Workspace_mobile_run.Running then
    fail "mobile scenario save requires a running app session";
  if session.platform <> Workspace_mobile_run.Android then
    fail "mobile scenarios currently require an Android session";
  let root = try Workspace_path.root_path root with Workspace_path.Error message -> fail message in
  if session.root <> root then fail "mobile scenario session belongs to another workspace";
  if List.length steps < 1 || List.length steps > max_steps then
    fail (Printf.sprintf "mobile scenarios require 1..%d steps" max_steps);
  List.iter (fun step ->
    ignore (Workspace_mobile_control.command session
      ~screen_size:session.screen_size step.action)) steps;
  let record = { name; identity = identity_of_session session; steps; phase = Idle } in
  with_store ~root (fun directory ->
    (try ignore (check_file (scenario_path directory name));
         fail ("mobile scenario already exists: " ^ name)
     with Error "mobile scenario not found" -> ());
    write_path directory record);
  record

let list ~root = with_store ~root (fun directory ->
  let entries = Sys.readdir directory |> Array.to_list
    |> List.filter (fun entry -> Filename.extension entry = ".json") in
  if List.length entries > 256 then fail "mobile scenario store exceeds its record limit";
  List.sort String.compare entries |> List.map (fun entry ->
    let name = Filename.remove_extension entry in
    load_path directory name))

let update ~root record = with_store ~root (fun directory ->
  ignore (load_path directory record.name);
  write_path directory record)

let delete ~root name = with_store ~root (fun directory ->
  check_name name;
  let path = scenario_path directory name in
  ignore (check_file path);
  Unix.unlink path)

let reset record =
  if (match record.phase with Running _ | Awaiting _ -> true | _ -> false) then
    fail "mobile scenario replay is already active; verify its pending step or start a new explicit replay after it stops";
  record.phase <- Running 0

let current_step record = match record.phase with
  | Running index -> List.nth record.steps index
  | Awaiting _ -> fail "verify the preceding mobile scenario step before advancing"
  | Idle -> fail "start an explicit mobile scenario replay before executing a step"
  | Failed _ -> fail "mobile scenario replay is failed; start a new explicit replay to retry"
  | Complete -> fail "mobile scenario replay is already complete"

let mark_awaiting record = match record.phase with
  | Running index -> record.phase <- Awaiting index
  | _ -> fail "mobile scenario step is not ready to execute"

let mark_failed record message = match record.phase with
  | Running index | Awaiting index -> record.phase <- Failed (index, message)
  | _ -> fail "mobile scenario has no active step to fail"

let node_field node field =
  match node with
  | `Assoc fields -> (match List.assoc_opt field fields with
      | Some (`String value) -> value
      | _ -> fail ("accessibility node has no string " ^ field))
  | _ -> fail "accessibility node must be an object"

let verify_tree step tree =
  if String.length tree > max_tree_bytes then fail "accessibility assertion tree exceeds its size limit";
  let json = try Yojson.Basic.from_string tree
    with Yojson.Json_error _ -> fail "accessibility assertion tree is malformed JSON" in
  if field "status" json <> `String "available" then
    fail "accessibility assertion requires an available observed tree";
  let nodes = match field "nodes" json with
    | `List nodes when List.length nodes <= max_nodes -> nodes
    | `List _ -> fail "accessibility assertion tree exceeds its node limit"
    | _ -> fail "accessibility assertion nodes must be an array" in
  let count = integer "accessibility node_count" 1 max_nodes (field "node_count" json) in
  if count <> List.length nodes then fail "accessibility assertion node count does not match the tree";
  let key = assertion_field_name step.assertion_field in
  List.exists (fun node -> node_field node key = step.expected) nodes

let advance record = match record.phase with
  | Awaiting index when index + 1 = List.length record.steps -> record.phase <- Complete
  | Awaiting index -> record.phase <- Running (index + 1)
  | _ -> fail "mobile scenario has no observation awaiting assertion"

let phase_name = function
  | Idle -> "idle" | Running _ -> "running" | Awaiting _ -> "awaiting_assertion"
  | Failed _ -> "failed" | Complete -> "complete"

let phase_detail = function
  | Idle -> "not started"
  | Running index | Awaiting index -> Printf.sprintf "step %d" (index + 1)
  | Failed (index, message) -> Printf.sprintf "step %d failed: %s" (index + 1) message
  | Complete -> "all assertions passed"

let render record =
  Printf.sprintf "%s · %s · %s steps · %s"
    record.name (record.identity.app_id) (string_of_int (List.length record.steps))
    (phase_detail record.phase)
