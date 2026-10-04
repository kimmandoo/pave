exception Error of string

let fail message = raise (Error message)

let max_output_bytes = 16_384
let max_identity_bytes = 256

type action = Boot | Readiness | Shutdown

type approval = {
  marker : string;
  action : action;
  session_id : string;
  inventory_id : string;
  target_id : string;
}

let valid_identity label value =
  if value = "" || String.length value > max_identity_bytes ||
     not (String.for_all (fun ch ->
       let code = Char.code ch in code >= 33 && code < 127) value) then
    fail ("invalid " ^ label)

let approval ~marker ~action ~session_id ~inventory_id ~target_id =
  valid_identity "approval marker" marker;
  valid_identity "mobile session ID" session_id;
  valid_identity "device inventory ID" inventory_id;
  valid_identity "device approval target" target_id;
  { marker; action; session_id; inventory_id; target_id }

let check_approval approval ~action ~session_id ~inventory_id ~target_id =
  valid_identity "approval marker" approval.marker;
  if approval.action <> action || approval.session_id <> session_id ||
     approval.inventory_id <> inventory_id || approval.target_id <> target_id then
    fail "device lifecycle approval does not match this session, inventory, action and exact device"

let bounded_output label output =
  if String.length output > max_output_bytes || String.contains output '\000' then
    fail (label ^ " output is invalid or exceeds its size limit")

let valid_avd name =
  name <> "" && String.length name <= 128 &&
  String.for_all (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '_' | '-' -> true
    | _ -> false) name

let valid_serial serial =
  Workspace_android_devices.emulator_serial serial

let parse_avd_name output =
  bounded_output "Android AVD identity" output;
  let lines = String.split_on_char '\n' output |> List.map (fun line ->
    let length = String.length line in
    if length > 0 && line.[length - 1] = '\r' then
      String.sub line 0 (length - 1) else line) in
  match lines with
  | [name; "OK"; ""] | [name; "OK"] when valid_avd name -> name
  | _ -> fail "adb returned a malformed or ambiguous AVD identity"

let avd_name_command serial =
  if not (valid_serial serial) then fail "AVD identity requires an exact emulator serial";
  "adb -s " ^ Filename.quote serial ^ " emu avd name"

type target =
  | Android_avd of { name : string; port : int }
  | Ios_simulator of { id : string }

let target_id = function
  | Android_avd { name; port } -> Printf.sprintf "android:%s@%d" name port
  | Ios_simulator { id } -> "ios:" ^ id

let target_serial = function
  | Android_avd { port; _ } -> Printf.sprintf "emulator-%04d" port
  | Ios_simulator _ -> fail "iOS simulators do not have Android serials"

type inventory = {
  session_id : string;
  inventory_id : string;
  configured_avds : string list;
  android_devices : Workspace_android_devices.device list;
  avd_bindings : (string * string) list;
  android_bindings_complete : bool;
  configured_simulator_ids : string list;
  ios_destinations : string list;
  compatible_simulators : Workspace_xcode.simulator list;
}

let valid_uuid id = String.length id = 36 &&
  List.for_all (fun offset -> id.[offset] = '-') [8; 13; 18; 23] &&
  String.for_all (function
    | '0'..'9' | 'a'..'f' | 'A'..'F' | '-' -> true | _ -> false) id

let configured_simulator_ids output =
  bounded_output "configured simulator inventory" output;
  let json = try Yojson.Basic.from_string output
    with Yojson.Json_error _ -> fail "simctl returned invalid configured-device JSON" in
  let fields = match json with
    | `Assoc fields when List.length fields =
        List.length (List.sort_uniq String.compare (List.map fst fields)) ->
        (match List.assoc_opt "devices" fields with
         | Some (`Assoc runtimes) when List.length runtimes =
             List.length (List.sort_uniq String.compare (List.map fst runtimes)) ->
             runtimes
         | _ -> fail "simctl returned no unique configured-device map")
    | _ -> fail "simctl returned no unique configured-device map" in
  let runtime_prefix = "com.apple.CoreSimulator.SimRuntime.iOS-" in
  let seen = Hashtbl.create 32 and ids = ref [] and count = ref 0 in
  List.iter (fun (runtime, devices) ->
    if String.starts_with ~prefix:runtime_prefix runtime then (
      let version = String.sub runtime (String.length runtime_prefix)
          (String.length runtime - String.length runtime_prefix) in
      if version = "" || String.length version > 32 ||
         not (String.for_all (function '0'..'9' | '-' -> true | _ -> false) version)
      then fail "simctl returned an invalid configured iOS runtime";
      let rows = match devices with
        | `List rows -> rows
        | _ -> fail "simctl returned an invalid configured-device list" in
      List.iter (fun device ->
        incr count;
        if !count > 100 then fail "simctl returned too many configured iOS devices";
        let fields = match device with
          | `Assoc fields when List.length fields =
              List.length (List.sort_uniq String.compare (List.map fst fields)) -> fields
          | _ -> fail "simctl returned an invalid configured simulator" in
        match List.assoc_opt "isAvailable" fields with
        | Some (`Bool false) -> ()
        | Some (`Bool true) ->
            let id = match List.assoc_opt "udid" fields with
              | Some (`String id) when valid_uuid id -> id
              | _ -> fail "simctl returned a configured simulator without a valid ID" in
            if Hashtbl.mem seen id then
              fail "simctl returned duplicate configured simulator IDs";
            Hashtbl.add seen id ();
            (match List.assoc_opt "state" fields with
             | Some (`String ("Booted" | "Shutdown")) -> ids := id :: !ids
             | _ -> fail "simctl returned an invalid configured simulator state")
        | _ -> fail "simctl returned an invalid configured simulator availability flag")
        rows)) fields;
  List.sort String.compare !ids

let create_inventory ~session_id ~inventory_id ~configured_avds ~android_devices
    ~avd_bindings ~android_bindings_complete ~configured_simulator_ids
    ~ios_destinations ~compatible_simulators =
  valid_identity "mobile session ID" session_id;
  valid_identity "device inventory ID" inventory_id;
  if List.length configured_avds > 100 || List.length android_devices > 100 ||
     List.length avd_bindings > 100 || List.length configured_simulator_ids > 100 ||
     List.length ios_destinations > 100 || List.length compatible_simulators > 100 then
    fail "device inventory exceeds its size limit";
  if not (List.for_all valid_avd configured_avds) ||
     List.length configured_avds <>
       List.length (List.sort_uniq String.compare configured_avds) then
    fail "device inventory has invalid or duplicate configured AVDs";
  let device_serials = List.map (fun (device : Workspace_android_devices.device) ->
    if device.serial = "" || String.length device.serial > 128 ||
       not (String.for_all (function
         | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | ':' | '-' | '_' -> true
         | _ -> false) device.serial) then
      fail "Android device inventory contains an invalid serial";
    device.serial) android_devices in
  if List.length device_serials <>
     List.length (List.sort_uniq String.compare device_serials) then
    fail "Android device inventory contains duplicate serials";
  List.iter (fun (name, serial) ->
    if not (valid_avd name && List.mem name configured_avds &&
            valid_serial serial && List.mem serial device_serials) then
      fail "Android AVD binding is outside the configured current inventory") avd_bindings;
  if List.length (List.map fst avd_bindings) <>
     List.length (List.sort_uniq String.compare (List.map fst avd_bindings)) ||
     List.length (List.map snd avd_bindings) <>
     List.length (List.sort_uniq String.compare (List.map snd avd_bindings)) then
    fail "Android AVD inventory contains duplicate device bindings";
  if android_bindings_complete &&
     List.exists (fun (device : Workspace_android_devices.device) ->
       device.emulator &&
       not (List.exists (fun (_, serial) -> serial = device.serial) avd_bindings))
       android_devices then

    fail "complete Android AVD inventory omitted a running emulator identity";
  if not (List.for_all valid_uuid configured_simulator_ids) ||
     List.length configured_simulator_ids <>
       List.length (List.sort_uniq String.compare configured_simulator_ids) then
    fail "configured iOS simulator inventory has invalid or duplicate IDs";
  if not (List.for_all valid_uuid ios_destinations) ||
     List.length ios_destinations <>
       List.length (List.sort_uniq String.compare ios_destinations) then
    fail "iOS simulator inventory has invalid or duplicate destination IDs";
  if List.length (List.map (fun (simulator : Workspace_xcode.simulator) -> simulator.id)
        compatible_simulators) <>
     List.length (List.sort_uniq String.compare
       (List.map (fun (simulator : Workspace_xcode.simulator) -> simulator.id)
         compatible_simulators)) ||
     List.exists (fun (simulator : Workspace_xcode.simulator) ->
       not (valid_uuid simulator.id &&
            List.mem simulator.id configured_simulator_ids &&
            List.mem simulator.id ios_destinations &&
            (simulator.state = "Booted" || simulator.state = "Shutdown")))
       compatible_simulators then
    fail "iOS simulator inventory is not the current compatible destination set";
  { session_id; inventory_id; configured_avds; android_devices; avd_bindings;
    android_bindings_complete; configured_simulator_ids; ios_destinations;
    compatible_simulators }

let inventory_session_id inventory = inventory.session_id
let inventory_id inventory = inventory.inventory_id

type ownership = Preexisting | Owned of {
  owner_session_id : string;
  ownership_id : string;
  launcher_id : string;
}

type managed = {
  managed_session_id : string;
  managed_inventory_id : string;
  managed_target : target;
  ownership : ownership;
}

let ownership managed = managed.ownership
let managed_target managed = managed.managed_target

type pending = {
  pending_session_id : string;
  pending_inventory_id : string;
  pending_target : target;
  ownership_id : string;
}

type booting = {
  boot_pending : pending;
  launcher_id : string;
}
let launcher_identity booting = booting.launcher_id

type boot_plan = Already_booted of managed | Start of { command : string; pending : pending }

let validate_target = function
  | Android_avd { name; port } ->
      if not (valid_avd name) then fail "selected AVD name is invalid";
      if port < 5554 || port > 5682 || port mod 2 <> 0 then
        fail "Android emulator port must be an even console port from 5554 through 5682"
  | Ios_simulator { id } ->
      if String.length id <> 36 ||
         not (String.for_all (function
           | '0'..'9' | 'a'..'f' | 'A'..'F' | '-' -> true | _ -> false) id) ||
         not (List.for_all (fun offset -> id.[offset] = '-') [8; 13; 18; 23]) then
        fail "selected simulator ID is invalid"

let check_context inventory ~session_id ~inventory_id =
  if inventory.session_id <> session_id || inventory.inventory_id <> inventory_id then
    fail "device lifecycle result is not bound to the current mobile session inventory"

let current_device inventory serial =
  List.find_opt (fun (device : Workspace_android_devices.device) ->
    device.serial = serial) inventory.android_devices

let current_avd_serial inventory name =
  List.assoc_opt name inventory.avd_bindings

let serial_port serial =
  if not (valid_serial serial) then fail "invalid current emulator serial";
  let prefix_length = String.length "emulator-" in
  let port = try int_of_string
      (String.sub serial prefix_length (String.length serial - prefix_length))
    with Failure _ -> fail "current emulator serial has an invalid console port" in
  if port < 5554 || port > 5682 || port mod 2 <> 0 then
    fail "current emulator serial is outside the supported console-port range";
  port

let simulator inventory id =
  List.find_opt (fun (device : Workspace_xcode.simulator) -> device.id = id)
    inventory.compatible_simulators

let boot_command ~inventory ~approval ~target ~ownership_id =
  validate_target target;
  valid_identity "device ownership ID" ownership_id;
  let id = target_id target in
  check_approval approval ~action:Boot ~session_id:inventory.session_id
    ~inventory_id:inventory.inventory_id ~target_id:id;
  match target with
  | Android_avd { name; port } ->
      if not (List.mem name inventory.configured_avds) then
        fail "selected AVD is not in the current configured-AVD inventory";
      if not inventory.android_bindings_complete then
        fail "refresh a complete Android AVD-to-emulator inventory before booting";
      if List.exists (fun (device : Workspace_android_devices.device) ->
        device.emulator && device.state <> Workspace_android_devices.Ready)
        inventory.android_devices then
        fail "an Android emulator is unavailable; refresh inventory before booting";
      (match current_avd_serial inventory name with
       | Some serial ->
           let actual_port = serial_port serial in
           if actual_port <> port then
             fail "selected AVD is already running on a different console port";
           (match current_device inventory serial with
            | Some { state = Workspace_android_devices.Ready; emulator = true; _ } ->
                let actual_target = Android_avd { name; port = actual_port } in
                Already_booted {
                  managed_session_id = inventory.session_id;
                  managed_inventory_id = inventory.inventory_id;
                  managed_target = actual_target; ownership = Preexisting }
            | _ -> fail "selected AVD identity is not a ready current emulator")
       | None ->
           let serial = target_serial target in
           if Option.is_some (current_device inventory serial) then
             fail "selected emulator port is already occupied in the current inventory";
           Start { command = "emulator -avd " ^ Filename.quote name ^
               " -port " ^ string_of_int port;
             pending = { pending_session_id = inventory.session_id;
               pending_inventory_id = inventory.inventory_id;
               pending_target = target; ownership_id } })

  | Ios_simulator { id } ->
      if not (List.mem id inventory.configured_simulator_ids) then
        fail "selected simulator is not in the current configured simulator inventory";
      (match simulator inventory id with
       | Some { state = "Booted"; _ } ->
           Already_booted {
             managed_session_id = inventory.session_id;
             managed_inventory_id = inventory.inventory_id;
             managed_target = target; ownership = Preexisting }
       | Some { state = "Shutdown"; _ } ->
           Start { command = "xcrun simctl boot " ^ Filename.quote id;
             pending = { pending_session_id = inventory.session_id;
               pending_inventory_id = inventory.inventory_id;
               pending_target = target; ownership_id } }
       | Some _ -> fail "selected simulator has an unsupported current state"
       | None -> fail "selected configured simulator is not compatible with the current destination")

let settle_launch pending = function
  | `Failed | `Cancelled -> None
  | `Started launcher_id ->
      valid_identity "owned launcher identity" launcher_id;
      Some { boot_pending = pending; launcher_id }

let readiness_command ~booting ~approval =
  let pending = booting.boot_pending in
  let target = pending.pending_target in
  let id = target_id target in
  check_approval approval ~action:Readiness ~session_id:pending.pending_session_id
    ~inventory_id:pending.pending_inventory_id ~target_id:id;
  match target with
  | Android_avd _ ->
      "adb -s " ^ Filename.quote (target_serial target) ^
      " shell getprop sys.boot_completed"
  | Ios_simulator _ -> "xcrun simctl list devices --json"

type readiness = Waiting | Ready of managed

let exact_status label output =
  bounded_output label output;
  let output = String.map (fun ch -> if ch = '\r' then '\n' else ch) output in
  let output = String.split_on_char '\n' output |> List.filter ((<>) "") in
  match output with
  | ["0"] -> false
  | ["1"] -> true
  | _ -> fail (label ^ " output is malformed")

let android_readiness ~booting ~inventory ~approval ~avd_name_output
    ~devices_output ~boot_status_output =
  let pending = booting.boot_pending in
  check_context inventory ~session_id:pending.pending_session_id
    ~inventory_id:pending.pending_inventory_id;
  let target = match pending.pending_target with
    | Android_avd _ as target -> target
    | Ios_simulator _ -> fail "Android readiness cannot settle an iOS simulator" in
  check_approval approval ~action:Readiness ~session_id:pending.pending_session_id
    ~inventory_id:pending.pending_inventory_id ~target_id:(target_id target);
  let expected_avd = match target with
    | Android_avd { name; _ } -> name
    | Ios_simulator _ -> fail "Android readiness cannot settle an iOS simulator" in
  if parse_avd_name avd_name_output <> expected_avd then
    fail "selected emulator serial now identifies a different AVD";
  bounded_output "ADB device inventory" devices_output;
  let devices = Workspace_android_devices.adb_devices devices_output in
  let serial = target_serial target in
  let device_ready = List.exists (fun (device : Workspace_android_devices.device) ->
    device.serial = serial && device.emulator &&
    device.state = Workspace_android_devices.Ready) devices in
  let current_binding =
    current_avd_serial inventory expected_avd = Some serial &&
    (match current_device inventory serial with
     | Some { emulator = true; state = Workspace_android_devices.Ready; _ } -> true
     | _ -> false) in
  let boot_completed = exact_status "Android boot readiness" boot_status_output in
  if device_ready && current_binding && boot_completed then
    Ready { managed_session_id = pending.pending_session_id;
      managed_inventory_id = pending.pending_inventory_id;
      managed_target = target;
      ownership = Owned { owner_session_id = pending.pending_session_id;
        ownership_id = pending.ownership_id; launcher_id = booting.launcher_id } }
  else Waiting

let ios_readiness ~booting ~inventory ~approval ~devices_json =
  let pending = booting.boot_pending in
  check_context inventory ~session_id:pending.pending_session_id
    ~inventory_id:pending.pending_inventory_id;
  let id = match pending.pending_target with
    | Ios_simulator { id } -> id
    | Android_avd _ -> fail "iOS readiness cannot settle an Android AVD" in
  check_approval approval ~action:Readiness ~session_id:pending.pending_session_id
    ~inventory_id:pending.pending_inventory_id ~target_id:(target_id pending.pending_target);
  bounded_output "simulator readiness" devices_json;
  let simulators = Workspace_xcode.compatible_simulators
      ~destinations:inventory.ios_destinations devices_json in
  match List.find_opt (fun (simulator : Workspace_xcode.simulator) ->
      simulator.id = id) simulators with
  | Some { state = "Booted"; _ } ->
      Ready { managed_session_id = pending.pending_session_id;
        managed_inventory_id = pending.pending_inventory_id;
        managed_target = pending.pending_target;
        ownership = Owned { owner_session_id = pending.pending_session_id;
          ownership_id = pending.ownership_id; launcher_id = booting.launcher_id } }
  | Some { state = "Shutdown"; _ } -> Waiting
  | Some _ -> fail "simulator returned an unsupported readiness state"
  | None -> fail "selected simulator is absent from the compatible readiness inventory"

let verify_managed_context inventory managed =
  check_context inventory ~session_id:managed.managed_session_id
    ~inventory_id:managed.managed_inventory_id;
  match managed.ownership with
  | Preexisting -> ()
  | Owned { owner_session_id; _ } when owner_session_id = managed.managed_session_id -> ()
  | Owned _ -> fail "device ownership is not bound to its mobile session"

let shutdown_command ~inventory ~approval ~managed =
  verify_managed_context inventory managed;
  let target = managed.managed_target in
  check_approval approval ~action:Shutdown ~session_id:managed.managed_session_id
    ~inventory_id:managed.managed_inventory_id ~target_id:(target_id target);
  match managed.ownership, target with
  | Preexisting, _ -> None
  | Owned _, Android_avd { name; _ } ->
      if current_avd_serial inventory name <> Some (target_serial target) then
        fail "owned Android AVD is not bound to its exact current emulator serial";
      (match current_device inventory (target_serial target) with
       | Some { emulator = true; state = Workspace_android_devices.Ready; _ } ->
           Some ("adb -s " ^ Filename.quote (target_serial target) ^ " emu kill")
       | _ -> fail "owned Android emulator is not ready in the current inventory")
  | Owned _, Ios_simulator { id } ->
      (match simulator inventory id with
       | Some { state = "Booted"; _ } ->
           Some ("xcrun simctl shutdown " ^ Filename.quote id)
       | _ -> fail "owned simulator is not booted in the current compatible inventory")

let complete_shutdown managed = function
  | `Succeeded ->
      (match managed.ownership with Preexisting -> Some managed | Owned _ -> None)
  | `Failed | `Cancelled -> Some managed

let complete_abort booting = function
  | `Succeeded -> None
  | `Failed | `Cancelled -> Some booting

let abort_command ~inventory ~approval ~booting =
  let pending = booting.boot_pending in
  check_context inventory ~session_id:pending.pending_session_id
    ~inventory_id:pending.pending_inventory_id;
  let target = pending.pending_target in
  check_approval approval ~action:Shutdown ~session_id:pending.pending_session_id
    ~inventory_id:pending.pending_inventory_id ~target_id:(target_id target);
  match target with
  | Android_avd { name; _ } ->
      if current_avd_serial inventory name <> Some (target_serial target) then None
      else
        (match current_device inventory (target_serial target) with
         | Some { emulator = true; state = Workspace_android_devices.Ready; _ } ->
             Some ("adb -s " ^ Filename.quote (target_serial target) ^ " emu kill")
         | _ -> None)
  | Ios_simulator { id } ->
      (match simulator inventory id with
       | Some { state = "Booted"; _ } ->
           Some ("xcrun simctl shutdown " ^ Filename.quote id)
       | _ -> None)
