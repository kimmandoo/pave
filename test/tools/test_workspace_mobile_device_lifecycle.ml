module Lifecycle = Pave.Workspace_mobile_device_lifecycle
module Devices = Pave.Workspace_android_devices
module Xcode = Pave.Workspace_xcode

let expect label condition = if not condition then failwith label
let rejects label operation =
  try ignore (operation ()); failwith ("accepted " ^ label)
  with Lifecycle.Error _ | Xcode.Error _ -> ()

let android_device ?(state = Devices.Ready) serial : Devices.device = {
  serial; state; emulator = true;
}

let inventory ?(id = "inventory-1") ?(configured_avds = ["Pixel_API_35"])
    ?(android_devices = []) ?(avd_bindings = []) ?(android_bindings_complete = true)
    ?(configured_simulator_ids = []) ?(ios_destinations = [])
    ?(compatible_simulators = []) () =
  Lifecycle.create_inventory ~session_id:"mobile-7" ~inventory_id:id
    ~configured_avds ~android_devices ~avd_bindings ~android_bindings_complete
    ~configured_simulator_ids ~ios_destinations ~compatible_simulators

let approved inventory action target = Lifecycle.approval
  ~marker:"operator-approved-device-effect" ~action
  ~session_id:(Lifecycle.inventory_session_id inventory)
  ~inventory_id:(Lifecycle.inventory_id inventory)
  ~target_id:(Lifecycle.target_id target)

let sim_id = "26ae0000-0000-0000-0000-000000000000"
let simulator state : Xcode.simulator = { runtime = "18.0"; id = sim_id;
  name = "iPhone Fixture"; state }

let sim_inventory ?(destinations = [sim_id]) ?(configured = [sim_id]) state =
  inventory ~configured_simulator_ids:configured ~ios_destinations:destinations
    ~compatible_simulators:(if List.mem sim_id destinations then [simulator state] else []) ()

let simctl_json state = Printf.sprintf
  "{\"devices\":{\"com.apple.CoreSimulator.SimRuntime.iOS-18-0\":[{\"udid\":\"%s\",\"name\":\"iPhone Fixture\",\"state\":\"%s\",\"isAvailable\":true}]}}"
  sim_id state

let () =
  let configured = Lifecycle.configured_simulator_ids
    (simctl_json "Shutdown") in
  expect "configured simulator parser retains available device IDs"
    (configured = [sim_id]);
  expect "ADB AVD identity parser accepts one exact console response"
    (Lifecycle.parse_avd_name "Pixel_API_35\r\nOK\r\n" = "Pixel_API_35");
  rejects "malformed AVD identity output" (fun () ->
    Lifecycle.parse_avd_name "Pixel_API_35\r\nother-device\r\n");
  let preexisting_android = inventory
      ~android_devices:[android_device "emulator-5554"]
      ~avd_bindings:["Pixel_API_35", "emulator-5554"] () in
  let existing_avd = Lifecycle.Android_avd { name = "Pixel_API_35"; port = 5554 } in
  rejects "pre-existing AVD on another port is not substituted for the selected target"
    (fun () ->
      Lifecycle.boot_command ~inventory:preexisting_android
        ~approval:(approved preexisting_android Lifecycle.Boot existing_avd)
        ~target:(Lifecycle.Android_avd { name = "Pixel_API_35"; port = 5556 })
        ~ownership_id:"wrong-port-owner");

  let existing_avd_managed = match Lifecycle.boot_command
      ~inventory:preexisting_android
      ~approval:(approved preexisting_android Lifecycle.Boot existing_avd)
      ~target:existing_avd ~ownership_id:"unused-android-owner" with
    | Lifecycle.Already_booted managed -> managed
    | _ -> failwith "pre-existing Android AVD was treated as newly owned" in
  expect "pre-existing AVD retains its exact current serial"
    (Lifecycle.target_serial (Lifecycle.managed_target existing_avd_managed) =
      "emulator-5554");
  expect "pre-existing Android emulator survives cleanup"
    (Lifecycle.shutdown_command ~inventory:preexisting_android
       ~approval:(approved preexisting_android Lifecycle.Shutdown
         (Lifecycle.managed_target existing_avd_managed))
       ~managed:existing_avd_managed = None);

  let configured_only = sim_inventory ~destinations:[] "Shutdown" in
  let unconfigured_target = Lifecycle.Ios_simulator { id = sim_id } in
  rejects "configured-only simulator boot" (fun () ->
    Lifecycle.boot_command ~inventory:configured_only
      ~approval:(approved configured_only Lifecycle.Boot unconfigured_target)
      ~target:unconfigured_target ~ownership_id:"op-ios-configured-only");
  let compatible = sim_inventory "Shutdown" in
  let ios_plan = Lifecycle.boot_command ~inventory:compatible
      ~approval:(approved compatible Lifecycle.Boot unconfigured_target)
      ~target:unconfigured_target ~ownership_id:"op-ios-compatible" in
  let ios_booting = match ios_plan with
    | Lifecycle.Start { pending; _ } ->
        Option.get (Lifecycle.settle_launch pending (`Started "simctl-job-2"))
    | _ -> failwith "configured simulator did not produce an owned boot" in
  let ios_readiness_approval = approved compatible Lifecycle.Readiness unconfigured_target in
  expect "simulator remains waiting after boot process start"
    (Lifecycle.ios_readiness ~booting:ios_booting ~inventory:compatible
       ~approval:ios_readiness_approval ~devices_json:(simctl_json "Shutdown") =
       Lifecycle.Waiting);
  let ios_ready_inventory = sim_inventory "Booted" in
  let ios_owned = match Lifecycle.ios_readiness ~booting:ios_booting
      ~inventory:ios_ready_inventory
      ~approval:(approved ios_ready_inventory Lifecycle.Readiness unconfigured_target)
      ~devices_json:(simctl_json "Booted") with
    | Lifecycle.Ready managed -> managed
    | Lifecycle.Waiting -> failwith "Booted simulator inventory was not ready" in
  expect "owned simulator shuts down by exact UUID"
    (Lifecycle.shutdown_command ~inventory:ios_ready_inventory
       ~approval:(approved ios_ready_inventory Lifecycle.Shutdown unconfigured_target)
       ~managed:ios_owned = Some ("xcrun simctl shutdown '" ^ sim_id ^ "'"));
  let malformed_android = inventory () in
  let malformed_target = Lifecycle.Android_avd { name = "Pixel_API_35"; port = 5556 } in
  rejects "boot without exact boot approval" (fun () ->
    Lifecycle.boot_command ~inventory:malformed_android
      ~approval:(approved malformed_android Lifecycle.Shutdown malformed_target)
      ~target:malformed_target ~ownership_id:"op-android-unapproved");
  let pending = match Lifecycle.boot_command ~inventory:malformed_android
      ~approval:(approved malformed_android Lifecycle.Boot malformed_target)
      ~target:malformed_target ~ownership_id:"op-android-malformed" with
    | Lifecycle.Start { pending; command } ->
        expect "Android boot command uses existing AVD without destructive flags"
          (command = "emulator -avd 'Pixel_API_35' -port 5556");
        pending
    | _ -> failwith "configured-only AVD did not produce a boot command" in
  expect "failed launcher does not transition" (Lifecycle.settle_launch pending `Failed = None);
  expect "cancelled launcher does not transition"
    (Lifecycle.settle_launch pending `Cancelled = None);
  let booting = Option.get (Lifecycle.settle_launch pending (`Started "process-job-44")) in
  let readiness_approval = approved malformed_android Lifecycle.Readiness malformed_target in
  expect "Android readiness command selects exact owned serial"
    (Lifecycle.readiness_command ~booting ~approval:readiness_approval =
      "adb -s 'emulator-5556' shell getprop sys.boot_completed");
  rejects "malformed Android readiness status" (fun () ->
    Lifecycle.android_readiness ~booting ~inventory:malformed_android
      ~approval:readiness_approval
      ~avd_name_output:"Pixel_API_35\r\nOK\r\n"
      ~devices_output:"List of devices attached\nemulator-5556\tdevice\n"
      ~boot_status_output:"boot-complete\n");
  let devices_output = "List of devices attached\nemulator-5556\tdevice\n" in
  expect "zero boot property is still waiting, not ready"
    (Lifecycle.android_readiness ~booting ~inventory:malformed_android
       ~approval:readiness_approval ~avd_name_output:"Pixel_API_35\r\nOK\r\n"
       ~devices_output ~boot_status_output:"0\n" =
       Lifecycle.Waiting);
  let ready_inventory = inventory ~android_devices:[android_device "emulator-5556"]
      ~avd_bindings:["Pixel_API_35", "emulator-5556"]
      ~android_bindings_complete:false () in
  let owned = match Lifecycle.android_readiness ~booting ~inventory:ready_inventory
      ~approval:(approved ready_inventory Lifecycle.Readiness malformed_target)
      ~avd_name_output:"Pixel_API_35\r\nOK\r\n"
      ~devices_output ~boot_status_output:"1\r\n" with
    | Lifecycle.Ready managed -> managed
    | Lifecycle.Waiting -> failwith "completed Android boot was not ready" in
  (match Lifecycle.ownership owned with
   | Lifecycle.Owned { owner_session_id = "mobile-7";
       ownership_id = "op-android-malformed"; launcher_id = "process-job-44" } ->
       expect "owned process identity remains available for cancellation cleanup"
         (Lifecycle.launcher_identity booting = "process-job-44")
   | _ -> failwith "ready emulator lost explicit current-session ownership");
  let shutdown_approval = approved ready_inventory Lifecycle.Shutdown malformed_target in
  expect "owned Android emulator shuts down by exact serial"
    (Lifecycle.shutdown_command ~inventory:ready_inventory
       ~approval:shutdown_approval ~managed:owned =
       Some "adb -s 'emulator-5556' emu kill");
  expect "failed shutdown keeps ownership for cleanup"
    (Lifecycle.complete_shutdown owned `Failed = Some owned);
  expect "cancelled shutdown keeps ownership for cleanup"
    (Lifecycle.complete_shutdown owned `Cancelled = Some owned);
  expect "successful owned shutdown releases lifecycle ownership"
    (Lifecycle.complete_shutdown owned `Succeeded = None);
  expect "failed boot cleanup retains exact owner state"
    (Lifecycle.complete_abort booting `Failed = Some booting);
  expect "cancelled boot cleanup retains exact owner state"
    (Lifecycle.complete_abort booting `Cancelled = Some booting);
  expect "successful boot cleanup clears owned pending state"
    (Lifecycle.complete_abort booting `Succeeded = None);

  let booting_inventory = inventory ~android_devices:[android_device "emulator-5556"]
      ~avd_bindings:["Pixel_API_35", "emulator-5556"] () in
  expect "cancelled boot can clean up only the current owned AVD"
    (Lifecycle.abort_command ~inventory:booting_inventory
       ~approval:(approved booting_inventory Lifecycle.Shutdown malformed_target)
       ~booting = Some "adb -s 'emulator-5556' emu kill");
  rejects "readiness from another exact inventory" (fun () ->
    let other_inventory = inventory ~id:"inventory-2" () in
    Lifecycle.android_readiness ~booting ~inventory:other_inventory
      ~approval:(approved other_inventory Lifecycle.Readiness malformed_target)
      ~avd_name_output:"Pixel_API_35\r\nOK\r\n"
      ~devices_output:"List of devices attached\nemulator-5556\tdevice\n"
      ~boot_status_output:"1\n");

  let existing = sim_inventory "Booted" in
  let existing_target = Lifecycle.Ios_simulator { id = sim_id } in
  let preexisting = match Lifecycle.boot_command ~inventory:existing
      ~approval:(approved existing Lifecycle.Boot existing_target)
      ~target:existing_target ~ownership_id:"unused-owner" with
    | Lifecycle.Already_booted managed -> managed
    | _ -> failwith "pre-existing simulator was treated as newly owned" in
  expect "pre-existing simulator survives cleanup"
    (Lifecycle.shutdown_command ~inventory:existing
       ~approval:(approved existing Lifecycle.Shutdown existing_target)
       ~managed:preexisting = None);
  expect "successful cleanup retains pre-existing lifecycle record"
    (Lifecycle.complete_shutdown preexisting `Succeeded = Some preexisting);

  rejects "malformed Simulator readiness JSON" (fun () ->
    Lifecycle.ios_readiness ~booting:ios_booting ~inventory:compatible
      ~approval:ios_readiness_approval ~devices_json:"not-json");
  print_endline "mobile device lifecycle fixtures: ok"
