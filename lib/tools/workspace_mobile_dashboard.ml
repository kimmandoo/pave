type action =
  | Build | Install | Launch | Stop | Observe | Control | Replay | Diagnostics
  | Verify | Save_baseline | Compare_baseline | Accessibility_audit | Environment
  | Deep_link | Lifecycle | Ios_tree | Android_performance | Flutter_integration
  | Ios_performance | Permission | Network

type item = {
  action : action;
  description : string;
  unavailable_reason : string option;
}

let label = function
  | Build -> "Build app" | Install -> "Install app"
  | Launch -> "Launch app" | Stop -> "Stop app"
  | Observe -> "Observe app" | Control -> "Control app"
  | Replay -> "Replay bug scenario" | Diagnostics -> "Read runtime diagnostics"
  | Verify -> "Verify guarded edit" | Save_baseline -> "Save screenshot baseline"
  | Compare_baseline -> "Compare screenshot baseline"
  | Accessibility_audit -> "Accessibility audit"
  | Environment -> "Environment experiment" | Deep_link -> "Exercise deep link"
  | Lifecycle -> "Lifecycle scenario" | Ios_tree -> "iOS accessibility tree"
  | Android_performance -> "Android performance"
  | Flutter_integration -> "Flutter integration test"
  | Ios_performance -> "iOS performance counters"
  | Permission -> "Permission-state experiments"
  | Network -> "Network-disruption experiments"

let key = function
  | Build -> "build" | Install -> "install" | Launch -> "launch" | Stop -> "stop"
  | Observe -> "observe" | Control -> "control" | Replay -> "replay"
  | Diagnostics -> "diagnostics" | Verify -> "verify"
  | Save_baseline -> "save_baseline" | Compare_baseline -> "compare_baseline"
  | Accessibility_audit -> "accessibility_audit" | Environment -> "environment"
  | Deep_link -> "deep_link" | Lifecycle -> "lifecycle" | Ios_tree -> "ios_tree"
  | Android_performance -> "android_performance"
  | Flutter_integration -> "flutter_integration"
  | Ios_performance -> "ios_performance" | Permission -> "permission"
  | Network -> "network"

let proposed = function
  | Accessibility_audit | Environment | Deep_link | Lifecycle | Ios_tree
  | Android_performance | Flutter_integration | Ios_performance | Permission
  | Network -> true
  | Build | Install | Launch | Stop | Observe | Control | Replay | Diagnostics
  | Verify | Save_baseline | Compare_baseline -> false

let description = function
  | Build -> "Build the selected app through the approved workspace workflow."
  | Install -> "Install the checked app artifact on the selected device."
  | Launch -> "Launch the installed selected app."
  | Stop -> "Stop the running selected app."
  | Observe -> "Capture the selected app screen and supported UI evidence."
  | Control -> "Send an approved semantic action to the selected app."
  | Replay -> "Replay an approved selected-app bug scenario."
  | Diagnostics -> "Read runtime diagnostics for the built selected app."
  | Verify -> "Verify the recorded guarded edit against its private snapshot."
  | Save_baseline -> "Save a screenshot baseline of the running app."
  | Compare_baseline -> "Compare the running app with its screenshot baseline."
  | Accessibility_audit -> "Audit captured UI accessibility evidence."
  | Environment -> "Exercise locale, theme and orientation with guarded restore."
  | Deep_link -> "Open a registered selected-app link and verify its destination."
  | Lifecycle -> "Exercise background, resume and process recreation."
  | Ios_tree -> "Capture accessibility from an approved Owned Simulator."
  | Android_performance -> "Measure app-bound launch, frames and memory."
  | Flutter_integration -> "Run an existing selected-app Flutter integration test."
  | Ios_performance -> "Capture and export selected-app iOS performance counters."
  | Permission -> "Exercise runtime permission-state transitions."
  | Network -> "Exercise emulator-wide network disruption."

let available action =
  { action; description = description action; unavailable_reason = None }
let unavailable action reason =
  { action; description = description action; unavailable_reason = Some reason }

let recommended ~state ~artifact_available = match state with
  | Workspace_mobile_run.Selected -> if artifact_available then Install else Build
  | Workspace_mobile_run.Built -> if artifact_available then Install else Build
  | Workspace_mobile_run.Installed | Workspace_mobile_run.Stopped -> Launch
  | Workspace_mobile_run.Running -> Observe

let transition action state =
  if Workspace_mobile_run.can_transition (key action) state then available action
  else unavailable action (Printf.sprintf "cannot %s while the selected app is %s."
    (key action) (Workspace_mobile_run.state_name state))

let running action state =
  if state = Workspace_mobile_run.Running then available action
  else unavailable action (Printf.sprintf
    "requires a running selected app; current app state is %s."
    (Workspace_mobile_run.state_name state))

let permission_reason =
  "runtime permission transitions remain unavailable by explicit user decision; a new design decision is required."

let network_reason =
  "emulator-wide network disruption remains unavailable by explicit user decision; a new design decision is required."

let mx02_reason =
  "MX02 remains open until a real supported macOS/Xcode run captures accessibility from an approved disposable Owned Simulator."

let mx05_reason =
  "MX05 remains open until a separately approved live Android accessibility tree completes its acceptance; fixture tests do not certify device capture."

let mx08_reason =
  "MX08 remains open until a separately approved disposable Android locale/theme/orientation change and guarded restore are observed."

let mx09_reason =
  "MX09 remains open until a real selected-app Android URL handler opens the exact registered link and verifies its destination."

let mx10_reason =
  "MX10 remains open until a real disposable Android app verifies background/resume and process-recreation state behavior."

let mx14_reason =
  "MX14 remains open until a real ready Android emulator yields app-bound launch/frame/memory measurements with actual units and completeness."

let mx17_reason =
  "MX17 remains open until an existing Flutter integration_test passes on the exact selected app and approved ready emulator."

let combine_reasons primary secondary =
  match secondary with
  | None -> primary
  | Some reason -> primary ^ " Current unmet prerequisite: " ^ reason

let ios_tree_reason ios_accessibility_reason =
  combine_reasons mx02_reason ios_accessibility_reason

let flutter_reason flutter_integration_reason =
  combine_reasons mx17_reason flutter_integration_reason


let entries ~platform ~state ~artifact_available ~ios_accessibility_reason
    ~flutter_integration_reason ~verification_available =
  let build = if state = Workspace_mobile_run.Running then
      unavailable Build "stop the running selected app before building."
    else available Build in
  let install =
    if not (Workspace_mobile_run.can_transition "install" state) then
      transition Install state
    else if not artifact_available then
      unavailable Install "requires a checked app artifact; build the selected app first."
    else available Install in
  let launch = if state = Workspace_mobile_run.Running then
      unavailable Launch "the selected app is already running."
    else transition Launch state in
  let diagnostics = match state with
    | Workspace_mobile_run.Selected -> unavailable Diagnostics
        "requires a built app; this session is only selected."
    | _ -> available Diagnostics in
  let verification = if verification_available then
      available Verify
    else unavailable Verify
      "no guarded apply_edits snapshot is recorded in this private session." in
  let common = [
    build; install; launch; transition Stop state;
    running Observe state;
    diagnostics;
    verification;
    running Save_baseline state;
    running Compare_baseline state;
  ] in
  let permission = unavailable Permission permission_reason
  and network = unavailable Network network_reason in
  match platform with
  | Workspace_mobile_run.Android ->
      common @ [
        running Control state;
        running Replay state;
        unavailable Accessibility_audit mx05_reason;
        unavailable Environment mx08_reason;
        unavailable Deep_link mx09_reason;
        unavailable Lifecycle mx10_reason;
        unavailable Android_performance mx14_reason;
        unavailable Flutter_integration
          (flutter_reason flutter_integration_reason);
        permission;
        network;
      ]
  | Workspace_mobile_run.Ios ->
      common @ [
        unavailable Ios_tree
          (ios_tree_reason ios_accessibility_reason);
        unavailable Control
          "iOS semantic UI actions have no supported backend; no input is sent.";
        unavailable Replay
          "iOS scenario replay requires unavailable iOS semantic controls; no replay runs.";
        unavailable Accessibility_audit
          "rule-based accessibility findings currently use Android UIAutomator trees only.";
        unavailable Environment
          "locale, theme and orientation experiments currently target Android emulators only.";
        unavailable Deep_link
          "verified selected-app URL-handler dispatch currently supports Android APKs only.";
        unavailable Lifecycle
          "background, resume and process-recreation scenarios currently support Android only.";
        unavailable Android_performance
          "measured launch, frame and memory workflows currently support Android only.";
        unavailable Flutter_integration
          "device integration_test execution currently supports installed Android Flutter apps only; MX17 acceptance remains open.";
        unavailable Ios_performance
          "MX15 remains open; iOS xctrace capture is raw and has no supported counter export.";
        permission;
        network;
      ]
