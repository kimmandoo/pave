type item = { action : string; unavailable_reason : string option }

let available action = { action; unavailable_reason = None }
let unavailable action reason = { action; unavailable_reason = Some reason }

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


let entries ~platform ~state ~ios_accessibility_reason
    ~flutter_integration_reason ~verification_available =
  let diagnostics = match state with
    | Workspace_mobile_run.Selected -> unavailable "Read runtime diagnostics"
        "requires a built app; this session is only selected."
    | _ -> available "Read runtime diagnostics" in
  let verification = if verification_available then
      available "Verify guarded edit"
    else unavailable "Verify guarded edit"
      "no guarded apply_edits snapshot is recorded in this private session." in
  let common = [
    running "Observe app" state;
    diagnostics;
    verification;
    running "Save screenshot baseline" state;
    running "Compare screenshot baseline" state;
  ] in
  let permission = unavailable "Permission-state experiments" permission_reason
  and network = unavailable "Network-disruption experiments" network_reason in
  match platform with
  | Workspace_mobile_run.Android ->
      common @ [
        running "Control app" state;
        running "Replay bug scenario" state;
        unavailable "Accessibility audit" mx05_reason;
        unavailable "Environment experiment" mx08_reason;
        unavailable "Exercise deep link" mx09_reason;
        unavailable "Lifecycle scenario" mx10_reason;
        unavailable "Android performance" mx14_reason;
        unavailable "Flutter integration test"
          (flutter_reason flutter_integration_reason);
        permission;
        network;
      ]
  | Workspace_mobile_run.Ios ->
      common @ [
        unavailable "iOS accessibility tree"
          (ios_tree_reason ios_accessibility_reason);
        unavailable "Control app"
          "iOS semantic UI actions have no supported backend; no input is sent.";
        unavailable "Replay bug scenario"
          "iOS scenario replay requires unavailable iOS semantic controls; no replay runs.";
        unavailable "Accessibility audit"
          "rule-based accessibility findings currently use Android UIAutomator trees only.";
        unavailable "Environment experiment"
          "locale, theme and orientation experiments currently target Android emulators only.";
        unavailable "Exercise deep link"
          "verified selected-app URL-handler dispatch currently supports Android APKs only.";
        unavailable "Lifecycle scenario"
          "background, resume and process-recreation scenarios currently support Android only.";
        unavailable "Android performance"
          "measured launch, frame and memory workflows currently support Android only.";
        unavailable "Flutter integration test"
          "device integration_test execution currently supports installed Android Flutter apps only; MX17 acceptance remains open.";
        unavailable "iOS performance counters"
          "MX15 remains open; iOS xctrace capture is raw and has no supported counter export.";
        permission;
        network;
      ]
