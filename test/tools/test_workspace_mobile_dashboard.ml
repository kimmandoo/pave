module Dashboard = Pave.Workspace_mobile_dashboard
module Run = Pave.Workspace_mobile_run

let fail label = failwith ("mobile dashboard: " ^ label)
let expect label condition = if not condition then fail label

let item entries action =
  match List.find_opt (fun item -> item.Dashboard.action = action) entries with
  | Some item -> item
  | None -> fail ("missing action " ^ action)

let available entries action =
  expect (action ^ " should be available")
    ((item entries action).Dashboard.unavailable_reason = None)

let unavailable entries action reason_part =
  match (item entries action).Dashboard.unavailable_reason with
  | Some reason ->
      expect (action ^ " should explain its exact gate")
        (let length = String.length reason_part in
         let rec contains index =
           index + length <= String.length reason &&
           (String.sub reason index length = reason_part || contains (index + 1)) in
         contains 0)
  | None -> fail (action ^ " must not be offered as usable")

let entries ?(ios_accessibility_reason = None)
    ?(flutter_integration_reason = None) ?(verification_available = false)
    platform state =
  Dashboard.entries ~platform ~state ~ios_accessibility_reason
    ~flutter_integration_reason ~verification_available

let () =
  let selected = entries Run.Android Run.Selected in
  unavailable selected "Observe app" "current app state is selected";
  unavailable selected "Read runtime diagnostics" "requires a built app";
  unavailable selected "Verify guarded edit" "no guarded apply_edits snapshot";
  unavailable selected "Control app" "current app state is selected";
  unavailable selected "Exercise deep link" "MX09 remains open";
  unavailable selected "Lifecycle scenario" "MX10 remains open";
  unavailable selected "Permission-state experiments" "explicit user decision";
  unavailable selected "Network-disruption experiments" "explicit user decision";

  let built = entries ~verification_available:true Run.Android Run.Built in
  available built "Read runtime diagnostics";
  available built "Verify guarded edit";
  unavailable built "Control app" "current app state is built";
  unavailable built "Android performance" "MX14 remains open";
  unavailable built "Exercise deep link" "MX09 remains open";

  let installed = entries ~flutter_integration_reason:(Some
      "no existing integration_test target is available")
      Run.Android Run.Installed in
  unavailable installed "Exercise deep link" "MX09 remains open";
  unavailable installed "Lifecycle scenario" "MX10 remains open";
  unavailable installed "Accessibility audit" "MX05 remains open";
  unavailable installed "Flutter integration test" "MX17 remains open";
  unavailable installed "Flutter integration test"
    "no existing integration_test target is available";

  let running = entries ~verification_available:true Run.Android Run.Running in
  List.iter (available running)
    ["Observe app"; "Read runtime diagnostics"; "Verify guarded edit";
     "Save screenshot baseline"; "Compare screenshot baseline"; "Control app";
     "Replay bug scenario"];
  unavailable running "Accessibility audit" "MX05 remains open";
  unavailable running "Environment experiment" "MX08 remains open";
  unavailable running "Exercise deep link" "MX09 remains open";
  unavailable running "Lifecycle scenario" "MX10 remains open";
  unavailable running "Android performance" "MX14 remains open";
  unavailable running "Flutter integration test" "MX17 remains open";
  unavailable running "Permission-state experiments" "explicit user decision";
  unavailable running "Network-disruption experiments" "explicit user decision";

  let ios = entries Run.Ios Run.Running in
  available ios "Observe app";
  unavailable ios "iOS accessibility tree" "MX02 remains open";
  unavailable ios "Control app" "no supported backend";
  unavailable ios "Replay bug scenario" "requires unavailable iOS semantic controls";
  unavailable ios "Accessibility audit" "Android UIAutomator trees only";
  unavailable ios "Exercise deep link" "Android APKs only";
  unavailable ios "Lifecycle scenario" "support Android only";
  unavailable ios "Flutter integration test" "installed Android Flutter apps only";
  unavailable ios "iOS performance counters" "MX15 remains open";

  let unbound_ios = entries ~ios_accessibility_reason:(Some
      "iOS app session has no bound device lifecycle identity") Run.Ios Run.Running in
  available unbound_ios "Observe app";
  unavailable unbound_ios "iOS accessibility tree" "no bound device lifecycle identity";
  unavailable unbound_ios "iOS accessibility tree" "MX02 remains open";
  print_endline "workspace mobile dashboard capabilities: ok"
