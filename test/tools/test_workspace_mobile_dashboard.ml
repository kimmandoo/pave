module Dashboard = Pave.Workspace_mobile_dashboard
module Run = Pave.Workspace_mobile_run

let fail label = failwith ("mobile dashboard: " ^ label)
let expect label condition = if not condition then fail label

let item entries action =
  match List.find_opt (fun item -> item.Dashboard.action = action) entries with
  | Some item -> item
  | None -> fail ("missing action " ^ Dashboard.key action)

let availability entries action expected =
  let entry = item entries action in
  expect (Dashboard.key action ^ " availability")
    ((entry.Dashboard.unavailable_reason = None) = expected);
  match entry.Dashboard.unavailable_reason with
  | None -> ()
  | Some reason -> expect "unavailable action has a reason" (String.length reason > 0)

let entries ?(ios_accessibility_reason = None)
    ?(flutter_integration_reason = None) ?(verification_available = false)
    ~artifact_available platform state =
  Dashboard.entries ~platform ~state ~artifact_available ~ios_accessibility_reason
    ~flutter_integration_reason ~verification_available

let states = [Run.Selected; Run.Built; Run.Installed; Run.Running; Run.Stopped]
let platforms = [Run.Android; Run.Ios]

let () =
  List.iter (fun platform ->
    List.iter (fun state ->
      List.iter (fun artifact_available ->
        List.iter (fun verification_available ->
          let actions = entries ~artifact_available ~verification_available platform state in
          let running = state = Run.Running in
          availability actions Dashboard.Build (not running);
          availability actions Dashboard.Install
            (artifact_available && Run.can_transition "install" state);
          availability actions Dashboard.Launch
            (not running && Run.can_transition "launch" state);
          availability actions Dashboard.Stop (Run.can_transition "stop" state);
          List.iter (fun action -> availability actions action running)
            [Dashboard.Observe; Dashboard.Save_baseline; Dashboard.Compare_baseline];
          List.iter (fun action -> availability actions action (running && platform = Run.Android))
            [Dashboard.Control; Dashboard.Replay];
          availability actions Dashboard.Diagnostics (state <> Run.Selected);
          availability actions Dashboard.Verify verification_available;
          List.iter (fun entry ->
            expect "nonempty description" (String.length entry.Dashboard.description > 0);
            expect "nonempty display label" (String.length (Dashboard.label entry.action) > 0);
            if Dashboard.proposed entry.action then
              availability actions entry.action false) actions;
          let keys = List.map (fun entry -> Dashboard.key entry.Dashboard.action) actions in
          expect "distinct action keys" (List.length keys = List.length (List.sort_uniq String.compare keys));
          let recommended = Dashboard.recommended ~state ~artifact_available in
          let expected = match state with
            | Run.Selected -> if artifact_available then Dashboard.Install else Dashboard.Build
            | Run.Built -> if artifact_available then Dashboard.Install else Dashboard.Build
            | Run.Installed | Run.Stopped -> Dashboard.Launch
            | Run.Running -> Dashboard.Observe in
          expect "state-aware recommendation" (recommended = expected);
          expect "recommended baseline action" (not (Dashboard.proposed recommended));
          availability actions recommended true
        ) [false; true]
      ) [false; true]
    ) states
  ) platforms;
  List.iter (fun platform ->
    let ready = entries ~artifact_available:true platform Run.Running in
    let unmet = entries ~artifact_available:true
        ~ios_accessibility_reason:(Some "missing bound simulator identity")
        ~flutter_integration_reason:(Some "missing existing integration target")
        platform Run.Running in
    List.iter (fun action ->
      availability ready action false;
      availability unmet action false;
      expect "current prerequisite supplements acceptance gate"
        ((item ready action).Dashboard.unavailable_reason <>
         (item unmet action).Dashboard.unavailable_reason)
    ) (match platform with
       | Run.Android -> [Dashboard.Flutter_integration]
       | Run.Ios -> [Dashboard.Ios_tree]);
    List.iter (fun action ->
      expect "permanently blocked feature is proposed" (Dashboard.proposed action);
      availability ready action false
    ) [Dashboard.Accessibility_audit; Dashboard.Environment; Dashboard.Deep_link;
       Dashboard.Lifecycle; Dashboard.Android_performance; Dashboard.Flutter_integration;
       Dashboard.Permission; Dashboard.Network]
  ) platforms;
  let ios = entries ~artifact_available:true Run.Ios Run.Running in
  List.iter (fun action ->
    expect "iOS acceptance-gated feature is proposed" (Dashboard.proposed action);
    availability ios action false
  ) [Dashboard.Ios_tree; Dashboard.Ios_performance];
  List.iter (fun action -> expect "baseline action is not proposed" (not (Dashboard.proposed action)))
    [Dashboard.Build; Dashboard.Install; Dashboard.Launch; Dashboard.Stop;
     Dashboard.Observe; Dashboard.Control; Dashboard.Replay; Dashboard.Diagnostics;
     Dashboard.Verify; Dashboard.Save_baseline; Dashboard.Compare_baseline];
  print_endline "workspace mobile dashboard capabilities: ok"
