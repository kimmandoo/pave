module Run = Pave.Workspace_mobile_run
module Lifecycle = Pave.Workspace_mobile_app_lifecycle
module Observe = Pave.Workspace_mobile_observe

let expect label condition = if not condition then failwith ("mobile app lifecycle: " ^ label)
let rejects label action =
  try ignore (action ()); failwith ("accepted " ^ label)
  with Lifecycle.Error _ -> ()

let session ?(platform = Run.Android) ?(state = Run.Running) ?(activity = Some "dev.example/.MainActivity") () =
  { Run.id = "mobile-1"; root = "/tmp/mobile"; subroot = "/tmp/mobile";
    platform; device = "emulator-5554"; app_id = "dev.example";
    app_path = "app/build/app.apk"; scheme = Some "demo"; variant = Some "debug";
    activity; ios_device_binding = None; screen_size = None; state }

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.iter (fun entry -> remove_tree (Filename.concat path entry));
      Unix.rmdir path
  | _ -> Unix.unlink path


let () =
  let selected = session () in
  let manifest =
    "<manifest package=\"dev.example\"><application><activity android:name=\".LinkActivity\" android:exported=\"true\"><intent-filter>" ^
    "<action android:name=\"android.intent.action.VIEW\"/>" ^
    "<category android:name=\"android.intent.category.DEFAULT\"/>" ^
    "<category android:name=\"android.intent.category.BROWSABLE\"/>" ^
    "<data android:scheme=\"demo\" android:host=\"links.example.test\" android:path=\"/item\"/>" ^
    "</intent-filter></activity></application></manifest>" in
  let handlers = Lifecycle.manifest_handlers ~app_id:"dev.example" manifest in
  expect "manifest evidence identifies the exact selected-app activity and URL"
    (handlers = [{Lifecycle.app_id="dev.example";
      activity="dev.example/dev.example.LinkActivity";
      scheme="demo"; host="links.example.test"; path="/item"}]);
  let unexported =
    "<manifest package=\"dev.example\"><application><activity android:name=\".LinkActivity\" android:exported=\"false\"><intent-filter>" ^
    "<action android:name=\"android.intent.action.VIEW\"/>" ^
    "<category android:name=\"android.intent.category.DEFAULT\"/>" ^
    "<category android:name=\"android.intent.category.BROWSABLE\"/>" ^
    "<data android:scheme=\"demo\" android:host=\"links.example.test\" android:path=\"/item\"/>" ^
    "</intent-filter></activity></application></manifest>" in
  expect "unexported activities are never treated as external deep-link destinations"
    (Lifecycle.manifest_handlers ~app_id:"dev.example" unexported = []);
  rejects "duplicate application scopes" (fun () ->
    Lifecycle.manifest_handlers ~app_id:"dev.example"
      "<manifest package=\"dev.example\"><application/><application/></manifest>");
  let broad =
    "<manifest package=\"dev.example\"><application><activity android:name=\".LinkActivity\" android:exported=\"true\"><intent-filter>" ^
    "<action android:name=\"android.intent.action.VIEW\"/>" ^
    "<category android:name=\"android.intent.category.DEFAULT\"/>" ^
    "<category android:name=\"android.intent.category.BROWSABLE\"/>" ^
    "<data android:scheme=\"demo\" android:host=\"links.example.test\" android:pathPrefix=\"/\"/>" ^
    "</intent-filter></activity></application></manifest>" in
  expect "path-prefix filter is not treated as an exact handler"
    (Lifecycle.manifest_handlers ~app_id:"dev.example" broad = []);
  rejects "foreign manifest package" (fun () ->
    Lifecycle.manifest_handlers ~app_id:"dev.example"
      (String.concat "" [
        "<manifest package=\"dev.foreign\"><application/></manifest>"]));
  rejects "misplaced intent filter" (fun () ->
    Lifecycle.manifest_handlers ~app_id:"dev.example"
      "<manifest package=\"dev.example\"><application><intent-filter/></application></manifest>");
  let identity_sample = Lifecycle.identity_of_session (session ()) ~build_id:"build-a" in
  let foreground = Lifecycle.parse_observation ~identity:identity_sample ~generation:1
    "mResumedActivity: ActivityRecord{1 u0 dev.example/.MainActivity t1}\nPAVE_LIFECYCLE_PID=123\nPAVE_LIFECYCLE_STATUS=ok\n" in
  expect "selected activity and exact PID form a foreground observation"
    (foreground.state = Lifecycle.Foreground && foreground.process_id = Some 123 &&
     foreground.resumed_activity = Some "dev.example/dev.example.MainActivity");
  let collision = Lifecycle.parse_observation ~identity:identity_sample ~generation:2
    "mResumedActivity: ActivityRecord{1 u0 dev.example.other/.MainActivity t1}\nPAVE_LIFECYCLE_PID=123\nPAVE_LIFECYCLE_STATUS=ok\n" in
  expect "package-prefix collision is not foreground evidence"
    (collision.state = Lifecycle.Backgrounded);
  rejects "failed lifecycle command cannot parse as a valid state" (fun () ->
    Lifecycle.parse_observation ~identity:identity_sample ~generation:3
      "mResumedActivity: ActivityRecord{1 u0 dev.example/.MainActivity t1}\nPAVE_LIFECYCLE_PID=123\nPAVE_LIFECYCLE_STATUS=failed\n");
  rejects "missing lifecycle command status is fail-closed" (fun () ->
    Lifecycle.parse_observation ~identity:identity_sample ~generation:3
      "PAVE_LIFECYCLE_PID=123\n");
  rejects "malformed live PID marker" (fun () ->
    Lifecycle.parse_observation ~identity:identity_sample ~generation:3
      "PAVE_LIFECYCLE_PID=not-a-pid\nPAVE_LIFECYCLE_STATUS=ok\n");
  rejects "empty PID lookup cannot produce a valid lifecycle observation" (fun () ->
    Lifecycle.parse_observation ~identity:identity_sample ~generation:3
      "PAVE_LIFECYCLE_PID=\nPAVE_LIFECYCLE_STATUS=ok\n");
  let handler = { Lifecycle.app_id = "dev.example";
    activity = "dev.example/dev.example.LinkActivity";
    scheme = "demo"; host = "links.example.test"; path = "/item" } in
  let command, link = Lifecycle.deep_link_preview ~approved_evidence:true selected
      ~handler ~url:"demo://links.example.test/item" in
  expect "exact registered handler returns exact link" (link.url = "demo://links.example.test/item");
  expect "deep-link command explicitly targets the verified handler activity"
    ((try ignore (Str.search_forward (Str.regexp_string "am start -W") command 0);
          ignore (Str.search_forward
            (Str.regexp_string "android.intent.action.VIEW") command 0);
          ignore (Str.search_forward
            (Str.regexp_string "-n dev.example/dev.example.LinkActivity") command 0);
          true
      with Not_found -> false));
  rejects "unapproved handler evidence" (fun () ->
    Lifecycle.deep_link_preview ~approved_evidence:false selected ~handler
      ~url:"demo://links.example.test/item");
  rejects "foreign scheme" (fun () ->
    Lifecycle.deep_link_preview ~approved_evidence:true selected ~handler
      ~url:"other://links.example.test/item");
  rejects "foreign host" (fun () ->
    Lifecycle.deep_link_preview ~approved_evidence:true selected ~handler
      ~url:"demo://foreign.example.test/item");
  rejects "foreign path" (fun () ->
    Lifecycle.deep_link_preview ~approved_evidence:true selected ~handler
      ~url:"demo://links.example.test/other");
  rejects "foreign app handler" (fun () ->
    Lifecycle.deep_link_preview ~approved_evidence:true selected
      ~handler:{ handler with app_id = "dev.foreign" }
      ~url:"demo://links.example.test/item");
  rejects "delegated iOS deep link" (fun () ->
    Lifecycle.deep_link_preview ~approved_evidence:true (session ~platform:Run.Ios ())
      ~handler ~url:"demo://links.example.test/item");
  let identity = Lifecycle.identity_of_session selected ~build_id:"sha256:build-a" in
  let node ?(package = "dev.example") ?(text = "") ?(description = "") () : Observe.node =
    { index = 0; parent = None; depth = 0; role = "android.widget.TextView"; package;
      text; description; identifier = ""; bounds = "[0,0][100,20]";
      clickable = false; scrollable = false; enabled = true; selected = false } in
  let accessibility nodes generation identity : Lifecycle.deep_link_accessibility_observation =
    { identity; generation; nodes } in
  let verified = Lifecycle.verify_deep_link_destination ~identity ~generation:5
      ~link ~observation:(accessibility
        [node ~text:"Item destination: item-42" (); node ~text:"item-42" ()] 5 identity)
      ~assertion:"item-42" in
  expect "exact fresh accessibility node verifies destination"
    (verified.assertion = "item-42" && verified.link.url = link.url);
  rejects "substring is not an exact destination node" (fun () ->
    Lifecycle.verify_deep_link_destination ~identity ~generation:5 ~link
      ~observation:(accessibility [node ~text:"Item destination: item-42" ()] 5 identity)
      ~assertion:"item-42");
  rejects "stale accessibility generation" (fun () ->
    Lifecycle.verify_deep_link_destination ~identity ~generation:5 ~link
      ~observation:(accessibility [node ~text:"item-42" ()] 4 identity) ~assertion:"item-42");
  let foreign_device = { identity with Lifecycle.device = "emulator-foreign" } in
  rejects "wrong accessibility device" (fun () ->
    Lifecycle.verify_deep_link_destination ~identity ~generation:5 ~link
      ~observation:(accessibility [node ~text:"item-42" ()] 5 foreign_device) ~assertion:"item-42");
  let foreign_app = { identity with Lifecycle.app_id = "dev.foreign" } in
  rejects "wrong accessibility app" (fun () ->
    Lifecycle.verify_deep_link_destination ~identity ~generation:5 ~link
      ~observation:(accessibility [node ~text:"item-42" ()] 5 foreign_app) ~assertion:"item-42");
  let foreign_build = { identity with Lifecycle.build_id = "sha256:build-b" } in
  rejects "wrong accessibility build" (fun () ->
    Lifecycle.verify_deep_link_destination ~identity ~generation:5 ~link
      ~observation:(accessibility [node ~text:"item-42" ()] 5 foreign_build) ~assertion:"item-42");
  rejects "matching text from a foreign package" (fun () ->
    Lifecycle.verify_deep_link_destination ~identity ~generation:5 ~link
      ~observation:(accessibility [node ~package:"android" ~text:"item-42" ()] 5 identity)
      ~assertion:"item-42");
  rejects "missing destination assertion" (fun () ->
    Lifecycle.verify_deep_link_destination ~identity ~generation:5 ~link
      ~observation:(accessibility [node ~text:"another destination" ()] 5 identity)
      ~assertion:"item-42");
  rejects "ambiguous destination nodes" (fun () ->
    Lifecycle.verify_deep_link_destination ~identity ~generation:5 ~link
      ~observation:(accessibility
        [node ~text:"item-42" (); node ~description:"item-42" ()] 5 identity)
      ~assertion:"item-42");
  rejects "empty destination assertion" (fun () ->
    Lifecycle.verify_deep_link_destination ~identity ~generation:5 ~link
      ~observation:(accessibility [node ~text:"item-42" ()] 5 identity) ~assertion:"");
  rejects "oversized destination assertion" (fun () ->
    Lifecycle.verify_deep_link_destination ~identity ~generation:5 ~link
      ~observation:(accessibility [node ~text:"item-42" ()] 5 identity)
      ~assertion:(String.make 513 'x'));
  let identity = Lifecycle.identity_of_session selected ~build_id:"sha256:build-a" in
  let observation ?(pid = 100) state identity generation =
    { Lifecycle.identity; state; generation; process_id = Some pid;
      resumed_activity = (if state = Lifecycle.Foreground then
        Some "dev.example/dev.example.MainActivity" else None) } in
  let record = Lifecycle.create_record ~name:"restore-check" ~identity in
  record.generation <- 0;
  record.process_id <- Some 100;
  let calls = ref [] in
  rejects "denied transition" (fun () ->
    Lifecycle.run_transition record ~approved:false
      ~observation:(observation Lifecycle.Foreground identity 1)
      ~activity:"dev.example/.MainActivity" ~run:(fun command -> calls := command :: !calls)
      ~observe:(fun () -> observation Lifecycle.Backgrounded identity 2)
      Lifecycle.Background);
  expect "denial has no effect" (!calls = [] && record.state = Lifecycle.Foreground);
  let other_build = { identity with Lifecycle.build_id = "sha256:build-b" } in
  rejects "stale build observation" (fun () ->
    Lifecycle.run_transition record ~approved:true
      ~observation:(observation Lifecycle.Foreground other_build 1)
      ~activity:"dev.example/.MainActivity" ~run:(fun command -> calls := command :: !calls)
      ~observe:(fun () -> observation Lifecycle.Backgrounded identity 2)
      Lifecycle.Background);
  expect "identity rejection has no effect" (!calls = [] && record.state = Lifecycle.Foreground);
  let background = Lifecycle.run_transition record ~approved:true
      ~observation:(observation Lifecycle.Foreground identity 1)
      ~activity:"dev.example/.MainActivity" ~run:(fun command -> calls := command :: !calls)
      ~observe:(fun () -> observation Lifecycle.Backgrounded identity 2)
      Lifecycle.Background in
  expect "background state transition is recorded" (record.state = Lifecycle.Backgrounded &&
    background.state = Lifecycle.Backgrounded && Lifecycle.same_identity background.identity identity);
  expect "background command only backgrounds selected device" (List.hd !calls =
    "adb -s " ^ Filename.quote "emulator-5554" ^ " shell input keyevent KEYCODE_HOME");
  rejects "replayed lifecycle observation" (fun () ->
    Lifecycle.run_transition record ~approved:true
      ~observation:(observation Lifecycle.Backgrounded identity 2)
      ~activity:"dev.example/.MainActivity" ~run:(fun command -> calls := command :: !calls)
      ~observe:(fun () -> observation Lifecycle.Foreground identity 3)
      Lifecycle.Resume);
  expect "stale observation has no effect" (record.state = Lifecycle.Backgrounded && List.length !calls = 1);
  let resumed = Lifecycle.run_transition record ~approved:true
      ~observation:(observation Lifecycle.Backgrounded identity 3)
      ~activity:"dev.example/.MainActivity" ~run:(fun command -> calls := command :: !calls)
      ~observe:(fun () -> observation Lifecycle.Foreground identity 4)
      Lifecycle.Resume in
  expect "resume transitions the selected app to foreground"
    (resumed.state = Lifecycle.Foreground && record.state = Lifecycle.Foreground &&
     List.hd !calls = "adb -s " ^ Filename.quote "emulator-5554" ^
       " shell am start -W -n dev.example/.MainActivity");
  let before_resume = List.length !calls in
(try
   ignore (Lifecycle.run_transition record ~approved:true
     ~observation:(observation Lifecycle.Foreground identity 5)
     ~activity:"dev.example/.MainActivity"
     ~run:(fun command -> calls := command :: !calls; failwith "first command failed")
     ~observe:(fun () -> observation Lifecycle.Foreground identity 6)
     Lifecycle.Recreate_process);
   failwith "process recreation did not propagate first command failure"
 with Failure message when message = "first command failed" -> ());
  expect "first failed command stops process recreation without retry"
    (List.length !calls = before_resume + 1 && match record.state with Lifecycle.Failed _ -> true | _ -> false);
  let recreated = Lifecycle.create_record ~name:"process-check" ~identity in
  recreated.generation <- 0;
  recreated.process_id <- Some 100;
  let recreation_commands = ref [] in
  let recreated_observation = Lifecycle.run_transition recreated ~approved:true
      ~observation:(observation Lifecycle.Foreground identity 1)
      ~activity:"dev.example/.MainActivity"
      ~run:(fun command -> recreation_commands := command :: !recreation_commands)
      ~observe:(fun () -> observation ~pid:101 Lifecycle.Foreground identity 2)
      Lifecycle.Recreate_process in
  expect "process recreation is bound to the selected app and build"
    (recreated.state = Lifecycle.Process_recreated &&
     Lifecycle.same_identity recreated_observation.identity identity &&
     List.length !recreation_commands = 2 &&
     List.exists (fun command ->
       try
         ignore (Str.search_forward
           (Str.regexp_string ("am force-stop " ^ Filename.quote "dev.example"))
           command 0);
         true
       with Not_found -> false) !recreation_commands);
  let same_pid = Lifecycle.create_record ~name:"same-pid" ~identity in
  same_pid.generation <- 0;
  same_pid.process_id <- Some 100;
  let same_pid_commands = ref [] in
  rejects "unchanged PID is not proof of process recreation" (fun () ->
    Lifecycle.run_transition same_pid ~approved:true
      ~observation:(observation Lifecycle.Foreground identity 1)
      ~activity:"dev.example/.MainActivity"
      ~run:(fun command -> same_pid_commands := command :: !same_pid_commands)
      ~observe:(fun () -> observation ~pid:100 Lifecycle.Foreground identity 2)
      Lifecycle.Recreate_process);
  expect "process recreation failure is retained without retry"
    (List.length !same_pid_commands = 2 &&
     match same_pid.state with Lifecycle.Failed _ -> true | _ -> false);
  expect "process recreation documents volatile-state loss without clearing data"
    (let text = Lifecycle.data_loss_description Lifecycle.Recreate_process in
     String.length text > 0 && (try ignore (Str.search_forward (Str.regexp_string "not cleared") text 0); true with Not_found -> false));
  let root = Filename.temp_file "pave-mobile-lifecycle-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    let stored = Lifecycle.create_record ~name:"local-record" ~identity in
    stored.generation <- 0;
    stored.process_id <- Some 100;
    Lifecycle.save ~root stored;
    let loaded = Lifecycle.load ~root "local-record" in
    expect "local record persists build/app/device/process identity"
      (Lifecycle.same_identity loaded.identity identity &&
       loaded.state = Lifecycle.Foreground && loaded.process_id = Some 100);
    loaded.state <- Lifecycle.Backgrounded;
    loaded.generation <- 1;
    Lifecycle.update ~root loaded;
    expect "lifecycle state persists in owned local record"
      ((Lifecycle.load ~root "local-record").state = Lifecycle.Backgrounded);
    expect "scenario listing is bounded and deterministic"
      (List.map (fun record -> record.Lifecycle.name) (Lifecycle.list ~root) = ["local-record"]);
    let stale = { loaded with Lifecycle.generation = 0; state = Lifecycle.Foreground } in
    rejects "stale lifecycle update" (fun () -> Lifecycle.update ~root stale);
    let retargeted = { loaded with Lifecycle.identity = other_build } in
    rejects "scenario retargeting to another build" (fun () -> Lifecycle.update ~root retargeted);
    Lifecycle.delete ~root "local-record";
    expect "explicit delete removes only the selected private record" (Lifecycle.list ~root = []));
  print_endline "workspace mobile app deep links and lifecycle transitions: ok"
