module Run = Pave.Workspace_mobile_run
module Control = Pave.Workspace_mobile_control

let expect label condition = if not condition then failwith label
let rejects label action =
  try ignore (action ()); failwith ("mobile control accepted " ^ label)
  with Control.Error _ -> ()

let session ?(platform = Run.Android) ?(state = Run.Running) ?(device = "emulator-5554") () =
  { Run.id = "mobile-1"; root = "/tmp/mobile"; subroot = "/tmp/mobile";
    platform; device; app_id = "dev.example"; app_path = "app.apk";
    scheme = None; variant = None; activity = None; state; screen_size = None }

let () =
  let app = session () in
  let size = Some (1080, 2400) in
  let adb = "adb -s " ^ Filename.quote app.device in
  expect "tap command" (Control.command app ~screen_size:size (Control.Tap { x = 9; y = 18 }) =
    adb ^ " shell " ^ Filename.quote "input tap 9 18");
  expect "last pixel coordinate accepted" (String.ends_with ~suffix:"input tap 1079 2399'"
    (Control.command app ~screen_size:size (Control.Tap { x = 1079; y = 2399 })));
  expect "swipe command" (Control.command app ~screen_size:size
    (Control.Swipe { x1 = 1; y1 = 2; x2 = 3; y2 = 4; duration_ms = 600 }) =
    adb ^ " shell " ^ Filename.quote "input swipe 1 2 3 4 600");
  expect "back command" (Control.command app ~screen_size:size Control.Back =
    adb ^ " shell " ^ Filename.quote "input keyevent KEYCODE_BACK");
  expect "Back does not need coordinate dimensions"
    (String.ends_with ~suffix:(Filename.quote "input keyevent KEYCODE_BACK")
      (Control.command app ~screen_size:None Control.Back));
  let injection = "a'; touch /tmp/pwned; echo '" in
  let text_command = Control.command app ~screen_size:size (Control.Text injection) in
  let encoded = String.concat "%s" (String.split_on_char ' ' injection) in
  let expected_remote = "input text " ^ Filename.quote encoded in
  expect "text command quotes shell metacharacters" (text_command =
    adb ^ " shell " ^ Filename.quote expected_remote);
  let spaces_remote = "input text " ^ Filename.quote "one%stwo" in
  expect "text spaces encoded" (Control.command app ~screen_size:size
    (Control.Text "one two") = adb ^ " shell " ^ Filename.quote spaces_remote);
  rejects "negative coordinate" (fun () -> Control.command app ~screen_size:size
    (Control.Tap { x = -1; y = 0 }));
  rejects "coordinate at width boundary" (fun () -> Control.command app ~screen_size:size
    (Control.Tap { x = 1080; y = 0 }));
  rejects "coordinate at height boundary" (fun () -> Control.command app ~screen_size:size
    (Control.Tap { x = 0; y = 2400 }));
  rejects "unobserved screen" (fun () -> Control.command app ~screen_size:None
    (Control.Tap { x = 0; y = 0 }));
  rejects "excessive swipe" (fun () -> Control.command app ~screen_size:size
    (Control.Swipe { x1 = 0; y1 = 0; x2 = 1; y2 = 1; duration_ms = 10_001 }));
  rejects "reserved text encoding" (fun () -> Control.command app ~screen_size:size
    (Control.Text "literal%stoken"));
  rejects "control characters" (fun () -> Control.command app ~screen_size:size
    (Control.Text "line\nbreak"));
  rejects "stopped session" (fun () -> Control.command (session ~state:Run.Stopped ())
    ~screen_size:size Control.Back);
  rejects "iOS simulator unsupported control" (fun () -> Control.command
    (session ~platform:Run.Ios ()) ~screen_size:size Control.Back);
  print_endline "workspace mobile UI control: ok"
