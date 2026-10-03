module Run = Pave.Workspace_mobile_run

let expect label condition = if not condition then failwith label
let rejects label f =
  try ignore (f ()); failwith ("mobile session accepted " ^ label)
  with Run.Error _ -> ()

let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec search i = i + m <= n &&
    (String.sub text i m = fragment || search (i + 1)) in
  search 0

let rec remove_tree path =
  try match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path |> Array.iter (fun name ->
          remove_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Unix.unlink path
  with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let () =
  let root = Filename.temp_file "pave-mobile-run-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  let root = Unix.realpath root in
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    Unix.mkdir (Filename.concat root "apps") 0o700;
    let apk = Filename.concat root "apps/My Fixture.apk" in
    let channel = open_out_bin apk in
    output_string channel "disposable apk";
    close_out channel;
    let app = Filename.concat root "My.app" in
    Unix.mkdir app 0o700;
    let manager = Run.create_manager () in
    let session = Run.select manager ~root ~subroot:"android"
      ~platform:"android" ~device:"emulator-5554"
      ~app_id:"com.example.fixture" ~app_path:"apps/My Fixture.apk"
      ~scheme:None ~variant:(Some "debug")
      ~activity:(Some "com.example.fixture/.MainActivity")
      ~device_ready:true ~scheme_ready:false in
    expect "session starts selected" (session.Run.state = Run.Selected);
    expect "session carries variant" (contains (Run.render session) "variant debug");
    expect "not listed on another manager" (Run.sessions (Run.create_manager ()) = []);
    let attempts = ref [] in
    let runner ~root ~command =
      attempts := (root, command) :: !attempts;
      "success\n" in
    rejects "external action without approval" (fun () ->
      Run.execute manager ~approved:false ~run:runner ~action:"install" ~id:session.id);
    expect "denial invokes no command" (!attempts = []);
    expect "denial leaves selected state" (session.state = Run.Selected);
    let install = Run.command "install" session in
    expect "APK path shell-quoted" (install =
      "adb -s " ^ Filename.quote session.device ^ " install -r " ^
        Filename.quote (Filename.concat root session.app_path));
    let output = Run.execute manager ~approved:true ~run:runner
      ~action:"install" ~id:session.id in
    expect "install output retained" (output = "success\n");
    expect "install changes state only after success" (session.state = Run.Installed);
    ignore (Run.execute manager ~approved:true ~run:runner
      ~action:"launch" ~id:session.id);
    expect "Android launch uses explicit selected component"
      (Run.command "launch" session =
        "adb -s " ^ Filename.quote session.device ^
        " shell am start -W -n " ^
        Filename.quote "com.example.fixture/.MainActivity");
    ignore (Run.execute manager ~approved:true ~run:runner
      ~action:"stop" ~id:session.id);
    expect "stop sets stopped" (session.state = Run.Stopped);
    expect "stop targets only selected package"
      (Run.command "stop" session =
        "adb -s " ^ Filename.quote session.device ^ " shell am force-stop " ^
          Filename.quote session.app_id);
    let attempts_before = List.length !attempts in
    rejects "stop from stopped state" (fun () ->
      Run.execute manager ~approved:true ~run:runner ~action:"stop" ~id:session.id);
    expect "invalid transition invokes no command"
      (List.length !attempts = attempts_before);
    rejects "shell-injected app ID" (fun () -> Run.select manager ~root
      ~subroot:"android" ~platform:"android" ~device:"emulator-5554"
      ~app_id:"com.example;touch" ~app_path:"apps/My Fixture.apk"
      ~scheme:None ~variant:None ~activity:None ~device_ready:true ~scheme_ready:false);
    rejects "cross-package launch activity" (fun () -> Run.select manager ~root
      ~subroot:"android" ~platform:"android" ~device:"emulator-5554"
      ~app_id:"com.example.fixture" ~app_path:"apps/My Fixture.apk"
      ~scheme:None ~variant:None ~activity:(Some "com.other/.Activity")
      ~device_ready:true ~scheme_ready:false);
    rejects "wrong artifact extension" (fun () -> Run.select manager ~root
      ~subroot:"android" ~platform:"android" ~device:"emulator-5554"
      ~app_id:"com.example.other" ~app_path:"My.app"
      ~scheme:None ~variant:None ~activity:None ~device_ready:true ~scheme_ready:false);
    let built = Run.mark_built manager ~id:session.id in
    expect "successful build state accepted" (built.state = Run.Built);
    expect "built state rendered" (contains (Run.render built) "android · built");
    let missing = Run.select manager ~root ~subroot:"android"
      ~platform:"android" ~device:"emulator-5554"
      ~app_id:"com.example.built" ~app_path:"future.apk"
      ~scheme:None ~variant:(Some "release") ~activity:None
      ~device_ready:true ~scheme_ready:false in
    rejects "marking absent artifact built" (fun () ->
      Run.mark_built manager ~id:missing.id);
    let ios = Run.select manager ~root ~subroot:"ios/App.xcodeproj"
      ~platform:"ios" ~device:"26ae0000-0000-0000-0000-000000000000"
      ~app_id:"com.example.ios" ~app_path:"My.app" ~scheme:(Some "App")
      ~variant:None ~activity:None ~device_ready:true ~scheme_ready:true in
    expect "iOS install selects exact simulator" (contains
      (Run.command "install" ios) "simctl install '26ae0000-0000-0000-0000-000000000000'");
    expect "iOS launch selects exact bundle" (contains
      (Run.command "launch" ios) "simctl launch '26ae0000-0000-0000-0000-000000000000' 'com.example.ios'");
    for _index = 4 to 12 do
      ignore (Run.select manager ~root ~subroot:"android"
        ~platform:"android" ~device:"emulator-5554"
        ~app_id:"com.example.fixture" ~app_path:"apps/My Fixture.apk"
        ~scheme:None ~variant:None ~activity:None
        ~device_ready:true ~scheme_ready:false)
    done;
    let session_ids = Run.sessions manager |> List.map (fun row -> row.Run.id) in
    expect "session rows are sorted by descending numeric ID"
      (session_ids = List.init 12 (fun index ->
        Printf.sprintf "mobile-%d" (12 - index)));
    rejects "iOS selection without scheme inventory" (fun () ->
      Run.select manager ~root ~subroot:"ios/App.xcodeproj" ~platform:"ios"
        ~device:"26ae0000-0000-0000-0000-000000000000"
        ~app_id:"com.example.ios" ~app_path:"My.app" ~scheme:(Some "App")
        ~variant:None ~activity:None ~device_ready:true ~scheme_ready:false);
    Run.close_manager manager;
    rejects "closed session manager" (fun () -> Run.sessions manager));
  print_endline "workspace mobile app sessions: ok"
