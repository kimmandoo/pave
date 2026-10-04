module Report = Pave.Workspace_mobile_report
let expect label value = if not value then failwith label
let rejects label action = try ignore (action ()); failwith ("accepted " ^ label) with Report.Error _ -> ()
let put path mode text = let oc=open_out_bin path in output_string oc text; close_out oc; Unix.chmod path mode
let () =
  let root = Filename.temp_file "mobile-report" ".dir" in
  Sys.remove root; Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () ->
    let rec remove path = if Sys.file_exists path then
      if (Unix.lstat path).Unix.st_kind=Unix.S_DIR then (Array.iter (fun x -> remove (Filename.concat path x)) (Sys.readdir path); Unix.rmdir path)
      else Unix.unlink path in remove root) (fun () ->
    let app=Filename.concat root "app.apk" and source=Filename.concat root "Main.kt" and evidence=Filename.concat root "evidence.json" in
    put app 0o644 "build-A"; put source 0o644 "source-A";
    let bundle=Filename.concat root "Sample.app" in Unix.mkdir bundle 0o700;
    put (Filename.concat bundle "Info.plist") 0o644 "bundle-A";
    put evidence 0o600 "{\"private_log\":\"TOKEN_SECRET\",\"media\":\"image-bytes\"}";
    let session = { (Pave.Workspace_mobile_run.select
      (Pave.Workspace_mobile_run.create_manager ())
      ~root ~subroot:"." ~platform:"android" ~device:"serial-1" ~app_id:"dev.example.app"
      ~app_path:"app.apk" ~scheme:None ~variant:(Some "debug") ~activity:None
      ~ios_device_binding:None ~device_ready:true ~scheme_ready:true) with state=Pave.Workspace_mobile_run.Built } in
    let simulator_id = "26ae0000-0000-0000-0000-000000000000" in
    let ios_binding = Some {
      Pave.Workspace_mobile_run.device_session_id = "device-session";
      inventory_id = "inventory";
      simulator_id; target_id = "ios:" ^ simulator_id } in
    let ios_session=Pave.Workspace_mobile_run.select
      (Pave.Workspace_mobile_run.create_manager ())
      ~root ~subroot:"." ~platform:"ios" ~device:simulator_id
      ~app_id:"dev.example.ios" ~app_path:"Sample.app" ~scheme:(Some "Sample")
      ~variant:None ~activity:None ~ios_device_binding:ios_binding
      ~device_ready:true ~scheme_ready:true in
    let bundle_hash= (Report.build_identity root ios_session).build_hash in
    put (Filename.concat bundle "Info.plist") 0o644 "bundle-B";
    expect "app bundle content changes build identity" ((Report.build_identity root ios_session).build_hash <> bundle_hash);
    Unix.symlink "Info.plist" (Filename.concat bundle "linked.plist");
    rejects "app bundle symlink" (fun () -> Report.build_identity root ios_session);
    Unix.unlink (Filename.concat bundle "linked.plist");
    let source_preview=Pave.Workspace_edit.apply_hunks ~root ~path:"Main.kt"
      ~expected_sha256:(Pave.Workspace_edit.sha256 "source-A")
      ~hunks:[{Pave.Workspace_edit.old_text="source-A";new_text="source-B"}] in
    let identity=Report.build_identity root session in
    let digest=Digestif.SHA256.(to_hex (digest_string (let ic=open_in_bin evidence in Fun.protect ~finally:(fun()->close_in ic) (fun()->really_input_string ic (in_channel_length ic))))) in
    let artifact={Report.kind=Report.Diagnostic;path="evidence.json";sha256=digest;identity} in
    let run termination truncated = {Pave.Workspace_process.termination;output="";bytes_received=0;truncated} in
    let report=Report.create ~root ~session ~sources:[source_preview] ~checks:[
      Report.process_check ~name:"approved-test-pass" (run (Pave.Workspace_process.Exited 0) false);
      Report.process_check ~name:"approved-test-fail" (run (Pave.Workspace_process.Exited 1) false);
      Report.not_run_check ~name:"not-run";
      Report.process_check ~name:"partial" (run (Pave.Workspace_process.Exited 0) true)] ~artifacts:[artifact] in
    let projection=Report.projection report in
    let json=Yojson.Basic.from_string projection in
    let checks=Yojson.Basic.Util.(json |> member "checks" |> to_list) in
    let statuses=List.map (fun c -> Yojson.Basic.Util.(c |> member "status" |> to_string)) checks in
    expect "four distinct outcomes" (statuses=["passed";"failed";"not_run";"incomplete"]);
    let foreign={artifact with identity={identity with device="other-device"}} in
    rejects "cross-device/build evidence" (fun () -> Report.create ~root ~session ~sources:[source_preview] ~checks:[] ~artifacts:[foreign]);
    let changed={artifact with sha256=String.make 64 '0'} in
    rejects "stale artifact reference" (fun () -> Report.create ~root ~session ~sources:[source_preview] ~checks:[] ~artifacts:[changed]);
    rejects "source changed after guarded preview" (fun () ->
      put source 0o644 "unexpected"; Report.create ~root ~session ~sources:[source_preview] ~checks:[] ~artifacts:[]);
    put source 0o644 "source-B";
    let cancellation_calls=ref 0 in
    rejects "write cancellation after staging" (fun () ->
      Report.write ~root ~path:"cancelled.json"
        ~cancelled:(fun () -> incr cancellation_calls; !cancellation_calls >= 2) report);
    expect "cancelled staged write leaves no report" (not (Sys.file_exists (Filename.concat root "cancelled.json")));
    Report.write ~root ~path:"report.json" ~cancelled:(fun()->false) report;
    let output=Filename.concat root "report.json" in
    expect "report mode private" ((Unix.stat output).Unix.st_perm land 0o077=0);
    rejects "no overwrite" (fun () -> Report.write ~root ~path:"report.json" ~cancelled:(fun()->false) report);
    let before=Digest.file output in
    expect "no-overwrite preserved report" (Digest.file output=before));
  print_endline "workspace mobile verification report: ok"
