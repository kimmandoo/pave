module Run = Pave.Workspace_mobile_run
module Scenario = Pave.Workspace_mobile_scenario
module Control = Pave.Workspace_mobile_control

let expect label condition = if not condition then failwith ("mobile scenario: " ^ label)
let rejects label action =
  try ignore (action ()); failwith ("mobile scenario accepted " ^ label)
  with Scenario.Error _ -> ()

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.iter (fun entry -> remove_tree (Filename.concat path entry));
      Unix.rmdir path
  | _ -> Unix.unlink path

let session ?(root = "/tmp/mobile") ?(device = "emulator-5554") ?(state = Run.Running) () =
  { Run.id = "mobile-1"; root; subroot = "app";
    platform = Run.Android; device; app_id = "dev.example";
    app_path = "app.apk"; scheme = None; variant = Some "debug";
    activity = None; ios_device_binding = None; state; screen_size = Some (1080, 2400) }

let tree description = Yojson.Basic.to_string (`Assoc [
  "status", `String "available";
  "node_count", `Int 1;
  "nodes", `List [`Assoc [
    "role", `String "android.widget.TextView";
    "text", `String "Counter 1";
    "description", `String description;
    "identifier", `String "dev.example:id/counter"]]
])

let () =
  let root = Filename.temp_file "pave-mobile-scenario-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  let root = Unix.realpath root in
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    let session ?device ?state () = session ~root ?device ?state () in
    let artifact = Filename.concat root "app.apk" in
    let save_artifact bytes =
      let channel = open_out_bin artifact in
      Fun.protect ~finally:(fun () -> close_out_noerr channel)
        (fun () -> output_string channel bytes) in
    save_artifact "selected-build";
    let step = Scenario.step_of_json (`Assoc [
      "action", `String "tap"; "x", `Int 120; "y", `Int 340;
      "expected_field", `String "description"; "expected_value", `String "count:1"])
    in
    let record = Scenario.save ~root ~name:"counter" ~session:(session ()) [step] in
    let path = Filename.concat root ".pave/mobile-scenarios/counter.json" in
    let file_stat = Unix.lstat path in
    let directory_stat = Unix.lstat (Filename.dirname path) in
    expect "private record permissions" (file_stat.Unix.st_perm land 0o077 = 0);
    expect "private scenario directory permissions" (directory_stat.Unix.st_perm land 0o077 = 0);
    expect "record identity is exact" (Scenario.same_identity record.identity
      (Scenario.identity_of_session (session ())));
    expect "device mismatch detected" (not (Scenario.same_identity record.identity
      (Scenario.identity_of_session (session ~device:"emulator-5556" ()))));
    expect "versioned JSON persisted" (String.starts_with ~prefix:"{\"version\":2"
      (Pave.Workspace_path.read_bounded path Scenario.max_record_bytes));
    save_artifact "another-build";
    expect "same artifact path with different build bytes is refused"
      (not (Scenario.same_identity record.identity
        (Scenario.identity_of_session (session ()))));
    save_artifact "selected-build";
    expect "different project is refused"
      (not (Scenario.same_identity record.identity
        (Scenario.identity_of_session { (session ()) with subroot = "another" })));
    let legacy = Scenario.to_json record |> function
      | `Assoc fields -> `Assoc (("version", `Int 1) :: List.remove_assoc "version" fields)
      | _ -> assert false in
    rejects "legacy record without exact build provenance" (fun () ->
      Scenario.from_json "counter" (Yojson.Basic.to_string legacy));
    rejects "duplicate scenario names" (fun () -> Scenario.save ~root ~name:"counter"
      ~session:(session ()) [step]);
    let loaded = Scenario.load ~root "counter" in
    expect "stored steps loaded" (List.length loaded.steps = 1);
    Scenario.reset loaded;
    expect "replay begins at step zero" (Scenario.current_step loaded = step);
    Scenario.mark_awaiting loaded;
    expect "matching accessibility assertion" (Scenario.verify_tree step (tree "count:1"));
    expect "mismatch does not pass" (not (Scenario.verify_tree step (tree "count:0")));
    Scenario.mark_failed loaded "assertion failed";
    Scenario.update ~root loaded;
    let failed = Scenario.load ~root "counter" in
    expect "first failed assertion persisted" (Scenario.phase_name failed.phase = "failed" &&
      String.starts_with ~prefix:"step 1 failed" (Scenario.phase_detail failed.phase));
    rejects "implicit retry after failure" (fun () -> ignore (Scenario.current_step failed));
    Scenario.reset failed;
    Scenario.mark_awaiting failed;
    expect "valid assertion passes" (Scenario.verify_tree step (tree "count:1"));
    Scenario.advance failed;
    Scenario.update ~root failed;
    expect "final step completion persisted"
      (Scenario.phase_name (Scenario.load ~root "counter").phase = "complete");
    expect "scenario appears in listing" (List.map (fun item -> item.Scenario.name)
      (Scenario.list ~root) = ["counter"]);
    rejects "mismatched accessibility count" (fun () ->
      ignore (Scenario.verify_tree step (Yojson.Basic.to_string (`Assoc [
        "status", `String "available"; "node_count", `Int 2;
        "nodes", `List [`Assoc ["description", `String "count:1"]]]))));
    rejects "invalid replay JSON" (fun () -> ignore (Scenario.verify_tree step "{"));
    rejects "invalid scenario name" (fun () -> Scenario.load ~root "../counter");
    (try
       ignore (Control.command (session ~state:Run.Stopped ()) ~screen_size:None step.action);
       failwith "mobile scenario accepted a stopped session control"
     with Control.Error _ -> ());
    Scenario.delete ~root "counter";
    rejects "deleted record" (fun () -> Scenario.load ~root "counter");
    print_endline "workspace mobile scenario persistence and assertions: ok")
