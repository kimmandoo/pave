module Portal = Pave.Workspace_portal
module Process = Pave.Workspace_process

let contains haystack needle =
  let hay_len = String.length haystack and n_len = String.length needle in
  let rec scan index =
    if index + n_len > hay_len then false
    else if String.sub haystack index n_len = needle then true
    else scan (index + 1) in
  scan 0

let with_fake_portal body =
  let dir = Filename.get_temp_dir_name () in
  let path = Filename.temp_file ~temp_dir:dir "fake-portal-" ".sh" in
  let oc = open_out path in
  output_string oc
    "#!/bin/sh\nif [ \"$1\" = \"expose\" ]; then\n\
     \techo '{\"level\":\"info\",\"public_url\":\"https://demo.test-relay.example\",\"time\":\"now\",\"message\":\"service ready at https://demo.test-relay.example\"}'\n\
     \twhile true; do sleep 1; done\n\
     else\n\techo 'unknown command' >&2; exit 64\nfi\n";
  close_out oc;
  Unix.chmod path 0o700;
  let previous = Sys.getenv_opt Portal.portal_variable in
  Unix.putenv Portal.portal_variable path;
  Fun.protect ~finally:(fun () ->
    (match previous with
     | Some value -> Unix.putenv Portal.portal_variable value
     | None -> Unix.putenv Portal.portal_variable "");
    try Sys.remove path with Sys_error _ -> ()) body

let test_detect_and_url () =
  with_fake_portal (fun () ->
    let executable = Portal.detect_portal () in
    assert (String.ends_with ~suffix:".sh" executable);
    let text = Printf.sprintf
      "prelude\nINF service ready at https://alpha.relay.example tail\n"
      in
    assert (Portal.public_url text = Some "https://alpha.relay.example");
    let json = "{\"level\":\"info\",\"public_url\":\"https://beta.relay.example\",\"message\":\"x\"}" in
    assert (Portal.public_url json = Some "https://beta.relay.example");
    assert (Portal.public_url "no url here" = None))

let test_publish_list_stop () =
  with_fake_portal (fun () ->
    let manager = Process.create_manager () in
    Fun.protect ~finally:(fun () -> Process.close_manager manager) (fun () ->
      let json = Portal.publish manager ~id:"portal:demo" ~port:3000
        ~name:"demo" in
      let text = Yojson.Basic.to_string json in
      assert (contains text "https://demo.test-relay.example");
      assert (contains text "\"published\"");
      let listed = Yojson.Basic.to_string (Portal.list manager) in
      assert (contains listed "demo");
      assert (contains listed "running");
      let stopped = Yojson.Basic.to_string (Portal.stop manager ~id:"portal:demo") in
      assert (contains stopped "stopped")))

(* A stopped or failed tunnel leaves a finished job record; publishing the
   same name again must replace it, while a live tunnel keeps refusing. *)
let test_republish_after_stop () =
  with_fake_portal (fun () ->
    let manager = Process.create_manager () in
    Fun.protect ~finally:(fun () -> Process.close_manager manager) (fun () ->
      let publish () =
        Yojson.Basic.to_string
          (Portal.publish manager ~id:"portal:again" ~port:3000 ~name:"again") in
      assert (contains (publish ()) "published");
      (try ignore (publish ()); assert false with Portal.Error _ -> ());
      ignore (Portal.stop manager ~id:"portal:again");
      assert (contains (publish ()) "published");
      ignore (Portal.stop manager ~id:"portal:again")))

let test_publish_name_validation () =
  assert (Portal.publish_name "my-app-1" = "my-app-1");
  (try ignore (Portal.publish_name "bad name"); assert false
   with Portal.Error _ -> ());
  (try ignore (Portal.publish_name ""); assert false
   with Portal.Error _ -> ())

let () =
  test_detect_and_url ();
  test_publish_list_stop ();
  test_republish_after_stop ();
  test_publish_name_validation ();
  print_endline "workspace_portal: ok"
