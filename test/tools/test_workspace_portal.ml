module Portal = Pave.Workspace_portal
module Process = Pave.Workspace_process

let with_dir body =
  let dir = Filename.temp_dir "pave-portal-test-" "" in
  let rec remove path =
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR -> Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path); Unix.rmdir path
    | _ -> Unix.unlink path in
  Fun.protect ~finally:(fun () -> remove dir) (fun () -> body dir)

let script dir name body =
  let path = Filename.concat dir name in
  let oc = open_out_bin path in
  output_string oc ("#!/bin/sh\n" ^ body); close_out oc;
  Unix.chmod path 0o700; path

let env portal key = if key = "PAVE_PORTAL" then Some portal else Sys.getenv_opt key
let with_listener body =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)); Unix.listen fd 16;
    let port = match Unix.getsockname fd with Unix.ADDR_INET (_, port) -> port | _ -> assert false in
    body port)

let with_manager body =
  let manager = Process.create_manager () in
  Fun.protect ~finally:(fun () -> Process.close_manager manager) (fun () -> body manager)

let field key json = Pave.Protocol.member key json
let rejected body = match body () with
  | _ -> assert false
  | exception Portal.Error _ -> ()

(* This fixture implements Portal's persisted-identity name precedence, so
   reusing identity.json across prefixes produces the wrong public endpoint. *)
let identity_portal dir = script dir "portal" {|name=''
identity='identity.json'
while [ "$#" -gt 0 ]; do
  case "$1" in
    --name) name="$2"; shift 2 ;;
    --identity-path) identity="$2"; shift 2 ;;
    *) shift ;;
  esac
done
if [ -f "$identity" ]; then
  name=$(/usr/bin/tr -d '\n' < "$identity" | /usr/bin/cut -d '"' -f 4)
else
  printf '{"name":"%s"}\n' "$name" > "$identity"
  chmod 600 "$identity"
fi
printf '{ "message": "service ready at https://%s.relay.example", "public_url": "https://%s.relay.example" }\n' "$name" "$name"
while true; do sleep 1; done
|}

let test_prefixes_and_lifecycle () = with_dir (fun dir ->
  let portal = identity_portal dir in
  with_listener (fun port -> with_manager (fun manager ->
    let publish name = Portal.publish ~env:(env portal) ~state_dir:dir manager
      ~id:(Portal.job_id name) ~port ~name in
    let first = publish "first" in
    let second = publish "second" in
    assert (field "url" first = `String "https://first.relay.example");
    assert (field "url" second = `String "https://second.relay.example");
    assert (field "identity_path" first <> field "identity_path" second);
    rejected (fun () -> publish "first");
    ignore (Portal.stop manager ~id:(Portal.job_id "first"));
    assert (field "url" (publish "first") = `String "https://first.relay.example");
    let rows = match field "tunnels" (Portal.list manager) with `List rows -> rows | _ -> assert false in
    assert (List.exists (fun row -> field "name" row = `String "second" && field "status" row = `String "running") rows);
    ignore (Portal.stop manager ~id:(Portal.job_id "first"));
    ignore (Portal.stop manager ~id:(Portal.job_id "second"));
    assert (List.for_all (fun (job : Process.job_summary) -> job.status <> Process.Running) (Process.jobs manager)))))

let test_dns_boundaries () =
  List.iter (fun name -> assert (Portal.publish_name name = name))
    ["a"; "my-app-1"; String.make Portal.max_name_length 'a'];
  List.iter (fun name -> rejected (fun () -> Portal.publish_name name))
    [""; "-bad"; "bad-"; "bad name"; "bad.name"; "BAD";
     String.make (Portal.max_name_length + 1) 'a'; String.make 63 'a'];
  let first = Portal.fresh_name () and second = Portal.fresh_name () in
  assert (first <> second);
  assert (Portal.publish_name first = first && Portal.publish_name second = second)

let test_ready_records () =
  assert (Portal.public_url "INF listener_relays=[\"https://listener.example\"]\n" = None);
  assert (Portal.public_url "INF service ready at https://demo.relay.example" = None);
  assert (Portal.public_url "INF service ready at https://demo.relay.example\n" = Some "https://demo.relay.example");
  assert (Portal.public_url "{ \"public_url\": \"https://demo.relay.example\", \"message\": \"service ready at https://demo.relay.example\" }\n" = Some "https://demo.relay.example");
  assert (Portal.public_url "{\"public_url\":\"https://demo.relay.example\",\"message\":\"service ready at https://other.relay.example\"}\n" = None);
  assert (Portal.public_url "{\"public_url\":\"https://demo.relay.example\",\"message\":\"service ready at https://user:secret@relay.example\"}\n" = None);
  assert (Portal.public_url "{\"public_url\":\"https://admin.relay.example\",\"message\":\"relay discovered\"}\n" = None);
  assert (Portal.public_url "INF service ready at https://user:secret@relay.example\n" = None);
  assert (Portal.public_url "INF service ready at https://demo.relay.example/evil\n" = None)

let test_identity_safety () = with_dir (fun dir ->
  let target = Filename.concat dir "outside.json" in
  let oc = open_out target in output_string oc "{\"name\":\"demo\"}"; close_out oc;
  Unix.chmod target 0o600;
  Unix.symlink target (Filename.concat dir "demo.json");
  rejected (fun () -> Portal.identity_path ~state_dir:dir ~name:"demo" ());
  Unix.unlink (Filename.concat dir "demo.json");
  let oc = open_out (Filename.concat dir "demo.json") in output_string oc "{\"name\":\"different\"}"; close_out oc;
  Unix.chmod (Filename.concat dir "demo.json") 0o600;
  rejected (fun () -> Portal.identity_path ~state_dir:dir ~name:"demo" ()))

let test_failed_start_and_cleanup () = with_dir (fun dir ->
  let bad = script dir "bad-portal" "echo 'service ready at https://wrong.relay.example'\nwhile true; do sleep 1; done\n" in
  with_listener (fun port -> with_manager (fun manager ->
    rejected (fun () -> Portal.publish ~env:(env bad) ~state_dir:dir manager
      ~id:"portal:demo" ~port ~name:"demo");
    assert (List.for_all (fun (job : Process.job_summary) -> job.status <> Process.Running) (Process.jobs manager));
    let inconsistent = script dir "inconsistent-portal"
      "echo '{\"message\":\"service ready at https://wrong.relay.example\",\"public_url\":\"https://demo.relay.example\"}'\nwhile true; do sleep 1; done\n" in
    rejected (fun () -> Portal.publish ~env:(env inconsistent) ~state_dir:dir manager
      ~id:"portal:demo" ~port ~name:"demo");
    assert (Process.job_status manager ~id:"portal:demo" <> Process.Running);
    let good = identity_portal dir in
    assert (field "url" (Portal.publish ~env:(env good) ~state_dir:dir manager
      ~id:"portal:demo" ~port ~name:"demo") = `String "https://demo.relay.example"))))

let test_absent_server_and_backend () = with_dir (fun dir ->
  let portal = identity_portal dir in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port = match Unix.getsockname socket with Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  Unix.close socket;
  with_manager (fun manager ->
    rejected (fun () -> Portal.publish ~env:(env portal) ~state_dir:dir manager ~id:"portal:demo" ~port ~name:"demo");
    assert (Process.jobs manager = []));
  let fallbacks = Filename.concat dir "fallbacks" in
  Unix.mkdir fallbacks 0o700;
  ignore (script fallbacks "cloudflared" "exit 0\n");
  ignore (script fallbacks "ssh" "exit 0\n");
  let missing key = match key with
    | "PATH" -> Some fallbacks
    | "HOME" -> Some dir
    | "XDG_STATE_HOME" -> Some (Filename.concat dir "state")
    | _ -> None in
  assert (Portal.setup_required ~env:missing ());
  assert (Portal.detect_portal ~env:missing () =
    Filename.concat dir "state/pave/portal/bin/portal")
)

let test_relay_validation () =
  assert (Portal.https_origin "https://relay.example:8443/" = "https://relay.example:8443");
  let long_host = "https://" ^ String.make 63 'a' ^ ".example" in
  assert (Portal.https_origin long_host = long_host);
  List.iter (fun relay -> rejected (fun () -> Portal.https_origin relay))
    ["http://relay.example"; "https://user@relay.example"; "https://relay.example/path";
     "https://relay.example?token=secret"; "https://relay.example:0"; "https://-bad.example"]

let managed_env root key =
  match key with
  | "HOME" -> Some root
  | "XDG_STATE_HOME" -> Some (Filename.concat root "state")
  | "PATH" -> Some (Filename.concat root "empty-path")
  | _ -> None

let test_automatic_setup_and_publish () = with_dir (fun dir ->
  let executable = script dir "downloaded-portal"
    "echo '{ \"message\": \"service ready at https://auto.relay.example\", \"public_url\": \"https://auto.relay.example\" }'\nwhile true; do sleep 1; done\n" in
  let contents = let ic = open_in_bin executable in
    Fun.protect ~finally:(fun () -> close_in ic)
      (fun () -> really_input_string ic (in_channel_length ic)) in
  let os, arch = Portal.platform_asset () in
  let asset = "portal-" ^ os ^ "-" ^ arch in
  let checksum = Digestif.SHA256.(to_hex (digest_string contents)) ^
    "  " ^ asset ^ "\n" in
  let transfer = {
    Portal.fetch_text = (fun ~url ~max_bytes ->
      assert (url = "https://github.com/gosuda/portal-tunnel/releases/latest/download/" ^ asset ^ ".sha256");
      assert (max_bytes = 1024);
      checksum);
    download_file = (fun ~url ~path ~max_bytes ->
      assert (url = "https://github.com/gosuda/portal-tunnel/releases/latest/download/" ^ asset);
      assert (max_bytes = 50_000_000);
      let oc = open_out_bin path in
      output_string oc contents;
      close_out oc)
  } in
  let env = managed_env dir in
  let installed = Portal.detect_portal ~env () in
  assert (Portal.setup_required ~env ());
  assert (Option.is_some (Portal.setup_description ~env ()));
  with_listener (fun port -> with_manager (fun manager ->
    let published = Portal.publish ~env ~transfer manager ~id:"portal:auto"
      ~port ~name:"auto" in
    assert (field "url" published = `String "https://auto.relay.example");
    assert (Sys.file_exists installed);
    let stat = Unix.lstat installed in
    assert (stat.Unix.st_kind = Unix.S_REG && stat.Unix.st_perm land 0o777 = 0o700);
    assert (not (Portal.setup_required ~env ()));
    ignore (Portal.stop manager ~id:"portal:auto"))))

let test_automatic_setup_rejects_bad_checksum () = with_dir (fun dir ->
  let executable = script dir "downloaded-portal" "exit 0\n" in
  let contents = let ic = open_in_bin executable in
    Fun.protect ~finally:(fun () -> close_in ic)
      (fun () -> really_input_string ic (in_channel_length ic)) in
  let os, arch = Portal.platform_asset () in
  let asset = "portal-" ^ os ^ "-" ^ arch in
  let transfer = {
    Portal.fetch_text = (fun ~url:_ ~max_bytes:_ ->
      String.make 64 '0' ^ "  " ^ asset ^ "\n");
    download_file = (fun ~url:_ ~path ~max_bytes:_ ->
      let oc = open_out_bin path in output_string oc contents; close_out oc)
  } in
  let env = managed_env dir in
  let installed = Portal.detect_portal ~env () in
  with_listener (fun port -> with_manager (fun manager ->
    rejected (fun () -> Portal.publish ~env ~transfer manager ~id:"portal:auto"
      ~port ~name:"auto");
    assert (not (Sys.file_exists installed));
    assert (Process.jobs manager = []))))

let test_cancelled_readiness_cleanup () = with_dir (fun dir ->
  let portal = identity_portal dir in
  with_listener (fun port -> with_manager (fun manager ->
    let seen_ready = ref 0 in
    let cancel () =
      (match Process.jobs manager with
       | [] -> ()
       | _ ->
           assert (Process.wait_ready manager ~id:"portal:cancel"
             ~timeout_seconds:5 ~log_regex:"service ready at https://[^\n]*\n" ());
           incr seen_ready);
      !seen_ready >= 2 in
    (match Portal.publish ~cancel ~env:(env portal) ~state_dir:dir manager
        ~id:"portal:cancel" ~port ~name:"cancel" with
     | _ -> assert false
     | exception Process.Error message -> assert (message = "process wait cancelled"));
    assert (!seen_ready = 2);
    assert (Process.job_status manager ~id:"portal:cancel" <> Process.Running);
    assert (field "status" (Portal.publish ~env:(env portal) ~state_dir:dir manager
      ~id:"portal:cancel" ~port ~name:"cancel") = `String "published"))))

let test_stop_scope () = with_manager (fun manager ->
  Process.start manager ~id:"ordinary" ~program:"/bin/sleep" ~arguments:["30"] ();
  rejected (fun () -> Portal.stop manager ~id:"ordinary");
  assert (Process.job_status manager ~id:"ordinary" = Process.Running);
  List.iter (fun id -> rejected (fun () -> Portal.stop manager ~id))
    ["portal:"; "portal:-bad"; "portal:" ^ String.make 23 'a'];
  Process.kill_job manager ~id:"ordinary")

let with_environment overrides body =
  let previous = List.map (fun (key, _) -> key, Sys.getenv_opt key) overrides in
  Fun.protect ~finally:(fun () ->
    List.iter (fun (key, value) -> Unix.putenv key (Option.value value ~default:"")) previous)
    (fun () ->
      List.iter (fun (key, value) -> Unix.putenv key value) overrides;
      body ())

let test_tool_registration_and_lifecycle () = with_dir (fun dir ->
  let module Tools = Pave.Tools in
  let portal = identity_portal dir in
  with_environment ["PAVE_PORTAL", portal; "PAVE_TUNNELS", "on";
    "XDG_STATE_HOME", Filename.concat dir "state"] (fun () ->
    with_listener (fun port -> with_manager (fun manager ->
      with_manager (fun other_manager ->
        let context = Tools.create_session_context ~owner:"portal-owner" ~root:dir
          ~hub_port:(fun () -> port) ~process_manager:manager
          ~read_artifact:(fun _ -> None)
          ~record_file_change:(fun ~path:_ ~before:_ ~after:_ -> ()) () in
        let other_context = Tools.create_session_context ~owner:"other-owner" ~root:dir
          ~process_manager:other_manager ~read_artifact:(fun _ -> None)
          ~record_file_change:(fun ~path:_ ~before:_ ~after:_ -> ()) () in
        Fun.protect ~finally:(fun () ->
          Tools.close_session_context context;
          Tools.close_session_context other_context) (fun () ->
          let definitions = Tools.available_for ~allow_shell:false
            ~enabled:(fun name -> name = "publish_web") in
          assert (List.length definitions = 1);
          let execute ?context ?(approved = false) fields =
            Tools.execute ?context ~approved ~root:dir ~name:"publish_web"
              ~args:(`Assoc fields) () in
          let result = function
            | Ok [Pave.Protocol.Text text] -> Yojson.Basic.from_string text
            | Ok _ -> assert false
            | Error message -> failwith message in
          let denied = function Error _ -> () | Ok _ -> assert false in
          denied (execute ["action", `String "list"]);
          let publish = ["action", `String "publish"; "port", `Int port;
            "name", `String "tool-preview"] in
          assert (Tools.requires_explicit_approval ~name:"publish_web"
            ~args:(`Assoc publish));
          denied (execute ~context publish);
          assert (Process.jobs manager = []);
          denied (execute ~context ~approved:true
            ["action", `String "publish"; "port", `Int port;
             "name", `String (String.make 23 'a')]);
          assert (field "url" (result (execute ~context ~approved:true publish))
            = `String "https://tool-preview.relay.example");
          let list = ["action", `String "list"; "relay", `String "http://unused.invalid"] in
          assert (not (Tools.requires_explicit_approval ~name:"publish_web"
            ~args:(`Assoc list)));
          assert (field "tunnels" (result (execute ~context:other_context list)) = `List []);
          let stop = ["action", `String "stop"; "name", `String "tool-preview";
            "relay", `String "http://unused.invalid"] in
          denied (execute ~context:other_context ~approved:true stop);
          denied (execute ~context stop);
          assert (Process.job_status manager ~id:"portal:tool-preview" = Process.Running);
          assert (field "status" (result (execute ~context ~approved:true stop))
            = `String "stopped");
          let attach = ["action", `String "attach"; "name", `String "tool-hub"] in
          denied (execute ~context:other_context ~approved:true attach);
          denied (execute ~context attach);
          assert (field "attach" (result (execute ~context ~approved:true attach)) = `Bool true);
          Tools.close_session_context context;
          Process.close_manager manager;
          assert (Process.job_status manager ~id:"portal:tool-hub" <> Process.Running);
          denied (execute ~context list)))))))

let () =
  test_dns_boundaries (); test_ready_records (); test_identity_safety ();
  test_prefixes_and_lifecycle (); test_failed_start_and_cleanup ();
  test_absent_server_and_backend (); test_relay_validation ();
  test_automatic_setup_and_publish (); test_automatic_setup_rejects_bad_checksum ();
  test_cancelled_readiness_cleanup (); test_stop_scope ();
  test_tool_registration_and_lifecycle ();
  print_endline "workspace_portal: ok"
