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
    ["a"; "my-app-1"; String.make 63 'a'];
  List.iter (fun name -> rejected (fun () -> Portal.publish_name name))
    [""; "-bad"; "bad-"; "bad name"; "bad.name"; "BAD"; String.make 64 'a'];
  let first = Portal.fresh_name () and second = Portal.fresh_name () in
  assert (first <> second);
  assert (Portal.publish_name first = first && Portal.publish_name second = second)

let test_ready_records () =
  assert (Portal.public_url "INF listener_relays=[\"https://listener.example\"]\n" = None);
  assert (Portal.public_url "INF service ready at https://demo.relay.example" = None);
  assert (Portal.public_url "INF service ready at https://demo.relay.example\n" = Some "https://demo.relay.example");
  assert (Portal.public_url "{ \"public_url\": \"https://demo.relay.example\", \"message\": \"service ready at https://demo.relay.example\" }\n" = Some "https://demo.relay.example");
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
  let missing key = if key = "PATH" then Some fallbacks else None in
  rejected (fun () -> Portal.detect_portal ~env:missing ()))

let test_relay_validation () =
  assert (Portal.https_origin "https://relay.example:8443/" = "https://relay.example:8443");
  List.iter (fun relay -> rejected (fun () -> Portal.https_origin relay))
    ["http://relay.example"; "https://user@relay.example"; "https://relay.example/path";
     "https://relay.example?token=secret"; "https://relay.example:0"; "https://-bad.example"]

let () =
  test_dns_boundaries (); test_ready_records (); test_identity_safety ();
  test_prefixes_and_lifecycle (); test_failed_start_and_cleanup ();
  test_absent_server_and_backend (); test_relay_validation ();
  print_endline "workspace_portal: ok"
