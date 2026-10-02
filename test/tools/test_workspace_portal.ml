module Portal = Pave.Workspace_portal
module Process = Pave.Workspace_process

let contains haystack needle =
  let hay_len = String.length haystack and n_len = String.length needle in
  let rec scan index =
    if index + n_len > hay_len then false
    else if String.sub haystack index n_len = needle then true
    else scan (index + 1) in
  scan 0

(* Deterministic environment seam: detect_* and publish take ?env, so tests
   never mutate process state. [vars] overlays defaults; [path_dirs] becomes
   the injected PATH. *)
let fake_env ?(vars = []) ?(path_dirs = []) () =
  let path = String.concat ":" path_dirs in
  fun name ->
    match List.assoc_opt name vars with
    | Some value -> value
    | None when name = "PATH" -> Some path
    | None -> None


let fake_script dir name body =
  let path = Filename.concat dir name in
  let oc = open_out_bin path in
  output_string oc ("#!/bin/sh\n" ^ body);
  close_out oc;
  Unix.chmod path 0o700;
  path

let with_temp_dir body =
  let dir = Filename.temp_dir "pave-portal-test-" "" in
  let rec rm path =
    match Sys.is_directory path with
    | true -> Sys.readdir path |> Array.iter (fun e -> rm (Filename.concat path e));
        Unix.rmdir path
    | false -> Sys.remove path in
  Fun.protect ~finally:(fun () -> try rm dir with _ -> ())
    (fun () -> body dir)

(* Fake portal honored through the PAVE_PORTAL pin, like before. *)
let with_fake_portal body =
  with_temp_dir (fun dir ->
    let path = fake_script dir "portal"
      "if [ \"$1\" = \"expose\" ]; then\n\
       \techo '{\"level\":\"info\",\"public_url\":\"https://demo.test-relay.example\",\"time\":\"now\",\"message\":\"service ready at https://demo.test-relay.example\"}'\n\
       \twhile true; do sleep 1; done\n\
       else\n\techo 'unknown command' >&2; exit 64\nfi\n" in
    body (fake_env ~vars:[Portal.portal_variable, Some path] ()) )

let test_detect_and_url env =
  let executable = Portal.detect_portal ~env () in
  assert (executable <> "");
  let text = Printf.sprintf
    "prelude\nINF service ready at https://alpha.relay.example tail\n" in
  assert (Portal.public_url text = Some "https://alpha.relay.example");
  let json = "{\"level\":\"info\",\"public_url\":\"https://beta.relay.example\",\"message\":\"x\"}" in
  assert (Portal.public_url json = Some "https://beta.relay.example");
  assert (Portal.public_url "no url here" = None)

let test_backend_detection () =
  with_temp_dir (fun dir ->
    let portal_dir = Filename.concat dir "portal-bin"
    and cf_dir = Filename.concat dir "cf-bin"
    and ssh_dir = Filename.concat dir "ssh-bin" in
    Unix.mkdir portal_dir 0o700; Unix.mkdir cf_dir 0o700;
    Unix.mkdir ssh_dir 0o700;
    let portal = fake_script portal_dir "portal" "exit 0\n"
    and cf = fake_script cf_dir "cloudflared" "exit 0\n"
    and ssh = fake_script ssh_dir "ssh" "exit 0\n" in
    let all = [portal_dir; cf_dir; ssh_dir] in
    (* PATH order is the chain: portal first, then cloudflared, then ssh. *)
    let detect ?(ssh_candidates = []) ?(vars = []) path_dirs =
      Portal.detect_backend ~env:(fake_env ~vars ~path_dirs ())
        ~ssh_candidates () in
    let backend, exe = detect all in
    assert (backend = Portal.Portal && exe = portal);
    let backend, exe = detect [cf_dir; ssh_dir] in
    assert (backend = Portal.Cloudflared && exe = cf);
    let backend, exe = detect [ssh_dir] in
    assert (backend = Portal.Localhost_run && exe = ssh);
    (* Conventional /usr/bin/ssh is checked before a PATH ssh when the
       candidate exists (tests pass it explicitly to stay hermetic). *)
    let backend, exe = detect ~ssh_candidates:[ssh] [] in
    assert (backend = Portal.Localhost_run && exe = ssh);
    (* Within a stage the pin beats PATH, and the pin path is what gets
       spawned. Stages still resolve in order: a portal PATH hit wins over
       a PAVE_SSH pin, and a cloudflared PATH hit wins over a PAVE_SSH pin. *)
    let other_dir = Filename.concat dir "cf-other" in
    Unix.mkdir other_dir 0o700;
    let pinned_cf = fake_script other_dir "cloudflared-pinned" "exit 0\n" in
    let vars = [ Portal.cloudflared_variable, Some pinned_cf ] in
    let backend, exe = detect ~vars [cf_dir; ssh_dir] in
    assert (backend = Portal.Cloudflared && exe = pinned_cf);
    let vars = [ Portal.ssh_variable, Some ssh ] in
    let backend, exe = detect ~vars [portal_dir] in
    assert (backend = Portal.Portal && exe = portal);
    let backend, exe = detect ~vars [cf_dir] in
    assert (backend = Portal.Cloudflared && exe = cf);
    let backend, exe = detect ~vars [] in
    assert (backend = Portal.Localhost_run && exe = ssh);
    (* Set-but-unusable pins fail loudly naming the variable, even when a
       later backend would be usable. *)
    let expect var vars path_dirs =
      try ignore (detect ~vars path_dirs); assert false
      with Portal.Error message -> assert (contains message var) in
    expect Portal.portal_variable
      [Portal.portal_variable, Some "/nonexistent/bad-portal"] all;
    expect Portal.portal_variable
      [Portal.portal_variable, Some ""] all;
    expect Portal.cloudflared_variable
      [ Portal.cloudflared_variable, Some "/nonexistent/bad-cf" ] [ssh_dir];
    expect Portal.ssh_variable
      [ Portal.ssh_variable, Some "/nonexistent/bad-ssh" ] [ssh_dir];
    (* PAVE_TUNNELS=off disables every backend, even pinned ones. *)
    let vars = [ Portal.tunnels_variable, Some "off";
                 Portal.portal_variable, Some portal ] in
    (try ignore (detect ~vars all); assert false
     with Portal.Error message -> assert (contains message "PAVE_TUNNELS")))

let test_no_backend_error () =
  try
    ignore (Portal.detect_backend ~env:(fake_env ~vars:[] ())
              ~ssh_candidates:[] ());
    assert false
  with Portal.Error message ->
    assert (contains message "portal-tunnel");
    assert (contains message "cloudflared");
    assert (contains message "ssh");
    assert (contains message "PAVE_PORTAL");
    assert (contains message "PAVE_CLOUDFLARED")

let test_backend_url_parsing () =
  let cf_log = "2026-10-02T00:00:00Z INF +------------------------------------------------+\n\
    2026-10-02T00:00:00Z INF |  Your quick Tunnel has been created!  |\n\
    2026-10-02T00:00:00Z INF |  https://abc-def-123.trycloudflare.com |\n" in
  assert (Portal.cloudflared_url cf_log =
    Some "https://abc-def-123.trycloudflare.com");
  assert (Portal.cloudflared_url "visit https://trycloudflare.com docs" = None);
  assert (Portal.cloudflared_url "no url" = None);
  let lhr = "Welcome to localhost.run!\n\
    Connect to http://localhost.run:8080\n\
    abc123def.lhr.life tunneled with tls termination, https://abc123def.lhr.life\n" in
  assert (Portal.localhost_run_url lhr = Some "https://abc123def.lhr.life");
  assert (Portal.localhost_run_url
    "tunneled at https://zz99.localhost.run\n" =
      Some "https://zz99.localhost.run");
  assert (Portal.localhost_run_url "https://lhr.life itself" = None);
  assert (Portal.localhost_run_url "nothing" = None);
  (* Readiness regexes match the URL shapes each backend emits. *)
  let matches regex text =
    match Str.search_forward (Str.regexp regex) text 0 with
    | _ -> true
    | exception Not_found -> false in
  assert (matches (Portal.ready_regex Portal.Cloudflared) cf_log);
  assert (matches (Portal.ready_regex Portal.Localhost_run) lhr);
  assert (matches (Portal.ready_regex Portal.Portal)
    "INF service ready at https://x.relay");
  assert (not (matches (Portal.ready_regex Portal.Cloudflared) lhr));
  (* The advertised name is the leftmost label of the emitted URL. *)
  assert (Portal.advertised_name "https://abc123def.lhr.life" =
    Some "abc123def");
  assert (Portal.advertised_name "https://demo.test-relay.example" =
    Some "demo");
  assert (Portal.advertised_name "ftp://nope.example" = None)

let test_argv_builders () =
  assert (Portal.cloudflared_arguments ~port:4321 ~name:"demo" =
    ["tunnel"; "--url"; "http://127.0.0.1:4321"; "--no-autoupdate"]);
  let ssh = Portal.localhost_run_arguments ~port:8080 ~name:"demo"
    ~known_hosts:"/state/tunnel_known_hosts" in
  assert (ssh = ["-NT"; "-R"; "80:127.0.0.1:8080";
                 "-o"; "BatchMode=yes";
                 "-o"; "StrictHostKeyChecking=accept-new";
                 "-o"; "UserKnownHostsFile=/state/tunnel_known_hosts";
                 "-o"; "ExitOnForwardFailure=yes";
                 "nokey@localhost.run"])

let test_publish_list_stop env =
  let manager = Process.create_manager () in
  Fun.protect ~finally:(fun () -> Process.close_manager manager) (fun () ->
    let json = Portal.publish ~env manager ~id:"portal:demo" ~port:3000
      ~name:"demo" in
    let text = Yojson.Basic.to_string json in
    assert (contains text "https://demo.test-relay.example");
    assert (contains text "\"published\"");
    assert (contains text "\"backend\":\"portal\"");
    assert (contains text "\"name\":\"demo\"");
    let listed = Yojson.Basic.to_string (Portal.list manager) in
    assert (contains listed "demo");
    assert (contains listed "running");
    let stopped = Yojson.Basic.to_string (Portal.stop manager ~id:"portal:demo") in
    assert (contains stopped "stopped"))

(* A stopped or failed tunnel leaves a finished job record; publishing the
   same name again must replace it, while a live tunnel keeps refusing. *)
let test_republish_after_stop env =
  let manager = Process.create_manager () in
  Fun.protect ~finally:(fun () -> Process.close_manager manager) (fun () ->
    let publish () =
      Yojson.Basic.to_string
        (Portal.publish ~env manager ~id:"portal:again" ~port:3000
           ~name:"again") in
    assert (contains (publish ()) "published");
    (try ignore (publish ()); assert false with Portal.Error _ -> ());
    ignore (Portal.stop manager ~id:"portal:again");
    assert (contains (publish ()) "published");
    ignore (Portal.stop manager ~id:"portal:again"))

let test_publish_cloudflared () =
  with_temp_dir (fun dir ->
    let capture = Filename.concat dir "argv" in
    let cf = fake_script dir "cloudflared"
      (Printf.sprintf "printf '%%s\\n' \"$@\" > %s\n\
       echo 'INF |  https://quick-99.trycloudflare.com |'\n\
       while true; do sleep 1; done\n" capture) in
    let env = fake_env
      ~vars:[ Portal.cloudflared_variable, Some cf ] () in
    let manager = Process.create_manager () in
    Fun.protect ~finally:(fun () -> Process.close_manager manager) (fun () ->
      let json = Portal.publish ~env manager ~id:"portal:cf" ~port:4321
        ~name:"ignored-name" in
      let text = Yojson.Basic.to_string json in
      assert (contains text "https://quick-99.trycloudflare.com");
      assert (contains text "\"backend\":\"cloudflared\"");
      (* `name` is the requested tunnel name used by `stop`; `advertised`
         carries the random subdomain the backend actually assigned. *)
      assert (contains text "\"name\":\"ignored-name\"");
      assert (contains text "\"advertised\":\"quick-99\"");
      let ic = open_in capture in
      let argv = really_input_string ic (in_channel_length ic) in
      close_in ic;
      assert (contains argv "tunnel");
      assert (contains argv "http://127.0.0.1:4321");
      assert (contains argv "--no-autoupdate");
      ignore (Portal.stop manager ~id:"portal:cf")))

(* localhost.run ignores the requested name without auth; publish reports
   the assigned subdomain and notes the caveat. *)
let test_publish_localhost_run () =
  with_temp_dir (fun dir ->
    let state = Filename.concat dir "state" in
    Unix.mkdir state 0o700;
    let capture = Filename.concat dir "argv" in
    let ssh = fake_script dir "ssh"
      (Printf.sprintf "printf '%%s\\n' \"$@\" > %s\n\
       echo 'abc123def.lhr.life tunneled with tls termination, https://abc123def.lhr.life'\n\
       while true; do sleep 1; done\n" capture) in
    let env = fake_env
      ~vars:[ Portal.ssh_variable, Some ssh ] () in
    let manager = Process.create_manager () in
    Fun.protect ~finally:(fun () -> Process.close_manager manager) (fun () ->
      let json = Portal.publish ~env ~state_dir:state manager
        ~id:"portal:lhr" ~port:8080 ~name:"wanted" in
      let text = Yojson.Basic.to_string json in
      assert (contains text "https://abc123def.lhr.life");
      assert (contains text "\"backend\":\"localhost.run\"");
      assert (contains text "\"name\":\"wanted\"");
      assert (contains text "\"advertised\":\"abc123def\"");
      let ic = open_in capture in
      let argv = really_input_string ic (in_channel_length ic) in
      close_in ic;
      assert (contains argv "-NT");
      assert (contains argv "80:127.0.0.1:8080");
      assert (contains argv "BatchMode=yes");
      assert (contains argv "StrictHostKeyChecking=accept-new");
      assert (contains argv
        ("UserKnownHostsFile=" ^ Filename.concat state "tunnel_known_hosts"));
      assert (contains argv "ExitOnForwardFailure=yes");
      assert (contains argv "nokey@localhost.run");
      assert (not (contains argv "wanted"));
      ignore (Portal.stop manager ~id:"portal:lhr")))

let test_publish_name_validation () =
  assert (Portal.publish_name "my-app-1" = "my-app-1");
  (try ignore (Portal.publish_name "bad name"); assert false
   with Portal.Error _ -> ());
  (try ignore (Portal.publish_name ""); assert false
   with Portal.Error _ -> ())

let () =
  with_fake_portal (fun env ->
    test_detect_and_url env;
    test_publish_list_stop env;
    test_republish_after_stop env);
  test_backend_detection ();
  test_no_backend_error ();
  test_backend_url_parsing ();
  test_argv_builders ();
  test_publish_cloudflared ();
  test_publish_localhost_run ();
  test_publish_name_validation ();
  print_endline "workspace_portal: ok"
