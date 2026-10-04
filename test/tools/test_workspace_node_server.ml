module Server = Pave.Workspace_node_server
module Process = Pave.Workspace_process

let expect label condition = if not condition then failwith label
let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec loop i = i + m <= n &&
    (String.sub text i m = fragment || loop (i + 1)) in
  loop 0

let expect_error label fragment fn =
  match fn () with
  | _ -> failwith (label ^ ": expected error")
  | exception Server.Error message ->
      expect (label ^ ": " ^ message) (contains message fragment)

let write path text =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc text)

let rec remove_tree path =
  try match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path |> Array.iter (fun name -> remove_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Unix.unlink path
  with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let () =
  let root = Filename.temp_file "pave-node-server-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    write (Filename.concat root "package.json")
      {|{"dependencies":{"expo":"1"},"scripts":{"start":"expo start","bad":4}}|};
    write (Filename.concat root "package-lock.json") "{}";
    expect_error "invalid host" "host must be" (fun () ->
      Server.start (Server.create_manager ~process_manager:(Process.create_manager ()))
        ~id:"bad-host" ~root ~subroot:"." ~script:"start" ~package_manager:"npm"
        ~host:"0.0.0.0" ~port:8081 ());
    expect_error "invalid port" "port must be" (fun () ->
      Server.start (Server.create_manager ~process_manager:(Process.create_manager ()))
        ~id:"bad-port" ~root ~subroot:"." ~script:"start" ~package_manager:"npm"
        ~host:"localhost" ~port:0 ());
    expect_error "undeclared script" "undeclared or non-string" (fun () ->
      Server.command ~root ~subroot:"." ~script:"bad" ~package_manager:"npm");
    write (Filename.concat root "yarn.lock") "";
    expect_error "conflicting lockfiles" "conflicting lockfiles" (fun () ->
      Server.command ~root ~subroot:"." ~script:"start" ~package_manager:"");
    Sys.remove (Filename.concat root "yarn.lock");
    expect_error "readiness timeout" "readiness timeout" (fun () ->
      Server.start (Server.create_manager ~process_manager:(Process.create_manager ()))
        ~id:"bad-timeout" ~root ~subroot:"." ~script:"start" ~package_manager:"npm"
        ~host:"localhost" ~port:8081 ~readiness_timeout_seconds:0 ());
    let process_manager = Process.create_manager () in
    let manager = Server.create_manager ~process_manager in
    Process.start process_manager ~id:"unrelated" ~program:"/bin/sleep"
      ~arguments:["10"] ();
    expect_error "foreign session" "unknown node server session" (fun () ->
      Server.stop manager ~id:"unrelated");
    expect "foreign process retained" (Process.job_status process_manager ~id:"unrelated" = Process.Running);
    Process.kill_job process_manager ~id:"unrelated";
    Process.start process_manager ~id:"workspace-node-server-collision"
      ~program:"/bin/sleep" ~arguments:["10"] ();
    expect_error "colliding foreign job" "already in use" (fun () ->
      Server.start manager ~id:"collision" ~root ~subroot:"." ~script:"start"
        ~package_manager:"npm" ~host:"localhost" ~port:44566 ());
    expect "collision did not kill foreign process"
      (Process.job_status process_manager ~id:"workspace-node-server-collision" = Process.Running);
    Process.kill_job process_manager ~id:"workspace-node-server-collision";
    let conflict_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt conflict_socket Unix.SO_REUSEADDR true;
    Unix.bind conflict_socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    Unix.listen conflict_socket 1;
    let conflict_port = match Unix.getsockname conflict_socket with
      | Unix.ADDR_INET (_, port) -> port | _ -> failwith "unexpected socket address" in
    let fake_bin = Filename.concat root "bin" in
    Unix.mkdir fake_bin 0o700;
    let shim = Filename.concat fake_bin "npm" in
    write shim "#!/usr/bin/python3\nimport sys\nsys.exit(17)\n";
    Unix.chmod shim 0o700;
    let old_path = Sys.getenv_opt "PATH" in
    Unix.putenv "PATH" (fake_bin ^ ":" ^ Option.value old_path ~default:"/usr/bin:/bin");
    expect_error "foreign listener rejected before spawn" "already occupied" (fun () ->
      Server.start manager ~id:"port-conflict" ~root ~subroot:"."
        ~script:"start" ~package_manager:"npm" ~host:"localhost" ~port:conflict_port ());
    let probe_connection, _ = Unix.accept conflict_socket in
    Unix.close probe_connection;
    let foreign_client = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.connect foreign_client (Unix.ADDR_INET (Unix.inet_addr_loopback, conflict_port));
    let foreign_connection, _ = Unix.accept conflict_socket in
    Unix.close foreign_connection;
    Unix.close foreign_client;
    Unix.close conflict_socket;
    let probe = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind probe (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    let child_exit_port = match Unix.getsockname probe with
      | Unix.ADDR_INET (_, port) -> port | _ -> failwith "unexpected socket address" in
    Unix.close probe;
    expect_error "exited child cannot become ready" "exit code 17" (fun () ->
      Server.start manager ~id:"child-exit" ~root ~subroot:"."
        ~script:"start" ~package_manager:"npm" ~host:"localhost" ~port:child_exit_port
        ~readiness_timeout_seconds:2 ());
    (match Process.job_status process_manager ~id:"workspace-node-server-child-exit" with
     | Process.Completed _ -> ()
     | Process.Running -> failwith "failed owned child was not reaped");
    write shim "#!/usr/bin/python3\nimport time\ntime.sleep(30)\n";
    Unix.chmod shim 0o700;
    let probe = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind probe (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    let race_port = match Unix.getsockname probe with
      | Unix.ADDR_INET (_, port) -> port | _ -> failwith "unexpected race port" in
    Unix.close probe;
    let race_result = ref None in
    let race = Thread.create (fun () ->
      race_result := (try
        ignore (Server.start manager ~id:"port-race" ~root ~subroot:"."
          ~script:"start" ~package_manager:"npm" ~host:"localhost"
          ~port:race_port ~readiness_timeout_seconds:5 ());
        None
      with exn -> Some exn)) () in
    let child_id = "workspace-node-server-port-race" in
    let deadline = Unix.gettimeofday () +. 3. in
    let rec wait_for_child () =
      if List.exists (fun (job : Process.job_summary) -> job.id = child_id)
          (Process.jobs process_manager) then ()
      else if Unix.gettimeofday () >= deadline then
        failwith "owned child was not launched for port-race test"
      else (Thread.delay 0.01; wait_for_child ()) in
    wait_for_child ();
    let foreign_listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt foreign_listener Unix.SO_REUSEADDR true;
    Unix.bind foreign_listener (Unix.ADDR_INET (Unix.inet_addr_loopback, race_port));
    Unix.listen foreign_listener 4;
    Thread.join race;
    expect "foreign listener cannot satisfy readiness"
      (match !race_result with
       | Some (Server.Error message) -> contains message "listener is not owned"
       | _ -> false);
    expect "failed startup reaps only its owned child"
      (match Process.job_status process_manager ~id:child_id with
       | Process.Completed _ -> true | Process.Running -> false);
    let foreign_client = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.connect foreign_client (Unix.ADDR_INET (Unix.inet_addr_loopback, race_port));
    let foreign_connection, _ = Unix.accept foreign_listener in
    Unix.close foreign_connection;
    Unix.close foreign_client;
    Unix.close foreign_listener;
    (match old_path with Some path -> Unix.putenv "PATH" path | None -> ());
    write shim "#!/usr/bin/python3\nimport time\ntime.sleep(30)\n";
    Unix.chmod shim 0o700;
    let old_path = Sys.getenv_opt "PATH" in
    Unix.putenv "PATH" (fake_bin ^ ":" ^ Option.value old_path ~default:"/usr/bin:/bin");
    let cancelled = ref false in
    let result = ref None in
    let worker = Thread.create (fun () ->
      result := (try ignore (Server.start manager ~id:"cancelled" ~root ~subroot:"."
        ~script:"start" ~package_manager:"npm" ~host:"localhost" ~port:44567
        ~readiness_timeout_seconds:60 ~cancel:(fun () -> !cancelled) ());
        None with exn -> Some exn)) () in
    Thread.delay 0.2;
    cancelled := true;
    Thread.join worker;
    expect "cancel propagated" (!result <> None);
    expect "cancel stopped owned child"
      (match Process.job_status process_manager ~id:"workspace-node-server-cancelled" with
       | Process.Completed _ -> true | Process.Running -> false);
    (match old_path with Some path -> Unix.putenv "PATH" path | None -> ());
    Process.close_manager process_manager)
