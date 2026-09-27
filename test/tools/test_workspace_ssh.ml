module Ssh = Pave.Workspace_ssh
module Process = Pave.Workspace_process
module Path = Pave.Workspace_path

let fail label = failwith ("workspace ssh: " ^ label)
let expect label condition = if not condition then fail label
let rejects label action =
  try ignore (action ()); fail (label ^ " was accepted")
  with Ssh.Error _ -> ()

let result ?(output = "") ?(status = Process.Exited 0) () =
  { Process.termination = status; output; bytes_received = String.length output; truncated = false }

type mock = {
  events : Ssh.run_request list ref;
  files : (string, string) Hashtbl.t;
  symlinks : (string, string) Hashtbl.t;
  canonical_paths : (string, string) Hashtbl.t;
}

let words text = String.split_on_char ' ' (String.trim text) |> List.filter (( <> ) "")
let starts_with text prefix =
  String.length text >= String.length prefix && String.sub text 0 (String.length prefix) = prefix

let mock_runner mock request =
  mock.events := request :: !(mock.events);
  match request.program with
  | "/usr/bin/sftp" ->
      let command = String.trim request.stdin in
      let lines = String.split_on_char '\n' command |> List.map String.trim
          |> List.filter (( <> ) "") in
      let tokens = words command in
      (match lines with
       | [cd; "@pwd"] when starts_with cd "@cd " ->
           let path = String.sub cd 4 (String.length cd - 4) in
           let resolved = Option.value (Hashtbl.find_opt mock.canonical_paths path)
               ~default:(Option.value (Hashtbl.find_opt mock.symlinks path) ~default:path) in
           result ~output:("Remote working directory: " ^ resolved ^ "\n") ()
       | _ ->
           (match tokens with
            | ["ls"; "-ln"; path] ->
                (match Hashtbl.find_opt mock.symlinks path with
                 | Some _ -> result ~output:("lrwxrwxrwx 1 501 20 1 Jan 1 00:00 " ^ path ^ " -> outside\n") ()
                 | None ->
                     (match Hashtbl.find_opt mock.files path with
                      | Some contents ->
                          result ~output:(Printf.sprintf "-rw-r--r-- 1 501 20 %d Jan 1 00:00 %s\n"
                                            (String.length contents) path) ()
                      | None -> result ~output:"not found\n" ~status:(Process.Exited 1) ()))
            | ["get"; remote; local] ->
                (match Hashtbl.find_opt mock.files remote with
                 | None -> result ~output:"not found\n" ~status:(Process.Exited 1) ()
                 | Some contents ->
                     Path.with_fd local [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
                       (fun fd -> Path.write_all fd contents);
                     result ())
            | ["put"; local; remote] ->
                Hashtbl.replace mock.files remote (Path.read_bounded local Ssh.max_write_bytes);
                result ()
            | _ -> fail ("unexpected SFTP batch " ^ command)))
  | "/usr/bin/ssh" -> result ~output:"remote-output\n" ()
  | _ -> fail ("unexpected executable " ^ request.program)

let make_endpoint known_hosts = {
  Ssh.host = "Build.Example.test";
  user = "builder";
  remote_root = "/srv/work";
  known_hosts;
}

let create_mock _root = {
  events = ref [];
  files = Hashtbl.create 16;
  symlinks = Hashtbl.create 8;
  canonical_paths = Hashtbl.create 8;
}

let event_count mock = List.length !(mock.events)
let has_event mock program fragment =
  List.exists (fun request -> request.Ssh.program = program &&
    (request.stdin = fragment || List.exists (( = ) fragment) request.arguments)) !(mock.events)

let expect_error_text label expected action =
  try ignore (action ()); fail (label ^ " was accepted")
  with Ssh.Error message ->
    if not (starts_with message expected) then fail (label ^ " returned an unexpected error")

let remote_read ?cancel ~owner ~network_approved session ~path () =
  Ssh.read_file ?cancel ~owner ~read_approved:true ~network_approved session ~path ()

let remote_read_uri ~owner ~network_approved session uri () =
  Ssh.read_uri ~owner ~read_approved:true ~network_approved session uri ()

let () =
  let known_hosts = Filename.temp_file "pave-ssh-known-hosts-" ".txt" in
  Path.with_fd known_hosts [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
    (fun fd -> Path.write_all fd "test key placeholder\n");
  Fun.protect ~finally:(fun () -> try Unix.unlink known_hosts with Unix.Unix_error _ -> ())
    (fun () ->
      let owner = "session-one" in
      let endpoint = make_endpoint known_hosts in
      let mock = create_mock "/srv/work" in
      let current_fingerprint = ref "SHA256:trustedkey" in
      let verify _runner _endpoint = !current_fingerprint in
      let runner = mock_runner mock in
      let session = Ssh.open_session ~runner ~verify_known_host:verify ~owner ~endpoint
          ~host_trusted:true ~network_approved:true () in
      expect "configured host normalized" (has_event mock "/usr/bin/sftp" "-oHostKeyAlias=build.example.test");
      expect "remote root is resolved before binding"
        (has_event mock "/usr/bin/sftp" "@cd /srv/work\n@pwd\n");
      expect "strict host checking is pinned" (has_event mock "/usr/bin/sftp" "-oStrictHostKeyChecking=yes");
      expect "SSH config is disabled" (has_event mock "/usr/bin/sftp" "/dev/null");

      let before = event_count mock in
      rejects "owner isolation" (fun () ->
        remote_read ~owner:"different-owner" ~network_approved:true session ~path:"main.ml" ());
      expect "owner denial has no network effect" (event_count mock = before);
      rejects "read approval is required" (fun () ->
        Ssh.read_file ~owner ~read_approved:false ~network_approved:true session ~path:"main.ml" ());
      expect "read denial has no network effect" (event_count mock = before);

      rejects "path traversal" (fun () ->
        remote_read ~owner ~network_approved:true session ~path:"../secret" ());
      rejects "option-like path" (fun () ->
        remote_read ~owner ~network_approved:true session ~path:"-oProxyCommand=evil" ());
      rejects "URI host alias" (fun () ->
        ignore (Ssh.parse_uri endpoint "ssh://builder@alias.example.test/main.ml"));
      rejects "URI selector injection" (fun () ->
        ignore (Ssh.parse_uri endpoint "ssh://builder@build.example.test/main.ml:-oProxyCommand"));
      let before_bad_host = event_count mock in
      rejects "SSH option injection through host" (fun () ->
        Ssh.open_session ~runner ~verify_known_host:verify ~owner
          ~endpoint:{ endpoint with Ssh.host = "-oProxyCommand=evil" }
          ~host_trusted:true ~network_approved:true ());
      expect "invalid host does not start SSH" (event_count mock = before_bad_host);

      Hashtbl.replace mock.files "/srv/work/main.ml" "one\ntwo\nthree\n";
      let selected = remote_read_uri ~owner ~network_approved:true session
          "ssh://builder@build.example.test/main.ml:2-3" () in
      expect "safe fixed URI selector" (selected = "two\nthree");
      let raw_uri = remote_read_uri ~owner ~network_approved:true session
          "ssh://builder@build.example.test/main.ml:raw" () in
      expect "raw URI selector" (raw_uri = "one\ntwo\nthree\n");

      Hashtbl.replace mock.symlinks "/srv/work/outside" "/etc/passwd";
      rejects "symlink traversal" (fun () ->
        remote_read ~owner ~network_approved:true session ~path:"outside" ());
      Hashtbl.replace mock.canonical_paths "/srv/work/linked" "/etc";
      let before_escape = event_count mock in
      rejects "canonical symlink escape" (fun () ->
        remote_read ~owner ~network_approved:true session ~path:"linked/escape" ());
      expect "symlink escape is rejected before file metadata is read"
        (event_count mock = before_escape + 1);
      Hashtbl.replace mock.files "/srv/work/large" (String.make (Ssh.max_read_bytes + 1) 'x');
      let before_large = event_count mock in
      rejects "oversized read" (fun () ->
        remote_read ~owner ~network_approved:true session ~path:"large" ());
      expect "oversized read does not transfer" (event_count mock = before_large + 2);
      Hashtbl.replace mock.files "/srv/work/exact" (String.make Ssh.max_read_bytes 'x');
      let exact = remote_read ~owner ~network_approved:true session ~path:"exact" () in
      expect "maximum read is allowed" (String.length exact = Ssh.max_read_bytes);

      let before_denial = event_count mock in
      rejects "write approval is required" (fun () ->
        Ssh.write_file ~owner ~network_approved:true ~write_approved:false session
          ~path:"new.txt" ~contents:"must not arrive" ());
      expect "write denial performs no remote operation" (event_count mock = before_denial);
      expect "write denial leaves remote unchanged"
        (not (Hashtbl.mem mock.files "/srv/work/new.txt"));
      rejects "network approval is required for transfer" (fun () ->
        Ssh.write_file ~owner ~network_approved:false ~write_approved:true session
          ~path:"new.txt" ~contents:"must not arrive" ());
      expect "network denial performs no remote operation" (event_count mock = before_denial);
      Hashtbl.replace mock.symlinks "/srv/work/write-link" "/etc/passwd";
      let before_symlink_write = event_count mock in
      rejects "write through symlink" (fun () ->
        Ssh.write_file ~owner ~network_approved:true ~write_approved:true session
          ~path:"write-link" ~contents:"must not arrive" ());
      expect "symlink write is stopped before transfer"
        (event_count mock = before_symlink_write + 2);
      expect "symlink write does not mutate remote"
        (not (Hashtbl.mem mock.files "/srv/work/write-link"));
      ignore (Ssh.write_file ~owner ~network_approved:true ~write_approved:true session
                ~path:"new.txt" ~contents:(String.make Ssh.max_write_bytes 'w') ());
      expect "maximum write is allowed"
        (Hashtbl.find mock.files "/srv/work/new.txt" = String.make Ssh.max_write_bytes 'w');
      rejects "oversized write" (fun () ->
        Ssh.write_file ~owner ~network_approved:true ~write_approved:true session
          ~path:"too-large" ~contents:(String.make (Ssh.max_write_bytes + 1) 'w') ());
      expect "oversized write does not mutate remote"
        (not (Hashtbl.mem mock.files "/srv/work/too-large"));

      let before_command_denial = event_count mock in
      rejects "execution approval is required" (fun () ->
        Ssh.run_command ~owner ~network_approved:true ~execution_approved:false session
          ~program:"/bin/echo" ~arguments:["x"] ());
      expect "execution denial starts no process" (event_count mock = before_command_denial);
      rejects "timeout cap" (fun () ->
        Ssh.run_command ~timeout_seconds:(Ssh.max_timeout_seconds + 1)
          ~owner ~network_approved:true ~execution_approved:true session
          ~program:"/bin/echo" ~arguments:["x"] ());
      expect "invalid timeout starts no process" (event_count mock = before_command_denial);
      let output = Ssh.run_command ~owner ~network_approved:true ~execution_approved:true session
          ~program:"/bin/echo"
          ~arguments:["-oProxyCommand=evil"; "value'; touch /tmp/pwn; echo '"] () in
      expect "approved command returns bounded output" (output = "remote-output\n");
      let ssh_call = List.find (fun request -> request.Ssh.program = "/usr/bin/ssh") !(mock.events) in
      let command = List.hd (List.rev ssh_call.Ssh.arguments) in
      expect "remote arguments are shell-quoted" (starts_with command "cd '/srv/work' && exec '/bin/echo'");
      expect "injected option remains a remote argument" (String.contains command '\\');
      let large_output_mock = create_mock "/srv/work" in
      let large_output_runner request =
        let response = mock_runner large_output_mock request in
        if request.Ssh.program = "/usr/bin/ssh" then
          let output = String.make (Ssh.max_output_bytes + 1) 'x' in
          { response with Process.output = output; bytes_received = String.length output }
        else response in
      let large_output_session = Ssh.open_session ~runner:large_output_runner
          ~verify_known_host:verify ~owner ~endpoint ~host_trusted:true ~network_approved:true () in
      rejects "oversized command output" (fun () ->
        Ssh.run_command ~owner ~network_approved:true ~execution_approved:true
          large_output_session ~program:"/bin/echo" ~arguments:["x"] ());
      Ssh.close_session ~owner large_output_session;

      let calls_before_changed_key = event_count mock in
      current_fingerprint := "SHA256:changedkey";
      expect_error_text "changed host key is rejected" "SSH host key changed"
        (fun () -> remote_read ~owner ~network_approved:true session ~path:"main.ml" ());
      expect "changed key is rejected before an SSH connection" (event_count mock = calls_before_changed_key);
      current_fingerprint := "SHA256:trustedkey";

      let untrusted_mock = create_mock "/srv/work" in
      let called_untrusted_verifier = ref false in
      rejects "explicit host trust required" (fun () ->
        Ssh.open_session ~runner:(mock_runner untrusted_mock)
          ~verify_known_host:(fun _ _ -> called_untrusted_verifier := true; !current_fingerprint)
          ~owner ~endpoint ~host_trusted:false ~network_approved:true ());
      expect "untrusted host does not invoke verifier" (not !called_untrusted_verifier);
      expect "untrusted host has no network effect" (event_count untrusted_mock = 0);
      let network_denied_mock = create_mock "/srv/work" in
      let verifier_called = ref false in
      rejects "network approval is required to open SSH" (fun () ->
        Ssh.open_session ~runner:(mock_runner network_denied_mock)
          ~verify_known_host:(fun _ _ -> verifier_called := true; !current_fingerprint)
          ~owner ~endpoint ~host_trusted:true ~network_approved:false ());
      expect "network denial performs no verification or connection"
        (not !verifier_called && event_count network_denied_mock = 0);

      let unknown_mock = create_mock "/srv/work" in
      rejects "unknown host key" (fun () ->
        Ssh.open_session ~runner:(mock_runner unknown_mock)
          ~verify_known_host:(fun _ _ -> raise (Ssh.Error "unknown"))
          ~owner ~endpoint ~host_trusted:true ~network_approved:true ());
      expect "unknown key cannot start SSH" (event_count unknown_mock = 0);

      let cancel = ref false in
      let cancel_mock = create_mock "/srv/work" in
      let cancel_session = Ssh.open_session ~runner:(mock_runner cancel_mock)
          ~verify_known_host:verify ~owner ~endpoint ~host_trusted:true ~network_approved:true () in
      let calls_before_cancel = event_count cancel_mock in
      cancel := true;
      rejects "cancelled read" (fun () ->
        remote_read ~cancel:(fun () -> !cancel) ~owner ~network_approved:true cancel_session
          ~path:"main.ml" ());
      expect "cancellation prevents further SSH calls" (event_count cancel_mock = calls_before_cancel);
      let cancel_during_mock = create_mock "/srv/work" in
      let cancel_during = ref false in
      let cancel_during_runner request =
        let response = mock_runner cancel_during_mock request in
        if request.Ssh.program = "/usr/bin/sftp" && starts_with request.Ssh.stdin "ls -ln " then (
          cancel_during := true;
          { response with Process.termination = Process.Cancelled })
        else response in
      let cancel_during_session = Ssh.open_session ~runner:cancel_during_runner
          ~verify_known_host:verify ~owner ~endpoint ~host_trusted:true ~network_approved:true () in
      rejects "in-flight cancellation" (fun () ->
        remote_read ~cancel:(fun () -> !cancel_during) ~owner ~network_approved:true
          cancel_during_session ~path:"main.ml" ());
      expect "cancellation reaches file metadata check"
        (has_event cancel_during_mock "/usr/bin/sftp" "ls -ln /srv/work/main.ml\n");
      expect "cancelled transfer stops before data download"
        (not (List.exists (fun request ->
          request.Ssh.program = "/usr/bin/sftp" && starts_with request.Ssh.stdin "get ")
          !(cancel_during_mock.events)));

      Ssh.close_session ~owner session;
      rejects "closed session" (fun () ->
        remote_read ~owner ~network_approved:true session ~path:"main.ml" ());
      print_endline "workspace SSH trust, isolation, transfer, and approval boundaries: ok")
