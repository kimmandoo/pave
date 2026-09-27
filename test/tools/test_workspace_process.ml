module Process = Pave.Workspace_process

let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec loop index =
    index + m <= n &&
    (String.sub text index m = fragment || loop (index + 1))
  in
  loop 0

let python3 =
  let path_dirs =
    match Sys.getenv_opt "PATH" with
    | None -> []
    | Some path -> String.split_on_char ':' path |> List.filter (( <> ) "") in
  let candidates =
    ["/usr/bin/python3"; "/usr/local/bin/python3"; "/opt/homebrew/bin/python3"] @
    List.map (fun directory -> Filename.concat directory "python3") path_dirs in
  let executable path =
    Sys.file_exists path &&
    try Unix.access path [Unix.X_OK]; true with Unix.Unix_error _ -> false in
  match List.find_opt executable candidates with
  | Some path -> path
  | None -> failwith "Python 3 runtime is required for the PTY process tests"

let wait_for_exit manager id =
  match Process.wait_job manager ~id ~timeout_seconds:5 () with
  | Process.Running -> failwith ("job did not exit: " ^ id)
  | Process.Completed termination -> termination

let output manager id =
  let chunk = Process.read_output manager ~id () in
  chunk.output

let fresh_port () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close socket) (fun () ->
    Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> failwith "unexpected socket address")

let wait_until_gone pid =
  let deadline = Unix.gettimeofday () +. 2.0 in
  let rec loop () =
    try
      Unix.kill pid Sys.sigcont;
      if Unix.gettimeofday () >= deadline then failwith "descendant survived process-group cancellation";
      Thread.delay 0.02;
      loop ()
    with Unix.Unix_error (Unix.ESRCH, _, _) -> ()
  in
  loop ()

let expect_error f =
  match f () with
  | () -> failwith "expected Workspace_process.Error"
  | exception Process.Error _ -> ()

let () =
  let merged = Process.run_shell
      ~command:"printf out; printf err >&2" () in
  assert (merged.termination = Process.Exited 0);
  assert (merged.output = "outerr");

  let literal = Process.run ~program:"/usr/bin/printf"
      ~arguments:["%s"; "$(echo not-a-shell) *"] () in
  assert (literal.output = "$(echo not-a-shell) *");

  let tailed = Process.run ~output_limit:5 ~program:"/usr/bin/printf"
      ~arguments:["%s"; "0123456789"] () in
  assert (tailed.output = "56789");
  assert (tailed.bytes_received = 10);
  assert tailed.truncated;

  let input_data = "provided input\n\000" in
  let input = Process.run ~stdin:input_data ~program:"/bin/cat" ~arguments:[] () in
  assert (input.output = input_data);
  let failed = Process.run_shell ~command:"exit 7" () in
  assert (failed.termination = Process.Exited 7);

  let timed = Process.run ~timeout_seconds:1 ~program:"/bin/sleep"
      ~arguments:["5"] () in
  assert (timed.termination = Process.Timed_out);

  let began = Unix.gettimeofday () in
  let cancelled = Process.run ~cancel:(fun () -> Unix.gettimeofday () -. began > 0.1)
      ~program:"/bin/sleep" ~arguments:["5"] () in
  assert (cancelled.termination = Process.Cancelled);

  let descendant_output = Process.run_shell ~timeout_seconds:1
      ~command:"sleep 30 & echo $!; wait" () in
  let descendant_pid =
    try int_of_string (String.trim descendant_output.output)
    with Failure _ -> failwith "did not capture descendant pid" in
  assert (descendant_output.termination = Process.Timed_out);
  wait_until_gone descendant_pid;

  let manager = Process.create_manager () in
  Process.start_shell manager ~id:"background"
    ~command:"printf READY; sleep 0.15; printf DONE" ();
  assert (Process.wait_ready manager ~id:"background" ~log_regex:"READY" ());
  assert (wait_for_exit manager "background" = Process.Exited 0);
  let background_output = output manager "background" in
  assert (contains background_output "READY");
  assert (contains background_output "DONE");
  let summaries = Process.jobs manager in
  assert (List.length summaries = 1);
  assert ((List.hd summaries).Process.status = Process.Completed (Process.Exited 0));
  Process.start manager ~id:"wait-cancel" ~program:"/bin/sleep"
    ~arguments:["5"] ();
  let wait_started = Unix.gettimeofday () in
  expect_error (fun () -> ignore (Process.wait_job manager ~id:"wait-cancel"
    ~timeout_seconds:5
    ~cancel:(fun () -> Unix.gettimeofday () -. wait_started > 0.1) ()));
  assert (Process.job_status manager ~id:"wait-cancel" = Process.Running);
  Process.kill_job manager ~id:"wait-cancel";
  assert (Process.job_status manager ~id:"wait-cancel" =
    Process.Completed Process.Cancelled);

  let offsets = Process.create_manager ~retained_output_bytes:4 () in
  Process.start offsets ~id:"tail" ~program:"/usr/bin/printf"
    ~arguments:["%s"; "abcdefgh"] ();
  assert (wait_for_exit offsets "tail" = Process.Exited 0);
  let first_page = Process.read_output offsets ~id:"tail" ~offset:0 () in
  assert (first_page.first_offset = 4);
  assert (first_page.offset = 4);
  assert (first_page.next_offset = 8);
  assert (first_page.output = "efgh");
  assert first_page.truncated;
  let last_page = Process.read_output offsets ~id:"tail" ~offset:6 ~max_bytes:2 () in
  assert (last_page.first_offset = 4);
  assert (last_page.offset = 6);
  assert (last_page.next_offset = 8);
  assert (last_page.output = "gh");
  Process.close_manager offsets;

  let env_name = "PAVE_WORKSPACE_PROCESS_TEST" in
  let inherited_value = Sys.getenv_opt env_name in
  let env_manager = Process.create_manager () in
  Process.start env_manager ~id:"environment" ~environment:[env_name, "child-only-secret"]
    ~program:"/usr/bin/printenv" ~arguments:[env_name] ();
  assert (wait_for_exit env_manager "environment" = Process.Exited 0);
  assert (output env_manager "environment" = "child-only-secret\n");
  assert (Sys.getenv_opt env_name = inherited_value);
  assert (not (contains (List.hd (Process.jobs env_manager)).Process.command "child-only-secret"));
  expect_error (fun () -> Process.start env_manager ~id:"invalid-env"
    ~environment:["bad=key", "value"] ~program:"/usr/bin/true" ~arguments:[] ());
  Process.close_manager env_manager;

  let records = Process.create_manager ~max_jobs:1 () in
  for index = 0 to 64 do
    let id = "record-" ^ string_of_int index in
    Process.start records ~id ~program:"/usr/bin/true" ~arguments:[] ();
    (match wait_for_exit records id with
     | Process.Exited 0 -> ()
     | Process.Exited code ->
         failwith (Printf.sprintf "process record %s exited %d: %S"
           id code (output records id))
     | Process.Signaled signal ->
         failwith (Printf.sprintf "process record %s received signal %d: %S"
           id signal (output records id))
     | Process.Timed_out -> failwith ("process record timed out: " ^ id)
     | Process.Cancelled -> failwith ("process record cancelled: " ^ id))
  done;
  assert (List.length (Process.jobs records) = 64);
  expect_error (fun () -> ignore (Process.job_status records ~id:"record-0"));
  assert (Process.job_status records ~id:"record-64" = Process.Completed (Process.Exited 0));
  Process.close_manager records;

  let not_ready = Process.create_manager () in
  Process.start not_ready ~id:"short" ~program:"/bin/sleep" ~arguments:["0.1"] ();
  assert (not (Process.wait_ready not_ready ~id:"short" ~timeout_seconds:1
    ~log_regex:"never" ~port:(fresh_port ()) ()));
  Process.close_manager not_ready;

  let port = fresh_port () in
  let port_manager = Process.create_manager () in
  Process.start port_manager ~id:"listener" ~program:python3
    ~arguments:["-c";
      "import socket,sys,time; s=socket.socket(); s.bind(('127.0.0.1',int(sys.argv[1]))); s.listen(); print('LISTENING',flush=True); time.sleep(5)";
      string_of_int port] ();
  assert (Process.wait_ready port_manager ~id:"listener" ~timeout_seconds:3
    ~log_regex:"LISTENING" ~port ());
  Process.kill_job port_manager ~id:"listener";
  assert (Process.job_status port_manager ~id:"listener" = Process.Completed Process.Cancelled);
  Process.close_manager port_manager;

  let stdin_manager = Process.create_manager () in
  Process.start stdin_manager ~id:"cat" ~program:"/bin/cat" ~arguments:[] ();
  Process.write_stdin stdin_manager ~id:"cat" ~data:"typed through the job\n";
  Process.close_stdin stdin_manager ~id:"cat";
  assert (wait_for_exit stdin_manager "cat" = Process.Exited 0);
  assert (output stdin_manager "cat" = "typed through the job\n");

  let pty = Process.run ~pty:true ~stdin:"pty line\n" ~program:"/bin/cat"
      ~arguments:[] () in
  assert (pty.termination = Process.Exited 0);
  assert (contains pty.output "pty line");

  let callback_count = ref 0 in
  let callback_manager = Process.create_manager ~on_exit:(fun ~id:_ _ ->
    incr callback_count; failwith "callback exception is contained") () in
  Process.start callback_manager ~id:"callback" ~program:"/bin/echo"
    ~arguments:["callback"] ();
  assert (wait_for_exit callback_manager "callback" = Process.Exited 0);
  assert (!callback_count = 1);
  Process.close_manager callback_manager;
  Process.close_manager callback_manager;
  assert (!callback_count = 1);

  let limited = Process.create_manager ~max_jobs:1 () in
  Process.start limited ~id:"one" ~program:"/bin/sleep" ~arguments:["5"] ();
  expect_error (fun () -> Process.start limited ~id:"two" ~program:"/usr/bin/true"
    ~arguments:[] ());
  assert (List.exists (fun (row : Process.job_summary) -> row.Process.id = "one")
    (Process.jobs limited));
  Process.kill_job limited ~id:"one";
  assert (Process.job_status limited ~id:"one" = Process.Completed Process.Cancelled);
  Process.start limited ~id:"two" ~program:"/usr/bin/true" ~arguments:[] ();
  assert (wait_for_exit limited "two" = Process.Exited 0);
  Process.close_manager limited;

  let closing = Process.create_manager () in
  Process.start_shell closing ~id:"owned" ~command:"sleep 30 & echo $!; wait" ();
  let deadline = Unix.gettimeofday () +. 3.0 in
  let rec read_descendant_pid () =
    match int_of_string_opt (String.trim (output closing "owned")) with
    | Some pid -> pid
    | None when Unix.gettimeofday () < deadline -> Thread.delay 0.02; read_descendant_pid ()
    | None -> failwith "manager job did not report its descendant pid"
  in
  let closing_descendant = read_descendant_pid () in
  Process.close_manager closing;
  Process.close_manager closing;
  assert (Process.job_status closing ~id:"owned" = Process.Completed Process.Cancelled);
  wait_until_gone closing_descendant;
  let natural_exit = Process.create_manager () in
  Process.start natural_exit ~id:"natural" ~program:python3
    ~arguments:["-c";
      "import subprocess; child=subprocess.Popen(['/bin/sleep','30']); print(child.pid,flush=True)"] ();
  let natural_deadline = Unix.gettimeofday () +. 3.0 in
  let rec natural_descendant_pid () =
    match int_of_string_opt (String.trim (output natural_exit "natural")) with
    | Some pid -> pid
    | None when Unix.gettimeofday () < natural_deadline ->
        Thread.delay 0.02; natural_descendant_pid ()
    | None -> failwith "completed job did not report its descendant pid"
  in
  let natural_descendant = natural_descendant_pid () in
  assert (wait_for_exit natural_exit "natural" = Process.Exited 0);
  wait_until_gone natural_descendant;
  Process.close_manager natural_exit;

  print_endline "workspace process: ok"
