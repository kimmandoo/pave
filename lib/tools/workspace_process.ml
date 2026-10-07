exception Error of string

(* Manager operations are safe to call from multiple OCaml system threads.
   The manager lock protects job metadata and retained logs; each job also has
   an input lock so a bounded write cannot race descriptor closure. Output is
   drained by one monitor thread per live job. [run] invokes callbacks on its
   calling thread; managed [on_exit] callbacks run once on the monitor thread
   after the child is reaped, and callback exceptions are contained. Keep them
   nonblocking (for example, post a UI notice). Managers own their process
   groups until [close_manager], which must be called when the owner ends. *)

type termination = Exited of int | Signaled of int | Timed_out | Cancelled

exception Start_interrupted of termination

type process_status = Running | Completed of termination

type result = {
  termination : termination;
  output : string;
  bytes_received : int;
  truncated : bool;
}

(* Offsets are absolute byte positions in merged output. [first_offset] is the
   earliest retained byte, [offset] is this page's first byte, and
   [next_offset] is the cursor for the next read. [truncated] means earlier
   output has been evicted from the bounded tail. *)
type output_chunk = {
  first_offset : int;
  offset : int;
  next_offset : int;
  output : string;
  truncated : bool;
}

type job_summary = {
  id : string;
  command : string;
  status : process_status;
  bytes_received : int;
  truncated : bool;
  created_at : float;
  ready : bool;
}

type job = {
  owner : manager;
  id : string;
  command : string;
  mutable pid : int;
  created_at : float;
  output_limit : int;
  mutable output : string;
  mutable output_start : int;
  mutable bytes_received : int;
  mutable truncated : bool;
  mutable status : process_status;
  mutable requested_termination : termination option;
  mutable stdin_fd : Unix.file_descr option;
  mutable stdout_fd : Unix.file_descr option;
  mutable worker_done : bool;
  mutable exit_notified : bool;
  mutable worker : Thread.t option;
  mutable ready : bool;
  deadline : float option;
  input_lock : Mutex.t;
}

and manager = {
  lock : Mutex.t;
  jobs : (string, job) Hashtbl.t;
  max_jobs : int;
  retained_output_bytes : int;
  mutable retained_bytes : int;
  mutable closed : bool;
  on_exit : (id:string -> result -> unit) option;
}

let default_output_limit = 65_536
(* Synchronous runs default to five minutes; manager jobs remain owner-managed
   when their caller does not supply a deadline. *)
let default_run_timeout_seconds = 300
let max_output_limit = 1_048_576
let max_manager_output = 4_194_304
let default_retained_output = 1_048_576
let default_max_jobs = 8
let hard_max_jobs = 32
let max_job_records = 64
let max_stdin_bytes = 65_536
let max_environment_entries = 128
let max_environment_bytes = 262_144

let fail message = raise (Error message)

let with_lock lock fn =
  Mutex.lock lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock lock) fn

let validate_limit label maximum value =
  if value < 0 || value > maximum then
    fail (Printf.sprintf "%s must be between 0 and %d" label maximum)

let validate_timeout ?(maximum = 86_400) label = function
  | None -> None
  | Some seconds when seconds <= 0 || seconds > maximum ->
      fail (Printf.sprintf "%s must be between 1 and %d seconds" label maximum)
  | Some seconds -> Some seconds

(* Managers retain at most 64 job records. Starting another job evicts only the
   oldest terminal record; live records are never evicted. [max_jobs] limits
   concurrently running direct children; retained_output_bytes is a shared
   tail budget across logs. [on_exit] runs once after each child is reaped. *)
let create_manager ?(max_jobs = default_max_jobs)
    ?(retained_output_bytes = default_retained_output) ?on_exit () =
  if max_jobs < 1 || max_jobs > hard_max_jobs then
    fail (Printf.sprintf "max_jobs must be between 1 and %d" hard_max_jobs);
  validate_limit "retained_output_bytes" max_manager_output retained_output_bytes;
  { lock = Mutex.create (); jobs = Hashtbl.create 16; max_jobs;
    retained_output_bytes; retained_bytes = 0; closed = false; on_exit }

let validate_text label limit value =
  if String.length value > limit || String.contains value '\000' then
    fail (label ^ " is invalid or exceeds its size limit")

let inherited_environment_names = [
  "PATH"; "HOME"; "TMPDIR"; "TEMP"; "LANG"; "LC_ALL"; "LC_CTYPE";
  "TERM"; "COLORTERM"; "USER"; "LOGNAME"; "SHELL"; "PWD"; "NO_COLOR";
  "JAVA_HOME"; "ANDROID_HOME"; "ANDROID_SDK_ROOT"; "ANDROID_USER_HOME"
]

(* Child processes inherit only portable runtime/navigation values. Provider,
   OAuth, cloud, search, and custom environment credentials are never ambient. *)

let validate_environment ?(inherit_environment = true) overrides =
  let rec enforce_count count = function
    | [] -> ()
    | _ :: _ when count >= max_environment_entries ->
        fail "too many environment overrides"
    | _ :: rest -> enforce_count (count + 1) rest
  in
  enforce_count 0 overrides;
  let env = Hashtbl.create 64 in
  if inherit_environment then
    Array.iter (fun entry ->
      match String.index_opt entry '=' with
      | None -> ()
      | Some index ->
          let name = String.sub entry 0 index in
          if List.mem name inherited_environment_names then
            Hashtbl.replace env name
              (String.sub entry (index + 1) (String.length entry - index - 1)))
      (Unix.environment ());
  let total = ref 0 in
  List.iter (fun (name, value) ->
    if name = "" || String.length name > 255 ||
       not (match name.[0] with 'A'..'Z' | 'a'..'z' | '_' -> true | _ -> false) ||
       not (String.for_all (function
         | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> true | _ -> false) name) ||
       String.contains value '\000' || String.length value > 65_536 then
      fail "invalid environment override";
    total := !total + String.length name + String.length value + 1;
    if !total > max_environment_bytes then fail "environment overrides exceed their size limit";
    Hashtbl.replace env name value) overrides;
  let total = Hashtbl.fold (fun name value total ->
    let entry_bytes = String.length name + String.length value + 1 in
    if entry_bytes > max_environment_bytes - total then
      fail "process environment exceeds its size limit";
    total + entry_bytes) env 0 in
  ignore total;
  Hashtbl.fold (fun name value entries -> (name ^ "=" ^ value) :: entries) env []
  |> Array.of_list

let validate_cwd = function
  | None -> ""
  | Some cwd ->
      validate_text "working directory" 4096 cwd;
      if cwd = "" then fail "working directory must not be empty";
      (try
         if (Unix.stat cwd).Unix.st_kind <> Unix.S_DIR then fail "working directory is not a directory"
       with Unix.Unix_error (error, _, _) ->
         fail ("working directory is unavailable: " ^ Unix.error_message error));
      cwd

let validate_program program arguments =
  validate_text "program" 4096 program;
  if program = "" then fail "program must not be empty";
  let rec check count total = function
    | [] -> ()
    | argument :: rest ->
        validate_text "argument" 65_536 argument;
        let total = total + String.length argument in
        if count >= 256 || total > 262_144 then
          fail "program arguments exceed their count or size limit";
        check (count + 1) total rest
  in
  check 0 (String.length program) arguments

let validate_id id =
  validate_text "job id" 128 id;
  if id = "" then fail "job id must not be empty"

let python_launcher () =
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
  | None -> fail "process execution requires an executable Python 3 runtime"

(* The helper creates a session before exec. PTY mode acquires a controlling
   pseudo-terminal and connects the target's standard streams to its slave;
   argv is passed directly to execvpe/Popen and is never parsed by a shell. *)
let launcher_code =
  "import errno, fcntl, os, pty, select, signal, subprocess, sys, termios\n" ^
  "mode, cwd = sys.argv[1], sys.argv[2]\n" ^
  "argv = sys.argv[3:]\n" ^
  "try:\n" ^
  "    os.setsid()\n" ^
  "    if cwd: os.chdir(cwd)\n" ^
  "    os.write(1, b'\\x00')\n" ^
  "    os.dup2(2, 1)\n" ^
  "    if mode == 'exec':\n" ^
  "        os.execvpe(argv[0], argv, os.environ.copy())\n" ^
  "    stopping = False\n" ^
  "    def stop_child(signum, frame):\n" ^
  "        global stopping\n" ^
  "        stopping = True\n" ^
  "    signal.signal(signal.SIGTERM, stop_child)\n" ^
  "    master, slave = pty.openpty()\n" ^
  "    fcntl.ioctl(slave, termios.TIOCSCTTY, 0)\n" ^
  "    child = subprocess.Popen(argv, stdin=slave, stdout=slave, stderr=slave, close_fds=True, env=os.environ.copy())\n" ^
  "    os.close(slave)\n" ^
  "    os.set_blocking(master, False)\n" ^
  "    os.set_blocking(0, False)\n" ^
  "    input_open, input_eof, eof_sent = True, False, False\n" ^
  "    pending = b''\n" ^
  "    while True:\n" ^
  "        readable = [master] + ([0] if input_open and len(pending) < 65536 else [])\n" ^
  "        writable = [master] if pending or (input_eof and not eof_sent) else []\n" ^
  "        try: ready, writable_ready, _ = select.select(readable, writable, [], 0.05)\n" ^
  "        except InterruptedError: continue\n" ^
  "        if stopping:\n" ^
  "            if child.poll() is None: child.kill()\n" ^
  "            break\n" ^
  "        if 0 in ready:\n" ^
  "            try: data = os.read(0, min(4096, 65536 - len(pending)))\n" ^
  "            except BlockingIOError: data = None\n" ^
  "            if data == b'': input_open, input_eof = False, True\n" ^
  "            elif data: pending += data\n" ^
  "        if master in writable_ready:\n" ^
  "            if pending:\n" ^
  "                try:\n" ^
  "                    count = os.write(master, pending)\n" ^
  "                    pending = pending[count:]\n" ^
  "                except BlockingIOError: pass\n" ^
  "                except OSError: pending, input_open, input_eof = b'', False, False\n" ^
  "            elif input_eof and not eof_sent:\n" ^
  "                try: os.write(master, b'\\x04\\x04')\n" ^
  "                except OSError: pass\n" ^
  "                eof_sent = True\n" ^
  "        if master in ready:\n" ^
  "            try: data = os.read(master, 8192)\n" ^
  "            except BlockingIOError: data = None\n" ^
  "            except OSError as exc:\n" ^
  "                if exc.errno == errno.EIO: data = b''\n" ^
  "                else: raise\n" ^
  "            if data:\n" ^
  "                view = memoryview(data)\n" ^
  "                while view:\n" ^
  "                    try: count = os.write(1, view)\n" ^
  "                    except BlockingIOError:\n" ^
  "                        select.select([], [1], [], 0.05); continue\n" ^
  "                    view = view[count:]\n" ^
  "        if child.poll() is not None:\n" ^
  "            try:\n" ^
  "                while True:\n" ^
  "                    data = os.read(master, 8192)\n" ^
  "                    if not data: break\n" ^
  "                    view = memoryview(data)\n" ^
  "                    while view: view = view[os.write(1, view):]\n" ^
  "            except OSError: pass\n" ^
  "            break\n" ^
  "    signal.signal(signal.SIGHUP, signal.SIG_IGN)\n" ^
  "    os.close(master)\n" ^
  "    code = child.wait()\n" ^
  "    if code < 0:\n" ^
  "        if -code in (signal.SIGTERM, signal.SIGHUP): signal.signal(-code, signal.SIG_DFL)\n" ^
  "        os.kill(os.getpid(), -code)\n" ^
  "    sys.exit(code)\n" ^
  "except Exception as exc:\n" ^
  "    try: os.write(2, ('pave process launcher: ' + str(exc) + '\\n').encode())\n" ^
  "    except Exception: pass\n" ^
  "    sys.exit(127)\n"

let environment_with_overrides ?inherit_environment overrides =
  validate_environment ?inherit_environment overrides

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

let close_tracked closed slot fd =
  if not closed.(slot) then (
    closed.(slot) <- true;
    close_fd fd)

let signal_group pid signal =
  try Unix.kill (-pid) signal
  with Unix.Unix_error ((Unix.ESRCH | Unix.EPERM), _, _) -> ()

let check_start ?cancel ?deadline () =
  (match cancel with
   | Some cancelled when cancelled () -> raise (Start_interrupted Cancelled)
   | _ -> ());
  match deadline with
  | Some expires when Unix.gettimeofday () >= expires ->
      raise (Start_interrupted Timed_out)
  | _ -> ()

let await_spawn_session ?cancel ?deadline ready_read =
  let startup_deadline = Unix.gettimeofday () +. 5.0 in
  let rec await () =
    check_start ?cancel ?deadline ();
    let remaining = startup_deadline -. Unix.gettimeofday () in
    if remaining <= 0. then fail "process launcher did not establish a session";
    let readable, _, _ = Unix.select [ready_read] [] [] (min 0.05 remaining) in
    if readable = [] then await ()
    else
      let marker = Bytes.create 1 in
      match Unix.read ready_read marker 0 1 with
      | 1 when Bytes.get marker 0 = Char.chr 0 -> check_start ?cancel ?deadline ()
      | 1 -> fail "process launcher returned an invalid session marker"
      | _ -> fail "process launcher exited before establishing a session" in
  await ()

let spawn_pty ?cancel ?deadline ~program ~arguments ~cwd ~environment () =
  check_start ?cancel ?deadline ();
  let python = python_launcher () in
  let stdin_read, stdin_write = Unix.pipe ~cloexec:true () in
  let stdout_read, stdout_write =
    try Unix.pipe ~cloexec:true ()
    with exn -> close_fd stdin_read; close_fd stdin_write; raise exn
  in
  let ready_read, ready_write =
    try Unix.pipe ~cloexec:true ()
    with exn ->
      close_fd stdin_read; close_fd stdin_write;
      close_fd stdout_read; close_fd stdout_write;
      raise exn
  in
  let helper_args =
    Array.of_list (python :: "-c" :: launcher_code :: "pty" :: cwd :: program :: arguments)
  in
  (* Track the stdin, stdout and readiness pipe pairs. Closed descriptor
     numbers may already belong to another thread when startup fails. *)
  let closed = Array.make 6 false in
  let cleanup () =
    close_tracked closed 0 stdin_read; close_tracked closed 1 stdin_write;
    close_tracked closed 2 stdout_read; close_tracked closed 3 stdout_write;
    close_tracked closed 4 ready_read; close_tracked closed 5 ready_write
  in
  let child = ref None in
  try
    let pid = Unix.create_process_env python helper_args environment
        stdin_read ready_write stdout_write in
    child := Some pid;
    close_tracked closed 0 stdin_read;
    close_tracked closed 5 ready_write;
    close_tracked closed 3 stdout_write;
    Unix.set_nonblock stdin_write;
    Unix.set_nonblock stdout_read;
    Unix.set_nonblock ready_read;
    await_spawn_session ?cancel ?deadline ready_read;
    close_tracked closed 4 ready_read;
    (pid, stdin_write, stdout_read)
  with exn ->
    (match !child with
     | None -> ()
     | Some pid ->
         signal_group pid Sys.sigkill;
         (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
         (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ()));
    cleanup ();
    raise exn

let environment_value environment name =
  let prefix = name ^ "=" in
  Array.find_map (fun entry ->
    if String.length entry >= String.length prefix &&
       String.sub entry 0 (String.length prefix) = prefix then
      Some (String.sub entry (String.length prefix) (String.length entry - String.length prefix))
    else None) environment

let exec_search program arguments environment =
  let argv = Array.of_list (program :: arguments) in
  let execute path = Unix.execve path argv environment in
  if String.contains program '/' then execute program
  else
    let path = Option.value (environment_value environment "PATH") ~default:"/bin:/usr/bin" in
    let denied = ref false in
    let rec try_directories = function
      | [] ->
          raise (Unix.Unix_error
            ((if !denied then Unix.EACCES else Unix.ENOENT), "execve", program))
      | directory :: rest ->
          let directory = if directory = "" then "." else directory in
          (try execute (Filename.concat directory program)
           with
           | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> try_directories rest
           | Unix.Unix_error (Unix.EACCES, _, _) -> denied := true; try_directories rest)
    in
    try_directories (String.split_on_char ':' path)

let spawn_native ?cancel ?deadline ~program ~arguments ~cwd ~environment ~merge_stderr () =
  check_start ?cancel ?deadline ();
  let stdin_read, stdin_write = Unix.pipe ~cloexec:true () in
  let stdout_read, stdout_write =
    try Unix.pipe ~cloexec:true ()
    with exn -> close_fd stdin_read; close_fd stdin_write; raise exn
  in
  let ready_read, ready_write =
    try Unix.pipe ~cloexec:true ()
    with exn ->
      close_fd stdin_read; close_fd stdin_write;
      close_fd stdout_read; close_fd stdout_write;
      raise exn
  in
  let closed = Array.make 6 false in
  let cleanup () =
    close_tracked closed 0 stdin_read; close_tracked closed 1 stdin_write;
    close_tracked closed 2 stdout_read; close_tracked closed 3 stdout_write;
    close_tracked closed 4 ready_read; close_tracked closed 5 ready_write
  in
  try
    match Unix.fork () with
    | 0 ->
        (try
           close_fd stdin_write; close_fd stdout_read; close_fd ready_read;
           ignore (Unix.setsid ());
           if cwd <> "" then Unix.chdir cwd;
           Unix.dup2 stdin_read Unix.stdin;
           Unix.dup2 stdout_write Unix.stdout;
           if merge_stderr then Unix.dup2 stdout_write Unix.stderr;
           close_fd stdin_read; close_fd stdout_write;
           if Unix.write ready_write (Bytes.make 1 (Char.chr 0)) 0 1 <> 1 then
             fail "could not establish process session";
           close_fd ready_write;
           exec_search program arguments environment
         with exn ->
           let message = "pave process launcher: " ^ Printexc.to_string exn ^ "\n" in
           (try ignore (Unix.write_substring Unix.stderr message 0 (String.length message))
            with _ -> ());
           Unix._exit 127)
    | pid ->
        close_tracked closed 0 stdin_read;
        close_tracked closed 3 stdout_write;
        close_tracked closed 5 ready_write;
        Unix.set_nonblock stdin_write;
        Unix.set_nonblock stdout_read;
        Unix.set_nonblock ready_read;
        (try
           await_spawn_session ?cancel ?deadline ready_read;
           close_tracked closed 4 ready_read;
           (pid, stdin_write, stdout_read)
         with exn ->
           signal_group pid Sys.sigkill;
           (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
           (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
           raise exn)
  with exn ->
    cleanup ();
    raise exn

let spawn ?cancel ?deadline ~program ~arguments ~cwd ~environment ~pty_mode () =
  if pty_mode then spawn_pty ?cancel ?deadline ~program ~arguments ~cwd ~environment ()
  else spawn_native ?cancel ?deadline ~program ~arguments ~cwd ~environment ~merge_stderr:true ()

let process_description program arguments =
  let rendered = Filename.quote_command program arguments in
  if String.length rendered <= 2048 then rendered
  else String.sub rendered 0 2045 ^ "..."

let status_of_unix (status : Unix.process_status) : termination =
  match status with
  | Unix.WEXITED code -> Exited code
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> Signaled signal

let termination_of_status (status : process_status) : termination =
  match status with
  | Running -> fail "process is still running"
  | Completed termination -> termination


let terminate_group pid =
  let exists =
    try Unix.kill (-pid) 0; true
    with Unix.Unix_error (Unix.ESRCH, _, _) -> false
       | Unix.Unix_error (Unix.EPERM, _, _) -> true in
  if exists then (
    signal_group pid Sys.sigterm;
    Thread.delay 0.12;
    signal_group pid Sys.sigkill)

let summary_locked (job : job) : job_summary = {
  id = job.id; command = job.command; status = job.status;
  bytes_received = job.bytes_received; truncated = job.truncated;
  created_at = job.created_at; ready = job.ready;
}

let trim_front job count =
  let count = min count (String.length job.output) in
  if count > 0 then (
    job.output <- String.sub job.output count (String.length job.output - count);
    job.output_start <- job.output_start + count;
    job.owner.retained_bytes <- job.owner.retained_bytes - count;
    job.truncated <- true)

let oldest_with_output manager except_id =
  Hashtbl.fold (fun _ job current ->
    if job.output = "" || job.id = except_id then current
    else match current with
      | None -> Some job
      | Some old when job.created_at < old.created_at -> Some job
      | Some _ -> current) manager.jobs None

let make_room manager except_id =
  let rec loop () =
    if manager.retained_bytes > manager.retained_output_bytes then (
      let candidate = match oldest_with_output manager except_id with
        | Some job -> Some job
        | None -> Hashtbl.find_opt manager.jobs except_id in
      match candidate with
      | Some job when job.output <> "" ->
          trim_front job (manager.retained_bytes - manager.retained_output_bytes);
          loop ()
      | _ -> ())
  in
  loop ()

let append_output manager job data count =
  if count > 0 then (
    let received = job.bytes_received + count in
    let received = if received < job.bytes_received then max_int else received in
    job.bytes_received <- received;
    let old_size = String.length job.output in
    if job.output_limit = 0 then (
      job.output_start <- received;
      job.truncated <- true)
    else (
      let combined = job.output ^ Bytes.sub_string data 0 count in
      let retained = min job.output_limit (String.length combined) in
      job.output <- String.sub combined (String.length combined - retained) retained;
      job.output_start <- received - retained;
      manager.retained_bytes <- manager.retained_bytes + retained - old_size;
      if retained < String.length combined then job.truncated <- true;
      make_room manager job.id))

let output_chunk_locked job ~offset ~max_bytes =
  if offset > job.bytes_received then fail "output offset is beyond the received output";
  let clipped = max offset job.output_start in
  let start = min (String.length job.output) (clipped - job.output_start) in
  let count = min max_bytes (String.length job.output - start) in
  { first_offset = job.output_start; offset = clipped;
    next_offset = clipped + count;
    output = String.sub job.output start count; truncated = job.truncated }

let lookup manager id =
  match Hashtbl.find_opt manager.jobs id with
  | Some job -> job
  | None -> fail ("unknown process job: " ^ id)

let close_stdin_job job =
  with_lock job.input_lock (fun () ->
    let fd = with_lock job.owner.lock (fun () ->
      let fd = job.stdin_fd in
      job.stdin_fd <- None;
      fd) in
    Option.iter close_fd fd)

let write_stdin_job ?cancel job data =
  if String.length data > max_stdin_bytes then fail "stdin data exceeds its size limit";
  with_lock job.input_lock (fun () ->
    let fd = with_lock job.owner.lock (fun () ->
      if job.status <> Running then fail "process job is not running";
      match job.stdin_fd with Some fd -> fd | None -> fail "process stdin is closed") in
    let bytes = Bytes.unsafe_of_string data in
    let deadline = Unix.gettimeofday () +. 1.0 in
    let rec loop offset =
      if offset >= Bytes.length bytes then true
      else if Option.fold ~none:false ~some:(fun callback -> callback ()) cancel then false
      else (
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining <= 0. then fail "process stdin write timed out";
        let _, writable, _ = Unix.select [] [fd] [] (min 0.05 remaining) in
        if writable <> [] then
          try
            let count = Unix.write fd bytes offset (Bytes.length bytes - offset) in
            if count = 0 then fail "process stdin is closed";
            loop (offset + count)
          with Unix.Unix_error (Unix.EINTR, _, _) -> loop offset
             | Unix.Unix_error ((Unix.EPIPE | Unix.EBADF), _, _) -> fail "process stdin is closed"
        else loop offset)
    in
    loop 0)

let notify_exit manager job =
  let notification = with_lock manager.lock (fun () ->
    if job.exit_notified then None
    else (
      job.exit_notified <- true;
      Option.map (fun callback ->
        callback,
        { termination = termination_of_status job.status;
          output = job.output;
          bytes_received = job.bytes_received;
          truncated = job.truncated }) manager.on_exit)) in
  Option.iter (fun (callback, result) ->
    try callback ~id:job.id result with _ -> ()) notification

let finalize_job job child_termination =
  let manager = job.owner in
  close_stdin_job job;
  let fd = with_lock manager.lock (fun () ->
    let fd = job.stdout_fd in
    job.stdout_fd <- None;
    fd) in
  Option.iter close_fd fd;
  with_lock manager.lock (fun () ->
    let final_termination = match job.requested_termination with
      | Some Timed_out -> Timed_out
      | Some Cancelled -> Cancelled
      | Some (Exited _ | Signaled _) -> child_termination
      | None -> child_termination
    in
    job.status <- Completed final_termination);
  notify_exit manager job;
  with_lock manager.lock (fun () -> job.worker_done <- true)

let close_stdout_job job =
  let fd = with_lock job.owner.lock (fun () ->
    let fd = job.stdout_fd in
    job.stdout_fd <- None;
    fd) in
  Option.iter close_fd fd

let read_available manager job fd bytes =
  let rec drain budget =
    if budget = 0 then `Again
    else
      try
        let count = Unix.read fd bytes 0 (Bytes.length bytes) in
        if count > 0 then (
          with_lock manager.lock (fun () -> append_output manager job bytes count);
          drain (budget - 1))
        else `Eof
      with
      | Unix.Unix_error (Unix.EINTR, _, _) -> drain budget
      | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> `Again
  in
  drain 16

let monitor_job job =
  let manager = job.owner in
  let pid = job.pid in
  let termination_sent = ref false in
  let child_result = ref None in
  let finished = ref false in
  let read_buffer = Bytes.create 8192 in
  let record_child_status child_status =
    child_result := Some child_status;
    job.status <- Completed (match job.requested_termination with
      | Some Timed_out -> Timed_out
      | Some Cancelled -> Cancelled
      | Some (Exited _ | Signaled _) | None -> child_status)
  in
  let poll_child () =
    if !child_result = None then
      try
        with_lock manager.lock (fun () ->
          match Unix.waitpid [Unix.WNOHANG] pid with
          | 0, _ -> ()
          | _, status -> record_child_status (status_of_unix status))
      with Unix.Unix_error (Unix.EINTR, _, _) -> ()
         | Unix.Unix_error (Unix.ECHILD, _, _) ->
             with_lock manager.lock (fun () -> record_child_status (Signaled Sys.sigkill)) in
  let get_request_or_timeout () = with_lock manager.lock (fun () ->
    match job.requested_termination with
    | Some reason -> Some reason
    | None -> (match job.deadline with
        | Some deadline when !child_result = None && Unix.gettimeofday () >= deadline ->
            job.requested_termination <- Some Timed_out;
            Some Timed_out
        | _ -> None)) in
  let rec loop () =
    if not !finished then (
      poll_child ();
      (match get_request_or_timeout () with
       | Some _ when not !termination_sent ->
           termination_sent := true;
           close_stdin_job job;
           terminate_group pid
       | _ -> ());
      (match job.stdout_fd with
       | Some fd ->
           (try ignore (Unix.select [fd] [] [] 0.05) with Unix.Unix_error (Unix.EINTR, _, _) -> ());
           (match read_available manager job fd read_buffer with
            | `Eof -> close_stdout_job job
            | `Again -> ())
       | None -> Thread.delay 0.02);
      (match !child_result with
       | Some result ->
           (* The group is the owned unit: terminate every same-session
              descendant before releasing this record or notifying its owner. *)
           if not !termination_sent then (
             termination_sent := true;
             terminate_group pid);
           (match job.stdout_fd with
            | Some fd -> ignore (read_available manager job fd read_buffer)
            | None -> ());
           finished := true;
           finalize_job job result
       | None -> loop ()))
  in
  try loop () with _ ->
    let child_termination = match !child_result with
      | Some result ->
          if not !termination_sent then (
            termination_sent := true;
            terminate_group pid);
          result
      | None ->
          signal_group pid Sys.sigkill;
          (try
             let _, status = Unix.waitpid [] pid in
             status_of_unix status
           with Unix.Unix_error _ -> Signaled Sys.sigkill)
    in
    let fd = with_lock manager.lock (fun () ->
      let fd = job.stdout_fd in job.stdout_fd <- None; fd) in
    Option.iter close_fd fd;
    close_stdin_job job;
    with_lock manager.lock (fun () ->
      job.status <- Completed (match job.requested_termination with
        | Some Timed_out -> Timed_out
        | Some Cancelled -> Cancelled
        | _ -> child_termination));
    notify_exit manager job;
    with_lock manager.lock (fun () -> job.worker_done <- true)
let remove_job_locked manager job =
  Hashtbl.remove manager.jobs job.id;
  manager.retained_bytes <- manager.retained_bytes - String.length job.output

let prune_records_locked manager =
  if Hashtbl.length manager.jobs >= max_job_records then (
    let candidate = Hashtbl.fold (fun _ job current ->
      if not job.worker_done then current
      else match current with
        | None -> Some job
        | Some old when job.created_at < old.created_at -> Some job
        | Some _ -> current) manager.jobs None in
    match candidate with Some job -> remove_job_locked manager job
    | None -> fail "too many retained process jobs")

(* Frees an id held by a finished record so it can start again; returns false
   when the job is still live. Unknown ids are free. *)
let release_finished manager ~id =
  validate_id id;
  with_lock manager.lock (fun () ->
    match Hashtbl.find_opt manager.jobs id with
    | None -> true
    | Some job when job.worker_done -> remove_job_locked manager job; true
    | Some _ -> false)

let start_internal manager ~id ?(cwd = None) ?(environment = [])
    ?(inherit_environment = true) ?timeout_seconds ?cancel
    ?(output_limit = default_output_limit) ?(pty_mode = false)
    ~program ~arguments () =
  validate_id id;
  validate_program program arguments;
  validate_limit "output_limit" max_output_limit output_limit;
  let timeout_seconds = validate_timeout "timeout_seconds" timeout_seconds in
  let cwd = validate_cwd cwd in
  let environment = environment_with_overrides ~inherit_environment environment in
  let command = process_description program arguments in
  check_start ?cancel ();
  with_lock manager.lock (fun () ->
    if manager.closed then fail "process manager is closed";
    if Hashtbl.mem manager.jobs id then fail "process job id is already in use";
    let live = Hashtbl.fold (fun _ job count -> if not job.worker_done then count + 1 else count) manager.jobs 0 in
    if live >= manager.max_jobs then fail "process manager live-job limit reached";
    prune_records_locked manager;
    let created_at = Unix.gettimeofday () in
    let deadline = Option.map (fun seconds -> created_at +. float seconds) timeout_seconds in
    let pid, stdin_fd, stdout_fd =
      try spawn ?cancel ?deadline ~program ~arguments ~cwd ~environment ~pty_mode ()
      with Unix.Unix_error (error, _, _) ->
        fail ("could not start process: " ^ Unix.error_message error)
    in
    let job = {
      owner = manager; id; command; pid; created_at; output_limit;
      output = ""; output_start = 0; bytes_received = 0; truncated = false;
      status = Running; requested_termination = None; stdin_fd = Some stdin_fd;
      stdout_fd = Some stdout_fd; worker_done = false; exit_notified = false;
      worker = None; ready = false;
      deadline;
      input_lock = Mutex.create ();
    } in
    Hashtbl.add manager.jobs id job;
    (try
       let worker = Thread.create monitor_job job in
       job.worker <- Some worker
     with exn ->
       Hashtbl.remove manager.jobs id;
       close_fd stdin_fd; close_fd stdout_fd;
       signal_group pid Sys.sigkill;
       (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
       raise exn))

let start manager ~id ?cwd ?environment ?inherit_environment ?timeout_seconds
    ?output_limit ?pty ~program ~arguments () =
  try
    start_internal manager ~id ?cwd ?environment ?inherit_environment
      ?timeout_seconds ?output_limit
      ~pty_mode:(Option.value pty ~default:false) ~program ~arguments ()
  with Start_interrupted Timed_out -> fail "process startup timed out"
     | Start_interrupted _ -> fail "process startup interrupted"

let start_shell manager ~id ?cwd ?environment ?inherit_environment
    ?timeout_seconds ?output_limit ?pty ~command () =
  validate_text "shell command" 65_536 command;
  start manager ~id ?cwd ?environment ?inherit_environment ?timeout_seconds
    ?output_limit ?pty ~program:"/bin/sh" ~arguments:["-c"; command] ()

let read_output manager ~id ?(offset = 0) ?(max_bytes = 65_536) () =
  validate_id id;
  validate_limit "max_bytes" 65_536 max_bytes;
  if offset < 0 then fail "output offset must be nonnegative";
  with_lock manager.lock (fun () -> output_chunk_locked (lookup manager id) ~offset ~max_bytes)

let job_status manager ~id =
  validate_id id;
  with_lock manager.lock (fun () -> (lookup manager id).status)

let job_pid manager ~id =
  validate_id id;
  with_lock manager.lock (fun () -> (lookup manager id).pid)

let jobs manager =
  with_lock manager.lock (fun () ->
    Hashtbl.fold (fun _ (job : job) rows -> summary_locked job :: rows) manager.jobs []
    |> List.sort (fun (left : job_summary) (right : job_summary) ->
      Float.compare left.created_at right.created_at))

let check_wait_cancel = function
  | Some cancelled when cancelled () -> fail "process wait cancelled"
  | _ -> ()

let wait_job manager ~id ?timeout_seconds ?cancel () =
  validate_id id;
  let timeout_seconds = validate_timeout ~maximum:86_400 "wait timeout" timeout_seconds in
  let deadline = Option.map (fun seconds -> Unix.gettimeofday () +. float seconds) timeout_seconds in
  let rec loop () =
    check_wait_cancel cancel;
    let status, worker_done = with_lock manager.lock (fun () ->
      let job = lookup manager id in job.status, job.worker_done) in
    if worker_done then status
    else match deadline with
      | Some expires when Unix.gettimeofday () >= expires -> status
      | _ -> Thread.delay 0.02; loop ()
  in
  loop ()

let request_termination manager ~id (reason : termination) =
  validate_id id;
  with_lock manager.lock (fun () ->
    if manager.closed then fail "process manager is closed";
    let job = lookup manager id in
    if job.status = Running && job.requested_termination = None then
      job.requested_termination <- Some reason)

let kill_job manager ~id =
  request_termination manager ~id Cancelled;
  ignore (wait_job manager ~id ~timeout_seconds:5 ())

let close_stdin manager ~id =
  validate_id id;
  let job = with_lock manager.lock (fun () -> lookup manager id) in
  close_stdin_job job

let write_stdin manager ~id ~data =
  validate_id id;
  let job = with_lock manager.lock (fun () -> lookup manager id) in
  ignore (write_stdin_job job data)

let port_accepting port =
  if port < 1 || port > 65_535 then fail "readiness port must be between 1 and 65535";
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> close_fd fd) (fun () ->
    Unix.set_nonblock fd;
    try
      Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
      true
    with
    | Unix.Unix_error ((Unix.EINPROGRESS | Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) ->
        let _, writable, _ =
          try Unix.select [] [fd] [] 0.05
          with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], [] in
        writable <> [] && Unix.getsockopt_error fd = None
    | Unix.Unix_error
        ((Unix.EINTR | Unix.ECONNREFUSED | Unix.ETIMEDOUT | Unix.EHOSTUNREACH | Unix.ENETUNREACH),
         _, _) -> false)

let wait_ready manager ~id ?cancel ?(timeout_seconds = 10) ?log_regex ?port () =
  validate_id id;
  let timeout_seconds = validate_timeout ~maximum:300 "readiness timeout" (Some timeout_seconds)
    |> Option.get in
  if log_regex = None && port = None then fail "readiness needs a log regex or loopback port";
  (match log_regex with
   | Some pattern ->
       validate_text "readiness regex" 512 pattern;
       (try ignore (Str.regexp pattern) with Failure _ -> fail "invalid readiness regex")
   | None -> ());
  Option.iter (fun value -> if value < 1 || value > 65_535 then fail "readiness port must be between 1 and 65535") port;
  let regex = Option.map Str.regexp log_regex in
  let deadline = Unix.gettimeofday () +. float timeout_seconds in
  let rec loop () =
    check_wait_cancel cancel;
    let status, content = with_lock manager.lock (fun () ->
      let job = lookup manager id in job.status, job.output) in
    if status <> Running then false
    else
      let log_ready = match regex with
        | None -> true
        | Some regex ->
            (try ignore (Str.search_forward regex content 0); true
             with Not_found -> false) in
      let port_ready =
        match port with None -> true | Some value -> port_accepting value in
      if log_ready && port_ready then
        with_lock manager.lock (fun () ->
          let job = lookup manager id in
          if job.status = Running then (job.ready <- true; true)
          else false)
      else if Unix.gettimeofday () >= deadline then false
      else (Thread.delay 0.05; loop ())
  in
  loop ()

let result_of_job manager ~id =
  with_lock manager.lock (fun () ->
    let job = lookup manager id in
    { termination = termination_of_status job.status; output = job.output;
      bytes_received = job.bytes_received; truncated = job.truncated })

(* Mark active jobs for cancellation; each monitor owns group signaling,
   descendant cleanup and direct-child reaping before its record becomes terminal. *)
let close_manager manager =
  let jobs = with_lock manager.lock (fun () ->
    if manager.closed then []
    else (
      manager.closed <- true;
      let jobs = Hashtbl.fold (fun _ job rows -> job :: rows) manager.jobs [] in
      List.iter (fun job ->
        if job.status = Running && job.requested_termination = None then
          job.requested_termination <- Some Cancelled) jobs;
      jobs)) in
  let self_id = Thread.id (Thread.self ()) in
  List.iter (fun job ->
    Option.iter (fun worker ->
      if Thread.id worker <> self_id then Thread.join worker) job.worker) jobs;
  with_lock manager.lock (fun () -> List.iter (fun job -> job.pid <- 0) jobs)

let run ?cancel ?on_progress ?timeout_seconds ?(output_limit = default_output_limit)
    ?cwd ?environment ?inherit_environment ?(stdin = "") ?(pty = false)
    ~program ~arguments () =
  validate_program program arguments;
  if String.length stdin > max_stdin_bytes then fail "stdin data exceeds its size limit";
  validate_limit "output_limit" max_output_limit output_limit;
  let timeout_seconds = Option.value timeout_seconds ~default:default_run_timeout_seconds in
  try
  check_start ?cancel ();
  let manager = create_manager ~max_jobs:1 ~retained_output_bytes:output_limit () in
  Fun.protect ~finally:(fun () -> close_manager manager) (fun () ->
    start_internal manager ~id:"run" ?cwd ?environment ?inherit_environment ?cancel
      ~timeout_seconds ~output_limit ~pty_mode:pty ~program ~arguments ();
    let job = with_lock manager.lock (fun () -> lookup manager "run") in
    let cancel_requested = ref false in
    if stdin <> "" && not (write_stdin_job ?cancel job stdin) then (
      cancel_requested := true;
      request_termination manager ~id:"run" Cancelled);
    close_stdin_job job;
    let last_progress = ref (-1) in
    let rec wait () =
      let status, received, worker_done =
        with_lock manager.lock (fun () -> job.status, job.bytes_received, job.worker_done) in
      (match on_progress with
       | Some callback when received <> !last_progress ->
           last_progress := received; callback received
       | _ -> ());
      (match cancel with
       | Some callback when status = Running && not !cancel_requested && callback () ->
           cancel_requested := true;
           request_termination manager ~id:"run" Cancelled
       | _ -> ());
      if not worker_done then (Thread.delay 0.02; wait ())
    in
    wait ();
    result_of_job manager ~id:"run")
  with Start_interrupted termination ->
    { termination; output = ""; bytes_received = 0; truncated = false }

let run_shell ?cancel ?on_progress ?timeout_seconds ?output_limit ?cwd
    ?environment ?inherit_environment ?stdin ?pty ~command () =
  validate_text "shell command" 65_536 command;
  run ?cancel ?on_progress ?timeout_seconds ?output_limit ?cwd ?environment
    ?inherit_environment ?stdin ?pty ~program:"/bin/sh"
    ~arguments:["-c"; command] ()

