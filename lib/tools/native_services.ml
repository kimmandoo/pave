type 'a availability = Available of 'a | Unavailable of string

exception Error of string
exception Helper_unavailable

let fail message = raise (Error message)

let max_image_bytes = 10 * 1024 * 1024
let max_clipboard_bytes = 1024 * 1024
let max_ocr_output_bytes = 1024 * 1024
let operation_timeout_seconds = 20.

type runner = {
  run : cancel:(unit -> bool) -> timeout_seconds:float -> output_limit:int ->
    program:string -> arguments:string list -> stdin:string ->
    Workspace_process.result;
}

let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

let helper_environment () =
  let variables = [
    "PATH", Some "/usr/bin:/bin:/usr/local/bin:/opt/homebrew/bin:/opt/local/bin";
    "LANG", Some "C";
    "LC_ALL", Some "C";
    "HOME", Sys.getenv_opt "HOME";
    "TMPDIR", Sys.getenv_opt "TMPDIR";
    "DISPLAY", Sys.getenv_opt "DISPLAY";
    "WAYLAND_DISPLAY", Sys.getenv_opt "WAYLAND_DISPLAY";
    "XDG_RUNTIME_DIR", Sys.getenv_opt "XDG_RUNTIME_DIR";
    "DBUS_SESSION_BUS_ADDRESS", Sys.getenv_opt "DBUS_SESSION_BUS_ADDRESS";
    "XAUTHORITY", Sys.getenv_opt "XAUTHORITY"
  ] in
  List.filter_map (fun (name, value) ->
    Option.map (fun value -> name ^ "=" ^ value) value) variables
  |> Array.of_list
let run_native ~cancel ~timeout_seconds ~output_limit ~program ~arguments ~stdin =
  let input_read, input_write = Unix.pipe ~cloexec:true () in
  let output_read, output_write =
    try Unix.pipe ~cloexec:true ()
    with exn -> close_noerr input_read; close_noerr input_write; raise exn in
  let null_fd =
    try Unix.openfile "/dev/null" [Unix.O_WRONLY] 0
    with exn ->
      List.iter close_noerr [input_read; input_write; output_read; output_write];
      raise exn in
  let argv = Array.of_list (program :: arguments) in
  let environment = helper_environment () in
  let pid =
    try
      match Unix.fork () with
      | 0 ->
          (try
             close_noerr input_write; close_noerr output_read;
             ignore (Unix.setsid ());
             Unix.dup2 input_read Unix.stdin;
             Unix.dup2 output_write Unix.stdout;
             Unix.dup2 null_fd Unix.stderr;
             List.iter close_noerr [input_read; output_write; null_fd];
             Unix.execve program argv environment
           with _ -> Unix._exit 127)
      | pid -> pid
    with exn ->
      List.iter close_noerr [input_read; input_write; output_read; output_write; null_fd];
      raise exn in
  close_noerr input_read;
  close_noerr output_write;
  close_noerr null_fd;
  (try Unix.set_nonblock input_write; Unix.set_nonblock output_read
   with exn ->
     (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> ());
     (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
     close_noerr input_write; close_noerr output_read;
     (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
     raise exn);
  let output = Buffer.create (min output_limit 4096) in
  let input_offset = ref 0 and input_open = ref true and output_open = ref true in
  let received = ref 0 and truncated = ref false and termination = ref None in
  let child_status = ref None and finished = ref false in
  let kill_owned () =
    (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> ());
    if !child_status = None then
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()) in
  let deadline = Unix.gettimeofday () +. timeout_seconds in
  let stop reason =
    kill_owned ();
    termination := Some reason
  in
  let drain () =
    let bytes = Bytes.create 8192 in
    let rec loop () =
      try
        let count = Unix.read output_read bytes 0
            (min (Bytes.length bytes) (max 1 (output_limit + 1 - !received))) in
        if count = 0 then (output_open := false; close_noerr output_read)
        else (
          let remaining = max 0 (output_limit - !received) in
          let retained = min count remaining in
          if retained > 0 then Buffer.add_subbytes output bytes 0 retained;
          received := !received + count;
          if !received > output_limit then (truncated := true; stop (Workspace_process.Exited 0))
          else loop ())
      with
      | Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
      | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
    in
    loop ()
  in
  let write_input () =
    if !input_open then
      if !input_offset = String.length stdin then
        (input_open := false; close_noerr input_write)
      else
        try
          let count = Unix.write_substring input_write stdin !input_offset
              (String.length stdin - !input_offset) in
          input_offset := !input_offset + count
        with
        | Unix.Unix_error (Unix.EINTR, _, _) -> ()
        | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
        | Unix.Unix_error (Unix.EPIPE, _, _) ->
            input_open := false; close_noerr input_write
  in
  let poll_child () =
    if !child_status = None then
      match Unix.waitpid [Unix.WNOHANG] pid with
      | 0, _ -> ()
      | _, status -> child_status := Some status
  in
  let rec loop () =
    if !termination = None then (
      if cancel () then stop Workspace_process.Cancelled
      else if Unix.gettimeofday () >= deadline then stop Workspace_process.Timed_out);
    if !termination = None then (
      let readable, writable, _ = Unix.select
          (if !output_open then [output_read] else [])
          (if !input_open then [input_write] else []) [] 0.02 in
      if readable <> [] then drain ();
      if writable <> [] then write_input ();
      poll_child ();
      if !child_status = None || !output_open then loop ())
  in
  Fun.protect
    ~finally:(fun () ->
      if !input_open then (input_open := false; close_noerr input_write);
      if !output_open then (output_open := false; close_noerr output_read);
      if not !finished then kill_owned ();
      (match !child_status with
       | Some _ -> ()
       | None ->
           (* Exceptional I/O/callback exits must not wait on a live helper. *)
           let rec reap () =
             try ignore (Unix.waitpid [] pid)
             with Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
                | Unix.Unix_error (Unix.ECHILD, _, _) -> () in
           reap ()))
    (fun () ->
      loop ();
      let status = match !child_status with
        | Some status -> status
        | None ->
            let status = snd (Unix.waitpid [] pid) in
            child_status := Some status;
            status in
      let child_result = match status with
        | Unix.WEXITED code -> Workspace_process.Exited code
        | Unix.WSIGNALED signal -> Workspace_process.Signaled signal
        | Unix.WSTOPPED signal -> Workspace_process.Signaled signal in
      let final = if !truncated then Workspace_process.Exited 0 else
        match !termination with
        | Some Workspace_process.Timed_out -> Workspace_process.Timed_out
        | Some Workspace_process.Cancelled -> Workspace_process.Cancelled
        | Some (Workspace_process.Exited _ | Workspace_process.Signaled _) -> child_result
        | None -> child_result in
      let result = { Workspace_process.termination = final; output = Buffer.contents output;
        bytes_received = !received; truncated = !truncated } in
      finished := true;
      result)

let system_runner = { run = run_native }

let executable path =
  Sys.file_exists path && try Unix.access path [Unix.X_OK]; true
  with Unix.Unix_error _ -> false

let first_executable paths = List.find_opt executable paths

let tesseract () = first_executable [
  "/usr/bin/tesseract"; "/usr/local/bin/tesseract";
  "/opt/homebrew/bin/tesseract"; "/opt/local/bin/tesseract"]

let starts_with bytes prefix =
  String.length bytes >= String.length prefix &&
  String.sub bytes 0 (String.length prefix) = prefix


let image_format mime bytes =
  let signatures = [
    "image/png", starts_with bytes "\x89PNG\r\n\x1a\n";
    "image/jpeg", starts_with bytes "\xff\xd8\xff";
    "image/gif", starts_with bytes "GIF87a" || starts_with bytes "GIF89a";
    "image/tiff", starts_with bytes "II*\000" || starts_with bytes "MM\000*";
    "image/bmp", starts_with bytes "BM"] in
  match List.find_opt (fun (kind, _) -> kind = mime) signatures with
  | None -> fail "OCR MIME type must be PNG, JPEG, GIF, TIFF, or BMP"
  | Some (_, false) -> fail "OCR image bytes do not match the declared MIME type"
  | Some (_, true) -> ()

let process_result runner ~cancel ~program ~arguments ~stdin ~limit =
  if cancel () then fail "native service cancelled";
  let result =
    try runner.run ~cancel ~timeout_seconds:operation_timeout_seconds
        ~output_limit:limit ~program ~arguments ~stdin
    with Unix.Unix_error ((Unix.ENOENT | Unix.EACCES), _, _) -> raise Helper_unavailable in
  match result.Workspace_process.termination with
  | Workspace_process.Cancelled -> fail "native service cancelled"
  | Workspace_process.Timed_out -> fail "native service timed out"
  | Workspace_process.Signaled signal ->
      fail (Printf.sprintf "native helper terminated by signal %d" signal)
  | Workspace_process.Exited 127 -> raise Helper_unavailable
  | Workspace_process.Exited 0 ->
      if result.truncated || result.bytes_received > limit ||
         String.length result.output > limit then
        fail "native helper output exceeded its size limit";
      result.output
  | Workspace_process.Exited code ->
      fail (Printf.sprintf "native helper failed (exit %d)" code)

let recognize ?(runner = system_runner) ?(cancel = fun () -> false)
    ?(find_tesseract = tesseract) ~mime bytes =
  if String.length bytes > max_image_bytes then fail "OCR image exceeds the 10 MiB limit";
  image_format mime bytes;
  match find_tesseract () with
  | None -> Unavailable "OCR is unavailable: Tesseract is not installed at a supported fixed path"
  | Some program ->
      (try
         let text = process_result runner ~cancel ~program
             ~arguments:["stdin"; "stdout"] ~stdin:bytes ~limit:max_ocr_output_bytes in
         Available text
       with Helper_unavailable ->
         Unavailable "OCR is unavailable: Tesseract could not be started")

type platform = Macos | Linux_wayland | Linux_x11 | Unsupported

let platform ?(getenv = Sys.getenv_opt) ?(os_type = Sys.os_type)
    ?(system_library_exists = fun () -> Sys.file_exists "/System/Library") () =
  match os_type with
  | "Unix" when system_library_exists () -> Macos
  | "Unix" ->
      (match getenv "WAYLAND_DISPLAY", getenv "DISPLAY" with
       | Some wayland, _ when wayland <> "" -> Linux_wayland
       | _, Some display when display <> "" -> Linux_x11
       | _ -> Unsupported)
  | _ -> Unsupported

let clipboard_helper ?(getenv = Sys.getenv_opt) ?(os_type = Sys.os_type)
    ?(system_library_exists = fun () -> Sys.file_exists "/System/Library")
    ~write () =
  match platform ~getenv ~os_type ~system_library_exists () with
  | Macos -> Some ((if write then "/usr/bin/pbcopy" else "/usr/bin/pbpaste"), [])
  | Linux_wayland ->
      let program = if write then "/usr/bin/wl-copy" else "/usr/bin/wl-paste" in
      Some (program, if write then [] else ["--no-newline"])
  | Linux_x11 ->
      let program = "/usr/bin/xclip" in
      Some (program, if write then ["-selection"; "clipboard"; "-in"]
             else ["-selection"; "clipboard"; "-out"])
  | Unsupported -> None

let clipboard ?(runner = system_runner) ?(cancel = fun () -> false)
    ?(getenv = Sys.getenv_opt) ?(os_type = Sys.os_type)
    ?(system_library_exists = fun () -> Sys.file_exists "/System/Library")
    ?(helper_exists = executable) () =
  match clipboard_helper ~getenv ~os_type ~system_library_exists ~write:false () with
  | None -> Unavailable "clipboard is unavailable on this platform or session"
  | Some (program, arguments) ->
      if not (helper_exists program) then
        Unavailable "clipboard is unavailable: required system helper is missing"
      else (try
        Available (process_result runner ~cancel ~program ~arguments ~stdin:""
                    ~limit:max_clipboard_bytes)
      with Helper_unavailable ->
        Unavailable "clipboard is unavailable: required system helper could not be started")

let clipboard_write ?(runner = system_runner) ?(cancel = fun () -> false)
    ?(getenv = Sys.getenv_opt) ?(os_type = Sys.os_type)
    ?(system_library_exists = fun () -> Sys.file_exists "/System/Library")
    ?(helper_exists = executable) text =
  if String.length text > max_clipboard_bytes then
    fail "clipboard data exceeds the 1 MiB limit";
  match clipboard_helper ~getenv ~os_type ~system_library_exists ~write:true () with
  | None -> Unavailable "clipboard is unavailable on this platform or session"
  | Some (program, arguments) ->
      if not (helper_exists program) then
        Unavailable "clipboard is unavailable: required system helper is missing"
      else try
        ignore (process_result runner ~cancel ~program ~arguments ~stdin:text ~limit:0);
        Available ()
      with Helper_unavailable ->
        Unavailable "clipboard is unavailable: required system helper could not be started"
