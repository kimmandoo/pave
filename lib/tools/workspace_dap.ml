exception Error of string
exception Cancelled
exception Not_approved of string

type authorization_effect =
  | Adapter_process
  | Launch
  | Remote_host of string
  | Breakpoints
  | Debug_execution
  | Evaluate

(* The injected transport owns adapter I/O. [receive] returns None on timeout/EOF;
   its cancellation callback lets a blocking transport stop promptly. *)
type transport = {
(* Remote transports should connect lazily on their first send. Requests on a
   configured remote session cannot reach that send until its host is trusted. *)
  send : timeout_seconds:float -> cancelled:(unit -> bool) -> string -> unit;
  receive : timeout_seconds:float -> cancelled:(unit -> bool) -> string option;
  close : unit -> unit;
}
type transport_factory = timeout_seconds:float -> cancelled:(unit -> bool) -> transport

type phase = Fresh | Initialized | Awaiting_configuration | Configured

(* Each effect is authorized separately by the parent tool layer. A denial must
   raise before the corresponding request is sent; no Boolean consent is accepted. *)
type manager = {
  owner_id : string;
  root : string;
  authorize : authorization_effect -> unit;
  sessions : (string, session) Hashtbl.t;
  lock : Mutex.t;
  mutable manager_closed : bool;
}
and session = {
  id : string;
  manager : manager;
  mutable transport : transport option;
  transport_factory : transport_factory option;
  operation_lock : Mutex.t;
  state_lock : Mutex.t;
  mutable closed : bool;
  mutable phase : phase;
  mutable next_request_seq : int;
  mutable last_adapter_seq : int;
  mutable receive_buffer : string;
  mutable retained_events : Yojson.Basic.t list;
  mutable retained_event_bytes : int;
  mutable stopped : bool;
  mutable state_event_generation : int;
  mutable terminated : bool;
  mutable trusted_attach_host : string option;
  configured_remote_host : string option;
}
type stdio_child = {
  pid : int;
  stdin_fd : Unix.file_descr;
  stdout_fd : Unix.file_descr;
  process_lock : Mutex.t;
  write_lock : Mutex.t;
  read_lock : Mutex.t;
  read_buffer : bytes;
  mutable process_closed : bool;
}

type breakpoint = {
  line : int;
  condition : string option;
  hit_condition : string option;
  log_message : string option;
}

let max_sessions = 32
let max_frame_bytes = 1_048_576
let max_path_bytes = 4_096
let max_header_bytes = 8_192
let max_buffer_bytes = (2 * max_frame_bytes) + (2 * max_header_bytes)
let max_event_bytes = 65_536
let max_retained_events = 64
let max_retained_event_bytes = 262_144
let max_request_seconds = 60.
let default_request_seconds = 10.
let max_stdio_read_bytes = 65_536
let max_stdio_write_bytes = max_frame_bytes + max_header_bytes

let fail message = raise (Error message)
let with_lock lock action =
  Mutex.lock lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock lock) action

let bounded_text label maximum text =
  if String.length text > maximum || String.contains text '\000' then
    fail (label ^ " exceeds its limit or contains a NUL byte");
  text

let valid_token label maximum token =
  if token = "" || String.length token > maximum ||
     not (String.for_all (fun c ->
       (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
       (c >= '0' && c <= '9') || c = '_' || c = '-' || c = '.') token) then
    fail ("invalid " ^ label);
  token

let normalize_host host =
  bounded_text "remote host" 253 host |> ignore;
  if host = "" || String.contains host '/' || String.contains host '@' ||
     String.contains host '#' || String.contains host '?' ||
     String.contains host ' ' || String.contains host '\t' ||
     String.contains host '\r' || String.contains host '\n' then
    fail "invalid remote host";
  if String.contains host ':' then (
    try ignore (Unix.inet_addr_of_string host); String.lowercase_ascii host
    with _ -> fail "invalid remote host")
  else
    let labels = String.split_on_char '.' host in
    let valid_label label =
      String.length label > 0 && String.length label <= 63 &&
      label.[0] <> '-' && label.[String.length label - 1] <> '-' &&
      String.for_all (fun c ->
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') || c = '-') label in
    if List.exists (fun label -> not (valid_label label)) labels then
      fail "invalid remote host";
    String.lowercase_ascii host

let create_manager ~owner ~workspace_root ~authorize =
  let owner_id = valid_token "session owner" 128 owner in
  let root = try Workspace_path.root_path workspace_root
    with Workspace_path.Error message -> raise (Error message) in
  { owner_id; root; authorize; sessions = Hashtbl.create 8;
    lock = Mutex.create (); manager_closed = false }

let owner manager = manager.owner_id
let workspace_root manager = manager.root

let validate_session_id id = valid_token "DAP session id" 128 id

let register_session manager ~id ~configured_remote_host ~transport ~transport_factory =
  let id = validate_session_id id in
  with_lock manager.lock (fun () ->
    if manager.manager_closed then fail "DAP manager is closed";
    if Hashtbl.mem manager.sessions id then fail "DAP session id is already in use";
    if Hashtbl.length manager.sessions >= max_sessions then
      fail "DAP manager session limit reached";
    let session = {
      id; manager; transport; transport_factory; operation_lock = Mutex.create ();
      state_lock = Mutex.create (); closed = false; phase = Fresh;
      next_request_seq = 1; last_adapter_seq = 0; receive_buffer = "";
      retained_events = []; retained_event_bytes = 0; stopped = false;
      terminated = false; state_event_generation = 0;
      trusted_attach_host = None; configured_remote_host;
    } in
    Hashtbl.add manager.sessions id session;
    session)

let create_session manager ~id ~transport =
  register_session manager ~id ~configured_remote_host:None
    ~transport:(Some transport) ~transport_factory:None

let create_remote_session manager ~id ~host ~transport_factory =
  let configured_remote_host = normalize_host host in
  register_session manager ~id ~configured_remote_host:(Some configured_remote_host)
    ~transport:None ~transport_factory:(Some transport_factory)
let find_session manager id =
  with_lock manager.lock (fun () ->
    if manager.manager_closed then fail "DAP manager is closed";
    match Hashtbl.find_opt manager.sessions id with
    | Some session -> session
    | None -> fail "unknown DAP session")

let is_closed session = with_lock session.state_lock (fun () -> session.closed)

let close_transport session =
  let transport = with_lock session.state_lock (fun () ->
    if session.closed then None
    else (session.closed <- true; session.transport)) in
  Option.iter (fun transport -> try transport.close () with _ -> ()) transport
let ensure_transport session ~timeout_seconds ~cancelled =
  let cached, factory = with_lock session.state_lock (fun () ->
    if session.closed then fail "DAP session is closed";
    session.transport, session.transport_factory) in
  match cached with
  | Some transport -> transport
  | None ->
      let factory = Option.value factory ~default:(fun ~timeout_seconds:_ ~cancelled:_ ->
        fail "DAP session has no adapter transport") in
      let created = factory ~timeout_seconds ~cancelled in
      let status = with_lock session.state_lock (fun () ->
        if session.closed then `Closed
        else match session.transport with
          | Some transport -> `Existing transport
          | None -> session.transport <- Some created; `Created) in
      (match status with
       | `Created -> created
       | `Existing transport ->
           (try created.close () with _ -> ());
           transport
       | `Closed ->
           (try created.close () with _ -> ());
           fail "DAP session is closed")

let remove_session session =
  let manager = session.manager in
  with_lock manager.lock (fun () ->
    match Hashtbl.find_opt manager.sessions session.id with
    | Some current when current == session -> Hashtbl.remove manager.sessions session.id
    | _ -> ());
  close_transport session

let close_session manager ~id =
  let session =
    with_lock manager.lock (fun () ->
      match Hashtbl.find_opt manager.sessions id with
      | None -> None
      | Some session -> Hashtbl.remove manager.sessions id; Some session) in
  Option.iter close_transport session

let close_manager manager =
  let sessions = with_lock manager.lock (fun () ->
    if manager.manager_closed then []
    else (
      manager.manager_closed <- true;
      let sessions = Hashtbl.fold (fun _ session values -> session :: values)
          manager.sessions [] in
      Hashtbl.clear manager.sessions;
      sessions)) in
  List.iter close_transport sessions

let ensure_open session =
  let manager_closed = with_lock session.manager.lock (fun () -> session.manager.manager_closed) in
  if manager_closed || is_closed session then fail "DAP session is closed"

let check_transport_trust session =
  match session.configured_remote_host with
  | None -> ()
  | Some host when session.trusted_attach_host = Some host -> ()
  | Some _ -> fail "remote DAP host has not been explicitly trusted"

let valid_timeout timeout =
  if not (Float.is_finite timeout) || timeout <= 0. || timeout > max_request_seconds then
    fail "DAP request timeout must be greater than zero and at most 60 seconds";
  timeout

let stdio_is_closed child =
  with_lock child.process_lock (fun () -> child.process_closed)

let close_stdio_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

let signal_stdio_child child signal =
  (try Unix.kill (-child.pid) signal with Unix.Unix_error _ -> ());
  (try Unix.kill child.pid signal with Unix.Unix_error _ -> ())

let close_stdio_child child =
  let close_now = with_lock child.process_lock (fun () ->
    if child.process_closed then false
    else (child.process_closed <- true; true)) in
  if close_now then (
    close_stdio_fd child.stdin_fd;
    close_stdio_fd child.stdout_fd;
    signal_stdio_child child Sys.sigterm;
    (try ignore (Unix.select [] [] [] 0.12) with _ -> ());
    signal_stdio_child child Sys.sigkill;
    let rec reap () =
      try ignore (Unix.waitpid [] child.pid)
      with Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
         | Unix.Unix_error (Unix.ECHILD, _, _) -> () in
    reap ())

let check_stdio_cancelled cancelled =
  if (try cancelled () with _ -> true) then raise Cancelled

let stdio_send child ~timeout_seconds ~cancelled text =
  let timeout_seconds = valid_timeout timeout_seconds in
  if String.length text > max_stdio_write_bytes then
    fail "DAP stdio request exceeds its write limit";
  with_lock child.write_lock (fun () ->
    let deadline = Unix.gettimeofday () +. timeout_seconds in
    let rec write offset =
      check_stdio_cancelled cancelled;
      if stdio_is_closed child then fail "DAP stdio adapter is closed";
      if offset = String.length text then ()
      else
        let count = min 16_384 (String.length text - offset) in
        try
          let written = Unix.write_substring child.stdin_fd text offset count in
          if written <= 0 then fail "DAP stdio adapter closed its input";
          write (offset + written)
        with
        | Unix.Unix_error (Unix.EINTR, _, _) -> write offset
        | Unix.Unix_error (Unix.EAGAIN, _, _) ->
            let remaining = deadline -. Unix.gettimeofday () in
            if remaining <= 0. then fail "DAP stdio write timed out";
            (try ignore (Unix.select [] [child.stdin_fd] [] (min 0.05 remaining))
             with
             | Unix.Unix_error (Unix.EINTR, _, _) -> ()
             | Unix.Unix_error _ when stdio_is_closed child ->
                 fail "DAP stdio adapter is closed"
             | Unix.Unix_error _ -> fail "DAP stdio adapter write failed");
            write offset
        | Unix.Unix_error _ when stdio_is_closed child ->
            fail "DAP stdio adapter is closed"
        | Unix.Unix_error _ -> fail "DAP stdio adapter write failed"
    in
    write 0)

let stdio_receive child ~timeout_seconds ~cancelled =
  let timeout_seconds = valid_timeout timeout_seconds in
  let deadline = Unix.gettimeofday () +. timeout_seconds in
  with_lock child.read_lock (fun () ->
  let buffer = child.read_buffer in
  let rec read_chunk () =
    if (try cancelled () with _ -> true) || stdio_is_closed child then None
    else
      let remaining = deadline -. Unix.gettimeofday () in
      if remaining <= 0. then None
      else
        try
          let readable, _, _ =
            Unix.select [child.stdout_fd] [] [] (min 0.05 remaining) in
          if readable = [] then read_chunk ()
          else
            let count = Unix.read child.stdout_fd buffer 0 (Bytes.length buffer) in
            if count = 0 then Some ""
            else Some (Bytes.sub_string buffer 0 count)
        with
        | Unix.Unix_error (Unix.EINTR, _, _) -> read_chunk ()
        | Unix.Unix_error (Unix.EAGAIN, _, _) -> read_chunk ()
        | Unix.Unix_error _ when stdio_is_closed child -> None
        | Unix.Unix_error _ -> fail "DAP stdio adapter read failed"
  in
  read_chunk ())

let stdio_transport ?(timeout_seconds = 5.) ?(cancel = fun () -> false)
    manager ~program ~arguments ~cwd =
  let timeout_seconds = valid_timeout timeout_seconds in
  let program = bounded_text "DAP adapter program" 4096 program in
  if Filename.is_relative program then fail "DAP adapter program must be an absolute path";
  if List.length arguments > 255 then fail "DAP adapter accepts at most 255 arguments";
  let total_argument_bytes = List.fold_left (fun total argument ->
    let argument = bounded_text "DAP adapter argument" 65_536 argument in
    total + String.length argument) (String.length program) arguments in
  if total_argument_bytes > 262_144 then fail "DAP adapter arguments exceed their size limit";
  let cwd = try Workspace_path.root_path cwd
    with Workspace_path.Error message -> raise (Error message)
       | Unix.Unix_error _ -> fail "DAP adapter working directory is unavailable" in
  if not (Workspace_path.within manager.root cwd) then
    fail "DAP adapter working directory is outside the workspace";
  (try
     if (Unix.stat program).Unix.st_kind <> Unix.S_REG then
       fail "DAP adapter program is not a regular file";
     Unix.access program [Unix.X_OK]
   with Unix.Unix_error _ -> fail "DAP adapter program is unavailable or not executable");
  check_stdio_cancelled cancel;
  let manager_closed = with_lock manager.lock (fun () -> manager.manager_closed) in
  if manager_closed then fail "DAP manager is closed";
  manager.authorize Adapter_process;
  check_stdio_cancelled cancel;
  let manager_closed = with_lock manager.lock (fun () -> manager.manager_closed) in
  if manager_closed then fail "DAP manager is closed";
  let environment = [|"PATH=/usr/bin:/bin"; "LANG=C"|] in
  let argv = Array.of_list (program :: arguments) in
  let stdin_read, stdin_write = Unix.pipe ~cloexec:true () in
  let stdout_read, stdout_write =
    try Unix.pipe ~cloexec:true ()
    with exn -> close_stdio_fd stdin_read; close_stdio_fd stdin_write; raise exn in
  let status_read, status_write =
    try Unix.pipe ~cloexec:true ()
    with exn ->
      List.iter close_stdio_fd [stdin_read; stdin_write; stdout_read; stdout_write];
      raise exn in
  let null_fd =
    try Unix.openfile "/dev/null" [Unix.O_WRONLY] 0
    with exn ->
      List.iter close_stdio_fd
        [stdin_read; stdin_write; stdout_read; stdout_write; status_read; status_write];
      raise exn in
  let child_pid =
    try Unix.fork ()
    with exn ->
      List.iter close_stdio_fd
        [stdin_read; stdin_write; stdout_read; stdout_write; status_read; status_write; null_fd];
      raise exn in
  match child_pid with
  | 0 ->
      close_stdio_fd stdin_write;
      close_stdio_fd stdout_read;
      close_stdio_fd status_read;
      (try
         ignore (Unix.setsid ());
         Unix.chdir cwd;
         Unix.dup2 stdin_read Unix.stdin;
         Unix.dup2 stdout_write Unix.stdout;
         Unix.dup2 null_fd Unix.stderr;
         List.iter close_stdio_fd [stdin_read; stdout_write; null_fd];
         Unix.execve program argv environment
       with _ ->
         (try ignore (Unix.write_substring status_write "x" 0 1) with _ -> ());
         Unix._exit 127)
  | pid ->
      close_stdio_fd stdout_write;
      close_stdio_fd status_write;
      close_stdio_fd null_fd;
      let child = {
        pid; stdin_fd = stdin_write; stdout_fd = stdout_read;
        read_lock = Mutex.create (); read_buffer = Bytes.create max_stdio_read_bytes;
        process_lock = Mutex.create (); write_lock = Mutex.create ();
        process_closed = false
      } in
      let close_status () = close_stdio_fd status_read in
      let await_exec () =
        let deadline = Unix.gettimeofday () +. timeout_seconds in
        let rec wait () =
          check_stdio_cancelled cancel;
          let remaining = deadline -. Unix.gettimeofday () in
          if remaining <= 0. then fail "DAP adapter startup timed out";
          let readable, _, _ =
            Unix.select [status_read] [] [] (min 0.05 remaining) in
          if readable = [] then wait ()
          else
            let marker = Bytes.create 1 in
            match Unix.read status_read marker 0 1 with
            | 0 -> ()
            | _ -> fail "DAP adapter failed to execute"
        in
        wait () in
      (try
         Unix.set_nonblock stdin_write;
         Unix.set_nonblock stdout_read;
         await_exec ();
         close_status ();
         { send = (fun ~timeout_seconds ~cancelled text ->
             stdio_send child ~timeout_seconds ~cancelled text);
           receive = (fun ~timeout_seconds ~cancelled ->
             stdio_receive child ~timeout_seconds ~cancelled);
           close = (fun () -> close_stdio_child child) }
       with exn ->
         close_status ();
         close_stdio_child child;
         raise exn)
let member key = function
  | `Assoc fields -> (match List.assoc_opt key fields with Some value -> value | None -> `Null)
  | _ -> `Null

let has_member key = function
  | `Assoc fields -> List.mem_assoc key fields
  | _ -> false

let object_fields label = function
  | `Assoc fields -> fields
  | _ -> fail (label ^ " must be an object")

let string_field label json = match json with
  | `String value -> value
  | _ -> fail (label ^ " must be a string")

let int_field label json = match json with
  | `Int value -> value
  | _ -> fail (label ^ " must be an integer")

let bool_field label json = match json with
  | `Bool value -> value
  | _ -> fail (label ^ " must be a boolean")

let positive_int label value =
  if value <= 0 then fail (label ^ " must be positive");
  value

let bounded_nonnegative label maximum value =
  if value < 0 || value > maximum then fail (label ^ " is out of range");
  value

let rec validate_json depth json =
  if depth > 64 then fail "DAP JSON nesting exceeds 64 levels";
  match json with
  | `Assoc fields ->
      let seen = Hashtbl.create (List.length fields) in
      List.iter (fun (key, value) ->
        if Hashtbl.mem seen key then fail "DAP JSON contains duplicate object fields";
        Hashtbl.add seen key ();
        validate_json (depth + 1) value) fields
  | `List values -> List.iter (validate_json (depth + 1)) values
  | `String value -> if String.contains value '\000' then fail "DAP JSON contains a NUL byte"
  | `Null | `Bool _ | `Int _ | `Float _ -> ()

let find_substring ?(from = 0) text needle =
  let text_length = String.length text and needle_length = String.length needle in
  let rec matches at index =
    index = needle_length ||
    (text.[at + index] = needle.[index] && matches at (index + 1)) in
  let rec search at =
    if at + needle_length > text_length then None
    else if matches at 0 then Some at
    else search (at + 1)
  in
  search from

let split_crlf text =
  let rec loop start values =
    match find_substring ~from:start text "\r\n" with
    | None -> List.rev (String.sub text start (String.length text - start) :: values)
    | Some offset ->
        loop (offset + 2) (String.sub text start (offset - start) :: values)
  in
  if text = "" then [] else loop 0 []

let trim_header_value value =
  let left = ref 0 and right = ref (String.length value - 1) in
  while !left <= !right && (value.[!left] = ' ' || value.[!left] = '\t') do incr left done;
  while !right >= !left && (value.[!right] = ' ' || value.[!right] = '\t') do decr right done;
  if !right < !left then "" else String.sub value !left (!right - !left + 1)

let parse_content_length header =
  let rows = split_crlf header in
  let length = ref None and content_type = ref None in
  List.iter (fun row ->
    if row = "" || String.contains row '\n' || String.contains row '\r' then
      fail "invalid DAP frame header";
    match String.index_opt row ':' with
    | None -> fail "invalid DAP frame header field"
    | Some colon ->
        if colon = 0 then fail "invalid DAP frame header field";
        let name = String.lowercase_ascii (String.sub row 0 colon) in
        let value = trim_header_value (String.sub row (colon + 1) (String.length row - colon - 1)) in
        (match name with
         | "content-length" ->
             if !length <> None || value = "" ||
                not (String.for_all (fun c -> c >= '0' && c <= '9') value) then
               fail "invalid DAP Content-Length";
             let parsed = try int_of_string value with _ -> fail "invalid DAP Content-Length" in
             if parsed <= 0 || parsed > max_frame_bytes then
               fail "DAP frame exceeds the 1048576-byte limit";
             length := Some parsed
         | "content-type" ->
             if !content_type <> None then fail "duplicate DAP Content-Type";
             let value = String.lowercase_ascii value in
             if value <> "application/vscode-jsonrpc" &&
                value <> "application/vscode-jsonrpc; charset=utf-8" then
               fail "unsupported DAP Content-Type";
             content_type := Some value
         | _ -> fail "unsupported DAP frame header")) rows;
  match !length with Some length -> length | None -> fail "DAP frame has no Content-Length"

let take_frame session =
  let buffer = session.receive_buffer in
  match find_substring buffer "\r\n\r\n" with
  | None ->
      if String.length buffer > max_header_bytes then fail "DAP frame header exceeds its limit";
      None
  | Some separator ->
      if separator > max_header_bytes then fail "DAP frame header exceeds its limit";
      let header = String.sub buffer 0 separator in
      let content_length = parse_content_length header in
      let body_start = separator + 4 in
      if String.length buffer - body_start < content_length then (
        if String.length buffer > max_frame_bytes + max_header_bytes then
          fail "DAP receive buffer exceeds its limit";
        None)
      else (
        let body = String.sub buffer body_start content_length in
        session.receive_buffer <- String.sub buffer (body_start + content_length)
            (String.length buffer - body_start - content_length);
        Some body)

let parse_message body =
  let json = try Yojson.Basic.from_string body
    with _ -> fail "invalid JSON in DAP frame" in
  validate_json 0 json;
  let fields = object_fields "DAP message" json in
  let seq = positive_int "DAP sequence number" (int_field "DAP sequence number" (member "seq" json)) in
  let kind = string_field "DAP message type" (member "type" json) in
  seq, kind, fields, json
let validate_message_fields allowed fields =
  List.iter (fun (key, _) ->
    if not (List.mem key allowed) then fail "DAP message contains an unexpected field")
    fields

let bounded_message_string label maximum value =
  if value = "" || String.length value > maximum ||
     not (String.for_all (fun c -> Char.code c >= 32 && Char.code c < 127) value) then
    fail ("invalid " ^ label);
  value

let process_event session seq fields json body_size =
  validate_message_fields ["seq"; "type"; "event"; "body"] fields;
  let event = match List.assoc_opt "event" fields with
    | Some (`String value) -> valid_token "DAP event name" 128 value
    | _ -> fail "DAP event has no valid name" in
  let body = match List.assoc_opt "body" fields with
    | None | Some `Null -> `Null
    | Some (`Assoc _ as value) -> value
    | Some _ -> fail "DAP event body must be an object" in
  if body_size > max_event_bytes then fail "DAP event exceeds its retained-message limit";
  if List.length session.retained_events >= max_retained_events ||
     session.retained_event_bytes + body_size > max_retained_event_bytes then
    fail "DAP retained-event limit reached";
  session.last_adapter_seq <- seq;
  session.retained_events <- json :: session.retained_events;
  session.retained_event_bytes <- session.retained_event_bytes + body_size;
  (match event with
   | ("stopped" | "continued") when session.terminated ->
       fail "DAP adapter reported execution after debuggee termination"
   | "stopped" ->
       session.stopped <- true;
       session.state_event_generation <- session.state_event_generation + 1
   | "continued" ->
       session.stopped <- false;
       session.state_event_generation <- session.state_event_generation + 1
   | "terminated" | "exited" ->
       session.stopped <- false;
       session.terminated <- true;
       session.state_event_generation <- session.state_event_generation + 1
   | _ -> ());
  ignore body

type response = { success : bool; message : string option; body : Yojson.Basic.t }

let decode_response session expected_seq expected_command body =
  let seq, kind, fields, json = parse_message body in
  if seq <= session.last_adapter_seq then fail "DAP adapter sequence is duplicate or out of order";
  match kind with
  | "event" -> process_event session seq fields json (String.length body); `Event
  | "response" ->
      validate_message_fields ["seq"; "type"; "request_seq"; "command"; "success";
                               "message"; "body"] fields;
      let request_seq = positive_int "DAP response request_seq"
          (int_field "DAP response request_seq" (member "request_seq" json)) in
      let command = valid_token "DAP response command" 128
          (string_field "DAP response command" (member "command" json)) in
      if request_seq <> expected_seq then fail "unsolicited or late DAP response";
      if command <> expected_command then fail "DAP response command does not match its request";
      let success = bool_field "DAP response success" (member "success" json) in
      let message = match List.assoc_opt "message" fields with
        | None | Some `Null -> None
        | Some (`String value) -> Some (bounded_text "DAP response message" 4096 value)
        | Some _ -> fail "invalid DAP response message" in
      let response_body = match List.assoc_opt "body" fields with
        | None | Some `Null -> `Null
        | Some (`Assoc _ as value) -> value
        | Some _ -> fail "DAP response body must be an object" in
      session.last_adapter_seq <- seq;
      `Response { success; message; body = response_body }
  | "request" -> fail "unexpected adapter-initiated DAP request"
  | _ -> fail "invalid DAP message type"

let cancel_requested session cancel =
  is_closed session || (try cancel () with _ -> true)

let poll_cancel session cancel =
  if is_closed session then fail "DAP session is closed";
  if (try cancel () with _ -> true) then (
    remove_session session;
    raise Cancelled)

let receive_chunk transport session timeout_seconds cancelled =
  let chunk = transport.receive ~timeout_seconds ~cancelled in
  match chunk with
  | None -> None
  | Some chunk when chunk = "" -> fail "DAP transport returned an empty chunk"
  | Some chunk when String.length chunk > max_buffer_bytes ->
      fail "DAP transport chunk exceeds its limit"
  | Some chunk ->
      if String.length session.receive_buffer + String.length chunk > max_buffer_bytes then
        fail "DAP receive buffer exceeds its limit";
      session.receive_buffer <- session.receive_buffer ^ chunk;
      Some ()

let drain_buffered_events session =
  let rec loop () =
    match take_frame session with
    | None -> ()
    | Some body ->
        (match decode_response session (-1) "" body with
         | `Event -> loop ()
         | `Response _ -> fail "unsolicited or late DAP response")
  in
  loop ()

let send_and_wait ?(timeout_seconds = default_request_seconds) ?(cancel = fun () -> false)
    session ~command ~arguments =
  let timeout_seconds = valid_timeout timeout_seconds in
  check_transport_trust session;
  if session.next_request_seq = max_int then fail "DAP request sequence limit reached";
  poll_cancel session cancel;
  let sequence = session.next_request_seq in
  let payload = `Assoc [
    "seq", `Int sequence; "type", `String "request";
    "command", `String command; "arguments", arguments
  ] |> Yojson.Basic.to_string in
  if String.length payload > max_frame_bytes then fail "DAP request exceeds the frame limit";
  let frame = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length payload) payload in
  let deadline = Unix.gettimeofday () +. timeout_seconds in
  let cancelled () = cancel_requested session cancel in
  let remaining = deadline -. Unix.gettimeofday () in
  if remaining <= 0. then fail "DAP request timed out";
  let transport = ensure_transport session ~timeout_seconds:remaining ~cancelled in
  session.next_request_seq <- sequence + 1;
  transport.send ~timeout_seconds:remaining ~cancelled frame;
  let rec wait () =
    poll_cancel session cancel;
    match take_frame session with
    | Some body ->
        (match decode_response session sequence command body with
         | `Event -> wait ()
         | `Response response ->
             drain_buffered_events session;
             response)
    | None ->
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining <= 0. then fail "DAP request timed out";
        (match receive_chunk transport session remaining cancelled with
         | None ->
             poll_cancel session cancel;
             fail "DAP request timed out"
         | Some () -> wait ())
  in
  wait ()

let check_phase expected session =
  if session.phase <> expected then
    fail "DAP operation is not valid in the current session phase";
  if session.terminated then fail "DAP debuggee has terminated"

let invoke ?(timeout_seconds = default_request_seconds) ?(cancel = fun () -> false)
    ?authorize ?(arguments = `Assoc []) ?(precondition = fun _ -> ())
    ?(validate = fun body -> body) ?(on_success = fun _ _ -> ())
    manager ~id ~command () =
  let timeout_seconds = valid_timeout timeout_seconds in
  let session = find_session manager id in
  Mutex.lock session.operation_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock session.operation_lock) (fun () ->
    ensure_open session;
    precondition session;
    poll_cancel session cancel;
    Option.iter (fun authorize -> authorize ()) authorize;
    let response =
      try send_and_wait ~timeout_seconds ~cancel session ~command ~arguments
      with
      | Cancelled as exn -> remove_session session; raise exn
      | exn -> remove_session session; raise exn in
    if not response.success then
      fail (Option.value response.message ~default:("DAP " ^ command ^ " request failed"));
    let body = try validate response.body with exn -> remove_session session; raise exn in
    on_success session body;
    body)

let no_arguments = `Assoc []

let initialize ?cancel ?timeout_seconds manager ~id ~adapter_id =
  let adapter_id = bounded_message_string "DAP adapter id" 128 adapter_id in
  invoke ?cancel ?timeout_seconds ~arguments:(`Assoc [
    "clientID", `String "pave"; "clientName", `String "Pave";
    "adapterID", `String adapter_id; "locale", `String "en-US";
    "linesStartAt1", `Bool true; "columnsStartAt1", `Bool true;
    "pathFormat", `String "path"
  ]) ~precondition:(fun session ->
      check_transport_trust session;
      check_phase Fresh session)
    ~validate:(fun body -> ignore (object_fields "DAP initialize response" body); body)
    ~on_success:(fun session _ -> session.phase <- Initialized)
    manager ~id ~command:"initialize" ()

let launch ?cancel ?timeout_seconds manager ~id ~target ~arguments =
  let target = bounded_text "launch target" max_path_bytes target in
  let target =
    try Workspace_path.regular_path manager.root target
    with Workspace_path.Error message -> raise (Error message)
       | Unix.Unix_error _ -> fail "launch target is not an existing workspace file" in
  if List.length arguments > 256 then fail "DAP launch accepts at most 256 arguments";
  let arguments = List.map (bounded_text "launch argument" 4096) arguments in
  if List.fold_left (fun size arg -> size + String.length arg) 0 arguments > 16_384 then
    fail "DAP launch arguments exceed 16384 bytes";
  let dap_arguments = `Assoc [
    "program", `String target;
    "args", `List (List.map (fun arg -> `String arg) arguments);
    "cwd", `String manager.root
  ] in
  invoke ?cancel ?timeout_seconds ~authorize:(fun () -> manager.authorize Launch)
    ~arguments:dap_arguments
    ~precondition:(fun session -> check_phase Initialized session)
    ~on_success:(fun session _ -> session.phase <- Awaiting_configuration)
    manager ~id ~command:"launch" ()

let trust_attach_host manager ~id ~host =
  let host = normalize_host host in
  let session = find_session manager id in
  with_lock session.operation_lock (fun () ->
    ensure_open session;
    (match session.configured_remote_host with
     | Some configured when configured <> host -> fail "remote host does not match this DAP session"
     | _ -> ());
    if session.phase <> Fresh && session.phase <> Initialized then
      fail "remote host trust must be established before attach";
    manager.authorize (Remote_host host);
    session.trusted_attach_host <- Some host)

let attach ?cancel ?timeout_seconds manager ~id ~host ~port =
  let host = normalize_host host in
  if port < 1 || port > 65_535 then fail "DAP attach port must be between 1 and 65535";
  invoke ?cancel ?timeout_seconds ~arguments:(`Assoc [
    "host", `String host; "port", `Int port
  ]) ~precondition:(fun session ->
      check_phase Initialized session;
      if session.trusted_attach_host <> Some host then
        fail "remote host must be explicitly trusted before attach")
    ~on_success:(fun session _ -> session.phase <- Awaiting_configuration)
    manager ~id ~command:"attach" ()

let configuration_done ?cancel ?timeout_seconds manager ~id =
  invoke ?cancel ?timeout_seconds ~authorize:(fun () -> manager.authorize Debug_execution)
    ~arguments:no_arguments
    ~precondition:(fun session -> check_phase Awaiting_configuration session)
    ~on_success:(fun session _ -> session.phase <- Configured)
    manager ~id ~command:"configurationDone" ()

let checked_workspace_file manager path =
  let path = bounded_text "DAP source path" max_path_bytes path in
  try Workspace_path.regular_path manager.root path
  with Workspace_path.Error message -> raise (Error message)
     | Unix.Unix_error _ -> fail "DAP source file is not a regular workspace file"

let validate_response_path root path =
  let path = bounded_text "DAP source path" max_path_bytes path in
  try
    let canonical =
      if Filename.is_relative path then Workspace_path.checked_path root path
      else Unix.realpath path in
    if not (Workspace_path.within root canonical) then
      fail "DAP adapter returned a source path outside the workspace";
    canonical
  with
  | Error _ as exn -> raise exn
  | Workspace_path.Error _ | Unix.Unix_error _ ->
      fail "DAP adapter returned an invalid or inaccessible source path"

let validate_source root source =
  match source with
  | `Assoc fields ->
      (match List.assoc_opt "path" fields with
       | None | Some `Null -> ()
       | Some (`String path) -> ignore (validate_response_path root path)
       | Some _ -> fail "DAP source path must be a string");
      source
  | `Null -> source
  | _ -> fail "DAP source must be an object"

let validate_breakpoint_list root expected_count body =
  ignore (object_fields "DAP setBreakpoints response" body);
  match member "breakpoints" body with
  | `List breakpoints when List.length breakpoints <= expected_count ->
      List.iter (fun breakpoint ->
        ignore (object_fields "DAP breakpoint" breakpoint);
        ignore (bool_field "DAP breakpoint verified" (member "verified" breakpoint));
        (match member "line" breakpoint with
         | `Null -> ()
         | `Int line -> ignore (positive_int "DAP breakpoint line" line)
         | _ -> fail "invalid DAP breakpoint line");
        if has_member "source" breakpoint then
          ignore (validate_source root (member "source" breakpoint))) breakpoints;
      body
  | `List _ -> fail "DAP adapter returned too many breakpoints"
  | _ -> fail "DAP setBreakpoints response has no breakpoint list"

let set_breakpoints ?cancel ?timeout_seconds manager ~id ~source ~breakpoints =
  let source_path = checked_workspace_file manager source in
  if List.length breakpoints > 128 then fail "at most 128 breakpoints may be set at once";
  List.iter (fun breakpoint ->
    ignore (positive_int "breakpoint line" breakpoint.line);
    List.iter (fun (label, value) ->
      Option.iter (fun value -> ignore (bounded_text label 1024 value)) value)
      ["breakpoint condition", breakpoint.condition;
       "breakpoint hit condition", breakpoint.hit_condition;
       "breakpoint log message", breakpoint.log_message]) breakpoints;
  let breakpoints_json = List.map (fun breakpoint ->
    `Assoc ([("line", `Int breakpoint.line)] @
      (match breakpoint.condition with None -> [] | Some value -> ["condition", `String value]) @
      (match breakpoint.hit_condition with None -> [] | Some value -> ["hitCondition", `String value]) @
      (match breakpoint.log_message with None -> [] | Some value -> ["logMessage", `String value])))
      breakpoints in
  let arguments = `Assoc [
    "source", `Assoc ["name", `String (Filename.basename source_path);
                       "path", `String source_path];
    "sourceModified", `Bool false;
    "breakpoints", `List breakpoints_json
  ] in
  invoke ?cancel ?timeout_seconds ~authorize:(fun () -> manager.authorize Breakpoints)
    ~arguments
    ~precondition:(fun session ->
      if session.phase <> Awaiting_configuration && session.phase <> Configured then
        fail "breakpoints require a launched or attached debuggee";
      if session.terminated then fail "DAP debuggee has terminated")
    ~validate:(validate_breakpoint_list manager.root (List.length breakpoints))
    manager ~id ~command:"setBreakpoints" ()

let validate_threads body =
  ignore (object_fields "DAP threads response" body);
  match member "threads" body with
  | `List threads when List.length threads <= 256 ->
      List.iter (fun thread ->
        ignore (object_fields "DAP thread" thread);
        ignore (positive_int "DAP thread id" (int_field "DAP thread id" (member "id" thread)));
        ignore (bounded_text "DAP thread name" 4096
          (string_field "DAP thread name" (member "name" thread)))) threads;
      body
  | `List _ -> fail "DAP adapter returned too many threads"
  | _ -> fail "DAP threads response has no thread list"

let threads ?cancel ?timeout_seconds manager ~id =
  invoke ?cancel ?timeout_seconds ~arguments:no_arguments
    ~precondition:(fun session ->
      if session.phase <> Awaiting_configuration && session.phase <> Configured then
        fail "threads require a launched or attached debuggee";
      if session.terminated then fail "DAP debuggee has terminated")
    ~validate:validate_threads manager ~id ~command:"threads" ()

let validate_frame_list root body =
  ignore (object_fields "DAP stackTrace response" body);
  match member "stackFrames" body with
  | `List frames when List.length frames <= 100 ->
      List.iter (fun frame ->
        ignore (object_fields "DAP stack frame" frame);
        ignore (positive_int "DAP stack frame id" (int_field "DAP stack frame id" (member "id" frame)));
        ignore (bounded_text "DAP stack frame name" 4096
          (string_field "DAP stack frame name" (member "name" frame)));
        ignore (positive_int "DAP stack frame line" (int_field "DAP stack frame line" (member "line" frame)));
        (match member "column" frame with
         | `Null -> ()
         | `Int column -> ignore (positive_int "DAP stack frame column" column)
         | _ -> fail "invalid DAP stack frame column");
        if has_member "source" frame then ignore (validate_source root (member "source" frame))) frames;
      body
  | `List _ -> fail "DAP adapter returned too many stack frames"
  | _ -> fail "DAP stackTrace response has no stackFrames list"

let stack_trace ?cancel ?timeout_seconds ?(start_frame = 0) ?(levels = 50) manager ~id ~thread_id () =
  ignore (positive_int "DAP thread id" thread_id);
  ignore (bounded_nonnegative "DAP start frame" 1_000_000 start_frame);
  if levels < 1 || levels > 100 then fail "DAP stackTrace levels must be between 1 and 100";
  invoke ?cancel ?timeout_seconds ~arguments:(`Assoc [
    "threadId", `Int thread_id; "startFrame", `Int start_frame; "levels", `Int levels
  ]) ~precondition:(fun session ->
      if session.phase <> Awaiting_configuration && session.phase <> Configured then
        fail "stackTrace requires a launched or attached debuggee";
      if not session.stopped then fail "stackTrace is available only while the debuggee is stopped";
      if session.terminated then fail "DAP debuggee has terminated")
    ~validate:(validate_frame_list manager.root)
    manager ~id ~command:"stackTrace" ()

let validate_scopes body =
  ignore (object_fields "DAP scopes response" body);
  match member "scopes" body with
  | `List scopes when List.length scopes <= 128 ->
      List.iter (fun scope ->
        ignore (object_fields "DAP scope" scope);
        ignore (bounded_text "DAP scope name" 4096
          (string_field "DAP scope name" (member "name" scope)));
        ignore (bounded_nonnegative "DAP scope variablesReference" max_int
          (int_field "DAP scope variablesReference" (member "variablesReference" scope)))) scopes;
      body
  | `List _ -> fail "DAP adapter returned too many scopes"
  | _ -> fail "DAP scopes response has no scopes list"

let scopes ?cancel ?timeout_seconds manager ~id ~frame_id =
  ignore (positive_int "DAP frame id" frame_id);
  invoke ?cancel ?timeout_seconds ~arguments:(`Assoc ["frameId", `Int frame_id])
    ~precondition:(fun session ->
      if session.phase <> Awaiting_configuration && session.phase <> Configured then
        fail "scopes require a launched or attached debuggee";
      if not session.stopped then fail "scopes are available only while the debuggee is stopped";
      if session.terminated then fail "DAP debuggee has terminated")
    ~validate:validate_scopes manager ~id ~command:"scopes" ()

let validate_variables maximum body =
  ignore (object_fields "DAP variables response" body);
  match member "variables" body with
  | `List variables when List.length variables <= maximum ->
      List.iter (fun variable ->
        ignore (object_fields "DAP variable" variable);
        ignore (bounded_text "DAP variable name" 4096
          (string_field "DAP variable name" (member "name" variable)));
        ignore (bounded_text "DAP variable value" 65_536
          (string_field "DAP variable value" (member "value" variable)));
        (match member "variablesReference" variable with
         | `Null -> ()
         | `Int reference -> ignore (bounded_nonnegative "DAP variable reference" max_int reference)
         | _ -> fail "invalid DAP variable reference")) variables;
      body
  | `List _ -> fail "DAP adapter returned too many variables"
  | _ -> fail "DAP variables response has no variables list"

let variables ?cancel ?timeout_seconds ?filter ?(start = 0) ?(count = 100)
    manager ~id ~reference () =
  ignore (positive_int "DAP variables reference" reference);
  ignore (bounded_nonnegative "DAP variables start" 1_000_000 start);
  if count < 1 || count > 1000 then fail "DAP variables count must be between 1 and 1000";
  let filter = match filter with
    | None -> []
    | Some ("named" | "indexed" as value) -> ["filter", `String value]
    | Some _ -> fail "DAP variables filter must be named or indexed" in
  invoke ?cancel ?timeout_seconds ~arguments:(`Assoc (
    ["variablesReference", `Int reference; "start", `Int start; "count", `Int count] @ filter))
    ~precondition:(fun session ->
      if session.phase <> Awaiting_configuration && session.phase <> Configured then
        fail "variables require a launched or attached debuggee";
      if not session.stopped then fail "variables are available only while the debuggee is stopped";
      if session.terminated then fail "DAP debuggee has terminated")
    ~validate:(validate_variables count) manager ~id ~command:"variables" ()

let execution_request ?cancel ?timeout_seconds ?(single_thread = false)
    manager ~id ~thread_id ~command =
  ignore (positive_int "DAP thread id" thread_id);
  let event_generation = ref None in
  invoke ?cancel ?timeout_seconds ~authorize:(fun () -> manager.authorize Debug_execution)
    ~arguments:(`Assoc ["threadId", `Int thread_id; "singleThread", `Bool single_thread])
    ~precondition:(fun session ->
      if session.phase <> Configured then fail "execution requires completed DAP configuration";
      if not session.stopped then fail "execution control requires a stopped debuggee";
      if session.terminated then fail "DAP debuggee has terminated";
      event_generation := Some session.state_event_generation)
    ~validate:(fun body -> ignore (object_fields "DAP execution response" body); body)
    ~on_success:(fun session _ ->
      match !event_generation with
      | Some before when before = session.state_event_generation -> session.stopped <- false
      | _ -> ())
    manager ~id ~command ()

let continue_ ?cancel ?timeout_seconds ?single_thread manager ~id ~thread_id () =
  execution_request ?cancel ?timeout_seconds ?single_thread manager ~id ~thread_id ~command:"continue"

let next ?cancel ?timeout_seconds ?single_thread manager ~id ~thread_id () =
  execution_request ?cancel ?timeout_seconds ?single_thread manager ~id ~thread_id ~command:"next"

let step_in ?cancel ?timeout_seconds ?single_thread manager ~id ~thread_id () =
  execution_request ?cancel ?timeout_seconds ?single_thread manager ~id ~thread_id ~command:"stepIn"

let step_out ?cancel ?timeout_seconds ?single_thread manager ~id ~thread_id () =
  execution_request ?cancel ?timeout_seconds ?single_thread manager ~id ~thread_id ~command:"stepOut"

let evaluate ?cancel ?timeout_seconds ?(context = "watch") manager ~id ~expression ~frame_id =
  let expression = bounded_text "DAP expression" 8192 expression in
  if expression = "" then fail "DAP expression must not be empty";
  if context <> "watch" && context <> "hover" then
    fail "DAP evaluate context must be watch or hover";
  Option.iter (fun frame_id -> ignore (positive_int "DAP frame id" frame_id)) frame_id;
  let arguments = `Assoc (["expression", `String expression; "context", `String context] @
    (match frame_id with None -> [] | Some value -> ["frameId", `Int value])) in
  invoke ?cancel ?timeout_seconds ~authorize:(fun () -> manager.authorize Evaluate)
    ~arguments
    ~precondition:(fun session ->
      if session.phase <> Awaiting_configuration && session.phase <> Configured then
        fail "evaluate requires a launched or attached debuggee";
      if not session.stopped then fail "evaluate is available only while the debuggee is stopped";
      if session.terminated then fail "DAP debuggee has terminated")
    ~validate:(fun body ->
      ignore (object_fields "DAP evaluate response" body);
      ignore (bounded_text "DAP evaluate result" 65_536
        (string_field "DAP evaluate result" (member "result" body)));
      (match member "variablesReference" body with
       | `Null -> ()
       | `Int value -> ignore (bounded_nonnegative "DAP evaluate variablesReference" max_int value)
       | _ -> fail "invalid DAP evaluate variablesReference");
      body)
    manager ~id ~command:"evaluate" ()

let disconnect ?cancel ?timeout_seconds ?(terminate_debuggee = false) manager ~id =
  let authorize = if terminate_debuggee then
      Some (fun () -> manager.authorize Debug_execution)
    else None in
  let session = find_session manager id in
  let result =
    try invoke ?cancel ?timeout_seconds ?authorize
      ~arguments:(`Assoc ["terminateDebuggee", `Bool terminate_debuggee])
      ~precondition:(fun session ->
        if session.phase = Fresh then fail "cannot disconnect before DAP initialization")
      ~validate:(fun body ->
        (match body with `Null | `Assoc _ -> body | _ -> fail "invalid DAP disconnect response"))
      manager ~id ~command:"disconnect" ()
    with exn -> remove_session session; raise exn in
  remove_session session;
  result

let take_events manager ~id =
  let session = find_session manager id in
  with_lock session.operation_lock (fun () ->
    ensure_open session;
    let events = List.rev session.retained_events in
    session.retained_events <- [];
    session.retained_event_bytes <- 0;
    events)
