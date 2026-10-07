(* Session-owned isolated headless browser automation over the Chrome DevTools
   Protocol. Each session launches one pinned Chromium-family process with a
   fresh throwaway profile, a loopback-only debugging endpoint and a bounded
   lifetime, then drives a single owned page target over a loopback WebSocket.
   No personal profile, existing tab or remote debugging endpoint is adopted;
   denial, cancellation, expiry or teardown kills the process and deletes the
   profile. *)

exception Error of string
exception Cancelled

let fail message = raise (Error message)
let with_lock lock action =
  Mutex.lock lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock lock) action

let member key = function
  | `Assoc fields -> (match List.assoc_opt key fields with
      | Some value -> value | None -> `Null)
  | _ -> `Null
let field key fields = match List.assoc_opt key fields with
  | Some value -> value | None -> `Null
let string_field key json = match member key json with
  | `String value -> value
  | _ -> fail ("malformed CDP response: " ^ key ^ " must be a string")

let starts_with text prefix =
  String.length text >= String.length prefix &&
  String.sub text 0 (String.length prefix) = prefix

let find_case_insensitive text needle start =
  let last = String.length text - String.length needle in
  let rec seek index =
    if index > last then None
    else if
      let rec matches offset =
        offset = String.length needle ||
        (Char.lowercase_ascii text.[index + offset] =
         Char.lowercase_ascii needle.[offset] && matches (offset + 1)) in
      matches 0
    then Some index
    else seek (index + 1) in
  seek (max 0 start)

(* ------------------------------------------------------------------ *)
(* Bounds                                                             *)

let max_sessions = 4
let max_url_bytes = 4096
let max_expression_bytes = 16_384
let max_tool_arguments_bytes = 16_384
let max_tool_name_bytes = 128
let max_result_bytes = 131_072
let max_screenshot_bytes = 8 * 1024 * 1024
let max_ws_message_bytes = 12 * 1024 * 1024
let max_ready_seconds = 15.
let default_operation_seconds = 30.
let max_operation_seconds = 120.
let max_session_seconds = 900.
let max_linger_seconds = 300.
let viewport_width = 1280
let viewport_height = 800
let max_page_tools = 16
let max_tool_description_bytes = 160
let max_tool_summary_bytes = 4096
let max_page_result_bytes = 65_536
let max_catalog_events = 256
let browser_variable = Web_search.browser_variable

(* ------------------------------------------------------------------ *)
(* Small utilities                                                    *)

let check_cancel cancel = if (try cancel () with _ -> true) then raise Cancelled

let with_cancellable_lock cancel lock action =
  let rec acquire () =
    check_cancel cancel;
    if not (Mutex.try_lock lock) then (Thread.delay 0.01; acquire ()) in
  acquire ();
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

let random_bytes count =
  let bytes = Bytes.create count in
  let channel = open_in_bin "/dev/urandom" in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    really_input channel bytes 0 count);
  Bytes.unsafe_to_string bytes

let base64_encode data =
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let length = String.length data in
  let output = Buffer.create (((length + 2) / 3) * 4) in
  let rec loop index =
    if index < length then begin
      let a = Char.code data.[index] in
      let b = if index + 1 < length then Char.code data.[index + 1] else 0 in
      let c = if index + 2 < length then Char.code data.[index + 2] else 0 in
      Buffer.add_char output alphabet.[a lsr 2];
      Buffer.add_char output alphabet.[((a land 3) lsl 4) lor (b lsr 4)];
      Buffer.add_char output
        (if index + 1 < length then alphabet.[((b land 15) lsl 2) lor (c lsr 6)] else '=');
      Buffer.add_char output
        (if index + 2 < length then alphabet.[c land 63] else '=');
      loop (index + 3)
    end in
  loop 0;
  Buffer.contents output

let websocket_accept key =
  base64_encode Digestif.SHA1.(to_raw_string (digest_string
    (key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))

let json_string_literal text = Yojson.Basic.to_string (`String text)

(* ------------------------------------------------------------------ *)
(* Loopback WebSocket client                                          *)

(* The endpoint is loopback-only by construction: the port comes from the
   owned process's DevToolsActivePort file and the path from the same file's
   /devtools/... second line. *)
type ws_transport = {
  fd : Unix.file_descr;
  write_lock : Mutex.t;
  mutable closed : bool;
}

let close_socket fd = try Unix.close fd with Unix.Unix_error _ -> ()

let read_exact ?(idle = 5.) cancel fd buffer offset remaining =
  (* Five seconds of silence inside a frame is a dead connection; the shorter
     select slice just keeps cancel and drain quiet-deadlines responsive. *)
  let idle_deadline = Unix.gettimeofday () +. idle in
  let rec loop offset remaining =
    if remaining <> 0 then begin
      check_cancel cancel;
      let readable, _, _ =
        try Unix.select [fd] [] [] 0.25
        with Unix.Unix_error (Unix.EINTR, _, _) -> [fd], [], [] in
      if readable = [] then begin
        if Unix.gettimeofday () > idle_deadline then
          fail "browser connection timed out waiting for data";
        loop offset remaining
      end else
        match Unix.read fd buffer offset remaining with
        | 0 -> fail "browser connection closed unexpectedly"
        | count -> loop (offset + count) (remaining - count)
        | exception Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
            loop offset remaining
    end in
  loop offset remaining

let write_all ?(cancel = fun () -> false) fd text =
  Unix.set_nonblock fd;
  let deadline = Unix.gettimeofday () +. 5. in
  let rec loop offset =
    if offset < String.length text then begin
      check_cancel cancel;
      if Unix.gettimeofday () >= deadline then
        fail "browser connection timed out writing data";
      let _, writable, _ = Unix.select [] [fd] [] 0.05 in
      if writable = [] then loop offset
      else
        match Unix.write_substring fd text offset (String.length text - offset) with
        | 0 -> fail "browser connection closed during write"
        | count -> loop (offset + count)
        | exception Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
            loop offset
    end in
  loop 0

let read_http_headers cancel fd =
  let buffer = Buffer.create 512 in
  let byte = Bytes.create 1 in
  let rec loop () =
    if Buffer.length buffer > 16_384 then fail "browser endpoint sent oversized headers";
    read_exact cancel fd byte 0 1;
    Buffer.add_bytes buffer byte;
    let text = Buffer.contents buffer in
    if String.length text >= 4 && String.sub text (String.length text - 4) 4 = "\r\n\r\n"
    then text else loop () in
  loop ()

(* Case-insensitive Sec-WebSocket-Accept extraction from response headers. *)
let header_value headers name =
  match find_case_insensitive headers (name ^ ":") 0 with
  | None -> None
  | Some index ->
      let start = index + String.length name + 1 in
      let stop = match String.index_from_opt headers start '\n' with
        | Some newline -> newline | None -> String.length headers in
      let raw = String.sub headers start (stop - start) in
      Some (String.trim raw)

let connect_ws ?(cancel = fun () -> false) ~port ~path () =
  if port < 1 || port > 65_535 || not (starts_with path "/") then
    fail "invalid browser debugging endpoint";
  let fd =
    try Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
    with Unix.Unix_error _ -> fail "could not create browser socket" in
  (try Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port))
   with Unix.Unix_error _ ->
     close_socket fd;
     fail "browser debugging endpoint is not accepting connections");
  Unix.set_nonblock fd;
  let key = base64_encode (random_bytes 16) in
  let request =
    "GET " ^ path ^ " HTTP/1.1\r\nHost: 127.0.0.1:" ^ string_of_int port ^
    "\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: " ^ key ^
    "\r\nSec-WebSocket-Version: 13\r\n\r\n" in
  (try
     write_all ~cancel fd request;
     let headers = read_http_headers cancel fd in
     let status = match String.index_opt headers ' ' with
       | Some space when String.length headers >= space + 4 ->
           (try int_of_string (String.sub headers (space + 1) 3) with _ -> 0)
       | _ -> 0 in
     if status <> 101 then fail "browser debugging endpoint refused the WebSocket upgrade";
     match header_value headers "Sec-WebSocket-Accept" with
     | Some accept when accept = websocket_accept key -> ()
     | _ -> fail "browser debugging endpoint returned a mismatched WebSocket accept"
   with exn -> close_socket fd; raise exn);
  { fd; write_lock = Mutex.create (); closed = false }

let encode_client_frame ~opcode payload =
  let length = String.length payload in
  let header = Buffer.create 14 in
  Buffer.add_char header (Char.chr (0x80 lor opcode));
  (if length < 126 then Buffer.add_char header (Char.chr (0x80 lor length))
   else if length <= 0xFFFF then begin
     Buffer.add_char header (Char.chr (0x80 lor 126));
     Buffer.add_char header (Char.chr ((length lsr 8) land 0xFF));
     Buffer.add_char header (Char.chr (length land 0xFF))
   end else begin
     Buffer.add_char header (Char.chr (0x80 lor 127));
     for i = 7 downto 0 do
       Buffer.add_char header
         (Char.chr (Int64.to_int (Int64.logand
           (Int64.shift_right_logical (Int64.of_int length) (i * 8)) 0xFFL)))
     done
   end);
  let mask = random_bytes 4 in
  Buffer.add_string header mask;
  let masked = Bytes.of_string payload in
  for index = 0 to length - 1 do
    Bytes.set masked index
      (Char.chr (Char.code (Bytes.get masked index) lxor Char.code mask.[index land 3]))
  done;
  Buffer.add_bytes header masked;
  Buffer.contents header

let read_ws_message ?(cancel = fun () -> false) ~limit transport =
  let payload = Buffer.create 4096 in
  let next_frame () =
    let header = Bytes.create 2 in
    (* Between messages the browser is legitimately silent while a page
       promise or navigation settles; the caller's operation bound applies. *)
    read_exact ~idle:(max_operation_seconds +. 10.) cancel transport.fd header 0 2;
    let first = Char.code (Bytes.get header 0) in
    let second = Char.code (Bytes.get header 1) in
    let fin = first land 0x80 <> 0 in
    let opcode = first land 0x0F in
    let masked = second land 0x80 <> 0 in
    let length = second land 0x7F in
    let length =
      if length < 126 then length
      else if length = 126 then
        let ext = Bytes.create 2 in
        read_exact cancel transport.fd ext 0 2;
        (Char.code (Bytes.get ext 0) lsl 8) lor Char.code (Bytes.get ext 1)
      else
        let ext = Bytes.create 8 in
        read_exact cancel transport.fd ext 0 8;
        let length = Bytes.get_int64_be ext 0 in
        if length < 0L || length > Int64.of_int max_ws_message_bytes then
          fail "browser sent an oversized message";
        Int64.to_int length in
    if length > max_ws_message_bytes then fail "browser sent an oversized message";
    let mask =
      if masked then begin
        let key = Bytes.create 4 in
        read_exact cancel transport.fd key 0 4;
        Some key
      end else None in
    let body = Bytes.create length in
    read_exact cancel transport.fd body 0 length;
    (match mask with
     | Some key ->
         for index = 0 to length - 1 do
           Bytes.set body index
             (Char.chr (Char.code (Bytes.get body index)
                lxor Char.code (Bytes.get key (index land 3))))
         done
     | None -> ());
    fin, opcode, Bytes.unsafe_to_string body in
  let rec collect () =
    let fin, opcode, body = next_frame () in
    match opcode with
    | 0x8 -> `Closed
    | 0x9 ->
        with_cancellable_lock cancel transport.write_lock (fun () ->
          write_all ~cancel transport.fd (encode_client_frame ~opcode:0xA body));
        collect ()
    | 0xA -> collect ()
    | (0x0 | 0x1 | 0x2) ->
        Buffer.add_string payload body;
        if Buffer.length payload > limit then fail "browser response exceeds its size limit";
        if fin then `Text (Buffer.contents payload) else collect ()
    | _ -> collect () in
  collect ()

let ws_send ?(cancel = fun () -> false) transport payload =
  if transport.closed then fail "browser debugging connection is closed";
  with_cancellable_lock cancel transport.write_lock (fun () ->
    write_all ~cancel transport.fd (encode_client_frame ~opcode:0x1 payload))

let ws_close transport =
  if not transport.closed then (
    transport.closed <- true;
    (* Closing the owned transport must not wait for a blocked writer or
       send a close frame into an unresponsive peer. *)
    (try Unix.shutdown transport.fd Unix.SHUTDOWN_ALL with Unix.Unix_error _ -> ());
    close_socket transport.fd)

(* ------------------------------------------------------------------ *)
(* CDP session layer                                                  *)

(* A connection is one ordered request/response channel to the browser-level
   endpoint. [receive] returns the next raw CDP message text; [close] is
   idempotent. Injected by tests to run an in-process fake endpoint. *)
type connection = {
  send : cancel:(unit -> bool) -> string -> unit;
  receive : cancel:(unit -> bool) -> string;
  close : unit -> unit;
  mutable next_id : int;
  io_lock : Mutex.t;
  (* Execution contexts per page session: CDP frame id -> main-world context
     id, plus the reverse index for destruction events. *)
  contexts : (string, int) Hashtbl.t;
  context_frames : (int, string) Hashtbl.t;
}

let ws_connection transport =
  { send = (fun ~cancel payload -> ws_send ~cancel transport payload);
    receive = (fun ~cancel ->
      match read_ws_message ~cancel ~limit:max_ws_message_bytes transport with
      | `Closed -> fail "browser closed the debugging connection"
      | `Text message -> message);
    close = (fun () -> ws_close transport);
    next_id = 1;
    io_lock = Mutex.create ();
    contexts = Hashtbl.create 8;
    context_frames = Hashtbl.create 8 }

let cdp_close connection = connection.close ()

(* Runtime execution-context events carry the sessionId of the owning page
   session; the key keeps separate page sessions isolated on one browser
   connection. Only default main-world contexts are recorded. *)
let context_key session_id frame_id = session_id ^ "\000" ^ frame_id

let note_context_event connection json =
  match member "method" json with
  | `String "Runtime.executionContextCreated" ->
      (match member "sessionId" json with
       | `String session_id ->
           let context = member "context" (member "params" json) in
           let aux = member "auxData" context in
           (* CDP reports the owning frame inside auxData, not on the
              context object itself. *)
           let is_default = match member "isDefault" aux with
             | `Bool value -> value | _ -> false in
           if is_default then
             (match member "frameId" aux, member "id" context with
              | `String frame_id, `Int context_id ->
                  let key = context_key session_id frame_id in
                  Hashtbl.replace connection.contexts key context_id;
                  Hashtbl.replace connection.context_frames context_id key
              | _ -> ())
       | _ -> ())
  | `String "Runtime.executionContextDestroyed" ->
      (match member "executionContextId" (member "params" json) with
       | `Int context_id ->
           (match Hashtbl.find_opt connection.context_frames context_id with
            | Some packed ->
                Hashtbl.remove connection.context_frames context_id;
                Hashtbl.remove connection.contexts packed
            | None -> ())
       | _ -> ())
  | `String "Runtime.executionContextsCleared" ->
      (match member "sessionId" json with
       | `String session_id ->
           Hashtbl.filter_map_inplace (fun key value ->
             if starts_with key (session_id ^ "\000") then None else Some value)
             connection.contexts;
           let stale = Hashtbl.fold (fun context_id packed acc ->
             if starts_with packed (session_id ^ "\000") then context_id :: acc
             else acc)
             connection.context_frames [] in
           List.iter (Hashtbl.remove connection.context_frames) stale
       | _ -> Hashtbl.reset connection.contexts;
              Hashtbl.reset connection.context_frames)
  | _ -> ()

(* Collects already-buffered events after Runtime.enable or a navigate; gives
   up once the wire is quiet for a short window. Best-effort only. *)
let drain_connection ?(quiet = 0.12) connection =
  let deadline = Unix.gettimeofday () +. quiet in
  let still_waiting () = Unix.gettimeofday () < deadline in
  with_lock connection.io_lock (fun () ->
    let rec loop () =
      match (try Some (connection.receive ~cancel:(fun () -> not (still_waiting ())))
             with Error _ | Cancelled | End_of_file | Unix.Unix_error _ -> None) with
      | None -> ()
      | Some message ->
          (match (try Yojson.Basic.from_string message with _ -> `Null) with
           | `Assoc _ as json when member "id" json = `Null ->
               note_context_event connection json
           | _ -> ());
          if still_waiting () then loop () in
    loop ())

let send_cdp ?(cancel = fun () -> false) ?session_id connection ~method_ ~params () =
  let deadline = Unix.gettimeofday () +. max_operation_seconds in
  let bounded_cancel () = cancel () || Unix.gettimeofday () >= deadline in
  try with_cancellable_lock bounded_cancel connection.io_lock (fun () ->
    let id = connection.next_id in
    connection.next_id <- id + 1;
    let fields =
      [ "id", `Int id;
        "method", `String method_;
        "params", params ] @
      (match session_id with
       | Some session -> ["sessionId", `String session]
       | None -> []) in
    connection.send ~cancel:bounded_cancel (Yojson.Basic.to_string (`Assoc fields));
    let rec wait () =
      check_cancel bounded_cancel;
      if Unix.gettimeofday () > deadline then
        fail "browser did not answer a command in time";
      let message = connection.receive ~cancel:bounded_cancel in
      (match (try Yojson.Basic.from_string message with _ -> `Null) with
       | `Assoc _ as json when member "id" json = `Int id ->
           (match member "error" json with
            | `Assoc error_fields ->
                fail ("browser command failed: " ^
                  (match List.assoc_opt "message" error_fields with
                   | Some (`String text) -> text | _ -> "unknown error"))
            | _ -> member "result" json)
       | `Assoc _ as json -> note_context_event connection json; wait ()
       | _ -> wait ()) in
    wait ())
  with Cancelled when not (try cancel () with _ -> true) ->
    fail "browser did not answer a command in time"

(* ------------------------------------------------------------------ *)
(* Browser process lifecycle                                          *)

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Array.iter (fun entry -> remove_tree (Filename.concat path entry)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error _ -> ()

type browser_child = {
  pid : int;
  profile : string;
}

let signal_child pid signal =
  if pid > 0 then (
    (try Unix.kill (-pid) signal with Unix.Unix_error _ -> ());
    (try Unix.kill pid signal with Unix.Unix_error _ -> ()))

let terminate_child child =
  if child.pid > 0 then (
    signal_child child.pid Sys.sigterm;
    (try ignore (Unix.select [] [] [] 0.15) with _ -> ());
    signal_child child.pid Sys.sigkill;
    let rec reap () =
      try ignore (Unix.waitpid [] child.pid)
      with Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
         | Unix.Unix_error (Unix.ECHILD, _, _) -> () in
    reap ());
  if child.profile <> "" then
    try remove_tree child.profile with _ -> ()

(* Spawns one pinned executable with a minimal environment, /dev/null stdio
   and its own process group; returns after the exec marker, like other
   session-owned children. *)
let spawn_browser ?(cancel = fun () -> false) ~program ~arguments () =
  let environment =
    [| "PATH=/usr/bin:/bin"; "LANG=C";
       "HOME=" ^ Filename.get_temp_dir_name () |] in
  let argv = Array.of_list (program :: arguments) in
  let status_read, status_write = Unix.pipe ~cloexec:true () in
  let null_fd =
    try Unix.openfile "/dev/null" [Unix.O_RDWR] 0
    with exn ->
      (try Unix.close status_read with _ -> ());
      (try Unix.close status_write with _ -> ());
      raise exn in
  let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> () in
  match (try Unix.fork () with exn ->
      close_fd status_read; close_fd status_write; close_fd null_fd; raise exn) with
  | 0 ->
      (try
         close_fd status_read;
         ignore (Unix.setsid ());
         Unix.dup2 null_fd Unix.stdin;
         Unix.dup2 null_fd Unix.stdout;
         Unix.dup2 null_fd Unix.stderr;
         close_fd null_fd;
         Unix.execve program argv environment
       with _ ->
         (try ignore (Unix.write_substring status_write "x" 0 1) with _ -> ());
         Unix._exit 127)
  | pid ->
      close_fd status_write;
      close_fd null_fd;
      (try
         let deadline = Unix.gettimeofday () +. max_ready_seconds in
         let rec wait_exec () =
           check_cancel cancel;
           let remaining = deadline -. Unix.gettimeofday () in
           if remaining <= 0. then fail "browser startup timed out";
           let readable, _, _ = Unix.select [status_read] [] [] (min 0.1 remaining) in
           if readable = [] then wait_exec ()
           else
             let marker = Bytes.create 1 in
             match Unix.read status_read marker 0 1 with
             | 0 -> ()
             | _ -> fail "browser failed to execute" in
         wait_exec ();
         close_fd status_read;
         pid
       with exn ->
         close_fd status_read;
         signal_child pid Sys.sigkill;
         (try ignore (Unix.waitpid [] pid) with _ -> ());
         raise exn)

(* ------------------------------------------------------------------ *)
(* Session state                                                      *)

type manager = {
  owner_id : string;
  sessions : (string, session) Hashtbl.t;
  lock : Mutex.t;
  mutable manager_closed : bool;
}
and session = {
  id : string;
  manager : manager;
  mutable connection : connection option;
  mutable cdp_session_id : string option;
  target_id : string;
  child : browser_child option;
  created_at : float;
  mutable last_used_at : float;
  operation_lock : Mutex.t;
  mutable closed : bool;
  mutable pending : bool;
  mutable lifetime_watch : Thread.t option;
  (* Page-declared tool catalog per frame ("frameId\000name" -> untrusted
     rendered record) plus the observed transition log. *)
  mutable native_available : bool;
  mutable root_frame : string;
  catalog : (string, Yojson.Basic.t) Hashtbl.t;
  mutable catalog_events : Yojson.Basic.t list;
  mutable event_sequence : int;
  mutable dropped_before : int;
}

let create_manager ~owner =
  { owner_id = valid_token "session owner" 128 owner;
    sessions = Hashtbl.create 4; lock = Mutex.create (); manager_closed = false }

let owner manager = manager.owner_id

(* Ready when the owned profile's DevToolsActivePort holds
   "port\n/devtools/browser/<id>". *)
let read_active_port cancel ~profile =
  let deadline = Unix.gettimeofday () +. max_ready_seconds in
  let file = Filename.concat profile "DevToolsActivePort" in
  let rec poll () =
    check_cancel cancel;
    if Unix.gettimeofday () > deadline then
      fail "browser did not open its debugging endpoint in time";
    let parsed =
      try
        let channel = open_in_bin file in
        Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
          let text = really_input_string channel (min 256 (in_channel_length channel)) in
          match String.split_on_char '\n' text with
          | port_text :: path :: _ when port_text <> "" && starts_with path "/devtools/" ->
              (try Some (int_of_string (String.trim port_text), path)
               with _ -> None)
          | _ -> None)
      with _ -> None in
    match parsed with
    | Some endpoint -> endpoint
    | None -> Thread.delay 0.05; poll () in
  poll ()

let detect_browser ?(env = Sys.getenv_opt) () =
  match Web_search.detect_browser ~env () with
  | Some path -> path
  | None ->
      fail ("no Chromium-family browser found; set " ^ browser_variable ^
        " to an absolute path or install Chromium or Chrome")

let browser_arguments ~profile = [
  "--headless"; "--disable-gpu"; "--no-first-run"; "--no-default-browser-check";
  "--disable-extensions"; "--disable-sync"; "--disable-background-networking";
  "--disable-component-update"; "--mute-audio"; "--hide-scrollbars";
  "--lang=en-US"; "--remote-debugging-port=0";
  "--user-data-dir=" ^ profile;
  "--user-agent=" ^ Web_search.browser_user_agent;
  Printf.sprintf "--window-size=%d,%d" viewport_width viewport_height;
  "about:blank" ]

(* Hook installed before every document in the owned page: if the page has no
   modelContext it gets a minimal in-page registration surface, and either way
   a mirror catalog records registrations so the host can list and invoke
   page-declared tools. Page-provided fields stay untrusted input. *)
let web_tools_hook = {js|
(() => {
  try {
    if (window.__paveWebTools) return;
    const owners = [navigator, document];
    const existing = document.modelContext || navigator.modelContext;
    const nativeAvailable = existing !== undefined;
    const tools = new Map();
    const target = new EventTarget();
    const clone = (value) => {
      if (value === undefined) return undefined;
      try { return JSON.parse(JSON.stringify(value)); }
      catch (_) { return undefined; }
    };
    const describe = (tool) => ({
      name: tool.name,
      description: typeof tool.description === "string" ? tool.description : "",
      inputSchema: clone(tool.inputSchema),
      annotations: clone(tool.annotations)
    });
    let polyfill;
    const notify = () => {
      const event = new Event("toolchange");
      try { target.dispatchEvent(event); } catch (_) {}
      if (polyfill && typeof polyfill.ontoolchange === "function") {
        try { polyfill.ontoolchange(event); } catch (_) {}
      }
    };
    const remember = (tool, signal) => {
      if (!tool || typeof tool.name !== "string" ||
          !/^[A-Za-z0-9_.-]{1,128}$/.test(tool.name))
        throw new TypeError("page tool names use 1-128 ASCII letters, digits, '_', '-' or '.'");
      if (typeof (tool.execute || tool.handler) !== "function")
        throw new TypeError("page tool " + JSON.stringify(tool.name) + " requires an execute or handler function");
      tools.set(tool.name, tool);
      if (signal) signal.addEventListener("abort", () => {
        if (tools.get(tool.name) === tool) { tools.delete(tool.name); notify(); }
      }, { once: true });
      notify();
    };
    const bridge = {
      nativeAvailable,
      snapshot() {
        return [...tools.values()].map(describe)
          .sort((a, b) => a.name < b.name ? -1 : a.name > b.name ? 1 : 0);
      },
      async invoke(name, input) {
        const tool = tools.get(String(name));
        if (!tool) throw new Error("no page-declared tool named " + JSON.stringify(name));
        const execute = tool.execute || tool.handler;
        const controller = new AbortController();
        return await execute(input === undefined ? {} : input,
          { signal: controller.signal });
      },
      uninstall() {
        if (existing) {
          try {
            if (typeof bridge.__originalRegister === "function")
              existing.registerTool = bridge.__originalRegister;
            if (typeof bridge.__originalUnregister === "function")
              existing.unregisterTool = bridge.__originalUnregister;
            delete bridge.__originalRegister;
            delete bridge.__originalUnregister;
          } catch (_) {}
        } else if (polyfill) {
          for (const owner of owners) {
            try { if (owner.modelContext === polyfill) delete owner.modelContext; }
            catch (_) {}
          }
        }
        delete window.__paveWebTools;
      }
    };
    const registerTool = async (tool, options) => {
      if (typeof bridge.__originalRegister === "function")
        await bridge.__originalRegister(tool, options);
      remember(tool, options && options.signal);
    };
    const unregisterTool = async (name) => {
      if (typeof bridge.__originalUnregister === "function")
        await bridge.__originalUnregister(name);
      if (tools.delete(String(name))) notify();
    };
    if (existing) {
      try {
        bridge.__originalRegister = typeof existing.registerTool === "function"
          ? existing.registerTool.bind(existing) : undefined;
        bridge.__originalUnregister = typeof existing.unregisterTool === "function"
          ? existing.unregisterTool.bind(existing) : undefined;
        Object.defineProperty(existing, "registerTool",
          { configurable: true, writable: true, value: registerTool });
        if (bridge.__originalUnregister)
          Object.defineProperty(existing, "unregisterTool",
            { configurable: true, writable: true, value: unregisterTool });
      } catch (_) {}
    } else {
      polyfill = {
        registerTool,
        unregisterTool,
        async provideContext(context) {
          const record = context && typeof context === "object" ? context : undefined;
          const provided = Array.isArray(context) ? context : record && record.tools;
          if (!Array.isArray(provided))
            throw new TypeError("provideContext() expects an array or { tools: [...] }");
          for (const tool of provided) await registerTool(tool);
        },
        async provide(context) { return this.provideContext(context); },
        async getTools() { return bridge.snapshot(); },
        async executeTool(tool, params) {
          return await bridge.invoke(tool && tool.name, params === undefined ? {} : params);
        },
        clearContext() { if (tools.size) { tools.clear(); notify(); } },
        addEventListener: target.addEventListener.bind(target),
        removeEventListener: target.removeEventListener.bind(target),
        dispatchEvent: target.dispatchEvent.bind(target),
        ontoolchange: null
      };
      for (const owner of owners) {
        try {
          Object.defineProperty(owner, "modelContext",
            { configurable: true, enumerable: false, value: polyfill });
        } catch (_) {}
      }
    }
    Object.defineProperty(window, "__paveWebTools",
      { configurable: true, enumerable: false, value: bridge });
  } catch (_) {}
})();
|js}

let close_session ?expected manager ~id =
  let id = valid_token "browser session id" 128 id in
  let session = with_lock manager.lock (fun () ->
    match Hashtbl.find_opt manager.sessions id with
    | None -> None
    | Some session ->
        if Option.fold ~none:false ~some:(fun expected -> expected != session) expected then
          None
        else (
          Hashtbl.remove manager.sessions id;
          session.closed <- true;
          Some session)) in
  match session with
  | None -> ()
  | Some session ->
      (* Break an in-flight socket wait before acquiring its operation lock. *)
      Option.iter cdp_close session.connection;
      with_lock session.operation_lock (fun () ->
        session.connection <- None;
        Option.iter terminate_child session.child);
      Option.iter (fun thread ->
        if Thread.id thread <> Thread.id (Thread.self ()) then Thread.join thread)
        session.lifetime_watch

let session_operation ?(cancel = fun () -> false)
    ?(timeout_seconds = default_operation_seconds) session action =
  let deadline = Unix.gettimeofday () +. timeout_seconds in
  let expiry_reason () =
    let now = Unix.gettimeofday () in
    if now -. session.created_at >= max_session_seconds then
      Some "browser session exceeded its lifetime bound"
    else if now -. session.last_used_at >= max_linger_seconds then
      Some "browser session expired from inactivity"
    else if now >= deadline then Some "browser operation exceeded its deadline"
    else None in
  let bounded_cancel () =
    cancel () || session.closed || Option.is_some (expiry_reason ()) in
  try with_cancellable_lock bounded_cancel session.operation_lock (fun () ->
    check_cancel bounded_cancel;
    let result = action bounded_cancel in
    check_cancel bounded_cancel;
    session.last_used_at <- Unix.gettimeofday ();
    result)
  with exn ->
    if session.closed || Option.is_some (expiry_reason ()) ||
       (try cancel () with _ -> true) || exn = Cancelled then (
      close_session ~expected:session session.manager ~id:session.id;
      if (try cancel () with _ -> true) then raise Cancelled;
      match expiry_reason () with
      | Some message -> fail (message ^ "; open a new session")
      | None when session.closed -> fail "browser session is closed"
      | None -> raise exn)
    else raise exn

let start_lifetime_watch session =
  let rec watch () =
    if not session.closed then (
      let now = Unix.gettimeofday () in
      if now -. session.created_at >= max_session_seconds ||
         now -. session.last_used_at >= max_linger_seconds then
        close_session ~expected:session session.manager ~id:session.id
      else (Thread.delay 0.25; watch ())) in
  session.lifetime_watch <- Some (Thread.create watch ())

let require_connection session =
  match session.connection, session.cdp_session_id with
  | Some connection, Some cdp_session -> connection, cdp_session
  | _ -> fail "browser session has no attached page"

let evaluate_raw ?(cancel = fun () -> false) ?context_id session ~expression
    ~timeout_seconds =
  let connection, cdp_session = require_connection session in
  send_cdp ~cancel ~session_id:cdp_session connection
    ~method_:"Runtime.evaluate"
    ~params:(`Assoc ([
      "expression", `String expression;
      "returnByValue", `Bool true;
      "awaitPromise", `Bool true;
      "timeout", `Int (int_of_float (timeout_seconds *. 1000.));
      "userGesture", `Bool false ] @
      (match context_id with
       | Some context_id -> ["contextId", `Int context_id]
       | None -> [])))
    ()

let evaluation_text json =
  match member "exceptionDetails" json with
  | `Assoc detail ->
      let text = match field "exception" detail with
        | `Assoc exn -> (match field "description" exn with
            | `String description -> description | _ -> "page threw an exception")
        | _ -> (match field "text" detail with
            | `String text -> text | _ -> "page threw an exception") in
      fail ("page evaluation failed: " ^
        (if String.length text > 512 then String.sub text 0 512 else text))
  | _ ->
      (match member "result" json with
       | `Assoc fields ->
           let field key = match List.assoc_opt key fields with
             | Some value -> value | None -> `Null in
           (match field "type", field "value" with
            | `String "undefined", _ -> "undefined"
            | _, `String value -> value
            | _, ((`Int _ | `Float _ | `Bool _) as value) -> Yojson.Basic.to_string value
            | `String kind, `Null ->
                (match field "unserializableValue" with
                 | `String value -> value
                 | _ -> "[" ^ kind ^ "]")
            | _, value -> Yojson.Basic.to_string value)
       | other -> Yojson.Basic.to_string other)

(* Truncates on a UTF-8 code-point boundary so previews never split a
   multi-byte character. *)
let utf8_prefix text limit =
  if String.length text <= limit then text
  else
    let rec boundary index =
      if index > 0 && (Char.code text.[index] land 0xc0) = 0x80 then
        boundary (index - 1)
      else index in
    String.sub text 0 (boundary limit)

(* Delimited untrusted-content boundary for page-provided text that must not
   be mistaken for host output. *)
let untrusted_boundary text =
  let nonce = Digestif.SHA1.(to_hex (digest_string (random_bytes 16))) in
  "[BEGIN UNTRUSTED PAGE CONTENT " ^ nonce ^ "]\n" ^
  utf8_prefix text (max_page_result_bytes / 2) ^
  "\n[END UNTRUSTED PAGE CONTENT " ^ nonce ^ "]"

(* ------------------------------------------------------------------ *)
(* Public operations                                                  *)

(* A placeholder reserves the id under the manager lock while the process and
   transport are established; close_manager can still observe and drop it. *)
let pending_session manager ~id =
  { id; manager; connection = None; cdp_session_id = None; target_id = "";
    child = None; created_at = Unix.gettimeofday ();
    last_used_at = Unix.gettimeofday ();
    operation_lock = Mutex.create (); closed = false; pending = true;
    lifetime_watch = None;
    native_available = false; root_frame = ""; catalog = Hashtbl.create 8;
    catalog_events = []; event_sequence = 0; dropped_before = 0 }

(* Every known frame id from Page.getFrameTree, root first. *)
let frame_ids json =
  let rec collect node acc =
    let frame = member "frame" node in
    let acc = match member "id" frame with
      | `String frame_id -> frame_id :: acc | _ -> acc in
    match member "childFrames" node with
    | `List children ->
        List.fold_left (fun acc child -> collect child acc) acc children
    | _ -> acc in
  match member "frameTree" json with
  | `Assoc _ as tree -> List.rev (collect tree [])
  | _ -> []

(* Runs an expression inside one frame's main world. The root frame can use
   the session's default context; subframes need their recorded context id. *)
let evaluate_in_frame ?(cancel = fun () -> false) session ~cdp_session
    ~frame_id ~is_root ~expression ~timeout_seconds =
  match session.connection with
  | None -> `Skipped
  | Some connection ->
      let context_id =
        Hashtbl.find_opt connection.contexts (context_key cdp_session frame_id) in
      (match context_id, is_root with
       | None, false -> `Skipped
       | context_id, _ ->
           `Done (evaluate_raw ~cancel ?context_id session ~expression
             ~timeout_seconds))

(* Installs the bridge into the current main world of every frame; the
   addScriptToEvaluateOnNewDocument preload only covers future documents.
   Best-effort: a missing frame tree or context just skips that frame. *)
let install_bridge ?(cancel = fun () -> false) session =
  match session.connection, session.cdp_session_id with
  | Some connection, Some cdp_session ->
      (match (try
          Some (send_cdp ~cancel ~session_id:cdp_session connection
            ~method_:"Page.getFrameTree" ~params:(`Assoc []) ())
        with Error _ -> None) with
       | None -> ()
       | Some tree ->
           let ids = frame_ids tree in
           (match ids with
            | [] -> ()
            | root :: _ ->
                session.root_frame <- root;
                List.iter (fun frame_id ->
                  ignore (evaluate_in_frame ~cancel session ~cdp_session
                    ~frame_id ~is_root:(frame_id = root)
                    ~expression:web_tools_hook ~timeout_seconds:5.)) ids))
  | _ -> ()

(* [spawn]/[connect] are injectable for in-process tests; production defaults
   launch the pinned executable and open its loopback WebSocket. *)
let open_session ?(env = Sys.getenv_opt) ?(cancel = fun () -> false)
    ?spawn ?connect manager ~id =
  let id = valid_token "browser session id" 128 id in
  let spawn = match spawn with
    | Some spawn -> spawn
    | None ->
        let program = detect_browser ~env () in
        fun ~cancel ->
          let profile = Filename.temp_dir "pave-browser-" "" in
          match spawn_browser ~cancel ~program ~arguments:(browser_arguments ~profile) () with
          | pid -> { pid; profile }
          | exception exn ->
              (try remove_tree profile with _ -> ());
              raise exn in
  let connect = match connect with
    | Some connect -> connect
    | None ->
        fun ~cancel ~profile ->
          let port, ws_path = read_active_port cancel ~profile in
          ws_connection (connect_ws ~cancel ~port ~path:ws_path ()) in
  check_cancel cancel;
  let reservation = pending_session manager ~id in
  let owned_slot = ref reservation in
  with_lock manager.lock (fun () ->
    if manager.manager_closed then fail "browser manager is closed";
    if Hashtbl.mem manager.sessions id then fail "browser session id is already in use";
    if Hashtbl.length manager.sessions >= max_sessions then
      fail "browser session limit reached";
    Hashtbl.replace manager.sessions id reservation);
  let user_cancel = cancel in
  let deadline = Unix.gettimeofday () +. max_ready_seconds in
  let cancel () =
    user_cancel () || reservation.closed || manager.manager_closed ||
    Unix.gettimeofday () >= deadline in
  (try
     let child = spawn ~cancel in
     (try
        check_cancel cancel;
        let connection = connect ~cancel ~profile:child.profile in
        (try
           let target =
             send_cdp ~cancel connection ~method_:"Target.createTarget"
               ~params:(`Assoc [
                 "url", `String "about:blank";
                 "width", `Int viewport_width;
                 "height", `Int viewport_height;
                 "newWindow", `Bool false ]) () in
           let target_id = string_field "targetId" target in
           let attached =
             send_cdp ~cancel connection ~method_:"Target.attachToTarget"
               ~params:(`Assoc [
                 "targetId", `String target_id;
                 "flatten", `Bool true ]) () in
           let cdp_session_id = string_field "sessionId" attached in
           ignore (send_cdp ~cancel ~session_id:cdp_session_id connection
             ~method_:"Page.enable" ~params:(`Assoc []) ());
           ignore (send_cdp ~cancel ~session_id:cdp_session_id connection
             ~method_:"Runtime.enable" ~params:(`Assoc []) ());
           (* Runtime.enable streams executionContextCreated events; buffer
              them before evaluating into specific frame contexts. *)
           drain_connection connection;
           ignore (send_cdp ~cancel ~session_id:cdp_session_id connection
             ~method_:"Page.addScriptToEvaluateOnNewDocument"
             ~params:(`Assoc ["source", `String web_tools_hook]) ());
           let session =
             { (pending_session manager ~id) with
               connection = Some connection;
               cdp_session_id = Some cdp_session_id;
               target_id;
               child = Some child;
               pending = false } in
           (* The preload only covers future documents; install the bridge
              into frames that already exist too. *)
           install_bridge ~cancel session;
           check_cancel cancel;
           with_lock manager.lock (fun () ->
             match Hashtbl.find_opt manager.sessions id with
             | Some placeholder when placeholder == reservation ->
                 Hashtbl.replace manager.sessions id session;
                 owned_slot := session
             | _ ->
                 (* The placeholder was closed meanwhile: drop everything. *)
                 raise (Error "browser session was closed during startup"));
           start_lifetime_watch session;
           id
         with exn ->
           cdp_close connection;
           raise exn)
      with exn ->
        terminate_child child;
        raise exn)
   with exn ->
     with_lock manager.lock (fun () ->
       match Hashtbl.find_opt manager.sessions id with
       | Some current when current == !owned_slot -> Hashtbl.remove manager.sessions id
       | _ -> ());
     if (try user_cancel () with _ -> true) then raise Cancelled;
     if Unix.gettimeofday () >= deadline then fail "browser startup exceeded its deadline";
     if reservation.closed || manager.manager_closed then
       fail "browser session was closed during startup";
     raise exn)

let lookup manager ~id =
  let id = valid_token "browser session id" 128 id in
  with_lock manager.lock (fun () ->
    match Hashtbl.find_opt manager.sessions id with
    | Some session when not session.closed && not session.pending -> session
    | Some _ | None -> fail "browser session is closed or unknown")


let close_manager manager =
  let ids = with_lock manager.lock (fun () ->
    manager.manager_closed <- true;
    Hashtbl.fold (fun id _ ids -> id :: ids) manager.sessions []) in
  List.iter (fun id -> close_session manager ~id) ids

(* Public http/https only: no credentials, no fragments, pinned optional port. *)
let validate_navigation_url url =
  let url = bounded_text "navigation URL" max_url_bytes url in
  let lower = String.lowercase_ascii url in
  if not (starts_with lower "https://" || starts_with lower "http://") then
    fail "browser navigation requires an http:// or https:// URL";
  if String.contains url '@' then fail "URLs with embedded credentials are not allowed";
  if String.contains url '#' then fail "URL fragments are not supported";
  let after_scheme =
    let marker = String.index url ':' + 3 in
    String.sub url marker (String.length url - marker) in
  let host_port = match String.index_opt after_scheme '/' with
    | Some slash -> String.sub after_scheme 0 slash
    | None -> after_scheme in
  if host_port = "" then fail "browser navigation URL has no host";
  (match String.index_opt host_port ':' with
   | Some colon ->
       (let port = String.sub host_port (colon + 1)
         (String.length host_port - colon - 1) in
        match (try Some (int_of_string port) with _ -> None) with
        | Some port when port >= 1 && port <= 65_535 -> ()
        | _ -> fail "browser navigation URL has an invalid port")
   | None -> ());
  url

let valid_timeout label timeout =
  if not (Float.is_finite timeout) || timeout <= 0. || timeout > max_operation_seconds then
    fail (label ^ " must be greater than zero and at most 120 seconds");
  timeout

let navigate ?(cancel = fun () -> false) manager ~id ~url ~timeout_seconds =
  let url = validate_navigation_url url in
  let timeout_seconds = valid_timeout "navigation timeout" timeout_seconds in
  let session = lookup manager ~id in
  session_operation ~cancel ~timeout_seconds session (fun cancel ->
    let connection, cdp_session = require_connection session in
    let answer =
      send_cdp ~cancel ~session_id:cdp_session connection
        ~method_:"Page.navigate"
        ~params:(`Assoc ["url", `String url]) () in
    (match member "errorText" answer with
     | `String text when text <> "" -> fail ("navigation failed: " ^ text)
     | _ -> ());
    let frame_id = match member "frameId" answer with
      | `String frame -> frame | _ -> "" in
    let deadline = Unix.gettimeofday () +. timeout_seconds in
    let rec await_ready () =
      check_cancel cancel;
      if Unix.gettimeofday () > deadline then
        fail "page load did not settle within the navigation timeout";
      (* Mid-navigation the previous context can still answer or the new one
         may not exist yet; treat evaluation errors as "not ready". *)
      (match (try Some (evaluate_raw ~cancel session ~timeout_seconds:5.
                  ~expression:"document.readyState")
              with Error _ -> None) with
       | Some probe ->
           (match member "result" probe with
            | `Assoc fields ->
                (match field "value" fields with
                 | `String "complete" -> ()
                 | _ -> Thread.delay 0.1; await_ready ())
            | _ -> Thread.delay 0.1; await_ready ())
       | None -> Thread.delay 0.1; await_ready ()) in
    await_ready ();
    `Assoc [
      "frame_id", `String frame_id;
      "url", `String url;
      "session", `String id ])

let evaluate ?(cancel = fun () -> false) manager ~id ~expression ~timeout_seconds =
  let expression = bounded_text "page expression" max_expression_bytes expression in
  if String.trim expression = "" then fail "page expression must not be empty";
  let timeout_seconds = valid_timeout "evaluation timeout" timeout_seconds in
  let session = lookup manager ~id in
  session_operation ~cancel ~timeout_seconds session (fun cancel ->
    let answer = evaluate_raw ~cancel session ~expression ~timeout_seconds in
    (* Oversized results are truncated, not failed: the page already computed
       them and refusing mid-result would abort otherwise valid work. *)
    let result = evaluation_text answer in
    let truncated = String.length result > max_result_bytes in
    `Assoc ([
      "session", `String id;
      "result", `String (utf8_prefix result max_result_bytes) ] @
      (if truncated
       then ["truncated", `Bool true;
             "original_bytes", `Int (String.length result)]
       else [])))

let observe ?(cancel = fun () -> false) manager ~id =
  let session = lookup manager ~id in
  session_operation ~cancel session (fun cancel ->
    let answer =
      evaluate_raw ~cancel session ~timeout_seconds:10.
        ~expression:
          "JSON.stringify({url: location.href, title: document.title, \
           ready: document.readyState, origin: location.origin})" in
    match member "result" answer with
    | `Assoc fields ->
        (match field "value" fields with
         | `String text ->
             if String.length text > max_result_bytes then
               fail "page observation exceeds its size limit";
             (match (try Yojson.Basic.from_string text with _ -> `Null) with
              | `Assoc fields -> `Assoc (("session", `String id) :: fields)
              | _ -> fail "page observation returned malformed state")
         | _ -> fail "page observation returned no state")
    | _ -> fail "page observation returned no state")

let screenshot ?(cancel = fun () -> false) manager ~id =
  let session = lookup manager ~id in
  session_operation ~cancel session (fun cancel ->
    let connection, cdp_session = require_connection session in
    let answer =
      send_cdp ~cancel ~session_id:cdp_session connection
        ~method_:"Page.captureScreenshot"
        ~params:(`Assoc [
          "format", `String "png";
          "captureBeyondViewport", `Bool false ]) () in
    let data = string_field "data" answer in
    if String.length data > max_screenshot_bytes then
      fail "screenshot exceeds its size limit";
    `Assoc [
      "session", `String id;
      "mime_type", `String "image/png";
      "data", `String data;
      "bytes", `Int (String.length data) ])

(* ------------------------------------------------------------------ *)
(* Page-declared tool catalog (modelContext bridge)                    *)

let bridge_snapshot_expression = {js|
(window.__paveWebTools ? JSON.stringify({
  nativeAvailable: !!window.__paveWebTools.nativeAvailable,
  origin: location.origin,
  tools: window.__paveWebTools.snapshot()
}) : null)|js}

let member_string key json =
  match member key json with `String value -> value | _ -> ""

let catalog_key frame_id name = frame_id ^ "\000" ^ name

(* One untrusted rendered catalog record per tool. Schemas and annotations
   are retained here but only emitted for exact-name reads. *)
let catalog_entry ~frame_id ~origin tool =
  `Assoc [
    "name", `String (member_string "name" tool);
    "description", `String (member_string "description" tool);
    "inputSchema", member "inputSchema" tool;
    "annotations", member "annotations" tool;
    "frameId", `String frame_id;
    "origin", `String origin ]

let record_event session ~kind entry =
  session.event_sequence <- session.event_sequence + 1;
  let event = `Assoc [
    "sequence", `Int session.event_sequence;
    "type", `String kind;
    "name", member "name" entry;
    "frameId", member "frameId" entry;
    "origin", member "origin" entry;
    "timestamp", `Int (int_of_float (Unix.gettimeofday () *. 1000.));
    "untrusted", `Bool true ] in
  session.catalog_events <- event :: session.catalog_events;
  if List.length session.catalog_events > max_catalog_events then begin
    session.catalog_events <-
      List.filteri (fun index _ -> index < max_catalog_events)
        session.catalog_events;
    (* Events are newest-first; the oldest retained sequence bounds
       what "since" can still see. *)
    match List.nth_opt session.catalog_events
            (List.length session.catalog_events - 1) with
    | Some oldest ->
        session.dropped_before <-
          (match member "sequence" oldest with `Int n -> n | _ -> 0)
    | None -> ()
  end

(* Re-reads every frame's bridge snapshot and records catalog transitions.
   Frames without a reachable context are skipped, matching detached-frame
   behavior. *)
let refresh_catalog ?(cancel = fun () -> false) session =
  let connection, cdp_session = require_connection session in
  drain_connection connection;
  let tree =
    send_cdp ~cancel ~session_id:cdp_session connection
      ~method_:"Page.getFrameTree" ~params:(`Assoc []) () in
  let ids = frame_ids tree in
  (match ids with
   | root :: _ -> session.root_frame <- root
   | [] -> ());
  let next = Hashtbl.create 8 in
  let catalog_bytes = ref 0 in
  session.native_available <- false;
  List.iter (fun frame_id ->
    let is_root = session.root_frame = frame_id in
    ignore (try
        evaluate_in_frame ~cancel session ~cdp_session ~frame_id
          ~is_root ~expression:web_tools_hook ~timeout_seconds:5.
      with Error _ -> `Skipped);
    match (try
        evaluate_in_frame ~cancel session ~cdp_session ~frame_id ~is_root
          ~expression:bridge_snapshot_expression ~timeout_seconds:5.
      with Error _ -> `Skipped) with
    | `Skipped -> ()
    | `Done answer ->
        (match member "result" answer with
         | `Assoc fields ->
             (match field "value" fields with
              | `String text ->
                  (match (try Yojson.Basic.from_string text with _ -> `Null) with
                   | `Assoc _ as snapshot ->
                       (match member "nativeAvailable" snapshot with
                        | `Bool true -> session.native_available <- true
                        | _ -> ());
                       let origin = member_string "origin" snapshot in
                       let origin = if origin = "" then "null" else origin in
                       (match member "tools" snapshot with
                        | `List tools ->
                            List.iter (fun tool ->
                              let entry = catalog_entry ~frame_id ~origin tool in
                              let bytes = String.length (Yojson.Basic.to_string entry) in
                              let key = catalog_key frame_id (member_string "name" entry) in
                              let previous_bytes = match Hashtbl.find_opt next key with
                                | Some previous -> String.length (Yojson.Basic.to_string previous)
                                | None -> 0 in
                              catalog_bytes := !catalog_bytes - previous_bytes + bytes;
                              if !catalog_bytes > max_result_bytes then
                                fail "page tool catalog exceeds its size limit";
                              Hashtbl.replace next key entry) tools
                        | _ -> ())
                   | _ -> ())
              | _ -> ())
         | _ -> ())) ids;
  Hashtbl.iter (fun key entry ->
    match Hashtbl.find_opt session.catalog key with
    | None -> record_event session ~kind:"registered" entry
    | Some previous ->
        if Yojson.Basic.to_string previous <> Yojson.Basic.to_string entry then
          record_event session ~kind:"updated" entry) next;
  Hashtbl.iter (fun key entry ->
    if not (Hashtbl.mem next key) then
      record_event session ~kind:"unregistered" entry) session.catalog;
  Hashtbl.reset session.catalog;
  Hashtbl.iter (Hashtbl.replace session.catalog) (Hashtbl.copy next)

let catalog_status session =
  if session.native_available || Hashtbl.length session.catalog > 0
  then "ready" else "unavailable"

let catalog_entries session ?name ?frame () =
  Hashtbl.fold (fun _ entry acc ->
    let matches_name = match name with
      | None -> true
      | Some wanted -> member_string "name" entry = wanted in
    let matches_frame = match frame with
      | None -> true
      | Some wanted -> member_string "frameId" entry = wanted in
    if matches_name && matches_frame then entry :: acc else acc)
    session.catalog []
  |> List.sort (fun a b ->
       let by_name = compare (member "name" a) (member "name" b) in
       if by_name <> 0 then by_name else
         compare (member "frameId" a) (member "frameId" b))

(* Exact-name reads expose full metadata (schemas, annotations); the default
   projection is a bounded summary that omits them and truncates
   descriptions. *)
let list_tools ?(cancel = fun () -> false) ?name ?frame manager ~id =
  let session = lookup manager ~id in
  session_operation ~cancel session (fun cancel ->
    refresh_catalog ~cancel session;
    let entries = catalog_entries session ?name ?frame () in
    let status = catalog_status session in
    let base = [
      "session", `String id;
      "status", `String status ] in
    let reason =
      if status = "unavailable"
      then ["reason", `String
        "the page declared no modelContext tools and no registration surface is available"]
      else [] in
    match name with
    | Some _ ->
        `Assoc (base @ reason @ [
          "tools", `List (List.map (fun entry -> `Assoc (
            match entry with
            | `Assoc fields -> ("untrusted", `Bool true) :: fields
            | other -> ["untrusted", `Bool true; "value", other])) entries);
          "truncated", `Bool false;
          "untrusted", `Bool true ])
    | None ->
        let tools, _, truncated =
          List.fold_left (fun (tools, bytes, truncated) entry ->
            if List.length tools >= max_page_tools then
              (tools, bytes, true)
            else
              let full = member_string "description" entry in
              let description = utf8_prefix full max_tool_description_bytes in
              let truncated =
                truncated || String.length description < String.length full in
              let record = `Assoc [
                "name", member "name" entry;
                "description", `String description;
                "frameId", member "frameId" entry;
                "origin", member "origin" entry;
                "untrusted", `Bool true ] in
              let record_bytes =
                String.length (Yojson.Basic.to_string record) + 1 in
              if bytes + record_bytes > max_tool_summary_bytes then
                (tools, bytes, true)
              else (record :: tools, bytes + record_bytes, truncated))
            ([], 128, false) entries in
        let truncated = truncated || List.length tools < List.length entries in
        `Assoc (base @ reason @ [
          "tools", `List (List.rev tools);
          "truncated", `Bool truncated;
          "untrusted", `Bool true ]))

let call_failure ~id ~name error = `Assoc [
  "session", `String id;
  "name", `String name;
  "ok", `Bool false;
  "error", `String (untrusted_boundary error);
  "untrusted", `Bool true ]

(* Wraps a page result: under the cap it passes through as the JSON value;
   over the cap it becomes a delimited preview instead of failing. *)
let page_result_json ~id ~name encoded =
  match (try Some (Yojson.Basic.from_string encoded) with _ -> None) with
  | None -> call_failure ~id ~name "page tool result was not JSON data"
  | Some value ->
      let bytes = String.length encoded in
      if bytes <= max_page_result_bytes then
        `Assoc [
          "session", `String id;
          "name", `String name;
          "ok", `Bool true;
          "result", value;
          "untrusted", `Bool true ]
      else
        `Assoc [
          "session", `String id;
          "name", `String name;
          "ok", `Bool true;
          "result", `Assoc [
            "preview", `String (untrusted_boundary
              (utf8_prefix encoded (max_page_result_bytes / 4)));
            "truncated", `Bool true;
            "originalBytes", `Int bytes ];
          "truncated", `Bool true;
          "originalBytes", `Int bytes;
          "untrusted", `Bool true ]

let call_tool ?(cancel = fun () -> false) ?frame manager ~id ~name ~arguments
    ~timeout_seconds =
  let name = valid_token "page tool name" max_tool_name_bytes name in
  let arguments = match arguments with
    | `Null -> `Assoc []
    | `Assoc _ as value -> value
    | _ -> fail "page tool arguments must be a JSON object" in
  let arguments_text = Yojson.Basic.to_string arguments in
  if String.length arguments_text > max_tool_arguments_bytes then
    fail "page tool arguments exceed their size limit";
  let timeout_seconds = valid_timeout "page tool timeout" timeout_seconds in
  let session = lookup manager ~id in
  session_operation ~cancel ~timeout_seconds session (fun cancel ->
    refresh_catalog ~cancel session;
    let matches = catalog_entries session ~name ?frame () in
    match matches with
    | [] -> call_failure ~id ~name ("no page-declared tool named " ^
        json_string_literal name ^
        (match frame with
         | Some frame -> " in frame " ^ json_string_literal frame
         | None -> ""))
    | _ :: _ :: _ ->
        let frames = String.concat ", " (List.map (fun entry ->
          member_string "frameId" entry) matches) in
        call_failure ~id ~name
          ("page tool " ^ json_string_literal name ^
           " exists in multiple frames: " ^ frames)
    | [entry] ->
        let frame_id = member_string "frameId" entry in
        let cdp_session = match session.cdp_session_id with
          | Some value -> value | None -> "" in
        let expression =
          "(async () => { const b = window.__paveWebTools; \
           if (!b) return {ok:false, error:'page tool bridge is unavailable'}; \
           try { const r = await b.invoke(" ^ json_string_literal name ^ ", " ^
           arguments_text ^ "); \
           return {ok:true, encoded: JSON.stringify(r === undefined ? null : r)}; } \
           catch (e) { return {ok:false, error: e && e.message ? e.message : String(e)}; } })()" in
        (match evaluate_in_frame ~cancel session ~cdp_session ~frame_id
           ~is_root:(frame_id = session.root_frame)
           ~expression ~timeout_seconds with
         | `Skipped -> call_failure ~id ~name
             "the owning frame has no reachable context"
         | `Done answer ->
             (match member "result" answer with
              | `Assoc fields ->
                  (match field "value" fields with
                   | `Assoc _ | `String _ ->
                       let envelope = match field "value" fields with
                         | `String text ->
                             (try Yojson.Basic.from_string text with _ -> `Null)
                         | `Assoc _ as value -> value
                         | _ -> `Null in
                       (match member "ok" envelope with
                        | `Bool true ->
                            (match member "encoded" envelope with
                             | `String encoded ->
                                 page_result_json ~id ~name encoded
                             | _ -> call_failure ~id ~name
                                 "page tool result was not serializable")
                        | `Bool false ->
                            call_failure ~id ~name
                              (member_string "error" envelope)
                        | _ -> call_failure ~id ~name
                            "page tool returned a malformed envelope")
                   | _ ->
                       (match member "exceptionDetails" answer with
                        | `Assoc _ as detail ->
                            call_failure ~id ~name
                              (member_string "text" detail)
                        | _ -> call_failure ~id ~name
                            "page tool call produced no response"))
              | _ -> call_failure ~id ~name
                  "page tool call produced no response")))

let tool_events ?(cancel = fun () -> false) ?since ?clear manager ~id =
  let session = lookup manager ~id in
  session_operation ~cancel session (fun cancel ->
    refresh_catalog ~cancel session;
    let since = match since with Some value -> value | None -> 0 in
    let kept = List.filter (fun event ->
      match member "sequence" event with
      | `Int sequence -> sequence > since
      | _ -> false) session.catalog_events in
    let truncated = since < session.dropped_before in
    let result = `Assoc [
      "session", `String id;
      "events", `List (List.rev kept);
      "cursor", `Int session.event_sequence;
      "truncated", `Bool truncated;
      "untrusted", `Bool true ] in
    (match clear with
     | Some true ->
         session.catalog_events <- [];
         session.dropped_before <- 0
     | _ -> ());
    result)
