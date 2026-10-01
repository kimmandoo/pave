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
let max_list_tools = 16
let max_tool_field_bytes = 160
let browser_variable = Web_search.browser_variable

(* ------------------------------------------------------------------ *)
(* Small utilities                                                    *)

let check_cancel cancel = if (try cancel () with _ -> true) then raise Cancelled

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

let rec read_exact cancel fd buffer offset remaining =
  if remaining <> 0 then begin
    check_cancel cancel;
    let readable, _, _ =
      try Unix.select [fd] [] [] 5.0
      with Unix.Unix_error (Unix.EINTR, _, _) -> [fd], [], [] in
    if readable = [] then fail "browser connection timed out waiting for data";
    match Unix.read fd buffer offset remaining with
    | 0 -> fail "browser connection closed unexpectedly"
    | count -> read_exact cancel fd buffer (offset + count) (remaining - count)
  end

let write_all fd text =
  let rec loop offset =
    if offset < String.length text then
      match Unix.write_substring fd text offset (String.length text - offset) with
      | 0 -> fail "browser connection closed during write"
      | count -> loop (offset + count)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop offset
  in
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
  let key = base64_encode (random_bytes 16) in
  let request =
    "GET " ^ path ^ " HTTP/1.1\r\nHost: 127.0.0.1:" ^ string_of_int port ^
    "\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: " ^ key ^
    "\r\nSec-WebSocket-Version: 13\r\n\r\n" in
  (try
     write_all fd request;
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
    read_exact cancel transport.fd header 0 2;
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
        Int64.to_int (Bytes.get_int64_be ext 0) in
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
        with_lock transport.write_lock (fun () ->
          write_all transport.fd (encode_client_frame ~opcode:0xA body));
        collect ()
    | 0xA -> collect ()
    | (0x0 | 0x1 | 0x2) ->
        Buffer.add_string payload body;
        if Buffer.length payload > limit then fail "browser response exceeds its size limit";
        if fin then `Text (Buffer.contents payload) else collect ()
    | _ -> collect () in
  collect ()

let ws_send transport payload =
  if transport.closed then fail "browser debugging connection is closed";
  with_lock transport.write_lock (fun () ->
    write_all transport.fd (encode_client_frame ~opcode:0x1 payload))

let ws_close transport =
  if not transport.closed then (
    transport.closed <- true;
    (try write_all transport.fd (encode_client_frame ~opcode:0x8 "")
     with _ -> ());
    close_socket transport.fd)

(* ------------------------------------------------------------------ *)
(* CDP session layer                                                  *)

(* A connection is one ordered request/response channel to the browser-level
   endpoint. [receive] returns the next raw CDP message text; [close] is
   idempotent. Injected by tests to run an in-process fake endpoint. *)
type connection = {
  send : string -> unit;
  receive : cancel:(unit -> bool) -> string;
  close : unit -> unit;
  mutable next_id : int;
  io_lock : Mutex.t;
}

let ws_connection transport =
  { send = (fun payload -> ws_send transport payload);
    receive = (fun ~cancel ->
      match read_ws_message ~cancel ~limit:max_ws_message_bytes transport with
      | `Closed -> fail "browser closed the debugging connection"
      | `Text message -> message);
    close = (fun () -> ws_close transport);
    next_id = 1;
    io_lock = Mutex.create () }

let cdp_close connection = connection.close ()

let send_cdp ?(cancel = fun () -> false) ?session_id connection ~method_ ~params () =
  with_lock connection.io_lock (fun () ->
    let id = connection.next_id in
    connection.next_id <- id + 1;
    let fields =
      [ "id", `Int id;
        "method", `String method_;
        "params", params ] @
      (match session_id with
       | Some session -> ["sessionId", `String session]
       | None -> []) in
    connection.send (Yojson.Basic.to_string (`Assoc fields));
    let deadline = Unix.gettimeofday () +. max_operation_seconds in
    let rec wait () =
      check_cancel cancel;
      if Unix.gettimeofday () > deadline then
        fail "browser did not answer a command in time";
      let message = connection.receive ~cancel in
      (match (try Yojson.Basic.from_string message with _ -> `Null) with
       | `Assoc _ as json when member "id" json = `Int id ->
           (match member "error" json with
            | `Assoc error_fields ->
                fail ("browser command failed: " ^
                  (match List.assoc_opt "message" error_fields with
                   | Some (`String text) -> text | _ -> "unknown error"))
            | _ -> member "result" json)
       | _ -> wait ()) in
    wait ())

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
let spawn_browser ~program ~arguments =
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
    const registry = new Map();
    const record = (tool) => {
      if (!tool || typeof tool !== "object") return tool;
      const name = String(tool.name || "");
      if (name) registry.set(name, tool);
      return tool;
    };
    window.__paveWebTools = {
      list() {
        return [...registry.values()].map(tool => ({
          name: String(tool.name || ""),
          description: String(tool.description || ""),
          inputSchema: tool.inputSchema,
          annotations: tool.annotations
        }));
      },
      count() { return registry.size; },
      async call(name, args) {
        const tool = registry.get(String(name));
        if (!tool) throw new Error("unknown page tool: " + name);
        if (typeof tool.execute === "function")
          return await tool.execute(args === undefined ? {} : args);
        return await tool;
      },
      _record: record
    };
    const patch = (owner) => {
      try {
        if (!owner || !owner.modelContext) return;
        const mc = owner.modelContext;
        for (const key of ["registerTool", "provide"]) {
          const original = mc[key];
          if (typeof original === "function" && !original.__paveWrapped) {
            const wrapped = function (...args) {
              for (const arg of args) {
                if (Array.isArray(arg)) arg.forEach(record); else record(arg);
              }
              const out = original.apply(this, args);
              if (out && out.tools && Array.isArray(out.tools)) out.tools.forEach(record);
              return out;
            };
            wrapped.__paveWrapped = true;
            mc[key] = wrapped;
          }
        }
        if (typeof mc.unregisterTool === "function" && !mc.unregisterTool.__paveWrapped) {
          const original = mc.unregisterTool;
          const wrapped = function (name) {
            registry.delete(String(name));
            return original.apply(this, arguments);
          };
          wrapped.__paveWrapped = true;
          mc.unregisterTool = wrapped;
        }
      } catch (_) {}
    };
    patch(navigator);
    patch(document);
    if (!navigator.modelContext && !document.modelContext) {
      const shim = {
        registerTool(tool) { record(tool); return tool; },
        provide(target) {
          if (target && target.tools && Array.isArray(target.tools))
            target.tools.forEach(record);
          return target;
        },
        unregisterTool(name) { registry.delete(String(name)); },
        clearContext() { registry.clear(); },
        get tools() { return [...registry.values()]; }
      };
      try { Object.defineProperty(navigator, "modelContext", { value: shim }); }
      catch (_) {}
      try { Object.defineProperty(document, "modelContext", { value: shim }); }
      catch (_) {}
    }
  } catch (_) {}
})();
|js}

let session_operation ?(cancel = fun () -> false) session action =
  with_lock session.operation_lock (fun () ->
    if session.closed then fail "browser session is closed";
    if Unix.gettimeofday () -. session.created_at > max_session_seconds then
      fail "browser session exceeded its lifetime bound; close it and open a new session";
    if Unix.gettimeofday () -. session.last_used_at > max_linger_seconds then
      fail "browser session expired from inactivity; close it and open a new session";
    check_cancel cancel;
    let result = action () in
    session.last_used_at <- Unix.gettimeofday ();
    result)

let require_connection session =
  match session.connection, session.cdp_session_id with
  | Some connection, Some cdp_session -> connection, cdp_session
  | _ -> fail "browser session has no attached page"

let evaluate_raw ?(cancel = fun () -> false) session ~expression ~timeout_seconds =
  let connection, cdp_session = require_connection session in
  send_cdp ~cancel ~session_id:cdp_session connection
    ~method_:"Runtime.evaluate"
    ~params:(`Assoc [
      "expression", `String expression;
      "returnByValue", `Bool true;
      "awaitPromise", `Bool true;
      "timeout", `Int (int_of_float (timeout_seconds *. 1000.));
      "userGesture", `Bool false ])
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

let bounded_result label text =
  if String.length text > max_result_bytes then
    fail (label ^ " exceeds its size limit");
  text

(* ------------------------------------------------------------------ *)
(* Public operations                                                  *)

(* A placeholder reserves the id under the manager lock while the process and
   transport are established; close_manager can still observe and drop it. *)
let pending_session manager ~id =
  { id; manager; connection = None; cdp_session_id = None; target_id = "";
    child = None; created_at = Unix.gettimeofday ();
    last_used_at = Unix.gettimeofday ();
    operation_lock = Mutex.create (); closed = false; pending = true }

(* [spawn]/[connect] are injectable for in-process tests; production defaults
   launch the pinned executable and open its loopback WebSocket. *)
let open_session ?(env = Sys.getenv_opt) ?(cancel = fun () -> false)
    ?spawn ?connect manager ~id =
  let id = valid_token "browser session id" 128 id in
  let spawn = match spawn with
    | Some spawn -> spawn
    | None ->
        let program = detect_browser ~env () in
        fun ~cancel:_ ->
          let profile = Filename.temp_dir "pave-browser-" "" in
          { pid = spawn_browser ~program
              ~arguments:(browser_arguments ~profile); profile } in
  let connect = match connect with
    | Some connect -> connect
    | None ->
        fun ~cancel ~profile ->
          let port, ws_path = read_active_port cancel ~profile in
          ws_connection (connect_ws ~cancel ~port ~path:ws_path ()) in
  check_cancel cancel;
  with_lock manager.lock (fun () ->
    if manager.manager_closed then fail "browser manager is closed";
    if Hashtbl.mem manager.sessions id then fail "browser session id is already in use";
    if Hashtbl.length manager.sessions >= max_sessions then
      fail "browser session limit reached";
    Hashtbl.replace manager.sessions id (pending_session manager ~id));
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
           with_lock manager.lock (fun () ->
             match Hashtbl.find_opt manager.sessions id with
             | Some placeholder when placeholder.pending ->
                 Hashtbl.replace manager.sessions id session
             | _ ->
                 (* The placeholder was closed meanwhile: drop everything. *)
                 raise (Error "browser session was closed during startup"));
           id
         with exn ->
           cdp_close connection;
           raise exn)
      with exn ->
        terminate_child child;
        raise exn)
   with exn ->
     with_lock manager.lock (fun () -> Hashtbl.remove manager.sessions id);
     raise exn)

let lookup manager ~id =
  let id = valid_token "browser session id" 128 id in
  with_lock manager.lock (fun () ->
    match Hashtbl.find_opt manager.sessions id with
    | Some session when not session.closed && not session.pending -> session
    | Some _ | None -> fail "browser session is closed or unknown")

let close_session manager ~id =
  let id = valid_token "browser session id" 128 id in
  let session = with_lock manager.lock (fun () ->
    match Hashtbl.find_opt manager.sessions id with
    | None -> None
    | Some session ->
        Hashtbl.remove manager.sessions id;
        Some session) in
  match session with
  | None -> ()
  | Some session ->
      with_lock session.operation_lock (fun () -> session.closed <- true);
      Option.iter cdp_close session.connection;
      session.connection <- None;
      Option.iter terminate_child session.child

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
  session_operation ~cancel session (fun () ->
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
      let probe =
        evaluate_raw ~cancel session ~timeout_seconds:5.
          ~expression:"document.readyState" in
      (match member "result" probe with
       | `Assoc fields ->
           (match field "value" fields with
            | `String "complete" -> ()
            | _ -> Thread.delay 0.1; await_ready ())
       | _ -> Thread.delay 0.1; await_ready ()) in
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
  session_operation ~cancel session (fun () ->
    let answer = evaluate_raw ~cancel session ~expression ~timeout_seconds in
    `Assoc [
      "session", `String id;
      "result", `String (bounded_result "page evaluation" (evaluation_text answer)) ])

let observe ?(cancel = fun () -> false) manager ~id =
  let session = lookup manager ~id in
  session_operation ~cancel session (fun () ->
    let answer =
      evaluate_raw ~cancel session ~timeout_seconds:10.
        ~expression:
          "JSON.stringify({url: location.href, title: document.title, \
           ready: document.readyState, origin: location.origin})" in
    match member "result" answer with
    | `Assoc fields ->
        (match field "value" fields with
         | `String text ->
             (match (try Yojson.Basic.from_string text with _ -> `Null) with
              | `Assoc fields -> `Assoc (("session", `String id) :: fields)
              | _ -> fail "page observation returned malformed state")
         | _ -> fail "page observation returned no state")
    | _ -> fail "page observation returned no state")

let screenshot ?(cancel = fun () -> false) manager ~id =
  let session = lookup manager ~id in
  session_operation ~cancel session (fun () ->
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

(* Mirrors the in-page catalog; native modelContext or the installed hook both
   record through __paveWebTools. Every field is page-provided and untrusted. *)
let list_tools ?(cancel = fun () -> false) manager ~id =
  let session = lookup manager ~id in
  session_operation ~cancel session (fun () ->
    let answer =
      evaluate_raw ~cancel session ~timeout_seconds:10.
        ~expression:"JSON.stringify(window.__paveWebTools ? \
          window.__paveWebTools.list() : null)" in
    match member "result" answer with
    | `Assoc fields ->
        (match field "value" fields with
         | `String "null" | `Null ->
             `Assoc [
               "session", `String id;
               "status", `String "unavailable";
               "tools", `List [];
               "untrusted", `Bool true ]
         | `String text ->
             (match (try Yojson.Basic.from_string text with _ -> `Null) with
              | `List tools ->
                  let render = function
                    | `Assoc fields ->
                        let name = match List.assoc_opt "name" fields with
                          | Some (`String name) -> name | _ -> "" in
                        let description = match List.assoc_opt "description" fields with
                          | Some (`String description) -> description | _ -> "" in
                        `Assoc [
                          "name", `String (if String.length name > max_tool_field_bytes
                            then String.sub name 0 max_tool_field_bytes else name);
                          "description", `String
                            (if String.length description > max_tool_field_bytes
                             then String.sub description 0 max_tool_field_bytes
                             else description) ]
                    | _ -> `Assoc ["name", `String ""; "description", `String ""] in
                  `Assoc [
                    "session", `String id;
                    "status", `String "ready";
                    "tools", `List (List.map render
                      (List.filteri (fun index _ -> index < max_list_tools) tools));
                    "truncated", `Bool (List.length tools > max_list_tools);
                    "untrusted", `Bool true ]
              | _ -> fail "page tool catalog returned malformed data")
         | _ -> fail "page tool catalog returned no data")
    | _ -> fail "page tool catalog returned no data")

let call_tool ?(cancel = fun () -> false) manager ~id ~name ~arguments
    ~timeout_seconds =
  let name = bounded_text "page tool name" max_tool_name_bytes name in
  if String.trim name = "" then fail "page tool name must not be empty";
  let arguments = match arguments with
    | `Null -> `Assoc []
    | `Assoc _ as value -> value
    | _ -> fail "page tool arguments must be a JSON object" in
  let arguments_text = Yojson.Basic.to_string arguments in
  if String.length arguments_text > max_tool_arguments_bytes then
    fail "page tool arguments exceed their size limit";
  let timeout_seconds = valid_timeout "page tool timeout" timeout_seconds in
  let session = lookup manager ~id in
  session_operation ~cancel session (fun () ->
    let expression =
      "(window.__paveWebTools ? window.__paveWebTools.call(" ^
      json_string_literal name ^ ", " ^ arguments_text ^
      ") : Promise.reject(new Error('page has no modelContext tools')))" in
    let answer =
      evaluate_raw ~cancel session ~expression ~timeout_seconds in
    `Assoc [
      "session", `String id;
      "name", `String name;
      "result", `String (bounded_result "page tool result" (evaluation_text answer));
      "untrusted", `Bool true ])
