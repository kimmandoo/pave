(* Loopback-only HTTP/1.1 server that exposes an owned live session to local
   clients (the "remote control attach" surface).  Every route except
   /healthz requires the configured CSRF token in the x-pave-csrf-token
   header; query-string tokens are intentionally not accepted because they
   leak into logs.  Never log request bodies. *)

type submission =
  | Submit of string
  | Cancel
  | Poll of { since : int }

type request = { meth : string; path : string;
                 headers : (string * string) list; body : string }

type callbacks = {
  read_title : unit -> string;
  read_entries : unit -> Yojson.Basic.t list;
  read_pending : unit -> int;
  submit : submission -> (unit, string) result;
}

type t = {
  listen : Unix.file_descr;
  stop_read : Unix.file_descr;
  stop_write : Unix.file_descr;
  bound_port : int;
  token : string;
  session_id : string;
  callbacks : callbacks;
  lock : Mutex.t;
  mutable alive : bool;
  mutable active : int;
  mutable clients : Unix.file_descr list;
  mutable accept_thread : Thread.t option;
}

let head_cap = 16 * 1024
let body_cap = 64 * 1024
let max_connections = 32
let io_timeout = 5.0
(* Total wall-clock budget per connection; per-read timeouts alone cannot
   stop a slow drip from holding a handler slot forever. *)
let request_deadline = 30.0

exception Http_error of int * string

let is_alive t = Mutex.protect t.lock (fun () -> t.alive)

(* Journal entries carry a dense 1-based "step" ordinal. *)
let entry_step (entry : Yojson.Basic.t) =
  match entry with
  | `Assoc fields ->
    (match List.assoc_opt "step" fields with
     | Some (`Int step) -> step
     | _ -> 0)
  | _ -> 0

let entries_after ~since entries =
  let fresh =
    if since <= 0 then entries
    else List.filter (fun entry -> entry_step entry > since) entries in
  let next = List.fold_left (fun best entry -> max best (entry_step entry)) since entries in
  `Assoc [ "entries", `List fresh; "next", `Int next ]

let json_error message = `Assoc [ "error", `String message ]

let reason status =
  match status with
  | 200 -> "OK" | 202 -> "Accepted" | 400 -> "Bad Request"
  | 403 -> "Forbidden" | 404 -> "Not Found" | 405 -> "Method Not Allowed"
  | 408 -> "Request Timeout" | 409 -> "Conflict" | 411 -> "Length Required"
  | 413 -> "Content Too Large" | 431 -> "Request Header Fields Too Large"
  | 500 -> "Internal Server Error" | 501 -> "Not Implemented"
  | 503 -> "Service Unavailable" | _ -> "Error"

let rec await_io ~deadline ~writing client =
  let remaining = deadline -. Unix.gettimeofday () in
  if remaining <= 0. then raise (Http_error (408, "timeout"));
  let readers, writers = if writing then [], [client] else [client], [] in
  match Unix.select readers writers [] (min io_timeout remaining) with
  | [], [], _ -> raise (Http_error (408, "timeout"))
  | _ -> ()
  | exception Unix.Unix_error (Unix.EINTR, _, _) ->
      await_io ~deadline ~writing client

let write_all ?(deadline = Unix.gettimeofday () +. request_deadline) client bytes =
  let total = Bytes.length bytes in
  let rec loop offset =
    if offset < total then (
      await_io ~deadline ~writing:true client;
      let n =
        try Unix.write client bytes offset (total - offset)
        with Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> -1 in
      if n = 0 then raise (Http_error (400, "closed connection"));
      loop (offset + max 0 n)) in
  loop 0

let write_response ?(deadline = Unix.gettimeofday () +. request_deadline)
    client status (json : Yojson.Basic.t) =
  let body = Yojson.Basic.to_string json in
  let head = Printf.sprintf
      "HTTP/1.1 %d %s\r\ncontent-type: application/json\r\ncontent-length: %d\r\nconnection: close\r\n\r\n"
      status (reason status) (String.length body) in
  write_all ~deadline client (Bytes.unsafe_of_string head);
  write_all ~deadline client (Bytes.unsafe_of_string body)

let header_value request name =
  match List.assoc_opt name request.headers with
  | Some value -> Some value
  | None -> None

(* Returns [Some index] where index is the offset just past the CRLFCRLF
   terminator, or [None] if not yet complete. *)
let find_head_end buffer start =
  let length = Buffer.length buffer in
  let rec scan i =
    if i + 3 >= length then None
    else if Buffer.nth buffer i = '\r' && Buffer.nth buffer (i + 1) = '\n'
        && Buffer.nth buffer (i + 2) = '\r' && Buffer.nth buffer (i + 3) = '\n'
    then Some (i + 4)
    else scan (i + 1) in
  if start >= length then None else scan start

let rec read_bytes ~deadline client bytes offset length =
  await_io ~deadline ~writing:false client;
  try Unix.read client bytes offset length
  with Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
    read_bytes ~deadline client bytes offset length

let split_head head =
  match String.split_on_char '\n' head with
  | request_line :: header_lines ->
    let request_line = String.trim request_line in
    let headers = List.map (fun line ->
        let line = if String.ends_with ~suffix:"\r" line then
          String.sub line 0 (String.length line - 1) else line in
        match String.index_opt line ':' with
        | None -> raise (Http_error (400, "malformed header"))
        | Some colon ->
            let name = String.sub line 0 colon in
            if name = "" || not (String.for_all (function
                | 'a'..'z' | 'A'..'Z' | '0'..'9'
                | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+'
                | '-' | '.' | '^' | '_' | '`' | '|' | '~' -> true
                | _ -> false) name) then
              raise (Http_error (400, "malformed header"));
            let value = String.sub line (colon + 1)
                (String.length line - colon - 1) in
            if String.exists (fun c ->
                (Char.code c < 32 && c <> '\t') || Char.code c = 127) value then
              raise (Http_error (400, "malformed header"));
            String.lowercase_ascii name, String.trim value) header_lines in
    List.iter (fun name ->
      if List.length (List.filter (fun (key, _) -> key = name) headers) > 1 then
        raise (Http_error (400, "duplicate header")))
      ["content-length"; "transfer-encoding"; "x-pave-csrf-token"];
    if List.mem_assoc "content-length" headers &&
       List.mem_assoc "transfer-encoding" headers then
      raise (Http_error (400, "ambiguous body framing"));
    request_line, headers
  | [] -> raise (Http_error (400, "empty request"))

let read_request ?(deadline = Unix.gettimeofday () +. request_deadline) client =
  let buffer = Buffer.create 1024 in
  let chunk = Bytes.create 4096 in
  let rec receive scan_from =
    match find_head_end buffer scan_from with
    | Some head_end ->
      if head_end > head_cap then raise (Http_error (431, "request head too large"));
      head_end
    | None ->
      if Buffer.length buffer >= head_cap then
        raise (Http_error (431, "request head too large"));
      let n = read_bytes ~deadline client chunk 0 (Bytes.length chunk) in
      if n = 0 then raise (Http_error (400, "incomplete request"));
      let old_length = Buffer.length buffer in
      Buffer.add_subbytes buffer chunk 0 n;
      receive (max 0 (old_length - 3)) in
  let head_end = receive 0 in
  let request_text = Buffer.contents buffer in
  let head = String.sub request_text 0 (head_end - 4) in
  let already = String.sub request_text head_end
      (String.length request_text - head_end) in
  let request_line, headers = split_head head in
  let meth, target =
    match List.filter (fun part -> part <> "")
            (String.split_on_char ' ' request_line) with
    | [meth; target; ("HTTP/1.0" | "HTTP/1.1")]
      when String.starts_with ~prefix:"/" target &&
           String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) target ->
      meth, target
    | _ -> raise (Http_error (400, "malformed request line")) in
  let request = { meth; path = target; headers; body = "" } in
  let body =
    match header_value request "transfer-encoding" with
    | Some encoding when String.lowercase_ascii encoding <> "identity" ->
      raise (Http_error (501, "unsupported transfer-encoding"))
    | _ ->
      match header_value request "content-length" with
      | None ->
        if meth = "POST" || meth = "PUT" || meth = "PATCH" then
          raise (Http_error (411, "missing content-length"))
        else ""
      | Some text ->
        if text = "" || not (String.for_all (function '0'..'9' -> true | _ -> false) text)
        then raise (Http_error (400, "invalid content-length"));
        let length =
          match int_of_string_opt text with
          | Some length when length >= 0 -> length
          | _ -> raise (Http_error (400, "invalid content-length")) in
        if length > body_cap then
          raise (Http_error (413, "request body too large"));
        if String.length already >= length then
          String.sub already 0 length
        else begin
          let body = Bytes.create length in
          Bytes.blit_string already 0 body 0 (String.length already);
          let rec fill offset =
            if offset < length then
              let n = read_bytes ~deadline client body offset (length - offset) in
              if n = 0 then raise (Http_error (400, "incomplete body"));
              fill (offset + n) in
          fill (String.length already);
          Bytes.unsafe_to_string body
        end in
  { request with body }

let query_param target name =
  match String.index_opt target '?' with
  | None -> None
  | Some query_start ->
    let query = String.sub target (query_start + 1)
        (String.length target - query_start - 1) in
    String.split_on_char '&' query
    |> List.find_map (fun pair ->
        match String.index_opt pair '=' with
        | Some eq when String.sub pair 0 eq = name ->
          Some (String.sub pair (eq + 1) (String.length pair - eq - 1))
        | _ -> None)

let path_of target =
  match String.index_opt target '?' with
  | None -> target
  | Some index -> String.sub target 0 index

let parse_since request =
  match query_param request.path "since" with
  | None -> 0
  | Some text ->
    (match int_of_string_opt text with
     | Some since when since >= 0 -> since
     | _ -> raise (Http_error (400, "invalid_since")))

let parse_text_field body =
  let json = try Yojson.Basic.from_string body
    with Yojson.Json_error _ -> raise (Http_error (400, "invalid_json")) in
  match json with
  | `Assoc fields ->
      (match List.assoc_opt "text" fields with
       | Some (`String text) when String.trim text <> "" -> text
       | Some (`String _) -> raise (Http_error (400, "empty_text"))
       | _ -> raise (Http_error (400, "missing_text")))
  | _ -> raise (Http_error (400, "invalid_json"))

let submit_result ~deadline client callbacks submission =
  match callbacks.submit submission with
  | Ok () -> write_response ~deadline client 202 (`Assoc [ "queued", `Bool true ])
  | Error message -> write_response ~deadline client 409 (json_error message)

let route ~deadline t client request =
  let write_response = write_response ~deadline in
  let path = path_of request.path in
  if path = "/healthz" then
    if request.meth = "GET" then
      write_response client 200 (`Assoc [ "ok", `Bool true ])
    else write_response client 405 (json_error "method_not_allowed")
  else begin
    (* CSRF-token convention: header only, never a query parameter. The
       comparison walks the full presented length so the byte position of a
       mismatch does not leak through early exit. *)
    let token_matches presented =
      let n = String.length presented in
      if n <> String.length t.token then false
      else
        let mismatch = ref 0 in
        for i = 0 to n - 1 do
          mismatch := !mismatch lor
            (Char.code presented.[i] lxor Char.code t.token.[i])
        done;
        !mismatch = 0 in
    (match header_value request "x-pave-csrf-token" with
     | Some presented when token_matches presented -> ()
     | _ -> write_response client 403 (json_error "csrf");
       raise Exit);
    match request.meth, path with
    | "GET", "/api/session" ->
      write_response client 200
        (`Assoc [ "sessionId", `String t.session_id;
                  "title", `String (t.callbacks.read_title ());
                  "pending", `Int (t.callbacks.read_pending ()) ])
    | "GET", "/api/entries" ->
      let since = parse_since request in
      write_response client 200
        (entries_after ~since (t.callbacks.read_entries ()))
    | "GET", "/api/poll" ->
      (* Read-only refresh: never touches the submission queue. *)
      let since = parse_since request in
      let base = entries_after ~since (t.callbacks.read_entries ()) in
      let fields = match base with `Assoc fields -> fields | _ -> [] in
      write_response client 200
        (`Assoc (("pending", `Int (t.callbacks.read_pending ())) :: fields))
    | "POST", "/api/prompt" ->
      submit_result ~deadline client t.callbacks (Submit (parse_text_field request.body))
    | "POST", "/api/cancel" ->
      submit_result ~deadline client t.callbacks Cancel
    | _, ("/api/session" | "/api/entries" | "/api/poll"
         | "/api/prompt" | "/api/cancel") ->
      write_response client 405 (json_error "method_not_allowed")
    | _ -> write_response client 404 (json_error "not_found")
  end

let acquire t client =
  Mutex.protect t.lock (fun () ->
      if not t.alive || t.active >= max_connections then false
      else begin
        t.active <- t.active + 1;
        t.clients <- client :: t.clients;
        true
      end)

let release t client =
  Mutex.protect t.lock (fun () ->
      t.active <- t.active - 1;
      t.clients <- List.filter (fun fd -> fd <> client) t.clients)

let serve_client t client =
  (* The descriptor is deregistered and shut down before close so a stale
     snapshot in [close] can never shutdown a reused fd number, and no path
     closes an fd that is still listed as a client. *)
  Fun.protect ~finally:(fun () ->
      release t client;
      (try Unix.shutdown client Unix.SHUTDOWN_ALL
       with Unix.Unix_error _ -> ());
      (try Unix.close client with Unix.Unix_error _ -> ())) (fun () ->
      (try Unix.set_close_on_exec client with Unix.Unix_error _ -> ());
      Unix.set_nonblock client;
      (try
         Unix.setsockopt_float client Unix.SO_RCVTIMEO io_timeout;
         Unix.setsockopt_float client Unix.SO_SNDTIMEO io_timeout
       with Unix.Unix_error _ -> ());
      (* Absolute per-connection deadline: the per-read timeout alone would
         let a client drip bytes forever and pin a handler slot. *)
      let deadline = Unix.gettimeofday () +. request_deadline in
      let safe_write status json =
        try write_response ~deadline client status json with _ -> () in
      try
        let request = read_request ~deadline client in
        if Unix.gettimeofday () > deadline then
          raise (Http_error (408, "timeout"))
        else route ~deadline t client request
      with
      | Exit -> ()  (* a response was already written (e.g. csrf) *)
      | Http_error (status, message) -> safe_write status (json_error message)
      | Unix.Unix_error _ -> safe_write 400 (json_error "io")
      | _ -> safe_write 500 (json_error "internal"))

let rec accept_loop t =
  match (try
           let ready, _, _ = Unix.select [t.listen; t.stop_read] [] [] (-1.) in
           if List.mem t.stop_read ready then None
           else Some (Unix.accept t.listen)
         with Unix.Unix_error _ -> None) with
  | Some (client, _peer) ->
    if acquire t client then begin
      let _thread : Thread.t = Thread.create (serve_client t) client in
      accept_loop t
    end
    else begin
      (* A saturated hub answers 503; a closing hub tears the connection down
         silently — a half-open socket that never joined must not observe a
         response after close. *)
      if is_alive t then (
        (try
           Unix.set_nonblock client;
           Unix.setsockopt_float client Unix.SO_SNDTIMEO io_timeout;
           write_response client 503 (json_error "busy")
         with _ -> ());
        (try Unix.close client with Unix.Unix_error _ -> ());
        accept_loop t)
      else
        (try Unix.close client with Unix.Unix_error _ -> ())
    end
  | None ->
    if is_alive t then begin
      (* Transient accept failure (e.g. EMFILE); avoid a busy spin. *)
      Unix.sleepf 0.01;
      accept_loop t
    end

let create ~port ~token ~session_id ~read_title ~read_entries ~read_pending ~submit () =
  let listen = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let stop_read, stop_write =
    try Unix.pipe () with exn -> Unix.close listen; raise exn in
  try
    Unix.set_close_on_exec listen;
    Unix.set_nonblock listen;
    Unix.set_close_on_exec stop_read;
    Unix.set_close_on_exec stop_write;
    Unix.setsockopt listen Unix.SO_REUSEADDR true;
    Unix.bind listen (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
    Unix.listen listen max_connections;
    let bound_port =
      match Unix.getsockname listen with
      | Unix.ADDR_INET (_, port) -> port
      | _ -> port in
    let t = { listen; stop_read; stop_write; bound_port; token; session_id;
              callbacks = { read_title; read_entries; read_pending; submit };
              lock = Mutex.create (); alive = true; active = 0;
              clients = []; accept_thread = None } in
    t.accept_thread <- Some (Thread.create accept_loop t);
    t
  with exn ->
    (try Unix.close listen with Unix.Unix_error _ -> ());
    (try Unix.close stop_read with Unix.Unix_error _ -> ());
    (try Unix.close stop_write with Unix.Unix_error _ -> ());
    raise exn

let port t = t.bound_port

let close t =
  let closing =
    Mutex.protect t.lock (fun () ->
        if not t.alive then false
        else begin
          t.alive <- false;
          (* Shutdown under the lock: handlers cannot release (and then close)
             an fd while we hold it, so each descriptor here is still open and
             still refers to its client socket. *)
          List.iter (fun client ->
              try Unix.shutdown client Unix.SHUTDOWN_ALL
              with Unix.Unix_error _ -> ()) t.clients;
          true
        end) in
  if closing then (
    (* Stop and join the acceptor before closing its descriptor: otherwise a
       recursive accept racing close could act on a reused descriptor number.
       The acceptor never invokes user callbacks. *)
    (* A pipe wakes select on every supported OS; shutdown on a listening
       socket alone is not a portable way to unblock accept. *)
    let wake = Bytes.of_string "x" in
    let rec signal () =
      try ignore (Unix.write t.stop_write wake 0 1)
      with Unix.Unix_error (Unix.EINTR, _, _) -> signal () in
    signal ();
    Option.iter Thread.join t.accept_thread;
    (try Unix.close t.stop_read with Unix.Unix_error _ -> ());
    (try Unix.close t.stop_write with Unix.Unix_error _ -> ());
    (try Unix.close t.listen with Unix.Unix_error _ -> ()))
