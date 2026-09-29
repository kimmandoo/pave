(* Streamable HTTP (MCP 2025-06-18). Each instance owns its session and
   serializes requests; curl never follows redirects or inherits proxy settings. *)
exception Error of string
exception Cancelled

let max_body = 1_048_576
let max_headers = 16_384
let version = "2025-06-18"
let fail message = raise (Error message)
let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()
let check_cancel cancelled = if (try cancelled () with _ -> true) then raise Cancelled
let with_lock lock f = Mutex.lock lock; Fun.protect ~finally:(fun () -> Mutex.unlock lock) f
let visible value = value <> "" && String.length value <= 4096 &&
  String.for_all (fun ch -> let n = Char.code ch in n >= 33 && n <= 126) value
let no_controls value = not (String.exists (fun c -> Char.code c < 32 || Char.code c = 127) value)

(* Reject URL features which can change the destination or escape curl's
   configuration syntax. Only an explicitly enabled loopback fixture uses HTTP. *)
let validate_endpoint ~allow_loopback_http url =
  if url = "" || String.length url > 4096 || not (no_controls url) ||
     String.contains url '#' then fail "invalid MCP HTTP endpoint";
  let scheme, rest =
    if String.starts_with ~prefix:"https://" url then "https", String.sub url 8 (String.length url - 8)
    else if String.starts_with ~prefix:"http://" url then "http", String.sub url 7 (String.length url - 7)
    else fail "MCP endpoint requires HTTPS" in
  let stop = match String.index_opt rest '/' with None -> String.length rest | Some n -> n in
  let stop = match String.index_opt rest '?' with None -> stop | Some n -> min n stop in
  let authority = String.sub rest 0 stop in
  if authority = "" || String.contains authority '@' || String.contains authority '\\' ||
     String.contains rest '\\' || String.contains authority ' ' then fail "invalid MCP HTTP endpoint authority";
  let host, port =
    if authority.[0] = '[' then
      match String.index_opt authority ']' with
      | None -> fail "invalid MCP HTTP IPv6 endpoint"
      | Some n -> String.sub authority 1 (n - 1),
          String.sub authority (n + 1) (String.length authority - n - 1)
    else match String.index_opt authority ':' with
      | None -> authority, ""
      | Some n -> String.sub authority 0 n, String.sub authority n (String.length authority - n) in
  let valid_host = if authority.[0] = '[' then
      (String.contains host ':' && try ignore (Unix.inet_addr_of_string host); true
       with Failure _ -> false)
    else String.for_all (function
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '-' -> true
      | _ -> false) host in
  if host = "" || not valid_host ||
     (port <> "" && (port.[0] <> ':' || String.length port < 2 ||
       not (String.for_all (function '0'..'9' -> true | _ -> false)
         (String.sub port 1 (String.length port - 1))))) then
    fail "invalid MCP HTTP endpoint authority";
  if port <> "" then (let n = try int_of_string (String.sub port 1 (String.length port - 1))
    with _ -> fail "invalid MCP HTTP endpoint port" in
    if n < 1 || n > 65535 then fail "invalid MCP HTTP endpoint port");
  if scheme = "http" && not (allow_loopback_http &&
      List.mem (String.lowercase_ascii host) ["127.0.0.1"; "localhost"; "::1"]) then
    fail "MCP HTTP requires HTTPS except for explicitly enabled loopback";
  if stop < String.length rest && rest.[stop] = '?' then
    fail "MCP endpoint must have an explicit path";
  if stop = String.length rest then fail "MCP endpoint must have an explicit path";
  scheme, host, port

let quote value =
  let b = Buffer.create (String.length value + 2) in
  Buffer.add_char b '"';
  String.iter (function
    | '"' -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | c -> Buffer.add_char b c) value;
  Buffer.add_char b '"'; Buffer.contents b

let index_from text start needle =
  let last = String.length text - String.length needle in
  let rec loop n = if n > last then None
    else if String.sub text n (String.length needle) = needle then Some n
    else loop (n + 1) in loop start
let field key fields = List.assoc_opt key fields
let object_fields = function
  | `Assoc fields ->
      let names = List.map fst fields in
      if List.length names <> List.length (List.sort_uniq String.compare names) then
        fail "duplicate MCP JSON-RPC field";
      fields
  | _ -> fail "MCP JSON-RPC response is not an object"
let response_for id text =
  let fields = object_fields (try Yojson.Basic.from_string text with _ -> fail "invalid MCP JSON response") in
  if field "jsonrpc" fields <> Some (`String "2.0") || field "id" fields <> Some (`Int id) ||
     field "method" fields <> None then fail "unexpected MCP JSON-RPC response ID or version";
  match field "result" fields, field "error" fields with
  | Some result, None -> result
  | None, Some error ->
      let detail = object_fields error in
      (match field "code" detail, field "message" detail with
       | Some (`Int _), Some (`String _) -> fail "MCP server returned a JSON-RPC error"
       | _ -> fail "invalid MCP JSON-RPC error")
  | _ -> fail "MCP response needs exactly one result or error"

let media_type value =
  String.lowercase_ascii (String.trim (List.hd (String.split_on_char ';' value)))
let headers text =
  let lines = String.split_on_char '\n' text in
  match lines with
  | status :: rest ->
      let status = String.trim status in
      let parts = String.split_on_char ' ' status |> List.filter ((<>) "") in
      let code = match parts with
        | protocol :: code :: _ when String.starts_with ~prefix:"HTTP/" protocol ->
            (try int_of_string code with _ -> fail "invalid MCP HTTP status")
        | _ -> fail "invalid MCP HTTP status" in
      let values = List.map (fun line ->
        let line = String.trim line in
        match String.index_opt line ':' with
        | None -> fail "invalid MCP HTTP header"
        | Some n ->
            let name = String.lowercase_ascii (String.sub line 0 n) in
            let value = String.trim (String.sub line (n + 1) (String.length line - n - 1)) in
            if name = "" || not (no_controls name && no_controls value) then
              fail "invalid MCP HTTP header";
            name, value) rest in
      let unique name = match List.filter (fun (key, _) -> key = name) values with
        | [] -> None | [(_, value)] -> Some value
        | _ -> fail ("duplicate MCP HTTP " ^ name ^ " header") in
      code, unique
  | _ -> fail "missing MCP HTTP response headers"

let sse_response id body =
  let cursor = ref 0 and events = ref 0 in
  let rec loop () =
    match index_from body !cursor "\n\n" with
    | None -> None
    | Some stop ->
        incr events;
        if !events > 64 then fail "too many MCP SSE events";
        let frame = String.sub body !cursor (stop - !cursor) in
        cursor := stop + 2;
        let data = Buffer.create 256 in
        String.split_on_char '\n' frame |> List.iter (fun line ->
          if String.starts_with ~prefix:"data:" line then (
            if Buffer.length data > 0 then Buffer.add_char data '\n';
            let value = String.sub line 5 (String.length line - 5) in
            Buffer.add_string data (if String.starts_with ~prefix:" " value then
              String.sub value 1 (String.length value - 1) else value)));
        if Buffer.length data = 0 then loop ()
        else
          let fields = object_fields (try Yojson.Basic.from_string (Buffer.contents data)
            with _ -> fail "invalid MCP SSE JSON event") in
          match field "id" fields with
          | Some (`Int actual) when actual = id -> Some (response_for id (Buffer.contents data))
          | Some _ -> fail "unexpected MCP SSE JSON-RPC response ID"
          | None ->
              (* Notifications can precede the response. Requests from the server
                 cannot be serviced by this request/response-only transport. *)
              if field "jsonrpc" fields <> Some (`String "2.0") ||
                field "result" fields <> None || field "error" fields <> None then
                fail "invalid MCP SSE message";
              (match field "method" fields with
               | Some (`String method_) when method_ <> "" -> loop ()
               | _ -> fail "unsupported MCP SSE server request")
  in loop ()

type t = {
  endpoint : string; scheme : string; host : string; port : string;
  bearer_token : string option; lock : Mutex.t;
  mutable session_id : string option; mutable initialized : bool;
  mutable next_id : int; mutable closed : bool;
}
let create ?bearer_token ?(allow_loopback_http = false) endpoint =
  let scheme, host, port = validate_endpoint ~allow_loopback_http endpoint in
  (match bearer_token with None -> () | Some token when visible token -> ()
    | Some _ -> fail "invalid MCP bearer token");
  { endpoint; scheme; host; port; bearer_token; lock = Mutex.create ();
    session_id = None; initialized = false; next_id = 1; closed = false }
let diagnose t operation =
  try operation () with Error message ->
    fail ("MCP HTTP " ^ t.host ^ ": " ^ message)

let send t ~method_ ~body ~timeout_seconds ~cancelled ~expected =
  if not (Float.is_finite timeout_seconds) || timeout_seconds <= 0. || timeout_seconds > 60. then
    fail "MCP HTTP deadline must be between 0 and 60 seconds";
  check_cancel cancelled;
  let deadline = Unix.gettimeofday () +. timeout_seconds in
  let config = Buffer.create 1024 in
  let option name value = Buffer.add_string config (name ^ " = " ^ quote value ^ "\n") in
  Buffer.add_string config "silent\nshow-error\ninclude\nno-buffer\nhttp1.1\ngloboff\npath-as-is\n";
  option "url" t.endpoint;
  option "request" method_;
  option "proto" ("=" ^ t.scheme);
  option "max-redirs" "0";
  option "noproxy" "*";
  option "connect-timeout" (string_of_int (max 1 (min 5 (int_of_float (ceil timeout_seconds)))));
  option "max-time" (string_of_int (max 2 (int_of_float (ceil timeout_seconds) + 1)));
  option "max-filesize" (string_of_int max_body);
  (if t.scheme = "http" && String.lowercase_ascii t.host = "localhost" then
    option "resolve" ("localhost:" ^ (if t.port = "" then "80" else
      String.sub t.port 1 (String.length t.port - 1)) ^ ":127.0.0.1"));
  List.iter (fun value -> option "header" value)
    (["Accept: application/json, text/event-stream"; "Content-Type: application/json";
      "Expect:"; "Connection: close"] @
     (if t.initialized then ["MCP-Protocol-Version: " ^ version] else []) @
     (match t.session_id with None -> [] | Some value -> ["Mcp-Session-Id: " ^ value]) @
     (match t.bearer_token with None -> [] | Some value -> ["Authorization: Bearer " ^ value]));
  (match body with None -> () | Some data -> option "data-binary" data);
  let config = Buffer.contents config in
  if String.length config > max_body + 8192 then fail "MCP HTTP request exceeds 1 MiB";
  let input_read, input_write = Unix.pipe ~cloexec:true () in
  let output_read, output_write = try Unix.pipe ~cloexec:true () with exn ->
    close_fd input_read; close_fd input_write; raise exn in
  let null_fd = try Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 with exn ->
    List.iter close_fd [input_read; input_write; output_read; output_write]; raise exn in
  let pid = try Unix.create_process_env "/usr/bin/curl"
    [|"curl"; "--disable"; "--config"; "-"|]
    [|"LANG=C"; "LC_ALL=C"; "HOME=/dev/null"|]
    input_read output_write null_fd
  with exn ->
    List.iter close_fd [input_read; input_write; output_read; output_write; null_fd];
    fail ("could not start MCP HTTP transport: " ^ Printexc.to_string exn) in
  List.iter close_fd [input_read; output_write; null_fd];
  let reaped = ref false in
  let reap kill =
    if not !reaped then (
      reaped := true;
      if kill then (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())) in
  Fun.protect ~finally:(fun () -> close_fd input_write; close_fd output_read; reap true) (fun () ->
    Unix.set_nonblock input_write; Unix.set_nonblock output_read;
    let pos = ref 0 and received = Buffer.create 4096 and header = ref None in
    let result = ref None and eof = ref false in
    let scan () =
      let text = Buffer.contents received in
      (match !header with
       | None ->
           (match index_from text 0 "\r\n\r\n" with
            | None -> if String.length text > max_headers then fail "MCP HTTP headers exceed 16 KiB"
            | Some stop ->
                if stop > max_headers then fail "MCP HTTP headers exceed 16 KiB";
                let code, get = headers (String.sub text 0 stop) in
                if code = 100 then fail "unexpected MCP HTTP interim response";
                header := Some (code, get, stop + 4))
       | Some _ -> ());
      (match !header with
       | None -> ()
       | Some (code, get, start) ->
           let length = String.length text - start in
           if length > max_body then fail "MCP HTTP response exceeds 1 MiB";
           if code >= 300 && code < 400 then fail "MCP HTTP redirect refused";
           if code = 404 && t.session_id <> None then (
             t.session_id <- None; t.initialized <- false;
             fail "MCP HTTP session expired; reinitialize before further requests");
           if expected = `Delete && (code = 200 || code = 202 || code = 204 || code = 405) then
             (if !eof then result := Some (code, get, ""))
           else if code <> (if expected = `Notification then 202 else 200) then
             fail (Printf.sprintf "MCP HTTP server returned status %d" code)
           else if expected = `Notification then (
             if length <> 0 then fail "MCP HTTP notification acknowledgement has a body";
             if !eof then result := Some (code, get, ""))
           else
             let content_type = match get "content-type" with Some value -> media_type value
               | None -> fail "missing MCP HTTP content type" in
             if content_type = "application/json" then (
               if !eof then result := Some (code, get, String.sub text start length))
             else if content_type = "text/event-stream" then (
               let body = String.sub text start length |> String.split_on_char '\r'
                 |> String.concat "" in
               match sse_response (match expected with `Response id -> id | _ -> assert false) body with
               | Some value -> result := Some (code, get, Yojson.Basic.to_string (`Assoc ["result", value]))
               | None -> if !eof then fail "MCP SSE stream ended before its response")
             else fail "unsupported MCP HTTP content type") in
    let rec loop () =
      check_cancel cancelled;
      if Unix.gettimeofday () >= deadline then fail "MCP HTTP request timed out";
      scan ();
      match !result with
      | Some value -> value
      | None ->
          if !eof then fail "MCP HTTP response incomplete";
          let wait = min 0.05 (deadline -. Unix.gettimeofday ()) in
          let ready_in, ready_out, _ = Unix.select [output_read]
            (if !pos < String.length config then [input_write] else []) [] wait in
          if ready_out <> [] then (
            let written = try Unix.write_substring input_write config !pos
              (min 16_384 (String.length config - !pos)) with
              | Unix.Unix_error (Unix.EAGAIN, _, _) | Unix.Unix_error (Unix.EINTR, _, _) -> 0
              | Unix.Unix_error _ -> fail "MCP HTTP configuration pipe failed" in
            pos := !pos + written;
            if !pos = String.length config then close_fd input_write);
          if ready_in <> [] then (
            let chunk = Bytes.create 8192 in
            let count = try Unix.read output_read chunk 0 (Bytes.length chunk) with
              | Unix.Unix_error (Unix.EAGAIN, _, _) | Unix.Unix_error (Unix.EINTR, _, _) -> -1
              | Unix.Unix_error _ -> fail "MCP HTTP response pipe failed" in
            if count = 0 then eof := true
            else if count > 0 then (
              if Buffer.length received + count > max_body + max_headers + 4 then
                fail "MCP HTTP response exceeds size limit";
              Buffer.add_subbytes received chunk 0 count));
          loop ()
    in let answer = loop () in
    if !eof then reap false;
    answer)

let request t ~method_ ~params ~timeout_seconds ~cancelled =
  diagnose t (fun () -> with_lock t.lock (fun () ->
    if t.closed then fail "MCP HTTP transport is closed";
    if t.next_id = max_int then fail "MCP HTTP request IDs exhausted";
    if method_ <> "initialize" && not t.initialized then fail "MCP HTTP server is not initialized";
    if method_ = "initialize" && t.initialized then fail "MCP HTTP server is already initialized";
    let id = t.next_id in t.next_id <- id + 1;
    let body = Yojson.Basic.to_string (`Assoc ["jsonrpc", `String "2.0";
      "id", `Int id; "method", `String method_; "params", params]) in
    if String.length body > max_body then fail "MCP HTTP request exceeds 1 MiB";
    let _, get, text = send t ~method_:"POST" ~body:(Some body)
      ~timeout_seconds ~cancelled ~expected:(`Response id) in
    let value = match get "content-type" with
      | Some content_type when media_type content_type = "text/event-stream" ->
          let fields = object_fields (Yojson.Basic.from_string text) in
          (match field "result" fields with Some value -> value | _ -> fail "invalid MCP SSE response")
      | _ -> response_for id text in
    (match get "mcp-session-id" with
     | None -> ()
     | Some session when visible session && (not t.initialized || t.session_id = Some session) ->
         t.session_id <- Some session
     | Some _ -> fail "invalid or changed MCP HTTP session ID");
    if method_ = "initialize" then t.initialized <- true;
    value))

let notify t ~method_ ~params ~timeout_seconds ~cancelled =
  diagnose t (fun () -> with_lock t.lock (fun () ->
    if t.closed then fail "MCP HTTP transport is closed";
    if not t.initialized then fail "MCP HTTP server is not initialized";
    let body = Yojson.Basic.to_string (`Assoc ["jsonrpc", `String "2.0";
      "method", `String method_; "params", params]) in
    if String.length body > max_body then fail "MCP HTTP notification exceeds 1 MiB";
    ignore (send t ~method_:"POST" ~body:(Some body)
      ~timeout_seconds ~cancelled ~expected:`Notification)))

let end_session t =
  if t.session_id <> None then
    (try ignore (send t ~method_:"DELETE" ~body:None ~timeout_seconds:5.
      ~cancelled:(fun () -> false) ~expected:`Delete) with Error _ | Cancelled -> ());
  t.session_id <- None;
  t.initialized <- false
let close t = with_lock t.lock (fun () ->
  if not t.closed then (end_session t; t.closed <- true))
let reload t = with_lock t.lock (fun () ->
  end_session t; t.next_id <- 1; t.closed <- false)
