(* An MCP session owns every subprocess it opens. Server bytes are untrusted;
   results retain provenance and are never promoted to instructions. *)
exception Error of string
exception Cancelled
let fail text = raise (Error text)
let max_frame = 1_048_576
let max_output = 65_536
let max_items = 128
let max_seconds = 60.
let check_cancel cancelled = if (try cancelled () with _ -> true) then raise Cancelled
let timeout seconds =
  if not (Float.is_finite seconds) || seconds <= 0. || seconds > max_seconds then
    fail "MCP deadline must be between 0 and 60 seconds";
  Unix.gettimeofday () +. seconds
let remaining deadline cancelled =
  check_cancel cancelled;
  let left = deadline -. Unix.gettimeofday () in
  if left <= 0. then fail "MCP request timed out";
  min 0.05 left
let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()
let signal pid sig_ =
  (try Unix.kill (-pid) sig_ with Unix.Unix_error _ -> ());
  (try Unix.kill pid sig_ with Unix.Unix_error _ -> ())

type approval = Start of Mcp_config.server | Effect of Mcp_config.server * string * Yojson.Basic.t
type client = {
  pid : int; input : Unix.file_descr;
  output : Unix.file_descr; mutable buffer : bytes; mutable buffered : int;
  mutable next_id : int;
  mutable closed : bool; lock : Mutex.t;
}
type session = {
  snapshot : Mcp_config.snapshot; authorize : approval -> bool;
  mutable clients : (string * client) list; mutable disposed : bool;
  lock : Mutex.t;
}
type sourced = { server : string; source : Mcp_config.source; value : Yojson.Basic.t }
type request = method_name:string -> params:Yojson.Basic.t ->
  timeout_seconds:float -> cancelled:(unit -> bool) -> Yojson.Basic.t
let with_lock lock fn = Mutex.lock lock; Fun.protect ~finally:(fun () -> Mutex.unlock lock) fn
let with_request_lock lock ~deadline ~cancelled fn =
  let rec acquire () =
    let wait = remaining deadline cancelled in
    if Mutex.try_lock lock then
      Fun.protect ~finally:(fun () -> Mutex.unlock lock) (fun () ->
        ignore (remaining deadline cancelled);
        fn ())
    else (
      ignore (Unix.select [] [] [] wait);
      acquire ()) in
  acquire ()
let dispose_client c =
  if not c.closed then (
    c.closed <- true; close_fd c.input; close_fd c.output;
    signal c.pid Sys.sigterm;
    (try ignore (Unix.select [] [] [] 0.1) with _ -> ());
    signal c.pid Sys.sigkill;
    let rec reap () = try ignore (Unix.waitpid [] c.pid) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
      | Unix.Unix_error (Unix.ECHILD, _, _) -> () in
    reap ())
let create ~snapshot ~authorize =
  { snapshot; authorize; clients = []; disposed = false; lock = Mutex.create () }
let dispose session = with_lock session.lock (fun () ->
  if not session.disposed then (
    session.disposed <- true;
    List.iter (fun (_, (client : client)) ->
      with_lock client.lock (fun () -> dispose_client client)) session.clients;
    session.clients <- []))
let ensure session = if session.disposed then fail "MCP session is disposed"
let approve session action =
  if not (try session.authorize action with _ -> false) then fail "MCP approval denied"
let spawn config =
  (* Recheck the exact executable at use time; no shell, inherited credentials,
     or ambient project environment are passed to the server. *)
  let program, arguments = match config.Mcp_config.transport with
    | Mcp_config.Stdio {program; arguments; _} -> program, arguments
    | Mcp_config.Http _ -> fail "HTTP MCP server cannot use stdio" in
  ignore (Mcp_config.program_path program);
  let environment = Array.of_list (["PATH=/usr/bin:/bin"; "LANG=C";
    "HOME=/nonexistent"] @
    List.map (fun (key, value) -> key ^ "=" ^ value)
      (Mcp_config.resolve_environment config)) in
  let input_read, input_write = Unix.pipe ~cloexec:true () in
  let output_read, output_write = try Unix.pipe ~cloexec:true () with exn ->
    close_fd input_read; close_fd input_write; raise exn in
  let status_read, status_write = try Unix.pipe ~cloexec:true () with exn ->
    List.iter close_fd [input_read; input_write; output_read; output_write]; raise exn in
  let null_fd = try Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 with exn ->
    List.iter close_fd [input_read; input_write; output_read; output_write; status_read; status_write]; raise exn in
  let pid = try Unix.fork () with exn ->
    List.iter close_fd [input_read; input_write; output_read; output_write; status_read; status_write; null_fd]; raise exn in
  if pid = 0 then (
    close_fd input_write; close_fd output_read; close_fd status_read;
    try
      ignore (Unix.setsid ()); Unix.chdir config.root;
      Unix.dup2 input_read Unix.stdin;
      Unix.dup2 output_write Unix.stdout;
      Unix.dup2 null_fd Unix.stderr;
      List.iter close_fd [input_read; output_write; null_fd];
      Unix.execve program (Array.of_list (program :: arguments)) environment
    with _ ->
      (try ignore (Unix.write_substring status_write "x" 0 1) with _ -> ());
      Unix._exit 127)
  else (
    List.iter close_fd [input_read; output_write; status_write; null_fd];
    let c = { pid; input = input_write; output = output_read;
      buffer = Bytes.create 16_384; buffered = 0; next_id = 1;
      closed = false; lock = Mutex.create () } in
    try Unix.set_nonblock c.input; Unix.set_nonblock c.output;
      c, status_read
    with exn -> close_fd status_read; dispose_client c; raise exn)
let wait_ready status ~deadline ~cancelled =
  Fun.protect ~finally:(fun () -> close_fd status) (fun () ->
    let rec loop () =
      let ready, _, _ = Unix.select [status] [] [] (remaining deadline cancelled) in
      if ready = [] then loop () else
      let byte = Bytes.create 1 in
      if Unix.read status byte 0 1 <> 0 then fail "MCP server failed to start"
    in loop ())
let write_all c text ~deadline ~cancelled =
  let rec send offset =
    if offset < String.length text then (
      let _, writable, _ = Unix.select [] [c.input] [] (remaining deadline cancelled) in
      if writable <> [] then
        let count = try Unix.write_substring c.input text offset
          (min 16_384 (String.length text - offset)) with
          | Unix.Unix_error (Unix.EINTR, _, _) | Unix.Unix_error (Unix.EAGAIN, _, _) -> 0
          | Unix.Unix_error _ -> fail "MCP server closed its input" in
        send (offset + count)
      else send offset)
  in send 0
let take_frame c ~deadline ~cancelled =
  let rec frame scanned =
    let rec newline index =
      if index >= c.buffered then None
      else if Bytes.get c.buffer index = '\n' then Some index
      else newline (index+1) in
    match newline scanned with
    | Some stop ->
        if stop = 0 || stop > max_frame then fail "invalid MCP newline frame";
        let payload = Bytes.sub_string c.buffer 0 stop in
        let remaining = c.buffered - stop - 1 in
        Bytes.blit c.buffer (stop+1) c.buffer 0 remaining;
        c.buffered <- remaining;
        (try Yojson.Basic.from_string payload with _ -> fail "invalid MCP JSON frame")
    | None ->
        if c.buffered > max_frame then fail "MCP frame exceeds 1 MiB";
        let ready, _, _ = Unix.select [c.output] [] [] (remaining deadline cancelled) in
        if ready = [] then frame c.buffered else (
          if c.buffered = Bytes.length c.buffer then (
            let capacity = min (max_frame + 16_384) (2 * Bytes.length c.buffer) in
            if capacity <= c.buffered then fail "MCP frame buffer exceeds limit";
            let grown = Bytes.create capacity in
            Bytes.blit c.buffer 0 grown 0 c.buffered;
            c.buffer <- grown);
          let prior = c.buffered in
          let got = try Unix.read c.output c.buffer prior
              (Bytes.length c.buffer - prior) with
            | Unix.Unix_error (Unix.EINTR, _, _) | Unix.Unix_error (Unix.EAGAIN, _, _) -> -1
            | Unix.Unix_error _ -> fail "MCP server output failed" in
          if got = 0 then fail "MCP server exited or closed stdout";
          if got > 0 then c.buffered <- prior + got;
          frame prior)
  in frame 0
let obj label = function
  | `Assoc fields ->
      let names = List.map fst fields in
      if List.length names <> List.length (List.sort_uniq String.compare names)
      then fail ("duplicate " ^ label ^ " field"); fields
  | _ -> fail (label ^ " must be an object")
let get key fields = match List.assoc_opt key fields with
  | Some v -> v | None -> fail ("missing MCP " ^ key)
let string label = function
  | `String s when s <> "" && String.length s <= 4096 &&
      not (String.exists (fun ch -> Char.code ch < 32 || Char.code ch = 127) s) -> s
  | _ -> fail (label ^ " must be bounded single-line text")
let result c ~method_name ~params ~deadline ~cancelled =
  if c.closed then fail "MCP server connection is closed";
  if c.next_id = max_int then fail "MCP request IDs exhausted";
  let id = c.next_id in c.next_id <- id + 1;
  let message = Yojson.Basic.to_string (`Assoc ["jsonrpc", `String "2.0";
    "id", `Int id; "method", `String method_name; "params", params]) in
  if String.length message > max_frame then fail "MCP request exceeds 1 MiB";
  write_all c (message ^ "\n") ~deadline ~cancelled;
  let rec response notifications =
    if notifications > 32 then fail "too many unsolicited MCP messages";
    let fields = obj "MCP message" (take_frame c ~deadline ~cancelled) in
    if get "jsonrpc" fields <> `String "2.0" then fail "invalid MCP JSON-RPC version";
    match List.assoc_opt "id" fields with
    | None ->
        ignore (string "MCP notification method" (get "method" fields));
        if List.mem_assoc "result" fields || List.mem_assoc "error" fields then
          fail "invalid MCP notification";
        response (notifications+1)
    | Some (`Int response_id) when response_id = id ->
        if List.mem_assoc "method" fields then fail "invalid MCP response method";
        (match List.assoc_opt "result" fields, List.assoc_opt "error" fields with
         | Some value, None -> value
         | None, Some error ->
             let detail = obj "MCP error" error in
             (match get "code" detail with `Int _ -> () | _ -> fail "invalid MCP error code");
             ignore (string "MCP error message" (get "message" detail));
             fail "MCP server returned an error"
         | _ -> fail "MCP response requires exactly one result or error")
    | Some _ -> fail "unexpected or duplicate MCP response ID"
  in response 0
let initialize c ~deadline ~cancelled =
  let init = result c ~method_name:"initialize" ~params:(`Assoc [
    "protocolVersion", `String "2025-06-18";
    "capabilities", `Assoc [];
    "clientInfo", `Assoc ["name", `String "pave"; "version", `String "1"]])
      ~deadline ~cancelled in
  let fields = obj "MCP initialize result" init in
  if get "protocolVersion" fields <> `String "2025-06-18" then
    fail "unsupported MCP protocol version";
  ignore (obj "MCP capabilities" (get "capabilities" fields));
  let info = obj "MCP serverInfo" (get "serverInfo" fields) in
  ignore (string "MCP server name" (get "name" info));
  ignore (string "MCP server version" (get "version" info));
  let notice = "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}" in
  write_all c (notice ^ "\n") ~deadline ~cancelled
let connect_until session ~server ~deadline ~cancelled =
  with_request_lock session.lock ~deadline ~cancelled (fun () ->
    ensure session;
    let config = match Mcp_config.find session.snapshot server with
      | Some config -> config | None -> fail "MCP server is not enabled" in
    (match config.transport with
     | Mcp_config.Stdio _ -> ()
     | Mcp_config.Http _ -> fail "HTTP MCP server cannot use stdio");
    match List.assoc_opt server session.clients with
    | Some c when not c.closed -> c
    | _ ->
        check_cancel cancelled;
        approve session (Start config);
        check_cancel cancelled;
        ensure session;
        let c, status = spawn config in
        (try
           wait_ready status ~deadline ~cancelled;
           with_request_lock c.lock ~deadline ~cancelled
             (fun () -> initialize c ~deadline ~cancelled);
           session.clients <- (server, c) :: List.remove_assoc server session.clients;
           c
         with exn -> close_fd status; dispose_client c; raise exn))
let connect session ~server ~timeout_seconds ~cancelled =
  connect_until session ~server ~deadline:(timeout timeout_seconds) ~cancelled
let call session ~server ~method_name ~params ~timeout_seconds ~cancelled =
  let deadline = timeout timeout_seconds in
  let c = connect_until session ~server ~deadline ~cancelled in
  with_request_lock c.lock ~deadline ~cancelled (fun () ->
    try
      ensure session; check_cancel cancelled;
      result c ~method_name ~params ~deadline ~cancelled
    with exn -> dispose_client c; raise exn)
let source c value = {server = c.Mcp_config.name; source = c.source; value}
(* A transport-independent request boundary. HTTP owns its own authorization,
   framing, IDs and lifecycle; both transports consume the same checked data. *)
let listed_with ~source:config ~request ~method_name ~key ~validate
    ~timeout_seconds ~cancelled =
  let rec pages cursor seen page acc =
    if page >= 8 then fail "MCP listing exceeds eight pages";
    check_cancel cancelled;
    let params = match cursor with None -> `Assoc [] | Some cursor ->
      `Assoc ["cursor", `String cursor] in
    let response = request ~method_name ~params ~timeout_seconds ~cancelled in
    if String.length (Yojson.Basic.to_string response) > max_output then
      fail "MCP listing exceeds 64 KiB";
    let fields = obj "MCP listing" response in
    let rows = match get key fields with
      | `List items when List.length items <= max_items -> items
      | _ -> fail "invalid or oversized MCP listing" in
    let values = List.map (fun item -> validate item; source config item) rows in
    if List.length acc + List.length values > max_items then fail "MCP listing exceeds 128 items";
    match List.assoc_opt "nextCursor" fields with
    | None -> acc @ values
    | Some json ->
        let next = string "MCP nextCursor" json in
        if List.mem next seen then fail "repeated MCP listing cursor";
        pages (Some next) (next :: seen) (page+1) (acc @ values)
  in pages None [] 0 []
let listed session ~server ~method_name ~key ~validate ~timeout_seconds ~cancelled =
  let config = match Mcp_config.find session.snapshot server with
    | Some c -> c | None -> fail "MCP server is not enabled" in
  listed_with ~source:config
    ~request:(fun ~method_name ~params ~timeout_seconds ~cancelled ->
      call session ~server ~method_name ~params ~timeout_seconds ~cancelled)
    ~method_name ~key ~validate ~timeout_seconds ~cancelled
let rec validate_schema_node depth json =
  if depth > 12 then fail "MCP schema nesting exceeds 12";
  let fields = obj "MCP schema" json in
  List.iter (fun (key, _) ->
    if not (List.mem key ["type"; "properties"; "required"; "additionalProperties";
      "items"; "enum"; "maxLength"; "minimum"; "maximum"; "description";
      "title"; "default"]) then fail ("unsupported MCP schema keyword " ^ key)) fields;
  let kind = match get "type" fields with
    | `String ("object" | "array" | "string" | "integer" | "boolean" | "number" | "null" as kind) -> kind
    | _ -> fail "MCP schema type is invalid" in
  (match List.assoc_opt "description" fields with
   | None | Some (`String _) -> ()
   | _ -> fail "invalid MCP schema description");
  (match List.assoc_opt "title" fields with
   | None | Some (`String _) -> ()
   | _ -> fail "invalid MCP schema title");
  (match List.assoc_opt "enum" fields with
   | None -> ()
   | Some (`List values) when values <> [] && List.length values <= 128 -> ()
   | _ -> fail "invalid MCP schema enum");
  (match List.assoc_opt "maxLength" fields with
   | None -> ()
   | Some (`Int n) when kind = "string" && n >= 0 -> ()
   | _ -> fail "invalid MCP schema maxLength");
  List.iter (fun key -> match List.assoc_opt key fields with
    | None -> ()
    | Some (`Int _) when kind = "integer" || kind = "number" -> ()
    | Some (`Float n) when kind = "number" && Float.is_finite n -> ()
    | _ -> fail ("invalid MCP schema " ^ key)) ["minimum"; "maximum"];
  let numeric = function `Int n -> float_of_int n | `Float n -> n | _ -> 0. in
  (match List.assoc_opt "minimum" fields, List.assoc_opt "maximum" fields with
   | Some minimum, Some maximum when numeric minimum > numeric maximum ->
       fail "MCP schema minimum exceeds maximum"
   | _ -> ());
  let properties = match List.assoc_opt "properties" fields with
    | None -> []
    | Some json when kind = "object" -> obj "MCP schema properties" json
    | _ -> fail "MCP properties require an object schema" in
  if List.length properties > 128 then fail "MCP schema has too many properties";
  List.iter (fun (name, child) ->
    ignore (string "MCP schema property name" (`String name));
    validate_schema_node (depth+1) child) properties;
  (match List.assoc_opt "required" fields with
   | None -> ()
   | Some (`List keys) when kind = "object" && List.length keys <= 128 ->
       let names = List.map (string "MCP required property") keys in
       if List.length names <> List.length (List.sort_uniq String.compare names) ||
          List.exists (fun name -> not (List.mem_assoc name properties)) names
       then fail "invalid MCP required properties"
   | _ -> fail "invalid MCP required properties");
  (match List.assoc_opt "additionalProperties" fields with
   | None -> ()
   | Some (`Bool _) when kind = "object" -> ()
   | Some (`Assoc _ as child) when kind = "object" -> validate_schema_node (depth+1) child
   | _ -> fail "invalid MCP additionalProperties");
  (match List.assoc_opt "items" fields with
   | None when kind <> "array" -> ()
   | Some child when kind = "array" -> validate_schema_node (depth+1) child
   | _ -> fail "MCP array requires an item schema")
let validate_schema json =
  if String.length (Yojson.Basic.to_string json) > 32_768 then
    fail "MCP inputSchema exceeds 32 KiB";
  validate_schema_node 0 json;
  if get "type" (obj "MCP inputSchema" json) <> `String "object" then
    fail "MCP tool inputSchema must describe an object"
let string_length text =
  Uutf.String.fold_utf_8 (fun count _ -> function
    | `Uchar _ -> count + 1
    | `Malformed _ -> fail "MCP tool argument string must contain valid UTF-8") 0 text
let rec validate_arguments_node schema value =
  let fields = obj "MCP argument schema" schema in
  let kind = get "type" fields in
  let valid = match kind, value with
    | `String "object", `Assoc _ | `String "array", `List _
    | `String "string", `String _ | `String "integer", `Int _
    | `String "number", `Int _ | `String "boolean", `Bool _
    | `String "null", `Null -> true
    | `String "number", `Float number -> Float.is_finite number
    | _ -> false in
  if not valid then fail "MCP tool argument type mismatch";
  (match List.assoc_opt "enum" fields with
   | Some (`List allowed) when not (List.mem value allowed) ->
       fail "MCP tool argument not among allowed values"
   | _ -> ());
  (match value with
   | `Assoc values ->
       ignore (obj "MCP tool arguments" value);
       let properties = match List.assoc_opt "properties" fields with
         | Some json -> obj "MCP schema properties" json | None -> [] in
       (match List.assoc_opt "required" fields with
        | Some (`List names) ->
            List.iter (function `String name when List.mem_assoc name values -> ()
              | _ -> fail "missing MCP required argument") names
        | _ -> ());
       List.iter (fun (name, item) ->
         match List.assoc_opt name properties with
         | Some child -> validate_arguments_node child item
         | None -> (match List.assoc_opt "additionalProperties" fields with
           | Some (`Bool false) -> fail ("unexpected MCP argument " ^ name)
           | Some (`Assoc _ as schema) -> validate_arguments_node schema item
           | _ -> ())) values
   | `List values ->
       let schema = get "items" fields in
       List.iter (validate_arguments_node schema) values
   | `String text ->
       let length = string_length text in
       (match List.assoc_opt "maxLength" fields with
        | Some (`Int maximum) when length > maximum ->
            fail "MCP tool argument string exceeds limit"
        | _ -> ())
   | `Int number ->
       (match List.assoc_opt "minimum" fields with
        | Some (`Int minimum) when number < minimum -> fail "MCP tool argument below minimum"
        | Some (`Float minimum) when float_of_int number < minimum ->
            fail "MCP tool argument below minimum"
        | _ -> ());
       (match List.assoc_opt "maximum" fields with
        | Some (`Int maximum) when number > maximum -> fail "MCP tool argument above maximum"
        | Some (`Float maximum) when float_of_int number > maximum ->
            fail "MCP tool argument above maximum"
        | _ -> ())
   | `Float number ->
       (match List.assoc_opt "minimum" fields with
        | Some (`Int minimum) when number < float_of_int minimum ->
            fail "MCP tool argument below minimum"
        | Some (`Float minimum) when number < minimum ->
            fail "MCP tool argument below minimum"
        | _ -> ());
       (match List.assoc_opt "maximum" fields with
        | Some (`Int maximum) when number > float_of_int maximum ->
            fail "MCP tool argument above maximum"
        | Some (`Float maximum) when number > maximum ->
            fail "MCP tool argument above maximum"
        | _ -> ())
   | _ -> ())
let validate_arguments ~schema value =
  validate_schema schema;
  validate_arguments_node schema value
let validate_tool item =
  let fields = obj "MCP tool" item in
  ignore (Mcp_config.identifier "MCP tool name" (string "tool name" (get "name" fields)));
  validate_schema (get "inputSchema" fields)
let validate_uri value =
  let text = string "MCP resource URI" value in
  match String.index_opt text ':' with
  | Some n when n > 0 && n < String.length text - 1 &&
      (match text.[0] with 'A'..'Z' | 'a'..'z' -> true | _ -> false) &&
      String.for_all (function
        | 'A'..'Z' | 'a'..'z' | '0'..'9' | '+' | '.' | '-' -> true
        | _ -> false) (String.sub text 0 n) -> text
  | _ -> fail "MCP resource URI requires an absolute scheme"
let base64 text =
  let length = String.length text in
  if length = 0 || length mod 4 <> 0 then fail "invalid MCP base64 content";
  let padding = if text.[length-1] = '=' then
    if text.[length-2] = '=' then 2 else 1 else 0 in
  for i = 0 to length - padding - 1 do
    match text.[i] with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '+' | '/' -> ()
    | _ -> fail "invalid MCP base64 content"
  done;
  for i = length - padding to length - 1 do
    if text.[i] <> '=' then fail "invalid MCP base64 padding"
  done
let validate_resource item =
  let fields = obj "MCP resource" item in
  ignore (validate_uri (get "uri" fields));
  ignore (string "resource name" (get "name" fields))
let validate_prompt item =
  let fields = obj "MCP prompt" item in
  ignore (Mcp_config.identifier "MCP prompt name" (string "prompt name" (get "name" fields)));
  (match List.assoc_opt "arguments" fields with
   | None -> ()
   | Some (`List args) when List.length args <= 32 ->
       let names = List.map (fun arg ->
         let arg = obj "MCP prompt argument" arg in
         let name = string "prompt argument name" (get "name" arg) in
         (match List.assoc_opt "required" arg with
          | None | Some (`Bool _) -> ()
          | _ -> fail "invalid MCP prompt required flag");
         name) args in
       if List.length names <> List.length (List.sort_uniq String.compare names)
       then fail "duplicate MCP prompt argument"
   | _ -> fail "invalid MCP prompt arguments")
let distinct key rows =
  let names = List.map (fun row -> get key (obj "MCP listing item" row.value)) rows in
  if List.length names <> List.length (List.sort_uniq compare names) then
    fail ("duplicate MCP " ^ key ^ " in listing");
  rows
let list_tools_with ~source ~request ~timeout_seconds ~cancelled =
  distinct "name" (listed_with ~source ~request ~method_name:"tools/list" ~key:"tools"
    ~validate:validate_tool ~timeout_seconds ~cancelled)
let list_resources_with ~source ~request ~timeout_seconds ~cancelled =
  distinct "uri" (listed_with ~source ~request ~method_name:"resources/list" ~key:"resources"
    ~validate:validate_resource ~timeout_seconds ~cancelled)
let list_prompts_with ~source ~request ~timeout_seconds ~cancelled =
  distinct "name" (listed_with ~source ~request ~method_name:"prompts/list" ~key:"prompts"
    ~validate:validate_prompt ~timeout_seconds ~cancelled)
let list_tools session ~server ~timeout_seconds ~cancelled =
  distinct "name" (listed session ~server ~method_name:"tools/list" ~key:"tools"
    ~validate:validate_tool ~timeout_seconds ~cancelled)
let list_resources session ~server ~timeout_seconds ~cancelled =
  distinct "uri" (listed session ~server ~method_name:"resources/list" ~key:"resources"
    ~validate:validate_resource ~timeout_seconds ~cancelled)
let list_prompts session ~server ~timeout_seconds ~cancelled =
  distinct "name" (listed session ~server ~method_name:"prompts/list" ~key:"prompts"
    ~validate:validate_prompt ~timeout_seconds ~cancelled)
let validate_tool_result value =
  if String.length (Yojson.Basic.to_string value) > max_output then
    fail "MCP tool output exceeds 64 KiB";
  let fields = obj "MCP tool result" value in
  let content = match get "content" fields with
    | `List items when List.length items <= max_items -> items
    | _ -> fail "invalid MCP tool content" in
  (match List.assoc_opt "isError" fields with
   | None | Some (`Bool _) -> ()
   | _ -> fail "invalid MCP tool error flag");
  List.iter (fun item ->
    let fields = obj "MCP tool content block" item in
    match get "type" fields with
    | `String "text" -> (match get "text" fields with `String _ -> () | _ -> fail "invalid MCP text block")
    | `String "image" ->
        ignore (string "MCP image MIME" (get "mimeType" fields));
        (match get "data" fields with
         | `String encoded -> base64 encoded
         | _ -> fail "invalid MCP image block")
    | _ -> fail "unsupported MCP tool result block") content
let validate_resource_result ~uri value =
  if String.length (Yojson.Basic.to_string value) > max_output then
    fail "MCP resource exceeds 64 KiB";
  let fields = obj "MCP resource result" value in
  match get "contents" fields with
  | `List rows when rows <> [] && List.length rows <= 32 -> List.iter (fun item ->
      let f = obj "MCP resource content" item in
      if validate_uri (get "uri" f) <> uri then fail "MCP resource URI mismatch";
      match List.assoc_opt "text" f, List.assoc_opt "blob" f with
      | Some (`String _), None -> ()
      | None, Some (`String encoded) -> base64 encoded
      | _ -> fail "invalid MCP resource content") rows
  | _ -> fail "invalid MCP resource contents"
let validate_prompt_result value =
  if String.length (Yojson.Basic.to_string value) > max_output then
    fail "MCP prompt exceeds 64 KiB";
  let fields = obj "MCP prompt result" value in
  match get "messages" fields with
  | `List rows when List.length rows <= 32 -> List.iter (fun item ->
      let f = obj "MCP prompt message" item in
      (match get "role" f with `String "user" | `String "assistant" -> ()
       | _ -> fail "invalid MCP prompt role");
      let content = obj "MCP prompt content" (get "content" f) in
      match get "type" content with
      | `String "text" -> (match get "text" content with `String _ -> () | _ -> fail "invalid MCP prompt text")
      | _ -> fail "unsupported MCP prompt content") rows
  | _ -> fail "invalid MCP prompt messages"
let call_tool session ~server ~name ~arguments ~timeout_seconds ~cancelled =
  ignore (Mcp_config.identifier "MCP tool name" name);
  ignore (obj "MCP tool arguments" arguments);
  if String.length (Yojson.Basic.to_string arguments) > max_output then
    fail "MCP tool arguments exceed 64 KiB";
  let config = match Mcp_config.find session.snapshot server with
    | Some c -> c | None -> fail "MCP server is not enabled" in
  let available = list_tools session ~server ~timeout_seconds ~cancelled in
  let advertised = match List.find_opt (fun row ->
    get "name" (obj "MCP listed tool" row.value) = `String name) available with
    | Some row -> row.value | None -> fail "MCP tool was not advertised" in
  validate_arguments ~schema:(get "inputSchema" (obj "MCP listed tool" advertised))
    arguments;
  (* Approval is for this exact invocation; neither discovery nor permissive
     local tool mode authorizes a remote effect. *)
  check_cancel cancelled;
  approve session (Effect (config, name, arguments));
  check_cancel cancelled;
  let params = `Assoc ["name", `String name; "arguments", arguments] in
  let value = call session ~server ~method_name:"tools/call" ~params
      ~timeout_seconds ~cancelled in
  validate_tool_result value;
  source config value
let read_resource session ~server ~uri ~timeout_seconds ~cancelled =
  ignore (validate_uri (`String uri));
  let config = match Mcp_config.find session.snapshot server with
    | Some c -> c | None -> fail "MCP server is not enabled" in
  let value = call session ~server ~method_name:"resources/read"
    ~params:(`Assoc ["uri", `String uri]) ~timeout_seconds ~cancelled in
  validate_resource_result ~uri value;
  source config value
let get_prompt session ~server ~name ~arguments ~timeout_seconds ~cancelled =
  ignore (Mcp_config.identifier "MCP prompt name" name);
  let supplied = obj "MCP prompt arguments" arguments in
  if List.length supplied > 32 then fail "too many MCP prompt arguments";
  List.iter (fun (_, value) -> match value with
    | `String text when String.length text <= 4096 -> ()
    | _ -> fail "MCP prompt arguments must be bounded strings") supplied;
  let config = match Mcp_config.find session.snapshot server with
    | Some c -> c | None -> fail "MCP server is not enabled" in
  let prompts = list_prompts session ~server ~timeout_seconds ~cancelled in
  let advertised = match List.find_opt (fun row ->
    get "name" (obj "MCP listed prompt" row.value) = `String name) prompts with
    | Some row -> obj "MCP listed prompt" row.value
    | None -> fail "MCP prompt was not advertised" in
  let expected = match List.assoc_opt "arguments" advertised with
    | None -> []
    | Some (`List args) -> args
    | _ -> fail "invalid MCP prompt arguments" in
  List.iter (fun item ->
    let fields = obj "MCP listed prompt argument" item in
    let key = string "MCP prompt argument" (get "name" fields) in
    if List.assoc_opt "required" fields = Some (`Bool true) &&
       not (List.mem_assoc key supplied) then fail "missing MCP prompt argument") expected;
  List.iter (fun (key, _) ->
    if not (List.exists (fun item ->
      get "name" (obj "MCP listed prompt argument" item) = `String key) expected)
    then fail "unexpected MCP prompt argument") supplied;
  let value = call session ~server ~method_name:"prompts/get" ~params:(`Assoc [
    "name", `String name; "arguments", arguments]) ~timeout_seconds ~cancelled in
  validate_prompt_result value;
  source config value
