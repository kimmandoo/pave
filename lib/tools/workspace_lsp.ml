exception Error of string

let fail message = raise (Error message)

let max_frame_bytes = 1_048_576
let max_header_bytes = 8_192
let max_message_bytes = 65_536
let max_documents = 64
let max_edit_files = 32
let max_diagnostics = 1_000

let max_pending_edit_previews = 32
let max_pending_edit_preview_bytes = 4 * max_frame_bytes

(* [read] must return promptly when [close] is called. The launcher is injected
   so protocol and lifecycle tests need no language-server executable. *)
type io = {
  read : bytes -> int -> int -> int;
  write : string -> unit;
  close : unit -> unit;
  terminate : unit -> unit;
}
type launcher =
  program:string -> arguments:string list -> cwd:string -> environment:string array -> io

type document = { uri : string; language_id : string; mutable version : int;
                  mutable text : string; mutable sha256 : string;
                  mutable diagnostics : Yojson.Basic.t list }
type identity = { owner : string; root : string; program : string; arguments : string list }
type pending = { condition : Condition.t; mutable response : Yojson.Basic.t option }
type edit_preview = {
  preview_id : string;
  owner : string;
  root : string;
  program : string;
  arguments : string list;
  title : string;
  files : Yojson.Basic.t;
  bytes : int;
}
type manager = {
  launch : launcher;
  lock : Mutex.t;
  request_lock : Mutex.t;
  write_lock : Mutex.t;
  pending : (int, pending) Hashtbl.t;
  mutable next_id : int;
  mutable io : io option;
  documents : (string, document) Hashtbl.t;
  mutable reader : Thread.t option;
  mutable identity : identity option;
  mutable capabilities : Yojson.Basic.t;
  mutable closed : bool;
  mutable documents_closed : bool;
  mutable failure : string option;
  mutable shutdown_sent : bool;
  edit_previews : (string, edit_preview) Hashtbl.t;
  mutable next_preview_id : int;
  mutable preview_bytes : int;
}

let rec create_manager ?(launcher = default_launcher) () =
  { launch = launcher; lock = Mutex.create (); request_lock = Mutex.create ();
    write_lock = Mutex.create (); pending = Hashtbl.create 16;
    next_id = 0; io = None; reader = None;
    identity = None; capabilities = `Null; documents = Hashtbl.create 16;
    closed = false; documents_closed = false;
    failure = None; shutdown_sent = false;
    edit_previews = Hashtbl.create 16; next_preview_id = 0;
    preview_bytes = 0 }

and safe_server_environment () =
  [|"PATH=/usr/bin:/bin:/usr/sbin:/sbin"; "LANG=C"; "LC_ALL=C"; "TMPDIR=/tmp"|]

and default_launcher ~program ~arguments ~cwd ~environment =
  if program = "" || String.contains program '\000' then fail "invalid LSP server program";
  List.iter (fun argument ->
    if String.contains argument '\000' || String.length argument > 4_096 then
      fail "invalid LSP server argument") arguments;
  (* The fixed shell wrapper only changes directory and execs the exact argv.
     Arguments and the path remain positional parameters, never shell source. *)
  let child_in, parent_in = Unix.pipe () and parent_out, child_out = Unix.pipe () in
  List.iter Unix.set_close_on_exec [child_in; parent_in; parent_out; child_out];
  let argv = Array.of_list
      (["/bin/sh"; "-c"; "cd \"$1\" || exit 127; shift; exec \"$@\"";
       "pave-lsp"; cwd; program] @ arguments) in
  let pid = Unix.create_process_env "/bin/sh" argv environment
      child_in child_out Unix.stderr in
  Unix.close child_in; Unix.close child_out;
  let closed = ref false and close_lock = Mutex.create () in
  let close () =
    Mutex.lock close_lock;
    if not !closed then (
      closed := true;
      (try Unix.close parent_in with _ -> ());
      (try Unix.close parent_out with _ -> ());
      Mutex.unlock close_lock)
    else Mutex.unlock close_lock
  in
  let terminate () =
    (try Unix.kill pid Sys.sigterm with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
    let rec reap remaining =
      if remaining = 0 then (
        (try Unix.kill pid Sys.sigkill with _ -> ());
        (try ignore (Unix.waitpid [] pid) with _ -> ()))
      else
        try
          match Unix.waitpid [Unix.WNOHANG] pid with
          | 0, _ -> Thread.delay 0.01; reap (remaining - 1)
          | _, Unix.WEXITED _ | _, Unix.WSIGNALED _ | _, Unix.WSTOPPED _ -> ()
        with Unix.Unix_error (Unix.ECHILD, _, _) -> ()
    in
    reap 100
  in
  { read = (fun bytes offset length -> Unix.read parent_out bytes offset length);
    write = (fun text ->
      let bytes = Bytes.unsafe_of_string text in
      let rec loop offset =
        if offset < Bytes.length bytes then
          try
            let count = Unix.write parent_in bytes offset (Bytes.length bytes - offset) in
            if count = 0 then fail "LSP server closed its input";
            loop (offset + count)
          with Unix.Unix_error (Unix.EINTR, _, _) -> loop offset
      in loop 0);
    close; terminate }

let with_lock lock fn =
  Mutex.lock lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock lock) fn

let json = Yojson.Basic.to_string
let assoc = function `Assoc fields -> fields | _ -> fail "expected a JSON object"
let member name value = try List.assoc name (assoc value) with Not_found -> `Null
let required_string name value = match member name value with
  | `String text when text <> "" && not (String.contains text '\000') -> text
  | _ -> fail (name ^ " must be a nonempty string")
let int_value name = function `Int n when n >= 0 -> n | _ -> fail (name ^ " must be a nonnegative integer")

let frame payload =
  let length = String.length payload in
  if length > max_frame_bytes then fail "LSP message exceeds the frame limit";
  Printf.sprintf "Content-Length: %d\r\n\r\n%s" length payload

let parse_header header =
  let lines = String.split_on_char '\n' header in
  let lengths = ref [] in
  List.iter (fun line ->
    let line = if String.ends_with ~suffix:"\r" line then String.sub line 0 (String.length line - 1) else line in
    match String.index_opt line ':' with
    | None -> if String.trim line <> "" then fail "malformed LSP frame header"
    | Some colon ->
        let name = String.lowercase_ascii (String.trim (String.sub line 0 colon)) in
        let value = String.trim (String.sub line (colon + 1) (String.length line - colon - 1)) in
        if name = "content-length" then (
          if value = "" || not (String.for_all (function '0'..'9' -> true | _ -> false) value) then
            fail "invalid LSP Content-Length";
          let length = try int_of_string value with _ -> fail "invalid LSP Content-Length" in
          lengths := length :: !lengths)) lines;
  match !lengths with
  | [length] when length <= max_frame_bytes -> length
  | [ _ ] -> fail "LSP message exceeds the frame limit"
  | _ -> fail "LSP frame must contain exactly one Content-Length"

let file_uri path =
  let safe = function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '/' | '-' | '_' | '.' | '~' | ':' -> true
    | _ -> false in
  let buffer = Buffer.create (String.length path + 16) in
  Buffer.add_string buffer "file://";
  String.iter (fun ch ->
    if safe ch then Buffer.add_char buffer ch
    else Buffer.add_string buffer (Printf.sprintf "%%%02X" (Char.code ch))) path;
  Buffer.contents buffer

let decode_uri uri =
  if not (String.starts_with ~prefix:"file://" uri) ||
     String.contains uri '?' || String.contains uri '#' then
    fail "LSP returned a non-file or unsupported URI";
  let encoded = String.sub uri 7 (String.length uri - 7) in
  if not (String.starts_with ~prefix:"/" encoded) then fail "invalid file URI";
  let buffer = Buffer.create (String.length encoded) in
  let rec loop index =
    if index < String.length encoded then
      if encoded.[index] = '%' then (
        if index + 2 >= String.length encoded then fail "invalid percent escape in file URI";
        let hex = String.sub encoded (index + 1) 2 in
        let byte = try int_of_string ("0x" ^ hex) with _ -> fail "invalid percent escape in file URI" in
        if byte = 0 then fail "NUL byte in file URI";
        Buffer.add_char buffer (Char.chr byte); loop (index + 3))
      else (Buffer.add_char buffer encoded.[index]; loop (index + 1))
  in
  loop 0; Buffer.contents buffer

let relative_file root uri =
  let path = decode_uri uri in
  if not (String.starts_with ~prefix:"/" path) then fail "LSP file URI is not absolute";
  let root_prefix = if root = "/" then "/" else root ^ "/" in
  if not (String.starts_with ~prefix:root_prefix path) then fail "LSP URI escapes the workspace";
  let relative = String.sub path (String.length root_prefix) (String.length path - String.length root_prefix) in
  ignore (try Workspace_path.regular_path root relative with Workspace_path.Error message -> fail message);
  relative

(* Strict UTF-8 decoding is needed to translate LSP UTF-16 positions without
   ever splitting a code point or surrogate pair. *)
let utf8_codepoint text index =
  let length = String.length text in
  let byte i = Char.code text.[i] in
  let continuation b = b land 0xc0 = 0x80 in
  if index >= length then fail "invalid UTF-8 position";
  let first = byte index in
  if first < 0x80 then first, index + 1
  else if first >= 0xc2 && first <= 0xdf && index + 1 < length && continuation (byte (index+1)) then
    ((first land 0x1f) lsl 6 lor (byte (index+1) land 0x3f)), index + 2
  else if first >= 0xe0 && first <= 0xef && index + 2 < length && continuation (byte (index+1)) && continuation (byte (index+2)) then
    let cp = ((first land 0xf) lsl 12) lor ((byte (index+1) land 0x3f) lsl 6) lor (byte (index+2) land 0x3f) in
    if cp < 0x800 || (cp >= 0xd800 && cp <= 0xdfff) then fail "invalid UTF-8 document";
    cp, index + 3
  else if first >= 0xf0 && first <= 0xf4 && index + 3 < length && continuation (byte (index+1)) && continuation (byte (index+2)) && continuation (byte (index+3)) then
    let cp = ((first land 7) lsl 18) lor ((byte (index+1) land 0x3f) lsl 12) lor ((byte (index+2) land 0x3f) lsl 6) lor (byte (index+3) land 0x3f) in
    if cp < 0x10000 || cp > 0x10ffff then fail "invalid UTF-8 document";
    cp, index + 4
  else fail "invalid UTF-8 document"

let validate_utf8 text =
  let rec loop index = if index < String.length text then let _, next = utf8_codepoint text index in loop next in
  loop 0

let position_offset text line character =
  if line < 0 || character < 0 then fail "LSP positions must be nonnegative";
  validate_utf8 text;
  let line_start = ref 0 and current_line = ref 0 in
  while !current_line < line do
    match String.index_from_opt text !line_start '\n' with
    | None -> fail "LSP line is outside the document"
    | Some newline -> line_start := newline + 1; incr current_line
  done;
  let line_end = match String.index_from_opt text !line_start '\n' with
    | None -> String.length text | Some newline -> if newline > !line_start && text.[newline-1] = '\r' then newline-1 else newline in
  let units = ref 0 and cursor = ref !line_start in
  while !cursor < line_end && !units < character do
    let cp, next = utf8_codepoint text !cursor in
    let width = if cp > 0xffff then 2 else 1 in
    if !units + width > character then fail "LSP position splits a UTF-16 surrogate pair";
    units := !units + width; cursor := next
  done;
  if !units <> character then fail "LSP character is outside the line";
  !cursor

let range_offsets text range =
  let start = member "start" range and finish = member "end" range in
  let position which value =
    let line = int_value (which ^ ".line") (member "line" value) in
    let character = int_value (which ^ ".character") (member "character" value) in
    position_offset text line character in
  let first = position "start" start and last = position "end" finish in
  if last < first then fail "LSP range ends before it starts";
  first, last

let rec validate_json depth value =
  if depth > 128 then fail "LSP JSON nesting exceeds the limit";
  match value with
  | `Assoc fields ->
      let names = Hashtbl.create (List.length fields) in
      List.iter (fun (name, value) ->
        validate_utf8 name;
        if Hashtbl.mem names name then fail "duplicate key in LSP JSON object";
        Hashtbl.add names name ();
        validate_json (depth + 1) value) fields
  | `List items -> List.iter (validate_json (depth + 1)) items
  | `String text -> validate_utf8 text
  | _ -> ()

let decode_message text =
  if String.length text > max_frame_bytes then fail "LSP message exceeds the frame limit";
  try
    let value = Yojson.Basic.from_string text in
    validate_json 0 value;
    value
  with
  | Yojson.Json_error _ -> fail "malformed LSP JSON message"
  | Stack_overflow -> fail "LSP JSON nesting exceeds the limit"

let send_raw manager text =
  let io = match manager.io with Some io -> io | None -> fail "LSP server is not running" in
  with_lock manager.write_lock (fun () -> io.write (frame text))

let send_json manager message = send_raw manager (json message)

let send_server_error manager id code message =
  send_json manager (`Assoc ["jsonrpc", `String "2.0"; "id", id;
    "error", `Assoc ["code", `Int code; "message", `String message]])

let notification manager method_ params =
  send_json manager (`Assoc ["jsonrpc", `String "2.0"; "method", `String method_; "params", params])

let fail_pending manager message =
  with_lock manager.lock (fun () ->
    manager.failure <- Some message;
    Hashtbl.iter (fun _ pending -> pending.response <- Some (`Assoc ["__failure", `String message]); Condition.broadcast pending.condition) manager.pending)

let document_for_uri manager uri =
  with_lock manager.lock (fun () ->
    match Hashtbl.find_opt manager.documents uri with
    | Some document -> document
    | None -> fail "LSP response references a document that was not opened")

let validate_diagnostics text = function
  | `List diagnostics when List.length diagnostics <= max_diagnostics ->
      List.iter (fun diagnostic ->
        ignore (assoc diagnostic);
        ignore (range_offsets text (member "range" diagnostic));
        let message = match member "message" diagnostic with
          | `String message when String.length message <= 16_384 -> message
          | _ -> fail "invalid LSP diagnostic message" in
        ignore message) diagnostics;
      diagnostics
  | _ -> fail "invalid LSP diagnostics list"
let dispatch manager message =
  let fields = assoc message in
  if member "jsonrpc" message <> `String "2.0" then fail "invalid LSP JSON-RPC version";
  match List.assoc_opt "method" fields with
  | Some (`String method_) ->
      let has_id = List.mem_assoc "id" fields in
      let id = member "id" message in
      if has_id && id = `Null then fail "LSP request id cannot be null";
      if has_id then send_server_error manager id (-32601) "server requests are not permitted";
      if not has_id && method_ = "textDocument/publishDiagnostics" then (
        let params = member "params" message in
        let uri = required_string "uri" params in
        let document = document_for_uri manager uri in
        let version = member "version" params in
        let expected_version = match version with
          | `Int n when n >= 0 -> Some n
          | `Null -> None
          | _ -> fail "invalid LSP diagnostic document version" in
        let text, opened_version = with_lock manager.lock (fun () ->
          document.text, document.version) in
        let diagnostics = validate_diagnostics text (member "diagnostics" params) in
        let current = match expected_version with
          | Some version -> version = opened_version
          | None -> true in
        if current then
          with_lock manager.lock (fun () ->
            if document.version = opened_version then
              document.diagnostics <- diagnostics))
  | Some _ -> fail "LSP method must be a string"
  | None ->
      if List.mem_assoc "method" fields then fail "invalid LSP method";
      let id = member "id" message in
      (match id with
       | `Int id when id > 0 ->
           let has_result = List.mem_assoc "result" fields
           and has_error = List.mem_assoc "error" fields in
           if has_result = has_error then fail "LSP response must contain exactly one result or error";
           if has_error then (
             let error = member "error" message in
             let fields = assoc error in
             (match List.assoc_opt "code" fields with Some (`Int _) -> () | _ -> fail "invalid LSP error code");
             ignore (required_string "message" error));
           with_lock manager.lock (fun () -> match Hashtbl.find_opt manager.pending id with
             | None -> ()
             | Some pending -> pending.response <- Some message; Condition.broadcast pending.condition)
       | _ -> fail "LSP response has an invalid request id")

type frame_parser = {
  mutable data : bytes;
  mutable length : int;
  mutable header_scan : int;
  mutable body_start : int;
  mutable content_length : int option;
}

let create_frame_parser () =
  { data = Bytes.create 4_096; length = 0; header_scan = 0;
    body_start = 0; content_length = None }

let parse_frames parser input count =
  let maximum_buffer = max_frame_bytes + max_header_bytes + 4 + 4_096 in
  let required = parser.length + count in
  if required > maximum_buffer then fail "LSP input buffer exceeds its limit";
  if required > Bytes.length parser.data then (
    let capacity = ref (Bytes.length parser.data) in
    while !capacity < required do capacity := min maximum_buffer (!capacity * 2) done;
    let expanded = Bytes.create !capacity in
    Bytes.blit parser.data 0 expanded 0 parser.length;
    parser.data <- expanded);
  Bytes.blit input 0 parser.data parser.length count;
  parser.length <- required;
  let rec extract reversed =
    match parser.content_length with
    | None ->
        let found = ref None and index = ref parser.header_scan in
        while !found = None && !index + 3 < parser.length do
          if Bytes.get parser.data !index = '\r' &&
             Bytes.get parser.data (!index + 1) = '\n' &&
             Bytes.get parser.data (!index + 2) = '\r' &&
             Bytes.get parser.data (!index + 3) = '\n' then
            found := Some !index
          else incr index
        done;
        (match !found with
         | None ->
             if parser.length > max_header_bytes + 3 then fail "LSP frame header exceeds limit";
             parser.header_scan <- max 0 (parser.length - 3);
             List.rev reversed
         | Some header_end ->
             if header_end > max_header_bytes then fail "LSP frame header exceeds limit";
             parser.content_length <- Some
               (parse_header (Bytes.sub_string parser.data 0 header_end));
             parser.body_start <- header_end + 4;
             extract reversed)
    | Some expected ->
        if parser.length - parser.body_start < expected then List.rev reversed
        else (
          let body = Bytes.sub_string parser.data parser.body_start expected in
          let consumed = parser.body_start + expected in
          let remaining = parser.length - consumed in
          Bytes.blit parser.data consumed parser.data 0 remaining;
          parser.length <- remaining;
          parser.header_scan <- 0;
          parser.body_start <- 0;
          parser.content_length <- None;
          extract (body :: reversed))
  in
  extract []

let reader_loop manager io =
  let parser = create_frame_parser () and bytes = Bytes.create 4_096 in
  try
    let rec loop () =
      let rec read () =
        try io.read bytes 0 (Bytes.length bytes)
        with Unix.Unix_error (Unix.EINTR, _, _) -> read () in
      let count = read () in
      if count = 0 then fail "LSP server closed its output";
      if count < 0 || count > Bytes.length bytes then fail "invalid LSP transport read";
      List.iter (fun frame -> dispatch manager (decode_message frame))
        (parse_frames parser bytes count);
      loop ()
    in loop ()
  with
  | Error message -> fail_pending manager message
  | _ -> fail_pending manager "LSP transport failed"

let next_id manager = with_lock manager.lock (fun () ->
  manager.next_id <- manager.next_id + 1;
  manager.next_id)

let request ?(timeout_seconds = 30.) ?(cancel = fun () -> false) manager method_ params =
  let id = next_id manager in
  let pending = { condition = Condition.create (); response = None } in
  with_lock manager.lock (fun () -> Hashtbl.add manager.pending id pending);
  (try send_json manager (`Assoc ["jsonrpc", `String "2.0"; "id", `Int id;
      "method", `String method_; "params", params])
   with exn -> with_lock manager.lock (fun () -> Hashtbl.remove manager.pending id); raise exn);
  let started = Unix.gettimeofday () in
  let rec await () =
    let response = with_lock manager.lock (fun () -> pending.response) in
    match response with
    | Some (`Assoc [("__failure", `String message)]) -> fail message
    | Some response -> response
    | None when cancel () ->
        notification manager "$/cancelRequest" (`Assoc ["id", `Int id]);
        with_lock manager.lock (fun () -> Hashtbl.remove manager.pending id);
        fail "LSP request cancelled"
    | None when Unix.gettimeofday () -. started > timeout_seconds ->
        notification manager "$/cancelRequest" (`Assoc ["id", `Int id]);
        with_lock manager.lock (fun () -> Hashtbl.remove manager.pending id);
        fail "LSP request timed out"
    | None -> Thread.delay 0.01; await ()
  in
  Fun.protect ~finally:(fun () -> with_lock manager.lock (fun () -> Hashtbl.remove manager.pending id)) await

let response_result response =
  match member "error" response with
  | `Assoc fields ->
      let message = match List.assoc_opt "message" fields with Some (`String text) -> text | _ -> "server error" in
      fail ("LSP request failed: " ^ message)
  | `Null -> member "result" response
  | _ -> fail "malformed LSP error response"

let capability_enabled capabilities name =
  let options =
    match member name capabilities with
    | `Bool enabled -> Some enabled
    | `Assoc fields ->
        let boolean_option key value =
          match value with
          | `Bool _ -> ()
          | _ -> fail ("malformed LSP capability option: " ^ key) in
        List.iter (fun (key, value) ->
          match name, key with
          | _, "workDoneProgress" -> boolean_option key value
          | "renameProvider", "prepareProvider"
          | ("renameProvider" | "codeActionProvider"), "resolveProvider" ->
              boolean_option key value
          | "codeActionProvider", "codeActionKinds" ->
              (match value with `List kinds when List.for_all (function `String _ -> true | _ -> false) kinds -> ()
               | _ -> fail "malformed LSP code action kinds")
          | _ -> fail ("unsupported LSP capability option: " ^ key)) fields;
        Some true
    | `Null -> None
    | _ -> fail ("malformed LSP server capability: " ^ name)
  in
  match options with Some enabled -> enabled | None -> false

let verify_capabilities result =
  let capabilities = member "capabilities" result in
  ignore (assoc capabilities);
  List.iter (fun name -> ignore (capability_enabled capabilities name))
    ["definitionProvider"; "referencesProvider"; "hoverProvider";
     "renameProvider"; "codeActionProvider"];
  (match List.assoc_opt "positionEncoding" (assoc capabilities) with
   | None | Some (`String "utf-16") -> ()
   | Some (`String _) -> fail "unsupported LSP server position encoding"
   | Some _ -> fail "malformed LSP server position encoding");
  capabilities
let validate_identity owner program arguments =
  if owner = "" || String.length owner > 256 || String.contains owner '\000' then
    fail "invalid LSP session owner";
  if program = "" || String.length program > 4_096 || String.contains program '\000' then
    fail "invalid LSP server program";
  if List.length arguments > 128 then fail "too many LSP server arguments";
  ignore (List.fold_left (fun total argument ->
    if String.contains argument '\000' || String.length argument > 4_096 then
      fail "invalid LSP server argument";
    if String.length argument > max_message_bytes - total then
      fail "LSP server arguments exceed the input limit";
    total + String.length argument) 0 arguments)

let ensure_identity manager owner root program arguments =
  let canonical_root =
    try Workspace_path.root_path root with Workspace_path.Error message -> fail message in
  let wanted = { owner; root = canonical_root; program; arguments } in
  let state = with_lock manager.lock (fun () ->
    manager.identity, manager.failure, manager.shutdown_sent, manager.closed) in
  match state with
  | Some identity, failure, shutdown_sent, closed
    when identity.owner = wanted.owner && identity.root = wanted.root &&
         identity.program = wanted.program && identity.arguments = wanted.arguments ->
      if closed then fail "LSP manager is closed";
      (match failure, shutdown_sent with
       | Some message, _ -> fail message
       | None, true -> fail "LSP manager has already shut down"
       | None, false -> ())
  | Some _, _, _, _ ->
      fail "LSP manager cannot be reused with a different owner, workspace, or server configuration"
  | None, _, _, true -> fail "LSP manager is closed"
  | None, _, _, false ->
      validate_identity owner program arguments;
      let io = manager.launch ~program ~arguments ~cwd:canonical_root
          ~environment:(safe_server_environment ()) in
      manager.io <- Some io;
      manager.reader <- Some (Thread.create (fun () -> reader_loop manager io) ());
      (try
         let result = request manager "initialize" (`Assoc [
           "processId", `Int (Unix.getpid ());
           "rootUri", `String (file_uri canonical_root);
           "capabilities", `Assoc ["workspace", `Assoc ["applyEdit", `Bool false;
             "workspaceEdit", `Assoc ["documentChanges", `Bool true]];
             "textDocument", `Assoc ["definition", `Assoc ["dynamicRegistration", `Bool false];
               "references", `Assoc ["dynamicRegistration", `Bool false];
               "hover", `Assoc ["dynamicRegistration", `Bool false];
               "rename", `Assoc ["dynamicRegistration", `Bool false];
               "codeAction", `Assoc ["dynamicRegistration", `Bool false]]]
         ]) in
         let initialized = response_result result in
         let capabilities = verify_capabilities initialized in
         notification manager "initialized" (`Assoc []);
         with_lock manager.lock (fun () ->
           manager.capabilities <- capabilities;
           manager.identity <- Some wanted)
       with exn ->
         (try io.close () with _ -> ());
         (try io.terminate () with _ -> ());
         with_lock manager.lock (fun () -> manager.closed <- true);
         manager.io <- None;
         (match manager.reader with Some thread ->
           if Thread.id thread <> Thread.id (Thread.self ()) then (try Thread.join thread with _ -> ())
           | None -> ());
         manager.reader <- None;
         raise exn)

let start manager ~owner ~root ~program ~args ~execution_approved =
  if not execution_approved then fail "LSP server execution requires explicit approval";
  with_lock manager.request_lock (fun () ->
    if manager.closed then fail "LSP manager is closed";
    ensure_identity manager owner root program args)

let require_identity manager owner root program arguments =
  let canonical_root =
    try Workspace_path.root_path root with Workspace_path.Error message -> fail message in
  let state = with_lock manager.lock (fun () ->
    manager.identity, manager.failure, manager.shutdown_sent, manager.closed) in
  match state with
  | Some identity, failure, shutdown_sent, closed ->
      if identity.owner <> owner || identity.root <> canonical_root ||
         identity.program <> program || identity.arguments <> arguments then
        fail "LSP manager cannot be reused with a different owner, workspace, or server configuration";
      if closed then fail "LSP manager is closed";
      (match failure, shutdown_sent with
       | Some message, _ -> fail message
       | None, true -> fail "LSP manager has already shut down"
       | None, false -> identity)
  | None, _, _, _ -> fail "LSP server has not been started"

let current_document manager root relative language_id =
  let absolute = try Workspace_path.regular_path root relative with Workspace_path.Error message -> fail message in
  let snapshot = try Workspace_edit.read_snapshot ~root ~path:relative with Workspace_edit.Error message -> fail message in
  let uri = file_uri absolute in
  validate_utf8 snapshot.contents;
  with_lock manager.lock (fun () ->
    match Hashtbl.find_opt manager.documents uri with
    | Some document ->
        if document.language_id <> language_id then fail "LSP document language cannot change during a session";
        if document.text <> snapshot.contents then (
          if document.version = max_int then fail "LSP document version limit reached";
          document.version <- document.version + 1;
          document.text <- snapshot.contents; document.sha256 <- snapshot.sha256;
          notification manager "textDocument/didChange" (`Assoc [
            "textDocument", `Assoc ["uri", `String uri; "version", `Int document.version];
            "contentChanges", `List [`Assoc ["text", `String snapshot.contents]]]));
        document
    | None ->
        if Hashtbl.length manager.documents >= max_documents then fail "LSP document limit reached";
        let document = { uri; language_id; version = 1; text = snapshot.contents;
                         sha256 = snapshot.sha256; diagnostics = [] } in
        Hashtbl.add manager.documents uri document;
        notification manager "textDocument/didOpen" (`Assoc [
          "textDocument", `Assoc ["uri", `String uri; "languageId", `String language_id;
            "version", `Int document.version; "text", `String document.text]]);
        document)
let did_close_documents manager =
  let uris = with_lock manager.lock (fun () ->
    if manager.documents_closed then []
    else (
      manager.documents_closed <- true;
      Hashtbl.fold (fun _ document uris -> document.uri :: uris)
        manager.documents [])) in
  List.iter (fun uri ->
    try notification manager "textDocument/didClose"
      (`Assoc ["textDocument", `Assoc ["uri", `String uri]])
    with _ -> ()) uris

let read_position arguments =
  let position = member "position" arguments in
  let line = int_value "position.line" (member "line" position) in
  let character = int_value "position.character" (member "character" position) in
  `Assoc ["line", `Int line; "character", `Int character]

let check_provider manager name =
  if not (capability_enabled manager.capabilities name) then fail ("LSP server does not support " ^ name)

let text_document_params document position = `Assoc [
  "textDocument", `Assoc ["uri", `String document.uri]; "position", position]

let snapshot_for_uri root uri =
  let relative = relative_file root uri in
  relative, (try Workspace_edit.read_snapshot ~root ~path:relative with Workspace_edit.Error message -> fail message)

let text_edits_preview manager ~root uri edits version =
  let relative, snapshot = snapshot_for_uri root uri in
  let document = document_for_uri manager uri in
  (match version with
   | Some (`Int value) when value = document.version -> ()
   | Some (`Int _) -> fail "LSP workspace edit has a stale document version"
   | None -> ()
   | Some _ -> fail "invalid LSP workspace edit version");
  if snapshot.sha256 <> document.sha256 then fail "workspace file changed since the LSP document version";
  if List.length edits = 0 then `Null else (
    if List.length edits > 2_000 then fail "LSP workspace edit contains too many edits";
    let ranges = List.map (fun edit ->
      let first, last = range_offsets snapshot.contents (member "range" edit) in
      let new_text = match member "newText" edit with
        | `String text when not (String.contains text '\000') -> text
        | _ -> fail "invalid LSP replacement text" in
      first, last, new_text) edits
      |> List.sort (fun (first, _, _) (next, _, _) -> compare first next) in
    let output = Buffer.create (String.length snapshot.contents) in
    let cursor = ref 0 and previous_range = ref None in
    List.iter (fun (first, last, replacement) ->
      (match !previous_range with
       | Some (previous_start, previous_end)
         when first < !cursor ||
              (first = previous_start && previous_start = previous_end) ->
           fail "LSP text edits overlap"
       | _ -> ());
      Buffer.add_substring output snapshot.contents !cursor (first - !cursor);
      Buffer.add_string output replacement;
      cursor := last;
      previous_range := Some (first, last)) ranges;
    Buffer.add_substring output snapshot.contents !cursor
      (String.length snapshot.contents - !cursor);
    let result = Buffer.contents output in
    if String.length result > Workspace_edit.max_file_bytes then
      fail "LSP edited file exceeds the workspace write limit";
    let changed = result <> snapshot.contents in
    validate_utf8 result;
    let preview = {
      Workspace_edit.path = relative;
      original_sha256 = snapshot.sha256;
      result_sha256 = Workspace_edit.sha256 result;
      content = result;
      changed;
    } in
    `Assoc ["path", `String relative; "version", `Int document.version;
      "original_sha256", `String preview.original_sha256;
      "result_sha256", `String preview.result_sha256;
      "content", `String preview.content; "changed", `Bool preview.changed])

let workspace_edit manager ~root edit =
  match edit with
  | `Null -> `List []
  | `Assoc _ ->
      let changes = member "changes" edit and document_changes = member "documentChanges" edit in
      if changes <> `Null && document_changes <> `Null then
        fail "LSP workspace edit has conflicting changes forms";
      let entries = match document_changes, changes with
        | `List items, _ ->
            if List.length items > max_edit_files then fail "LSP workspace edit affects too many files";
            List.map (fun item ->
              if member "kind" item <> `Null then fail "LSP resource operations are not supported";
              let text_document = member "textDocument" item in
              let uri = required_string "uri" text_document in
              let version = Some (member "version" text_document) in
              uri, version,
              (match member "edits" item with `List edits -> edits | _ -> fail "invalid LSP text edits"))
              items
        | _, `Assoc fields ->
            if List.length fields > max_edit_files then fail "LSP workspace edit affects too many files";
            List.map (fun (uri, edits) -> uri, None,
              (match edits with `List edits -> edits | _ -> fail "invalid LSP changes list")) fields
        | `Null, `Null -> []
        | _ -> fail "invalid LSP workspace edit" in
      let previews = List.map (fun (uri, version, edits) ->
        text_edits_preview manager ~root uri edits version) entries in
      let output = `List (List.filter (( <> ) `Null) previews) in
      if String.length (json output) > max_frame_bytes then
        fail "LSP workspace edit preview exceeds the output limit";
      output
  | _ -> fail "invalid LSP workspace edit"


let preview_file_rows = function
  | `List rows when List.for_all (function `Assoc _ -> true | _ -> false) rows -> rows
  | _ -> fail "invalid pending LSP preview"

let preview_paths files =
  preview_file_rows files
  |> List.map (fun row -> required_string "path" row)

let preview_has_changes files =
  preview_file_rows files
  |> List.exists (fun row -> member "changed" row = `Bool true)

let store_edit_preview manager ~owner ~root ~program ~arguments ~title files =
  let root = try Workspace_path.root_path root
    with Workspace_path.Error message -> fail message in
  let bytes = String.length (json files) in
  if bytes > max_pending_edit_preview_bytes then
    fail "LSP edit preview exceeds the pending-preview limit";
  with_lock manager.lock (fun () ->
    if manager.closed then fail "LSP manager is closed";
    (match manager.identity with
     | Some identity when identity.owner = owner && identity.root = root &&
                          identity.program = program && identity.arguments = arguments -> ()
     | Some _ -> fail "LSP manager cannot be reused with a different owner, workspace, or server configuration"
     | None -> fail "LSP server has not been started");
    if Hashtbl.length manager.edit_previews >= max_pending_edit_previews ||
       bytes > max_pending_edit_preview_bytes - manager.preview_bytes then
      fail "too many pending LSP edit previews";
    if manager.next_preview_id = max_int then fail "LSP preview ID limit reached";
    manager.next_preview_id <- manager.next_preview_id + 1;
    let preview_id = "lsp-preview-" ^ string_of_int manager.next_preview_id in
    let preview = { preview_id; owner; root; program; arguments;
      title; files; bytes } in
    Hashtbl.add manager.edit_previews preview_id preview;
    manager.preview_bytes <- manager.preview_bytes + bytes;
    preview_id)

let get_edit_preview manager ~owner ~root ~program ~arguments preview_id =
  let root = try Workspace_path.root_path root
    with Workspace_path.Error message -> fail message in
  with_lock manager.lock (fun () ->
    if manager.closed then fail "LSP manager is closed";
    (match manager.identity with
     | Some identity when identity.owner = owner && identity.root = root &&
                          identity.program = program && identity.arguments = arguments -> ()
     | Some _ -> fail "LSP manager cannot be reused with a different owner, workspace, or server configuration"
     | None -> fail "LSP server has not been started");
    match Hashtbl.find_opt manager.edit_previews preview_id with
    | Some preview when preview.owner = owner && preview.root = root &&
                        preview.program = program && preview.arguments = arguments ->
        preview
    | _ -> fail "unknown or expired LSP edit preview")

let preview_details manager ~owner ~root ~program ~arguments preview_id =
  let preview = get_edit_preview manager ~owner ~root ~program ~arguments preview_id in
  preview.title, preview.files

let apply_edit_preview manager ~owner ~root ~program ~arguments ~preview_id
    ~apply_approved ~on_file_change =
  if not apply_approved then fail "applying an LSP edit preview requires explicit approval";
  let preview = get_edit_preview manager ~owner ~root ~program ~arguments preview_id in
  let files = preview_file_rows preview.files in
  if List.length files > max_edit_files then fail "LSP preview affects too many files";
  let seen = Hashtbl.create (List.length files) in
  let prepared = List.map (fun row ->
    let path = required_string "path" row in
    if Hashtbl.mem seen path then fail "LSP preview contains a duplicate file path";
    Hashtbl.add seen path ();
    let original_sha256 = required_string "original_sha256" row in
    let result_sha256 = required_string "result_sha256" row in
    let content = match member "content" row with
      | `String content -> content
      | _ -> fail "LSP preview content is invalid" in
    let changed = match member "changed" row with
      | `Bool changed -> changed
      | _ -> fail "LSP preview change marker is invalid" in
    if Workspace_edit.sha256 content <> result_sha256 then
      fail "LSP preview result hash is invalid";
    let absolute = try Workspace_path.regular_path preview.root path
      with Workspace_path.Error message -> fail message in
    let current = try Workspace_edit.read_snapshot ~root:preview.root ~path
      with Workspace_edit.Error message -> fail message in
    (try Workspace_edit.verify_snapshot original_sha256 current.sha256
     with Workspace_edit.Error message -> fail message);
    if changed <> (content <> current.contents) then
      fail "LSP preview change marker does not match its contents";
    path, absolute, current.contents, content, changed) files in
  with_lock manager.lock (fun () ->
    if Hashtbl.mem manager.edit_previews preview.preview_id then (
      Hashtbl.remove manager.edit_previews preview.preview_id;
      manager.preview_bytes <- max 0 (manager.preview_bytes - preview.bytes)));
  List.iter (fun (path, absolute, before, after, changed) ->
    if changed then (
      let current = try Workspace_edit.read_snapshot ~root:preview.root ~path
        with Workspace_edit.Error message -> fail message in
      (try Workspace_edit.verify_snapshot (Workspace_edit.sha256 before) current.sha256
       with Workspace_edit.Error message -> fail message);
      (try Workspace_path.atomic_write absolute after
       with Workspace_path.Error message -> fail message);
      on_file_change ~path ~before ~after)) prepared;
  `Assoc [
    "applied", `Bool true;
    "preview_id", `String preview.preview_id;
    "files", `List (List.filter_map (fun (path, _, _, _, changed) ->
      if changed then Some (`String path) else None) prepared)
  ]

let add_preview_id id = function
  | `Assoc fields -> `Assoc (("preview_id", `String id) :: fields)
  | _ -> fail "invalid LSP file preview"

let validate_location root location =
  ignore (assoc location);
  let uri, ranges =
    if member "targetUri" location <> `Null then
      let uri = required_string "targetUri" location in
      uri, [member "targetRange" location; member "targetSelectionRange" location]
    else
      let uri = required_string "uri" location in
      uri, [member "range" location] in
  let relative, snapshot = snapshot_for_uri root uri in
  ignore relative;
  List.iter (fun range -> ignore (range_offsets snapshot.contents range)) ranges

let validate_locations root ~multiple result =
  match result with
  | `Null -> result
  | `List locations ->
      if List.length locations > 2_000 then fail "too many LSP locations";
      List.iter (validate_location root) locations;
      result
  | `Assoc _ when not multiple -> validate_location root result; result
  | _ -> fail "invalid LSP location result"
let validate_hover text result =
  let rec content = function
    | `String _ -> ()
    | `List values when List.length values <= 1_000 -> List.iter content values
    | `Assoc fields ->
        let object_ = `Assoc fields in
        let kind = member "kind" object_ and value = member "value" object_ in
        let language = match member "language" object_ with `String _ -> true | _ -> false in
        (match kind, value, language with
         | `String ("plaintext" | "markdown"), `String _, _ -> ()
         | `Null, `String _, true -> ()
         | _ -> fail "invalid LSP hover contents")
    | _ -> fail "invalid LSP hover contents"
  in
  match result with
  | `Null -> result
  | `Assoc _ ->
      content (member "contents" result);
      (match member "range" result with `Null -> () | range -> ignore (range_offsets text range));
      result
  | _ -> fail "invalid LSP hover result"


let cancel_request manager id =
  if id <= 0 then fail "LSP request id must be positive";
  notification manager "$/cancelRequest" (`Assoc ["id", `Int id])


let validate_action action =
  match action with
  | "definition" | "references" | "hover" | "diagnostics"
  | "rename" | "code_actions" | "apply_preview" | "shutdown" -> ()
  | "cancel" -> ()
  | _ -> fail "unsupported LSP action"

let rec execute manager ~owner ~root ~program ~args ?(cancel = fun () -> false)
    ?(apply_approved = false)
    ?(on_file_change = fun ~path:_ ~before:_ ~after:_ -> ()) arguments =
  if String.length (json arguments) > max_message_bytes then fail "LSP arguments exceed the input limit";
  let action = required_string "action" arguments in
  validate_action action;
  let close_on_exit = ref false in
  Fun.protect
    ~finally:(fun () -> if !close_on_exit then close_manager manager)
    (fun () ->
      if action = "cancel" then (
        let canonical_root =
          try Workspace_path.root_path root with Workspace_path.Error message -> fail message in
        let id = int_value "request_id" (member "request_id" arguments) in
        with_lock manager.lock (fun () ->
          if manager.closed then fail "LSP manager is closed";
          match manager.identity with
          | Some identity when identity.owner = owner && identity.root = canonical_root &&
                               identity.program = program && identity.arguments = args ->
              if not (Hashtbl.mem manager.pending id) then fail "LSP request id is not active"
          | Some _ -> fail "LSP manager cannot be reused with a different owner, workspace, or server configuration"
          | None -> fail "LSP server has no active request");
        cancel_request manager id;
        `Assoc ["cancelled", `Int id]
      ) else with_lock manager.request_lock (fun () ->
        if manager.closed then fail "LSP manager is closed";
        let started = with_lock manager.lock (fun () -> manager.identity) in
        match action, started with
        | "shutdown", None ->
            close_on_exit := true;
            `Assoc ["shutdown", `Bool true]
        | "shutdown", Some identity ->
            let canonical_root =
              try Workspace_path.root_path root with Workspace_path.Error message -> fail message in
            if identity.owner <> owner || identity.root <> canonical_root ||
               identity.program <> program || identity.arguments <> args then
              fail "LSP manager cannot be reused with a different owner, workspace, or server configuration";
            close_on_exit := true;
            ignore (require_identity manager owner root program args);
            did_close_documents manager;
            let response = request ~cancel manager "shutdown" `Null in
            ignore (response_result response);
            with_lock manager.lock (fun () -> manager.shutdown_sent <- true);
            notification manager "exit" `Null;
            `Assoc ["shutdown", `Bool true]
        | "apply_preview", Some _ ->
            let identity = require_identity manager owner root program args in
            apply_edit_preview manager ~owner ~root:identity.root ~program
              ~arguments:args
              ~preview_id:(required_string "preview_id" arguments)
              ~apply_approved ~on_file_change
        | "apply_preview", None ->
            fail "LSP server has not been started"
        | _ ->
            let identity = require_identity manager owner root program args in
            let root = identity.root in
            (
      let relative = required_string "path" arguments in
      let language_id = required_string "language_id" arguments in
      if String.length language_id > 128 then fail "LSP language_id exceeds the limit";
      let document = current_document manager root relative language_id in
      if action = "diagnostics" then (
        let diagnostics = with_lock manager.lock (fun () -> `List document.diagnostics) in
        let output = `Assoc ["uri", `String document.uri; "version", `Int document.version;
          "diagnostics", diagnostics] in
        if String.length (json output) > max_frame_bytes then
          fail "LSP diagnostics exceed the output limit";
        output)
      else (
        let position =
          if action = "code_actions" && member "position" arguments = `Null then (
            let start = member "start" (member "range" arguments) in
            `Assoc ["line", `Int (int_value "range.start.line" (member "line" start));
              "character", `Int (int_value "range.start.character" (member "character" start))])
          else read_position arguments in
        ignore (position_offset document.text (int_value "line" (member "line" position))
          (int_value "character" (member "character" position)));
        let response = match action with
          | "definition" -> check_provider manager "definitionProvider";
              request ~cancel manager "textDocument/definition" (text_document_params document position)
          | "references" -> check_provider manager "referencesProvider";
              let params = assoc (text_document_params document position) @
                ["context", `Assoc ["includeDeclaration", `Bool true]] in
              request ~cancel manager "textDocument/references" (`Assoc params)
          | "hover" -> check_provider manager "hoverProvider";
              request ~cancel manager "textDocument/hover" (text_document_params document position)
          | "rename" ->
              check_provider manager "renameProvider";
              let new_name = required_string "new_name" arguments in
              request ~cancel manager "textDocument/rename"
                (`Assoc (assoc (text_document_params document position) @ ["newName", `String new_name]))
          | "code_actions" ->
              check_provider manager "codeActionProvider";
              let range = match member "range" arguments with `Null ->
                `Assoc ["start", position; "end", position] | range -> range in
              ignore (range_offsets document.text range);
              request ~cancel manager "textDocument/codeAction" (`Assoc [
                "textDocument", `Assoc ["uri", `String document.uri]; "range", range;
                "context", `Assoc ["diagnostics", `List document.diagnostics]])
          | _ -> assert false in
        let result = response_result response in
        let output =
          if action = "rename" then (
            if apply_approved then
              fail "preview the rename first, then apply its preview ID";
            let files = workspace_edit manager ~root result in
            if preview_has_changes files then (
              let preview_id = store_edit_preview manager ~owner ~root ~program
                ~arguments:args ~title:("Rename to " ^
                  required_string "new_name" arguments) files in
              match files with
              | `List rows ->
                  `List (List.map (add_preview_id preview_id) rows)
              | _ -> assert false)
            else files)
          else if action = "code_actions" then (
            if apply_approved then
              fail "preview the code action first, then apply its preview ID";
            let actions = match result with
              | `List actions -> actions
              | `Null -> []
              | _ -> fail "invalid LSP code action result" in
            if List.length actions > 256 then fail "too many LSP code actions";
            let output_size = ref 2 in
            let rows = List.mapi (fun _index action ->
              ignore (assoc action);
              if member "command" action <> `Null then
                fail "server-provided commands are not executed";
              let title = required_string "title" action in
              let kind = match member "kind" action with
                | `Null -> `Null
                | `String _ as kind -> kind
                | _ -> fail "invalid LSP code action kind" in
              let diagnostics = match member "diagnostics" action with
                | `Null -> `Null
                | value -> `List (validate_diagnostics document.text value) in
              let edit = member "edit" action in
              let previews = if edit = `Null then `List [] else
                workspace_edit manager ~root edit in
              let preview_id =
                if preview_has_changes previews then
                  `String (store_edit_preview manager ~owner ~root ~program
                    ~arguments:args ~title previews)
                else `Null in
              let row = `Assoc ["title", `String title; "kind", kind;
                "diagnostics", diagnostics; "previews", previews;
                "preview_id", preview_id] in
              let row_size = String.length (json row) + 1 in
              if row_size > max_frame_bytes - !output_size then
                fail "LSP code-action results exceed the output limit";
              output_size := !output_size + row_size;
              row) actions in
            `List rows)
          else if action = "hover" then validate_hover document.text result
          else if action = "definition" then validate_locations root ~multiple:false result
          else if action = "references" then validate_locations root ~multiple:true result
          else result in
        if String.length (json output) > max_frame_bytes then
          fail "LSP result exceeds the output limit";
        output))))

and close_manager manager =
  with_lock manager.request_lock (fun () ->
    let first_close, should_shutdown = with_lock manager.lock (fun () ->
      if manager.closed then false, false
      else (
        manager.closed <- true;
        let should_shutdown =
          manager.identity <> None && not manager.shutdown_sent in
        if should_shutdown then manager.shutdown_sent <- true;
        true, should_shutdown)) in
    if first_close then (
      did_close_documents manager;
      (match manager.io with
       | None -> ()
       | Some io ->
           if should_shutdown then (
             (try
                ignore (response_result
                  (request ~timeout_seconds:2. manager "shutdown" `Null))
              with _ -> ());
             (try notification manager "exit" `Null with _ -> ()));
           (try io.close () with _ -> ());
           (try io.terminate () with _ -> ());
           manager.io <- None);
      (match manager.reader with Some thread when Thread.id thread <> Thread.id (Thread.self ()) ->
          (try Thread.join thread with _ -> ()) | _ -> ());
      manager.reader <- None;
      with_lock manager.lock (fun () ->
        Hashtbl.clear manager.documents;
        Hashtbl.clear manager.edit_previews;
        manager.preview_bytes <- 0;
        manager.identity <- None;
        manager.capabilities <- `Null))
  )
