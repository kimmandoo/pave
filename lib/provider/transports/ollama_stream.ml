let invalid text = raise (Protocol.Invalid_response ("invalid Ollama stream: " ^ text))
let field = Protocol.member
let max_line_bytes = 1_048_576
let max_response_bytes = 16_777_216
let max_tool_calls = 128

type arguments = Complete of Yojson.Basic.t | Fragments of Buffer.t

type call = {
  index : int;
  name : string;
  arguments : arguments;
}

type t = {
  on_text : string -> unit;
  line : Buffer.t;
  content : Buffer.t;
  mutable content_seen : bool;
  mutable after_cr : bool;
  mutable done_seen : bool;
  mutable response_bytes : int;
  mutable next_call : int;
  calls : call list ref;
  indexed : (int, call) Hashtbl.t;
  mutable result : Protocol.message option;
  mutable usage : Protocol.usage option;
}

let create ~on_text =
  { on_text; line = Buffer.create 256; content = Buffer.create 256;
    content_seen = false; after_cr = false; done_seen = false;
    response_bytes = 0; next_call = 0; calls = ref [];
    indexed = Hashtbl.create 4; result = None; usage = None }

let reserve t length =
  if length > max_response_bytes - t.response_bytes then invalid "response exceeds 16 MiB";
  t.response_bytes <- t.response_bytes + length

let parse_json line =
  try Yojson.Basic.from_string line
  with Yojson.Json_error _ -> invalid "invalid NDJSON frame"

let parse_arguments text =
  let json = parse_json text in
  match json with `Assoc _ -> json | _ -> invalid "tool arguments must be an object"

let required_name fn = match field "name" fn with
  | `String name when name <> "" -> name
  | _ -> invalid "missing tool name"

let add_call t index name arguments =
  if t.next_call >= max_tool_calls then invalid "too many tool calls";
  let call = { index = t.next_call; name; arguments } in
  t.next_call <- t.next_call + 1;
  t.calls := call :: !(t.calls);
  (match index with Some index -> Hashtbl.add t.indexed index call | None -> ())

let handle_call t json =
  (match field "type" json with
   | `Null | `String "function" -> ()
   | _ -> invalid "unsupported tool call type");
  let fn = match field "function" json with
    | `Assoc _ as fn -> fn
    | _ -> invalid "missing tool function" in
  let index = match field "index" fn with
    | `Null -> None
    | `Int index when index >= 0 -> Some index
    | _ -> invalid "invalid tool call index" in
  let arguments = field "arguments" fn in
  match index with
  | Some index when Hashtbl.mem t.indexed index ->
      let call = Hashtbl.find t.indexed index in
      (match field "name" fn with
       | `Null -> ()
       | `String name when name = call.name -> ()
       | _ -> invalid "inconsistent tool name");
      (match call.arguments, arguments with
       | Complete _, _ -> invalid "duplicate completed tool call"
       | Fragments buffer, `String part ->
           Buffer.add_string buffer part
       | _ -> invalid "invalid tool arguments fragment")
  | _ ->
      let name = required_name fn in
      let arguments = match arguments with
        | `Assoc _ as args -> Complete args
        | `String part ->
            if index = None then Complete (parse_arguments part)
            else let buffer = Buffer.create 128 in
              Buffer.add_string buffer part;
              Fragments buffer
        | _ -> invalid "missing tool arguments" in
      add_call t index name arguments

let finish_calls t =
  List.rev_map (fun call ->
    let arguments = match call.arguments with
      | Complete args -> args
      | Fragments buffer -> parse_arguments (Buffer.contents buffer) in
    { Protocol.id = Printf.sprintf "ollama:%d:%s" call.index call.name;
      name = call.name; arguments }) !(t.calls)

let handle_frame t line =
  if line <> "" then (
    reserve t (String.length line);
    if t.done_seen then invalid "frame after done";
    let json = parse_json line in
    (match json with `Assoc _ -> () | _ -> invalid "frame must be an object");
    Ollama_wire.check_error json;
    let message = match field "message" json with
      | `Assoc _ as message -> message
      | _ -> invalid "missing message" in
    (match field "role" message with
     | `String "assistant" -> ()
     | _ -> invalid "unexpected message role");
    (match field "content" message with
     | `Null -> ()
     | `String text ->
         t.content_seen <- true;
         Buffer.add_string t.content text;
         if text <> "" then t.on_text text
     | _ -> invalid "invalid message content");
    (match field "thinking" message with
     | `Null | `String _ -> ()
     | _ -> invalid "invalid thinking");
    (match field "tool_calls" message with
     | `Null -> ()
     | `List calls -> List.iter (handle_call t) calls
     | _ -> invalid "invalid tool_calls");
    match field "done" json with
    | `Null | `Bool false ->
        if field "done_reason" json <> `Null then invalid "done_reason before done"
    | `Bool true ->
        let tool_calls = finish_calls t in
        Ollama_wire.check_done_reason json (tool_calls <> []);
        let content = if t.content_seen then Some (Buffer.contents t.content) else None in
        if tool_calls = [] && (content = None || content = Some "") then
          invalid "empty assistant response";
        t.usage <- Ollama_wire.usage json;
        t.result <- Some { Protocol.role = "assistant"; content;
          tool_calls; tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] };
        t.done_seen <- true
    | _ -> invalid "invalid done marker")

let process_line t =
  let line = Buffer.contents t.line in
  Buffer.clear t.line;
  handle_frame t line

let feed t bytes =
  String.iter (fun byte ->
    if t.after_cr && byte = '\n' then t.after_cr <- false
    else (
      t.after_cr <- false;
      match byte with
      | '\n' -> process_line t
      | '\r' -> process_line t; t.after_cr <- true
      | _ ->
          if Buffer.length t.line >= max_line_bytes then invalid "NDJSON line exceeds 1 MiB";
          Buffer.add_char t.line byte)) bytes

let is_done t = t.done_seen
let is_finished t = t.done_seen

let usage t = t.usage
let finish t =
  if Buffer.length t.line <> 0 then process_line t;
  match t.result with
  | Some result -> result
  | None -> invalid "missing done frame"
