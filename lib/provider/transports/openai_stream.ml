(* Bound assembled assistant content separately from the shared SSE event parser. *)
let max_response_bytes = 16_777_216
let max_tool_calls = 128

let invalid message = raise (Protocol.Invalid_response message)

type call = {
  index : int;
  id : Buffer.t;
  name : Buffer.t;
  arguments : Buffer.t;
}

type t = {
  on_text : string -> unit;
  on_tool_arguments : (Protocol.tool_argument_delta -> unit) option;
  mutable done_seen : bool;
  mutable finish_reason : string option;
  mutable content_seen : bool;
  mutable usage : Protocol.usage option;
  mutable failed : bool;
  content : Buffer.t;
  calls : (int, call) Hashtbl.t;
  mutable response_bytes : int;
  mutable parser : Sse.t option;
}


let reserve t length =
  if length > max_response_bytes - t.response_bytes then
    invalid "streamed response exceeds 16 MiB";
  t.response_bytes <- t.response_bytes + length

let append t buffer fragment =
  reserve t (String.length fragment);
  Buffer.add_string buffer fragment

let field key json = Protocol.member key json

let optional_string label = function
  | `Null -> None
  | `String value -> Some value
  | _ -> invalid ("invalid " ^ label ^ " in streamed response")

let error_text json =
  match field "error" json with
  | `String message -> message
  | `Assoc _ as err ->
      (match field "message" err with
       | `String message -> message
       | _ -> "OpenAI stream error")
  | _ -> "OpenAI stream error"

let parse_json data =
  try Yojson.Basic.from_string data
  with Yojson.Json_error _ -> invalid "invalid JSON in SSE event"

let get_call t index =
  if index < 0 then invalid "negative tool call index";
  match Hashtbl.find_opt t.calls index with
  | Some call -> call
  | None ->
      if Hashtbl.length t.calls >= max_tool_calls then invalid "too many tool calls";
      let call = { index; id = Buffer.create 32; name = Buffer.create 32;
                   arguments = Buffer.create 128 } in
      Hashtbl.add t.calls index call;
      call

let parse_tool_delta t json =
  let index = match field "index" json with
    | `Int index -> index
    | _ -> invalid "missing or invalid tool call index" in
  let call = get_call t index in
  (match field "type" json with
   | `Null | `String "function" -> ()
   | _ -> invalid "unsupported tool call type");
  (match optional_string "tool call id" (field "id" json) with
   | Some part -> append t call.id part | None -> ());
  let fragment = match field "function" json with
    | `Null -> ""
    | `Assoc _ as fn ->
        (match optional_string "function name" (field "name" fn) with
         | Some part -> append t call.name part | None -> ());
        (match field "arguments" fn with
         | `Null -> ""
         | `String part -> part
         | (`Assoc _ | `List _ | `Int _ | `Float _ | `Bool _) as value ->
             (* Compatible hosts may send a complete argument value once. *)
             Yojson.Basic.to_string value)
    | _ -> invalid "invalid tool call function" in
  append t call.arguments fragment;
  (match t.on_tool_arguments with
   | None -> ()
   | Some emit ->
       let id = Buffer.contents call.id in
       emit { Protocol.key = Printf.sprintf "chat:%d" index;
         call_id = (if id = "" then None else Some id);
         name = Buffer.contents call.name; fragment })
let content_text json =
  match json with
  | `Null -> None
  | `String text -> Some text
  | `List parts ->
      (* Some compatible hosts (Mistral-style) stream content as typed parts. *)
      let text = Buffer.create 64 in
      List.iter (fun part ->
        match field "type" part, field "text" part with
        | `String "text", `String piece -> Buffer.add_string text piece
        | `String "refusal", _ -> invalid "refusal"
        | _ -> invalid "unsupported content delta part") parts;
      Some (Buffer.contents text)
  | _ -> invalid "invalid content delta"

let parse_choice t json =
  (match field "index" json with
   | `Null | `Int 0 -> ()
   | _ -> invalid "unexpected completion choice index");
  if t.finish_reason <> None then invalid "completion chunk after finish_reason";
  (match field "delta" json with
   | `Null -> ()
   | `Assoc _ as delta ->
       (match optional_string "assistant role" (field "role" delta) with
        | None | Some "assistant" -> ()
        | Some _ -> invalid "unexpected streamed message role");
       (match field "refusal" delta with
        | `Null | `String "" -> ()
        | `String _ -> invalid "refusal"
        | _ -> invalid "invalid refusal");
       (match content_text (field "content" delta) with
        | None -> ()
        | Some text ->
            t.content_seen <- true;
            append t t.content text;
            if text <> "" then t.on_text text);
       (match field "tool_calls" delta with
        | `Null -> ()
        | `List calls -> List.iter (parse_tool_delta t) calls
        | _ -> invalid "invalid tool_calls delta")
   | _ -> invalid "invalid completion delta");
  (match field "finish_reason" json with
   | `Null -> ()
   | `String reason ->
       (* Compatible hosts report the same outcome under several spellings;
          normalize like the non-streamed parser. *)
       let reason = String.lowercase_ascii reason in
       (match reason with
        | "stop" | "end" | "tool_calls" | "function_call" ->
            if t.finish_reason = None then t.finish_reason <- Some reason
        | "length" | "max_tokens" ->
            Protocol.truncated ("finish_reason " ^ reason)
        | "content_filter" | "network_error" ->
            invalid ("provider finish_reason: " ^ reason)
        | "error" | "insufficient_system_resource" ->
            invalid ("provider returned error finish_reason" ^
              (if reason = "error" then "" else ": " ^ reason))
        | _ -> invalid ("provider finish_reason: " ^ reason))
   | _ -> invalid "invalid finish_reason")

let parse_chunk t json =
  (match field "error" json with
   | `Null -> ()
   | _ -> invalid (error_text json));
  (match field "choices" json with
   | `Null | `List [] -> () (* Usage-only and keep-alive chunks carry none. *)
   | `List [ choice ] -> parse_choice t choice
   | _ -> invalid "ambiguous completion choices");
  if t.finish_reason <> None then
    match Protocol.completion_usage json with
    | None -> ()
    | Some usage ->
        if t.usage <> None then invalid "duplicate completion usage";
        t.usage <- Some usage

let handle_event t event data =
  if event = Some "error" then (
    let json = parse_json data in
    let message = match field "error" json with `Null ->
      (match field "message" json with
       | `String message -> message | _ -> "OpenAI stream error")
      | _ -> error_text json in
    invalid message);
  if data = "[DONE]" then (
    if t.done_seen then invalid "duplicate [DONE] event";
    t.done_seen <- true)
  else if t.done_seen then invalid "completion chunk after [DONE]"
  else parse_chunk t (parse_json data)

let create ?on_tool_arguments ~on_text () =
  let t = { on_text; on_tool_arguments; done_seen = false; finish_reason = None;
    content_seen = false; usage = None; failed = false;
    content = Buffer.create 256;
    calls = Hashtbl.create 4; response_bytes = 0;
    parser = None } in
  t.parser <- Some (Sse.create ~on_event:(handle_event t));
  t

let feed t bytes =
  if t.failed then invalid "stream is invalid";
  try match t.parser with
  | Some parser -> Sse.feed parser bytes
  | None -> invalid "SSE parser was not initialized"
  with Protocol.Invalid_response _ as error ->
    t.failed <- true;
    raise error
let is_done t = t.done_seen && not t.failed
let is_finished t = t.finish_reason <> None && not t.failed
let usage t = if t.done_seen && not t.failed && t.finish_reason <> None
  then t.usage else None


let finish t =
  if t.failed then invalid "stream is invalid";
  try
    (match t.parser with Some parser -> Sse.finish parser
      | None -> invalid "SSE parser was not initialized");
    if not t.done_seen then
      invalid (if t.finish_reason = None
        then "stream ended before the reply finished (connection closed early)"
        else "stream ended without the [DONE] terminator");
    if t.finish_reason = None then invalid "missing finish_reason";
    let calls = Hashtbl.fold (fun _ call acc -> call :: acc) t.calls []
      |> List.sort (fun a b -> Int.compare a.index b.index) in
    let ids = Hashtbl.create (List.length calls) in
    let calls = List.map (fun call ->
      let id = Buffer.contents call.id in
      let name = Buffer.contents call.name in
      if id = "" || name = "" then invalid "empty tool call id or name";
      if Hashtbl.mem ids id then invalid "duplicate tool call id";
      Hashtbl.add ids id ();
      let raw = Buffer.contents call.arguments in
      (* Compatible servers omit the arguments of parameterless tools. *)
      let arguments = Protocol.decode_tool_arguments raw in
      { Protocol.id = id; name; arguments }) calls in
    (match t.finish_reason, calls with
     | Some ("tool_calls" | "function_call"), [] ->
         invalid "tool finish_reason without tool calls"
     | Some ("stop" | "end"), _ :: _ ->
         invalid "text finish_reason with tool calls"
     | _ -> ());
    { Protocol.role = "assistant"; content = (if t.content_seen then Some (Buffer.contents t.content) else None);
    tool_calls = calls; tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] }
  with Protocol.Invalid_response _ as error ->
    t.failed <- true;
    raise error
