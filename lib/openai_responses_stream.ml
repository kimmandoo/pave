let invalid detail = raise (Protocol.Invalid_response ("invalid Responses stream: " ^ detail))
let field = Protocol.member
let max_response_bytes = 16_777_216
let max_output_items = 128

type item =
  | Message of { id : string; text : Buffer.t; mutable saw_delta : bool;
                 mutable done_item : bool }
  | Call of { id : string; call_id : string; name : string; args : Buffer.t;
              mutable args_done : string option; mutable done_item : bool }
  | Ignored of string

type t = {
  on_text : string -> unit;
  items : (int, item) Hashtbl.t;
  mutable response_bytes : int;
  mutable completed : Protocol.message option;
  mutable done_seen : bool;
  mutable parser : Sse.t option;
}

let required_string name json = match field name json with
  | `String value when value <> "" -> value
  | _ -> invalid ("missing or invalid " ^ name)
let string_field name json = match field name json with
  | `String value -> value
  | _ -> invalid ("missing or invalid " ^ name)
let index json = match field "output_index" json with
  | `Int n when n >= 0 && n < max_output_items -> n
  | _ -> invalid "invalid output index"
let parse_json data = try Yojson.Basic.from_string data
  with Yojson.Json_error _ -> invalid "invalid SSE JSON"

let reserve t length =
  if length > max_response_bytes - t.response_bytes then
    invalid "response exceeds 16 MiB";
  t.response_bytes <- t.response_bytes + length

let append t buffer fragment =
  reserve t (String.length fragment);
  Buffer.add_string buffer fragment

let item_id = function
  | Message message -> message.id
  | Call call -> call.id
  | Ignored id -> id

let find t json =
  let n = index json in
  match Hashtbl.find_opt t.items n with
  | None -> invalid "output event before item added"
  | Some item ->
      if required_string "item_id" json <> item_id item then
        invalid "output item ID mismatch";
      item

let handle_added t json =
  let n = index json in
  if Hashtbl.mem t.items n then invalid "duplicate output item";
  let output = field "item" json in
  let id = required_string "id" output in
  if Hashtbl.length t.items >= max_output_items then invalid "too many output items";
  let item = match field "type" output with
    | `String "message" ->
        if field "role" output <> `String "assistant" then invalid "unexpected message role";
        Message { id; text = Buffer.create 128; saw_delta = false; done_item = false }
    | `String "function_call" ->
        let call_id = required_string "call_id" output in
        let name = required_string "name" output in
        let args = Buffer.create 128 in
        (match field "arguments" output with
         | `Null -> ()
         | `String value -> append t args value
         | _ -> invalid "invalid function arguments");
        Call { id; call_id; name; args; args_done = None; done_item = false }
    | `String "reasoning" -> Ignored id
    | _ -> invalid "unsupported output item" in
  Hashtbl.add t.items n item

let message_text output =
  match field "content" output with
  | `List content -> String.concat "" (List.map (fun part ->
      match field "type" part, field "text" part with
      | `String "output_text", `String text -> text
      | `String "refusal", _ -> invalid "refusal"
      | _ -> invalid "unsupported message content") content)
  | _ -> invalid "missing message content"

let handle_item_done t json =
  let output = field "item" json in
  let item = find t (`Assoc [ "output_index", field "output_index" json;
    "item_id", field "id" output ]) in
  match item with
  | Message message ->
      if message.done_item || field "type" output <> `String "message" ||
         field "role" output <> `String "assistant" then invalid "invalid completed message";
      (match field "status" output with
       | `Null | `String "completed" -> ()
       | _ -> invalid "incomplete message");
      let text = message_text output in
      if message.saw_delta then (
        if Buffer.contents message.text <> text then invalid "text delta mismatch")
      else if text <> "" then (append t message.text text; t.on_text text);
      message.done_item <- true
  | Call call ->
      if call.done_item || field "type" output <> `String "function_call" ||
         required_string "call_id" output <> call.call_id ||
         required_string "name" output <> call.name then invalid "invalid completed function call";
      (match field "status" output with
       | `Null | `String "completed" -> ()
       | _ -> invalid "incomplete function call");
      let args = string_field "arguments" output in
      (match (try Yojson.Basic.from_string args
              with Yojson.Json_error _ -> invalid "invalid function arguments JSON") with
       | `Assoc _ -> ()
       | _ -> invalid "tool arguments must be an object");
      if Buffer.length call.args > 0 && Buffer.contents call.args <> args then
        invalid "function arguments delta mismatch";
      (match call.args_done with
       | Some finished when finished <> args -> invalid "function arguments done mismatch"
       | _ -> ());
      if Buffer.length call.args = 0 then reserve t (String.length args);
      call.args_done <- Some args;
      call.done_item <- true
  | Ignored _ -> ()

let handle_delta t json =
  match find t json with
  | Message message ->
      if message.done_item then invalid "text delta after output item done";
      (match field "content_index" json with
       | `Int n when n >= 0 -> ()
       | _ -> invalid "invalid text content index");
      let delta = string_field "delta" json in
      message.saw_delta <- true;
      append t message.text delta;
      if delta <> "" then t.on_text delta
  | _ -> invalid "text delta for non-message item"

let handle_arguments t ~done_event json =
  match find t json with
  | Call call ->
      if call.done_item then invalid "arguments after output item done";
      if done_event then (
        if call.args_done <> None then invalid "duplicate arguments done";
        let args = string_field "arguments" json in
        if Buffer.length call.args > 0 && Buffer.contents call.args <> args then
          invalid "function arguments delta mismatch";
        (match field "name" json with
         | `String name when name = call.name -> ()
         | _ -> invalid "function name mismatch");
        call.args_done <- Some args)
      else (
        if call.args_done <> None then invalid "arguments after arguments done";
        append t call.args (string_field "delta" json))
  | _ -> invalid "function arguments for non-call item"

let handle_completed t json =
  if t.completed <> None then invalid "duplicate response.completed";
  let response = field "response" json in
  let result = Openai_responses_wire.parse_completion response in
  let outputs = match field "output" response with `List outputs -> outputs | _ -> assert false in
  Hashtbl.iter (fun index item ->
    let output = try List.nth outputs index with Failure _ -> invalid "missing completed output item" in
    if field "id" output <> `String (item_id item) then invalid "completed output ID mismatch";
    match item with
    | Message message ->
        if not message.done_item ||
           Buffer.contents message.text <> message_text output then
          invalid "completed output message mismatch"
    | Call call ->
        if not call.done_item || field "call_id" output <> `String call.call_id ||
           field "name" output <> `String call.name ||
           call.args_done <> Some (string_field "arguments" output) then
          invalid "completed function call mismatch"
    | Ignored _ -> ()) t.items;
  List.iteri (fun index output ->
    if not (Hashtbl.mem t.items index) && field "type" output = `String "message" then (
      let text = message_text output in
      if text <> "" then (reserve t (String.length text); t.on_text text))) outputs;
  t.completed <- Some result;
  t.done_seen <- true

let handle_event t event data =
  if data = "[DONE]" then (
    if not t.done_seen then invalid "[DONE] before response.completed")
  else (
    if t.done_seen then invalid "event after response.completed";
    let json = parse_json data in
    let kind = match field "type" json, event with
      | `String kind, Some label when label <> "message" && label <> kind ->
          invalid "SSE event type mismatch"
      | `String kind, _ -> kind
      | _, Some "error" -> "error"
      | _ -> invalid "missing event type" in
    match kind with
    | "response.output_item.added" -> handle_added t json
    | "response.output_item.done" -> handle_item_done t json
    | "response.output_text.delta" -> handle_delta t json
    | "response.function_call_arguments.delta" -> handle_arguments t ~done_event:false json
    | "response.function_call_arguments.done" -> handle_arguments t ~done_event:true json
    | "response.completed" -> handle_completed t json
    | "error" | "response.failed" | "response.incomplete" -> invalid "response failed or incomplete"
    | _ when String.length kind >= 9 && String.sub kind 0 9 = "response." -> ()
    | _ -> invalid "unknown event")

let create ~on_text =
  let t = { on_text; items = Hashtbl.create 4; response_bytes = 0;
    completed = None; done_seen = false; parser = None } in
  t.parser <- Some (Sse.create ~on_event:(handle_event t));
  t

let feed t bytes = match t.parser with
  | Some parser -> Sse.feed parser bytes
  | None -> invalid "SSE parser not initialized"
let is_done t = t.done_seen
let is_finished t = t.completed <> None

let finish t =
  (match t.parser with Some parser -> Sse.finish parser
   | None -> invalid "SSE parser not initialized");
  match t.completed with
  | Some response -> response
  | None -> invalid "missing response.completed"
