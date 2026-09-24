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
  mutable done_seen : bool;
  mutable finish_reason : string option;
  mutable content_seen : bool;
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
    | _ -> invalid "missing tool call index" in
  let call = get_call t index in
  (match field "type" json with
   | `Null | `String "function" -> ()
   | _ -> invalid "unsupported tool call type");
  (match optional_string "tool call id" (field "id" json) with
   | Some part -> append t call.id part | None -> ());
  (match field "function" json with
   | `Null -> ()
   | `Assoc _ as fn ->
       (match optional_string "function name" (field "name" fn) with
        | Some part -> append t call.name part | None -> ());
       (match optional_string "function arguments" (field "arguments" fn) with
        | Some part -> append t call.arguments part | None -> ())
   | _ -> invalid "invalid tool call function")

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
       (match optional_string "content" (field "content" delta) with
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
   | `String ("stop" | "tool_calls" as reason) -> t.finish_reason <- Some reason
   | `String reason -> invalid ("unexpected finish_reason: " ^ reason)
   | _ -> invalid "invalid finish_reason")

let parse_chunk t json =
  (match field "error" json with
   | `Null -> ()
   | _ -> invalid (error_text json));
  match field "choices" json with
  | `List [] -> () (* Optional usage-only chunk after the final choice. *)
  | `List [ choice ] -> parse_choice t choice
  | _ -> invalid "missing or ambiguous completion choices"

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

let create ~on_text =
  let t = { on_text; done_seen = false; finish_reason = None;
    content_seen = false; content = Buffer.create 256;
    calls = Hashtbl.create 4; response_bytes = 0; parser = None } in
  t.parser <- Some (Sse.create ~on_event:(handle_event t));
  t

let feed t bytes =
  match t.parser with
  | Some parser -> Sse.feed parser bytes
  | None -> invalid "SSE parser was not initialized"
let is_done t = t.done_seen
let is_finished t = t.finish_reason <> None


let finish t =
  (match t.parser with Some parser -> Sse.finish parser
    | None -> invalid "SSE parser was not initialized");
  if not t.done_seen && t.finish_reason = None then
    invalid "missing finish_reason or [DONE] event";
  let calls = Hashtbl.fold (fun _ call acc -> call :: acc) t.calls []
    |> List.sort (fun a b -> Int.compare a.index b.index) in
  let ids = Hashtbl.create (List.length calls) in
  let calls = List.map (fun call ->
    let id = Buffer.contents call.id in
    let name = Buffer.contents call.name in
    if id = "" || name = "" then invalid "empty tool call id or name";
    if Hashtbl.mem ids id then invalid "duplicate tool call id";
    Hashtbl.add ids id ();
    let arguments = try Yojson.Basic.from_string (Buffer.contents call.arguments)
      with Yojson.Json_error _ -> invalid "invalid function arguments JSON" in
    { Protocol.id = id; name; arguments }) calls in
  (match t.finish_reason, calls with
   | Some "tool_calls", [] -> invalid "finish_reason tool_calls without tool calls"
   | _ -> ());
  { Protocol.role = "assistant";
    content = (if t.content_seen then Some (Buffer.contents t.content) else None);
    tool_calls = calls; tool_call_id = None }
