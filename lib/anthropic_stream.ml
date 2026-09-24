type block =
  | Text of Buffer.t
  | Tool of string * string * Yojson.Basic.t * Buffer.t
  | Ignored

type t = {
  on_text : string -> unit;
  blocks : (int, block * bool) Hashtbl.t;
  mutable started : bool;
  mutable stopped : bool;
  mutable reason : string option;
  mutable response_bytes : int;
  mutable parser : Sse.t option;
}

let invalid text = raise (Protocol.Invalid_response ("invalid Anthropic stream: " ^ text))
let field = Protocol.member
let required_string key json = match field key json with
  | `String text -> text
  | _ -> invalid ("missing " ^ key)
let index json = match field "index" json with
  | `Int n when n >= 0 -> n
  | _ -> invalid "missing or invalid block index"
let parse_json text = try Yojson.Basic.from_string text
  with Yojson.Json_error _ -> invalid "invalid SSE JSON"

let reserve t text =
  let length = String.length text in
  if length > 16_777_216 - t.response_bytes then invalid "response exceeds 16 MiB";
  t.response_bytes <- t.response_bytes + length

let handle_block_start t json =
  let n = index json in
  if Hashtbl.mem t.blocks n then invalid "duplicate block index";
  if Hashtbl.length t.blocks >= 128 then invalid "too many content blocks";
  let content = field "content_block" json in
  let block = match field "type" content with
    | `String "text" ->
        let initial = required_string "text" content in
        reserve t initial;
        if initial <> "" then t.on_text initial;
        let text = Buffer.create (String.length initial + 64) in
        Buffer.add_string text initial;
        Text text
    | `String "tool_use" ->
        let id = required_string "id" content in
        let name = required_string "name" content in
        if id = "" || name = "" then invalid "empty tool id or name";
        let input = match field "input" content with
          | `Null -> `Assoc []
          | (`Assoc _ as json) -> json
          | _ -> invalid "tool input must be an object" in
        Tool (id, name, input, Buffer.create 128)
    | `String ("thinking" | "redacted_thinking") -> Ignored
    | _ -> invalid "unsupported content block" in
  Hashtbl.add t.blocks n (block, false)

let handle_block_delta t json =
  let n = index json in
  let block, closed = match Hashtbl.find_opt t.blocks n with
    | Some value -> value | None -> invalid "delta without block start" in
  if closed then invalid "delta after block stop";
  let delta = field "delta" json in
  (match field "type" delta, block with
   | `String "text_delta", Text text ->
       let value = required_string "text" delta in
       reserve t value;
       Buffer.add_string text value;
       if value <> "" then t.on_text value
   | `String "input_json_delta", Tool (_, _, _, partial) ->
       let value = required_string "partial_json" delta in
       reserve t value;
       Buffer.add_string partial value
   | `String ("thinking_delta" | "signature_delta"), Ignored -> ()
   | _ -> invalid "delta type does not match content block")

let handle_message_delta t json =
  match field "stop_reason" (field "delta" json) with
  | `Null -> ()
  | `String reason ->
      if t.reason <> None then invalid "duplicate stop reason";
      t.reason <- Some reason
  | _ -> invalid "invalid stop reason"

let handle_event t event data =
  let json = parse_json data in
  let kind = required_string "type" json in
  (match event with
   | Some name when name <> kind -> invalid "SSE event name and type differ"
   | _ -> ());
  if t.stopped then invalid "event after message_stop";
  match kind with
  | "ping" -> ()
  | "error" ->
      let message = match field "message" (field "error" json) with
        | `String text -> text | _ -> "provider error" in
      invalid message
  | "message_start" ->
      if t.started then invalid "duplicate message_start";
      if field "role" (field "message" json) <> `String "assistant" then
        invalid "unexpected message role";
      t.started <- true
  | "content_block_start" ->
      if not t.started || t.reason <> None then invalid "block outside active message";
      handle_block_start t json
  | "content_block_delta" ->
      if not t.started || t.reason <> None then invalid "delta outside active message";
      handle_block_delta t json
  | "content_block_stop" ->
      let n = index json in
      let block, closed = match Hashtbl.find_opt t.blocks n with
        | Some value -> value | None -> invalid "block stop without start" in
      if closed then invalid "duplicate block stop";
      Hashtbl.replace t.blocks n (block, true)
  | "message_delta" ->
      if not t.started then invalid "message_delta before start";
      handle_message_delta t json
  | "message_stop" ->
      if not t.started || t.reason = None then invalid "message_stop before terminal reason";
      t.stopped <- true
  | _ -> invalid ("unknown event: " ^ kind)

let create ~on_text =
  let t = { on_text; blocks = Hashtbl.create 4; started = false; stopped = false;
    reason = None; response_bytes = 0; parser = None } in
  t.parser <- Some (Sse.create ~on_event:(handle_event t));
  t

let feed t bytes = match t.parser with
  | Some parser -> Sse.feed parser bytes
  | None -> invalid "SSE parser was not initialized"

let is_done t = t.stopped
let is_finished t = t.reason <> None

let finish t =
  (match t.parser with Some parser -> Sse.finish parser | None -> invalid "missing parser");
  if not t.started || t.reason = None then invalid "incomplete message";
  let blocks = Hashtbl.fold (fun n (block, closed) items ->
    if not closed then invalid "unclosed content block";
    (n, block) :: items) t.blocks [] |> List.sort (fun (a, _) (b, _) -> Int.compare a b) in
  let text = Buffer.create 128 in
  let calls = ref [] in
  List.iter (fun (_, block) -> match block with
    | Text part -> Buffer.add_buffer text part
    | Tool (id, name, input, partial) ->
        let arguments = if Buffer.length partial = 0 then input else
          parse_json (Buffer.contents partial) in
        (match arguments with `Assoc _ -> () | _ -> invalid "tool input must be an object");
        if List.exists (fun (call : Protocol.tool_call) -> call.id = id) !calls then
          invalid "duplicate tool id";
        calls := { Protocol.id; name; arguments } :: !calls
    | Ignored -> ()) blocks;
  let calls = List.rev !calls in
  (match t.reason with
   | Some "end_turn" when calls = [] -> ()
   | Some "tool_use" when calls <> [] -> ()
   | Some reason -> invalid ("unexpected stop_reason: " ^ reason)
   | None -> assert false);
  { Protocol.role = "assistant";
    content = (if Buffer.length text = 0 then None else Some (Buffer.contents text));
    tool_calls = calls; tool_call_id = None }
