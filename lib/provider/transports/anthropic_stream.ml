type block =
  | Text of Buffer.t
  | Tool of string * string * Yojson.Basic.t * Buffer.t
  (* Thinking/redacted_thinking replay: accumulate the reconstructed block so
     finish can attach it to provider_state. *)
  | Thinking of { kind : string; text : Buffer.t; signature : Buffer.t }

type t = {
  on_text : string -> unit;
  provider : string;
  model : string;
  endpoint_digest : string option;
  on_tool_arguments : (Protocol.tool_argument_delta -> unit) option;
  blocks : (int, block * bool) Hashtbl.t;
  mutable started : bool;
  mutable stopped : bool;
  mutable reason : string option;
  mutable response_bytes : int;
  mutable input_tokens : int option;
  mutable output_tokens : int option;
  mutable cached_input_tokens : int option;
  mutable cache_creation_input_tokens : int option;
  mutable failed : bool;
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
        (match t.on_tool_arguments with
         | None -> ()
         | Some emit -> emit { Protocol.key = Printf.sprintf "anthropic:%d" n;
             call_id = Some id; name; fragment = "" });
        Tool (id, name, input, Buffer.create 128)
    | `String "thinking" ->
        let text = required_string "thinking" content in
        reserve t text;
        let signature_text = match field "signature" content with
          | `Null -> "" | `String value -> value
          | _ -> invalid "invalid thinking signature" in
        reserve t signature_text;
        let text_buffer = Buffer.create (String.length text + 64) in
        Buffer.add_string text_buffer text;
        let signature = Buffer.create (String.length signature_text + 64) in
        Buffer.add_string signature signature_text;
        Thinking { kind = "thinking"; text = text_buffer; signature }
    | `String "redacted_thinking" ->
        let data = required_string "data" content in
        reserve t data;
        let signature = Buffer.create (String.length data + 8) in
        Buffer.add_string signature data;
        Thinking { kind = "redacted_thinking"; text = Buffer.create 0;
          signature }
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
   | `String "input_json_delta", Tool (id, name, _, partial) ->
       let value = required_string "partial_json" delta in
       reserve t value;
       Buffer.add_string partial value;
       (match t.on_tool_arguments with
        | None -> ()
        | Some emit -> emit { Protocol.key = Printf.sprintf "anthropic:%d" n;
            call_id = Some id; name; fragment = value })
   | `String "thinking_delta", Thinking { text; _ } ->
       let value = required_string "thinking" delta in
       reserve t value;
       Buffer.add_string text value
   | `String "signature_delta", Thinking { signature; _ } ->
       let value = required_string "signature" delta in
       reserve t value;
       Buffer.add_string signature value
   | _ -> invalid "delta type does not match content block")

let handle_message_delta t json =
  (match field "usage" json with
   | `Null -> ()
   | reported ->
       t.output_tokens <- Anthropic_wire.output_usage reported;
       (match Anthropic_wire.input_usage reported with
        | Some (input_tokens, cache_creation, cache_read) ->
            t.input_tokens <- Some input_tokens;
            t.cache_creation_input_tokens <- cache_creation;
            t.cached_input_tokens <- cache_read
        | None -> ()));
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
      let message = field "message" json in
      if field "type" message <> `String "message" ||
         field "role" message <> `String "assistant" then
        invalid "unexpected message type or role";
      (match Anthropic_wire.input_usage (field "usage" message) with
       | Some (input_tokens, cache_creation, cache_read) ->
           t.input_tokens <- Some input_tokens;
           t.cache_creation_input_tokens <- cache_creation;
           t.cached_input_tokens <- cache_read
       | None -> ());
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
      if not t.started then invalid "message_stop before message_start";
      t.stopped <- true
  | _ -> () (* unknown events ignored for forward compatibility *)

let create ?on_tool_arguments ?endpoint_digest ?(provider = "anthropic")
    ?(model = "") ~on_text () =
  let t = { on_text; provider; model; endpoint_digest; on_tool_arguments;
    blocks = Hashtbl.create 4; started = false; stopped = false;
    reason = None; input_tokens = None; output_tokens = None;
    cached_input_tokens = None; cache_creation_input_tokens = None;
    failed = false; response_bytes = 0; parser = None } in
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

let is_done t = t.stopped && not t.failed
let is_finished t = t.stopped && not t.failed

let usage t = match t.stopped, t.failed, t.input_tokens, t.output_tokens with
  | true, false, Some input_tokens, Some output_tokens ->
      Some { Protocol.input_tokens; output_tokens;
        cached_input_tokens = t.cached_input_tokens;
        cache_creation_input_tokens = t.cache_creation_input_tokens;
        reasoning_output_tokens = None; input_modality_tokens = None;
        cached_input_modality_tokens = None; output_modality_tokens = None }
  | _ -> None
let finish t =
  if t.failed then invalid "stream is invalid";
  try
    (match t.parser with Some parser -> Sse.finish parser | None -> invalid "missing parser");
    if not t.started || not t.stopped || t.reason = None then
      invalid "incomplete message";
    let blocks = Hashtbl.fold (fun n (block, closed) items ->
      (n, block, closed) :: items) t.blocks []
      |> List.sort (fun (a, _, _) (b, _, _) -> Int.compare a b) in
    let text = Buffer.create 128 in
    let calls = ref [] in
    let state_blocks = ref [] in
    List.iter (fun (_, block, closed) -> match block with
      | Text part ->
          if not closed then invalid "unclosed content block";
          Buffer.add_buffer text part;
          state_blocks := `Assoc ["type", `String "text";
            "text", `String (Buffer.contents part)] :: !state_blocks
      | Tool (id, name, input, partial) ->
          if not closed then invalid "unclosed content block";
          let arguments = if Buffer.length partial = 0 then input else
            Protocol.decode_tool_arguments (Buffer.contents partial) in
          (match arguments with `Assoc _ -> () | _ -> invalid "tool input must be an object");
          if List.exists (fun (call : Protocol.tool_call) -> call.id = id) !calls then
            invalid "duplicate tool id";
          calls := { Protocol.id; name; arguments } :: !calls;
          state_blocks := `Assoc ["type", `String "tool_use"; "id", `String id;
            "name", `String name; "input", arguments] :: !state_blocks
      | Thinking { kind; text = thinking_text; signature } ->
          if not closed then invalid "unclosed content block";
          let fields = if kind = "thinking"
            then ["type", `String "thinking";
                  "thinking", `String (Buffer.contents thinking_text)]
            else ["type", `String "redacted_thinking";
                  "data", `String (Buffer.contents signature)] in
          let fields = if kind = "thinking" && Buffer.length signature > 0
            then fields @ ["signature", `String (Buffer.contents signature)]
            else fields in
          state_blocks := `Assoc fields :: !state_blocks) blocks;
    let calls = List.rev !calls in
    (match t.reason with
     | Some "end_turn" when calls = [] -> ()
     | Some "tool_use" when calls <> [] -> ()
     | Some ("end_turn" | "tool_use") ->
         invalid "stop_reason/content mismatch"
     | Some "stop_sequence" when calls = [] -> ()
     | Some ("max_tokens" | "model_context_window_exceeded" as reason) ->
         Protocol.truncated ("stop_reason " ^ reason)
     | Some ("refusal" | "sensitive" as reason) -> invalid reason
     | Some reason -> invalid ("unsupported stop_reason: " ^ reason)
     | None -> assert false);
    let provider_state =
      let blocks = List.rev !state_blocks in
      if t.model <> "" &&
         List.exists Anthropic_wire.native_thinking_block blocks
      then Some (`Assoc (Anthropic_wire.native_state_tag
        ?endpoint_digest:t.endpoint_digest t.provider t.model @
        ["content", `List blocks]))
      else None in
    { Protocol.role = "assistant"; content = (if Buffer.length text = 0 then None else Some (Buffer.contents text));
    tool_calls = calls; tool_call_id = None; tool_result_content = None; provider_state; attachments = [] }
  with Protocol.Invalid_response _ as error ->
    t.failed <- true;
    raise error
