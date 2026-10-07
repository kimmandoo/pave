open Protocol

let invalid detail = raise (Invalid_response ("invalid Responses API response: " ^ detail))

let required_string name json = match member name json with
  | `String value when value <> "" -> value
  | _ -> invalid ("missing or invalid " ^ name)
let optional_token_detail reported group key total =
  match member key (member group reported) with
  | `Int count when count >= 0 && count <= total -> Some count
  | _ -> None

let usage json =
  let reported = member "usage" json in
  match member "input_tokens" reported, member "output_tokens" reported with
  | `Int input_tokens, `Int output_tokens
    when input_tokens >= 0 && output_tokens >= 0 ->
      let cached_input_tokens = optional_token_detail reported
        "input_tokens_details" "cached_tokens" input_tokens in
      let reasoning_output_tokens = optional_token_detail reported
        "output_tokens_details" "reasoning_tokens" output_tokens in
      Some { input_tokens; output_tokens; cached_input_tokens;
        cache_creation_input_tokens = None; reasoning_output_tokens;
        input_modality_tokens = None; cached_input_modality_tokens = None;
        output_modality_tokens = None }
  | _ -> None

let tool_schema json =
  let fn = member "function" json in
  match member "type" json, fn with
  | `String "function", `Assoc _ ->
      let name = required_string "name" fn in
      let parameters = member "parameters" fn in
      (match member "type" parameters with
       | `String "object" -> ()
       | _ -> invalid "tool parameters must be an object schema");
      let strict = match member "strict" fn with
        | `Null -> `Bool false
        | `Bool _ as value -> value
        | _ -> invalid "invalid strict tool setting" in
      let fields = [ "type", `String "function"; "name", `String name;
        "parameters", parameters; "strict", strict ] in
      let fields = match member "description" fn with
        | `Null -> fields
        | `String description -> fields @ [ "description", `String description ]
        | _ -> invalid "invalid tool description" in
      `Assoc fields
  | _ -> invalid "invalid tool definition"
let tool_result_output (msg : message) =
  let blocks = content_blocks_of_tool_result msg in
  if List.exists (function Image _ -> true | Text _ -> false) blocks then
    let blocks = if text_of_content_blocks blocks = "" then
      List.filter (function Image _ -> true | Text _ -> false) blocks @
        [Text "(see attached image)"] else blocks in
    `List (List.map (function
      | Text text -> `Assoc ["type", `String "input_text"; "text", `String text]
      | Image { mime_type; data } -> `Assoc ["type", `String "input_image";
          "image_url", `String ("data:" ^ mime_type ^ ";base64," ^ data)]) blocks)
  else `String (text_of_content_blocks blocks)




let native_compaction_items ~model (message : Protocol.message) =
  match message.provider_state with
  | None -> None
  | Some state ->
      if Protocol.member "provider" state <> `String "openai" ||
         Protocol.member "route" state <> `String "responses" ||
         Protocol.member "model" state <> `String model then
        invalid "native compaction state belongs to a different route or model";
      if message.content = None || message.attachments <> [] ||
         message.tool_calls <> [] || message.tool_call_id <> None ||
         message.tool_result_content <> None then
        invalid "native compaction state is malformed";
      let items = match Protocol.member "items" state with
        | `List items -> items
        | _ -> invalid "native compaction state has no replay items" in
      let valid_item item =
        match Protocol.member "type" item with
        | `String "compaction" ->
            (match Protocol.member "encrypted_content" item with
             | `String value -> value <> ""
             | _ -> false)
        | `String "compaction_summary" ->
            (match Protocol.member "summary" item with
             | `String value -> String.trim value <> ""
             | _ -> false)
        | `String "message" ->
            List.mem (Protocol.member "role" item)
              [`String "assistant"; `String "user"]
        | _ -> false in
      if items = [] || not (List.for_all valid_item items) ||
         not (List.exists (fun item ->
           match Protocol.member "type" item with
           | `String ("compaction" | "compaction_summary") -> true
           | _ -> false) items) then
        invalid "native compaction state has no valid compaction item";
      Some items


let assistant_message_item text =
  `Assoc ["type", `String "message"; "role", `String "assistant";
    "status", `String "completed";
    "content", `List [`Assoc ["type", `String "output_text";
      "text", `String text; "annotations", `List []]]]

let request ?(stream = false) ?thinking ?max_output_tokens ~model messages tools =
  if model = "" then invalid_arg "empty Responses model";
  (match thinking with
   | Some level when not (List.mem level
       ["none"; "minimal"; "low"; "medium"; "high"; "xhigh"; "max"]) ->
       invalid "unsupported reasoning effort"
   | _ -> ());
  let instructions = ref [] and input = ref [] and pending = ref [] in
  let emit json = input := json :: !input in
  List.iter (fun (msg : message) ->
    match msg.role with
    | "system" | "developer" ->
        if !pending <> [] || msg.tool_calls <> [] || msg.tool_call_id <> None then
          invalid "instructions during tool results";
        (match msg.content with
         | Some text -> instructions := text :: !instructions
         | None -> invalid "empty instructions")
    | "user" ->
        if !pending <> [] || msg.tool_calls <> [] || msg.tool_call_id <> None then
          invalid "user message during tool results";
        (match native_compaction_items ~model msg with
         | Some items -> List.iter emit items
         | None ->
             let content =
               (match msg.content with
               | Some text -> [`Assoc ["type", `String "input_text"; "text", `String text]]
               | None -> []) @
               List.map (fun (attachment : attachment) ->
                 if not (List.mem attachment.mime_type ["image/png"; "image/jpeg"; "image/webp"])
                 then invalid ("unsupported user image MIME type " ^ attachment.mime_type);
                 `Assoc ["type", `String "input_image";
                   "image_url", `String ("data:" ^ attachment.mime_type ^ ";base64," ^ attachment.data)])
                 msg.attachments in
             if content = [] then invalid "user message without content";
             emit (`Assoc ["role", `String "user"; "content", `List content]))
    | "assistant" ->
        if !pending <> [] || msg.tool_call_id <> None then
          invalid "assistant message during tool results";
        (* The text item precedes its own calls: strict gateways reject a
           message wedged between a call and its output. *)
        (match msg.content with
         | Some text when text <> "" -> emit (assistant_message_item text)
         | _ when msg.tool_calls = [] -> invalid "empty assistant message"
         | _ -> ());
        List.iter (fun (call : tool_call) ->
          if call.id = "" || call.name = "" then invalid "empty tool call id or name";
          if List.mem call.id !pending then invalid "duplicate tool call id";
          (match call.arguments with `Assoc _ -> () | _ -> invalid "tool arguments must be an object");
          pending := call.id :: !pending;
          emit (`Assoc [ "type", `String "function_call";
            "call_id", `String call.id; "name", `String call.name;
            "arguments", `String (Yojson.Basic.to_string call.arguments) ]))
          msg.tool_calls
    | "tool" ->
        (match msg.content, msg.tool_call_id, msg.tool_calls with
         | Some _, Some id, [] when List.mem id !pending ->
             pending := List.filter (( <> ) id) !pending;
             emit (`Assoc [ "type", `String "function_call_output";
               "call_id", `String id; "output", tool_result_output msg ])
         | _ -> invalid "unexpected or malformed tool result")
    | _ -> invalid "unsupported transcript role") messages;
  if !pending <> [] then invalid "missing tool results";
  let fields = [ "model", `String model; "input", `List (List.rev !input);
                 "store", `Bool false ] in
  let fields = match List.rev !instructions with
    | [] -> fields
    | texts -> fields @ [ "instructions", `String (String.concat "\n\n" texts) ] in
  let fields = match thinking with
    | None -> fields
    | Some level -> fields @ ["reasoning", `Assoc ["effort", `String level]] in
  let fields = match max_output_tokens with
    | None -> fields
    | Some tokens when tokens > 0 ->
        fields @ ["max_output_tokens", `Int tokens]
    | Some _ -> invalid "invalid max_output_tokens" in
  let fields = if tools = [] then fields else
    fields @ [ "tools", `List (List.map tool_schema tools) ] in
  `Assoc (if stream then fields @ [ "stream", `Bool true ] else fields)

let error_detail json =
  let text = function
    | `String text when String.trim text <> "" -> Some text
    | _ -> None in
  let response = member "response" json in
  let sources = [true, member "error" json; true, member "error" response;
    false, json; false, response] in
  let message = List.find_map (fun (_, value) ->
    match value with
    | `String _ -> text value
    | _ -> text (member "message" value)) sources in
  let code = List.find_map (fun (error, value) ->
    match text (member "code" value) with
    | Some _ as code -> code
    | None when error -> text (member "type" value)
    | None -> None) sources in
  match message, code with
  | Some message, Some code when message <> code ->
      Some (message ^ " (code=" ^ code ^ ")")
  | Some message, _ -> Some message
  | None, code -> code

(* Name why a response did not complete; terminal failure event types are
   authoritative even when their response object omits its status. *)
let reject_unfinished ?status ~invalid envelope =
  let response = match status, member "response" envelope with
    | Some _, (`Assoc _ as response) -> response
    | _ -> envelope in
  let incomplete () =
    match member "reason" (member "incomplete_details" response) with
    | `String "max_output_tokens" ->
        truncated "incomplete_details max_output_tokens"
    | `String reason when reason <> "" -> invalid ("incomplete response: " ^ reason)
    | _ -> invalid "incomplete response" in
  let failed prefix =
    match error_detail envelope with
    | Some detail -> invalid (prefix ^ ": " ^ detail)
    | None -> invalid prefix in
  match (match status with Some status -> `String status
    | None -> member "status" response) with
  | `String "incomplete" -> incomplete ()
  | `String "failed" -> failed "failed response"
  | _ ->
      if member "error" response <> `Null then failed "response error";
      if member "incomplete_details" response <> `Null then incomplete ()

let parse_completion json =
  reject_unfinished ~invalid json;
  (match member "status" json with
   | `String "completed" -> ()
   | _ -> invalid "response not completed");
  let outputs = match member "output" json with
    | `List outputs -> outputs
    | _ -> invalid "missing output items" in
  let texts = ref [] and calls = ref [] and ids = Hashtbl.create 4 in
  List.iter (fun item ->
    (match member "status" item with
     | `Null | `String "completed" -> ()
     | _ -> invalid "incomplete output item");
    match member "type" item with
    | `String "message" ->
        if member "role" item <> `String "assistant" then invalid "unexpected output role";
        let content = match member "content" item with
          | `List content -> content
          | _ -> invalid "missing message content" in
        List.iter (fun part -> match member "type" part with
          | `String "output_text" ->
              (match member "text" part with
               | `String text -> texts := text :: !texts
               | _ -> invalid "invalid output text")
          | `String "refusal" -> invalid "refusal"
          | `String _ -> () (* annotations and future content parts *)
          | _ -> invalid "malformed message content") content
    | `String "function_call" ->
        let id = required_string "call_id" item in
        let name = required_string "name" item in
        if Hashtbl.mem ids id then invalid "duplicate tool call id";
        Hashtbl.add ids id ();
        let args = required_string "arguments" item in
        let arguments = decode_tool_arguments args in
        calls := { id; name; arguments } :: !calls
    | `String "reasoning" -> ()
    | `String _ -> () (* server tool calls and future item kinds *)
    | _ -> invalid "malformed output item") outputs;
  let content = match List.rev !texts with
    | [] -> None | texts -> Some (String.concat "" texts) in
  if !calls = [] && (content = None || content = Some "") then
    invalid "empty assistant response";
  { role = "assistant"; content;
    tool_calls = List.rev !calls; tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] }
