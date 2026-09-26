open Protocol

let invalid detail = raise (Invalid_response ("invalid Anthropic response: " ^ detail))

let required_string name json =
  match member name json with
  | `String value -> value
  | _ -> invalid ("missing or invalid " ^ name)
let input_usage reported =
  let cache key = match member key reported with
    | `Null -> Ok None
    | `Int count when count >= 0 -> Ok (Some count)
    | _ -> Error () in
  match member "input_tokens" reported,
    cache "cache_creation_input_tokens",
    cache "cache_read_input_tokens" with
  | `Int input, Ok created, Ok read when input >= 0 ->
      let created_count = Option.value ~default:0 created
      and read_count = Option.value ~default:0 read in
      if created_count > max_int - input ||
         read_count > max_int - input - created_count then
        invalid "input token total exceeds host integer";
      Some (input + created_count + read_count, created, read)
  | _ -> None

let output_usage reported =
  match member "output_tokens" reported with
  | `Int count when count >= 0 -> Some count
  | _ -> None

let usage json =
  let reported = member "usage" json in
  match input_usage reported, output_usage reported with
  | Some (input_tokens, cache_creation_input_tokens, cached_input_tokens),
      Some output_tokens ->
      Some { input_tokens; output_tokens; cached_input_tokens;
        cache_creation_input_tokens; reasoning_output_tokens = None }
  | _ -> None


let text_block text = `Assoc [ "type", `String "text"; "text", `String text ]

let call_block (call : tool_call) =
  `Assoc [ "type", `String "tool_use"; "id", `String call.id;
           "name", `String call.name; "input", call.arguments ]

let image_block mime_type data =
  if not (List.mem mime_type ["image/jpeg"; "image/png"; "image/gif"; "image/webp"]) then
    invalid ("unsupported tool result image MIME type " ^ mime_type);
  `Assoc [ "type", `String "image";
    "source", `Assoc [ "type", `String "base64";
      "media_type", `String mime_type; "data", `String data ] ]

let tool_result_content (message : message) =
  match message.tool_result_content with
  | None ->
      (match message.content with
       | Some text -> `String text
       | None -> invalid "malformed tool result")
  | Some _ ->
      let blocks = content_blocks_of_tool_result message in
      if blocks = [] then invalid "malformed tool result";
      let blocks = if text_of_content_blocks blocks = "" &&
        List.exists (function Image _ -> true | Text _ -> false) blocks then
        [Text "(see attached image)"] @
          List.filter (function Image _ -> true | Text _ -> false) blocks
        else blocks in
      `List (List.map (function
        | Text text -> text_block text
        | Image { mime_type; data } -> image_block mime_type data) blocks)

let tool_result_block id content =
  `Assoc [ "type", `String "tool_result"; "tool_use_id", `String id;
           "content", content ]

let wire_message role content =
  `Assoc [ "role", `String role; "content", content ]

let tool_schema json =
  let fn = member "function" json in
  match member "type" json, fn with
  | `String "function", `Assoc _ ->
      let name = required_string "name" fn in
      let input_schema = member "parameters" fn in
      if name = "" || member "type" input_schema <> `String "object" then
        invalid "tool definition must have a name and object parameters";
      let fields = [ "name", `String name; "input_schema", input_schema ] in
      let fields = match member "description" fn with
        | `String description -> fields @ [ "description", `String description ]
        | `Null -> fields
        | _ -> invalid "invalid tool description" in
      `Assoc fields
  | _ -> invalid "invalid tool definition"

let compaction_beta = "compact-2026-09-04"
let max_compaction_summary_bytes = 65_536
let max_compaction_signature_bytes = 16_384

let compaction_payload ~model (message : message) =
  match message.provider_state with
  | Some state when member "provider" state = `String "anthropic" ->
      if message.role <> "user" ||
         member "route" state <> `String "messages" ||
         member "model" state <> `String model then
        invalid "native compaction state belongs to a different route or model";
      let content = required_string "content" state in
      let signature = required_string "signature" state in
      if String.trim content = "" ||
         String.length content > max_compaction_summary_bytes ||
         signature = "" ||
         String.length signature > max_compaction_signature_bytes ||
         message.content <> Some content || message.attachments <> [] ||
         message.tool_calls <> [] || message.tool_call_id <> None ||
         message.tool_result_content <> None then
        invalid "malformed native compaction state";
      Some (content, signature)
  | _ -> None

let requires_compaction_beta ~model messages =
  List.exists (fun (message : message) ->
    Option.is_some (compaction_payload ~model message)) messages

let compaction_state ~model ~content ~signature =
  `Assoc [
    "provider", `String "anthropic";
    "route", `String "messages";
    "model", `String model;
    "content", `String content;
    "signature", `String signature ]

let compaction_block content signature =
  `Assoc [
    "type", `String "compaction";
    "content", `String content;
    "signature", `String signature ]

let request ?(allow_compaction = false) ?(allow_prompt_caching = false)
    ~model ~max_tokens messages tools =
  if model = "" || max_tokens <= 0 then invalid_arg "invalid Anthropic model or max_tokens";
  let systems = ref [] in
  let wire = ref [] in
  let pending = ref [] in
  let append msg = wire := msg :: !wire in
  let rec replay = function
    | [] -> if !pending <> [] then invalid "missing tool results"
    | (msg : message) :: rest ->
        (match msg.role with
         | "system" ->
             if !pending <> [] || msg.tool_calls <> [] || msg.tool_call_id <> None then
               invalid "system message during tool results";
             (match msg.content with
              | Some text -> systems := text :: !systems
              | None -> invalid "system message without content");
             replay rest
         | "user" ->
             if !pending <> [] || msg.tool_calls <> [] || msg.tool_call_id <> None then
               invalid "user message during tool results";
             (match (if allow_compaction then
                 compaction_payload ~model msg else None) with
              | Some _ when !wire <> [] ->
                  invalid "native compaction block must lead the message list"
              | Some (content, signature) ->
                  append (wire_message "assistant"
                    (`List [compaction_block content signature]));
                  replay rest
              | None ->
                  let content = match msg.attachments with
                    | [] ->
                        (match msg.content with
                         | Some text -> `String text
                         | None -> invalid "user message without content")
                    | attachments ->
                        let blocks = (match msg.content with
                          | Some text when text <> "" -> [text_block text]
                          | _ -> []) @ List.map
                            (fun (attachment : attachment) ->
                              if not (List.mem attachment.mime_type
                                ["image/png"; "image/jpeg"; "image/webp"]) then
                                invalid ("unsupported user image MIME type " ^
                                  attachment.mime_type);
                              image_block attachment.mime_type attachment.data)
                            attachments in
                        `List blocks in
                  append (wire_message "user" content);
                  replay rest)
         | "assistant" ->
             if !pending <> [] || msg.tool_call_id <> None then
               invalid "assistant message during tool results";
             let ids = List.map (fun (call : tool_call) ->
               if call.id = "" || call.name = "" then invalid "empty tool use id or name";
               (match call.arguments with `Assoc _ -> () | _ -> invalid "tool input must be an object");
               call.id) msg.tool_calls in
             if List.length ids <> List.length (List.sort_uniq String.compare ids) then
               invalid "duplicate tool use id";
             let blocks = (match msg.content with
               | Some text when text <> "" -> [ text_block text ]
               | _ -> []) @ List.map call_block msg.tool_calls in
             if blocks = [] then invalid "empty assistant message";
             append (wire_message "assistant" (`List blocks));
             pending := ids;
             replay rest
         | "tool" ->
             (* A whole run of tool results is one Anthropic user turn. *)
             let rec collect acc = function
              | ({ role = "tool"; tool_call_id = Some id; tool_calls = []; _ } as result) :: remaining ->
                  if not (List.mem id !pending) then invalid "unexpected or duplicate tool result";
                  pending := List.filter (( <> ) id) !pending;
                  collect (tool_result_block id (tool_result_content result) :: acc) remaining
               | ({ role = "tool"; _ } : message) :: _ -> invalid "malformed tool result"
               | remaining ->
                   if !pending <> [] then invalid "missing tool results";
                   append (wire_message "user" (`List (List.rev acc)));
                   replay remaining
             in
             collect [] (msg :: rest)
         | _ -> invalid "unsupported transcript role")
  in
  replay messages;
  let fields = [ "model", `String model; "max_tokens", `Int max_tokens;
                 "messages", `List (List.rev !wire) ] in
  let fields = match List.rev !systems with
    | [] -> fields
    | texts -> fields @ [ "system", `String (String.concat "\n\n" texts) ] in
  let fields = match tools with
    | [] -> fields
    | definitions -> fields @ [ "tools", `List (List.map tool_schema definitions) ] in
  let fields = if allow_prompt_caching then
    fields @ [ "cache_control", `Assoc [ "type", `String "ephemeral" ] ]
    else fields in
  `Assoc fields

let compaction_request ?(allow_prompt_caching = false)
    ~model ~max_tokens ~instructions messages tools =
  match request ~allow_compaction:true ~allow_prompt_caching
      ~model ~max_tokens messages tools with
  | `Assoc fields ->
      `Assoc (fields @ ["compaction", `Assoc [
        "type", `String "summarize";
        "instructions", `String instructions]])
  | _ -> assert false

let parse_compaction_response json =
  if member "type" json <> `String "message" ||
     member "role" json <> `String "assistant" then
    invalid "compaction response is not an assistant message";
  if member "stop_reason" json <> `String "compaction" then
    invalid "compaction response did not stop with compaction";
  let blocks = match member "content" json with
    | `List blocks -> blocks
    | _ -> invalid "compaction response has no content blocks" in
  match blocks with
  | [block] when member "type" block = `String "compaction" ->
      let content = required_string "content" block in
      let signature = required_string "signature" block in
      if String.trim content = "" ||
         String.length content > max_compaction_summary_bytes ||
         signature = "" ||
         String.length signature > max_compaction_signature_bytes then
        invalid "compaction response has an invalid signed block";
      content, signature
  | _ -> invalid "compaction response must contain one signed compaction block"

let parse_response json =
  if member "type" json <> `String "message" then
    invalid "response is not a message";
  if member "role" json <> `String "assistant" then
    invalid "response is not an assistant message";
  let blocks = match member "content" json with
    | `List blocks -> blocks
    | _ -> invalid "missing content blocks" in
  let texts = ref [] and calls = ref [] in
  List.iter (fun block ->
    match member "type" block with
    | `String "text" -> texts := required_string "text" block :: !texts
    | `String "tool_use" ->
        let id = required_string "id" block in
        let name = required_string "name" block in
        let arguments = member "input" block in
        if id = "" || name = "" then invalid "empty tool use id or name";
        (match arguments with `Assoc _ -> () | _ -> invalid "tool input must be an object");
        calls := { id; name; arguments } :: !calls
    | `String ("thinking" | "redacted_thinking") -> ()
    | `String "refusal" -> invalid "refusal"
    | `String block_type -> invalid ("unsupported content block: " ^ block_type)
    | _ -> invalid "missing content block type") blocks;
  let tool_calls = List.rev !calls in
  let ids = List.map (fun (call : tool_call) -> call.id) tool_calls in
  if List.length ids <> List.length (List.sort_uniq String.compare ids) then
    invalid "duplicate tool use id";
  (match member "stop_reason" json with
   | `String "end_turn" when tool_calls = [] -> ()
   | `String "tool_use" when tool_calls <> [] -> ()
   | `String "max_tokens" -> invalid "max_tokens (truncated response)"
   | `String "refusal" -> invalid "refusal"
   | `String reason -> invalid ("unexpected stop_reason: " ^ reason)
   | _ -> invalid "missing stop_reason");
  let content = match List.rev !texts with
    | [] -> None
    | texts -> Some (String.concat "" texts) in
  { role = "assistant"; content; tool_calls; tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] }
