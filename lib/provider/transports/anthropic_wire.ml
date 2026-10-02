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
        cache_creation_input_tokens; reasoning_output_tokens = None;
        input_modality_tokens = None; cached_input_modality_tokens = None;
        output_modality_tokens = None }
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
      (* Anthropic rejects combinator keys at the input_schema root; spill
         them into the description so the constraint text survives. *)
      let input_schema, spilled = match input_schema with
        | `Assoc fields ->
            let spilled, kept = List.partition (fun (key, _) ->
              List.mem key ["oneOf"; "anyOf"; "allOf"]) fields in
            if spilled = [] then input_schema, ""
            else `Assoc kept,
              String.concat "" (List.map (fun (key, value) ->
                Printf.sprintf "%s: %s" key (Yojson.Basic.to_string value))
                spilled)
        | schema -> schema, "" in
      let fields = [ "name", `String name; "input_schema", input_schema ] in
      let fields = match member "description" fn with
        | `String description ->
            let description = if spilled = "" then description
              else description ^ "\n\n" ^ spilled in
            fields @ [ "description", `String description ]
        | `Null ->
            if spilled = "" then fields
            else fields @ [ "description", `String spilled ]
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
    ?replay_assistant_content ?thinking ~model ~max_tokens messages tools =
  if model = "" || max_tokens <= 0 then invalid_arg "invalid Anthropic model or max_tokens";
  (* Thinking budget must fit inside max_tokens with headroom for the reply. *)
  let thinking_budget thinking =
    let floor = function
      | "none" -> None
      | "minimal" -> Some 1024
      | "low" -> Some 2048
      | "medium" -> Some 8192
      | "high" -> Some 16384
      | "xhigh" -> Some 32768
      | "max" -> Some 65536
      | _ -> invalid_arg "unsupported thinking level" in
    match floor thinking with
    | None -> Some (`Assoc ["type", `String "disabled"])
    | Some budget ->
        let budget = min budget (max_tokens - 4000) in
        if budget <= 0 then invalid_arg
          "max_tokens too small for the requested thinking level"
        else Some (`Assoc ["type", `String "enabled";
          "budget_tokens", `Int budget]) in
  let thinking_field = match thinking with
    | None -> None
    | Some level -> thinking_budget level in
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
             let content = match replay_assistant_content with
               | None -> `List blocks
               | Some replay ->
                   Option.value ~default:(`List blocks) (replay msg) in
             if blocks = [] && content = `List [] then
               invalid "empty assistant message";
             append (wire_message "assistant" content);
             pending := ids;
             replay rest
         | "tool" ->
             (* A whole run of tool results is one Anthropic user turn. *)
             let rec collect acc = function
              | ({ role = "tool"; tool_call_id = Some id; tool_calls = []; _ } as result) :: remaining ->
                  if not (List.mem id !pending) then invalid "unexpected or duplicate tool result";
                  pending := List.filter (( <> ) id) !pending;
                  collect (tool_result_block id
                    (tool_result_content result) :: acc) remaining
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
  (* Prompt caching anchors on the stable head: the last tool definition and
     the last system block. A top-level cache_control field is invalid, and
     cache_control on a block requires the array form of `system`. *)
  let cache = `Assoc [ "type", `String "ephemeral" ] in
  let apply_cache block = match block with
    | `Assoc fields when List.assoc_opt "cache_control" fields = None ->
        `Assoc (fields @ [ "cache_control", cache ])
    | other -> other in
  let fields = match List.rev !systems with
    | [] -> fields
    | texts ->
        if allow_prompt_caching then
          match List.rev texts with
          | last :: rest ->
              fields @ [ "system", `List (List.rev (
                apply_cache (text_block last) ::
                List.map text_block rest)) ]
          | [] -> fields
        else fields @ [ "system", `String (String.concat "\n\n" texts) ] in
  let fields = match tools with
    | [] -> fields
    | definitions ->
        let converted = List.map tool_schema definitions in
        let converted = if allow_prompt_caching then
          match List.rev converted with
          | last :: rest -> List.rev (apply_cache last :: rest)
          | [] -> converted
          else converted in
        fields @ [ "tools", `List converted ] in
  (* Rolling anchor on the newest message's last text-capable block: a
     breakpoint there caches the entire preceding conversation, which is where
     the reuse actually is. Generated reasoning and boundary blocks reject
     cache_control, so the scan walks backward past them. *)
  let fields = if not allow_prompt_caching then fields else
    let markable = function
      | `Assoc block ->
          (match List.assoc_opt "type" block with
           | Some (`String ("thinking" | "redacted_thinking" | "fallback"
               | "tool_addition" | "tool_removal")) -> false
           | _ -> List.assoc_opt "cache_control" block = None)
      | _ -> false in
    List.map (fun (key, value) ->
      if key <> "messages" then key, value else
      match value with
      | `List messages ->
          let mark content = match content with
            | `String text -> `List [ apply_cache (text_block text) ]
            | `List blocks ->
                let rec last_markable = function
                  | [] -> None
                  | (`Assoc _ as block) :: earlier when markable block ->
                      Some (List.rev (apply_cache block :: earlier))
                  | _ :: earlier -> last_markable earlier in
                (match last_markable (List.rev blocks) with
                 | Some blocks -> `List blocks
                 | None -> content)
            | _ -> content in
          let marked_last (msg : Yojson.Basic.t) = match msg with
            | `Assoc fields ->
                (match List.assoc_opt "content" fields with
                 | Some _ ->
                     `Assoc (List.map (fun (k, v) ->
                       if k = "content" then k, mark v else k, v) fields)
                 | None -> msg)
            | _ -> msg in
          key, `List (match List.rev messages with
            | last :: rest -> List.rev (marked_last last :: rest)
            | [] -> messages)
      | _ -> key, value) fields in
  let fields = match thinking_field with
    | None -> fields
    | Some value -> fields @ [ "thinking", value ] in
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
   | `String ("end_turn" | "tool_use") -> invalid "stop_reason/content mismatch"
   | `String ("pause_turn" | "stop_sequence") -> ()
   | `String ("max_tokens" | "model_context_window_exceeded" as reason) ->
       Protocol.truncated ("stop_reason " ^ reason)
   | `String ("refusal" | "sensitive" as reason) -> invalid reason
   | `String _ -> () (* New stop reasons ship server-side first; degrade to stop. *)
   | _ -> invalid "missing stop_reason");
  let content = match List.rev !texts with
    | [] -> None
    | texts -> Some (String.concat "" texts) in
  { role = "assistant"; content; tool_calls; tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] }
let native_thinking_block block =
  match member "type" block with
  | `String ("thinking" | "redacted_thinking") -> true
  | _ -> false

let native_state_tag provider model = [
  "provider", `String provider;
  "route", `String "messages";
  "model", `String model;
]

let parse_native_completion ~provider ~model json =
  let reply = parse_response json in
  match member "content" json with
  | `List blocks when List.exists native_thinking_block blocks ->
      { reply with provider_state = Some (`Assoc (
          native_state_tag provider model @ ["content", `List blocks])) }
  | _ -> reply

let replay_native_content ~provider ~model (message : message) =
  match message.provider_state with
  | Some (`Assoc fields)
    when List.for_all
      (fun (key, value) -> List.assoc_opt key fields = Some value)
      (native_state_tag provider model) ->
      (match List.assoc_opt "content" fields with
       | Some (`List blocks)
         when List.exists native_thinking_block blocks ->
           let stop_reason = if message.tool_calls = [] then "end_turn"
             else "tool_use" in
           let canonical = parse_response (`Assoc [
             "type", `String "message";
             "role", `String "assistant";
             "content", `List blocks;
             "stop_reason", `String stop_reason;
           ]) in
           if canonical.content <> message.content ||
              canonical.tool_calls <> message.tool_calls then
             invalid "native thinking state does not match visible assistant message";
           Some (`List blocks)
       | _ -> None)
  | _ -> None
