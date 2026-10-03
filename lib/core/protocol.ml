type tool_call = { id : string; name : string; arguments : Yojson.Basic.t }
type tool_argument_delta = {
  key : string;
  call_id : string option;
  name : string;
  fragment : string;
}
type modality_token_count = { modality : string; token_count : int }
type usage = {
  input_tokens : int;
  output_tokens : int;
  cached_input_tokens : int option;
  cache_creation_input_tokens : int option;
  reasoning_output_tokens : int option;
  input_modality_tokens : modality_token_count list option;
  cached_input_modality_tokens : modality_token_count list option;
  output_modality_tokens : modality_token_count list option;
}



type content_block =
  | Text of string
  | Image of { mime_type : string; data : string }

type attachment = { name : string; mime_type : string; data : string }
type attachment_kind = Image_attachment | Audio_attachment | Video_attachment

let attachment_kind = function
  | "image/png" | "image/jpeg" | "image/webp" -> Some Image_attachment
  | "audio/wav" | "audio/mp3" | "audio/mpeg" | "audio/aac"
  | "audio/ogg" | "audio/flac" | "audio/m4a" | "audio/opus" ->
      Some Audio_attachment
  | "video/mp4" | "video/webm" -> Some Video_attachment
  | _ -> None


type message = {
  role : string;
  content : string option;
  tool_result_content : content_block list option;
  tool_calls : tool_call list;
  tool_call_id : string option;
  provider_state : Yojson.Basic.t option;
  attachments : attachment list;
}

exception Invalid_response of string

(* Every wire reports a reply cut off by the output-token limit with the same
   user-facing text, so callers need not know each vendor's reason code. *)
let truncated_prefix = "Response stopped at the model's output token limit"
let truncated reason =
  raise (Invalid_response (truncated_prefix ^ " (" ^ reason ^
    "). The partial reply was not added to the conversation; ask for a shorter answer or smaller steps."))

let valid_image_content mime_type data =
  String.starts_with ~prefix:"image/" mime_type &&
  String.length mime_type > String.length "image/" && data <> ""

let max_attachment_bytes = 10 * 1024 * 1024
let max_attachments = 8

let valid_attachment_mime mime_type = Option.is_some (attachment_kind mime_type)


let valid_base64 data =
  let length = String.length data in
  let value = function
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '+' | '/' -> true
    | _ -> false in
  let padding = if length > 0 && data.[length - 1] = '=' then
    if length > 1 && data.[length - 2] = '=' then 2 else 1
    else 0 in
  length > 0 && length mod 4 = 0 &&
  let rec check index =
    if index = length then true
    else if index >= length - padding then data.[index] = '='
    else value data.[index] && check (index + 1) in
  check 0

let validate_attachments attachments =
  if List.length attachments > max_attachments then
    raise (Invalid_response "too many media attachments");
  let total = List.fold_left (fun total (attachment : attachment) ->
    let { name; mime_type; data } = attachment in
    if name = "" || String.length name > 255 ||
       String.exists (fun c -> let code = Char.code c in
         code < 32 || code = 127 || c = '/' || c = '\\') name then
      raise (Invalid_response "invalid media attachment name");
    if not (valid_attachment_mime mime_type) then
      raise (Invalid_response "unsupported media attachment type");
    if String.length data > max_attachment_bytes || not (valid_base64 data) then
      raise (Invalid_response "invalid or oversized media attachment data");
    if String.length data > max_attachment_bytes - total then
      raise (Invalid_response "media attachments exceed size limit");
    total + String.length data) 0 attachments in
  ignore total

let validate_content_blocks blocks =
  List.iter (function
    | Image { mime_type; data } when valid_image_content mime_type data -> ()
    | Image _ -> raise (Invalid_response "invalid tool-result image content")
    | Text _ -> ()) blocks
let checked_token_sum left right =
  if right > max_int - left then
    raise (Invalid_response "provider token totals exceed host integer");
  left + right

let add_optional_tokens left right =
  match left, right with
  | Some left, Some right -> Some (checked_token_sum left right)
  | _ -> None
let add_modality_tokens left right =
  match left, right with
  | Some left, Some right ->
      let add counts { modality; token_count } =
        match List.find_opt (fun detail -> detail.modality = modality) counts with
        | None -> counts @ [{ modality; token_count }]
        | Some previous ->
            let token_count = checked_token_sum previous.token_count token_count in
            List.map (fun detail ->
              if detail.modality = modality then
                { detail with token_count = token_count }
              else detail) counts in
      Some (List.fold_left add left right)
  | _ -> None


let add_usage left right =
  { input_tokens = checked_token_sum left.input_tokens right.input_tokens;
    output_tokens = checked_token_sum left.output_tokens right.output_tokens;
    cached_input_tokens = add_optional_tokens left.cached_input_tokens
      right.cached_input_tokens;
    cache_creation_input_tokens = add_optional_tokens
      left.cache_creation_input_tokens right.cache_creation_input_tokens;
    reasoning_output_tokens = add_optional_tokens left.reasoning_output_tokens
      right.reasoning_output_tokens;
    input_modality_tokens = add_modality_tokens left.input_modality_tokens
      right.input_modality_tokens;
    cached_input_modality_tokens = add_modality_tokens
      left.cached_input_modality_tokens right.cached_input_modality_tokens;
    output_modality_tokens = add_modality_tokens left.output_modality_tokens
      right.output_modality_tokens }


let user ?(attachments = []) content =
  validate_attachments attachments;
  { role = "user"; content = Some content; tool_result_content = None;
    tool_calls = []; tool_call_id = None; provider_state = None; attachments }
let tool_result id content =
  { role = "tool"; content = Some content; tool_result_content = None;
    tool_calls = []; tool_call_id = Some id; provider_state = None;
    attachments = [] }
let tool_result_blocks id blocks =
  validate_content_blocks blocks;
  { role = "tool"; content = Some (String.concat "\n" (List.filter_map
      (function Text text -> Some text | Image _ -> None) blocks));
    tool_result_content = Some blocks; tool_calls = [];
    tool_call_id = Some id; provider_state = None; attachments = [] }

let content_blocks_of_tool_result (message : message) =
  match message.tool_result_content with
  | Some blocks -> blocks
  | None -> (match message.content with Some text -> [Text text] | None -> [])

let text_of_content_blocks blocks =
  String.concat "\n" (List.filter_map
    (function Text text -> Some text | Image _ -> None) blocks)

let display_content_blocks blocks =
  String.concat "\n" (List.map (function
    | Text text -> text
    | Image { mime_type; _ } -> "[" ^ mime_type ^ " image]") blocks)

let member key = function
  | `Assoc fields -> (match List.assoc_opt key fields with Some v -> v | None -> `Null)
  | _ -> `Null

let string = function `String s -> s | _ -> raise (Invalid_response "expected string")
let attachment_to_json (attachment : attachment) =
  `Assoc ["name", `String attachment.name;
    "mimeType", `String attachment.mime_type;
    "data", `String attachment.data]

let attachment_from_json json =
  let name = member "name" json |> string
  and mime_type = member "mimeType" json |> string
  and data = member "data" json |> string in
  let attachment = { name; mime_type; data } in
  validate_attachments [attachment];
  attachment

let call_to_json call =
  `Assoc [ "id", `String call.id; "type", `String "function";
    "function", `Assoc [ "name", `String call.name;
                            "arguments", `String (Yojson.Basic.to_string call.arguments) ] ]
let content_block_to_json = function
  | Text text -> `Assoc ["type", `String "text"; "text", `String text]
  | Image { mime_type; data } ->
      `Assoc ["type", `String "image"; "mimeType", `String mime_type;
        "data", `String data]

let content_block_from_json json =
  match member "type" json with
  | `String "text" -> Text (member "text" json |> string)
  | `String "image" ->
      let mime_type = member "mimeType" json |> string in
      let data = member "data" json |> string in
      if not (valid_image_content mime_type data) then
        raise (Invalid_response "invalid stored image content");
      Image { mime_type; data }
  | _ -> raise (Invalid_response "invalid stored tool-result content block")

let message_to_json ?(stored = false) (msg : message) =
  (match msg.tool_result_content with
   | None -> ()
   | Some blocks ->
       validate_content_blocks blocks;
       if msg.role <> "tool" || msg.tool_calls <> [] ||
          msg.tool_call_id = None || msg.provider_state <> None ||
          msg.content <> Some (text_of_content_blocks blocks) then
         raise (Invalid_response "invalid tool-result content"));
  validate_attachments msg.attachments;
  if msg.attachments <> [] &&
     (msg.role <> "user" || msg.tool_result_content <> None ||
      msg.tool_calls <> [] || msg.tool_call_id <> None ||
      msg.provider_state <> None) then
    raise (Invalid_response "media attachments require a plain user message");
  if not stored && List.exists (fun (attachment : attachment) ->
      attachment_kind attachment.mime_type <> Some Image_attachment)
      msg.attachments then
    raise (Invalid_response
      "audio/video attachments require provider-native media transport");

  let fields = [ "role", `String msg.role ] in
  let fields = match msg.content, stored, msg.attachments with
    | None, _, [] | None, true, _ -> fields
    | Some text, true, _ -> fields @ ["content", `String text]
    | Some text, false, [] -> fields @ ["content", `String text]
    | content, false, attachments ->
        let text = match content with Some text when text <> "" ->
          [`Assoc ["type", `String "text"; "text", `String text]]
        | _ -> [] in
        let images = List.map (fun (attachment : attachment) ->
          `Assoc ["type", `String "image_url";
            "image_url", `Assoc ["url", `String
              ("data:" ^ attachment.mime_type ^ ";base64," ^ attachment.data)]])
          attachments in
        fields @ ["content", `List (text @ images)] in

  let fields = match stored, msg.tool_result_content with
    | true, Some blocks ->
        fields @ ["tool_result_content", `List (List.map content_block_to_json blocks)]
    | _ -> fields in
  let fields = if msg.tool_calls = [] then fields
    else fields @ [ "tool_calls", `List (List.map call_to_json msg.tool_calls) ] in
  let fields = match msg.tool_call_id with None -> fields
    | Some id -> fields @ [ "tool_call_id", `String id ] in
  let fields = match stored, msg.provider_state with
    | true, Some state -> fields @ [ "provider_state", state ]
    | _ -> fields in
  `Assoc fields


let chat_messages_to_json ?serialize messages =
  let serialize = match serialize with
    | Some serialize -> serialize
    | None -> (fun message -> message_to_json message) in
  let with_content json content =
    match json with
    | `Assoc fields ->
        let found = List.mem_assoc "content" fields in
        let fields = List.map (fun (key, value) ->
          if key = "content" then key, content else key, value) fields in
        `Assoc (if found then fields else fields @ ["content", content])
    | _ -> json in
  let image_message blocks =
    let images = List.filter_map (function
      | Image { mime_type; data } ->
          Some (`Assoc ["type", `String "image_url";
            "image_url", `Assoc ["url", `String (
              "data:" ^ mime_type ^ ";base64," ^ data)]])
      | Text _ -> None) blocks in
    `Assoc [
      "role", `String "user";
      "content", `List (
        `Assoc ["type", `String "text";
          "text", `String "Attached image(s) from tool result:"] :: images)
    ] in
  let rec collect reversed images_rev = function
    | ({ role = "tool"; _ } as message) :: rest ->
        let blocks = content_blocks_of_tool_result message in
        validate_content_blocks blocks;
        let images = List.filter (function Image _ -> true | Text _ -> false) blocks in
        let json = serialize message in
        let json = if images = [] then json else
          let text = text_of_content_blocks blocks in
          with_content json (`String (
            if text = "" then "(see attached image)" else text)) in
        collect (json :: reversed) (List.rev_append images images_rev) rest
    | rest -> reversed, images_rev, rest in
  let rec encode reversed = function
    | [] -> List.rev reversed
    | ({ role = "tool"; _ } :: _ as messages) ->
        let reversed, images_rev, rest = collect reversed [] messages in
        let reversed = match images_rev with
          | [] -> reversed
          | _ -> image_message (List.rev images_rev) :: reversed in
        encode reversed rest
    | message :: rest -> encode (serialize message :: reversed) rest
in
  let has_images = List.exists (fun message ->
    message.role = "tool" &&
    (match message.tool_result_content with
     | Some blocks -> List.exists (function Image _ -> true | Text _ -> false) blocks
     | None -> false)) messages in
  `List (if has_images then encode [] messages
    else List.map serialize messages)



(* Models sometimes emit malformed arguments. Carry them to the tool loop as an
   error marker so the model can resend, rather than failing the whole turn. *)
let invalid_arguments_key = "__pave_invalid_arguments"

let decode_tool_arguments raw =
  let parse text = try Some (Yojson.Basic.from_string text) with Yojson.Json_error _ -> None in
  let rec as_object depth text =
    match parse text with
    | Some (`Assoc _ as arguments) -> Some arguments
    | Some (`String inner) when depth < 2 && String.trim inner <> "" -> as_object (depth + 1) inner
    | Some (`String _) -> Some (`Assoc [])
    | Some _ -> None
    | None when depth = 0 ->
        (* Recover one object wrapped in prose or a Markdown fence. *)
        (match String.index_opt text '{', String.rindex_opt text '}' with
         | Some first, Some last when last > first ->
             (match parse (String.sub text first (last - first + 1)) with
              | Some (`Assoc _ as arguments) -> Some arguments
              | _ -> None)
         | _ -> None)
    | None -> None in
  if String.trim raw = "" then `Assoc []
  else match as_object 0 raw with
    | Some arguments -> arguments
    | None ->
        let shown = if String.length raw > 512 then String.sub raw 0 512 ^ "..." else raw in
        `Assoc [invalid_arguments_key, `String shown]

let parse_call json =
  let id = member "id" json |> string in
  let fn = member "function" json in
  let name = member "name" fn |> string in
  (* Compatible servers omit or blank the arguments of parameterless tools. *)
  let arguments = match member "arguments" fn with
    | `Null -> `Assoc []
    | `Assoc _ as arguments -> arguments
    | value -> decode_tool_arguments (string value) in
  if id = "" || name = "" then raise (Invalid_response "empty tool call id or name");
  { id; name; arguments }

let parse_calls = function
  | `Null -> []
  | `List calls ->
      let calls = List.map parse_call calls in
      let ids = List.map (fun call -> call.id) calls in
      if List.length (List.sort_uniq String.compare ids) <> List.length ids then
        raise (Invalid_response "duplicate tool call id");
      calls
  | _ -> raise (Invalid_response "invalid tool_calls")

let parse_message json =
  let content = match member "content" json with
    | `Null -> None | `String s -> Some s
    | _ -> raise (Invalid_response "invalid assistant content") in
  let tool_calls = parse_calls (member "tool_calls" json) in
  { role = "assistant"; content; tool_result_content = None;
    tool_calls; tool_call_id = None; provider_state = None; attachments = [] }

let message_from_json json =
  let role = member "role" json |> string in
  let content = match member "content" json with
    | `Null -> None | `String s -> Some s | _ -> raise (Invalid_response "invalid content") in
  let tool_result_content = match member "tool_result_content" json with
    | `Null -> None
    | `List blocks -> Some (List.map content_block_from_json blocks)
    | _ -> raise (Invalid_response "invalid tool-result content") in
  let tool_calls = parse_calls (member "tool_calls" json) in
  let tool_call_id = match member "tool_call_id" json with
    | `Null -> None | `String id -> Some id | _ -> raise (Invalid_response "invalid tool_call_id") in
  let provider_state = match member "provider_state" json with
    | `Null -> None
    | (`Assoc _ as state) -> Some state
    | _ -> raise (Invalid_response "invalid provider state") in
  (match role, content, tool_result_content, tool_calls, tool_call_id, provider_state with
  | "user", Some _, None, [], None, None
  | "assistant", _, None, _, None, _
  | "tool", Some _, _, [], Some _, None ->
      (match tool_result_content with
       | Some blocks when content <> Some (text_of_content_blocks blocks) ->
           raise (Invalid_response "stored tool-result text differs from its content blocks")
       | _ -> ())
  | _ -> raise (Invalid_response "invalid stored message"));
  { role; content; tool_result_content; tool_calls; tool_call_id;
    provider_state; attachments = [] }

let parse_completion json =
  match member "choices" json with
  | `List [choice] ->
      let member name value = match value with
        | `Assoc _ -> member name value
        | _ -> raise (Invalid_response ("invalid " ^ name ^ " object")) in
      (match member "index" choice with
       | `Null | `Int 0 -> ()
       | `Int _ -> raise (Invalid_response "unexpected choice index")
       | _ -> raise (Invalid_response "invalid choice index"));
      let response = member "message" choice in
      (match member "role" response with
       | `String "assistant" -> ()
       | _ -> raise (Invalid_response "completion message role is not assistant"));
      let refusal = member "refusal" response in
      (match refusal with
       | `Null | `String "" -> ()
       | `String _ -> raise (Invalid_response "assistant refusal")
       | _ -> raise (Invalid_response "invalid refusal"));
      let msg = parse_message response in
      let finish = member "finish_reason" choice in
      (* Normalize documented compatible spellings, but never turn a terminal
         text-only outcome into permission to execute tool calls. *)
      let normalized = match finish with
        | `String reason -> String.lowercase_ascii reason
        | _ -> "" in
      (match normalized with
      | "stop" | "end" when msg.tool_calls = [] -> msg
      | "tool_calls" | "function_call" when msg.tool_calls <> [] -> msg
      | "stop" | "end" | "tool_calls" | "function_call" ->
          raise (Invalid_response "finish_reason/tool calls mismatch")
      | "length" | "max_tokens" -> truncated ("finish_reason " ^ normalized)
      | "error" | "insufficient_system_resource" ->
          raise (Invalid_response ("provider returned error finish_reason" ^
            (if normalized = "error" then "" else ": " ^ normalized)))
      | "" -> raise (Invalid_response "missing finish_reason")
      | reason -> raise (Invalid_response ("provider finish_reason: " ^ reason)))
  | `List [] -> raise (Invalid_response "missing choices")
  | `List _ -> raise (Invalid_response "multiple choices")
  | _ -> raise (Invalid_response "missing choices")

let optional_token_detail json key total =
  match member key json with
  | `Int count when count >= 0 && count <= total -> Some count
  | _ -> None
let parse_modality_tokens json key total =
  let valid_name name =
    name <> "" && String.length name <= 64 &&
    String.for_all (function
      | 'A'..'Z' | '0'..'9' | '_' -> true
      | _ -> false) name in
  match member key json with
  | `List values ->
      let seen = Hashtbl.create 4 in
      let rec parse = function
        | [] -> Some []
        | item :: rest ->
            (match member "modality" item, member "tokenCount" item with
             | `String modality, `Int token_count
               when valid_name modality && token_count >= 0 &&
                    token_count <= total && not (Hashtbl.mem seen modality) ->
                 Hashtbl.add seen modality ();
                 Option.map (fun tail ->
                   { modality; token_count } :: tail) (parse rest)
             | _ -> None) in
      (match parse values with
       | None -> None
       | Some parsed ->
           let rec sum count = function
             | [] -> Some parsed
             | item :: rest when item.token_count <= total - count ->
                 sum (count + item.token_count) rest
             | _ -> None in
           sum 0 parsed)
  | `Null -> None
  | _ -> None


let completion_usage json =
  let reported = member "usage" json in
  match member "prompt_tokens" reported, member "completion_tokens" reported with
  | `Int input_tokens, `Int output_tokens
    when input_tokens >= 0 && output_tokens >= 0 ->
    (* Compatible hosts spell cache reads differently: DeepSeek reports
       prompt_cache_hit_tokens; others a top-level cached_tokens or the
       Gemini-style cachedContentTokenCount. *)
    let cached_input_tokens =
      match optional_token_detail
          (member "prompt_tokens_details" reported) "cached_tokens" input_tokens with
      | Some _ as hit -> hit
      | None ->
          let nonneg = function `Int value when value >= 0 -> Some value
            | _ -> None in
          List.find_map (fun value -> match value with
            | Some value when value <= input_tokens -> Some value
            | _ -> None)
            [ nonneg (member "prompt_cache_hit_tokens" reported);
              nonneg (member "cached_tokens" reported);
              nonneg (member "cachedContentTokenCount" reported) ] in
    let reasoning_output_tokens = optional_token_detail
        (member "completion_tokens_details" reported) "reasoning_tokens" output_tokens in
    Some { input_tokens; output_tokens; cached_input_tokens;
      cache_creation_input_tokens = None; reasoning_output_tokens;
      input_modality_tokens = None; cached_input_modality_tokens = None;
      output_modality_tokens = None }
  | _ -> None

(* Replay hardening: model turns can leave malformed tool calls (empty id or
   name), duplicate call ids, or results that never paired. Providers reject
   such transcripts and wedge the session in an error loop, so every request
   is sanitized first:
     - drop unsigned calls with blank id/name and their paired results;
     - rewrite repeated unsigned call IDs, pairing their results;
     - preserve native signed calls verbatim or reject an invalid turn;
     - a call never followed by a result gains a synthetic "No result provided";
     - drop results whose call ID is absent or already paired in this turn. *)
let sanitize_messages (messages : message list) : message list =
  let malformed (call : tool_call) =
    String.trim call.id = "" || String.trim call.name = "" in
  let drop_queue : (string, bool Queue.t) Hashtbl.t = Hashtbl.create 8 in
  let stage1 = List.filter_map (fun (msg : message) ->
    match msg.role with
    | "assistant" ->
        Hashtbl.reset drop_queue;
        let calls = List.map (fun (call : tool_call) ->
          let bad = malformed call in
          let queue = match Hashtbl.find_opt drop_queue call.id with
            | Some queue -> queue
            | None -> let queue = Queue.create () in
              Hashtbl.add drop_queue call.id queue; queue in
          Queue.add bad queue; bad, call) msg.tool_calls in
        let kept = List.filter_map (fun (bad, call) ->
          if bad then None else Some call) calls in
        if msg.provider_state <> None && kept <> msg.tool_calls then
          raise (Invalid_response "cannot sanitize signed native tool calls");
        if kept = [] && (msg.content = None || msg.content = Some "") &&
           msg.provider_state = None then None
        else Some (if List.length kept = List.length calls then msg
          else { msg with tool_calls = kept })
    | "tool" ->
        (match msg.tool_call_id with
         | Some id ->
             (match Hashtbl.find_opt drop_queue id with
              | Some queue when not (Queue.is_empty queue) ->
                  if Queue.pop queue then None else Some msg
              | _ -> Some msg)
         | None -> Some msg)
    | _ -> Hashtbl.reset drop_queue; Some msg) messages in
  (* Rename duplicate call ids; enqueue the mapping for the next result that
     carries the original id. *)
  let seen : (string, int) Hashtbl.t = Hashtbl.create 8 in
  let rename_map : (string, string option Queue.t) Hashtbl.t = Hashtbl.create 8 in
  let suffix id n =
    let mark = "_dup" ^ string_of_int n in
    let base = if String.length id + String.length mark <= 64 then id
      else String.sub id 0 (64 - String.length mark) in
    base ^ mark in
  let rec fresh id n =
    if not (Hashtbl.mem seen id) then id
    else if not (Hashtbl.mem seen (suffix id n)) then suffix id n
    else fresh id (n + 1) in
  let enqueue id replacement =
    let queue = match Hashtbl.find_opt rename_map id with
      | Some queue -> queue
      | None -> let queue = Queue.create () in
        Hashtbl.add rename_map id queue; queue in
    Queue.add replacement queue in
  let stage2 = List.map (fun (msg : message) ->
    match msg.role with
    | "assistant" ->
        (* A new turn clears the leftover rewrite queues: an older turn's
           expected result never arrived, so a later real result belongs to
           this turn. Calls inside one turn must keep their queued order. *)
        Hashtbl.iter (fun _ queue -> Queue.clear queue) rename_map;
        if msg.provider_state <> None then (
          (* Native replay binds the model-issued IDs to opaque state. *)
          let ids = Hashtbl.create (List.length msg.tool_calls) in
          List.iter (fun (call : tool_call) ->
            if Hashtbl.mem ids call.id then
              raise (Invalid_response "duplicate native tool call id");
            Hashtbl.add ids call.id ();
            Hashtbl.replace seen call.id 1;
            enqueue call.id None) msg.tool_calls;
          msg)
        else
          let calls = List.map (fun (call : tool_call) ->
            match Hashtbl.find_opt seen call.id with
            | None -> Hashtbl.add seen call.id 1; enqueue call.id None; call
            | Some count ->
                let id = fresh call.id count in
                Hashtbl.replace seen call.id (count + 1);
                Hashtbl.add seen id 1;
                enqueue call.id (Some id);
                { call with id }) msg.tool_calls in
          if calls = msg.tool_calls then msg else { msg with tool_calls = calls }
    | "tool" ->
        (match msg.tool_call_id with
         | Some id ->
             (match Hashtbl.find_opt rename_map id with
              | Some queue when not (Queue.is_empty queue) ->
                  (match Queue.pop queue with
                   | Some replacement -> { msg with tool_call_id = Some replacement }
                   | None -> msg)
              | _ -> msg)
         | None -> msg)
    | _ -> msg) stage1 in
  (* Ensure each surviving call is followed by exactly one result: pull the
     earliest unconsumed real result located after the call's turn forward,
     synthesize when none exists, and drop results that never pair. *)
  let real : (string, (int * message) list ref) Hashtbl.t = Hashtbl.create 8 in
  List.iteri (fun index (msg : message) ->
    match msg.role, msg.tool_call_id with
    | "tool", Some id ->
        (match Hashtbl.find_opt real id with
         | Some entries -> entries := !entries @ [index, msg]
         | None -> Hashtbl.add real id (ref [index, msg]))
    | _ -> ()) stage2;
  let consumed : (string * int, unit) Hashtbl.t = Hashtbl.create 8 in
  let pick ~after id =
    match Hashtbl.find_opt real id with
    | None -> None
    | Some entries ->
        let rec scan = function
          | [] -> None
          | (index, msg) :: rest ->
              if index <= after || Hashtbl.mem consumed (id, index)
              then scan rest
              else (Hashtbl.add consumed (id, index) (); Some msg) in
        scan !entries in
  let resolved : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let out : message Queue.t = Queue.create () in
  let pending : tool_call list ref = ref [] and pending_at = ref (-1) in
  let flush () =
    List.iter (fun (call : tool_call) ->
      if not (Hashtbl.mem resolved call.id) then (
        Hashtbl.add resolved call.id ();
        match pick ~after:!pending_at call.id with
        | Some msg -> Queue.add msg out
        | None ->
            Queue.add (tool_result_blocks call.id
              [Text "No result provided"]) out)) !pending;
    pending := [] in
  let at_head id = match !pending with
    | (call : tool_call) :: _ when call.id = id -> true
    | _ -> false in
  List.iteri (fun index (msg : message) ->
    match msg.role, msg.tool_call_id with
    | "assistant", _ ->
        flush ();
        Hashtbl.reset resolved;
        if msg.tool_calls <> [] then (pending := msg.tool_calls; pending_at := index);
        Queue.add msg out
    | "tool", Some id ->
        if at_head id && not (Hashtbl.mem consumed (id, index)) then (
          Hashtbl.add consumed (id, index) ();
          Hashtbl.add resolved id ();
          Queue.add msg out;
          pending := List.tl !pending;
          flush ())
        else if not (Hashtbl.mem consumed (id, index)) then
          ()  (* orphan: call missing, already paired, or out of order *)
    | "tool", None -> ()
    | _ ->
        flush ();
        Queue.add msg out) stage2;
  flush ();
  List.of_seq (Queue.to_seq out)
