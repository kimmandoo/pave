type tool_call = { id : string; name : string; arguments : Yojson.Basic.t }

type usage = {
  input_tokens : int;
  output_tokens : int;
  cached_input_tokens : int option;
  cache_creation_input_tokens : int option;
  reasoning_output_tokens : int option;
}
type content_block =
  | Text of string
  | Image of { mime_type : string; data : string }

type attachment = { name : string; mime_type : string; data : string }

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

let valid_image_content mime_type data =
  String.starts_with ~prefix:"image/" mime_type &&
  String.length mime_type > String.length "image/" && data <> ""

let max_attachment_bytes = 10 * 1024 * 1024
let max_attachments = 8

let valid_attachment_mime = function
  | "image/png" | "image/jpeg" | "image/webp" -> true
  | _ -> false

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
    raise (Invalid_response "too many image attachments");
  let total = List.fold_left (fun total (attachment : attachment) ->
    let { name; mime_type; data } = attachment in
    if name = "" || String.length name > 255 ||
       String.exists (fun c -> let code = Char.code c in
         code < 32 || code = 127 || c = '/' || c = '\\') name then
      raise (Invalid_response "invalid image attachment name");
    if not (valid_attachment_mime mime_type) then
      raise (Invalid_response "unsupported image attachment type");
    if String.length data > max_attachment_bytes || not (valid_base64 data) then
      raise (Invalid_response "invalid or oversized image attachment data");
    if String.length data > max_attachment_bytes - total then
      raise (Invalid_response "image attachments exceed size limit");
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

let add_usage left right =
  { input_tokens = checked_token_sum left.input_tokens right.input_tokens;
    output_tokens = checked_token_sum left.output_tokens right.output_tokens;
    cached_input_tokens = add_optional_tokens left.cached_input_tokens
      right.cached_input_tokens;
    cache_creation_input_tokens = add_optional_tokens
      left.cache_creation_input_tokens right.cache_creation_input_tokens;
    reasoning_output_tokens = add_optional_tokens left.reasoning_output_tokens
      right.reasoning_output_tokens }

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
    raise (Invalid_response "image attachments require a plain user message");
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



let parse_call json =
  let id = member "id" json |> string in
  let fn = member "function" json in
  let name = member "name" fn |> string in
  let args = member "arguments" fn |> string in
  let arguments = try Yojson.Basic.from_string args
    with Yojson.Json_error _ -> raise (Invalid_response "invalid function arguments JSON") in
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
      (match finish with
      | `String "stop" when msg.tool_calls = [] -> msg
      | `String "tool_calls" when msg.tool_calls <> [] -> msg
      | `String reason -> raise (Invalid_response ("unexpected finish_reason: " ^ reason))
      | _ -> raise (Invalid_response "missing finish_reason"))
  | `List [] -> raise (Invalid_response "missing choices")
  | `List _ -> raise (Invalid_response "multiple choices")
  | _ -> raise (Invalid_response "missing choices")

let optional_token_detail json key total =
  match member key json with
  | `Int count when count >= 0 && count <= total -> Some count
  | _ -> None

let completion_usage json =
  let reported = member "usage" json in
  match member "prompt_tokens" reported, member "completion_tokens" reported with
  | `Int input_tokens, `Int output_tokens
    when input_tokens >= 0 && output_tokens >= 0 ->
      let cached_input_tokens = optional_token_detail
        (member "prompt_tokens_details" reported) "cached_tokens" input_tokens in
      let reasoning_output_tokens = optional_token_detail
        (member "completion_tokens_details" reported) "reasoning_tokens" output_tokens in
      Some { input_tokens; output_tokens; cached_input_tokens;
        cache_creation_input_tokens = None; reasoning_output_tokens }
  | _ -> None
