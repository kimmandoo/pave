open Protocol

let invalid detail = raise (Invalid_response ("invalid Bedrock Converse response: " ^ detail))
let required_string name json = match member name json with
  | `String value -> value
  | _ -> invalid ("missing or invalid " ^ name)

let encode_component text =
  let b = Buffer.create (String.length text) in
  String.iter (fun c -> match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' -> Buffer.add_char b c
    | _ -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c))) text;
  Buffer.contents b

type target = {
  url : string;
  host : string;
  path : string;
  query : (string * string) list;
}

(* A caller can choose the AWS regional endpoint or a numeric loopback fixture.
   Arbitrary remote base URLs would disclose the SigV4 authorization header. *)
let endpoint ?base_url ~region ~model () =
  ignore (Aws_auth.region ~getenv:(fun _ -> Some region) ());
  if model = "" || String.length model > 2048 then invalid_arg "invalid Bedrock model ID";
  let official = "https://bedrock-runtime." ^ region ^ ".amazonaws.com" in
  let base = Option.value base_url ~default:official in
  let host = if base = official then
      "bedrock-runtime." ^ region ^ ".amazonaws.com"
    else if String.starts_with ~prefix:"http://127.0.0.1:" base then (
      let suffix = String.sub base 17 (String.length base - 17) in
      let port = try int_of_string suffix with Failure _ -> 0 in
      if port < 1 || port > 65535 || string_of_int port <> suffix then
        invalid_arg "Bedrock fixture must use a numeric loopback host and port";
      "127.0.0.1:" ^ suffix)
    else invalid_arg "Bedrock endpoint must be the regional AWS runtime or numeric loopback fixture" in
  let path = "/model/" ^ encode_component model ^ "/converse" in
  { url = base ^ path; host; path; query = [] }
let converse_stream_endpoint ?base_url ~region ~model () =
  let target = endpoint ?base_url ~region ~model () in
  let suffix = "/converse" in
  let prefix_length = String.length target.path - String.length suffix in
  let path = String.sub target.path 0 prefix_length ^ "/converse-stream" in
  let url_prefix_length = String.length target.url - String.length suffix in
  { target with url = String.sub target.url 0 url_prefix_length ^ "/converse-stream"; path }


let text text = `Assoc ["text", `String text]
let wire_message role blocks = `Assoc ["role", `String role; "content", `List blocks]
let image mime_type data =
  let format = match mime_type with
    | "image/jpeg" -> "jpeg"
    | "image/png" -> "png"
    | "image/gif" -> "gif"
    | "image/webp" -> "webp"
    | _ -> invalid ("unsupported tool result image MIME type " ^ mime_type) in
  `Assoc ["image", `Assoc ["format", `String format;
    "source", `Assoc ["bytes", `String data]]]

let tool_result_content (message : message) =
  match message.tool_result_content with
  | None ->
      (match message.content with
       | Some content -> [text content]
       | None -> invalid "malformed tool result")
  | Some _ ->
      let blocks = content_blocks_of_tool_result message in
      if blocks = [] then invalid "malformed tool result";
      List.map (function
        | Text content -> text content
        | Image { mime_type; data } -> image mime_type data) blocks
let tool_use (call : tool_call) =
  if call.id = "" || call.name = "" then invalid "empty tool use ID or name";
  (match call.arguments with `Assoc _ -> () | _ -> invalid "tool input must be an object");
  `Assoc ["toolUse", `Assoc ["toolUseId", `String call.id;
    "name", `String call.name; "input", call.arguments]]

let tool_spec json =
  let fn = member "function" json in
  match member "type" json, fn with
  | `String "function", `Assoc _ ->
      let name = required_string "name" fn in
      let schema = member "parameters" fn in
      if name = "" || member "type" schema <> `String "object" then
        invalid "tool definition requires a name and object schema";
      let fields = ["name", `String name; "inputSchema", `Assoc ["json", schema]] in
      let fields = match member "description" fn with
        | `Null -> fields
        | `String desc -> fields @ ["description", `String desc]
        | _ -> invalid "invalid tool description" in
      `Assoc ["toolSpec", `Assoc fields]
  | _ -> invalid "invalid tool definition"

let request messages tools : Yojson.Basic.t =
  let systems = ref [] and turns = ref [] and pending = ref [] and used_tools = ref [] in
  let append message = turns := message :: !turns in
  let rec replay = function
    | [] -> if !pending <> [] then invalid "missing tool results"
    | (message : message) :: rest ->
        (match message.role with
        | "system" ->
            if !pending <> [] || message.tool_calls <> [] || message.tool_call_id <> None then
              invalid "system message during tool results";
            (match message.content with
             | Some content -> systems := text content :: !systems
             | None -> invalid "system message without content");
            replay rest
        | "user" ->
            if !pending <> [] || message.tool_calls <> [] || message.tool_call_id <> None then
              invalid "user message during tool results";
            let blocks =
              (match message.content with
              | Some content -> [text content]
              | None -> []) @ List.map (fun (attachment : attachment) ->
                if not (List.mem attachment.mime_type
                  ["image/png"; "image/jpeg"; "image/webp"]) then
                  invalid ("unsupported user image MIME type " ^ attachment.mime_type);
                let format = match attachment.mime_type with
                  | "image/jpeg" -> "jpeg"
                  | "image/png" -> "png"
                  | "image/webp" -> "webp"
                  | _ -> assert false in
                `Assoc ["image", `Assoc ["format", `String format;
                  "source", `Assoc ["bytes", `String attachment.data]]])
                message.attachments in
            if blocks = [] then invalid "user message without content";
            append (wire_message "user" blocks);
            replay rest
        | "assistant" ->
            if !pending <> [] || message.tool_call_id <> None then
              invalid "assistant message during tool results";
            let ids = List.map (fun (call : tool_call) -> call.id) message.tool_calls in
            if List.length ids <> List.length (List.sort_uniq String.compare ids) then
              invalid "duplicate tool use ID";
            let blocks = (match message.content with
              | Some content when content <> "" -> [text content]
              | _ -> []) @ List.map tool_use message.tool_calls in
            if blocks = [] then invalid "empty assistant message";
            used_tools := List.rev_append (List.map (fun (call : tool_call) -> call.name) message.tool_calls) !used_tools;
            append (wire_message "assistant" blocks);
            pending := ids;
            replay rest
        | "tool" ->
            let rec gather acc = function
              | ({ role = "tool"; tool_call_id = Some id; tool_calls = []; _ } as result) :: tail ->
                  if not (List.mem id !pending) then invalid "unexpected or duplicate tool result";
                  pending := List.filter ((<>) id) !pending;
                  let content = tool_result_content result in
                  gather (`Assoc ["toolResult", `Assoc ["toolUseId", `String id;
                    "content", `List content]] :: acc) tail
              | ({ role = "tool"; _ } : message) :: _ -> invalid "malformed tool result"
              | tail ->
                  if !pending <> [] then invalid "missing tool results";
                  append (wire_message "user" (List.rev acc));
                  replay tail
            in gather [] (message :: rest)
        | _ -> invalid "unsupported transcript role") in
  replay messages;
  let fields = ["messages", `List (List.rev !turns)] in
  let fields = if !systems = [] then fields else
    fields @ ["system", `List (List.rev !systems)] in
  let definitions = List.map tool_spec tools in
  if List.exists (fun name -> not (List.exists (fun definition ->
      member "name" (member "toolSpec" definition) = `String name) definitions)) !used_tools then
    invalid "tool history requires the actual tool definitions";
  let fields = if definitions = [] then fields else
    fields @ ["toolConfig", `Assoc ["tools", `List definitions]] in
  `Assoc fields

let parse_response (json : Yojson.Basic.t) =
  let output = member "output" json in
  let message = member "message" output in
  if member "role" message <> `String "assistant" then invalid "missing assistant role";
  let blocks = match member "content" message with
    | `List (blocks : Yojson.Basic.t list) -> blocks
    | _ -> invalid "missing content blocks" in
  let texts = ref [] and calls = ref [] in
  List.iter (fun (block : Yojson.Basic.t) -> match block with
    | `Assoc ["text", `String value] -> texts := value :: !texts
    | `Assoc ["toolUse", tool] ->
        let id = required_string "toolUseId" tool in
        let name = required_string "name" tool in
        let arguments = member "input" tool in
        if id = "" || name = "" then invalid "empty tool use ID or name";
        (match arguments with `Assoc _ -> () | _ -> invalid "tool input must be an object");
        calls := { id; name; arguments } :: !calls
    | `Assoc _ as other ->
        (* reasoningContent cannot round-trip through this transport: its
           signature is required for replay but the wire drops it, so a signed
           block surviving only as text would 400 the next request. *)
        if member "reasoningContent" other <> `Null then
          invalid "reasoning content cannot be preserved across Converse turns"
        else if member "guardrailContent" other = `Null &&
           member "image" other = `Null && member "document" other = `Null &&
           member "video" other = `Null &&
           member "citationsContent" other = `Null &&
           member "searchResultContent" other = `Null then
          invalid "unsupported content block"
    | _ -> invalid "malformed content block") blocks;
  let calls = List.rev !calls in
  let ids = List.map (fun (call : tool_call) -> call.id) calls in
  if List.length ids <> List.length (List.sort_uniq String.compare ids) then
    invalid "duplicate tool use ID";
  (match member "stopReason" json with
   | `String ("end_turn" | "stop_sequence") when calls = [] -> ()
   | `String "tool_use" when calls <> [] -> ()
   | `String ("max_tokens" | "model_context_window_exceeded" as reason) ->
       truncated ("stop reason " ^ reason)
   | `String reason -> invalid ("provider stop reason: " ^ reason)
   | _ -> invalid "missing stop reason");
  let content = match List.rev !texts with
    | [] -> None
    | chunks -> Some (String.concat "" chunks) in
  { role = "assistant"; content; tool_calls = calls;
    tool_call_id = None; tool_result_content = None; provider_state = None;
    attachments = [] }

let usage json =
  let reported = member "usage" json in
  match reported with
  | `Assoc _ ->
      let count key = match member key reported with
        | `Int value when value >= 0 -> Some value
        | _ -> None in
      let input_tokens = Option.value ~default:0 (count "inputTokens") in
      let output_tokens = Option.value ~default:0 (count "outputTokens") in
      Some { input_tokens; output_tokens;
        cached_input_tokens = count "cacheReadInputTokens";
        cache_creation_input_tokens = count "cacheWriteInputTokens";
        reasoning_output_tokens = None;
        input_modality_tokens = None; cached_input_modality_tokens = None;
        output_modality_tokens = None }
  | _ -> None

type converse_stream_event =
  | Text_delta of string
  | Tool_call of tool_call
  | Message_stop of string
  | Usage of usage

type stream_block =
  | Text_block of int
  | Tool_block of int * string * string * Buffer.t
  (* Members we don't consume (reasoningContent, images, toolResult echoes)
     still occupy an index; stop events must match without misrouting. *)
  | Ignored_block of int

type converse_stream = {
  frames : Aws_event_stream.decoder;
  on_tool_arguments : (Protocol.tool_argument_delta -> unit) option;
  mutable message_started : bool;
  mutable message_stopped : string option;
  mutable metadata_seen : bool;
  mutable active_block : stream_block option;
  mutable block_indices : int list;
  mutable tool_ids : string list;
}

let create_converse_stream ?on_tool_arguments () = {
  on_tool_arguments;
  frames = Aws_event_stream.create ();
  message_started = false;
  message_stopped = None;
  metadata_seen = false;
  active_block = None;
  block_indices = [];
  tool_ids = [];
}

let json_event payload =
  try Yojson.Basic.from_string payload with
  | Yojson.Json_error _ -> invalid "invalid ConverseStream event JSON"
  | Stack_overflow -> invalid "ConverseStream event JSON is too deeply nested"

let validate_json_shape json =
  let nodes = ref 0 in
  let rec visit depth value =
    incr nodes;
    if depth > 64 || !nodes > 100_000 then
      invalid "ConverseStream event JSON exceeds structure limits";
    match value with
    | `Assoc fields -> List.iter (fun (_, child) -> visit (depth + 1) child) fields
    | `List values -> List.iter (visit (depth + 1)) values
    | _ -> () in
  visit 0 json

let stream_int name json =
  match member name json with
  | `Int value when value >= 0 -> value
  | _ -> invalid ("missing or invalid " ^ name)
let event_payload expected json =
  match json with
  | `Assoc [name, value] when name = expected -> value
  | _ -> invalid ("invalid ConverseStream " ^ expected ^ " event")


let unique_block stream index =
  if List.mem index stream.block_indices then invalid "duplicate ConverseStream content block index";
  stream.block_indices <- index :: stream.block_indices

let bounded_error_text value =
  let out = Buffer.create (min 1024 (String.length value)) in
  let index = ref 0 in
  while !index < String.length value && Buffer.length out < 1024 do
    let c = value.[!index] in
    if Char.code c >= 32 && Char.code c <> 127 then Buffer.add_char out c;
    incr index
  done;
  Buffer.contents out

let decode_converse_event stream frame =
  let headers = frame.Aws_event_stream.headers in
  let message_type = Aws_event_stream.string_header headers ":message-type" in
  (match message_type with
   | Some ("exception" | "error") ->
       let error_name = match Aws_event_stream.string_header headers ":exception-type" with
         | Some name -> name
         | None -> Option.value ~default:"provider exception"
             (Aws_event_stream.string_header headers ":error-code") in
       let error_name = bounded_error_text error_name in
       let detail = try
           let json = json_event frame.payload in
           match member "message" json with `String value -> bounded_error_text value
           | _ -> error_name
         with Protocol.Invalid_response _ -> error_name in
       invalid ("Bedrock ConverseStream " ^ error_name ^ ": " ^ detail)
   | Some "event" -> ()
   | _ -> invalid "invalid ConverseStream message type");
  (match Aws_event_stream.string_header headers ":content-type" with
   | Some "application/json" -> ()
   | _ -> invalid "invalid ConverseStream event content type");
  let name = match Aws_event_stream.string_header headers ":event-type" with
    | Some name -> name
    | None -> invalid "missing ConverseStream event type" in
  let json = json_event frame.payload in
  validate_json_shape json;
  let value = event_payload name json in
  if stream.metadata_seen then invalid "ConverseStream event after metadata";
  match name with
  | "messageStart" ->
      if stream.message_started || stream.message_stopped <> None ||
         member "role" value <> `String "assistant" then
        invalid "invalid ConverseStream message start";
      stream.message_started <- true;
      []
  | "contentBlockStart" ->
      if not stream.message_started || stream.message_stopped <> None ||
         stream.active_block <> None then invalid "unexpected ConverseStream content block start";
      let index = stream_int "contentBlockIndex" value in
      unique_block stream index;
      (match member "start" value with
       | `Assoc ["toolUse", tool] ->
           let id = required_string "toolUseId" tool in
           let name = required_string "name" tool in
           if id = "" || name = "" || String.length id > 1024 ||
              String.length name > 256 || List.mem id stream.tool_ids then
             invalid "invalid or duplicate ConverseStream tool use";
           stream.active_block <- Some (Tool_block (index, id, name, Buffer.create 128));
           (match stream.on_tool_arguments with
            | None -> ()
            | Some emit -> emit { Protocol.key = Printf.sprintf "bedrock:%d" index;
                call_id = Some id; name; fragment = "" });
           []
       | `Assoc ["text", _] | `Assoc [] | `Null ->
           stream.active_block <- Some (Text_block index);
           []
       | _ ->
           stream.active_block <- Some (Ignored_block index);
           [])
  | "contentBlockDelta" ->
      if not stream.message_started || stream.message_stopped <> None then
        invalid "unexpected ConverseStream content delta";
      let index = stream_int "contentBlockIndex" value in
      (match member "delta" value with
       | `Assoc ["text", `String text] ->
           (match stream.active_block with
            | None ->
                unique_block stream index;
                stream.active_block <- Some (Text_block index)
            | Some (Text_block active) when active = index -> ()
            | _ -> invalid "mismatched ConverseStream text block");
           [Text_delta text]
       | `Assoc ["toolUse", `Assoc ["input", `String fragment]] ->
           (match stream.active_block with
            | Some (Tool_block (active, id, name, input)) when active = index ->
                if Buffer.length input + String.length fragment > 1_048_576 then
                  invalid "ConverseStream tool input exceeds 1 MiB";
                Buffer.add_string input fragment;
                (match stream.on_tool_arguments with
                 | None -> ()
                 | Some emit -> emit { Protocol.key = Printf.sprintf "bedrock:%d" index;
                     call_id = Some id; name; fragment });
                []
            | _ -> invalid "mismatched ConverseStream tool input block")
       | _ -> []) (* reasoningContent and other delta members we don't consume *)
  | "contentBlockStop" ->
      if not stream.message_started || stream.message_stopped <> None then
        invalid "unexpected ConverseStream content block stop";
      let index = stream_int "contentBlockIndex" value in
      (match stream.active_block with
       | Some (Text_block active) when active = index ->
           stream.active_block <- None;
           []
       | Some (Tool_block (active, id, name, input)) when active = index ->
           stream.active_block <- None;
           let arguments = json_event (Buffer.contents input) in
           validate_json_shape arguments;
           (match arguments with `Assoc _ -> () | _ -> invalid "ConverseStream tool input must be an object");
           if List.mem id stream.tool_ids then invalid "duplicate ConverseStream tool use ID";
           stream.tool_ids <- id :: stream.tool_ids;
           [Tool_call { id; name; arguments }]
       | Some (Ignored_block active) when active = index ->
           stream.active_block <- None;
           []
       | _ -> invalid "mismatched ConverseStream content block stop")
  | "messageStop" ->
      if not stream.message_started || stream.message_stopped <> None ||
         stream.active_block <> None then invalid "unexpected ConverseStream message stop";
      let reason = required_string "stopReason" value in
      if String.length reason > 64 then invalid "invalid ConverseStream stop reason";
      (match reason with
       | "end_turn" | "stop_sequence" | "tool_use" -> ()
       | "max_tokens" | "model_context_window_exceeded" ->
           truncated ("stop reason " ^ reason)
       | _ -> invalid ("provider stop reason: " ^ reason));
      stream.message_stopped <- Some reason;
      [Message_stop reason]
  | "metadata" ->
      if stream.message_stopped = None then invalid "ConverseStream metadata before message stop";
      stream.metadata_seen <- true;
      let report = member "usage" value in
      (match report with
       | `Null -> []
       | _ ->
           (match usage (`Assoc ["usage", report]) with
            | Some value -> [Usage value]
            | None -> invalid "invalid ConverseStream usage"))
  | _ -> [] (* forward compatibility: unknown event types carry no content *)

let feed_converse_stream stream data =
  try
    Aws_event_stream.feed stream.frames data
    |> List.concat_map (decode_converse_event stream)
  with Aws_event_stream.Invalid_message detail -> invalid ("invalid AWS EventStream: " ^ detail)

let finish_converse_stream stream =
  (try Aws_event_stream.finish stream.frames with
   | Aws_event_stream.Invalid_message detail -> invalid ("invalid AWS EventStream: " ^ detail));
  if not stream.message_started || stream.message_stopped = None || stream.active_block <> None then
    invalid "incomplete Bedrock ConverseStream response"

(* Control-plane listings do not establish account/model-access permission to
   invoke any returned foundation model or inference profile. *)
let discovery_endpoint ~region () =
  ignore (Aws_auth.region ~getenv:(fun _ -> Some region) ());
  let host = "bedrock." ^ region ^ ".amazonaws.com" in
  let path = "/foundation-models" in
  { url = "https://" ^ host ^ path; host; path; query = [] }

let inference_profiles_endpoint ~region ?next_token () =
  ignore (Aws_auth.region ~getenv:(fun _ -> Some region) ());
  let host = "bedrock." ^ region ^ ".amazonaws.com" in
  let path = "/inference-profiles" in
  let query = ("maxResults", "100") ::
    Option.fold ~none:[] ~some:(fun value -> ["nextToken", value]) next_token in
  let query_text = Aws_auth.query_string query in
  { url = "https://" ^ host ^ path ^ "?" ^ query_text;
    host; path; query }

let parse_models json =
  let summaries = match member "modelSummaries" json with
    | `List summaries -> summaries
    | _ -> invalid "missing foundation model summaries" in
  List.filter_map (fun summary ->
    match member "modelId" summary, member "outputModalities" summary,
      member "inferenceTypesSupported" summary, member "modelLifecycle" summary with
    | `String id, `List output, `List inference, lifecycle
      when id <> "" && List.mem (`String "TEXT") output &&
        List.mem (`String "ON_DEMAND") inference &&
        member "status" lifecycle = `String "ACTIVE" -> Some id
    | _ -> None) summaries

let valid_listing_text ~max_length value =
  value <> "" && String.length value <= max_length &&
  not (String.exists (fun character ->
    Char.code character < 32 || Char.code character = 127) value)

let parse_inference_profiles json =
  let summaries = match member "inferenceProfileSummaries" json with
    | `List summaries -> summaries
    | _ -> invalid "missing inference profile summaries" in
  let ids = List.filter_map (fun summary ->
    let id = required_string "inferenceProfileId" summary in
    let status = required_string "status" summary in
    let profile_type = required_string "type" summary in
    if not (valid_listing_text ~max_length:2048 id) then
      invalid "invalid inference profile ID";
    if not (valid_listing_text ~max_length:32 status) ||
       not (List.mem profile_type ["SYSTEM_DEFINED"; "APPLICATION"]) then
      invalid "invalid inference profile status or type";
    (match member "models" summary with
     | `List models ->
         List.iter (fun model ->
           let arn = required_string "modelArn" model in
           if not (valid_listing_text ~max_length:4096 arn) then
             invalid "invalid inference profile model ARN") models
     | _ -> invalid "missing inference profile models");
    if status = "ACTIVE" then Some id else None) summaries in
  if List.length ids <> List.length (List.sort_uniq String.compare ids) then
    invalid "duplicate inference profile ID";
  let next_token = match member "nextToken" json with
    | `Null -> None
    | `String token when valid_listing_text ~max_length:4096 token -> Some token
    | _ -> invalid "invalid inference profile pagination token" in
  ids, next_token
