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

type target = { url : string; host : string; path : string }

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
  { url = base ^ path; host; path }

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

let request messages tools =
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

let parse_response json =
  let output = member "output" json in
  let message = member "message" output in
  if member "role" message <> `String "assistant" then invalid "missing assistant role";
  let blocks = match member "content" message with
    | `List blocks -> blocks
    | _ -> invalid "missing content blocks" in
  let texts = ref [] and calls = ref [] in
  List.iter (fun block -> match block with
    | `Assoc ["text", `String value] -> texts := value :: !texts
    | `Assoc ["toolUse", tool] ->
        let id = required_string "toolUseId" tool in
        let name = required_string "name" tool in
        let arguments = member "input" tool in
        if id = "" || name = "" then invalid "empty tool use ID or name";
        (match arguments with `Assoc _ -> () | _ -> invalid "tool input must be an object");
        calls := { id; name; arguments } :: !calls
    | `Assoc ["reasoningContent", _] ->
        invalid "reasoning content cannot be preserved across Converse turns"
    | _ -> invalid "unsupported content block") blocks;
  let calls = List.rev !calls in
  let ids = List.map (fun (call : tool_call) -> call.id) calls in
  if List.length ids <> List.length (List.sort_uniq String.compare ids) then
    invalid "duplicate tool use ID";
  (match member "stopReason" json with
   | `String ("end_turn" | "stop_sequence") when calls = [] -> ()
   | `String "tool_use" when calls <> [] -> ()
   | `String reason -> invalid ("unsupported stop reason: " ^ reason)
   | _ -> invalid "missing stop reason");
  let content = match List.rev !texts with
    | [] -> None
    | chunks -> Some (String.concat "" chunks) in
  { role = "assistant"; content; tool_calls = calls;
    tool_call_id = None; tool_result_content = None; provider_state = None;
    attachments = [] }

let usage json =
  let reported = member "usage" json in
  match member "inputTokens" reported, member "outputTokens" reported with
  | `Int input_tokens, `Int output_tokens when input_tokens >= 0 && output_tokens >= 0 ->
      Some { input_tokens; output_tokens }
  | _ -> None

(* Control-plane discovery only: this list excludes inference profiles and does
   not establish account/model-access permission to invoke any returned ID. *)
let discovery_endpoint ~region () =
  ignore (Aws_auth.region ~getenv:(fun _ -> Some region) ());
  let host = "bedrock." ^ region ^ ".amazonaws.com" in
  let path = "/foundation-models" in
  { url = "https://" ^ host ^ path; host; path }

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
