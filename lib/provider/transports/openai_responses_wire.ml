open Protocol

let invalid detail = raise (Invalid_response ("invalid Responses API response: " ^ detail))

let required_string name json = match member name json with
  | `String value when value <> "" -> value
  | _ -> invalid ("missing or invalid " ^ name)
let usage json =
  let reported = member "usage" json in
  match member "input_tokens" reported, member "output_tokens" reported with
  | `Int input_tokens, `Int output_tokens
    when input_tokens >= 0 && output_tokens >= 0 ->
      Some { input_tokens; output_tokens }
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



let request ?(stream = false) ~model messages tools =
  if model = "" then invalid_arg "empty Responses model";
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
        (match msg.content with
         | Some text -> emit (`Assoc [ "role", `String "user";
             "content", `List [ `Assoc [ "type", `String "input_text";
               "text", `String text ] ] ])
         | None -> invalid "user message without content")
    | "assistant" ->
        if !pending <> [] || msg.tool_call_id <> None then
          invalid "assistant message during tool results";
        (match msg.content with
         | Some text -> emit (`Assoc [ "role", `String "assistant";
             "content", `String text ])
         | None when msg.tool_calls = [] -> invalid "empty assistant message"
         | None -> ());
        List.iter (fun (call : tool_call) ->
          if call.id = "" || call.name = "" then invalid "empty tool call id or name";
          if List.mem call.id !pending then invalid "duplicate tool call id";
          (match call.arguments with `Assoc _ -> () | _ -> invalid "tool arguments must be an object");
          pending := call.id :: !pending;
          emit (`Assoc [ "type", `String "function_call";
            "call_id", `String call.id; "name", `String call.name;
            "arguments", `String (Yojson.Basic.to_string call.arguments) ])) msg.tool_calls
    | "tool" ->
        (match msg.content, msg.tool_call_id, msg.tool_calls with
         | Some _, Some id, [] when List.mem id !pending ->
             pending := List.filter (( <> ) id) !pending;
             emit (`Assoc [ "type", `String "function_call_output";
               "call_id", `String id; "output", tool_result_output msg ])
         | _ -> invalid "unexpected or malformed tool result")
    | _ -> invalid "unsupported transcript role") messages;
  if !pending <> [] then invalid "missing tool results";
  let fields = [ "model", `String model; "input", `List (List.rev !input) ] in
  let fields = match List.rev !instructions with
    | [] -> fields
    | texts -> fields @ [ "instructions", `String (String.concat "\n\n" texts) ] in
  let fields = if tools = [] then fields else
    fields @ [ "tools", `List (List.map tool_schema tools) ] in
  `Assoc (if stream then fields @ [ "stream", `Bool true ] else fields)

let parse_completion json =
  (match member "status" json with
   | `String "completed" -> ()
   | `String "incomplete" -> invalid "incomplete response"
   | `String "failed" -> invalid "failed response"
   | _ -> invalid "response not completed");
  (match member "error" json with `Null -> () | _ -> invalid "response error");
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
          | _ -> invalid "unsupported message content") content
    | `String "function_call" ->
        let id = required_string "call_id" item in
        let name = required_string "name" item in
        if Hashtbl.mem ids id then invalid "duplicate tool call id";
        Hashtbl.add ids id ();
        let args = required_string "arguments" item in
        let arguments = try Yojson.Basic.from_string args
          with Yojson.Json_error _ -> invalid "invalid function arguments JSON" in
        (match arguments with `Assoc _ -> () | _ -> invalid "tool arguments must be an object");
        calls := { id; name; arguments } :: !calls
    | `String "reasoning" -> ()
    | _ -> invalid "unsupported output item") outputs;
  { role = "assistant"; content = (match List.rev !texts with
      | [] -> None | texts -> Some (String.concat "" texts));
    tool_calls = List.rev !calls; tool_call_id = None; tool_result_content = None; provider_state = None }
