open Protocol

let invalid detail = raise (Invalid_response ("invalid Ollama response: " ^ detail))
let field = member

let required_string name json = match field name json with
  | `String value when value <> "" -> value
  | _ -> invalid ("missing or invalid " ^ name)

let object_arguments = function
  | `Assoc _ as arguments -> arguments
  | `String text ->
      (match (try Yojson.Basic.from_string text with Yojson.Json_error _ -> invalid "invalid tool arguments JSON") with
       | `Assoc _ as arguments -> arguments
       | _ -> invalid "tool arguments must be an object")
  | _ -> invalid "tool arguments must be an object"

let tool_schema json =
  let fn = field "function" json in
  match field "type" json, fn with
  | `String "function", `Assoc _ ->
      let name = required_string "name" fn in
      let parameters = field "parameters" fn in
      if field "type" parameters <> `String "object" then
        invalid "tool definition must have object parameters";
      let description = match field "description" fn with
        | `Null -> []
        | `String text -> [ "description", `String text ]
        | _ -> invalid "invalid tool description" in
      `Assoc [ "type", `String "function";
        "function", `Assoc ([ "name", `String name ] @ description @
          [ "parameters", parameters ]) ]
  | _ -> invalid "invalid tool definition"

let request ~model messages tools =
  if model = "" then invalid_arg "empty Ollama model";
  let pending = ref [] in
  let convert (msg : message) =
    match msg.role with
    | "system" | "user" ->
        if !pending <> [] || msg.tool_calls <> [] || msg.tool_call_id <> None then
          invalid "message before tool results";
        let content = match msg.content with Some text -> text | None -> invalid "missing message content" in
        `Assoc [ "role", `String msg.role; "content", `String content ]
    | "assistant" ->
        if !pending <> [] || msg.tool_call_id <> None then invalid "assistant before tool results";
        let ids = Hashtbl.create (List.length msg.tool_calls) in
        let calls = List.map (fun (call : tool_call) ->
          if call.id = "" || call.name = "" || Hashtbl.mem ids call.id then
            invalid "empty or duplicate tool call id or name";
          Hashtbl.add ids call.id ();
          (match call.arguments with `Assoc _ -> () | _ -> invalid "tool arguments must be an object");
          pending := (call.id, call.name) :: !pending;
          `Assoc [ "type", `String "function";
            "function", `Assoc [ "name", `String call.name; "arguments", call.arguments ] ])
          msg.tool_calls in
        if calls = [] && (msg.content = None || msg.content = Some "") then
          invalid "empty assistant message";
        let content = match msg.content with Some text -> text | None -> "" in
        `Assoc ([ "role", `String "assistant"; "content", `String content ] @
          if calls = [] then [] else [ "tool_calls", `List calls ])
    | "tool" ->
        (match msg.tool_call_id, msg.content, msg.tool_calls with
         | Some id, Some content, [] ->
             let name = match List.assoc_opt id !pending with
               | Some name -> name | None -> invalid "unexpected or duplicate tool result" in
             pending := List.remove_assoc id !pending;
             `Assoc [ "role", `String "tool"; "content", `String content;
               "tool_name", `String name ]
         | _ -> invalid "malformed tool result")
    | _ -> invalid "unsupported transcript role" in
  let converted = List.map convert messages in
  if !pending <> [] then invalid "missing tool results";
  `Assoc ([ "model", `String model; "messages", `List converted; "stream", `Bool false ] @
    if tools = [] then [] else [ "tools", `List (List.map tool_schema tools) ])

let parse_call index json =
  (match field "type" json with `Null | `String "function" -> ()
   | _ -> invalid "unsupported tool call type");
  let fn = match field "function" json with `Assoc _ as fn -> fn
    | _ -> invalid "missing tool function" in
  let name = required_string "name" fn in
  let arguments = object_arguments (field "arguments" fn) in
  { id = Printf.sprintf "ollama:%d:%s" index name; name; arguments }

let parse_calls = function
  | `Null -> []
  | `List calls ->
      if List.length calls > 128 then invalid "too many tool calls";
      List.mapi parse_call calls
  | _ -> invalid "invalid tool_calls"

let parse_message json =
  (match field "role" json with
   | `String "assistant" -> ()
   | _ -> invalid "missing assistant role");
  let content = match field "content" json with
    | `Null -> None
    | `String text -> Some text
    | _ -> invalid "invalid assistant content" in
  let tool_calls = parse_calls (field "tool_calls" json) in
  if tool_calls = [] && (content = None || content = Some "") then
    invalid "empty assistant response";
  { role = "assistant"; content; tool_calls; tool_call_id = None;
    provider_state = None }

let check_done_reason json has_calls =
  match field "done_reason" json with
  | `Null | `String "stop" -> ()
  | `String "tool_calls" when has_calls -> ()
  | `String "length" -> invalid "length (truncated response)"
  | `String "load" -> invalid "load (no response generated)"
  | `String reason -> invalid ("unexpected done_reason: " ^ reason)
  | _ -> invalid "invalid done_reason"

let check_error json = match field "error" json with
  | `Null -> ()
  | `String text -> invalid ("model error: " ^ text)
  | _ -> invalid "invalid model error"

let usage json =
  match field "prompt_eval_count" json, field "eval_count" json with
  | `Int input_tokens, `Int output_tokens
    when input_tokens >= 0 && output_tokens >= 0 ->
      Some { Protocol.input_tokens; output_tokens }
  | _ -> None

let parse_completion json =
  check_error json;
  if field "done" json <> `Bool true then invalid "missing done marker";
  let message = match field "message" json with
    | `Assoc _ as message -> parse_message message
    | _ -> invalid "missing assistant message" in
  check_done_reason json (message.tool_calls <> []);
  message
