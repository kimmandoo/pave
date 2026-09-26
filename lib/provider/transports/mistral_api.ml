let chat_url = "https://api.mistral.ai/v1/chat/completions"
let models_url = "https://api.mistral.ai/v1/models"

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Mistral API key requires its pinned Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Mistral API key";
  ["Authorization: Bearer " ^ api_key]

let valid_effort = function
  | "none" | "minimal" | "low" | "medium" | "high" | "xhigh" -> true
  | _ -> false

let state_tag model = [
  "provider", `String "mistral";
  "route", `String "chat";
  "model", `String model;
]

let matching_state model = function
  | Some (`Assoc fields)
    when List.for_all (fun (key, value) -> List.assoc_opt key fields = Some value)
      (state_tag model) -> Some fields
  | _ -> None

let replace_field name value fields =
  let found = ref false in
  let fields = List.map (fun (key, old) ->
    if key = name then (found := true; key, value) else key, old) fields in
  if !found then fields else fields @ [name, value]

let request ~model ?thinking messages tools =
  let serialize (message : Protocol.message) =
    match Protocol.message_to_json message with
    | `Assoc fields when message.role = "assistant" ->
        (match matching_state model message.provider_state with
         | Some state ->
             (match List.assoc_opt "content" state with
              | Some content -> `Assoc (replace_field "content" content fields)
              | None -> `Assoc fields)
         | None -> `Assoc fields)
    | json -> json in
  let fields = ["model", `String model;
    "messages", Protocol.chat_messages_to_json ~serialize messages;
    "stream", `Bool false] in
  let fields = match thinking with
    | None -> fields
    | Some level when valid_effort level ->
        fields @ ["reasoning_effort", `String level]
    | Some _ -> invalid_arg
        "Mistral thinking level must be none, minimal, low, medium, high, or xhigh" in
  `Assoc (if tools = [] then fields else fields @ ["tools", `List tools])

let unique fields =
  let names = List.map fst fields in
  List.length names = List.length (List.sort_uniq String.compare names)

let text_field field json = match Protocol.member field json with
  | `String value -> value
  | _ -> raise (Protocol.Invalid_response ("invalid Mistral " ^ field))

let visible_text = function
  | `String text -> text, None
  | `Null -> "", None
  | `List chunks ->
      let text = Buffer.create 128 in
      List.iter (fun chunk ->
        match chunk with
        | `Assoc fields when unique fields ->
            (match Protocol.member "type" chunk with
             | `String "text" -> Buffer.add_string text (text_field "text" chunk)
             | `String "thinking" ->
                 (match Protocol.member "thinking" chunk with
                  | `List parts -> List.iter (fun part ->
                      match part with
                      | `Assoc fields when unique fields &&
                          Protocol.member "type" part = `String "text" ->
                          ignore (text_field "text" part)
                      | _ -> raise (Protocol.Invalid_response
                          "invalid Mistral thinking chunk")) parts
                  | _ -> raise (Protocol.Invalid_response
                      "invalid Mistral thinking content"))
             | _ -> raise (Protocol.Invalid_response "unknown Mistral content chunk"))
        | _ -> raise (Protocol.Invalid_response "invalid Mistral content chunk")) chunks;
      Buffer.contents text, Some (`List chunks)
  | _ -> raise (Protocol.Invalid_response "invalid Mistral assistant content")

let parse_completion json =
  let choices = Protocol.member "choices" json in
  let message = match choices with
    | `List [choice] -> Protocol.member "message" choice
    | _ -> raise (Protocol.Invalid_response "missing Mistral Chat message") in
  let content = Protocol.member "content" message in
  let text, native_content = visible_text content in
  let normalized = match json, choices with
    | `Assoc top, `List [(`Assoc choice_fields as choice)] ->
        let message_fields = match Protocol.member "message" choice with
          | `Assoc fields when unique fields ->
              replace_field "content"
                (if text = "" then `Null else `String text) fields
          | _ -> raise (Protocol.Invalid_response "invalid Mistral assistant message") in
        let normalized_choice = replace_field "message" (`Assoc message_fields)
          choice_fields in
        `Assoc (replace_field "choices" (`List [`Assoc normalized_choice]) top)
    | _ -> raise (Protocol.Invalid_response "invalid Mistral Chat response") in
  let reply = Protocol.parse_completion normalized in
  if reply.tool_calls = [] && (reply.content = None || reply.content = Some "") then
    raise (Protocol.Invalid_response "missing Mistral final text");
  match native_content with
  | None -> reply
  | Some content ->
      let model = match Protocol.member "model" json with
        | `String model when model <> "" -> model
        | _ -> raise (Protocol.Invalid_response "missing Mistral response model") in
      { reply with provider_state = Some (`Assoc (state_tag model @ ["content", content])) }
