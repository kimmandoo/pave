let chat_url = "https://api.deepseek.com/chat/completions"
let models_url = "https://api.deepseek.com/models"

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "DeepSeek API key requires its pinned Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid DeepSeek API key";
  ["Authorization: Bearer " ^ api_key]

let state_tag model = [
  "provider", `String "deepseek";
  "route", `String "chat";
  "model", `String model;
]

let matching_state model = function
  | Some (`Assoc fields)
    when List.for_all (fun (key, value) -> List.assoc_opt key fields = Some value)
      (state_tag model) -> Some fields
  | _ -> None

let effort = function
  | None -> `Assoc ["type", `String "enabled"], None
  | Some "none" -> `Assoc ["type", `String "disabled"], None
  | Some ("low" | "high" | "max" as value) ->
      `Assoc ["type", `String "enabled"], Some value
  | Some _ -> invalid_arg "DeepSeek thinking level must be none, low, high, or max"

let request ~model ?thinking messages tools =
  let serialize (message : Protocol.message) =
    match Protocol.message_to_json message with
    | `Assoc fields when message.role = "assistant" ->
        let fields = if message.content = None &&
            not (List.mem_assoc "content" fields) then
          fields @ ["content", `String ""] else fields in
        let fields = match matching_state model message.provider_state with
          | None -> fields
          | Some state ->
              (match List.assoc_opt "reasoning_content" state with
               | Some value -> fields @ ["reasoning_content", value]
               | None -> fields) in
        `Assoc fields
    | json -> json in
  let thinking, reasoning_effort = effort thinking in
  let fields = ["model", `String model;
    "messages", Protocol.chat_messages_to_json ~serialize messages;
    "stream", `Bool false;
    "thinking", thinking] in
  let fields = match reasoning_effort with
    | None -> fields
    | Some value -> fields @ ["reasoning_effort", `String value] in
  `Assoc (if tools = [] then fields else fields @ ["tools", `List tools])

let parse_completion json =
  let message = match Protocol.member "choices" json with
    | `List [choice] -> Protocol.member "message" choice
    | _ -> raise (Protocol.Invalid_response "missing DeepSeek Chat message") in
  let fields = match Protocol.member "reasoning_content" message with
    | `Null -> []
    | (`String _ as value) -> ["reasoning_content", value]
    | _ -> raise (Protocol.Invalid_response "invalid DeepSeek reasoning_content") in
  let reply = Protocol.parse_completion json in
  if fields = [] then reply
  else { reply with provider_state = Some (`Assoc (state_tag
    (match Protocol.member "model" json with
     | `String model when model <> "" -> model
     | _ -> raise (Protocol.Invalid_response "missing DeepSeek response model")) @ fields)) }
