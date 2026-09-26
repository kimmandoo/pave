let chat_url = "https://api.fireworks.ai/inference/v1/chat/completions"

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Fireworks API key requires its pinned Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Fireworks API key";
  ["Authorization: Bearer " ^ api_key]

let state_tag model = [
  "provider", `String "fireworks";
  "route", `String "chat";
  "model", `String model;
]

let matching_state model = function
  | Some (`Assoc fields)
    when List.for_all (fun (key, value) -> List.assoc_opt key fields = Some value)
      (state_tag model) -> Some fields
  | _ -> None

let reasoning_effort = function
  | None -> None
  | Some "minimal" -> Some "none"
  | Some ("none" | "low" | "medium" | "high" | "xhigh" | "max" as value) ->
      Some value
  | Some _ -> invalid_arg
      "Fireworks thinking level must be none, minimal, low, medium, high, xhigh, or max"

let request ~model ?thinking messages tools =
  let serialize (message : Protocol.message) =
    match Protocol.message_to_json message with
    | `Assoc fields when message.role = "assistant" ->
        let fields = match matching_state model message.provider_state with
          | None -> fields
          | Some state ->
              (match List.assoc_opt "reasoning_content" state with
               | Some (`String _ as value) -> fields @ ["reasoning_content", value]
               | _ -> fields) in
        `Assoc fields
    | json -> json in
  let fields = ["model", `String model;
    "messages", Protocol.chat_messages_to_json ~serialize messages;
    "stream", `Bool false] in
  let fields = match reasoning_effort thinking with
    | None -> fields
    | Some value -> fields @ ["reasoning_effort", `String value] in
  `Assoc (if tools = [] then fields else fields @ ["tools", `List tools])

let parse_completion ~model json =
  let message = match Protocol.member "choices" json with
    | `List [choice] -> Protocol.member "message" choice
    | _ -> raise (Protocol.Invalid_response "missing Fireworks Chat message") in
  let reasoning = match Protocol.member "reasoning_content" message with
    | `Null -> None
    | `String value -> Some (`String value)
    | _ -> raise (Protocol.Invalid_response "invalid Fireworks reasoning_content") in
  let reply = Protocol.parse_completion json in
  match reasoning with
  | None -> reply
  | Some value -> { reply with provider_state = Some (`Assoc
      (state_tag model @ ["reasoning_content", value])) }
