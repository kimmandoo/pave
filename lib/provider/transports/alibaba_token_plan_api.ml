let chat_url =
  "https://token-plan.cn-beijing.maas.aliyuncs.com/compatible-mode/v1/chat/completions"

let valid_key key = String.starts_with ~prefix:"sk-sp-" key &&
  String.length key > String.length "sk-sp-" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Alibaba Token Plan key requires the documented Beijing Chat endpoint";
  if not (valid_key api_key) then
    invalid_arg "invalid Alibaba Token Plan subscription key";
  ["Authorization: Bearer " ^ api_key]

let state_tag model = [
  "provider", `String "alibaba-token-plan";
  "route", `String "chat";
  "model", `String model;
]

let matching_state model = function
  | Some (`Assoc fields)
    when List.for_all (fun (key, value) -> List.assoc_opt key fields = Some value)
      (state_tag model) -> Some fields
  | _ -> None

let thinking_enabled = function
  | None -> None
  | Some "none" -> Some false
  | Some ("minimal" | "low" | "medium" | "high" | "xhigh" | "max") ->
      Some true
  | Some _ -> invalid_arg
      "Alibaba Token Plan thinking level must be none, minimal, low, medium, high, xhigh, or max"

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
               | Some (`String _ as reasoning) ->
                   fields @ ["reasoning_content", reasoning]
               | _ -> fields) in
        `Assoc fields
    | json -> json in
  let fields = ["model", `String model;
    "messages", Protocol.chat_messages_to_json ~serialize messages;
    "stream", `Bool false] in
  let fields = match thinking_enabled thinking with
    | None -> fields
    | Some enabled -> fields @ ["enable_thinking", `Bool enabled] in
  `Assoc (if tools = [] then fields else fields @ ["tools", `List tools])

let parse_completion ~model json =
  let message = match Protocol.member "choices" json with
    | `List [choice] -> Protocol.member "message" choice
    | _ -> raise (Protocol.Invalid_response
        "missing Alibaba Token Plan Chat message") in
  let reasoning = match Protocol.member "reasoning_content" message with
    | `Null -> None
    | `String value -> Some (`String value)
    | _ -> raise (Protocol.Invalid_response
        "invalid Alibaba Token Plan reasoning_content") in
  let reply = Protocol.parse_completion json in
  match reasoning with
  | None -> reply
  | Some value -> { reply with provider_state = Some (`Assoc
      (state_tag model @ ["reasoning_content", value])) }
