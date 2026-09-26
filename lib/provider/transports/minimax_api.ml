let chat_url = "https://api.minimax.io/v1/chat/completions"
let models_url = "https://api.minimax.io/v1/models"

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "MiniMax API key requires its pinned international Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid MiniMax API key";
  ["Authorization: Bearer " ^ api_key]

let state_tag model = [
  "provider", `String "minimax";
  "route", `String "chat";
  "model", `String model;
]

let matching_state model = function
  | Some (`Assoc fields)
    when List.for_all (fun (key, value) -> List.assoc_opt key fields = Some value)
      (state_tag model) -> Some fields
  | _ -> None

let request ~model messages tools =
  let serialize (message : Protocol.message) =
    match Protocol.message_to_json message with
    | `Assoc fields when message.role = "assistant" ->
        let fields = if message.content = None &&
            not (List.mem_assoc "content" fields) then
          fields @ ["content", `String ""] else fields in
        let fields = match matching_state model message.provider_state with
          | None -> fields
          | Some state ->
              List.fold_left (fun fields name ->
                match List.assoc_opt name state with
                | Some value -> fields @ [name, value]
                | None -> fields) fields ["reasoning_details"; "reasoning_content"] in
        `Assoc fields
    | json -> json in
  let fields = ["model", `String model;
    "messages", Protocol.chat_messages_to_json ~serialize messages;
    "stream", `Bool false] in
  let fields = if model = "MiniMax-M3" then
      fields @ ["reasoning_split", `Bool true]
    else fields in
  `Assoc (if tools = [] then fields else fields @ ["tools", `List tools])

let parse_completion json =
  let message = match Protocol.member "choices" json with
    | `List [choice] -> Protocol.member "message" choice
    | _ -> raise (Protocol.Invalid_response "missing MiniMax Chat message") in
  let fields = List.filter_map (fun name ->
    match Protocol.member name message with
    | `Null -> None
    | (`List _ as value) when name = "reasoning_details" -> Some (name, value)
    | (`String _ as value) when name = "reasoning_content" -> Some (name, value)
    | _ -> raise (Protocol.Invalid_response ("invalid MiniMax " ^ name)))
    ["reasoning_details"; "reasoning_content"] in
  let reply = Protocol.parse_completion json in
  if fields = [] then reply
  else { reply with provider_state = Some (`Assoc (state_tag
    (match Protocol.member "model" json with
     | `String model when model <> "" -> model
     | _ -> raise (Protocol.Invalid_response "missing MiniMax response model")) @ fields)) }
