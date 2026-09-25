(* MiniMax Token Plan subscription keys (not pay-as-you-go API keys).
   https://platform.minimax.io/docs/token-plan/other-tools
   https://platform.minimaxi.com/docs/token-plan/other-tools
   https://platform.minimax.io/docs/api-reference/text-chat-openai
   https://platform.minimaxi.com/docs/api-reference/text-chat-openai
   https://platform.minimax.io/docs/guides/text-m3-function-call
   https://platform.minimax.io/docs/api-reference/models/openai/list-models
   https://platform.minimaxi.com/docs/api-reference/models/openai/list-models
   The documented GET /v1/models authenticates an ordinary account-management
   API key, not a Token Plan subscription key. No plan-key listing is assumed. *)
let intl_chat_url = "https://api.minimax.io/v1/chat/completions"
let china_chat_url = "https://api.minimax.cn/v1/chat/completions"

let valid_key key = String.starts_with ~prefix:"sk-cp-" key &&
  String.length key > String.length "sk-cp-" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let key_from_env name = match Sys.getenv_opt name with
  | Some key when valid_key key -> Some key
  | _ -> None

let env_intl_api_key () = key_from_env "MINIMAX_CODE_API_KEY"
let env_china_api_key () = key_from_env "MINIMAX_CODE_CN_API_KEY"

let chat_headers ~endpoint ~api_key =
  if endpoint <> intl_chat_url && endpoint <> china_chat_url then
    invalid_arg "MiniMax subscription key requires a pinned regional Token Plan Chat endpoint";
  if not (valid_key api_key) then
    invalid_arg "invalid MiniMax Token Plan subscription key";
  ["Authorization: Bearer " ^ api_key]

(* Replay the complete assistant turn before the tool result, preserving
   native <think> content or optional separate reasoning when returned.
   Do not select a thinking mode from the caller's model ID. *)
let request ~model messages tools =
  let message (msg : Protocol.message) =
    match Protocol.message_to_json msg with
    | `Assoc fields when msg.role = "assistant" ->
        let fields = if msg.content = None then
          fields @ ["content", `String ""] else fields in
        let fields = match msg.provider_state with
          | Some (`Assoc state) ->
              List.fold_left (fun fields name ->
                match List.assoc_opt name state with
                | Some value -> fields @ [name, value]
                | None -> fields) fields ["reasoning_details"; "reasoning_content"]
          | _ -> fields in
        `Assoc fields
    | json -> json in
  let fields = ["model", `String model;
    "messages", Protocol.chat_messages_to_json ~serialize:message messages;
    "stream", `Bool false] in
  `Assoc (if tools = [] then fields else fields @ ["tools", `List tools])

let parse_completion json =
  let choice = match Protocol.member "choices" json with
    | `List (choice :: _) -> choice
    | _ -> raise (Protocol.Invalid_response "missing MiniMax Chat choices") in
  let message = Protocol.member "message" choice in
  (match Protocol.member "tool_calls" message with
  | `List calls -> List.iter (fun call ->
      let fn = Protocol.member "function" call in
      match Protocol.member "type" call,
        Protocol.member "arguments" fn with
      | `String "function", `String arguments ->
          (match (try Some (Yojson.Basic.from_string arguments)
            with Yojson.Json_error _ -> None) with
          | Some (`Assoc _) -> ()
          | _ -> raise (Protocol.Invalid_response
              "invalid MiniMax function arguments"))
      | _ -> raise (Protocol.Invalid_response
          "invalid MiniMax function call")) calls
  | _ -> ());
  let result = Protocol.parse_completion json in
  let fields = List.filter_map (fun name ->
    match Protocol.member name message with
    | `Null -> None
    | (`List _ as value) when name = "reasoning_details" -> Some (name, value)
    | (`String _ as value) when name = "reasoning_content" -> Some (name, value)
    | _ -> raise (Protocol.Invalid_response ("invalid MiniMax " ^ name)))
    ["reasoning_details"; "reasoning_content"] in
  if fields = [] then result
  else { result with provider_state = Some (`Assoc fields) }
