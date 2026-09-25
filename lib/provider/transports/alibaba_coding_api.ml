(* Alibaba Cloud Model Studio Coding Plan, not pay-as-you-go Model Studio.
   https://help.aliyun.com/en/model-studio/coding-plan
   https://help.aliyun.com/en/model-studio/coding-plan-faq
   https://help.aliyun.com/en/model-studio/qwen-function-calling
   The user must select a region explicitly. A plan key cannot be used with
   /compatible-mode/v1, and no account-scoped Coding Plan model-list GET is
   documented. The plan's published model names are not a dynamic catalog. *)
let china_chat_url = "https://coding.dashscope.aliyuncs.com/v1/chat/completions"
let intl_chat_url = "https://coding-intl.dashscope.aliyuncs.com/v1/chat/completions"

let valid_key key = String.starts_with ~prefix:"sk-sp-" key &&
  String.length key > String.length "sk-sp-" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let env_api_key () = match Sys.getenv_opt "ALIBABA_CODING_PLAN_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> china_chat_url && endpoint <> intl_chat_url then
    invalid_arg "Coding Plan key requires an explicitly selected official Coding Plan Chat endpoint";
  if not (valid_key api_key) then
    invalid_arg "invalid Coding Plan API key";
  ["Authorization: Bearer " ^ api_key]

(* The documented Chat function loop appends the assistant's native tool call,
   then role=tool with the original tool_call_id. Replay a reported Qwen
   reasoning_content intact; never enable/disable thinking based on model ID. *)
let request ~model messages tools =
  let message (msg : Protocol.message) =
    let json = Protocol.message_to_json msg in
    match json with
    | `Assoc fields when msg.role = "assistant" ->
        let fields = if msg.content = None then
          fields @ ["content", `String ""] else fields in
        let fields = match msg.provider_state with
          | Some (`Assoc state) ->
              (match List.assoc_opt "reasoning_content" state with
              | Some (`String _ as reasoning) ->
                  fields @ ["reasoning_content", reasoning]
              | _ -> fields)
          | _ -> fields in
        `Assoc fields
    | _ -> json in
  let fields = ["model", `String model;
    "messages", Protocol.chat_messages_to_json ~serialize:message messages;
    "stream", `Bool false] in
  `Assoc (if tools = [] then fields else fields @ ["tools", `List tools])

let parse_completion json =
  let choice = match Protocol.member "choices" json with
    | `List (choice :: _) -> choice
    | _ -> raise (Protocol.Invalid_response "missing Coding Plan choices") in
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
              "invalid Coding Plan function arguments"))
      | _ -> raise (Protocol.Invalid_response
          "invalid Coding Plan function call")) calls
  | _ -> ());
  let result = Protocol.parse_completion json in
  match Protocol.member "reasoning_content" message with
  | `Null -> result
  | `String reasoning ->
      { result with provider_state = Some (`Assoc ["reasoning_content", `String reasoning]) }
  | _ -> raise (Protocol.Invalid_response "invalid Coding Plan reasoning_content")
