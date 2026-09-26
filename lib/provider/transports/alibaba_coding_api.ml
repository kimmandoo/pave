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

(* The documented Qwen Code config uses enable_thinking as an explicit
   on/off request setting. It is omitted by default and never inferred from a
   model ID. *)
let region = function
  | endpoint when endpoint = china_chat_url -> "china"
  | endpoint when endpoint = intl_chat_url -> "intl"
  | _ -> invalid_arg "invalid Alibaba Coding Plan region endpoint"

let state_tag endpoint model = [
  "provider", `String "alibaba-coding-plan";
  "route", `String (region endpoint);
  "model", `String model;
]

let matching_state endpoint model = function
  | Some (`Assoc fields)
    when List.for_all (fun (key, value) -> List.assoc_opt key fields = Some value)
      (state_tag endpoint model) -> Some fields
  | _ -> None

let thinking_enabled = function
  | None -> None
  | Some "none" -> Some false
  | Some ("minimal" | "low" | "medium" | "high" | "xhigh" | "max") ->
      Some true
  | Some _ -> invalid_arg
      "Alibaba Coding Plan thinking level must be none, minimal, low, medium, high, xhigh, or max"

let request ~endpoint ~model ?thinking messages tools =
  let serialize (msg : Protocol.message) =
    match Protocol.message_to_json msg with
    | `Assoc fields when msg.role = "assistant" ->
        let fields = if msg.content = None &&
            not (List.mem_assoc "content" fields) then
          fields @ ["content", `String ""] else fields in
        let fields = match matching_state endpoint model msg.provider_state with
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

let parse_completion ~endpoint ~model json =
  let choice = match Protocol.member "choices" json with
    | `List (choice :: _) -> Protocol.member "message" choice
    | _ -> raise (Protocol.Invalid_response "missing Coding Plan choices") in
  (match Protocol.member "tool_calls" choice with
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
  match Protocol.member "reasoning_content" choice with
  | `Null -> result
  | `String reasoning ->
      { result with provider_state = Some (`Assoc
          (state_tag endpoint model @
            ["reasoning_content", `String reasoning])) }
  | _ -> raise (Protocol.Invalid_response "invalid Coding Plan reasoning_content")
