(* Global SiliconFlow hosted API, not the separately hosted China endpoint.
   https://docs.siliconflow.com/en/api-reference/models/get-model-list
   https://docs.siliconflow.com/en/api-reference/chat-completions/chat-completions
   https://docs.siliconflow.com/en/userguide/guides/function-calling
   The model listing has no per-model tool capability or context metadata: the
   documented type/sub_type query selects chat models, not tool-enabled ones. *)
let models_url = "https://api.siliconflow.com/v1/models?type=text&sub_type=chat"
let chat_url = "https://api.siliconflow.com/v1/chat/completions"
let cn_models_url = "https://api.siliconflow.cn/v1/models?type=text&sub_type=chat"
let cn_chat_url = "https://api.siliconflow.cn/v1/chat/completions"
let max_response_bytes = 1_048_576

type error =
  | Invalid_credential
  | Transport_error
  | Http_error of int
  | Invalid_response of string

type http = url:string -> headers:(string * string) list ->
  (int * string, error) result

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let env_api_key () = match Sys.getenv_opt "SILICONFLOW_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "SiliconFlow credential requires the official global Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid SiliconFlow API key";
  ["Authorization: Bearer " ^ api_key]
let cn_chat_headers ~endpoint ~api_key =
  if endpoint <> cn_chat_url then
    invalid_arg "SiliconFlow CN credential requires the official China Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid SiliconFlow CN API key";
  ["Authorization: Bearer " ^ api_key]

let valid_id id = id <> "" && String.length id <= 256 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) id

let parse_models body =
  let json = try Some (Yojson.Basic.from_string body)
    with Yojson.Json_error _ -> None in
  match json with
  | Some (`Assoc fields) ->
      (match List.assoc_opt "data" fields with
      | Some (`List rows) ->
          let seen = Hashtbl.create (List.length rows) in
          let models = ref [] in
          let valid = List.for_all (function
            | `Assoc fields ->
                (match List.assoc_opt "id" fields with
                | Some (`String id) when valid_id id ->
                    if Hashtbl.mem seen id then false
                    else (
                      Hashtbl.add seen id ();
                      models := id :: !models;
                      true)
                | _ -> false)
            | _ -> false) rows in
          if valid then Ok (List.rev !models)
          else Error (Invalid_response "invalid SiliconFlow model ID")
      | _ -> Error (Invalid_response "missing data array"))
  | _ -> Error (Invalid_response "malformed model listing")

(* Production HTTP imposes its own download cap and disallows redirecting
   bearer credentials. Keep the injected request pinned for testability. *)
let discover ~http ~api_key () =
  if not (valid_key api_key) then Error Invalid_credential
  else
    let headers = ["Authorization", "Bearer " ^ api_key;
      "Accept", "application/json"] in
    match http ~url:models_url ~headers with
    | Error failure -> Error failure
    | Ok (status, _) when status < 200 || status >= 300 ->
        Error (Http_error status)
    | Ok (_, body) when String.length body > max_response_bytes ->
        Error (Invalid_response "listing exceeds size limit")
    | Ok (_, body) -> parse_models body

(* SiliconFlow's function-calling guide appends the returned assistant tool
   message, then a role=tool message carrying its original tool_call_id.
   Preserve reasoning_content where supplied, without guessing whether a
   discovered model supports thinking. Certain model-specific combinations,
   e.g. DeepSeek-V3.1 tools with enable_thinking=true, need vendor-specific
   selection and are not inferred from model IDs here. *)
let request ~model messages tools =
  let message (msg : Protocol.message) =
    let json = Protocol.message_to_json msg in
    match json with
    | `Assoc fields when msg.role = "assistant" ->
        let fields = if msg.content = None then fields @ ["content", `Null]
          else fields in
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
  let result = Protocol.parse_completion json in
  let message = match Protocol.member "choices" json with
    | `List (choice :: _) -> Protocol.member "message" choice
    | _ -> assert false in
  match Protocol.member "reasoning_content" message with
  | `Null -> result
  | `String reasoning ->
      { result with provider_state = Some (`Assoc ["reasoning_content", `String reasoning]) }
  | _ -> raise (Protocol.Invalid_response "invalid SiliconFlow reasoning_content")
