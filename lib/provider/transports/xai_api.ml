(* xAI inference API: the language-model listing is account-scoped, unlike a
   public catalog, and its model IDs are the request's model values.
   https://docs.x.ai/developers/rest-api-reference/inference/models
   https://docs.x.ai/developers/rest-api-reference/inference/chat-completions *)
let models_url = "https://api.x.ai/v1/language-models"
let chat_url = "https://api.x.ai/v1/chat/completions"
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

let env_api_key () = match Sys.getenv_opt "XAI_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then invalid_arg "xAI chat endpoint must be https://api.x.ai/v1/chat/completions";
  if not (valid_key api_key) then invalid_arg "invalid xAI API key";
  ["Authorization: Bearer " ^ api_key]


let valid_id id = id <> "" && String.length id <= 256 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) id

let parse_models body =
  let json = try Some (Yojson.Basic.from_string body)
    with Yojson.Json_error _ -> None in
  match json with
  | Some (`Assoc fields) ->
      (match List.assoc_opt "models" fields with
       | Some (`List rows) ->
           let seen = Hashtbl.create (List.length rows) in
           let models = ref [] in
           let valid = List.for_all (function
             | `Assoc fields ->
                 (match List.assoc_opt "id" fields with
                  | Some (`String id) when valid_id id ->
                      if not (Hashtbl.mem seen id) then (
                        Hashtbl.add seen id ();
                        models := id :: !models);
                      true
                  | _ -> false)
             | _ -> false) rows in
           if valid then Ok (List.rev !models)
           else Error (Invalid_response "invalid language-model ID")
       | _ -> Error (Invalid_response "missing models array"))
  | _ -> Error (Invalid_response "malformed language-model listing")

(* The caller owns the bounded HTTP executor (Model_discovery.default_http in
   production). This keeps the wire helper independent of Provider, which
   itself consumes the helper. Only this pinned URL is ever handed to HTTP. *)
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

(* Chat Completions is stateless. xAI returns reasoning_content on some model
   responses; preserve it with the assistant's tool calls on replay instead of
   silently dropping the conversation state. The API does not require a
   guessed reasoning_effort: each model has its own documented default.
   https://docs.x.ai/developers/advanced-api-usage/prompt-caching/multi-turn *)
let request ~model messages tools =
  let message (msg : Protocol.message) =
    let json = Protocol.message_to_json msg in
    match json, msg.provider_state with
    | `Assoc fields, Some (`Assoc state) when msg.role = "assistant" ->
        (match List.assoc_opt "reasoning_content" state with
         | Some (`String _ as reasoning) ->
             `Assoc (fields @ ["reasoning_content", reasoning])
         | _ -> json)
    | _ -> json in
  let fields = ["model", `String model;
                "messages", Protocol.chat_messages_to_json ~serialize:message messages] in
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
  | _ -> raise (Protocol.Invalid_response "invalid xAI reasoning_content")
