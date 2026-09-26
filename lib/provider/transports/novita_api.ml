(* Novita's OpenAI-compatible hosted Chat and authenticated LLM model listing.
   https://docs.novita.ai/guides/llm-api
   https://docs.novita.ai/api-reference/model-apis-llm-list-models
   https://docs.novita.ai/guides/llm-function-calling
   https://docs.novita.ai/guides/llm-interleaved-thinking
   The listing returns IDs, not per-model function-calling entitlements. *)
let models_url = "https://api.novita.ai/openai/v1/models"
let chat_url = "https://api.novita.ai/openai/v1/chat/completions"
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

let env_api_key () = match Sys.getenv_opt "NOVITA_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Novita credential requires the official hosted Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Novita API key";
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
          else Error (Invalid_response "invalid Novita model ID")
      | _ -> Error (Invalid_response "missing data array"))
  | _ -> Error (Invalid_response "malformed Novita model listing")

(* Production supplies a bounded HTTPS executor which must not follow redirects.
   Never send the bearer token to a caller-specified listing URL. *)
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

(* Function calling replays the assistant's call before the tool's result.
   Interleaved thinking also requires replaying reasoning_details, if returned.
   Preserve reasoning_content when supplied without inferring a model's thinking
   mode or inventing missing state for model-specific requirements. *)
let reasoning_fields json = match json with
  | `Assoc fields ->
      List.filter_map (fun key -> match List.assoc_opt key fields with
        | None | Some `Null -> None
        | Some (`String _ as value) when key = "reasoning_content" ->
            Some (key, value)
        | Some (`List segments as value) when key = "reasoning_details" &&
            List.for_all (function `Assoc _ -> true | _ -> false) segments ->
            Some (key, value)
        | Some _ -> raise (Protocol.Invalid_response ("invalid Novita " ^ key)))
        ["reasoning_content"; "reasoning_details"]
  | _ -> raise (Protocol.Invalid_response "invalid Novita assistant state")

let request ~model messages tools =
  let message (msg : Protocol.message) =
    let json = Protocol.message_to_json msg in
    match json with
    | `Assoc fields when msg.role = "assistant" ->
        let fields = if msg.content = None then fields @ ["content", `Null]
          else fields in
        let fields = match msg.provider_state with
          | None -> fields
          | Some state -> fields @ reasoning_fields state in
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
  match reasoning_fields message with
  | [] -> result
  | fields -> { result with provider_state = Some (`Assoc fields) }
