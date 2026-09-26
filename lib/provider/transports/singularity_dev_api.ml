(* Universal api.singularityapi.dev gateway, not the unrelated .tech lane.
   https://docs.singularityapi.dev/api/models-receipts.md
   https://docs.singularityapi.dev/api/chat-completions.md
   https://docs.singularityapi.dev/get-started/open-ai-compatibility.md
   The authenticated listing identifies Chat endpoint support, not per-model
   function-tool support. Do not classify its Chat models as tool-capable. *)
let models_url = "https://api.singularityapi.dev/v1/models"
let chat_url = "https://api.singularityapi.dev/v1/chat/completions"
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

let env_api_key () = match Sys.getenv_opt "SINGULARITYAPI_DEV_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "SingularityAPI credential requires the official .dev Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid SingularityAPI .dev API key";
  ["Authorization: Bearer " ^ api_key]

let valid_id id = id <> "" && String.length id <= 256 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) id

let parse_models body =
  let json = try Some (Yojson.Basic.from_string body)
    with Yojson.Json_error _ -> None in
  match json with
  | Some (`Assoc fields) when List.assoc_opt "object" fields = Some (`String "list") ->
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
                      match List.assoc_opt "object" fields,
                            List.assoc_opt "capabilities" fields with
                      | Some (`String "model"), Some (`List capabilities) ->
                          let chat = ref false in
                          let valid_capabilities = List.for_all (function
                            | `Assoc fields ->
                                (match List.assoc_opt "endpoint" fields with
                                | Some (`String "/v1/chat/completions") ->
                                    chat := true; true
                                | Some (`String _) -> true
                                | _ -> false)
                            | _ -> false) capabilities in
                          if valid_capabilities then (
                            if !chat then models := id :: !models;
                            true)
                          else false
                      | _ -> false)
                | _ -> false)
            | _ -> false) rows in
          if valid then Ok (List.rev !models)
          else Error (Invalid_response "invalid SingularityAPI model capability")
      | _ -> Error (Invalid_response "missing data array"))
  | _ -> Error (Invalid_response "malformed model listing")

(* Production HTTP caps downloads and refuses bearer-token redirects.
   Discovery has no caller-supplied URL or query string. *)
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

(* Preserve the returned assistant call IDs, null content, and any actual
   reasoning_content when continuing with OpenAI-native role=tool results.
   Never add model-family-specific reasoning settings based on the model ID. *)
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
  | _ -> raise (Protocol.Invalid_response "invalid SingularityAPI reasoning_content")
