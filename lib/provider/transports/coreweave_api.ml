(* CoreWeave Serverless Inference is delivered through W&B Inference:
   https://docs.coreweave.com/products/inference/serverless
   https://docs.wandb.ai/inference/api-reference/list-models
   https://docs.wandb.ai/inference/api-reference/chat-completions
   https://docs.wandb.ai/inference/response-settings/tool-calling
   https://docs.wandb.ai/inference/response-settings/reasoning
   These endpoints require a W&B Inference credential, not a CoreWeave
   control-plane token. A model listing reports IDs, not tool capabilities. *)
let models_url = "https://api.inference.wandb.ai/v1/models"
let chat_url = "https://api.inference.wandb.ai/v1/chat/completions"
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

(* Prefer an explicitly configured CoreWeave key, then the documented W&B
   credential for the same Serverless Inference service. *)
let env_api_key () = match Sys.getenv_opt "COREWEAVE_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> (match Sys.getenv_opt "WANDB_API_KEY" with
    | Some key when valid_key key -> Some key
    | _ -> None)

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "CoreWeave credential requires the official Serverless Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid CoreWeave Inference API key";
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
                    if not (Hashtbl.mem seen id) then (
                      Hashtbl.add seen id ();
                      models := id :: !models);
                    true
                | _ -> false)
            | _ -> false) rows in
          if valid then Ok (List.rev !models)
          else Error (Invalid_response "invalid CoreWeave model ID")
      | _ -> Error (Invalid_response "missing data array"))
  | _ -> Error (Invalid_response "malformed CoreWeave model listing")

(* The production HTTP callback must enforce HTTPS, bound the response and
   reject redirects. Never attach a bearer token to a caller-supplied URL. *)
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

(* OpenAI Chat requires the assistant tool call before a tool result. The
   optional reasoning field is reported on responses, but not required as
   input on the next turn; do not invent reasoning settings from model IDs. *)
let request ~model messages tools =
  let message (msg : Protocol.message) =
    let json = Protocol.message_to_json msg in
    match msg.role, msg.content, json with
    | "assistant", None, `Assoc fields -> `Assoc (fields @ ["content", `Null])
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
  match message with
  | `Assoc fields -> (match List.assoc_opt "reasoning" fields with
    | None | Some `Null -> result
    | Some (`String reasoning) ->
        { result with provider_state = Some (`Assoc ["reasoning", `String reasoning]) }
    | Some _ -> raise (Protocol.Invalid_response "invalid CoreWeave reasoning"))
  | _ -> result
