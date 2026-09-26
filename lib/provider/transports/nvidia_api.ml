(* NVIDIA API Catalog uses the hosted NIM Chat Completions wire format. The
   authenticated model list reports IDs, not tool capability or token limits;
   callers must not derive either from model names. *)
let models_url = "https://integrate.api.nvidia.com/v1/models"
let chat_url = "https://integrate.api.nvidia.com/v1/chat/completions"
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

let env_api_key () = match Sys.getenv_opt "NVIDIA_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "NVIDIA credential requires the official hosted Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid NVIDIA API key";
  ["Authorization: Bearer " ^ api_key]

(* NVIDIA's hosted Chat schema requires content even for an assistant message
   containing only tool_calls. Protocol.message_to_json omits absent content. *)
let message_to_json (message : Protocol.message) =
  let json = Protocol.message_to_json message in
  match message.role, message.content, json with
  | "assistant", None, `Assoc fields -> `Assoc (fields @ ["content", `Null])
  | _ -> json

let request ~model messages tools =
  let fields = ["model", `String model;
    "messages", Protocol.chat_messages_to_json ~serialize:message_to_json messages;
    "stream", `Bool false] in
  let fields = if tools = [] then fields else fields @ ["tools", `List tools] in
  `Assoc fields

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
          else Error (Invalid_response "invalid NVIDIA model ID")
      | _ -> Error (Invalid_response "missing data array"))
  | _ -> Error (Invalid_response "malformed model listing")

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
