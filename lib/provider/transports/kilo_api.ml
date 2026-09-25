(* Kilo AI Gateway's public model catalog and OpenAI-compatible Chat API.
   https://kilo.ai/docs/gateway/api-reference
   https://kilo.ai/docs/gateway/authentication
   https://kilo.ai/docs/getting-started/setup-authentication#kilo-gateway-api-key
   /models is anonymous: never send a Kilo API key to the listing endpoint.
   Listing metadata does not certify Chat or tool support for individual IDs;
   do not route by model name. FIM has a separate, unsupported endpoint. *)
let models_url = "https://api.kilo.ai/api/gateway/models"
let chat_url = "https://api.kilo.ai/api/gateway/chat/completions"
let max_response_bytes = 1_048_576
let max_models = 4096

type error =
  | Invalid_credential
  | Transport_error
  | Http_error of int
  | Invalid_response of string

type http = url:string -> headers:(string * string) list ->
  (int * string, error) result

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let env_api_key () = match Sys.getenv_opt "KILO_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Kilo API key requires the official Gateway Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Kilo Gateway API key";
  ["Authorization: Bearer " ^ api_key]

let valid_id id = id <> "" && String.length id <= 256 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) id

let parse_models body =
  let json = try Some (Yojson.Basic.from_string body)
    with Yojson.Json_error _ -> None in
  match json with
  | Some (`Assoc fields) ->
      (match List.assoc_opt "data" fields with
       | Some (`List rows) when List.length rows <= max_models ->
           let seen = Hashtbl.create (List.length rows) in
           let ids = ref [] in
           let valid = List.for_all (function
             | `Assoc model ->
                 (match List.assoc_opt "id" model, List.assoc_opt "object" model with
                  | Some (`String id), Some (`String "model") when valid_id id ->
                      if not (Hashtbl.mem seen id) then (
                        Hashtbl.add seen id ();
                        ids := id :: !ids);
                      true
                  | _ -> false)
             | _ -> false) rows in
           if valid then Ok (List.rev !ids)
           else Error (Invalid_response "invalid Kilo model object")
       | Some (`List _) -> Error (Invalid_response "too many Kilo models")
       | _ -> Error (Invalid_response "missing Kilo model data array"))
  | _ -> Error (Invalid_response "malformed Kilo model listing")

(* The production HTTPS executor caps downloads and disallows redirects.
   The documented listing is anonymous; an optional key is checked but never sent. *)
let discover ~http ~api_key () =
  if api_key <> "" && not (valid_key api_key) then Error Invalid_credential
  else match http ~url:models_url ~headers:["Accept", "application/json"] with
    | Error failure -> Error failure
    | Ok (status, _) when status < 200 || status >= 300 ->
        Error (Http_error status)
    | Ok (_, body) when String.length body > max_response_bytes ->
        Error (Invalid_response "listing exceeds size limit")
    | Ok (_, body) -> parse_models body

(* A tool result must follow the original assistant tool call. Preserve any
   supplied reasoning extension verbatim across turns; the official response
   schema does not promise it, so no reasoning fields are synthesized. *)
let reasoning_fields json = match json with
  | `Assoc fields ->
      List.filter_map (fun key -> match List.assoc_opt key fields with
        | None | Some `Null -> None
        | Some (`String _ as value) when key <> "reasoning_details" ->
            Some (key, value)
        | Some (`List _ as value) when key = "reasoning_details" ->
            Some (key, value)
        | Some _ -> raise (Protocol.Invalid_response ("invalid Kilo " ^ key)))
        ["reasoning"; "reasoning_content"; "reasoning_details"]
  | _ -> raise (Protocol.Invalid_response "invalid Kilo assistant state")

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
  let assistant = match Protocol.member "choices" json with
    | `List (choice :: _) -> Protocol.member "message" choice
    | _ -> assert false in
  match reasoning_fields assistant with
  | [] -> result
  | fields -> { result with provider_state = Some (`Assoc fields) }
