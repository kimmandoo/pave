(* Vercel AI Gateway's global OpenAI-compatible Chat endpoint and model list.
   https://vercel.com/docs/ai-gateway/sdks-and-apis/openai-chat-completions/rest-api
   Model IDs come from the live catalog; neither an upstream provider nor tool
   capability is inferred from an ID. The bearer token is pinned to this host. *)
let models_url = "https://ai-gateway.vercel.sh/v1/models"
let chat_url = "https://ai-gateway.vercel.sh/v1/chat/completions"
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

let env_api_key () =
  let read name = match Sys.getenv_opt name with
    | Some key when valid_key key -> Some key
    | _ -> None in
  match read "AI_GATEWAY_API_KEY" with
  | Some _ as key -> key
  | None -> read "VERCEL_AI_GATEWAY_API_KEY"

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Vercel credential requires the official AI Gateway Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Vercel AI Gateway key";
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
            | `Assoc fields ->
                (match List.assoc_opt "id" fields with
                | Some (`String id) when valid_id id ->
                    if Hashtbl.mem seen id then false
                    else (
                      Hashtbl.add seen id ();
                      ids := id :: !ids;
                      true)
                | _ -> false)
            | _ -> false) rows in
          if valid then Ok (List.rev !ids)
          else Error (Invalid_response "invalid Vercel model ID")
      | _ -> Error (Invalid_response "missing or oversized Vercel model list"))
  | _ -> Error (Invalid_response "malformed Vercel model listing")

(* The supplied executor must enforce HTTPS, a response-size cap, and no
   redirects. The caller cannot choose a destination for this credential. *)
let discover ~http ~api_key () =
  if not (valid_key api_key) then Error Invalid_credential
  else
    match http ~url:models_url ~headers:[
      "Authorization", "Bearer " ^ api_key;
      "Accept", "application/json"] with
    | Error failure -> Error failure
    | Ok (status, _) when status < 200 || status >= 300 ->
        Error (Http_error status)
    | Ok (_, body) when String.length body > max_response_bytes ->
        Error (Invalid_response "Vercel model listing exceeds size limit")
    | Ok (_, body) -> parse_models body

(* Preserve gateway-provided reasoning fields, if any, on the original
   assistant message before replaying a result for that assistant's tool call. *)
let reasoning_fields json = match json with
  | `Assoc fields ->
      List.filter_map (fun key -> match List.assoc_opt key fields with
        | None | Some `Null -> None
        | Some (`String _ as value) when key <> "reasoning_details" ->
            Some (key, value)
        | Some (`List _ as value) when key = "reasoning_details" ->
            Some (key, value)
        | Some _ -> raise (Protocol.Invalid_response ("invalid Vercel " ^ key)))
        ["reasoning"; "reasoning_content"; "reasoning_details"]
  | _ -> raise (Protocol.Invalid_response "invalid Vercel assistant state")

let request ~model messages tools =
  if not (valid_id model) then invalid_arg "invalid Vercel model ID";
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
