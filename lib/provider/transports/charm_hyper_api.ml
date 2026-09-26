(* Charm Hyper's hosted OpenAI-compatible Chat API and public model catalog.
   https://hyper.charm.land/docs/api/authentication.md
   https://hyper.charm.land/docs/api/openai-chat-completions.md
   https://hyper.charm.land/docs/api/list-models.md
   The catalog does not assert per-model tool support. *)
let models_url = "https://hyper.charm.land/v1/models"
let chat_url = "https://hyper.charm.land/v1/chat/completions"
let max_response_bytes = 1_048_576
let max_models = 4096

type error =
  | Transport_error
  | Http_error of int
  | Invalid_response of string

type http = url:string -> headers:(string * string) list ->
  (int * string, error) result

let valid_key key = String.starts_with ~prefix:"sk-hyper-" key &&
  String.length key > String.length "sk-hyper-" &&
  String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let env_api_key () =
  let read name = match Sys.getenv_opt name with
    | Some key when valid_key key -> Some key
    | _ -> None in
  match read "CHARM_HYPER_API_KEY" with
  | Some _ as key -> key
  | None -> read "HYPER_API_KEY"

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Charm Hyper credential requires the official hosted Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Charm Hyper API key";
  ["Authorization: Bearer " ^ api_key]

let valid_id id = id <> "" && String.length id <= 256 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) id

let parse_models body =
  let json = try Some (Yojson.Basic.from_string body)
    with Yojson.Json_error _ -> None in
  match json with
  | Some (`Assoc fields) ->
      (match List.assoc_opt "object" fields, List.assoc_opt "data" fields with
      | Some (`String "list"), Some (`List rows) when List.length rows <= max_models ->
          let seen = Hashtbl.create (List.length rows) in
          let duplicate = ref false in
          let ids = ref [] in
          let valid = List.for_all (function
            | `Assoc fields ->
                (match List.assoc_opt "id" fields,
                  List.assoc_opt "object" fields with
                | Some (`String id), Some (`String "model") when valid_id id ->
                    if Hashtbl.mem seen id then duplicate := true
                    else (
                      Hashtbl.add seen id ();
                      ids := id :: !ids);
                    true
                | _ -> false)
            | _ -> false) rows in
          if valid && not !duplicate then Ok (List.rev !ids)
          else Error (Invalid_response "invalid Charm Hyper model object")
      | Some (`String "list"), Some (`List _) ->
          Error (Invalid_response "too many Charm Hyper models")
      | _ -> Error (Invalid_response "invalid Charm Hyper model listing"))
  | _ -> Error (Invalid_response "malformed Charm Hyper model listing")

(* GET /v1/models is explicitly public; never transmit a credential with
   discovery, nor accept a caller-specified destination. The HTTPS executor
   must cap downloads and must not follow redirects. *)
let discover ~http ~api_key:_ () =
  match http ~url:models_url ~headers:["Accept", "application/json"] with
  | Error failure -> Error failure
  | Ok (status, _) when status < 200 || status >= 300 ->
      Error (Http_error status)
  | Ok (_, body) when String.length body > max_response_bytes ->
      Error (Invalid_response "listing exceeds size limit")
  | Ok (_, body) -> parse_models body

(* Some Hyper models return reasoning_content in addition to the standard
   OpenAI Chat fields. Keep it with the assistant turn for tool continuation. *)
let reasoning_fields json = match json with
  | `Assoc fields ->
      (match List.assoc_opt "reasoning_content" fields with
      | None | Some `Null -> []
      | Some (`String _ as value) -> ["reasoning_content", value]
      | Some _ -> raise (Protocol.Invalid_response
          "invalid Charm Hyper reasoning_content"))
  | _ -> raise (Protocol.Invalid_response "invalid Charm Hyper assistant state")

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
