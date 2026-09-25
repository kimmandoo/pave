(* Yolo-Auto's hosted OpenAI-compatible Chat API.
   https://yolo-auto.com/docs#openai-compatible-api-endpoints
   https://yolo-auto.com/docs#request-examples
   https://yolo-auto.com/models
   GET /v1/models lists the models available to the authenticated plan. Its
   context and thinking metadata are plan-bounded; IDs alone cannot establish
   tool or reasoning support for each model. *)
let models_url = "https://yolo-auto.com/v1/models"
let chat_url = "https://yolo-auto.com/v1/chat/completions"
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

let env_api_key () = match Sys.getenv_opt "YOLO_AUTO_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Yolo-Auto credential requires the official hosted Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Yolo-Auto API key";
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
          let ids = ref [] in
          let valid = List.for_all (function
            | `Assoc fields ->
                (match List.assoc_opt "id" fields, List.assoc_opt "object" fields with
                | Some (`String id), (None | Some (`String "model")) when valid_id id ->
                    if not (Hashtbl.mem seen id) then (
                      Hashtbl.add seen id ();
                      ids := id :: !ids);
                    true
                | _ -> false)
            | _ -> false) rows in
          if valid then Ok (List.rev !ids)
          else Error (Invalid_response "invalid Yolo-Auto model object")
      | Some (`String "list"), Some (`List _) ->
          Error (Invalid_response "too many Yolo-Auto models")
      | _ -> Error (Invalid_response "invalid Yolo-Auto model listing"))
  | _ -> Error (Invalid_response "malformed Yolo-Auto model listing")

(* The production HTTPS executor must cap downloads and refuse redirects. The
   credential is only sent to the first-party, pinned discovery endpoint. *)
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

(* The documented Pi integration uses the Qwen chat-template thinking format.
   When Chat returns reasoning_content, carry its authentic text with the
   assistant tool call; never synthesize thinking or forward unrelated state. *)
let reasoning_fields json = match json with
  | `Assoc fields ->
      (match List.assoc_opt "reasoning_content" fields with
      | None | Some `Null -> []
      | Some (`String _ as reasoning) -> ["reasoning_content", reasoning]
      | Some _ -> raise (Protocol.Invalid_response "invalid Yolo-Auto reasoning_content"))
  | _ -> raise (Protocol.Invalid_response "invalid Yolo-Auto assistant state")

let request ~model messages tools =
  let message (msg : Protocol.message) =
    let json = Protocol.message_to_json msg in
    match json with
    | `Assoc fields when msg.role = "assistant" ->
        let fields = if msg.content = None then fields @ ["content", `Null]
          else fields in
        let reasoning = match msg.provider_state with
          | None -> []
          | Some state -> reasoning_fields state in
        `Assoc (fields @ reasoning)
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
