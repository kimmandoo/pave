(* Synthetic OpenAI-compatible API, not the separate Anthropic-compatible API.
   https://dev.synthetic.new/docs/api/overview
   https://dev.synthetic.new/docs/openai/models
   https://dev.synthetic.new/docs/openai/chat-completions
   /models supplies IDs, not a documented per-model tool-capability schema;
   listed IDs are unclassified (the catalog includes embedding models). *)
let models_url = "https://api.synthetic.new/openai/v1/models"
let chat_url = "https://api.synthetic.new/openai/v1/chat/completions"
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

let env_api_key () = match Sys.getenv_opt "SYNTHETIC_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Synthetic credential requires the official OpenAI Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Synthetic API key";
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
            | `Assoc row ->
                (match List.assoc_opt "id" row with
                | Some (`String id) when valid_id id ->
                    if Hashtbl.mem seen id then false
                    else (
                      Hashtbl.add seen id ();
                      ids := id :: !ids;
                      true)
                | _ -> false)
            | _ -> false) rows in
          if valid then Ok (List.rev !ids)
          else Error (Invalid_response "invalid Synthetic model object")
      | Some (`List _) -> Error (Invalid_response "too many Synthetic models")
      | _ -> Error (Invalid_response "missing data array"))
  | _ -> Error (Invalid_response "malformed Synthetic model listing")

(* The injected executor must enforce HTTPS, response bounds and no redirects;
   this layer additionally caps responses from any injected implementation. *)
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

(* Do not infer reasoning mode or tool support from an ID. A returned
   reasoning_content accompanies the assistant's tool calls on replay. *)
let reasoning_fields json = match json with
  | `Assoc fields ->
      (match List.assoc_opt "reasoning_content" fields with
      | None | Some `Null -> []
      | Some (`String _ as value) -> ["reasoning_content", value]
      | Some _ -> raise (Protocol.Invalid_response "invalid Synthetic reasoning_content"))
  | _ -> raise (Protocol.Invalid_response "invalid Synthetic assistant state")

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
  let choice = match Protocol.member "choices" json with
    | `List (choice :: _) -> choice
    | _ -> raise (Protocol.Invalid_response "missing Synthetic choices") in
  let message = Protocol.member "message" choice in
  (match Protocol.member "tool_calls" message with
  | `List calls -> List.iter (fun call ->
      let fn = Protocol.member "function" call in
      match Protocol.member "type" call, Protocol.member "arguments" fn with
      | `String "function", `String arguments ->
          (match (try Some (Yojson.Basic.from_string arguments)
            with Yojson.Json_error _ -> None) with
          | Some (`Assoc _) -> ()
          | _ -> raise (Protocol.Invalid_response
              "invalid Synthetic function arguments"))
      | _ -> raise (Protocol.Invalid_response
          "invalid Synthetic function call")) calls
  | _ -> ());
  let result = Protocol.parse_completion json in
  match reasoning_fields message with
  | [] -> result
  | fields -> { result with provider_state = Some (`Assoc fields) }
