(* OpenCode Go is the subscription route, not pay-as-you-go OpenCode Zen.
   https://opencode.ai/docs/go/#endpoints
   The account key is obtained at https://opencode.ai/auth after subscribing to Go.
   The Go model listing includes IDs but no per-ID protocol/tool capabilities;
   a user must explicitly select a known Chat model to use this transport.
   Other documented models use /v1/responses or /v1/messages, not Chat. *)
let models_url = "https://opencode.ai/zen/go/v1/models"
let chat_url = "https://opencode.ai/zen/go/v1/chat/completions"
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

let env_api_key () = match Sys.getenv_opt "OPENCODE_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "OpenCode Go credential requires the official Go Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid OpenCode Go API key";
  ["Authorization: Bearer " ^ api_key; "User-Agent: pave"]

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
            | `Assoc row ->
                (match List.assoc_opt "id" row, List.assoc_opt "object" row with
                | Some (`String id), Some (`String "model") when valid_id id ->
                    if Hashtbl.mem seen id then duplicate := true
                    else (
                      Hashtbl.add seen id ();
                      ids := id :: !ids);
                    true
                | _ -> false)
            | _ -> false) rows in
          if valid && not !duplicate then Ok (List.rev !ids)
          else Error (Invalid_response "invalid OpenCode Go model object")
      | Some (`String "list"), Some (`List _) ->
          Error (Invalid_response "too many OpenCode Go models")
      | _ -> Error (Invalid_response "malformed OpenCode Go model listing"))
  | _ -> Error (Invalid_response "malformed OpenCode Go model listing")

(* The production executor enforces HTTPS, response limits and no redirects;
   this bound also protects callers injecting their own executor. The public
   model listing has no account-scoped entitlement or Chat/tool capabilities. *)
let discover ~http ~api_key:_ () =
  let headers = ["Accept", "application/json"; "User-Agent", "pave"] in
  match http ~url:models_url ~headers with
  | Error failure -> Error failure
  | Ok (status, _) when status < 200 || status >= 300 ->
      Error (Http_error status)
  | Ok (_, body) when String.length body > max_response_bytes ->
      Error (Invalid_response "listing exceeds size limit")
  | Ok (_, body) -> parse_models body

(* Replay only genuine reasoning returned with the preceding assistant tool
   call; never manufacture reasoning from the model name or empty state. *)
let reasoning_fields json = match json with
  | `Assoc fields ->
      (match List.assoc_opt "reasoning_content" fields with
      | None | Some `Null -> []
      | Some (`String _ as value) -> ["reasoning_content", value]
      | Some _ -> raise (Protocol.Invalid_response
          "invalid OpenCode Go reasoning_content"))
  | _ -> raise (Protocol.Invalid_response "invalid OpenCode Go assistant state")

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
  let choice = match Protocol.member "choices" json with
    | `List (choice :: _) -> choice
    | _ -> raise (Protocol.Invalid_response "missing OpenCode Go choices") in
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
              "invalid OpenCode Go function arguments"))
      | _ -> raise (Protocol.Invalid_response
          "invalid OpenCode Go function call")) calls
  | _ -> ());
  let result = Protocol.parse_completion json in
  match reasoning_fields message with
  | [] -> result
  | fields -> { result with provider_state = Some (`Assoc fields) }
