(* SingularityAPI's booked .tech lane, not its pay-as-you-go .dev API.
   The sibling oh-my-pi runtime pins api.singularityapi.tech/v1 and reports
   successful bearer-authenticated Chat probes in September 2026. An
   unauthenticated POST /v1/chat/completions confirms the route and requires
   "Authorization: Bearer <key>". Public .tech documentation describes Chat
   and tool calls, but gives neither an authenticated Chat response example
   nor a model-listing contract. No models or entitlements are inferred here. *)
let chat_url = "https://api.singularityapi.tech/v1/chat/completions"
let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let env_api_key () = match Sys.getenv_opt "SINGULARITYAPI_TECH_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "reserved lane credential requires the pinned .tech Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid reserved lane API key";
  ["Authorization: Bearer " ^ api_key]

let reasoning_fields json = match json with
  | `Assoc fields ->
      (match List.assoc_opt "reasoning_content" fields with
      | None | Some `Null -> []
      | Some (`String _ as reasoning) -> ["reasoning_content", reasoning]
      | Some _ -> raise (Protocol.Invalid_response "invalid lane reasoning_content"))
  | _ -> raise (Protocol.Invalid_response "invalid lane assistant state")

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
