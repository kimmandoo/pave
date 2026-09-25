(* Cloudflare AI Gateway's account-scoped, OpenAI-compatible unified route.
   https://developers.cloudflare.com/ai-gateway/usage/chat-completion/
   The explicit account/gateway identity is required even with a valid token.
   BYOK or Unified Billing must be configured at Cloudflare: the gateway token
   must never be forwarded as the upstream provider's Authorization header.
   Cloudflare does not document a gateway-specific model-list endpoint. *)
let base_url = "https://gateway.ai.cloudflare.com/v1/"
let max_response_bytes = 1_048_576

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let env_api_key () = match Sys.getenv_opt "CLOUDFLARE_AI_GATEWAY_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let valid_account_id id = String.length id = 32 &&
  String.for_all (function
    | 'a'..'f' | 'A'..'F' | '0'..'9' -> true
    | _ -> false) id

let valid_gateway_id id = id <> "" && String.length id <= 64 &&
  String.for_all (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' -> true
    | _ -> false) id

let chat_url ~account_id ~gateway_id =
  if not (valid_account_id account_id) then
    invalid_arg "invalid Cloudflare account ID";
  if not (valid_gateway_id gateway_id) then
    invalid_arg "invalid Cloudflare gateway ID";
  base_url ^ account_id ^ "/" ^ gateway_id ^ "/compat/chat/completions"

let env_chat_url () = match Sys.getenv_opt "CLOUDFLARE_ACCOUNT_ID",
  Sys.getenv_opt "CLOUDFLARE_GATEWAY_ID" with
  | Some account_id, Some gateway_id -> Some (chat_url ~account_id ~gateway_id)
  | _ -> None

(* An exact comparison with the trusted account/gateway-derived URL rejects
   lookalike hosts, arbitrary path prefixes, redirects, and userinfo URLs. *)
let chat_headers ~endpoint ~api_key =
  if not (valid_key api_key) then invalid_arg "invalid Cloudflare gateway token";
  if Some endpoint <> env_chat_url () then
    invalid_arg "Cloudflare gateway token requires its configured account and gateway endpoint";
  ["cf-aig-authorization: Bearer " ^ api_key]

let valid_id id = id <> "" && String.length id <= 256 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) id

(* Keep optional reasoning extensions with the original assistant turn when
   replaying a tool result; never synthesize reasoning for a model ID. *)
let reasoning_fields json = match json with
  | `Assoc fields ->
      List.filter_map (fun key -> match List.assoc_opt key fields with
        | None | Some `Null -> None
        | Some (`String _ as value) when key <> "reasoning_details" ->
            Some (key, value)
        | Some (`List _ as value) when key = "reasoning_details" ->
            Some (key, value)
        | Some _ ->
            raise (Protocol.Invalid_response ("invalid Cloudflare " ^ key)))
        ["reasoning"; "reasoning_content"; "reasoning_details"]
  | _ -> raise (Protocol.Invalid_response "invalid Cloudflare assistant state")

(* /compat requires a provider-qualified model ID, including dynamic/<route>
   for an account-configured dynamic route. No static model list is guessed. *)
let request ~model messages tools =
  if not (valid_id model) || not (String.contains model '/') then
    invalid_arg "Cloudflare unified gateway requires a provider/model or dynamic/route ID";
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
    "messages", `List (List.map message messages);
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
