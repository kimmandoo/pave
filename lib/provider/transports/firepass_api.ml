(* Fire Pass has a dedicated fpk_ credential, not a standard Fireworks fw_ key.
   https://docs.fireworks.ai/firepass
   https://docs.fireworks.ai/guides/function-calling
   https://docs.fireworks.ai/guides/reasoning
   Fire Pass documents its Chat endpoint and manual full router resource IDs.
   Official FireConnect source says fpk_ keys cannot list the catalog and
   instead uses frozen fallback routers; we require the user's full router ID
   rather than freeze transient entitlements or query standard Fireworks:
   https://github.com/fw-ai/fireconnect/blob/main/packages/setup-cli/lib/fireworks/models.mjs *)
let chat_url = "https://api.fireworks.ai/inference/v1/chat/completions"
let router_prefix = "accounts/fireworks/routers/"

let valid_key key = String.starts_with ~prefix:"fpk_" key &&
  String.length key > String.length "fpk_" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let env_api_key () = match Sys.getenv_opt "FIREPASS_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Fire Pass key requires the official Fire Pass Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Fire Pass API key";
  ["Authorization: Bearer " ^ api_key]

(* There is no stable published account-scoped listing for fpk_ credentials.
   A caller supplies the full router resource ID enabled for their own pass;
   do not derive a router from a model family, a short alias, or a default. *)
let valid_model model =
  String.starts_with ~prefix:router_prefix model &&
  String.length model > String.length router_prefix &&
  String.length model <= 256 &&
  String.rindex_opt model '/' = Some (String.length router_prefix - 1) &&
  String.for_all (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' | '/' -> true
    | _ -> false) model

(* Fireworks requires the original assistant tool_calls before each tool
   result and, when present, reasoning_content for interleaved thinking. *)
let reasoning_content message =
  match Protocol.member "reasoning_content" message with
  | `Null -> None
  | `String _ as reasoning -> Some reasoning
  | _ -> raise (Protocol.Invalid_response "invalid Fire Pass reasoning_content")

let request ~model messages tools =
  if not (valid_model model) then
    invalid_arg "Fire Pass requires a full accounts/fireworks/routers/<id> model ID";
  let message (msg : Protocol.message) =
    let json = Protocol.message_to_json msg in
    match json with
    | `Assoc fields when msg.role = "assistant" ->
        let fields = if msg.content = None then fields @ ["content", `Null]
          else fields in
        let fields = match msg.provider_state with
          | None -> fields
          | Some state -> (match reasoning_content state with
              | None -> fields
              | Some reasoning -> fields @ ["reasoning_content", reasoning]) in
        `Assoc fields
    | _ -> json in
  let fields = ["model", `String model;
    "messages", `List (List.map message messages);
    "stream", `Bool false] in
  `Assoc (if tools = [] then fields else fields @ ["tools", `List tools])

let parse_completion json =
  let result = Protocol.parse_completion json in
  let message = match Protocol.member "choices" json with
    | `List (choice :: _) -> Protocol.member "message" choice
    | _ -> assert false in
  match reasoning_content message with
  | None -> result
  | Some reasoning ->
      { result with provider_state = Some (`Assoc ["reasoning_content", reasoning]) }
