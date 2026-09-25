(* Standard Z.AI API (not the separate GLM Coding Plan endpoint).
   https://docs.z.ai/api-reference/introduction
   https://docs.z.ai/api-reference/llm/chat-completion
   https://docs.z.ai/guides/capabilities/function-calling
   https://docs.z.ai/guides/capabilities/thinking-mode
   The published API reference has no authenticated model-list endpoint; do not
   assume the OpenAI-compatible /models path exists or seed model IDs. *)
let chat_url = "https://api.z.ai/api/paas/v4/chat/completions"

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let env_api_key () = match Sys.getenv_opt "ZAI_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Z.AI credential requires the official standard Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Z.AI API key";
  ["Authorization: Bearer " ^ api_key]

let reasoning_fields json = match json with
  | `Assoc fields ->
      (match List.assoc_opt "reasoning_content" fields with
      | None | Some `Null -> []
      | Some (`String _ as value) -> ["reasoning_content", value]
      | Some _ -> raise (Protocol.Invalid_response "invalid Z.AI reasoning_content"))
  | _ -> raise (Protocol.Invalid_response "invalid Z.AI assistant state")

(* The official interleaved-thinking example returns the assistant's exact
   reasoning_content alongside its tool calls before the matching tool result.
   Do not force model-specific thinking controls or change preserved content. *)
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
    "messages", `List (List.map message messages);
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
