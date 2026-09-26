(* Kimi Code requires API-key clients to retain their true User-Agent identity. *)
let intl_openai_chat_url = "https://api.kimi.ai/coding/v1/chat/completions"
let china_openai_chat_url = "https://api.kimi.com/coding/v1/chat/completions"
let intl_messages_url = "https://api.kimi.ai/coding/v1/messages"
let china_messages_url = "https://api.kimi.com/coding/v1/messages"

let urls = [intl_openai_chat_url; china_openai_chat_url;
  intl_messages_url; china_messages_url]

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let check_endpoint endpoint api_key =
  if not (List.mem endpoint urls) then
    invalid_arg "Kimi Code credentials require a pinned regional API endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Kimi Code API key"

let chat_headers ~endpoint ~api_key =
  if endpoint <> intl_openai_chat_url && endpoint <> china_openai_chat_url then
    invalid_arg "Kimi Code API key requires its pinned OpenAI-compatible endpoint";
  check_endpoint endpoint api_key;
  ["Authorization: Bearer " ^ api_key; "User-Agent: Pave"]

let messages_headers ~endpoint ~api_key =
  if endpoint <> intl_messages_url && endpoint <> china_messages_url then
    invalid_arg "Kimi Code API key requires its pinned Anthropic-compatible endpoint";
  check_endpoint endpoint api_key;
  ["x-api-key", api_key; "anthropic-version", "2023-06-01";
   "User-Agent", "Pave"]
