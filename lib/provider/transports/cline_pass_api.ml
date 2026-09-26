let chat_url = "https://api.cline.bot/api/v1/chat/completions"

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Cline Pass key requires its pinned Chat Completions endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Cline Pass API key";
  ["Authorization: Bearer " ^ api_key]
