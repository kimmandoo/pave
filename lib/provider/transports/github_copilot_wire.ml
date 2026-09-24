(* Public GitHub Copilot Chat only. Never send a GitHub OAuth bearer token to a
   configured host, an Enterprise endpoint, or an unsupported model route. *)
let endpoint = "https://api.githubcopilot.com/chat/completions"
let supported_models = ["gpt-4.1"; "gpt-4o"]
let supported_model model = List.mem model supported_models

let fail message = invalid_arg ("GitHub Copilot Chat: " ^ message)

let validate ~endpoint:target ~model ~token =
  if target <> endpoint then fail "unsupported endpoint";
  if not (supported_model model) then fail "unsupported model";
  if token = "" || String.length token > 8192 ||
     String.exists (fun c -> Char.code c <= 32 || Char.code c = 127) token
  then fail "invalid OAuth bearer token"

let initiator messages =
  let rec last_role current = function
    | [] -> current
    | (message : Protocol.message) :: rest ->
        last_role (if message.role = "user" then "user" else "agent") rest in
  last_role "user" messages

let headers ~endpoint:target ~model ~token ~messages =
  validate ~endpoint:target ~model ~token;
  let initiator = initiator messages in
  [ "Authorization: Bearer " ^ token;
    "User-Agent: copilot/1.0.82";
    "Editor-Version: copilot/1.0.82";
    "Copilot-Integration-Id: copilot-chat";
    "Copilot-Harness-Id: copilot-sdk";
    "Openai-Intent: conversation-agent";
    "X-GitHub-Api-Version: 2026-08-01";
    "X-Initiator: " ^ initiator;
    "X-Interaction-Type: conversation-" ^ initiator ]
