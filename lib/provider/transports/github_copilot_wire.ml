(* Public GitHub Copilot Chat only. Never send a GitHub OAuth bearer token to a
   configured host or an Enterprise endpoint. The pinned service decides which
   discovered Chat model IDs the authenticated account may invoke. *)
let endpoint = "https://api.githubcopilot.com/chat/completions"

let fail message = invalid_arg ("GitHub Copilot Chat: " ^ message)

let validate ~endpoint:target ~model ~token =
  if target <> endpoint then fail "unsupported endpoint";
  if model = "" || String.length model > 256 ||
    String.exists (fun c -> Char.code c <= 32 || Char.code c = 127) model
  then fail "invalid model ID";
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
