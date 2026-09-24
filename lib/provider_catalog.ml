type route = { name : string; wire : Provider.api; endpoint : string }

type descriptor = {
  id : string;
  display_name : string;
  routes : route list;
  default_route : string;
  model_routes : (string * string) list;
  api_key_env : string option;
  oauth : string option;
  default_model : string option;
}

(* Adding a compatible provider changes only this list. New wire protocols get
   their own adapter in Provider; an absent route must never be guessed. *)
let builtins = [
  { id = "openai"; display_name = "OpenAI";
    routes = [
      { name = "chat"; wire = Provider.Openai_completions;
        endpoint = "https://api.openai.com/v1/chat/completions" };
      { name = "responses"; wire = Provider.Openai_responses;
        endpoint = "https://api.openai.com/v1/responses" } ];
    default_route = "chat";
    model_routes = [ "gpt-5", "responses"; "o1", "responses";
      "o3", "responses"; "o4", "responses"; "daybreak-", "responses" ];
    api_key_env = Some "OPENAI_API_KEY";
    oauth = None; default_model = Some "gpt-4.1-mini" };
  { id = "openai-codex"; display_name = "OpenAI Codex subscription";
    routes = [ { name = "responses"; wire = Provider.Codex_responses;
      endpoint = "https://chatgpt.com/backend-api/codex/responses" } ];
    default_route = "responses"; model_routes = [];
    api_key_env = None; oauth = Some "openai-codex"; default_model = None };
  { id = "anthropic"; display_name = "Anthropic";
    routes = [ { name = "messages"; wire = Provider.Anthropic_messages;
      endpoint = "https://api.anthropic.com/v1/messages" } ];
    default_route = "messages"; model_routes = [];
    api_key_env = Some "ANTHROPIC_API_KEY";
    oauth = Some "anthropic"; default_model = None };
  { id = "ollama"; display_name = "Ollama (local)";
    routes = [ { name = "chat"; wire = Provider.Ollama_chat;
      endpoint = "http://127.0.0.1:11434/api/chat" } ];
    default_route = "chat"; model_routes = [];
    api_key_env = None; oauth = None; default_model = None };
  { id = "google"; display_name = "Google Gemini API";
    routes = [ { name = "generate"; wire = Provider.Gemini_direct;
      endpoint = "https://generativelanguage.googleapis.com/v1beta/models" } ];
    default_route = "generate"; model_routes = [];
    api_key_env = Some "GEMINI_API_KEY"; oauth = None; default_model = None };
  { id = "deepseek"; display_name = "DeepSeek";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://api.deepseek.com/chat/completions" } ];
    default_route = "chat"; model_routes = [];
    api_key_env = Some "DEEPSEEK_API_KEY"; oauth = None; default_model = None };
  { id = "groq"; display_name = "Groq";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://api.groq.com/openai/v1/chat/completions" } ];
    default_route = "chat"; model_routes = [];
    api_key_env = Some "GROQ_API_KEY"; oauth = None; default_model = None };
  { id = "mistral"; display_name = "Mistral";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://api.mistral.ai/v1/chat/completions" } ];
    default_route = "chat"; model_routes = [];
    api_key_env = Some "MISTRAL_API_KEY"; oauth = None; default_model = None };
  { id = "openrouter"; display_name = "OpenRouter";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://openrouter.ai/api/v1/chat/completions" } ];
    default_route = "chat"; model_routes = [];
    api_key_env = Some "OPENROUTER_API_KEY"; oauth = Some "openrouter";
    default_model = None };
]

let all () = builtins
let find id = List.find_opt (fun provider -> provider.id = id) builtins
let route provider ~model name =
  let name = if name <> "" then name else
    match List.find_opt (fun (prefix, _) ->
      String.starts_with ~prefix model) provider.model_routes with
    | Some (_, route) -> route
    | None -> provider.default_route in
  List.find_opt (fun entry -> entry.name = name) provider.routes
