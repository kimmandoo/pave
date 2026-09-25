type route = { name : string; wire : Provider.api; endpoint : string }

type descriptor = {
  id : string;
  display_name : string;
  routes : route list;
  default_route : string;
  api_key_env : string option;
  oauth : string option;
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
    default_route = "responses";
    api_key_env = Some "OPENAI_API_KEY";
    oauth = None };
  { id = "openai-codex"; display_name = "OpenAI Codex subscription";
    routes = [ { name = "responses"; wire = Provider.Codex_responses;
      endpoint = "https://chatgpt.com/backend-api/codex/responses" } ];
    default_route = "responses";
    api_key_env = None; oauth = Some "openai-codex" };
  { id = "anthropic"; display_name = "Anthropic";
    routes = [ { name = "messages"; wire = Provider.Anthropic_messages;
      endpoint = "https://api.anthropic.com/v1/messages" } ];
    default_route = "messages";
    api_key_env = Some "ANTHROPIC_API_KEY";
    oauth = Some "anthropic" };
  { id = "ollama"; display_name = "Ollama (local)";
    routes = [ { name = "chat"; wire = Provider.Ollama_chat;
      endpoint = "http://127.0.0.1:11434/api/chat" } ];
    default_route = "chat";
    api_key_env = None; oauth = None };
  { id = "google"; display_name = "Google Gemini API";
    routes = [ { name = "generate"; wire = Provider.Gemini_direct;
      endpoint = "https://generativelanguage.googleapis.com/v1beta/models" } ];
    default_route = "generate";
    api_key_env = Some "GEMINI_API_KEY"; oauth = None };
  { id = "deepseek"; display_name = "DeepSeek";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://api.deepseek.com/chat/completions" } ];
    default_route = "chat";
    api_key_env = Some "DEEPSEEK_API_KEY"; oauth = None };
  { id = "groq"; display_name = "Groq";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://api.groq.com/openai/v1/chat/completions" } ];
    default_route = "chat";
    api_key_env = Some "GROQ_API_KEY"; oauth = None };
  { id = "mistral"; display_name = "Mistral";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://api.mistral.ai/v1/chat/completions" } ];
    default_route = "chat";
    api_key_env = Some "MISTRAL_API_KEY"; oauth = None };
  { id = "openrouter"; display_name = "OpenRouter";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://openrouter.ai/api/v1/chat/completions" } ];
    default_route = "chat";
    api_key_env = Some "OPENROUTER_API_KEY"; oauth = Some "openrouter" };
  { id = "together"; display_name = "Together AI";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://api.together.ai/v1/chat/completions" } ];
    default_route = "chat";
    api_key_env = Some "TOGETHER_API_KEY"; oauth = None };
  { id = "cerebras"; display_name = "Cerebras";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://api.cerebras.ai/v1/chat/completions" } ];
    default_route = "chat";
    api_key_env = Some "CEREBRAS_API_KEY"; oauth = None };
  { id = "venice"; display_name = "Venice";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://api.venice.ai/api/v1/chat/completions" } ];
    default_route = "chat";
    api_key_env = Some "VENICE_API_KEY"; oauth = None };
  { id = "deepinfra"; display_name = "DeepInfra";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://api.deepinfra.com/v1/openai/chat/completions" } ];
    default_route = "chat";
    api_key_env = Some "DEEPINFRA_API_KEY"; oauth = None };
  { id = "fireworks"; display_name = "Fireworks AI";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://api.fireworks.ai/inference/v1/chat/completions" } ];
    default_route = "chat";
    api_key_env = Some "FIREWORKS_API_KEY"; oauth = None };
  { id = "baseten"; display_name = "Baseten";
    routes = [ { name = "chat"; wire = Provider.Openai_completions;
      endpoint = "https://inference.baseten.co/v1/chat/completions" } ];
    default_route = "chat";
    api_key_env = Some "BASETEN_API_KEY"; oauth = None };
  { id = "lm-studio"; display_name = "LM Studio (local)";
    routes = [ { name = "chat"; wire = Provider.Local_chat;
      endpoint = "http://127.0.0.1:1234/v1/chat/completions" } ];
    default_route = "chat";
    api_key_env = Some "LM_STUDIO_API_KEY"; oauth = None };
  { id = "llama.cpp"; display_name = "llama.cpp (local)";
    routes = [ { name = "chat"; wire = Provider.Local_chat;
      endpoint = "http://127.0.0.1:8080/v1/chat/completions" } ];
    default_route = "chat";
    api_key_env = Some "LLAMA_CPP_API_KEY"; oauth = None };
  { id = "vllm"; display_name = "vLLM (local)";
    routes = [ { name = "chat"; wire = Provider.Local_chat;
      endpoint = "http://127.0.0.1:8000/v1/chat/completions" } ];
    default_route = "chat";
    api_key_env = Some "VLLM_API_KEY"; oauth = None };
  { id = "github-copilot"; display_name = "GitHub Copilot Chat (public github.com)";
    routes = [ { name = "chat"; wire = Provider.Copilot_chat;
      endpoint = Github_copilot_wire.endpoint } ];
    default_route = "chat";
    api_key_env = None; oauth = Some "github-copilot" };
]

let all () = builtins
let find id = List.find_opt (fun provider -> provider.id = id) builtins

let route provider name =
  let name = if name = "" then provider.default_route else name in
  match List.find_opt (fun entry -> entry.name = name) provider.routes with
  | Some entry when Local_compat.engine provider.id <> None ->
      Some { entry with endpoint = Local_compat.endpoint ~provider:provider.id () }
  | route -> route
