let fail label = failwith label
let invalid label f =
  try ignore (f ()); fail (label ^ ": accepted invalid selector")
  with Invalid_argument _ -> ()

let () =
  let open Pave.Interaction in
  (match parse "/login" with Login None -> () | _ -> fail "login selector missing");
  (match parse "/login openrouter" with
   | Login (Some "openrouter") -> () | _ -> fail "login provider parsing");
  (match parse "/model" with Model None -> () | _ -> fail "model selector missing");
  (match parse "/model openrouter/openai/gpt-4o" with
   | Model (Some "openrouter/openai/gpt-4o") -> ()
   | _ -> fail "namespaced model parsing");
  (match parse "/models" with Unknown _ -> () | _ -> fail "command prefix confused");
  (match parse "/model/foo" with Unknown _ -> () | _ -> fail "slash form confused");
  (match parse "/branch abc123" with
   | Branch "abc123" -> () | _ -> fail "journal branch ID parsing");
  (match parse "/resume /tmp/saved session.jsonl" with
   | Resume (Some "/tmp/saved session.jsonl") -> ()
   | _ -> fail "resume path with spaces");
  (match parse "/fork /tmp/saved session.jsonl" with
   | Fork "/tmp/saved session.jsonl" -> ()
   | _ -> fail "fork path with spaces");
  (match parse "/new", parse "/entries", parse "/tree", parse "/tools",
    parse "/quit", parse "mobile task" with
   | New, Entries, Tree, Tools None, Quit, Prompt "mobile task" -> ()
   | _ -> fail "slash commands versus model prompt");
  (match parse "/tools read_file" with
   | Tools (Some "read_file") -> ()
   | _ -> fail "tool detail selection");
  if suggestions "/model/foo" <> [] then fail "slash completion matched invalid prefix";
  invalid "multiple model arguments" (fun () -> parse "/model openai/gpt-5 extra");
  invalid "control in login provider" (fun () -> parse "/login openrouter\tother");
  invalid "missing branch ID" (fun () -> parse "/branch");
  invalid "missing fork path" (fun () -> parse "/fork");
  invalid "trailing command arguments" (fun () -> parse "/new accidental");
  invalid "multiple tool arguments" (fun () -> parse "/tools read_file write_file");
  invalid "tree takes no argument" (fun () -> parse "/tree missing");
  let descriptor, model, route = resolve_model ~current_provider:"openai" ~input:"gpt-5" in
  if descriptor.id <> "openai" || model <> "gpt-5" || route.name <> "responses" then
    fail "model-specific Responses route was not selected";
  let openai = Option.get (Pave.Provider_catalog.find "openai") in
  let default = Option.get openai.default_model in
  if default <> "gpt-6-sol" then fail "OpenAI startup did not select the current coding model";
  (match Pave.Provider_catalog.route openai ~model:default "" with
   | Some route when route.wire = Pave.Provider.Openai_responses -> ()
   | _ -> fail "default coding model did not use the Responses transport");
  let descriptor, model, route = resolve_model ~current_provider:"openai"
    ~input:"openrouter/openai/gpt-4o" in
  if descriptor.id <> "openrouter" || model <> "openai/gpt-4o" || route.name <> "chat" then
    fail "provider-prefix selector lost namespaced model ID";
  let descriptor, _, route = resolve_model ~current_provider:"ollama"
    ~input:"anthropic/claude-sonnet-4-5" in
  if descriptor.id <> "anthropic" || route.name <> "messages" then
    fail "explicit provider was not routed to native Messages";
  let descriptor, _, route = resolve_model ~current_provider:"openai"
    ~input:"github-copilot/gpt-4.1" in
  if descriptor.id <> "github-copilot" ||
     route.wire <> Pave.Provider.Copilot_chat then
    fail "Copilot model selected an incompatible transport";
  invalid "unsupported Copilot model" (fun () ->
    resolve_model ~current_provider:"openai" ~input:"github-copilot/gpt-5");
  invalid "unknown provider" (fun () -> resolve_model ~current_provider:"openai" ~input:"missing/foo");
  invalid "empty model" (fun () -> resolve_model ~current_provider:"openai" ~input:"openrouter/");
  invalid "control in model ID" (fun () -> resolve_model ~current_provider:"openai" ~input:"gpt-5\nother");
  let native = `Assoc [
    "provider", `String "openai-codex";
    "model", `String "codex-model-a";
    "output", `List [] ] in
  let assistant : Pave.Protocol.message = {
    role = "assistant"; content = Some "visible answer"; tool_calls = [];
    tool_call_id = None; provider_state = Some native } in
  let history = [ Pave.Protocol.user "original prompt"; assistant ] in
  let same = history_for_model ~wire:Pave.Provider.Codex_responses
    ~model:"codex-model-a" history in
  let other_model = history_for_model ~wire:Pave.Provider.Codex_responses
    ~model:"codex-model-b" history in
  let other_protocol = history_for_model ~wire:Pave.Provider.Openai_completions
    ~model:"codex-model-a" history in
  let state = function
    | [ _; (message : Pave.Protocol.message) ] ->
        if message.content <> Some "visible answer" then fail "visible history lost";
        message.provider_state
    | _ -> fail "conversation history changed length" in
  if state same <> Some native || state other_model <> None ||
     state other_protocol <> None || state history <> Some native then
    fail "opaque Codex state crossed the model or protocol boundary";
  let signed = `Assoc [
    "provider", `String "google"; "model", `String "gemini-3-pro";
    "parts", `List [] ] in
  let google_history = [ Pave.Protocol.user "original prompt";
    { assistant with provider_state = Some signed } ] in
  if state (history_for_model ~wire:Pave.Provider.Gemini_direct
       ~model:"gemini-3-pro" google_history) <> Some signed ||
     state (history_for_model ~wire:Pave.Provider.Gemini_direct
       ~model:"gemini-3-flash" google_history) <> None ||
     state (history_for_model ~wire:Pave.Provider.Codex_responses
       ~model:"gemini-3-pro" google_history) <> None then
    fail "signed Gemini state crossed the model or protocol boundary";
  print_endline "interactive login and model routing: ok"
