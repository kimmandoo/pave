let fail label = failwith label
let invalid label f =
  try ignore (f ()); fail (label ^ ": accepted invalid selector")
  with Invalid_argument _ -> ()

let () =
  let open Pave.Interaction in
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
   | Fork (Some "/tmp/saved session.jsonl") -> ()
   | _ -> fail "fork path with spaces");
  (match parse "/clear", parse "/fresh", parse "/rename Fix login flow",
    parse "/label Need review", parse "/label", parse "/pin",
    parse "/approval yolo", parse "/approval default",
    parse "/thinking high", parse "/thinking default",
    parse "/tool disable write_file", parse "/attach Images/screen.png",
    parse "/attach clear", parse "/fork" with
   | Clear, Fresh, Rename "Fix login flow", Label (Some "Need review"),
     Label None, Pin, Approval (Some "yolo"), Approval (Some "default"),
     Thinking (Some "high"), Thinking (Some "default"),
     Tool_toggle { name = "write_file"; enabled = false },
     Attach (Some "Images/screen.png"), Attach None, Fork None -> ()
   | _ -> fail "journal lifecycle command parsing");
  (match parse "/new", parse "/entries", parse "/tree", parse "/tools",
    parse "/quit", parse "mobile task" with
   | New, Entries, Tree, Tools None, Quit, Prompt "mobile task" -> ()
   | _ -> fail "slash commands versus model prompt");
  (match parse "/tools read_file" with
   | Tools (Some "read_file") -> ()
   | _ -> fail "tool detail selection");
  (match parse "/queue inspect the active branch" with
   | Queue_prompt "inspect the active branch" -> ()
   | _ -> fail "queued follow-up parsing");
  if not (List.exists (fun item -> item.name = "/queue") (suggestions "/q")) then
    fail "queued follow-up is missing from command completion";
  invalid "missing queued prompt" (fun () -> parse "/queue");
  if suggestions "/model/foo" <> [] then fail "slash completion matched invalid prefix";
  invalid "multiple model arguments" (fun () -> parse "/model openai/gpt-5 extra");
  invalid "missing branch ID" (fun () -> parse "/branch");
  invalid "trailing command arguments" (fun () -> parse "/new accidental");
  invalid "multiple tool arguments" (fun () -> parse "/tools read_file write_file");
  invalid "tree takes no argument" (fun () -> parse "/tree missing");
  let descriptor, identity, route = resolve_model
    ~current_provider:"openai" ~input:"gpt-5" () in
  if descriptor.id <> "openai" || identity.upstream_id <> "gpt-5" ||
     route.name <> "responses" then
    fail "model-specific Responses route was not selected";
  let openai = Option.get (Pave.Provider_catalog.find "openai") in
  (match Pave.Provider_catalog.route openai "" with
   | Some route when route.wire = Pave.Provider.Openai_responses -> ()
   | _ -> fail "default OpenAI wire API should support discovered models");
  let descriptor, identity, route = resolve_model ~current_provider:"openai"
    ~input:"openrouter/openai/gpt-4o" () in
  if descriptor.id <> "openrouter" ||
     identity.upstream_id <> "openai/gpt-4o" || route.name <> "chat" then
    fail "provider-prefix selector lost namespaced model ID";
  let descriptor, _, route = resolve_model ~current_provider:"ollama"
    ~input:"anthropic/claude-sonnet-4-5" () in
  if descriptor.id <> "anthropic" || route.name <> "messages" then
    fail "explicit provider was not routed to native Messages";
  let descriptor, _, route = resolve_model ~current_provider:"openai"
    ~input:"github-copilot/gpt-4.1" () in
  if descriptor.id <> "github-copilot" ||
     route.wire <> Pave.Provider.Copilot_chat then
    fail "Copilot model selected an incompatible transport";
  let descriptor, _, route = resolve_model ~current_provider:"openai"
    ~input:"github-copilot/new-chat-model" () in
  if descriptor.id <> "github-copilot" ||
    route.wire <> Pave.Provider.Copilot_chat then
    fail "newly discovered Copilot model could not use pinned Chat route";
  let descriptor, identity, route = resolve_model ~current_provider:"openai"
    ~input:"commandcode@messages/future-studio-model" () in
  if descriptor.id <> "commandcode" ||
     identity.upstream_id <> "future-studio-model" ||
     route.name <> "messages" then
    fail "explicit native provider route could not be selected interactively";
  let _, identity, route = resolve_model ~current_provider:"commandcode"
    ~current_route:"messages" ~input:"future-studio-next" () in
  if identity.upstream_id <> "future-studio-next" ||
     route.name <> "messages" then
    fail "choosing another model reset the active Messages API route";
  let _, slash_identity, _ = resolve_model ~current_provider:"openai"
    ~input:"org/model/with/slashes" () in
  if slash_identity.provider <> "openai" ||
     slash_identity.upstream_id <> "org/model/with/slashes" then
    fail "slash-bearing exact model ID was parsed as a provider";
  let scoped = Pave.Model_identity.make ~provider:"github-copilot"
    ~account_id:"org/alice#primary" ~route:"chat"
    ~upstream_id:"models/chat/one" () in
  let _, parsed_scoped, _ = resolve_model ~current_provider:"openai"
    ~input:(Pave.Model_identity.selector scoped) () in
  if not (Pave.Model_identity.equal scoped parsed_scoped) then
    fail "canonical provider/account/route/model selector did not roundtrip";
  if Pave.Model_identity.selector scoped <>
      "github-copilot@chat#org%2Falice%23primary/models/chat/one" then
    fail "canonical account selector did not escape separator characters";
  let same_model_other_account = Pave.Model_identity.make
    ~provider:"github-copilot" ~account_id:"org/bob#primary" ~route:"chat"
    ~upstream_id:"models/chat/one" () in
  if Pave.Model_identity.equal scoped same_model_other_account ||
     Pave.Model_identity.selector scoped =
       Pave.Model_identity.selector same_model_other_account then
    fail "identical upstream IDs from different accounts were conflated";
  let _, parsed_other_account, _ = resolve_model ~current_provider:"openai"
    ~input:(Pave.Model_identity.selector same_model_other_account) () in
  if not (Pave.Model_identity.equal parsed_other_account
      same_model_other_account) then
    fail "second account identity did not roundtrip";
  let _, inherited_account, _ = resolve_model
    ~current_provider:"github-copilot" ~current_route:"chat"
    ~current_account_id:"active-user" ~input:"models/chat/two" () in
  if inherited_account.account_id <> Some "active-user" ||
     inherited_account.upstream_id <> "models/chat/two" then
    fail "account scope was not retained for an exact slash-bearing ID";
  invalid "missing explicit route" (fun () ->
    resolve_model ~current_provider:"openai" ~input:"commandcode/future-model" ());
  invalid "unknown native route" (fun () ->
    resolve_model ~current_provider:"openai"
      ~input:"commandcode@unknown/future-model" ());
  invalid "empty route" (fun () ->
    resolve_model ~current_provider:"openai" ~input:"commandcode@/future-model" ());
  invalid "unknown provider" (fun () ->
    resolve_model ~current_provider:"openai" ~input:"missing@route/foo" ());
  invalid "malformed account escape" (fun () ->
    resolve_model ~current_provider:"openai"
      ~input:"github-copilot@chat#%XZ/model/id" ());
  invalid "empty model" (fun () -> resolve_model
    ~current_provider:"openai" ~input:"openrouter/" ());
  invalid "control in model ID" (fun () -> resolve_model
    ~current_provider:"openai" ~input:"gpt-5\nother" ());
  let native = `Assoc [
    "provider", `String "openai-codex";
    "model", `String "codex-model-a";
    "output", `List [] ] in
  let assistant : Pave.Protocol.message = { role = "assistant"; content = Some "visible answer"; tool_calls = [];
  tool_call_id = None; tool_result_content = None; provider_state = Some native; attachments = [] } in
  let history = [Pave.Protocol.user "original prompt"; assistant] in
  let history_for ~provider ~route ~wire ~model messages =
    history_for_model ~provider ~route ~wire ~model messages in
  let same = history_for ~provider:"openai-codex" ~route:"responses"
    ~wire:Pave.Provider.Codex_responses ~model:"codex-model-a" history in
  let other_model = history_for ~provider:"openai-codex" ~route:"responses"
    ~wire:Pave.Provider.Codex_responses ~model:"codex-model-b" history in
  let other_protocol = history_for ~provider:"openai-codex" ~route:"responses"
    ~wire:Pave.Provider.Openai_completions ~model:"codex-model-a" history in
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
  if state (history_for ~provider:"google" ~route:"generate"
       ~wire:Pave.Provider.Gemini_direct ~model:"gemini-3-pro" google_history)
       <> Some signed ||
     state (history_for ~provider:"google" ~route:"generate"
       ~wire:Pave.Provider.Gemini_direct ~model:"gemini-3-flash" google_history)
       <> None ||
     state (history_for ~provider:"openai-codex" ~route:"responses"
       ~wire:Pave.Provider.Codex_responses ~model:"gemini-3-pro" google_history)
       <> None then
    fail "signed Gemini state crossed the model or protocol boundary";
  let check_native ~provider ~wire ~other_wire ?route () =
    let fields = ["provider", `String provider; "model", `String "same-model"] in
    let fields = match route with
      | None -> fields
      | Some name -> ("route", `String name) :: fields in
    let native = `Assoc fields in
    let messages = [Pave.Protocol.user "prompt";
      { assistant with provider_state = Some native }] in
    if state (history_for ~provider ~route:(Option.value ~default:"responses" route)
         ~wire ~model:"same-model" messages) <> Some native ||
       state (history_for ~provider ~route:(Option.value ~default:"responses" route)
         ~wire ~model:"different-model" messages) <> None ||
       state (history_for ~provider ~route:(Option.value ~default:"responses" route)
         ~wire:other_wire ~model:"same-model" messages) <> None
    then fail ("native signed state lost or crossed route: " ^ provider) in
  check_native ~provider:"meta" ~wire:Pave.Provider.Meta_responses
    ~other_wire:Pave.Provider.Openai_responses ();
  check_native ~provider:"opencode-zen"
    ~wire:Pave.Provider.Opencode_zen_responses
    ~other_wire:Pave.Provider.Meta_responses ();
  check_native ~provider:"devin" ~wire:Pave.Provider.Devin_connect
    ~other_wire:Pave.Provider.Meta_responses ();
  check_native ~provider:"commandcode" ~route:"messages"
    ~wire:Pave.Provider.Commandcode_messages
    ~other_wire:Pave.Provider.Commandcode_responses ();
  check_native ~provider:"gitlab-duo" ~route:"anthropic"
    ~wire:Pave.Provider.Gitlab_duo_messages
    ~other_wire:Pave.Provider.Gitlab_duo_responses ();
  check_native ~provider:"anthropic" ~route:"messages"
    ~wire:Pave.Provider.Anthropic_messages
    ~other_wire:Pave.Provider.Gemini_direct ();
  let anthropic_state = `Assoc [
    "provider", `String "anthropic"; "route", `String "messages";
    "model", `String "same-model";
    "content", `String "signed summary"; "signature", `String "opaque"] in
  let signed_summary = { (Pave.Protocol.user "signed summary") with
    provider_state = Some anthropic_state } in
  let signed_summary_state = function
    | [(message : Pave.Protocol.message)] ->
        if message.content <> Some "signed summary" then
          fail "signed summary text was lost";
        message.provider_state
    | _ -> fail "signed summary changed history length" in
  if signed_summary_state (history_for ~provider:"anthropic" ~route:"messages"
       ~wire:Pave.Provider.Anthropic_messages ~model:"same-model" [signed_summary])
       <> Some anthropic_state ||
     signed_summary_state (history_for ~provider:"anthropic" ~route:"messages"
       ~wire:Pave.Provider.Anthropic_messages ~model:"different-model" [signed_summary])
       <> None ||
     signed_summary_state (history_for ~provider:"anthropic" ~route:"chat"
       ~wire:Pave.Provider.Anthropic_messages ~model:"same-model" [signed_summary])
       <> None then
    fail "Anthropic compaction state crossed provider, API or model boundaries";
  let openai_state = `Assoc [
    "provider", `String "openai"; "route", `String "responses";
    "model", `String "same-model";
    "items", `List [`Assoc [
      "type", `String "compaction"; "encrypted_content", `String "opaque"]]] in
  let compacted = [{ (Pave.Protocol.user "native summary") with
    provider_state = Some openai_state }] in
  let state_on_summary = function
    | [(message : Pave.Protocol.message)] -> message.provider_state
    | _ -> fail "native compaction summary changed history length" in
  if state_on_summary (history_for ~provider:"openai" ~route:"responses"
       ~wire:Pave.Provider.Openai_responses ~model:"same-model" compacted)
       <> Some openai_state ||
     state_on_summary (history_for ~provider:"openai" ~route:"chat"
       ~wire:Pave.Provider.Openai_responses ~model:"same-model" compacted)
       <> None ||
     state_on_summary (history_for ~provider:"sakana" ~route:"responses"
       ~wire:Pave.Provider.Openai_responses ~model:"same-model" compacted)
       <> None then
    fail "OpenAI compaction state crossed provider, API or model boundaries";
  print_endline "interactive model routing: ok"
