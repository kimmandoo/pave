let fail label = failwith label
let invalid label f =
  try ignore (f ()); fail (label ^ ": accepted invalid selector")
  with Invalid_argument _ -> ()
let invalid_with_suffix label suffix f =
  match f () with
  | _ -> fail (label ^ ": accepted malformed input")
  | exception Invalid_argument message when
      String.ends_with ~suffix message -> ()
  | exception Invalid_argument message ->
      fail (label ^ ": missing shared usage: " ^ message)


let () =
  let open Pave.Interaction in
  let has_help help name =
    List.exists (fun line ->
      let line = String.trim line in
      line = name || String.starts_with ~prefix:(name ^ " ") line)
      help in
  let check_capabilities ~session ~interactive =
    let help = help ~session ~interactive () in
    List.iter (fun item ->
      let enabled = available ~session ~interactive item in
      if has_help help item.name <> enabled then
        fail ("help capability mismatch for " ^ item.name);
      if List.mem item (suggestions ~session ~interactive item.name) <> enabled
      then fail ("completion capability mismatch for " ^ item.name))
      commands in
  check_capabilities ~session:false ~interactive:true;
  check_capabilities ~session:true ~interactive:true;
  check_capabilities ~session:true ~interactive:false;
  List.iter (fun line ->
    (match parse line with Unknown _ -> () | _ -> fail "disabled child workflow was accepted");
    (match parse ~subagents:true ~session:false line with
     | Unknown _ -> () | _ -> fail "child workflow accepted without a saved session"))
    ["/delegate review inspect files"; "/plan inspect files"; "/advisor inspect files";
     "/watchdog inspect files"; "/loop inspect files"; "/autoresearch inspect files"];
  if has_help (help ()) "/delegate" || suggestions "/del" <> [] then
    fail "disabled subagents remained visible";
  if not (has_help (help ~subagents:true ()) "/delegate") ||
     not (has_help (help ()) "/jobs") || not (has_help (help ()) "/mobile") then
    fail "opt-in delegation or session-owned dashboards disappeared";
  let tool_commands = tool_commands
    (Pave.Tools.available_for ~allow_shell:true ~enabled:(fun name ->
      List.mem name ["mobile_project"; "mobile_session"; "publish_web"])) in
  (match parse ~external_commands:tool_commands
      {|/publish_web {"action":"list"}|},
      parse ~external_commands:tool_commands "/mobile_project" with
   | Tool_call { name = "publish_web"; args = Some (`Assoc ["action", `String "list"]) },
     Tool_call { name = "mobile_project"; args = None } -> ()
   | _ -> fail "direct tool slash parsing lost exact JSON or schema inspection");
  invalid "direct tool malformed JSON" (fun () ->
    parse ~external_commands:tool_commands "/publish_web [1]");
  invalid "direct tool non-JSON" (fun () ->
    parse ~external_commands:tool_commands "/mobile_session list");
  (match parse "/publish_web" with
   | Unknown _ -> () | _ -> fail "unregistered tool shortcut remained callable");
  let external_commands = [
    command "/skill:review" No_arguments "Review [user skill]" (A_skill "review");
    command "/summarize" No_arguments "Summarize [project command]"
      (A_prompt_command "summarize") ] in
  (match parse ~external_commands "/skill:review",
         parse ~external_commands "/summarize",
         parse "/skill:review" with
   | Skill "review", Prompt_command "summarize", Unknown _ -> ()
   | _ -> fail "dynamic command parsing crossed its snapshot");
  invalid "dynamic command arguments"
    (fun () -> parse ~external_commands "/skill:review extra");
  if not (has_help (help ~external_commands ()) "/skill:review") ||
     suggestions ~external_commands "/sk" = [] ||
     has_help (help ()) "/summarize" then
    fail "external command help and suggestions diverged";
  let mcp_command = command ~interactive_only:true "/mcp:local" No_arguments
    "Connect local server" (A_mcp_connect "local") in
  (match parse ~external_commands:[mcp_command] "/mcp:local",
         parse ~external_commands:[mcp_command]
           ~interactive:false "/mcp:local",
         parse "/mcp:local" with
   | Mcp (Some "connect local"), Unknown _, Unknown _ -> ()
   | _ -> fail "MCP disabled-server and noninteractive suggestions diverged");
  (match parse "/login", parse ~interactive:false "/login",
      parse ~interactive:false "/setup", parse ~interactive:false "/hotkeys",
      parse ~session:false "/entries", parse "/exit" with
   | Login, Unknown _, Unknown _, Unknown _, Unknown _, Unknown _ -> ()
   | _ -> fail "interactive/session capabilities or removed alias mismatch");
  let tool_item = List.find (fun item -> item.name = "/tool") commands in
  let tool_usage = tool_item.name ^ " " ^ usage tool_item in
  invalid_with_suffix "tool usage" (" (usage: " ^ tool_usage ^ ")")
    (fun () -> parse "/tool enable");
  invalid_with_suffix "tool detail usage" " (usage: /tools [NAME])"
    (fun () -> parse "/tools read file");
  invalid_with_suffix "thinking usage" " (usage: /thinking [LEVEL|default])"
    (fun () -> parse "/thinking high extra");
  invalid_with_suffix "approval usage"
    " (usage: /approval [always-ask|write|yolo|default])"
    (fun () -> parse "/approval yolo extra");
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
   | Fork { path = Some "/tmp/saved session.jsonl"; _ } -> ()
   | _ -> fail "fork path with spaces");
  List.iter (fun line ->
    match parse line with
    | Fork { path = Some "/tmp/saved  session.jsonl"; until } ->
        if until <> (if String.ends_with ~suffix:"until=7" line then Some 7 else None)
        then fail "fork boundary changed"
    | _ -> fail "fork collapsed significant path whitespace")
    ["/fork /tmp/saved  session.jsonl";
     "/fork /tmp/saved  session.jsonl until=7"];
  (match parse "/fork until=7 /tmp/saved  session.jsonl" with
   | Fork { path = Some "/tmp/saved  session.jsonl"; until = Some 7 } -> ()
   | _ -> fail "fork prefix boundary lost path bytes");
  invalid "duplicate fork boundary"
    (fun () -> parse "/fork until=7 until=9");
  invalid "fork path control text"
    (fun () -> parse "/fork /tmp/saved\tsession.jsonl");
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
     Attach (Some "Images/screen.png"), Attach None, Fork { path = None; until = None } -> ()
   | _ -> fail "journal lifecycle command parsing");
  (match parse "/new", parse "/entries", parse "/tree", parse "/tools",
    parse "/quit", parse "mobile task" with
   | New, Entries, Tree, Tools None, Quit, Prompt "mobile task" -> ()
   | _ -> fail "slash commands versus model prompt");
  (match parse "/tools read_file" with
   | Tools (Some "read_file") -> ()
   | _ -> fail "tool detail selection");
  (match parse "/jobs", parse "/mobile",
      parse "/wait 0123456789abcdef0123456789abcdef",
      parse "/cancel-job 0123456789abcdef0123456789abcdef",
      parse "/artifact 0123456789abcdef0123456789abcdef",
      parse ~subagents:true "/delegate reviewer inspect the selected source files",
      parse ~subagents:true "/plan simplify the session lifecycle",
      parse "/goal simplify the session lifecycle", parse "/goal",
      parse ~subagents:true "/advisor check the proposed plan",
      parse ~subagents:true "/watchdog inspect scope drift",
      parse ~subagents:true "/loop review the goal",
      parse ~subagents:true "/autoresearch locate existing patterns",
      parse "/rule stop before irreversible changes", parse "/rule" with
   | Jobs, Mobile, Wait "0123456789abcdef0123456789abcdef",
     Cancel_job "0123456789abcdef0123456789abcdef",
     Artifact (Some "0123456789abcdef0123456789abcdef"),
     Delegate { label = "reviewer"; task = "inspect the selected source files" },
     Plan (Some "simplify the session lifecycle"),
     Goal (Some "simplify the session lifecycle"), Goal None,
     Advisor (Some "check the proposed plan"),
     Watchdog (Some "inspect scope drift"), Loop (Some "review the goal"),
     Autoresearch (Some "locate existing patterns"),
     Rule (Some "stop before irreversible changes"), Rule None -> ()
   | _ -> fail "mobile dashboard and session workflow command parsing");
(match parse "/rewind",
    parse "/rewind 0123456789abcdef0123456789abcdef" with
 | Rewind None, Rewind (Some "0123456789abcdef0123456789abcdef") -> ()
 | _ -> fail "workspace rewind command parsing");
if not (List.exists (fun item -> item.name = "/rewind")
    (suggestions "/rew")) then
  fail "workspace rewind is missing from completion";
  invalid "missing wait ID" (fun () -> parse "/wait");
  invalid "missing delegation task" (fun () -> parse ~subagents:true "/delegate reviewer");
  invalid "multiple rewind IDs" (fun () -> parse
    "/rewind 0123456789abcdef0123456789abcdef extra");
  if not (List.exists (fun item -> item.name = "/delegate")
      (suggestions ~subagents:true "/del")) then fail "child delegation is missing from completion";
  (match parse "/queue", parse "/queue   ",
         parse "/queue inspect  @src/main.ml after this",
         parse ~interactive:false "/queue follow up",
         parse "/queueing something" with
   | Queue_view, Queue_view, Queue_prompt "inspect  @src/main.ml after this",
     Queue_prompt "follow up", Unknown _ -> ()
   | _ -> fail "queue management and exact queued prompt parsing diverged");
  (match parse "/steer inspect  @src/main.ml instead",
         parse ~interactive:false "/steer stop searching",
         parse "/steering inspect files" with
   | Steer_prompt "inspect  @src/main.ml instead",
     Steer_prompt "stop searching", Unknown _ -> ()
   | _ -> fail "explicit steering must remain distinct from model text and unknown commands");
  invalid "missing steering prompt" (fun () -> parse "/steer");
  invalid "blank steering prompt" (fun () -> parse "/steer   ");
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
  let route_choices = model_route_browse_choices () in
  if not (List.mem ("commandcode@chat/",
      "commandcode@chat · browse API models") route_choices &&
      List.mem ("gitlab-duo@messages/",
        "gitlab-duo@messages · browse API models") route_choices &&
      List.mem ("amazon-bedrock@converse-stream/",
        "amazon-bedrock@converse-stream · browse API models") route_choices) then
    fail "multi-route provider model browsing choices are missing";
  (match model_route_browse_selection "commandcode@chat/" with
   | Some (descriptor, route)
     when descriptor.id = "commandcode" && route.name = "chat" -> ()
   | _ -> fail "registered route browse action was not resolved");
  (match model_route_browse_selection "amazon-bedrock@converse-stream/" with
   | Some (descriptor, route)
     when descriptor.id = "amazon-bedrock" && route.name = "converse-stream" -> ()
   | _ -> fail "Bedrock ConverseStream browse action was not resolved");
  if model_route_browse_selection "commandcode@unknown/" <> None ||
     model_route_browse_selection "ollama@chat/" <> None ||
     model_route_browse_selection "gitlab-duo@agent/" <> None then
    fail "unregistered or single-route model browse action was accepted";

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
  let direct_call : Pave.Protocol.tool_call = {
    id = "local-operation"; name = "read_file"; arguments = `Assoc [] } in
  let direct_history = [Pave.Protocol.user "/read_file";
    Pave.Protocol.direct_tool_message direct_call;
    Pave.Protocol.tool_result direct_call.id "local output"] in
  List.iter (fun (provider, route, wire) ->
    let selected = history_for ~provider ~route ~wire ~model:"another-model"
      direct_history in
    if selected <> direct_history then
      fail "model switch discarded local direct-tool provenance";
    ignore (Pave.Gemini_wire.request ~model:"gemini-model"
      (Pave.Protocol.replay_messages selected) []))
    ["google", "generate", Pave.Provider.Gemini_direct;
     "openai-codex", "responses", Pave.Provider.Codex_responses;
     "openai", "chat", Pave.Provider.Openai_completions];
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
  let custom = Pave.Custom_provider.parse
    (Yojson.Basic.from_string
      {|{"id":"team-gateway","display_name":"Team Gateway","default_route":"chat","routes":[{"name":"chat","api":"openai-chat","endpoint":"https://team.example.test/v1/chat/completions","account_id":"team-7","api_key_env":"TEAM_GATEWAY_KEY","models":[{"id":"model-a","tools":true}]}]}|}) in
  let registry = match Pave.Provider_catalog.create_registry [custom] with
    | Ok registry -> registry
    | Error message -> failwith message in
  let descriptor, identity, route = resolve_model ~registry
    ~current_provider:"team-gateway" ~input:"model-a" () in
  if descriptor.id <> "team-gateway" || route.name <> "chat" ||
     identity.upstream_id <> "model-a" ||
     identity.account_id <> Some "team-7" ||
     identity.config_revision <>
       Some (Pave.Custom_provider.fingerprint (List.hd custom.routes) ) then
    fail "custom provider identity lost its route/account revision";
  let _, roundtrip, _ = resolve_model ~registry ~current_provider:"openai"
    ~input:(Pave.Model_identity.selector identity) () in
  if not (Pave.Model_identity.equal identity roundtrip) then
    fail "custom provider canonical identity did not roundtrip";
  invalid "custom route account override" (fun () ->
    let other = Pave.Model_identity.make ~provider:"team-gateway"
      ~account_id:"other-team" ~config_revision:
        (Pave.Custom_provider.fingerprint (List.hd custom.routes))
      ~route:"chat" ~upstream_id:"model-a" () in
    resolve_model ~registry ~current_provider:"openai"
      ~input:(Pave.Model_identity.selector other) ());
  let changed = Pave.Custom_provider.parse
    (Yojson.Basic.from_string
      {|{"id":"team-gateway","display_name":"Team Gateway","default_route":"chat","routes":[{"name":"chat","api":"openai-chat","endpoint":"https://new.example.test/v1/chat/completions","account_id":"team-7","api_key_env":"TEAM_GATEWAY_KEY","models":[{"id":"model-a","tools":true}]}]}|}) in
  let changed_registry = match
      Pave.Provider_catalog.create_registry [changed] with
    | Ok registry -> registry
    | Error message -> failwith message in
  let _, changed_identity, _ = resolve_model ~registry:changed_registry
    ~current_provider:"team-gateway" ~input:"model-a" () in
  if Pave.Model_identity.equal identity changed_identity ||
     changed_identity.config_revision = identity.config_revision then
    fail "custom endpoint change did not change model identity";
  print_endline "interactive model routing: ok"
