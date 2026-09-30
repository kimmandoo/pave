module Discovery = Pave.Model_discovery
module Catalog = Pave.Model_catalog
module Identity = Pave.Model_identity
module Coordinator = Pave.Model_discovery_coordinator

let descriptor provider =
  match Pave.Provider_catalog.find provider with
  | Some descriptor -> descriptor
  | None -> failwith ("missing registered provider " ^ provider)

let scope ?account_id provider route : Coordinator.scope =
  { provider; route; account_id }

let ids descriptor scope listing =
  List.map (fun (model : Discovery.model) -> model.identity.upstream_id)
    (Model_picker.eligible_models descriptor scope listing)

let listing ?(source = Catalog.Pinned_account_listing) ?(fresh = true)
    ?account_id ~provider ~route ~ids () : Discovery.listing =
  let provenance : Catalog.provenance = {
    id_source = source;
    capability_source = None;
    endpoint = Some "https://example.test/models";
    retrieved_at = if fresh then Some (Unix.gettimeofday ()) else None;
  } in
  let models = List.map (fun upstream_id ->
    ({ identity = Identity.make ~provider ~route ?account_id ~upstream_id ();
       display_name = None; capabilities = Catalog.empty_capabilities;
       provenance } : Discovery.model)) ids in
  { models; source = provenance }

let () =
  let routed = descriptor "abliteration" in
  let chat = scope ~account_id:"team-a" "abliteration" "chat" in
  let responses = scope ~account_id:"team-a" "abliteration" "responses" in
  let chat_listing = listing ~provider:"abliteration" ~route:"chat"
    ~account_id:"team-a" ~ids:["gpt-chat"] () in
  let response_listing = listing ~provider:"abliteration" ~route:"responses"
    ~account_id:"team-a" ~ids:["gpt-response"] () in
  assert (ids routed chat chat_listing = ["gpt-chat"]);
  assert (ids routed responses response_listing = ["gpt-response"]);
  assert (ids routed chat response_listing = []);
  assert (ids routed (scope ~account_id:"team-b" "abliteration" "chat")
    chat_listing = []);
  assert (ids routed (scope "abliteration" "chat") chat_listing = []);
  let other_account = listing ~provider:"abliteration" ~route:"chat"
    ~account_id:"team-b" ~ids:["other-team-only"] () in
  assert (ids routed chat other_account = []);
  assert (ids routed (scope ~account_id:"team-b" "abliteration" "chat")
    other_account = ["other-team-only"]);
  let shared_a = listing ~provider:"abliteration" ~route:"chat"
    ~account_id:"team-a" ~ids:["shared-id"] () in
  let shared_b = listing ~provider:"abliteration" ~route:"chat"
    ~account_id:"team-b" ~ids:["shared-id"] () in
  let model_a = List.hd (Model_picker.eligible_models routed chat shared_a) in
  let model_b = List.hd (Model_picker.eligible_models routed
    (scope ~account_id:"team-b" "abliteration" "chat") shared_b) in
  assert (model_a.identity.upstream_id = model_b.identity.upstream_id &&
    Model_picker.identity_selector model_a <>
      Model_picker.identity_selector model_b &&
    Model_picker.identity_label model_a <> Model_picker.identity_label model_b);
  let named_a = { model_a with Catalog.display_name = Some "GPT-4o" }
  and named_b = { model_b with Catalog.display_name = Some "GPT-4o" } in
  let exact_a = Model_picker.identity_selector named_a
  and exact_b = Model_picker.identity_selector named_b in
  assert (
    exact_a <> exact_b &&
    exact_a = Pave.Model_identity.selector named_a.identity &&
    exact_b = Pave.Model_identity.selector named_b.identity);
  let terminal_name = { named_a with
    Catalog.display_name = Some "GPT-4o\027[31m" } in
  assert (not (String.contains (Model_picker.identity_label terminal_name) '\027') &&
    (match Model_picker.model_detail routed terminal_name with
     | Some details -> not (String.contains details '\027')
     | None -> false));
  let rows = Model_picker.row_labels ~current_model:exact_a
    [named_a; { named_a with Catalog.identity = { named_a.identity with
      upstream_id = "gpt-4o-mini" } }] in
  assert (List.map snd rows =
    ["GPT-4o · shared-id  (current)"; "GPT-4o · gpt-4o-mini"]);
  assert (List.map snd (Model_picker.row_labels [model_a]) = ["shared-id"]);
  let slash_listing = listing ~provider:"abliteration" ~route:"chat"
    ~account_id:"team-a" ~ids:["org/model"] () in
  let slash_model = List.hd
    (Model_picker.eligible_models routed chat slash_listing) in
  let _, parsed_slash, slash_route = Pave.Interaction.resolve_model
    ~current_provider:"abliteration" ~current_route:"responses"
    ~input:(Model_picker.identity_selector slash_model) () in
  assert (Identity.equal slash_model.identity parsed_slash &&
    slash_route.name = "chat" &&
    parsed_slash.upstream_id = "org/model");
  let off_route : Discovery.listing = { chat_listing with
    models = List.map (fun (model : Discovery.model) ->
      { model with capabilities = {
          model.capabilities with supported_endpoints = Some ["responses"] } })
      chat_listing.models } in
  assert (ids routed chat off_route = []);
  let conflicting_provenance : Discovery.listing = { chat_listing with
    models = List.map (fun (model : Discovery.model) ->
      { model with provenance = {
          model.provenance with id_source = Catalog.Explicit_user_input } })
      chat_listing.models } in
  assert (ids routed chat conflicting_provenance = []);
  assert (ids routed chat (listing ~provider:"abliteration" ~route:"chat"
    ~account_id:"team-a" ~ids:["stale"] ~fresh:false ()) = []);
  assert (ids routed chat (listing ~provider:"abliteration" ~route:"chat"
    ~account_id:"team-a" ~ids:["configured"]
    ~source:Catalog.Explicit_user_input ()) = []);
  assert (ids routed chat (listing ~provider:"abliteration" ~route:"chat"
    ~account_id:"team-a" ~ids:["OS-default"]
    ~source:Catalog.Runtime_default ()) = []);
  let wrong_provider = listing ~provider:"unknown" ~route:"chat"
    ~account_id:"team-a" ~ids:["fake"] () in
  assert (ids routed chat wrong_provider = []);
  assert (ids routed (scope "unknown" "chat") chat_listing = []);
  assert (ids routed (scope "abliteration" "not-registered") chat_listing = []);
  let unclassified = descriptor "minimax" in
  let minimax_route = unclassified.default_route in
  let unclassified_listing = listing ~provider:"minimax" ~route:minimax_route
    ~ids:["listed-but-unverified"] () in
  assert (ids unclassified (scope "minimax" minimax_route)
    unclassified_listing = []);
  let openai = descriptor "openai" in
  List.iter (fun route ->
    let http ~url ~headers =
      assert (url = Discovery.openai_url &&
        headers = ["Authorization", "Bearer fixture-openai-key"]);
      Ok (200, {|{"data":[{"id":"future/exact-chat"},{"id":"future/exact-embedding"}]}|}) in
    let listing = match Discovery.discover ~http ~provider:"openai"
        ~route_name:route ~credential:(Discovery.Api_key "fixture-openai-key") () with
      | Ok listing -> listing
      | Error error -> failwith (Discovery.message error) in
    assert (Discovery.model_ids listing =
      ["future/exact-chat"; "future/exact-embedding"]);
    assert (ids openai (scope "openai" route) listing = []);
    let selector = Pave.Model_identity.selector (List.hd listing.models).identity in
    let _, identity, selected_route = Pave.Interaction.resolve_model
      ~current_provider:"openai" ~input:selector () in
    assert (identity.upstream_id = "future/exact-chat" &&
      selected_route.name = route)) ["chat"; "responses"];
  let anonymous = descriptor "ollama" in
  let anonymous_listing = listing ~source:Catalog.Provider_listing
    ~provider:"ollama" ~route:"chat" ~ids:["locally-listed"] () in
  assert (ids anonymous (scope "ollama" "chat") anonymous_listing =
    ["locally-listed"]);
  let devin = descriptor "devin" in
  let devin_scope = scope ~account_id:"account-a" "devin" "connect" in
  let devin_listing = listing ~provider:"devin" ~route:"connect"
    ~account_id:"account-a" ~ids:["router"; "text-only"] () in
  let devin_listing = { devin_listing with models =
    List.map (fun (model : Discovery.model) ->
      if model.identity.upstream_id = "text-only" then
        { model with capabilities = {
          model.capabilities with tools = Some false } }
      else model) devin_listing.models } in
  assert (ids devin devin_scope devin_listing = ["router"]);
  let umans = descriptor "umans" in
  let umans_listing = listing ~source:Catalog.Provider_listing
    ~provider:"umans" ~route:"chat" ~ids:["public-but-needs-key"] () in
  let old_key = Sys.getenv_opt "UMANS_AI_CODING_PLAN_API_KEY" in
  Fun.protect ~finally:(fun () ->
    Unix.putenv "UMANS_AI_CODING_PLAN_API_KEY"
      (Option.value ~default:"" old_key)) (fun () ->
    Unix.putenv "UMANS_AI_CODING_PLAN_API_KEY" "";
    assert (ids umans (scope "umans" "chat") umans_listing = []));
  let old_local_base = Sys.getenv_opt "LM_STUDIO_BASE_URL" in
  Fun.protect ~finally:(fun () ->
    Unix.putenv "LM_STUDIO_BASE_URL"
      (Option.value ~default:"" old_local_base)) (fun () ->
    Unix.putenv "LM_STUDIO_BASE_URL" "http://127.0.0.1:45437/v1";
    let local = descriptor "lm-studio" in
    let local_scope = scope "lm-studio" "chat" in
    let local_listing = listing ~source:Catalog.Provider_listing
      ~provider:"lm-studio" ~route:"chat" ~ids:["configured-local-model"] () in
    assert (ids local local_scope local_listing = ["configured-local-model"]);
    let model = List.hd local_listing.models in
    assert (not (Discovery.model_supports_endpoint ~provider:"lm-studio" model
      ~endpoint:"http://127.0.0.1:45438/v1/chat/completions")));
  let calls = ref 0 in
  let http ~url ~headers =
    assert (url = Discovery.ollama_url && headers = []);
    incr calls;
    Ok (200, Printf.sprintf {|{"models":[{"name":"fresh-model-%d"}]}|} !calls) in
  let request = Model_picker.discovery_request ~http anonymous
    (scope "ollama" "chat") in
  let _hidden_request = Model_picker.discovery_request ~http openai
    (scope "openai" "responses") in
  let scopes = Model_picker.available_scopes () in
  List.iter (fun (entry : Pave.Provider_catalog.descriptor) ->
    List.iter (fun (route : Pave.Provider_catalog.route) ->
      assert (List.exists (fun ((candidate : Coordinator.scope), _) ->
        candidate.provider = entry.id && candidate.route = route.name) scopes))
      entry.routes) (Pave.Provider_catalog.all ());
  assert (!calls = 0);
  let run () = match request.run (fun () -> false) with
    | Ok listing, None -> ids anonymous (scope "ollama" "chat") listing
    | _ -> failwith "scoped anonymous listing failed" in
  assert (run () = ["fresh-model-1"] && !calls = 1);
  assert (run () = ["fresh-model-2"] && !calls = 2);
  let codex = descriptor "openai-codex" in
  let codex_route = Option.get (Pave.Provider_catalog.route codex "responses") in
  let effort_model = List.hd (listing ~provider:"openai-codex"
    ~route:"responses" ~account_id:"team-a" ~ids:["reported-model"] ()).models in
  let reported = { effort_model with Catalog.capabilities = {
    effort_model.capabilities with effort_levels = Some ["low"; "future-level"; "high"] } } in
  let offered = Model_picker.effort_options codex_route reported in
  assert (offered = ["low"; "high"]);
  assert (Model_picker.initial_effort ~current_thinking:(Some "high") offered = "high");
  assert (Model_picker.initial_effort ~current_thinking:(Some "future-level") offered =
    "Provider default");
  assert (Model_picker.effort_options codex_route effort_model = []);
  assert (Model_picker.initial_effort ~current_thinking:(Some "high") [] =
    "Provider default");
  let unsupported_route = Option.get
    (Pave.Provider_catalog.route anonymous "chat") in
  assert (Model_picker.effort_options unsupported_route reported = []);
  print_endline "model picker verified route/account candidates: ok"
