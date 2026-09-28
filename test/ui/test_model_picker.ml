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
  let openai = descriptor "openai" in
  let chat = scope ~account_id:"team-a" "openai" "chat" in
  let responses = scope ~account_id:"team-a" "openai" "responses" in
  let chat_listing = listing ~provider:"openai" ~route:"chat"
    ~account_id:"team-a" ~ids:["gpt-chat"] () in
  let response_listing = listing ~provider:"openai" ~route:"responses"
    ~account_id:"team-a" ~ids:["gpt-response"] () in
  assert (ids openai chat chat_listing = ["gpt-chat"]);
  assert (ids openai responses response_listing = ["gpt-response"]);
  assert (ids openai chat response_listing = []);
  assert (ids openai (scope ~account_id:"team-b" "openai" "chat")
    chat_listing = []);
  assert (ids openai (scope "openai" "chat") chat_listing = []);
  let other_account = listing ~provider:"openai" ~route:"chat"
    ~account_id:"team-b" ~ids:["other-team-only"] () in
  assert (ids openai chat other_account = []);
  assert (ids openai (scope ~account_id:"team-b" "openai" "chat")
    other_account = ["other-team-only"]);
  let shared_a = listing ~provider:"openai" ~route:"chat"
    ~account_id:"team-a" ~ids:["shared-id"] () in
  let shared_b = listing ~provider:"openai" ~route:"chat"
    ~account_id:"team-b" ~ids:["shared-id"] () in
  let model_a = List.hd (Model_picker.eligible_models openai chat shared_a) in
  let model_b = List.hd (Model_picker.eligible_models openai
    (scope ~account_id:"team-b" "openai" "chat") shared_b) in
  assert (model_a.identity.upstream_id = model_b.identity.upstream_id &&
    Model_picker.identity_selector model_a <>
      Model_picker.identity_selector model_b &&
    Model_picker.identity_label model_a <> Model_picker.identity_label model_b);
  let slash_listing = listing ~provider:"openai" ~route:"chat"
    ~account_id:"team-a" ~ids:["org/model"] () in
  let slash_model = List.hd
    (Model_picker.eligible_models openai chat slash_listing) in
  let _, parsed_slash, slash_route = Pave.Interaction.resolve_model
    ~current_provider:"openai" ~current_route:"responses"
    ~input:(Model_picker.identity_selector slash_model) () in
  assert (Identity.equal slash_model.identity parsed_slash &&
    slash_route.name = "chat" &&
    parsed_slash.upstream_id = "org/model");
  let off_route : Discovery.listing = { chat_listing with
    models = List.map (fun (model : Discovery.model) ->
      { model with capabilities = {
          model.capabilities with supported_endpoints = Some ["responses"] } })
      chat_listing.models } in
  assert (ids openai chat off_route = []);
  let conflicting_provenance : Discovery.listing = { chat_listing with
    models = List.map (fun (model : Discovery.model) ->
      { model with provenance = {
          model.provenance with id_source = Catalog.Explicit_user_input } })
      chat_listing.models } in
  assert (ids openai chat conflicting_provenance = []);
  assert (ids openai chat (listing ~provider:"openai" ~route:"chat"
    ~account_id:"team-a" ~ids:["stale"] ~fresh:false ()) = []);
  assert (ids openai chat (listing ~provider:"openai" ~route:"chat"
    ~account_id:"team-a" ~ids:["configured"]
    ~source:Catalog.Explicit_user_input ()) = []);
  assert (ids openai chat (listing ~provider:"openai" ~route:"chat"
    ~account_id:"team-a" ~ids:["OS-default"]
    ~source:Catalog.Runtime_default ()) = []);
  let wrong_provider = listing ~provider:"unknown" ~route:"chat"
    ~account_id:"team-a" ~ids:["fake"] () in
  assert (ids openai chat wrong_provider = []);
  assert (ids openai (scope "unknown" "chat") chat_listing = []);
  assert (ids openai (scope "openai" "not-registered") chat_listing = []);
  let unclassified = descriptor "minimax" in
  let minimax_route = unclassified.default_route in
  let unclassified_listing = listing ~provider:"minimax" ~route:minimax_route
    ~ids:["listed-but-unverified"] () in
  assert (ids unclassified (scope "minimax" minimax_route)
    unclassified_listing = []);
  let anonymous = descriptor "ollama" in
  let anonymous_listing = listing ~source:Catalog.Provider_listing
    ~provider:"ollama" ~route:"chat" ~ids:["locally-listed"] () in
  assert (ids anonymous (scope "ollama" "chat") anonymous_listing =
    ["locally-listed"]);
  let umans = descriptor "umans" in
  let umans_listing = listing ~source:Catalog.Provider_listing
    ~provider:"umans" ~route:"chat" ~ids:["public-but-needs-key"] () in
  let old_key = Sys.getenv_opt "UMANS_AI_CODING_PLAN_API_KEY" in
  Fun.protect ~finally:(fun () ->
    Unix.putenv "UMANS_AI_CODING_PLAN_API_KEY"
      (Option.value ~default:"" old_key)) (fun () ->
    Unix.putenv "UMANS_AI_CODING_PLAN_API_KEY" "";
    assert (ids umans (scope "umans" "chat") umans_listing = []));
  print_endline "model picker verified route/account candidates: ok"
