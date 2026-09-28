(* Discovery runs on cancellable workers. Only fresh, route-compatible IDs
   from a registered provider's successful listing become model choices. *)
let saved_account_label (account : Pave.Oauth_store.account) =
  match account.credential.account_id with
  | Some id -> "Account ID: " ^ Printf.sprintf "%S" id
  | None -> "Local sign-in ID: " ^ Printf.sprintf "%S" account.selection_id
let credential ?registry ?route_name ?account_id
    (descriptor : Pave.Provider_catalog.descriptor) =
  let registry = Option.value ~default:Pave.Provider_catalog.builtin_registry registry in
  let route_name = Option.value ~default:descriptor.default_route route_name in
  match Pave.Provider_catalog.custom_route registry ~provider:descriptor.id
      ~route:route_name with
  | Some custom_route ->
      (match custom_route.auth with
       | Pave.Custom_provider.No_auth -> None
       | Pave.Custom_provider.Api_key_env name ->
           Option.bind (Sys.getenv_opt name) (fun key ->
             if key = "" then None
             else Some (Pave.Model_discovery.Api_key key)))
  | None when descriptor.id = "azure" ->
      Option.map (fun key -> Pave.Model_discovery.Api_key key)
        (Cli_auth.api_key descriptor)
  | None ->
      let route = Pave.Provider_catalog.route descriptor route_name in
      let with_route f = Option.bind route f in
      let stored_accounts = lazy (Pave.Oauth_store.accounts
        ~path:(Pave.Oauth_store.default_path ()) ~provider:descriptor.id) in
      let select_account () = match account_id, Lazy.force stored_accounts with
        | Some id, _ ->
            Pave.Oauth_store.account
              ~path:(Pave.Oauth_store.default_path ()) ~provider:descriptor.id
              ~account_id:(Some id)
        | None, [ account ] -> Some account
        | None, [] -> None
        | None, _ ->
            failwith ("multiple saved accounts for " ^ descriptor.id ^
              "; pass --account or select an account-scoped model") in
      let oauth service =
        match Cli_auth.api_key descriptor with
        | Some key -> Some (Pave.Model_discovery.Api_key key)
        | None ->
            (match select_account () with
             | None -> None
             | Some account ->
                 with_route (fun route ->
                   let authentication, _, resolve = Cli_auth.resolve_authentication
                     ~account_id:account.Pave.Oauth_store.selection_id
                     ~descriptor ~route ~endpoint:route.endpoint () in
                   match authentication, resolve with
                   | Pave.Provider.OAuth, Some resolve ->
                       let (credential : Pave.Provider.credentials) = resolve () in
                       Some (Pave.Model_discovery.OAuth {
                         service; access = credential.access;
                         account_id = credential.account_id;
                         selection_id = Some account.selection_id })
                   | _ -> None)) in
      let stored_api_key () =
        match Cli_auth.api_key descriptor with
        | Some key -> Some (Pave.Model_discovery.Api_key key)
        | None ->
            (match select_account () with
             | None -> None
             | Some account ->
                 with_route (fun route ->
                   let authentication, key, resolve =
                     Cli_auth.resolve_authentication
                       ~account_id:account.Pave.Oauth_store.selection_id
                       ~descriptor ~route ~endpoint:route.endpoint () in
                   if authentication = Pave.Provider.Api_key && key <> "" then
                     Some (Pave.Model_discovery.Api_key key)
                   else Option.map (fun resolve ->
                     let (credential : Pave.Provider.credentials) = resolve () in
                     Pave.Model_discovery.Account_api_key {
                       key = credential.access;
                       account_id = Some account.selection_id }) resolve)) in
      (match Pave.Model_discovery.credential_policy descriptor.id with
       | Some Pave.Model_discovery.Anonymous
       | Some Pave.Model_discovery.Ambient_credentials -> None
       | Some Pave.Model_discovery.Optional_api_key
       | Some Pave.Model_discovery.Required_api_key ->
           Option.map (fun key -> Pave.Model_discovery.Api_key key)
             (Cli_auth.api_key descriptor)
       | Some (Pave.Model_discovery.Stored_api_key _) ->
           stored_api_key ()
       | Some (Pave.Model_discovery.OAuth_account service) -> oauth service
       | None -> None)

let credential_account_id ?registry ~provider ~route = function
  | Some (Pave.Model_discovery.OAuth { account_id; selection_id; _ }) ->
      (match selection_id with Some id -> Some id | None -> account_id)
  | Some (Pave.Model_discovery.Account_api_key { account_id; _ }) -> account_id
  | _credential ->
      let registry = Option.value ~default:Pave.Provider_catalog.builtin_registry registry in
      Option.bind (Pave.Provider_catalog.custom_route registry ~provider ~route)
        (fun custom -> custom.account_id)

let supports_route ?registry (descriptor : Pave.Provider_catalog.descriptor)
    (model : Pave.Model_discovery.model)
    (route : Pave.Provider_catalog.route) =
  (descriptor.id <> "devin" || model.capabilities.tools <> Some false) &&
  Pave.Model_discovery.model_supports_endpoint ?registry
    ~provider:descriptor.id model ~endpoint:route.endpoint

let account_listing (descriptor : Pave.Provider_catalog.descriptor) =
  match Pave.Model_discovery.credential_policy descriptor.id with
  | Some (Pave.Model_discovery.OAuth_account _)
  | Some (Pave.Model_discovery.Stored_api_key _)
    when descriptor.oauth <> None -> true
  | _ -> false

(* A successful listing is not an inference authorization check. In particular,
   configured IDs, OS defaults and unclassified catalogues are not evidence
   that a model works on this route. The coordinator has already checked the
   credential and attached its resolved account to the snapshot. *)
let eligible_models ?registry (descriptor : Pave.Provider_catalog.descriptor)
    (scope : Pave.Model_discovery_coordinator.scope)
    (listing : Pave.Model_discovery.listing) =
  let registry = Option.value ~default:Pave.Provider_catalog.builtin_registry registry in
  match Pave.Provider_catalog.find ~registry scope.provider,
    Pave.Provider_catalog.route descriptor scope.route with
  | Some registered, Some route
    when registered.id = descriptor.id &&
      not (Pave.Provider_catalog.unclassified_models ~registry descriptor.id) &&
      not (descriptor.api_key_env <> None &&
        Pave.Model_discovery.credential_policy descriptor.id =
          Some Pave.Model_discovery.Anonymous &&
        Cli_auth.api_key descriptor = None) &&
      (match listing.source.id_source with
       | Pave.Model_catalog.Pinned_account_listing
       | Pave.Model_catalog.Provider_listing
       | Pave.Model_catalog.Capability_response -> true
       | Pave.Model_catalog.Runtime_default
       | Pave.Model_catalog.Explicit_user_input -> false) &&
      Option.is_some listing.source.retrieved_at ->
      List.filter (fun (model : Pave.Model_discovery.model) ->
        model.identity.provider = scope.provider &&
        model.identity.route = scope.route &&
        model.identity.account_id = scope.account_id &&
        model.provenance.id_source = listing.source.id_source &&
        model.provenance.retrieved_at = listing.source.retrieved_at &&
        supports_route ~registry descriptor model route) listing.models
  | _ -> []

let scope_label (scope : Pave.Model_discovery_coordinator.scope) =
  scope.provider ^ "@" ^ scope.route ^
  (match scope.account_id with
   | None -> ""
   | Some account -> "#" ^ Pave.Model_identity.encode_component account)

let listing_status ?registry (descriptor : Pave.Provider_catalog.descriptor)
    scope (listing : Pave.Model_discovery.listing) models =
  let prefix = scope_label scope in
  if listing.source.id_source =
      Pave.Model_catalog.Runtime_default then
    prefix ^ ": OS-managed model readiness is not verified by discovery"
  else if listing.source.id_source =
      Pave.Model_catalog.Explicit_user_input then
    prefix ^ ": configured IDs are not verified by a live listing"
  else if Pave.Provider_catalog.unclassified_models ?registry descriptor.id then
    prefix ^ ": listed IDs have unverified route compatibility"
  else
    let excluded = List.length listing.models - List.length models in
    Printf.sprintf "%s: %d route-compatible listed ID%s%s; inference authorization is not guaranteed"
      prefix (List.length models) (if List.length models = 1 then "" else "s")
      (if excluded = 0 then "" else
        Printf.sprintf " · %d excluded" excluded)

let identity_selector (model : Pave.Model_discovery.model) =
  Pave.Model_identity.selector model.identity

let identity_label (model : Pave.Model_discovery.model) =
  let identity = model.identity in
  let display = Option.value ~default:identity.upstream_id model.display_name in
  identity.provider ^ "@" ^ identity.route ^
  (match identity.account_id with
   | None -> ""
   | Some account -> "#" ^ Pave.Model_identity.encode_component account) ^
  " · " ^ display ^ " [" ^ identity.upstream_id ^ "]"


let model_detail ?registry (descriptor : Pave.Provider_catalog.descriptor)
    (model : Pave.Model_discovery.model) =
  let capabilities = model.capabilities in
  let display_name = Option.map
    (fun value -> "display name " ^ value) model.display_name in
  let context = Option.map
    (fun tokens -> Printf.sprintf "context %d tokens" tokens)
    capabilities.context_window_tokens in
  let endpoints = match capabilities.supported_endpoints with
    | None -> None
    | Some _ ->
        let names = List.filter_map
          (fun (route : Pave.Provider_catalog.route) ->
            if Pave.Model_discovery.model_supports_endpoint ?registry
                ~provider:descriptor.id model ~endpoint:route.endpoint
            then Some route.name else None) descriptor.routes in
        if names = [] then None
        else Some ("APIs " ^ String.concat "/" names) in
  let tokenizer = Option.map
    (Printf.sprintf "provider tokenizer type %s; metadata only")
    capabilities.provider_tokenizer in
  let tools = Option.map (fun supported ->
    match model.provenance.capability_source, supported with
    | Some Pave.Model_catalog.Explicit_user_input, true ->
        "tool support enabled in custom-provider settings"
    | Some Pave.Model_catalog.Explicit_user_input, false ->
        "tool support disabled in custom-provider settings"
    | _, true -> "tool support reported"
    | _, false -> "tool support not supported (provider-reported)")
      capabilities.tools in
  let compaction = match capabilities.native_compaction_supported with
    | Some true -> Some "native compaction supported (provider-reported)"
    | Some false -> Some "native compaction not supported (provider-reported)"
    | None -> None in
  let output_limit = Option.map
    (Printf.sprintf "maximum output %d tokens") capabilities.max_output_tokens in
  let id_source = Some (match model.provenance.id_source with
    | Pave.Model_catalog.Pinned_account_listing -> "IDs from pinned account listing"
    | Pave.Model_catalog.Provider_listing -> "IDs from provider listing"
    | Pave.Model_catalog.Capability_response -> "IDs from capability response"
    | Pave.Model_catalog.Runtime_default -> "OS-managed runtime default"
    | Pave.Model_catalog.Explicit_user_input -> "explicit user input") in
  let capability_source = Some (match model.provenance.capability_source with
    | None -> "capabilities not reported"
    | Some Pave.Model_catalog.Pinned_account_listing ->
        "capabilities from pinned account listing"
    | Some Pave.Model_catalog.Provider_listing ->
        "capabilities from provider listing"
    | Some Pave.Model_catalog.Capability_response ->
        "capabilities provider-reported"
    | Some Pave.Model_catalog.Explicit_user_input ->
        "capabilities from explicit user input"
    | Some Pave.Model_catalog.Runtime_default ->
        "capabilities from the local runtime contract") in
  let listing_endpoint = Option.map
    (fun endpoint -> "listing endpoint " ^ endpoint)
    model.provenance.endpoint in
  let retrieved_at = Option.map (fun time ->
    let observed = Unix.gmtime time in
    Printf.sprintf "retrieved %04d-%02d-%02dT%02d:%02d:%02dZ"
      (observed.tm_year + 1900) (observed.tm_mon + 1) observed.tm_mday
      observed.tm_hour observed.tm_min observed.tm_sec)
    model.provenance.retrieved_at in
  match List.filter_map Fun.id
    [context; id_source; capability_source; retrieved_at; output_limit;
     compaction; tools; display_name; endpoints; tokenizer; listing_endpoint] with
  | [] -> None
  | parts -> Some (String.concat " · " parts)

let model_details ?registry (descriptor : Pave.Provider_catalog.descriptor) models =
  List.filter_map (fun (model : Pave.Model_discovery.model) ->
    Option.map (fun detail -> identity_selector model, detail)
      (model_detail ?registry descriptor model)) models


let choose ?registry screen ~(descriptor : Pave.Provider_catalog.descriptor)
    ?(intro = []) ?(plain = []) ?route_name
    ?account_id:selected_account_id ~title () =
  let registry = Option.value ~default:Pave.Provider_catalog.builtin_registry registry in
  let route_name = Option.value ~default:descriptor.default_route route_name in
  let custom_account_id = Option.bind
    (Pave.Provider_catalog.custom_route registry ~provider:descriptor.id
      ~route:route_name)
    (fun custom -> custom.account_id) in
  let account_selection_cancelled = ref false in
  let account_id = match custom_account_id,
      (if account_listing descriptor then Cli_auth.api_key descriptor else None),
      selected_account_id with
    | Some configured, _, Some requested when configured <> requested ->
        failwith "selected account does not match the configured custom route"
    | Some configured, _, _ -> Some configured
    | None, Some _, Some requested ->
        failwith ("selected account " ^ requested ^
          " cannot be used with an environment API key")
    | None, Some _, None -> None
    | None, None, Some requested -> Some requested
    | None, None, None when account_listing descriptor ->
        let accounts = Pave.Oauth_store.accounts
          ~path:(Pave.Oauth_store.default_path ()) ~provider:descriptor.id in
        (match accounts with
         | [] -> None
         | [ account ] -> Some account.Pave.Oauth_store.selection_id
         | accounts ->
             let options = List.map (fun account ->
               let selection_id = account.Pave.Oauth_store.selection_id in
               saved_account_label account, selection_id) accounts in
             (match Tui.choose screen
               ~intro:["Provider credentials are stored separately by account.";
                 "Select the account allowed to receive this model listing."]
               ~title:("Models · " ^ descriptor.id ^ " account")
               ~choices:(List.map fst options) with
              | None -> account_selection_cancelled := true; None
              | Some label -> List.assoc_opt label options))
    | None, None, None -> None in
  if !account_selection_cancelled then None else
  let request = {
    Pave.Model_discovery_coordinator.scope = {
      provider = descriptor.id; account_id; route = route_name };
    run = (fun cancel ->
      let access = credential ~registry ?account_id ~route_name descriptor in
      let resolved_account = credential_account_id ~registry
        ~provider:descriptor.id ~route:route_name access in
      let result =
        if descriptor.api_key_env <> None &&
          Pave.Model_discovery.credential_policy descriptor.id =
            Some Pave.Model_discovery.Anonymous &&
          Cli_auth.api_key descriptor = None then
          Error (Pave.Model_discovery.Credential_error
            "API key required for inference; public listing is insufficient")
        else if account_id <> None && account_id <> resolved_account &&
          descriptor.id <> "azure"
        then Error Pave.Model_discovery.Invalid_credential
        else try Pave.Model_discovery.discover ~registry ~cancel
          ~provider:descriptor.id ~route_name ?account_id:resolved_account
          ?credential:access ()
        with Pave.Provider.Cancelled ->
          Error (Pave.Model_discovery.Transport_error "request timed out") in
      result, resolved_account);
  } in
  let coordinator = Pave.Model_discovery_coordinator.start [request] in
  Fun.protect ~finally:(fun () ->
    Pave.Model_discovery_coordinator.close coordinator) (fun () ->
    let on_wake () =
      match Pave.Model_discovery_coordinator.poll coordinator with
      | [{ scope; status = Pave.Model_discovery_coordinator.Loading }] ->
          Tui.update_choices screen ~verified:[]
            ~status:(Some (scope_label scope ^ ": checking live model availability")) ()
      | [{ scope; status = Ready listing }] ->
          let routed_models = eligible_models ~registry descriptor scope listing in
          let values = List.map identity_selector routed_models in
          let details = model_details ~registry descriptor routed_models in
          let labels = List.map (fun model ->
            identity_selector model, identity_label model) routed_models in
          Tui.update_choices screen ~verified:values ~details ~labels
            ~status:(Some (listing_status ~registry descriptor scope listing
              routed_models)) ()
      | [{ scope; status = Unsupported error }]
      | [{ scope; status = Failed error }] ->
          Tui.update_choices screen ~verified:[]
            ~status:(Some (scope_label scope ^ ": " ^
              Pave.Model_discovery.message error)) ()
      | _ -> () in
    let selected = Tui.choose screen ~intro ~plain
      ~initial_status:"Loading available models…"
      ~wake_fd:(Pave.Model_discovery_coordinator.read_fd coordinator)
      ~on_wake ~title ~choices:[] in
    Option.map (fun input ->
      try
        let _, identity, _ = Pave.Interaction.resolve_model ~registry
          ~current_provider:descriptor.id ~current_route:route_name
          ?current_account_id:account_id ~input () in
        Pave.Model_identity.selector identity
      with Invalid_argument _ -> input) selected)

(* Each registered route/account has its own discovery scope. A model discovered
   for one route or sign-in never gets reused under another identity. *)
let choose_all ?registry screen ~(active : Pave.Provider_catalog.descriptor)
    ~current_route () =
  let registry = Option.value ~default:Pave.Provider_catalog.builtin_registry registry in
  let providers = active :: List.filter
    (fun (entry : Pave.Provider_catalog.descriptor) -> entry.id <> active.id)
    (Pave.Provider_catalog.all ~registry ()) in
  let skipped = ref [] in
  let requests = List.concat_map (fun (descriptor : Pave.Provider_catalog.descriptor) ->
    let routes = if descriptor.id = active.id then
      List.sort (fun (a : Pave.Provider_catalog.route) b ->
        compare (a.name <> current_route) (b.name <> current_route))
        descriptor.routes
    else descriptor.routes in
    List.concat_map (fun (route : Pave.Provider_catalog.route) ->
      let custom_account_id = Option.bind
        (Pave.Provider_catalog.custom_route registry ~provider:descriptor.id
          ~route:route.name)
        (fun custom -> custom.account_id) in
      let accounts = match custom_account_id with
        | Some id -> [Some id]
        | None when account_listing descriptor &&
            Cli_auth.api_key descriptor = None ->
            (match Pave.Oauth_store.accounts
              ~path:(Pave.Oauth_store.default_path ()) ~provider:descriptor.id with
             | [] -> [None]
             | accounts -> List.map (fun account ->
                 Some account.Pave.Oauth_store.selection_id) accounts)
        | None -> [None] in
      let unavailable = if Pave.Provider_catalog.unclassified_models
          ~registry descriptor.id then
          Some "route compatibility cannot be verified from this listing"
        else if descriptor.id = "apple" then
          Some "OS-managed model readiness is not verified by discovery"
        else if descriptor.api_key_env <> None &&
          Pave.Model_discovery.credential_policy descriptor.id =
            Some Pave.Model_discovery.Anonymous &&
          Cli_auth.api_key descriptor = None then
          Some "API key required for inference; the public listing is not enough"
        else
          match Pave.Model_discovery.adapter_for ~provider:descriptor.id
            ~route with
          | None -> Some "no supported model listing for this API route"
          | Some adapter ->
              (match adapter.credential_policy with
               | Pave.Model_discovery.Required_api_key
                 when Cli_auth.api_key descriptor = None ->
                   Some "credentials required"
               | Pave.Model_discovery.OAuth_account _
               | Pave.Model_discovery.Stored_api_key _
                 when accounts = [None] && Cli_auth.api_key descriptor = None ->
                   Some "saved sign-in or API key required"
               | _ -> None) in
      match unavailable with
      | Some reason ->
          List.iter (fun account_id ->
            skipped := (scope_label {
              Pave.Model_discovery_coordinator.provider = descriptor.id;
              route = route.name; account_id } ^ ": " ^ reason) :: !skipped)
            accounts;
          []
      | None -> List.map (fun requested_account ->
        {
          Pave.Model_discovery_coordinator.scope = {
            provider = descriptor.id; account_id = requested_account;
            route = route.name };
          run = (fun cancel ->
            let access = credential ~registry ?account_id:requested_account
              ~route_name:route.name descriptor in
            let resolved_account = credential_account_id ~registry
              ~provider:descriptor.id ~route:route.name access in
            let result =
              if requested_account <> None &&
                requested_account <> resolved_account && descriptor.id <> "azure"
              then Error Pave.Model_discovery.Invalid_credential
              else try Pave.Model_discovery.discover ~registry ~cancel
                ~provider:descriptor.id ~route_name:route.name
                ?account_id:resolved_account ?credential:access ()
              with Pave.Provider.Cancelled ->
                Error (Pave.Model_discovery.Transport_error "request timed out") in
            result, resolved_account);
        }) accounts) routes) providers in
  let coordinator = Pave.Model_discovery_coordinator.start
    ~max_workers:4 ~timeout_seconds:20. requests in
  let ready_cache = Array.make (List.length requests) None in
  Fun.protect ~finally:(fun () ->
    Pave.Model_discovery_coordinator.close coordinator) (fun () ->
    let on_wake () =
      let snapshots = Pave.Model_discovery_coordinator.poll coordinator in
      let verified = ref [] and details = ref [] and labels = ref [] in
      let state_text = List.mapi (fun index
          (snapshot : Pave.Model_discovery_coordinator.snapshot) ->
        let provider = scope_label snapshot.scope in
        match snapshot.status with
        | Loading -> provider ^ ": loading"
        | Unsupported error | Failed error ->
            provider ^ ": " ^ Pave.Model_discovery.message error
        | Ready listing ->
            let values, annotations, display_labels, text =
              match ready_cache.(index) with
              | Some cached -> cached
              | None ->
                  let descriptor = match Pave.Provider_catalog.find ~registry
                      snapshot.scope.provider with
                    | Some descriptor -> descriptor
                    | None -> assert false in
                  let models = eligible_models ~registry descriptor
                    snapshot.scope listing in
                  let cached = (
                    List.map identity_selector models,
                    model_details ~registry descriptor models,
                    List.map (fun model ->
                      identity_selector model, identity_label model) models,
                    listing_status ~registry descriptor snapshot.scope listing
                      models) in
                  ready_cache.(index) <- Some cached;
                  cached in
            verified := List.rev_append values !verified;
            details := List.rev_append annotations !details;
            labels := List.rev_append display_labels !labels;
            text)
          snapshots @ List.rev !skipped in
      let finished = List.fold_left (fun count snapshot ->
        match snapshot.Pave.Model_discovery_coordinator.status with
        | Loading -> count | _ -> count + 1) 0 snapshots in
      let total = List.length snapshots + List.length !skipped in
      let status = Printf.sprintf
        "%d/%d route/account checks complete · %d listed IDs · Tab: status"
        (finished + List.length !skipped) total (List.length !verified) in
      Tui.update_choices screen ~verified:(List.rev !verified)
        ~details:(List.rev !details) ~labels:(List.rev !labels)
        ~status_pages:state_text ~status:(Some status) () in
    Tui.choose screen
      ~intro:["Only freshly listed, route-compatible IDs appear here.";
        "Listing does not guarantee inference permission; failures are on the status pages.";
        "Selection changes this conversation only."]
      ~initial_status:(if requests = [] then
        "No eligible model listings; check credentials or use a known ID in the CLI"
        else Printf.sprintf "Checking %d registered route/account listings…"
          (List.length requests))
      ~wake_fd:(Pave.Model_discovery_coordinator.read_fd coordinator)
      ~on_wake ~title:"Models · available routes" ~choices:[])
