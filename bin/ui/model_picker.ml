(* Discovery runs on a cancellable worker; unclassified listings never become
   verified model suggestions in the chooser. *)
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
      let stored_accounts = Pave.Oauth_store.accounts
        ~path:(Pave.Oauth_store.default_path ()) ~provider:descriptor.id in
      let select_account () = match account_id, stored_accounts with
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
  Pave.Model_discovery.model_supports_endpoint ?registry
    ~provider:descriptor.id model ~endpoint:route.endpoint

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

let scope_label (scope : Pave.Model_discovery_coordinator.scope) =
  scope.provider ^ "@" ^ scope.route ^
  (match scope.account_id with
   | None -> ""
   | Some account -> "#" ^ Pave.Model_identity.encode_component account)

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
    ?account_id:selected_account_id ~title ~choices () =
  let registry = Option.value ~default:Pave.Provider_catalog.builtin_registry registry in
  let route_name = Option.value ~default:descriptor.default_route route_name in
  let aws_process_note =
    if Sys.getenv_opt "PAVE_AWS_CREDENTIAL_PROCESS" = Some "allow" then
      "credential_process runs local code and is enabled for this session."
    else "credential_process is disabled; opt in only for trusted profiles." in
  let intro = match descriptor.id with
    | "azure" -> [
        "Enter Azure deployment name.";
        "Set endpoint; use an API key or current Azure CLI identity.";
        "Listing needs current-subscription Azure CLI management read access."]
      @ intro
    | "google-vertex" -> [
        "Type a known Vertex model ID.";
        "Set Google project/location; authorized-user and service-account ADC work directly.";
        "Impersonated ADC needs gcloud; no publisher model-list API exists."]
      @ intro
    | "apple" -> [
        "OS-managed ID: default.";
        "Needs macOS 26+, Apple silicon and Apple Intelligence.";
        "Text only; no Pave tools or model listing."]
      @ intro
    | "amazon-bedrock" -> [
        aws_process_note;
        "Active on-demand text IDs and inference profiles are listed; neither proves Invoke permission.";
        if route_name = "converse-stream" then
          "ConverseStream streams text and retains native tool continuation."
        else
          "Converse buffers; select ConverseStream to stream text."]
      @ intro
    | _ -> intro in
  let custom_account_id = Option.bind
    (Pave.Provider_catalog.custom_route registry ~provider:descriptor.id
      ~route:route_name)
    (fun custom -> custom.account_id) in
  let account_selection_cancelled = ref false in
  let account_id = match custom_account_id,
      (if descriptor.oauth <> None then Cli_auth.api_key descriptor else None),
      selected_account_id with
    | Some configured, _, Some requested when configured <> requested ->
        failwith "selected account does not match the configured custom route"
    | Some configured, _, _ -> Some configured
    | None, Some _, _ -> None
    | None, None, Some requested -> Some requested
    | None, None, None when descriptor.oauth <> None ->
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
      let account_id = credential_account_id ~registry
        ~provider:descriptor.id ~route:route_name access in
      let result =
        try Pave.Model_discovery.discover ~registry ~cancel
          ~provider:descriptor.id ~route_name ?account_id ?credential:access ()
        with Pave.Provider.Cancelled ->
          Error (Pave.Model_discovery.Transport_error "request timed out") in
      result, account_id);
  } in
  let coordinator = Pave.Model_discovery_coordinator.start [request] in
  Fun.protect ~finally:(fun () ->
    Pave.Model_discovery_coordinator.close coordinator) (fun () ->
    let on_wake () =
      match Pave.Model_discovery_coordinator.poll coordinator with
      | [{ scope; status = Pave.Model_discovery_coordinator.Loading }] ->
          Tui.update_choices screen ~verified:[]
            ~status:(Some (scope_label scope ^
              ": refreshing the provider listing or configured model roster")) ()
      | [{ scope; status = Ready listing }] ->
          let prefix = scope_label scope in
          let route = Pave.Provider_catalog.route descriptor route_name in
          let routed_models = match route with
            | None -> []
            | Some route -> List.filter
                (fun (model : Pave.Model_discovery.model) ->
                  supports_route ~registry descriptor model route) listing.models in
          let values = List.map identity_selector routed_models in
          let details = model_details ~registry descriptor routed_models in
          let labels = List.map (fun model ->
            identity_selector model, identity_label model) routed_models in
          if listing.source.id_source = Pave.Model_catalog.Runtime_default then
            Tui.update_choices screen ~verified:values ~details ~labels
              ~status:(Some
                "macOS 26+ · Apple silicon · Apple Intelligence") ()
          else if listing.source.id_source =
              Pave.Model_catalog.Explicit_user_input then
            Tui.update_choices screen ~verified:[] ~listed:values ~details
              ~labels
              ~status:(Some (Printf.sprintf
                "%s: %d configured IDs; provider compatibility is unverified"
                prefix (List.length values))) ()
          else if Pave.Provider_catalog.unclassified_models ~registry descriptor.id then
            Tui.update_choices screen ~verified:[] ~listed:values ~details
              ~labels
              ~status:(Some (Printf.sprintf
                "%s: %d live IDs; inference compatibility is unverified"
                prefix (List.length values))) ()
          else
            let excluded = List.length listing.models -
              List.length routed_models in
            Tui.update_choices screen ~verified:values ~details ~labels
              ~status:(Some (Printf.sprintf "%s: %d live model%s%s"
                prefix (List.length values)
                (if List.length values = 1 then "" else "s")
                (if excluded = 0 then "" else
                  Printf.sprintf " · %d don't advertise %s"
                    excluded route_name))) ()
      | [{ scope; status = Unsupported error }]
      | [{ scope; status = Failed error }] ->
          Tui.update_choices screen ~verified:[]
            ~status:(Some (scope_label scope ^ ": " ^
              Pave.Model_discovery.message error)) ()
      | _ -> () in
    let selected = Tui.choose screen ~allow_custom:true ~intro ~plain
      ~initial_status:"Loading available models…"
      ~wake_fd:(Pave.Model_discovery_coordinator.read_fd coordinator)
      ~on_wake ~title ~choices in
    Option.map (fun input ->
      try
        let _, identity, _ = Pave.Interaction.resolve_model ~registry
          ~current_provider:descriptor.id ~current_route:route_name
          ?current_account_id:account_id ~input () in
        Pave.Model_identity.selector identity
      with Invalid_argument _ -> input) selected)

(* Every provider gets a snapshot. Unsupported routes and missing credentials
   remain visible in the status summary; only account-scoped live IDs become
   model choices. *)
let choose_all ?registry screen ~(active : Pave.Provider_catalog.descriptor)
    ~current_route () =
  let registry = Option.value ~default:Pave.Provider_catalog.builtin_registry registry in
  let providers = active :: List.filter
    (fun (entry : Pave.Provider_catalog.descriptor) -> entry.id <> active.id)
    (Pave.Provider_catalog.all ~registry ()) in
  let route_actions = Pave.Interaction.model_route_browse_choices ~registry () in
  let requests = List.map (fun (descriptor : Pave.Provider_catalog.descriptor) ->
    let route = if descriptor.id = active.id then current_route
      else descriptor.default_route in
    let account_id = Option.bind
      (Pave.Provider_catalog.custom_route registry ~provider:descriptor.id
        ~route)
      (fun custom -> custom.account_id) in
    {
      Pave.Model_discovery_coordinator.scope = {
        provider = descriptor.id; account_id; route };
      run = (fun cancel ->
        let access = credential ~registry ?account_id ~route_name:route descriptor in
        let account_id = credential_account_id ~registry
          ~provider:descriptor.id ~route access in
        let result =
          try Pave.Model_discovery.discover ~registry ~cancel
            ~provider:descriptor.id ~route_name:route ?account_id
            ?credential:access ()
          with Pave.Provider.Cancelled ->
            Error (Pave.Model_discovery.Transport_error "request timed out") in
        result, account_id);
    }) providers in
  let coordinator = Pave.Model_discovery_coordinator.start
    ~max_workers:4 ~timeout_seconds:20. requests in
  Fun.protect ~finally:(fun () ->
    Pave.Model_discovery_coordinator.close coordinator) (fun () ->
    let on_wake () =
      let snapshots = Pave.Model_discovery_coordinator.poll coordinator in
      let verified = ref [] and listed = ref [] and details = ref []
      and labels = ref [] in
      let state_text = List.map (fun
          (snapshot : Pave.Model_discovery_coordinator.snapshot) ->
        let descriptor = match Pave.Provider_catalog.find ~registry
            snapshot.Pave.Model_discovery_coordinator.scope.provider with
          | Some descriptor -> descriptor
          | None -> assert false in
        let provider = scope_label snapshot.scope in
        match snapshot.status with
        | Loading -> provider ^ ": loading"
        | Unsupported (Pave.Model_discovery.Unsupported_route _) ->
            provider ^ ": choose a registered API route"
        | Unsupported error ->
            provider ^ ": " ^ Pave.Model_discovery.message error
        | Failed Pave.Model_discovery.Missing_credential ->
            provider ^ ": credentials required"
        | Failed error ->
            provider ^ ": " ^ Pave.Model_discovery.message error
        | Ready listing ->
            let route = Pave.Provider_catalog.route descriptor
              snapshot.scope.route in
            let models = match route with
              | None -> []
              | Some route -> List.filter
                  (fun (model : Pave.Model_discovery.model) ->
                    supports_route ~registry descriptor model route) listing.models in
            let values = List.map identity_selector models in
            let annotations = model_details ~registry descriptor models in
            let display_labels = List.map (fun model ->
              identity_selector model, identity_label model) models in
            if Pave.Provider_catalog.unclassified_models ~registry descriptor.id then
              listed := !listed @ values
            else verified := !verified @ values;
            details := !details @ annotations;
            labels := !labels @ display_labels;
            Printf.sprintf "%s: %d %s" provider (List.length models)
              (if listing.source.id_source = Pave.Model_catalog.Explicit_user_input
               then "configured IDs" else "live IDs")) snapshots in
      let route_action_values = List.map fst route_actions in
      let route_action_labels = route_actions in
      labels := !labels @ route_action_labels;
      let finished = List.fold_left (fun count snapshot ->
        match snapshot.Pave.Model_discovery_coordinator.status with
        | Loading -> count | _ -> count + 1) 0 snapshots in
      let ready = List.fold_left (fun count snapshot ->
        match snapshot.Pave.Model_discovery_coordinator.status with
        | Ready _ -> count + 1 | _ -> count) 0 snapshots in
      let status = Printf.sprintf
        "%d/%d listings complete · %d ready · Tab browses status · select a [route] action for other APIs"
        finished (List.length snapshots) ready in
      Tui.update_choices screen ~verified:!verified ~listed:!listed
        ~actions:route_action_values ~details:!details ~labels:!labels
        ~status_pages:state_text ~status:(Some status) () in
    Tui.choose screen ~allow_custom:true
      ~intro:["Listed or configured model IDs retain account and route scope.";
        "Select a [route] action to browse another registered API route.";
        "Unverified IDs are marked; missing credentials and listing failures are reported.";
        "Selecting a listed model changes this conversation only."]
      ~initial_status:(Printf.sprintf "Discovering %d registered providers…"
        (List.length requests))
      ~wake_fd:(Pave.Model_discovery_coordinator.read_fd coordinator)
      ~on_wake ~title:"Models · registered providers" ~choices:[])
