(* Discovery runs on cancellable workers. Only fresh, route-compatible IDs
   from a registered provider's successful listing become model choices. *)
type selection = {
  selector : string;
  display_name : string option;
  thinking : string option option;
}

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

(* Pair the readable provider name with the exact scope so every picker
   screen says whose models are listed. *)
let scope_title (descriptor : Pave.Provider_catalog.descriptor) scope =
  let name = Tui.single_line descriptor.display_name |> String.trim in
  if name = "" || name = descriptor.id then scope_label scope
  else name ^ " · " ^ scope_label scope

let listing_status ?registry (descriptor : Pave.Provider_catalog.descriptor)
    _scope (listing : Pave.Model_discovery.listing) models =
  if listing.source.id_source =
      Pave.Model_catalog.Runtime_default then
    "OS-managed model; readiness is not verified by discovery"
  else if listing.source.id_source =
      Pave.Model_catalog.Explicit_user_input then
    "Configured IDs; not verified by a live listing"
  else if Pave.Provider_catalog.unclassified_models ?registry descriptor.id then
    "Listed IDs; route compatibility unverified"
  else
    let excluded = List.length listing.models - List.length models in
    Printf.sprintf "Fresh listing%s · access is confirmed on the first request"
      (if excluded = 0 then "" else
        Printf.sprintf " · %d other-API ID%s hidden" excluded
          (if excluded = 1 then "" else "s"))

let identity_selector (model : Pave.Model_discovery.model) =
  Pave.Model_identity.selector model.identity

let visible_model_name (model : Pave.Model_discovery.model) =
  let sanitize value = value |> Tui.single_line |> String.trim in
  let fallback = sanitize model.identity.upstream_id in
  match model.display_name with
  | None -> fallback
  | Some name ->
      let name = sanitize name in
      if name = "" then fallback else name

let identity_label (model : Pave.Model_discovery.model) =
  let identity = model.identity in
  visible_model_name model ^ " · " ^ identity.provider ^ "@" ^ identity.route ^
  (match identity.account_id with
   | None -> ""
   | Some account -> "#" ^ Pave.Model_identity.encode_component account)

(* Rows in a single-scope list omit the scope already shown in the title;
   repeated display names fall back to the exact upstream ID. *)
let row_labels ?provider ?current_model (models : Pave.Model_discovery.model list) =
  let names = List.map visible_model_name models in
  List.map (fun (model : Pave.Model_discovery.model) ->
    let name = visible_model_name model in
    let shared = List.length (List.filter (String.equal name) names) > 1 in
    let upstream = Tui.single_line model.identity.upstream_id in
    let label = if shared && name <> upstream then name ^ " · " ^ upstream
      else name in
    let label = match provider with
      | Some provider when provider <> "" -> label ^ "  · " ^ provider
      | _ -> label in
    let selector = identity_selector model in
    selector, (if current_model = Some selector then label ^ "  (current)"
      else label)) models

let model_detail ?registry (descriptor : Pave.Provider_catalog.descriptor)
    (model : Pave.Model_discovery.model) =
  let capabilities = model.capabilities in
  let display_name = match model.display_name with
    | None -> None
    | Some name ->
        let name = Tui.single_line name |> String.trim in
        if name = "" then None else Some ("display name " ^ name) in
  let context = Option.map
    (fun tokens -> Printf.sprintf "context %d tokens" tokens)
    capabilities.context_window_tokens in
  let endpoints = match capabilities.supported_endpoints with
    | None -> None
    | Some _ ->
        let names = List.filter_map
          (fun (route : Pave.Provider_catalog.route) ->
            Option.bind (Pave.Provider_catalog.route descriptor route.name)
              (fun route ->
                if Pave.Model_discovery.model_supports_endpoint ?registry
                    ~provider:descriptor.id model ~endpoint:route.endpoint
                then Some route.name else None)) descriptor.routes in
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
  let efforts = match capabilities.effort_levels with
    | Some (_ :: _ as levels) ->
        Some ("effort " ^ String.concat "/" (List.map Tui.single_line levels))
    | _ -> None in
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
    [context; output_limit; tools; efforts; endpoints; compaction;
     Some (identity_selector model); display_name; capability_source;
     id_source; retrieved_at; tokenizer; listing_endpoint] with
  | [] -> None
  | parts -> Some (String.concat " · " parts)

let model_details ?registry (descriptor : Pave.Provider_catalog.descriptor) models =
  List.filter_map (fun (model : Pave.Model_discovery.model) ->
    Option.map (fun detail -> identity_selector model, detail)
      (model_detail ?registry descriptor model)) models


let effort_options (route : Pave.Provider_catalog.route)
    (model : Pave.Model_discovery.model) =
  Pave.Provider.effort_choices route.wire model.capabilities.effort_levels

let initial_effort ~current_thinking options =
  match current_thinking with
  | Some level when List.mem level options -> level
  | _ -> "Provider default"

let configure_model_effort screen ~current_thinking
    (descriptor : Pave.Provider_catalog.descriptor)
    (model : Pave.Model_discovery.model) =
  let route = Option.get (Pave.Provider_catalog.route descriptor model.identity.route) in
  let options = effort_options route model in
  if options = [] then Some None else
  let provenance = match model.provenance.capability_source with
    | Some Pave.Model_catalog.Pinned_account_listing -> "fresh account listing"
    | Some Pave.Model_catalog.Provider_listing -> "fresh provider listing"
    | Some Pave.Model_catalog.Capability_response -> "fresh capability response"
    | Some Pave.Model_catalog.Explicit_user_input -> "explicit configuration"
    | Some Pave.Model_catalog.Runtime_default -> "local runtime"
    | None -> "fresh model listing; effort metadata not reported" in
  let explanation = match model.capabilities.effort_levels, options with
    | None, _ ->
        "Effort metadata unknown. Default sends no override."
    | Some [], _ ->
        "This model reports no effort levels. Default sends no override."
    | Some _, [] ->
        "No reported level is supported by this API. Default sends no override."
    | Some _, _ ->
        "From " ^ provenance ^
        "; API-supported levels only. Default sends no override." in
  Tui.choose screen ~segmented:true
    ~initial_selected:(initial_effort ~current_thinking options)
    ~intro:[visible_model_name model; identity_selector model]
    ~initial_status:explanation
    ~title:"Model + effort"
    ~choices:("Provider default" :: options)
  |> Option.map (function "Provider default" -> None | level -> Some level)

let discovery_request ?registry ?http
    (descriptor : Pave.Provider_catalog.descriptor)
    (scope : Pave.Model_discovery_coordinator.scope) :
    Pave.Model_discovery_coordinator.request =
  let registry = Option.value ~default:Pave.Provider_catalog.builtin_registry registry in
  { scope; run = (fun cancel ->
      let access = credential ~registry ?account_id:scope.account_id
        ~route_name:scope.route descriptor in
      let resolved_account = credential_account_id ~registry
        ~provider:descriptor.id ~route:scope.route access in
      let result =
        if descriptor.api_key_env <> None &&
          Pave.Model_discovery.credential_policy descriptor.id =
            Some Pave.Model_discovery.Anonymous &&
          Cli_auth.api_key descriptor = None then
          Error (Pave.Model_discovery.Credential_error
            "API key required for inference; public listing is insufficient")
        else if scope.account_id <> None &&
          scope.account_id <> resolved_account && descriptor.id <> "azure"
        then Error Pave.Model_discovery.Invalid_credential
        else try Pave.Model_discovery.discover ~registry ?http ~cancel
          ~provider:descriptor.id ~route_name:scope.route
          ?account_id:resolved_account ?credential:access ()
        with Pave.Provider.Cancelled ->
          Error (Pave.Model_discovery.Transport_error "request timed out") in
      result, resolved_account) }

let choose ?registry screen ~(descriptor : Pave.Provider_catalog.descriptor)
    ?(intro = []) ?(plain = []) ?route_name ?initial_filter ?scope_action
    ?(configure_effort = false) ?current_thinking ?current_model
    ?account_id:selected_account_id ~title () =
  let registry = Option.value ~default:Pave.Provider_catalog.builtin_registry registry in
  let route_name = Option.value ~default:descriptor.default_route route_name in
  let fresh_models = Hashtbl.create 16 in
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
  let request = discovery_request ~registry descriptor {
    Pave.Model_discovery_coordinator.provider = descriptor.id;
    account_id; route = route_name } in
  let coordinator = Pave.Model_discovery_coordinator.start [request] in
  Fun.protect ~finally:(fun () ->
    Pave.Model_discovery_coordinator.close coordinator) (fun () ->
    let on_wake () =
      match Pave.Model_discovery_coordinator.poll coordinator with
      | [{ scope; status = Pave.Model_discovery_coordinator.Loading }] ->
          Tui.update_choices screen ~verified:[]
            ~status:(Some ("Checking " ^ scope_title descriptor scope ^ "…")) ()
      | [{ scope; status = Ready listing }] ->
          let routed_models = eligible_models ~registry descriptor scope listing in
          List.iter (fun model ->
            Hashtbl.replace fresh_models (identity_selector model) model) routed_models;
          let values = List.map identity_selector routed_models in
          let details = model_details ~registry descriptor routed_models in
          let labels = row_labels ~provider:(Tui.single_line descriptor.display_name)
            ?current_model routed_models in
          Tui.update_choices screen ~verified:values ~details ~labels
            ?preferred:current_model
            ~status:(Some (listing_status ~registry descriptor scope listing
              routed_models)) ()
      | [{ status = Unsupported error; _ }]
      | [{ status = Failed error; _ }] ->
          Tui.update_choices screen ~verified:[]
            ~status:(Some (Pave.Model_discovery.message error)) ()
      | _ -> () in
    let selected = Tui.choose ?initial_filter ?scope_action screen ~intro ~plain
      ~initial_status:"Loading available models…"
      ~wake_fd:(Pave.Model_discovery_coordinator.read_fd coordinator)
      ~on_wake ~title ~choices:[] in
    Option.bind selected (fun input ->
      let selector =
        try
          let _, identity, _ = Pave.Interaction.resolve_model ~registry
            ~current_provider:descriptor.id ~current_route:route_name
            ?current_account_id:account_id ~input () in
          Pave.Model_identity.selector identity
        with Invalid_argument _ -> input in
      match Hashtbl.find_opt fresh_models selector with
      | None -> Some { selector = input; display_name = None; thinking = None }
      | Some model ->
          let thinking = if configure_effort then
            configure_model_effort screen ~current_thinking descriptor model
            |> Option.map Option.some
          else Some None in
          Option.map (fun thinking ->
            { selector; display_name = model.display_name; thinking }) thinking))

(* Enumerating scopes is local metadata work, never model discovery. Each scope
   is opened explicitly and gets a new, isolated discovery worker. *)
let available_scopes ?registry () =
  let registry = Option.value ~default:Pave.Provider_catalog.builtin_registry registry in
  Pave.Provider_catalog.all ~registry () |> List.concat_map
    (fun (descriptor : Pave.Provider_catalog.descriptor) ->
      descriptor.routes |> List.concat_map (fun (route : Pave.Provider_catalog.route) ->
        let configured_account = Option.bind
          (Pave.Provider_catalog.custom_route registry ~provider:descriptor.id
            ~route:route.name) (fun custom -> custom.account_id) in
        let accounts = match configured_account with
          | Some id -> [Some id]
          | None when account_listing descriptor && Cli_auth.api_key descriptor = None ->
              (match Pave.Oauth_store.accounts ~path:(Pave.Oauth_store.default_path ())
                  ~provider:descriptor.id with
               | [] -> [None]
               | accounts -> List.map (fun account ->
                   Some account.Pave.Oauth_store.selection_id) accounts)
          | None -> [None] in
        List.map (fun account_id ->
          ({ Pave.Model_discovery_coordinator.provider = descriptor.id;
             route = route.name; account_id }, descriptor)) accounts))

(* A cheap local check: environment keys, saved sign-ins and keyless local
   routes. It never resolves or refreshes a credential. *)
let scope_ready ?registry (descriptor : Pave.Provider_catalog.descriptor)
    (scope : Pave.Model_discovery_coordinator.scope) =
  let registry = Option.value ~default:Pave.Provider_catalog.builtin_registry registry in
  match Pave.Provider_catalog.custom_route registry ~provider:descriptor.id
      ~route:scope.route with
  | Some { auth = Pave.Custom_provider.No_auth; _ } -> true
  | Some { auth = Pave.Custom_provider.Api_key_env name; _ } ->
      (match Sys.getenv_opt name with Some key -> key <> "" | None -> false)
  | None ->
      Cli_auth.api_key descriptor <> None || scope.account_id <> None ||
      (match Pave.Model_discovery.credential_policy descriptor.id with
       | Some Pave.Model_discovery.Anonymous -> descriptor.api_key_env = None
       | Some Pave.Model_discovery.Ambient_credentials -> true
       | Some Pave.Model_discovery.Optional_api_key -> descriptor.id <> "azure"
       | Some (Pave.Model_discovery.Stored_api_key _)
       | Some (Pave.Model_discovery.OAuth_account _) ->
           Pave.Oauth_store.accounts ~path:(Pave.Oauth_store.default_path ())
             ~provider:descriptor.id <> []
       | Some Pave.Model_discovery.Required_api_key | None -> false)

(* Group scopes by provider in catalog order, then list the active provider,
   ready providers and providers still needing credentials. *)
let provider_groups ~ready ~active_id scopes =
  let ordered = List.fold_left (fun groups
      (scope, (descriptor : Pave.Provider_catalog.descriptor)) ->
    if List.exists (fun ((known : Pave.Provider_catalog.descriptor), _) ->
        known.id = descriptor.id) groups then
      List.map (fun ((known : Pave.Provider_catalog.descriptor), members) ->
        known, if known.id = descriptor.id then members @ [scope] else members) groups
    else groups @ [descriptor, [scope]]) [] scopes in
  let entries = List.map (fun (descriptor, members) ->
    descriptor, members, List.exists (ready descriptor) members) ordered in
  let current, others = List.partition
    (fun ((descriptor : Pave.Provider_catalog.descriptor), _, _) ->
      descriptor.id = active_id) entries in
  let available, missing = List.partition (fun (_, _, ready) -> ready) others in
  current @ available @ missing

let provider_label ~active_id
    ((descriptor : Pave.Provider_catalog.descriptor), members, ready) =
  let name = Tui.single_line descriptor.display_name |> String.trim in
  let name = if name = "" || name = descriptor.id then descriptor.id
    else name ^ " · " ^ descriptor.id in
  let count = List.length members in
  let name = if count > 1 then Printf.sprintf "%s  · %d APIs/accounts" name count
    else name in
  if descriptor.id = active_id then name ^ "  (current)"
  else if ready then name
  else name ^ "  · needs sign-in or API key"

let scope_choice_label (descriptor : Pave.Provider_catalog.descriptor)
    (scope : Pave.Model_discovery_coordinator.scope) =
  scope.route ^
  (match scope.account_id with
   | None -> ""
   | Some account -> " · account " ^ Tui.single_line account) ^
  (if scope.route = descriptor.default_route then "  (default API)" else "")

(* Provider first, then model: the provider list is local metadata, and only
   the scope the user opens is fetched. Esc in a model list returns to the
   provider list. A typed filter (`/model foo`) searches the active scope. *)
let browse ?registry ?initial_filter ?(configure_effort = false)
    ?current_thinking ?current_model ?current_account_id screen
    ~(active : Pave.Provider_catalog.descriptor) ~current_route () =
  let registry = Option.value ~default:Pave.Provider_catalog.builtin_registry registry in
  let initial_scope : Pave.Model_discovery_coordinator.scope = {
    provider = active.id; route = current_route; account_id = current_account_id } in
  let providers_action = "← Choose another provider" in
  let open_models initial_filter scope descriptor =
    let selection = choose ~registry ?initial_filter screen ~descriptor
        ~route_name:scope.Pave.Model_discovery_coordinator.route
        ?account_id:scope.account_id ~configure_effort ?current_thinking ?current_model
        ~scope_action:providers_action ~plain:[providers_action]
        ~intro:[(if initial_filter = None
            then "Type to filter · Tab or Esc returns to providers."
            else "Type to filter · Tab chooses another provider.");
          "Applies to this conversation only; /setup saves a default."]
        ~title:("Models · " ^ scope_title descriptor scope) () in
    match selection with
    | Some selection when selection.selector = providers_action -> `Providers
    | Some selection -> `Picked selection
    | None -> `Cancelled in
  let rec providers last =
    let scopes = available_scopes ~registry () in
    let scopes = if List.exists (fun (candidate, _) -> candidate = initial_scope) scopes
      then scopes else (initial_scope, active) :: scopes in
    let groups = provider_groups ~active_id:active.id
      ~ready:(fun descriptor scope -> scope_ready ~registry descriptor scope) scopes in
    let options = List.map (fun entry ->
      provider_label ~active_id:active.id entry, entry) groups in
    let initial_selected = List.find_map (fun (label,
        ((descriptor : Pave.Provider_catalog.descriptor), _, _)) ->
      if descriptor.id = last then Some label else None) options in
    match Tui.choose screen ?initial_selected
        ~intro:["Step 1 of 2 · choose a provider, then its model.";
          "Ready providers first · type to search · Esc cancels."]
        ~title:"Models · provider" ~choices:(List.map fst options) with
    | None -> None
    | Some label ->
        (match List.assoc_opt label options with
         | None -> None
         | Some (descriptor, members, _) ->
             let scope = match members with
               | [ scope ] -> Some scope
               | members ->
                   let preferred = if descriptor.id = active.id
                     then Some initial_scope else None in
                   let options = List.map (fun scope ->
                     scope_choice_label descriptor scope, scope) members in
                   let initial_selected = Option.bind preferred (fun preferred ->
                     List.find_map (fun (label, scope) ->
                       if scope = preferred then Some label else None) options) in
                   Option.bind (Tui.choose screen ?initial_selected
                     ~intro:["This provider has several APIs or accounts.";
                       "Enter opens a fresh model listing · Esc returns."]
                     ~title:("Models · " ^ Tui.single_line descriptor.display_name ^
                       " API/account") ~choices:(List.map fst options))
                     (fun label -> List.assoc_opt label options) in
             match scope with
             | None -> providers descriptor.id
             | Some scope ->
                 (match open_models None scope descriptor with
                  | `Picked selection -> Some selection
                  | `Providers | `Cancelled -> providers descriptor.id)) in
  match initial_filter with
  | Some _ ->
      (match open_models initial_filter initial_scope active with
       | `Picked selection -> Some selection
       | `Providers -> providers active.id
       | `Cancelled -> None)
  | None -> providers active.id
