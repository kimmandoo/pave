type result =
  | Skipped
  | Selected of Pave.Provider_catalog.descriptor * Pave.Model_identity.t *
      Pave.Provider_catalog.route * string option

let provider_choices registry =
  List.map (fun (entry : Pave.Provider_catalog.descriptor) ->
    entry.id ^ " · " ^ entry.display_name, entry)
    (Pave.Interaction.selectable_providers ~registry ())

let account_label (account : Pave.Oauth_store.account) =
  match account.credential.account_id with
  | Some id -> "Account ID: " ^ Printf.sprintf "%S" id
  | None -> "Local sign-in ID: " ^ Printf.sprintf "%S" account.selection_id

let select_saved_account screen provider accounts =
  match accounts with
  | [] -> None
  | [account] -> Some (Some account.Pave.Oauth_store.selection_id)
  | accounts ->
      let options = List.map (fun account ->
        account_label account, Some account.Pave.Oauth_store.selection_id)
        accounts in
      Option.bind
        (Tui.choose screen
          ~intro:["Credentials remain private and are selected by provider account ID or Pave-local sign-in ID.";
            "Escape returns to provider access choices."]
          ~title:("SETUP · " ^ provider ^ " saved account")
          ~choices:(List.map fst options))
        (fun label -> List.assoc_opt label options)

let run screen ~registry =
  let skip = Skipped in
  let rec welcome () =
    match Tui.choose screen
      ~intro:["SETUP = choose a default provider and model.";
        "Sign in here only if your provider requires it.";
        "Nothing runs until you send a prompt."]
      ~title:"SETUP · Save your default"
      ~choices:["Start · provider → access → model"; "Skip for now"] with
    | Some "Start · provider → access → model" -> provider ()
    | _ -> skip
  and provider () =
    let choices = provider_choices registry in
    match Tui.choose screen
      ~intro:["01 / 03  ·  PROVIDER";
        "Choose a service or a local model host.";
        "Use letters to filter, arrows to move, Enter to choose."]
      ~title:"SETUP · Select provider"
      ~choices:(List.map fst choices @ ["Back · welcome"; "Skip setup"]) with
    | None | Some "Skip setup" -> skip
    | Some "Back · welcome" -> welcome ()
    | Some choice -> authentication (List.assoc choice choices)
  and authentication ?initial_status
      (descriptor : Pave.Provider_catalog.descriptor) =
    if Pave.Provider_catalog.custom_provider registry descriptor.id <> None then
      model descriptor None
    else
    let key = descriptor.api_key_env in
    let oauth = descriptor.oauth <> None in
    let saved_accounts, saved_accounts_status = if oauth then
      (try
         Pave.Oauth_store.accounts
           ~path:(Pave.Oauth_store.default_path ()) ~provider:descriptor.id,
         None
       with Pave.Oauth_store.Storage_error message ->
         [], Some ("Saved sign-ins unavailable: " ^ message))
      else [], None in
    let chooser_status = match initial_status with
      | Some _ -> initial_status
      | None -> saved_accounts_status in
    let saved_oauth = saved_accounts <> [] in
    let key_is_set = match key with
      | Some env -> (match Sys.getenv_opt env with
          | Some value -> value <> ""
          | None -> false)
      | None -> false in
    let key_label = Option.map (fun env ->
      env ^ (if key_is_set then " (set; takes precedence over saved grants)"
        else " (not set)")) key in
    let saved_label =
      if key_is_set then "Continue with environment key (saved grants inactive)"
      else if List.length saved_accounts > 1 then "Choose saved sign-in account"
      else "Continue with saved sign-in" in
    let login_label = "Sign in (browser or device code)" in
    let azure_endpoint_set = match Sys.getenv_opt "AZURE_OPENAI_ENDPOINT" with
      | Some value -> value <> ""
      | None -> false in
    let azure_cli_label = "Continue with Azure CLI identity" in
    let azure_cli_available =
      descriptor.id = "azure" && not key_is_set && azure_endpoint_set in
    let intro = if descriptor.id = "azure" then [
      "Configure AZURE_OPENAI_ENDPOINT for the Azure OpenAI or Foundry resource.";
      "When no API key is set, Azure CLI az login supplies the local Entra identity.";
      "Deployment listing uses the current subscription and requires management read access."]
      else [
        "A key or saved sign-in lets this provider receive prompts.";
        "An environment API key takes precedence over every saved grant.";
        "Use /setup for account access and saved defaults."] in
    let choices =
      (if key_is_set then Option.to_list key_label else []) @
      (if saved_oauth then [saved_label] else []) @
      (if not key_is_set then Option.to_list key_label else []) @
      (if azure_cli_available then [azure_cli_label] else []) @
      (if oauth then [login_label] else []) @
      ["Back · providers"; "Skip setup"] in
    let local = List.exists (fun (route : Pave.Provider_catalog.route) ->
      route.wire = Pave.Provider.Local_chat) descriptor.routes in
    if (key = None || (local && not key_is_set)) && not oauth then
      model descriptor None
    else match Tui.choose screen ?initial_status:chooser_status
        ~intro ~title:"SETUP · Connect provider" ~choices with
    | None | Some "Skip setup" -> skip
    | Some "Back · providers" -> provider ()
    | Some choice when Some choice = key_label ->
        let env = Option.get key in
        (match Sys.getenv_opt env with
         | Some value when value <> "" -> model descriptor None
         | _ -> key_instruction descriptor env)
    | Some choice when choice = azure_cli_label && azure_cli_available ->
        model descriptor None
    | Some choice when choice = saved_label ->
        if key_is_set then model descriptor None
        else
          (match select_saved_account screen descriptor.id saved_accounts with
           | None -> authentication descriptor
           | Some account_id -> model ~account_id descriptor None)
    | Some choice when choice = login_label ->
        (try
           Tui.suspend screen (fun () ->
             ignore (Cli_auth.handle_action ~login:descriptor.id
               ~login_manual:"" ~logout:"" ()));
           if key_is_set then (
             Tui.alert screen
               "The configured environment API key takes precedence over this saved grant.";
             model descriptor None)
           else
             let accounts = Pave.Oauth_store.accounts
               ~path:(Pave.Oauth_store.default_path ()) ~provider:descriptor.id in
             (match select_saved_account screen descriptor.id accounts with
              | None -> authentication
                  ~initial_status:"Sign-in completed, but no saved account is available."
                  descriptor
              | Some account_id -> model ~account_id descriptor None)
         with
         | Sys.Break ->
             authentication
               ~initial_status:"Sign-in cancelled; choose an account action."
               descriptor
         | Pave.Oauth_device.OAuth_error
             ("OAuth device authorization timed out"
             | "OAuth device authorization expired"
             | "Kilo device authorization expired") ->
             authentication ~initial_status:
               "Device authorization expired or timed out; no credential was selected. Retry or choose another method."
               descriptor
         | Pave.Oauth_device.OAuth_error
             ("OAuth device authorization denied"
             | "Kilo device authorization denied") ->
             authentication ~initial_status:
               "Device authorization was denied; no credential was selected."
               descriptor
         | Pave.Oauth_flow.OAuth_error _
         | Pave.Oauth_device.OAuth_error _
         | Pave.Oauth_store.Storage_error _
         | Unix.Unix_error _
         | Sys_error _ | Failure _ | Invalid_argument _ ->
             authentication ~initial_status:
               "Sign-in failed; no credential was selected. Retry or choose another method."
               descriptor)
    | Some _ -> authentication descriptor
  and key_instruction descriptor env =
    let intro = if descriptor.id = "azure" then [
      "Configure AZURE_OPENAI_ENDPOINT and sign in with Azure CLI az login.";
      "AZURE_OPENAI_API_KEY is optional; Azure CLI identity is used when it is absent.";
      "Deployment discovery needs current-subscription management read access."]
      else [
        "02 / 03  ·  KEY NOT SET";
        "Set this variable in your shell; Pave never stores a typed key.";
        "You can save a model now, but prompts will need the key."] in
    match Tui.choose screen ~intro
      ~title:("SETUP · " ^ env ^ " is missing")
      ~choices:["Choose model without key"; "Back · authentication";
        "Skip setup"] with
    | Some "Choose model without key" -> model descriptor (Some env)
    | Some "Back · authentication" -> authentication descriptor
    | _ -> skip
  and model ?(account_id=None)
      (descriptor : Pave.Provider_catalog.descriptor) missing_key =
    let azure_endpoint_set = match Sys.getenv_opt "AZURE_OPENAI_ENDPOINT" with
      | Some value -> value <> ""
      | None -> false in
    if descriptor.id = "azure" && not azure_endpoint_set then (
      Tui.alert screen
        "Set AZURE_OPENAI_ENDPOINT to a documented Azure OpenAI or Foundry resource before selecting its deployment.";
      authentication descriptor
    ) else
    let local_without_key = List.exists
      (fun (route : Pave.Provider_catalog.route) ->
        route.wire = Pave.Provider.Local_chat) descriptor.routes &&
      Option.value ~default:"" (Option.bind descriptor.api_key_env Sys.getenv_opt) = "" in
    let back = if local_without_key then "Back · providers"
      else "Back · authentication" in
    let choices = [back; "Skip setup"] in
    let selected_api =
      if Pave.Provider_catalog.route descriptor "" <> None then
        Some descriptor.default_route
      else Tui.choose screen ~title:"SETUP · Select API route"
        ~intro:["This provider serves different wire APIs.";
          "Choose the route documented for your model."]
        ~choices:(List.map
          (fun (route : Pave.Provider_catalog.route) -> route.name)
          descriptor.routes) in
    match selected_api with
    | None -> if local_without_key then provider () else authentication descriptor
    | Some route_name ->
    let missing_key = match Pave.Provider_catalog.custom_route registry
        ~provider:descriptor.id ~route:route_name with
      | Some { auth = Pave.Custom_provider.Api_key_env env; _ }
        when Option.value ~default:"" (Sys.getenv_opt env) = "" -> Some env
      | Some _ | None -> missing_key in
    match Model_picker.choose ~registry screen ~descriptor ~route_name
      ?account_id ~plain:choices
      ~intro:["03 / 03  ·  MODEL";
        "Use arrows and Enter to choose an available model.";
        "Type an ID only if the model you need is not listed."]
      ~title:"SETUP · Select model"
      ~choices () with
    | None | Some "Skip setup" -> skip
    | Some choice when choice = back ->
        if local_without_key then provider () else authentication descriptor
    | Some choice ->
        (try
           let selected, identity, route = Pave.Interaction.resolve_model
             ~registry ~current_route:route_name
             ?current_account_id:account_id
             ~current_provider:descriptor.id ~input:choice () in
           if selected.id <> descriptor.id then
             invalid_arg "choose a model from the selected provider";
           finish selected identity route missing_key account_id
         with (Invalid_argument _ | Failure _) as exn ->
           Tui.alert screen ("Model unavailable: " ^ Printexc.to_string exn);
           model ~account_id descriptor missing_key)
  and finish descriptor identity (route : Pave.Provider_catalog.route)
      missing_key account_id =
    let label = Pave.Model_identity.selector identity in
    let title = match missing_key with
      | Some env -> "SETUP · Set " ^ env ^ " before prompts"
      | None -> "SETUP · Confirm your model" in
    match Tui.choose screen
      ~intro:["READY  ·  REVIEW";
        "Save this provider + API + model as your user default.";
        "It does not change project policy or your shell keys."]
      ~title
      ~choices:["Save " ^ label; "Back · models"; "Skip setup"] with
    | Some selected when selected = "Save " ^ label ->
        Selected (descriptor, identity, route, missing_key)
    | Some "Back · models" -> model ~account_id descriptor missing_key
    | _ -> skip in
  welcome ()
