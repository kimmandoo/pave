type result =
  | Skipped
  | Selected of Pave.Provider_catalog.descriptor * string *
      Pave.Provider_catalog.route * string option

let provider_choices () =
  List.map (fun (entry : Pave.Provider_catalog.descriptor) ->
    entry.id ^ " · " ^ entry.display_name, entry)
    (Pave.Interaction.selectable_providers ())

let run screen =
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
    let choices = provider_choices () in
    match Tui.choose screen
      ~intro:["01 / 03  ·  PROVIDER";
        "Choose a service or a local model host.";
        "Use letters to filter, arrows to move, Enter to choose."]
      ~title:"SETUP · Select provider"
      ~choices:(List.map fst choices @ ["Back · welcome"; "Skip setup"]) with
    | None | Some "Skip setup" -> skip
    | Some "Back · welcome" -> welcome ()
    | Some choice -> authentication (List.assoc choice choices)
  and authentication (descriptor : Pave.Provider_catalog.descriptor) =
    let key = descriptor.api_key_env in
    let oauth = descriptor.oauth <> None in
    let saved_oauth = oauth &&
      (try Pave.Oauth_store.get ~path:(Pave.Oauth_store.default_path ())
        ~provider:descriptor.id <> None
       with Pave.Oauth_store.Storage_error _ -> false) in
    let key_is_set = match key with
      | Some env -> (match Sys.getenv_opt env with
          | Some value -> value <> ""
          | None -> false)
      | None -> false in
    let key_label = Option.map (fun env ->
      env ^ (if key_is_set then " (set)" else " (not set)")) key in
    let saved_label = "Continue with saved sign-in" in
    let login_label = "Sign in (browser or device code)" in
    let choices =
      (if key_is_set then Option.to_list key_label else []) @
      (if saved_oauth then [saved_label] else []) @
      (if not key_is_set then Option.to_list key_label else []) @
      (if oauth then [login_label] else []) @
      ["Back · providers"; "Skip setup"] in
    let local = List.exists (fun (route : Pave.Provider_catalog.route) ->
      route.wire = Pave.Provider.Local_chat) descriptor.routes in
    if (key = None || (local && not key_is_set)) && not oauth then
      model descriptor None
    else match Tui.choose screen
      ~intro:["02 / 03  ·  ACCESS";
        "A key or saved sign-in lets this provider receive prompts.";
        "Use /setup for account access and saved defaults."]
      ~title:"SETUP · Connect provider"
      ~choices with
    | None | Some "Skip setup" -> skip
    | Some "Back · providers" -> provider ()
    | Some choice when Some choice = key_label ->
        let env = Option.get key in
        (match Sys.getenv_opt env with
         | Some value when value <> "" -> model descriptor None
         | _ -> key_instruction descriptor env)
    | Some choice when choice = saved_label -> model descriptor None
    | Some choice when choice = login_label ->
        (try
           Tui.suspend screen (fun () ->
             ignore (Cli_auth.handle_action ~login:descriptor.id
               ~login_manual:"" ~logout:""));
           model descriptor None
         with
         | Sys.Break ->
             Tui.alert screen "Sign-in cancelled; choose an account action.";
             authentication descriptor
         | Pave.Oauth_flow.OAuth_error _
         | Pave.Oauth_device.OAuth_error _
         | Pave.Oauth_store.Storage_error _
         | Unix.Unix_error _
         | Sys_error _ | Failure _ | Invalid_argument _ ->
             Tui.alert screen "Sign-in failed; retry or choose another method.";
             authentication descriptor)
    | Some _ -> authentication descriptor
  and key_instruction descriptor env =
    match Tui.choose screen
      ~intro:["02 / 03  ·  KEY NOT SET";
        "Set this variable in your shell; Pave never stores a typed key.";
        "You can save a model now, but prompts will need the key."]
      ~title:("SETUP · " ^ env ^ " is missing")
      ~choices:["Choose model without key"; "Back · authentication";
        "Skip setup"] with
    | Some "Choose model without key" -> model descriptor (Some env)
    | Some "Back · authentication" -> authentication descriptor
    | _ -> skip
  and model (descriptor : Pave.Provider_catalog.descriptor) missing_key =
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
    match Model_picker.choose screen ~descriptor
      ~route_name ~plain:choices
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
           let selected, id, route = Pave.Interaction.resolve_model
             ~current_route:route_name ~current_provider:descriptor.id
             ~input:choice () in
           if selected.id <> descriptor.id then
             invalid_arg "choose a model from the selected provider";
           finish selected id route missing_key
         with (Invalid_argument _ | Failure _) as exn ->
           Tui.alert screen ("Model unavailable: " ^ Printexc.to_string exn);
           model descriptor missing_key)
  and finish descriptor id (route : Pave.Provider_catalog.route) missing_key =
    let label = descriptor.id ^ "@" ^ route.name ^ "/" ^ id in
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
        Selected (descriptor, id, route, missing_key)
    | Some "Back · models" -> model descriptor missing_key
    | _ -> skip in
  welcome ()
