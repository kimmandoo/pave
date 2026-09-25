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
      ~intro:["◆  YOUR WORKSPACE, YOUR MODEL";
        "Choose a provider, then the model you want to use.";
        "Nothing runs until you send a prompt."]
      ~title:"SETUP · Welcome to Pave"
      ~choices:["Enter · provider + model"; "Esc · skip for now"] with
    | Some "Enter · provider + model" -> provider ()
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
    let key_label = Option.map (fun env ->
      env ^ (match Sys.getenv_opt env with
        | Some value when value <> "" -> " (set)"
        | _ -> " (not set)")) key in
    let saved_label = "Use saved OAuth sign-in" in
    let login_label = "Sign in via OAuth" in
    let choices =
      (match key_label with Some label -> [label] | None -> []) @
      (if saved_oauth then [saved_label] else []) @
      (if oauth then [login_label] else []) @
      ["Back · providers"; "Skip setup"] in
    if key = None && not oauth then model descriptor None
    else match Tui.choose screen
      ~intro:["02 / 03  ·  ACCESS";
        "Use an existing key or sign in; secrets stay out of chat.";
        "A missing key can be configured in your shell later."]
      ~title:"SETUP · Choose authentication"
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
      ~intro:["02 / 03  ·  KEY REQUIRED";
        "Set this environment variable in your shell before prompts.";
        "Skip only if you want to set the key later."]
      ~title:("SETUP · Set " ^ env ^ " in shell")
      ~choices:["Skip key · choose model"; "Back · authentication";
        "Skip setup"] with
    | Some "Skip key · choose model" -> model descriptor (Some env)
    | Some "Back · authentication" -> authentication descriptor
    | _ -> skip
  and model (descriptor : Pave.Provider_catalog.descriptor) missing_key =
    let models = Pave.Provider_catalog.known_models descriptor in
    let models = match descriptor.default_model with
      | Some default when not (List.mem default models) -> default :: models
      | _ -> models in
    let choices = List.map (fun id -> descriptor.id ^ "/" ^ id) models in
    let instructions = "Type " ^ descriptor.id ^ "/MODEL_ID" in
    match Model_picker.choose screen ~descriptor
      ~plain:[instructions; "Back · authentication"; "Skip setup"]
      ~intro:["03 / 03  ·  MODEL";
        "Filter the list or type PROVIDER/MODEL_ID.";
        "Only routable model IDs can become your default."]
      ~title:"SETUP · Choose model (type ID)"
      ~choices:(choices @ [instructions; "Back · authentication"; "Skip setup"]) () with
    | None | Some "Skip setup" -> skip
    | Some "Back · authentication" -> authentication descriptor
    | Some choice when choice = instructions ->
        Tui.alert screen ("Type " ^ descriptor.id ^
          "/MODEL_ID in the chooser, then Enter");
        model descriptor missing_key
    | Some choice ->
        (try
           let selected, id, route = Pave.Interaction.resolve_model
             ~current_provider:descriptor.id ~input:choice in
           if selected.id <> descriptor.id then
             invalid_arg "choose a model from the selected provider";
           finish selected id route missing_key
         with (Invalid_argument _ | Failure _) as exn ->
           Tui.alert screen ("Model unavailable: " ^ Printexc.to_string exn);
           model descriptor missing_key)
  and finish descriptor id route missing_key =
    let label = descriptor.id ^ "/" ^ id in
    let title = match missing_key with
      | Some env -> "SETUP · Set " ^ env ^ " before prompts"
      | None -> "SETUP · Confirm your model" in
    match Tui.choose screen
      ~intro:["READY  ·  REVIEW";
        "This default is stored in your user config.";
        "Project policy and explicit flags still take precedence."]
      ~title
      ~choices:["Save " ^ label; "Back · models"; "Skip setup"] with
    | Some selected when selected = "Save " ^ label ->
        Selected (descriptor, id, route, missing_key)
    | Some "Back · models" -> model descriptor missing_key
    | _ -> skip in
  welcome ()
