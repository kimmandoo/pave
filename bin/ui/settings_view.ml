let configured = function Some value -> value | None -> "(automatic)"

let open_view screen ~root =
  let save change =
    ignore (Pave.Settings.update_project ~root change);
    Tui.event screen "Project settings saved for the next launch; active turns are unchanged." in
  let rec loop () =
    let loaded = Pave.Settings.load ~root in
    let values = loaded.values in
    let provider = "Default provider: " ^ configured values.default_provider in
    let model = "Default model: " ^ configured values.default_model in
    let shell = "Disable shell tools: " ^ string_of_bool values.disable_shell in
    let turns = "Maximum model turns: " ^
      (match values.max_turns with Some count -> string_of_int count
       | None -> "20 (default)") in
    match Tui.choose screen ~title:"Project settings · Esc closes"
      ~choices:[provider; model; shell; turns] with
    | None -> ()
    | Some choice ->
        (if choice = provider then (
           let options = List.map
             (fun (entry : Pave.Provider_catalog.descriptor) ->
               entry.id ^ "  " ^ entry.display_name, entry.id)
             (Pave.Provider_catalog.all ()) in
           match Tui.choose screen ~title:"Default provider"
             ~choices:(List.map fst options) with
           | None -> ()
           | Some selected ->
               let id = List.assoc selected options in
               save (fun current -> { current with
                 default_provider = Some id; default_model = None }))
         else if choice = model then (
           let provider_id = Option.value ~default:"openai"
             values.default_provider in
           let descriptor = match Pave.Provider_catalog.find provider_id with
             | Some descriptor -> descriptor
             | None -> invalid_arg "unknown configured provider" in
           match Model_picker.choose screen ~descriptor
             ~title:"Default model · live IDs (type an ID if unavailable)"
             ~choices:[] () with
           | None -> ()
           | Some selector ->
               let descriptor, model, _ = Pave.Interaction.resolve_model
                 ~current_provider:descriptor.id ~input:selector in
               save (fun current -> { current with
                 default_provider = Some descriptor.id;
                 default_model = Some model }))
         else if choice = shell then
           save (fun current -> { current with
             disable_shell = not current.disable_shell })
         else if choice = turns then (
           let options = ["5"; "10"; "20"; "40"; "80"] in
           match Tui.choose screen ~title:"Maximum turns per prompt"
             ~choices:options with
           | None -> ()
           | Some count -> save (fun current -> { current with
               max_turns = Some (int_of_string count) })));
        loop () in
  loop ()
