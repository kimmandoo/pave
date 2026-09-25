let configured = function Some value -> value | None -> "(automatic)"

let approval_tools = [
  "read_file", "read";
  "list_files", "read";
  "glob", "read";
  "search", "read";
  "grep", "read";
  "mobile_project", "read";
  "write_file", "write";
  "edit_file", "write";
  "run_command", "exec (per-command prompt remains mandatory)"
]

let open_view screen ~root =
  let save change =
    ignore (Pave.Settings.update_project ~root change);
    Tui.event screen "Project settings saved for the next launch; active turns are unchanged." in
  let mode_name = function
    | None -> "write (default)"
    | Some mode -> Pave.Approval.string_of_mode mode in
  let policy_name = function
    | None -> "inherit global mode"
    | Some policy -> Pave.Approval.string_of_policy policy in
  let rec loop () =
    let loaded = Pave.Settings.load ~root in
    let values = loaded.values in
    let provider = "Default provider: " ^ configured values.default_provider in
    let model = "Default model: " ^ configured values.default_model in
    let api = "Default API: " ^ configured values.default_api in
    let shell = "Disable shell tools: " ^ string_of_bool values.disable_shell in
    let turns = "Maximum model turns: " ^
      (match values.max_turns with Some count -> string_of_int count
       | None -> "20 (default)") in
    let approval_mode = "Approval mode: " ^ mode_name values.approval_mode in
    let tool_approval = "Per-tool approval overrides" in
    match Tui.choose screen ~title:"Project settings · Esc closes"
      ~choices:[provider; model; api; shell; turns; approval_mode; tool_approval] with
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
                 default_provider = Some id; default_model = None;
                 default_api = None }))
         else if choice = model then (
           let provider_id = Option.value ~default:"openai"
             values.default_provider in
           let descriptor = match Pave.Provider_catalog.find provider_id with
             | Some descriptor -> descriptor
             | None -> invalid_arg "unknown configured provider" in
           let selected_api = match values.default_api with
             | Some api -> Some api
             | None when Pave.Provider_catalog.route descriptor "" <> None ->
                 Some descriptor.default_route
             | None -> Tui.choose screen ~title:"Select API for default model"
                 ~choices:(List.map
                   (fun (route : Pave.Provider_catalog.route) -> route.name)
                   descriptor.routes) in
           match selected_api with
           | None -> ()
           | Some route_name ->
           match Model_picker.choose screen ~descriptor ~route_name
             ~title:"Default model · live IDs (type an ID if unavailable)"
             ~choices:[] () with
           | None -> ()
           | Some selector ->
               let descriptor, model, route = Pave.Interaction.resolve_model
                 ~current_route:route_name
                 ~current_provider:descriptor.id ~input:selector () in
               save (fun current -> { current with
                 default_provider = Some descriptor.id;
                 default_model = Some model; default_api = Some route.name }))
         else if choice = api then (
           match values.default_provider with
           | None -> Tui.alert screen "Choose a default provider first"
           | Some id ->
               let descriptor = match Pave.Provider_catalog.find id with
                 | Some descriptor -> descriptor
                 | None -> invalid_arg "unknown configured provider" in
               (match Tui.choose screen ~title:"Default API route"
                 ~choices:(List.map
                   (fun (route : Pave.Provider_catalog.route) -> route.name)
                   descriptor.routes) with
                | None -> ()
                | Some name -> save (fun current ->
                    { current with default_api = Some name })))
         else if choice = shell then
           save (fun current -> { current with
             disable_shell = not current.disable_shell })
         else if choice = turns then (
           let options = ["5"; "10"; "20"; "40"; "80"] in
           match Tui.choose screen ~title:"Maximum turns per prompt"
             ~choices:options with
           | None -> ()
           | Some count -> save (fun current -> { current with
               max_turns = Some (int_of_string count) }))
         else if choice = approval_mode then (
           let options = ["always-ask"; "write"; "yolo";
             "Inherit from user/default"] in
           match Tui.choose screen ~title:"Global tool approval mode"
             ~choices:options with
           | None -> ()
           | Some "Inherit from user/default" ->
               save (fun current -> { current with approval_mode = None })
           | Some selected ->
               (match Pave.Approval.mode_of_string selected with
                | None -> assert false
                | Some mode ->
                    save (fun current ->
                      { current with approval_mode = Some mode })))
         else if choice = tool_approval then (
           let choices = List.map (fun (name, tier) ->
             let policy = List.assoc_opt name values.tool_approval in
             Printf.sprintf "%s (%s): %s" name tier (policy_name policy), name)
             approval_tools in
           match Tui.choose screen ~title:"Per-tool approval overrides"
             ~choices:(List.map fst choices) with
           | None -> ()
           | Some selected ->
               let name = List.assoc selected choices in
               let current_policy = List.assoc_opt name values.tool_approval in
               let options = ["Inherit global mode"; "allow"; "prompt"; "deny"] in
               (match Tui.choose screen
                 ~title:(name ^ " · " ^ policy_name current_policy)
                 ~choices:options with
                | None -> ()
                | Some selected ->
                    let policy = match selected with
                      | "Inherit global mode" -> None
                      | other -> Pave.Approval.policy_of_string other in
                    save (fun current ->
                      let policies = List.remove_assoc name current.tool_approval in
                      { current with tool_approval = match policy with
                        | None -> policies
                        | Some value -> policies @ [name, value] }))));
        loop () in
  loop ()
