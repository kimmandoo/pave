let configured = function Some value -> value | None -> "(automatic)"

let approval_tools = [
  "read_file", "read";
  "workspace_snapshot", "read";
  "list_files", "read";
  "glob", "read";
  "search", "read";
  "grep", "read";
  "mobile_project", "read";
  "write_file", "write";
  "edit_file", "write";
  "apply_edits", "write";
  "ast_edit", "write (dry-run is read)";
  "run_command", "shell command"
]

let account_label id = "Account ID: " ^ Printf.sprintf "%S" id

let approval_mode_label = function
  | Pave.Approval.Ask_writes -> "Ask before writes and commands; reads automatic"
  | Pave.Approval.Ask_exec -> "Allow reads and writes; ask before commands"
  | Pave.Approval.Auto_all -> "Automatic where permitted; command policies and safety gates still apply"

let approval_policy_label = function
  | Pave.Approval.Allow -> "Allow without routine prompts; safety gates still apply"
  | Pave.Approval.Prompt -> "Ask before each use"
  | Pave.Approval.Deny -> "Block this tool"

(* Read each scope separately for selection state; merged values describe effects,
   not whether the project has an override. Match Settings.load's safeguards. *)
let approval_scope ~user directory =
  try
    if not user && (Unix.lstat directory).Unix.st_kind <> Unix.S_DIR then
      invalid_arg "project .pave must be a real directory";
    Pave.Settings.read ~allow_custom:user (Filename.concat directory "settings.json")
  with
  | Unix.Unix_error _ | Sys_error _ | Invalid_argument _ | Yojson.Json_error _ ->
      Pave.Settings.empty

let approval_safety =
  "Commands need approval unless a command rule allows them · unsandboxed · blocks win"

let open_view screen ~root ~registry =
  let save change =
    ignore (Pave.Settings.update_project ~root change);
    Tui.event screen "Project settings saved for the next launch; active turns are unchanged." in
  let mode_name mode = approval_mode_label
    (Option.value ~default:Pave.Approval.Ask_exec mode) in
  let project_scope () =
    approval_scope ~user:false (Filename.concat root ".pave") in
  let user_scope () =
    let home, _ = Pave.Settings.config_home () in
    approval_scope ~user:true (Filename.concat home "pave") in
  let marked_options current options =
    List.map (fun (label, value) ->
      (if value = current then "Current project · " ^ label else label),
      value) options in
  let current_label current options =
    fst (List.find (fun (_, value) -> value = current) options) in
  let rec loop ?initial_selected () =
    let loaded = Pave.Settings.load ~root in
    let values = loaded.values in
    let provider = "Default provider: " ^ configured values.default_provider in
    let model = "Default model: " ^ configured values.default_model in
    let api = "Default API: " ^ configured values.default_api in
    let account = "Default account scope: " ^
      configured values.default_account_id in
    let shell = "Disable shell tools: " ^ string_of_bool values.disable_shell in
    let turns = "Maximum model turns: " ^
      (match values.max_turns with Some count -> string_of_int count
       | None -> "20 (default)") in
    let approval_mode = "Tool approval default: " ^ mode_name values.approval_mode in
    let tool_approval = "Per-tool approval defaults" in
    let rows = ["provider", provider; "model", model; "api", api;
      "account", account; "shell", shell; "turns", turns;
      "approval", approval_mode; "tools", tool_approval] in
    let selection = Option.bind initial_selected (fun key -> List.assoc_opt key rows) in
    match Tui.choose screen ~title:"Project settings" ?initial_selected:selection
      ~intro:["Saved defaults apply on the next launch, not to active turns.";
        "This is not a one-time tool approval. Escape closes settings."]
      ~choices:(List.map snd rows) with
    | None -> ()
    | Some choice ->
        (if choice = provider then (
           let options = List.map
             (fun (entry : Pave.Provider_catalog.descriptor) ->
               entry.id ^ "  " ^ entry.display_name, entry.id)
             (Pave.Provider_catalog.all ~registry ()) in
           match Tui.choose screen ~title:"Default provider"
             ~choices:(List.map fst options) with
           | None -> ()
           | Some selected ->
               let id = List.assoc selected options in
               save (fun current -> { current with
                 default_provider = Some id; default_model = None;
                 default_api = None; default_account_id = None }))
         else if choice = model then (
           let provider_id = Option.value ~default:"openai"
             values.default_provider in
           let descriptor = match Pave.Provider_catalog.find ~registry provider_id with
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
           match Model_picker.choose ~registry screen ~descriptor ~route_name
             ?account_id:values.default_account_id
             ~title:"Default model · available on this route" () with
           | None -> ()
           | Some selection ->
               let selector = selection.selector in
               let descriptor, identity, route = Pave.Interaction.resolve_model
                 ~registry ~current_route:route_name
                 ?current_account_id:values.default_account_id
                 ~current_provider:descriptor.id ~input:selector () in
               save (fun current -> { current with
                 default_provider = Some descriptor.id;
                 default_model = Some identity.upstream_id;
                 default_api = Some route.name;
                 default_account_id = identity.account_id }))
         else if choice = api then (
           match values.default_provider with
           | None -> Tui.alert screen "Choose a default provider first"
           | Some id ->
               let descriptor = match Pave.Provider_catalog.find ~registry id with
                 | Some descriptor -> descriptor
                 | None -> invalid_arg "unknown configured provider" in
               (match Tui.choose screen ~title:"Default API route"
                 ~choices:(List.map
                   (fun (route : Pave.Provider_catalog.route) -> route.name)
                   descriptor.routes) with
                | None -> ()
                | Some name -> save (fun current ->
                    { current with default_api = Some name;
                      default_model = None; default_account_id = None })))
         else if choice = account then (
           match values.default_provider with
           | None -> Tui.alert screen "Choose a default provider first"
           | Some provider_id ->
               (match Pave.Provider_catalog.find ~registry provider_id with
                | None -> Tui.alert screen "The configured provider is unavailable"
                | Some descriptor when descriptor.oauth = None ->
                    Tui.alert screen
                      "This provider has no saved OAuth account; configure its API key in your shell."
                | Some _ ->
                    let accounts = try
                      Pave.Oauth_store.accounts
                        ~path:(Pave.Oauth_store.default_path ())
                        ~provider:provider_id
                    with Pave.Oauth_store.Storage_error message ->
                      Tui.alert screen ("Saved sign-ins unavailable: " ^ message);
                      [] in
                    if accounts = [] then
                      Tui.alert screen
                        "No saved OAuth accounts; use /setup to sign in."
                    else
                      let options = ("Use automatic selection", None) ::
                        List.map (fun account ->
                          let label = match account.Pave.Oauth_store.credential.account_id with
                            | Some id -> account_label id
                            | None -> "Local sign-in ID: " ^
                                Printf.sprintf "%S" account.selection_id in
                          label, Some account.selection_id) accounts in
                      (match Tui.choose screen ~title:"Default saved account"
                        ~choices:(List.map fst options) with
                       | None -> ()
                       | Some selected ->
                           (match List.assoc_opt selected options with
                            | Some account_id ->
                                save (fun current -> { current with
                                  default_account_id = account_id })
                            | None -> assert false))))
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
           let project = project_scope () and user = user_scope () in
           let options = marked_options project.approval_mode [
             "Use user/default setting: " ^ mode_name user.approval_mode, None;
             approval_mode_label Pave.Approval.Ask_writes, Some Pave.Approval.Ask_writes;
             approval_mode_label Pave.Approval.Ask_exec, Some Pave.Approval.Ask_exec;
             approval_mode_label Pave.Approval.Auto_all, Some Pave.Approval.Auto_all
           ] in
           match Tui.choose screen ~title:"Tool approval · next-launch project default"
            ~intro:["Effective default: " ^ mode_name values.approval_mode;
              approval_safety]
             ~initial_selected:(current_label project.approval_mode options)
             ~choices:(List.map fst options) with
           | None -> ()
           | Some selected ->
               let mode = List.assoc selected options in
               if mode <> project.approval_mode then
                 save (fun current -> { current with approval_mode = mode }))
         else if choice = tool_approval then (
           let rec tools ?selected_tool () =
             let values = (Pave.Settings.load ~root).values in
             let project = project_scope () and user = user_scope () in
             let effective name =
               match List.assoc_opt name values.tool_approval with
               | Some policy -> approval_policy_label policy
               | None -> "Global default: " ^ mode_name values.approval_mode in
             let choices = List.map (fun (name, tier) ->
               let origin = if List.mem_assoc name project.tool_approval
                 then "project override" else "inherited" in
               Printf.sprintf "%s (%s) · %s · %s" name tier origin (effective name),
               name) approval_tools in
             let initial_selected = Option.bind selected_tool (fun name ->
               List.find_opt (fun (_, tool) -> tool = name) choices |> Option.map fst) in
             match Tui.choose screen ~title:"Per-tool defaults · next launch"
              ~intro:["Choose a tool to edit its next-launch default, not approve one use.";
                approval_safety]
               ?initial_selected ~choices:(List.map fst choices) with
             | None -> ()
             | Some selected ->
                 let name = List.assoc selected choices in
                 let current_policy = List.assoc_opt name project.tool_approval in
                 let inherited = match List.assoc_opt name user.tool_approval with
                   | Some policy -> "User policy: " ^ approval_policy_label policy
                   | None -> "Global default: " ^ mode_name values.approval_mode in
                 let allow_label = if name = "run_command" then
                   "Allow subject to command policies and safety gates (unsandboxed)"
                   else approval_policy_label Pave.Approval.Allow in
                 let options = marked_options current_policy [
                   "Use inherited setting · " ^ inherited, None;
                   allow_label, Some Pave.Approval.Allow;
                   approval_policy_label Pave.Approval.Prompt, Some Pave.Approval.Prompt;
                   approval_policy_label Pave.Approval.Deny, Some Pave.Approval.Deny
                 ] in
                 (match Tui.choose screen ~title:(name ^ " · next-launch default")
                  ~intro:["Effective saved policy: " ^ effective name; approval_safety]
                   ~initial_selected:(current_label current_policy options)
                   ~choices:(List.map fst options) with
                  | None -> ()
                  | Some selected ->
                      let policy = List.assoc selected options in
                      if policy <> current_policy then
                        save (fun current ->
                          let policies = List.remove_assoc name current.tool_approval in
                          { current with tool_approval = match policy with
                            | None -> policies
                            | Some value -> policies @ [name, value] }));
                 tools ~selected_tool:name () in
           tools ()));
        let key, _ = List.find (fun (_, label) -> label = choice) rows in
        loop ~initial_selected:key () in
  loop ()
