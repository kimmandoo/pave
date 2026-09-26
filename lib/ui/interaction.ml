type command =
  | Model of string option
  | Quit
  | Cancel
  | Help
  | Settings
  | Setup
  | New
  | Resume of string option
  | Clear
  | Fresh
  | Rename of string
  | Label of string option
  | Pin
  | Approval of string option
  | Thinking of string option
  | Tool_toggle of { name : string; enabled : bool }
  | Attach of string option
  | Compact
  | Retry
  | Tree
  | Branch of string
  | Fork of string option
  | Tools of string option
  | Context
  | Usage
  | Hotkeys
  | Entries
  | Queue_prompt of string
  | Prompt of string
  | Unknown of string

type action =
  | A_model | A_settings | A_setup | A_new | A_resume | A_clear | A_fresh
  | A_rename | A_label | A_pin | A_approval | A_thinking | A_tool | A_attach
  | A_cancel | A_queue | A_entries | A_tree | A_tools | A_context | A_usage
  | A_hotkeys | A_branch | A_fork | A_compact | A_retry | A_help | A_quit

type shortcut = { name : string; usage : string; summary : string; action : action }

let commands = [
  { name = "/model"; usage = "[PROVIDER[@API]/MODEL]"; summary = "Switch model for this conversation"; action = A_model };
  { name = "/settings"; usage = ""; summary = "View or edit project defaults"; action = A_settings };
  { name = "/setup"; usage = ""; summary = "Connect and save your user default model"; action = A_setup };
  { name = "/new"; usage = ""; summary = "Start a private saved session"; action = A_new };
  { name = "/resume"; usage = "[ID|TITLE|PATH]"; summary = "Search or reopen saved sessions"; action = A_resume };
  { name = "/clear"; usage = ""; summary = "Reset active context without deleting journal history"; action = A_clear };
  { name = "/fresh"; usage = ""; summary = "Rebuild the local provider agent from saved context"; action = A_fresh };
  { name = "/rename"; usage = "TITLE"; summary = "Set a durable session title"; action = A_rename };
  { name = "/label"; usage = "[TEXT]"; summary = "Set or clear a label on the selected entry"; action = A_label };
  { name = "/pin"; usage = ""; summary = "Toggle this session in the pinned resume list"; action = A_pin };
  { name = "/approval"; usage = "[always-ask|write|yolo]"; summary = "Show or set this branch's approval mode"; action = A_approval };
  { name = "/thinking"; usage = "[LEVEL|default]"; summary = "Store branch-local thinking-level metadata; provider defaults remain unchanged"; action = A_thinking };
  { name = "/tool"; usage = "enable|disable NAME"; summary = "Set branch-local tool availability"; action = A_tool };
  { name = "/attach"; usage = "PATH|clear"; summary = "Attach an image to the next prompt"; action = A_attach };
  { name = "/queue"; usage = "MESSAGE"; summary = "Queue a follow-up without interrupting the active turn"; action = A_queue };
  { name = "/cancel"; usage = ""; summary = "Cancel the active turn"; action = A_cancel };
  { name = "/retry"; usage = ""; summary = "Retry the last turn only if no tools ran"; action = A_retry };
  { name = "/tools"; usage = "[NAME]"; summary = "List or inspect enabled tools"; action = A_tools };
  { name = "/context"; usage = ""; summary = "Inspect active model and saved context"; action = A_context };
  { name = "/usage"; usage = ""; summary = "Inspect reported token usage by model"; action = A_usage };
  { name = "/hotkeys"; usage = ""; summary = "Show interactive terminal shortcuts"; action = A_hotkeys };
  { name = "/entries"; usage = ""; summary = "List journal entries"; action = A_entries };
  { name = "/tree"; usage = ""; summary = "Search journal ancestry and branch"; action = A_tree };
  { name = "/branch"; usage = "ID"; summary = "Continue from an earlier entry"; action = A_branch };
  { name = "/fork"; usage = "[PATH]"; summary = "Fork the selected journal branch into a private session"; action = A_fork };
  { name = "/compact"; usage = ""; summary = "Summarize older turns"; action = A_compact };
  { name = "/help"; usage = ""; summary = "Show commands and keys"; action = A_help };
  { name = "/quit"; usage = ""; summary = "Exit Pave"; action = A_quit };
]

let suggestions prefix =
  if not (String.starts_with ~prefix:"/" prefix) then []
  else List.filter (fun item -> String.starts_with ~prefix item.name) commands

let help () =
  List.map (fun item ->
    item.name ^ (if item.usage = "" then "" else " " ^ item.usage) ^
    " · " ^ item.summary) commands

let invalid_argument message = invalid_arg ("interaction: " ^ message)

let is_whitespace_or_control char =
  let code = Char.code char in
  code <= 32 || code = 127

let require_single_argument command argument =
  let argument = String.trim argument in
  if argument = "" then invalid_argument (command ^ " requires a nonempty argument");
  if String.exists is_whitespace_or_control argument then
    invalid_argument (command ^ " accepts only one argument");
  argument

let require_path command argument =
  let argument = String.trim argument in
  if argument = "" || String.exists (fun char ->
    let code = Char.code char in
    (code < 32 && char <> ' ') || code = 127) argument then
    invalid_argument (command ^ " requires a readable path");
  argument

let require_text command argument =
  if String.trim argument = "" || String.exists (fun char ->
    let code = Char.code char in code < 32 || code = 127) argument then
    invalid_argument (command ^ " requires nonempty single-line text");
  argument
let parse line =
  let line = String.trim line in
  if not (String.starts_with ~prefix:"/" line) then Prompt line
  else
    let offset = ref 0 in
    while !offset < String.length line &&
      not (is_whitespace_or_control line.[!offset]) do incr offset done;
    let name = String.sub line 0 !offset in
    let argument = if !offset = String.length line then None
      else if line.[!offset] <> ' ' then
        invalid_argument (name ^ " requires a space before its argument")
      else
        let text = String.trim (String.sub line (!offset + 1)
          (String.length line - !offset - 1)) in
        if text = "" then None else Some text in
    let single command = function
      | None -> None
      | Some text -> Some (require_single_argument command text) in
    let no_args () = if argument <> None then
      invalid_argument (name ^ " takes no arguments") in
    let action = match List.find_opt (fun item -> item.name = name) commands with
      | Some item -> Some item.action
      | None when name = "/exit" -> Some A_quit
      | None -> None in
    match action with
    | Some A_model -> Model (single name argument)
    | Some A_quit -> no_args (); Quit
    | Some A_cancel -> no_args (); Cancel
    | Some A_help -> no_args (); Help
    | Some A_settings -> no_args (); Settings
    | Some A_setup -> no_args (); Setup
    | Some A_new -> no_args (); New
    | Some A_resume -> Resume (Option.map (require_path name) argument)
    | Some A_clear -> no_args (); Clear
    | Some A_fresh -> no_args (); Fresh
    | Some A_rename ->
        Rename (require_text name (Option.value ~default:"" argument))
    | Some A_label -> Label (Option.map (require_text name) argument)
    | Some A_pin -> no_args (); Pin
    | Some A_approval -> Approval (single name argument)
    | Some A_thinking -> Thinking (single name argument)

    | Some A_tool ->
        (match argument with
         | None -> invalid_argument (name ^ " requires enable|disable NAME")
         | Some text ->
             (match String.index_opt text ' ' with
              | None -> invalid_argument (name ^ " requires enable|disable NAME")
              | Some offset ->
                  let operation = String.sub text 0 offset in
                  let tool_name = String.trim
                    (String.sub text (offset + 1) (String.length text - offset - 1))
                    |> require_single_argument name in
                  let enabled = match operation with
                    | "enable" -> true | "disable" -> false
                    | _ -> invalid_argument (name ^ " requires enable|disable NAME") in
                  Tool_toggle { name = tool_name; enabled }))
    | Some A_attach ->
        (match argument with
         | None -> invalid_argument (name ^ " requires an image path or clear")
         | Some "clear" -> Attach None
         | Some path -> Attach (Some (require_path name path)))
    | Some A_queue ->
        (match argument with
        | Some text -> Queue_prompt text
        | None -> invalid_argument (name ^ " requires a nonempty prompt"))
    | Some A_compact -> no_args (); Compact
    | Some A_retry -> no_args (); Retry
    | Some A_branch -> Branch (require_single_argument name
        (Option.value ~default:"" argument))
    | Some A_fork -> Fork (Option.map (require_path name) argument)
    | Some A_tools -> Tools (single name argument)
    | Some A_context -> no_args (); Context
    | Some A_usage -> no_args (); Usage
    | Some A_hotkeys -> no_args (); Hotkeys
    | Some A_entries -> no_args (); Entries
    | Some A_tree -> no_args (); Tree
    | None -> Unknown line

let selectable_providers () = Provider_catalog.all ()

let resolve_model ?current_route ~current_provider ~input () =
  let input = String.trim input in
  if input = "" || String.exists is_whitespace_or_control input then
    invalid_argument "model selector must be a nonempty single argument";
  let provider_selector, model = match String.index_opt input '/' with
    | Some slash ->
      String.sub input 0 slash,
      String.sub input (slash + 1) (String.length input - slash - 1)
    | None -> current_provider, input in
  if model = "" then invalid_argument "model ID must not be empty";
  let provider_id, selected_route = match String.index_opt provider_selector '@' with
    | None -> provider_selector, ""
    | Some at ->
        String.sub provider_selector 0 at,
        String.sub provider_selector (at + 1)
          (String.length provider_selector - at - 1) in
  if provider_id = "" || (selected_route = "" &&
      String.contains provider_selector '@') then
    invalid_argument "use PROVIDER@API/MODEL to select a wire route";
  let descriptor = match Provider_catalog.find provider_id with
    | Some descriptor -> descriptor
    | None -> invalid_argument ("unknown provider: " ^ provider_id) in
  let route_name = if selected_route <> "" then selected_route
    else if provider_id = current_provider then
      Option.value ~default:"" current_route
    else "" in
  let route = match Provider_catalog.route descriptor route_name with
    | Some route -> route
    | None -> invalid_argument
        ("no route for " ^ provider_id ^ "; use " ^ provider_id ^ "@API/MODEL") in
  descriptor, model, route

let history_for_model ~provider:active_provider ~route:active_route
    ~wire ~model messages =
  let retain (message : Protocol.message) =
    match message.provider_state with
    | None -> true
    | Some state ->
        let provider, route = match wire with
          | Provider.Openai_responses -> "openai", Some "responses"
          | Provider.Codex_responses -> "openai-codex", None
          | Provider.Gemini_direct -> "google", None
          | Provider.Meta_responses -> "meta", None
          | Provider.Opencode_zen_responses -> "opencode-zen", None
          | Provider.Devin_connect -> "devin", None
          | Provider.Commandcode_chat -> "commandcode", Some "chat"
          | Provider.Commandcode_messages -> "commandcode", Some "messages"
          | Provider.Commandcode_responses -> "commandcode", Some "responses"
          | Provider.Gitlab_duo_messages -> "gitlab-duo", Some "anthropic"
          | Provider.Gitlab_duo_responses -> "gitlab-duo", Some "responses"
          | _ -> "", None in
        provider <> "" && active_provider = provider &&
        (match route with
         | None -> true | Some name -> active_route = name) &&
        Protocol.member "provider" state = `String provider &&
        Protocol.member "model" state = `String model &&
        (match route with
         | None -> true
         | Some name -> Protocol.member "route" state = `String name) in
  if List.for_all retain messages then messages
  else List.map (fun (message : Protocol.message) ->
    if retain message then message else { message with provider_state = None })
    messages
