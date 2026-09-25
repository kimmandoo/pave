type command =
  | Login of string option
  | Model of string option
  | Quit
  | Cancel
  | Help
  | Settings
  | Setup
  | New
  | Resume of string option
  | Compact
  | Retry
  | Tree
  | Branch of string
  | Fork of string
  | Tools of string option
  | Context
  | Usage
  | Hotkeys
  | Entries
  | Prompt of string
  | Unknown of string

type action =
  | A_login | A_model | A_settings | A_setup | A_new | A_resume | A_cancel
  | A_entries | A_tree | A_tools | A_context | A_usage | A_hotkeys | A_branch | A_fork
  | A_compact | A_retry | A_help | A_quit

type shortcut = { name : string; usage : string; summary : string; action : action }

let commands = [
  { name = "/login"; usage = "[PROVIDER]"; summary = "Sign in to a provider"; action = A_login };
  { name = "/model"; usage = "[PROVIDER/MODEL]"; summary = "Choose an inference model"; action = A_model };
  { name = "/settings"; usage = ""; summary = "View or edit project defaults"; action = A_settings };
  { name = "/setup"; usage = ""; summary = "Choose a user default provider and model"; action = A_setup };
  { name = "/new"; usage = ""; summary = "Start a private saved session"; action = A_new };
  { name = "/resume"; usage = "[PATH]"; summary = "Search or reopen saved sessions"; action = A_resume };
  { name = "/cancel"; usage = ""; summary = "Cancel the active turn"; action = A_cancel };
  { name = "/retry"; usage = ""; summary = "Retry the last turn only if no tools ran"; action = A_retry };
  { name = "/tools"; usage = "[NAME]"; summary = "List or inspect enabled tools"; action = A_tools };
  { name = "/context"; usage = ""; summary = "Inspect active model and saved context"; action = A_context };
  { name = "/usage"; usage = ""; summary = "Inspect reported token usage by model"; action = A_usage };
  { name = "/hotkeys"; usage = ""; summary = "Show interactive terminal shortcuts"; action = A_hotkeys };
  { name = "/entries"; usage = ""; summary = "List journal entries"; action = A_entries };
  { name = "/tree"; usage = ""; summary = "Search journal ancestry and branch"; action = A_tree };
  { name = "/branch"; usage = "ID"; summary = "Continue from an earlier entry"; action = A_branch };
  { name = "/fork"; usage = "PATH"; summary = "Copy the selected journal branch"; action = A_fork };
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
    | Some A_login -> Login (single name argument)
    | Some A_model -> Model (single name argument)
    | Some A_quit -> no_args (); Quit
    | Some A_cancel -> no_args (); Cancel
    | Some A_help -> no_args (); Help
    | Some A_settings -> no_args (); Settings
    | Some A_setup -> no_args (); Setup
    | Some A_new -> no_args (); New
    | Some A_resume -> Resume (Option.map (require_path name) argument)
    | Some A_compact -> no_args (); Compact
    | Some A_retry -> no_args (); Retry
    | Some A_branch -> Branch (require_single_argument name
        (Option.value ~default:"" argument))
    | Some A_fork -> Fork (require_path name (Option.value ~default:"" argument))
    | Some A_tools -> Tools (single name argument)
    | Some A_context -> no_args (); Context
    | Some A_usage -> no_args (); Usage
    | Some A_hotkeys -> no_args (); Hotkeys
    | Some A_entries -> no_args (); Entries
    | Some A_tree -> no_args (); Tree
    | None -> Unknown line

let selectable_providers () = Provider_catalog.all ()

let resolve_model ~current_provider ~input =
  let input = String.trim input in
  if input = "" || String.exists is_whitespace_or_control input then
    invalid_argument "model selector must be a nonempty single argument";
  let provider_id, model = match String.index_opt input '/' with
    | Some slash ->
      String.sub input 0 slash,
      String.sub input (slash + 1) (String.length input - slash - 1)
    | None -> current_provider, input in
  if model = "" then invalid_argument "model ID must not be empty";
  let descriptor = match Provider_catalog.find provider_id with
    | Some descriptor -> descriptor
    | None -> invalid_argument ("unknown provider: " ^ provider_id) in
  let route = match Provider_catalog.route descriptor "" with
    | Some route -> route
    | None -> invalid_argument ("no route for provider: " ^ provider_id) in
  descriptor, model, route

let history_for_model ~wire ~model messages =
  let retain (message : Protocol.message) =
    match message.provider_state with
    | None -> true
    | Some state ->
        let compatible = match wire with
          | Provider.Codex_responses ->
              Protocol.member "provider" state = `String "openai-codex"
          | Provider.Gemini_direct ->
              Protocol.member "provider" state = `String "google"
          | _ -> false in
        compatible && Protocol.member "model" state = `String model in
  if List.for_all retain messages then messages
  else List.map (fun (message : Protocol.message) ->
    if retain message then message else { message with provider_state = None })
    messages
