type command =
  | Model of string option
  | Login
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
  | Jobs
  | Wait of string
  | Cancel_job of string
  | Artifact of string option
  | Rewind of string option
  | Delegate of { label : string; task : string }
  | Plan of string option
  | Goal of string option
  | Advisor of string option
  | Watchdog of string option
  | Loop of string option
  | Autoresearch of string option
  | Rule of string option
  | Queue_prompt of string
  | Quit
  | Prompt of string
  | Skill of string
  | Prompt_command of string
  | Plugin of string option
  | Mcp of string option
  | Unknown of string

type action =
  | A_model | A_settings | A_setup | A_login | A_new | A_resume | A_clear | A_fresh
  | A_rename | A_label | A_pin | A_approval | A_thinking | A_tool | A_attach
  | A_cancel | A_queue | A_entries | A_tree | A_tools | A_context | A_usage
  | A_hotkeys | A_branch | A_fork | A_compact | A_retry | A_help | A_quit
  | A_jobs | A_wait | A_cancel_job | A_artifact | A_rewind | A_delegate
  | A_plan | A_goal | A_advisor | A_watchdog | A_loop | A_autoresearch | A_rule
  | A_skill of string | A_prompt_command of string
  | A_plugin
  | A_mcp
  | A_mcp_connect of string

type grammar =
  | No_arguments
  | Optional_word of string
  | Optional_choice of string list
  | Required_word of string
  | Optional_text of string
  | Required_text of string
  | Optional_path of string
  | Required_path of string
  | Path_or_clear of string
  | Required_choice_word of string * string * string
  | Required_word_and_text of string * string

type shortcut = {
  name : string;
  grammar : grammar;
  summary : string;
  action : action;
  session_only : bool;
  interactive_only : bool;
}

let command ?(session_only = false) ?(interactive_only = false)
    name grammar summary action =
  { name; grammar; summary; action; session_only; interactive_only }

let commands = [
  command "/model" (Optional_word "PROVIDER[@API][#ACCOUNT]/MODEL") "Switch model for this conversation" A_model;
  command "/settings" No_arguments "View or edit project defaults" A_settings;
  command ~interactive_only:true "/setup" No_arguments "Connect and save your user default model" A_setup;
  command ~interactive_only:true "/login" No_arguments "Connect an account without changing the active model" A_login;
  command "/new" No_arguments "Start a private saved session" A_new;
  command "/resume" (Optional_path "ID|TITLE|PATH") "Search or reopen saved sessions" A_resume;
  command "/clear" No_arguments "Reset active context without deleting journal history" A_clear;
  command "/fresh" No_arguments "Rebuild the local provider agent from saved context" A_fresh;
  command ~session_only:true "/rename" (Required_text "TITLE") "Set a durable session title" A_rename;
  command ~session_only:true "/label" (Optional_text "TEXT") "Set or clear a label on the selected entry" A_label;
  command ~session_only:true "/pin" No_arguments "Toggle this session in the pinned resume list" A_pin;
  command "/approval" (Optional_choice ["always-ask"; "write"; "yolo"; "default"]) "Show or set this branch's approval mode" A_approval;
  command "/thinking" (Optional_word "LEVEL|default") "Store branch-local thinking level; compatible providers receive the selected reasoning control" A_thinking;
  command "/tool" (Required_choice_word ("enable", "disable", "NAME")) "Set branch-local tool availability" A_tool;
  command "/attach" (Path_or_clear "PATH") "Stage supported image, audio, or video media for the next prompt" A_attach;
  command "/queue" (Required_text "MESSAGE") "Queue a follow-up without interrupting the active turn" A_queue;
  command "/cancel" No_arguments "Cancel the active turn" A_cancel;
  command "/retry" No_arguments "Retry the last turn only if no tools ran" A_retry;
  command "/tools" (Optional_word "NAME") "List or inspect enabled tools" A_tools;
  command "/plugin" (Optional_text "list|enable NAME|disable NAME|reload")
    "Inspect or manage private local plugin capabilities" A_plugin;
  command "/context" No_arguments "Inspect active model and saved context" A_context;
  command "/mcp" (Optional_text "list|connect SERVER|tools SERVER|resources SERVER|prompts SERVER|read SERVER URI|get SERVER NAME|reload")
    "Inspect and explicitly connect configured MCP servers" A_mcp;
  command "/usage" No_arguments "Inspect reported token usage by model" A_usage;
  command ~interactive_only:true "/hotkeys" No_arguments "Show interactive terminal shortcuts" A_hotkeys;
  command ~session_only:true "/entries" No_arguments "List journal entries" A_entries;
  command ~session_only:true "/tree" No_arguments "Search journal ancestry and branch" A_tree;
  command ~session_only:true "/branch" (Required_word "ID") "Continue from an earlier entry" A_branch;
  command ~session_only:true "/fork" (Optional_path "PATH") "Fork the selected journal branch into a private session" A_fork;
  command ~session_only:true "/compact" No_arguments "Summarize older turns" A_compact;
  command ~session_only:true "/jobs" No_arguments "List session-owned background jobs" A_jobs;
  command ~session_only:true "/wait" (Required_word "JOB_ID") "Wait for a session-owned job result" A_wait;
  command ~session_only:true "/cancel-job" (Required_word "JOB_ID") "Cancel a background job" A_cancel_job;
  command ~session_only:true "/artifact" (Optional_word "ID") "List session artifacts or show text output" A_artifact;
  command ~session_only:true "/rewind" (Optional_word "EFFECT_ID") "List or request a confirmed, hash-guarded workspace restore" A_rewind;
  command ~session_only:true "/delegate" (Required_word_and_text ("LABEL", "TASK")) "Start a bounded read-only child agent" A_delegate;
  command ~session_only:true "/plan" (Optional_text "GOAL") "Create a review-only plan artifact" A_plan;
  command ~session_only:true "/goal" (Optional_text "TEXT|clear") "Show, set, or clear the session goal" A_goal;
  command ~session_only:true "/advisor" (Optional_text "QUESTION") "Request an independent read-only review" A_advisor;
  command ~session_only:true "/watchdog" (Optional_text "QUESTION") "Run a review-only scope and safety check" A_watchdog;
  command ~session_only:true "/loop" (Optional_text "GOAL") "Run a bounded review-only planning loop" A_loop;
  command ~session_only:true "/autoresearch" (Optional_text "QUESTION") "Run bounded read-only research" A_autoresearch;
  command ~session_only:true "/rule" (Optional_text "TEXT|clear") "Show, set, or clear a session interruption rule" A_rule;
  command "/help" No_arguments "Show commands and keys" A_help;
  command "/quit" No_arguments "Exit Pave" A_quit;
]

let usage item =
  match item.grammar with
  | No_arguments -> ""
  | Optional_word value | Optional_text value | Optional_path value ->
      "[" ^ value ^ "]"
  | Optional_choice choices -> "[" ^ String.concat "|" choices ^ "]"
  | Required_word value | Required_text value | Required_path value -> value
  | Path_or_clear value -> value ^ "|clear"
  | Required_choice_word (first, second, value) ->
      first ^ "|" ^ second ^ " " ^ value
  | Required_word_and_text (first, second) -> first ^ " " ^ second

let available ?(session = true) ?(interactive = true) ?(subagents = false) item =
  (not item.session_only || session) &&
  (not item.interactive_only || interactive) &&
  (subagents || match item.action with
    | A_delegate | A_plan | A_advisor | A_watchdog | A_loop | A_autoresearch -> false
    | _ -> true)

let suggestions ?(session = true) ?(interactive = true) ?(subagents = false)
    ?(external_commands = []) prefix =
  if not (String.starts_with ~prefix:"/" prefix) then []
  else List.filter (fun item ->
    available ~session ~interactive ~subagents item &&
    String.starts_with ~prefix item.name) (commands @ external_commands)

let help ?(session = true) ?(interactive = true) ?(subagents = false)
    ?(external_commands = []) () =
  List.filter (available ~session ~interactive ~subagents) (commands @ external_commands)
  |> List.map (fun item ->
    let usage = usage item in
    item.name ^ (if usage = "" then "" else " " ^ usage) ^
    " · " ^ item.summary)


let is_whitespace_or_control char =
  let code = Char.code char in
  code <= 32 || code = 127

let require_single_argument command argument =
  let argument = String.trim argument in
  if argument = "" then invalid_arg (command ^ " requires a nonempty argument");
  if String.exists is_whitespace_or_control argument then
    invalid_arg (command ^ " accepts only one argument");
  argument

let require_path command argument =
  let argument = String.trim argument in
  if argument = "" || String.exists (fun char ->
    let code = Char.code char in
    (code < 32 && char <> ' ') || code = 127) argument then
    invalid_arg (command ^ " requires a readable path");
  argument

let require_text command argument =
  if String.trim argument = "" || String.exists (fun char ->
    let code = Char.code char in code < 32 || code = 127) argument then
    invalid_arg (command ^ " requires nonempty single-line text");
  argument
type parsed_arguments =
  | No_argument
  | Optional_argument of string option
  | Required_argument of string
  | Pair_argument of string * string

let split_pair command text =
  let offset = ref 0 in
  while !offset < String.length text &&
    not (is_whitespace_or_control text.[!offset]) do incr offset done;
  if !offset = String.length text then
    invalid_arg (command ^ " requires two arguments");
  let first = String.sub text 0 !offset in
  let start = ref !offset in
  while !start < String.length text &&
    is_whitespace_or_control text.[!start] do incr start done;
  let rest = String.sub text !start (String.length text - !start) in
  if rest = "" then invalid_arg (command ^ " requires two arguments");
  first, rest

let parse_arguments name grammar argument =
  let single = function
    | None -> None
    | Some text -> Some (require_single_argument name text) in
  let text = Option.map (require_text name) argument in
  match grammar, argument with
  | No_arguments, None -> No_argument
  | No_arguments, Some _ ->
      invalid_arg (name ^ " takes no arguments")
  | Optional_word _, _ -> Optional_argument (single argument)
  | Optional_choice choices, _ ->
      let selected = single argument in
      Option.iter (fun value ->
        if not (List.mem value choices) then
          invalid_arg (name ^ " expects " ^ String.concat "|" choices))
        selected;
      Optional_argument selected
  | Required_word _, Some _ ->
      Required_argument (Option.get (single argument))
  | Required_word _, None ->
      invalid_arg (name ^ " requires an argument")
  | Optional_text _, _ -> Optional_argument text
  | Required_text _, Some _ ->
      Required_argument (Option.get text)
  | Required_text _, None ->
      invalid_arg (name ^ " requires nonempty text")
  | Optional_path _, _ ->
      Optional_argument (Option.map (require_path name) argument)
  | Required_path _, Some path ->
      Required_argument (require_path name path)
  | Required_path _, None ->
      invalid_arg (name ^ " requires a readable path")
  | Path_or_clear _, Some "clear" -> Required_argument "clear"
  | Path_or_clear _, Some path ->
      Required_argument (require_path name path)
  | Path_or_clear _, None ->
      invalid_arg (name ^ " requires a media path or clear")
  | Required_choice_word (first, second, _), Some value ->
      let operation, argument = split_pair name value in
      if operation <> first && operation <> second then
        invalid_arg (name ^ " requires " ^ first ^ "|" ^ second);
      Pair_argument (operation, require_single_argument name argument)
  | Required_choice_word _, None ->
      invalid_arg (name ^ " requires two arguments")
  | Required_word_and_text _, Some value ->
      let first, rest = split_pair name value in
      Pair_argument (require_single_argument name first,
        require_text name rest)
  | Required_word_and_text _, None ->
      invalid_arg (name ^ " requires two arguments")

let parse ?(session = true) ?(interactive = true) ?(subagents = false)
    ?(external_commands = []) line =
  let line = String.trim line in
  if not (String.starts_with ~prefix:"/" line) then Prompt line
  else
    let offset = ref 0 in
    while !offset < String.length line &&
      not (is_whitespace_or_control line.[!offset]) do incr offset done;
    let name = String.sub line 0 !offset in
    let argument = if !offset = String.length line then None
      else
        let text = String.trim (String.sub line !offset
          (String.length line - !offset)) in
        if text = "" then None else Some text in
    match List.find_opt (fun item -> item.name = name)
      (commands @ external_commands) with
    | None -> Unknown line
    | Some item when not (available ~session ~interactive ~subagents item) -> Unknown line
    | Some item ->
        let arguments =
          try parse_arguments name item.grammar argument
          with Invalid_argument message ->
            let detail = if String.starts_with ~prefix:"interaction: " message
              then String.sub message 13 (String.length message - 13)
              else message in
            let syntax = usage item in
            let usage_text = if syntax = "" then item.name
              else item.name ^ " " ^ syntax in
            invalid_arg (detail ^ " (usage: " ^ usage_text ^ ")") in
        match item.action, arguments with
        | A_model, Optional_argument selector -> Model selector
        | A_login, No_argument -> Login
        | A_quit, No_argument -> Quit
        | A_cancel, No_argument -> Cancel
        | A_help, No_argument -> Help
        | A_settings, No_argument -> Settings
        | A_setup, No_argument -> Setup
        | A_new, No_argument -> New
        | A_resume, Optional_argument path -> Resume path
        | A_clear, No_argument -> Clear
        | A_fresh, No_argument -> Fresh
        | A_rename, Required_argument title -> Rename title
        | A_label, Optional_argument label -> Label label
        | A_pin, No_argument -> Pin
        | A_plugin, Optional_argument operation -> Plugin operation
        | A_approval, Optional_argument value -> Approval value
        | A_mcp, Optional_argument operation -> Mcp operation
        | A_mcp_connect name, No_argument -> Mcp (Some ("connect " ^ name))
        | A_thinking, Optional_argument value -> Thinking value
        | A_tool, Pair_argument (operation, tool_name) ->
            Tool_toggle { name = tool_name; enabled = operation = "enable" }
        | A_attach, Required_argument "clear" -> Attach None
        | A_attach, Required_argument path -> Attach (Some path)
        | A_queue, Required_argument text -> Queue_prompt text
        | A_compact, No_argument -> Compact
        | A_retry, No_argument -> Retry
        | A_branch, Required_argument id -> Branch id
        | A_fork, Optional_argument path -> Fork path
        | A_tools, Optional_argument selected -> Tools selected
        | A_context, No_argument -> Context
        | A_usage, No_argument -> Usage
        | A_hotkeys, No_argument -> Hotkeys
        | A_entries, No_argument -> Entries
        | A_tree, No_argument -> Tree
        | A_jobs, No_argument -> Jobs
        | A_wait, Required_argument id -> Wait id
        | A_cancel_job, Required_argument id -> Cancel_job id
        | A_artifact, Optional_argument id -> Artifact id
        | A_rewind, Optional_argument id -> Rewind id
        | A_delegate, Pair_argument (label, task) ->
            Delegate { label; task }
        | A_plan, Optional_argument goal -> Plan goal
        | A_goal, Optional_argument goal -> Goal goal
        | A_advisor, Optional_argument question -> Advisor question
        | A_watchdog, Optional_argument question -> Watchdog question
        | A_loop, Optional_argument goal -> Loop goal
        | A_autoresearch, Optional_argument question -> Autoresearch question
        | A_rule, Optional_argument text -> Rule text
        | A_skill name, No_argument -> Skill name
        | A_prompt_command name, No_argument -> Prompt_command name
        | _ -> invalid_arg
            ("interaction: command descriptor has incompatible grammar: " ^
              name)

let selectable_providers ?registry () = Provider_catalog.all ?registry ()

let model_route_browse_choices ?registry () =
  Provider_catalog.all ?registry ()
  |> List.concat_map (fun (descriptor : Provider_catalog.descriptor) ->
    if List.length descriptor.routes < 2 then [] else
    List.map (fun (route : Provider_catalog.route) ->
      let value = Printf.sprintf "%s@%s/" descriptor.id route.name in
      let label = Printf.sprintf "%s@%s · browse API models"
        descriptor.id route.name in
      value, label) descriptor.routes)

let model_route_browse_selection ?registry input =
  Provider_catalog.all ?registry ()
  |> List.find_map (fun (descriptor : Provider_catalog.descriptor) ->
    if List.length descriptor.routes < 2 then None else
    List.find_map (fun (route : Provider_catalog.route) ->
      if input = Printf.sprintf "%s@%s/" descriptor.id route.name
      then Some (descriptor, route)
      else None) descriptor.routes)

let resolve_model ?registry ?current_route ?current_account_id
    ~current_provider ~input () =
  let input = String.trim input in
  if input = "" || String.exists is_whitespace_or_control input then
    invalid_arg "model selector must be a nonempty single argument";
  let provider_id, selected_route, selected_account, model =
    match String.index_opt input '/' with
    | None -> current_provider, None, None, input
    | Some slash ->
        let prefix = String.sub input 0 slash in
        let model = String.sub input (slash + 1)
          (String.length input - slash - 1) in
        if String.contains prefix '@' || String.contains prefix '#' ||
           Provider_catalog.find ?registry prefix <> None then
          let provider, route, account =
            Model_identity.parse_selector_prefix prefix in
          provider, route, account, model
        else current_provider, None, None, input in
  if model = "" then invalid_arg "model ID must not be empty";
  let descriptor = match Provider_catalog.find ?registry provider_id with
    | Some descriptor -> descriptor
    | None -> invalid_arg ("unknown provider: " ^ provider_id) in
  let route_name = match selected_route with
    | Some name -> name
    | None when provider_id = current_provider ->
        Option.value ~default:"" current_route
    | None -> "" in
  let route = match Provider_catalog.route descriptor route_name with
    | Some route -> route
    | None -> invalid_arg
        ("no route for " ^ provider_id ^ "; use " ^ provider_id ^ "@API/MODEL") in
  let custom_route = Provider_catalog.custom_route
      (Option.value ~default:Provider_catalog.builtin_registry registry)
      ~provider:provider_id ~route:route.name in
  let account_id = match custom_route with
    | Some custom ->
        if selected_account <> None && selected_account <> custom.account_id then
          invalid_arg "custom provider account scope is configured on its route";
        if provider_id = current_provider && current_account_id <> None &&
           current_account_id <> custom.account_id then
          invalid_arg "selected account does not match the configured custom route";
        custom.account_id
    | None ->
        (match selected_account with
         | Some _ -> selected_account
         | None when provider_id = current_provider -> current_account_id
         | None -> None) in
  let config_revision = Option.map Custom_provider.fingerprint custom_route in
  let identity = Model_identity.make ~provider:provider_id ?account_id
    ?config_revision ~route:route.name ~upstream_id:model () in
  descriptor, identity, route

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
          | Provider.Vertex_anthropic -> "google-vertex", Some "messages"
          | Provider.Meta_responses -> "meta", None
          | Provider.Opencode_zen_responses -> "opencode-zen", None
          | Provider.Devin_connect -> "devin", None
          | Provider.Commandcode_chat -> "commandcode", Some "chat"
          | Provider.Commandcode_messages -> "commandcode", Some "messages"
          | Provider.Commandcode_responses -> "commandcode", Some "responses"
          | Provider.Gitlab_duo_messages -> "gitlab-duo", Some "anthropic"
          | Provider.Gitlab_duo_responses -> "gitlab-duo", Some "responses"
          | Provider.Anthropic_messages -> "anthropic", Some "messages"
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
