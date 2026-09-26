type tool_state =
  | Tool_started
  | Tool_settled of { is_error : bool }
  | Tool_aborted of { side_effects_may_have_occurred : bool }
type tool_lifecycle = { call_id : string; name : string; state : tool_state }
type pending_state =
  | Unknown
  | Started
  | Settled
  | Aborted of { side_effects_may_have_occurred : bool }
type pending_tool_call = {
  call_id : string; name : string; state : pending_state
}
type exit_kind = Normal | Signal | Fatal | Process_exit
type kind =
  | Message of Protocol.message
  | Compaction of {
      summary : string; first_kept_id : string;
      provider_state : Yojson.Basic.t option
    }
  | Model of Model_identity.t
  | Thinking of string option
  | Tool_selection of string list
  | Mode_change of Approval.mode option
  | Title of string
  | Label of { target_id : string; label : string option }
  | Pin of bool
  | Reset_boundary
  | Usage of {
      provider : string; account_id : string option; route : string option;
      model : string; tokens : Protocol.usage
    }
  | Branch
  | Tool_lifecycle of tool_lifecycle
  | Session_exit of { kind : exit_kind; pending_tool_calls : pending_tool_call list }
type entry = { id : string; parent_id : string option; timestamp : string; kind : kind }

type t = {
  path : string;
  header : Yojson.Basic.t;
  mutable records_rev : entry list;
  by_id : (string, entry) Hashtbl.t;
  mutable leaf : string option;
  mutable disk_size : int;
}

let invalid text = raise (Protocol.Invalid_response ("invalid session journal: " ^ text))

let fresh_id () =
  let bytes = Bytes.create 16 in
  let fd = Unix.openfile "/dev/urandom" [ Unix.O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let rec fill offset =
      if offset < Bytes.length bytes then (
        let n = Unix.read fd bytes offset (Bytes.length bytes - offset) in
        if n = 0 then failwith "could not generate session ID";
        fill (offset + n)) in
    fill 0);
  let hex = "0123456789abcdef" in
  let result = Bytes.create 32 in
  for i = 0 to 15 do
    let byte = Char.code (Bytes.get bytes i) in
    Bytes.set result (2 * i) hex.[byte lsr 4];
    Bytes.set result (2 * i + 1) hex.[byte land 15]
  done;
  Bytes.unsafe_to_string result

let timestamp () =
  let time = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (time.Unix.tm_year + 1900) (time.Unix.tm_mon + 1) time.Unix.tm_mday
    time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec

let option_json = function None -> `Null | Some text -> `String text

let model_identity_json (identity : Model_identity.t) =
  `Assoc [
    "provider", `String identity.provider;
    "accountId", option_json identity.account_id;
    "route", `String identity.route;
    "upstreamId", `String identity.upstream_id;
  ]

let new_header ?parent_session cwd =
  `Assoc [ "type", `String "session"; "version", `Int 1;
           "id", `String (fresh_id ()); "timestamp", `String (timestamp ());
           "cwd", `String cwd; "parentSession", option_json parent_session ]

let entry_json entry =
  let type_name = match entry.kind with
    | Message _ -> "message" | Compaction _ -> "compaction"
    | Model _ -> "model" | Thinking _ -> "thinking_level_change"
    | Tool_selection _ -> "tool_selection" | Mode_change _ -> "mode_change"
    | Title _ -> "title_change" | Label _ -> "label"
    | Pin _ -> "pin_change" | Reset_boundary -> "reset_boundary"
    | Usage _ -> "usage" | Branch -> "branch"
    | Tool_lifecycle _ -> "tool" | Session_exit _ -> "exit" in
  let fields = [ "type", `String type_name;
    "id", `String entry.id; "parentId", option_json entry.parent_id;
    "timestamp", `String entry.timestamp ] in
  match entry.kind with
  | Message message ->
      Protocol.validate_attachments message.attachments;
      let message_json = Protocol.message_to_json ~stored:true
        { message with attachments = [] } in
      `Assoc (fields @ ["message", message_json] @
        (if message.attachments = [] then [] else
          ["attachments", `List (List.map Protocol.attachment_to_json
            message.attachments)]))
  | Compaction { summary; first_kept_id; provider_state } ->
      `Assoc (fields @ [ "summary", `String summary;
                         "firstKeptEntryId", `String first_kept_id ] @
        (match provider_state with
         | None -> [] | Some state -> ["providerState", state]))
  | Model identity ->
      `Assoc (fields @ ["identity", model_identity_json identity])
  | Thinking level ->
      `Assoc (fields @ ["thinkingLevel", option_json level])
  | Tool_selection disabled ->
      `Assoc (fields @ ["disabledTools", `List (List.map (fun name ->
        `String name) disabled)])
  | Mode_change mode ->
      `Assoc (fields @ ["mode", option_json
        (Option.map Approval.string_of_mode mode)])
  | Title title -> `Assoc (fields @ ["title", `String title])
  | Label { target_id; label } ->
      `Assoc (fields @ ["targetId", `String target_id;
        "label", option_json label])
  | Pin pinned -> `Assoc (fields @ ["pinned", `Bool pinned])
  | Reset_boundary | Branch -> `Assoc fields
  | Usage { provider; account_id; route; model; tokens } ->
      let optional_count name = function
        | None -> []
        | Some count -> [name, `Int count] in
      let optional_text name = function
        | None -> []
        | Some value -> [name, `String value] in
      `Assoc (fields @ ["provider", `String provider; "model", `String model] @
        optional_text "accountId" account_id @ optional_text "route" route @
        ["inputTokens", `Int tokens.input_tokens;
         "outputTokens", `Int tokens.output_tokens] @
        optional_count "cachedInputTokens" tokens.cached_input_tokens @
        optional_count "cacheCreationInputTokens"
          tokens.cache_creation_input_tokens @
        optional_count "reasoningOutputTokens" tokens.reasoning_output_tokens)
  | Tool_lifecycle { call_id; name; state } ->
      let state_fields = match state with
        | Tool_started -> ["state", `String "started"]
        | Tool_settled { is_error } ->
            ["state", `String "settled"; "isError", `Bool is_error]
        | Tool_aborted { side_effects_may_have_occurred } ->
            ["state", `String "aborted";
             "sideEffectsMayHaveOccurred", `Bool side_effects_may_have_occurred] in
      `Assoc (fields @ ["toolCallId", `String call_id;
        "toolName", `String name] @ state_fields)
  | Session_exit { kind; pending_tool_calls } ->
      let pending = List.map (fun call ->
        let state_fields = match call.state with
          | Unknown -> ["state", `String "unknown"]
          | Started -> ["state", `String "started"]
          | Settled -> ["state", `String "settled"]
          | Aborted { side_effects_may_have_occurred } ->
              ["state", `String "aborted";
               "sideEffectsMayHaveOccurred", `Bool side_effects_may_have_occurred] in
        `Assoc (["toolCallId", `String call.call_id;
          "toolName", `String call.name] @ state_fields)) pending_tool_calls in
      `Assoc (fields @ ["exitKind", `String (match kind with
        | Normal -> "normal" | Signal -> "signal"
        | Fatal -> "fatal" | Process_exit -> "process_exit");
        "pendingToolCalls", `List pending])

let write_all fd text =
  let rec loop offset =
    if offset < String.length text then (
      let n = Unix.write_substring fd text offset (String.length text - offset) in
      if n = 0 then failwith "could not write session journal";
      loop (offset + n)) in
  loop 0

let line json = Yojson.Basic.to_string json ^ "\n"

let write_new_file path content =
  let temp = Filename.temp_file ~temp_dir:(Filename.dirname path) ".pave-journal-" ".tmp" in
  Fun.protect ~finally:(fun () -> try Unix.unlink temp with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
    (fun () ->
      let fd = Unix.openfile temp [ Unix.O_WRONLY; Unix.O_TRUNC ] 0 in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
        Unix.fchmod fd 0o600;
        write_all fd content;
        Unix.fsync fd);
      Unix.link temp path)

let append_line t json =
  let fd = Unix.openfile t.path [ Unix.O_WRONLY; Unix.O_APPEND ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    Unix.lockf fd Unix.F_LOCK 0;
    Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) (fun () ->
      if (Unix.fstat fd).Unix.st_size <> t.disk_size then
        failwith "session changed on disk; reopen before writing";
      write_all fd (line json);
      Unix.fsync fd;
      t.disk_size <- (Unix.fstat fd).Unix.st_size))

let valid_model_field text =
  text <> "" && not (String.exists (fun char ->
    Char.code char <= 32 || Char.code char = 127) text)
let valid_text_field limit text =
  text <> "" && String.length text <= limit &&
  not (String.exists (fun char ->
    let code = Char.code char in code < 32 || code = 127) text)
let parse_model_identity json =
  let fields = match json with
    | `Assoc fields -> fields
    | _ -> invalid "invalid model identity" in
  let names = List.map fst fields in
  let allowed = ["provider"; "accountId"; "route"; "upstreamId"] in
  if List.length names <> List.length (List.sort_uniq String.compare names) then
    invalid "duplicate model identity field";
  List.iter (fun name ->
    if not (List.mem name allowed) then invalid "unknown model identity field")
    names;
  let field name = match List.assoc_opt name fields with
    | Some value -> value
    | None -> invalid "missing model identity field" in
  let text name = match field name with
    | `String value when valid_model_field value -> value
    | _ -> invalid "invalid model identity field" in
  let provider = text "provider" and route = text "route"
  and upstream_id = text "upstreamId" in
  let account_id = match field "accountId" with
    | `Null -> None
    | `String value when valid_model_field value -> Some value
    | _ -> invalid "invalid model identity account ID" in
  try Model_identity.make ~provider ?account_id ~route ~upstream_id ()
  with Invalid_argument _ -> invalid "invalid model identity"

let legacy_model_identity provider model api =
  let route = match api with
    | `String route when valid_model_field route -> route
    | `Null ->
        (match Provider_catalog.find provider with
         | Some descriptor ->
             Option.fold ~none:"legacy-unscoped"
               ~some:(fun route -> route.Provider_catalog.name)
               (Provider_catalog.route descriptor "")
         | None -> "legacy-unscoped")
    | _ -> invalid "invalid legacy model route" in
  try Model_identity.make ~provider ~route ~upstream_id:model ()
  with Invalid_argument _ -> invalid "invalid legacy model identity"


let parse_optional_text get name limit =
  match get name with
  | `Null -> None
  | `String value when valid_text_field limit value -> Some value
  | _ -> invalid ("invalid " ^ name)

let parse_attachment_list json =
  match json with
  | `Null -> []
  | `List items ->
      let attachments = List.map Protocol.attachment_from_json items in
      (try Protocol.validate_attachments attachments
       with Protocol.Invalid_response _ -> invalid "invalid attachments");
      attachments
  | _ -> invalid "invalid attachments"

let parse_disabled_tools json =
  match json with
  | `List items ->
      let names = List.map (function
        | `String name when valid_model_field name -> name
        | _ -> invalid "invalid disabled tool name") items in
      if List.length names > 128 ||
         List.length names <> List.length (List.sort_uniq String.compare names) then
        invalid "invalid disabled tools";
      names
  | _ -> invalid "invalid disabled tools"


let parse_entry json =
  let get key = Protocol.member key json in
  let id = match get "id" with `String value when value <> "" -> value
    | _ -> invalid "entry ID missing" in
  let parent_id = match get "parentId" with `String value -> Some value
    | `Null -> None | _ -> invalid "invalid parent ID" in
  let timestamp = match get "timestamp" with `String value -> value
    | _ -> invalid "entry timestamp missing" in
  let pending_tool_call json =
    let call_id = Protocol.member "toolCallId" json
    and name = Protocol.member "toolName" json
    and state = Protocol.member "state" json in
    match call_id, name, state with
    | `String call_id, `String name, `String "unknown"
      when valid_model_field call_id && valid_model_field name ->
        { call_id; name; state = Unknown }
    | `String call_id, `String name, `String "started"
      when valid_model_field call_id && valid_model_field name ->
        { call_id; name; state = Started }
    | `String call_id, `String name, `String "settled"
      when valid_model_field call_id && valid_model_field name ->
        { call_id; name; state = Settled }
    | `String call_id, `String name, `String "aborted"
      when valid_model_field call_id && valid_model_field name ->
        (match Protocol.member "sideEffectsMayHaveOccurred" json with
         | `Bool side_effects_may_have_occurred ->
             { call_id; name; state = Aborted { side_effects_may_have_occurred } }
         | _ -> invalid "invalid pending tool state")
    | _ -> invalid "invalid pending tool call" in
  let kind = match get "type" with
    | `String "message" ->
        let message = Protocol.message_from_json (get "message") in
        let attachments = parse_attachment_list (get "attachments") in
        if attachments <> [] && message.role <> "user" then
          invalid "attachments on a non-user message";
        Message { message with attachments }
    | `String "compaction" ->
        (match get "summary", get "firstKeptEntryId", get "providerState" with
         | `String summary, `String first_kept_id, provider_state
           when String.trim summary <> "" && first_kept_id <> "" ->
             Compaction { summary; first_kept_id;
               provider_state = (match provider_state with
                 | `Null -> None | value -> Some value) }
         | _ -> invalid "invalid compaction")
    | `String "model" ->
        (match get "identity" with
         | `Assoc _ as identity -> Model (parse_model_identity identity)
         | `Null ->
             (match get "provider", get "model", get "api" with
              | `String provider, `String model, (`Null | `String _ as api)
                when valid_model_field provider && valid_model_field model ->
                  Model (legacy_model_identity provider model api)
              | _ -> invalid "invalid model selection")
         | _ -> invalid "invalid model identity")
    | `String "thinking_level_change" ->
        Thinking (parse_optional_text get "thinkingLevel" 32)
    | `String "tool_selection" ->
        Tool_selection (parse_disabled_tools (get "disabledTools"))
    | `String "mode_change" ->
        (match get "mode" with
         | `Null -> Mode_change None
         | `String value ->
             (match Approval.mode_of_string value with
              | Some mode -> Mode_change (Some mode)
              | None -> invalid "invalid approval mode")
         | _ -> invalid "invalid approval mode")
    | `String "title_change" ->
        (match get "title" with
         | `String title when valid_text_field 256 title -> Title title
         | _ -> invalid "invalid session title")
    | `String "label" ->
        (match get "targetId", parse_optional_text get "label" 128 with
         | `String target_id, label when valid_model_field target_id ->
             Label { target_id; label }
         | _ -> invalid "invalid entry label")
    | `String "pin_change" ->
        (match get "pinned" with
         | `Bool pinned -> Pin pinned
         | _ -> invalid "invalid pin metadata")
    | `String "reset_boundary" -> Reset_boundary
    | `String "usage" ->
        let optional_count name = match get name with
          | `Null -> None
          | `Int count when count >= 0 -> Some count
          | _ -> invalid "invalid provider token usage detail" in
        let account_id = match get "accountId" with
          | `Null -> None
          | `String account when valid_model_field account -> Some account
          | _ -> invalid "invalid provider usage account ID" in
        let route = match get "route" with
          | `Null -> None
          | `String route when valid_model_field route -> Some route
          | _ -> invalid "invalid provider usage route" in
        (match get "provider", get "model",
          get "inputTokens", get "outputTokens" with
         | `String provider, `String model, `Int input_tokens, `Int output_tokens
           when valid_model_field provider && valid_model_field model &&
             input_tokens >= 0 && output_tokens >= 0 ->
             let cached_input_tokens = optional_count "cachedInputTokens"
             and cache_creation_input_tokens =
               optional_count "cacheCreationInputTokens"
             and reasoning_output_tokens = optional_count "reasoningOutputTokens" in
             let within total = function
               | None -> true
               | Some detail -> detail <= total in
             let cache_details_fit = match cached_input_tokens,
               cache_creation_input_tokens with
               | Some cached, Some created ->
                   cached <= input_tokens && created <= input_tokens - cached
               | cached, created ->
                   within input_tokens cached && within input_tokens created in
             if not cache_details_fit ||
                not (within output_tokens reasoning_output_tokens) then
               invalid "provider usage detail exceeds reported totals";
             Usage { provider; account_id; route; model; tokens = {
               input_tokens; output_tokens; cached_input_tokens;
               cache_creation_input_tokens; reasoning_output_tokens } }
         | _ -> invalid "invalid provider token usage")
    | `String "branch" -> Branch
    | `String "tool" ->
        (match get "toolCallId", get "toolName", get "state" with
         | `String call_id, `String name, `String "started"
           when valid_model_field call_id && valid_model_field name ->
             Tool_lifecycle { call_id; name; state = Tool_started }
         | `String call_id, `String name, `String "settled"
           when valid_model_field call_id && valid_model_field name ->
             (match get "isError" with
              | `Bool is_error ->
                  Tool_lifecycle { call_id; name; state = Tool_settled { is_error } }
              | _ -> invalid "invalid settled tool event")
         | `String call_id, `String name, `String "aborted"
           when valid_model_field call_id && valid_model_field name ->
             (match get "sideEffectsMayHaveOccurred" with
              | `Bool side_effects_may_have_occurred ->
                  Tool_lifecycle { call_id; name; state =
                    Tool_aborted { side_effects_may_have_occurred } }
              | _ -> invalid "invalid aborted tool event")
         | _ -> invalid "invalid tool lifecycle event")
    | `String "exit" ->
        let kind = match get "exitKind" with
          | `String "normal" -> Normal
          | `String "signal" -> Signal
          | `String "fatal" -> Fatal
          | `String "process_exit" -> Process_exit
          | _ -> invalid "invalid session exit kind" in
        let pending_tool_calls = match get "pendingToolCalls" with
          | `List calls -> List.map pending_tool_call calls
          | _ -> invalid "invalid pending tool calls" in
        Session_exit { kind; pending_tool_calls }
    | _ -> invalid "unsupported journal entry type" in
  { id; parent_id; timestamp; kind }

let branch_entries_at t leaf =
  let rec walk id items =
    match id with
    | None -> items
    | Some id ->
        let entry = try Hashtbl.find t.by_id id
          with Not_found -> invalid ("missing parent entry: " ^ id) in
        walk entry.parent_id (entry :: items) in
  walk leaf []

let branch_entries t = branch_entries_at t t.leaf
let entries t = List.rev t.records_rev
let leaf_id t = t.leaf
let parent_session t = match Protocol.member "parentSession" t.header with
  | `String id -> Some id | _ -> None

let latest_value entries select initial =
  List.fold_left (fun current entry ->
    match select entry.kind with None -> current | Some value -> value)
    initial entries

let model_at t leaf =
  latest_value (branch_entries_at t leaf) (function
    | Model identity -> Some (Some identity)
    | _ -> None) None

let model t = model_at t t.leaf

let thinking_at t leaf =
  latest_value (branch_entries_at t leaf) (function
    | Thinking level -> Some level | _ -> None) None

let thinking t = thinking_at t t.leaf

let disabled_tools_at t leaf =
  latest_value (branch_entries_at t leaf) (function
    | Tool_selection disabled -> Some disabled | _ -> None) []

let disabled_tools t = disabled_tools_at t t.leaf
let tool_enabled t name = not (List.mem name (disabled_tools t))

let mode_at t leaf =
  latest_value (branch_entries_at t leaf) (function
    | Mode_change mode -> Some mode | _ -> None) None

let mode t = mode_at t t.leaf

let title_at t _leaf =
  let title = latest_value (entries t) (function
    | Title title -> Some (Some title) | _ -> None) None in
  match title with
  | Some _ -> title
  | None -> (match Protocol.member "title" t.header with
      | `String value when value <> "" -> Some value | _ -> None)

let title t = title_at t t.leaf

let labels_at t leaf =
  List.fold_left (fun labels entry -> match entry.kind with
    | Label { target_id; label } ->
        (match label with
         | None -> List.remove_assoc target_id labels
         | Some text -> (target_id, text) :: List.remove_assoc target_id labels)
    | _ -> labels) [] (branch_entries_at t leaf)

let label_at t target_id =
  List.assoc_opt target_id (labels_at t t.leaf)

let labels t = labels_at t t.leaf

let label_target t =
  let entries = List.rev (branch_entries t) in
  let rec find = function
    | [] -> t.leaf
    | entry :: rest ->
        (match entry.kind with
         | Message _ | Compaction _ -> Some entry.id
         | Model _ | Thinking _ | Tool_selection _ | Mode_change _
         | Title _ | Label _ | Pin _ | Reset_boundary | Usage _ | Branch
         | Tool_lifecycle _ | Session_exit _ -> find rest) in
  find entries
let usage t =
  List.fold_left (fun total entry -> match entry.kind with
    | Usage { tokens; _ } ->
        (match total with
         | None -> Some tokens
         | Some previous -> Some (Protocol.add_usage previous tokens))
    | Message _ | Compaction _ | Model _ | Thinking _ | Tool_selection _
    | Mode_change _ | Title _ | Label _ | Pin _ | Reset_boundary | Branch
    | Tool_lifecycle _ | Session_exit _ -> total)
    None (branch_entries t)
module Usage_routes = Map.Make (struct
  type t = string * string option * string option * string
  let compare = Stdlib.compare
end)

let usage_by_route t =
  let routes = List.fold_left (fun routes entry -> match entry.kind with
    | Usage { provider; account_id; route; model; tokens } ->
        let key = provider, account_id, route, model in
        Usage_routes.update key (function
          | None -> Some tokens
          | Some previous -> Some (Protocol.add_usage previous tokens)) routes
    | Message _ | Compaction _ | Model _ | Thinking _ | Tool_selection _
    | Mode_change _ | Title _ | Label _ | Pin _ | Reset_boundary | Branch
    | Tool_lifecycle _ | Session_exit _ -> routes)
    Usage_routes.empty (branch_entries t) in
  Usage_routes.bindings routes

let messages entries =
  List.filter_map (fun entry -> match entry.kind with
    | Message message -> Some message
    | Compaction _ | Model _ | Thinking _ | Tool_selection _
    | Mode_change _ | Title _ | Label _ | Pin _ | Reset_boundary | Usage _ | Branch
    | Tool_lifecycle _ | Session_exit _ -> None) entries
let history t = messages (branch_entries t)
let retryable_history history =
  let rec find safe = function
    | [] -> None
    | (message : Protocol.message) :: earlier ->
        (match message.role, message.content, message.tool_calls with
         | "user", Some text, [] when safe && String.trim text <> "" ->
             Some (List.rev earlier, message)
         | "user", _, _ -> None
         | "assistant", _, [] -> find safe earlier
         | _ -> find false earlier) in
  find true (List.rev history)

let retry_candidate t =
  let rec find = function
    | [] -> None
    | { kind = Message ({ role = "user"; content = Some text; _ } as message);
        parent_id = Some parent; _ } :: _ when String.trim text <> "" ->
        Some (parent, message)
    | { kind = Message { role = "assistant"; tool_calls = []; _ }; _ } :: rest
    | { kind = Usage _; _ } :: rest
    | { kind = Tool_lifecycle _; _ } :: rest
    | { kind = Session_exit _; _ } :: rest
    | { kind = Title _; _ } :: rest
    | { kind = Label _; _ } :: rest
    | { kind = Pin _; _ } :: rest -> find rest
    | { kind = Reset_boundary; _ } :: _ -> None
    | _ -> None in
  find (List.rev (branch_entries t))

let context t =
  let path = branch_entries t in
  let latest = List.fold_left (fun found entry -> match entry.kind with
    | Compaction { summary; first_kept_id; provider_state } ->
        `Compaction (entry.id, summary, first_kept_id, provider_state)
    | Reset_boundary -> `Reset entry.id
    | Message _ | Model _ | Thinking _ | Tool_selection _ | Mode_change _
    | Title _ | Label _ | Pin _ | Usage _ | Branch | Tool_lifecycle _ | Session_exit _ -> found)
    `None path in
  match latest with
  | `None -> messages path
  | `Reset marker_id ->
      let rec after = function
        | [] -> invalid "reset boundary missing from branch"
        | entry :: rest when entry.id = marker_id -> messages rest
        | _ :: rest -> after rest in
      after path
  | `Compaction (marker_id, summary, first_kept_id, provider_state) ->
      let rec split before = function
        | [] -> invalid "compaction marker missing from branch"
        | entry :: after when entry.id = marker_id -> List.rev before, after
        | entry :: rest -> split (entry :: before) rest in
      let before, after = split [] path in
      let rec kept = function
        | [] -> invalid "compaction boundary missing from branch"
        | entry :: rest when entry.id = first_kept_id -> entry :: rest
        | _ :: rest -> kept rest in
      { (Protocol.user summary) with provider_state } ::
        messages (kept before @ after)


let compaction_plan t =
  let path = branch_entries t in
  let active_path = List.fold_left (fun entries entry ->
    match entry.kind with
    | Reset_boundary -> []
    | _ -> entry :: entries) [] path |> List.rev in
  let rec last_user candidate = function
    | [] -> candidate
    | { id; kind = Message { role = "user"; _ }; _ } :: rest ->
        last_user (Some id) rest
    | _ :: rest -> last_user candidate rest in
  match last_user None active_path with
  | None -> invalid "nothing to compact"
  | Some first_kept_id ->
      let rec before_last_user = function
        | [] -> invalid "compaction boundary missing from context"
        | message :: rest when message.Protocol.role = "user" -> List.rev rest
        | _ :: rest -> before_last_user rest in
      let prefix = before_last_user (List.rev (context t)) in
      if prefix = [] then invalid "nothing to compact";
      first_kept_id, prefix

let unresolved_tool_calls entries =
  let pending = ref [] in
  let update call_id state =
    pending := List.map (fun call ->
      if call.call_id = call_id then { call with state } else call) !pending in
  let remove call_id =
    if not (List.exists (fun call -> call.call_id = call_id) !pending) then
      invalid "orphan tool result";
    pending := List.filter (fun call -> call.call_id <> call_id) !pending in
  List.iter (fun entry -> match entry.kind with
    | Message { role = "assistant"; tool_calls; _ } ->
        if !pending <> [] then invalid "assistant before outstanding tool results";
        pending := List.map (fun (call : Protocol.tool_call) ->
          { call_id = call.id; name = call.name; state = Unknown }) tool_calls
    | Message { role = "tool"; tool_call_id = Some call_id; _ } -> remove call_id
    | Message { role = "user"; _ } ->
        if !pending <> [] then invalid "user before outstanding tool results"
    | Message _ -> invalid "unsupported transcript role"
    | Tool_lifecycle { call_id; state = Tool_started; _ } ->
        update call_id Started
    | Tool_lifecycle { call_id; state = Tool_settled _; _ } ->
        update call_id Settled
    | Tool_lifecycle { call_id; state = Tool_aborted { side_effects_may_have_occurred }; _ } ->
        update call_id (Aborted { side_effects_may_have_occurred })
    | Reset_boundary ->
        if !pending <> [] then invalid "reset boundary with outstanding tool calls"
    | Compaction _ | Model _ | Thinking _ | Tool_selection _
    | Mode_change _ | Title _ | Label _ | Pin _ | Usage _ | Branch | Session_exit _ -> ()) entries;
  List.rev !pending

let missing_results entries = unresolved_tool_calls entries

let append_entry t kind =
  let entry = { id = fresh_id (); parent_id = t.leaf; timestamp = timestamp (); kind } in
  append_line t (entry_json entry);
  t.records_rev <- entry :: t.records_rev;
  Hashtbl.add t.by_id entry.id entry;
  t.leaf <- Some entry.id;
  entry

let recovery_result (call : pending_tool_call) =
  match call.state with
  | Unknown ->
      "Error: prior process stopped before a durable tool result was recorded; " ^
      "execution status is unknown and side effects may have occurred. " ^
      "Do not rerun this call automatically.",
      Some (Tool_aborted { side_effects_may_have_occurred = true })
  | Started ->
      "Error: prior process stopped while this tool was running; side effects may " ^
      "have occurred. Do not rerun this call automatically.",
      Some (Tool_aborted { side_effects_may_have_occurred = true })
  | Settled ->
      "Error: tool execution was recorded as settled but its result was missing; " ^
      "side effects may have occurred. Do not rerun this call automatically.",
      None
  | Aborted { side_effects_may_have_occurred } ->
      (if side_effects_may_have_occurred then
         "Error: tool was aborted while running; side effects may have occurred. " ^
         "Do not rerun this call automatically."
       else
         "Error: tool was aborted before execution; no side effects were recorded. " ^
         "Do not rerun this call automatically."),
      None

let recover_pending_tools t =
  List.iter (fun call ->
    let result, recovery_state = recovery_result call in
    ignore (append_entry t (Message (Protocol.tool_result call.call_id result)));
    Option.iter (fun state -> ignore (append_entry t (Tool_lifecycle {
      call_id = call.call_id; name = call.name; state
    }))) recovery_state) (missing_results (branch_entries t))

let compact ?provider_state t ~summary ~first_kept_id =
  if String.trim summary = "" then invalid "empty compaction summary";
  let planned_id, _ = compaction_plan t in
  if planned_id <> first_kept_id then invalid "compaction must retain the latest user turn";
  if missing_results (branch_entries t) <> [] then invalid "unresolved tool results";
  (append_entry t (Compaction { summary; first_kept_id; provider_state })).id

let append t (message : Protocol.message) =
  (match message.role, message.content, message.tool_calls, message.tool_call_id with
   | "user", Some _, [], None | "assistant", _, _, None
   | "tool", Some _, [], Some _ -> ()
   | _ -> invalid "unsupported message");
  (try Protocol.validate_attachments message.attachments
   with Protocol.Invalid_response _ -> invalid "invalid user attachments");
  if message.attachments <> [] && message.role <> "user" then
    invalid "attachments on a non-user message";
  (append_entry t (Message message)).id

let record_tool_event t ~call_id ~name state =
  if not (valid_model_field call_id && valid_model_field name) then
    invalid "invalid tool lifecycle event";
  (append_entry t (Tool_lifecycle { call_id; name; state })).id

let record_tool_started t ~call_id ~name =
  record_tool_event t ~call_id ~name Tool_started

let record_tool_settled t ~call_id ~name ~is_error =
  record_tool_event t ~call_id ~name (Tool_settled { is_error })

let record_tool_aborted t ~call_id ~name ~side_effects_may_have_occurred =
  record_tool_event t ~call_id ~name
    (Tool_aborted { side_effects_may_have_occurred })

let pending_tool_calls t = unresolved_tool_calls (branch_entries t)

let record_exit t ~kind =
  let path = branch_entries t in
  if List.exists (function
    | { kind = Message { role = "assistant"; _ }; _ } -> true
    | _ -> false) path then
    let pending_tool_calls = unresolved_tool_calls path in
    Some (append_entry t (Session_exit { kind; pending_tool_calls })).id
  else None

let set_model t (identity : Model_identity.t) =
  let normalized =
    try Model_identity.make ~provider:identity.provider
      ?account_id:identity.account_id ~route:identity.route
      ~upstream_id:identity.upstream_id ()
    with Invalid_argument _ -> invalid "invalid model selection" in
  (match Provider_catalog.find normalized.provider with
   | Some descriptor when Provider_catalog.route descriptor normalized.route <> None -> ()
   | _ -> invalid "model selection uses an unsupported provider route");
  if model t <> Some normalized then (
    let entry = { id = fresh_id (); parent_id = t.leaf;
      timestamp = timestamp (); kind = Model normalized } in
    append_line t (entry_json entry);
    t.records_rev <- entry :: t.records_rev;
    Hashtbl.add t.by_id entry.id entry;
    t.leaf <- Some entry.id)
let clear t =
  if unresolved_tool_calls (branch_entries t) <> [] then
    invalid "cannot clear while tool calls are unresolved";
  (append_entry t Reset_boundary).id

let set_thinking t level =
  (match level with
   | None -> ()
   | Some value when valid_text_field 32 value -> ()
   | Some _ -> invalid "invalid thinking level");
  if thinking t <> level then ignore (append_entry t (Thinking level))

let set_disabled_tools t disabled =
  let disabled = List.sort_uniq String.compare disabled in
  if List.length disabled > 128 ||
     not (List.for_all valid_model_field disabled) then
    invalid "invalid disabled tools";
  if disabled_tools t <> disabled then
    ignore (append_entry t (Tool_selection disabled))

let set_mode t selected =
  if mode t <> selected then ignore (append_entry t (Mode_change selected))

let set_title t selected =
  let selected = String.trim selected in
  if not (valid_text_field 256 selected) then invalid "invalid session title";
  if selected <> Option.value ~default:"" (title t) then
    ignore (append_entry t (Title selected))
let set_label t ~target_id label =
  if not (List.exists (fun entry -> entry.id = target_id)
      (branch_entries t)) then invalid "label target is not on the selected branch";
  (match label with
   | None -> ()
   | Some value when valid_text_field 128 value -> ()
   | Some _ -> invalid "invalid entry label");
  if label_at t target_id <> label then
    ignore (append_entry t (Label { target_id; label }))

let pinned t =
  latest_value (entries t) (function
    | Pin selected -> Some selected
    | _ -> None) false

let set_pinned t selected =
  if pinned t <> selected then ignore (append_entry t (Pin selected))

let append_usage ?account_id ?route t ~provider ~model (tokens : Protocol.usage) =
  let within total = function
    | None -> true | Some detail -> detail >= 0 && detail <= total in
  let cache_details_fit = match tokens.cached_input_tokens,
    tokens.cache_creation_input_tokens with
    | Some cached, Some created ->
        cached >= 0 && created >= 0 && cached <= tokens.input_tokens &&
        created <= tokens.input_tokens - cached
    | cached, created ->
        within tokens.input_tokens cached && within tokens.input_tokens created in
  let route_valid = match route with
    | None -> true
    | Some value -> valid_model_field value in
  let account_valid = match account_id with
    | None -> true
    | Some value -> valid_model_field value in
  if not (valid_model_field provider && valid_model_field model &&
    account_valid && route_valid) ||
    tokens.input_tokens < 0 || tokens.output_tokens < 0 ||
    not cache_details_fit ||
    not (within tokens.output_tokens tokens.reasoning_output_tokens) then
    invalid "invalid provider token usage";
  let entry = { id = fresh_id (); parent_id = t.leaf;
    timestamp = timestamp ();
    kind = Usage { provider; account_id; route; model; tokens } } in
  append_line t (entry_json entry);
  t.records_rev <- entry :: t.records_rev;
  Hashtbl.add t.by_id entry.id entry;
  t.leaf <- Some entry.id

let branch t id =
  if not (Hashtbl.mem t.by_id id) then invalid ("entry not found: " ^ id);
  let marker = { id = fresh_id (); parent_id = Some id; timestamp = timestamp ();
                 kind = Branch } in
  append_line t (entry_json marker);
  t.records_rev <- marker :: t.records_rev;
  Hashtbl.add t.by_id marker.id marker;
  t.leaf <- Some id;
  recover_pending_tools t
let load_journal path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    let size = in_channel_length ic in
    if size = 0 then invalid "empty file";
    seek_in ic (size - 1);
    if input_char ic <> '\n' then invalid "truncated last entry";
    seek_in ic 0;
    let read_json () =
      try Yojson.Basic.from_string (input_line ic)
      with Yojson.Json_error _ -> invalid "invalid JSONL entry" in
    let header = read_json () in
    if Protocol.member "type" header <> `String "session"
      || Protocol.member "version" header <> `Int 1 then invalid "unsupported session header";
    (match Protocol.member "id" header, Protocol.member "timestamp" header,
      Protocol.member "cwd" header with
     | `String id, `String _, `String _ when id <> "" -> ()
     | _ -> invalid "incomplete session header");
    (match Protocol.member "parentSession" header with
     | `Null -> ()
     | `String parent when valid_text_field 128 parent -> ()
     | _ -> invalid "invalid parent session ID");
    let records = ref [] in
    let by_id = Hashtbl.create 32 in
    let seen_ids = Hashtbl.create 32 in
    let leaf = ref None in
    (try while true do
      let entry = parse_entry (read_json ()) in
      if Hashtbl.mem seen_ids entry.id then invalid "duplicate entry ID";
      Hashtbl.add seen_ids entry.id ();
      (match entry.parent_id with
       | Some parent when not (Hashtbl.mem by_id parent) ->
           invalid ("entry has missing parent: " ^ parent)
       | _ -> ());
      let is_ancestor target start =
        let rec walk = function
          | None -> false
          | Some id when id = target -> true
          | Some id -> walk (Hashtbl.find by_id id).parent_id in
        walk start in
      (match entry.kind with
       | Label { target_id; _ } ->
           if not (is_ancestor target_id entry.parent_id) then
             invalid "label target is not an ancestor"
       | _ -> ());
      (match entry.kind with
       | Message _ | Model _ | Thinking _ | Tool_selection _ | Mode_change _
       | Title _ | Label _ | Pin _ | Reset_boundary | Usage _ | Tool_lifecycle _
       | Session_exit _ ->
           Hashtbl.add by_id entry.id entry; leaf := Some entry.id
       | Compaction { first_kept_id; _ } ->
           let rec ancestor = function
             | None -> false
             | Some id when id = first_kept_id ->
                 (match (Hashtbl.find by_id id).kind with
                  | Message { role = "user"; _ } -> true | _ -> false)
             | Some id ->
                 (match (Hashtbl.find by_id id).kind with
                  | Reset_boundary -> false
                  | _ -> ancestor (Hashtbl.find by_id id).parent_id) in
           if not (ancestor entry.parent_id) then
             invalid "compaction boundary is not an ancestor user entry after reset";
           Hashtbl.add by_id entry.id entry; leaf := Some entry.id
       | Branch ->
           Hashtbl.add by_id entry.id entry;
           leaf := entry.parent_id);
      records := entry :: !records
    done with End_of_file -> ());
    { path; header; records_rev = !records; by_id; leaf = !leaf;
      disk_size = size })

let migrate_legacy ~cwd path =
  let fd = Unix.openfile path [ Unix.O_RDWR ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    Unix.lockf fd Unix.F_LOCK 0;
    Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) (fun () ->
      let opened = Unix.fstat fd and current = Unix.stat path in
      if opened.Unix.st_ino = current.Unix.st_ino
        && opened.Unix.st_dev = current.Unix.st_dev then (
        let ic = Unix.in_channel_of_descr (Unix.dup fd) in
        let legacy = Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
          if input_char ic = '[' then (
            seek_in ic 0;
            Some (Yojson.Basic.from_channel ic))
          else None) in
        match legacy with
        | None -> ()
        | Some legacy ->
            let messages = match legacy with
              | `List values -> List.map Protocol.message_from_json values
              | _ -> invalid "legacy session is not a JSON array" in
            let header = new_header cwd in
            let parent = ref None in
            let records = List.map (fun message ->
              let entry = { id = fresh_id (); parent_id = !parent;
                            timestamp = timestamp (); kind = Message message } in
              parent := Some entry.id;
              entry) messages in
            let body = line header ^ String.concat ""
              (List.map (fun entry -> line (entry_json entry)) records) in
            let temp = Filename.temp_file ~temp_dir:(Filename.dirname path)
              ".pave-migrate-" ".tmp" in
            Fun.protect ~finally:(fun () ->
              try Unix.unlink temp with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
              (fun () ->
                let dest = Unix.openfile temp [ Unix.O_WRONLY; Unix.O_TRUNC ] 0 in
                Fun.protect ~finally:(fun () -> Unix.close dest) (fun () ->
                  Unix.fchmod dest 0o600;
                  write_all dest body;
                  Unix.fsync dest);
                Unix.rename temp path))))

let rec open_file ?(cwd = Unix.getcwd ()) path =
  let exists = try
    let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
    Unix.close fd; true
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false in
  if not exists then (
    let header = new_header cwd in
    (try write_new_file path (line header)
     with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    open_file ~cwd path)
  else (
    let ic = open_in_bin path in
    let first = Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
      try Some (input_char ic) with End_of_file -> None) in
    if first = Some '[' then (
      migrate_legacy ~cwd path;
      open_file ~cwd path)
    else (
      let session = load_journal path in
      recover_pending_tools session;
      session))
let create_managed ~cwd ~directory =
  let rec create () =
    let header = new_header cwd in
    let id = match Protocol.member "id" header with
      | `String id -> id | _ -> invalid "session ID missing during create" in
    let path = Filename.concat directory (id ^ ".jsonl") in
    try
      write_new_file path (line header);
      open_file ~cwd path
    with Unix.Unix_error (Unix.EEXIST, _, _) -> create () in
  create ()


let fork_header session =
  let cwd = match Protocol.member "cwd" session.header with
    | `String cwd -> cwd | _ -> Unix.getcwd () in
  let parent_session = match Protocol.member "id" session.header with
    | `String id -> id | _ -> invalid "session ID missing during fork" in
  new_header ~parent_session cwd

let fork_content session header =
  line header ^ String.concat ""
    (List.map (fun entry -> line (entry_json entry)) (branch_entries session))

let finish_fork session path header =
  write_new_file path (fork_content session header);
  let forked = open_file path in
  (match title session with
   | Some selected when title forked <> Some selected ->
       set_title forked selected
   | _ -> ());
  if pinned forked then set_pinned forked false;
  forked

let fork session path =
  finish_fork session path (fork_header session)

let fork_managed session directory =
  let rec create () =
    let header = fork_header session in
    let id = match Protocol.member "id" header with
      | `String id -> id | _ -> invalid "session ID missing during fork" in
    let path = Filename.concat directory (id ^ ".jsonl") in
    try finish_fork session path header with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> create () in
  create ()
