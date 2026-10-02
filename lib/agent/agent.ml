type workspace_effect = Session_rewind.workspace_effect
type phase = Model | Tool of string

type tool_event =
  | Tool_draft of Protocol.tool_argument_delta
  | Tool_draft_ended of { key : string; call_id : string option; valid : bool }
  | Tool_started of {
      call_id : string; name : string; target : string option;
      write_content : string option
    }
  | Tool_executing of { call_id : string; name : string }
  | Tool_updated of { call_id : string; name : string; received_bytes : int }
  | Tool_settled of {
      call_id : string; name : string; result : string; is_error : bool;
      elapsed_ms : int option
    }
  | Tool_aborted of {
      call_id : string;
      name : string;
      result : string;
      side_effects_may_have_occurred : bool;
      elapsed_ms : int option;
    }

(* Stage events journal one named phase's elapsed time (turn boundary, model
   request, tool execution); the consumer persists them as timeline records. *)
type stage = { name : string; elapsed_ms : int; detail : string option }

type draft_metadata = {
  scoped_key : string;
  mutable draft_call_id : string option;
  mutable draft_name : string;
}

type t = {
  secret_mask : Secret_mask.t option;
  provider : Provider.config;
  authentication : Provider.authentication;
  resolve_credential : (unit -> Provider.credentials) option;
  thinking : unit -> string option;
  max_output_tokens : int option;
  root : string;
  workspace_context : Tools.session_context option;
  allow_shell : bool;
  tool_available : string -> bool;
  external_tools : Yojson.Basic.t list;
  execute_external : (name:string -> args:Yojson.Basic.t ->
    cancel:(unit -> bool) -> (string, string) result) option;
  validate_external_tool : (name:string -> args:Yojson.Basic.t ->
    (unit, string) result) option;
  external_approval_details : (string -> string list) option;
  delegate_task : (cancel:(unit -> bool) -> label:string -> task:string ->
    model:string option -> string) option;
  stream : bool;
  preview_tools : bool;
  mutable request_serial : int;
  approval_mode : Approval.mode;
  tool_approval : (string * Approval.policy) list;
  mutable command_patterns : Approval.command_rule list;
  approve_command : string -> bool;
  approve_tool : (Approval.request -> bool) option;
  system : string;
  mutable scoped_pending : (string * string) list;
  mutable history_rev : Protocol.message list;
  on_event : string -> unit;
  on_delta : string -> unit;
  on_change : Protocol.message -> unit;
  on_usage : (Protocol.usage -> unit) option;
  on_phase : (phase -> unit) option;
  on_tool_event : (tool_event -> unit) option;
  on_workspace_effect : (workspace_effect -> unit) option;
  on_stage : (stage -> unit) option;
  before_request : (cancel:(unit -> bool) option ->
    system:string -> messages:Protocol.message list ->
    tools:Yojson.Basic.t list -> Protocol.message list option) option;
}
let create ~provider ~root ~system ?workspace_context
    ?(authentication = Provider.Api_key)
    ?resolve_credential ?secret_mask ?before_request ?(history = [])
    ?(thinking = fun () -> None) ?max_output_tokens ?(allow_shell = false)
    ?(tool_available = fun _ -> true) ?delegate_task ?(stream = false)
    ?(preview_tools = false)
    ?(external_tools = []) ?execute_external ?validate_external_tool
    ?external_approval_details
    ?(approval_mode = Approval.Ask_exec) ?(tool_approval = [])
    ?(command_patterns = []) ?(approve_command = fun _ -> false)
    ?approve_tool ?on_usage ?on_phase ?on_tool_event ?on_stage ?on_workspace_effect
    ?(on_change = fun _ -> ())
    ?(on_delta = fun _ -> ()) ~on_event () =
  let redact = match secret_mask with
    | Some mask -> Secret_mask.redact mask
    | None -> Fun.id in
  { provider; authentication; resolve_credential; thinking; max_output_tokens;
    root; workspace_context;
    system; secret_mask;
    allow_shell; tool_available; external_tools; execute_external;
    validate_external_tool; external_approval_details;
    delegate_task; stream; preview_tools; request_serial = 0; approval_mode;
    tool_approval; command_patterns;
    approve_command = (fun command -> approve_command (redact command));
    approve_tool = Option.map (fun approve (request : Approval.request) ->
      approve { request with
        tool_name = redact request.tool_name; impact = redact request.impact;
        trigger = request.trigger;
        details = List.map redact request.details;
        reason = Option.map redact request.reason }) approve_tool;
    before_request;
    history_rev = List.rev history; scoped_pending = [];
    on_change; on_delta = (fun text -> on_delta (redact text));
    on_event = (fun text -> on_event (redact text));
    on_usage; on_phase; on_stage;
    on_tool_event = Option.map (fun notify event ->
      let event = match event with
        | Tool_draft _ | Tool_draft_ended _ -> event
        | Tool_started { call_id; name; target; write_content } -> Tool_started {
            call_id = redact call_id; name = redact name;
            target = Option.map redact target;
            write_content = Option.map redact write_content }
        | Tool_executing { call_id; name } ->
            Tool_executing { call_id = redact call_id; name = redact name }
        | Tool_updated { call_id; name; received_bytes } -> Tool_updated {
            call_id = redact call_id; name = redact name; received_bytes }
        | Tool_settled { call_id; name; result; is_error; elapsed_ms } ->
            Tool_settled {
              call_id = redact call_id; name = redact name;
              result = redact result; is_error; elapsed_ms }
        | Tool_aborted { call_id; name; result; side_effects_may_have_occurred;
            elapsed_ms } ->
            Tool_aborted {
              call_id = redact call_id; name = redact name;
              result = redact result; side_effects_may_have_occurred;
              elapsed_ms } in
      notify event) on_tool_event;
    on_workspace_effect }

let task_definition = `Assoc [
  "type", `String "function";
  "function", `Assoc [
    "name", `String "task";
    "description", `String
      "Start a bounded read-only child agent. It cannot edit files or run shell commands. The result is saved as a session-owned artifact. The optional model selects a tier name (inherit, light, heavy, fastapply) or a provider@route/MODEL selector; inherit or omission uses the parent model.";
    "parameters", `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "label", `Assoc ["type", `String "string"; "maxLength", `Int 256];
        "task", `Assoc ["type", `String "string"; "maxLength", `Int 8192];
        "model", `Assoc ["type", `String "string"; "maxLength", `Int 256;
          "description", `String "Tier name (inherit, light, heavy, fastapply) resolved via the modelTiers setting, or a provider@route[#account]/MODEL selector"]];
      "required", `List [`String "label"; `String "task"];
      "additionalProperties", `Bool false]]]


let mask_tool_calls mask calls =
  List.map (fun (call : Protocol.tool_call) ->
    { call with arguments =
        Secret_mask.mask_tool_arguments mask call.arguments }) calls

let messages t = List.rev t.history_rev
let append t (message : Protocol.message) =
  let message = match t.secret_mask with
    | Some mask ->
        let content = Option.map (Secret_mask.mask mask) message.content in
        let tool_result_content = Option.map (List.map (function
          | Protocol.Text text -> Protocol.Text (Secret_mask.mask mask text)
          | Protocol.Image _ as image -> image)) message.tool_result_content in
        let tool_calls = mask_tool_calls mask message.tool_calls in
        { message with content; tool_result_content; tool_calls }
    | None -> message in
  t.on_change message;
  t.history_rev <- message :: t.history_rev

let milliseconds since =
  max 0 (int_of_float ((Unix.gettimeofday () -. since) *. 1000.))

let emit_stage t name ~since ?detail () =
  match t.on_stage with
  | None -> ()
  | Some notify ->
      (* Details may embed provider error text; cap them and run them through
         the secret mask before they reach the journal or a recording. *)
      let detail = Option.map (fun detail ->
        let detail = match t.secret_mask with
          | Some mask -> Secret_mask.redact mask detail
          | None -> detail in
        if String.length detail <= 512 then detail
        else String.sub detail 0 512 ^ "…") detail in
      (try notify { name; elapsed_ms = milliseconds since; detail }
       with _ -> ())

let emit_tool_event t event =
  match t.on_tool_event with
  | Some notify -> notify event
  | None ->
      (match event with
       | Tool_started { name; _ } -> t.on_event ("[" ^ name ^ "]")
       | Tool_draft _ | Tool_draft_ended _ | Tool_executing _ | Tool_updated _ -> ()
       | Tool_settled { name; result; _ }
       | Tool_aborted { name; result; _ } ->
           t.on_event ("[" ^ name ^ "] " ^ result))

let non_reversible_notice =
  "External, process, network, or clipboard effects may be non-reversible; /rewind does not undo them."

let max_scoped_context_bytes = Project_context.max_total_bytes

let file_mutation (call : Protocol.tool_call) =
  match call.name with
  | "write_file" | "edit_file" | "apply_edits" -> true
  | "ast_edit" -> Protocol.member "dry_run" call.arguments = `Bool false
  | "lsp" -> Protocol.member "action" call.arguments = `String "apply_preview"
  | _ -> false

let file_scope ?cancel t call =
  let resolve_path path =
    match path with
    | path when String.starts_with ~prefix:"https://" path ||
        String.starts_with ~prefix:"http://" path ||
        String.starts_with ~prefix:"artifact://" path -> Ok (path, "", None)
    | path ->
        (match Tools.resolve_file_location ?cancel
            ?context:t.workspace_context ~root:t.root path with
         | None when file_mutation call ->
             Error "Error: file mutation requires a workspace or owned worktree path"
         | None -> Ok (path, "", None)
         | Some location ->
             let normalized = Project_context.normalize location.path in
             if (normalized = "" || normalized = ".") && file_mutation call then
               Error "Error: a workspace-relative file path is required; file operation not executed"
             else
               let scope = Project_context.resolve_scoped
                 ~root:location.root ~path:normalized () in
               if scope.safe then
                 let key = match location.worktree_id with
                   | Some id -> "worktree://" ^ id ^ "/" ^ normalized
                   | None -> normalized in
                 Ok (key, scope.text, Some { location with path = normalized })
               else
                 let codes = List.map (fun (d : Project_context.diagnostic) -> d.code)
                   scope.diagnostics |> List.sort_uniq String.compare in
                 Error ("Error: scoped instructions could not be resolved (" ^
                   String.concat ", " codes ^ "); file operation not executed")) in
  try
    let paths =
      if call.name = "lsp" &&
         Protocol.member "action" call.arguments = `String "apply_preview" then
        Tools.lsp_preview_paths ?context:t.workspace_context ~root:t.root
          call.arguments
      else
        match Protocol.member "path" call.arguments with
        | `String path -> [path]
        | _ -> [] in
    if paths = [] then
      Error "Error: a workspace-relative file path is required; file operation not executed"
    else
      let rec resolve reversed = function
        | [] -> Ok (List.rev reversed)
        | path :: rest ->
            (match resolve_path path with
             | Error _ as error -> error
             | Ok scope -> resolve (scope :: reversed) rest) in
      resolve [] paths
  with
  | Tools.Tool_error message -> Error ("Error: " ^ message)
  | Workspace_git.Error message ->
      (match cancel with
       | Some cancelled when cancelled () -> raise Tools.Cancelled
       | _ -> Error ("Error: " ^ message))
  | Unix.Unix_error _ | Sys_error _ | Invalid_argument _ | Failure _ ->
      Error "Error: scoped instructions unavailable; file operation not executed"

let queue_scope t path text =
  if text = "" then true else
  let pending = List.remove_assoc path t.scoped_pending in
  let size = List.fold_left (fun total (_, content) ->
    total + String.length content) (String.length text) pending in
  if size > max_scoped_context_bytes then false
  else (t.scoped_pending <- (path, text) :: pending; true)

let run ?(max_turns = 20) ?cancel ?(attachments = []) t text =
  if String.trim text = "" then invalid_arg "empty prompt";
  if max_turns <= 0 then invalid_arg "max_turns must be positive";
  Provider.check_cancel cancel;
  append t (Protocol.user ~attachments text);
  let rec turn remaining =
    Provider.check_cancel cancel;
    if remaining = 0 then failwith "tool-call limit reached; inspect workspace before continuing";
    let visible = t.scoped_pending in
    let scoped = List.rev visible |> List.map (fun (path, text) ->
      Printf.sprintf "For workspace file %S:\n%s" path text) in
    let system_text = if scoped = [] then t.system else
      t.system ^ "\n\nPath-scoped project instructions (lower priority than mobile safety):\n" ^
      String.concat "\n\n" scoped in
    let definitions = Tools.available_for ~allow_shell:t.allow_shell
      ~enabled:t.tool_available in
    let definitions = match t.delegate_task with
      | Some _ when t.tool_available "task" -> definitions @ [task_definition]
      | _ -> definitions in
    let definitions = definitions @ List.filter (fun definition ->
      match Protocol.member "function" definition with
      | `Assoc fields -> (match List.assoc_opt "name" fields with
          | Some (`String name) -> t.tool_available name
          | _ -> false)
      | _ -> false) t.external_tools in
    let system : Protocol.message =
      { role = "system"; content = Some system_text; tool_calls = [];
        tool_call_id = None; tool_result_content = None; provider_state = None;
        attachments = [] } in
    (match t.on_phase with None -> () | Some notify -> notify Model);
    let mask = match t.secret_mask with
      | Some mask -> Secret_mask.mask mask
      | None -> Fun.id in
    let mask_message (message : Protocol.message) =
      let content = Option.map mask message.content in
      let tool_result_content = Option.map (List.map (function
        | Protocol.Text text -> Protocol.Text (mask text)
        | Protocol.Image _ as image -> image)) message.tool_result_content in
      let tool_calls = Option.fold ~none:message.tool_calls
        ~some:(fun secret_mask -> mask_tool_calls secret_mask message.tool_calls)
        t.secret_mask in
      { message with content; tool_result_content; tool_calls } in
    let request_messages = match t.before_request with
      | None -> messages t
      | Some prepare ->
          let current = List.map mask_message (messages t) in
          (match prepare ~cancel ~system:(mask system_text)
            ~messages:current ~tools:definitions with
           | None -> current
           | Some replacement ->
               let replacement = List.map mask_message replacement in
               t.history_rev <- List.rev replacement;
               replacement) in
    let transcript = { system with content = Option.map mask system.content } ::
      List.map mask_message request_messages in
    let streamed_text = Buffer.create 128 in
    let on_text = match t.secret_mask with
      | Some _ when t.stream -> fun text -> Buffer.add_string streamed_text text
      | _ -> t.on_delta in
    let drafts =
      if t.stream && t.preview_tools && Option.is_some t.on_tool_event &&
        Option.is_none t.secret_mask then Some (Hashtbl.create 4)
      else None in
    let request = t.request_serial in
    t.request_serial <- request + 1;
    let on_tool_arguments = Option.map (fun drafts ->
      fun (delta : Protocol.tool_argument_delta) ->
        let metadata = match Hashtbl.find_opt drafts delta.key with
          | Some metadata -> Some metadata
          | None when Hashtbl.length drafts < 128 ->
              let metadata = {
                scoped_key = Printf.sprintf "%d:%s" request delta.key;
                draft_call_id = delta.call_id; draft_name = delta.name } in
              Hashtbl.add drafts delta.key metadata; Some metadata
          | None -> None in
        Option.iter (fun metadata ->
          if Option.is_some delta.call_id then metadata.draft_call_id <- delta.call_id;
          metadata.draft_name <- delta.name;
          emit_tool_event t (Tool_draft { delta with key = metadata.scoped_key }))
          metadata) drafts in
    let end_drafts calls = Option.iter (fun drafts ->
      Hashtbl.iter (fun _ metadata ->
        let call = Option.bind metadata.draft_call_id (fun id ->
          List.find_opt (fun (call : Protocol.tool_call) ->
            call.id = id && call.name = metadata.draft_name) calls) in
        let valid = match call with
          | Some call when call.name = "write_file" ->
              (try Tools.validate_arguments ~name:call.name
                  ~args:(Tools.normalize_tool_arguments ~name:call.name ~args:call.arguments);
                true with Tools.Tool_error _ -> false)
          | Some _ -> true | None -> false in
        emit_tool_event t (Tool_draft_ended {
          key = metadata.scoped_key;
          call_id = Option.map (fun (call : Protocol.tool_call) -> call.id) call;
          valid })) drafts;
      Hashtbl.clear drafts) drafts in
    let model_started = Unix.gettimeofday () in
    let reply =
      try
        let reply =
          if t.stream then Provider.complete ~authentication:t.authentication
            ?resolve_credential:t.resolve_credential ?thinking:(t.thinking ())
            ?max_output_tokens:t.max_output_tokens
            ~on_text ?on_tool_arguments ?on_usage:t.on_usage ?cancel
            t.provider transcript definitions
          else Provider.complete ~authentication:t.authentication
            ?resolve_credential:t.resolve_credential ?thinking:(t.thinking ())
            ?max_output_tokens:t.max_output_tokens
            ?on_usage:t.on_usage ?cancel t.provider transcript definitions in
        Provider.check_cancel cancel;
        end_drafts reply.tool_calls;
        emit_stage t "model" ~since:model_started ();
        reply
      with exn ->
        end_drafts [];
        emit_stage t "model" ~since:model_started
          ~detail:(Printexc.to_string exn) ();
        raise exn in
    (match t.secret_mask with
     | Some _ when t.stream && Buffer.length streamed_text > 0 ->
         t.on_delta (Buffer.contents streamed_text)
     | _ -> ());
    t.scoped_pending <- [];
    (match reply.content with
     | Some s when s <> "" ->
         if t.stream then t.on_delta "\n" else t.on_event s
     | _ -> ());
    Provider.check_cancel cancel;
    match reply.tool_calls with
    | [] ->
        (* An empty assistant turn is rejected when history is replayed
           (Anthropic, Responses, Chat Completions), so it is not retained. *)
        if Option.value ~default:"" reply.content = "" then
          t.on_event "The model finished without a reply."
        else append t reply;
        (match reply.content with
         | Some text -> (match t.secret_mask with
             | Some mask -> Secret_mask.redact mask text
             | None -> text)
         | None -> "")
    | calls ->
        append t reply;
        let cancellation_result =
          "Error: turn cancelled before this tool ran; do not assume it executed" in
        let abort (call : Protocol.tool_call) result side_effects_may_have_occurred =
          emit_tool_event t (Tool_aborted {
            call_id = call.id; name = call.name; result;
            side_effects_may_have_occurred; elapsed_ms = None
          }) in
        let calls = Array.of_list calls in
        let tool_started_at = Array.make (Array.length calls) 0. in
        (* End time is captured when the tool body returns, so queue/delivery
           latency in the scheduler does not inflate the reported duration. *)
        let tool_ended_at = Array.make (Array.length calls) 0. in
        let skipped result start =
          for index = start to Array.length calls - 1 do
            let call = calls.(index) in
            abort call result false;
            append t (Protocol.tool_result_blocks call.id [Protocol.Text result])
          done in
        let cancellation_requested = ref false and scheduler_failure = ref None in
        let elapsed index =
          let start = tool_started_at.(index) in
          let stop = tool_ended_at.(index) in
          if start <= 0. || stop <= 0. then None
          else Some (max 0 (int_of_float ((stop -. start) *. 1000.))) in
        let make_task index (call : Protocol.tool_call) :
            (Protocol.content_block list, string) result Tool_scheduler.task =
          let call = match t.secret_mask with
            | Some mask -> { call with arguments =
                call.arguments
                |> Secret_mask.mask_tool_arguments mask
                |> Secret_mask.restore_tool_arguments mask }
            | None -> call in
          let call = { call with arguments =
            match List.find_opt (fun definition ->
              Protocol.member "name" (Protocol.member "function" definition) =
                `String call.name) (task_definition :: t.external_tools) with
            | Some definition -> Tools.normalize_arguments call.arguments
                ~schema:(Protocol.member "parameters" (Protocol.member "function" definition))
            | None -> Tools.normalize_tool_arguments ~name:call.name ~args:call.arguments } in
          let prepared = ref None in
          let tracked_path : Tools.file_location option ref = ref None in
          let on_progress = match call.name, t.on_tool_event with
            | "run_command", Some _ ->
                Some (fun received_bytes ->
                  emit_tool_event t (Tool_updated {
                    call_id = call.id; name = call.name; received_bytes
                  }))
            | _ -> None in
          let complete message =
            Tool_scheduler.Complete (Error message) in
          let prepare () =
            let argument key = match Protocol.member key call.arguments with
              | `String value -> Some value
              | _ -> None in
            (* The one argument that tells the user what this call acts on. *)
            let target = match call.name with
              | "read_file" | "write_file" | "edit_file" | "apply_edits"
              | "list_files" -> argument "path"
              | "glob" | "search" | "grep" -> argument "pattern"
              | "run_command" -> argument "command"
              | "web_search" -> argument "query"
              | "web_fetch" -> argument "url"
              | "browser" ->
                  (match argument "action" with
                   | Some ("navigate") -> argument "url"
                   | Some action -> Some action
                   | None -> None)
              | _ -> None in
            let write_content = if call.name = "write_file" &&
              t.preview_tools && Option.is_some t.on_tool_event then
              try
                Tools.validate_arguments ~name:call.name ~args:call.arguments;
                match Protocol.member "content" call.arguments with
                | `String content -> Some content | _ -> None
              with Tools.Tool_error _ -> None
              else None in
            emit_tool_event t (Tool_started {
              call_id = call.id; name = call.name; target; write_content
            });
            Provider.check_cancel cancel;
            (match t.on_phase with
             | None -> ()
             | Some notify -> notify (Tool call.name));
            try
              match Protocol.member Protocol.invalid_arguments_key call.arguments with
              | `String received ->
                  complete ("Error: tool arguments were not a valid JSON object; " ^
                    "resend the call with one JSON object matching the tool schema. Received: " ^
                    received)
              | _ ->
              if call.name = "task" then (
                if not (t.tool_available "task") then
                  complete "Error: child-agent delegation is no longer available"
                else match t.delegate_task with
                | None -> complete "Error: child-agent delegation is unavailable"
                | Some delegate ->
                    let valid_fields = match call.arguments with
                      | `Assoc fields ->
                          (List.length fields = 2 || List.length fields = 3) &&
                          List.sort String.compare (List.map fst fields) =
                            (if List.length fields = 3
                             then ["label"; "model"; "task"]
                             else ["label"; "task"]) &&
                          List.for_all (function
                            | ("label" | "task" | "model"), `String _ -> true
                            | _ -> false) fields
                      | _ -> false in
                    let argument name = match Protocol.member name call.arguments with
                      | `String value -> Some value | _ -> None in
                    let model = match argument "model" with
                      | None | Some "" -> None
                      | Some value when String.length value <= 256 &&
                          not (String.exists (fun c ->
                            Char.code c < 32 || Char.code c = 127) value) ->
                          Some value
                      | Some _ -> Some "\xffinvalid" in
                    if not valid_fields then
                      complete "Error: task requires label, task and optional model strings"
                    else match argument "label", argument "task", model with
                    | Some label, Some task, model
                      when model <> Some "\xffinvalid" &&
                        String.trim label <> "" && String.length label <= 256 &&
                        not (String.exists (fun c ->
                          Char.code c < 32 || Char.code c = 127) label) &&
                        String.trim task <> "" && String.length task <= 8192 &&
                        not (String.contains task (Char.chr 0)) ->
                        prepared := Some (fun ?cancel ?on_progress:_ ?approved:_ () ->
                          Provider.check_cancel cancel;
                          let job_id = delegate
                            ~cancel:(Option.value ~default:(fun () -> false) cancel)
                            ~label ~task ~model in
                          Ok [Protocol.Text ("Started read-only child job " ^
                            job_id ^ ".")]);
                        Tool_scheduler.Run
                    | _ -> complete
                        "Error: task requires a single-line label, a nonempty task of at most 8192 bytes, and an optional valid model selector")
              else if List.exists (fun definition ->
                Protocol.member "name" (Protocol.member "function" definition) =
                  `String call.name) t.external_tools then
                (match t.execute_external with
                 | None -> complete "Error: external tool is unavailable"
                 | Some _ when not (t.tool_available call.name) ->
                     complete "Error: external tool is no longer available"
                 | Some execute ->
                     (match t.validate_external_tool with
                      | None -> complete "Error: external tool validator is unavailable"
                      | Some validate ->
                          (match validate ~name:call.name ~args:call.arguments with
                           | Error message -> complete ("Error: " ^ message)
                           | Ok () ->
                               prepared := Some (fun ?cancel ?on_progress:_
                                   ?approved:_ () ->
                                 match execute ~name:call.name ~args:call.arguments
                                   ~cancel:(Option.value
                                     ~default:(fun () -> false) cancel) with
                                 | Ok text -> Ok [Protocol.Text text]
                                 | Error message -> Error ("Error: " ^ message));
                               Tool_scheduler.Run)))
              else
                match Tools.prepare ?cancel ?context:t.workspace_context
                  ~root:t.root ~name:call.name ~args:call.arguments () with
                | Error result -> complete result
                | Ok execute ->
                    if Tools.is_shell_tool call.name && not t.allow_shell then
                      complete "Error: shell execution disabled; ask the user to restart with --allow-shell"
                    else if not (t.tool_available call.name) then
                      complete "Error: tool is no longer available"
                    else if List.mem call.name
                      ["write_file"; "edit_file"; "read_file"; "workspace_snapshot";
                       "apply_edits"; "ast_edit"] ||
                      (call.name = "lsp" &&
                       (Protocol.member "path" call.arguments <> `Null ||
                        Protocol.member "action" call.arguments = `String "apply_preview")) then
                      (match file_scope ?cancel t call with
                       | Error message -> complete message
                       | Ok scopes ->
                           let mutating = file_mutation call in
                           if mutating && call.name <> "lsp" then
                             tracked_path := (match scopes with
                               | (_, _, location) :: _ -> location
                               | [] -> None);
                           let unseen = List.filter (fun (scope_key, scoped, _) ->
                             scoped <> "" &&
                             List.assoc_opt scope_key visible <> Some scoped) scopes in
                           if unseen <> [] then (
                             let queued = List.fold_left (fun ok (scope_key, scoped, _) ->
                               let added = queue_scope t scope_key scoped in
                               added && ok) true unseen in
                             if not queued then complete
                               ("Error: scoped instructions exceed the per-turn context limit; " ^
                                "file operation not executed")
                             else if mutating then complete
                               ("Error: file mutation withheld until path-scoped instructions " ^
                                "for every target are presented as system context; retry this tool call next turn")
                             else (
                               prepared := Some execute;
                               Tool_scheduler.Run))
                           else (
                             prepared := Some execute;
                             Tool_scheduler.Run))
                    else (
                      prepared := Some execute;
                      Tool_scheduler.Run)
            with
            | Provider.Cancelled -> raise Provider.Cancelled
            | Tools.Cancelled -> raise Tools.Cancelled
            | exn -> complete ("Error: " ^ Printexc.to_string exn) in
          let run () =
            let execute = match !prepared with
              | Some execute -> execute
              | None -> assert false in
            tool_started_at.(index) <- Unix.gettimeofday ();
            Fun.protect ~finally:(fun () ->
              tool_ended_at.(index) <- Unix.gettimeofday ()) (fun () ->
            try
              Provider.check_cancel cancel;
              let decision = Tools.approval_decision
                ~command_patterns:t.command_patterns ~name:call.name
                ~args:call.arguments in
              let resolution = Approval.resolve ~mode:t.approval_mode
                ~decision
                ~user_policy:(List.assoc_opt call.name t.tool_approval) in
              let shell = call.name = "run_command" || call.name = "start_shell" in
              match resolution with
              | Approval.Denied reason ->
                  Error ("Error: " ^ reason)
              | (Approval.Allowed | Approval.Requires_prompt _) as resolved ->
                  let reason = match resolved with
                    | Approval.Requires_prompt reason -> reason
                    | Approval.Allowed -> decision.reason
                    | Approval.Denied _ -> assert false in
                  let delegate = call.name = "task" in
                  let external_tool = List.exists (fun definition ->
                    Protocol.member "name" (Protocol.member "function" definition) =
                      `String call.name) t.external_tools in
                  let explicit_prompt = external_tool || Tools.requires_explicit_approval
                    ~name:call.name ~args:call.arguments in
                  (* A persisted exact-command Allow rule is the recorded user
                     grant; it exempts the per-command shell prompt. *)
                  let rule_allows = decision.Approval.policy = Some Approval.Allow in
                  let prompt_required = delegate ||
                    (explicit_prompt && not rule_allows) ||
                    (shell && not rule_allows) ||
                    (match resolved with
                     | Approval.Requires_prompt _ -> true
                     | _ -> false) in
                  let request = if external_tool then
                    { Approval.tool_name = call.name; tier = Approval.Exec;
                      trigger = Approval.Tool_call;
                      impact = "Runs an explicitly selected external tool; process or network effects may be non-reversible.";
                      details = ["Workspace: " ^ t.root] @
                        Option.fold ~none:[] ~some:(fun describe ->
                          describe call.name) t.external_approval_details @
                        ["Arguments: " ^ Tools.preview_text
                          (Yojson.Basic.to_string call.arguments)];
                      reason = Some "External tools require per-call interactive approval." }
                  else if delegate then
                    { Approval.tool_name = "task"; tier = Approval.Exec;
                      trigger = Approval.Tool_call;
                      impact = "Starts a bounded read-only child agent; provider usage may be billed.";
                      details = [
                        "Label: " ^ Option.value ~default:"(missing)"
                          (match Protocol.member "label" call.arguments with
                           | `String value -> Some value | _ -> None);
                        "Task: " ^ Tools.preview_text
                          (Option.value ~default:"(missing)"
                            (match Protocol.member "task" call.arguments with
                             | `String value -> Some value | _ -> None))];
                      reason = Some "Child-agent work requires explicit approval." }
                  else
                    let args = match t.secret_mask, call.name, call.arguments with
                      | Some mask, "write_file", `Assoc fields ->
                          `Assoc (List.map (function
                            | "content", `String value ->
                                "content", `String (Secret_mask.redact mask value)
                            | field -> field) fields)
                      | _ -> call.arguments in
                    let request = Tools.approval_request ?cancel
                      ?context:t.workspace_context ~root:t.root
                      ~name:call.name ~args decision in
                    (* Redact complete path values before quoting; resolution above
                       must continue to use the actual workspace path. *)
                    match t.secret_mask, call.name, request.details with
                    | Some mask, "write_file", _ :: details ->
                        let path = match Protocol.member "path" call.arguments with
                          | `String path -> Secret_mask.redact mask path
                          | _ -> "(missing)" in
                        { request with details =
                          ("Path: " ^ Printf.sprintf "%S" path) :: details }
                    | _ -> request in
                  let request = { request with
                    Approval.reason = (match reason with
                      | Some _ -> reason
                      | None -> request.reason) } in
                  let approved =
                    if not prompt_required then true
                    else match t.approve_tool with
                      | Some approve -> approve request
                      | None when call.name = "run_command" ->
                          (match Protocol.member "command" call.arguments with
                           | `String command -> t.approve_command command
                           | _ -> false)
                      | None -> false in
                  if not approved then
                    Error (if shell then
                      "Error: command not approved"
                      else "Error: tool approval denied")
                  else (
                    if external_tool || Tools.non_reversible_tool
                      ~name:call.name ~args:call.arguments then (
                      let action = match Protocol.member "action" call.arguments with
                        | `String action -> " (" ^ action ^ ")"
                        | _ -> "" in
                      let detail = if call.name = "run_command" then
                        "Shell command was attempted; workspace and external side effects may have occurred. /rewind does not reverse shell effects."
                      else
                        call.name ^ action ^
                          " was attempted; external, process, network, or clipboard side effects may have occurred. /rewind does not reverse this action." in
                      Option.iter (fun notify ->
                        try notify (Session_rewind.Non_reversible_effect {
                          tool_name = call.name; detail })
                        with exn -> t.on_event
                          ("Action started, but rewind tracking failed; treat it as non-reversible: " ^
                           Printexc.to_string exn)) t.on_workspace_effect;
                      t.on_event non_reversible_notice);
                    let before = match !tracked_path, t.on_workspace_effect with
                      | Some location, Some _ when location.worktree_id = None ->
                          Some (location, Session_rewind.snapshot_file
                            ~root:location.root ~path:location.path)
                      | _ -> None in
                    Provider.check_cancel cancel;
                    if call.name = "write_file" then
                      emit_tool_event t (Tool_executing {
                        call_id = call.id; name = call.name });
                    let content = execute ?cancel ?on_progress ~approved () in
                    let failed = Result.is_error content in
                    (match !tracked_path, before with
                     | Some location, Some (_, before) when not failed ->
                         (try
                            let after = Session_rewind.snapshot_file
                              ~root:location.root ~path:location.path in
                            match t.on_workspace_effect with
                            | Some notify ->
                                notify (Session_rewind.File_change {
                                  tool_name = call.name; path = location.path; before; after })
                            | None -> ()
                          with exn -> t.on_event
                            ("Workspace file change completed, but rewind tracking could not be confirmed: " ^
                             Printexc.to_string exn))
                     | Some location, None when not failed ->
                         (match location.worktree_id with
                          | Some id ->
                              let detail = Printf.sprintf
                                "File change in session-owned worktree %s at %s is outside workspace snapshots; /rewind does not restore it."
                                id (Tools.preview_text location.path) in
                              (match t.on_workspace_effect with
                               | Some notify ->
                                   (try notify (Session_rewind.Non_reversible_effect {
                                      tool_name = call.name; detail })
                                    with exn -> t.on_event
                                      ("Worktree file change completed, but rewind tracking failed: " ^
                                       Printexc.to_string exn))
                               | None -> t.on_event detail)
                          | None ->
                              t.on_event "Workspace file changed in an unsaved session; no durable rewind record exists.")
                     | _ -> ());
                    content)
            with
            | Provider.Cancelled -> raise Provider.Cancelled
            | Tools.Cancelled -> raise Tools.Cancelled
            | exn -> Error ("Error: " ^ Printexc.to_string exn)) in
          { Tool_scheduler.mode = Tools.execution_mode call.name;
            prepare; run } in
        let tasks = Array.mapi make_task calls in
        let on_complete index outcome =
          let (call : Protocol.tool_call) = calls.(index) in
          match outcome with
          | Tool_scheduler.Completed outcome ->
              let content, is_error = match outcome with
                | Ok content -> content, false
                | Error message -> [Protocol.Text message], true in
              let result = Protocol.display_content_blocks content in
              emit_tool_event t (Tool_settled {
                call_id = call.id; name = call.name; result; is_error;
                elapsed_ms = elapsed index
              });
              append t (Protocol.tool_result_blocks call.id content)
          | Tool_scheduler.Failed Tools.Cancelled ->
              cancellation_requested := true;
              let result =
                "Error: command cancelled while running; side effects may have occurred" in
              abort call result true;
              append t (Protocol.tool_result_blocks call.id [Protocol.Text result])
          | Tool_scheduler.Failed Provider.Cancelled ->
              cancellation_requested := true;
              abort call cancellation_result false;
              append t (Protocol.tool_result_blocks call.id
                [Protocol.Text cancellation_result])
          | Tool_scheduler.Failed exn ->
              scheduler_failure := Some exn;
              let result = "Error: " ^ Printexc.to_string exn in
              emit_tool_event t (Tool_settled {
                call_id = call.id; name = call.name; result; is_error = true;
                elapsed_ms = elapsed index
              });
              append t (Protocol.tool_result_blocks call.id [Protocol.Text result])
          | Tool_scheduler.Skipped -> () in
        let cancelled () = match cancel with
          | Some check -> check ()
          | None -> false in
        let tools_started = Unix.gettimeofday () in
        let outcomes = Tool_scheduler.run ~cancelled ~on_complete tasks in
        emit_stage t "tools" ~since:tools_started ();
        let rec first_skipped index =
          if index >= Array.length outcomes then None
          else match outcomes.(index) with
            | Tool_scheduler.Skipped -> Some index
            | _ -> first_skipped (index + 1) in
        (match first_skipped 0 with
         | Some index ->
             let result = match !scheduler_failure with
               | Some _ ->
                   "Error: tool scheduler stopped; do not assume remaining calls executed"
               | None -> cancellation_result in
             skipped result index;
             (match !scheduler_failure with
              | Some exn -> raise exn
              | None -> raise Provider.Cancelled)
         | None ->
             match !scheduler_failure with
             | Some exn -> raise exn
             | None when !cancellation_requested -> raise Provider.Cancelled
             | None -> turn (remaining - 1))
  in
  let turn_started = Unix.gettimeofday () in
  try
    let result = turn max_turns in
    emit_stage t "turn" ~since:turn_started ();
    result
  with exn ->
    emit_stage t "turn" ~since:turn_started
      ~detail:(Printexc.to_string exn) ();
    t.scoped_pending <- [];
    raise exn
