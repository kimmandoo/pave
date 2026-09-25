type phase = Model | Tool of string

type tool_event =
  | Tool_started of { call_id : string; name : string }
  | Tool_updated of { call_id : string; name : string; received_bytes : int }
  | Tool_settled of {
      call_id : string; name : string; result : string; is_error : bool
    }
  | Tool_aborted of {
      call_id : string;
      name : string;
      result : string;
      side_effects_may_have_occurred : bool;
    }

type t = {
  provider : Provider.config;
  authentication : Provider.authentication;
  resolve_credential : (unit -> Provider.credentials) option;
  root : string;
  allow_shell : bool;
  tool_available : string -> bool;
  stream : bool;
  approve_command : string -> bool;
  system : string;
  mutable scoped_pending : (string * string) list;
  mutable history_rev : Protocol.message list;
  on_event : string -> unit;
  on_delta : string -> unit;
  on_change : Protocol.message -> unit;
  on_usage : (Protocol.usage -> unit) option;
  on_phase : (phase -> unit) option;
  on_tool_event : (tool_event -> unit) option;
}

let create ~provider ~root ~system ?(authentication = Provider.Api_key)
    ?resolve_credential ?(allow_shell = false)
    ?(tool_available = fun _ -> true) ?(stream = false)
    ?(approve_command = fun _ -> false) ?(history = [])
    ?on_usage ?on_phase ?on_tool_event ?(on_change = fun _ -> ())
    ?(on_delta = fun _ -> ()) ~on_event () =
  { provider; authentication; resolve_credential; root; system; allow_shell;
    tool_available; stream;
    approve_command; history_rev = List.rev history; scoped_pending = [];
    on_change; on_delta; on_event; on_usage; on_phase; on_tool_event }

let messages t = List.rev t.history_rev
let append t message =
  t.on_change message;
  t.history_rev <- message :: t.history_rev

let emit_tool_event t event =
  match t.on_tool_event with
  | Some notify -> notify event
  | None ->
      (match event with
       | Tool_started { name; _ } -> t.on_event ("[" ^ name ^ "]")
       | Tool_updated _ -> ()
       | Tool_settled { name; result; _ }
       | Tool_aborted { name; result; _ } ->
           t.on_event ("[" ^ name ^ "] " ^ result))

let max_scoped_context_bytes = Project_context.max_total_bytes

let file_scope t call =
  try
    match Protocol.member "path" call.Protocol.arguments with
    | `String path when path <> "" && Filename.is_relative path &&
        not (String.contains path '\000') &&
        not (List.mem ".." (String.split_on_char '/' path)) ->
        let path = Project_context.normalize path in
        if path = "" || path = "." then
          Error "Error: a workspace-relative file path is required; file operation not executed"
        else
          let scope = Project_context.resolve_scoped ~root:t.root ~path () in
          if scope.safe then Ok (path, scope.text)
          else
            let codes = List.map (fun (d : Project_context.diagnostic) -> d.code)
              scope.diagnostics |> List.sort_uniq String.compare in
            Error ("Error: scoped instructions could not be resolved (" ^
              String.concat ", " codes ^ "); file operation not executed")
    | _ -> Error "Error: a workspace-relative file path is required; file operation not executed"
  with Unix.Unix_error _ | Sys_error _ | Invalid_argument _ | Failure _ ->
    Error "Error: scoped instructions unavailable; file operation not executed"

let queue_scope t path text =
  if text = "" then true else
  let pending = List.remove_assoc path t.scoped_pending in
  let size = List.fold_left (fun total (_, content) ->
    total + String.length content) (String.length text) pending in
  if size > max_scoped_context_bytes then false
  else (t.scoped_pending <- (path, text) :: pending; true)

let run ?(max_turns = 20) ?cancel t text =
  if String.trim text = "" then invalid_arg "empty prompt";
  if max_turns <= 0 then invalid_arg "max_turns must be positive";
  Provider.check_cancel cancel;
  append t (Protocol.user text);
  let rec turn remaining =
    Provider.check_cancel cancel;
    if remaining = 0 then failwith "tool-call limit reached; inspect workspace before continuing";
    let visible = t.scoped_pending in
    let scoped = List.rev visible |> List.map (fun (path, text) ->
      Printf.sprintf "For workspace file %S:\n%s" path text) in
    let system_text = if scoped = [] then t.system else
      t.system ^ "\n\nPath-scoped project instructions (lower priority than mobile safety):\n" ^
      String.concat "\n\n" scoped in
    let system : Protocol.message =
      { role = "system"; content = Some system_text; tool_calls = [];
        tool_call_id = None; provider_state = None } in
    let definitions =
      Tools.available_for ~allow_shell:t.allow_shell ~enabled:t.tool_available in
    (match t.on_phase with None -> () | Some notify -> notify Model);
    let transcript = system :: messages t in
    let reply =
      if t.stream then Provider.complete ~authentication:t.authentication
        ?resolve_credential:t.resolve_credential ~on_text:t.on_delta
        ?on_usage:t.on_usage ?cancel t.provider transcript definitions
      else Provider.complete ~authentication:t.authentication
        ?resolve_credential:t.resolve_credential ?on_usage:t.on_usage
        ?cancel t.provider transcript definitions in
    Provider.check_cancel cancel;
    t.scoped_pending <- [];
    (match reply.content with
     | Some s when s <> "" ->
         if t.stream then t.on_delta "\n" else t.on_event s
     | _ -> ());
    Provider.check_cancel cancel;
    match reply.tool_calls with
    | [] ->
        append t reply;
        (match reply.content with Some s -> s | None -> "")
    | calls ->
        append t reply;
        let cancellation_result =
          "Error: turn cancelled before this tool ran; do not assume it executed" in
        let abort (call : Protocol.tool_call) result side_effects_may_have_occurred =
          emit_tool_event t (Tool_aborted {
            call_id = call.id; name = call.name; result;
            side_effects_may_have_occurred
          }) in
        let skipped pending =
          List.iter (fun (call : Protocol.tool_call) ->
            abort call cancellation_result false;
            append t (Protocol.tool_result call.id cancellation_result)) pending in
        let rec execute = function
          | [] -> turn (remaining - 1)
          | (call : Protocol.tool_call) :: rest as pending ->
              emit_tool_event t (Tool_started {
                call_id = call.id; name = call.name
              });
              (match cancel with
               | Some cancelled when cancelled () ->
                   skipped pending;
                   raise Provider.Cancelled
               | _ -> ());
              (match t.on_phase with
               | None -> ()
               | Some notify -> notify (Tool call.name));
              let on_progress = match call.name, t.on_tool_event with
                | "run_command", Some _ ->
                    Some (fun received_bytes ->
                      emit_tool_event t (Tool_updated {
                        call_id = call.id; name = call.name; received_bytes
                      }))
                | _ -> None in
              let preflight () =
                if call.name = "run_command" && not t.allow_shell then
                  Some "Error: shell execution disabled; ask the user to restart with --allow-shell"
                else if not (t.tool_available call.name) then
                  Some "Error: tool is no longer available"
                else if call.name = "run_command" then
                  (match Protocol.member "command" call.arguments with
                   | `String command when t.approve_command command ->
                       Provider.check_cancel cancel;
                       None
                   | `String _ -> Some "Error: command not approved"
                   | _ -> Some "Error: missing command")
                else if call.name = "write_file" || call.name = "edit_file" ||
                  call.name = "read_file" then
                  (match file_scope t call with
                   | Error message -> Some message
                   | Ok (path, scoped) ->
                     if scoped <> "" && List.assoc_opt path visible <> Some scoped then
                       if queue_scope t path scoped then
                         if call.name = "read_file" then None
                         else Some
                           ("Error: file mutation withheld until path-scoped instructions " ^
                            "are presented as system context; retry this tool call next turn")
                       else Some
                         ("Error: scoped instructions exceed the per-turn context limit; " ^
                          "file operation not executed")
                     else None)
                else None in
              let result =
                try
                  Provider.check_cancel cancel;
                  (try
                     Tools.execute ?cancel ?on_progress ~preflight ~root:t.root
                       ~name:call.name ~args:call.arguments ()
                   with
                   | Provider.Cancelled -> raise Provider.Cancelled
                   | Tools.Cancelled -> raise Tools.Cancelled
                   | exn -> "Error: " ^ Printexc.to_string exn)
                with
                | Tools.Cancelled ->
                    let result =
                      "Error: command cancelled while running; side effects may have occurred" in
                    abort call result true;
                    append t (Protocol.tool_result call.id result);
                    skipped rest;
                    raise Provider.Cancelled
                | Provider.Cancelled ->
                    skipped pending;
                    raise Provider.Cancelled in
              emit_tool_event t (Tool_settled {
                call_id = call.id; name = call.name; result;
                is_error = String.starts_with ~prefix:"Error:" result
              });
              append t (Protocol.tool_result call.id result);
              execute rest
        in
        execute calls
  in
  try turn max_turns with exn ->
    t.scoped_pending <- [];
    raise exn
