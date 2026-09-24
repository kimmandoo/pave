type t = {
  provider : Provider.config;
  authentication : Provider.authentication;
  resolve_credential : (unit -> Provider.credentials) option;
  root : string;
  allow_shell : bool;
  stream : bool;
  approve_command : string -> bool;
  system : string;
  mutable scoped_pending : (string * string) list;
  mutable history_rev : Protocol.message list;
  on_event : string -> unit;
  on_delta : string -> unit;
  on_change : Protocol.message -> unit;
}

let create ~provider ~root ~system ?(authentication = Provider.Api_key)
    ?resolve_credential ?(allow_shell = false) ?(stream = false)
    ?(approve_command = fun _ -> false) ?(history = [])
    ?(on_change = fun _ -> ()) ?(on_delta = fun _ -> ()) ~on_event () =
  { provider; authentication; resolve_credential; root; system; allow_shell; stream;
    approve_command; history_rev = List.rev history; scoped_pending = [];
    on_change; on_delta; on_event }

let messages t = List.rev t.history_rev
let append t message =
  t.on_change message;
  t.history_rev <- message :: t.history_rev

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
    let definitions = if t.allow_shell then Tools.definitions else
      List.filter (fun json -> Protocol.member "name" (Protocol.member "function" json)
        <> `String "run_command") Tools.definitions in
    let transcript = system :: messages t in
    let reply =
      if t.stream then Provider.complete ~authentication:t.authentication
        ?resolve_credential:t.resolve_credential ~on_text:t.on_delta ?cancel t.provider
        transcript definitions
      else Provider.complete ~authentication:t.authentication
        ?resolve_credential:t.resolve_credential ?cancel t.provider transcript definitions in
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
        let skipped pending =
          List.iter (fun (call : Protocol.tool_call) ->
            append t (Protocol.tool_result call.id
              "Error: turn cancelled before this tool ran; do not assume it executed")) pending in
        let rec execute first = function
          | [] -> turn (remaining - 1)
          | (call : Protocol.tool_call) :: rest as pending ->
              t.on_event ("[" ^ call.name ^ "]");
              (match cancel with
               | Some cancelled when cancelled () ->
                   if not first then skipped pending;
                   raise Provider.Cancelled
               | _ -> ());
              if first then append t reply;
              let result =
                try
                  Provider.check_cancel cancel;
                  (try
                     if call.name = "run_command" then
                       if not t.allow_shell then
                         "Error: shell execution disabled; ask the user to restart with --allow-shell"
                       else (match Protocol.member "command" call.arguments with
                         | `String command when t.approve_command command ->
                             Provider.check_cancel cancel;
                             Tools.execute ?cancel ~root:t.root ~name:call.name ~args:call.arguments ()
                         | `String _ -> "Error: command not approved"
                         | _ -> "Error: missing command")
                     else if call.name = "write_file" || call.name = "edit_file" ||
                       call.name = "read_file" then
                       (match file_scope t call with
                        | Error message -> message
                        | Ok (path, scoped) ->
                          if scoped <> "" && List.assoc_opt path visible <> Some scoped then
                            if queue_scope t path scoped then
                              if call.name = "read_file" then
                                Tools.execute ~root:t.root ~name:call.name ~args:call.arguments ()
                              else
                                "Error: file mutation withheld until path-scoped instructions " ^
                                "are presented as system context; retry this tool call next turn"
                            else "Error: scoped instructions exceed the per-turn context limit; " ^
                              "file operation not executed"
                          else Tools.execute ~root:t.root ~name:call.name ~args:call.arguments ())
                     else Tools.execute ~root:t.root ~name:call.name ~args:call.arguments ()
                   with
                   | Provider.Cancelled -> raise Provider.Cancelled
                   | Tools.Cancelled -> raise Tools.Cancelled
                   | exn -> "Error: " ^ Printexc.to_string exn)
                with
                | Tools.Cancelled ->
                    append t (Protocol.tool_result call.id
                      "Error: command cancelled while running; side effects may have occurred");
                    skipped rest;
                    raise Provider.Cancelled
                | Provider.Cancelled ->
                    skipped pending;
                    raise Provider.Cancelled in
              append t (Protocol.tool_result call.id result);
              t.on_event ("[" ^ call.name ^ "] " ^ result);
              execute false rest in
        execute true calls
  in
  try turn max_turns with exn ->
    t.scoped_pending <- [];
    raise exn
