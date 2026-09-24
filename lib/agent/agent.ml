type t = {
  provider : Provider.config;
  authentication : Provider.authentication;
  resolve_credential : (unit -> Provider.credentials) option;
  root : string;
  allow_shell : bool;
  stream : bool;
  approve_command : string -> bool;
  system : string;
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
    approve_command; history_rev = List.rev history;
    on_change; on_delta; on_event }

let messages t = List.rev t.history_rev
let append t message =
  t.on_change message;
  t.history_rev <- message :: t.history_rev

let run ?(max_turns = 20) ?cancel t text =
  if String.trim text = "" then invalid_arg "empty prompt";
  if max_turns <= 0 then invalid_arg "max_turns must be positive";
  Provider.check_cancel cancel;
  append t (Protocol.user text);
  let rec turn remaining =
    Provider.check_cancel cancel;
    if remaining = 0 then failwith "tool-call limit reached; inspect workspace before continuing";
    let system : Protocol.message =
      { role = "system"; content = Some t.system; tool_calls = [];
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
  turn max_turns
