type t = {
  provider : Provider.config;
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

let create ~provider ~root ~system ?(allow_shell = false) ?(stream = false)
    ?(approve_command = fun _ -> false) ?(history = [])
    ?(on_change = fun _ -> ()) ?(on_delta = fun _ -> ()) ~on_event () =
  { provider; root; system; allow_shell; stream; approve_command;
    history_rev = List.rev history; on_change; on_delta; on_event }

let messages t = List.rev t.history_rev
let append t message =
  t.on_change message;
  t.history_rev <- message :: t.history_rev

let run ?(max_turns = 20) t text =
  if String.trim text = "" then invalid_arg "empty prompt";
  if max_turns <= 0 then invalid_arg "max_turns must be positive";
  append t (Protocol.user text);
  let rec turn remaining =
    if remaining = 0 then failwith "tool-call limit reached; inspect workspace before continuing";
    let system : Protocol.message =
      { role = "system"; content = Some t.system; tool_calls = []; tool_call_id = None } in
    let definitions = if t.allow_shell then Tools.definitions else
      List.filter (fun json -> Protocol.member "name" (Protocol.member "function" json)
        <> `String "run_command") Tools.definitions in
    let transcript = system :: messages t in
    let reply =
      if t.stream then Provider.complete ~on_text:t.on_delta t.provider
        transcript definitions
      else Provider.complete t.provider transcript definitions in
    append t reply;
    (match reply.content with
     | Some s when s <> "" ->
         if t.stream then t.on_delta "\n" else t.on_event s
     | _ -> ());
    match reply.tool_calls with
    | [] -> (match reply.content with Some s -> s | None -> "")
    | calls ->
        List.iter (fun (call : Protocol.tool_call) ->
          t.on_event ("[" ^ call.name ^ "]");
          let result = try
            if call.name = "run_command" then
              if not t.allow_shell then
                "Error: shell execution disabled; ask the user to restart with --allow-shell"
              else (match Protocol.member "command" call.arguments with
                | `String command when t.approve_command command ->
                    Tools.execute ~root:t.root ~name:call.name ~args:call.arguments
                | `String _ -> "Error: command not approved"
                | _ -> "Error: missing command")
            else Tools.execute ~root:t.root ~name:call.name ~args:call.arguments
            with exn -> "Error: " ^ Printexc.to_string exn in
          append t (Protocol.tool_result call.id result);
          t.on_event ("[" ^ call.name ^ "] " ^ result)) calls;
        turn (remaining - 1)
  in
  turn max_turns
