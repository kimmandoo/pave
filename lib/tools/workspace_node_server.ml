exception Error of string

type exposure = Localhost | Lan

type state = Starting | Ready | Failed of string | Stopped

type session = {
  root : string;
  id : string;
  process_id : string;
  command : string;
  cwd : string;
  host : string;
  port : int;
  exposure : exposure;
  mutable state : state;
}

type manager = {
  processes : Workspace_process.manager;
  lock : Mutex.t;
  sessions : (string, session) Hashtbl.t;
}

let fail message = raise (Error message)

let create_manager ~process_manager =
  { processes = process_manager; lock = Mutex.create (); sessions = Hashtbl.create 4 }

let with_lock lock fn =
  Mutex.lock lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock lock) fn

let validate_port port =
  if port < 1 || port > 65_535 then fail "port must be between 1 and 65535"

let parse_exposure = function
  | "localhost" -> Localhost
  | "lan" -> Lan
  | _ -> fail "host must be localhost or lan"

let display_host = function Localhost -> "localhost" | Lan -> "0.0.0.0 (LAN)"

let preview session =
  let suffix = match session.exposure with
    | Localhost -> ""
    | Lan -> " (LAN exposure enabled; connect using this machine's LAN address)" in
  Printf.sprintf "http://localhost:%d%s" session.port suffix

let command ~root ~subroot ~script ~package_manager =
  try
    Workspace_node_scripts.command_for_script ~root ~subroot ~script
      ~manager:package_manager
  with Workspace_node_scripts.Error message -> fail message

let package_manager_and_args ~manager ~script ~host ~port =
  let executable, prefix = match manager with
    | "npm" -> "npm", ["run"; script; "--"]
    | "yarn" -> "yarn", ["run"; script; "--"]
    | "pnpm" -> "pnpm", ["run"; script; "--"]
    | _ -> fail "unsupported package manager" in
  let host_arg = match host with Localhost -> "localhost" | Lan -> "lan" in
  executable, prefix @ ["--host"; host_arg; "--port"; string_of_int port]

let run_bounded program arguments =
  try Workspace_process.run ~timeout_seconds:5 ~output_limit:4_096
      ~program ~arguments ()
  with Workspace_process.Error message ->
    fail ("could not verify dev-server listener ownership: " ^ message)

let command_output label program arguments =
  let result = run_bounded program arguments in
  match result.Workspace_process.termination with
  | Workspace_process.Exited 0 when not result.truncated -> result.output
  | _ -> fail ("could not verify dev-server listener ownership: " ^ label)

let listener_pids port =
  let output = command_output "listener inspection failed" "lsof"
      ["-nP"; "-t"; "-iTCP:" ^ string_of_int port; "-sTCP:LISTEN"] in
  let pids = String.split_on_char '\n' output
    |> List.filter_map (fun row -> int_of_string_opt (String.trim row))
    |> List.sort_uniq Int.compare in
  if pids = [] then fail "dev server became reachable without an identifiable listening process";
  pids

let process_group pid =
  let output = command_output "process-group inspection failed" "ps"
      ["-o"; "pgid="; "-p"; string_of_int pid] in
  match int_of_string_opt (String.trim output) with
  | Some group when group > 0 -> group
  | _ -> fail "dev-server listener process has no verifiable process group"

let listener_owned process_manager process_id port =
  let owner_group = Workspace_process.job_pid process_manager ~id:process_id in
  if owner_group <= 0 then false
  else
    List.for_all (fun pid -> process_group pid = owner_group) (listener_pids port)

let start manager ~id ~root ~subroot ~script ~package_manager ~host ~port
    ?(readiness_timeout_seconds = 45) ?cancel () =
  if id = "" || String.contains id '\000' || String.length id > 100 then
    fail "invalid session id";
  validate_port port;
  if Workspace_process.port_accepting port then
    fail "selected Node dev server port is already occupied by another process";
  if readiness_timeout_seconds < 1 || readiness_timeout_seconds > 300 then
    fail "readiness timeout must be between 1 and 300 seconds";
  let root = Workspace_path.root_path root in
  let exposure = parse_exposure host in
  let selected_manager, cwd = command ~root ~subroot ~script ~package_manager in
  let executable, arguments = package_manager_and_args ~manager:selected_manager
      ~script ~host:exposure ~port in
  let process_id = "workspace-node-server-" ^ id in
  let selected_command = String.concat " " (executable :: arguments) in
  let session = { root; id; process_id; command = selected_command; cwd;
    host = display_host exposure; port; exposure; state = Starting } in
  with_lock manager.lock (fun () ->
    if Hashtbl.mem manager.sessions id then fail "node server session id is already in use";
    Hashtbl.add manager.sessions id session);
  let launched = ref false in
  try
    Workspace_process.start manager.processes ~id:process_id ~cwd:(Some cwd)
      ~output_limit:65_536 ~program:executable ~arguments ();
    launched := true;
    let ready = Workspace_process.wait_ready manager.processes ~id:process_id
        ?cancel ~timeout_seconds:readiness_timeout_seconds ~port () in
    (* A loopback listener is not evidence that this child owns the endpoint:
       another process may already have claimed the requested port. *)
    let status = Workspace_process.job_status manager.processes ~id:process_id in
    let owned = ready && status = Workspace_process.Running &&
      listener_owned manager.processes process_id port in
    if owned then (
      with_lock manager.lock (fun () -> session.state <- Ready);
      session)
    else (
      let reason = match status with
        | Workspace_process.Completed termination ->
            Printf.sprintf "dev server process exited before becoming ready (%s)"
              (match termination with
               | Workspace_process.Exited code -> Printf.sprintf "exit code %d" code
               | Workspace_process.Signaled signal -> Printf.sprintf "signal %d" signal
               | Workspace_process.Timed_out -> "timed out"
               | Workspace_process.Cancelled -> "cancelled")
        | Workspace_process.Running when ready ->
            "dev-server port is reachable but its listener is not owned by this session"
        | Workspace_process.Running ->
            "dev server did not become ready before timeout or cancellation" in
      with_lock manager.lock (fun () -> session.state <- Failed reason);
      Workspace_process.kill_job manager.processes ~id:process_id;
      fail reason)
  with exn ->
    if !launched then
      (try Workspace_process.kill_job manager.processes ~id:process_id with _ -> ());
    with_lock manager.lock (fun () ->
      if session.state = Starting then session.state <- Failed "startup cancelled or failed");
    (match exn with
     | Workspace_process.Error message -> fail message
     | _ -> raise exn)

let refresh manager session =
  match session.state with
  | Ready ->
      (match Workspace_process.job_status manager.processes ~id:session.process_id with
       | Workspace_process.Running -> ()
       | Workspace_process.Completed _ -> session.state <- Stopped)
  | _ -> ()

let get manager ~id =
  with_lock manager.lock (fun () ->
    match Hashtbl.find_opt manager.sessions id with
    | None -> fail "unknown node server session"
    | Some session -> refresh manager session; session)

let stop manager ~id =
  let session = get manager ~id in
  (match session.state with
   | Stopped | Failed _ -> ()
   | Starting | Ready ->
       (try Workspace_process.kill_job manager.processes ~id:session.process_id
        with Workspace_process.Error _ -> ());
       with_lock manager.lock (fun () -> session.state <- Stopped));
  session

let render session =
  let status = match session.state with
    | Starting -> "starting" | Ready -> "ready" | Failed message -> "failed: " ^ message
    | Stopped -> "stopped" in
  Printf.sprintf "Node dev server %s (%s): %s; %s; %s"
    session.id status session.command session.host (preview session)

let close_manager manager =
  let sessions = with_lock manager.lock (fun () ->
    Hashtbl.fold (fun id _ ids -> id :: ids) manager.sessions []) in
  List.iter (fun id -> try ignore (stop manager ~id) with _ -> ()) sessions
