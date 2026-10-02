exception Error of string
let job_id name = "portal:" ^ name

let fail message = raise (Error message)

(* publish_web tries tunnel backends in order:
   1. portal: PAVE_PORTAL pin or `portal` on PATH (`portal expose` honors
      the requested name).
   2. cloudflared: PAVE_CLOUDFLARED pin or `cloudflared` on PATH
      (`cloudflared tunnel --url` quick tunnels assign a random
      trycloudflare.com subdomain; the requested name is not used).
   3. localhost.run over ssh: PAVE_SSH pin, else the conventional
      /usr/bin/ssh from workspace_ssh.ml, else `ssh` on PATH
      (anonymous localhost.run relays assign their own *.lhr.life or
      *.localhost.run subdomain; the requested name is best-effort and
      usually ignored without an authenticated account).
   A pin that is set but unusable fails loudly instead of falling through.
   PAVE_TUNNELS=off disables every backend. *)
type backend = Portal | Cloudflared | Localhost_run

let portal_variable = "PAVE_PORTAL"
let cloudflared_variable = "PAVE_CLOUDFLARED"
let ssh_variable = "PAVE_SSH"
let tunnels_variable = "PAVE_TUNNELS"

let backend_name = function
  | Portal -> "portal"
  | Cloudflared -> "cloudflared"
  | Localhost_run -> "localhost.run"
let executable_path path =
  let is_file = match Sys.is_directory path with
    | dir -> not dir
    | exception Sys_error _ -> false in
  is_file &&
  (try Unix.access path [Unix.X_OK]; true with Unix.Unix_error _ -> false)

let find_on_path env program =
  let dirs = match env "PATH" with
    | Some path -> String.split_on_char ':' path
    | None -> [] in
  List.find_map (fun dir ->
    let candidate = Filename.concat dir program in
    if executable_path candidate then Some candidate else None) dirs

(* A pin set-but-unusable is a loud failure: it names the variable rather
   than silently degrading to PATH or the next backend. *)
let pinned_executable env variable =
  match env variable with
  | Some path when path <> "" ->
      if executable_path path then Some path
      else fail (variable ^ " is set but not executable: " ^ path)
  | Some _ -> fail (variable ^ " is empty")
  | None -> None

let resolve_portal env =
  match pinned_executable env portal_variable with
  | Some _ as pinned -> pinned
  | None -> find_on_path env "portal"

let resolve_cloudflared env =
  match pinned_executable env cloudflared_variable with
  | Some _ as pinned -> pinned
  | None -> find_on_path env "cloudflared"

(* workspace_ssh.ml runs the conventional /usr/bin/ssh; prefer it, then
   accept a PATH ssh so non-FHS systems still have a fallback. *)
let resolve_ssh env ssh_candidates =
  match pinned_executable env ssh_variable with
  | Some _ as pinned -> pinned
  | None ->
      (match List.find_opt executable_path ssh_candidates with
       | Some _ as conventional -> conventional
       | None -> find_on_path env "ssh")

let default_ssh_candidates = ["/usr/bin/ssh"]

let detect_portal ?(env = Sys.getenv_opt) () =
  match resolve_portal env with
  | Some path -> path
  | None -> fail
      "portal CLI not found; install portal-tunnel or set PAVE_PORTAL"

let detect_cloudflared ?(env = Sys.getenv_opt) () =
  match resolve_cloudflared env with
  | Some path -> path
  | None -> fail
      "cloudflared not found; install cloudflared or set PAVE_CLOUDFLARED"

let detect_ssh ?(env = Sys.getenv_opt)
    ?(ssh_candidates = default_ssh_candidates) () =
  match resolve_ssh env ssh_candidates with
  | Some path -> path
  | None -> fail "ssh not found; install an OpenSSH client or set PAVE_SSH"

let tunnels_disabled env =
  match env tunnels_variable with
  | Some value -> String.lowercase_ascii (String.trim value) = "off"
  | None -> false

let detect_backend ?(env = Sys.getenv_opt)
    ?(ssh_candidates = default_ssh_candidates) () =
  if tunnels_disabled env then
    fail (tunnels_variable ^ "=off disables tunnel publishing");
  match resolve_portal env with
  | Some path -> Portal, path
  | None ->
      (match resolve_cloudflared env with
       | Some path -> Cloudflared, path
       | None ->
           (match resolve_ssh env ssh_candidates with
            | Some path -> Localhost_run, path
            | None -> fail
                ("no tunnel backend available; install portal-tunnel or set " ^
                 portal_variable ^ ", install cloudflared or set " ^
                 cloudflared_variable ^
                 ", or install an OpenSSH client (ssh) for the \
                  localhost.run relay")))

let publish_name name =
  if String.length name = 0 || String.length name > 63 then
    fail "tunnel name must be 1-63 characters"
  else if not (String.for_all (function
      | 'a'..'z' | '0'..'9' | '-' -> true
      | _ -> false) name) then
    fail "tunnel name must be a lowercase DNS label (letters, digits, hyphens)"
  else name

let portal_arguments ~port ~name =
  ["expose"; string_of_int port; "--name"; name]

(* `cloudflared tunnel --url` starts a quick tunnel; it does not accept a
   custom subdomain, so the requested name is not forwarded. *)
let cloudflared_arguments ~port ~name:_ =
  ["tunnel"; "--url"; "http://127.0.0.1:" ^ string_of_int port;
   "--no-autoupdate"]

(* Anonymous localhost.run does not honor a requested hostname; it emits the
   assigned https://LABEL.lhr.life (or .localhost.run) URL on its own. *)
let localhost_run_arguments ~port ~name:_ ~known_hosts =
  ["-NT"; "-R"; "80:127.0.0.1:" ^ string_of_int port;
   "-o"; "BatchMode=yes";
   "-o"; "StrictHostKeyChecking=accept-new";
   "-o"; "UserKnownHostsFile=" ^ known_hosts;
   "-o"; "ExitOnForwardFailure=yes";
   "nokey@localhost.run"]

let rec find_substring text needle from =
  if String.length text - from < String.length needle then None
  else if String.sub text from (String.length needle) = needle then Some from
  else find_substring text needle (from + 1)

let token_end text start =
  let finish = ref start in
  while !finish < String.length text &&
    (let c = text.[!finish] in
     Char.code c > 32 && c <> '"' && c <> '\'' && c <> '}' && c <> ']')
  do incr finish done;
  !finish

(* zerolog console output: `… INF service ready at https://NAME.RELAY …`;
   JSON output carries "public_url":"https://NAME.RELAY". The first wins. *)
let public_url output =
  let url start = String.sub output start (token_end output start - start) in
  let json () =
    let key = "\"public_url\":\"https://" in
    match find_substring output key 0 with
    | Some index -> Some (url (index + String.length key - 8))
    | None -> None in
  let needle = "service ready at " in
  match find_substring output needle 0 with
  | Some index ->
      let at = index + String.length needle in
      if at < String.length output then (
        let value = url at in
        if String.starts_with ~prefix:"https://" value then
          Some (String.trim value)
        else json ())
      else json ()
  | None -> json ()

let host_end text start =
  let finish = ref start in
  while !finish < String.length text &&
    (match text.[!finish] with
     | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '-' -> true
     | _ -> false)
  do incr finish done;
  !finish

(* Find `https://<host>` tokens whose host ends with the relay suffix
   (leading dot included, nonempty label required). *)
let host_suffix_url output suffix =
  let needle = "https://" in
  let rec scan from =
    match find_substring output needle from with
    | None -> None
    | Some index ->
        let start = index + String.length needle in
        let stop = host_end output start in
        let host = String.sub output start (stop - start) in
        if String.length host > String.length suffix &&
           String.ends_with ~suffix host then
          Some ("https://" ^ host)
        else scan (index + 1) in
  scan 0

let cloudflared_url output = host_suffix_url output ".trycloudflare.com"

let localhost_run_url output =
  match host_suffix_url output ".lhr.life" with
  | Some _ as url -> url
  | None -> host_suffix_url output ".localhost.run"

let backend_url backend output =
  match backend with
  | Portal -> public_url output
  | Cloudflared -> cloudflared_url output
  | Localhost_run -> localhost_run_url output

let ready_regex = function
  | Portal -> "service ready at \\|\"public_url\""
  | Cloudflared -> "https://[a-z0-9-]+\\.trycloudflare\\.com"
  | Localhost_run ->
      "https://[a-z0-9-]+\\(\\.lhr\\.life\\|\\.localhost\\.run\\)"

(* The leftmost DNS label of the emitted URL is the name the relay actually
   advertised; it may differ from the requested name. *)
let advertised_name url =
  if String.starts_with ~prefix:"https://" url then
    let host = String.sub url 8 (String.length url - 8) in
    match String.index_opt host '.' with
    | Some dot when dot > 0 -> Some (String.sub host 0 dot)
    | _ -> None
  else None

(* Session state for the ssh backend: $XDG_STATE_HOME/pave/tunnels (or
   ~/.local/state/pave/tunnels), the same root as session_store.ml. Missing
   directories are created private; a symlinked or foreign-owned leaf fails
   closed. *)
let tunnel_state_dir ?(env = Sys.getenv_opt) () =
  let state_home = match env "XDG_STATE_HOME" with
    | Some path when path <> "" && not (Filename.is_relative path) -> path
    | _ ->
        (match env "HOME" with
         | Some home when home <> "" && not (Filename.is_relative home) ->
             Filename.concat (Filename.concat home ".local") "state"
         | _ -> fail
             "HOME or an absolute XDG_STATE_HOME is required for the \
              ssh tunnel state directory") in
  Filename.concat (Filename.concat state_home "pave") "tunnels"

let ensure_private_dir path =
  let rec make path =
    match Unix.lstat path with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
        let parent = Filename.dirname path in
        if parent = path then fail "tunnel state directory is unavailable";
        make parent;
        (try Unix.mkdir path 0o700
         with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    | stat ->
        if stat.Unix.st_kind <> Unix.S_DIR then
          fail "tunnel state path exists and is not a directory"
        else if stat.Unix.st_uid <> Unix.geteuid () then
          fail "tunnel state directory is not owned by the current user" in
  make path

(* accept-new appends localhost.run host keys here. *)
let tunnel_known_hosts ?(env = Sys.getenv_opt) ?state_dir () =
  let dir = match state_dir with
    | Some dir -> dir
    | None -> tunnel_state_dir ~env () in
  ensure_private_dir dir;
  Filename.concat dir "tunnel_known_hosts"

let publish ?cancel ?(env = Sys.getenv_opt)
    ?(ssh_candidates = default_ssh_candidates) ?state_dir
    manager ~id ~port ~name =
  if port < 1 || port > 65_535 then
    fail "publish requires a loopback port between 1 and 65535";
  let backend, executable = detect_backend ~env ~ssh_candidates () in
  if not (Workspace_process.release_finished manager ~id) then
    fail ("tunnel " ^ name ^ " is already running; stop it first");
  let arguments = match backend with
    | Portal -> portal_arguments ~port ~name
    | Cloudflared -> cloudflared_arguments ~port ~name
    | Localhost_run ->
        localhost_run_arguments ~port ~name
          ~known_hosts:(tunnel_known_hosts ~env ?state_dir ()) in
  Workspace_process.start manager ~id ~program:executable ~arguments ();
  let ready =
    try Workspace_process.wait_ready manager ~id ?cancel
      ~timeout_seconds:45 ~log_regex:(ready_regex backend) ()
    with exn ->
      Workspace_process.kill_job manager ~id;
      raise exn in
  if not ready then (
    Workspace_process.kill_job manager ~id;
    let output = (Workspace_process.read_output manager ~id ()).output in
    fail (backend_name backend ^
      " did not publish a public URL within 45 s: " ^ String.trim output))
  else
    let output = (Workspace_process.read_output manager ~id ()).output in
    match backend_url backend output with
    | None -> fail (backend_name backend ^
        " reported readiness without a public URL")
    | Some url ->
        let advertised = match advertised_name url with
          | Some value -> value
          | None -> name in
        let caveat = match backend with
          | Portal -> ""
          | Cloudflared ->
              " Quick tunnels assign a random subdomain; the requested \
               name is not used."
          | Localhost_run ->
              " localhost.run assigns its own subdomain; the requested \
               name may be ignored." in
        `Assoc [ "status", `String "published";
                 "id", `String id;
                 "backend", `String (backend_name backend);
                 "url", `String url;
                 "name", `String advertised;
                 "port", `Int port;
                 "note", `String
                   ("Public until stopped with publish_web stop or \
                     session exit." ^ caveat) ]

let stop manager ~id =
  Workspace_process.kill_job manager ~id;
  `Assoc [ "status", `String "stopped"; "id", `String id ]

let list manager =
  let rows = Workspace_process.jobs manager in
  let tunnels = List.filter_map (fun (job : Workspace_process.job_summary) ->
    if String.starts_with ~prefix:"portal:" job.id then
      Some (`Assoc [ "id", `String (String.sub job.id 7
                       (String.length job.id - 7));
                     "status", `String (match job.status with
                       | Workspace_process.Running -> "running"
                       | Workspace_process.Completed _ -> "exited");
                     "command", `String job.command ])
    else None) rows in
  `Assoc [ "tunnels", `List tunnels ]
