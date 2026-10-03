exception Error of string
let fail message = raise (Error message)
let portal_variable = "PAVE_PORTAL"
let tunnels_variable = "PAVE_TUNNELS"
let job_id name = "portal:" ^ name

let executable_path path =
  try (Unix.stat path).Unix.st_kind = Unix.S_REG &&
      (Unix.access path [Unix.X_OK]; true)
  with Unix.Unix_error _ -> false

let detect_portal ?(env = Sys.getenv_opt) () =
  (match env tunnels_variable with
   | Some value when String.lowercase_ascii (String.trim value) = "off" ->
       fail "PAVE_TUNNELS=off disables Portal publishing"
   | _ -> ());
  match env portal_variable with
  | Some path when path <> "" && not (Filename.is_relative path) &&
                   executable_path path -> path
  | Some _ -> fail "PAVE_PORTAL must name an absolute executable portal CLI"
  | None ->
      let dirs = Option.fold ~none:[] ~some:(String.split_on_char ':')
        (env "PATH") in
      match List.find_map (fun dir ->
        if dir = "" || Filename.is_relative dir then None else
        let path = Filename.concat dir "portal" in
        if executable_path path then Some path else None) dirs with
      | Some path -> path
      | None -> fail
          "gosuda portal-tunnel CLI not found; install https://github.com/gosuda/portal-tunnel or set PAVE_PORTAL (no other tunnel service is used)"

let publish_name name =
  let length = String.length name in
  let alnum = function 'a'..'z' | '0'..'9' -> true | _ -> false in
  if length = 0 || length > 63 || not (alnum name.[0]) ||
     not (alnum name.[length - 1]) ||
     not (String.for_all (fun c -> alnum c || c = '-') name) then
    fail "prefix must be a 1-63 character lowercase DNS label, starting and ending with a letter or digit";
  name

let fresh_name () = "pave-" ^ String.sub (Session.fresh_id ()) 0 16

let https_origin value =
  if not (String.starts_with ~prefix:"https://" value) then
    fail "relay must be an HTTPS origin";
  let authority = String.sub value 8 (String.length value - 8) in
  let authority = if String.ends_with ~suffix:"/" authority then
      String.sub authority 0 (String.length authority - 1) else authority in
  let host, port = match String.split_on_char ':' authority with
    | [host] -> host, None
    | [host; port] -> host, Some port
    | _ -> fail "relay must be an HTTPS hostname with an optional port" in
  if host = "" || String.length host > 253 ||
     not (List.for_all (fun label ->
       try ignore (publish_name label); true with Error _ -> false)
       (String.split_on_char '.' host)) then
    fail "relay has an invalid hostname";
  Option.iter (fun port -> match int_of_string_opt port with
    | Some value when value >= 1 && value <= 65535 &&
        String.for_all (function '0'..'9' -> true | _ -> false) port -> ()
    | _ -> fail "relay has an invalid port") port;
  "https://" ^ authority

let advertised_name url =
  try
    let origin = https_origin url in
    let host = String.sub origin 8 (String.length origin - 8) in
    Some (List.hd (String.split_on_char '.' (List.hd (String.split_on_char ':' host))))
  with Error _ -> None

(* Ignore listener/discovery URLs and incomplete output fragments. Only a
   complete Portal service-ready record advertises the tenant endpoint. *)
let public_url output =
  let lines = String.split_on_char '\n' output in
  let complete = match List.rev lines with [] -> [] | _ :: rest -> List.rev rest in
  let valid url = try Some (https_origin url) with Error _ -> None in
  List.find_map (fun line ->
    let line = String.trim line in
    if String.starts_with ~prefix:"{" line then
      try
        let json = Yojson.Basic.from_string line in
        match Protocol.member "message" json, Protocol.member "public_url" json with
        | `String message, `String url when
            String.starts_with ~prefix:"service ready at https://" message -> valid url
        | _ -> None
      with Yojson.Json_error _ -> None
    else
      let needle = "service ready at https://" in
      try
        let at = Str.search_forward (Str.regexp_string needle) line 0 +
          String.length "service ready at " in
        let stop = ref at in
        while !stop < String.length line && Char.code line.[!stop] > 32 do
          incr stop
        done;
        valid (String.sub line at (!stop - at))
      with Not_found -> None) complete

let identity_path ?(env = Sys.getenv_opt) ?state_dir ~name () =
  let dir = match state_dir with
    | Some path when not (Filename.is_relative path) -> path
    | Some _ -> fail "Portal state directory must be absolute"
    | None ->
        let home = match env "XDG_STATE_HOME" with
          | Some path when path <> "" && not (Filename.is_relative path) -> path
          | _ -> (match env "HOME" with
              | Some path when path <> "" && not (Filename.is_relative path) ->
                  Filename.concat path ".local/state"
              | _ -> fail "HOME or absolute XDG_STATE_HOME is required for Portal identity") in
        let parent = Filename.concat home "pave" in
        Session_store.ensure_directory parent;
        Filename.concat parent "portal" in
  (try Session_store.ensure_directory dir
   with Invalid_argument message -> fail message);
  let path = Filename.concat dir (publish_name name ^ ".json") in
  (match Unix.lstat path with
   | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
   | _ ->
       (match Session_store.read_private_file path 8192 with
        | None -> fail "Portal identity must be a private owned regular file, not a symlink"
        | Some text ->
            let json = try Yojson.Basic.from_string text
              with Yojson.Json_error _ -> fail "Portal identity is invalid JSON" in
            if Protocol.member "name" json <> `String name then
              fail "Portal identity does not match the requested prefix"));
  path

let portal_arguments ~port ~name ~identity ?relay () =
  ["expose"; "127.0.0.1:" ^ string_of_int port;
   "--name"; name; "--identity-path"; identity] @
  (match relay with None -> [] | Some relay ->
    ["--relays"; https_origin relay; "--discovery=false"])

let publish ?cancel ?(env = Sys.getenv_opt) ?state_dir ?relay
    manager ~id ~port ~name =
  if port < 1 || port > 65535 then
    fail "publish requires a loopback port between 1 and 65535";
  let name = publish_name name in
  if id <> job_id name then fail "tunnel ID must match its prefix";
  let executable = detect_portal ~env () in
  let relay = Option.map https_origin relay in
  Workspace_process.check_wait_cancel cancel;
  if not (Workspace_process.port_accepting port) then
    fail "localhost web server is not listening on the requested 127.0.0.1 port";
  if not (Workspace_process.release_finished manager ~id) then
    fail ("tunnel " ^ name ^ " is already running; stop it first");
  let identity = identity_path ~env ?state_dir ~name () in
  let arguments = portal_arguments ~port ~name ~identity ?relay () in
  Workspace_process.start manager ~id ~cwd:(Some (Filename.dirname identity))
    ~environment:["NO_COLOR", "1"] ~program:executable ~arguments ();
  try
    let ready = Workspace_process.wait_ready manager ~id ?cancel
      ~timeout_seconds:45 ~log_regex:"service ready at https://[^\n]*\n" () in
    let output = (Workspace_process.read_output manager ~id ()).output in
    if not ready then fail
      ("Portal did not publish a public URL within 45 s: " ^ String.trim output);
    if not (List.exists (fun (job : Workspace_process.job_summary) ->
        job.id = id && job.status = Workspace_process.Running)
        (Workspace_process.jobs manager)) then
      fail "Portal exited before publication completed";
    let url = match public_url output with
      | Some url -> url
      | None -> fail "Portal reported readiness without a valid public HTTPS URL" in
    if advertised_name url <> Some name then
      fail "Portal advertised a different prefix; refusing to report publication";
    `Assoc ["status", `String "published"; "id", `String id;
      "backend", `String "portal"; "url", `String url; "name", `String name;
      "advertised", `String name; "port", `Int port;
      "identity_path", `String identity;
      "note", `String ("Public and relay-listed until publish_web stop name=" ^
        name ^ " or session exit; keep the localhost server running.")]
  with exn ->
    (try Workspace_process.kill_job manager ~id with Workspace_process.Error _ -> ());
    raise exn

let stop manager ~id =
  Workspace_process.kill_job manager ~id;
  `Assoc ["status", `String "stopped"; "id", `String id]

let list manager =
  let tunnels = List.filter_map (fun (job : Workspace_process.job_summary) ->
    if String.starts_with ~prefix:"portal:" job.id then
      Some (`Assoc ["name", `String (String.sub job.id 7 (String.length job.id - 7));
        "status", `String (match job.status with
          | Workspace_process.Running -> "running" | Workspace_process.Completed _ -> "exited");
        "command", `String job.command])
    else None) (Workspace_process.jobs manager) in
  `Assoc ["tunnels", `List tunnels]
