exception Error of string
let job_id name = "portal:" ^ name


let fail message = raise (Error message)

let portal_variable = "PAVE_PORTAL"

(* `portal expose` resolves on PATH; PAVE_PORTAL pins an exact binary. *)
let detect_portal () =
  let executable path =
    (not (Sys.is_directory path)) &&
    (try Unix.access path [Unix.X_OK]; true with Unix.Unix_error _ -> false) in
  (match Sys.getenv_opt portal_variable with
   | Some path when path <> "" ->
       if executable path then path
       else fail (portal_variable ^ " is set but not executable: " ^ path)
   | Some _ -> fail (portal_variable ^ " is empty")
   | None ->
       let dirs = match Sys.getenv_opt "PATH" with
         | Some path -> String.split_on_char ':' path
         | None -> [] in
       match List.find_map (fun dir ->
         let candidate = Filename.concat dir "portal" in
         if executable candidate then Some candidate else None) dirs with
       | Some path -> path
       | None -> fail
         "portal CLI not found; install portal-tunnel or set PAVE_PORTAL")

let publish_name name =
  if String.length name = 0 || String.length name > 63 then
    fail "tunnel name must be 1-63 characters"
  else if not (String.for_all (function
      | 'a'..'z' | '0'..'9' | '-' -> true
      | _ -> false) name) then
    fail "tunnel name must be a lowercase DNS label (letters, digits, hyphens)"
  else name

(* zerolog console output: `… INF service ready at https://NAME.RELAY …`;
   JSON output carries "public_url":"https://NAME.RELAY". The first wins. *)
let public_url output =
  let url text start =
    let finish = ref start in
    while !finish < String.length text &&
      (let c = text.[!finish] in
       Char.code c > 32 && c <> '"' && c <> '\'' && c <> '}' && c <> ']')
    do incr finish done;
    String.sub text start (!finish - start) in
  let needle = "service ready at " in
  let rec find from =
    match find_substring output needle from with
    | Some index ->
        let at = index + String.length needle in
        if at < String.length output then Some (url output at)
        else None
    | None -> None
  and find_substring text needle from =
    if String.length text - from < String.length needle then None
    else if String.sub text from (String.length needle) = needle then Some from
    else find_substring text needle (from + 1) in
  (match find 0 with
   | Some value when String.starts_with ~prefix:"https://" value ->
       Some (String.trim value)
   | _ ->
       (* JSON form: "public_url":"https://…" *)
       let key = "\"public_url\":\"https://" in
       let rec find_key from =
         if String.length output - from < String.length key then None
         else if String.sub output from (String.length key) = key
           then Some from
         else find_key (from + 1) in
       match find_key 0 with
       | Some index -> Some (url output (index + String.length key - 8))
       | None -> None)

let ready_regex = "service ready at \\|\"public_url\""

let publish ?cancel manager ~id ~port ~name =
  let executable = detect_portal () in
  if not (Workspace_process.release_finished manager ~id) then
    fail ("tunnel " ^ name ^ " is already running; stop it first");
  let arguments = ["expose"; string_of_int port; "--name"; name] in
  Workspace_process.start manager ~id ~program:executable ~arguments ();
  let ready =
    try Workspace_process.wait_ready manager ~id ?cancel
      ~timeout_seconds:45 ~log_regex:ready_regex ()
    with exn ->
      Workspace_process.kill_job manager ~id;
      raise exn in
  if not ready then (
    Workspace_process.kill_job manager ~id;
    let output = (Workspace_process.read_output manager ~id ()).output in
    fail ("portal did not publish a public URL within 45 s: " ^
      String.trim output))
  else
    let output = (Workspace_process.read_output manager ~id ()).output in
    let url = match public_url output with
      | Some value -> value
      | None -> fail "portal reported readiness without a public URL" in
    `Assoc [ "status", `String "published";
             "id", `String id;
             "url", `String url;
             "port", `Int port;
             "note", `String
               "Public until stopped with publish_web stop or session exit" ]

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
