module Backend : sig
  exception Error of string
  val max_read_bytes : int
  val max_write_bytes : int
  val max_output_bytes : int
  val max_timeout_seconds : int
  val default_timeout_seconds : int

  type endpoint = {
    host : string;
    user : string;
    remote_root : string;
    known_hosts : string;
  }

  type run_request = {
    program : string;
    arguments : string list;
    stdin : string;
    timeout_seconds : int;
    output_limit : int;
    cancel : (unit -> bool) option;
  }

  type runner = run_request -> Workspace_process.result
  type known_host_verifier = runner -> endpoint -> string
  type selector = Raw | Lines of (int * int) list | Tail of int
  type session

  val open_session :
    ?runner:runner ->
    ?verify_known_host:known_host_verifier ->
    ?cancel:(unit -> bool) ->
    ?timeout_seconds:int ->
    owner:string -> endpoint:endpoint -> host_trusted:bool ->
    network_approved:bool -> unit -> session

  val close_session : owner:string -> session -> unit
  val session_endpoint : session -> endpoint

  val read_file :
    ?cancel:(unit -> bool) -> ?timeout_seconds:int ->
    owner:string -> read_approved:bool -> network_approved:bool -> session ->
    path:string -> unit -> string

  val write_file :
    ?cancel:(unit -> bool) -> ?timeout_seconds:int ->
    owner:string -> network_approved:bool -> write_approved:bool ->
    session -> path:string -> contents:string -> unit -> int

  val run_command :
    ?cancel:(unit -> bool) -> ?timeout_seconds:int ->
    owner:string -> network_approved:bool -> execution_approved:bool ->
    session -> program:string -> arguments:string list -> unit -> string

  val parse_uri : endpoint -> string -> string * selector option

  val read_uri :
    ?cancel:(unit -> bool) -> ?timeout_seconds:int ->
    owner:string -> read_approved:bool -> network_approved:bool ->
    session -> string -> unit -> string
end = struct

exception Error of string

let max_read_bytes = 65_536
let max_write_bytes = 65_536
let max_output_bytes = 65_536
let max_timeout_seconds = 120
let default_timeout_seconds = 20
let max_remote_path_bytes = 1_024

type endpoint = {
  host : string;
  user : string;
  remote_root : string;
  known_hosts : string;
}

type run_request = {
  program : string;
  arguments : string list;
  stdin : string;
  timeout_seconds : int;
  output_limit : int;
  cancel : (unit -> bool) option;
}

type runner = run_request -> Workspace_process.result
type known_host_verifier = runner -> endpoint -> string

type selector = Raw | Lines of (int * int) list | Tail of int

type session = {
  owner : string;
  endpoint : endpoint;
  fingerprint : string;
  runner : runner;
  verify_known_host : known_host_verifier;
  mutable closed : bool;
}

let fail message = raise (Error message)

let safe_environment () =
  let variables = [
    "PATH", Some "/usr/bin:/bin:/usr/local/bin:/opt/homebrew/bin:/opt/local/bin";
    "LANG", Some "C";
    "HOME", Sys.getenv_opt "HOME";
    "SSH_AUTH_SOCK", Sys.getenv_opt "SSH_AUTH_SOCK"
  ] in
  List.filter_map (fun (name, value) ->
    Option.map (fun value -> name, value) value) variables

let default_runner request =
  Workspace_process.run ?cancel:request.cancel
    ~inherit_environment:false ~environment:(safe_environment ())
    ~timeout_seconds:request.timeout_seconds ~output_limit:request.output_limit
    ~stdin:request.stdin ~program:request.program ~arguments:request.arguments ()

let is_ascii_alnum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

let is_safe_host_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' -> true
  | _ -> false

let normalize_host host =
  if host = "" || String.length host > 253 ||
     not (String.for_all is_safe_host_char host) then
    fail "SSH host must be a DNS name or IPv4 address";
  let host = String.lowercase_ascii host in
  let labels = String.split_on_char '.' host in
  if List.exists (fun label ->
       label = "" || String.length label > 63 ||
       not (is_ascii_alnum label.[0]) ||
       not (is_ascii_alnum label.[String.length label - 1]) ||
       not (String.for_all (fun c -> is_ascii_alnum c || c = '-') label)) labels then
    fail "SSH host must be a DNS name or IPv4 address";
  host

let validate_user user =
  if user = "" || String.length user > 64 ||
     not (is_ascii_alnum user.[0] || user.[0] = '_') ||
     not (String.for_all (fun c -> is_ascii_alnum c || c = '_' || c = '-' || c = '.') user) then
    fail "SSH user is invalid";
  user

let safe_path_chars path =
  String.for_all (fun c -> is_ascii_alnum c || c = '/' || c = '_' || c = '-' || c = '.') path

let path_segments path = String.split_on_char '/' path

let normalize_remote_path ~absolute path =
  if path = "" || String.length path > max_remote_path_bytes ||
     String.contains path '\000' || not (safe_path_chars path) ||
     (absolute && path.[0] <> '/') || ((not absolute) && path.[0] = '/') then
    fail "SSH path is invalid";
  let segments = path_segments path in
  if List.exists (fun segment -> segment = "." || segment = "..") segments then
    fail "SSH path traversal is not allowed";
  let segments = List.filter (( <> ) "") segments in
  if not absolute && segments = [] then fail "SSH file path is required";
  let joined = String.concat "/" segments in
  if absolute then (if joined = "" then "/" else "/" ^ joined) else joined

let within_remote root path =
  path = root ||
  (let prefix = if root = "/" then root else root ^ "/" in
   String.length path >= String.length prefix &&
   String.sub path 0 (String.length prefix) = prefix)

let canonical_known_hosts path =
  try
    let stat = Unix.lstat path in
    if stat.Unix.st_kind <> Unix.S_REG || stat.Unix.st_uid <> Unix.getuid () ||
       stat.Unix.st_perm land 0o022 <> 0 || stat.Unix.st_size > 1_048_576 then
      fail "known_hosts must be an owner-controlled regular file under 1 MiB";
    let canonical = Unix.realpath path in
    if not (safe_path_chars canonical) then
      fail "known_hosts path contains unsupported characters";
    canonical
  with
  | Error _ as exn -> raise exn
  | Unix.Unix_error _ -> fail "known_hosts file is unavailable"

let normalize_endpoint endpoint =
  let host = normalize_host endpoint.host in
  let user = validate_user endpoint.user in
  let remote_root = normalize_remote_path ~absolute:true endpoint.remote_root in
  let known_hosts = canonical_known_hosts endpoint.known_hosts in
  { host; user; remote_root; known_hosts }

let valid_fingerprint fingerprint =
  let prefix = "SHA256:" in
  let n = String.length prefix in
  String.length fingerprint > n &&
  String.sub fingerprint 0 n = prefix &&
  String.for_all (function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | '/' | '=' -> true
    | _ -> false) (String.sub fingerprint n (String.length fingerprint - n))

let validate_fingerprint fingerprint =
  if String.length fingerprint > 128 || not (valid_fingerprint fingerprint) then
    fail "known_hosts did not provide a valid SSH host fingerprint";
  fingerprint

let successful result = result.Workspace_process.termination = Workspace_process.Exited 0 &&
                        not result.Workspace_process.truncated

let run_request (runner : runner) ?cancel ?(timeout_seconds = default_timeout_seconds)
    ?(output_limit = max_output_bytes) ~program ~arguments ~stdin () =
  if timeout_seconds < 1 || timeout_seconds > max_timeout_seconds then
    fail "SSH timeout must be between 1 and 120 seconds";
  if output_limit < 0 || output_limit > max_output_bytes then
    fail "SSH output limit is outside the supported bound";
  (match cancel with Some cancelled when cancelled () -> fail "SSH operation cancelled" | _ -> ());
  let request = { program; arguments; stdin; timeout_seconds; output_limit; cancel } in
  let result = try runner request with
    | _ -> fail "SSH operation could not be started" in
  if String.length result.Workspace_process.output > output_limit ||
     result.Workspace_process.bytes_received > output_limit then
    fail "SSH process output exceeded its bound";
  result

let check_process result =
  if result.Workspace_process.termination = Workspace_process.Cancelled then
    fail "SSH operation cancelled";
  if result.Workspace_process.termination = Workspace_process.Timed_out then
    fail "SSH operation timed out";
  if not (successful result) then fail "SSH operation failed"

let words line =
  String.split_on_char ' ' (String.trim line) |> List.filter (( <> ) "")

let default_known_host_verifier runner endpoint =
  let query = run_request runner ~program:"/usr/bin/ssh-keygen"
      ~arguments:["-F"; endpoint.host; "-f"; endpoint.known_hosts]
      ~stdin:"" ~output_limit:max_output_bytes () in
  if not (successful query) then fail "SSH host is unknown or its pinned key is unavailable";
  let key_lines = String.split_on_char '\n' query.output
      |> List.map String.trim
      |> List.filter (fun line -> line <> "" && line.[0] <> '#') in
  let public_key_line = match key_lines with
    | [line] ->
        (match words line with
         | marker :: _ when marker = "@cert-authority" || marker = "@revoked" ->
             fail "known_hosts entry does not pin one host key"
         | _host :: key_type :: key_data :: _ ->
             let begins prefix text =
               String.length text >= String.length prefix &&
               String.sub text 0 (String.length prefix) = prefix in
             let contains fragment text =
               let n = String.length text and m = String.length fragment in
               let rec loop index = index + m <= n &&
                 (String.sub text index m = fragment || loop (index + 1)) in
               loop 0 in
             let supported_type =
               begins "ssh-" key_type || begins "ecdsa-" key_type || begins "sk-" key_type in
             if not supported_type || contains "-cert-" key_type then
               fail "known_hosts entry does not pin one host key";
             key_type ^ " " ^ key_data
         | _ -> fail "known_hosts entry does not pin one host key")
    | _ -> fail "known_hosts must contain exactly one matching host key"
  in
  let temp = Filename.temp_file ~temp_dir:"/tmp" "pave-ssh-key-" ".known" in
  Fun.protect ~finally:(fun () -> try Unix.unlink temp with Unix.Unix_error _ -> ())
    (fun () ->
       Workspace_path.with_fd temp [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
         (fun fd -> Workspace_path.write_all fd (public_key_line ^ "\n"));
       let fingerprint = run_request runner ~program:"/usr/bin/ssh-keygen"
           ~arguments:["-lf"; temp; "-E"; "sha256"] ~stdin:"" ~output_limit:4_096 () in
       if not (successful fingerprint) then
         fail "known_hosts host key could not be fingerprinted";
       match String.split_on_char '\n' fingerprint.output
             |> List.map words |> List.filter (fun fields -> fields <> []) with
       | [ _bits :: value :: _ ] -> validate_fingerprint value
       | _ -> fail "known_hosts host key could not be fingerprinted")

(* Ignore SSH config aliases, proxies, forwarding, and control sockets. The literal
   configured host is also the known_hosts alias, so a user config cannot retarget it. *)
let host_options endpoint =
  ["-F"; "/dev/null";
   "-oBatchMode=yes";
   "-oStrictHostKeyChecking=yes";
   "-oUserKnownHostsFile=" ^ endpoint.known_hosts;
   "-oGlobalKnownHostsFile=/dev/null";
   "-oUpdateHostKeys=no";
   "-oCheckHostIP=no";
   "-oVerifyHostKeyDNS=no";
   "-oCanonicalizeHostname=no";
   "-oHostKeyAlias=" ^ endpoint.host;
   "-oProxyCommand=none";
   "-oProxyJump=none";
   "-oPermitLocalCommand=no";
   "-oForwardAgent=no";
   "-oForwardX11=no";
   "-oClearAllForwardings=yes";
   "-oControlMaster=no";
   "-oControlPath=none";
   "-oIdentitiesOnly=yes"]

let target endpoint = endpoint.user ^ "@" ^ endpoint.host

let sftp_arguments endpoint =
  host_options endpoint @ ["-q"; "-b"; "-"; target endpoint]
let verification_runner ?cancel ?timeout_seconds runner request =
  let timeout_seconds = match timeout_seconds with
    | None -> request.timeout_seconds
    | Some limit -> min limit request.timeout_seconds in
  let cancelled () =
    Option.fold ~none:false ~some:(fun check -> check ()) cancel ||
    Option.fold ~none:false ~some:(fun check -> check ()) request.cancel in
  if cancelled () then fail "SSH operation cancelled";
  let result = runner { request with timeout_seconds; cancel = Some cancelled } in
  (match result.Workspace_process.termination with
   | Workspace_process.Cancelled -> fail "SSH operation cancelled"
   | Workspace_process.Timed_out -> fail "SSH operation timed out"
   | _ -> ());
  result


let verify_session ?cancel ?timeout_seconds session =
  if session.closed then fail "SSH session is closed";
  (match cancel with Some cancelled when cancelled () -> fail "SSH operation cancelled" | _ -> ());
  let current = try session.verify_known_host
      (verification_runner ?cancel ?timeout_seconds session.runner) session.endpoint
    |> validate_fingerprint with
    | Error ("SSH operation cancelled" | "SSH operation timed out") as exn -> raise exn
    | _ -> fail "SSH host key is not trusted" in
  if current <> session.fingerprint then
    fail "SSH host key changed since this owner-bound session was opened"

let sftp_call ?cancel ?(timeout_seconds = default_timeout_seconds) session script =
  verify_session ?cancel ~timeout_seconds session;
  let result = run_request session.runner ?cancel ~timeout_seconds
      ~program:"/usr/bin/sftp" ~arguments:(sftp_arguments session.endpoint)
      ~stdin:(script ^ "\n") ~output_limit:max_output_bytes () in
  result

let require_sftp_success result = check_process result; result

let parse_realpath output =
  let lines = String.split_on_char '\n' output |> List.map String.trim
      |> List.filter (( <> ) "") in
  let prefix = "Remote working directory: " in
  match lines with
  | [line] when String.length line >= String.length prefix &&
                String.sub line 0 (String.length prefix) = prefix ->
      let resolved = String.sub line (String.length prefix)
          (String.length line - String.length prefix) in
      (try normalize_remote_path ~absolute:true resolved
       with Error _ -> fail "SSH server returned an invalid canonical path")
  | _ -> fail "SSH server did not return one canonical path"

let remote_realpath ?cancel ?timeout_seconds session path =
  let result = sftp_call ?cancel ?timeout_seconds session
      ("@cd " ^ path ^ "\n@pwd")
      |> require_sftp_success in
  parse_realpath result.Workspace_process.output

let ensure_remote_scope session path =
  if not (within_remote session.endpoint.remote_root path) then
    fail "SSH path escapes the configured remote root";
  path

let check_owner session owner =
  if owner = "" || owner <> session.owner then
    fail "SSH session belongs to another owner";
  if session.closed then fail "SSH session is closed"

let check_approval label approved = if not approved then fail (label ^ " approval is required")

let check_timeout timeout =
  if timeout < 1 || timeout > max_timeout_seconds then
    fail "SSH timeout must be between 1 and 120 seconds"

let open_session ?(runner = default_runner) ?(verify_known_host = default_known_host_verifier)
    ?cancel ?(timeout_seconds = default_timeout_seconds) ~owner ~endpoint
    ~host_trusted ~network_approved () =
  if owner = "" || String.length owner > 256 || String.contains owner '\000' then
    fail "SSH owner is invalid";
  check_approval "Remote host trust" host_trusted;
  check_approval "Network access" network_approved;
  check_timeout timeout_seconds;
  (match cancel with Some cancelled when cancelled () -> fail "SSH operation cancelled" | _ -> ());
  let endpoint = normalize_endpoint endpoint in
  let fingerprint = try verify_known_host
      (verification_runner ?cancel ~timeout_seconds runner) endpoint |> validate_fingerprint with
    | Error ("SSH operation cancelled" | "SSH operation timed out") as exn -> raise exn
    | _ -> fail "SSH host is unknown or its pinned key is unavailable" in
  let provisional = { owner; endpoint; fingerprint;
                      runner; verify_known_host; closed = false } in
  let resolved_root = remote_realpath ?cancel ~timeout_seconds provisional endpoint.remote_root in
  if resolved_root <> endpoint.remote_root then
    fail "configured SSH root resolves through a remote symlink";
  let endpoint = { endpoint with remote_root = resolved_root } in
  { provisional with endpoint }

let close_session ~owner session =
  check_owner session owner;
  session.closed <- true
let session_endpoint session = session.endpoint

let check_relative_path path = normalize_remote_path ~absolute:false path

let remote_file_path session relative =
  let relative = check_relative_path relative in
  let path = if session.endpoint.remote_root = "/" then "/" ^ relative
    else session.endpoint.remote_root ^ "/" ^ relative in
  if String.length path > max_remote_path_bytes then fail "SSH path is too long";
  path

let parse_listing output =
  let lines = String.split_on_char '\n' output |> List.map String.trim
      |> List.filter (( <> ) "") in
  match lines with
  | [line] ->
      (match words line with
       | mode :: _links :: _uid :: _gid :: size :: _ when String.length mode >= 1 ->
           let kind = mode.[0] in
           let size = try int_of_string size with Failure _ -> -1 in
           if kind <> '-' then fail "SSH path is not a regular file";
           if size < 0 then fail "SSH server returned an invalid file size";
           size
       | _ -> fail "SSH server returned an invalid file listing")
  | _ -> fail "SSH server did not return one file listing"

let contains_case_insensitive text fragment =
  let lower = String.lowercase_ascii text in
  let fragment = String.lowercase_ascii fragment in
  let n = String.length lower and m = String.length fragment in
  let rec loop i = i + m <= n &&
    (String.sub lower i m = fragment || loop (i + 1)) in
  loop 0

let target_is_absent result =
  (match result.Workspace_process.termination with
   | Workspace_process.Exited code when code <> 0 -> true
   | _ -> false) &&
  (contains_case_insensitive result.output "no such file" ||
   contains_case_insensitive result.output "not found")
let remote_size ?cancel ?timeout_seconds session path =
  let result = sftp_call ?cancel ?timeout_seconds session ("ls -ln " ^ path) in
  match result.Workspace_process.termination with
  | Workspace_process.Exited 0 -> Some (parse_listing result.output)
  | _ when target_is_absent result -> None
  | _ ->
      check_process result;
      None

let validate_existing_remote_path ?cancel ?timeout_seconds session path =
  let parent = Filename.dirname path in
  let resolved_parent = remote_realpath ?cancel ?timeout_seconds session parent in
  ignore (ensure_remote_scope session resolved_parent);
  let resolved = if resolved_parent = "/" then "/" ^ Filename.basename path
    else resolved_parent ^ "/" ^ Filename.basename path in
  ensure_remote_scope session resolved

let temp_file () =
  try Filename.temp_file ~temp_dir:"/tmp" "pave-ssh-transfer-" ".tmp"
  with _ -> fail "could not create a bounded SSH transfer file"

let remove_temp path = try Unix.unlink path with Unix.Unix_error _ -> ()

let read_file ?cancel ?(timeout_seconds = default_timeout_seconds) ~owner
    ~read_approved ~network_approved session ~path () =
  check_owner session owner;
  check_approval "Remote read" read_approved;
  check_approval "Network access" network_approved;
  check_timeout timeout_seconds;
  let path = remote_file_path session path in
  let path = validate_existing_remote_path ?cancel ~timeout_seconds session path in
  let size = match remote_size ?cancel ~timeout_seconds session path with
    | Some size when size <= max_read_bytes -> size
    | Some _ -> fail "SSH file exceeds the 65536-byte read limit"
    | None -> fail "SSH remote file is unavailable" in
  let local = temp_file () in
  Fun.protect ~finally:(fun () -> remove_temp local) (fun () ->
    let result = sftp_call ?cancel ~timeout_seconds session ("get " ^ path ^ " " ^ local)
        |> require_sftp_success in
    if result.Workspace_process.truncated then fail "SSH transfer output exceeded its bound";
    let stat = Unix.lstat local in
    if stat.Unix.st_kind <> Unix.S_REG || stat.Unix.st_size > max_read_bytes ||
       stat.Unix.st_size <> size then fail "SSH file changed or exceeded the read limit";
    Workspace_path.read_bounded local max_read_bytes)

let ensure_remote_write_target ?cancel ?timeout_seconds session path =
  let parent = Filename.dirname path in
  let resolved_parent = remote_realpath ?cancel ?timeout_seconds session parent in
  ignore (ensure_remote_scope session resolved_parent);
  let path =
    if resolved_parent = "/" then "/" ^ Filename.basename path
    else resolved_parent ^ "/" ^ Filename.basename path in
  (match remote_size ?cancel ?timeout_seconds session path with
   | Some _ -> ()
   | None ->
       let result = sftp_call ?cancel ?timeout_seconds session ("ls -ln " ^ path) in
       if not (target_is_absent result) then fail "SSH remote write target could not be checked");
  path

let write_file ?cancel ?(timeout_seconds = default_timeout_seconds) ~owner
    ~network_approved ~write_approved session ~path ~contents () =
  check_owner session owner;
  check_approval "Network access" network_approved;
  check_approval "Remote write" write_approved;
  check_timeout timeout_seconds;
  if String.length contents > max_write_bytes then
    fail "SSH content exceeds the 65536-byte write limit";
  let path = remote_file_path session path in
  let path = ensure_remote_write_target ?cancel ~timeout_seconds session path in
  let local = temp_file () in
  Fun.protect ~finally:(fun () -> remove_temp local) (fun () ->
    Workspace_path.with_fd local [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
      (fun fd -> Workspace_path.write_all fd contents);
    let result = sftp_call ?cancel ~timeout_seconds session ("put " ^ local ^ " " ^ path)
        |> require_sftp_success in
    if result.Workspace_process.truncated then fail "SSH transfer output exceeded its bound";
    String.length contents)

let shell_quote text =
  "'" ^ String.concat "'\\''" (String.split_on_char '\'' text) ^ "'"

let validate_command_part label max_length text =
  if text = "" || String.length text > max_length || String.contains text '\000' ||
     String.contains text '\n' || String.contains text '\r' then
    fail (label ^ " is invalid or too long");
  text

let ssh_arguments endpoint = host_options endpoint @ [target endpoint]

let run_command ?cancel ?(timeout_seconds = default_timeout_seconds) ~owner
    ~network_approved ~execution_approved session ~program ~arguments () =
  check_owner session owner;
  check_approval "Network access" network_approved;
  check_approval "Remote execution" execution_approved;
  check_timeout timeout_seconds;
  let program = validate_command_part "Remote program" 256 program in
  if List.length arguments > 128 then fail "too many remote command arguments";
  let arguments = List.map (validate_command_part "Remote argument" 4_096) arguments in
  if List.fold_left (fun size argument -> size + String.length argument) (String.length program) arguments > 32_768 then
    fail "remote command arguments exceed their size limit";
  verify_session ?cancel ~timeout_seconds session;
  let command = "cd " ^ shell_quote session.endpoint.remote_root ^ " && exec " ^
    String.concat " " (List.map shell_quote (program :: arguments)) in
  if String.length command > max_output_bytes then fail "remote command is too long";
  let result = run_request session.runner ?cancel ~timeout_seconds ~output_limit:max_output_bytes
      ~program:"/usr/bin/ssh" ~arguments:(ssh_arguments session.endpoint @ [command])
      ~stdin:"" () in
  if result.Workspace_process.truncated then fail "SSH command output exceeded its bound";
  check_process result;
  result.output

let starts_with text prefix =
  String.length text >= String.length prefix && String.sub text 0 (String.length prefix) = prefix

let parse_positive text =
  if text = "" || not (String.for_all (fun c -> c >= '0' && c <= '9') text) then None
  else try
    let n = int_of_string text in
    if n > 0 && n <= max_read_bytes then Some n else None
  with Failure _ | Invalid_argument _ -> None

let parse_range text =
  match String.index_opt text '-' with
  | None -> Option.map (fun line -> (line, line)) (parse_positive text)
  | Some split when split > 0 && split + 1 < String.length text ->
      (match parse_positive (String.sub text 0 split),
             parse_positive (String.sub text (split + 1) (String.length text - split - 1)) with
       | Some first, Some last when first <= last -> Some (first, last)
       | _ -> None)
  | _ -> None

let parse_selector text =
  if text = "raw" then Some Raw
  else if starts_with text "-" then
    Option.map (fun count -> Tail count) (parse_positive (String.sub text 1 (String.length text - 1)))
  else
    let pieces = String.split_on_char ',' text in
    if List.length pieces > 64 then None
    else
      let parsed = List.map parse_range pieces in
      if parsed <> [] && List.for_all Option.is_some parsed then
        Some (Lines (List.map Option.get parsed))
      else None

let parse_uri endpoint uri =
  let endpoint = normalize_endpoint endpoint in
  if String.length uri > 8_192 || not (starts_with uri "ssh://") ||
     String.contains uri '?' || String.contains uri '#' || String.contains uri '%' then
    fail "SSH URI must use the fixed ssh://user@host/path form";
  let rest = String.sub uri 6 (String.length uri - 6) in
  let slash = match String.index_opt rest '/' with Some index -> index | None -> fail "SSH URI is missing a path" in
  let authority = String.sub rest 0 slash in
  let file_and_selector = String.sub rest (slash + 1) (String.length rest - slash - 1) in
  let expected = endpoint.user ^ "@" ^ endpoint.host in
  if authority <> expected then fail "SSH URI does not match the configured host and user";
  let file, selector = match String.rindex_opt file_and_selector ':' with
    | None -> file_and_selector, None
    | Some index ->
        let token = String.sub file_and_selector (index + 1) (String.length file_and_selector - index - 1) in
        (match parse_selector token with
         | Some selector -> String.sub file_and_selector 0 index, Some selector
         | None -> fail "SSH URI selector is invalid") in
  let path = check_relative_path file in
  (path, selector)

let content_lines contents =
  let lines = Array.of_list (String.split_on_char '\n' contents) in
  let count = Array.length lines in
  let count =
    if count > 0 && lines.(count - 1) = "" then count - 1 else count in
  lines, count

let select_line_ranges contents lines count ranges =
  let output = Buffer.create (min (String.length contents) max_output_bytes) in
  let selected = ref false in
  List.iter (fun (first, last) ->
    for index = max 1 first to min last count do
      let line = lines.(index - 1) in
      let separator = if !selected then 1 else 0 in
      if String.length line + separator > max_output_bytes - Buffer.length output then
        fail "SSH selected output exceeded its bound";
      if !selected then Buffer.add_char output '\n';
      Buffer.add_string output line;
      selected := true
    done) ranges;
  Buffer.contents output

let select_lines contents ranges =
  let lines, count = content_lines contents in
  select_line_ranges contents lines count ranges

let apply_selector contents = function
  | Raw -> contents
  | Lines ranges ->
      if String.contains contents '\000' then fail "SSH file is binary; use :raw for bounded bytes";
      select_lines contents ranges
  | Tail count ->
      if String.contains contents '\000' then fail "SSH file is binary; use :raw for bounded bytes";
      let lines, line_count = content_lines contents in
      select_line_ranges contents lines line_count [max 1 (line_count - count + 1), line_count]

let read_uri ?cancel ?timeout_seconds ~owner ~read_approved ~network_approved session uri () =
  check_owner session owner;
  let path, selector = parse_uri session.endpoint uri in
  let contents = read_file ?cancel ?timeout_seconds ~owner ~read_approved
      ~network_approved session ~path () in
  match selector with
  | None ->
      if String.contains contents '\000' then fail "SSH file is binary; use :raw for bounded bytes";
      contents
  | Some selector -> apply_selector contents selector

end

include Backend
