type credential = {
  access : string;
  refresh : string option;
  expires_at : float option;
  account_id : string option;
  metadata : (string * string) list;
}

exception Storage_error of string

let fail reason = raise (Storage_error reason)

let guard f =
  try f () with
  | Unix.Unix_error _ | Sys_error _ -> fail "OAuth credential storage I/O failed"
  | Yojson.Json_error _ -> fail "Invalid OAuth credential data"

let max_bytes = 1024 * 1024

let default_path () =
  let config_home = match Sys.getenv_opt "XDG_CONFIG_HOME" with
    | Some dir when dir <> "" && not (Filename.is_relative dir) -> dir
    | _ -> match Sys.getenv_opt "HOME" with
      | Some home when home <> "" && not (Filename.is_relative home) ->
          Filename.concat home ".config"
      | _ -> fail "No home directory for OAuth credentials" in
  Filename.concat (Filename.concat config_home "pave") "oauth.json"

let stat_if_exists path =
  try Some (Unix.lstat path) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> None

let ensure_directory target =
  let rec create dir =
    match stat_if_exists dir with
    | Some stat ->
        let kind =
          if stat.Unix.st_kind = Unix.S_LNK && dir <> target then
            (Unix.stat dir).Unix.st_kind
          else stat.Unix.st_kind in
        if kind <> Unix.S_DIR then fail "OAuth config directory is not a directory"
    | None ->
        let parent = Filename.dirname dir in
        if parent = dir then fail "OAuth config directory is unavailable";
        create parent;
        (try Unix.mkdir dir 0o700 with
         | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
        (match stat_if_exists dir with
         | Some stat when stat.Unix.st_kind = Unix.S_DIR
                       && stat.Unix.st_uid = Unix.geteuid () -> ()
         | _ -> fail "OAuth config directory is unsafe")
  in
  (* Only the credential directory is made private; existing XDG/HOME ancestors
     may intentionally be shared and must not be chmod'ed. *)
  create target;
  let stat = Unix.lstat target in
  if stat.Unix.st_kind <> Unix.S_DIR || stat.Unix.st_uid <> Unix.geteuid () then
    fail "OAuth config directory is unsafe";
  if stat.Unix.st_perm <> 0o700 then Unix.chmod target 0o700;
  if (Unix.lstat target).Unix.st_ino <> stat.Unix.st_ino then
    fail "OAuth config directory changed during access"

let regular_file path =
  match stat_if_exists path with
  | None -> None
  | Some stat ->
      if stat.Unix.st_kind <> Unix.S_REG || stat.Unix.st_nlink <> 1
         || stat.Unix.st_uid <> Unix.geteuid () then
        fail "OAuth credential file is unsafe";
      Some stat

let open_checked path flags =
  let before = match regular_file path with
    | Some stat -> stat
    | None -> fail "OAuth credential file disappeared" in
  let fd = Unix.openfile path (Unix.O_CLOEXEC :: flags) 0 in
  try
    let after = Unix.fstat fd in
    let current = regular_file path in
    if after.Unix.st_dev <> before.Unix.st_dev
       || after.Unix.st_ino <> before.Unix.st_ino
       || after.Unix.st_nlink <> 1
       || after.Unix.st_uid <> Unix.geteuid ()
       || (match current with
           | None -> true
           | Some stat -> stat.Unix.st_dev <> after.Unix.st_dev
                        || stat.Unix.st_ino <> after.Unix.st_ino) then
      fail "OAuth credential file changed during access";
    fd
  with exn -> Unix.close fd; raise exn

let create_checked path =
  let fd = Unix.openfile path
    [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o600 in
  try Unix.fchmod fd 0o600; fd
  with exn -> Unix.close fd; raise exn

let read_all fd =
  let stat = Unix.fstat fd in
  if stat.Unix.st_size > max_bytes then fail "OAuth credential data exceeds size limit";
  if stat.Unix.st_perm land 0o077 <> 0 then
    fail "OAuth credential file is not private";
  let buffer = Bytes.create 8192 in
  let result = Buffer.create (min stat.Unix.st_size 8192) in
  let rec loop () =
    let n = Unix.read fd buffer 0 (min 8192 (max_bytes + 1 - Buffer.length result)) in
    if n > 0 then (
      Buffer.add_subbytes result buffer 0 n;
      if Buffer.length result > max_bytes then
        fail "OAuth credential data exceeds size limit";
      loop ())
  in
  loop ();
  Buffer.contents result

let string = function
  | `String value -> value
  | _ -> fail "Invalid OAuth credential data"

let optional_string = function
  | `Null -> None
  | json -> Some (string json)

let object_fields = function
  | `Assoc fields ->
      let seen = Hashtbl.create (List.length fields) in
      List.iter (fun (key, _) ->
        if Hashtbl.mem seen key then fail "Duplicate OAuth credential field";
        Hashtbl.add seen key ()) fields;
      fields
  | _ -> fail "Invalid OAuth credential data"

let field fields key =
  match List.assoc_opt key fields with
  | Some value -> value
  | None -> fail "Invalid OAuth credential data"

let credential_of_json json =
  let fields = object_fields json in
  let expires_at = match field fields "expires_at" with
    | `Null -> None
    | `Int n -> Some (float_of_int n)
    | `Float n when Float.is_finite n -> Some n
    | _ -> fail "Invalid OAuth credential data" in
  let metadata = object_fields (field fields "metadata")
    |> List.map (fun (key, value) -> (key, string value)) in
  { access = string (field fields "access");
    refresh = optional_string (field fields "refresh");
    expires_at;
    account_id = optional_string (field fields "account_id");
    metadata }

let json_of_credential credential =
  let optional = function None -> `Null | Some text -> `String text in
  let expires_at = match credential.expires_at with
    | None -> `Null
    | Some n when Float.is_finite n -> `Float n
    | Some _ -> fail "Invalid OAuth credential expiry" in
  let metadata = List.sort (fun (a, _) (b, _) -> String.compare a b)
    credential.metadata in
  let seen = Hashtbl.create (List.length metadata) in
  List.iter (fun (key, _) ->
    if key = "" || Hashtbl.mem seen key then
      fail "Invalid OAuth credential metadata";
    Hashtbl.add seen key ()) metadata;
  `Assoc ["access", `String credential.access;
          "refresh", optional credential.refresh;
          "expires_at", expires_at;
          "account_id", optional credential.account_id;
          "metadata", `Assoc (List.map (fun (key, value) -> key, `String value) metadata)]

let read_store path =
  match regular_file path with
  | None -> []
  | Some _ ->
      let fd = open_checked path [Unix.O_RDONLY] in
      let text = Fun.protect ~finally:(fun () -> Unix.close fd)
        (fun () -> read_all fd) in
      let json = Yojson.Basic.from_string text in
      let root = object_fields json in
      if field root "version" <> `Int 1 then fail "Unsupported OAuth credential version";
      object_fields (field root "providers")
      |> List.map (fun (name, value) -> name, credential_of_json value)

let write_all fd data =
  let rec loop offset =
    if offset < String.length data then (
      let count = try Unix.write_substring fd data offset (String.length data - offset)
        with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
      if count = 0 then fail "OAuth credential write failed";
      if count < 0 then loop offset else loop (offset + count)) in
  loop 0

let write_store path providers =
  let json = `Assoc ["version", `Int 1;
    "providers", `Assoc (List.sort (fun (a, _) (b, _) -> String.compare a b)
      (List.map (fun (name, credential) -> name, json_of_credential credential) providers))] in
  let text = Yojson.Basic.to_string json ^ "\n" in
  if String.length text > max_bytes then fail "OAuth credential data exceeds size limit";
  let dir = Filename.dirname path in
  let temp, fd =
    let rec create attempts =
      if attempts = 0 then fail "Cannot create OAuth credential temp file";
      let suffix = Printf.sprintf "%08x%08x" (Random.bits ()) (Random.bits ()) in
      let temp = Filename.concat dir (".oauth-" ^ suffix ^ ".tmp") in
      try temp, create_checked temp with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> create (attempts - 1) in
    create 10 in
  Fun.protect ~finally:(fun () ->
    (try Unix.close fd with Unix.Unix_error _ -> ());
    (try Unix.unlink temp with Unix.Unix_error (Unix.ENOENT, _, _) -> ()))
    (fun () ->
      write_all fd text;
      Unix.fsync fd;
      ignore (regular_file path);
      Unix.rename temp path;
      let dir_fd = Unix.openfile dir [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
      Fun.protect ~finally:(fun () -> Unix.close dir_fd)
        (fun () -> Unix.fsync dir_fd))

(* The lock file is never renamed; replacing the data inode cannot invalidate
   a lock. Nested calls on this domain reuse it, keeping refresh read/write
   transactions serialized. Mutex additionally serializes domains, since lockf
   locks belong to the process rather than the individual descriptor. *)
let domain_paths = Domain.DLS.new_key (fun () -> ref [])
let process_mutex = Mutex.create ()

let with_lock ~path f = guard (fun () ->
  if Filename.is_relative path || Filename.basename path = "."
     || Filename.basename path = ".." then
    fail "OAuth credential path must name a file under an absolute directory";
  let active = Domain.DLS.get domain_paths in
  if !active <> [] && not (List.mem path !active) then
    fail "Nested OAuth credential locks must use the same path";
  if List.mem path !active then f () else (
    Mutex.lock process_mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock process_mutex) (fun () ->
      ensure_directory (Filename.dirname path);
      let lock_path = path ^ ".lock" in
      let fd = match regular_file lock_path with
        | Some _ -> open_checked lock_path [Unix.O_RDWR]
        | None ->
            (try create_checked lock_path with
             | Unix.Unix_error (Unix.EEXIST, _, _) ->
                 open_checked lock_path [Unix.O_RDWR]) in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
        Unix.fchmod fd 0o600;
        Unix.lockf fd Unix.F_LOCK 0;
        Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0)
          (fun () ->
            let current = regular_file lock_path in
            let locked = Unix.fstat fd in
            if (match current with
                | None -> true
                | Some stat -> stat.Unix.st_dev <> locked.Unix.st_dev
                            || stat.Unix.st_ino <> locked.Unix.st_ino) then
              fail "OAuth credential lock changed during access";
            active := path :: !active;
            Fun.protect ~finally:(fun () -> active := List.tl !active) f)))))

let check_provider provider =
  if provider = "" || String.length provider > 256 then
    fail "Invalid OAuth provider name"

let get ~path ~provider =
  check_provider provider;
  with_lock ~path (fun () -> List.assoc_opt provider (read_store path))

let put ~path ~provider credential =
  check_provider provider;
  with_lock ~path (fun () ->
    let providers = read_store path in
    write_store path ((provider, credential) :: List.remove_assoc provider providers))

let remove ~path ~provider =
  check_provider provider;
  with_lock ~path (fun () ->
    let providers = read_store path in
    if List.mem_assoc provider providers then
      write_store path (List.remove_assoc provider providers))
