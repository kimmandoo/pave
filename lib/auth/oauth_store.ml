type credential = {
  access : string;
  refresh : string option;
  expires_at : float option;
  account_id : string option;
  metadata : (string * string) list;
}
type grant_type = Authorization_code | Device_approval | Provider_session

type binding = {
  provider : string;
  grant_type : grant_type;
  routes : (string * string) list;
}

type account = {
  credential : credential;
  binding : binding option;
  selection_id : string;
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
let check_account_id = function
  | None -> ()
  | Some id when id <> "" && String.length id <= 256 &&
      not (String.exists (fun c -> Char.code c < 32 || Char.code c = 127) id) -> ()
  | Some _ -> fail "Invalid OAuth account identifier"
let local_selection_prefix = "pave-local:"

let is_local_selection_id value =
  let prefix_length = String.length local_selection_prefix in
  String.length value = prefix_length + 32 &&
  String.sub value 0 prefix_length = local_selection_prefix &&
  String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false)
    (String.sub value prefix_length 32)

let rec fresh_local_selection_id existing =
  let bytes = Bytes.create 16 in
  let fd = Unix.openfile "/dev/urandom" [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let rec read offset =
      if offset < Bytes.length bytes then
        let count =
          try Unix.read fd bytes offset (Bytes.length bytes - offset)
          with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
        if count = 0 then fail "System random source returned no data";
        if count < 0 then read offset else read (offset + count) in
    read 0);
  let hex = "0123456789abcdef" in
  let encoded = Bytes.create 32 in
  Bytes.iteri (fun index byte ->
    let value = Char.code byte in
    Bytes.set encoded (index * 2) hex.[value lsr 4];
    Bytes.set encoded ((index * 2) + 1) hex.[value land 15]) bytes;
  let selection_id = local_selection_prefix ^ Bytes.to_string encoded in
  if List.exists (fun entry -> entry.selection_id = selection_id) existing then
    fresh_local_selection_id existing
  else selection_id

let validate_selection_id selection_id =
  check_account_id (Some selection_id);
  if not (is_local_selection_id selection_id) then
    fail "Invalid local OAuth account selector"

let account_selection_id credential selection_id existing =
  match credential.account_id, selection_id with
  | Some account_id, None -> account_id
  | Some account_id, Some selection_id when account_id = selection_id ->
      account_id
  | Some _, Some _ -> fail "OAuth account selector does not match provider identity"
  | None, Some selection_id ->
      validate_selection_id selection_id;
      selection_id
  | None, None -> fresh_local_selection_id existing

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
  let account_id = optional_string (field fields "account_id") in
  check_account_id account_id;
  { access = string (field fields "access");
    refresh = optional_string (field fields "refresh");
    expires_at; account_id; metadata }

let json_of_credential credential =
  check_account_id credential.account_id;
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

let check_provider provider =
  if provider = "" || String.length provider > 256 then
    fail "Invalid OAuth provider name"

let grant_type_name = function
  | Authorization_code -> "authorization_code"
  | Device_approval -> "device_approval"
  | Provider_session -> "provider_session"

let grant_type_of_name = function
  | "authorization_code" -> Authorization_code
  | "device_approval" -> Device_approval
  | "provider_session" -> Provider_session
  | _ -> fail "Invalid OAuth credential binding"

let json_of_binding = function
  | None -> `Null
  | Some binding ->
      check_provider binding.provider;
      let routes = List.sort compare binding.routes in
      let seen = Hashtbl.create (List.length routes) in
      List.iter (fun (name, endpoint) ->
        if name = "" || endpoint = "" || Hashtbl.mem seen name then
          fail "Invalid OAuth credential binding";
        Hashtbl.add seen name ()) routes;
      `Assoc ["provider", `String binding.provider;
        "grant_type", `String (grant_type_name binding.grant_type);
        "routes", `Assoc (List.map (fun (name, endpoint) ->
          name, `String endpoint) routes)]

let binding_of_json = function
  | `Null -> None
  | json ->
      let fields = object_fields json in
      let routes = object_fields (field fields "routes")
        |> List.map (fun (name, endpoint) -> name, string endpoint) in
      let binding = { provider = string (field fields "provider");
        grant_type = grant_type_of_name (string (field fields "grant_type"));
        routes } in
      ignore (json_of_binding (Some binding));
      Some binding

let selection_id_compare left right =
  String.compare left.selection_id right.selection_id

let validate_account_selection entry =
  check_account_id (Some entry.selection_id);
  match entry.credential.account_id with
  | Some account_id when account_id = entry.selection_id -> ()
  | Some _ -> fail "OAuth account selector does not match provider identity"
  | None -> validate_selection_id entry.selection_id

let validate_store store =
  let seen_providers = Hashtbl.create (List.length store) in
  List.iter (fun (provider, entries) ->
    check_provider provider;
    if Hashtbl.mem seen_providers provider then fail "Duplicate OAuth account identity";
    Hashtbl.add seen_providers provider ();
    let seen_accounts = Hashtbl.create (List.length entries) in
    List.iter (fun entry ->
      validate_account_selection entry;
      if Hashtbl.mem seen_accounts entry.selection_id then
        fail "Duplicate OAuth account identity";
      Hashtbl.add seen_accounts entry.selection_id ();
      ignore (json_of_credential entry.credential);

      (match entry.binding with
       | Some binding when binding.provider <> provider ->
           fail "Invalid OAuth credential binding"
       | _ -> ());
      ignore (json_of_binding entry.binding)) entries) store

let write_all fd data =
  let rec loop offset =
    if offset < String.length data then (
      let count = try Unix.write_substring fd data offset (String.length data - offset)
        with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
      if count = 0 then fail "OAuth credential write failed";
      if count < 0 then loop offset else loop (offset + count)) in
  loop 0

let write_store path store =
  validate_store store;
  let store = List.sort (fun (a, _) (b, _) -> String.compare a b) store
    |> List.map (fun (provider, entries) ->
      provider, List.sort selection_id_compare entries) in
  let accounts = List.concat_map (fun (provider, entries) ->
    List.map (fun entry -> `Assoc ["provider", `String provider;
      "selection_id", `String entry.selection_id;
      "credential", json_of_credential entry.credential;
      "binding", json_of_binding entry.binding]) entries) store in
  let text = Yojson.Basic.to_string (`Assoc ["version", `Int 3;
    "accounts", `List accounts]) ^ "\n" in
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

let read_store path =
  match regular_file path with
  | None -> []
  | Some _ ->
      let fd = open_checked path [Unix.O_RDONLY] in
      let text = Fun.protect ~finally:(fun () -> Unix.close fd)
        (fun () -> read_all fd) in
      let root = object_fields (Yojson.Basic.from_string text) in
      match field root "version" with
      | `Int 1 ->
          let store = object_fields (field root "providers")
            |> List.map (fun (name, value) ->
              let credential = credential_of_json value in
              let selection_id = account_selection_id credential None [] in
              name, [{ credential; binding = None; selection_id }]) in
          validate_store store;
          write_store path store;
          store
      | `Int version when version = 2 || version = 3 ->
          let rows = match field root "accounts" with
            | `List rows -> rows
            | _ -> fail "Invalid OAuth credential data" in
          let rows = List.map (fun row ->
            let fields = object_fields row in
            let provider = string (field fields "provider") in
            let credential = credential_of_json (field fields "credential") in
            let binding = binding_of_json (field fields "binding") in
            let selection_id = if version = 2 then credential.account_id
              else Some (string (field fields "selection_id")) in
            provider, credential, binding, selection_id) rows in
          let grouped = List.fold_left (fun store
              (provider, credential, binding, requested_selection_id) ->
            let entries = Option.value ~default:[]
              (List.assoc_opt provider store) in
            let selection_id = account_selection_id credential
              requested_selection_id entries in
            let entry = { credential; binding; selection_id } in
            (provider, entries @ [entry]) :: List.remove_assoc provider store)
            [] rows in
          validate_store grouped;
          if version = 2 then write_store path grouped;
          grouped
      | _ -> fail "Unsupported OAuth credential version"

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


let accounts ~path ~provider =
  check_provider provider;
  with_lock ~path (fun () ->
    Option.value ~default:[] (List.assoc_opt provider (read_store path))
    |> List.sort selection_id_compare)

let account ~path ~provider ~account_id =
  let entries = accounts ~path ~provider in
  match account_id, entries with
  | None, [entry] when entry.credential.account_id = None -> Some entry
  | None, _ -> None
  | Some selection_id, _ ->
      List.find_opt (fun entry -> entry.selection_id = selection_id) entries

let put_account_with_selection ~path ~provider ~binding ?selection_id credential =
  if binding.provider <> provider then fail "Invalid OAuth credential binding";
  check_provider provider;
  with_lock ~path (fun () ->
    let store = read_store path in
    let entries = Option.value ~default:[] (List.assoc_opt provider store) in
    let selection_id =
      account_selection_id credential selection_id entries in
    let other_entries = List.filter (fun entry ->
      entry.selection_id <> selection_id) entries in
    let replacement = { credential; binding = Some binding; selection_id } in
    let entries = replacement :: other_entries in
    let store = (provider, entries) :: List.remove_assoc provider store in
    write_store path store;
    selection_id)

let put_account ~path ~provider ~binding ?selection_id credential =
  ignore (put_account_with_selection ~path ~provider ~binding
    ?selection_id credential)

let remove_account ~path ~provider ~account_id =
  check_provider provider;
  with_lock ~path (fun () ->
    let store = read_store path in
    match List.assoc_opt provider store with
    | None -> ()
    | Some entries ->
        let selection_id = match account_id with
          | Some selection_id -> Some selection_id
          | None ->
              (match List.filter (fun entry ->
                 entry.credential.account_id = None) entries with
               | [entry] -> Some entry.selection_id
               | [] -> None
               | _ -> fail "OAuth account selector is required for multiple accounts") in
        let remaining = match selection_id with
          | None -> entries
          | Some selection_id -> List.filter (fun entry ->
              entry.selection_id <> selection_id) entries in
        if List.length remaining <> List.length entries then
          write_store path ((provider, remaining) :: List.remove_assoc provider store))

let remove_provider ~path ~provider =
  check_provider provider;
  with_lock ~path (fun () ->
    let store = read_store path in
    if List.mem_assoc provider store then
      write_store path (List.remove_assoc provider store))
