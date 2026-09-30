type item = {
  id : string;
  owner : string;
  name : string;
  mime_type : string;
  size : int;
  sha256 : string;
  created_at : float;
}

type t = { dir : string }
type writer = {
  store : t;
  owner : string;
  name : string;
  mime_type : string;
  id : string;
  temp_path : string;
  fd : Unix.file_descr;
  mutable size : int;
  mutable digest : Digestif.SHA256.ctx;
  mutable closed : bool;
}

exception Error of string

(* Public conservative storage limits: 16 MiB per item, 256 items, 128 MiB total. *)
let max_artifact_bytes = 16 * 1024 * 1024
let max_artifacts = 256
let max_total_bytes = 128 * 1024 * 1024

let fail message = raise (Error message)
let valid_id id = String.length id = 32 &&
  String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false) id

let validate_text label value limit =
  if value = "" || String.length value > limit ||
     String.exists (fun c -> Char.code c < 32 || Char.code c = 127) value then
    fail ("invalid artifact " ^ label)

let random_id () =
  let bytes = Bytes.create 16 in
  let fd = Unix.openfile "/dev/urandom" [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let rec fill offset =
      if offset < Bytes.length bytes then
        let count = try Unix.read fd bytes offset (Bytes.length bytes - offset)
          with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
        if count = 0 then fail "system random source returned no data";
        if count < 0 then fill offset else fill (offset + count) in
    fill 0);
  let hex = "0123456789abcdef" in
  let encoded = Bytes.create 32 in
  Bytes.iteri (fun index byte ->
    let value = Char.code byte in
    Bytes.set encoded (index * 2) hex.[value lsr 4];
    Bytes.set encoded (index * 2 + 1) hex.[value land 15]) bytes;
  Bytes.to_string encoded

let state_home () =
  match Sys.getenv_opt "XDG_STATE_HOME" with
  | Some path when path <> "" && not (Filename.is_relative path) -> path
  | _ -> (match Sys.getenv_opt "HOME" with
    | Some home when home <> "" && not (Filename.is_relative home) ->
        Filename.concat (Filename.concat home ".local") "state"
    | _ -> fail "HOME or an absolute XDG_STATE_HOME is required for artifacts")

let ensure_directory path =
  let rec make path =
    try ignore (Unix.lstat path) with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        let parent = Filename.dirname path in
        if parent = path then fail "artifact directory is unavailable";
        make parent;
        (try Unix.mkdir path 0o700 with
         | Unix.Unix_error (Unix.EEXIST, _, _) -> ()) in
  make (Filename.dirname path);
  (try Unix.mkdir path 0o700 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let stat = Unix.lstat path in
  if stat.Unix.st_kind <> Unix.S_DIR || stat.Unix.st_uid <> Unix.geteuid () ||
     stat.Unix.st_perm land 0o077 <> 0 then
    fail "artifact directory must be private, owned, and not a symlink"

let open_for_workspace ~root =
  let root = try Unix.realpath root with Unix.Unix_error _ ->
    fail "artifact workspace does not exist" in
  let digest = Digestif.SHA256.(to_hex (digest_string root)) in
  let pave = Filename.concat (state_home ()) "pave" in
  let sessions = Filename.concat pave "sessions" in
  let workspace = Filename.concat sessions digest in
  let dir = Filename.concat workspace "artifacts" in
  ensure_directory pave;
  ensure_directory sessions;
  ensure_directory workspace;
  ensure_directory dir;
  { dir }

let private_file stat =
  stat.Unix.st_kind = Unix.S_REG && stat.Unix.st_uid = Unix.geteuid () &&
  stat.Unix.st_nlink = 1 && stat.Unix.st_perm land 0o077 = 0
let same_inode a b = a.Unix.st_dev = b.Unix.st_dev && a.Unix.st_ino = b.Unix.st_ino
let data_path store id = Filename.concat store.dir (id ^ ".data")
let meta_path store id = Filename.concat store.dir (id ^ ".json")
(* POSIX record locks are process-owned, so threads must serialize before
   opening descriptors that could also release another thread's file lock. *)
let process_mutex = Mutex.create ()


let with_lock store action =
  Mutex.lock process_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock process_mutex) (fun () ->
  let path = Filename.concat store.dir ".lock" in
  let rec open_lock () =
    try
      let fd = Unix.openfile path
        [Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o600 in
      Unix.fchmod fd 0o600;
      fd
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) ->
        let before = Unix.lstat path in
        if not (private_file before) then fail "artifact lock must be private and regular";
        let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CLOEXEC] 0 in
        let opened = Unix.fstat fd and current = Unix.lstat path in
        if not (private_file opened && same_inode before opened &&
          same_inode opened current) then (
          Unix.close fd;
          fail "artifact lock changed while opening");
        fd
    | Unix.Unix_error (Unix.ENOENT, _, _) -> open_lock () in
  let fd = open_lock () in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    Unix.lockf fd Unix.F_LOCK 0;
    Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) action))

let write_all fd text =
  let rec loop offset =
    if offset < String.length text then
      let count = try Unix.write_substring fd text offset (String.length text - offset)
        with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
      if count = 0 then fail "artifact write failed";
      if count < 0 then loop offset else loop (offset + count) in
  loop 0

let read_private path limit =
  try
    let before = Unix.lstat path in
    if not (private_file before) || before.Unix.st_size < 0 || before.Unix.st_size > limit
    then None else
    let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      let opened = Unix.fstat fd and current = Unix.lstat path in
      if not (private_file opened && same_inode before opened && same_inode opened current &&
        before.Unix.st_size = opened.Unix.st_size) then None else
      let bytes = Bytes.create opened.Unix.st_size in
      let rec loop offset =
        if offset = Bytes.length bytes then Some (Bytes.unsafe_to_string bytes)
        else let count = Unix.read fd bytes offset (Bytes.length bytes - offset) in
          if count = 0 then None else loop (offset + count) in
      loop 0)
  with Unix.Unix_error _ | Sys_error _ -> None

let item_json (item : item) = `Assoc [
  "id", `String item.id; "owner", `String item.owner;
  "name", `String item.name; "mime_type", `String item.mime_type;
  "size", `Int item.size; "sha256", `String item.sha256;
  "created_at", `Float item.created_at]

let parse_item json =
  let field name = try List.assoc name (Yojson.Basic.Util.to_assoc json)
    with _ -> fail "invalid artifact metadata" in
  let string name = match field name with `String value -> value
    | _ -> fail "invalid artifact metadata" in
  let number name = match field name with `Int value -> value
    | _ -> fail "invalid artifact metadata" in
  let id = string "id" and owner = string "owner" and name = string "name"
  and mime_type = string "mime_type" and size = number "size"
  and sha256 = string "sha256" in
  let created_at = match field "created_at" with `Float x -> x | `Int x -> float x
    | _ -> fail "invalid artifact metadata" in
  if not (valid_id id && valid_id owner && size >= 0 && size <= max_artifact_bytes &&
      String.length sha256 = 64 && String.for_all
        (function '0'..'9' | 'a'..'f' -> true | _ -> false) sha256 &&
      classify_float created_at <> FP_nan && classify_float created_at <> FP_infinite) then
    fail "invalid artifact metadata";
  validate_text "name" name 1024;
  validate_text "MIME type" mime_type 256;
  { id; owner; name; mime_type; size; sha256; created_at }

let metadata store id =
  match read_private (meta_path store id) 16_384 with
  | None -> None
  | Some text -> (try Some (parse_item (Yojson.Basic.from_string text))
    with Yojson.Json_error _ | Error _ -> None)

let list_unlocked store =
  Sys.readdir store.dir |> Array.to_list |> List.filter_map (fun filename ->
    if Filename.check_suffix filename ".json" then
      let id = String.sub filename 0 (String.length filename - 5) in
      if valid_id id then match metadata store id with
        | Some item when item.id = id -> Some item | _ -> None
      else None else None) |> List.sort (fun a b -> compare a.created_at b.created_at)

let list store ?owner () =
  (match owner with Some owner when not (valid_id owner) ->
    fail "invalid artifact owner" | _ -> ());
  with_lock store (fun () -> list_unlocked store |> List.filter (fun (item : item) ->
    match owner with None -> true | Some owner -> item.owner = owner))
type retained_size = {
  mutable declared : int;
  mutable data : int;
  mutable staging : int;
}

let storage_usage_unlocked store =
  let files = Sys.readdir store.dir in
  if Array.length files > max_artifacts * 4 + 1 then
    fail "artifact directory contains too many retained files";
  let sizes = Hashtbl.create (min max_artifacts (Array.length files)) in
  let entry key =
    match Hashtbl.find_opt sizes key with
    | Some size -> size
    | None ->
        let size = { declared = 0; data = 0; staging = 0 } in
        Hashtbl.add sizes key size;
        size in
  let add left right =
    if right < 0 || right > max_total_bytes - left then
      fail "artifact storage quota exceeded";
    left + right in
  let id_with_suffix name suffix =
    if String.length name = 32 + String.length suffix &&
       Filename.check_suffix name suffix then
      let id = String.sub name 0 32 in
      if valid_id id then Some id else None
    else None in
  Array.iter (fun filename ->
    if filename <> ".lock" then (
      let json_id = id_with_suffix filename ".json" in
      let stored = match json_id with
        | Some id ->
            (match metadata store id with
             | Some item when item.id = id -> Some item
             | _ -> None)
        | None -> None in
      match stored with
      | Some item -> (entry item.id).declared <- item.size
      | None ->
          let path = Filename.concat store.dir filename in
          let stat = try Some (Unix.lstat path) with
            | Unix.Unix_error (Unix.ENOENT, _, _) -> None in
          (match stat with
           | None -> () (* An active writer may abort its own staging file. *)
           | Some stat ->
               if not (private_file stat) || stat.Unix.st_size < 0 then
                 fail "retained artifact files must be private, owned, and regular";
               match json_id, id_with_suffix filename ".data" with
               | None, Some id -> (entry id).data <- stat.Unix.st_size
               | _ ->
                   let staged_id = match json_id with
                     | Some id -> Some id
                     | None ->
                         if String.starts_with ~prefix:"." filename then
                           let name = String.sub filename 1 (String.length filename - 1) in
                           match id_with_suffix name ".tmp" with
                           | Some id -> Some id
                           | None -> id_with_suffix name ".meta.tmp"
                         else None in
                   let size = entry (Option.value ~default:filename staged_id) in
                   size.staging <- add size.staging stat.Unix.st_size))) files;
  let total = Hashtbl.fold (fun _ size total ->
    add (add total (max size.declared size.data)) size.staging) sizes 0 in
  Hashtbl.length sizes, total


let begin_write store ~owner ~name ~mime_type =
  if not (valid_id owner) then fail "invalid artifact owner";
  validate_text "name" name 1024;
  validate_text "MIME type" mime_type 256;
  let rec create () =
    let id = random_id () in
    let temp_path = Filename.concat store.dir ("." ^ id ^ ".tmp") in
    try
      let fd = Unix.openfile temp_path
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o600 in
      Unix.fchmod fd 0o600;
      { store; owner; name; mime_type; id; temp_path; fd; size = 0;
        digest = Digestif.SHA256.init (); closed = false }
    with Unix.Unix_error (Unix.EEXIST, _, _) -> create () in
  create ()

let abort writer =
  if not writer.closed then (
    writer.closed <- true;
    (try Unix.close writer.fd with Unix.Unix_error _ -> ());
    (try Unix.unlink writer.temp_path with Unix.Unix_error _ -> ()))

let write writer chunk =
  if writer.closed then fail "artifact writer is closed";
  let length = String.length chunk in
  if length > max_artifact_bytes - writer.size then fail "artifact exceeds per-file limit";
  try
    write_all writer.fd chunk;
    writer.digest <- Digestif.SHA256.feed_string writer.digest chunk;
    writer.size <- writer.size + length
  with exn -> abort writer; raise exn

let finish writer =
  if writer.closed then fail "artifact writer is closed";
  try
    Unix.fsync writer.fd;
    Unix.close writer.fd;
    writer.closed <- true;
    with_lock writer.store (fun () ->
      (* Count retained data even without valid metadata, including active or
         crash-left staging files. Never delete or adopt another writer's files. *)
      (try
         let count, _ = storage_usage_unlocked writer.store in
         if count > max_artifacts then fail "artifact count limit reached"
       with exn ->
         (* Reclaim only this failed candidate while still holding the lock, so
            a competing boundary writer can consume its released allowance. *)
         (try Unix.unlink writer.temp_path with Unix.Unix_error _ -> ());
         raise exn);
      let digest = Digestif.SHA256.(to_hex (get writer.digest)) in
      let item = { id = writer.id; owner = writer.owner; name = writer.name;
        mime_type = writer.mime_type; size = writer.size; sha256 = digest;
        created_at = Unix.gettimeofday () } in
      let destination = data_path writer.store writer.id in
      Unix.rename writer.temp_path destination;
      let meta_temp = Filename.concat writer.store.dir ("." ^ writer.id ^ ".meta.tmp") in
      let fd = Unix.openfile meta_temp
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o600 in
      Fun.protect ~finally:(fun () ->
        (try Unix.close fd with Unix.Unix_error _ -> ());
        (try Unix.unlink meta_temp with Unix.Unix_error _ -> ())) (fun () ->
        Unix.fchmod fd 0o600;
        write_all fd (Yojson.Basic.to_string (item_json item));
        Unix.fsync fd;
        Unix.rename meta_temp (meta_path writer.store writer.id));
      let dir_fd = Unix.openfile writer.store.dir [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
      Fun.protect ~finally:(fun () -> Unix.close dir_fd)
        (fun () -> Unix.fsync dir_fd);
      item)
  with exn ->
    (try Unix.unlink writer.temp_path with Unix.Unix_error _ -> ());
    (match metadata writer.store writer.id with
     | None -> (try Unix.unlink (data_path writer.store writer.id)
       with Unix.Unix_error _ -> ())
     | Some _ -> ());
    abort writer;
    raise exn

let put store ~owner ~name ~mime_type bytes =
  let writer = begin_write store ~owner ~name ~mime_type in
  try write writer bytes; finish writer with exn -> abort writer; raise exn

let read store ~owner ~id =
  if not (valid_id id) then fail "invalid artifact ID";
  if not (valid_id owner) then fail "invalid artifact owner";
  with_lock store (fun () ->
    let item = match metadata store id with Some item when item.id = id -> item
      | _ -> fail "artifact not found or invalid" in
    if item.owner <> owner then fail "artifact owner mismatch";
    match read_private (data_path store id) max_artifact_bytes with
    | None -> fail "artifact data is missing or unsafe"
    | Some bytes ->
        if String.length bytes <> item.size then fail "artifact size mismatch";
        let digest = Digestif.SHA256.(to_hex (digest_string bytes)) in
        if digest <> item.sha256 then fail "artifact digest mismatch";
        bytes)
