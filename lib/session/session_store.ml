type recent = {
  id : string;
  path : string;
  title : string;
  started : string;
  modified : float;
  pinned : bool;
}

let max_files = 1_024
let max_recent = 100
let preview_bytes = 4_096
let valid_utf8 text =
  let valid = ref true in
  ignore (Uutf.String.fold_utf_8 (fun () _ -> function
    | `Uchar _ -> ()
    | `Malformed _ -> valid := false) () text);
  !valid


let state_home () =
  match Sys.getenv_opt "XDG_STATE_HOME" with
  | Some path when path <> "" && not (Filename.is_relative path) -> path
  | _ -> match Sys.getenv_opt "HOME" with
    | Some home when home <> "" && not (Filename.is_relative home) ->
        Filename.concat (Filename.concat home ".local") "state"
    | _ -> invalid_arg "HOME or an absolute XDG_STATE_HOME is required for sessions"

let safe_directory path =
  let stat = Unix.lstat path in
  if stat.Unix.st_kind <> Unix.S_DIR || stat.Unix.st_uid <> Unix.geteuid () ||
     stat.Unix.st_perm land 0o077 <> 0 then
    invalid_arg "session directory must be private, owned, and not a symlink"

let ensure_directory path =
  let rec make path =
    try ignore (Unix.stat path) with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        let parent = Filename.dirname path in
        if parent = path then invalid_arg "session directory is unavailable";
        make parent;
        (try Unix.mkdir path 0o700 with
         | Unix.Unix_error (Unix.EEXIST, _, _) -> ()) in
  make (Filename.dirname path);
  (try Unix.mkdir path 0o700 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  safe_directory path

let base_dir () = Filename.concat (Filename.concat (state_home ()) "pave") "sessions"

let directory ~root =
  let root = Unix.realpath root in
  let digest = Digestif.SHA256.(to_hex (digest_string root)) in
  Filename.concat (base_dir ()) digest

let ensure ~root =
  let base = base_dir () in
  let parent = Filename.dirname base in
  ensure_directory parent;
  ensure_directory base;
  let path = directory ~root in
  ensure_directory path;
  path

let valid_name name =
  let suffix = ".jsonl" in
  String.length name = 32 + String.length suffix &&
  String.sub name 32 (String.length suffix) = suffix &&
  let rec hex index =
    index = 32 ||
    (let c = name.[index] in
     (('0' <= c && c <= '9') || ('a' <= c && c <= 'f')) && hex (index + 1)) in
  hex 0

let private_file stat =
  stat.Unix.st_kind = Unix.S_REG && stat.Unix.st_uid = Unix.geteuid () &&
  stat.Unix.st_nlink = 1 && stat.Unix.st_perm land 0o077 = 0

let same_inode a b = a.Unix.st_dev = b.Unix.st_dev && a.Unix.st_ino = b.Unix.st_ino
let valid_id id =
  String.length id = 32 &&
  String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false) id

let read_private_file path limit =
  try
    let before = Unix.lstat path in
    if not (private_file before) || before.Unix.st_size < 0 ||
       before.Unix.st_size > limit then None
    else
      let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC] 0 in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
        let opened = Unix.fstat fd and current = Unix.lstat path in
        if not (private_file opened && same_inode before opened &&
                same_inode opened current &&
                opened.Unix.st_size = before.Unix.st_size) then None
        else
          let bytes = Bytes.create opened.Unix.st_size in
          let rec read offset =
            if offset = Bytes.length bytes then Some (Bytes.unsafe_to_string bytes)
            else
              let count = Unix.read fd bytes offset (Bytes.length bytes - offset) in
              if count = 0 then None else read (offset + count) in
          read 0)
  with Unix.Unix_error _ | Sys_error _ | Invalid_argument _ -> None

let write_all fd text =
  let rec write offset =
    if offset < String.length text then
      let count = try Unix.write_substring fd text offset
        (String.length text - offset) with
        | Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
      if count = 0 then failwith "session metadata write failed";
      if count < 0 then write offset else write (offset + count) in
  write 0

let write_atomic ~dir ~prefix path text =
  let temp = Filename.concat dir (prefix ^ Session.fresh_id () ^ ".tmp") in
  let fd = Unix.openfile temp
    [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o600 in
  Fun.protect ~finally:(fun () ->
    (try Unix.close fd with Unix.Unix_error _ -> ());
    (try Unix.unlink temp with Unix.Unix_error (Unix.ENOENT, _, _) -> ()))
    (fun () ->
      Unix.fchmod fd 0o600;
      write_all fd text;
      Unix.fsync fd;
      Unix.rename temp path;
      let dir_fd = Unix.openfile dir [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
      Fun.protect ~finally:(fun () -> Unix.close dir_fd)
        (fun () -> Unix.fsync dir_fd))

let with_file_lock path action =
  let rec open_lock () =
    try Unix.openfile path
      [Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o600
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) ->
        let before = Unix.lstat path in
        if not (private_file before) then
          invalid_arg "session pin lock must be private and regular";
        let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CLOEXEC] 0 in
        let opened = Unix.fstat fd and current = Unix.lstat path in
        if not (private_file opened && same_inode before opened &&
                same_inode opened current) then (
          Unix.close fd;
          invalid_arg "session pin lock changed during opening");
        fd
    | Unix.Unix_error (Unix.ENOENT, _, _) -> open_lock () in
  let fd = open_lock () in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    Unix.fchmod fd 0o600;
    Unix.lockf fd Unix.F_LOCK 0;
    Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0)
      action)

let pin_file dir = Filename.concat dir "pins.json"

let read_pin_ids dir =
  match read_private_file (pin_file dir) (max_files * 64) with
  | None -> []
  | Some text ->
      (try match Yojson.Basic.from_string text with
       | `List ids ->
           List.filter_map (function
             | `String id when valid_id id -> Some id
             | _ -> None) ids
           |> List.sort_uniq String.compare
       | _ -> []
       with Yojson.Json_error _ -> [])

let write_pin_ids dir ids =
  let ids = List.sort_uniq String.compare ids in
  write_atomic ~dir ~prefix:".pave-pins-" (pin_file dir)
    (Yojson.Basic.to_string (`List (List.map (fun id -> `String id) ids)) ^ "\n")

let title_file dir id = Filename.concat dir (id ^ ".title")

let read_title_cache dir id =
  if not (valid_id id) then None
  else match read_private_file (title_file dir id) 4_096 with
  | None -> None
  | Some text ->
      (try
         let json = Yojson.Basic.from_string text in
         match Protocol.member "id" json, Protocol.member "title" json with
         | `String cached_id, `String title
           when cached_id = id && valid_utf8 title -> Some title
         | _ -> None
       with Yojson.Json_error _ -> None)

let write_title_cache dir id title =
  let json = `Assoc ["id", `String id; "title", `String title] in
  write_atomic ~dir ~prefix:".pave-title-" (title_file dir id)
    (Yojson.Basic.to_string json ^ "\n")


let read_preview path stat =
  if not (private_file stat) then invalid_arg "session journal must be private and regular";
  let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let opened = Unix.fstat fd and current = Unix.lstat path in
    if not (private_file opened && same_inode stat opened && same_inode opened current) then
      invalid_arg "session journal changed during opening";
    let buffer = Bytes.create preview_bytes in
    let count = Unix.read fd buffer 0 preview_bytes in
    let content = Bytes.sub_string buffer 0 count in
    match String.index_opt content '\n' with
    | None -> invalid_arg "session journal header exceeds 4 KiB or is truncated"
    | Some ending ->
        let header = Yojson.Basic.from_string (String.sub content 0 ending) in
        let field name = Protocol.member name header in
        let root, started, id = match field "type", field "version", field "cwd",
          field "timestamp", field "id" with
          | `String "session", `Int 1, `String root, `String started,
            `String id when valid_id id -> root, started, id
          | _ -> invalid_arg "session journal has an invalid header" in
        let rec title start =
          match String.index_from_opt content start '\n' with
          | None -> "(untitled)"
          | Some finish ->
              let line = String.sub content start (finish - start) in
              (try
                 let json = Yojson.Basic.from_string line in
                 let message = Protocol.member "message" json in
                 let value = Protocol.member "content" message in
                 match Protocol.member "type" json,
                   Protocol.member "role" message, value with
                 | `String "message", `String "user", `String text ->
                     let summary = String.split_on_char '\n' text |> List.hd
                       |> String.trim in
                     if summary = "" then "(untitled)" else summary
                 | _ -> title (finish + 1)
               with Yojson.Json_error _ -> "(untitled)") in
        root, started, id, title (ending + 1))

let preview_in ~root ~dir ~is_pinned path =
  let stat = Unix.lstat path in
  let journal_root, started, id, raw_title = read_preview path stat in
  if journal_root <> root then
    invalid_arg "session belongs to a different workspace";
  let raw_title = Option.value ~default:raw_title (read_title_cache dir id) in
  let title =
    let text = if valid_utf8 raw_title then raw_title else "(untitled)" in
    match Session_tree.first_line text with
    | "" -> "(untitled)"
    | safe -> safe in
  { id; path; started = Session_tree.first_line started; title;
    modified = stat.Unix.st_mtime; pinned = is_pinned id }

let preview ~root path =
  let root = Unix.realpath root in
  let dir = directory ~root in
  let pinned_ids = read_pin_ids dir in
  preview_in ~root ~dir ~is_pinned:(fun id -> List.mem id pinned_ids) path

let recent ~root =
  let root = Unix.realpath root in
  let path = directory ~root in
  let entries =
    try
      safe_directory path;
      Sys.readdir path
    with Unix.Unix_error (Unix.ENOENT, _, _) -> [||] in
  let entries = Array.to_list entries |> List.filter valid_name in
  if List.length entries > max_files then
    invalid_arg "session directory exceeds 1024 journals; inspect it manually";
  let pinned_ids = Hashtbl.create 16 in
  List.iter (fun id -> Hashtbl.replace pinned_ids id ())
    (read_pin_ids path);
  let records = List.filter_map (fun name ->
    let file = Filename.concat path name in
    try Some (preview_in ~root ~dir:path
      ~is_pinned:(fun id -> Hashtbl.mem pinned_ids id) file) with
    | Unix.Unix_error _ | Sys_error _ | Invalid_argument _ | Yojson.Json_error _ -> None)
    entries in
  let ordered = List.sort (fun left right ->
    let by_pin = Bool.compare right.pinned left.pinned in
    if by_pin <> 0 then by_pin else
    let by_time = Float.compare right.modified left.modified in
    if by_time <> 0 then by_time else String.compare left.path right.path) records in
  List.filteri (fun index _ -> index < max_recent) ordered


let contains ~needle text =
  let needle = String.lowercase_ascii needle
  and text = String.lowercase_ascii text in
  let needle_length = String.length needle in
  let rec find offset =
    offset + needle_length <= String.length text &&
    (String.sub text offset needle_length = needle || find (offset + 1)) in
  needle = "" || find 0

let search ~root query =
  let query = String.trim query |> String.lowercase_ascii in
  let matches (item : recent) =
    let basename = Filename.basename item.path in
    let stem = if String.ends_with ~suffix:".jsonl" basename then
      String.sub basename 0 (String.length basename - 6) else basename in
    String.starts_with ~prefix:query (String.lowercase_ascii item.id) ||
    String.starts_with ~prefix:query (String.lowercase_ascii stem) ||
    contains ~needle:query item.title in
  List.filter matches (recent ~root)

let title_cache_location ~root session =
  try
    let item = preview ~root session.Session.path in
    let dir = directory ~root in
    let real_dir = Unix.realpath dir
    and real_path = Unix.realpath session.Session.path in
    let id = match Protocol.member "id" session.Session.header with
      | `String id when id = item.id -> id
      | _ -> raise Not_found in
    if Filename.dirname real_path = real_dir &&
       Filename.basename real_path = id ^ ".jsonl" then Some (dir, id)
    else None
  with Unix.Unix_error _ | Sys_error _ | Invalid_argument _
     | Yojson.Json_error _ | Not_found -> None

let set_title ~root session title =
  Session.set_title session title;
  match Session.title session, title_cache_location ~root session with
  | Some title, Some (dir, id) -> write_title_cache dir id title
  | _ -> ()

let toggle_pin ~root session =
  let dir = ensure ~root in
  let item = preview ~root session.Session.path in
  let id = match Protocol.member "id" session.Session.header with
    | `String id when valid_id id && id = item.id -> id
    | _ -> invalid_arg "session ID is invalid" in
  let real_dir = Unix.realpath dir in
  let real_path = Unix.realpath session.Session.path in
  if Filename.dirname real_path <> real_dir ||
     Filename.basename real_path <> id ^ ".jsonl" then
    invalid_arg "session is not a private workspace journal";
  with_file_lock (Filename.concat dir "pins.lock") (fun () ->
    let pinned = not (Session.pinned session) in
    Session.set_pinned session pinned;
    let current = read_pin_ids dir in
    let updated = if pinned then
      if List.mem id current then current else id :: current
    else List.filter ((<>) id) current in
    write_pin_ids dir updated;
    pinned)

let fork ~root session =
  let dir = ensure ~root in
  let forked = Session.fork_managed session dir in
  (match Session.title forked with
   | Some title ->
       let id = Protocol.member "id" forked.Session.header in
       (match id with
        | `String id when valid_id id -> write_title_cache dir id title
        | _ -> invalid_arg "forked session ID is invalid")
   | None -> ());
  forked


let create ~root =
  let dir = ensure ~root in
  Session.create_managed ~cwd:(Unix.realpath root) ~directory:dir

let open_existing ~root path =
  ignore (preview ~root path);
  Session.open_file ~cwd:(Unix.realpath root) path
