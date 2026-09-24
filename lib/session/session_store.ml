type recent = {
  path : string;
  title : string;
  started : string;
  modified : float;
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
        let root, started = match field "type", field "version", field "cwd", field "timestamp" with
          | `String "session", `Int 1, `String root, `String started -> root, started
          | _ -> invalid_arg "session journal has an invalid header" in
        let title =
          let start = ending + 1 in
          match String.index_from_opt content start '\n' with
          | None -> "(untitled)"
          | Some finish ->
              let line = String.sub content start (finish - start) in
              (try
                let json = Yojson.Basic.from_string line in
                let message = Protocol.member "message" json in
                let content = Protocol.member "content" message in
                match Protocol.member "type" json,
                  Protocol.member "role" message, content with
                | `String "message", `String "user", `String text ->
                    let summary = String.split_on_char '\n' text |> List.hd
                      |> String.trim in
                    if summary = "" then "(untitled)" else summary
                | _ -> "(untitled)"
               with Yojson.Json_error _ -> "(untitled)") in
        root, started, title)

let preview ~root path =
  let stat = Unix.lstat path in
  let journal_root, started, raw_title = read_preview path stat in
  if journal_root <> Unix.realpath root then
    invalid_arg "session belongs to a different workspace";
  let title =
    let text = if valid_utf8 raw_title then raw_title else "(untitled)" in
    match Session_tree.first_line text with
    | "" -> "(untitled)"
    | safe -> safe in
  { path; started = Session_tree.first_line started; title;
    modified = stat.Unix.st_mtime }

let recent ~root =
  let path = directory ~root in
  let entries =
    try
      safe_directory path;
      Sys.readdir path
    with Unix.Unix_error (Unix.ENOENT, _, _) -> [||] in
  if Array.length entries > max_files then
    invalid_arg "session directory exceeds 1024 entries; inspect it manually";
  let entries = Array.to_list entries |> List.filter valid_name in
  let records = List.filter_map (fun name ->
    let file = Filename.concat path name in
    try Some (preview ~root file) with
    | Unix.Unix_error _ | Sys_error _ | Invalid_argument _ | Yojson.Json_error _ -> None)
    entries in
  let ordered = List.sort (fun left right ->
    let by_time = Float.compare right.modified left.modified in
    if by_time <> 0 then by_time else String.compare right.path left.path) records in
  List.filteri (fun index _ -> index < max_recent) ordered

let create ~root =
  let dir = ensure ~root in
  let name = Session.fresh_id () ^ ".jsonl" in
  Session.open_file ~cwd:(Unix.realpath root) (Filename.concat dir name)

let open_existing ~root path =
  ignore (preview ~root path);
  Session.open_file ~cwd:(Unix.realpath root) path
