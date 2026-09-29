(* Manifests contain references, never executable definitions. Snapshot construction
   fails closed for ambiguous names; state changes are committed before publication. *)
type capabilities = { skills : string list; commands : string list; tools : string list }
type diagnostic = { path : string; code : string; message : string }
type plugin = {
  name : string; version : string; source : string; digest : string;
  enabled : bool; references : capabilities; active : capabilities;
}
type snapshot = { plugins : plugin list; active : capabilities; diagnostics : diagnostic list }
type registry = {
  directory : string; available : capabilities; builtins : capabilities;
  mutable current : snapshot;
}

exception Invalid of string
let fail message = raise (Invalid message)
let empty = { skills = []; commands = []; tools = [] }
let issue path code message = { path; code; message }
let valid_name text =
  let n = String.length text in
  n > 0 && n <= 48 && (match text.[0] with 'a'..'z' -> true | _ -> false) &&
  String.for_all (function 'a'..'z' | '0'..'9' | '_' | '-' -> true | _ -> false) text
let version text =
  let part s =
    s <> "" && (s = "0" || s.[0] <> '0') &&
    String.for_all (function '0'..'9' -> true | _ -> false) s in
  match String.split_on_char '.' text with
  | [a; b; c] -> part a && part b && part c && String.length text <= 32
  | _ -> false
let sorted_unique values = List.sort_uniq String.compare values
let fields label = function
  | `Assoc fields ->
    let names = List.map fst fields in
    if List.length names <> List.length (sorted_unique names) then
      fail ("duplicate " ^ label ^ " field");
    fields
  | _ -> fail (label ^ " must be an object")
let exact label expected fields =
  if List.sort String.compare (List.map fst fields) <> List.sort String.compare expected then
    fail ("unexpected or missing " ^ label ^ " fields")
let field fields name = List.assoc name fields
let text label = function
  | `String value -> value
  | _ -> fail (label ^ " must be a string")
let names label = function
  | `List values when List.length values <= 64 ->
    let values = List.map (fun value ->
      let name = text label value in
      if not (valid_name name) then fail ("invalid " ^ label ^ " name");
      name) values in
    if List.length values <> List.length (sorted_unique values) then
      fail ("duplicate " ^ label ^ " reference");
    values
  | _ -> fail (label ^ " must be an array of at most 64 names")
let parse_manifest path data =
  let json = try Yojson.Basic.from_string data
    with Yojson.Json_error _ -> fail "invalid manifest JSON" in
  let fields = fields "manifest" json in
  exact "manifest" ["schemaVersion"; "name"; "version"; "skills"; "commands"; "tools"] fields;
  if field fields "schemaVersion" <> `Int 1 then fail "unsupported manifest schemaVersion";
  let name = text "plugin name" (field fields "name") in
  let version_text = text "plugin version" (field fields "version") in
  if not (valid_name name) then fail "invalid plugin name";
  if not (version version_text) then fail "plugin version must be major.minor.patch";
  if Filename.basename path <> name ^ ".json" then fail "plugin name differs from filename";
  let references = {
    skills = names "skill" (field fields "skills");
    commands = names "command" (field fields "commands");
    tools = names "tool" (field fields "tools");
  } in
  name, version_text, references

let same a b = a.Unix.st_dev = b.Unix.st_dev && a.Unix.st_ino = b.Unix.st_ino
let private_directory path =
  let stat = Unix.lstat path in
  if stat.Unix.st_kind <> Unix.S_DIR || stat.Unix.st_uid <> Unix.geteuid () ||
     stat.Unix.st_perm land 0o077 <> 0 then
    fail ("plugin directory is not private and user-owned: " ^ path);
  stat
let private_file stat limit =
  stat.Unix.st_kind = Unix.S_REG && stat.Unix.st_uid = Unix.geteuid () &&
  stat.Unix.st_nlink = 1 && stat.Unix.st_perm land 0o077 = 0 &&
  stat.Unix.st_size >= 0 && stat.Unix.st_size <= limit
let stable a b =
  same a b && a.Unix.st_size = b.Unix.st_size &&
  a.Unix.st_mtime = b.Unix.st_mtime && a.Unix.st_ctime = b.Unix.st_ctime
let read_file path limit =
  let before = Unix.lstat path in
  if not (private_file before limit) then fail ("unsafe or oversized private file: " ^ path);
  let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let opened = Unix.fstat fd in
    if not (stable before opened && private_file opened limit) then
      fail ("file changed while opening: " ^ path);
    let buffer = Bytes.create (limit + 1) in
    let rec read offset =
      if offset = limit + 1 then offset else
      let count = Unix.read fd buffer offset (limit + 1 - offset) in
      if count = 0 then offset else read (offset + count) in
    let length = read 0 in
    let after = Unix.lstat path in
    if length > limit || length <> opened.Unix.st_size ||
       not (stable opened after && private_file after limit) then
      fail ("file changed or exceeded limit while reading: " ^ path);
    Bytes.sub_string buffer 0 length)
let state_file dir = Filename.concat dir ".state.json"
let state_limit = 8192
let read_state dir =
  let path = state_file dir in
  let data = try Some (read_file path state_limit) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> None in
  match data with
  | None -> []
  | Some data ->
    let json = try Yojson.Basic.from_string data
      with Yojson.Json_error _ -> fail "invalid plugin registry state JSON" in
    let fields = fields "registry state" json in
    exact "registry state" ["schemaVersion"; "enabled"] fields;
    if field fields "schemaVersion" <> `Int 1 then fail "unsupported plugin registry state version";
    names "enabled plugin" (field fields "enabled")
let check_ancestors path =
  if Filename.is_relative path then fail "plugin user directory must be absolute";
  let parts = String.split_on_char '/' path in
  if List.exists (fun part -> part = "." || part = "..") parts then
    fail "plugin user directory cannot contain traversal";
  let rec inspect base = function
    | [] -> ()
    | "" :: rest -> inspect base rest
    | part :: rest ->
      let next = Filename.concat base part in
      let stat = Unix.lstat next in
      if stat.Unix.st_kind <> Unix.S_DIR then
        fail ("plugin directory path contains symlink or non-directory: " ^ next);
      inspect next rest in
  inspect "/" parts
let ensure_dir user_dir =
  check_ancestors user_dir;
  ignore (private_directory user_dir);
  let dir = Filename.concat user_dir "plugins" in
  (try Unix.mkdir dir 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  ignore (private_directory dir);
  dir
let entries dir =
  let handle = Unix.opendir dir in
  Fun.protect ~finally:(fun () -> Unix.closedir handle) (fun () ->
    let rec collect acc count = match Unix.readdir handle with
      | "." | ".." | ".state.json" | ".lock" -> collect acc count
      | name when String.starts_with ~prefix:".state." name -> collect acc count
      | name ->
        if count >= 64 then fail "plugin directory exceeds 64 manifest candidates";
        collect (name :: acc) (count + 1)
      | exception End_of_file -> List.sort String.compare acc in
    collect [] 0)
let scan directory available builtins enabled =
  let dir_before = private_directory directory in
  let candidates = entries directory in
  let discovered = ref [] and diagnostics = ref [] in
  List.iter (fun filename ->
    let path = Filename.concat directory filename in
    try
      if Filename.extension filename <> ".json" ||
         not (valid_name (Filename.remove_extension filename)) then
        fail "plugin filename must be <lowercase-name>.json";
      let data = read_file path 65_536 in
      let name, version, references = parse_manifest path data in
      let digest = Digestif.SHA256.(to_hex (digest_string data)) in
      discovered := (name, version, path, digest, references) :: !discovered
    with
    | Invalid message -> diagnostics := issue path "invalid_manifest" message :: !diagnostics
    | Unix.Unix_error _ | Sys_error _ ->
      diagnostics := issue path "unsafe_path" "cannot read private plugin manifest" :: !diagnostics
  ) candidates;
  let discovered = List.rev !discovered in
  let collisions = Hashtbl.create 64 in
  let record kind builtin selector =
    List.iter (fun (name, _, path, _, refs) ->
      List.iter (fun reference ->
        let key = kind ^ ":" ^ reference in
        let prior = match Hashtbl.find_opt collisions key with
          | None -> [] | Some xs -> xs in
        Hashtbl.replace collisions key ((name, path) :: prior);
        if List.mem reference builtin then
          diagnostics := issue path "builtin_collision"
            ("plugin references reserved " ^ kind ^ " " ^ reference) :: !diagnostics
      ) (selector refs)) discovered in
  record "skill" builtins.skills (fun refs -> refs.skills);
  record "command" builtins.commands (fun refs -> refs.commands);
  record "tool" builtins.tools (fun refs -> refs.tools);
  let blocked = Hashtbl.create 64 in
  List.iter (fun diagnostic ->
    if diagnostic.code = "builtin_collision" then
      Hashtbl.replace blocked diagnostic.path ()) !diagnostics;
  Hashtbl.iter (fun key owners ->
    if List.length owners > 1 then List.iter (fun (_, path) ->
      Hashtbl.replace blocked path ();
      diagnostics := issue path "duplicate_reference"
        ("ambiguous plugin capability " ^ key) :: !diagnostics) owners
  ) collisions;
  let active = ref empty in
  let plugins = List.filter_map (fun (name, version, source, digest, references) ->
    if Hashtbl.mem blocked source then None else
    let enabled = List.mem name enabled in
    let selected available values =
      if enabled then List.filter (fun value -> List.mem value available) values else [] in
    let selected = {
      skills = selected available.skills references.skills;
      commands = selected available.commands references.commands;
      tools = selected available.tools references.tools;
    } in
    if enabled then (
      active := { skills = !active.skills @ selected.skills;
                  commands = !active.commands @ selected.commands;
                  tools = !active.tools @ selected.tools };
      let missing label available values = List.iter (fun value ->
        if not (List.mem value available) then
          diagnostics := issue source "unavailable_reference"
            ("plugin " ^ label ^ " is not loaded: " ^ value) :: !diagnostics) values in
      missing "skill" available.skills references.skills;
      missing "command" available.commands references.commands;
      missing "tool" available.tools references.tools);
    Some { name; version; source; digest; enabled; references; active = selected }
  ) discovered in
  let dir_after = private_directory directory in
  if not (same dir_before dir_after) then fail "plugin directory changed during scan";
  { plugins; active = !active;
    diagnostics = List.sort (fun a b ->
      let cmp = String.compare a.path b.path in
      if cmp <> 0 then cmp else String.compare a.code b.code) !diagnostics }
let protect f =
  try Ok (f ()) with
  | Invalid message -> Error message
  | Unix.Unix_error (error, operation, _) ->
    Error (operation ^ ": " ^ Unix.error_message error)
  | Sys_error message -> Error message
let load ~user_dir ~available ~builtins = protect (fun () ->
  let directory = ensure_dir user_dir in
  let enabled = read_state directory in
  let current = scan directory available builtins enabled in
  { directory; available; builtins; current })
let snapshot registry = registry.current
let reload registry = protect (fun () ->
  let enabled = read_state registry.directory in
  let next = scan registry.directory registry.available registry.builtins enabled in
  registry.current <- next; next)

(* The persistent lock serializes independent processes, while re-reading state
   inside it avoids updates based on stale in-memory snapshots. *)
let with_lock dir f =
  ignore (private_directory dir);
  let path = Filename.concat dir ".lock" in
  let before = try Some (Unix.lstat path) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> None in
  (match before with Some stat when not (private_file stat 1024) ->
    fail "unsafe plugin registry lock file" | _ -> ());
  let fd = Unix.openfile path
    [Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let opened = Unix.fstat fd and after = Unix.lstat path in
    if not (private_file opened 1024 && same opened after) ||
       (match before with Some stat -> not (same stat opened) | None -> false) then
      fail "plugin registry lock changed while opening";
    Unix.lockf fd Unix.F_LOCK 0;
    Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) f)
let write_state dir enabled =
  let path = state_file dir in
  (try let prior = Unix.lstat path in
       if not (private_file prior state_limit) then fail "unsafe plugin registry state file"
   with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  let data = Yojson.Basic.to_string (`Assoc [
    "schemaVersion", `Int 1;
    "enabled", `List (List.map (fun name -> `String name) enabled)]) in
  let rec open_temp attempt =
    if attempt = 32 then fail "cannot allocate private plugin state temp file";
    let temp = Filename.concat dir
      (Printf.sprintf ".state.%d.%d" (Unix.getpid ()) attempt) in
    try temp, Unix.openfile temp
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o600
    with Unix.Unix_error (Unix.EEXIST, _, _) -> open_temp (attempt + 1) in
  let temp, fd = open_temp 0 in
  Fun.protect ~finally:(fun () ->
    (try Unix.close fd with Unix.Unix_error _ -> ());
    (try Unix.unlink temp with Unix.Unix_error (Unix.ENOENT, _, _) -> ()))
    (fun () ->
      let bytes = Bytes.unsafe_of_string data in
      let rec output start = if start < Bytes.length bytes then
        let n = Unix.write fd bytes start (Bytes.length bytes - start) in
        if n = 0 then fail "plugin registry state write failed" else output (start + n) in
      output 0;
      Unix.fsync fd;
      ignore (private_directory dir);
      Unix.rename temp path;
      let directory_fd = Unix.openfile dir [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
      Fun.protect ~finally:(fun () -> Unix.close directory_fd)
        (fun () -> Unix.fsync directory_fd))
let change registry name enabled = protect (fun () ->
  if not (valid_name name) then fail "invalid plugin name";
  with_lock registry.directory (fun () ->
    let prior = read_state registry.directory in
    let current = scan registry.directory registry.available registry.builtins prior in
    if not (List.exists (fun plugin -> plugin.name = name) current.plugins ||
            (not enabled && List.mem name prior)) then
      fail ("unknown or invalid plugin: " ^ name);
    let next_names = if enabled then sorted_unique (name :: prior)
      else List.filter ((<>) name) prior in
    let next = scan registry.directory registry.available registry.builtins next_names in
    if next_names <> prior then write_state registry.directory next_names;
    registry.current <- next;
    next))
let enable registry name = change registry name true
let disable registry name = change registry name false
