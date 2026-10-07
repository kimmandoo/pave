type file_snapshot =
  | Missing
  | Captured of { data : string; mode : int }
  | Unavailable of { exists : bool option; mode : int option; reason : string }
type workspace_effect =
  | File_change of {
      tool_name : string;
      path : string;
      before : file_snapshot;
      after : file_snapshot;
    }
  | Non_reversible_effect of { tool_name : string; detail : string }

type status = Rewindable | Non_reversible | Rewinding | Reverted

type rewind_effect = {
  id : string;
  owner : string;
  tool_name : string;
  path : string option;
  before_exists : bool option;
  before_mode : int option;
  before_sha256 : string option;
  before_size : int option;
  after_exists : bool option;
  after_mode : int option;
  after_sha256 : string option;
  after_size : int option;
  status : status;
  detail : string;
  created_at : float;
}

type t = {
  root : string;
  owner : string;
  directory : string;
  mutex : Mutex.t;
  effects : (string, rewind_effect) Hashtbl.t;
  mutable snapshot_bytes : int;
}

exception Error of string

let max_snapshot_bytes = 1_048_576
let max_total_snapshot_bytes = 128 * 1024 * 1024
let max_effects = 512
let max_metadata_bytes = 65_536
let bounded_detail text =
  if String.length text <= 4096 then text else String.sub text 0 4096

let fail message = raise (Error message)
let non_reversible_tool_name = function
  | "run_command" | "start_process" | "start_shell"
  | "process_stdin" | "process_close_stdin" | "process_kill"
  | "worktree_create" | "worktree_commit" | "worktree_remove"
  | "workspace_eval" | "lsp_start" | "dap_start" | "dap" | "browser"
  | "ssh_open" | "ssh_read" | "ssh_write" | "ssh_command"
  | "web_search" | "web_fetch" | "clipboard_write"
  | "publish_web" | "xcode_preflight" | "mobile_check" | "mobile_session"
  | "mobile_scenario" | "mobile_verify" | "mobile_visual" | "android_devices"
  | "mobile_control" | "mobile_environment" | "mobile_dev_server"
  | "mobile_app_lifecycle" | "mobile_performance" | "mobile_device_lifecycle"
  | "write_file" | "edit_file" | "apply_edits" | "ast_edit" -> true
  | _ -> false

let with_lock t action =
  Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) action
let valid_id value = Session_store.valid_id value
let sha256 text = Digestif.SHA256.(to_hex (digest_string text))
let hex_digest value = String.length value = 64 && String.for_all
  (function '0'..'9' | 'a'..'f' -> true | _ -> false) value
let valid_mode value = value >= 0 && value <= 0o7777
let option_json encode = function None -> `Null | Some value -> encode value
let text_json = option_json (fun value -> `String value)
let bool_json = option_json (fun value -> `Bool value)
let int_json = option_json (fun value -> `Int value)

let status_text = function
  | Rewindable -> "rewindable" | Non_reversible -> "non-reversible"
  | Rewinding -> "rewind interrupted" | Reverted -> "rewound"

let status_json = function
  | Rewindable -> `String "rewindable"
  | Non_reversible -> `String "non_reversible"
  | Rewinding -> `String "rewinding"
  | Reverted -> `String "reverted"

let option_bool = function
  | `Null -> None | `Bool value -> Some value
  | _ -> fail "invalid persisted workspace rewind boolean"
let option_int = function
  | `Null -> None | `Int value -> Some value
  | _ -> fail "invalid persisted workspace rewind integer"
let option_text = function
  | `Null -> None | `String value -> Some value
  | _ -> fail "invalid persisted workspace rewind text"

let effect_json rewind_entry = `Assoc [
  "version", `Int 1;
  "id", `String rewind_entry.id;
  "owner", `String rewind_entry.owner;
  "toolName", `String rewind_entry.tool_name;
  "path", text_json rewind_entry.path;
  "beforeExists", bool_json rewind_entry.before_exists;
  "beforeMode", int_json rewind_entry.before_mode;
  "beforeSha256", text_json rewind_entry.before_sha256;
  "beforeSize", int_json rewind_entry.before_size;
  "afterExists", bool_json rewind_entry.after_exists;
  "afterMode", int_json rewind_entry.after_mode;
  "afterSha256", text_json rewind_entry.after_sha256;
  "afterSize", int_json rewind_entry.after_size;
  "status", status_json rewind_entry.status;
  "detail", `String rewind_entry.detail;
  "createdAt", `Float rewind_entry.created_at]

let parse_effect ~owner ~id text =
  let json = Yojson.Basic.from_string text in
  let fields = Yojson.Basic.Util.to_assoc json in
  let field name = match List.assoc_opt name fields with
    | Some value -> value | None -> fail "invalid persisted workspace rewind metadata" in
  let string name = match field name with
    | `String value -> value | _ -> fail "invalid persisted workspace rewind metadata" in
  let version = field "version" in
  let stored_id = string "id" and stored_owner = string "owner" in
  let tool_name = string "toolName" in
  let path = option_text (field "path") in
  let before_exists = option_bool (field "beforeExists")
  and before_mode = option_int (field "beforeMode")
  and before_sha256 = option_text (field "beforeSha256")
  and before_size = option_int (field "beforeSize")
  and after_exists = option_bool (field "afterExists")
  and after_mode = option_int (field "afterMode")
  and after_sha256 = option_text (field "afterSha256")
  and after_size = option_int (field "afterSize") in
  let status = match field "status" with
    | `String "rewindable" -> Rewindable
    | `String "non_reversible" -> Non_reversible
    | `String "rewinding" -> Rewinding
    | `String "reverted" -> Reverted
    | _ -> fail "invalid persisted workspace rewind status" in
  let detail = string "detail" in
  let created_at = match field "createdAt" with
    | `Float value -> value | `Int value -> float value
    | _ -> fail "invalid persisted workspace rewind timestamp" in
  let valid_option validate = function None -> true | Some value -> validate value in
  let path_valid = match tool_name, path with
    | name, None when non_reversible_tool_name name -> true
    | ("write_file" | "edit_file" | "apply_edits" | "ast_edit" | "lsp"), Some path ->
        String.length path > 0 && String.length path <= 4096 &&
        not (String.contains path '\000')
    | _ -> false in
  let before_consistent = match before_exists with
    | None -> before_mode = None && before_sha256 = None && before_size = None
    | Some false -> before_mode = None && before_sha256 = None && before_size = None
    | Some true -> valid_option valid_mode before_mode &&
        valid_option hex_digest before_sha256 &&
        valid_option (fun size -> size >= 0 && size <= max_snapshot_bytes) before_size &&
        (Option.is_some before_sha256 = Option.is_some before_size) in
  let after_consistent = match after_exists with
    | None -> after_mode = None && after_sha256 = None && after_size = None
    | Some false -> after_mode = None && after_sha256 = None && after_size = None
    | Some true -> valid_option valid_mode after_mode &&
        valid_option hex_digest after_sha256 &&
        valid_option (fun size -> size >= 0 && size <= max_snapshot_bytes) after_size &&
        (Option.is_some after_sha256 = Option.is_some after_size) in
  if version <> `Int 1 || stored_id <> id || stored_owner <> owner ||
     not (valid_id id && valid_id owner) || String.length detail > 4096 ||
     classify_float created_at = FP_nan || classify_float created_at = FP_infinite ||
     not path_valid || not before_consistent || not after_consistent ||
     (status = Rewindable || status = Rewinding || status = Reverted) &&
       (path = None || before_exists = None || after_exists = None ||
        (before_exists = Some true && Option.is_none before_sha256) ||
        (after_exists = Some true && Option.is_none after_sha256)) then
    fail "invalid persisted workspace rewind metadata";
  { id; owner; tool_name; path; before_exists; before_mode; before_sha256;
    before_size; after_exists; after_mode; after_sha256; after_size;
    status; detail; created_at }

let metadata_path t id = Filename.concat t.directory (id ^ ".json")
let backup_path t id = Filename.concat t.directory (id ^ ".before")

let persist t rewind_entry =
  Session_store.write_atomic ~dir:t.directory ~prefix:"rewind-"
    (metadata_path t rewind_entry.id)
    (Yojson.Basic.to_string (effect_json rewind_entry) ^ "\n")

let update t rewind_entry =
  persist t rewind_entry;
  Hashtbl.replace t.effects rewind_entry.id rewind_entry

let same_inode a b = a.Unix.st_dev = b.Unix.st_dev && a.Unix.st_ino = b.Unix.st_ino
let stable_stat a b = same_inode a b && a.Unix.st_kind = b.Unix.st_kind &&
  a.Unix.st_size = b.Unix.st_size && a.Unix.st_perm = b.Unix.st_perm &&
  a.Unix.st_mtime = b.Unix.st_mtime && a.Unix.st_ctime = b.Unix.st_ctime

let read_exact fd size =
  let bytes = Bytes.create size in
  let rec loop offset =
    if offset = size then Some (Bytes.unsafe_to_string bytes)
    else
      let count = try Unix.read fd bytes offset (size - offset) with
        | Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
      if count = 0 then None
      else if count < 0 then loop offset
      else loop (offset + count) in
  match loop 0 with
  | None -> None
  | Some data ->
      let extra = Bytes.create 1 in
      let count = Unix.read fd extra 0 1 in
      if count = 0 then Some data else None

let snapshot_file ~root ~path =
  let unavailable ?exists ?mode reason =
    Unavailable { exists; mode; reason } in
  try
    let root = Workspace_path.root_path root in
    let absolute = Workspace_path.writable_path root path in
    let before = try Unix.lstat absolute with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> raise Not_found in
    if before.Unix.st_kind <> Unix.S_REG then
      unavailable ~exists:true ~mode:(before.Unix.st_perm land 0o7777)
        "target is not a regular file" 
    else if before.Unix.st_size < 0 || before.Unix.st_size > max_snapshot_bytes then
      unavailable ~exists:true ~mode:(before.Unix.st_perm land 0o7777)
        "file exceeds the 1 MiB rewind snapshot limit"
    else
      let fd = Unix.openfile absolute
        [Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC] 0 in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
        let opened = Unix.fstat fd and current = Unix.lstat absolute in
        if not (opened.Unix.st_kind = Unix.S_REG &&
                same_inode before opened && same_inode opened current &&
                opened.Unix.st_size = before.Unix.st_size) then
          unavailable ~exists:true ~mode:(before.Unix.st_perm land 0o7777)
            "file changed while taking a rewind snapshot"
        else match read_exact fd opened.Unix.st_size with
          | None -> unavailable ~exists:true ~mode:(opened.Unix.st_perm land 0o7777)
              "file changed while taking a rewind snapshot"
          | Some data ->
              let after = Unix.fstat fd and current = Unix.lstat absolute in
              if not (stable_stat opened after && same_inode after current &&
                      current.Unix.st_kind = Unix.S_REG) then
                unavailable ~exists:true ~mode:(after.Unix.st_perm land 0o7777)
                  "file changed while taking a rewind snapshot"
              else Captured { data; mode = after.Unix.st_perm land 0o7777 })
  with
  | Not_found -> Missing
  | exn -> unavailable ?exists:None
      ("safe file snapshot unavailable: " ^ Printexc.to_string exn)

let snapshot_exists = function
  | Missing -> Some false
  | Captured _ -> Some true
  | Unavailable { exists; _ } -> exists

let snapshot_mode = function
  | Captured { mode; _ } -> Some mode
  | Unavailable { mode; _ } -> mode
  | Missing -> None

let snapshot_data = function Captured { data; _ } -> Some data | _ -> None
let snapshot_size snapshot = Option.map String.length (snapshot_data snapshot)
let snapshot_digest snapshot = Option.map sha256 (snapshot_data snapshot)

let snapshot_reason = function
  | Missing -> "file was absent"
  | Captured _ -> "file was captured"
  | Unavailable { reason; _ } -> reason

let snapshot_equal a b = match a, b with
  | Missing, Missing -> true
  | Captured a, Captured b -> a.mode = b.mode && String.equal a.data b.data
  | _ -> false


let create_effect t ~tool_name ~path ~before ~after ~status ~detail =
  let id = Session.fresh_id () in
  let rewind_entry = {
    id; owner = t.owner; tool_name; path;
    before_exists = snapshot_exists before;
    before_mode = snapshot_mode before;
    before_sha256 = snapshot_digest before;
    before_size = snapshot_size before;
    after_exists = snapshot_exists after;
    after_mode = snapshot_mode after;
    after_sha256 = snapshot_digest after;
    after_size = snapshot_size after;
    status; detail = bounded_detail detail; created_at = Unix.gettimeofday () } in
  rewind_entry

let effect_count t = Hashtbl.length t.effects

let write_effect t rewind_entry backup =
  if Hashtbl.mem t.effects rewind_entry.id then fail "workspace rewind effect ID collision";
  if effect_count t >= max_effects then fail "workspace rewind effect limit reached";
  let backup_bytes = match backup with None -> 0 | Some data -> String.length data in
  let can_store = backup_bytes = 0 ||
    t.snapshot_bytes <= max_total_snapshot_bytes - backup_bytes in
  let rewind_entry, backup = if can_store then rewind_entry, backup else
    { rewind_entry with status = Non_reversible;
      detail = "private rewind snapshot quota reached; this file change cannot be reversed" },
    None in
  (match backup with
   | None -> ()
   | Some data ->
       Session_store.write_atomic ~dir:t.directory ~prefix:"snapshot-"
         (backup_path t rewind_entry.id) data);
  (try persist t rewind_entry with exn ->
    (match backup with None -> () | Some _ ->
      (try Unix.unlink (backup_path t rewind_entry.id)
       with Unix.Unix_error _ -> ()));
    raise exn);
  Hashtbl.add t.effects rewind_entry.id rewind_entry;
  t.snapshot_bytes <- t.snapshot_bytes +
    (match backup with None -> 0 | Some data -> String.length data);
  rewind_entry

let record_file_change t ~tool_name ~path ~before ~after =
  with_lock t (fun () ->
    if not (List.mem tool_name
        ["write_file"; "edit_file"; "apply_edits"; "ast_edit"; "lsp"]) then
      fail "unsupported workspace rewind tool";
    if String.length path = 0 || String.length path > 4096 ||
       String.contains path '\000' then fail "invalid workspace rewind path";
    ignore (Workspace_path.writable_path t.root path);
    if snapshot_equal before after then None
    else (
      let status, detail, backup = match before, after with
        | Missing, Captured _ -> Rewindable, "", None
        | Captured old, Captured current when old.mode = current.mode ->
            Rewindable, "", Some old.data
        | Captured _, Captured _ ->
            Non_reversible,
            "file mode changed during the operation; refusing an incomplete rewind",
            None
        | _, _ ->
            Non_reversible,
            "file change could not be captured safely (before: " ^
            snapshot_reason before ^ "; after: " ^ snapshot_reason after ^ ")",
            None in
      let rewind_entry = create_effect t ~tool_name ~path:(Some path)
        ~before ~after ~status ~detail in
      let rewind_entry = write_effect t rewind_entry backup in
      Some rewind_entry))

let record_non_reversible t ~tool_name ~detail =
  with_lock t (fun () ->
    if not (non_reversible_tool_name tool_name) then
      fail "unsupported non-reversible workspace effect";
    if String.length detail > 4096 then fail "workspace effect detail is too long";
    let rewind_entry = create_effect t ~tool_name ~path:None ~before:Missing
      ~after:(Unavailable { exists = None; mode = None; reason = detail })
      ~status:Non_reversible ~detail in
    write_effect t rewind_entry None)

let load_effect t id =
  match Session_store.read_private_file (metadata_path t id) max_metadata_bytes with
  | None -> fail "workspace rewind metadata is missing or unsafe"
  | Some text ->
      (try parse_effect ~owner:t.owner ~id text with
       | Error _ as exn -> raise exn
       | exn -> fail ("invalid workspace rewind metadata: " ^ Printexc.to_string exn))

let verify_backup t rewind_entry =
  match rewind_entry.before_exists, rewind_entry.before_sha256,
      rewind_entry.before_size with
  | Some false, None, None -> true
  | Some true, Some digest, Some size ->
      (match Session_store.read_private_file
          (backup_path t rewind_entry.id) max_snapshot_bytes with
       | Some data -> String.length data = size && sha256 data = digest
       | None -> false)
  | _ -> false

let matches_snapshot snapshot ~exists ~mode ~digest =
  match exists, snapshot with
  | Some false, Missing -> true
  | Some true, Captured { data; mode = current_mode } ->
      Some current_mode = mode && Some (sha256 data) = digest
  | _ -> false

let matches_before t rewind_entry = match rewind_entry.path with
  | None -> false
  | Some path -> matches_snapshot (snapshot_file ~root:t.root ~path)
      ~exists:rewind_entry.before_exists ~mode:rewind_entry.before_mode
      ~digest:rewind_entry.before_sha256

let matches_after t rewind_entry = match rewind_entry.path with
  | None -> false
  | Some path -> matches_snapshot (snapshot_file ~root:t.root ~path)
      ~exists:rewind_entry.after_exists ~mode:rewind_entry.after_mode
      ~digest:rewind_entry.after_sha256

let remove_backup t id =
  try Unix.unlink (backup_path t id) with Unix.Unix_error _ -> ()

let recover_rewinding t rewind_entry =
  let next = if matches_before t rewind_entry then
      { rewind_entry with status = Reverted;
        detail = "rewind completed before the previous process stopped" }
    else if matches_after t rewind_entry && verify_backup t rewind_entry then
      { rewind_entry with status = Rewindable; detail = "" }
    else
      { rewind_entry with status = Non_reversible;
        detail = "rewind was interrupted and the workspace no longer matches either recorded state" } in
  update t next;
  if next.status = Reverted || next.status = Non_reversible then
    remove_backup t rewind_entry.id

let create ~root ~session =
  let root = try Unix.realpath root with Unix.Unix_error _ ->
    fail "workspace rewind root does not exist" in
  let owner = Session.session_id session in
  let workspace = Session_store.ensure ~root in
  let parent = Filename.concat workspace "rewind" in
  Session_store.ensure_directory parent;
  let directory = Filename.concat parent owner in
  Session_store.ensure_directory directory;
  let t = { root; owner; directory; mutex = Mutex.create ();
    effects = Hashtbl.create 32; snapshot_bytes = 0 } in
  let names = Sys.readdir directory |> Array.to_list |> List.sort String.compare in
  List.iter (fun name ->
    if String.length name = 37 && String.sub name 32 5 = ".json" then (
      let id = String.sub name 0 32 in
      if not (valid_id id) then fail "invalid workspace rewind filename";
      let rewind_entry = load_effect t id in
      if Hashtbl.mem t.effects id then fail "duplicate workspace rewind effect";
      Hashtbl.add t.effects id rewind_entry)) names;
  if Hashtbl.length t.effects > max_effects then fail "workspace rewind effect limit exceeded";
  List.iter (fun rewind_entry -> match rewind_entry.status with
    | Rewindable when not (verify_backup t rewind_entry) ->
        update t { rewind_entry with status = Non_reversible;
          detail = "rewind snapshot is missing or fails integrity verification" }
    | Rewinding -> recover_rewinding t rewind_entry
    | _ -> ()) (Hashtbl.fold (fun _ rewind_entry all ->
      rewind_entry :: all) t.effects []);
  t.snapshot_bytes <- Hashtbl.fold (fun _ rewind_entry total ->
    if rewind_entry.status = Rewindable &&
        rewind_entry.before_exists = Some true then
      total + Option.value ~default:0 rewind_entry.before_size
    else total) t.effects 0;
  if t.snapshot_bytes > max_total_snapshot_bytes then
    fail "workspace rewind snapshots exceed the private storage limit";
  t

let list t =
  Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) (fun () ->
    Hashtbl.fold (fun _ rewind_entry all -> rewind_entry :: all) t.effects []
    |> List.sort (fun a b ->
      let order = compare b.created_at a.created_at in
      if order = 0 then String.compare a.id b.id else order))

let find t ~id =
  if not (valid_id id) then None
  else (
    Mutex.lock t.mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) (fun () ->
      Hashtbl.find_opt t.effects id))

let update_status t rewind_entry status detail =
  let next = { rewind_entry with status; detail } in
  update t next;
  next

let restore_file t rewind_entry =
  let path = match rewind_entry.path with
    | Some path -> path | None -> fail "effect has no workspace path" in
  let absolute = Workspace_path.writable_path t.root path in
  if not (matches_after t rewind_entry) then
    fail "workspace file changed after the rewind preview; no change was made";
  match rewind_entry.before_exists with
  | Some false ->
      (match snapshot_file ~root:t.root ~path with
       | Captured _ -> Unix.unlink absolute
       | _ -> fail "workspace file is no longer a regular file; no change was made")
  | Some true ->
      let data = match Session_store.read_private_file
          (backup_path t rewind_entry.id) max_snapshot_bytes with
        | Some data when Some (String.length data) = rewind_entry.before_size &&
            Some (sha256 data) = rewind_entry.before_sha256 -> data
        | _ -> fail "private rewind snapshot failed integrity verification" in
      if not (matches_after t rewind_entry) then
        fail "workspace file changed during rewind; no change was made";
      Workspace_path.atomic_write absolute data
  | None -> fail "effect has no reversible prior file state"

let rewind t ~id =
  Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) (fun () ->
    let rewind_entry = match Hashtbl.find_opt t.effects id with
      | Some rewind_entry -> rewind_entry
      | None -> fail "workspace rewind effect was not found" in
    let rewind_entry = if rewind_entry.status = Rewinding then (
      recover_rewinding t rewind_entry;
      Hashtbl.find t.effects id) else rewind_entry in
    match rewind_entry.status with
    | Non_reversible -> fail ("effect is not reversible: " ^ rewind_entry.detail)
    | Rewinding -> fail "rewind remains interrupted; inspect the workspace"
    | Reverted -> fail "workspace effect has already been rewound"
    | Rewindable ->
        if not (verify_backup t rewind_entry) then
          fail "private rewind snapshot failed integrity verification";
        if not (matches_after t rewind_entry) then
          fail "workspace file changed since this effect; refusing to overwrite user changes";
        ignore (update_status t rewind_entry Rewinding "rewind in progress");
        let current = Hashtbl.find t.effects id in
        (try restore_file t current with exn ->
          fail ("rewind did not complete; the next session will inspect the file state: " ^
            Printexc.to_string exn));
        if not (matches_before t current) then
          fail "rewind output did not match the recorded prior state; inspect the workspace";
        let reverted = update_status t current Reverted
          "workspace file restored from its recorded pre-change snapshot" in
        if reverted.before_exists = Some true then (
          t.snapshot_bytes <- max 0 (t.snapshot_bytes -
            Option.value ~default:0 reverted.before_size);
          remove_backup t id);
        "Rewound " ^ Option.value ~default:"workspace file" reverted.path)
