(* Write side of the project-memory store. Reads go through
   Pave.Project_memory; this module only creates, replaces and removes
   <name>.md files inside <root>/.pave/memory with the same hardening as
   the reader: the .pave and memory directories must be real directories
   owned by the caller, symlinks are never followed, and each write is
   staged 0600 then renamed. *)

exception Error of string

let fail message = raise (Error message)

let guard = Mutex.create ()

let ordinary_file path =
  match Unix.lstat path with
  | stat when stat.Unix.st_kind = Unix.S_REG -> Some stat
  | _ -> fail (path ^ ": not a regular file")
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None

let memory_dir ~root =
  let dir = Filename.concat root ".pave" in
  let memory = Filename.concat dir "memory" in
  let check path =
    let stat = try Unix.lstat path with
      | Unix.Unix_error (error, operation, _) ->
          fail (operation ^ ": " ^ Unix.error_message error) in
    if stat.Unix.st_kind <> Unix.S_DIR then
      fail (path ^ ": not an ordinary directory")
    else if stat.Unix.st_uid <> Unix.geteuid () then
      fail (path ^ ": directory is not owned by the caller")
    else if stat.Unix.st_perm land 0o022 <> 0 then
      fail (path ^ ": directory is writable by other users")
    else stat in
  ignore (check root);
  (try Unix.mkdir dir 0o700 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  ignore (check dir);
  (try Unix.mkdir memory 0o700 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  ignore (check memory);
  memory

let path_for ~root name =
  if not (Local_content.name_ok name) then
    fail ("invalid memory name: " ^ name ^
      " (expected [a-z][a-z0-9_-]{0,47})");
  Filename.concat (memory_dir ~root) (name ^ ".md")

(* lockf serializes processes; the mutex also serializes threads because
   POSIX record locks are process-scoped. The lock stays outside memory so
   it cannot consume a memory entry or become prompt data. *)
let with_store ~root ~name action =
  Mutex.protect guard (fun () ->
    let path = path_for ~root name in
    let roots = match Project_memory.memory_dir root with
      | Project_memory.Present (_, roots) -> roots
      | _ -> fail "memory directory changed" in
    let unchanged () =
      if not (Project_memory.dirs_unchanged roots) then
        fail "memory directory changed during update" in
    let lock_path = Filename.concat (Filename.concat root ".pave") "memory.lock" in
    ignore (ordinary_file lock_path);
    let fd = Unix.openfile lock_path
      [Unix.O_RDWR; Unix.O_CREAT; Unix.O_NONBLOCK; Unix.O_CLOEXEC] 0o600 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      let opened = Unix.fstat fd in
      (match ordinary_file lock_path with
       | Some stat when Project_memory.same_stats opened stat &&
                        stat.Unix.st_uid = Unix.geteuid () &&
                        stat.Unix.st_nlink = 1 &&
                        stat.Unix.st_perm land 0o077 = 0 -> ()
       | _ -> fail "unsafe memory lock file");
      unchanged ();
      Unix.fchmod fd 0o600;
      Unix.lockf fd Unix.F_LOCK 0;
      Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) (fun () ->
        unchanged ();
        action path unchanged)))

let check_entry_limit dir path =
  match ordinary_file path with
  | Some _ -> ()
  | None ->
      let names = match Project_memory.entry_names dir with
        | `Names names -> names
        | _ -> fail "cannot safely count memory entries" in
      let count = List.fold_left (fun count filename ->
        if Filename.extension filename = ".md" &&
           Local_content.name_ok (Filename.remove_extension filename) then
          match Unix.lstat (Filename.concat dir filename) with
          | stat when stat.Unix.st_kind = Unix.S_REG -> count + 1
          | _ -> count
        else count) 0 names in
      if count >= Project_memory.max_entries then
        fail ("memory entry limit reached (" ^
          string_of_int Project_memory.max_entries ^ ")")

let put ~root ~name text =
  if String.length text > Project_memory.max_file_bytes then
    fail ("memory entry exceeds " ^
      string_of_int Project_memory.max_file_bytes ^ " bytes");
  if not (Local_content.plain_text text) then
    fail "memory entries hold plain UTF-8 text only";
  with_store ~root ~name (fun path unchanged ->
  let dir = Filename.dirname path in
  check_entry_limit dir path;
  let rec open_temp attempt =
    if attempt = 32 then fail "cannot allocate memory staging file";
    let temp = Printf.sprintf "%s.tmp.%d.%d" path (Unix.getpid ()) attempt in
    try temp, Unix.openfile temp
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o600
    with Unix.Unix_error (Unix.EEXIST, _, _) -> open_temp (attempt + 1) in
  let temp, fd = open_temp 0 in
  let staged = Unix.fstat fd in
  let opened = ref true in
  (try
     Unix.fchmod fd 0o600;
     let bytes = Bytes.unsafe_of_string text in
     let rec write_all start =
       if start < Bytes.length bytes then
         let n = try Unix.write fd bytes start (Bytes.length bytes - start)
           with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
         if n = 0 then fail "memory write produced no progress"
         else write_all (start + max 0 n) in
     write_all 0;
     Unix.fsync fd;
     unchanged ();
     (match ordinary_file temp with
      | Some stat when Project_memory.same_stats staged stat -> ()
      | _ -> fail "memory staging file changed unexpectedly");
     ignore (ordinary_file path);
     Unix.close fd;
     opened := false;
     Unix.rename temp path;
     path
   with exn ->
     if !opened then (try Unix.close fd with Unix.Unix_error _ -> ());
     (try
        unchanged ();
        match ordinary_file temp with
          | Some stat when Project_memory.same_stats staged stat -> Unix.unlink temp
          | _ -> ()
      with Unix.Unix_error _ | Error _ -> ());
     raise exn)
  )

let forget ~root ~name =
  with_store ~root ~name (fun path unchanged ->
    match ordinary_file path with
    | None -> false
    | Some _ -> unchanged (); Unix.unlink path; true)
