(* Write side of the project-memory store. Reads go through
   Pave.Project_memory; this module only creates, replaces and removes
   <name>.md files inside <root>/.pave/memory with the same hardening as
   the reader: the .pave and memory directories must be real directories
   owned by the caller, symlinks are never followed, and each write is
   staged 0600 then renamed. *)

exception Error of string

let fail message = raise (Error message)

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
    else stat in
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

let put ~root ~name text =
  if String.length text > Project_memory.max_file_bytes then
    fail ("memory entry exceeds " ^
      string_of_int Project_memory.max_file_bytes ^ " bytes");
  if not (Local_content.plain_text text) then
    fail "memory entries hold plain UTF-8 text only";
  let path = path_for ~root name in
  let dir = Filename.dirname path in
  let rec open_temp attempt =
    if attempt = 32 then fail "cannot allocate memory staging file";
    let temp = Printf.sprintf "%s.tmp.%d.%d" path (Unix.getpid ()) attempt in
    try temp, Unix.openfile temp
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC] 0o600
    with Unix.Unix_error (Unix.EEXIST, _, _) -> open_temp (attempt + 1) in
  let temp, fd = open_temp 0 in
  (try
     let bytes = Bytes.unsafe_of_string text in
     let rec write_all start =
       if start < Bytes.length bytes then
         let n = Unix.write fd bytes start (Bytes.length bytes - start) in
         if n = 0 then fail "memory write produced no progress"
         else write_all (start + n) in
     write_all 0;
     Unix.fsync fd;
     Unix.close fd;
     (match Unix.lstat temp with
      | stat when stat.Unix.st_kind = Unix.S_REG -> ()
      | _ -> fail "memory staging file changed unexpectedly"
      | exception exn -> raise exn);
     Unix.rename temp path;
     path
   with exn ->
     (try Unix.close fd with Unix.Unix_error _ -> ());
     (try Unix.unlink temp with Unix.Unix_error _ -> ());
     ignore dir;
     raise exn)

let forget ~root ~name =
  let path = path_for ~root name in
  (match Unix.lstat path with
   | stat when stat.Unix.st_kind = Unix.S_REG -> Unix.unlink path; true
   | _ -> fail (path ^ ": not a regular file")
   | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false
   | exception exn -> raise exn)
