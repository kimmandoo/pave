exception Error of string

let fail message = raise (Error message)

let max_read_bytes = 65_536
let max_write_bytes = 1_048_576

let within root path =
  path = root ||
  (let prefix = if root = "/" then root else root ^ "/" in
   String.length path >= String.length prefix &&
   String.sub path 0 (String.length prefix) = prefix)

let root_path root =
  let root = Unix.realpath root in
  if (Unix.stat root).Unix.st_kind <> Unix.S_DIR then
    fail "workspace root is not a directory";
  root

let checked_path root relative =
  if relative = "" || String.contains relative '\000' || not (Filename.is_relative relative) ||
     List.exists (( = ) "..") (String.split_on_char '/' relative) then
    fail "path must be a nonempty workspace-relative path without '..'";
  let path = Filename.concat root relative in
  (* Checking the canonical parent also handles a new file, for which realpath
     on the final component cannot yet succeed. *)
  let parent = Unix.realpath (Filename.dirname path) in
  if not (within root parent) then fail ("path escapes workspace: " ^ relative);
  let path = Filename.concat parent (Filename.basename path) in
  (try
     let canonical = Unix.realpath path in
     if not (within root canonical) then fail ("path escapes workspace: " ^ relative)
   with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  path

let regular_path root relative =
  let path = checked_path root relative in
  if (Unix.stat path).Unix.st_kind <> Unix.S_REG then
    fail ("not a regular file: " ^ relative);
  path

let writable_path root relative =
  if relative = "." || Filename.basename relative = "." then fail "a file path is required";
  let path = checked_path root relative in
  (try
     let stat = Unix.lstat path in
     if stat.Unix.st_kind <> Unix.S_REG then
       fail ("not a regular file (or is a symlink): " ^ relative)
   with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  path

let with_fd path flags permissions fn =
  let fd = Unix.openfile path flags permissions in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> fn fd)

let read_bounded path limit =
  let size = (Unix.stat path).Unix.st_size in
  if size > limit then fail (Printf.sprintf "file exceeds %d-byte limit: %s" limit path);
  with_fd path [Unix.O_RDONLY] 0 (fun fd ->
    let buffer = Bytes.create 8192 in
    let result = Buffer.create (min size limit) in
    let rec loop () =
      let count = Unix.read fd buffer 0 (min 8192 (limit + 1 - Buffer.length result)) in
      if count <> 0 then (
        Buffer.add_subbytes result buffer 0 count;
        if Buffer.length result > limit then
          fail (Printf.sprintf "file exceeds %d-byte limit: %s" limit path);
        loop ())
    in
    loop ();
    Buffer.contents result)

let write_all fd text =
  let bytes = Bytes.unsafe_of_string text in
  let rec loop offset =
    if offset < Bytes.length bytes then (
      let written = Unix.write fd bytes offset (Bytes.length bytes - offset) in
      if written = 0 then fail "could not write file";
      loop (offset + written))
  in
  loop 0

let atomic_write path text =
  if String.length text > max_write_bytes then
    fail (Printf.sprintf "content exceeds %d-byte write limit" max_write_bytes);
  let mode = try (Unix.stat path).Unix.st_perm with Unix.Unix_error (Unix.ENOENT, _, _) -> 0o600 in
  let temp = Filename.temp_file ~temp_dir:(Filename.dirname path) ".pave-" ".tmp" in
  Fun.protect ~finally:(fun () -> try Unix.unlink temp with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
    (fun () ->
       with_fd temp [Unix.O_WRONLY] 0 (fun fd ->
         Unix.fchmod fd mode;
         write_all fd text;
         Unix.fsync fd);
       Unix.rename temp path)
