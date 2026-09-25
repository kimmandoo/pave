type status = Complete | Skipped

type loaded = { status : status option; diagnostics : string list }

let path () =
  let home, diagnostics = Settings.config_home () in
  if diagnostics <> [] || Filename.is_relative home then
    invalid_arg "XDG_CONFIG_HOME must be absolute to load setup status";
  Filename.concat (Filename.concat home "pave") "setup.json"

let parse text =
  match Yojson.Basic.from_string text with
  | `Assoc [ ("version", `Int 1); ("status", `String "complete") ]
  | `Assoc [ ("status", `String "complete"); ("version", `Int 1) ] -> Complete
  | `Assoc [ ("version", `Int 1); ("status", `String "skipped") ]
  | `Assoc [ ("status", `String "skipped"); ("version", `Int 1) ] -> Skipped
  | _ -> invalid_arg "unsupported setup status (expected version 1)"

let load () =
  try
    let path = path () in
    let before = Unix.lstat path in
    if before.Unix.st_kind <> Unix.S_REG then
      invalid_arg "setup status must be a regular file, not a symlink";
    let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_NONBLOCK] 0 in
    let input = Unix.in_channel_of_descr fd in
    let status = Fun.protect ~finally:(fun () -> close_in input) (fun () ->
      let current = Unix.fstat fd and after = Unix.lstat path in
      if current.Unix.st_kind <> Unix.S_REG || current.Unix.st_size > 1024 ||
         not (Settings.same_file before current &&
           Settings.same_file current after) then
        invalid_arg "setup status changed or exceeds 1 KiB";
      parse (really_input_string input current.Unix.st_size)) in
    { status = Some status; diagnostics = [] }
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      { status = None; diagnostics = [] }
  | (Unix.Unix_error _ | Sys_error _ | Invalid_argument _ | Yojson.Json_error _) as exn ->
      { status = None; diagnostics = [Printexc.to_string exn] }

let mark status =
  let directory = Settings.user_directory () in
  let path = Filename.concat directory "setup.json" in
  (try
     let previous = Unix.lstat path in
     if previous.Unix.st_kind <> Unix.S_REG ||
        previous.Unix.st_uid <> Unix.geteuid () then
       invalid_arg "setup status must be an owned regular file"
   with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  let value = match status with Complete -> "complete" | Skipped -> "skipped" in
  let text = Yojson.Basic.to_string (`Assoc [
    "version", `Int 1; "status", `String value ]) ^ "\n" in
  let temp, output = Filename.open_temp_file ~mode:[Open_binary]
    ~temp_dir:directory "setup-" ".tmp" in
  Fun.protect ~finally:(fun () ->
    close_out_noerr output;
    try Sys.remove temp with Sys_error _ -> ()) (fun () ->
    Unix.chmod temp 0o600;
    output_string output text;
    flush output;
    Unix.fsync (Unix.descr_of_out_channel output);
    close_out output;
    Unix.rename temp path;
    let directory_fd = Unix.openfile directory [Unix.O_RDONLY] 0 in
    Fun.protect ~finally:(fun () -> Unix.close directory_fd) (fun () ->
      Unix.fsync directory_fd))
