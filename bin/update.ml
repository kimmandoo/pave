let fail message = failwith ("update: " ^ message)

let is_regular path =
  try (Unix.lstat path).Unix.st_kind = Unix.S_REG
  with Unix.Unix_error _ -> false

let native_install_dir () =
  let executable = Sys.executable_name in
  if Filename.is_relative executable || Filename.basename executable <> "pave" ||
     not (is_regular executable) then
    fail "only an installer-owned native pave binary can self-update; reinstall with install.sh or update your source package separately";
  let directory = Filename.dirname executable in
  let marker = Filename.concat
    (Filename.concat (Filename.dirname directory) "share/licenses/pave")
    ".native-install" in
  if not (is_regular marker) then
    fail "native-install marker missing; rerun install.sh once to enable self-update (opam installs must use opam)";
  let input = open_in_bin marker in
  let recognized = Fun.protect ~finally:(fun () -> close_in input) (fun () ->
    let size = in_channel_length input in
    size = String.length "pave-native-v1\n" &&
    really_input_string input size = "pave-native-v1\n") in
  if not recognized then fail "invalid native-install marker; refusing to overwrite this executable";
  directory

let rec wait_for pid =
  try snd (Unix.waitpid [] pid) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> wait_for pid

let run () =
  let directory = native_install_dir () in
  let script, output = Filename.open_temp_file ~mode:[ Open_binary ]
    "pave-update-" ".sh" in
  Fun.protect ~finally:(fun () ->
    close_out_noerr output;
    try Sys.remove script with Sys_error _ -> ()) (fun () ->
    output_string output Embedded_installer.script;
    close_out output;
    let keep entry =
      not (String.starts_with ~prefix:"PAVE_INSTALL_DIR=" entry ||
           String.starts_with ~prefix:"PAVE_VERSION=" entry) in
    let environment = Array.of_list
      (("PAVE_INSTALL_DIR=" ^ directory) ::
        List.filter keep (Array.to_list (Unix.environment ()))) in
    let pid = Unix.create_process_env "/bin/sh" [| "/bin/sh"; script |]
      environment Unix.stdin Unix.stdout Unix.stderr in
    match wait_for pid with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED code -> fail (Printf.sprintf "installer exited with status %d; inspect the installation before retrying" code)
    | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
        fail (Printf.sprintf "installer terminated with signal %d; check the installation before retrying" signal))
