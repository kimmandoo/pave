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

let version_number tag =
  if String.length tag < 6 || String.length tag > 48 || tag.[0] <> 'v' then
    fail "invalid release version";
  match String.split_on_char '.'
    (String.sub tag 1 (String.length tag - 1)) with
  | [ major; minor; patch ] ->
      let number part =
        if part = "" || not (String.for_all (function
          | '0'..'9' -> true | _ -> false) part) then None
        else int_of_string_opt part in
      (match number major, number minor, number patch with
       | Some major, Some minor, Some patch -> major, minor, patch
       | _ -> fail "invalid release version")
  | _ -> fail "invalid release version"

let latest_version () =
  let path, output = Filename.open_temp_file ~mode:[ Open_binary ]
    "pave-release-" ".json" in
  Fun.protect ~finally:(fun () ->
    close_out_noerr output;
    try Sys.remove path with Sys_error _ -> ()) (fun () ->
    close_out output;
    let arguments = [| "curl"; "--fail"; "--silent"; "--show-error";
      "--location"; "--proto"; "=https"; "--proto-redir"; "=https";
      "--connect-timeout"; "5"; "--max-time"; "15";
      "--max-filesize"; "65536";
      "--header"; "Accept: application/vnd.github+json";
      "--header"; "User-Agent: pave-updater";
      "--output"; path;
      "https://api.github.com/repos/kimmandoo/pave/releases/latest" |] in
    let pid = Unix.create_process "curl" arguments
      Unix.stdin Unix.stdout Unix.stderr in
    (match wait_for pid with
     | Unix.WEXITED 0 -> ()
     | _ -> fail "cannot check latest release (network, GitHub error or rate limit)");
    let input = open_in_bin path in
    let json = Fun.protect ~finally:(fun () -> close_in input) (fun () ->
      let size = in_channel_length input in
      if size > 65_536 then fail "release metadata exceeds 64 KiB";
      try Yojson.Basic.from_string (really_input_string input size)
      with Yojson.Json_error _ -> fail "release metadata is not valid JSON") in
    match Pave.Protocol.member "tag_name" json with
    | `String tag -> tag
    | _ -> fail "release metadata has no tag_name")

let check () =
  ignore (native_install_dir ());
  let installed = Embedded_installer.version in
  if installed = "source" then
    fail "this build has no published release version; install an official native release";
  let installed_number = version_number installed in
  let latest = latest_version () in
  let latest_number = version_number latest in
  let order = Stdlib.compare installed_number latest_number in
  if order = 0 then
    Printf.printf "Pave %s is up to date.\n" installed
  else if order > 0 then
    Printf.printf "Pave %s is newer than the latest published release %s.\n"
      installed latest
  else
    Printf.printf "Pave %s → %s available; run pave update.\n" installed latest
