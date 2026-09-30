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

let uninstall () =
  let directory = native_install_dir () in
  let license_dir = Filename.concat
    (Filename.dirname directory) "share/licenses/pave" in
  let binary = Filename.concat directory "pave" in
  let files = [ binary; Filename.concat license_dir "LICENSE";
    Filename.concat license_dir "THIRD_PARTY_NOTICES";
    Filename.concat license_dir ".native-install" ] in
  if not (List.for_all is_regular files) then
    fail "installation is incomplete or contains non-regular files; refusing to remove it";
  List.iter Sys.remove files;
  (try Unix.rmdir license_dir with Unix.Unix_error (Unix.ENOTEMPTY, _, _) -> ());
  Printf.printf "Removed Pave from %s\nSaved settings, sessions and credentials were kept.\n%!"
    directory


let rec wait_for pid =
  try snd (Unix.waitpid [] pid) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> wait_for pid

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

(* Progress: while a child process runs, a small cat paves a road on one
   terminal line. Plain terminals get one line per phase instead. *)
let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec scan i = i + m <= n && (String.sub text i m = fragment || scan (i + 1)) in
  scan 0

let fancy_output () =
  Unix.isatty Unix.stdout &&
  match Sys.getenv_opt "TERM" with
  | None | Some ("" | "dumb") -> false
  | Some _ -> true

let utf8_locale () =
  let rec first = function
    | [] -> ""
    | name :: rest ->
        (match Sys.getenv_opt name with
         | Some value when value <> "" -> String.lowercase_ascii value
         | _ -> first rest) in
  let locale = first [ "LC_ALL"; "LC_CTYPE"; "LANG" ] in
  contains locale "utf-8" || contains locale "utf8"

let road_width = 16

let frame ~utf8 ~color tick label =
  let step = tick mod (road_width + 4) in
  let position = min step road_width in
  let face =
    if step > road_width then "(=^w^=)"
    else if tick mod 16 = 15 then "(=-.-=)"
    else if tick mod 2 = 0 then "(=^.^=)" else "(=^o^=)" in
  let repeat text count = String.concat "" (List.init count (fun _ -> text)) in
  let paved = repeat (if utf8 then "\u{25B0}" else "=") position
  and rest = repeat (if utf8 then "\u{25B1}" else "-") (road_width - position)
  and dots = String.make (1 + tick / 3 mod 3) '.' in
  let paint code text = if color then "\027[" ^ code ^ "m" ^ text ^ "\027[0m" else text in
  "  " ^ paint "36" paved ^ paint "1;33" face ^ paint "2" rest ^ "  " ^ label ^ dots

let phase_prefix = "pave-phase: "

(* Runs [spawn output] with the child's stdout and stderr on a pipe, animating
   until the pipe closes. Phase lines update the label; other lines are
   returned for the caller. *)
let supervise ~label spawn =
  let read_fd, write_fd = Unix.pipe ~cloexec:true () in
  let pid =
    try spawn write_fd
    with exn -> Unix.close read_fd; Unix.close write_fd; raise exn in
  Unix.close write_fd;
  let fancy = fancy_output () in
  let utf8 = utf8_locale () and color = Sys.getenv_opt "NO_COLOR" = None in
  let label = ref label and lines = ref [] and kept = ref 0 in
  let pending = Buffer.create 256 and chunk = Bytes.create 4096 in
  let announce () = if not fancy then Printf.printf "  %s...\n%!" !label in
  let accept line =
    if String.starts_with ~prefix:phase_prefix line then (
      label := String.sub line (String.length phase_prefix)
        (String.length line - String.length phase_prefix);
      announce ())
    else if !kept < 65_536 then (
      kept := !kept + String.length line;
      lines := line :: !lines) in
  let split () =
    let text = Buffer.contents pending in
    let parts = String.split_on_char '\n' text in
    let rec feed = function
      | [] -> ()
      | [ last ] -> Buffer.clear pending; Buffer.add_string pending last
      | line :: rest -> accept line; feed rest in
    feed parts in
  let started = Unix.gettimeofday () in
  let render () =
    if fancy then (
      let tick = int_of_float ((Unix.gettimeofday () -. started) /. 0.12) in
      print_string ("\r\027[2K" ^ frame ~utf8 ~color tick !label);
      flush stdout) in
  let previous = Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> raise Sys.Break)) in
  if fancy then (print_string "\027[?25l"; flush stdout) else announce ();
  let finish () =
    if fancy then (print_string "\r\027[2K\027[?25h"; flush stdout);
    Sys.set_signal Sys.sigint previous;
    Unix.close read_fd in
  Fun.protect ~finally:finish (fun () ->
    let rec loop () =
      render ();
      let ready =
        try let ready, _, _ = Unix.select [ read_fd ] [] [] 0.12 in ready <> []
        with Unix.Unix_error (Unix.EINTR, _, _) -> false in
      if not ready then loop ()
      else match Unix.read read_fd chunk 0 (Bytes.length chunk) with
        | 0 -> ()
        | count -> Buffer.add_subbytes pending chunk 0 count; split (); loop ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop () in
    (try loop () with Sys.Break ->
       (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
       ignore (wait_for pid);
       fail "interrupted");
    if Buffer.length pending > 0 then accept (Buffer.contents pending);
    let status = wait_for pid in
    status, List.rev !lines)

let print_lines lines = List.iter prerr_endline lines

let release_tag_prefix = "https://github.com/kimmandoo/pave/releases/tag/"
let redirect_prefix = "pave-redirect: "

(* The release web page redirects to the latest tag without touching the
   REST API and its 60-requests-per-hour anonymous limit. *)
let latest_from_redirect () =
  let status, lines = supervise ~label:"Checking the latest release" (fun output ->
    Unix.create_process "curl" [| "curl"; "--silent"; "--show-error";
      "--proto"; "=https"; "--connect-timeout"; "5"; "--max-time"; "15";
      "--max-filesize"; "1048576"; "--header"; "User-Agent: pave-updater";
      "--output"; "/dev/null";
      "--write-out"; redirect_prefix ^ "%{http_code} %{redirect_url}\\n";
      "https://github.com/kimmandoo/pave/releases/latest" |]
      Unix.stdin output output) in
  let redirect = List.find_map (fun line ->
    if String.starts_with ~prefix:redirect_prefix line then
      Some (String.sub line (String.length redirect_prefix)
        (String.length line - String.length redirect_prefix))
    else None) lines in
  match status, redirect with
  | Unix.WEXITED 0, Some reply ->
      (match String.index_opt reply ' ' with
       | Some space when String.sub reply 0 space = "302" ->
           let location = String.sub reply (space + 1) (String.length reply - space - 1) in
           if String.starts_with ~prefix:release_tag_prefix location then (
             let tag = String.sub location (String.length release_tag_prefix)
               (String.length location - String.length release_tag_prefix) in
             match version_number tag with
             | _ -> Some tag
             | exception Failure _ -> None)
           else None
       | _ -> None)
  | _ -> None

let latest_from_api () =
  let path, output = Filename.open_temp_file ~mode:[ Open_binary ]
    "pave-release-" ".json" in
  Fun.protect ~finally:(fun () ->
    close_out_noerr output;
    try Sys.remove path with Sys_error _ -> ()) (fun () ->
    close_out output;
    let status, lines = supervise ~label:"Asking the GitHub API" (fun output ->
      Unix.create_process "curl" [| "curl"; "--fail"; "--silent"; "--show-error";
        "--location"; "--proto"; "=https"; "--proto-redir"; "=https";
        "--connect-timeout"; "5"; "--max-time"; "15";
        "--max-filesize"; "65536";
        "--header"; "Accept: application/vnd.github+json";
        "--header"; "User-Agent: pave-updater";
        "--output"; path;
        "https://api.github.com/repos/kimmandoo/pave/releases/latest" |]
        Unix.stdin output output) in
    (match status with
     | Unix.WEXITED 0 -> ()
     | _ ->
         print_lines lines;
         fail (if List.exists (fun line -> contains line "403" || contains line "429") lines
           then "GitHub API rate limit reached and the release page was unreachable; retry later"
           else "cannot check latest release (network or GitHub error)"));
    let input = open_in_bin path in
    let json = Fun.protect ~finally:(fun () -> close_in input) (fun () ->
      let size = in_channel_length input in
      if size > 65_536 then fail "release metadata exceeds 64 KiB";
      try Yojson.Basic.from_string (really_input_string input size)
      with Yojson.Json_error _ -> fail "release metadata is not valid JSON") in
    match Pave.Protocol.member "tag_name" json with
    | `String tag -> tag
    | _ -> fail "release metadata has no tag_name")

let latest_version () =
  match latest_from_redirect () with
  | Some tag -> tag
  | None -> latest_from_api ()

let run () =
  let directory = native_install_dir () in
  (* The mutable /latest/download redirect can lag behind the release API.
     Pin both archive and checksum fetches to the same validated tag. *)
  let target = latest_version () in
  let installed = Embedded_installer.version in
  if installed = "source" then
    fail "this build has no published release version; install an official native release";
  if Stdlib.compare (version_number target) (version_number installed) < 0 then
    fail ("published latest " ^ target ^ " is older than installed " ^
      installed ^ "; refusing to downgrade");
  let script, output = Filename.open_temp_file ~mode:[ Open_binary ]
    "pave-update-" ".sh" in
  Fun.protect ~finally:(fun () ->
    close_out_noerr output;
    try Sys.remove script with Sys_error _ -> ()) (fun () ->
    output_string output Embedded_installer.script;
    close_out output;
    let keep entry =
      not (String.starts_with ~prefix:"PAVE_INSTALL_DIR=" entry ||
           String.starts_with ~prefix:"PAVE_VERSION=" entry ||
           String.starts_with ~prefix:"PAVE_UPDATE_OUTPUT=" entry) in
    let environment = Array.of_list
      (("PAVE_INSTALL_DIR=" ^ directory) :: ("PAVE_VERSION=" ^ target) ::
        "PAVE_UPDATE_OUTPUT=1" ::
        List.filter keep (Array.to_list (Unix.environment ()))) in
    let status, lines = supervise ~label:("Paving " ^ target) (fun output ->
      Unix.create_process_env "/bin/sh" [| "/bin/sh"; script |]
        environment Unix.stdin output output) in
    if status <> Unix.WEXITED 0 then print_lines lines;
    match status with
    | Unix.WEXITED 0 ->
        let arrow = if utf8_locale () then "\u{2192}" else "->" in
        Printf.printf "  %sPave %s\n  Path  %s/pave\n%!"
          (if fancy_output () then "(=^w^=)  " else "")
          (if target = installed then "reinstalled " ^ installed
           else "updated " ^ installed ^ " " ^ arrow ^ " " ^ target)
          directory
    | Unix.WEXITED code -> fail (Printf.sprintf "installer exited with status %d; inspect the installation before retrying" code)
    | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
        fail (Printf.sprintf "installer terminated with signal %d; check the installation before retrying" signal))


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
