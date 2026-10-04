exception Error of string

let fail message = raise (Error message)

let shell_quote text =
  "'" ^ String.concat "'\\''" (String.split_on_char '\'' text) ^ "'"

(* Keep path spellings literal: canonicalization alone would accept symlinks within
   the workspace and aliases for a different selected package. *)
let components relative =
  if relative = "" || not (Filename.is_relative relative) ||
     String.contains relative '\000' then
    fail "expected a workspace-relative path";
  let parts = String.split_on_char '/' relative in
  if List.exists (fun part -> part = "" || part = "." || part = "..") parts then
    fail "expected an exact workspace-relative path";
  parts

let checked_components root parts =
  let rec walk parent = function
    | [] -> parent
    | part :: rest ->
        let path = Filename.concat parent part in
        let stat = Unix.lstat path in
        if stat.Unix.st_kind = Unix.S_LNK then fail "symlink traversal is not allowed";
        if rest <> [] && stat.Unix.st_kind <> Unix.S_DIR then
          fail "path component is not a directory";
        walk path rest
  in
  let relative = String.concat "/" parts in
  let path = Workspace_path.checked_path root relative in
  let literal = walk root parts in
  if literal <> path then fail "path does not name the selected workspace entry";
  path

let flutter_dependency pubspec =
  let section = ref "" and subsection = ref "" in
  let dependencies = ref 0 and flutter = ref 0 and sdk = ref 0 in
  let found = ref false and invalid_indentation = ref false in
  String.split_on_char '\n' pubspec |> List.iter (fun line ->
    let trimmed = String.trim line in
    if trimmed <> "" && trimmed.[0] <> '#' then (
      let rec spaces n =
        if n < String.length line && line.[n] = ' ' then spaces (n + 1)
        else n in
      let indent = spaces 0 in
      if String.contains line '\t' then invalid_indentation := true;
      if indent = 0 then (
        section := trimmed; subsection := "";
        if trimmed = "dependencies:" then incr dependencies)
      else if indent = 2 then (
        subsection := trimmed;
        if !section = "dependencies:" &&
           String.starts_with ~prefix:"flutter:" trimmed then
          incr flutter)
      else if indent = 4 && !section = "dependencies:" &&
              !subsection = "flutter:" &&
              String.starts_with ~prefix:"sdk:" trimmed then (
        incr sdk;
        if trimmed = "sdk: flutter" then found := true)));
  !found && !dependencies = 1 && !flutter = 1 && !sdk = 1 &&
  not !invalid_indentation

let integration_test_dependency pubspec =
  let section = ref "" and subsection = ref "" in
  let dependencies = ref 0 and integration_test = ref 0 and sdk = ref 0 in
  let found = ref false and invalid_indentation = ref false in
  String.split_on_char '\n' pubspec |> List.iter (fun line ->
    let trimmed = String.trim line in
    if trimmed <> "" && trimmed.[0] <> '#' then (
      let rec spaces n =
        if n < String.length line && line.[n] = ' ' then spaces (n + 1)
        else n in
      let indent = spaces 0 in
      if String.contains line '\t' then invalid_indentation := true;
      if indent = 0 then (
        section := trimmed; subsection := "";
        if trimmed = "dev_dependencies:" then incr dependencies)
      else if indent = 2 then (
        subsection := trimmed;
        if !section = "dev_dependencies:" &&
           String.starts_with ~prefix:"integration_test:" trimmed then
          incr integration_test)
      else if indent = 4 && !section = "dev_dependencies:" &&
              !subsection = "integration_test:" &&
              String.starts_with ~prefix:"sdk:" trimmed then (
        incr sdk;
        if trimmed = "sdk: flutter" then found := true)));
  !found && !dependencies = 1 && !integration_test = 1 && !sdk = 1 &&
  not !invalid_indentation

let flutter_runtime_available () =
  match Sys.getenv_opt "PATH" with
  | None -> false
  | Some path ->
      String.split_on_char ':' path
      |> List.exists (fun directory ->
        let directory = if directory = "" then "." else directory in
        let executable = Filename.concat directory "flutter" in
        try
          let stat = Unix.stat executable in
          stat.Unix.st_kind = Unix.S_REG &&
          (Unix.access executable [Unix.X_OK]; true)
        with Unix.Unix_error _ -> false)

type integration_target = {
  package_root : string;
  path : string;
  relative_path : string;
}

type integration_discovery = {
  root : string;
  subroot : string;
  package_root : string;
  targets : integration_target list;
}

let max_integration_entries = 2048
let max_integration_depth = 16

let discover_integration_tests ~root ~subroot =
  try
    let root = Workspace_path.root_path root in
    let package_parts = if subroot = "" || subroot = "." then []
      else components subroot in
    let package_root = if package_parts = [] then root
      else checked_components root package_parts in
    if (Unix.lstat package_root).Unix.st_kind <> Unix.S_DIR then
      fail "selected Flutter package is not a directory";
    let manifest = checked_components root (package_parts @ ["pubspec.yaml"]) in
    let stat = Unix.lstat manifest in
    if stat.Unix.st_kind <> Unix.S_REG then fail "pubspec.yaml is not a regular file";
    if stat.Unix.st_size > Workspace_path.max_write_bytes then
      fail "pubspec.yaml exceeds the read limit";
    let pubspec = Workspace_path.read_bounded manifest Workspace_path.max_write_bytes in
    if not (flutter_dependency pubspec) then
      fail "selected pubspec.yaml does not declare a Flutter SDK dependency";
    if not (integration_test_dependency pubspec) then
      fail "selected pubspec.yaml does not declare integration_test from the Flutter SDK";
    let directory_parts = package_parts @ ["integration_test"] in
    let directory = checked_components root directory_parts in
    if (Unix.lstat directory).Unix.st_kind <> Unix.S_DIR then
      fail "selected Flutter package has no integration_test/ directory";
    let entries = ref 0 in
    let rec walk parts depth =
      if depth > max_integration_depth then
        fail "Flutter integration test discovery exceeds its depth limit";
      let path = checked_components root parts in
      let directory_handle = Unix.opendir path in
      let names = Fun.protect
        ~finally:(fun () -> Unix.closedir directory_handle)
        (fun () ->
          let rec collect names =
            match Unix.readdir directory_handle with
            | exception End_of_file -> List.sort String.compare names
            | "." | ".." -> collect names
            | name ->
                incr entries;
                if !entries > max_integration_entries then
                  fail "Flutter integration test discovery exceeds its entry limit";
                collect (name :: names)
          in
          collect []) in
      List.concat_map (fun name ->
        if name = "" || name = "." || name = ".." ||
           String.contains name '/' || String.contains name '\000' then
          fail "Flutter integration test discovery found an invalid entry";
        let child_parts = parts @ [name] in
        let child = Filename.concat path name in
        match (Unix.lstat child).Unix.st_kind with
        | Unix.S_DIR -> walk child_parts (depth + 1)
        | Unix.S_REG when Filename.check_suffix name ".dart" ->
            let full_path = String.concat "/" child_parts in
            let relative_path =
              String.concat "/" (List.filteri (fun i _ -> i >= List.length package_parts)
                child_parts) in
            [{ package_root; path = full_path; relative_path }]
        | Unix.S_REG -> []
        | Unix.S_LNK -> []
        | _ -> []) names
    in
    let targets = walk directory_parts 0 |> List.sort (fun a b ->
      String.compare a.path b.path) in
    if targets = [] then fail "selected Flutter package has no integration test targets";
    { root; subroot; package_root; targets }
  with
  | Workspace_path.Error message -> fail message
  | Unix.Unix_error (_, _, _) -> fail "Flutter package or integration_test directory is inaccessible"
  | Sys_error _ -> fail "Flutter pubspec.yaml or integration_test directory is unreadable"

let discovered_target discovery target =
  List.find_opt (fun discovered -> discovered.path = target) discovery.targets


let validate_ready_emulator serial =
  if not (Workspace_android_devices.emulator_serial serial) then
    fail "Flutter integration tests require an exact ready emulator serial"

let command ?ready_device_id ~root ~subroot ~action ~target () =
  try
    let root = Workspace_path.root_path root in
    let package_parts = if subroot = "" || subroot = "." then []
      else components subroot in
    let cwd = if package_parts = [] then root
      else checked_components root package_parts in
    if (Unix.lstat cwd).Unix.st_kind <> Unix.S_DIR then
      fail "selected Flutter package is not a directory";
    let manifest_parts = package_parts @ ["pubspec.yaml"] in
    let manifest = checked_components root manifest_parts in
    let stat = Unix.lstat manifest in
    if stat.Unix.st_kind <> Unix.S_REG then fail "pubspec.yaml is not a regular file";
    if stat.Unix.st_size > Workspace_path.max_write_bytes then
      fail "pubspec.yaml exceeds the read limit";
    let pubspec = Workspace_path.read_bounded manifest Workspace_path.max_write_bytes in
    if not (flutter_dependency pubspec) then
      fail "selected pubspec.yaml does not declare a Flutter SDK dependency";
    match action with
    | "analyze" ->
        if target <> "" then fail "Flutter analyze does not accept a target";
        "flutter analyze --no-pub", cwd
    | "test" ->
        let target_parts = components target in
        let rec drop_prefix prefix rest = match prefix, rest with
          | [], remaining -> remaining
          | p :: ps, q :: qs when p = q -> drop_prefix ps qs
          | _ -> fail "test target is outside the selected Flutter package" in
        let relative_parts = drop_prefix package_parts target_parts in
        (match relative_parts with
         | "test" :: (_ :: _ as path) ->
             let name = List.hd (List.rev path) in
             if not (Filename.check_suffix name ".dart") then
               fail "Flutter test target must be a .dart file"
         | _ -> fail "Flutter test target must be under the package test/ directory");
        let file = checked_components root target_parts in
        if (Unix.lstat file).Unix.st_kind <> Unix.S_REG then
          fail "Flutter test target is not a regular file";
        "flutter test --no-pub " ^
          shell_quote (String.concat "/" relative_parts), cwd
    | "integration_test" ->
        let serial = match ready_device_id with
          | Some serial -> serial
          | None -> fail "Flutter integration tests require a ready device session" in
        validate_ready_emulator serial;
        if not (flutter_runtime_available ()) then
          fail "Flutter runtime is unavailable";
        if not (integration_test_dependency pubspec) then
          fail "selected pubspec.yaml does not declare integration_test from the Flutter SDK";
        let discovery = discover_integration_tests ~root ~subroot in
        if discovered_target discovery target = None then
          fail "Flutter integration test target was not discovered for this package";
        let target_parts = components target in
        let rec drop_prefix prefix rest = match prefix, rest with
          | [], remaining -> remaining
          | p :: ps, q :: qs when p = q -> drop_prefix ps qs
          | _ -> fail "integration test target is outside the selected Flutter package" in
        let relative_parts = drop_prefix package_parts target_parts in
        (match relative_parts with
         | "integration_test" :: (_ :: _ as path) ->
             let name = List.hd (List.rev path) in
             if not (Filename.check_suffix name ".dart") then
               fail "Flutter integration test target must be a .dart file"
         | _ -> fail "Flutter integration test target must be under the package integration_test/ directory");
        let file = checked_components root target_parts in
        if (Unix.lstat file).Unix.st_kind <> Unix.S_REG then
          fail "Flutter integration test target is not a regular file";
        "flutter test --no-pub " ^
          shell_quote (String.concat "/" relative_parts) ^
          " -d " ^ shell_quote serial, cwd
    | _ -> fail "unsupported Flutter focused action"
  with
  | Workspace_path.Error message -> fail message
  | Unix.Unix_error (_, _, _) -> fail "Flutter package or test target is inaccessible"
  | Sys_error _ -> fail "Flutter pubspec.yaml is unreadable"
