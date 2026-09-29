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

let command ~root ~subroot ~action ~target =
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
    | _ -> fail "unsupported Flutter focused action"
  with
  | Workspace_path.Error message -> fail message
  | Unix.Unix_error (_, _, _) -> fail "Flutter package or test target is inaccessible"
  | Sys_error _ -> fail "Flutter pubspec.yaml is unreadable"
