exception Error of string
let fail text = raise (Error text)

let selected_directory root subroot =
  let root = Workspace_path.root_path root in
  if subroot = "" || subroot = "." then root
  else (
    if not (Filename.is_relative subroot) ||
       String.contains subroot '\000' then fail "invalid Swift package root";
    let parts = String.split_on_char '/' subroot in
    if List.exists (fun part -> part = "" || part = "." || part = "..") parts
    then fail "select an exact Swift package root";
    List.fold_left (fun directory part ->
      let path = Filename.concat directory part in
      if (Unix.lstat path).Unix.st_kind <> Unix.S_DIR then
        fail "Swift package root contains a symlink or non-directory";
      path) root parts)

let command ~root ~subroot ~action ~target =
  let subroot = if subroot = "" then "." else subroot in
  let cwd = selected_directory root subroot in
  let package = Filename.concat cwd "Package.swift" in
  let stat = Unix.lstat package in
  if stat.Unix.st_kind <> Unix.S_REG ||
     stat.Unix.st_size > Workspace_path.max_write_bytes then
    fail "selected Swift package has no bounded regular Package.swift";
  let contents = Workspace_path.read_bounded package Workspace_path.max_write_bytes in
  if String.contains contents '\000' then fail "invalid Swift package manifest";
  (* SwiftPM may resolve and download package dependencies even while listing tests. *)
  let contains needle =
    let rec has i = i + String.length needle <= String.length contents &&
      (String.sub contents i (String.length needle) = needle || has (i + 1)) in
    has 0 in
  if contains ".package(" || contains ".package (" then
    fail "Swift package dependencies need an independent approved fetch; discovery cannot install them";
  if contains "dependencies:" then (
    let needle = "dependencies:" in
    let rec find i =
      if i + String.length needle > String.length contents then assert false
      else if String.sub contents i (String.length needle) = needle then i
      else find (i + 1) in
    let offset = find 0 + String.length needle in
    let rec skip i =
      if i < String.length contents &&
        (contents.[i] = ' ' || contents.[i] = '\n')
      then skip (i + 1) else i in
    let offset = skip offset in
    if offset + 2 > String.length contents ||
       String.sub contents offset 2 <> "[]" then
      fail "Swift package dependencies must be statically empty for offline test discovery");
  if (Unix.lstat cwd).Unix.st_kind <> Unix.S_DIR then
    fail "selected Swift package root is not a directory";
  match action with
  | "discover" when target = "" ->
      "swift test --disable-automatic-resolution list", cwd
  | "run" when target <> "" && String.length target <= 256 &&
      String.for_all (fun char -> Char.code char >= 33 &&
        Char.code char < 127 && char <> '\'' && char <> '`' &&
        char <> '$' && char <> '\\') target ->
      "swift test --disable-automatic-resolution --filter " ^
      Filename.quote target, cwd
  | "discover" -> fail "test filter is not accepted during discovery"
  | "run" -> fail "select an exact discovered Swift test filter"
  | _ -> fail "SwiftPM action must be discover or run"

let tests output =
  let rows = String.split_on_char '\n' output |> List.filter_map (fun row ->
    let row = String.trim row in
    if row <> "" && String.length row <= 256 &&
       String.contains row '.' &&
       not (String.contains row ' ') &&
       String.for_all (fun char ->
         (char >= 'A' && char <= 'Z') ||
         (char >= 'a' && char <= 'z') ||
         (char >= '0' && char <= '9') ||
         List.mem char ['.'; '_'; '/'; '('; ')'; '-']) row then Some row
    else None) in
  let rows = List.sort_uniq String.compare rows in
  if rows = [] || List.length rows > 500 then
    fail "SwiftPM did not return a bounded concrete test list";
  rows

let no_tests output =
  let contains fragment =
    let rec find i =
      i + String.length fragment <= String.length output &&
      (String.sub output i (String.length fragment) = fragment ||
       find (i + 1)) in
    find 0 in
  List.exists contains ["Executed 0 tests"; "0 tests passed";
    "No matching test cases were run"; "No matching tests were run"]
