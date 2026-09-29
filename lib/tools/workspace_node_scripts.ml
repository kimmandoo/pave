exception Error of string

let fail message =
  raise (Error (message ^ "; scripts execute arbitrary project code; review before approval"))

let unique_object label = function
  | `Assoc fields ->
      let seen = Hashtbl.create (List.length fields) in
      List.iter (fun (key, _) ->
        if Hashtbl.mem seen key then fail ("duplicate " ^ label ^ " key: " ^ key);
        Hashtbl.add seen key ()) fields;
      fields
  | _ -> fail (label ^ " must be a JSON object")

let optional_object label fields =
  match List.assoc_opt label fields with
  | None -> []
  | Some value -> unique_object label value

let regular_file path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_REG -> true
    | _ -> fail ("not a regular file (or is a symlink): " ^ path)
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false

let selected_directory root subroot =
  let root = Workspace_path.root_path root in
  if subroot = "" || subroot = "." then root
  else (
    if not (Filename.is_relative subroot) ||
       String.contains subroot '\000' then fail "invalid project root";
    let parts = String.split_on_char '/' subroot in
    if List.exists (fun part -> part = "" || part = "." || part = "..") parts
    then fail "path must select an exact project root";
    List.fold_left (fun directory part ->
      let path = Filename.concat directory part in
      if (Unix.lstat path).Unix.st_kind <> Unix.S_DIR then
        fail "project root contains a symlink or non-directory";
      path) root parts)

let command ~root ~subroot ~action ~manager =
  try
    if action <> "test" && action <> "lint" then fail "unsupported node script action";
    if manager <> "" && manager <> "npm" && manager <> "yarn" && manager <> "pnpm" then
      fail "unsupported package manager";
    let directory = selected_directory root subroot in
    let manifest = Filename.concat directory "package.json" in
    if not (regular_file manifest) then fail "package.json is missing";
    let text = Workspace_path.read_bounded manifest Workspace_path.max_write_bytes in
    let json = try Yojson.Basic.from_string text
      with Yojson.Json_error _ -> fail "invalid package.json JSON" in
    let fields = unique_object "package.json" json in
    let has_dependency section =
      let entries = optional_object section fields in
      List.exists (fun name -> match List.assoc_opt name entries with
        | Some (`String _) -> true
        | _ -> false) ["react-native"; "expo"] in
    let dependencies = has_dependency "dependencies" in
    let dev_dependencies = has_dependency "devDependencies" in
    if not (dependencies || dev_dependencies) then
      fail "package.json must declare react-native or expo as a string dependency";
    let scripts = optional_object "scripts" fields in
    (match List.assoc_opt action scripts with
     | Some (`String _) -> ()
     | _ -> fail ("undeclared or non-string script: " ^ action));
    let cwd = directory in
    let locks = ["npm", "package-lock.json"; "npm", "npm-shrinkwrap.json";
                 "yarn", "yarn.lock"; "pnpm", "pnpm-lock.yaml"] in
    let choices = List.filter_map (fun (kind, name) ->
      let path = Filename.concat directory name in
      if regular_file path then Some kind else None) locks
      |> List.sort_uniq String.compare in
    let selected = match manager, choices with
      | "", [only] -> only
      | "", [] -> fail "no package-manager lockfile beside package.json"
      | "", _ -> fail "conflicting lockfiles: choose a package manager explicitly"
      | explicit, _ when List.mem explicit choices -> explicit
      | _ -> fail ("no matching lockfile for package manager: " ^ manager) in
    ((if selected = "npm" then "npm run " else selected ^ " ") ^ action, cwd)
  with
  | Workspace_path.Error message -> fail message
  | Unix.Unix_error (error, operation, path) ->
      fail (Printf.sprintf "%s: %s (%s)" operation (Unix.error_message error) path)
