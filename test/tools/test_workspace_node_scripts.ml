module Scripts = Pave.Workspace_node_scripts

let expect label condition = if not condition then failwith label

let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec search i =
    i + m <= n && (String.sub text i m = fragment || search (i + 1)) in
  search 0

let expect_error label fragment fn =
  match fn () with
  | _ -> failwith (label ^ ": expected an error")
  | exception Scripts.Error message ->
      expect (label ^ ": " ^ message) (contains message fragment);
      expect (label ^ ": missing arbitrary-code warning")
        (contains message "scripts execute arbitrary project code")

let write path text =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output text)

let rec remove_tree path =
  try match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path |> Array.iter (fun name ->
          remove_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Unix.unlink path
  with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let () =
  let root = Filename.temp_file "pave-node-scripts-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    let app = Filename.concat root "mobile app" in
    Unix.mkdir app 0o700;
    let package = Filename.concat app "package.json" in
    let npm = Filename.concat app "package-lock.json" in
    let yarn = Filename.concat app "yarn.lock" in
    let pnpm = Filename.concat app "pnpm-lock.yaml" in
    let shrinkwrap = Filename.concat app "npm-shrinkwrap.json" in
    let marker = Filename.concat app "should-not-exist" in
    let valid = "{\"dependencies\":{\"react-native\":\"0.72.0\"}," ^
      "\"scripts\":{\"test\":\"touch should-not-exist\",\"lint\":\"echo ok\"}}" in
    let command ?(manager = "") ?(action = "test") ?(subroot = "mobile app") () =
      Scripts.command ~root ~subroot ~action ~manager in
    write package valid;
    write npm "{}";
    expect "npm command and manifest cwd"
      (command () = ("npm run test", app));
    expect "script is never executed" (not (Sys.file_exists marker));
    expect "lint command" (command ~action:"lint" () = ("npm run lint", app));
    expect_error "unsupported action" "unsupported" (fun () ->
      command ~action:"build" ());
    expect_error "unknown manager" "unsupported" (fun () ->
      command ~manager:"npx" ());
    write yarn "";
    expect_error "ambiguous locks" "conflicting lockfiles" command;
    expect "explicit yarn under conflict" (command ~manager:"yarn" () = ("yarn test", app));
    expect "explicit npm under conflict" (command ~manager:"npm" () = ("npm run test", app));
    expect_error "absent pnpm lock" "matching lockfile" (fun () ->
      command ~manager:"pnpm" ());
    write pnpm "";
    expect "explicit pnpm under conflict" (command ~manager:"pnpm" () = ("pnpm test", app));
    Unix.unlink npm;
    Unix.unlink yarn;
    expect "pnpm inferred" (command () = ("pnpm test", app));
    Unix.unlink pnpm;
    write shrinkwrap "{}";
    expect "npm shrinkwrap inferred" (command () = ("npm run test", app));
    Unix.unlink shrinkwrap;
    expect_error "no lock" "no package-manager lockfile" command;
    write npm "{}";
    write package "{\"dependencies\":{\"expo\":\"~50\"},\"scripts\":{\"lint\":\"eslint .\"}}";
    expect "Expo lint script" (command ~action:"lint" () = ("npm run lint", app));
    write package "{\"devDependencies\":{\"expo\":\"~50\"},\"scripts\":{\"test\":\"echo ok\"}}";
    expect "Expo devDependency" (command () = ("npm run test", app));
    expect_error "undeclared script" "undeclared" (fun () ->
      command ~action:"lint" ());
    write package "{\"devDependencies\":{\"expo\":\"~50\"},\"scripts\":{\"test\":42}}";
    expect_error "non-string script" "non-string" command;
    write package "{\"dependencies\":{\"expo\":42},\"scripts\":{\"test\":\"ok\"}}";
    expect_error "non-string dependency" "string dependency" command;
    write package "{\"dependencies\":{\"expo\":\"1\",\"expo\":\"2\"},\"scripts\":{\"test\":\"ok\"}}";
    expect_error "duplicate dependency" "duplicate" command;
    write package "{\"dependencies\":{\"expo\":\"1\"},\"scripts\":{\"test\":\"ok\",\"test\":\"bad\"}}";
    expect_error "duplicate script" "duplicate" command;
    write package ("{\"dependencies\":{\"expo\":\"1\"},\"scripts\":{\"test\":\"ok\"},\"padding\":\"" ^
      String.make Pave.Workspace_path.max_write_bytes 'x' ^ "\"}");
    expect_error "oversized package" "exceeds" command;
    write package valid;
    Unix.unlink npm;
    Unix.symlink package npm;
    expect_error "symlinked lock" "symlink" command;
    Unix.unlink npm;
    Unix.unlink package;
    Unix.symlink (Filename.concat root "outside.json") package;
    expect_error "symlinked manifest" "symlink" command;
    expect_error "escaping subroot" "path" (fun () ->
      command ~subroot:"../outside" ());
    expect "script did not run" (not (Sys.file_exists marker)))
