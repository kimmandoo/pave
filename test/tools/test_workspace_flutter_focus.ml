module Flutter = Pave.Workspace_flutter_focus

let expect label condition =
  if not condition then failwith ("Flutter focused checks: " ^ label)

let rejects label fn =
  try ignore (fn ()); failwith ("Flutter focused checks accepted " ^ label)
  with Flutter.Error _ -> ()

let write path contents =
  Pave.Workspace_path.with_fd path
    [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600
    (fun fd -> Pave.Workspace_path.write_all fd contents)

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.iter (fun name ->
        remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let () =
  let root = Filename.temp_file "pave-flutter-focus-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  let root = Unix.realpath root in
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    let at path = Filename.concat root path in
    let mkdir path = Unix.mkdir (at path) 0o700 in
    let create path contents = write (at path) contents in
    let command ?(subroot="flutter") ?(target="") action =
      Flutter.command ~root ~subroot ~action ~target in
    mkdir "flutter";
    mkdir "flutter/test";
    mkdir "flutter/test/nested";
    mkdir "dart";
    mkdir "dart/test";
    mkdir "elsewhere";
    create "flutter/pubspec.yaml"
      "name: sample\ndependencies:\n  flutter:\n    sdk: flutter\n";
    create "flutter/test/one.dart" "void main() {}\n";
    create "flutter/test/nested/o'brien.dart" "void main() {}\n";
    create "flutter/lib.dart" "void main() {}\n";
    create "flutter/test/notes.txt" "not Dart\n";
    create "dart/pubspec.yaml"
      "name: sample\ndev_dependencies:\n  flutter:\n    sdk: flutter\n";
    create "dart/test/one.dart" "void main() {}\n";
    create "elsewhere/other.dart" "void main() {}\n";
    expect "analyze avoids implicit pub get"
      (command "analyze" = ("flutter analyze --no-pub", at "flutter"));
    expect "test target is relative to selected package and shell-quoted"
      (command ~target:"flutter/test/nested/o'brien.dart" "test" =
       ("flutter test --no-pub 'test/nested/o'\\''brien.dart'", at "flutter"));
    rejects "analyze target" (fun () -> command ~target:"flutter/test/one.dart" "analyze");
    rejects "unsupported action" (fun () -> command "pub get");
    rejects "Dart-only package" (fun () -> command ~subroot:"dart" "analyze");
    rejects "test outside selected package"
      (fun () -> command ~target:"dart/test/one.dart" "test");
    rejects "file outside test directory"
      (fun () -> command ~target:"flutter/lib.dart" "test");
    rejects "test directory itself"
      (fun () -> command ~target:"flutter/test" "test");
    rejects "non-Dart test file"
      (fun () -> command ~target:"flutter/test/notes.txt" "test");
    rejects "missing test file"
      (fun () -> command ~target:"flutter/test/missing.dart" "test");
    rejects "relative traversal"
      (fun () -> command ~target:"flutter/test/../../elsewhere/other.dart" "test");
    rejects "absolute target"
      (fun () -> command ~target:(at "flutter/test/one.dart") "test");
    rejects "subroot alias"
      (fun () -> command ~subroot:"flutter/." "analyze");
    Unix.symlink (at "flutter/test/one.dart") (at "flutter/test/link.dart");
    rejects "symlink test file"
      (fun () -> command ~target:"flutter/test/link.dart" "test");
    Unix.symlink (at "flutter/test") (at "flutter/alias");
    rejects "symlink test traversal"
      (fun () -> command ~target:"flutter/alias/one.dart" "test");
    Unix.symlink (at "flutter") (at "alias");
    rejects "symlink package"
      (fun () -> command ~subroot:"alias" "analyze");
    Unix.unlink (at "flutter/pubspec.yaml");
    Unix.symlink (at "dart/pubspec.yaml") (at "flutter/pubspec.yaml");
    rejects "symlink manifest"
      (fun () -> command "analyze");
    Unix.unlink (at "flutter/pubspec.yaml");
    let misleading = [
      "# dependencies:\n#   flutter:\n#     sdk: flutter\n";
      "description: flutter SDK\n";
      "dev_dependencies:\n  flutter:\n    sdk: flutter\n";
      "dependencies:\n  something:\n    sdk: flutter\n";
      "dependencies:\n  flutter: any\n    sdk: flutter\n";
      "dependencies:\n  flutter:\n      sdk: flutter\n";
      "dependencies:\n  flutter:\n\tsdk: flutter\n";
      "dependencies:\n  flutter:\n    sdk: flutter\n    sdk: dart\n";
      "dependencies:\n  flutter:\n    sdk: flutter\n  flutter: any\n";
      "dependencies:\n  flutter:\n    sdk: flutter\ndependencies:\n  other: any\n";
    ] in
    List.iteri (fun index pubspec ->
      create "flutter/pubspec.yaml" pubspec;
      rejects ("misleading pubspec " ^ string_of_int index)
        (fun () -> command "analyze")) misleading;
    create "flutter/pubspec.yaml"
      ("dependencies:\n  flutter:\n    sdk: flutter\n" ^
       String.make Pave.Workspace_path.max_write_bytes ' ');
    rejects "oversized pubspec" (fun () -> command "analyze");
    create "flutter/pubspec.yaml" "name: sample\ndependencies:\n  flutter:\n    sdk: flutter\n";
    create "pubspec.yaml" "dependencies:\n  flutter:\n    sdk: flutter\n";
    mkdir "test";
    create "test/root.dart" "void main() {}\n";
    expect "workspace-root Flutter package"
      (command ~subroot:"" ~target:"test/root.dart" "test" =
       ("flutter test --no-pub 'test/root.dart'", root));
    print_endline "Flutter focused checks: ok")
