module Flutter = Pave.Workspace_flutter_focus

let expect label condition =
  if not condition then failwith ("Flutter integration focus: " ^ label)

let rejects label fn =
  try ignore (fn ()); failwith ("Flutter integration focus accepted " ^ label)
  with Flutter.Error _ -> ()

let write path contents =
  Pave.Workspace_path.with_fd path
    [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o700
    (fun fd -> Pave.Workspace_path.write_all fd contents)

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.iter (fun name ->
        remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let () =
  let root = Filename.temp_file "pave-flutter-integration-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  let root = Unix.realpath root in
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    let at path = Filename.concat root path in
    let mkdir path = Unix.mkdir (at path) 0o700 in
    let create path contents = write (at path) contents in
    mkdir "flutter";
    mkdir "flutter/integration_test";
    mkdir "flutter/test";
    mkdir "runtime";
    create "runtime/flutter" "#!/bin/sh\nexit 0\n";
    Unix.chmod (at "runtime/flutter") 0o700;
    create "flutter/pubspec.yaml"
      "dependencies:\n  flutter:\n    sdk: flutter\ndev_dependencies:\n  integration_test:\n    sdk: flutter\n";
    create "flutter/integration_test/app_test.dart" "void main() {}\n";
    create "flutter/integration_test/notes.txt" "not Dart\n";
    create "flutter/test/unit_test.dart" "void main() {}\n";
    Unix.putenv "PATH" (at "runtime");
    let command ?(target="flutter/integration_test/app_test.dart")
        ?(ready_device_id="emulator-5554") () =
      Flutter.command ~root ~subroot:"flutter" ~action:"integration_test"
        ~target ~ready_device_id () in
    let discovery = Flutter.discover_integration_tests ~root ~subroot:"flutter" in
    expect "discovery retains package root and exact target identity"
      (discovery.root = root && discovery.subroot = "flutter" &&
       discovery.package_root = at "flutter" &&
       List.map (fun target ->
         target.Flutter.path, target.relative_path, target.package_root)
         discovery.targets =
       [("flutter/integration_test/app_test.dart",
         "integration_test/app_test.dart", at "flutter")]);
    expect "targets exact integration test on selected ready device"
      (command () =
       ("flutter test --no-pub 'integration_test/app_test.dart' -d 'emulator-5554'",
        at "flutter"));
    rejects "missing ready session binding"
      (fun () -> Flutter.command ~root ~subroot:"flutter" ~action:"integration_test"
        ~target:"flutter/integration_test/app_test.dart" ());
    rejects "malformed device serial"
      (fun () -> command ~ready_device_id:"emulator-5554-extra" ());
    rejects "unit-test target substitution"
      (fun () -> command ~target:"flutter/test/unit_test.dart" ());
    rejects "outside-package target"
      (fun () -> command ~target:"outside/integration_test/app_test.dart" ());
    rejects "non-Dart target"
      (fun () -> command ~target:"flutter/integration_test/notes.txt" ());
    rejects "missing target"
      (fun () -> command ~target:"flutter/integration_test/missing.dart" ());
    rejects "caller-supplied traversal is not a discovered target"
      (fun () -> command ~target:"flutter/integration_test/../test/unit_test.dart" ());
    Unix.symlink (at "flutter/integration_test/app_test.dart")
      (at "flutter/integration_test/link.dart");
    rejects "symlink target"
      (fun () -> command ~target:"flutter/integration_test/link.dart" ());
    Unix.unlink (at "flutter/integration_test/app_test.dart");
    Unix.unlink (at "flutter/integration_test/link.dart");
    Unix.unlink (at "flutter/integration_test/notes.txt");
    Unix.rmdir (at "flutter/integration_test");
    Unix.mkdir (at "flutter/integration_test") 0o700;
    mkdir "flutter/integration_test/directory.dart";
    rejects "nonregular target"
      (fun () -> command ~target:"flutter/integration_test/directory.dart" ());
    rejects "absent integration tests are an explicit discovery failure"
      (fun () -> Flutter.discover_integration_tests ~root ~subroot:"flutter");
    mkdir "bounded";
    mkdir "bounded/integration_test";
    create "bounded/pubspec.yaml"
      "dependencies:\n  flutter:\n    sdk: flutter\ndev_dependencies:\n  integration_test:\n    sdk: flutter\n";
    for index = 0 to Flutter.max_integration_entries do
      create ("bounded/integration_test/entry-" ^ string_of_int index ^ ".txt") ""
    done;
    rejects "integration target discovery is bounded"
      (fun () -> Flutter.discover_integration_tests ~root ~subroot:"bounded");
    create "flutter/pubspec.yaml"
      "dependencies:\n  flutter:\n    sdk: flutter\n";
    rejects "missing integration_test dependency"
      (fun () -> command ());
    create "flutter/pubspec.yaml"
      "dependencies:\n  flutter:\n    sdk: flutter\ndev_dependencies:\n  integration_test:\n    sdk: flutter\n";
    Unix.putenv "PATH" (at "missing-runtime");
    rejects "missing Flutter runtime"
      (fun () -> command ());
    print_endline "Flutter integration focus: ok")
