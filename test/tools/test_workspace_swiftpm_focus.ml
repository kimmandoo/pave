let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec loop i = i + m <= n &&
    (String.sub text i m = fragment || loop (i + 1)) in
  loop 0

let () =
  let root = Filename.temp_file "pave-swiftpm-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let root = Unix.realpath root in
  Fun.protect ~finally:(fun () ->
    Sys.remove (Filename.concat root "Package.swift"); Unix.rmdir root)
    (fun () ->
      let package = Filename.concat root "Package.swift" in
      let save text =
        let oc = open_out package in
        Fun.protect ~finally:(fun () -> close_out_noerr oc)
          (fun () -> output_string oc text) in
      save "// swift-tools-version: 6.0\nimport PackageDescription\nlet package = Package(name: \"Fixture\", targets: [.testTarget(name: \"FixtureTests\")])\n";
      let discover, cwd = Pave.Workspace_swiftpm_focus.command
        ~root ~subroot:"" ~action:"discover" ~target:"" in
      assert (contains discover "swift test" && cwd = root);
      let tests = Pave.Workspace_swiftpm_focus.tests
        "FixtureTests.ExampleTests/testWorks()\n" in
      assert (tests = ["FixtureTests.ExampleTests/testWorks()"]);
      let command, _ = Pave.Workspace_swiftpm_focus.command ~root
        ~subroot:"" ~action:"run" ~target:(List.hd tests) in
      assert (contains command "--filter" && contains command "testWorks()");
      assert (try ignore (Pave.Workspace_swiftpm_focus.tests
        "warning: no tests\n"); false
        with Pave.Workspace_swiftpm_focus.Error _ -> true);
      assert (Pave.Workspace_swiftpm_focus.no_tests
        "Test Suite 'Selected tests' passed. Executed 0 tests, with 0 failures.\n");
      assert (not (Pave.Workspace_swiftpm_focus.no_tests
        "Test Suite 'Selected tests' passed. Executed 1 test, with 0 failures.\n"));
      assert (Pave.Workspace_swiftpm_focus.no_tests
        "✔ Test run with 0 tests passed after 0.001 seconds.\n");
      List.iter (fun count ->
        assert (not (Pave.Workspace_swiftpm_focus.no_tests
          (Printf.sprintf "✔ Test run with %d tests passed after 0.001 seconds.\n" count))))
        [1; 10; 20; 100; 200];
      Unix.symlink root (Filename.concat root "linked");
      assert (try ignore (Pave.Workspace_swiftpm_focus.command
        ~root ~subroot:"linked" ~action:"discover" ~target:""); false
        with Pave.Workspace_swiftpm_focus.Error _ -> true);
      Unix.unlink (Filename.concat root "linked");
      save "// swift-tools-version: 6.0\nlet deps = [.package(url: \"https://example.invalid/dep\", from: \"1.0.0\")]\n";
      assert (try ignore (Pave.Workspace_swiftpm_focus.command
        ~root ~subroot:"" ~action:"discover" ~target:""); false
        with Pave.Workspace_swiftpm_focus.Error _ -> true));
  print_endline "SwiftPM focus: ok"
