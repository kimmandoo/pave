module Workspace_edit = Pave.Workspace_edit

let expect_error fn =
  match fn () with
  | _ -> failwith "expected Workspace_edit.Error"
  | exception Workspace_edit.Error _ -> ()

let write path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () -> output_string channel contents)

let read path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () -> really_input_string channel (in_channel_length channel))

let () =
  let root = Filename.temp_file "pave-workspace-edit-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let cleanup_files = ref [] in
  let create name contents =
    let path = Filename.concat root name in
    cleanup_files := path :: !cleanup_files;
    write path contents;
    path
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun path -> try Sys.remove path with Sys_error _ -> ()) !cleanup_files;
      Unix.rmdir root)
    (fun () ->
      let conflict_path = create "conflict.txt" "one\ntwo\n" in
      let stale = Workspace_edit.read_snapshot ~root ~path:"conflict.txt" in
      write conflict_path "concurrent user edit\n";
      expect_error (fun () -> Workspace_edit.apply_hunks ~root ~path:"conflict.txt"
        ~expected_sha256:stale.sha256
        ~hunks:[{ Workspace_edit.old_text = "one"; new_text = "changed" }]);
      assert (read conflict_path = "concurrent user edit\n");
      let binary_path = create "binary.bin" "before\000after" in
      expect_error (fun () -> Workspace_edit.read_snapshot ~root ~path:"binary.bin");
      assert (read binary_path = "before\000after");
      expect_error (fun () -> Workspace_edit.replace_unique ~root ~path:"binary.bin"
        ~old_text:"before" ~new_text:"replacement");
      assert (read binary_path = "before\000after");

      let hunk_path = create "hunks.txt" "alpha beta alpha\n" in
      let hunk_snapshot = Workspace_edit.read_snapshot ~root ~path:"hunks.txt" in
      let unchanged () = assert (read hunk_path = "alpha beta alpha\n") in
      expect_error (fun () -> Workspace_edit.apply_hunks ~root ~path:"hunks.txt"
        ~expected_sha256:hunk_snapshot.sha256
        ~hunks:[{ Workspace_edit.old_text = "alpha"; new_text = "A" }]);
      unchanged ();
      expect_error (fun () -> Workspace_edit.apply_hunks ~root ~path:"hunks.txt"
        ~expected_sha256:hunk_snapshot.sha256
        ~hunks:[{ Workspace_edit.old_text = "missing"; new_text = "M" }]);
      unchanged ();
      expect_error (fun () -> Workspace_edit.apply_hunks ~root ~path:"hunks.txt"
        ~expected_sha256:hunk_snapshot.sha256
        ~hunks:[{ Workspace_edit.old_text = "alpha"; new_text = "A\000B" }]);
      unchanged ();
      expect_error (fun () -> Workspace_edit.apply_hunks ~root ~path:"hunks.txt"
        ~expected_sha256:hunk_snapshot.sha256
        ~hunks:[
          { Workspace_edit.old_text = "alpha beta"; new_text = "first" };
          { Workspace_edit.old_text = "beta alpha"; new_text = "second" };
        ]);
      unchanged ();
      let successful = Workspace_edit.apply_hunks ~root ~path:"hunks.txt"
        ~expected_sha256:hunk_snapshot.sha256
        ~hunks:[
          { Workspace_edit.old_text = "alpha beta"; new_text = "A B" };
          { Workspace_edit.old_text = " alpha\n"; new_text = " omega\n" };
        ] in
      assert successful.changed;
      assert (read hunk_path = "A B omega\n");

      let filler = String.make (Workspace_edit.max_file_bytes - 3) 'x' in
      let bounded_path = create "bounded.txt" ("A" ^ filler ^ "BC") in
      let bounded_snapshot = Workspace_edit.read_snapshot ~root ~path:"bounded.txt" in
      let bounded = Workspace_edit.apply_hunks ~root ~path:"bounded.txt"
        ~expected_sha256:bounded_snapshot.sha256
        ~hunks:[
          { Workspace_edit.old_text = "A"; new_text = "AA" };
          { Workspace_edit.old_text = "BC"; new_text = "B" };
        ] in
      assert bounded.changed;
      assert (read bounded_path = "AA" ^ filler ^ "B");
      let at_limit = Workspace_edit.read_snapshot ~root ~path:"bounded.txt" in
      expect_error (fun () -> Workspace_edit.apply_hunks ~root ~path:"bounded.txt"
        ~expected_sha256:at_limit.sha256
        ~hunks:[{ Workspace_edit.old_text = "AA"; new_text = "AAA" }]);
      assert (read bounded_path = at_limit.contents);

      expect_error (fun () -> Workspace_edit.replace_unique ~root ~path:"hunks.txt"
        ~old_text:"alpha" ~new_text:"A");

      let source = "let old = old + 1\n(* old in comment *)\nlet text = \"old\"\n" in
      let ast_path = create "sample.ml" source in
      let ast_snapshot = Workspace_edit.read_snapshot ~root ~path:"sample.ml" in
      let renamed = Workspace_edit.apply_ast ~root ~language:"ocaml"
        ~edit:{ Workspace_edit.path = "sample.ml";
          expected_sha256 = ast_snapshot.sha256;
          operation = Workspace_edit.Rename_identifier { old_name = "old"; new_name = "fresh" } } in
      assert renamed.changed;
      assert (read ast_path =
        "let fresh = fresh + 1\n(* old in comment *)\nlet text = \"old\"\n");

      let after_rename = Workspace_edit.read_snapshot ~root ~path:"sample.ml" in
      let replaced = Workspace_edit.apply_ast ~root ~language:"ocaml"
        ~edit:{ Workspace_edit.path = "sample.ml";
          expected_sha256 = after_rename.sha256;
          operation = Workspace_edit.Replace_expression {
            target = "fresh + 1"; replacement = "fresh + 2" } } in
      assert replaced.changed;
      assert (read ast_path =
        "let fresh = fresh + 2\n(* old in comment *)\nlet text = \"old\"\n");
      let after_expression = Workspace_edit.read_snapshot ~root ~path:"sample.ml" in
      expect_error (fun () -> Workspace_edit.preview_ast ~root ~language:"ocaml"
        ~edits:[{ Workspace_edit.path = "sample.ml";
          expected_sha256 = after_expression.sha256;
          operation = Workspace_edit.Replace_expression {
            target = "fresh + 2"; replacement = "fresh +\000 3" } }]);
      assert (read ast_path = after_expression.contents);

      let ast_conflict_path = create "ast-conflict.ml" "let before = 1\n" in
      let ast_conflict = Workspace_edit.read_snapshot ~root ~path:"ast-conflict.ml" in
      write ast_conflict_path "let concurrent = 1\n";
      expect_error (fun () -> Workspace_edit.apply_ast ~root ~language:"ocaml"
        ~edit:{ Workspace_edit.path = "ast-conflict.ml";
          expected_sha256 = ast_conflict.sha256;
          operation = Workspace_edit.Rename_identifier { old_name = "before"; new_name = "after" } });
      assert (read ast_conflict_path = "let concurrent = 1\n");

      let dry_snapshot = Workspace_edit.read_snapshot ~root ~path:"sample.ml" in
      let dry = Workspace_edit.preview_ast ~root ~language:"ocaml"
        ~edits:[{ Workspace_edit.path = "sample.ml";
          expected_sha256 = dry_snapshot.sha256;
          operation = Workspace_edit.Rename_identifier { old_name = "fresh"; new_name = "new_name" } }] in
      assert ((List.hd dry).changed);
      assert (read ast_path = dry_snapshot.contents);
      expect_error (fun () -> Workspace_edit.preview_ast ~root ~language:"swift"
        ~edits:[{ Workspace_edit.path = "sample.ml";
          expected_sha256 = dry_snapshot.sha256;
          operation = Workspace_edit.Rename_identifier { old_name = "fresh"; new_name = "new_name" } }]);
      assert (read ast_path = dry_snapshot.contents);

      let invalid_path = create "invalid.ml" "let =\n" in
      let invalid_snapshot = Workspace_edit.read_snapshot ~root ~path:"invalid.ml" in
      expect_error (fun () -> Workspace_edit.preview_ast ~root ~language:"ocaml"
        ~edits:[{ Workspace_edit.path = "invalid.ml";
          expected_sha256 = invalid_snapshot.sha256;
          operation = Workspace_edit.Rename_identifier { old_name = "x"; new_name = "y" } }]);
      assert (read invalid_path = "let =\n");

      let first_path = create "first.ml" "let before = 1\n" in
      let first_snapshot = Workspace_edit.read_snapshot ~root ~path:"first.ml" in
      let invalid_before = Workspace_edit.read_snapshot ~root ~path:"invalid.ml" in
      expect_error (fun () -> Workspace_edit.preview_ast ~root ~language:"ocaml" ~edits:[
        { Workspace_edit.path = "first.ml"; expected_sha256 = first_snapshot.sha256;
          operation = Workspace_edit.Rename_identifier { old_name = "before"; new_name = "after" } };
        { Workspace_edit.path = "invalid.ml"; expected_sha256 = invalid_before.sha256;
          operation = Workspace_edit.Rename_identifier { old_name = "x"; new_name = "y" } };
      ]);
      assert (read first_path = "let before = 1\n");
      assert (read invalid_path = "let =\n");

      let second_path = create "second.ml" "let other = 2\n" in
      let first_snapshot = Workspace_edit.read_snapshot ~root ~path:"first.ml" in
      let second_snapshot = Workspace_edit.read_snapshot ~root ~path:"second.ml" in
      let dry_batch = Workspace_edit.preview_ast ~root ~language:"ocaml" ~edits:[
        { Workspace_edit.path = "first.ml"; expected_sha256 = first_snapshot.sha256;
          operation = Workspace_edit.Rename_identifier { old_name = "before"; new_name = "after" } };
        { Workspace_edit.path = "second.ml"; expected_sha256 = second_snapshot.sha256;
          operation = Workspace_edit.Rename_identifier { old_name = "other"; new_name = "another" } };
      ] in
      assert (List.length dry_batch = 2);
      assert (read first_path = "let before = 1\n");
      assert (read second_path = "let other = 2\n");
      assert (Workspace_edit.supported_languages = ["ocaml"]);
      assert (Workspace_edit.supported_syntaxes =
        ["OCaml implementation source (compiler-libs Parse.implementation)"]);
      print_endline "workspace edit: ok")
