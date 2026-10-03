let child path name = Filename.concat path name

let rec remove path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter (fun name -> remove (child path name)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let read_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    really_input_string channel (in_channel_length channel))

let write_tool ~root ~path contents =
  let args = `Assoc ["path", `String path; "content", `String contents] in
  match Pave.Tools.prepare ~root ~name:"write_file" ~args () with
  | Error message -> failwith message
  | Ok execute ->
      (match execute () with
       | Ok [Pave.Protocol.Text _] -> ()
       | Error message -> failwith message
       | _ -> failwith "unexpected write_file result")

let expect_error action =
  try ignore (action ()); failwith "expected guarded rewind rejection"
  with Pave.Session_rewind.Error _ -> ()

let () =
  let previous_home = Sys.getenv_opt "HOME"
  and previous_state = Sys.getenv_opt "XDG_STATE_HOME" in
  let base = Filename.temp_file "pave-session-rewind-" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  Fun.protect ~finally:(fun () ->
    (match previous_home with Some value -> Unix.putenv "HOME" value
     | None -> Unix.putenv "HOME" "");
    (match previous_state with Some value -> Unix.putenv "XDG_STATE_HOME" value
     | None -> Unix.putenv "XDG_STATE_HOME" "");
    remove base) (fun () ->
    let root = child base "workspace" and state_home = child base "state" in
    Unix.mkdir root 0o700;
    Unix.mkdir state_home 0o700;
    Unix.putenv "HOME" base;
    Unix.putenv "XDG_STATE_HOME" state_home;
    let session = Pave.Session_store.create ~root in
    let manager = Pave.Session_rewind.create ~root ~session in

    let existing_path = child root "existing.txt" in
    Pave.Workspace_path.atomic_write existing_path "before\n";
    Unix.chmod existing_path 0o640;
    let before = Pave.Session_rewind.snapshot_file ~root ~path:"existing.txt" in
    write_tool ~root ~path:"existing.txt" "after\n";
    let after = Pave.Session_rewind.snapshot_file ~root ~path:"existing.txt" in
    let existing = Option.get (Pave.Session_rewind.record_file_change manager
      ~tool_name:"write_file" ~path:"existing.txt" ~before ~after) in
    assert (existing.status = Pave.Session_rewind.Rewindable);
    ignore (Pave.Session_rewind.rewind manager ~id:existing.id);
    assert (read_file existing_path = "before\n");
    assert ((Unix.stat existing_path).Unix.st_perm land 0o7777 = 0o640);
    expect_error (fun () -> Pave.Session_rewind.rewind manager ~id:existing.id);

    let created_path = child root "created.txt" in
    let before = Pave.Session_rewind.snapshot_file ~root ~path:"created.txt" in
    assert (before = Pave.Session_rewind.Missing);
    write_tool ~root ~path:"created.txt" "created\n";
    let after = Pave.Session_rewind.snapshot_file ~root ~path:"created.txt" in
    let created = Option.get (Pave.Session_rewind.record_file_change manager
      ~tool_name:"write_file" ~path:"created.txt" ~before ~after) in
    ignore (Pave.Session_rewind.rewind manager ~id:created.id);
    assert (not (Sys.file_exists created_path));

    let conflict_path = child root "conflict.txt" in
    Pave.Workspace_path.atomic_write conflict_path "initial\n";
    let before = Pave.Session_rewind.snapshot_file ~root ~path:"conflict.txt" in
    write_tool ~root ~path:"conflict.txt" "tool result\n";
    let after = Pave.Session_rewind.snapshot_file ~root ~path:"conflict.txt" in
    let conflict = Option.get (Pave.Session_rewind.record_file_change manager
      ~tool_name:"write_file" ~path:"conflict.txt" ~before ~after) in
    Unix.chmod conflict_path 0o640;
    expect_error (fun () -> Pave.Session_rewind.rewind manager ~id:conflict.id);
    assert (read_file conflict_path = "tool result\n" &&
      (Unix.stat conflict_path).Unix.st_perm land 0o7777 = 0o640);
    Unix.chmod conflict_path 0o600;
    Pave.Workspace_path.atomic_write conflict_path "user update\n";
    expect_error (fun () -> Pave.Session_rewind.rewind manager ~id:conflict.id);
    assert (read_file conflict_path = "user update\n");
    let lsp_path = child root "lsp-edited.txt" in
    Pave.Workspace_path.atomic_write lsp_path "before LSP\n";
    let lsp_before = Pave.Session_rewind.snapshot_file ~root ~path:"lsp-edited.txt" in
    Pave.Workspace_path.atomic_write lsp_path "after LSP\n";
    let lsp_after = Pave.Session_rewind.snapshot_file ~root ~path:"lsp-edited.txt" in
    let lsp_entry = Option.get (Pave.Session_rewind.record_file_change manager
      ~tool_name:"lsp" ~path:"lsp-edited.txt"
      ~before:lsp_before ~after:lsp_after) in
    ignore (Pave.Session_rewind.rewind manager ~id:lsp_entry.id);
    assert (read_file lsp_path = "before LSP\n");

    let irreversible = Pave.Session_rewind.record_non_reversible manager
      ~tool_name:"run_command"
      ~detail:"Shell command may have caused workspace or external effects." in
    assert (irreversible.status = Pave.Session_rewind.Non_reversible);
    expect_error (fun () -> Pave.Session_rewind.rewind manager ~id:irreversible.id);
    let non_reversible_tools = [
      "start_process"; "start_shell"; "process_stdin"; "process_close_stdin";
      "process_kill"; "worktree_create"; "worktree_commit"; "worktree_remove";
      "workspace_eval"; "lsp_start"; "dap_start"; "dap";
      "ssh_open"; "ssh_read"; "ssh_write"; "ssh_command";
      "web_search"; "web_fetch"; "clipboard_write";
      "publish_web"; "xcode_preflight"; "mobile_check"; "mobile_session";
      "mobile_scenario"; "mobile_verify"; "mobile_visual"; "android_devices";
      "write_file"; "edit_file"; "apply_edits"; "ast_edit"
    ] in
    List.iter (fun tool_name ->
      let entry = Pave.Session_rewind.record_non_reversible manager
        ~tool_name ~detail:(tool_name ^ " effect is not reversible") in
      assert (entry.status = Pave.Session_rewind.Non_reversible);
      expect_error (fun () -> Pave.Session_rewind.rewind manager ~id:entry.id))
      non_reversible_tools;
    expect_error (fun () -> Pave.Session_rewind.record_non_reversible manager
      ~tool_name:"process_wait" ~detail:"observation only");

    let unchanged_before = Pave.Session_rewind.snapshot_file ~root ~path:"conflict.txt" in
    write_tool ~root ~path:"conflict.txt" "user update\n";
    let unchanged_after = Pave.Session_rewind.snapshot_file ~root ~path:"conflict.txt" in
    assert (Pave.Session_rewind.record_file_change manager
      ~tool_name:"write_file" ~path:"conflict.txt"
      ~before:unchanged_before ~after:unchanged_after = None);

    let reopened = Pave.Session_store.open_existing ~root session.path in
    let reopened_manager = Pave.Session_rewind.create ~root ~session:reopened in
    let recovered = Pave.Session_rewind.list reopened_manager in
    assert (List.exists (fun (rewind_entry : Pave.Session_rewind.rewind_effect) ->
      rewind_entry.id = existing.id &&
      rewind_entry.status = Pave.Session_rewind.Reverted) recovered);
    assert (List.exists (fun (rewind_entry : Pave.Session_rewind.rewind_effect) ->
      rewind_entry.id = lsp_entry.id &&
      rewind_entry.status = Pave.Session_rewind.Reverted) recovered);
    assert (List.exists (fun (rewind_entry : Pave.Session_rewind.rewind_effect) ->
      rewind_entry.id = conflict.id &&
      rewind_entry.status = Pave.Session_rewind.Rewindable) recovered);
    assert (List.exists (fun (rewind_entry : Pave.Session_rewind.rewind_effect) ->
      rewind_entry.id = irreversible.id &&
      rewind_entry.status = Pave.Session_rewind.Non_reversible) recovered);

    let fork = Pave.Session_store.fork ~root reopened in
    let fork_manager = Pave.Session_rewind.create ~root ~session:fork in
    assert (Pave.Session_rewind.list fork_manager = []);
    print_endline "guarded session workspace rewind: ok")
