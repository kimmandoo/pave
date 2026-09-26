let write path text =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out output) (fun () ->
    output_string output text)

let () =
  let base = Filename.temp_file "pave-settings-" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  let user_home = Filename.concat base "config" in
  let user_dir = Filename.concat user_home "pave" in
  let workspace = Filename.concat base "workspace" in
  let project_dir = Filename.concat workspace ".pave" in
  Unix.mkdir user_home 0o700;
  Unix.mkdir user_dir 0o700;
  Unix.mkdir workspace 0o700;
  let previous = Sys.getenv_opt "XDG_CONFIG_HOME" in
  Unix.putenv "XDG_CONFIG_HOME" user_home;
  Fun.protect ~finally:(fun () ->
    (match previous with Some value -> Unix.putenv "XDG_CONFIG_HOME" value
      | None -> Unix.putenv "XDG_CONFIG_HOME" "");
    (try match (Unix.lstat project_dir).Unix.st_kind with
     | Unix.S_DIR ->
         (try Sys.remove (Filename.concat project_dir "settings.json")
          with Sys_error _ -> ());
         (try Sys.remove (Filename.concat project_dir "settings.lock")
          with Sys_error _ -> ());
         Unix.rmdir project_dir
     | Unix.S_LNK -> Sys.remove project_dir
     | _ -> ()
     with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
    (try Sys.remove (Filename.concat user_dir "setup.json")
     with Sys_error _ -> ());
    (try Sys.remove (Filename.concat user_dir "settings.lock")
     with Sys_error _ -> ());
    Sys.remove (Filename.concat user_dir "settings.json");
    Unix.rmdir workspace; Unix.rmdir user_dir; Unix.rmdir user_home;
    Unix.rmdir base) (fun () ->
    write (Filename.concat user_dir "settings.json")
      {|{"default_provider":"openai","default_model":"gpt-6-sol","default_api":"responses","default_account_id":"account-1","disable_shell":true,"max_turns":12,"tools":{"approvalMode":"yolo","approval":{"write_file":"deny","run_command":"allow"},"commandPatterns":[{"match":"rm -rf *","approval":"deny"}]}}|};
    let inherited = Pave.Settings.load ~root:workspace in
    assert (inherited.values.default_model = Some "gpt-6-sol");
    assert (inherited.values.default_api = Some "responses");
    assert (inherited.values.default_account_id = Some "account-1");
    let orphan_account = try
      ignore (Pave.Settings.parse {|{"default_account_id":"account-1"}|});
      false
    with Invalid_argument _ -> true in
    assert orphan_account;
    assert (inherited.values.max_turns = Some 12);
    assert (inherited.values.approval_mode = Some Pave.Approval.Auto_all);
    assert (List.assoc "write_file" inherited.values.tool_approval =
      Pave.Approval.Deny);
    assert (List.length inherited.values.command_patterns = 1);
    Unix.mkdir project_dir 0o700;
    let project_file = Filename.concat project_dir "settings.json" in
    write project_file
      {|{"default_provider":"anthropic","disable_shell":false,"max_turns":6,"tools":{"approvalMode":"write","approval":{"write_file":"allow","read_file":"prompt"},"commandPatterns":[{"match":"git status*","approval":"allow"}]}}|};
    let project = Pave.Settings.load ~root:workspace in
    assert (project.values.default_provider = Some "anthropic");
    assert (project.values.default_model = None);
    assert (project.values.default_api = None);
    assert (project.values.default_account_id = None);
    assert (project.values.disable_shell);
    assert (project.values.approval_mode = Some Pave.Approval.Ask_exec);
    assert (List.assoc "write_file" project.values.tool_approval =
      Pave.Approval.Deny);
    assert (List.assoc "read_file" project.values.tool_approval =
      Pave.Approval.Prompt);
    assert (List.length project.values.command_patterns = 2);
    ignore (Pave.Settings.update_project ~root:workspace (fun current ->
      { current with max_turns = Some 9 }));
    assert ((Pave.Settings.load ~root:workspace).values.max_turns = Some 9);
    let rejected = try
      ignore (Pave.Settings.update_project ~root:workspace (fun current ->
        { current with max_turns = Some 0 }));
      false
    with Invalid_argument _ -> true in
    assert (rejected);
    assert ((Pave.Settings.load ~root:workspace).values.max_turns = Some 9);
    write project_file {|{"max_turns":2,"max_turns":90}|};
    let invalid = Pave.Settings.load ~root:workspace in
    assert (invalid.values.max_turns = Some 12);
    assert (invalid.diagnostics <> []);
    Sys.remove project_file;
    ignore (Pave.Settings.update_user (fun current ->
      { current with default_provider = Some "ollama";
        default_model = Some "llama3.2"; default_api = Some "chat";
        default_account_id = Some "local-profile" }));
    assert ((Pave.Settings.load ~root:workspace).values.default_provider =
      Some "ollama");
    assert ((Pave.Settings.load ~root:workspace).values.default_api =
      Some "chat");
    assert ((Pave.Settings.load ~root:workspace).values.default_account_id =
      Some "local-profile");
    assert ((Pave.Settings.load ~root:workspace).values.disable_shell);
    assert ((Pave.Settings.load ~root:workspace).values.max_turns = Some 12);
    Pave.Setup_state.mark Pave.Setup_state.Complete;
    assert ((Pave.Setup_state.load ()).status =
      Some Pave.Setup_state.Complete);
    let status_path = Filename.concat user_dir "setup.json" in
    Sys.remove status_path;
    Unix.symlink (Filename.concat user_dir "settings.json") status_path;
    assert ((Pave.Setup_state.load ()).diagnostics <> []);
    let rejected_status = try
      Pave.Setup_state.mark Pave.Setup_state.Skipped; false
    with Invalid_argument _ -> true in
    assert (rejected_status);
    assert ((Pave.Settings.load ~root:workspace).values.default_model =
      Some "llama3.2");
    Sys.remove status_path;
    Pave.Setup_state.mark Pave.Setup_state.Skipped;
    assert ((Pave.Setup_state.load ()).status =
      Some Pave.Setup_state.Skipped);
    Sys.remove (Filename.concat project_dir "settings.lock");
    Unix.rmdir project_dir;
    Unix.symlink user_dir project_dir;
    let escaped = Pave.Settings.load ~root:workspace in
    assert (escaped.values.default_provider = Some "ollama");
    assert (escaped.diagnostics <> []));
  print_endline "settings precedence: ok"
