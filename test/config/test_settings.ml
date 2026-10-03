let write path text =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out output) (fun () ->
    output_string output text)


let invalid label action =
  match action () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith (label ^ " was accepted")

let custom_provider_json endpoint =
  Printf.sprintf
    {|{"id":"custom-gateway","display_name":"Custom Gateway","default_route":"chat","routes":[{"name":"chat","api":"openai-chat","endpoint":"%s","account_id":"workspace-7","api_key_env":"CUSTOM_GATEWAY_KEY","models":[{"id":"model-a","display_name":"Model A","tools":true}]}]}|}
    endpoint

let parse_custom text =
  Pave.Custom_provider.parse_list (`List [Yojson.Basic.from_string text])

let invalid_custom label text =
  invalid label (fun () -> ignore (parse_custom text))

let () =
  let valid_provider = custom_provider_json
    "https://api.example.test/v1/chat/completions" in
  let provider = List.hd (parse_custom valid_provider) in
  assert (provider.Pave.Custom_provider.id = "custom-gateway");
  assert ((List.hd provider.routes).Pave.Custom_provider.auth =
    Pave.Custom_provider.Api_key_env "CUSTOM_GATEWAY_KEY");
  invalid "project-scoped custom providers" (fun () ->
    Pave.Settings.parse
      ("{\"custom_providers\":[" ^ valid_provider ^ "]}"));
  invalid_custom "remote HTTP endpoint"
    (custom_provider_json "http://api.example.test/v1/chat/completions");
  invalid_custom "endpoint user information"
    (custom_provider_json "https://user@api.example.test/v1/chat/completions");
  invalid_custom "endpoint query"
    (custom_provider_json "https://api.example.test/v1/chat/completions?key=secret");
  invalid_custom "endpoint fragment"
    (custom_provider_json "https://api.example.test/v1/chat/completions#fragment");
  invalid_custom "secret-valued configuration"
    (String.sub valid_provider 0 (String.length valid_provider - 1) ^
      {|,"api_key":"secret"}|});
  invalid_custom "duplicate model IDs"
    {|{"id":"custom-gateway","display_name":"Custom","default_route":"chat","routes":[{"name":"chat","api":"openai-chat","endpoint":"https://api.example.test/v1/chat/completions","models":[{"id":"same"},{"id":"same"}]}]}|};
  invalid_custom "cross-origin model listing"
    {|{"id":"custom-gateway","display_name":"Custom","default_route":"chat","routes":[{"name":"chat","api":"openai-chat","endpoint":"https://api.example.test/v1/chat/completions","models_endpoint":"https://other.example.test/v1/models"}]}|};
  invalid_custom "non-model listing path"
    {|{"id":"custom-gateway","display_name":"Custom","default_route":"chat","routes":[{"name":"chat","api":"openai-chat","endpoint":"https://api.example.test/v1/chat/completions","models_endpoint":"https://api.example.test/v1/catalog"}]}|};
  (match Pave.Provider_catalog.create_registry
      [{ provider with id = "openai" }] with
   | Error _ -> ()
   | Ok _ -> failwith "built-in provider ID collision was accepted");
  invalid "duplicate custom provider IDs" (fun () ->
    ignore (Pave.Custom_provider.parse_list
      (`List [Yojson.Basic.from_string valid_provider;
        Yojson.Basic.from_string valid_provider])));
  let tier_json = {|{"modelTiers":{"light":"openai@responses/model-a","review":"anthropic@messages/model-b"}}|} in
  let tiers = (Pave.Settings.parse tier_json).model_tiers in
  assert (List.assoc "review" tiers = "anthropic@messages/model-b");
  invalid "reserved inherit tier" (fun () ->
    Pave.Settings.parse {|{"modelTiers":{"inherit":"openai@responses/model-a"}}|});
  invalid "duplicate model tier" (fun () ->
    Pave.Settings.parse {|{"modelTiers":{"light":"a","light":"b"}}|});
  invalid "control-bearing model tier selector" (fun () ->
    Pave.Settings.parse {|{"modelTiers":{"light":"openai@responses/model\u007f"}}|});
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
    ignore (Pave.Settings.update_user (fun current ->
      { current with custom_providers = [provider] }));
    let inherited = Pave.Settings.load ~root:workspace in
    assert (List.map (fun item -> item.Pave.Custom_provider.id)
      inherited.values.custom_providers = ["custom-gateway"]);
    assert ((List.hd inherited.values.custom_providers).routes =
      provider.routes);
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
    ignore (Pave.Settings.update_user (fun current ->
      { current with model_tiers = tiers }));
    assert ((Pave.Settings.load ~root:workspace).values.model_tiers = tiers);
    Unix.mkdir project_dir 0o700;
    let project_file = Filename.concat project_dir "settings.json" in
    let project_settings =
      {|{"default_provider":"anthropic","disable_shell":false,"max_turns":6,"tools":{"approvalMode":"write","approval":{"write_file":"allow","read_file":"prompt"},"commandPatterns":[{"match":"git status*","approval":"allow"}]}}|} in
    write project_file project_settings;
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
      { current with model_tiers = ["light", "google@generateContent/model-c"] }));
    let merged_tiers = (Pave.Settings.load ~root:workspace).values.model_tiers in
    assert (List.assoc "light" merged_tiers = "google@generateContent/model-c");
    assert (List.assoc "review" merged_tiers = "anthropic@messages/model-b");
    write project_file
      ("{\"default_provider\":\"openai\",\"custom_providers\":[" ^
        valid_provider ^ "]}");
    let rejected_project_custom = Pave.Settings.load ~root:workspace in
    assert (rejected_project_custom.diagnostics <> []);
    assert (rejected_project_custom.values.custom_providers =
      [provider]);
    write project_file project_settings;
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
    assert ((Pave.Settings.load ~root:workspace).values.custom_providers =
      [provider]);
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
