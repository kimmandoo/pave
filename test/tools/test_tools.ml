let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec loop i = i + m <= n &&
    (String.sub text i m = fragment || loop (i + 1)) in
  loop 0

let args fields = `Assoc (List.map (fun (k, v) -> k, `String v) fields)
let tool_json root name fields =
  Pave.Tools.execute ~root ~name ~args:(`Assoc fields) ()
let tool root name fields = Pave.Tools.execute ~root ~name ~args:(args fields) ()
let rejected f =
  try contains (String.lowercase_ascii (f ())) "error"
  with _ -> true

let lsp_frame message =
  let body = Yojson.Basic.to_string message in
  Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body

let fake_lsp_io () =
  let lock = Mutex.create () and ready = Condition.create () in
  let incoming = Queue.create () and current = ref None and offset = ref 0 in
  let closed = ref false in
  let enqueue text =
    Mutex.lock lock;
    Queue.add text incoming;
    Condition.signal ready;
    Mutex.unlock lock in
  let read bytes destination length =
    Mutex.lock lock;
    let rec await () =
      match !current with
      | Some text when !offset < String.length text -> ()
      | _ ->
          current := None;
          offset := 0;
          if not (Queue.is_empty incoming) then (
            current := Some (Queue.take incoming);
            await ())
          else if not !closed then (
            Condition.wait ready lock;
            await ()) in
    await ();
    let count = match !current with
      | None -> 0
      | Some text ->
          let count = min length (String.length text - !offset) in
          Bytes.blit_string text !offset bytes destination count;
          offset := !offset + count;
          count in
    Mutex.unlock lock;
    count in
  let write wire =
    let marker = "\r\n\r\n" in
    let body_start = Str.search_forward (Str.regexp_string marker) wire 0 +
      String.length marker in
    let body = String.sub wire body_start (String.length wire - body_start) in
    let request = Yojson.Basic.from_string body in
    let answer = match Pave.Workspace_lsp.member "method" request with
      | `String "initialize" ->
          let capabilities = `Assoc [
            "positionEncoding", `String "utf-16";
            "definitionProvider", `Bool true;
            "referencesProvider", `Bool true;
            "hoverProvider", `Bool true;
            "renameProvider", `Bool true;
            "codeActionProvider", `Bool true] in
          Some (`Assoc ["jsonrpc", `String "2.0";
            "id", Pave.Workspace_lsp.member "id" request;
            "result", `Assoc ["capabilities", capabilities]])
      | `String "shutdown" ->
          Some (`Assoc ["jsonrpc", `String "2.0";
            "id", Pave.Workspace_lsp.member "id" request; "result", `Null])
      | _ -> None in
    Option.iter (fun answer -> enqueue (lsp_frame answer)) answer in
  let close () =
    Mutex.lock lock;
    closed := true;
    Condition.broadcast ready;
    Mutex.unlock lock in
  { Pave.Workspace_lsp.read = read; write; close; terminate = (fun () -> ()) }

let fake_lsp_manager () =
  Pave.Workspace_lsp.create_manager
    ~launcher:(fun ~program:_ ~arguments:_ ~cwd:_ ~environment:_ -> fake_lsp_io ()) ()
let () =
  let root = Filename.temp_file "pave-tools-" "" in
  Sys.remove root; Unix.mkdir root 0o700;
  let outside = Filename.temp_file "pave-outside-" ".swift" in
  let outside_mobile = Filename.temp_file "pave-outside-mobile-" "" in
  Sys.remove outside_mobile; Unix.mkdir outside_mobile 0o700;
  let outside_manifest = Filename.concat outside_mobile "Package.swift" in
  let files = ref [] and directories = ref [] in
  let create path text =
    let absolute = Filename.concat root path in
    files := absolute :: !files;
    let oc = open_out absolute in
    Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc text) in
  let directory path =
    let absolute = Filename.concat root path in
    Unix.mkdir absolute 0o700;
    directories := absolute :: !directories in
  let oc = open_out outside in output_string oc "outside secret\n"; close_out oc;
  let process_manager = Pave.Workspace_process.create_manager () in
  let lsp_manager = fake_lsp_manager () in
  let tool_context = Pave.Tools.create_session_context ~lsp_manager
    ~owner:"tools-test-session" ~root ~process_manager
    ~read_artifact:(fun _ -> None)
    ~record_file_change:(fun ~path:_ ~before:_ ~after:_ -> ()) () in
  Fun.protect ~finally:(fun () ->
    Pave.Tools.close_session_context tool_context;
    Pave.Workspace_process.close_manager process_manager;
    List.iter (fun path -> try Sys.remove path with Sys_error _ -> ()) !files;
    List.iter (fun path -> try Unix.rmdir path with Unix.Unix_error _ -> ()) !directories;
    Sys.remove outside;
    (try Sys.remove outside_manifest with Sys_error _ -> ());
    Unix.rmdir outside_mobile; Unix.rmdir root) (fun () ->
    create "App.swift" "one\none\n";
    create "snapshot.txt" "alpha beta\n";
    let snapshot = Pave.Workspace_edit.read_snapshot ~root ~path:"snapshot.txt" in
    let snapshot_page = tool_json root "workspace_snapshot"
      ["path", `String "snapshot.txt"; "max_bytes", `Int 5] in
    assert (contains snapshot_page snapshot.sha256 && contains snapshot_page "alpha");
    let edit_args hunks = `Assoc [
      "path", `String "snapshot.txt";
      "expected_sha256", `String snapshot.sha256;
      "hunks", `List hunks
    ] in
    let replacement_hunks = [
      `Assoc ["old_text", `String "alpha"; "new_text", `String "A"];
      `Assoc ["old_text", `String "beta"; "new_text", `String "B"]
    ] in
    let applied = Pave.Tools.execute ~root ~name:"apply_edits"
      ~args:(edit_args replacement_hunks) () in
    assert (not (contains (String.lowercase_ascii applied) "error"));
    assert (Pave.Workspace_path.read_bounded
      (Filename.concat root "snapshot.txt") 65_536 = "A B\n");
    let stale = Pave.Tools.execute ~root ~name:"apply_edits"
      ~args:(edit_args replacement_hunks) () in
    assert (contains stale "changed since");
    assert (Pave.Workspace_path.read_bounded
      (Filename.concat root "snapshot.txt") 65_536 = "A B\n");

    create "ast_sample.ml" "let old = old + 1\n(* old *)\nlet text = \"old\"\n";
    let ast_snapshot = Pave.Workspace_edit.read_snapshot ~root ~path:"ast_sample.ml" in
    let ast_args dry_run language = `Assoc [
      "path", `String "ast_sample.ml";
      "language", `String language;
      "operation", `String "rename_identifier";
      "expected_sha256", `String ast_snapshot.sha256;
      "old_name", `String "old";
      "new_name", `String "fresh";
      "dry_run", `Bool dry_run
    ] in
    let ast_preview = Pave.Tools.execute ~root ~name:"ast_edit"
      ~args:(ast_args true "ocaml") () in
    assert (contains ast_preview "AST preview");
    assert (Pave.Workspace_path.read_bounded
      (Filename.concat root "ast_sample.ml") 65_536 = ast_snapshot.contents);
    assert (rejected (fun () -> Pave.Tools.execute ~root ~name:"ast_edit"
      ~args:(ast_args false "swift") ()));
    let ast_result = Pave.Tools.execute ~root ~name:"ast_edit"
      ~args:(ast_args false "ocaml") () in
    assert (not (contains (String.lowercase_ascii ast_result) "error"));
    assert (Pave.Workspace_path.read_bounded
      (Filename.concat root "ast_sample.ml") 65_536 =
      "let fresh = fresh + 1\n(* old *)\nlet text = \"old\"\n");

    assert (contains (tool root "read_file" ["path", "App.swift"]) "one");
    assert (rejected (fun () -> tool root "edit_file"
      ["path", "App.swift"; "old_string", "one"; "new_string", "two"]));
    ignore (tool root "edit_file" ["path", "App.swift";
      "old_string", "one\none\n"; "new_string", "two\n"]);
    assert (contains (tool root "read_file" ["path", "App.swift"]) "two");
    let link = Filename.concat root "escape.swift" in
    Unix.symlink outside link; files := link :: !files;
    let folder = Filename.temp_file "pave-outside-dir-" "" in
    Sys.remove folder; Unix.mkdir folder 0o700;
    let external_file = Filename.concat folder "hidden.swift" in
    let oc = open_out external_file in output_string oc "outside secret\n"; close_out oc;
    Fun.protect ~finally:(fun () -> Sys.remove external_file; Unix.rmdir folder) (fun () ->
      let link = Filename.concat root "external" in
      Unix.symlink folder link; files := link :: !files;
      assert (not (contains (tool root "fuzzy_file_search" ["query", "hidden"])
        "external/hidden.swift"));
      assert (rejected (fun () -> tool root "read_file" ["path", "../escape.swift"]));
      assert (rejected (fun () -> tool root "read_file" ["path", "escape.swift"]));
      assert (rejected (fun () -> tool root "read_file" ["path", "external/hidden.swift"]));
      assert (rejected (fun () -> tool root "glob" ["pattern", "*.swift"; "path", "external"]));
      assert (rejected (fun () -> tool root "grep" ["pattern", "outside"; "path", "external"]));
      assert (rejected (fun () -> tool root "glob" ["pattern", "../*.swift"]));
      assert (rejected (fun () -> tool root "write_file"
        ["path", "escape.swift"; "content", "bad"]));
      assert (rejected (fun () -> tool root "write_file"
        ["path", "../outside.swift"; "content", "bad"])));
    directory "src"; directory "src/nested"; directory "ignored"; directory "build";
    directory ".private"; directory "src/Folder.swift";
    create ".gitignore" "ignored/\n*.tmp\n!keep.tmp\n/root-only.swift\n\\#literal.swift\n";
    create "src/.gitignore" "*.log\n!keep.log\nnested/*.swift\n!nested/Keep.swift\n";
    create "src/Match.swift" "needle-123\nquiet\nneedle-456\n";
    create "src/hidden.log" "needle-789\n";
    create "src/keep.log" "needle-555\n";
    create "src/nested/Drop.swift" "needle-666\n";
    create "src/nested/Keep.swift" "needle-333\n";
    create "ignored/Omit.swift" "needle-999\n";
    create "build/Omit.swift" "needle-888\n";
    create "hidden.tmp" "needle-777\n";
    create "keep.tmp" "needle-444\n";
    create ".private/Visible.swift" "needle-222\n";
    create ".secret.swift" "needle-111\n";
    create "#literal.swift" "needle-000\n";
    create "root-only.swift" "needle-222\n";
    create "src/root-only.swift" "needle-223\n";
    directory "src/History";
    create "history-search.ts" "";
    create "src/history-search.ts" "";
    create ".private/history-search.ts" "";
    create "ignored/history-search.ts" "";
    create "src/Σummary.ml" "";
    let fuzzy = Yojson.Basic.from_string
      (tool_json root "fuzzy_file_search" ["query", `String "histsr"]) in
    let fuzzy_matches = Yojson.Basic.Util.to_list
      (Yojson.Basic.Util.member "matches" fuzzy) in
    assert (Yojson.Basic.Util.member "total_matches" fuzzy = `Int 2 &&
      Yojson.Basic.Util.member "truncated" fuzzy = `Bool false);
    assert (Yojson.Basic.Util.member "path" (List.hd fuzzy_matches) =
      `String "history-search.ts" &&
      not (contains (Yojson.Basic.to_string fuzzy) ".private") &&
      not (contains (Yojson.Basic.to_string fuzzy) "ignored/"));
    let capped_fuzzy = Yojson.Basic.from_string
      (tool_json root "fuzzy_file_search"
        ["query", `String "hist"; "max_results", `Int 1]) in
    assert (Yojson.Basic.Util.member "total_matches" capped_fuzzy = `Int 3 &&
      Yojson.Basic.Util.member "truncated" capped_fuzzy = `Bool true &&
      Yojson.Basic.Util.member "path"
        (List.hd (Yojson.Basic.Util.to_list
          (Yojson.Basic.Util.member "matches" capped_fuzzy))) =
        `String "history-search.ts");
    let directory_fuzzy = Yojson.Basic.from_string
      (tool_json root "fuzzy_file_search" ["query", `String "hist"]) in
    assert (List.exists (fun result ->
      Yojson.Basic.Util.member "path" result = `String "src/History" &&
      Yojson.Basic.Util.member "is_directory" result = `Bool true)
      (Yojson.Basic.Util.to_list
        (Yojson.Basic.Util.member "matches" directory_fuzzy)));
    let hidden_fuzzy = Yojson.Basic.from_string
      (tool_json root "fuzzy_file_search"
        ["query", `String "histsr"; "hidden", `Bool true]) in
    assert (Yojson.Basic.Util.member "total_matches" hidden_fuzzy = `Int 3 &&
      contains (Yojson.Basic.to_string hidden_fuzzy) ".private/history-search.ts");
    let unicode_fuzzy = Yojson.Basic.from_string
      (tool_json root "fuzzy_file_search" ["query", `String "σmm"]) in
    assert (contains (Yojson.Basic.to_string unicode_fuzzy) "src/Σummary.ml");
    assert (contains (tool root "fuzzy_file_search" ["query", ""])
      "query must not be empty");
    assert (contains (tool root "fuzzy_file_search"
      ["query", String.make 1 '\xff']) "valid UTF-8");
    let scanner_secret = "integration-secret-94821" in
    create ".env" ("API_KEY=" ^ scanner_secret ^ "\n");
    let scan = tool_json root "repository_security_scan"
      ["format", `String "summary"] in
    let scan_json = Yojson.Basic.from_string scan in
    let findings =
      Yojson.Basic.Util.to_list
        (Yojson.Basic.Util.member "findings" scan_json) in
    assert (contains scan ".env" && not (contains scan scanner_secret));
    assert (not (contains scan "escape.swift" || contains scan "external"));
    assert (List.exists (fun finding ->
      Yojson.Basic.Util.member "path" finding = `String ".env" &&
      Yojson.Basic.Util.member "start_line" finding = `Int 1 &&
      Yojson.Basic.Util.member "validated" finding = `Bool true) findings);
    let sarif = tool_json root "repository_security_scan"
      ["format", `String "sarif"] in
    assert (Yojson.Basic.Util.member "version" (Yojson.Basic.from_string sarif) =
      `String "2.1.0" && not (contains sarif scanner_secret));
    let scan_decision = Pave.Tools.approval_decision
      ~command_patterns:[] ~name:"repository_security_scan" ~args:(`Assoc []) in
    assert (scan_decision.tier = Pave.Approval.Read &&
      not (Pave.Tools.requires_explicit_approval
        ~name:"repository_security_scan" ~args:(`Assoc [])));
    assert (List.exists (function
      | `Assoc fields -> (match List.assoc_opt "function" fields with
          | Some (`Assoc desc) -> List.assoc_opt "name" desc =
              Some (`String "repository_security_scan")
          | _ -> false)
      | _ -> false) Pave.Tools.definitions);
    let capped_scan = Yojson.Basic.from_string
      (tool_json root "repository_security_scan"
        ["format", `String "summary"; "finding_limit", `Int 0]) in
    assert (Yojson.Basic.Util.member "findings" capped_scan = `List [] &&
      Yojson.Basic.Util.member "truncated" capped_scan = `Bool true);
    let fuzzy_decision = Pave.Tools.approval_decision
      ~command_patterns:[] ~name:"fuzzy_file_search"
      ~args:(`Assoc ["query", `String "hist"]) in
    assert (fuzzy_decision.tier = Pave.Approval.Read &&
      Pave.Tools.execution_mode "fuzzy_file_search" = Pave.Tool_scheduler.Shared);
    create "sample.png" "not a valid image";
    let approval_case ?context name fields expected_tier =
      let args = `Assoc fields in
      let decision = Pave.Tools.approval_decision
        ~command_patterns:[] ~name ~args in
      assert (decision.tier = expected_tier);
      assert (Pave.Tools.requires_explicit_approval ~name ~args);
      let request = Pave.Tools.approval_request ?context ~root ~name ~args decision in
      assert (request.impact <> "");
      request in
    let search_secret = "approval-secret-must-not-render" in
    let previous_search_key = Sys.getenv_opt "BRAVE_SEARCH_API_KEY" in
    Unix.putenv "BRAVE_SEARCH_API_KEY" search_secret;
    let search_request = approval_case "web_search"
      ["query", `String "release documentation"] Pave.Approval.Exec in
    let search_preview =
      String.concat "\n" (search_request.impact :: search_request.details) in
    assert (contains search_preview "release documentation" &&
      contains search_preview "brave" &&
      not (contains search_preview search_secret));
    Unix.putenv "BRAVE_SEARCH_API_KEY"
      (Option.value ~default:"" previous_search_key);
    ignore (approval_case "web_fetch"
      ["url", `String "https://example.com/"] Pave.Approval.Exec);
    let ocr_request = approval_case "image_ocr"
      ["path", `String "sample.png"; "mime", `String "image/png"]
      Pave.Approval.Exec in
    assert (contains ocr_request.impact "no shell or network");
    let clipboard_read = approval_case "clipboard_read" []
      Pave.Approval.Read in
    assert (clipboard_read.details =
      ["No clipboard content is read before approval."]);
    ignore (approval_case "clipboard_write"
      ["text", `String "reviewed clipboard content"] Pave.Approval.Write);
    List.iter (fun (name, fields) ->
      let result = tool_json root name fields in
      assert (contains result "requires explicit interactive approval"))
      ["web_search", ["query", `String "never sent"];
       "web_fetch", ["url", `String "https://example.com/"];
       "image_ocr", ["path", `String "sample.png";
                     "mime", `String "image/png"];
       "clipboard_read", [];
       "clipboard_write", ["text", `String "must not be copied"]];
    let lsp_start_request = approval_case ~context:tool_context "lsp_start"
      ["program", `String "/usr/bin/example-lsp";
       "arguments", `List [`String "--stdio"]] Pave.Approval.Exec in
    let lsp_start_preview = String.concat "\n"
      (lsp_start_request.impact :: lsp_start_request.details) in
    assert (contains lsp_start_preview "/usr/bin/example-lsp" &&
      contains lsp_start_preview "--stdio" && contains lsp_start_preview "unsandboxed");
    Pave.Workspace_lsp.start tool_context.lsp_manager
      ~owner:"tools-test-session" ~root ~program:"/usr/bin/example-lsp"
      ~args:["--stdio"] ~execution_approved:true;
    directory "other";
    create "other/Second.swift" "old second\n";
    directory ".pave";
    directory ".pave/rules";
    create ".pave/rules/app.md"
      "---\npaths: App.swift\n---\nAPP-SCOPE-ONLY instruction\n";
    create ".pave/rules/second.md"
      "---\npaths: other/Second.swift\n---\nSECOND-SCOPE-ONLY instruction\n";
    let original = Pave.Workspace_edit.read_snapshot ~root ~path:"App.swift" in
    let second_original = Pave.Workspace_edit.read_snapshot ~root
      ~path:"other/Second.swift" in
    let proposed = "Renamed\n" and second_proposed = "changed second\n" in
    let preview_file path original content = `Assoc [
      "path", `String path;
      "original_sha256", `String original.Pave.Workspace_edit.sha256;
      "result_sha256", `String (Pave.Workspace_edit.sha256 content);
      "content", `String content;
      "changed", `Bool true
    ] in
    let preview_files = `List [
      preview_file "App.swift" original proposed;
      preview_file "other/Second.swift" second_original second_proposed
    ] in
    let preview_id = Pave.Workspace_lsp.store_edit_preview
      tool_context.lsp_manager ~owner:"tools-test-session" ~root
      ~program:"/usr/bin/example-lsp" ~arguments:["--stdio"]
      ~title:"Rename App symbol" preview_files in
    let apply_request = approval_case ~context:tool_context "lsp"
      ["action", `String "apply_preview";
       "program", `String "/usr/bin/example-lsp";
       "arguments", `List [`String "--stdio"];
       "preview_id", `String preview_id] Pave.Approval.Write in
    let apply_preview = String.concat "\n"
      (apply_request.impact :: apply_request.details) in
    assert (contains apply_preview "Rename App symbol" &&
      contains apply_preview "App.swift" &&
      contains apply_preview "other/Second.swift" &&
      contains apply_preview original.sha256 &&
      contains apply_preview (Pave.Workspace_edit.sha256 proposed) &&
      contains apply_preview "Exact proposed contents:\nRenamed\n" &&
      contains apply_preview "Exact proposed contents:\nchanged second\n");
    let lsp_read = Pave.Tools.approval_decision ~command_patterns:[]
      ~name:"lsp" ~args:(`Assoc [
        "action", `String "rename";
        "program", `String "/usr/bin/example-lsp";
        "path", `String "App.swift"
      ]) in
    assert (lsp_read.tier = Pave.Approval.Read &&
      not (Pave.Tools.requires_explicit_approval ~name:"lsp"
        ~args:(`Assoc ["action", `String "rename"])));
    let scoped_provider : Pave.Provider.config = {
      endpoint = ""; api_key = ""; model = "";
      api = Pave.Provider.Openai_completions
    } in
    let scoped_agent = Pave.Agent.create ~provider:scoped_provider ~root
      ~system:"scope test" ~workspace_context:tool_context
      ~on_event:(fun _ -> ()) () in
    let scoped_call : Pave.Protocol.tool_call = {
      id = "lsp-apply"; name = "lsp";
      arguments = `Assoc [
        "action", `String "apply_preview";
        "program", `String "/usr/bin/example-lsp";
        "arguments", `List [`String "--stdio"];
        "preview_id", `String preview_id
      ]
    } in
    (match Pave.Agent.file_scope scoped_agent scoped_call with
     | Ok scopes ->
         assert (List.length scopes = 2);
         assert (List.exists (fun (path, text, _) ->
           path = "App.swift" && contains text "APP-SCOPE-ONLY instruction") scopes);
         assert (List.exists (fun (path, text, _) ->
           path = "other/Second.swift" &&
           contains text "SECOND-SCOPE-ONLY instruction") scopes)
     | Error message -> failwith message);
    let eval_request = approval_case ~context:tool_context "workspace_eval"
      ["language", `String "python"; "code", `String "print(1)"]
      Pave.Approval.Exec in
    assert (contains (String.concat "\n" eval_request.details) "print(1)");
    let eval_code code =
      Pave.Tools.execute ~root ~name:"workspace_eval" ~context:tool_context
        ~approved:true ~args:(`Assoc [
          "language", `String "python";
          "code", `String code;
          "timeout_seconds", `Int 10
        ]) () |> Yojson.Basic.from_string in
    let eval_output result =
      assert (Yojson.Basic.Util.member "error" result = `Null);
      Yojson.Basic.Util.member "output" result |> Yojson.Basic.Util.to_string in
    assert (contains (eval_output (eval_code
      "print(pave.tool('read_file', {'path': 'snapshot.txt'}))")) "A B");
    let bridge_denied = eval_code
      "pave.tool('read_file', {'path': 'HTTPS://example.com/'})" in
    let bridge_denied_error =
      Yojson.Basic.Util.member "error" bridge_denied |> Yojson.Basic.Util.to_string in
    if not (contains bridge_denied_error "tool bridge callback failed") then
      failwith ("workspace evaluator bridge error was unexpected: " ^ bridge_denied_error);
    ignore (Pave.Tools.execute ~root ~name:"start_process" ~context:tool_context
      ~approved:true ~args:(`Assoc [
        "id", `String "eval-bridge-job";
        "program", `String "/usr/bin/printf";
        "arguments", `List [`String "job-bridge-output"]
      ]) ());
    let job_bridge = eval_output (eval_code
      "pave.tool('process_wait', {'id': 'eval-bridge-job', 'timeout_seconds': 2})\n\
       page = pave.tool('process_output', {'id': 'eval-bridge-job'})\n\
       print('job-bridge-output' in page)") in
    assert (contains job_bridge "True");
    ignore (Pave.Tools.execute ~root ~name:"start_process" ~context:tool_context
      ~approved:true ~args:(`Assoc [
        "id", `String "eval-mutation-job";
        "program", `String "/bin/sleep";
        "arguments", `List [`String "30"]
      ]) ());
    let mutation_denied = eval_code
      "pave.tool('process_kill', {'id': 'eval-mutation-job'})" in
    let mutation_error =
      Yojson.Basic.Util.member "error" mutation_denied |> Yojson.Basic.Util.to_string in
    assert (contains mutation_error "tool bridge callback failed");
    let jobs = Pave.Tools.execute ~root ~name:"process_list" ~context:tool_context
      ~args:(`Assoc []) () in
    assert (contains jobs "eval-mutation-job · running");
    ignore (Pave.Tools.execute ~root ~name:"process_kill" ~context:tool_context
      ~approved:true ~args:(`Assoc ["id", `String "eval-mutation-job"]) ());
    let ssh_request = approval_case ~context:tool_context "ssh_open"
      ["id", `String "ssh-test"; "host", `String "host.example.org";
       "user", `String "dev"; "remote_root", `String "/workspace/project"]
      Pave.Approval.Exec in
    let ssh_preview = String.concat "\n" (ssh_request.impact :: ssh_request.details) in
    assert (contains ssh_preview "host.example.org" &&
      contains ssh_preview "/workspace/project" &&
      contains ssh_preview "no Pave OAuth/API credentials");
    let dap_start_request = approval_case ~context:tool_context "dap_start"
      ["id", `String "dap-test"; "program", `String "/usr/bin/example-dap";
       "arguments", `List [`String "--stdio"]] Pave.Approval.Exec in
    assert (contains (String.concat "\n" dap_start_request.details)
      "/usr/bin/example-dap");
    let dap_launch_request = approval_case ~context:tool_context "dap"
      ["id", `String "dap-test"; "action", `String "launch";
       "target", `String "App.swift";
       "arguments", `List [`String "--debug"]] Pave.Approval.Exec in
    assert (contains (String.concat "\n" dap_launch_request.details)
      "\"target\":\"App.swift\"");
    let token_result = Yojson.Basic.from_string
      (tool_json root "token_count"
        ["encoding", `String "cl100k_base"; "text", `String "hello world"]) in
    assert (Yojson.Basic.Util.member "token_count" token_result = `Int 2);
    assert (not (Pave.Tools.requires_explicit_approval ~name:"dap"
      ~args:(`Assoc ["action", `String "threads"])));
    assert (Pave.Tools.non_reversible_tool ~name:"workspace_eval"
      ~args:(`Assoc ["action", `String "run"]));
    assert (not (Pave.Tools.non_reversible_tool ~name:"workspace_eval"
      ~args:(`Assoc ["action", `String "reset"])));
    List.iter (fun (name, fields) ->
      let result = Pave.Tools.execute ~context:tool_context ~root ~name
        ~args:(`Assoc fields) () in
      assert (contains result "requires explicit interactive approval"))
      ["lsp_start", ["program", `String "/usr/bin/example-lsp"];
       "lsp", ["action", `String "apply_preview";
               "program", `String "/usr/bin/example-lsp";
               "arguments", `List [`String "--stdio"];
               "preview_id", `String "unknown-preview"];
       "workspace_eval", ["language", `String "python";
                          "code", `String "print(1)"];
       "ssh_open", ["id", `String "ssh-test"; "host", `String "host.example.org";
                    "user", `String "dev"; "remote_root", `String "/workspace"];
       "ssh_read", ["id", `String "missing"; "path", `String "README.md"];
       "ssh_write", ["id", `String "missing"; "path", `String "README.md";
                     "contents", `String "changed"];
       "ssh_command", ["id", `String "missing"; "program", `String "true"];
       "dap_start", ["id", `String "dap-test"; "program", `String "/usr/bin/example-dap"];
       "dap", ["id", `String "dap-test"; "action", `String "launch";
               "target", `String "App.swift"]];
    
    create "src/Folder.swift/inside.txt" "needle-001\n";
    let glob = tool root "glob" ["pattern", "**/*.swift"] in
    assert (contains glob "App.swift" && contains glob "src/Match.swift");
    assert (not (contains glob "ignored/Omit.swift"));
    assert (not (contains glob "build/Omit.swift"));
    assert (not (contains glob "escape.swift"));
    assert (contains glob "src/nested/Keep.swift");
    assert (contains glob "src/root-only.swift");
    assert (not (contains glob "src/nested/Drop.swift"));
    assert (not (List.mem "root-only.swift" (String.split_on_char '\n' glob)));
    assert (not (contains glob "#literal.swift"));
    assert (not (contains glob ".secret.swift"));
    assert (not (contains glob ".private/Visible.swift"));
    let with_hidden = tool_json root "glob"
      ["pattern", `String "**/*.swift"; "hidden", `Bool true] in
    assert (contains with_hidden ".secret.swift");
    assert (contains with_hidden ".private/Visible.swift");
    assert (not (contains with_hidden "#literal.swift"));
    assert (contains (tool root "glob" ["pattern", "src/[MR]atch.swift"]) "src/Match.swift");
    assert (not (contains (tool root "glob" ["pattern", "src/M[!a]tch.swift"]) "src/Match.swift"));
    assert (not (contains (tool root "glob" ["pattern", "*.swift"])
      "src/Folder.swift/inside.txt"));
    assert (contains (tool root "glob" ["pattern", "*.tmp"]) "keep.tmp");
    assert (not (contains (tool root "glob" ["pattern", "*.tmp"]) "hidden.tmp"));
    let matches = tool root "grep" ["pattern", "needle-[0-9][0-9][0-9]"] in
    assert (contains matches "src/Match.swift:1:needle-123");
    assert (contains matches "src/keep.log:1:needle-555");
    assert (not (contains matches "src/hidden.log"));
    assert (not (contains matches "ignored/Omit.swift"));
    assert (not (contains matches "build/Omit.swift"));
    assert (not (contains matches "outside secret"));
    assert (contains matches "src/nested/Keep.swift:1:needle-333");
    assert (not (contains matches "src/nested/Drop.swift"));
    assert (not (contains matches ".secret.swift"));
    assert (contains (tool_json root "grep"
      ["pattern", `String "needle-[0-9]+"; "hidden", `Bool true]) ".secret.swift");
    assert (rejected (fun () -> tool root "grep" ["pattern", "\\(a+\\)+"]));
    assert (rejected (fun () -> tool root "grep" ["pattern", "a*a*"]));
    assert (rejected (fun () -> tool root "grep" ["pattern", "\\1"]));
    assert (rejected (fun () -> tool root "grep" ["pattern", "["]));
    assert (rejected (fun () -> tool_json root "glob"
      ["pattern", `String "*.swift"; "hidden", `String "yes"]));
    let literal = tool root "search" ["pattern", "needle-123"] in
    assert (contains literal "src/Match.swift");
    assert (not (contains literal ".secret.swift"));
    assert (not (contains literal "src/hidden.log"));
    assert (not (contains (tool_json root "search"
      ["pattern", `String "needle-111"]) ".secret.swift"));
    assert (contains (tool_json root "search"
      ["pattern", `String "needle-111"; "hidden", `Bool true]) ".secret.swift");
    assert (not (contains (tool root "search" ["pattern", "NEEDLE-123"])
      "src/Match.swift"));
    assert (contains (tool_json root "search"
      ["pattern", `String "NEEDLE-123"; "case_sensitive", `Bool false])
      "src/Match.swift");
    let filtered = tool_json root "search"
      ["pattern", `String "needle"; "path", `String "src";
       "glob", `String "**/Keep.swift"] in
    assert (contains filtered "src/nested/Keep.swift");
    assert (not (contains filtered "src/Match.swift"));
    assert (contains (tool_json root "grep"
      ["pattern", `String "NEEDLE-[0-9]+"; "case_sensitive", `Bool false])
      "src/Match.swift");
    assert (rejected (fun () -> tool_json root "search"
      ["pattern", `String "needle"; "glob", `String "../*.swift"]));
    assert (contains (tool_json root "grep"
      ["pattern", `String "needle"; "limit", `Int 1]) "[truncated;");
    assert (contains (tool_json root "glob"
      ["pattern", `String "*.swift"; "limit", `Int 1]) "[truncated;");
    create "src/long-line.txt" (String.make 5000 'a' ^ "needle-333\n");
    assert (contains (tool root "grep" ["pattern", "needle"; "path", "src"])
      "[truncated;");
    assert (rejected (fun () -> tool root "glob" ["pattern", "*.swift"; "path", "ignored"]));
    assert (List.exists (function
      | `Assoc fields -> (match List.assoc_opt "function" fields with
          | Some (`Assoc desc) -> List.assoc_opt "name" desc = Some (`String "glob")
          | _ -> false)
      | _ -> false) Pave.Tools.definitions);
    assert (rejected (fun () -> tool_json root "read_file"
      ["path", `String "App.swift"; "unexpected", `Bool true]));
    assert (rejected (fun () -> tool_json root "read_file" []));
    assert (rejected (fun () -> tool_json root "read_file"
      ["path", `Int 7]));
    assert (rejected (fun () -> tool_json root "read_file"
      ["path", `String "App.swift"; "path", `String "missing.swift"]));
    assert (rejected (fun () -> tool_json root "glob"
      ["pattern", `String "*.swift"; "limit", `Int 501]));
    create "pages.txt" "first\nsecond\nthird\n";
    let first = tool_json root "read_file"
      ["path", `String "pages.txt"; "max_lines", `Int 1] in
    assert (contains first "first\n");
    assert (not (contains first "second\n"));
    assert (contains first "bytes: 6; lines: 1-1; next offset: 6; next line: 2; file size: 19; truncated");
    let second = tool_json root "read_file"
      ["path", `String "pages.txt"; "line", `Int 2; "max_bytes", `Int 3] in
    assert (contains second "sec");
    assert (contains second "offset: 6; bytes: 3; lines: 2-2; next offset: 9; next line: 2");
    assert (contains (tool_json root "read_file"
      ["path", `String "pages.txt"; "offset", `Int 9; "max_lines", `Int 1])
      "ond\n");
    assert (rejected (fun () -> tool_json root "read_file"
      ["path", `String "pages.txt"; "offset", `Int 3; "line", `Int 2]));
    assert (rejected (fun () -> tool_json root "read_file"
      ["path", `String "pages.txt"; "line", `Int 9]));
    create "large.txt" (String.make 70_000 'x' ^ "\nTARGET-END\n");
    let first_page = tool root "read_file" ["path", "large.txt"] in
    assert (contains first_page "next offset: 16384; next line: 1; file size: 70012; truncated");
    assert (String.length first_page < 65_536);
    let page = tool_json root "read_file"
      ["path", `String "large.txt"; "offset", `Int 69995; "max_bytes", `Int 32] in
    assert (contains page "TARGET-END");
    assert (contains page "offset: 69995; bytes: 17; lines: 1-2; next offset: 70012");
    assert (contains (tool_json root "read_file"
      ["path", `String "large.txt"; "line", `Int 2]) "TARGET-END");
    assert (rejected (fun () -> tool_json root "read_file"
      ["path", `String "large.txt"; "offset", `Int 70013]));
    create "settings.gradle.kts"
      {|rootProject.name = "workspace"
include(":docs", "feature:shared", ":missing")
include(*computedModules)
// include(":ghost")
|};
    create "gradlew" "#!/bin/sh\nexit 99\n";
    directory "ios";
    directory "ios/App.xcodeproj";
    create "ios/App.xcodeproj/project.pbxproj" "// iOS project manifest\n";
    directory "ios/App.xcodeproj/xcshareddata";
    directory "ios/App.xcodeproj/xcshareddata/xcschemes";
    create "ios/App.xcodeproj/xcshareddata/xcschemes/AppShared.xcscheme"
      "<Scheme version = \"1.7\"></Scheme>\n";
    create "ios/App.xcodeproj/xcshareddata/xcschemes/Oversized.xcscheme"
      (String.make (Pave.Workspace_path.max_write_bytes + 1) 'x');

    directory "ios/Core.xcodeproj";
    create "ios/Core.xcodeproj/project.pbxproj" "// Core project manifest\n";
    directory "ios/Core.xcodeproj/xcshareddata";
    directory "ios/Core.xcodeproj/xcshareddata/xcschemes";
    create "ios/Core.xcodeproj/xcshareddata/xcschemes/CoreShared.xcscheme"
      "<Scheme version = \"1.7\"></Scheme>\n";
    directory "ios/Workspace.xcworkspace";
    create "ios/Workspace.xcworkspace/contents.xcworkspacedata"
      {|<Workspace version = "1.0"><FileRef location = "group:../App.xcodeproj"/><FileRef location = "group:../Core.xcodeproj"/></Workspace>|};
    directory "ios/Workspace.xcworkspace/xcshareddata";
    directory "ios/Workspace.xcworkspace/xcshareddata/xcschemes";
    create "ios/Workspace.xcworkspace/xcshareddata/xcschemes/WorkspaceFlow.xcscheme"
      "<Scheme version = \"1.7\"></Scheme>\n";
    directory "ios/Private.xcodeproj";
    create "ios/Private.xcodeproj/project.pbxproj" "// Private-only project manifest\n";
    directory "ios/Private.xcodeproj/xcuserdata";
    directory "ios/Private.xcodeproj/xcuserdata/alice.xcuserdatad";
    directory "ios/Private.xcodeproj/xcuserdata/alice.xcuserdatad/xcschemes";
    create "ios/Private.xcodeproj/xcuserdata/alice.xcuserdatad/xcschemes/Personal.xcscheme"
      "<Scheme version = \"1.7\"></Scheme>\n";

    directory "packages";
    directory "packages/swift";
    create "packages/swift/Package.swift"
      {|import PackageDescription
let targets = computedTargets()
let package = Package(name: "fixture", targets: targets)
// .testTarget(name: "CommentOnlyTests", path: "CommentOnlyTests")
|};
    directory "packages/swift/Tests";
    directory "packages/swift/Tests/FixtureTests";
    directory "packages/flutter";
    create "packages/flutter/pubspec.yaml"
      "name: sample\ndependencies:\n  flutter:\n    sdk: flutter\nflutter:\n  plugin:\n    platforms:\n      ios:\n      android:\n";
    directory "packages/flutter/ios";
    directory "packages/flutter/android";
    create "packages/flutter/android/settings.gradle" "include ':app'\n";
    directory "packages/flutter/ios/Runner.xcodeproj";
    create "packages/flutter/ios/Runner.xcodeproj/project.pbxproj"
      "// Flutter-owned native host\n";
    directory "packages/flutter/ios/Runner.xcodeproj/xcshareddata";
    directory "packages/flutter/ios/Runner.xcodeproj/xcshareddata/xcschemes";
    create "packages/flutter/ios/Runner.xcodeproj/xcshareddata/xcschemes/Runner.xcscheme"
      "<Scheme/>\n";
    directory "packages/dart";
    create "packages/dart/pubspec.yaml"
      "name: dart_only\n# flutter:\ndescription: flutter: not a dependency\n";
    directory "packages/app";
    create "packages/app/pubspec.yaml"
      "name: app\ndependencies:\n  flutter:\n    sdk: flutter\n";
    directory "packages/app/android";
    directory "packages/library";
    create "packages/library/pubspec.yaml"
      "name: library\ndependencies:\n  flutter:\n    sdk: flutter\n";
    directory "packages/react-native";
    create "packages/react-native/package.json"
      {|{"dependencies":{"react-native":"1"},"scripts":{"test":"jest"}}|};
    create "packages/react-native/yarn.lock" "# fixture\n";
    directory "packages/react-native/ios";
    directory "packages/expo";
    create "packages/expo/package.json"
      {|{"dependencies":{"expo":"52","react-native":"0.76"},"scripts":{"lint":"eslint ."}}|};
    create "packages/expo/package-lock.json" "{}";
    create "packages/expo/pnpm-lock.yaml" "lockfileVersion: '9.0'\n";
    directory "packages/expo/android";
    directory "packages/plain-node";
    create "packages/plain-node/package.json"
      {|{"description":"expo","scripts":{"test":"echo no"}}|};
    create "packages/plain-node/package-lock.json" "{}";
    directory "android";
    create "android/settings.gradle.kts"
      {|rootProject.name = "nested"
include(":app")
include(":feature:${computedName}")
if (featureEnabled)
  include(":conditional")
include(":actual")
other.include(":not-a-gradle-module")
|};
    directory "android/app";
    directory "android/app/src";
    directory "android/app/src/main";
    directory "android/actual";
    directory "android/actual/src";
    directory "android/actual/src/main";
    directory "legacy";
    create "legacy/settings.gradle" "include ':legacy-app', ':legacy-lib:shared'\n";
    directory "legacy/legacy-app";
    directory "legacy/legacy-app/src";
    directory "legacy/legacy-app/src/test";
    directory "docs";
    directory "docs/src";
    directory "docs/src/main";
    directory "feature";
    directory "feature/shared";
    directory "feature/shared/src";
    directory "feature/shared/src/androidTest";
    directory "Pods";
    directory "Pods/Hidden.xcodeproj";
    create "Pods/Hidden.xcodeproj/project.pbxproj" "// generated dependency\n";
    directory "build/Generated.xcodeproj";
    create "build/Generated.xcodeproj/project.pbxproj" "// generated project\n";
    create ".gitignore" "vendor/\n";
    directory "vendor";
    directory "vendor/Ignored.xcodeproj";
    create "vendor/Ignored.xcodeproj/project.pbxproj" "// ignored project\n";
    let outside_channel = open_out outside_manifest in
    output_string outside_channel "// outside Swift package\n";
    close_out outside_channel;
    let link = Filename.concat root "linked-mobile" in
    Unix.symlink outside_mobile link;
    files := link :: !files;
    directory "large";
    create "large/Package.swift"
      (String.make (Pave.Workspace_path.max_write_bytes + 1) 'x');
    let groovy_modules, groovy_unresolved =
      Pave.Tools.gradle_included_modules
        "include ':legacy-app', ':legacy-lib:shared'\n" in
    if groovy_modules <> ["legacy-app"; "legacy-lib/shared"] ||
       groovy_unresolved then
      failwith (Printf.sprintf "static Groovy includes parsed as [%s], unresolved=%b"
        (String.concat ", " groovy_modules) groovy_unresolved);
    let mobile = tool root "mobile_project" [] in
    if not (contains mobile "Xcode project: ios/App.xcodeproj/project.pbxproj")
    then failwith ("mobile inventory missed the iOS manifest:\n" ^ mobile);
    assert (not (contains mobile
      "cd 'ios' && xcodebuild -list -project 'App.xcodeproj'"));
    assert (contains mobile
      "Candidate shared scheme: AppShared (ios/App.xcodeproj/xcshareddata/xcschemes/AppShared.xcscheme)");
    assert (contains mobile (Printf.sprintf
      "Ignored oversized Xcode shared scheme: ios/App.xcodeproj/xcshareddata/xcschemes/Oversized.xcscheme (exceeds %d-byte limit; no scheme commands suggested)."
      Pave.Workspace_path.max_write_bytes));
    assert (not (contains mobile "Candidate shared scheme: Oversized"));
    assert (not (contains mobile "-scheme 'Oversized'"));

    assert (not (contains mobile
      "cd 'ios' && xcodebuild -project 'App.xcodeproj' -scheme 'AppShared' build"));
    let selected_xcode = tool root "mobile_project"
      ["subroot", "ios/App.xcodeproj"; "platform", "ios"] in
    assert (contains selected_xcode
      "Use separately approved xcode_preflight schemes, destinations, then build/test");
    assert (not (contains selected_xcode "xcodebuild -project"));
    assert (contains mobile
      "Xcode workspace: ios/Workspace.xcworkspace/contents.xcworkspacedata");
    let selected_workspace = tool root "mobile_project"
      ["subroot", "ios/Workspace.xcworkspace"; "platform", "ios"] in
    assert (contains selected_workspace
      "Candidate shared scheme: WorkspaceFlow");
    assert (not (contains selected_workspace "xcodebuild -workspace"));
    assert (not (contains selected_workspace
      "cd 'ios' && xcodebuild -project 'Core.xcodeproj' -scheme 'CoreShared' build"));
    assert (not (contains mobile
      "cd 'ios' && xcodebuild -project 'Core.xcodeproj' -scheme 'AppShared'"));
    assert (not (contains mobile
      "cd 'ios' && xcodebuild -workspace 'Workspace.xcworkspace' -scheme 'AppShared'"));
    assert (contains mobile
      "Candidate shared schemes: none found; private/user schemes remain unknown.");
    assert (not (contains mobile "Personal.xcscheme"));
    assert (not (contains mobile "-scheme 'Personal'"));
    assert (not (contains mobile "<scheme-from-list>"));

    assert (contains mobile "Android Gradle settings: settings.gradle.kts");
    assert (contains mobile "Gradle wrapper script: gradlew");
    assert (contains mobile "Declared module: :docs");
    assert (contains mobile "Candidate source root: docs/src/main");
    assert (contains mobile "Declared module: :feature:shared");
    assert (contains mobile "Candidate source root: feature/shared/src/androidTest");
    assert (contains mobile "Declared module: :missing");
    assert (contains mobile "Conventional module directory not found: missing");
    assert (contains mobile "Unresolved dynamic or unsupported module include");
    assert (contains mobile "Task names, variants, projectDir remapping and SDK readiness remain unknown.");
    assert (not (contains mobile "assembleDebug") &&
      not (contains mobile "gradle tasks"));
    assert (contains mobile "Android Gradle settings: android/settings.gradle.kts");
    assert (contains mobile
      "Gradle wrapper: not found beside settings; system Gradle availability is unknown.");
    assert (contains mobile "Declared module: :app");
    assert (not (contains mobile "Declared module: :feature:computedName"));
    assert (contains mobile "Declared module: :actual");
    assert (contains mobile "Candidate source root: android/actual/src/main");
    let unexpected_modules = ["conditional"; "not-a-gradle-module"]
      |> List.filter (fun name ->
        contains mobile ("Declared module: :" ^ name)) in
    if unexpected_modules <> [] then
      failwith ("Gradle parser inferred guarded or method includes: " ^
        String.concat ", " unexpected_modules);
    assert (contains mobile "Android Gradle settings: legacy/settings.gradle");
    if not (contains mobile "Declared module: :legacy-app") then
      failwith ("Gradle Groovy include missing from mobile map:\n" ^ mobile);
    assert (contains mobile "Candidate source root: legacy/legacy-app/src/test");
    assert (contains mobile "Swift Package Manager: packages/swift/Package.swift");
    assert (contains mobile "Package root: packages/swift");
    assert (contains mobile "Candidate test root: packages/swift/Tests");
    assert (contains mobile "Candidate test root: packages/swift/Tests/FixtureTests");
    assert (contains mobile "Computed/unsupported test targets and SDK requirements remain unknown.");
    assert (not (contains mobile "CommentOnlyTests") &&
      not (contains mobile "computedTargets"));
    assert (not (contains mobile "swift build") &&
      not (contains mobile "swift test"));
    assert (contains mobile "Flutter plugin: packages/flutter/pubspec.yaml");
    assert (contains mobile "Existing ios host root: packages/flutter/ios");
    assert (contains mobile "Existing android host root: packages/flutter/android");
    assert (contains mobile "Flutter app: packages/app/pubspec.yaml");
    assert (contains mobile "Existing android host root: packages/app/android");
    assert (contains mobile "Flutter package: packages/library/pubspec.yaml");
    assert (contains mobile "Dart package: packages/dart/pubspec.yaml");
    assert (not (contains mobile "Flutter app: packages/dart"));
    assert (not (contains mobile "flutter build"));
    assert (contains mobile "React Native: packages/react-native/package.json");
    assert (contains mobile "Lockfile: packages/react-native/yarn.lock");
    assert (contains mobile "Package manager: yarn");
    assert (contains mobile "Existing ios host root: packages/react-native/ios");
    assert (not (contains mobile "cd 'packages/react-native' && yarn 'test'"));
    let selected_rn = tool root "mobile_project"
      ["subroot", "packages/react-native"; "platform", "android"] in
    assert (contains selected_rn "cd 'packages/react-native' && yarn 'test'");
    assert (not (contains selected_rn "xcodebuild -project"));
    let host = tool root "mobile_project"
      ["subroot", "packages/flutter/ios/Runner.xcodeproj";
       "platform", "ios"] in
    assert (contains host "Flutter plugin: packages/flutter/pubspec.yaml");
    assert (contains host
      "Xcode project: packages/flutter/ios/Runner.xcodeproj/project.pbxproj");
    assert (not (contains host "xcodebuild -project"));
    assert (not (contains selected_xcode "Runner.xcodeproj' -scheme"));
    assert (rejected (fun () -> tool root "mobile_project"
      ["subroot", "packages/flutter/ios/Runner.xcodeproj"]));
    assert (rejected (fun () -> tool root "mobile_project"
      ["subroot", "../outside"; "platform", "ios"]));
    let mismatched = tool root "mobile_project"
      ["subroot", "ios/App.xcodeproj"; "platform", "android"] in
    assert (contains mismatched "No mobile stack matched selected subroot");
    assert (not (contains mismatched "xcodebuild -project"));
    let flutter_android_host = tool root "mobile_project"
      ["subroot", "packages/flutter/android"; "platform", "android"] in
    assert (contains flutter_android_host
      "Android Gradle settings: packages/flutter/android/settings.gradle");
    assert (not (contains flutter_android_host "yarn 'test'"));
    assert (contains selected_rn "React Native: packages/react-native/package.json");
    let cancelled_inventory = try
      ignore (Pave.Tools.mobile_project ~cancel:(fun () -> true) root
        (`Assoc ["subroot", `String "ios/App.xcodeproj";
                 "platform", `String "ios"]));
      false
    with Pave.Tools.Cancelled -> true in
    assert cancelled_inventory;
    assert (contains mobile "Expo: packages/expo/package.json");
    assert (contains mobile "Lockfile: packages/expo/package-lock.json");
    assert (contains mobile "Lockfile: packages/expo/pnpm-lock.yaml");
    assert (contains mobile "Conflicting lockfiles: choose a package manager explicitly");
    assert (contains mobile "Declared script: lint");
    assert (contains mobile "Existing android host root: packages/expo/android");
    assert (not (contains mobile "npm run 'lint'"));
    assert (not (contains mobile "pnpm 'lint'"));
    assert (not (contains mobile "pnpm 'test'"));
    assert (not (contains mobile "npx expo"));
    assert (not (contains mobile "plain-node/package.json"));
    assert (not (contains mobile "yarn 'build'"));
    assert (Pave.Workspace_xcode.schemes
      {|{"workspace":{"schemes":["Flow","Review"]}}|} =
      ["Flow"; "Review"]);
    assert (try ignore (Pave.Workspace_xcode.schemes
      {|{"project":{"schemes":["Flow","Flow"]}}|}); false
      with Pave.Workspace_xcode.Error _ -> true);
    assert (Pave.Workspace_xcode.destinations
      "Available destinations:\n  { platform:iOS Simulator, id:12345678-1234-1234-1234-123456789abc, name:Phone }\nIneligible destinations:\n  { platform:iOS Simulator, id:aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa, error:runtime }\n" =
      ["12345678-1234-1234-1234-123456789abc"]);
    let xcode_args action more = `Assoc
      (["action", `String action;
        "subroot", `String "ios/App.xcodeproj"] @ more) in
    let xcode ?(approved = false) action more =
      Pave.Tools.execute ~root ~context:tool_context ~approved
        ~name:"xcode_preflight" ~args:(xcode_args action more) () in
    let scheme = ["scheme", `String "AppShared"] in
    let destination =
      ["destination", `String "12345678-1234-1234-1234-123456789abc"] in
    assert (contains (xcode "schemes" []) "explicit interactive approval");
    assert (contains (xcode ~approved:true "destinations" scheme)
      "approve scheme discovery");
    assert (contains (xcode ~approved:true "build" (scheme @ destination))
      "approve scheme discovery");
    assert (Pave.Tools.requires_explicit_approval
      ~name:"xcode_preflight" ~args:(xcode_args "schemes" []));
    let xcode_preview = Pave.Tools.approval_request ~root
      ~name:"xcode_preflight" ~args:(xcode_args "schemes" [])
      (Pave.Tools.approval_decision ~command_patterns:[]
        ~name:"xcode_preflight" ~args:(xcode_args "schemes" [])) in
    assert (contains (String.concat "\n" xcode_preview.details)
      "xcodebuild -project 'App.xcodeproj' -list -json");
    let prior_path = Sys.getenv_opt "PATH" in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" (Option.value prior_path ~default:"")) (fun () ->
      Unix.putenv "PATH" (Filename.concat root "missing-xcode");
      assert (contains (xcode ~approved:true "schemes" [])
        "Xcode scheme discovery: exit 127");
      assert (contains (xcode ~approved:true "destinations" scheme)
        "approve scheme discovery"));
    directory "fake-xcode-bin";
    create "fake-xcode-bin/xcodebuild" {|#!/bin/sh
case " $* " in
  *" -list -json "*) printf '%s\n' '{"project":{"schemes":["AppShared"]}}' ;;
  *" -showdestinations "*) printf '%s\n' 'Available destinations for the \"AppShared\" scheme:' '  { platform:iOS Simulator, id:12345678-1234-1234-1234-123456789abc, OS:17.0, name:iPhone }' 'Ineligible destinations for the \"AppShared\" scheme:' '  { platform:iOS Simulator, id:aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa, error:missing runtime }' ;;
  *" build "*) printf '%s\n' 'fixture build failed'; exit 7 ;;
  *) printf '%s\n' 'unexpected xcodebuild invocation'; exit 8 ;;
esac
|};
    Unix.chmod (Filename.concat root "fake-xcode-bin/xcodebuild") 0o700;
    let saved_path = Sys.getenv_opt "PATH" in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" (Option.value saved_path ~default:"")) (fun () ->
      Unix.putenv "PATH" (Filename.concat root "fake-xcode-bin" ^ ":" ^
        Option.value saved_path ~default:"/usr/bin:/bin");
      assert (contains (xcode ~approved:true "schemes" [])
        "Verified schemes: AppShared");
      assert (contains (xcode ~approved:true "destinations"
        ["scheme", `String "Wrong"]) "scheme was not discovered");
      assert (contains (xcode ~approved:true "destinations" scheme)
        "Available iOS Simulator IDs: 12345678-1234-1234-1234-123456789abc");
      assert (contains (xcode ~approved:true "build"
        (scheme @ ["destination", `String
          "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"]))
        "destination was not discovered");
      let build = xcode ~approved:true "build" (scheme @ destination) in
      assert (contains build "Xcode build: exit 7" &&
        contains build "fixture build failed");
      create "ios/App.xcodeproj/project.pbxproj" "// changed Xcode manifest\n";
      assert (contains (xcode ~approved:true "build" (scheme @ destination))
        "manifest changed since discovery");
      assert (contains (xcode ~approved:true "destinations" scheme)
        "approve scheme discovery"));
    directory "focus";
    directory "focus/node";
    create "focus/node/package.json"
      {|{"dependencies":{"react-native":"1"},"scripts":{"test":"node -e 'process.stdout.write(\"mobile-script-ok\\n\")'","lint":"node -e 'process.exit(4)'"}}|};
    create "focus/node/package-lock.json" "{}";
    let mobile stack action subroot extra approved =
      Pave.Tools.execute ~root ~context:tool_context ~approved
        ~name:"mobile_check" ~args:(`Assoc
          (["stack", `String stack; "action", `String action;
            "subroot", `String subroot] @ extra)) () in
    assert (Pave.Tools.is_shell_tool "mobile_check");
    assert (Pave.Tools.requires_explicit_approval ~name:"mobile_check"
      ~args:(`Assoc ["stack", `String "node"; "action", `String "test";
        "subroot", `String "focus/node"]));
    assert (contains (mobile "node" "test" "focus/node" [] false)
      "explicit interactive approval");
    let node_preview = Pave.Tools.approval_request ~root
      ~name:"mobile_check"
      ~args:(`Assoc ["stack", `String "node"; "action", `String "test";
        "subroot", `String "focus/node"])
      (Pave.Tools.approval_decision ~command_patterns:[]
        ~name:"mobile_check" ~args:(`Assoc [])) in
    assert (contains (String.concat "\n" node_preview.details) "npm run test");
    let node_result = mobile "node" "test" "focus/node" [] true in
    assert (contains node_result "exit 0" &&
      contains node_result "mobile-script-ok");
    assert (contains (mobile "node" "lint" "focus/node" [] true) "exit 4");
    directory "focus/swift";
    create "focus/swift/Package.swift"
      "// swift-tools-version: 6.0\nimport PackageDescription\nlet package = Package(name: \"Fixture\", targets: [.testTarget(name: \"FixtureTests\")])\n";
    directory "focus/bin";
    create "focus/bin/swift" {|#!/bin/sh
case " $* " in
  *" list "*) printf '%s\n' 'FixtureTests.TestCase/testWorks()' ;;
  *" --filter "*) printf '%s\n' 'selected-test-ok' ;;
  *) exit 9 ;;
esac
|};
    Unix.chmod (Filename.concat root "focus/bin/swift") 0o700;
    let path_before = Sys.getenv_opt "PATH" in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" (Option.value path_before ~default:"")) (fun () ->
      Unix.putenv "PATH" (Filename.concat root "focus/bin" ^ ":" ^
        Option.value path_before ~default:"/usr/bin:/bin");
      let target = ["target", `String "FixtureTests.TestCase/testWorks()"] in
      assert (contains (mobile "swiftpm" "run" "focus/swift" target true)
        "approve focused task discovery");
      assert (contains (mobile "swiftpm" "discover" "focus/swift" [] true)
        "FixtureTests.TestCase/testWorks()");
      assert (contains (mobile "swiftpm" "run" "focus/swift"
        ["target", `String "Undiscovered/test()"] true)
        "not in the approved discovery");
      assert (contains (mobile "swiftpm" "run" "focus/swift" target true)
        "selected-test-ok");
      create "focus/swift/Package.swift"
        "// swift-tools-version: 6.0\nimport PackageDescription\nlet package = Package(name: \"Changed\", targets: [.testTarget(name: \"FixtureTests\")])\n";
      assert (contains (mobile "swiftpm" "run" "focus/swift" target true)
        "manifest changed since task discovery"));
    directory "many";
    for index = 0 to 100 do
      let project = Printf.sprintf "many/Project%03d.xcodeproj" index in
      directory project;
      create (project ^ "/project.pbxproj") "// project manifest\n"
    done;
    let truncated_mobile = tool root "mobile_project" [] in
    assert (contains truncated_mobile
      "Android Gradle settings: android/settings.gradle.kts");
    assert (contains truncated_mobile
      "Xcode project: ios/App.xcodeproj/project.pbxproj");
    assert (contains truncated_mobile "[truncated; narrow the workspace and retry]");
    assert (String.length truncated_mobile <= Pave.Workspace_path.max_read_bytes);
    let command = tool root "run_command" ["command", "printf 'command-ok\\n'"] in
    assert (contains command "Status: exit 0");
    assert (contains command "command-ok");
    let timeout = Pave.Tools.execute ~root ~name:"run_command"
      ~args:(`Assoc ["command", `String "sleep 3"; "timeout_seconds", `Int 1]) () in
    assert (contains timeout "Status: timed out");
    let started = Unix.gettimeofday () in
    let cancelled = try
      ignore (Pave.Tools.execute
        ~cancel:(fun () -> Unix.gettimeofday () -. started > 0.1)
        ~root ~name:"run_command"
        ~args:(`Assoc ["command", `String "sleep 5";
          "timeout_seconds", `Int 20]) ());
      false
    with Pave.Tools.Cancelled -> true in
    assert (cancelled && Unix.gettimeofday () -. started < 2.);
    let ic = open_in outside in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
      assert (input_line ic = "outside secret")));
  print_endline "workspace tools: ok"
