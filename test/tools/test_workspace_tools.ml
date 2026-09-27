module Tools = Pave.Tools
module Process = Pave.Workspace_process

let fail label = failwith ("workspace tools integration: " ^ label)
let expect label condition = if not condition then fail label

let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec search index =
    index + m <= n &&
    (String.sub text index m = fragment || search (index + 1))
  in
  search 0

let run program arguments =
  let result = Process.run ~timeout_seconds:10 ~output_limit:65_536
      ~program ~arguments () in
  match result.Process.termination with
  | Process.Exited 0 -> result.output
  | _ -> fail (Printf.sprintf "%s failed: %s" program result.output)

let write path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let rec remove_tree path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path
        |> Array.iter (fun name -> remove_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Unix.unlink path
  with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let tool ?context ?(approved = false) ~root name fields =
  Tools.execute ?context ~approved ~root ~name ~args:(`Assoc fields) ()

let () =
  let root = Filename.temp_file "pave-workspace-tools-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let worktree_path = Filename.temp_file "pave-workspace-tools-worktree-" "" in
  Sys.remove worktree_path;
  let manager = Process.create_manager () in
  let other_manager = Process.create_manager () in
  let owner = "workspace-tools-owner" in
  let context = {
    Tools.owner = owner;
    process_manager = manager;
    read_artifact = (fun id ->
      if id = "private-fixture" then Some "owned artifact text\n" else None);
  } in
  let other_context = {
    Tools.owner = "different-owner";
    process_manager = other_manager;
    read_artifact = (fun _ -> None);
  } in
  Fun.protect
    ~finally:(fun () ->
      (try Process.close_manager manager with _ -> ());
      (try Process.close_manager other_manager with _ -> ());
      remove_tree worktree_path;
      remove_tree root)
    (fun () ->
      write (Filename.concat root "notes.txt") "workspace source\n";
      let local = tool ~context ~root "read_file"
        ["path", `String "local://notes.txt"; "max_lines", `Int 1] in
      expect "local URI reaches the workspace reader"
        (contains local "workspace source");
      expect "an exact final line does not get an omission marker"
        (not (contains local "[more lines omitted]"));
      let artifact = tool ~context ~root "read_file"
        ["path", `String "artifact://private-fixture"] in
      expect "artifact reads use the session callback"
        (contains artifact "owned artifact text");
      let missing_artifact = tool ~context ~root "read_file"
        ["path", `String "artifact://not-owned"] in
      expect "unowned artifacts fail closed" (contains missing_artifact "not owned");
      let no_context = tool ~root "process_list" [] in
      expect "process tools require a private session context"
        (contains no_context "private saved session");
      let invalid_environment = tool ~context ~root "start_process" [
        "id", `String "invalid-env";
        "program", `String "/usr/bin/printf";
        "environment", `Assoc ["PAVE_TEST", `Int 1]] in
      expect "nested environment schema is checked before execution"
        (contains invalid_environment "must have JSON type string");

      let denied_start = tool ~context ~root "start_process" [
        "id", `String "literal";
        "program", `String "/usr/bin/printf";
        "arguments", `List [`String "%s"; `String "literal;$(touch should-not-exist)\n"]] in
      expect "process start requires explicit approval"
        (contains denied_start "explicit interactive approval");
      let started = tool ~context ~approved:true ~root "start_process" [
        "id", `String "literal";
        "program", `String "/usr/bin/printf";
        "arguments", `List [`String "%s"; `String "literal;$(touch should-not-exist)\n"]] in
      expect "approved direct executable starts" (contains started "Started process job");
      let waited = tool ~context ~root "process_wait"
        ["id", `String "literal"; "timeout_seconds", `Int 5] in
      expect "managed process reaches exit" (contains waited "exit 0");
      let output = tool ~context ~root "process_output" ["id", `String "literal"] in
      expect "argv is not shell-evaluated" (contains output "literal;$(touch should-not-exist)");
      expect "literal shell metacharacters did not execute"
        (not (Sys.file_exists (Filename.concat root "should-not-exist")));
      expect "process output exposes absolute paging metadata"
        (contains output "output page offset 0; earliest retained 0; next");
      let ready_start = tool ~context ~approved:true ~root "start_process" [
        "id", `String "ready"; "program", `String "/usr/bin/printf";
        "arguments", `List [`String "PAVE_READY\n"]] in
      expect "readiness fixture started" (contains ready_start "Started process job");
      let ready = tool ~context ~root "process_ready" [
        "id", `String "ready"; "log_regex", `String "PAVE_READY";
        "timeout_seconds", `Int 2] in
      expect "log readiness detects retained output" (contains ready "is ready");

      let cat = tool ~context ~approved:true ~root "start_process" [
        "id", `String "input"; "program", `String "/bin/cat"] in
      expect "interactive process starts" (contains cat "Started process job");
      expect "stdin requires explicit approval"
        (contains (tool ~context ~root "process_stdin"
          ["id", `String "input"; "data", `String "input-data\n"])
          "explicit interactive approval");
      let wrote = tool ~context ~approved:true ~root "process_stdin"
        ["id", `String "input"; "data", `String "input-data\n"] in
      expect "approved stdin is written" (contains wrote "Wrote 11 bytes");
      expect "stdin close requires explicit approval"
        (contains (tool ~context ~root "process_close_stdin" ["id", `String "input"])
          "explicit interactive approval");
      ignore (tool ~context ~approved:true ~root "process_close_stdin" ["id", `String "input"]);
      let input_wait = tool ~context ~root "process_wait"
        ["id", `String "input"; "timeout_seconds", `Int 5] in
      expect "stdin close terminates cat" (contains input_wait "exit 0");
      let input_output = tool ~context ~root "process_output" ["id", `String "input"] in
      expect "stdin arrives at child" (contains input_output "input-data");

      let killed = tool ~context ~approved:true ~root "start_process" [
        "id", `String "kill-me"; "program", `String "/bin/sleep";
        "arguments", `List [`String "30"]] in
      expect "kill fixture started" (contains killed "Started process job");
      expect "process kill requires explicit approval"
        (contains (tool ~context ~root "process_kill" ["id", `String "kill-me"])
          "explicit interactive approval");
      let killed = tool ~context ~approved:true ~root "process_kill"
        ["id", `String "kill-me"] in
      expect "approved process kill cancels the job" (contains killed "cancelled");

      ignore (run "/usr/bin/git" ["-C"; root; "init"; "-q"; "-b"; "main"]);
      List.iter (fun (key, value) ->
        ignore (run "/usr/bin/git" ["-C"; root; "config"; key; value])) [
          "user.name", "Pave Test";
          "user.email", "pave@example.invalid";
          "core.hooksPath", "/dev/null";
          "commit.gpgsign", "false";
          "core.excludesFile", "/dev/null";
        ];
      write (Filename.concat root "seed.txt") "base commit\n";
      ignore (run "/usr/bin/git" ["-C"; root; "add"; "--"; "seed.txt"]);
      ignore (run "/usr/bin/git" ["-C"; root; "commit"; "-qm"; "initial"]);
      let no_approval = tool ~context ~root "worktree_create" [
        "id", `String "tool-worktree"; "path", `String worktree_path;
        "branch", `String "tool/worktree"] in
      expect "worktree creation requires explicit approval"
        (contains no_approval "explicit approval");
      let created = tool ~context ~approved:true ~root "worktree_create" [
        "id", `String "tool-worktree"; "path", `String worktree_path;
        "branch", `String "tool/worktree"] in
      expect "approved worktree is created" (contains created "Worktree tool-worktree");
      let listed = tool ~context ~root "worktree_list" [] in
      expect "owner sees the managed worktree" (contains listed "tool-worktree");
      let hidden = tool ~context:other_context ~root "worktree_list" [] in
      expect "another session cannot list the worktree" (not (contains hidden "tool-worktree"));
      let other_read = tool ~context:other_context ~root "read_file"
        ["path", `String "worktree://tool-worktree/seed.txt"] in
      expect "another session cannot read the worktree" (contains other_read "belongs to");
      let status = tool ~context ~root "worktree_status"
        ["id", `String "tool-worktree"] in
      expect "owner can inspect worktree status" (contains status "Worktree tool-worktree status");
      let uri = "worktree://tool-worktree/edited.txt" in
      let wrote = tool ~context ~root "write_file"
        ["path", `String uri; "content", `String "worktree edit\n"] in
      expect "worktree URI writes resolve to the owned worktree" (contains wrote "Wrote");
      let read_back = tool ~context ~root "read_file" ["path", `String uri] in
      expect "worktree URI reads resolve to the owned worktree" (contains read_back "worktree edit");
      expect "commit requires explicit approval"
        (contains (tool ~context ~root "worktree_commit" [
          "id", `String "tool-worktree";
          "paths", `List [`String "edited.txt"];
          "message", `String "tool commit"])
          "explicit approval");
      let committed = tool ~context ~approved:true ~root "worktree_commit" [
        "id", `String "tool-worktree";
        "paths", `List [`String "edited.txt"];
        "message", `String "tool commit"] in
      expect ("approved selected-path commit succeeds: " ^ committed)
        (contains committed "exit 0" && contains committed "Committed paths:\nedited.txt");
      let history = tool ~context ~root "worktree_history"
        ["id", `String "tool-worktree"; "count", `Int 5] in
      expect "worktree history is readable" (contains history "tool commit");
      expect "clean worktree removal requires explicit approval"
        (contains (tool ~context ~root "worktree_remove" ["id", `String "tool-worktree"])
          "explicit interactive approval");
      let removed = tool ~context ~approved:true ~root "worktree_remove"
        ["id", `String "tool-worktree"] in
      expect "approved clean worktree removal succeeds" (contains removed "exit 0");
      expect "removed worktree is no longer owned"
        (contains (tool ~context ~root "worktree_list" []) "No managed worktrees");

      let url = "https://example.invalid/document.txt" in
      let url_args = `Assoc ["path", `String url] in
      let decision = Tools.approval_decision ~command_patterns:[] ~name:"read_file"
        ~args:url_args in
      let request = Tools.approval_request ~root ~name:"read_file"
        ~args:url_args decision in
      expect "public HTTPS read is classified for explicit approval"
        (Tools.requires_explicit_approval ~name:"read_file" ~args:url_args &&
         contains request.impact "without credentials" &&
         List.exists (fun detail -> contains detail url) request.details))
