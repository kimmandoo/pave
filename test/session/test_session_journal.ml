let message text = Pave.Protocol.user text

let () =
  let dir = Filename.temp_file "pave-journal-" "" in
  Sys.remove dir; Unix.mkdir dir 0o700;
  let path = Filename.concat dir "session.jsonl" in
  let fork_path = Filename.concat dir "fork.jsonl" in
  let metadata_path = Filename.concat dir "metadata.jsonl" in
  let metadata_fork = Filename.concat dir "metadata-fork.jsonl" in
  Fun.protect ~finally:(fun () ->
    (try Sys.remove fork_path with Sys_error _ -> ());
    (try Sys.remove metadata_fork with Sys_error _ -> ());
    (try Sys.remove metadata_path with Sys_error _ -> ());
    (try Sys.remove path with Sys_error _ -> ());
    Unix.rmdir dir) (fun () ->
    let journal = Pave.Session.open_file path in
    let base = Pave.Session.append journal (message "base") in
    assert (Pave.Session.retry_candidate journal = None);
    let abandoned = Pave.Session.append journal (message "abandoned") in
    Pave.Session.branch journal base;
    assert (Pave.Session.history journal = [ message "base" ]);
    let selected = Pave.Session.append journal (message "selected") in
    assert (Pave.Session.history journal = [ message "base"; message "selected" ]);
    assert (List.length (Pave.Session.entries journal) = 4);
    let reopened = Pave.Session.open_file path in
    assert (Pave.Session.history reopened = [ message "base"; message "selected" ]);
    Pave.Session.branch reopened abandoned;
    let reread = Pave.Session.open_file path in
    assert (Pave.Session.history reread = [ message "base"; message "abandoned" ]);
    assert (Pave.Session.leaf_id reread = Some abandoned);
    let fork = Pave.Session.fork reread fork_path in
    ignore (Pave.Session.append fork (message "fork-only"));
    assert (Pave.Session.history (Pave.Session.open_file path) =
      [ message "base"; message "abandoned" ]);
    assert (Pave.Session.history (Pave.Session.open_file fork_path) =
      [ message "base"; message "abandoned"; message "fork-only" ]);
    let call : Pave.Protocol.tool_call = { id = "call-1"; name = "read_file";
      arguments = `Assoc [ "path", `String "App.swift" ] } in
    (match Pave.Session.branch journal selected with
     | exception Failure message ->
         assert (message = "session changed on disk; reopen before writing")
     | _ -> failwith "stale session writer was not rejected");
    let current = Pave.Session.open_file path in
    Pave.Session.branch current selected;
    let pending_id = Pave.Session.append current { role = "assistant"; content = None;
      tool_calls = [ call ]; tool_call_id = None; provider_state = None } in
    let recovered = Pave.Session.open_file path in
    (match List.rev (Pave.Session.history recovered) with
     | result :: _ ->
         assert (result.role = "tool");
         assert (result.tool_call_id = Some "call-1")
     | [] -> assert false);
    Pave.Session.branch recovered pending_id;
    assert (List.length (Pave.Session.history recovered) = 4);
    assert (Pave.Session.history (Pave.Session.open_file path) =
      Pave.Session.history recovered);
    let metadata = Pave.Session.open_file metadata_path in
    Pave.Session.set_model metadata ~provider:"openai" ~model:"gpt-6-sol";
    Pave.Session.set_model metadata ~provider:"openai" ~model:"gpt-6-sol";
    assert (List.length (Pave.Session.entries metadata) = 1);
    let first = Pave.Session.append metadata (message "first") in
    Pave.Session.set_model metadata ~provider:"ollama" ~model:"local";
    let second = Pave.Session.append metadata (message "second") in
    let counted : Pave.Protocol.usage =
      { input_tokens = 18; output_tokens = 7 } in
    Pave.Session.append_usage metadata ~provider:"ollama" ~model:"local" counted;
    let measured_tip = Option.get (Pave.Session.leaf_id metadata) in
    assert (Pave.Session.usage metadata = Some counted);
    assert (Pave.Session.model metadata = Some ("ollama", "local"));
    Pave.Session.branch metadata first;
    assert (Pave.Session.model metadata = Some ("openai", "gpt-6-sol"));
    assert (Pave.Session.usage metadata = None);
    assert (Pave.Session.history metadata = [message "first"]);
    let branch_marker = (List.hd (List.rev (Pave.Session.entries metadata))).id in
    let branched = Pave.Session.open_file metadata_path in
    assert (Pave.Session.model branched = Some ("openai", "gpt-6-sol"));
    assert (Pave.Session.model_at branched (Some second) =
      Some ("ollama", "local"));
    Pave.Session.branch branched branch_marker;
    Pave.Session.branch branched measured_tip;
    assert (Pave.Session.usage branched = Some counted);
    assert (Pave.Session.history branched = [message "first"; message "second"]);
    let copy = Pave.Session.fork branched metadata_fork in
    assert (Pave.Session.usage copy = Some counted);
    assert (Pave.Session.history copy = [message "first"; message "second"]);
    Pave.Session.append_usage copy ~provider:"ollama" ~model:"local"
      { input_tokens = 3; output_tokens = 2 };
    Pave.Session.append_usage copy ~provider:"ollama" ~model:"other"
      { input_tokens = 1; output_tokens = 1 };
    assert (Pave.Session.usage_by_model copy =
      [(("ollama", "local"), { input_tokens = 21; output_tokens = 9 });
       (("ollama", "other"), { input_tokens = 1; output_tokens = 1 })]);
    Pave.Session.branch copy first;
    assert (Pave.Session.usage copy = None);
    assert (Pave.Session.usage_by_model copy = []);
    Pave.Session.branch branched branch_marker;
    assert (Pave.Session.model branched = Some ("openai", "gpt-6-sol"));
    assert (Pave.Session.history (Pave.Session.open_file metadata_path) =
      [message "first"]);
    assert (Pave.Session.model copy = Some ("openai", "gpt-6-sol"));
    assert (Pave.Session.history copy = [message "first"]);
    let assistant text : Pave.Protocol.message = {
      role = "assistant"; content = Some text; tool_calls = [];
      tool_call_id = None; provider_state = None } in
    let before = [message "first"; assistant "old"; message "retry me"] in
    assert (Pave.Session.retryable_history (before @ [assistant "answer"]) =
      Some ([message "first"; assistant "old"], "retry me"));
    assert (Pave.Session.retryable_history
      (before @ [assistant "answer"; Pave.Protocol.tool_result "call-1" "done"]) =
      None);
    let prior = Pave.Session.append copy (assistant "old answer") in
    ignore (Pave.Session.append copy (message "retry me"));
    ignore (Pave.Session.append copy (assistant "first answer"));
    assert (Pave.Session.retry_candidate copy = Some (prior, "retry me"));
    Pave.Session.branch copy prior;
    ignore (Pave.Session.append copy (message "retry me"));
    ignore (Pave.Session.append copy (assistant "new answer"));
    assert (Pave.Session.history copy =
      [message "first"; assistant "old answer";
       message "retry me"; assistant "new answer"]);
    ignore (Pave.Session.append copy (message "tool turn"));
    ignore (Pave.Session.append copy {
      role = "assistant"; content = None; tool_calls = [ call ];
      tool_call_id = None; provider_state = None });
    ignore (Pave.Session.append copy (Pave.Protocol.tool_result "call-1" "done"));
    assert (Pave.Session.retry_candidate copy = None);
    Pave.Session.branch copy prior;
    ignore (Pave.Session.append copy (message "switch model"));
    Pave.Session.set_model copy ~provider:"ollama" ~model:"local";
    assert (Pave.Session.retry_candidate copy = None);
    (match Pave.Session.set_model copy ~provider:"openai" ~model:"invalid name" with
     | exception Pave.Protocol.Invalid_response _ -> ()
     | _ -> failwith "invalid model marker was accepted"));
  print_endline "session journal branches: ok"
