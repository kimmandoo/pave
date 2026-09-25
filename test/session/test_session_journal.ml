let message text = Pave.Protocol.user text

let () =
  let dir = Filename.temp_file "pave-journal-" "" in
  Sys.remove dir; Unix.mkdir dir 0o700;
  let path = Filename.concat dir "session.jsonl" in
  let fork_path = Filename.concat dir "fork.jsonl" in
  let metadata_path = Filename.concat dir "metadata.jsonl" in
  let metadata_fork = Filename.concat dir "metadata-fork.jsonl" in
  let lifecycle_path = Filename.concat dir "lifecycle.jsonl" in
  let lifecycle_fork = Filename.concat dir "lifecycle-fork.jsonl" in
  let terminal_path = Filename.concat dir "terminal.jsonl" in
  Fun.protect ~finally:(fun () ->
    (try Sys.remove fork_path with Sys_error _ -> ());
    (try Sys.remove metadata_fork with Sys_error _ -> ());
    (try Sys.remove metadata_path with Sys_error _ -> ());
    (try Sys.remove lifecycle_fork with Sys_error _ -> ());
    (try Sys.remove lifecycle_path with Sys_error _ -> ());
    (try Sys.remove terminal_path with Sys_error _ -> ());
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
    Pave.Session.record_tool_started current ~call_id:call.id ~name:call.name
    |> ignore;
    assert (Option.is_some
      (Pave.Session.record_exit current ~kind:Pave.Session.Fatal));
    let recovered = Pave.Session.open_file path in
    (match List.rev (Pave.Session.history recovered) with
     | result :: _ ->
         assert (result.role = "tool");
         assert (result.tool_call_id = Some "call-1")
     | [] -> assert false);
    let exits = List.filter_map (fun (entry : Pave.Session.entry) ->
      match entry.kind with
      | Pave.Session.Session_exit { kind; pending_tool_calls } ->
          Some (kind, pending_tool_calls)
      | _ -> None) (Pave.Session.entries recovered) in
    (match exits with
     | [(Pave.Session.Fatal, [pending])] ->
         assert (pending.call_id = "call-1");
         assert (pending.name = "read_file");
         assert (pending.state = Pave.Session.Started)
     | _ -> failwith "session exit did not retain its pending started call");
    let recovered_states = List.filter_map (fun (entry : Pave.Session.entry) ->
      match entry.kind with
      | Pave.Session.Tool_lifecycle { state; _ } -> Some state
      | _ -> None) (Pave.Session.branch_entries recovered) in
    assert (recovered_states = [
      Pave.Session.Tool_started;
      Pave.Session.Tool_aborted { side_effects_may_have_occurred = true }]);
    Pave.Session.branch recovered pending_id;
    assert (List.length (Pave.Session.history recovered) = 4);
    (match List.rev (Pave.Session.history recovered) with
     | { content = Some text; _ } :: _ ->
         assert (String.starts_with
           ~prefix:"Error: prior process stopped before a durable tool result was recorded; execution status is unknown" text);
         assert (String.ends_with
           ~suffix:"Do not rerun this call automatically." text)
     | _ -> failwith "recovery did not explain possible tool side effects");
    assert (Pave.Session.history (Pave.Session.open_file path) =
      Pave.Session.history recovered);
    let terminal = Pave.Session.open_file terminal_path in
    let terminal_call : Pave.Protocol.tool_call = {
      id = "settled-call"; name = "write_file"; arguments = `Assoc [] } in
    ignore (Pave.Session.append terminal (message "terminal recovery"));
    ignore (Pave.Session.append terminal {
      role = "assistant"; content = None; tool_calls = [terminal_call];
      tool_call_id = None; provider_state = None });
    Pave.Session.record_tool_started terminal
      ~call_id:terminal_call.id ~name:terminal_call.name |> ignore;
    Pave.Session.record_tool_settled terminal
      ~call_id:terminal_call.id ~name:terminal_call.name ~is_error:false |> ignore;
    ignore (Pave.Session.record_exit terminal ~kind:Pave.Session.Fatal);
    let terminal = Pave.Session.open_file terminal_path in
    (match List.rev (Pave.Session.history terminal) with
     | { content = Some text; role = "tool"; tool_call_id = Some "settled-call"; _ } :: _ ->
         assert (String.starts_with
           ~prefix:"Error: tool execution was recorded as settled but its result was missing" text)
     | _ -> failwith "recovery did not pair a durably settled tool call");
    let terminal_states session = List.filter_map
      (fun (entry : Pave.Session.entry) -> match entry.kind with
       | Pave.Session.Tool_lifecycle { state; _ } -> Some state
       | _ -> None) (Pave.Session.branch_entries session) in
    assert (terminal_states terminal = [
      Pave.Session.Tool_started;
      Pave.Session.Tool_settled { is_error = false }]);
    let terminal_again = Pave.Session.open_file terminal_path in
    assert (List.length (Pave.Session.history terminal_again) = 3);
    assert (terminal_states terminal_again = [
      Pave.Session.Tool_started;
      Pave.Session.Tool_settled { is_error = false }]);
    let aborted_call : Pave.Protocol.tool_call = {
      id = "aborted-recovery"; name = "run_command"; arguments = `Assoc [] } in
    ignore (Pave.Session.append terminal {
      role = "assistant"; content = None; tool_calls = [aborted_call];
      tool_call_id = None; provider_state = None });
    Pave.Session.record_tool_started terminal
      ~call_id:aborted_call.id ~name:aborted_call.name |> ignore;
    Pave.Session.record_tool_aborted terminal
      ~call_id:aborted_call.id ~name:aborted_call.name
      ~side_effects_may_have_occurred:true |> ignore;
    ignore (Pave.Session.record_exit terminal ~kind:Pave.Session.Fatal);
    let terminal = Pave.Session.open_file terminal_path in
    (match List.rev (Pave.Session.history terminal) with
     | { content = Some text; tool_call_id = Some "aborted-recovery"; _ } :: _ ->
         assert (String.starts_with
           ~prefix:"Error: tool was aborted while running; side effects may have occurred" text)
     | _ -> failwith "recovery lost an aborted call outcome");
    assert (terminal_states terminal = [
      Pave.Session.Tool_started;
      Pave.Session.Tool_settled { is_error = false };
      Pave.Session.Tool_started;
      Pave.Session.Tool_aborted { side_effects_may_have_occurred = true }]);
    let terminal = Pave.Session.open_file terminal_path in
    assert (List.length (Pave.Session.history terminal) = 5);
    assert (terminal_states terminal = [
      Pave.Session.Tool_started;
      Pave.Session.Tool_settled { is_error = false };
      Pave.Session.Tool_started;
      Pave.Session.Tool_aborted { side_effects_may_have_occurred = true }]);
    let lifecycle = Pave.Session.open_file lifecycle_path in
    let lifecycle_user = Pave.Session.append lifecycle (message "lifecycle") in
    let call_message (call : Pave.Protocol.tool_call) : Pave.Protocol.message = {
      role = "assistant"; content = None; tool_calls = [call];
      tool_call_id = None; provider_state = None } in
    let finished_call : Pave.Protocol.tool_call = {
      id = "finished-call"; name = "read_file"; arguments = `Assoc [] } in
    ignore (Pave.Session.append lifecycle (call_message finished_call));
    Pave.Session.record_tool_started lifecycle
      ~call_id:finished_call.id ~name:finished_call.name |> ignore;
    let finished_result = Pave.Protocol.tool_result finished_call.id "read complete" in
    Pave.Session.record_tool_settled lifecycle
      ~call_id:finished_call.id ~name:finished_call.name ~is_error:false |> ignore;
    ignore (Pave.Session.append lifecycle finished_result);
    let aborted_call : Pave.Protocol.tool_call = {
      id = "aborted-call"; name = "run_command"; arguments = `Assoc [] } in
    ignore (Pave.Session.append lifecycle (call_message aborted_call));
    Pave.Session.record_tool_started lifecycle
      ~call_id:aborted_call.id ~name:aborted_call.name |> ignore;
    let aborted_result = Pave.Protocol.tool_result aborted_call.id
      "Error: command cancelled while running; side effects may have occurred" in
    Pave.Session.record_tool_aborted lifecycle
      ~call_id:aborted_call.id ~name:aborted_call.name
      ~side_effects_may_have_occurred:true |> ignore;
    ignore (Pave.Session.append lifecycle aborted_result);
    ignore (Pave.Session.record_exit lifecycle ~kind:Pave.Session.Normal);
    let lifecycle = Pave.Session.open_file lifecycle_path in
    assert (Pave.Session.history lifecycle =
      [message "lifecycle"; call_message finished_call; finished_result;
       call_message aborted_call; aborted_result]);
    let events = List.filter_map (fun (entry : Pave.Session.entry) ->
      match entry.kind with
      | Pave.Session.Tool_lifecycle { state; _ } -> Some state
      | _ -> None) (Pave.Session.branch_entries lifecycle) in
    assert (events = [
      Pave.Session.Tool_started;
      Pave.Session.Tool_settled { is_error = false };
      Pave.Session.Tool_started;
      Pave.Session.Tool_aborted { side_effects_may_have_occurred = true }]);
    let exits = List.filter_map (fun (entry : Pave.Session.entry) ->
      match entry.kind with
      | Pave.Session.Session_exit { kind; pending_tool_calls } ->
          Some (kind, pending_tool_calls)
      | _ -> None) (Pave.Session.branch_entries lifecycle) in
    assert (exits = [(Pave.Session.Normal, [])]);
    Pave.Session.branch lifecycle lifecycle_user;
    assert (Pave.Session.history lifecycle = [message "lifecycle"]);
    assert (List.filter_map (fun (entry : Pave.Session.entry) ->
      match entry.kind with
      | Pave.Session.Tool_lifecycle _ | Pave.Session.Session_exit _ ->
          Some entry.id
      | _ -> None) (Pave.Session.branch_entries lifecycle) = []);
    let isolated = Pave.Session.fork lifecycle lifecycle_fork in
    assert (Pave.Session.history isolated = [message "lifecycle"]);
    assert (List.filter_map (fun (entry : Pave.Session.entry) ->
      match entry.kind with
      | Pave.Session.Tool_lifecycle _ | Pave.Session.Session_exit _ ->
          Some entry.id
      | _ -> None) (Pave.Session.branch_entries isolated) = []);
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
    Pave.Session.set_model ~api:"messages" copy
      ~provider:"commandcode" ~model:"future-model";
    let route_tip = Option.get (Pave.Session.leaf_id copy) in
    Pave.Session.set_model ~api:"messages" copy
      ~provider:"commandcode" ~model:"future-model";
    assert (Pave.Session.leaf_id copy = Some route_tip);
    assert (Pave.Session.api (Pave.Session.open_file metadata_fork) =
      Some "messages");
    Pave.Session.set_model ~api:"responses" copy
      ~provider:"commandcode" ~model:"future-model";
    assert (Pave.Session.api copy = Some "responses");
    assert (Pave.Session.api_at copy (Some route_tip) = Some "messages");
    Pave.Session.branch copy route_tip;
    assert (Pave.Session.api (Pave.Session.open_file metadata_fork) =
      Some "messages");
    (match Pave.Session.set_model ~api:"invalid route" copy
       ~provider:"commandcode" ~model:"future-model" with
     | exception Pave.Protocol.Invalid_response _ -> ()
     | _ -> failwith "invalid API marker was accepted");
    (match Pave.Session.set_model copy ~provider:"openai" ~model:"invalid name" with
     | exception Pave.Protocol.Invalid_response _ -> ()
     | _ -> failwith "invalid model marker was accepted"));
  print_endline "session journal branches: ok"
