let message text = Pave.Protocol.user text

let model_identity ?account_id provider route upstream_id =
  Pave.Model_identity.make ~provider ?account_id ~route ~upstream_id ()

let contains text needle =
  let rec seek offset =
    offset + String.length needle <= String.length text &&
    (String.sub text offset (String.length needle) = needle ||
     seek (offset + 1)) in
  seek 0

let rec remove_tree path =
  try
    let stat = Unix.lstat path in
    if stat.Unix.st_kind = Unix.S_DIR then (
      Array.iter (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path)
    else Sys.remove path
  with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let () =
  let dir = Filename.temp_file "pave-journal-" "" in
  Sys.remove dir; Unix.mkdir dir 0o700;
  let previous_home = Sys.getenv_opt "HOME"
  and previous_state = Sys.getenv_opt "XDG_STATE_HOME" in
  Unix.putenv "HOME" dir;
  Unix.putenv "XDG_STATE_HOME" (Filename.concat dir "state");
  let path = Filename.concat dir "session.jsonl" in
  let fork_path = Filename.concat dir "fork.jsonl" in
  let metadata_path = Filename.concat dir "metadata.jsonl" in
  let metadata_fork = Filename.concat dir "metadata-fork.jsonl" in
  let legacy_path = Filename.concat dir "legacy.jsonl" in
  let lifecycle_path = Filename.concat dir "lifecycle.jsonl" in
  let lifecycle_fork = Filename.concat dir "lifecycle-fork.jsonl" in
  let terminal_path = Filename.concat dir "terminal.jsonl" in
  let multimodal_path = Filename.concat dir "multimodal.jsonl" in
  let settings_path = Filename.concat dir "settings.jsonl" in
  let settings_fork = Filename.concat dir "settings-fork.jsonl" in
  let reset_path = Filename.concat dir "reset.jsonl" in
  let attachment_path = Filename.concat dir "attachment.jsonl" in
  let large_path = Filename.concat dir "large.jsonl" in
  let large_fork_path = Filename.concat dir "large-fork.jsonl" in
  let jobs_path = Filename.concat dir "jobs.jsonl" in
  let jobs_fork_path = Filename.concat dir "jobs-fork.jsonl" in
  Fun.protect ~finally:(fun () ->
    (match previous_home with Some value -> Unix.putenv "HOME" value
      | None -> Unix.putenv "HOME" "");
    (match previous_state with Some value -> Unix.putenv "XDG_STATE_HOME" value
      | None -> Unix.putenv "XDG_STATE_HOME" "");
    remove_tree dir) (fun () ->
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
      tool_calls = [ call ]; tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] } in
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
    ignore (Pave.Session.append terminal { role = "assistant"; content = None; tool_calls = [terminal_call];
    tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] });
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
    ignore (Pave.Session.append terminal { role = "assistant"; content = None; tool_calls = [aborted_call];
    tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] });
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
    let call_message (call : Pave.Protocol.tool_call) : Pave.Protocol.message = { role = "assistant"; content = None; tool_calls = [call];
    tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] } in
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
    let openai_model = model_identity "openai" "responses" "gpt-6-sol"
    and ollama_model = model_identity "ollama" "chat" "local"
    and commandcode_messages = model_identity "commandcode" "messages"
      "future-model"
    and commandcode_responses = model_identity "commandcode" "responses"
      "future-model" in
    let metadata = Pave.Session.open_file metadata_path in
    Pave.Session.set_model metadata openai_model;
    Pave.Session.set_model metadata openai_model;
    assert (List.length (Pave.Session.entries metadata) = 1);
    let first = Pave.Session.append metadata (message "first") in
    Pave.Session.set_model metadata ollama_model;
    let second = Pave.Session.append metadata (message "second") in
    let counted : Pave.Protocol.usage = {
      input_tokens = 18; output_tokens = 7; cached_input_tokens = None;
      cache_creation_input_tokens = None; reasoning_output_tokens = None;
      input_modality_tokens = None; cached_input_modality_tokens = None;
      output_modality_tokens = None } in
    Pave.Session.append_usage metadata ~provider:"ollama" ~model:"local" counted;
    let detailed : Pave.Protocol.usage = {
      input_tokens = 12; output_tokens = 9; cached_input_tokens = Some 5;
      cache_creation_input_tokens = Some 2; reasoning_output_tokens = Some 3;
      input_modality_tokens = Some [
        { Pave.Protocol.modality = "IMAGE"; token_count = 3 };
        { modality = "TEXT"; token_count = 2 } ];
      cached_input_modality_tokens = Some [
        { Pave.Protocol.modality = "IMAGE"; token_count = 2 } ];
      output_modality_tokens = Some [
        { Pave.Protocol.modality = "TEXT"; token_count = 5 }] } in
    Pave.Session.append_usage ~account_id:"anthropic-account-1" ~route:"messages"
      metadata ~provider:"anthropic" ~model:"claude-test" detailed;
    let combined = Pave.Protocol.add_usage counted detailed in
    let measured_tip = Option.get (Pave.Session.leaf_id metadata) in
    assert (Pave.Session.usage metadata = Some combined);
    assert (List.assoc ("anthropic", Some "anthropic-account-1",
      Some "messages", "claude-test") (Pave.Session.usage_by_route metadata)
      = detailed);
    assert (Pave.Session.model metadata = Some ollama_model);
    Pave.Session.branch metadata first;
    assert (Pave.Session.model metadata = Some openai_model);
    assert (Pave.Session.usage metadata = None);
    assert (Pave.Session.history metadata = [message "first"]);
    let branch_marker = (List.hd (List.rev (Pave.Session.entries metadata))).id in
    let branched = Pave.Session.open_file metadata_path in
    assert (Pave.Session.model branched = Some openai_model);
    assert (Pave.Session.model_at branched (Some second) = Some ollama_model);
    Pave.Session.branch branched branch_marker;
    Pave.Session.branch branched measured_tip;
    assert (Pave.Session.usage branched = Some combined);
    assert (List.assoc ("anthropic", Some "anthropic-account-1",
      Some "messages", "claude-test") (Pave.Session.usage_by_route branched)
      = detailed);
    assert (Pave.Session.history branched = [message "first"; message "second"]);
    let copy = Pave.Session.fork branched metadata_fork in
    assert (Pave.Session.usage copy = Some combined);
    assert (Pave.Session.history copy = [message "first"; message "second"]);
    Pave.Session.append_usage copy ~provider:"ollama" ~model:"local"
      { input_tokens = 3; output_tokens = 2; cached_input_tokens = None;
        cache_creation_input_tokens = None; reasoning_output_tokens = None;
        input_modality_tokens = None; cached_input_modality_tokens = None;
        output_modality_tokens = None };
    Pave.Session.append_usage copy ~provider:"ollama" ~model:"other"
      { input_tokens = 1; output_tokens = 1; cached_input_tokens = None;
        cache_creation_input_tokens = None; reasoning_output_tokens = None;
        input_modality_tokens = None; cached_input_modality_tokens = None;
        output_modality_tokens = None };
    Pave.Session.append_usage ~account_id:"anthropic-account-1" ~route:"responses"
      copy ~provider:"anthropic" ~model:"claude-test"
      { input_tokens = 2; output_tokens = 1; cached_input_tokens = None;
        cache_creation_input_tokens = None; reasoning_output_tokens = None;
        input_modality_tokens = None; cached_input_modality_tokens = None;
        output_modality_tokens = None };
    Pave.Session.append_usage ~account_id:"anthropic-account-2" ~route:"messages"
      copy ~provider:"anthropic" ~model:"claude-test"
      { input_tokens = 3; output_tokens = 1; cached_input_tokens = None;
        cache_creation_input_tokens = None; reasoning_output_tokens = None;
        input_modality_tokens = None; cached_input_modality_tokens = None;
        output_modality_tokens = None };
    let by_route = Pave.Session.usage_by_route copy in
    assert (List.assoc ("anthropic", Some "anthropic-account-1",
      Some "messages", "claude-test") by_route = detailed);
    assert (List.assoc ("anthropic", Some "anthropic-account-1",
      Some "responses", "claude-test") by_route = {
        input_tokens = 2; output_tokens = 1; cached_input_tokens = None;
        cache_creation_input_tokens = None; reasoning_output_tokens = None;
        input_modality_tokens = None; cached_input_modality_tokens = None;
        output_modality_tokens = None });
    assert (List.assoc ("anthropic", Some "anthropic-account-2",
      Some "messages", "claude-test") by_route = {
        input_tokens = 3; output_tokens = 1; cached_input_tokens = None;
        cache_creation_input_tokens = None; reasoning_output_tokens = None;
        input_modality_tokens = None; cached_input_modality_tokens = None;
        output_modality_tokens = None });
    assert (List.assoc ("ollama", None, None, "local") by_route = {
      input_tokens = 21; output_tokens = 9; cached_input_tokens = None;
      cache_creation_input_tokens = None; reasoning_output_tokens = None;
      input_modality_tokens = None; cached_input_modality_tokens = None;
      output_modality_tokens = None });
    assert (List.assoc ("ollama", None, None, "other") by_route = {
      input_tokens = 1; output_tokens = 1; cached_input_tokens = None;
      cache_creation_input_tokens = None; reasoning_output_tokens = None;
      input_modality_tokens = None; cached_input_modality_tokens = None;
      output_modality_tokens = None });
    Pave.Session.branch copy first;
    assert (Pave.Session.usage copy = None);
    assert (Pave.Session.usage_by_route copy = []);
    Pave.Session.branch branched branch_marker;
    assert (Pave.Session.model branched = Some openai_model);
    assert (Pave.Session.history (Pave.Session.open_file metadata_path) =
      [message "first"]);
    assert (Pave.Session.model copy = Some openai_model);
    assert (Pave.Session.history copy = [message "first"]);
    let assistant text : Pave.Protocol.message = { role = "assistant"; content = Some text; tool_calls = [];
    tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] } in
    let retry_attachment : Pave.Protocol.attachment = {
      name = "retry.png"; mime_type = "image/png"; data = "iVBORw0KGgo=" } in
    let retry_message = { (message "retry me") with
      attachments = [retry_attachment] } in
    let before = [message "first"; assistant "old"; retry_message] in
    assert (Pave.Session.retryable_history (before @ [assistant "answer"]) =
      Some ([message "first"; assistant "old"], retry_message));
    assert (Pave.Session.retryable_history
      (before @ [assistant "answer"; Pave.Protocol.tool_result "call-1" "done"]) =
      None);
    let prior = Pave.Session.append copy (assistant "old answer") in
    ignore (Pave.Session.append copy retry_message);
    ignore (Pave.Session.append copy (assistant "first answer"));
    assert (Pave.Session.retry_candidate copy =
      Some (prior, retry_message));
    Pave.Session.branch copy prior;
    ignore (Pave.Session.append copy retry_message);
    ignore (Pave.Session.append copy (assistant "new answer"));
    assert (Pave.Session.history copy =
      [message "first"; assistant "old answer";
       retry_message; assistant "new answer"]);
    let completed_tip = Option.get (Pave.Session.leaf_id copy) in
    ignore (Pave.Session.append copy (message "interrupted request"));
    assert (Pave.Session.retry_candidate copy =
      Some (completed_tip, message "interrupted request"));
    assert (Pave.Session.retryable_history [message "interrupted request"] =
      Some ([], message "interrupted request"));
    Pave.Session.branch copy completed_tip;
    ignore (Pave.Session.append copy (message "tool turn"));
    ignore (Pave.Session.append copy { role = "assistant"; content = None; tool_calls = [ call ];
    tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] });
    ignore (Pave.Session.append copy (Pave.Protocol.tool_result "call-1" "done"));
    assert (Pave.Session.retry_candidate copy = None);
    Pave.Session.branch copy prior;
    ignore (Pave.Session.append copy (message "switch model"));
    Pave.Session.set_model copy ollama_model;
    assert (Pave.Session.retry_candidate copy = None);
    Pave.Session.set_model copy commandcode_messages;
    let route_tip = Option.get (Pave.Session.leaf_id copy) in
    Pave.Session.set_model copy commandcode_messages;
    assert (Pave.Session.leaf_id copy = Some route_tip);
    assert (Pave.Session.model (Pave.Session.open_file metadata_fork) =
      Some commandcode_messages);
    Pave.Session.set_model copy commandcode_responses;
    assert (Pave.Session.model copy = Some commandcode_responses);
    assert (Pave.Session.model_at copy (Some route_tip) =
      Some commandcode_messages);
    Pave.Session.branch copy route_tip;
    assert (Pave.Session.model (Pave.Session.open_file metadata_fork) =
      Some commandcode_messages);
    let settings = Pave.Session.open_file settings_path in
    let settings_target = Pave.Session.append settings (message "settings base") in
    Pave.Session.set_model settings (model_identity "openai" "responses" "gpt-5");
    Pave.Session.set_thinking settings (Some "high");
    Pave.Session.set_disabled_tools settings ["write_file"];
    Pave.Session.set_mode settings (Some Pave.Approval.Ask_writes);
    Pave.Session.set_title settings "Release review";
    Pave.Session.set_label settings ~target_id:settings_target (Some "review");
    Pave.Session.set_pinned settings true;
    Pave.Session.set_goal settings (Some "Ship the session slice");
    Pave.Session.set_interruption_rule settings (Some "Stop after a failed gate");
    let settings_tip = Option.get (Pave.Session.leaf_id settings) in
    assert (Pave.Session.model settings = Some
      (model_identity "openai" "responses" "gpt-5"));
    assert (Pave.Session.thinking settings = Some "high");
    assert (Pave.Session.disabled_tools settings = ["write_file"]);
    assert (Pave.Session.mode settings = Some Pave.Approval.Ask_writes);
    assert (Pave.Session.title settings = Some "Release review");
    assert (Pave.Session.labels settings = [settings_target, "review"]);
    assert (Pave.Session.pinned settings);
    assert (Pave.Session.goal settings = Some "Ship the session slice");
    assert (Pave.Session.interruption_rule settings =
      Some "Stop after a failed gate");
    assert (Pave.Session.history settings = [message "settings base"]);
    assert (Pave.Session.label_target settings = Some settings_target);
    let settings_reopened = Pave.Session.open_file settings_path in
    assert (Pave.Session.thinking settings_reopened = Some "high");
    assert (Pave.Session.disabled_tools settings_reopened = ["write_file"]);
    assert (Pave.Session.mode settings_reopened = Some Pave.Approval.Ask_writes);
    assert (Pave.Session.title settings_reopened = Some "Release review");
    assert (Pave.Session.labels settings_reopened = [settings_target, "review"]);
    assert (Pave.Session.pinned settings_reopened);
    assert (Pave.Session.goal settings_reopened = Some "Ship the session slice");
    assert (Pave.Session.interruption_rule settings_reopened =
      Some "Stop after a failed gate");
    let parent_id = Pave.Protocol.member "id" settings.Pave.Session.header in
    let settings_copy = Pave.Session.fork settings settings_fork in
    assert (Pave.Session.goal settings_copy = Some "Ship the session slice");
    assert (Pave.Session.interruption_rule settings_copy =
      Some "Stop after a failed gate");
    assert (Pave.Session.parent_session settings_copy =
      (match parent_id with `String id -> Some id | _ -> assert false));
    assert (Pave.Session.title settings_copy = Some "Release review");
    assert (Pave.Session.mode settings_copy = Some Pave.Approval.Ask_writes);
    assert (not (Pave.Session.pinned settings_copy));
    Pave.Session.branch settings settings_target;
    assert (Pave.Session.thinking settings = None);
    assert (Pave.Session.disabled_tools settings = []);
    assert (Pave.Session.mode settings = None);
    assert (Pave.Session.title settings = Some "Release review");
    assert (Pave.Session.labels settings = []);
    assert (Pave.Session.model settings = None);
    assert (Pave.Session.pinned settings);
    assert (Pave.Session.goal settings = None);
    assert (Pave.Session.interruption_rule settings = None);
    Pave.Session.branch settings settings_tip;
    assert (Pave.Session.model settings = Some
      (model_identity "openai" "responses" "gpt-5"));
    assert (Pave.Session.thinking settings = Some "high");
    assert (Pave.Session.disabled_tools settings = ["write_file"]);
    assert (Pave.Session.mode settings = Some Pave.Approval.Ask_writes);
    assert (Pave.Session.title settings = Some "Release review");
    assert (Pave.Session.labels settings = [settings_target, "review"]);
    assert (Pave.Session.goal settings = Some "Ship the session slice");
    assert (Pave.Session.interruption_rule settings =
      Some "Stop after a failed gate");
    let reset = Pave.Session.open_file reset_path in
    ignore (Pave.Session.append reset (message "old context"));
    let reset_call : Pave.Protocol.tool_call = {
      id = "reset-call"; name = "read_file"; arguments = `Assoc [] } in
    ignore (Pave.Session.append reset {
      role = "assistant"; content = None; tool_calls = [reset_call];
      tool_call_id = None; tool_result_content = None;
      provider_state = None; attachments = [] });
    (match Pave.Session.clear reset with
     | exception Pave.Protocol.Invalid_response _ -> ()
     | _ -> failwith "clear crossed an unresolved tool call");
    ignore (Pave.Session.append reset
      (Pave.Protocol.tool_result reset_call.id "completed"));
    Pave.Session.set_model reset (model_identity "openai" "responses" "gpt-5");
    ignore (Pave.Session.clear reset);
    assert (Pave.Session.history reset = [
      message "old context";
      { role = "assistant"; content = None; tool_calls = [reset_call];
        tool_call_id = None; tool_result_content = None; provider_state = None;
        attachments = [] };
      Pave.Protocol.tool_result reset_call.id "completed"]);
    assert (Pave.Session.context reset = []);
    assert (Pave.Session.model reset = Some
      (model_identity "openai" "responses" "gpt-5"));
    ignore (Pave.Session.append reset (message "new request"));
    ignore (Pave.Session.append reset (assistant "new answer"));
    let latest_user = Pave.Session.append reset (message "latest request") in
    let first_kept, prefix = Pave.Session.compaction_plan reset in
    assert (first_kept = latest_user);
    assert (prefix = [message "new request"; assistant "new answer"]);
    ignore (Pave.Session.compact reset ~summary:"Reset-era summary"
      ~first_kept_id:first_kept);
    assert (Pave.Session.history reset = [
      message "old context";
      { role = "assistant"; content = None; tool_calls = [reset_call];
        tool_call_id = None; tool_result_content = None; provider_state = None;
        attachments = [] };
      Pave.Protocol.tool_result reset_call.id "completed";
      message "new request"; assistant "new answer"; message "latest request"]);
    assert (Pave.Session.context reset =
      [message "Reset-era summary"; message "latest request"]);
    let reset_reopened = Pave.Session.open_file reset_path in
    assert (Pave.Session.context reset_reopened = Pave.Session.context reset);
    assert (Pave.Session.history reset_reopened = Pave.Session.history reset);
    let attachment = {
      Pave.Protocol.name = "screenshot.png";
      mime_type = "image/png"; data = "iVBORw0KGgo="
    } in
    let attached_user = Pave.Protocol.user ~attachments:[attachment] "Inspect" in
    let attached_session = Pave.Session.open_file attachment_path in
    ignore (Pave.Session.append attached_session attached_user);
    let channel = open_in_bin attachment_path in
    let stored_entry = Fun.protect ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        ignore (input_line channel);
        Yojson.Basic.from_string (input_line channel)) in
    let nested = Pave.Protocol.member "message" stored_entry in
    assert (Pave.Protocol.member "content" nested = `String "Inspect");
    assert (Pave.Protocol.member "attachments" nested = `Null);
    assert (Pave.Protocol.member "attachments" stored_entry =
      `List [Pave.Protocol.attachment_to_json attachment]);
    assert (Pave.Session.history (Pave.Session.open_file attachment_path) =
      [attached_user]);
    let large_data = String.make 65540 'A' in
    let large_attachment : Pave.Protocol.attachment = {
      name = "large.png"; mime_type = "image/png"; data = large_data } in
    let large_user = Pave.Protocol.user ~attachments:[large_attachment] "large" in
    let large = Pave.Session.open_file large_path in
    ignore (Pave.Session.append large large_user);
    let large_input = open_in_bin large_path in
    let large_json = Fun.protect ~finally:(fun () -> close_in_noerr large_input)
      (fun () ->
        ignore (input_line large_input);
        let row = input_line large_input in
        assert (not (contains row large_data));
        Yojson.Basic.from_string row) in
    assert (Pave.Protocol.member "attachments" large_json = `Null);
    assert (Pave.Protocol.member "attachmentRefs" large_json <> `Null);
    let reopened_large = Pave.Session.open_file large_path in
    assert (Pave.Session.history reopened_large = [large_user]);
    let large_fork = Pave.Session.fork reopened_large large_fork_path in
    assert (Pave.Session.history large_fork = [large_user]);
    let owner = match Pave.Protocol.member "id" large.Pave.Session.header with
      | `String id -> id | _ -> assert false in
    let private_item = Pave.Session.store_artifact large ~name:"private.png"
      ~mime_type:"image/png" "aGVsbG8=" in
    let unrelated_path = Filename.concat dir "unrelated.jsonl" in
    let unrelated = Pave.Session.open_file unrelated_path in
    (match Pave.Session.read_artifact unrelated ~owner ~id:private_item.id with
     | exception Pave.Protocol.Invalid_response _ -> ()
     | _ -> failwith "unreferenced cross-session artifact was readable");
    assert (Pave.Session.read_artifact large ~owner ~id:private_item.id =
      "aGVsbG8=");
    assert (List.exists (fun (item : Pave.Session_artifact.item) ->
      item.Pave.Session_artifact.owner = owner &&
      item.name = "large.png" && item.mime_type = "image/png" &&
      item.size = String.length large_data && String.length item.sha256 = 64)
      (Pave.Session.list_artifacts reopened_large));
    let jobs = Pave.Session.open_file jobs_path in
    let anchor = Pave.Session.append jobs (message "job anchor") in
    let job_owner = match Pave.Protocol.member "id" jobs.Pave.Session.header with
      | `String id -> id | _ -> assert false in
    let job_id = "11111111111111111111111111111111" in
    ignore (Pave.Session.append_job_started jobs ~job_id ~label:"Build" ~job_kind:"build");
    ignore (Pave.Session.append_job_started jobs ~job_id ~label:"Build" ~job_kind:"build");
    let delivery : Pave.Session.job_delivery = {
      owner = job_owner; job_id; label = "Build"; status = Pave.Session.Completed;
      summary = "Finished"; artifact = None } in
    ignore (Pave.Session.append_job_delivery jobs delivery);
    ignore (Pave.Session.append_job_delivery jobs delivery);
    let job_entry_id = match List.find (fun (entry : Pave.Session.entry) ->
      match entry.kind with Pave.Session.Job_delivery _ -> true | _ -> false)
      (Pave.Session.entries jobs) with
      | entry -> entry.id in
    (match List.rev (Pave.Session.history jobs) with
     | { Pave.Protocol.role = "assistant"; content = Some summary; _ } :: _ ->
         assert (contains summary job_id && contains summary "completed" &&
           contains summary "Finished")
     | _ -> failwith "job delivery was not projected into history");
    Pave.Session.branch jobs anchor;
    let jobs_reopened = Pave.Session.open_file jobs_path in
    ignore (Pave.Session.append_job_delivery jobs_reopened delivery);
    assert (List.length (List.filter (fun (entry : Pave.Session.entry) ->
      match entry.kind with Pave.Session.Job_delivery _ -> true | _ -> false)
      (Pave.Session.entries jobs_reopened)) = 1);
    assert (match Pave.Session.job_states jobs_reopened with
      | [{ started; delivery = Some received }] ->
          started.job_id = job_id && received.status = Pave.Session.Completed
      | _ -> false);
    Pave.Session.branch jobs_reopened job_entry_id;
    assert (match List.rev (Pave.Session.history jobs_reopened) with
      | { Pave.Protocol.role = "assistant"; content = Some summary; _ } :: _ ->
          contains summary job_id
      | _ -> false);
    let jobs_fork = Pave.Session.fork jobs_reopened jobs_fork_path in
    assert (Pave.Session.history jobs_fork = [message "job anchor"]);
    assert (Pave.Session.job_states jobs_fork = []);
    let multimodal = Pave.Session.open_file multimodal_path in
    let image_call : Pave.Protocol.tool_call = {
      id = "image-call"; name = "inspect_image"; arguments = `Assoc [] } in
    let image_assistant : Pave.Protocol.message = {
      role = "assistant"; content = None; tool_result_content = None;
      tool_calls = [image_call]; tool_call_id = None; provider_state = None; attachments = [] } in
    let image_result = Pave.Protocol.tool_result_blocks image_call.id [
      Pave.Protocol.Text "Screenshot details";
      Pave.Protocol.Image { mime_type = "image/png"; data = "aGVsbG8=" }
    ] in
    ignore (Pave.Session.append multimodal image_assistant);
    ignore (Pave.Session.append multimodal image_result);
    let multimodal_reopened = Pave.Session.open_file multimodal_path in
    assert (Pave.Session.history multimodal_reopened =
      [image_assistant; image_result]);
    ignore (Pave.Session.open_file legacy_path);
    let legacy_output = open_out_gen [Open_append; Open_binary] 0o600
      legacy_path in
    output_string legacy_output
      "{\"type\":\"model\",\"id\":\"legacy-model\",\"parentId\":null,\"timestamp\":\"2025-01-01T00:00:00Z\",\"provider\":\"openai\",\"model\":\"gpt/legacy\",\"api\":null}\n";
    close_out legacy_output;
    let legacy = Pave.Session.open_file legacy_path in
    assert (Pave.Session.model legacy = Some
      (model_identity "openai" "responses" "gpt/legacy"));
    let account_model = model_identity ~account_id:"org/alice#1"
      "github-copilot" "chat" "models/assistant/id" in
    Pave.Session.set_model legacy account_model;
    assert (Pave.Session.model (Pave.Session.open_file legacy_path) =
      Some account_model);
    let custom_provider = Pave.Custom_provider.parse
      (Yojson.Basic.from_string
        {|{"id":"team-gateway","display_name":"Team Gateway","default_route":"chat","routes":[{"name":"chat","api":"openai-chat","endpoint":"https://team.example.test/v1/chat/completions","account_id":"team-7","api_key_env":"TEAM_GATEWAY_KEY","models":[{"id":"model-a","tools":true}]}]}|}) in
    let custom_registry = match
        Pave.Provider_catalog.create_registry [custom_provider] with
      | Ok registry -> registry
      | Error message -> failwith message in
    let custom_route = List.hd custom_provider.routes in
    let custom_identity = Pave.Model_identity.make
      ~provider:"team-gateway" ~account_id:"team-7"
      ~config_revision:(Pave.Custom_provider.fingerprint custom_route)
      ~route:"chat" ~upstream_id:"model-a" () in
    Pave.Session.set_model ~registry:custom_registry legacy custom_identity;
    let reopened_custom = Pave.Session.open_file legacy_path in
    assert (Pave.Session.model reopened_custom = Some custom_identity);
    let input = open_in_bin legacy_path in
    let contents = Fun.protect ~finally:(fun () -> close_in_noerr input)
      (fun () -> really_input_string input (in_channel_length input)) in
    assert (contains contents "configRevision");
    assert (not (contains contents "https://team.example.test"));
    assert (not (contains contents "TEAM_GATEWAY_KEY"));
    assert (not (contains contents "never-store-this-key"));
    let changed_provider = Pave.Custom_provider.parse
      (Yojson.Basic.from_string
        {|{"id":"team-gateway","display_name":"Team Gateway","default_route":"chat","routes":[{"name":"chat","api":"openai-chat","endpoint":"https://new.example.test/v1/chat/completions","account_id":"team-7","api_key_env":"TEAM_GATEWAY_KEY","models":[{"id":"model-a","tools":true}]}]}|}) in
    let changed_registry = match
        Pave.Provider_catalog.create_registry [changed_provider] with
      | Ok registry -> registry
      | Error message -> failwith message in
    (match Pave.Session.set_model ~registry:changed_registry reopened_custom
       custom_identity with
     | exception Pave.Protocol.Invalid_response _ -> ()
     | _ -> failwith "session accepted an identity from a changed custom route");
    (match Pave.Session.set_model copy
       (model_identity "commandcode" "unknown-route" "future-model") with
     | exception Pave.Protocol.Invalid_response _ -> ()
     | _ -> failwith "unsupported model route was accepted");
    (match Pave.Session.set_model copy
       { (model_identity "openai" "responses" "valid-model") with
         upstream_id = "invalid name" } with
     | exception Pave.Protocol.Invalid_response _ -> ()
     | _ -> failwith "invalid model marker was accepted"));
  print_endline "session journal branches: ok"
