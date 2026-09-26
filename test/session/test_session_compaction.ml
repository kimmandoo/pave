let user = Pave.Protocol.user
let assistant text : Pave.Protocol.message =
  { role = "assistant"; content = Some text; tool_calls = [];
    tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] }

let () =
  let path = Filename.temp_file "pave-compaction-" ".jsonl" in
  Sys.remove path;
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let session = Pave.Session.open_file path in
    let original = Pave.Session.append session (user "old request") in
    ignore (Pave.Session.append session (assistant "old answer"));
    let current = Pave.Session.append session (user "recent request") in
    let before = Pave.Session.history session in
    (match Pave.Session.compact session ~summary:"not enough" ~first_kept_id:original with
     | exception Pave.Protocol.Invalid_response _ -> ()
     | _ -> failwith "compaction accepted an earlier boundary");
    assert (Pave.Session.history session = before);
    let summary = "Summary of old request and answer" in
    let native_state = `Assoc [
      "provider", `String "openai"; "route", `String "responses";
      "model", `String "fixture-model";
      "items", `List [`Assoc [
        "type", `String "compaction";
        "encrypted_content", `String "opaque payload"]] ] in
    let compacted = { (user summary) with provider_state = Some native_state } in
    let marker = Pave.Session.compact ~provider_state:native_state session
      ~summary ~first_kept_id:current in
    assert (Pave.Session.history session = before);
    assert (Pave.Session.context session = [compacted; user "recent request"]);
    let call : Pave.Protocol.tool_call = { id = "call-1"; name = "read_file";
      arguments = `Assoc [ "path", `String "App.swift" ] } in
    ignore (Pave.Session.append session { role = "assistant"; content = None;
      tool_calls = [ call ]; tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] });
    let reopened = Pave.Session.open_file path in
    (match List.rev (Pave.Session.context reopened) with
     | result :: _ ->
         assert (result.role = "tool");
         assert (result.tool_call_id = Some "call-1")
     | [] -> assert false);
    assert (Pave.Session.context (Pave.Session.open_file path) =
      Pave.Session.context reopened);
    Pave.Session.branch reopened original;
    assert (Pave.Session.context reopened = [ user "old request" ]);
    Pave.Session.branch reopened marker;
    assert (Pave.Session.context reopened = [compacted; user "recent request"]);
    assert (Pave.Session.history reopened = before);
    let anthropic_path = Filename.temp_file
      "pave-anthropic-compaction-" ".jsonl" in
    Sys.remove anthropic_path;
    Fun.protect ~finally:(fun () -> Sys.remove anthropic_path) (fun () ->
      let journal = Pave.Session.open_file anthropic_path in
      let old_turn = Pave.Session.append journal (user "older Anthropic turn") in
      let current_turn =
        Pave.Session.append journal (user "recent Anthropic turn") in
      let history = Pave.Session.history journal in
      let content = "signed Anthropic summary" in
      let signature = "opaque-anthropic-signature" in
      let state = Pave.Anthropic_wire.compaction_state
        ~model:"claude-fixture" ~content ~signature in
      let marker = Pave.Session.compact ~provider_state:state journal
        ~summary:content ~first_kept_id:current_turn in
      let expected = [{ (user content) with provider_state = Some state };
        user "recent Anthropic turn"] in
      assert (Pave.Session.context journal = expected);
      assert (Pave.Session.history journal = history);
      let reopened = Pave.Session.open_file anthropic_path in
      assert (Pave.Session.context reopened = expected);
      Pave.Session.branch reopened old_turn;
      assert (Pave.Session.context reopened = [user "older Anthropic turn"]);
      Pave.Session.branch reopened marker;
      assert (Pave.Session.context reopened = expected);
      assert (Pave.Session.history reopened = history));
    let single_path = Filename.temp_file "pave-compaction-single-" ".jsonl" in
    Sys.remove single_path;
    Fun.protect ~finally:(fun () -> Sys.remove single_path) (fun () ->
      let single = Pave.Session.open_file single_path in
      ignore (Pave.Session.append single (user "older request"));
      let latest = Pave.Session.append single (user "latest request") in
      let before = Pave.Session.history single in
      let first_kept_id, prefix = Pave.Session.compaction_plan single in
      assert (first_kept_id = latest && prefix = [user "older request"]);
      ignore (Pave.Session.compact single ~summary:"Older request summary"
        ~first_kept_id);
      assert (Pave.Session.history single = before);
      assert (Pave.Session.context single =
        [user "Older request summary"; user "latest request"]));
    let fork_path = path ^ ".fork" in
    Fun.protect ~finally:(fun () -> Sys.remove fork_path) (fun () ->
      let fork = Pave.Session.fork reopened fork_path in
      assert (Pave.Session.context fork = Pave.Session.context reopened)));
  print_endline "session compaction: ok"
