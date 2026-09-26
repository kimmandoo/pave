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
    let marker = Pave.Session.compact session ~summary:"Summary of old request and answer"
      ~first_kept_id:current in
    assert (Pave.Session.history session = before);
    assert (Pave.Session.context session =
      [ user "Summary of old request and answer"; user "recent request" ]);
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
    assert (Pave.Session.context reopened =
      [ user "Summary of old request and answer"; user "recent request" ]);
    assert (Pave.Session.history reopened = before);
    let fork_path = path ^ ".fork" in
    Fun.protect ~finally:(fun () -> Sys.remove fork_path) (fun () ->
      let fork = Pave.Session.fork reopened fork_path in
      assert (Pave.Session.context fork = Pave.Session.context reopened)));
  print_endline "session compaction: ok"
