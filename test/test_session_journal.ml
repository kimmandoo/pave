let message text = Pave.Protocol.user text

let () =
  let dir = Filename.temp_file "pave-journal-" "" in
  Sys.remove dir; Unix.mkdir dir 0o700;
  let path = Filename.concat dir "session.jsonl" in
  let fork_path = Filename.concat dir "fork.jsonl" in
  Fun.protect ~finally:(fun () ->
    (try Sys.remove fork_path with Sys_error _ -> ());
    (try Sys.remove path with Sys_error _ -> ());
    Unix.rmdir dir) (fun () ->
    let journal = Pave.Session.open_file path in
    let base = Pave.Session.append journal (message "base") in
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
      tool_calls = [ call ]; tool_call_id = None } in
    let recovered = Pave.Session.open_file path in
    (match List.rev (Pave.Session.history recovered) with
     | result :: _ ->
         assert (result.role = "tool");
         assert (result.tool_call_id = Some "call-1")
     | [] -> assert false);
    Pave.Session.branch recovered pending_id;
    assert (List.length (Pave.Session.history recovered) = 4);
    assert (Pave.Session.history (Pave.Session.open_file path) =
      Pave.Session.history recovered));
  print_endline "session journal branches: ok"
