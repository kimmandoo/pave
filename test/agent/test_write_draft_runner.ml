let submission prompt : Pave.Turn_runner.submission = {
  prompt; display_prompt = prompt; attachments = []; paste_ranges = [] }
let await predicate =
  let deadline = Unix.gettimeofday () +. 5. in
  while not (predicate ()) do
    assert (Unix.gettimeofday () < deadline);
    Thread.delay 0.001
  done
let () =
  let runner_ref = ref None and ready = Atomic.make false and release = Atomic.make false in
  let events = ref [] in
  let draft key call_id fragment = Pave.Agent.Tool_draft {
    Pave.Protocol.key; call_id = Some call_id; name = "write_file"; fragment } in
  let run ~cancel (submission : Pave.Turn_runner.submission) =
    let runner = Option.get !runner_ref in
    Pave.Turn_runner.tool runner (draft "0:a" "a" {|{"path":"a","content":"|});
    if submission.prompt = "cancel" then (
      Pave.Turn_runner.tool runner (draft "0:a" "a" "live");
      Atomic.set ready true;
      await cancel;
      Pave.Turn_runner.tool runner (Pave.Agent.Tool_draft_ended {
        key = "0:a"; call_id = None; valid = false });
      raise Pave.Provider.Cancelled)
    else (
      for _ = 1 to 10_000 do
        Pave.Turn_runner.tool runner (draft "0:a" "a" "x\\n")
      done;
      Pave.Turn_runner.tool runner (draft "0:a" "a" {|"}|});
      Pave.Turn_runner.tool runner (draft "0:b" "b" {|{"content":"two","path":"b"}|});
      Atomic.set ready true;
      await (fun () -> Atomic.get release);
      List.iter (fun (key, call_id) ->
        Pave.Turn_runner.tool runner (Pave.Agent.Tool_draft_ended {
          key; call_id = Some call_id; valid = true });
        Pave.Turn_runner.tool runner (Pave.Agent.Tool_started {
          call_id; name = "write_file"; target = Some call_id;
          write_content = Some "validated" })) ["0:a", "a"; "0:b", "b"];
      List.iter (fun call_id ->
        Pave.Turn_runner.tool runner (Pave.Agent.Tool_settled {
          call_id; name = "write_file"; result = "Error: denied"; is_error = true; elapsed_ms = None })) ["b"; "a"]) in
  let runner = Pave.Turn_runner.create ~run
    ~on_event:(fun event -> events := event :: !events)
    ~on_approve:(fun _ -> false) ~on_queued:ignore () in
  runner_ref := Some runner;
  Fun.protect ~finally:(fun () -> Pave.Turn_runner.close runner) (fun () ->
    Pave.Turn_runner.start runner (submission "coalesce");
    await (fun () -> Atomic.get ready);
    assert (Pave.Turn_runner.with_guard runner (fun () -> Queue.length runner.notices) = 2);
    Pave.Turn_runner.drain runner;
    let previews = List.filter_map (function
      | Pave.Turn_runner.Draft_preview { key; preview; _ } -> Some (key, preview)
      | _ -> None) !events in
    assert (List.length previews = 2);
    let preview = List.assoc "0:a" previews in
    assert (preview.total_lines = 10_001 && preview.omitted_lines = 9985);
    assert (List.length preview.lines = Pave.Write_preview.max_lines);
    assert ((List.assoc "0:b" previews).path = Some "b");
    Atomic.set release true;
    await (fun () -> Pave.Turn_runner.drain runner; not (Pave.Turn_runner.busy runner));
    let settled = List.filter_map (function
      | Pave.Turn_runner.Tool_event { event = Pave.Agent.Tool_settled { call_id; _ }; _ } -> Some call_id
      | _ -> None) (List.rev !events) in
    assert (settled = ["b"; "a"]);
    assert (Hashtbl.length runner.drafts = 0);
    events := []; Atomic.set ready false;
    Pave.Turn_runner.start runner (submission "cancel");
    await (fun () -> Atomic.get ready);
    Pave.Turn_runner.drain runner;
    assert (List.exists (function Pave.Turn_runner.Draft_preview _ -> true | _ -> false) !events);
    Pave.Turn_runner.cancel runner;
    await (fun () -> Pave.Turn_runner.drain runner; not (Pave.Turn_runner.busy runner));
    assert (Hashtbl.length runner.drafts = 0);
    assert (List.exists (function
      | Pave.Turn_runner.Tool_event { event = Pave.Agent.Tool_draft_ended { valid = false; _ }; _ } -> true
      | _ -> false) !events));
  print_endline "bounded coalesced drafts, independent call IDs and cancellation: ok"
