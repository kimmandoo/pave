let () =
  let events = ref [] in
  let event value = events := value :: !events in
  let release_late_events = Atomic.make false in
  let release_follow_up = Atomic.make false in
  let late_thread = ref None in
  let runner_ref = ref None in
  let caller_thread = Thread.id (Thread.self ()) in
  let run ~cancel text =
    let runner = Option.get !runner_ref in
    match text with
    | "hang" ->
        Pave.Turn_runner.message runner "working";
        while not (cancel ()) do Thread.delay 0.01 done;
        raise Pave.Provider.Cancelled
    | "cancel-late" ->
        Pave.Turn_runner.message runner "late-ready";
        while not (Atomic.get release_late_events) do Thread.delay 0.001 done;
        Pave.Turn_runner.delta runner "late";
        Pave.Turn_runner.message runner "late";
        Pave.Turn_runner.phase runner (Pave.Agent.Tool "stale");
        raise Pave.Provider.Cancelled
    | "spawn-late" ->
        late_thread := Some (Thread.create (fun () ->
          while not (Atomic.get release_late_events) do Thread.delay 0.001 done;
          Pave.Turn_runner.message runner "old-turn-message";
          Pave.Turn_runner.delta runner "old-turn-delta";
          Pave.Turn_runner.phase runner (Pave.Agent.Tool "old-turn-tool")) ());
        Pave.Turn_runner.message runner "old-turn-ready";
        while not (cancel ()) do Thread.delay 0.001 done;
        raise Pave.Provider.Cancelled
    | "follow-up" ->
        Pave.Turn_runner.message runner "follow-up-ready";
        while not (Atomic.get release_follow_up) && not (cancel ()) do
          Thread.delay 0.001
        done;
        Pave.Turn_runner.message runner "follow-up-complete"
    | "cancel-return" ->
        Pave.Turn_runner.message runner "cancel-return-ready";
        while not (Atomic.get release_follow_up) do Thread.delay 0.001 done
    | "approve" ->
        if Pave.Turn_runner.approve runner "printf approved" then
          Pave.Turn_runner.delta runner "approved"
    | _ -> failwith "unexpected turn" in
  let runner = Pave.Turn_runner.create ~run
    ~on_message:(fun message -> event ("message:" ^ message))
    ~on_delta:(fun text -> event ("delta:" ^ text))
    ~on_approve:(fun command ->
      assert (Thread.id (Thread.self ()) = caller_thread);
      assert (command = "printf approved");
      event "approved-request";
      true)
    ~on_phase:(fun phase -> event ("phase:" ^ match phase with
      | Pave.Agent.Model -> "model"
      | Pave.Agent.Tool name -> name))
    ~on_start:(fun text -> event ("start:" ^ text))
    ~on_finish:(fun outcome -> event (match outcome with
      | Pave.Turn_runner.Completed -> "completed"
      | Pave.Turn_runner.Cancelled -> "cancelled"
      | Pave.Turn_runner.Failed exn -> Printexc.to_string exn))
    ~on_queued:(fun count -> event ("queued:" ^ string_of_int count)) () in
  runner_ref := Some runner;
  Fun.protect ~finally:(fun () ->
    Atomic.set release_late_events true;
    Atomic.set release_follow_up true;
    Option.iter Thread.join !late_thread;
    Pave.Turn_runner.close runner) (fun () ->
    let rec until expected =
      if not (List.mem expected !events) then (
        let ready, _, _ = Unix.select [Pave.Turn_runner.fd runner] [] [] 3. in
        assert (ready <> []);
        Pave.Turn_runner.drain runner;
        until expected) in
    let rec until_idle () =
      if Pave.Turn_runner.busy runner then (
        let ready, _, _ = Unix.select [Pave.Turn_runner.fd runner] [] [] 3. in
        assert (ready <> []);
        Pave.Turn_runner.drain runner;
        until_idle ()) in
    Pave.Turn_runner.submit runner "hang";
    until "message:working";
    Pave.Turn_runner.submit runner "approve";
    assert (Pave.Turn_runner.busy runner);
    Pave.Turn_runner.cancel runner;
    until "completed";
    let chronological = List.rev !events in
    assert (chronological = ["start:hang"; "message:working";
      "queued:1"; "cancelled"; "start:approve"; "queued:0";
      "approved-request"; "delta:approved"; "completed"]);
    Pave.Turn_runner.submit runner "cancel-late";
    until "message:late-ready";
    Pave.Turn_runner.cancel runner;
    Atomic.set release_late_events true;
    until_idle ();
    assert (not (List.mem "delta:late" !events));
    assert (not (List.mem "message:late" !events));
    assert (not (List.mem "phase:stale" !events));
    Atomic.set release_late_events false;
    Atomic.set release_follow_up false;
    Pave.Turn_runner.submit runner "spawn-late";
    until "message:old-turn-ready";
    Pave.Turn_runner.submit runner "follow-up";
    Pave.Turn_runner.cancel runner;
    until "message:follow-up-ready";
    Atomic.set release_late_events true;
    Option.iter Thread.join !late_thread;
    Atomic.set release_follow_up true;
    until_idle ();
    assert (not (List.mem "message:old-turn-message" !events));
    assert (not (List.mem "delta:old-turn-delta" !events));
    assert (not (List.mem "phase:old-turn-tool" !events));
    assert (List.length (List.filter ((=) "start:follow-up") !events) = 1);
    assert (List.length (List.filter
      ((=) "message:follow-up-complete") !events) = 1);
    Atomic.set release_follow_up false;
    let cancelled_before = List.length (List.filter ((=) "cancelled") !events) in
    Pave.Turn_runner.submit runner "cancel-return";
    until "message:cancel-return-ready";
    Pave.Turn_runner.cancel runner;
    Atomic.set release_follow_up true;
    until_idle ();
    assert (List.length (List.filter ((=) "cancelled") !events)
      = cancelled_before + 1);
    assert (not (Pave.Turn_runner.busy runner)));
  print_endline "turn runner: ok"
