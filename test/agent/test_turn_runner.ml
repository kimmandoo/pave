let () =
  let events = ref [] in
  let event value = events := value :: !events in
  let runner_ref = ref None in
  let caller_thread = Thread.id (Thread.self ()) in
  let run ~cancel text =
    let runner = Option.get !runner_ref in
    match text with
    | "hang" ->
        Pave.Turn_runner.message runner "working";
        while not (cancel ()) do Thread.delay 0.01 done;
        raise Pave.Provider.Cancelled
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
    ~on_start:(fun text -> event ("start:" ^ text))
    ~on_finish:(fun outcome -> event (match outcome with
      | Pave.Turn_runner.Completed -> "completed"
      | Pave.Turn_runner.Cancelled -> "cancelled"
      | Pave.Turn_runner.Failed exn -> Printexc.to_string exn))
    ~on_queued:(fun count -> event ("queued:" ^ string_of_int count)) () in
  runner_ref := Some runner;
  Fun.protect ~finally:(fun () -> Pave.Turn_runner.close runner) (fun () ->
    let rec until expected =
      if not (List.mem expected !events) then (
        let ready, _, _ = Unix.select [Pave.Turn_runner.fd runner] [] [] 3. in
        assert (ready <> []);
        Pave.Turn_runner.drain runner;
        until expected) in
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
    assert (not (Pave.Turn_runner.busy runner)));
  print_endline "turn runner: ok"
