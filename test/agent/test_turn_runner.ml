let () =
  let events = ref [] in
  let event value = events := value :: !events in
  let release_late_events = Atomic.make false in
  let release_follow_up = Atomic.make false in
  let late_thread = ref None in
  let runner_ref = ref None in
  let active_turn_id = ref None in
  let seen_turn_ids = Hashtbl.create 8 in
  let require_owner turn_id = assert (!active_turn_id = Some turn_id) in
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
        Pave.Turn_runner.tool runner (Pave.Agent.Tool_updated {
          call_id = "late-call"; name = "run_command"; received_bytes = 5
        });
        raise Pave.Provider.Cancelled
    | "spawn-late" ->
        late_thread := Some (Thread.create (fun () ->
          while not (Atomic.get release_late_events) do Thread.delay 0.001 done;
          Pave.Turn_runner.message runner "old-turn-message";
          Pave.Turn_runner.delta runner "old-turn-delta";
          Pave.Turn_runner.phase runner (Pave.Agent.Tool "old-turn-tool");
          Pave.Turn_runner.tool runner (Pave.Agent.Tool_settled {
            call_id = "old-call"; name = "read_file"; result = "stale";
            is_error = false
          })) ());
        Pave.Turn_runner.message runner "old-turn-ready";
        while not (cancel ()) do Thread.delay 0.001 done;
        raise Pave.Provider.Cancelled
    | "follow-up" ->
        Pave.Turn_runner.message runner "follow-up-ready";
        while not (Atomic.get release_follow_up) && not (cancel ()) do
          Thread.delay 0.001
        done;
        Pave.Turn_runner.message runner "follow-up-complete"
    | "steering-source" | "dequeue-source" ->
        let label = if text = "steering-source" then "steering-source-ready"
          else "dequeue-source-ready" in
        Pave.Turn_runner.message runner label;
        while not (cancel ()) do Thread.delay 0.001 done;
        raise Pave.Provider.Cancelled
    | "steered" -> Pave.Turn_runner.message runner "steered-complete"
    | "after-steer" ->
        Pave.Turn_runner.message runner "after-steer-complete"
    | "dequeue-removed" ->
        Pave.Turn_runner.message runner "dequeue-removed-ran"
    | "dequeue-retained" ->
        Pave.Turn_runner.message runner "dequeue-retained-complete"

    | "boundary-source" ->
        Pave.Turn_runner.message runner "boundary-source-ready";
        while not (Atomic.get release_follow_up) do Thread.delay 0.001 done;
        Pave.Turn_runner.message runner "boundary-source-complete"
    | "cancel-return" ->
        Pave.Turn_runner.message runner "cancel-return-ready";
        while not (Atomic.get release_follow_up) do Thread.delay 0.001 done
    | "approve" ->
        if Pave.Turn_runner.approve runner "printf approved" then
          Pave.Turn_runner.delta runner "approved"
    | "fail" -> failwith "expected turn failure"
    | "cancel-abort" ->
        Pave.Turn_runner.tool runner (Pave.Agent.Tool_started {
          call_id = "aborted-call"; name = "read_file"
        });
        Pave.Turn_runner.message runner "cancel-abort-ready";
        while not (cancel ()) do Thread.delay 0.001 done;
        Pave.Turn_runner.tool runner (Pave.Agent.Tool_aborted {
          call_id = "aborted-call"; name = "read_file";
          result = "Error: turn canceled before execution";
          side_effects_may_have_occurred = false
        });
        raise Pave.Provider.Cancelled
    | _ -> failwith "unexpected turn" in
  let runner = Pave.Turn_runner.create ~run
    ~on_event:(fun turn_event ->
      assert (Thread.id (Thread.self ()) = caller_thread);
      match turn_event with
      | Pave.Turn_runner.Turn_started { turn_id; prompt } ->
          assert (!active_turn_id = None);
          assert (not (Hashtbl.mem seen_turn_ids turn_id));
          Hashtbl.add seen_turn_ids turn_id ();
          active_turn_id := Some turn_id;
          event ("start:" ^ prompt)
      | Pave.Turn_runner.Transcript_message { turn_id; text } ->
          require_owner turn_id;
          event ("message:" ^ text)
      | Pave.Turn_runner.Text_delta { turn_id; text } ->
          require_owner turn_id;
          event ("delta:" ^ text)
      | Pave.Turn_runner.Activity_phase { turn_id; phase } ->
          require_owner turn_id;
          event ("phase:" ^ match phase with
            | Pave.Agent.Model -> "model"
            | Pave.Agent.Tool name -> name)
      | Pave.Turn_runner.Tool_event { turn_id; event = tool_event } ->
          require_owner turn_id;
          (match tool_event with
           | Pave.Agent.Tool_started { call_id; name } ->
               event ("tool-start:" ^ call_id ^ ":" ^ name)
           | Pave.Agent.Tool_updated { call_id; received_bytes; _ } ->
               event (Printf.sprintf "tool-update:%s:%d" call_id received_bytes)
           | Pave.Agent.Tool_settled { call_id; _ } ->
               event ("tool-settled:" ^ call_id)
           | Pave.Agent.Tool_aborted { call_id;
               side_effects_may_have_occurred; _ } ->
               event ("tool-abort:" ^ call_id ^ ":" ^
                 string_of_bool side_effects_may_have_occurred))
      | Pave.Turn_runner.Turn_completed { turn_id } ->
          require_owner turn_id;
          active_turn_id := None;
          event "completed"
      | Pave.Turn_runner.Turn_cancelled { turn_id } ->
          require_owner turn_id;
          active_turn_id := None;
          event "cancelled"
      | Pave.Turn_runner.Turn_failed { turn_id; error } ->
          require_owner turn_id;
          active_turn_id := None;
          event ("failure:" ^ Printexc.to_string error))
    ~on_approve:(fun command ->
      assert (Thread.id (Thread.self ()) = caller_thread);
      assert (command = "printf approved");
      event "approved-request";
      true)
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
    let count value values =
      List.length (List.filter ((=) value) values) in
    let after prior =
      let rec drop remaining = function
        | _ :: rest when remaining > 0 -> drop (remaining - 1) rest
        | rest -> rest in
      drop prior (List.rev !events) in
    let position value values =
      let rec find index = function
        | [] -> failwith ("missing event: " ^ value)
        | item :: rest ->
            if item = value then index else find (index + 1) rest in
      find 0 values in
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
    assert (not (Pave.Turn_runner.busy runner));
    let failures_before = List.length (List.filter
      (String.starts_with ~prefix:"failure:") !events) in
    Pave.Turn_runner.submit runner "fail";
    until "failure:Failure(\"expected turn failure\")";
    until_idle ();
    assert (List.length (List.filter
      (String.starts_with ~prefix:"failure:") !events) = failures_before + 1);
    assert (!active_turn_id = None);
    Pave.Turn_runner.submit runner "cancel-abort";
    until "message:cancel-abort-ready";
    Pave.Turn_runner.cancel runner;
    until "tool-abort:aborted-call:false";
    until_idle ();
    assert (List.length (List.filter
      ((=) "tool-abort:aborted-call:false") !events) = 1);
    let prior = List.length !events in
    Pave.Turn_runner.submit runner "steering-source";
    until "message:steering-source-ready";
    Pave.Turn_runner.follow_up runner "after-steer";
    Pave.Turn_runner.steer runner "steered";
    until_idle ();
    let sequence = after prior in
    assert (count "start:steered" sequence = 1);
    assert (count "start:after-steer" sequence = 1);
    assert (position "start:steered" sequence <
      position "start:after-steer" sequence);
    let prior = List.length !events in
    Atomic.set release_follow_up false;
    Pave.Turn_runner.submit runner "boundary-source";
    until "message:boundary-source-ready";
    Pave.Turn_runner.follow_up runner "after-boundary";
    Thread.delay 0.01;
    let sequence = after prior in
    assert (Pave.Turn_runner.busy runner);
    assert (not (List.mem "cancelled" sequence));
    assert (count "start:after-boundary" sequence = 0);
    Atomic.set release_follow_up true;
    until_idle ();
    let sequence = after prior in
    assert (count "start:after-boundary" sequence = 1);
    assert (position "completed" sequence <
      position "start:after-boundary" sequence);
    let prior = List.length !events in
    Pave.Turn_runner.submit runner "dequeue-source";
    until "message:dequeue-source-ready";
    Pave.Turn_runner.follow_up runner "dequeue-retained";
    Pave.Turn_runner.steer runner "dequeue-removed";
    let removed = Option.get (Pave.Turn_runner.dequeue_last runner) in
    assert (removed.kind = Pave.Turn_runner.Steering);
    assert (removed.prompt = "dequeue-removed");
    Pave.Turn_runner.restore_dequeued runner removed;
    let removed_again = Option.get (Pave.Turn_runner.dequeue_last runner) in
    assert (removed_again = removed);
    until_idle ();
    let sequence = after prior in
    assert (count "start:dequeue-removed" sequence = 0);
    assert (count "start:dequeue-retained" sequence = 1);
    assert (count "message:dequeue-retained-complete" sequence = 1);
  );
  print_endline "turn runner: ok"
