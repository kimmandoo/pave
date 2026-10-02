let shutdown_cases () =
  List.iter (fun mode ->
    let child = Unix.fork () in
    if child = 0 then (
      (try
         let ready = Atomic.make false in
         let events = ref [] and prompts = ref 0 and runner_ref = ref None in
         let run ~cancel (_ : Pave.Turn_runner.submission) =
           let runner = Option.get !runner_ref in
           Pave.Turn_runner.tool runner (Pave.Agent.Tool_started {
             call_id = "shutdown-call"; name = "run_command"; target = None;
             write_content = None });
           Atomic.set ready true;
           Fun.protect ~finally:(fun () ->
             Pave.Turn_runner.tool runner (Pave.Agent.Tool_aborted {
               call_id = "shutdown-call"; name = "run_command";
               result = "Error: cancelled before execution";
               side_effects_may_have_occurred = false; elapsed_ms = None })) (fun () ->
             (match mode with
              | "running" ->
                  while not (cancel ()) do Thread.delay 0.001 done
              | "shell-approval" ->
                  ignore (Pave.Turn_runner.approve runner "printf approved")
              | "tool-approval" ->
                  ignore (Pave.Turn_runner.approve_tool runner {
                    Pave.Approval.tool_name = "write_file";
                    tier = Pave.Approval.Write;
                    trigger = Pave.Approval.File_access;
                    impact = "Writes a file";
                    details = ["Path: blocked.txt"]; reason = None })
              | _ -> assert false);
             raise Pave.Provider.Cancelled) in
         let runner = Pave.Turn_runner.create ~run
           ~on_event:(function
             | Pave.Turn_runner.Turn_started _ -> events := "started" :: !events
             | Pave.Turn_runner.Tool_event {
                 event = Pave.Agent.Tool_aborted { call_id; _ }; _ } ->
                 assert (call_id = "shutdown-call");
                 events := "aborted" :: !events
             | Pave.Turn_runner.Turn_cancelled _ ->
                 events := "cancelled" :: !events
             | _ -> failwith "unexpected shutdown event")
           ~on_approve:(fun _ -> incr prompts; true)
           ~on_approve_tool:(fun _ -> incr prompts; true)
           ~on_queued:(fun _ -> ()) () in
         runner_ref := Some runner;
         Pave.Turn_runner.submit runner "active";
         while not (Atomic.get ready) do Thread.delay 0.001 done;
         Pave.Turn_runner.follow_up runner "must-not-start";
         Pave.Turn_runner.close runner;
         Pave.Turn_runner.close runner;
         Pave.Turn_runner.post runner "late background event";
         assert (not (Pave.Turn_runner.busy runner));
         assert (!prompts = 0);
         assert (List.rev !events = ["started"; "aborted"; "cancelled"]);
         exit 0
       with exn ->
         prerr_endline (mode ^ ": " ^ Printexc.to_string exn);
         exit 2));
    let deadline = Unix.gettimeofday () +. 3. in
    let rec wait () =
      match Unix.waitpid [Unix.WNOHANG] child with
      | 0, _ when Unix.gettimeofday () < deadline ->
          Thread.delay 0.005; wait ()
      | 0, _ ->
          Unix.kill child Sys.sigkill;
          ignore (Unix.waitpid [] child);
          failwith (mode ^ ": shutdown did not cancel and release its worker")
      | _, Unix.WEXITED 0 -> ()
      | _ -> failwith (mode ^ ": shutdown failed") in
    wait ()) ["running"; "shell-approval"; "tool-approval"]

let queue_management_cases () =
  let module Runner = Pave.Turn_runner in
  let ready = Atomic.make false and release = Atomic.make false in
  let active_cancel = ref (fun () -> false) in
  let started = ref [] and cancellations = ref 0 and counts = ref [] in
  let run ~cancel (submission : Runner.submission) =
    if submission.prompt = "active" then (
      active_cancel := cancel;
      Atomic.set ready true;
      while not (Atomic.get release) && not (cancel ()) do
        Thread.delay 0.001
      done) in
  let runner = Runner.create ~run
    ~on_event:(function
      | Runner.Turn_started { submission; _ } ->
          started := submission :: !started
      | Runner.Turn_cancelled _ -> incr cancellations
      | Runner.Turn_completed _ -> ()
      | Runner.Turn_failed { error; _ } -> raise error
      | _ -> failwith "unexpected queue management event")
    ~on_approve:(fun _ -> false)
    ~on_queued:(fun count -> counts := count :: !counts) () in
  let ids () = List.map (fun (item : Runner.queued_submission) -> item.id)
    (Runner.queued runner) in
  let start_active () =
    Atomic.set ready false;
    Atomic.set release false;
    Runner.submit runner "active";
    let deadline = Unix.gettimeofday () +. 3. in
    while not (Atomic.get ready) do
      assert (Unix.gettimeofday () < deadline);
      Thread.delay 0.001
    done in
  let rec drain_idle () =
    if Runner.busy runner then (
      let readable, _, _ = Unix.select [Runner.fd runner] [] [] 3. in
      assert (readable <> []);
      Runner.drain runner;
      drain_idle ()) in
  Fun.protect ~finally:(fun () ->
    Atomic.set release true; Runner.close runner) (fun () ->
    start_active ();
    let attachments = [{
      Pave.Protocol.name = "same.png"; mime_type = "image/png"; data = "same-data"
    }] in
    let duplicate () = Runner.submit runner ~display_prompt:"same @same.png"
      ~attachments ~paste_ranges:[5, 14] "same prepared payload" in
    duplicate (); duplicate ();
    Runner.submit runner "tail";
    let first, second, tail = match Runner.queued runner with
      | [first; second; tail] -> first, second, tail
      | _ -> assert false in
    assert (first.id < second.id && second.id < tail.id);
    assert (first.submission = second.submission);
    assert (Runner.take_queued runner ~id:first.id = Some first);
    assert (ids () = [second.id; tail.id]);
    assert (not ((!active_cancel) ()));
    let count_events = !counts in
    assert (Runner.take_queued runner ~id:first.id = None);
    assert (not (Runner.prioritize_queued runner ~id:first.id ~interrupt:true));
    assert (!counts = count_events && not ((!active_cancel) ()));
    Runner.restore_dequeued runner first;
    assert (ids () = [second.id; tail.id; first.id]);
    assert (Runner.dequeue_last runner = Some first);
    assert (Runner.prioritize_queued runner ~id:second.id ~interrupt:false);
    assert (ids () = [second.id; tail.id]);
    assert (not ((!active_cancel) ()));
    Atomic.set release true;
    drain_idle ();
    assert (!cancellations = 0);
    assert (List.rev !started = [
      Runner.make_submission "active"; second.submission; tail.submission
    ]);
    started := [];
    start_active ();
    (* A snapshot ID which has started must never cancel a later active turn. *)
    assert (not (Runner.prioritize_queued runner ~id:second.id ~interrupt:true));
    assert (not ((!active_cancel) ()));
    List.iter (fun text -> Runner.submit runner text)
      ["fifo-a"; "priority-b"; "selected-c"; "fifo-d"];
    let a, b, c, d = match Runner.queued runner with
      | [a; b; c; d] -> a, b, c, d
      | _ -> assert false in
    assert (tail.id < a.id && a.id < b.id && b.id < c.id && c.id < d.id);
    assert (Runner.prioritize_queued runner ~id:b.id ~interrupt:false);
    assert (ids () = [b.id; a.id; c.id; d.id]);
    assert (not ((!active_cancel) ()));
    assert (Runner.prioritize_queued runner ~id:c.id ~interrupt:true);
    assert ((!active_cancel) ());
    assert (ids () = [c.id; b.id; a.id; d.id]);
    drain_idle ();
    assert (!cancellations = 1);
    assert (List.rev !started = [
      Runner.make_submission "active";
      c.submission; b.submission; a.submission; d.submission
    ]);
    (* An item restored after the active turn ended runs once, not twice. *)
    started := [];
    Runner.restore_dequeued runner first;
    assert (Runner.prioritize_queued runner ~id:first.id ~interrupt:true);
    assert (Runner.queued runner = []);
    drain_idle ();
    assert (List.rev !started = [first.submission]);
    assert (!cancellations = 1))

let remote_submission_case () =
  let module Runner = Pave.Turn_runner in
  let events = ref [] and cancelled = ref 0 in
  let recorded = ref [] and finished = ref 0 in
  let release_first = Atomic.make false in
  let owner = Thread.self () in
  let run ~cancel (submission : Runner.submission) =
    if submission.prompt = "first" then
      while not (Atomic.get release_first) && not (cancel ()) do
        Thread.delay 0.001
      done in
  let runner = Runner.create ~run
    ~on_event:(function
      | Runner.Turn_started { submission; _ } ->
          assert (Thread.self () = owner);
          events := ("started:" ^ submission.prompt) :: !events
      | Runner.Turn_cancelled _ -> incr cancelled; incr finished
      | Runner.Turn_completed _ ->
          events := "completed" :: !events;
          incr finished
      | Runner.Turn_failed { error; _ } -> raise error
      | _ -> failwith "unexpected remote event")
    ~on_approve:(fun _ -> false)
    ~on_queued:(fun _ -> ())
    ~on_record:(fun (submission : Runner.submission) ->
      recorded := submission.prompt :: !recorded) () in
  Fun.protect ~finally:(fun () -> Runner.close runner) (fun () ->
    let pump () =
      let readable, _, _ = Unix.select [Runner.fd runner] [] [] 3. in
      assert (readable <> []);
      Runner.drain runner in
    let wait_finished n =
      let deadline = Unix.gettimeofday () +. 3. in
      while !finished < n do
        assert (Unix.gettimeofday () < deadline);
        pump ()
      done in
    let drain_notice () =
      let readable, _, _ = Unix.select [Runner.fd runner] [] [] 0.2 in
      if readable <> [] then Runner.drain runner in
    (* A foreign thread's submit must reach the owner via the pipe; the turn
       events fire on the owner thread, not the submitter's. *)
    let submitter = Thread.create (fun () ->
      Runner.submit runner "remote-first") () in
    Thread.join submitter;
    wait_finished 1;
    assert (List.rev !events = ["started:remote-first"; "completed"]);
    assert (List.rev !recorded = ["remote-first"]);
    events := []; recorded := [];
    (* Remote submit while busy queues; remote cancel cancels the active turn. *)
    Runner.submit runner "first";
    drain_notice ();
    assert (Runner.busy runner);
    let submitter = Thread.create (fun () ->
      Runner.submit runner "remote-second") () in
    Thread.join submitter;
    let deadline = Unix.gettimeofday () +. 3. in
    while Runner.queued_count runner = 0 do
      assert (Unix.gettimeofday () < deadline);
      drain_notice ()
    done;
    assert (Runner.queued_count runner = 1);
    let canceller = Thread.create (fun () -> Runner.cancel runner) () in
    Thread.join canceller;
    (* Cancel cancels the running turn; the queued submission then starts and
       finishes, so two more turns complete. *)
    wait_finished 3;
    assert (List.mem "started:remote-second" !events);
    assert (List.rev !recorded = ["first"; "remote-second"]))

let () =
  shutdown_cases ();
  queue_management_cases ();
  remote_submission_case ();
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
  let run ~cancel submission =
    let text = submission.Pave.Turn_runner.prompt in
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
            is_error = false; elapsed_ms = None
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
    | "steered" ->
        assert (submission.display_prompt = "change @focus.png");
        assert (submission.attachments = [{
          Pave.Protocol.name = "focus.png"; mime_type = "image/png";
          data = "focus-data"
        }]);
        assert (submission.paste_ranges = [7, 17]);
        Pave.Turn_runner.message runner "steered-complete"
    | "after-steer" ->
        Pave.Turn_runner.message runner "after-steer-complete"
    | "dequeue-removed" ->
        Pave.Turn_runner.message runner "dequeue-removed-ran"
    | "dequeue-retained" ->
        Pave.Turn_runner.message runner "dequeue-retained-complete"

    | "boundary-source" ->
        Pave.Turn_runner.message runner "boundary-source-ready";
        while not (Atomic.get release_follow_up) do Thread.delay 0.001 done;
        assert (not (cancel ()));
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
          call_id = "aborted-call"; name = "read_file"; target = None;
          write_content = None
        });
        Pave.Turn_runner.message runner "cancel-abort-ready";
        while not (cancel ()) do Thread.delay 0.001 done;
        Pave.Turn_runner.tool runner (Pave.Agent.Tool_aborted {
          call_id = "aborted-call"; name = "read_file";
          result = "Error: turn canceled before execution";
          side_effects_may_have_occurred = false; elapsed_ms = None
        });
        raise Pave.Provider.Cancelled
    | "payload-one" ->
        assert (submission.display_prompt = "display one");
        assert (submission.paste_ranges = [8, 11]);
        assert (submission.attachments = [{
          Pave.Protocol.name = "one.png";
          mime_type = "image/png";
          data = "one-data"
        }]);
        Pave.Turn_runner.message runner "payload-one-ran"
    | "payload-two" ->
        assert (submission.display_prompt = "display two");
        assert (submission.paste_ranges = [8, 11]);
        assert (submission.attachments = [{
          Pave.Protocol.name = "two.png";
          mime_type = "image/png";
          data = "two-data"
        }]);
        Pave.Turn_runner.message runner "payload-two-ran"
    | _ -> failwith "unexpected turn" in
  let runner = Pave.Turn_runner.create ~run
    ~on_event:(fun turn_event ->
      assert (Thread.id (Thread.self ()) = caller_thread);
      match turn_event with
      | Pave.Turn_runner.Turn_started { turn_id; submission } ->
          assert (!active_turn_id = None);
          assert (not (Hashtbl.mem seen_turn_ids turn_id));
          Hashtbl.add seen_turn_ids turn_id ();
          active_turn_id := Some turn_id;
          event ("start:" ^ submission.display_prompt);
          (match submission.prompt with
           | "payload-one" ->
               assert (submission.attachments = [{
                 Pave.Protocol.name = "one.png"; mime_type = "image/png";
                 data = "one-data"
               }])
           | "payload-two" ->
               assert (submission.attachments = [{
                 Pave.Protocol.name = "two.png"; mime_type = "image/png";
                 data = "two-data"
               }])
           | _ -> ())
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
           | Pave.Agent.Tool_draft _ | Pave.Agent.Tool_draft_ended _ |
             Pave.Agent.Tool_executing _ -> ()
           | Pave.Agent.Tool_started { call_id; name; _ } ->
               event ("tool-start:" ^ call_id ^ ":" ^ name)
           | Pave.Agent.Tool_updated { call_id; received_bytes; _ } ->
               event (Printf.sprintf "tool-update:%s:%d" call_id received_bytes)
           | Pave.Agent.Tool_settled { call_id; _ } ->
               event ("tool-settled:" ^ call_id)
           | Pave.Agent.Tool_aborted { call_id;
               side_effects_may_have_occurred; _ } ->
               event ("tool-abort:" ^ call_id ^ ":" ^
                 string_of_bool side_effects_may_have_occurred))
      | Pave.Turn_runner.Draft_preview { turn_id; _ } ->
          require_owner turn_id
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
          event ("failure:" ^ Printexc.to_string error)
      | Pave.Turn_runner.Background_notice { message } ->
          assert (!active_turn_id = None);
          event ("background:" ^ message))
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
    Pave.Turn_runner.submit runner "hang";
    until "message:working";
    let one = [{
      Pave.Protocol.name = "one.png"; mime_type = "image/png";
      data = "one-data"
    }] in
    let two = [{
      Pave.Protocol.name = "two.png"; mime_type = "image/png";
      data = "two-data"
    }] in
    Pave.Turn_runner.follow_up runner ~display_prompt:"display one"
      ~attachments:one ~paste_ranges:[8, 11] "payload-one";
    Pave.Turn_runner.submit runner ~display_prompt:"display two"
      ~attachments:two ~paste_ranges:[8, 11] "payload-two";
    Pave.Turn_runner.cancel runner;
    until_idle ();
    let chronological = List.rev !events in
    assert (List.mem "start:display one" chronological);
    assert (List.mem "start:display two" chronological);
    assert (position "start:display one" chronological <
      position "start:display two" chronological);
    assert (List.mem "message:payload-one-ran" chronological);
    assert (List.mem "message:payload-two-ran" chronological);
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
    Pave.Turn_runner.submit runner "after-steer";
    Pave.Turn_runner.steer runner ~display_prompt:"change @focus.png"
      ~attachments:[{
        Pave.Protocol.name = "focus.png"; mime_type = "image/png";
        data = "focus-data"
      }] ~paste_ranges:[7, 17] "steered";
    until_idle ();
    let sequence = after prior in
    assert (count "cancelled" sequence = 1);
    assert (count "start:change @focus.png" sequence = 1);
    assert (count "message:steered-complete" sequence = 1);
    assert (count "start:after-steer" sequence = 1);
    assert (position "start:change @focus.png" sequence <
      position "start:after-steer" sequence);
    let prior = List.length !events in
    Atomic.set release_follow_up false;
    Pave.Turn_runner.submit runner "boundary-source";
    until "message:boundary-source-ready";
    Pave.Turn_runner.submit runner "after-boundary";
    Thread.delay 0.01;
    let sequence = after prior in
    assert (Pave.Turn_runner.busy runner);
    assert (not (List.mem "cancelled" sequence));
    assert (count "start:after-boundary" sequence = 0);
    Atomic.set release_follow_up true;
    until_idle ();
    let sequence = after prior in
    assert (not (List.mem "cancelled" sequence));
    assert (count "message:boundary-source-complete" sequence = 1);
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
    assert (removed.submission.prompt = "dequeue-removed");
    assert (removed.submission.display_prompt = "dequeue-removed");
    Pave.Turn_runner.restore_dequeued runner removed;
    let removed_again = Option.get (Pave.Turn_runner.dequeue_last runner) in
    assert (removed_again = removed);
    until_idle ();
    let sequence = after prior in
    assert (count "start:dequeue-removed" sequence = 0);
    assert (count "start:dequeue-retained" sequence = 1);
    assert (count "message:dequeue-retained-complete" sequence = 1);
    let prior = List.length !events in
    let producer = Thread.create (fun () ->
      Pave.Turn_runner.post runner "job finished") () in
    Thread.join producer;
    let ready, _, _ = Unix.select [Pave.Turn_runner.fd runner] [] [] 3. in
    assert (ready <> []);
    Pave.Turn_runner.drain runner;
    let delivered = after prior in
    assert (delivered = ["background:job finished"]);
  );
  print_endline "turn runner: ok"
