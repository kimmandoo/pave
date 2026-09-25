let prepare_threads = ref []

let task mode run : string Pave.Tool_scheduler.task = {
  mode;
  prepare = (fun () ->
    prepare_threads := Thread.id (Thread.self ()) :: !prepare_threads;
    Pave.Tool_scheduler.Run);
  run
}
let () =
  let mutex = Mutex.create () and condition = Condition.create () in
  let release = ref false and started = ref 0 and active = ref 0 in
  let maximum_active = ref 0 and exclusive_ran = ref false in
  let shared number () =
    Mutex.lock mutex;
    incr started;
    incr active;
    maximum_active := max !maximum_active !active;
    Condition.broadcast condition;
    let rec await_release () =
      if number < 4 && not !release then (
        Condition.wait condition mutex;
        await_release ()) in
    await_release ();
    let exclusive_ran = !exclusive_ran in
    decr active;
    Mutex.unlock mutex;
    if number = 6 then assert exclusive_ran;
    Printf.sprintf "shared-%d" number in
  let exclusive () =
    Mutex.lock mutex;
    let active = !active in
    exclusive_ran := true;
    Mutex.unlock mutex;
    assert (active = 0);
    "exclusive" in
  let jobs = List.init 5 (fun number -> task Pave.Tool_scheduler.Shared (shared number)) @
    [ task Pave.Tool_scheduler.Exclusive exclusive;
      task Pave.Tool_scheduler.Shared (shared 6) ] in
  let result = ref None and completions = ref [] in
  let owner_thread = ref None and completion_threads = ref [] in
  let scheduler = Thread.create (fun () ->
    owner_thread := Some (Thread.id (Thread.self ()));
    result := Some (Pave.Tool_scheduler.run ~cancelled:(fun () -> false)
      ~on_complete:(fun index _ ->
        completions := index :: !completions;
        completion_threads := Thread.id (Thread.self ()) :: !completion_threads)
      (Array.of_list jobs))) () in
  let deadline = Unix.gettimeofday () +. 5. in
  let rec await_four_started () =
    Mutex.lock mutex;
    let count = !started in
    Mutex.unlock mutex;
    if count < 4 && Unix.gettimeofday () < deadline then (
      Thread.delay 0.001;
      await_four_started ())
    else count in
  let started_before_release = await_four_started () in
  Mutex.lock mutex;
  release := true;
  Condition.broadcast condition;
  let observed_maximum = !maximum_active in
  Mutex.unlock mutex;
  Thread.join scheduler;
  assert (started_before_release = 4);
  assert (observed_maximum = 4);
  assert (List.length !prepare_threads = 7);
  assert (List.for_all (fun thread_id -> Some thread_id = !owner_thread)
    !prepare_threads);
  assert (List.rev !completions = [0; 1; 2; 3; 4; 5; 6]);
  assert (List.for_all (fun thread_id -> Some thread_id = !owner_thread)
    !completion_threads);
  (match !result with
   | Some [| Pave.Tool_scheduler.Completed "shared-0";
             Pave.Tool_scheduler.Completed "shared-1";
             Pave.Tool_scheduler.Completed "shared-2";
             Pave.Tool_scheduler.Completed "shared-3";
             Pave.Tool_scheduler.Completed "shared-4";
             Pave.Tool_scheduler.Completed "exclusive";
             Pave.Tool_scheduler.Completed "shared-6" |] -> ()
   | _ -> failwith "tool scheduler lost provider order or the exclusive barrier");
  prepare_threads := [];
  let caller_thread = Thread.id (Thread.self ()) in
  let cancelled = ref false and prepared_after_cancel = ref false in
  let executed_after_cancel = ref false in
  let first = task Pave.Tool_scheduler.Exclusive (fun () ->
    cancelled := true;
    "completed before cancellation") in
  let second : string Pave.Tool_scheduler.task = {
    mode = Pave.Tool_scheduler.Exclusive;
    prepare = (fun () ->
      prepare_threads := Thread.id (Thread.self ()) :: !prepare_threads;
      prepared_after_cancel := true;
      Pave.Tool_scheduler.Run);
    run = (fun () -> executed_after_cancel := true; "must not run");
  } in
  let outcomes = Pave.Tool_scheduler.run ~cancelled:(fun () -> !cancelled)
    ~on_complete:(fun _ _ -> ()) [| first; second |] in
  assert (List.length !prepare_threads = 2);
  assert (List.for_all (( = ) caller_thread) !prepare_threads);
  assert (!prepared_after_cancel);
  assert (not !executed_after_cancel);
  (match outcomes with
   | [| Pave.Tool_scheduler.Completed "completed before cancellation";
        Pave.Tool_scheduler.Skipped |] -> ()
   | _ -> failwith "tool scheduler executed a call after cancellation");
  print_endline "tool scheduler: ok"
