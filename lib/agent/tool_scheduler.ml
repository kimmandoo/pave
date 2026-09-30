type mode = Shared | Exclusive

type 'a dispatch = Run | Complete of 'a

type 'a task = {
  mode : mode;
  prepare : unit -> 'a dispatch;
  run : unit -> 'a;
}

type 'a outcome = Completed of 'a | Failed of exn | Skipped

let max_shared = 4

let run ~cancelled ~on_complete tasks =
  let outcomes = Array.make (Array.length tasks) Skipped in
  let count = Array.length tasks in
  let record index outcome =
    outcomes.(index) <- outcome;
    match outcome with
    | Skipped -> ()
    | Completed _ | Failed _ -> on_complete index outcome in
  let run_task task =
    try Completed (task.run ()) with exn -> Failed exn in
  let prepare_task task =
    try Ok (task.prepare ()) with exn -> Error exn in
  (* Workers only store outcomes and signal; preparation and settlement stay
     on the calling (owner) thread. *)
  let lock = Mutex.create () and finished = Condition.create () in
  let done_ = Array.make count false in
  let settled = ref 0 in
  let settle () =
    (* Deliver results in provider order as soon as each prefix is complete. *)
    let rec loop () =
      if !settled < count then (
        Mutex.lock lock;
        let ready = done_.(!settled) in
        let outcome = outcomes.(!settled) in
        Mutex.unlock lock;
        if ready then (
          record !settled outcome;
          incr settled;
          loop ())) in
    loop () in
  let finish index outcome =
    Mutex.lock lock;
    outcomes.(index) <- outcome;
    done_.(index) <- true;
    Condition.broadcast finished;
    Mutex.unlock lock in
  let failed index =
    Mutex.lock lock;
    let result = done_.(index) && (match outcomes.(index) with
      | Completed _ -> false | Failed _ | Skipped -> true) in
    Mutex.unlock lock;
    result in
  let active = ref [] and workers = ref [] and stopped = ref false in
  let refresh () =
    active := List.filter (fun index ->
      Mutex.lock lock;
      let running = not done_.(index) in
      Mutex.unlock lock;
      if not running && failed index then stopped := true;
      running) !active in
  let await_slot limit =
    Mutex.lock lock;
    let rec wait () =
      let running = List.filter (fun index -> not done_.(index)) !active in
      if List.length running > limit then (
        Condition.wait finished lock;
        wait ()) in
    wait ();
    Mutex.unlock lock;
    refresh ();
    settle () in
  let start index =
    let task = tasks.(index) in
    let spawned =
      try
        workers := Thread.create (fun () -> finish index (run_task task)) () :: !workers;
        true
      with Failure _ -> false in
    if spawned then active := index :: !active
    else (finish index (run_task task); if failed index then stopped := true) in
  let rec schedule index =
    if index >= count || !stopped then ()
    else (
      let task = tasks.(index) in
      (* Exclusive work waits for every running shared call; shared work waits
         only for a free slot. *)
      await_slot (match task.mode with Exclusive -> 0 | Shared -> max_shared - 1);
      if !stopped then ()
      else match prepare_task task with
        | Error exn -> finish index (Failed exn); stopped := true
        | Ok (Complete value) -> finish index (Completed value); schedule (index + 1)
        | Ok Run when cancelled () -> finish index Skipped; stopped := true
        | Ok Run ->
            (match task.mode with
             | Exclusive ->
                 finish index (run_task task);
                 if failed index then stopped := true
             | Shared -> start index);
            schedule (index + 1)) in
  schedule 0;
  let rec drain () =
    refresh ();
    settle ();
    if !active <> [] then (
      Mutex.lock lock;
      if List.for_all (fun index -> not done_.(index)) !active then
        Condition.wait finished lock;
      Mutex.unlock lock;
      drain ()) in
  drain ();
  List.iter Thread.join !workers;
  settle ();
  (* A stop leaves later calls unsettled; report them as skipped. *)
  Array.iteri (fun index ready -> if not ready then outcomes.(index) <- Skipped) done_;
  outcomes
