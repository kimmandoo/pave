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
  let rec last_shared index remaining =
    if index >= count || remaining = 0 || tasks.(index).mode <> Shared then index - 1
    else last_shared (index + 1) (remaining - 1) in
  let rec schedule index =
    if index >= count then ()
    else match tasks.(index).mode with
      | Exclusive ->
          let outcome = match prepare_task tasks.(index) with
            | Error exn -> Failed exn
            | Ok (Complete value) -> Completed value
            | Ok Run ->
                if cancelled () then Skipped else run_task tasks.(index) in
          record index outcome;
          (match outcome with Completed _ -> schedule (index + 1) | Failed _ | Skipped -> ())
      | Shared ->
          let last = last_shared index max_shared in
          let workers = ref [] in
          let stopped = ref false in
          for current = index to last do
            if not !stopped then
              match prepare_task tasks.(current) with
              | Error exn ->
                  outcomes.(current) <- Failed exn;
                  stopped := true
              | Ok (Complete value) -> outcomes.(current) <- Completed value
              | Ok Run ->
                  if cancelled () then (
                    outcomes.(current) <- Skipped;
                    stopped := true)
                  else (
                    let task = tasks.(current) in
                    let task_index = current in
                    try
                      let worker = Thread.create (fun () ->
                        outcomes.(task_index) <- run_task task) () in
                      workers := worker :: !workers
                    with Failure _ ->
                      let outcome = run_task task in
                      outcomes.(task_index) <- outcome;
                      (match outcome with Failed _ -> stopped := true | _ -> ()))
          done;
          List.iter Thread.join !workers;
          for current = index to last do
            record current outcomes.(current)
          done;
          let finished = ref true in
          for current = index to last do
            match outcomes.(current) with
            | Completed _ -> ()
            | Failed _ | Skipped -> finished := false
          done;
          if !finished then schedule (last + 1)
  in
  schedule 0;
  outcomes
