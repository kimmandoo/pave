type completion = Completed | Cancelled | Failed of exn

type event =
  | Turn_started of { turn_id : int; prompt : string }
  | Transcript_message of { turn_id : int; text : string }
  | Text_delta of { turn_id : int; text : string }
  | Activity_phase of { turn_id : int; phase : Agent.phase }
  | Tool_event of { turn_id : int; event : Agent.tool_event }
  | Turn_completed of { turn_id : int }
  | Turn_cancelled of { turn_id : int }
  | Turn_failed of { turn_id : int; error : exn }

type turn = {
  id : int;
  cancelled : bool Atomic.t;
  mutable thread_id : int option;
}

type approval = {
  mutex : Mutex.t;
  condition : Condition.t;
  mutable answer : bool option;
}

type notice =
  | Message of int * string
  | Delta of int * string
  | Phase of int * Agent.phase
  | Tool of int * Agent.tool_event
  | Approve of int * string * approval * (unit -> bool)
  | Finished of int * completion

type t = {
  read_fd : Unix.file_descr;
  write_fd : Unix.file_descr;
  wake_byte : bytes;
  drain_bytes : bytes;
  guard : Mutex.t;
  notices : notice Queue.t;
  mutable approvals : approval list;
  pending : string Queue.t;
  mutable worker : Thread.t option;
  mutable active_turn : turn option;
  mutable next_turn_id : int;
  mutable closed : bool;
  run : cancel:(unit -> bool) -> string -> unit;
  on_event : event -> unit;
  on_approve : string -> bool;
  on_queued : int -> unit;
}

let with_guard t callback =
  Mutex.lock t.guard;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.guard) callback

let create ~run ~on_event ~on_approve ~on_queued () =
  let read_fd, write_fd = Unix.pipe () in
  Unix.set_close_on_exec read_fd;
  Unix.set_close_on_exec write_fd;
  Unix.set_nonblock read_fd;
  Unix.set_nonblock write_fd;
  { read_fd; write_fd; wake_byte = Bytes.of_string "x";
    drain_bytes = Bytes.create 256;
    guard = Mutex.create (); notices = Queue.create (); approvals = [];
    pending = Queue.create (); worker = None; active_turn = None;
    next_turn_id = 0; closed = false;
    run; on_event; on_approve; on_queued }

let fd t = t.read_fd
let busy t = t.worker <> None

let emit_finish t turn_id = function
  | Completed -> t.on_event (Turn_completed { turn_id })
  | Cancelled -> t.on_event (Turn_cancelled { turn_id })
  | Failed error -> t.on_event (Turn_failed { turn_id; error })

let active_turn t id =
  with_guard t (fun () ->
    match t.active_turn with
    | Some turn when turn.id = id -> Some turn
    | _ -> None)

let cancel_requested t id = match active_turn t id with
  | Some turn -> Atomic.get turn.cancelled
  | None -> true

(* Notices belong to the thread executing this turn. Detached producers cannot
   borrow the current owner or have late callbacks mislabeled as follow-ups. *)
let worker_turn t =
  let thread_id = Thread.id (Thread.self ()) in
  with_guard t (fun () ->
    match t.active_turn with
    | Some turn when turn.thread_id = Some thread_id -> Some turn
    | _ -> None)

(* Completion enqueue and cancellation share the guard, so the first one wins. *)
let notify t turn notice =
  let wake = with_guard t (fun () ->
    match t.active_turn with
    | Some active when not t.closed && active.id = turn.id ->
        let notice = match notice with
          | Finished (id, Completed) when Atomic.get turn.cancelled ->
              Finished (id, Cancelled)
          | _ -> notice in
        let empty = Queue.is_empty t.notices in
        Queue.add notice t.notices;
        empty
    | _ -> false) in
  if wake then (
    let rec write () =
      try ignore (Unix.write t.write_fd t.wake_byte 0 1)
      with
      | Unix.Unix_error (Unix.EINTR, _, _) -> write ()
      | Unix.Unix_error (Unix.EAGAIN, _, _) -> () in
    write ())

let message t text =
  Option.iter (fun turn -> notify t turn (Message (turn.id, text)))
    (worker_turn t)

let delta t text =
  Option.iter (fun turn -> notify t turn (Delta (turn.id, text)))
    (worker_turn t)

let phase t value =
  Option.iter (fun turn -> notify t turn (Phase (turn.id, value)))
    (worker_turn t)

let tool t event =
  Option.iter (fun turn -> notify t turn (Tool (turn.id, event)))
    (worker_turn t)

let answer request result =
  Mutex.lock request.mutex;
  (match request.answer with
   | None -> request.answer <- Some result; Condition.signal request.condition
   | Some _ -> ());
  Mutex.unlock request.mutex

let approve t command =
  let turn = match worker_turn t with
    | Some turn -> turn
    | None -> raise Provider.Cancelled in
  let cancel () = Atomic.get turn.cancelled in
  if cancel () then raise Provider.Cancelled;
  let request = { mutex = Mutex.create (); condition = Condition.create ();
    answer = None } in
  with_guard t (fun () -> t.approvals <- request :: t.approvals);
  notify t turn (Approve (turn.id, command, request, cancel));
  Mutex.lock request.mutex;
  let rec await () = match request.answer with
    | Some result -> result
    | None -> Condition.wait request.condition request.mutex; await () in
  let result = Fun.protect ~finally:(fun () -> Mutex.unlock request.mutex) await in
  with_guard t (fun () ->
    t.approvals <- List.filter (fun candidate -> candidate != request) t.approvals);
  if cancel () then raise Provider.Cancelled;
  result

let start t text =
  if t.closed then invalid_arg "turn runner closed";
  let turn_id = t.next_turn_id in
  t.next_turn_id <- t.next_turn_id + 1;
  t.on_event (Turn_started { turn_id; prompt = text });
  let turn = { id = turn_id; cancelled = Atomic.make false;
    thread_id = None } in
  with_guard t (fun () -> t.active_turn <- Some turn);
  (try
     t.worker <- Some (Thread.create (fun () ->
       with_guard t (fun () -> turn.thread_id <- Some (Thread.id (Thread.self ())));
       let cancel () = Atomic.get turn.cancelled in
       let outcome = try
         t.run ~cancel text;
         if cancel () then Cancelled else Completed
       with
       | Provider.Cancelled -> Cancelled
       | exn -> Failed exn in
       notify t turn (Finished (turn.id, outcome))) ())
   with exn ->
     with_guard t (fun () ->
       match t.active_turn with
       | Some active when active.id = turn.id -> t.active_turn <- None
       | _ -> ());
     emit_finish t turn.id (Failed exn))

let submit t text =
  if t.closed then invalid_arg "turn runner closed";
  if busy t then (
    Queue.add text t.pending;
    t.on_queued (Queue.length t.pending))
  else start t text

let cancel t =
  let requests = with_guard t (fun () ->
    Option.iter (fun turn -> Atomic.set turn.cancelled true) t.active_turn;
    t.approvals) in
  List.iter (fun request -> answer request false) requests

let drain_pipe t =
  let bytes = t.drain_bytes in
  let rec read () =
    try
      let count = Unix.read t.read_fd bytes 0 (Bytes.length bytes) in
      if count > 0 then read ()
    with Unix.Unix_error ((Unix.EAGAIN | Unix.EINTR), _, _) -> () in
  read ()

let drain t =
  drain_pipe t;
  let rec handle () =
    let notice = with_guard t (fun () ->
      if Queue.is_empty t.notices then None else Some (Queue.take t.notices)) in
    match notice with
    | None -> ()
    | Some (Message (id, text)) ->
        if not (cancel_requested t id) then
          t.on_event (Transcript_message { turn_id = id; text });
        handle ()
    | Some (Delta (id, text)) ->
        if not (cancel_requested t id) then
          t.on_event (Text_delta { turn_id = id; text });
        handle ()
    | Some (Phase (id, phase)) ->
        if not (cancel_requested t id) then
          t.on_event (Activity_phase { turn_id = id; phase });
        handle ()
    | Some (Tool (id, event)) ->
        let terminal = match event with
          | Agent.Tool_settled _ | Agent.Tool_aborted _ -> true
          | Agent.Tool_started _ | Agent.Tool_updated _ -> false in
        if terminal || not (cancel_requested t id) then
          t.on_event (Tool_event { turn_id = id; event });
        handle ()
    | Some (Approve (id, command, request, cancelled)) ->
        if cancelled () || cancel_requested t id then answer request false
        else (try answer request (t.on_approve command)
          with exn ->
            answer request false;
            if not (cancel_requested t id) then
              t.on_event (Transcript_message {
                turn_id = id;
                text = "Error: shell approval failed: " ^ Printexc.to_string exn
              }));
        handle ()
    | Some (Finished (id, outcome)) ->
        (match active_turn t id with
         | None -> ()
         | Some _ ->
             (match t.worker with Some worker -> Thread.join worker | None -> ());
             t.worker <- None;
             with_guard t (fun () -> t.active_turn <- None);
             emit_finish t id outcome;
             if not t.closed && not (Queue.is_empty t.pending) then (
               start t (Queue.take t.pending);
               t.on_queued (Queue.length t.pending)));
        handle () in
  handle ()

let close t =
  if not t.closed then (
    Queue.clear t.pending;
    cancel t;
    (match t.worker with Some worker -> Thread.join worker | None -> ());
    t.worker <- None;
    with_guard t (fun () -> t.active_turn <- None);
    t.closed <- true;
    Unix.close t.read_fd;
    Unix.close t.write_fd)
