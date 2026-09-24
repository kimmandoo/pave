type completion = Completed | Cancelled | Failed of exn

type approval = {
  mutex : Mutex.t;
  condition : Condition.t;
  mutable answer : bool option;
}

type notice =
  | Message of string
  | Delta of string
  | Approve of string * approval * (unit -> bool)
  | Finished of completion

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
  mutable cancel_flag : bool Atomic.t option;
  mutable closed : bool;
  run : cancel:(unit -> bool) -> string -> unit;
  on_message : string -> unit;
  on_delta : string -> unit;
  on_approve : string -> bool;
  on_start : string -> unit;
  on_finish : completion -> unit;
  on_queued : int -> unit;
}

let with_guard t callback =
  Mutex.lock t.guard;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.guard) callback

let create ~run ~on_message ~on_delta ~on_approve ~on_start ~on_finish
    ~on_queued () =
  let read_fd, write_fd = Unix.pipe () in
  Unix.set_close_on_exec read_fd;
  Unix.set_close_on_exec write_fd;
  Unix.set_nonblock read_fd;
  Unix.set_nonblock write_fd;
  { read_fd; write_fd; wake_byte = Bytes.of_string "x";
    drain_bytes = Bytes.create 256;
    guard = Mutex.create (); notices = Queue.create (); approvals = [];
    pending = Queue.create (); worker = None; cancel_flag = None; closed = false;
    run; on_message; on_delta; on_approve; on_start; on_finish; on_queued }

let fd t = t.read_fd
let busy t = t.worker <> None

let notify t notice =
  let wake = with_guard t (fun () ->
    if t.closed then false else (
      let empty = Queue.is_empty t.notices in
      Queue.add notice t.notices;
      empty)) in
  if wake then (
    let rec write () =
      try ignore (Unix.write t.write_fd t.wake_byte 0 1)
      with
      | Unix.Unix_error (Unix.EINTR, _, _) -> write ()
      | Unix.Unix_error (Unix.EAGAIN, _, _) -> () in
    write ())

let message t text = notify t (Message text)
let delta t text = notify t (Delta text)

let answer request result =
  Mutex.lock request.mutex;
  (match request.answer with
   | None -> request.answer <- Some result; Condition.signal request.condition
   | Some _ -> ());
  Mutex.unlock request.mutex

let approve t command =
  let cancel () = match t.cancel_flag with
    | Some flag -> Atomic.get flag
    | None -> true in
  if cancel () then raise Provider.Cancelled;
  let request = { mutex = Mutex.create (); condition = Condition.create ();
    answer = None } in
  with_guard t (fun () -> t.approvals <- request :: t.approvals);
  notify t (Approve (command, request, cancel));
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
  t.on_start text;
  let flag = Atomic.make false in
  t.cancel_flag <- Some flag;
  t.worker <- Some (Thread.create (fun () ->
    let cancel () = Atomic.get flag in
    let outcome = try
      t.run ~cancel text;
      Completed
    with
    | Provider.Cancelled -> Cancelled
    | exn -> Failed exn in
    notify t (Finished outcome)) ())

let submit t text =
  if t.closed then invalid_arg "turn runner closed";
  if busy t then (
    Queue.add text t.pending;
    t.on_queued (Queue.length t.pending))
  else start t text

let cancel t =
  (match t.cancel_flag with None -> () | Some flag -> Atomic.set flag true);
  let requests = with_guard t (fun () -> t.approvals) in
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
    | Some (Message text) -> t.on_message text; handle ()
    | Some (Delta text) -> t.on_delta text; handle ()
    | Some (Approve (command, request, cancelled)) ->
        if cancelled () then answer request false
        else (try answer request (t.on_approve command)
          with exn ->
            answer request false;
            t.on_message ("Error: shell approval failed: " ^ Printexc.to_string exn));
        handle ()
    | Some (Finished outcome) ->
        (match t.worker with Some worker -> Thread.join worker | None -> ());
        t.worker <- None;
        t.cancel_flag <- None;
        t.on_finish outcome;
        if not t.closed && not (Queue.is_empty t.pending) then (
          start t (Queue.take t.pending);
          t.on_queued (Queue.length t.pending));
        handle () in
  handle ()

let close t =
  if not t.closed then (
    Queue.clear t.pending;
    cancel t;
    (match t.worker with Some worker -> Thread.join worker | None -> ());
    t.worker <- None;
    t.closed <- true;
    Unix.close t.read_fd;
    Unix.close t.write_fd)
