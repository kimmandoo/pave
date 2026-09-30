type completion = Completed | Cancelled | Failed of exn
type submission = {
  prompt : string;
  display_prompt : string;
  attachments : Protocol.attachment list;
  paste_ranges : (int * int) list;
}
type event =
  | Turn_started of { turn_id : int; submission : submission }
  | Transcript_message of { turn_id : int; text : string }
  | Text_delta of { turn_id : int; text : string }
  | Activity_phase of { turn_id : int; phase : Agent.phase }
  | Tool_event of { turn_id : int; event : Agent.tool_event }
  | Draft_preview of {
      turn_id : int; key : string; name : string;
      preview : Write_preview.snapshot
    }
  | Turn_completed of { turn_id : int }
  | Turn_cancelled of { turn_id : int }
  | Turn_failed of { turn_id : int; error : exn }
  | Background_notice of { message : string }

type submission_kind = Steering | Follow_up

type queued_submission = {
  id : int;
  kind : submission_kind;
  submission : submission;
}

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

type draft = {
  key : string;
  mutable name : string;
  decoder : Write_preview.t;
  mutable pending : bool;
}

type notice =
  | Message of int * string
  | Delta of int * string
  | Phase of int * Agent.phase
  | Tool of int * Agent.tool_event
  | Preview of int * draft
  | Approve of int * string * approval * (unit -> bool)
  | Approve_tool of int * Approval.request * approval * (unit -> bool)
  | Finished of int * completion
  | External of string

type t = {
  read_fd : Unix.file_descr;
  write_fd : Unix.file_descr;
  wake_byte : bytes;
  drain_bytes : bytes;
  guard : Mutex.t;
  notices : notice Queue.t;
  drafts : (string, draft) Hashtbl.t;
  mutable approvals : approval list;
  steering : queued_submission Queue.t;
  follow_ups : queued_submission Queue.t;
  mutable worker : Thread.t option;
  mutable active_turn : turn option;
  mutable next_turn_id : int;
  mutable next_queue_id : int;
  mutable closed : bool;
  run : cancel:(unit -> bool) -> submission -> unit;
  on_event : event -> unit;
  on_approve : string -> bool;
  on_approve_tool : Approval.request -> bool;
  on_queued : int -> unit;
}

let with_guard t callback =
  Mutex.lock t.guard;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.guard) callback

let create ~run ~on_event ~on_approve ~on_queued
    ?(on_approve_tool = fun _ -> false) () =
  let read_fd, write_fd = Unix.pipe () in
  Unix.set_close_on_exec read_fd;
  Unix.set_close_on_exec write_fd;
  Unix.set_nonblock read_fd;
  Unix.set_nonblock write_fd;
  { read_fd; write_fd; wake_byte = Bytes.of_string "x";
    drain_bytes = Bytes.create 256;
    guard = Mutex.create (); notices = Queue.create ();
    drafts = Hashtbl.create 8; approvals = [];
    steering = Queue.create (); follow_ups = Queue.create ();
    worker = None; active_turn = None;
    next_turn_id = 0; next_queue_id = 0; closed = false;
    run; on_event; on_approve; on_approve_tool; on_queued }
let fd t = t.read_fd
let busy t = t.worker <> None


let queued_count t = Queue.length t.steering + Queue.length t.follow_ups

let queued t =
  List.rev (Queue.fold (fun pending item -> item :: pending)
    (Queue.fold (fun pending item -> item :: pending) [] t.steering)
    t.follow_ups)

let make_queued t kind submission =
  let id = t.next_queue_id in
  t.next_queue_id <- id + 1;
  { id; kind; submission }


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
      | Unix.Unix_error ((Unix.EAGAIN | Unix.EPIPE | Unix.EBADF), _, _) -> () in
    write ())

let post t message =
  let wake = with_guard t (fun () ->
    if t.closed then false
    else (
      let empty = Queue.is_empty t.notices in
      Queue.add (External message) t.notices;
      empty)) in
  if wake then (
    let rec write () =
      try ignore (Unix.write t.write_fd t.wake_byte 0 1)
      with
      | Unix.Unix_error (Unix.EINTR, _, _) -> write ()
      | Unix.Unix_error ((Unix.EAGAIN | Unix.EPIPE | Unix.EBADF), _, _) -> () in
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
  Option.iter (fun turn ->
    match event with
    | Agent.Tool_draft delta ->
        let draft = with_guard t (fun () ->
          if Atomic.get turn.cancelled then None else
          let draft = match Hashtbl.find_opt t.drafts delta.key with
            | Some draft -> Some draft
            | None when Hashtbl.length t.drafts < 128 ->
                let draft = { key = delta.key; name = delta.name;
                  decoder = Write_preview.create (); pending = false } in
                Hashtbl.add t.drafts delta.key draft; Some draft
            | None -> None in
          Option.bind draft (fun draft ->
            draft.name <- delta.name;
            Write_preview.feed draft.decoder delta.fragment;
            if draft.pending then None else (
              draft.pending <- true; Some draft))) in
        Option.iter (fun draft -> notify t turn (Preview (turn.id, draft))) draft
    | Agent.Tool_draft_ended { key; _ } ->
        with_guard t (fun () -> Hashtbl.remove t.drafts key);
        notify t turn (Tool (turn.id, event))
    | _ -> notify t turn (Tool (turn.id, event))) (worker_turn t)

let answer request result =
  Mutex.lock request.mutex;
  (match request.answer with
   | None -> request.answer <- Some result; Condition.signal request.condition
   | Some _ -> ());
  Mutex.unlock request.mutex

let register_approval t turn request =
  with_guard t (fun () ->
    if t.closed || Atomic.get turn.cancelled then raise Provider.Cancelled;
    t.approvals <- request :: t.approvals)

let approve t command =
  let turn = match worker_turn t with
    | Some turn -> turn
    | None -> raise Provider.Cancelled in
  let cancel () = Atomic.get turn.cancelled in
  if cancel () then raise Provider.Cancelled;
  let request = { mutex = Mutex.create (); condition = Condition.create ();
    answer = None } in
  register_approval t turn request;
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
let approve_tool t approval_request =
  let turn = match worker_turn t with
    | Some turn -> turn
    | None -> raise Provider.Cancelled in
  let cancel () = Atomic.get turn.cancelled in
  if cancel () then raise Provider.Cancelled;
  let request = { mutex = Mutex.create (); condition = Condition.create ();
    answer = None } in
  register_approval t turn request;
  notify t turn (Approve_tool (turn.id, approval_request, request, cancel));
  Mutex.lock request.mutex;
  let rec await () = match request.answer with
    | Some result -> result
    | None -> Condition.wait request.condition request.mutex; await () in
  let result = Fun.protect ~finally:(fun () -> Mutex.unlock request.mutex) await in
  with_guard t (fun () ->
    t.approvals <- List.filter (fun candidate -> candidate != request) t.approvals);
  if cancel () then raise Provider.Cancelled;
  result

let start t submission =
  if t.closed then invalid_arg "turn runner closed";
  let turn_id = t.next_turn_id in
  t.next_turn_id <- t.next_turn_id + 1;
  t.on_event (Turn_started { turn_id; submission });
  let turn = { id = turn_id; cancelled = Atomic.make false;
    thread_id = None } in
  with_guard t (fun () -> t.active_turn <- Some turn);
  (try
     t.worker <- Some (Thread.create (fun () ->
       with_guard t (fun () -> turn.thread_id <- Some (Thread.id (Thread.self ())));
       let cancel () = Atomic.get turn.cancelled in
       let outcome = try
         t.run ~cancel submission;
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

let make_submission ?display_prompt ?(attachments = []) ?(paste_ranges = [])
    prompt =
  let display_prompt = Option.value display_prompt ~default:prompt in
  { prompt; display_prompt; attachments; paste_ranges }

let follow_up t ?display_prompt ?(attachments = []) ?(paste_ranges = []) text =
  if t.closed then invalid_arg "turn runner closed";
  let submission = make_submission ?display_prompt ~attachments
      ~paste_ranges text in
  if busy t then (
    Queue.add (make_queued t Follow_up submission) t.follow_ups;
    t.on_queued (queued_count t))
  else start t submission

let submit = follow_up

let cancel t =
  let requests = with_guard t (fun () ->
    Option.iter (fun turn -> Atomic.set turn.cancelled true) t.active_turn;
    t.approvals) in
  List.iter (fun request -> answer request false) requests

let steer t ?display_prompt ?(attachments = []) ?(paste_ranges = []) text =
  if t.closed then invalid_arg "turn runner closed";
  let submission = make_submission ?display_prompt ~attachments
      ~paste_ranges text in
  if busy t then (
    Queue.add (make_queued t Steering submission) t.steering;
    t.on_queued (queued_count t);
    cancel t)
  else start t submission

let remove_queued t ~id =
  let remove queue =
    if not (Queue.fold (fun found (item : queued_submission) ->
      found || item.id = id) false queue) then None
    else (
      let found = ref None in
      for _ = 1 to Queue.length queue do
        let (item : queued_submission) = Queue.take queue in
        if item.id = id then found := Some item
        else Queue.add item queue
      done;
      !found) in
  match remove t.steering with
  | Some _ as found -> found
  | None -> remove t.follow_ups

let take_queued t ~id =
  let selected = remove_queued t ~id in
  Option.iter (fun _ -> t.on_queued (queued_count t)) selected;
  selected

let prioritize_queued t ~id ~interrupt =
  if t.closed then false
  else match remove_queued t ~id with
  | None -> false
  | Some selected ->
      if busy t then (
        let remaining = Queue.create () in
        Queue.transfer t.steering remaining;
        Queue.add { selected with kind = Steering } t.steering;
        Queue.transfer remaining t.steering;
        t.on_queued (queued_count t);
        if interrupt then cancel t)
      else (
        start t selected.submission;
        t.on_queued (queued_count t));
      true

let pop_last queue =
  if Queue.is_empty queue then None
  else (
    let earlier = Queue.create () in
    while Queue.length queue > 1 do
      Queue.add (Queue.take queue) earlier
    done;
    let last = Queue.take queue in
    Queue.iter (fun queued -> Queue.add queued queue) earlier;
    Some last)

let dequeue_last t =
  let queued = match pop_last t.steering with
    | Some queued -> Some queued
    | None -> pop_last t.follow_ups in
  (match queued with
  | None -> ()
  | Some _ -> t.on_queued (queued_count t));
  queued

let restore_dequeued t queued =
  if t.closed then invalid_arg "turn runner closed";
  (match queued.kind with
  | Steering -> Queue.add queued t.steering
  | Follow_up -> Queue.add queued t.follow_ups);
  t.on_queued (queued_count t)
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
    | Some (External message) ->
        t.on_event (Background_notice { message });
        handle ()
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
    | Some (Preview (id, draft)) ->
        let name, preview = with_guard t (fun () ->
          draft.pending <- false;
          draft.name, Write_preview.snapshot draft.decoder) in
        if not (cancel_requested t id) then
          t.on_event (Draft_preview {
            turn_id = id; key = draft.key; name; preview });
        handle ()
    | Some (Tool (id, event)) ->
        let terminal = match event with
          | Agent.Tool_settled _ | Agent.Tool_aborted _ |
            Agent.Tool_draft_ended _ -> true
          | Agent.Tool_draft _ | Agent.Tool_started _ | Agent.Tool_executing _ |
            Agent.Tool_updated _ -> false in
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
    | Some (Approve_tool (id, approval_request, request, cancelled)) ->
        if cancelled () || cancel_requested t id then answer request false
        else (try answer request (t.on_approve_tool approval_request)
          with exn ->
            answer request false;
            if not (cancel_requested t id) then
              t.on_event (Transcript_message {
                turn_id = id;
                text = "Error: tool approval failed: " ^ Printexc.to_string exn
              }));
        handle ()
    | Some (Finished (id, outcome)) ->
        (match active_turn t id with
        | None -> ()
        | Some _ ->
            (match t.worker with Some worker -> Thread.join worker | None -> ());
            t.worker <- None;
            with_guard t (fun () -> Hashtbl.clear t.drafts);
            with_guard t (fun () -> t.active_turn <- None);
            emit_finish t id outcome;
            if not t.closed then (
              let queued = if not (Queue.is_empty t.steering) then
                  Some (Queue.take t.steering)
                else if not (Queue.is_empty t.follow_ups) then
                  Some (Queue.take t.follow_ups)
                else None in
              Option.iter (fun queued ->
                start t queued.submission;
                t.on_queued (queued_count t)) queued));
        handle () in
  handle ()

let close t =
  if not t.closed then (
    Queue.clear t.steering;
    Queue.clear t.follow_ups;
    cancel t;
    (match t.worker with Some worker -> Thread.join worker | None -> ());
    with_guard t (fun () -> t.closed <- true);
    Fun.protect ~finally:(fun () ->
      t.worker <- None;
      with_guard t (fun () ->
        t.active_turn <- None;
        Hashtbl.clear t.drafts;
        Queue.clear t.notices);
      Unix.close t.read_fd;
      Unix.close t.write_fd) (fun () -> drain t))
