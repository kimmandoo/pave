(* Approval consumers may explicitly request shutdown without teaching this
   library about terminal-specific exception types. *)
exception Stop of exn
exception Queue_full


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
  (* Submissions originate on the owning thread; producers on other
     threads (e.g. the session hub) enqueue here so every queue/start
     mutation stays on the thread that calls drain. *)
  | Enqueue of queued_submission
  | Cancel_remote of int option

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
  dequeued : (int, queued_submission) Hashtbl.t;
  mutable queued_bytes : int;
  mutable notice_bytes : int;

  pending : int Atomic.t;
  dequeued_count : int Atomic.t;
  mutable worker : Thread.t option;
  mutable active_turn : turn option;
  mutable next_turn_id : int;
  mutable next_queue_id : int;
  mutable closed : bool;
  owner : Thread.t;
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
    dequeued = Hashtbl.create 4; queued_bytes = 0; notice_bytes = 0;
    pending = Atomic.make 0; dequeued_count = Atomic.make 0;
    worker = None; active_turn = None;
    next_turn_id = 0; next_queue_id = 0; closed = false;
    owner = Thread.self ();
    run; on_event; on_approve; on_approve_tool; on_queued }

let fd t = t.read_fd
let busy t = t.worker <> None


let queued_count t =
  Atomic.get t.pending - Atomic.get t.dequeued_count
(* Follow-up/steering admission: 32 retained submissions / 1 MiB counting
   prompt/display text, attachment names/MIME/payload, and paste-range metadata. *)
let max_queued_items = 32
let max_queued_bytes = 1_048_576

let submission_bytes submission =
  let attachment_bytes =
    List.fold_left (fun total (attachment : Protocol.attachment) ->
      total + String.length attachment.name + String.length attachment.mime_type +
      String.length attachment.data) 0 submission.attachments in
  let ranges_bytes = 16 * List.length submission.paste_ranges in
  String.length submission.prompt + String.length submission.display_prompt +
  attachment_bytes + ranges_bytes

let reserve_queued t kind submission =
  with_guard t (fun () ->
    if t.closed then invalid_arg "turn runner closed";
    let bytes = submission_bytes submission in
    if Atomic.get t.pending >= max_queued_items ||
       bytes > max_queued_bytes - t.queued_bytes then raise Queue_full;
    let id = t.next_queue_id in
    t.next_queue_id <- id + 1;
    t.queued_bytes <- t.queued_bytes + bytes;
    ignore (Atomic.fetch_and_add t.pending 1);
    { id; kind; submission })

let release_queued t queued =
  with_guard t (fun () ->
    t.queued_bytes <- max 0
      (t.queued_bytes - submission_bytes queued.submission);
    let pending = Atomic.get t.pending in
    if pending > 0 then Atomic.set t.pending (pending - 1))


let queued t =
  List.rev (Queue.fold (fun pending item -> item :: pending)
    (Queue.fold (fun pending item -> item :: pending) [] t.steering)
    t.follow_ups)

let make_queued = reserve_queued

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
let wake t =
  let rec write () =
    try ignore (Unix.write t.write_fd t.wake_byte 0 1)
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> write ()
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EPIPE | Unix.EBADF), _, _) -> () in
  write ()

(* Notice backlog: 4096 items / 4 MiB total; ordinary stream notices stop at
   3840 items / 3 MiB, leaving terminal/approval/tool outcomes reserved. *)
let max_notice_events = 4096
let max_notice_bytes = 4_194_304
let reserved_notice_events = 256
let reserved_notice_bytes = 1_048_576
(* Keep terminal-input work responsive when a worker streams continuously. *)
let max_drain_events = 128

let optional_string_bytes = function
  | None -> 0
  | Some text -> String.length text

let approval_bytes (request : Approval.request) =
  let bytes = String.length request.tool_name + String.length request.impact +
    optional_string_bytes request.reason +
    List.fold_left (fun bytes detail -> bytes + String.length detail) 0 request.details in
  bytes + Option.fold ~none:0 ~some:(fun (review : Approval.sensitive_review) ->
    List.fold_left (fun bytes (effect : Approval.sensitive_effect) ->
      bytes + String.length effect.effect_path + String.length effect.effect_summary)
      0 review.effects +
    List.fold_left (fun bytes path -> bytes + String.length path) 0 review.unresolved +
    List.fold_left (fun bytes (target : Approval.sensitive_target) ->
      bytes + String.length target.target_path + String.length target.original_sha256 +
      String.length target.result_sha256) 0 review.targets) request.sensitive

let tool_event_bytes = function
  | Agent.Tool_started { call_id; name; target; write_content } ->
      String.length call_id + String.length name + optional_string_bytes target +
      optional_string_bytes write_content
  | Agent.Tool_settled { call_id; name; result; _ }
  | Agent.Tool_aborted { call_id; name; result; _ } ->
      String.length call_id + String.length name + String.length result
  | Agent.Tool_executing { call_id; name }
  | Agent.Tool_updated { call_id; name; _ } ->
      String.length call_id + String.length name
  | Agent.Tool_draft delta ->
      String.length delta.key + optional_string_bytes delta.call_id +
      String.length delta.name + String.length delta.fragment
  | Agent.Tool_draft_ended { key; call_id; _ } ->
      String.length key + optional_string_bytes call_id

let notice_bytes = function
  | Enqueue queued -> submission_bytes queued.submission
  | Message (_, text) | Delta (_, text) | External text -> String.length text
  | Tool (_, event) -> tool_event_bytes event + 64
  | Approve (_, command, _, _) -> String.length command + 64
  | Approve_tool (_, request, _, _) -> approval_bytes request + 64
  | _ -> 64

let critical_notice = function
  | Finished _ | Approve _ | Approve_tool _ | Cancel_remote _ -> true
  | Tool (_, Agent.Tool_settled _)
  | Tool (_, Agent.Tool_aborted _)
  | Tool (_, Agent.Tool_draft_ended _) -> true
  | _ -> false

let post_notice t notice =
  let size = notice_bytes notice in
  let fresh = with_guard t (fun () ->
    let critical = critical_notice notice in
    let ceiling = if critical then max_notice_events
      else max_notice_events - reserved_notice_events in
    let byte_ceiling = if critical then max_notice_bytes
      else max_notice_bytes - reserved_notice_bytes in
    if t.closed || Queue.length t.notices >= ceiling ||
       size > byte_ceiling - t.notice_bytes then false
    else (
      let empty = Queue.is_empty t.notices in
      Queue.add notice t.notices;
      t.notice_bytes <- t.notice_bytes + size;
      empty)) in
  if fresh then wake t

let enqueue_remote t kind submission =
  let queued_size = submission_bytes submission in
  let notice = Enqueue { id = 0; kind; submission } in
  let size = notice_bytes notice in
  let fresh = with_guard t (fun () ->
    if t.closed then invalid_arg "turn runner closed";
    let byte_ceiling = max_notice_bytes - reserved_notice_bytes in
    if Atomic.get t.pending >= max_queued_items ||
       queued_size > max_queued_bytes - t.queued_bytes ||
       Queue.length t.notices >= max_notice_events - reserved_notice_events ||
       size > byte_ceiling - t.notice_bytes then raise Queue_full;
    let id = t.next_queue_id in
    t.next_queue_id <- id + 1;
    let queued = { id; kind; submission } in
    let empty = Queue.is_empty t.notices in
    Queue.add (Enqueue queued) t.notices;
    t.queued_bytes <- t.queued_bytes + queued_size;
    t.notice_bytes <- t.notice_bytes + size;
    ignore (Atomic.fetch_and_add t.pending 1);
    empty) in
  if fresh then wake t
let notify t turn notice =
  let rec enqueue () =
    let result = with_guard t (fun () ->
      match t.active_turn with
      | Some active when active.id = turn.id &&
                         (not t.closed || critical_notice notice) ->
          let notice = match notice with
            | Finished (id, Completed) when Atomic.get turn.cancelled ->
                Finished (id, Cancelled)
            | _ -> notice in
          let size = notice_bytes notice in
          if size > max_notice_bytes then `Too_large
          else
            let critical = critical_notice notice ||
              size > max_notice_bytes - reserved_notice_bytes in
            let ceiling = if critical then max_notice_events
              else max_notice_events - reserved_notice_events in
            let byte_ceiling = if critical then max_notice_bytes
              else max_notice_bytes - reserved_notice_bytes in
            if Queue.length t.notices >= ceiling ||
               size > byte_ceiling - t.notice_bytes then `Full
            else (
              let empty = Queue.is_empty t.notices in
              Queue.add notice t.notices;
              t.notice_bytes <- t.notice_bytes + size;
              `Accepted empty)
      | _ -> `Gone) in
    match result with
    | `Accepted fresh -> if fresh then wake t
    | `Full -> Thread.delay 0.001; enqueue ()
    | `Gone -> ()
    | `Too_large ->
        invalid_arg "turn runner notice exceeds the bounded event size" in
  enqueue ()

let post t message = post_notice t (External message)

let remote t = Thread.self () <> t.owner

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

let start_worker t ~on_start ~run =
  if remote t then invalid_arg "turn runner work must start on its owner thread";
  if t.closed then invalid_arg "turn runner closed";
  if busy t then invalid_arg "turn runner busy";
  let turn_id = t.next_turn_id in
  t.next_turn_id <- t.next_turn_id + 1;
  on_start turn_id;
  let turn = { id = turn_id; cancelled = Atomic.make false;
    thread_id = None } in
  with_guard t (fun () -> t.active_turn <- Some turn);
  (try
     t.worker <- Some (Thread.create (fun () ->
       with_guard t (fun () -> turn.thread_id <- Some (Thread.id (Thread.self ())));
       let cancel () = Atomic.get turn.cancelled in
       let outcome = try
         run ~cancel;
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

let start t submission =
  start_worker t
    ~on_start:(fun turn_id ->
      t.on_event (Turn_started { turn_id; submission }))
    ~run:(fun ~cancel -> t.run ~cancel submission)

(* User-launched non-model work shares cancellation, approval ownership and
   FIFO continuation, but never invents a provider prompt or recording frame. *)
let start_work t ~run () =
  start_worker t ~on_start:(fun _ -> ()) ~run

let make_submission ?display_prompt ?(attachments = []) ?(paste_ranges = [])
    prompt =
  let display_prompt = Option.value display_prompt ~default:prompt in
  { prompt; display_prompt; attachments; paste_ranges }

(* A submission arriving on another thread is marshalled through the notice
   pipe: the owning thread performs the queue/start mutation inside drain,
   keeping queues, turn ids and TUI events single-threaded. *)
let follow_up t ?display_prompt ?(attachments = []) ?(paste_ranges = []) text =
  if t.closed then invalid_arg "turn runner closed";
  let submission = make_submission ?display_prompt ~attachments
      ~paste_ranges text in
  if remote t then
    enqueue_remote t Follow_up submission
  else if busy t then (
    let queued = make_queued t Follow_up submission in
    Queue.add queued t.follow_ups;
    t.on_queued (queued_count t))
  else start t submission

let submit = follow_up

let cancel_local t =
  let requests = with_guard t (fun () ->
    Option.iter (fun turn -> Atomic.set turn.cancelled true) t.active_turn;
    t.approvals) in
  List.iter (fun request -> answer request false) requests

let cancel t =
  if remote t then (
    let turn = with_guard t (fun () -> t.active_turn) in
    Option.iter (fun turn ->
      (* Cancellation is an admitted control, not a best-effort background
         notice: backpressure must never silently discard it. *)
      notify t turn (Cancel_remote (Some turn.id))) turn)
  else cancel_local t

let steer t ?display_prompt ?(attachments = []) ?(paste_ranges = []) text =
  if t.closed then invalid_arg "turn runner closed";
  let submission = make_submission ?display_prompt ~attachments
      ~paste_ranges text in
  if remote t then
    enqueue_remote t Steering submission
  else if busy t then (
    let queued = make_queued t Steering submission in
    Queue.add queued t.steering;
    t.on_queued (queued_count t);
    cancel_local t)
  else start t submission

let remove_queued ?(release = true) t ~id =
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
  let selected = match remove t.steering with
    | Some _ as found -> found
    | None -> remove t.follow_ups in
  if release then Option.iter (release_queued t) selected;
  selected

let take_queued t ~id =
  let selected = remove_queued t ~id in
  Option.iter (fun _ -> t.on_queued (queued_count t)) selected;
  selected

let prioritize_queued t ~id ~interrupt =
  if t.closed then false
  else match remove_queued ~release:false t ~id with
  | None -> false
  | Some selected ->
      if busy t then (
        let remaining = Queue.create () in
        Queue.transfer t.steering remaining;
        Queue.add { selected with kind = Steering } t.steering;
        Queue.transfer remaining t.steering;
        t.on_queued (queued_count t);
        if interrupt then cancel_local t)
      else (
        release_queued t selected;
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
  match pop_last t.steering with
  | Some queued ->
      with_guard t (fun () ->
        Hashtbl.add t.dequeued queued.id queued;
        ignore (Atomic.fetch_and_add t.dequeued_count 1));
      t.on_queued (queued_count t);
      Some queued
  | None ->
      (match pop_last t.follow_ups with
       | None -> None
       | Some queued ->
           with_guard t (fun () ->
             Hashtbl.add t.dequeued queued.id queued;
             ignore (Atomic.fetch_and_add t.dequeued_count 1));
           t.on_queued (queued_count t);
           Some queued)

let release_dequeued (t : t) (queued : queued_submission) =
  let released = with_guard t (fun () ->
    match Hashtbl.find_opt t.dequeued queued.id with
    | Some retained when retained = queued ->
        Hashtbl.remove t.dequeued queued.id;
        t.queued_bytes <- t.queued_bytes - submission_bytes queued.submission;
        ignore (Atomic.fetch_and_add t.pending (-1));
        ignore (Atomic.fetch_and_add t.dequeued_count (-1));
        true
    | _ -> false) in
  if not released then invalid_arg "turn runner dequeued item is not retained"

let restore_dequeued (t : t) (queued : queued_submission) =
  let restored = with_guard t (fun () ->
    if t.closed then invalid_arg "turn runner closed";
    match Hashtbl.find_opt t.dequeued queued.id with
    | Some retained when retained = queued ->
        Hashtbl.remove t.dequeued queued.id;
        (match queued.kind with
         | Steering -> Queue.add queued t.steering
         | Follow_up -> Queue.add queued t.follow_ups);
        ignore (Atomic.fetch_and_add t.dequeued_count (-1));
        true
    | _ -> false) in
  if not restored then invalid_arg "turn runner dequeued item is not retained";
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
  let rec handle remaining =
    if remaining = 0 then (
      let pending = with_guard t (fun () -> not (Queue.is_empty t.notices)) in
      if pending then wake t)
    else
      let notice = with_guard t (fun () ->
        if Queue.is_empty t.notices then None
        else
          let notice = Queue.take t.notices in
          t.notice_bytes <- max 0 (t.notice_bytes - notice_bytes notice);
          Some notice) in
      match notice with
      | None -> ()
    | Some (External message) ->
        t.on_event (Background_notice { message });
        handle (remaining - 1)
    | Some (Enqueue queued) ->
        if t.closed then release_queued t queued
        else if busy t then (
          (match queued.kind with
           | Steering -> Queue.add queued t.steering
           | Follow_up -> Queue.add queued t.follow_ups);
          t.on_queued (queued_count t);
          if queued.kind = Steering then cancel_local t)
        else (
          release_queued t queued;
          start t queued.submission);
        handle (remaining - 1)
    | Some (Cancel_remote turn_id) ->
        (match turn_id with
         | Some id when Option.is_some (active_turn t id) -> cancel_local t
         | _ -> ());
        handle (remaining - 1)
    | Some (Message (id, text)) ->
        if not (cancel_requested t id) then
          t.on_event (Transcript_message { turn_id = id; text });
        handle (remaining - 1)
    | Some (Delta (id, text)) ->
        if not (cancel_requested t id) then
          t.on_event (Text_delta { turn_id = id; text });
        handle (remaining - 1)
    | Some (Phase (id, phase)) ->
        if not (cancel_requested t id) then
          t.on_event (Activity_phase { turn_id = id; phase });
        handle (remaining - 1)
    | Some (Preview (id, draft)) ->
        let name, preview = with_guard t (fun () ->
          draft.pending <- false;
          draft.name, Write_preview.snapshot draft.decoder) in
        if not (cancel_requested t id) then
          t.on_event (Draft_preview {
            turn_id = id; key = draft.key; name; preview });
        handle (remaining - 1)
    | Some (Tool (id, event)) ->
        let terminal = match event with
          | Agent.Tool_settled _ | Agent.Tool_aborted _ |
            Agent.Tool_draft_ended _ -> true
          | Agent.Tool_draft _ | Agent.Tool_started _ | Agent.Tool_executing _ |
            Agent.Tool_updated _ -> false in
        if terminal || not (cancel_requested t id) then
          t.on_event (Tool_event { turn_id = id; event });
        handle (remaining - 1)
    | Some (Approve (id, command, request, cancelled)) ->
        if cancelled () || cancel_requested t id then answer request false
        else (try answer request (t.on_approve command)
          with
          | Stop cause -> answer request false; raise cause
          | exn ->
            answer request false;
            if not (cancel_requested t id) then
              t.on_event (Transcript_message {
                turn_id = id;
                text = "Error: shell approval failed: " ^ Printexc.to_string exn
              }));
        handle (remaining - 1)
    | Some (Approve_tool (id, approval_request, request, cancelled)) ->
        if cancelled () || cancel_requested t id then answer request false
        else (try answer request (t.on_approve_tool approval_request)
          with
          | Stop cause -> answer request false; raise cause
          | exn ->
            answer request false;
            if not (cancel_requested t id) then
              t.on_event (Transcript_message {
                turn_id = id;
                text = "Error: tool approval failed: " ^ Printexc.to_string exn
              }));
        handle (remaining - 1)
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
                release_queued t queued;
                start t queued.submission;
                t.on_queued (queued_count t)) queued));
        handle (remaining - 1) in
  handle max_drain_events

let close t =
  let closing = with_guard t (fun () ->
    if t.closed then false
    else (t.closed <- true; true)) in
  if closing then (
    Queue.clear t.steering;
    Queue.clear t.follow_ups;
    with_guard t (fun () ->
      t.queued_bytes <- 0;
      Hashtbl.clear t.dequeued;
      Atomic.set t.pending 0;
      Atomic.set t.dequeued_count 0);
    cancel_local t;
    Fun.protect ~finally:(fun () ->
      with_guard t (fun () ->
        t.active_turn <- None;
        Hashtbl.clear t.drafts;
        Queue.clear t.notices;
        t.notice_bytes <- 0);
      Unix.close t.read_fd;
      Unix.close t.write_fd) (fun () ->
        (* Terminal notices may be waiting for bounded backlog capacity.
           Keep consuming them until Finished joins the worker; joining
           first would deadlock that producer against this owner thread. *)
        let rec drain_pending () =
          drain t;
          let pending = with_guard t (fun () -> not (Queue.is_empty t.notices)) in
          if busy t then (
            if not pending then
              ignore (Unix.select [t.read_fd] [] [] 0.05);
            drain_pending ())
          else if pending then drain_pending ()
        in
        drain_pending ()))
