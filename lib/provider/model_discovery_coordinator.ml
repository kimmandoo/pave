type scope = { provider : string; account_id : string option; route : string }

type status =
  | Loading
  | Ready of Model_discovery.listing
  | Unsupported of Model_discovery.error
  | Failed of Model_discovery.error

type snapshot = { scope : scope; status : status }
type request = {
  scope : scope;
  run :
    (unit -> bool) ->
    ((Model_discovery.listing, Model_discovery.error) result * string option);
}

type t = {
  read_fd : Unix.file_descr;
  write_fd : Unix.file_descr;
  cancelled : bool Atomic.t;
  lock : Mutex.t;
  mutable pending : (int * request) list;
  mutable outcomes : (int * status * string option) list;
  snapshots : snapshot array;
  mutable workers : Thread.t list;
  mutable closed : bool;
  timeout_seconds : float;
}

let notify t =
  try ignore (Unix.write_substring t.write_fd "x" 0 1)
  with Unix.Unix_error _ -> ()

let take t =
  Mutex.lock t.lock;
  let next = match t.pending with
    | [] -> None
    | entry :: rest ->
        t.pending <- rest;
        Some entry in
  Mutex.unlock t.lock;
  next

let publish t index status account_id =
  if not (Atomic.get t.cancelled) then (
    Mutex.lock t.lock;
    t.outcomes <- (index, status, account_id) :: t.outcomes;
    Mutex.unlock t.lock;
    notify t)

let classify = function
  | Ok listing -> Ready listing
  | Error (Model_discovery.Unsupported_provider _ as error)
  | Error (Model_discovery.Unsupported_route _ as error) -> Unsupported error
  | Error error -> Failed error

let rec work t () =
  if not (Atomic.get t.cancelled) then
    match take t with
    | None -> ()
    | Some (index, request) ->
        let deadline = Unix.gettimeofday () +. t.timeout_seconds in
        let timed_out () = Unix.gettimeofday () >= deadline in
        let request_cancelled () = Atomic.get t.cancelled || timed_out () in
        let outcome =
          try
            let result, account_id = request.run request_cancelled in
            if timed_out () then
              Some (Failed (Model_discovery.Transport_error "request timed out"),
                account_id)
            else Some (classify result, account_id)
          with
          | Provider.Cancelled when Atomic.get t.cancelled -> None
          | Provider.Cancelled ->
              Some (Failed (Model_discovery.Transport_error "request timed out"),
                None)
          | Failure _ ->
              Some (Failed (Model_discovery.Credential_error
                "credential resolution failed"), None)
          | Unix.Unix_error _ | Sys_error _ ->
              Some (Failed (Model_discovery.Transport_error
                "request failed or timed out"), None)
          | _ ->
              Some (Failed (Model_discovery.Credential_error
                "credential resolution failed"), None) in
        Option.iter (fun (status, account_id) ->
          publish t index status account_id) outcome;
        work t ()

let start ?(max_workers = 4) ?(timeout_seconds = 20.) requests =
  if max_workers < 1 || max_workers > 16 then
    invalid_arg "discovery worker limit must be between 1 and 16";
  if timeout_seconds <= 0. then
    invalid_arg "discovery request deadline must be positive";
  let seen = Hashtbl.create (List.length requests) in
  List.iter (fun request ->
    let scope = request.scope in
    let key = scope.provider, scope.account_id, scope.route in
    if Hashtbl.mem seen key then invalid_arg "duplicate discovery request scope";
    Hashtbl.add seen key ()) requests;
  let read_fd, write_fd = Unix.pipe () in
  Unix.set_close_on_exec read_fd;
  Unix.set_close_on_exec write_fd;
  Unix.set_nonblock read_fd;
  let t = {
    read_fd; write_fd; cancelled = Atomic.make false; lock = Mutex.create ();
    pending = List.mapi (fun index request -> index, request) requests;
    outcomes = [];
    snapshots = Array.of_list (List.map (fun request ->
      { scope = request.scope; status = Loading }) requests);
    workers = []; closed = false; timeout_seconds;
  } in
  t.workers <- List.init (min max_workers (List.length requests))
    (fun _ -> Thread.create (work t) ());
  t
let read_fd t =
  if t.closed then invalid_arg "discovery coordinator is closed";
  t.read_fd

let poll t =
  if t.closed then invalid_arg "discovery coordinator is closed";
  let buffer = Bytes.create 256 in
  let rec drain () =
    try
      if Unix.read t.read_fd buffer 0 (Bytes.length buffer) > 0 then drain ()
    with
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> () in
  drain ();
  Mutex.lock t.lock;
  let ready = t.outcomes in
  t.outcomes <- [];
  Mutex.unlock t.lock;
  List.iter (fun (index, status, account_id) ->
    let snapshot = t.snapshots.(index) in
    let scope = match account_id with
      | None -> snapshot.scope
      | Some account_id -> { snapshot.scope with account_id = Some account_id } in
    t.snapshots.(index) <- { scope; status }) (List.rev ready);
  Array.to_list t.snapshots

let complete t =
  Array.for_all (fun snapshot -> snapshot.status <> Loading) t.snapshots

let cancel t = Atomic.set t.cancelled true

let close t =
  if not t.closed then (
    cancel t;
    List.iter Thread.join t.workers;
    Unix.close t.write_fd;
    Unix.close t.read_fd;
    t.closed <- true)
