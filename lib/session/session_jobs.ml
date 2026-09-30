type status = Running | Completed | Failed | Cancelled | Interrupted

type job = {
  id : string;
  owner : string;
  label : string;
  kind : string;
  status : status;
  summary : string;
  artifact : (string * string) option;
  created_at : float;
  delivered : bool;
}

type runtime = {
  mutable job : job;
  cancelled : bool Atomic.t;
}

type t = {
  session : Session.t;
  owner : string;
  directory : string;
  mutex : Mutex.t;
  changed : Condition.t;
  jobs : (string, runtime) Hashtbl.t;
  on_notice : string -> unit;
  closed : bool Atomic.t;
}

exception Error of string

let max_active_jobs = 4
let max_output_bytes = 1_048_576
let max_summary_bytes = 4096
let summary_truncation_marker = " [truncated]"

let bounded_summary text =
  if String.length text <= max_summary_bytes && Session_store.valid_utf8 text then text
  else
    let limit = max_summary_bytes - String.length summary_truncation_marker in
    let output = Buffer.create (min limit (String.length text)) in
    let exception Full in
    let truncated =
      try
        ignore (Uutf.String.fold_utf_8 (fun () _ decoded ->
          let character = match decoded with
            | `Uchar character -> character
            | `Malformed _ -> Uchar.rep in
          let width = Uchar.utf_8_byte_length character in
          if width > limit - Buffer.length output then raise Full;
          Uutf.Buffer.add_utf_8 output character) () text);
        false
      with Full -> true in
    if truncated then Buffer.add_string output summary_truncation_marker;
    Buffer.contents output
let valid_id id = String.length id = 32 &&
  String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false) id

let with_lock t action =
  Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) action

let status_text = function
  | Running -> "running" | Completed -> "completed" | Failed -> "failed"
  | Cancelled -> "cancelled" | Interrupted -> "interrupted"

let status_of_session = function
  | Session.Completed -> Completed | Session.Failed -> Failed
  | Session.Cancelled -> Cancelled | Session.Interrupted -> Interrupted

let session_status = function
  | Completed -> Session.Completed | Failed -> Session.Failed
  | Cancelled -> Session.Cancelled | Interrupted -> Session.Interrupted
  | Running -> invalid_arg "running jobs cannot be delivered"

let status_json status = `String (status_text status)
let artifact_json = function
  | None -> `Null
  | Some (owner, id) -> `Assoc ["owner", `String owner; "id", `String id]

let json_of_job job = `Assoc [
  "version", `Int 1; "id", `String job.id; "owner", `String job.owner;
  "label", `String job.label; "kind", `String job.kind;
  "status", status_json job.status; "summary", `String job.summary;
  "artifact", artifact_json job.artifact; "createdAt", `Float job.created_at;
  "delivered", `Bool job.delivered]

let metadata_path directory id = Filename.concat directory (id ^ ".json")

let persist t job =
  Session_store.write_atomic ~dir:t.directory ~prefix:".pave-job-"
    (metadata_path t.directory job.id)
    (Yojson.Basic.to_string (json_of_job job) ^ "\n")

let parse_job_status = function
  | `String "running" -> Running | `String "completed" -> Completed
  | `String "failed" -> Failed | `String "cancelled" -> Cancelled
  | `String "interrupted" -> Interrupted
  | _ -> raise (Error "invalid persisted job status")

let parse_metadata ~owner ~id text =
  let json = Yojson.Basic.from_string text in
  let fields = Yojson.Basic.Util.to_assoc json in
  let field name = try List.assoc name fields
    with Not_found -> raise (Error "missing persisted job field") in
  let string name = match field name with
    | `String value -> value
    | _ -> raise (Error "invalid persisted job field") in
  let stored_id = string "id" and stored_owner = string "owner" in
  let label = string "label" and kind = string "kind"
  and summary = string "summary" in
  let status = parse_job_status (field "status") in
  let created_at = match field "createdAt" with
    | `Float value -> value | `Int value -> float value
    | _ -> raise (Error "invalid persisted job timestamp") in
  let delivered = match field "delivered" with
    | `Bool value -> value | _ -> raise (Error "invalid delivery marker") in
  let artifact = match field "artifact" with
    | `Null -> None
    | value ->
        let fields = Yojson.Basic.Util.to_assoc value in
        (match List.assoc_opt "owner" fields, List.assoc_opt "id" fields with
         | Some (`String artifact_owner), Some (`String artifact_id) ->
             Some (artifact_owner, artifact_id)
         | _ -> raise (Error "invalid persisted job artifact")) in
  if field "version" <> `Int 1 || stored_id <> id || stored_owner <> owner ||
     not (valid_id id) || not (valid_id owner) || label = "" ||
     String.length label > 256 || kind = "" || String.length kind > 128 ||
     String.length summary > max_summary_bytes ||
     classify_float created_at = FP_nan || classify_float created_at = FP_infinite ||
     (match artifact with Some (artifact_owner, artifact_id) ->
        not (valid_id artifact_owner && valid_id artifact_id) | None -> false) then
    raise (Error "invalid persisted job metadata");
  { id; owner; label; kind; status; summary; artifact; created_at; delivered }

let read_metadata t id =
  match Session_store.read_private_file (metadata_path t.directory id) 65_536 with
  | None -> None
  | Some text -> (try Some (parse_metadata ~owner:t.owner ~id text)
    with _ -> None)
let delivery_job started delivery previous =
  let status = status_of_session delivery.Session.status in
  let created_at = Option.fold ~none:(Unix.gettimeofday ())
    ~some:(fun job -> job.created_at) previous in
  { id = started.Session.job_id; owner = started.owner;
    label = started.label; kind = started.job_kind; status;
    summary = delivery.summary; artifact = delivery.artifact;
    created_at; delivered = true }

let interrupted_job started previous =
  let created_at = match previous with
    | Some job -> job.created_at | None -> Unix.gettimeofday () in
  let artifact = Option.bind previous (fun job -> job.artifact) in
  { id = started.Session.job_id; owner = started.owner;
    label = started.label; kind = started.job_kind; status = Interrupted;
    summary = (match previous with
      | Some { status = Running; _ } ->
          "The process stopped before this job finished. Provider usage may have been charged."
      | Some job when job.summary <> "" -> job.summary
      | _ -> "Job metadata was unavailable when the session resumed; " ^
             "the result cannot be recovered and provider usage may have been charged.");
    artifact; created_at; delivered = false }

let create ~root ~session ?(on_notice = fun _ -> ()) () =
  let owner = Session.session_id session in
  let workspace = Session_store.ensure ~root in
  let parent = Filename.concat workspace "jobs" in
  Session_store.ensure_directory parent;
  let directory = Filename.concat parent owner in
  Session_store.ensure_directory directory;
  let t = { session; owner; directory; mutex = Mutex.create ();
    changed = Condition.create (); jobs = Hashtbl.create 16; on_notice;
    closed = Atomic.make false } in
  List.iter (fun (state : Session.job_state) ->
    let started = state.started in
    if started.owner = owner && valid_id started.job_id then (
      let previous = read_metadata t started.job_id in
      let job = match state.delivery, previous with
        | Some delivery, previous -> delivery_job started delivery previous
        | None, Some ({ status = Running; _ } as previous) ->
            interrupted_job started (Some previous)
        | None, Some previous ->
            { previous with owner; label = started.label; kind = started.job_kind;
              delivered = false }
        | None, None -> interrupted_job started None in
      Hashtbl.replace t.jobs job.id { job; cancelled = Atomic.make false };
      persist t job)) (Session.job_states session);
  t

let snapshot runtime = runtime.job

let jobs t =
  let all = with_lock t (fun () ->
    Hashtbl.fold (fun _ runtime all -> snapshot runtime :: all) t.jobs []) in
  List.sort (fun a b ->
    let order = compare a.created_at b.created_at in
    if order = 0 then String.compare a.id b.id else order) all

let find t ~id =
  if not (valid_id id) then None
  else with_lock t (fun () -> Option.map snapshot (Hashtbl.find_opt t.jobs id))

let update t runtime job =
  runtime.job <- job;
  persist t job;
  Condition.broadcast t.changed

let deliver_pending t =
  let pending = with_lock t (fun () ->
    Hashtbl.fold (fun _ runtime pending ->
      let job = runtime.job in
      if job.status <> Running && not job.delivered then job :: pending
      else pending) t.jobs []) in
  List.filter_map (fun (job : job) ->
    let delivery : Session.job_delivery = {
      job_id = job.id; owner = job.owner; label = job.label;
      status = session_status job.status; summary = job.summary;
      artifact = job.artifact } in
    try
      ignore (Session.append_job_delivery t.session delivery);
      with_lock t (fun () -> match Hashtbl.find_opt t.jobs job.id with
        | None -> ()
        | Some runtime when not runtime.job.delivered ->
            update t runtime { runtime.job with delivered = true }
        | Some _ -> ());
      None
    with exn -> Some (job.id ^ ": " ^ Printexc.to_string exn)) pending

let finish t runtime status summary artifact =
  let completed = with_lock t (fun () ->
    if Atomic.get t.closed then None
    else
      let status, summary = if Atomic.get runtime.cancelled then
        Cancelled, "Cancelled. Provider usage may already have been charged." 
      else status, summary in
      let job = { runtime.job with status; summary = bounded_summary summary; artifact } in
      update t runtime job;
      Some job) in
  match completed with
  | None -> ()
  | Some job ->
      t.on_notice (Printf.sprintf "Background job %s (%s) %s. Use /jobs for its result."
        job.label job.id (status_text job.status))

let execute t runtime task =
  let cancel () = Atomic.get t.closed || Atomic.get runtime.cancelled in
  let outcome =
    try
      let output = task ~cancel in
      if cancel () then Cancelled, "", None
      else if String.length output > max_output_bytes then
        Failed, "Job result exceeded the 1 MiB workflow artifact limit.", None
      else
        let artifact = Session.store_artifact t.session
          ~name:(runtime.job.label ^ ".md") ~mime_type:"text/markdown; charset=utf-8" output in
        Completed, "Result saved as artifact " ^ artifact.id ^ ".",
        Some (artifact.owner, artifact.id)
    with
    | Provider.Cancelled -> Cancelled, "Cancelled. Provider usage may already have been charged.", None
    | exn -> Failed, Printexc.to_string exn, None in
  let status, summary, artifact = outcome in
  finish t runtime status summary artifact

let start t ~kind ~label ~task =
  if String.trim label = "" || String.length label > 256 ||
     String.exists (fun c -> Char.code c < 32 || Char.code c = 127) label then
    raise (Error "job label must be a nonempty single line of at most 256 bytes");
  if String.trim kind = "" || String.length kind > 128 then
    raise (Error "job kind must be a nonempty value of at most 128 bytes");
  let id = Session.fresh_id () in
  let job = { id; owner = t.owner; label; kind; status = Running;
    summary = "Running"; artifact = None; created_at = Unix.gettimeofday ();
    delivered = false } in
  let runtime = { job; cancelled = Atomic.make false } in
  with_lock t (fun () ->
    if Atomic.get t.closed then raise (Error "job manager is closed");
    let active = Hashtbl.fold (fun _ item count ->
      if item.job.status = Running then count + 1 else count) t.jobs 0 in
    if active >= max_active_jobs then raise (Error "session job limit reached");
    persist t job;
    (match Session.append_job_started t.session ~job_id:id ~label ~job_kind:kind with
     | Some _ -> ()
     | None ->
         (try Unix.unlink (metadata_path t.directory id)
          with Unix.Unix_error _ -> ());
         raise (Error "job ID is already present in the session journal"));
    Hashtbl.add t.jobs id runtime);
  try
    ignore (Thread.create (fun () -> execute t runtime task) ());
    id
  with exn ->
    finish t runtime Failed ("Could not start the job worker: " ^ Printexc.to_string exn) None;
    id

let cancel t ~id =
  match find t ~id with
  | None -> false
  | Some job when job.status <> Running -> false
  | Some _ -> with_lock t (fun () -> match Hashtbl.find_opt t.jobs id with
      | Some runtime when runtime.job.status = Running ->
          Atomic.set runtime.cancelled true;
          update t runtime { runtime.job with
            summary = "Cancellation requested. Provider usage may already have been charged." };
          true
      | _ -> false)

let wait t ~id =
  with_lock t (fun () -> Option.map snapshot (Hashtbl.find_opt t.jobs id))

let await t ~id =
  Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) (fun () ->
    let rec loop () = match Hashtbl.find_opt t.jobs id with
      | None -> None
      | Some runtime when runtime.job.status = Running ->
          Condition.wait t.changed t.mutex;
          loop ()
      | Some runtime -> Some (snapshot runtime) in
    loop ())

let close t =
  with_lock t (fun () ->
    if not (Atomic.get t.closed) then (
      Atomic.set t.closed true;
      Hashtbl.iter (fun _ runtime ->
        if runtime.job.status = Running then (
          Atomic.set runtime.cancelled true;
          update t runtime { runtime.job with status = Interrupted;
            summary = "Pave exited before this job stopped. Provider usage may " ^
              "already have been charged." })) t.jobs));
  let delivery_errors = deliver_pending t in
  if delivery_errors <> [] then
    raise (Error ("could not deliver completed jobs: " ^
      String.concat "; " delivery_errors))
