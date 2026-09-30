let child path name = Filename.concat path name

let rec remove path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter (fun name -> remove (child path name)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let wait_until predicate =
  let deadline = Unix.gettimeofday () +. 3. in
  while not (predicate ()) && Unix.gettimeofday () < deadline do
    Thread.delay 0.001
  done;
  assert (predicate ())

let delivery_count session job_id =
  List.fold_left (fun count (entry : Pave.Session.entry) ->
    match entry.kind with
    | Pave.Session.Job_delivery delivery when delivery.job_id = job_id -> count + 1
    | _ -> count) 0 (Pave.Session.entries session)

exception Diagnostic of string

let () =
  Printexc.register_printer (function Diagnostic text -> Some text | _ -> None);
  let previous_home = Sys.getenv_opt "HOME"
  and previous_state = Sys.getenv_opt "XDG_STATE_HOME" in
  let base = Filename.temp_file "pave-session-jobs-" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  Fun.protect ~finally:(fun () ->
    (match previous_home with Some value -> Unix.putenv "HOME" value
     | None -> Unix.putenv "HOME" "");
    (match previous_state with Some value -> Unix.putenv "XDG_STATE_HOME" value
     | None -> Unix.putenv "XDG_STATE_HOME" "");
    remove base) (fun () ->
    let root = child base "workspace" and state_home = child base "state" in
    Unix.mkdir root 0o700;
    Unix.mkdir state_home 0o700;
    Unix.putenv "HOME" base;
    Unix.putenv "XDG_STATE_HOME" state_home;
    let session = Pave.Session_store.create ~root in
    let manager = Pave.Session_jobs.create ~root ~session () in
    let complete_id = Pave.Session_jobs.start manager ~kind:"delegate"
      ~label:"review" ~task:(fun ~cancel ->
        assert (not (cancel ())); "review findings with evidence") in
    let completed = Option.get (Pave.Session_jobs.await manager ~id:complete_id) in
    assert (completed.status = Pave.Session_jobs.Completed);
    let owner, artifact_id = Option.get completed.artifact in
    assert (Pave.Session.read_artifact session ~owner ~id:artifact_id =
      "review findings with evidence");
    assert (Pave.Session_jobs.deliver_pending manager = []);
    assert (delivery_count session complete_id = 1);
    assert (Pave.Session_jobs.deliver_pending manager = []);
    assert (delivery_count session complete_id = 1);

    let entered = Atomic.make false and stopped = Atomic.make false in
    let cancel_id = Pave.Session_jobs.start manager ~kind:"delegate"
      ~label:"cancel fixture" ~task:(fun ~cancel ->
        Atomic.set entered true;
        while not (cancel ()) do Thread.delay 0.001 done;
        Atomic.set stopped true;
        raise Pave.Provider.Cancelled) in
    wait_until (fun () -> Atomic.get entered);
    assert (Pave.Session_jobs.cancel manager ~id:cancel_id);
    let cancelled = Option.get (Pave.Session_jobs.await manager ~id:cancel_id) in
    assert (cancelled.status = Pave.Session_jobs.Cancelled);
    assert (String.starts_with ~prefix:"Cancelled." cancelled.summary);
    wait_until (fun () -> Atomic.get stopped);
    assert (Pave.Session_jobs.deliver_pending manager = []);
    assert (delivery_count session cancel_id = 1);

    let reopened = Pave.Session_store.open_existing ~root session.path in
    let reopened_manager = Pave.Session_jobs.create ~root ~session:reopened () in
    let recovered = Pave.Session_jobs.jobs reopened_manager in
    assert (List.length recovered = 2);
    assert (List.exists (fun (job : Pave.Session_jobs.job) ->
      job.id = complete_id && job.status = Pave.Session_jobs.Completed && job.delivered)
      recovered);
    assert (List.exists (fun (job : Pave.Session_jobs.job) ->
      job.id = cancel_id && job.status = Pave.Session_jobs.Cancelled && job.delivered)
      recovered);
    assert (Pave.Session_jobs.deliver_pending reopened_manager = []);
    assert (delivery_count reopened complete_id = 1);
    assert (delivery_count reopened cancel_id = 1);

    let failure_origin = Pave.Session_store.create ~root in
    let failure_origin_manager = Pave.Session_jobs.create ~root ~session:failure_origin () in
    let diagnostic = String.concat "" (List.init 2000 (fun _ -> "진단")) in
    let failed_id = Pave.Session_jobs.start failure_origin_manager ~kind:"delegate"
      ~label:"oversized failure" ~task:(fun ~cancel:_ -> raise (Diagnostic diagnostic)) in
    let failed = Option.get (Pave.Session_jobs.await failure_origin_manager ~id:failed_id) in
    assert (failed.status = Pave.Session_jobs.Failed);
    assert (String.length failed.summary <= 4096);
    assert (Pave.Session_store.valid_utf8 failed.summary);
    assert (String.ends_with ~suffix:" [truncated]" failed.summary);
    let failed_session = Pave.Session_store.open_existing ~root failure_origin.path in
    let failed_manager = Pave.Session_jobs.create ~root ~session:failed_session () in
    let recovered_failure = Option.get (Pave.Session_jobs.find failed_manager ~id:failed_id) in
    assert (recovered_failure.status = Pave.Session_jobs.Failed);
    assert (recovered_failure.summary = failed.summary);
    assert (Pave.Session_jobs.deliver_pending failed_manager = []);
    assert (Pave.Session_jobs.deliver_pending failed_manager = []);
    assert (delivery_count failed_session failed_id = 1);
    let delivered_session = Pave.Session_store.open_existing ~root failure_origin.path in
    let delivered_manager = Pave.Session_jobs.create ~root ~session:delivered_session () in
    assert (Pave.Session_jobs.deliver_pending delivered_manager = []);
    assert (delivery_count delivered_session failed_id = 1);
    Pave.Session_jobs.close failed_manager;
    Pave.Session_jobs.close delivered_manager;

    let fork = Pave.Session_store.fork ~root reopened in
    let fork_manager = Pave.Session_jobs.create ~root ~session:fork () in
    assert (Pave.Session_jobs.jobs fork_manager = []);
    assert (not (Pave.Session_jobs.cancel fork_manager ~id:complete_id));

    Pave.Session_jobs.close manager;
    Pave.Session_jobs.close reopened_manager;
    Pave.Session_jobs.close fork_manager;
    print_endline "session-owned async jobs: ok")
