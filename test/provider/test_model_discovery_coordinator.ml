module Coordinator = Pave.Model_discovery_coordinator
module Discovery = Pave.Model_discovery

let listing ?account_id provider route model_id =
  let identity = Pave.Model_identity.make ~provider ?account_id ~route
    ~upstream_id:model_id () in
  let source : Pave.Model_catalog.provenance = {
    id_source = Pave.Model_catalog.Provider_listing;
    capability_source = None; endpoint = Some "https://pinned.invalid/models";
    retrieved_at = Some (Unix.gettimeofday ());
  } in
  let model : Pave.Model_catalog.model = {
    identity; display_name = None;
    capabilities = Pave.Model_catalog.empty_capabilities;
    provenance = source;
  } in
  { Discovery.models = [model]; source }

let wait_until predicate timeout =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    if predicate () then true
    else if Unix.gettimeofday () >= deadline then false
    else (Thread.delay 0.001; loop ()) in
  loop ()

let await coordinator =
  let deadline = Unix.gettimeofday () +. 2. in
  let rec loop () =
    ignore (Coordinator.poll coordinator);
    if Coordinator.complete coordinator then Coordinator.poll coordinator
    else if Unix.gettimeofday () >= deadline then
      failwith "discovery coordinator did not settle"
    else (
      ignore (Unix.select [Coordinator.read_fd coordinator] [] [] 0.05);
      loop ()) in
  loop ()

let contains text needle =
  let rec loop index =
    index + String.length needle <= String.length text &&
    (String.sub text index (String.length needle) = needle ||
      loop (index + 1)) in
  loop 0

let () =
  let active = ref 0 and maximum = ref 0 and lock = Mutex.create () in
  let enter () =
    Mutex.lock lock;
    incr active;
    maximum := max !maximum !active;
    Mutex.unlock lock in
  let leave () =
    Mutex.lock lock;
    decr active;
    Mutex.unlock lock in
  let requests = List.init 8 (fun index ->
    let provider = Printf.sprintf "provider-%d" index in
    let scope : Coordinator.scope = {
      provider; account_id = None; route = "chat" } in
    { Coordinator.scope; run = (fun _cancel ->
        enter ();
        Thread.delay 0.01;
        leave ();
        let account_id =
          if index = 0 then Some "account-0"
          else if index = 3 then Some "account-3" else None in
        let result = match index with
          | 1 -> Error (Discovery.Unsupported_route (provider, "bad"))
          | 2 -> Error Discovery.Missing_credential
          | 3 -> Error (Discovery.Invalid_response "invalid account listing")
          | _ -> Ok (listing ?account_id provider "chat" "exact/model/id") in
        result, account_id) }) in
  let coordinator = Coordinator.start ~max_workers:2 requests in
  let snapshots = Fun.protect
    ~finally:(fun () -> Coordinator.close coordinator)
    (fun () -> await coordinator) in
  assert (!maximum = 2);
  (match List.hd snapshots with
   | { Coordinator.scope = { account_id = Some "account-0"; _ };
       status = Coordinator.Ready listing } ->
       assert ((List.hd listing.models).identity.account_id =
         Some "account-0")
   | _ -> failwith "ready snapshot lost its resolved account scope");
  (match List.nth snapshots 3 with
   | { Coordinator.scope = { account_id = Some "account-3"; _ };
       status = Coordinator.Failed
         (Discovery.Invalid_response "invalid account listing") } -> ()
   | _ -> failwith "failed snapshot lost its resolved account scope");
  assert (List.map (fun snapshot -> match snapshot.Coordinator.status with
    | Coordinator.Ready listing ->
        assert (Discovery.model_ids listing = ["exact/model/id"]);
        "ready"
    | Coordinator.Unsupported (Discovery.Unsupported_route _) -> "unsupported"
    | Coordinator.Failed Discovery.Missing_credential -> "failed"
    | Coordinator.Failed (Discovery.Invalid_response _) -> "failed"
    | Coordinator.Loading | Coordinator.Unsupported _ | Coordinator.Failed _ ->
        "loading") snapshots =
    ["ready"; "unsupported"; "failed"; "failed"; "ready"; "ready";
     "ready"; "ready"]);

  let slow_scope = { Coordinator.provider = "slow"; account_id = None;
    route = "chat" } in
  let timed = Coordinator.start ~timeout_seconds:0.02 [
    { Coordinator.scope = slow_scope; run = (fun cancelled ->
        while not (cancelled ()) do Thread.delay 0.001 done;
        Error (Discovery.Transport_error "cancelled"), None) }
  ] in
  let timed_snapshots = Fun.protect
    ~finally:(fun () -> Coordinator.close timed)
    (fun () -> await timed) in
  (match (List.hd timed_snapshots).Coordinator.status with
   | Coordinator.Failed (Discovery.Transport_error "request timed out") -> ()
   | _ -> failwith "request deadline was not reflected in its snapshot");

  let private_error = "credential-secret-value" in
  let failure_scope = { slow_scope with provider = "credential-failure" } in
  let failure = Coordinator.start [
    { Coordinator.scope = failure_scope;
      run = (fun _ -> failwith private_error) }
  ] in
  let failure_snapshot = Fun.protect
    ~finally:(fun () -> Coordinator.close failure)
    (fun () -> List.hd (await failure)) in
  (match failure_snapshot.Coordinator.status with
   | Coordinator.Failed (Discovery.Credential_error detail)
       when not (contains (Discovery.message (Discovery.Credential_error detail))
         private_error) -> ()
   | _ -> failwith "credential failure exposed private data");

  let started = Atomic.make false and observed_cancel = Atomic.make false in
  let cancelled = Coordinator.start [
    { Coordinator.scope = { slow_scope with provider = "cancel" };
      run = (fun cancel ->
        Atomic.set started true;
        while not (cancel ()) do Thread.delay 0.001 done;
        Atomic.set observed_cancel true;
        Ok (listing "cancel" "chat" "must-not-publish"), None) }
  ] in
  assert (wait_until (fun () -> Atomic.get started) 1.);
  Coordinator.cancel cancelled;
  assert (wait_until (fun () -> Atomic.get observed_cancel) 1.);
  let after_cancel = Coordinator.poll cancelled in
  assert (not (Coordinator.complete cancelled));
  assert ((List.hd after_cancel).Coordinator.status = Coordinator.Loading);
  Coordinator.close cancelled;
  print_endline "bounded discovery snapshots, deadlines, and cancellation: ok"
