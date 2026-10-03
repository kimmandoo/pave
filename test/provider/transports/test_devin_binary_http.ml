module Http = Pave.Devin_binary_http

let fail message = failwith ("Devin binary HTTP: " ^ message)

let write_file path value =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel value)

let binary_body = "\000\255\128Connect\000\001\254"

(* This executable is the owned curl subprocess. It emits real pipe bytes, and
   has its own watchdog so even a broken receiver cannot strand the fixture. *)
let fixture () =
  ignore (Unix.alarm 8);
  let directory = Sys.getenv "PAVE_DEVIN_BINARY_FIXTURE_DIR" in
  let path name = Filename.concat directory name in
  write_file (path "pid.tmp") (string_of_int (Unix.getpid ()));
  Unix.rename (path "pid.tmp") (path "pid");
  (try while true do ignore (input_line stdin) done with End_of_file -> ());
  let emit value = output_string stdout value; flush stdout in
  let complete value = write_file (path "completed") ""; emit value in
  let started () = emit "body"; write_file (path "started") "" in
  match Sys.getenv "PAVE_DEVIN_BINARY_FIXTURE_MODE" with
  | "ready" -> complete "ready200"
  | "delayed" -> Unix.sleepf 0.7; complete "delayed200"
  | "active" ->
      for _ = 1 to 12 do emit "chunk"; Unix.sleepf 0.15 done;
      complete "200"
  | "first-stall" -> Unix.sleepf 3.; complete "late200"
  | "idle-stall" | "cancel" ->
      started (); Unix.sleepf 3.; complete "late200"
  | "total" ->
      for _ = 1 to 30 do emit "chunk"; Unix.sleepf 0.1 done;
      complete "200"
  | "oversize" ->
      for _ = 1 to 8 do emit "abcdefgh"; Unix.sleepf 0.03 done;
      Unix.sleepf 3.; complete "200"
  | "binary" ->
      String.iter (fun byte -> emit (String.make 1 byte); Unix.sleepf 0.01)
        binary_body;
      emit "4"; Unix.sleepf 0.05;
      emit "2"; Unix.sleepf 0.05;
      complete "9"
  | "short" -> complete "20"
  | "malformed" -> complete "body2x0"
  | "invalid-range" -> complete "body000"
  | "nonzero" -> complete "body200"; exit 56
  | mode -> fail ("unknown fixture mode " ^ mode)

let () =
  if Array.length Sys.argv >= 3 && Sys.argv.(1) = "--disable" &&
     Sys.argv.(2) = "--config" then (
    fixture ();
    exit 0)

let read_pid directory =
  let path = Filename.concat directory "pid" in
  if not (Sys.file_exists path) then None
  else
    let channel = open_in path in
    Fun.protect ~finally:(fun () -> close_in_noerr channel)
      (fun () -> Some (int_of_string (input_line channel)))

let with_fixture mode f =
  let directory = Filename.temp_file "pave-devin-binary-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let directory_env = "PAVE_DEVIN_BINARY_FIXTURE_DIR"
  and mode_env = "PAVE_DEVIN_BINARY_FIXTURE_MODE" in
  let previous_directory = Sys.getenv_opt directory_env
  and previous_mode = Sys.getenv_opt mode_env in
  Unix.putenv directory_env directory;
  Unix.putenv mode_env mode;
  Fun.protect ~finally:(fun () ->
    (* Assertion failures must also kill and reap a receiver's forgotten child.
       Only signal a PID while waitpid proves it is still our live child. *)
    (match read_pid directory with
     | None -> ()
     | Some pid ->
         (try
            if fst (Unix.waitpid [Unix.WNOHANG] pid) = 0 then (
              (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
              ignore (Unix.waitpid [] pid))
          with Unix.Unix_error (Unix.ECHILD, _, _) -> ()));
    Array.iter (fun name -> Sys.remove (Filename.concat directory name))
      (Sys.readdir directory);
    Unix.rmdir directory;
    Unix.putenv directory_env (Option.value ~default:"" previous_directory);
    Unix.putenv mode_env (Option.value ~default:"" previous_mode))
    (fun () -> f directory)

let assert_reaped directory =
  match read_pid directory with
  | None -> fail "fixture did not start"
  | Some pid ->
      (match Unix.waitpid [Unix.WNOHANG] pid with
       | exception Unix.Unix_error (Unix.ECHILD, _, _) -> ()
       | 0, _ -> fail "subprocess still running after post returned"
       | _ -> fail "subprocess was left as a zombie")

let assert_interrupted directory =
  if Sys.file_exists (Filename.concat directory "completed") then
    fail "receiver waited for fixture completion instead of interrupting receipt";
  assert_reaped directory

let timeouts first idle total : Http.timeouts =
  { first_byte_seconds = first; idle_seconds = idle; total_seconds = total }

let post ?cancel ?(max_bytes=4096) limits () =
  Http.post ?cancel ~timeouts:limits ~url:"https://fixture.invalid/connect"
    ~headers:[] ~body:"\000request\255" ~max_bytes ()

let expect_failure call =
  match call () with
  | exception Http.Failed _ -> ()
  | _ -> fail "accepted an incomplete, oversized or failed response"

let expect_cancelled call =
  match call () with
  | exception Http.Cancelled -> ()
  | _ -> fail "cancellation was not propagated"

let () =
  Http.Test.use_curl_helper Sys.executable_name;
  with_fixture "ready" (fun directory ->
    let unrelated = ref [] in
    Fun.protect ~finally:(fun () ->
      List.iter (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ()) !unrelated)
      (fun () ->
        let completed = Filename.concat directory "completed" in
        let cancel () =
          if !unrelated = [] && Sys.file_exists completed then
            for _ = 1 to 8 do
              unrelated := Unix.openfile completed [Unix.O_RDONLY] 0 :: !unrelated
            done;
          false in
        let response = post ~cancel (timeouts 2. 2. 5.) () in
        if response <> (200, "ready") then fail "descriptor ownership corrupted response";
        List.iter (fun fd ->
          if (Unix.fstat fd).Unix.st_kind <> Unix.S_REG then
            fail "request cleanup changed an unrelated descriptor") !unrelated;
        assert_reaped directory));
  with_fixture "delayed" (fun directory ->
    let response = post (timeouts 2. 0.3 4.) () in
    if response <> (200, "delayed") then fail "delayed first body was corrupted";
    assert_reaped directory);
  with_fixture "ready" (fun directory ->
    let cancel () =
      (* Simulate caller scheduling delay after bytes/EOF are already waiting;
         a readable stream must not become an inactivity failure. *)
      if Sys.file_exists (Filename.concat directory "completed") then Unix.sleepf 0.4;
      false in
    let response = post ~cancel (timeouts 2. 0.2 6.) () in
    if response <> (200, "ready") then fail "already-readable completion was lost at the idle boundary";
    assert_reaped directory);
  with_fixture "active" (fun directory ->
    let response = post (timeouts 0.8 0.6 4.) () in
    if response <> (200, String.concat "" (List.init 12 (fun _ -> "chunk"))) then
      fail "active response did not survive the first-byte window";
    assert_reaped directory);
  List.iter (fun (mode, limits) ->
    with_fixture mode (fun directory ->
      expect_failure (post limits);
      assert_interrupted directory))
    [ "first-stall", timeouts 0.4 0.8 5.;
      "idle-stall", timeouts 2. 0.4 5.;
      "total", timeouts 2. 0.6 1.2 ];
  with_fixture "cancel" (fun directory ->
    let cancel () =
      (match Unix.waitpid [Unix.WNOHANG] (-1) with
       | exception Unix.Unix_error (Unix.ECHILD, _, _) -> ()
       | _ -> fail "cancellation was first checked after spawning a child");
      true
    in
    expect_cancelled (post ~cancel (timeouts 2. 2. 5.));
    if Sys.file_exists (Filename.concat directory "pid") then
      fail "pre-cancelled request spawned its helper";
    (match Unix.waitpid [Unix.WNOHANG] (-1) with
     | exception Unix.Unix_error (Unix.ECHILD, _, _) -> ()
     | _ -> fail "pre-cancelled request left a child"));
  with_fixture "cancel" (fun directory ->
    let observed_response = ref false in
    let cancel () =
      let started = Sys.file_exists (Filename.concat directory "started") in
      if started then observed_response := true;
      started
    in
    expect_cancelled (post ~cancel (timeouts 2. 2. 5.));
    if not !observed_response then fail "cancellation never observed response bytes";
    assert_interrupted directory);
  with_fixture "oversize" (fun directory ->
    expect_failure (post ~max_bytes:16 (timeouts 2. 4. 6.));
    assert_interrupted directory);
  with_fixture "binary" (fun directory ->
    let response = post ~max_bytes:(String.length binary_body) (timeouts 2. 1. 4.) () in
    if response <> (429, binary_body) then
      fail "binary body or byte-split trailing HTTP status was corrupted";
    assert_reaped directory);
  List.iter (fun mode ->
    with_fixture mode (fun directory ->
      expect_failure (post (timeouts 2. 1. 4.));
      assert_reaped directory)) ["short"; "malformed"; "invalid-range"];
  with_fixture "nonzero" (fun directory ->
    expect_failure (post (timeouts 2. 1. 4.));
    assert_reaped directory);
  print_endline "Devin binary HTTP phase deadlines, cancellation, bounded binary receipt: ok"
