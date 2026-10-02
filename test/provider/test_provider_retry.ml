(* Streaming transport failures must keep their own message once retries are
   exhausted or do not apply; they must never degrade to a generic header
   error or be swallowed into an apparently successful stream. *)
let with_failing_curl status f =
  let curl = Filename.temp_file "fake-curl-" ".sh" in
  let oc = open_out curl in
  Printf.fprintf oc "#!/bin/sh\ncat >/dev/null\nexit %d\n" status;
  close_out oc;
  Unix.chmod curl 0o700;
  Pave.Provider.Test.use_curl_helper curl;
  Fun.protect ~finally:(fun () -> try Sys.remove curl with Sys_error _ -> ()) f

let contains text needle =
  let n = String.length needle in
  let rec scan index =
    index + n <= String.length text &&
    (String.sub text index n = needle || scan (index + 1)) in
  scan 0

let stream_error status =
  with_failing_curl status (fun () ->
    match Pave.Provider.post_stream
        ~endpoint:"https://api.example.test/v1/chat/completions"
        ~headers:[] ~secret:"fixture-secret" (`Assoc ["model", `String "m"])
        ~on_chunk:(fun _ -> ()) ~is_done:(fun () -> false)
        ~is_finished:(fun () -> false) with
    | () -> "completed"
    | exception Pave.Provider.Provider_error message -> message)

let () =
  (* Transient status: retried to the limit, then reported as itself. *)
  let transient = stream_error 7 in
  assert (contains transient "exit status 7");
  (* Non-retryable status: reported immediately, not turned into success. *)
  let permanent = stream_error 23 in
  assert (contains permanent "exit status 23");
  print_endline "provider_retry: ok"
