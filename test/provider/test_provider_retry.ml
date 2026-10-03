(* Completion failures retain their original cause and never automatically
   replay a request whose remote acceptance is unknown. *)
let with_failing_curl ?(stdout = "") status f =
  let curl = Filename.temp_file "fake-curl-" ".sh" in
  let oc = open_out curl in
  let attempts = Filename.temp_file "curl-attempts-" ".txt" in
  Printf.fprintf oc
    "#!/bin/sh\nprintf 'attempt\\n' >> %s\ncat >/dev/null\nprintf '%%s' %s\nexit %d\n"
    (Filename.quote attempts) (Filename.quote stdout) status;
  close_out oc;
  Unix.chmod curl 0o700;
  Pave.Provider.Test.use_curl_helper curl;
  Fun.protect ~finally:(fun () ->
    List.iter (fun path -> try Sys.remove path with Sys_error _ -> ()) [curl; attempts])
    (fun () ->
      let result = f () in
      let input = open_in attempts in
      let count = Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
        let rec lines n = match input_line input with
          | _ -> lines (n + 1) | exception End_of_file -> n in
        lines 0) in
      assert (count = 1);
      result)

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

let buffered_error status =
  with_failing_curl ~stdout:"{\"incomplete\":200" status (fun () ->
    match Pave.Provider.post_json
        ~endpoint:"https://api.example.test/v1/chat/completions"
        ~headers:[] ~secret:"fixture-secret" (`Assoc ["model", `String "m"]) with
    | _ -> "completed"
    | exception Pave.Provider.Provider_error message -> message)

let () =
  (* A connection failure is still one request, with its original error. *)
  let transient = stream_error 7 in
  assert (contains transient "exit status 7");
  (* Other curl failures also remain failures, not missing-header errors. *)
  let permanent = stream_error 23 in
  assert (contains permanent "exit status 23");
  (* Even a connection lost after response bytes does not replay completion. *)
  assert (contains (buffered_error 56) "exit status 56");
  print_endline "provider_retry: ok"
