(* Pinned binary Connect requests need a curl executor independent of Provider's
   JSON transport; otherwise Devin_api and Provider form a compile-time cycle. *)
exception Failed of string
exception Cancelled

type timeouts = {
  first_byte_seconds : float;
  idle_seconds : float;
  total_seconds : float;
}

let unary_timeouts =
  { first_byte_seconds = 120.; idle_seconds = 120.; total_seconds = 120. }

(* Completion is a Connect stream, not a two-minute unary operation. Match
   the shared provider transport's upload/prefill, inactivity and total bounds. *)
let completion_timeouts =
  { first_byte_seconds = 600.; idle_seconds = 120.; total_seconds = 3600. }

let with_temp_file f =
  let path, output = Filename.open_temp_file ~mode:[Open_binary] "pave-devin-" ".bin" in
  Fun.protect ~finally:(fun () ->
    close_out_noerr output;
    try Sys.remove path with Sys_error _ -> ())
    (fun () -> f path output)

let quote value =
  if String.exists (fun c -> Char.code c < 32 || Char.code c = 127) value then
    raise (Failed "invalid curl configuration value");
  let buffer = Buffer.create (String.length value + 2) in
  Buffer.add_char buffer '"';
  String.iter (function
    | '"' -> Buffer.add_string buffer "\\\""
    | '\\' -> Buffer.add_string buffer "\\\\"
    | c -> Buffer.add_char buffer c) value;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()
let curl_path = "/usr/bin/curl"
let curl_environment = [|"LANG=C"; "LC_ALL=C"|]

(* Explicit fixture injection for tests; production requests use curl_path. *)

module Test = struct
  let curl_helper = ref None

  let use_curl_helper executable =
    curl_helper := Some (Unix.realpath executable)
end



let curl_failure = function
  | 6 -> "could not resolve pinned host"
  | 7 -> "could not connect to pinned host"
  | 28 -> "request timed out (remote acceptance unknown)"
  | 35 -> "TLS handshake failed"
  | 52 -> "pinned host sent an empty reply"
  | 55 -> "failed to send request"
  | 56 -> "connection closed while receiving response"
  | 60 -> "TLS certificate verification failed"
  | 63 -> "response exceeds size limit"
  | code -> Printf.sprintf "curl failed (exit status %d)" code

let run ?cancel ~timeouts ~max_bytes configuration =
  let check_cancel () = match cancel with
    | Some check when check () -> raise Cancelled
    | _ -> () in
  check_cancel ();
  let valid seconds = Float.is_finite seconds && seconds > 0. in
  if not (valid timeouts.first_byte_seconds && valid timeouts.idle_seconds &&
          valid timeouts.total_seconds) then
    invalid_arg "invalid Devin transport timeout";
  if max_bytes < 0 then invalid_arg "invalid Devin response limit";
  let started_at = Unix.gettimeofday () and last_received_at = ref None in
  let wait_seconds ~ready_first () =
    check_cancel ();
    let now = Unix.gettimeofday () in
    let remaining = started_at +. timeouts.total_seconds -. now in
    if remaining <= 0. then
      raise (Failed (Printf.sprintf
        "request timed out at total deadline (%g s; remote acceptance unknown)"
        timeouts.total_seconds));
    let phase_remaining, reason = match !last_received_at with
      | None ->
          started_at +. timeouts.first_byte_seconds -. now,
          "before first response data (upload/prefill deadline; remote acceptance unknown)"
      | Some received ->
          received +. timeouts.idle_seconds -. now,
          "after response data (stream stalled; remote acceptance unknown)" in
    if phase_remaining <= 0. && not ready_first then
      raise (Failed ("request timed out " ^ reason));
    min 0.1 (max 0. (min remaining phase_remaining)) in
  let executable, environment = match !Test.curl_helper with
    | Some executable -> executable, Unix.environment ()
    | None ->
        if not (Sys.file_exists curl_path &&
            (try Unix.access curl_path [Unix.X_OK]; true
             with Unix.Unix_error _ -> false)) then
          raise (Failed "trusted curl executable unavailable");
        curl_path, curl_environment in
  let reader, writer = Unix.pipe () in
  let output_read, output_write = try Unix.pipe () with exn ->
    List.iter close_fd [reader; writer]; raise exn in
  let errors = try Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 with exn ->
    List.iter close_fd [reader; writer; output_read; output_write]; raise exn in
  let pid = try
    Unix.set_close_on_exec writer;
    Unix.set_close_on_exec output_read;
    Unix.create_process_env executable
      [|"curl"; "--disable"; "--config"; "-"|] environment
      reader output_write errors
  with exn ->
    List.iter close_fd [reader; writer; output_read; output_write; errors];
    raise exn in
  List.iter close_fd [reader; output_write; errors];
  let waited = ref false in
  Fun.protect ~finally:(fun () ->
    close_fd writer; close_fd output_read;
    if not !waited then (
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())))
    (fun () ->
      Unix.set_nonblock writer;
      let rec send position =
        check_cancel ();
        if position < String.length configuration then (
          let _, ready, _ =
            try Unix.select [] [writer] [] (wait_seconds ~ready_first:false ())
            with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], [] in
          if ready = [] then send position
          else
            let count = try Unix.write_substring writer configuration position
              (String.length configuration - position)
              with
              | Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> -1
              | Unix.Unix_error (Unix.EPIPE, _, _) ->
                  raise (Failed "curl closed its configuration input") in
            if count = 0 then raise (Failed "curl did not accept its configuration");
            send (position + max 0 count)) in
      send 0;
      close_fd writer;
      let received = Buffer.create (min max_bytes 8192) in
      (* curl appends exactly three status bytes. Retain only that suffix,
         including across one-byte reads; binary response bytes stay opaque. *)
      let chunk = Bytes.create 8195 and held = ref 0 in
      let rec receive () =
        let ready, _, _ =
          try Unix.select [output_read] [] [] (wait_seconds ~ready_first:true ())
          with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], [] in
        if ready = [] then (
          ignore (wait_seconds ~ready_first:false ());
          receive ())
        else (
          check_cancel ();
          let count = try Unix.read output_read chunk !held 8192
            with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
          if count < 0 then receive ()
          else if count > 0 then (
            last_received_at := Some (Unix.gettimeofday ());
            let available = !held + count in
            let body_bytes = max 0 (available - 3) in
            if body_bytes > max_bytes - Buffer.length received then
              raise (Failed "response exceeds size limit");
            Buffer.add_subbytes received chunk 0 body_bytes;
            held := available - body_bytes;
            Bytes.blit chunk body_bytes chunk 0 !held;
            receive ())) in
      receive ();
      close_fd output_read;
      let rec await () =
        check_cancel ();
        match Unix.waitpid [Unix.WNOHANG] pid with
        | 0, _ ->
            (try ignore (Unix.select [] [] [] (wait_seconds ~ready_first:false ()))
             with Unix.Unix_error (Unix.EINTR, _, _) -> ());
            await ()
        | _, status -> waited := true; status
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> await () in
      let status = await () in
      check_cancel ();
      (match status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED code -> raise (Failed (curl_failure code))
      | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
          raise (Failed (Printf.sprintf "curl terminated (signal %d)" signal)));
      if !held <> 3 then raise (Failed "curl returned an invalid HTTP status");
      let hundreds = Bytes.get chunk 0
      and tens = Bytes.get chunk 1
      and ones = Bytes.get chunk 2 in
      if hundreds < '1' || hundreds > '5' ||
         tens < '0' || tens > '9' || ones < '0' || ones > '9' then
        raise (Failed "curl returned an invalid HTTP status");
      let status = (Char.code hundreds - 48) * 100 +
        (Char.code tens - 48) * 10 + Char.code ones - 48 in
      status, Buffer.contents received)

let post ?cancel ?(timeouts=unary_timeouts) ~url ~headers ~body ~max_bytes () =
  with_temp_file (fun body_path body_output ->
    output_string body_output body;
    close_out body_output;
    let option key value = key ^ " = " ^ quote value ^ "\n" in
    let configuration = "silent\nno-buffer\n" ^ option "url" url ^
      option "request" "POST" ^ option "data-binary" ("@" ^ body_path) ^
      option "output" "/dev/stdout" ^ option "write-out" "%{http_code}" ^
      option "connect-timeout" "10" ^
      option "max-time" (Printf.sprintf "%.17g" timeouts.total_seconds) ^
      option "max-filesize" (string_of_int max_bytes) ^
      option "proto" "=https" ^ option "proxy" "" ^
      option "max-redirs" "0" ^
      String.concat "" (List.map (fun (key, value) ->
        option "header" (key ^ ": " ^ value)) headers) in
    run ?cancel ~timeouts ~max_bytes configuration)
