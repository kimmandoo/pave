(* Pinned binary Connect requests need a curl executor independent of Provider's
   JSON transport; otherwise Devin_api and Provider form a compile-time cycle. *)
exception Failed of string
exception Cancelled

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


let read_limited path max_bytes =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
    let length = in_channel_length input in
    if length > max_bytes then raise (Failed "response exceeds size limit");
    really_input_string input length)

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

let run ?cancel configuration =
  let executable, environment = match !Test.curl_helper with
    | Some executable -> executable, Unix.environment ()
    | None ->
        if not (Sys.file_exists curl_path &&
            (try Unix.access curl_path [Unix.X_OK]; true
             with Unix.Unix_error _ -> false)) then
          raise (Failed "trusted curl executable unavailable");
        curl_path, curl_environment in
  let reader, writer = Unix.pipe () in
  let output_read, output_write = Unix.pipe () in
  let errors = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
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
      let rec send position =
        (match cancel with Some check when check () -> raise Cancelled | _ -> ());
        if position < String.length configuration then (
          let count = try Unix.write_substring writer configuration position
            (String.length configuration - position)
            with Unix.Unix_error (Unix.EPIPE, _, _) ->
              raise (Failed "curl closed its configuration input") in
          if count = 0 then raise (Failed "curl did not accept its configuration");
          send (position + count)) in
      send 0;
      close_fd writer;
      let result = Bytes.create 4 in
      let rec read_status position =
        (match cancel with Some check when check () -> raise Cancelled | _ -> ());
        let ready, _, _ = Unix.select [output_read] [] [] 0.1 in
        if ready = [] then read_status position
        else
          let count = Unix.read output_read result position (4 - position) in
          if count = 0 then position
          else if position + count = 4 then
            raise (Failed "curl returned an invalid HTTP status")
          else read_status (position + count) in
      let count = read_status 0 in
      close_fd output_read;
      let rec await () =
        (match cancel with Some check when check () -> raise Cancelled | _ -> ());
        match Unix.waitpid [Unix.WNOHANG] pid with
        | 0, _ -> ignore (Unix.select [] [] [] 0.1); await ()
        | _, status -> waited := true; status in
      let status = await () in
      (match status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED code -> raise (Failed (curl_failure code))
      | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
          raise (Failed (Printf.sprintf "curl terminated (signal %d)" signal)));
      if count <> 3 then raise (Failed "curl returned an invalid HTTP status");
      let hundreds = Bytes.get result 0
      and tens = Bytes.get result 1
      and ones = Bytes.get result 2 in
      if hundreds < '1' || hundreds > '5' ||
         tens < '0' || tens > '9' || ones < '0' || ones > '9' then
        raise (Failed "curl returned an invalid HTTP status");
      (Char.code hundreds - 48) * 100 +
      (Char.code tens - 48) * 10 + Char.code ones - 48)

let post ?cancel ~url ~headers ~body ~max_bytes () =
  with_temp_file (fun body_path body_output ->
    output_string body_output body;
    close_out body_output;
    with_temp_file (fun response_path response_output ->
      close_out response_output;
      let option key value = key ^ " = " ^ quote value ^ "\n" in
      let configuration = "silent\n" ^ option "url" url ^
        option "request" "POST" ^ option "data-binary" ("@" ^ body_path) ^
        option "output" response_path ^ option "write-out" "%{http_code}" ^
        option "connect-timeout" "10" ^ option "max-time" "120" ^
        option "max-filesize" (string_of_int max_bytes) ^
        option "proto" "=https" ^ option "proxy" "" ^
        option "max-redirs" "0" ^
        String.concat "" (List.map (fun (key, value) ->
          option "header" (key ^ ": " ^ value)) headers) in
      let status = run ?cancel configuration in
      status, read_limited response_path max_bytes))
