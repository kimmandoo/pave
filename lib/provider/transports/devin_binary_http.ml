(* Pinned binary Connect requests need a curl executor independent of Provider's
   JSON transport; otherwise Devin_api and Provider form a compile-time cycle. *)
exception Failed
exception Cancelled

let with_temp_file f =
  let path, output = Filename.open_temp_file ~mode:[Open_binary] "pave-devin-" ".bin" in
  Fun.protect ~finally:(fun () ->
    close_out_noerr output;
    try Sys.remove path with Sys_error _ -> ())
    (fun () -> f path output)

let quote value =
  if String.exists (fun c -> Char.code c < 32 || Char.code c = 127) value then
    raise Failed;
  let buffer = Buffer.create (String.length value + 2) in
  Buffer.add_char buffer '"';
  String.iter (function
    | '"' -> Buffer.add_string buffer "\\\""
    | '\\' -> Buffer.add_string buffer "\\\\"
    | c -> Buffer.add_char buffer c) value;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

let read_limited path max_bytes =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
    let length = in_channel_length input in
    if length > max_bytes then raise Failed;
    really_input_string input length)

let run ?cancel configuration =
  let reader, writer = Unix.pipe () in
  let output_read, output_write = Unix.pipe () in
  let errors = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
  let pid = try
    Unix.set_close_on_exec writer;
    Unix.set_close_on_exec output_read;
    Unix.create_process "curl" [|"curl"; "--disable"; "--config"; "-"|]
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
        if position < String.length configuration then (
          let count = Unix.write_substring writer configuration position
            (String.length configuration - position) in
          if count = 0 then raise Failed;
          send (position + count)) in
      send 0;
      close_fd writer;
      let result = Bytes.create 16 in
      let rec read_status () =
        (match cancel with Some check when check () -> raise Cancelled | _ -> ());
        let ready, _, _ = Unix.select [output_read] [] [] 0.1 in
        if ready = [] then read_status ()
        else Unix.read output_read result 0 (Bytes.length result) in
      let count = read_status () in
      close_fd output_read;
      let rec await () =
        (match cancel with Some check when check () -> raise Cancelled | _ -> ());
        match Unix.waitpid [Unix.WNOHANG] pid with
        | 0, _ -> ignore (Unix.select [] [] [] 0.1); await ()
        | _, status -> waited := true; status in
      (match await () with Unix.WEXITED 0 -> () | _ -> raise Failed);
      if count <> 3 then raise Failed;
      try int_of_string (Bytes.sub_string result 0 count)
      with Failure _ -> raise Failed)

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
