let limit = 16_777_216

let read_request input =
  ignore (input_line input);
  let length = ref 0 in
  let rec headers () =
    let line = input_line input in
    if line <> "\r" && line <> "" then (
      if String.starts_with ~prefix:"content-length:" (String.lowercase_ascii line) then
        length := int_of_string (String.trim (String.sub line 15 (String.length line - 15)));
      headers ()) in
  headers ();
  ignore (really_input_string input !length)

let run ~chunked ~status ~bytes ~valid_json =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 1;
  let port = match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let report_read, report_write = Unix.pipe () in
  let child = Unix.fork () in
  if child = 0 then (
    Unix.close report_read;
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    let client, _ = Unix.accept socket in
    Unix.close socket;
    let input = Unix.in_channel_of_descr client in
    let output = Unix.out_channel_of_descr client in
    let sent = ref 0 in
    (try
      read_request input;
      Printf.fprintf output "HTTP/1.1 %d Fixture\r\nConnection: close\r\n%s\r\n"
        status (if chunked then "Transfer-Encoding: chunked\r\n"
          else Printf.sprintf "Content-Length: %d\r\n" bytes);
      flush output;
      let emit text =
        if chunked then Printf.fprintf output "%x\r\n" (String.length text);
        output_string output text;
        if chunked then output_string output "\r\n";
        flush output;
        sent := !sent + String.length text in
      if valid_json then emit "{\"padding\":\"";
      let suffix = if valid_json then 2 else 0 in
      let chunk = String.make 8192 'x' in
      while !sent < bytes - suffix do
        emit (if bytes - suffix - !sent >= String.length chunk then chunk
          else String.sub chunk 0 (bytes - suffix - !sent))
      done;
      if valid_json then emit "\"}";
      if chunked then (output_string output "0\r\n\r\n"; flush output)
    with Sys_error _ | Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) -> ());
    close_out_noerr output;
    close_in_noerr input;
    let report = Unix.out_channel_of_descr report_write in
    output_string report (string_of_int !sent); close_out report;
    exit 0);
  Unix.close socket;
  Unix.close report_write;
  Fun.protect ~finally:(fun () ->
    (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
    (try ignore (Unix.waitpid [] child) with Unix.Unix_error _ -> ());
    Unix.close report_read) (fun () ->
    let outcome = try
      let json = Pave.Provider.post_json
        ~endpoint:(Printf.sprintf "http://127.0.0.1:%d/complete" port)
        ~headers:[] ~secret:"" (`Assoc []) in
      Ok json
    with Pave.Provider.Provider_error reason -> Error reason in
    let report = Unix.in_channel_of_descr report_read in
    let sent = int_of_string (input_line report) in
    let _, status = Unix.waitpid [] child in
    assert (status = Unix.WEXITED 0);
    outcome, sent)

let () =
  List.iter (fun chunked ->
    List.iter (fun status ->
      let outcome, sent = run ~chunked ~status ~bytes:(2 * limit) ~valid_json:false in
      (match outcome with
       | Error "completion response exceeds 16 MiB" -> ()
       | _ -> failwith "oversized completion body was not rejected");
      (* A bounded receiver disconnects while the server still has data. *)
      assert (sent < 2 * limit)) [200; 503];
    let outcome, _ = run ~chunked ~status:200 ~bytes:limit ~valid_json:true in
    (match outcome with
     | Ok json ->
         (match Pave.Protocol.member "padding" json with
          | `String text -> assert (String.length text = limit - 14)
          | _ -> assert false)
     | Error reason -> failwith reason);
    let outcome, _ = run ~chunked ~status:200 ~bytes:(limit + 1) ~valid_json:true in
    (match outcome with
     | Error "completion response exceeds 16 MiB" -> ()
     | _ -> failwith "one-byte oversized response was accepted")) [false; true];
  print_endline "buffered completion response limits: ok"
