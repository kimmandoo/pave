let member = Pave.Protocol.member

let stream_body =
  "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Hello \"},\"finish_reason\":null}]}\n\n" ^
  "data: {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call-1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\"}}]},\"finish_reason\":null}]}\n\n" ^
  "data: {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"\\\"App.swift\\\"}\"}}]},\"finish_reason\":null}]}\n\n" ^
  "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}]}\n\n" ^
  "data: [DONE]\n\n"

let anthropic_stream =
  "event: message_start\ndata: " ^
  {|{"type":"message_start","message":{"id":"msg_1","role":"assistant","usage":{"input_tokens":2}}}|} ^
  "\n\n" ^
  "event: content_block_start\ndata: " ^
  {|{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}|} ^
  "\n\n" ^
  "event: content_block_delta\ndata: " ^
  {|{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hello from Claude"}}|} ^
  "\n\n" ^
  "event: content_block_stop\ndata: " ^
  {|{"type":"content_block_stop","index":0}|} ^
  "\n\n" ^
  "event: message_delta\ndata: " ^
  {|{"type":"message_delta","delta":{"stop_reason":"end_turn"}}|} ^
  "\n\n" ^
  "event: message_stop\ndata: " ^
  {|{"type":"message_stop"}|} ^ "\n\n"

let read_request ic =
  let headers = ref [] and length = ref 0 in
  let rec consume () =
    let line = input_line ic in
    if line <> "\r" && line <> "" then (
      let lower = String.lowercase_ascii line in
      headers := lower :: !headers;
      if String.starts_with ~prefix:"content-length:" lower then
        length := int_of_string (String.trim (String.sub line 15 (String.length line - 15)));
      consume ()) in
  consume ();
  let body = Yojson.Basic.from_string (really_input_string ic !length) in
  !headers, body

let has_header prefix headers = List.exists (String.starts_with ~prefix) headers

let serve client step =
  let ic = Unix.in_channel_of_descr client in
  let oc = Unix.out_channel_of_descr client in
  let headers, request = read_request ic in
  let status, content_type, body = match step with
    | 0 ->
        assert (has_header "authorization: bearer mock-openai" headers);
        assert (member "stream" request = `Bool true);
        200, "text/event-stream", stream_body
    | 1 ->
        assert (has_header "x-api-key: mock-anthropic" headers);
        assert (has_header "anthropic-version: 2023-06-01" headers);
        assert (member "system" request = `String "mobile system");
        assert (member "messages" request = `List [ `Assoc [
          "role", `String "user"; "content", `String "inspect" ] ]);
        200, "application/json",
        {|{"type":"message","role":"assistant","content":[{"type":"text","text":"Inspected."}],"stop_reason":"end_turn"}|}
    | 2 | 3 ->
        assert (has_header "authorization: bearer mock-openai" headers);
        429, "application/json",
        {|{"error":{"message":"rate limited: mock-openai"}}|}
    | 4 ->
        assert (has_header "x-api-key: mock-anthropic" headers);
        assert (member "stream" request = `Bool true);
        200, "text/event-stream", anthropic_stream
    | _ ->
        assert (has_header "authorization: bearer mock-openai" headers);
        200, "text/event-stream", stream_body in
  if step = 5 then (
    Printf.fprintf oc "HTTP/1.1 %d Mock\r\nContent-Type: %s\r\nConnection: keep-alive\r\n\r\n%s"
      status content_type body;
    flush oc;
    ignore (Unix.select [] [] [] 3.))
  else (
    Printf.fprintf oc "HTTP/1.1 %d Mock\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
      status content_type (String.length body) body;
    flush oc);
  close_in_noerr ic; close_out_noerr oc

let () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 6;
  let port = match Unix.getsockname socket with Unix.ADDR_INET (_, p) -> p | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    (try for step = 0 to 5 do
       let client, _ = Unix.accept socket in serve client step
     done with exn -> prerr_endline (Printexc.to_string exn); exit 2);
    exit 0);
  Unix.close socket;
  Fun.protect ~finally:(fun () ->
    (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
    ignore (Unix.waitpid [] child)) (fun () ->
    let endpoint = Printf.sprintf "http://127.0.0.1:%d/complete" port in
    let openai : Pave.Provider.config = { endpoint; api_key = "mock-openai";
      model = "mock"; api = Pave.Provider.Openai_completions } in
    let anthropic : Pave.Provider.config = { endpoint; api_key = "mock-anthropic";
      model = "mock-claude"; api = Pave.Provider.Anthropic_messages } in
    let system : Pave.Protocol.message = { role = "system"; content = Some "mobile system";
      tool_calls = []; tool_call_id = None } in
    let user = Pave.Protocol.user "inspect" in
    let credential_read = ref false in
    let foreign = { anthropic with endpoint = "https://attacker.example/v1/messages" } in
    (match Pave.Provider.complete ~authentication:Pave.Provider.OAuth
      ~resolve_key:(fun () -> credential_read := true; "sensitive") foreign
      [ system; user ] [] with
     | exception Pave.Provider.Provider_error _ -> assert (not !credential_read)
     | _ -> failwith "OAuth credential accepted by a foreign HTTPS endpoint");
    let deltas = ref [] in
    let streamed = Pave.Provider.complete ~on_text:(fun delta -> deltas := delta :: !deltas)
      openai [ system; user ] [] in
    assert (!deltas = [ "Hello " ]);
    assert (streamed.content = Some "Hello ");
    assert (streamed.tool_calls = [ { Pave.Protocol.id = "call-1"; name = "read_file";
      arguments = `Assoc [ "path", `String "App.swift" ] } ]);
    let reply = Pave.Provider.complete anthropic [ system; user ] [] in
    assert (reply.content = Some "Inspected.");
    (match Pave.Provider.complete openai [ system; user ] [] with
     | exception Pave.Provider.Provider_error message ->
         assert (message = "HTTP 429: rate limited: [redacted]")
     | _ -> failwith "expected HTTP error");
    let emitted = ref false in
    (match Pave.Provider.complete ~on_text:(fun _ -> emitted := true)
      openai [ system; user ] [] with
     | exception Pave.Provider.Provider_error message ->
         assert (message = "HTTP 429: rate limited: [redacted]")
     | _ -> failwith "expected streaming HTTP error");
    assert (not !emitted);
    let anthro_deltas = ref [] in
    let response = Pave.Provider.complete ~on_text:(fun part -> anthro_deltas := part :: !anthro_deltas)
      anthropic [ system; user ] [] in
    assert (List.rev !anthro_deltas = [ "Hello from Claude" ]);
    assert (response.content = Some "Hello from Claude");
    let started = Unix.gettimeofday () in
    let answer = Pave.Provider.complete ~on_text:(fun _ -> ())
      openai [ system; user ] [] in
    assert (answer.content = Some "Hello ");
    assert (Unix.gettimeofday () -. started < 2.5));
  print_endline "provider HTTP: ok"
