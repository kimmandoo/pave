let member = Pave.Protocol.member
let leaks_key text =
  let key = "mock-openai" in
  let rec seek offset =
    match String.index_from_opt text offset key.[0] with
    | None -> false
    | Some index ->
        (index + String.length key <= String.length text &&
         String.sub text index (String.length key) = key) ||
        seek (index + 1) in
  seek 0

let stream_body =
  "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Hello \"},\"finish_reason\":null}]}\n\n" ^
  "data: {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call-1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\"}}]},\"finish_reason\":null}]}\n\n" ^
  "data: {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"\\\"App.swift\\\"}\"}}]},\"finish_reason\":null}]}\n\n" ^
  "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}]}\n\n" ^
  "data: [DONE]\n\n"

let anthropic_stream =
  "event: message_start\ndata: " ^
  {|{"type":"message_start","message":{"id":"msg_1","type":"message","role":"assistant","usage":{"input_tokens":2}}}|} ^
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
  let request_line = input_line ic in
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
  let path = match String.split_on_char ' ' request_line with
    | _method :: path :: _ -> path
    | _ -> failwith "malformed HTTP request" in
  path, !headers, body

let has_header prefix headers = List.exists (String.starts_with ~prefix) headers

let write_file path contents =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () ->
    output_string oc contents)

let read_capture path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    really_input_string ic (in_channel_length ic))

let curl_timeout_smoke () =
  let directory = Filename.temp_file "pave-provider-timeout-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let curl = Filename.concat directory "curl" in
  let captured_config = Filename.concat directory "config" in
  let calls = Filename.concat directory "calls" in
  write_file curl {|#!/bin/sh
set -eu
[ "$1" = "--disable" ] && [ "$2" = "--config" ] && [ "$3" = "-" ]
printf '%s\n' "$PAVE_PROVIDER_HTTP_TIMEOUT_MODE" >> "$PAVE_PROVIDER_HTTP_TIMEOUT_CALLS"
cat > "$PAVE_PROVIDER_HTTP_TIMEOUT_CONFIG"
header=$(sed -n 's/^dump-header = "\(.*\)"/\1/p' "$PAVE_PROVIDER_HTTP_TIMEOUT_CONFIG" | tail -n 1)
if [ -n "$header" ]; then printf 'HTTP/1.1 200 OK\r\n\r\n' > "$header"; fi
if [ "$PAVE_PROVIDER_HTTP_TIMEOUT_MODE" = body ]; then
  printf 'data: {"choices":[{"index":0,"delta":{"content":"partial"},"finish_reason":null}]}\n\n'
fi
exit 28
|};
  Unix.chmod curl 0o700;
  let old_path = Sys.getenv_opt "PATH"
  and old_mode = Sys.getenv_opt "PAVE_PROVIDER_HTTP_TIMEOUT_MODE"
  and old_config = Sys.getenv_opt "PAVE_PROVIDER_HTTP_TIMEOUT_CONFIG"
  and old_calls = Sys.getenv_opt "PAVE_PROVIDER_HTTP_TIMEOUT_CALLS" in
  let restore name = function
    | Some value -> Unix.putenv name value
    | None -> Unix.putenv name "" in
  Fun.protect
    ~finally:(fun () ->
      restore "PATH" old_path;
      restore "PAVE_PROVIDER_HTTP_TIMEOUT_MODE" old_mode;
      restore "PAVE_PROVIDER_HTTP_TIMEOUT_CONFIG" old_config;
      restore "PAVE_PROVIDER_HTTP_TIMEOUT_CALLS" old_calls;
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path)
        [curl; captured_config; calls];
      Unix.rmdir directory)
    (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ Option.value ~default:"" old_path);
      Unix.putenv "PAVE_PROVIDER_HTTP_TIMEOUT_CONFIG" captured_config;
      Unix.putenv "PAVE_PROVIDER_HTTP_TIMEOUT_CALLS" calls;
      let config : Pave.Provider.config = {
        endpoint = "http://127.0.0.1:1/complete"; api_key = "mock-openai";
        model = "timeout-fixture"; api = Pave.Provider.Openai_completions } in
      let expect prefix f =
        match f () with
        | exception Pave.Provider.Provider_error message ->
            assert (String.starts_with ~prefix message);
            assert (not (leaks_key message))
        | _ -> failwith ("expected timeout classification: " ^ prefix) in
      Unix.putenv "PAVE_PROVIDER_HTTP_TIMEOUT_MODE" "buffered";
      expect "provider request timed out" (fun () ->
        Pave.Provider.complete config [Pave.Protocol.user "timeout"] []);
      Unix.putenv "PAVE_PROVIDER_HTTP_TIMEOUT_MODE" "empty";
      expect "provider stream timed out before the first response data byte" (fun () ->
        Pave.Provider.complete ~on_text:(fun _ -> ()) config
          [Pave.Protocol.user "timeout"] []);
      Unix.putenv "PAVE_PROVIDER_HTTP_TIMEOUT_MODE" "body";
      let visible = ref [] in
      expect "provider stream timed out after response data" (fun () ->
        Pave.Provider.complete ~on_text:(fun text -> visible := text :: !visible)
          config [Pave.Protocol.user "timeout"] []);
      assert (List.rev !visible = ["partial"]);
      let requests =
        read_capture calls |> String.split_on_char '\n'
        |> List.filter (fun line -> line <> "") in
      assert (List.length requests = 3))

let serve client step signal_write closed_write =
  let ic = Unix.in_channel_of_descr client in
  let oc = Unix.out_channel_of_descr client in
  let path, headers, request = read_request ic in
  let status, content_type, body = match step with
    | 0 ->
        assert (has_header "authorization: bearer mock-openai" headers);
        assert (member "messages" request = `List [`Assoc [
          "role", `String "user"; "content", `List [
            `Assoc ["type", `String "text"; "text", `String "inspect"];
            `Assoc ["type", `String "image_url";
              "image_url", `Assoc ["url", `String
                "data:image/png;base64,aGVsbG8="]]]]]);
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
    | 5 ->
        assert (has_header "authorization: bearer mock-openai" headers);
        200, "text/event-stream", stream_body
    | 6 | 7 ->
        assert (has_header "authorization: bearer mock-openai" headers);
        assert (member "stream" request =
          (if step = 7 then `Bool true else `Null));
        200, (if step = 7 then "text/event-stream" else "application/json"),
        (if step = 7 then
           "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"partial\"},\"finish_reason\":null}]}\n\n"
         else {|{"choices":[{"message":{"role":"assistant","content":"partial|})
    | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19
    | 20 | 21 | 22 | 23 ->
        let provider, key = match (step - 8) / 2 with
          | 0 -> "together", "mock-together"
          | 1 -> "cerebras", "mock-cerebras"
          | 2 -> "venice", "mock-venice"
          | 3 -> "deepinfra", "mock-deepinfra"
          | 4 -> "fireworks", "mock-fireworks"
          | 5 -> "baseten", "mock-baseten"
          | 6 -> "huggingface", "mock-huggingface"
          | _ -> "nanogpt", "mock-nanogpt" in
        assert (path = "/chat/completions");
        assert (has_header ("authorization: bearer " ^ key) headers);
        assert (member "model" request = `String "discovered-next-model");
        let tool_id = "call-" ^ provider in
        let user = Pave.Protocol.message_to_json (Pave.Protocol.user "look up detail") in
        if step mod 2 = 0 then (
          assert (member "messages" request = `List [user]);
          assert (member "tools" request =
            `List [`Assoc ["type", `String "function";
              "function", `Assoc ["name", `String "lookup";
                "parameters", `Assoc ["type", `String "object"]]]]);
          let call = Pave.Protocol.call_to_json { Pave.Protocol.id = tool_id;
            name = "lookup"; arguments = `Assoc ["query", `String "detail"] } in
          200, "application/json",
          Yojson.Basic.to_string (`Assoc ["choices", `List [`Assoc [
            "finish_reason", `String "tool_calls";
            "message", `Assoc ["role", `String "assistant";
              "content", `Null; "tool_calls", `List [call]]]]]))
        else (
          (match member "messages" request with
          | `List [first; assistant; result] ->
              assert (first = user);
              assert (member "role" assistant = `String "assistant");
              assert (member "tool_calls" assistant =
                `List [Pave.Protocol.call_to_json { Pave.Protocol.id = tool_id;
                  name = "lookup";
                  arguments = `Assoc ["query", `String "detail"] }]);
              assert (member "role" result = `String "tool");
              assert (member "tool_call_id" result = `String tool_id);
              assert (member "content" result = `String "found")
          | _ -> failwith "second inference omitted the tool-result turn");
          200, "application/json",
          Yojson.Basic.to_string (`Assoc ["choices", `List [`Assoc [
            "finish_reason", `String "stop";
            "message", `Assoc ["role", `String "assistant";
              "content", `String (provider ^ "-reply")]]]]))
    | 24 -> assert (has_header "authorization: bearer mock-openai" headers);
        401, "application/json",
        {|{"error":{"message":"invalid key mock-openai"}}|}
    | 25 -> assert (has_header "authorization: bearer mock-openai" headers);
        403, "application/json", {|{"error":{"message":"permission denied"}}|}
    | 26 -> assert (has_header "authorization: bearer mock-openai" headers);
        404, "application/json", {|{"error":{"message":"model not found"}}|}
    | 27 -> assert (has_header "authorization: bearer mock-openai" headers);
        413, "application/json", {|{"error":{"message":"request too large"}}|}
    | 28 -> assert (has_header "authorization: bearer mock-openai" headers);
        429, "application/json", {|{"error":{"message":"rate limited"}}|}
    | 29 -> assert (has_header "authorization: bearer mock-openai" headers);
        503, "application/json", {|{"error":{"message":"overloaded"}}|}
    | 30 -> assert (has_header "authorization: bearer mock-openai" headers);
        400, "application/json",
        {|{"error":{"code":"context_length_exceeded","message":"too many tokens"}}|}
    | 31 -> assert (has_header "authorization: bearer mock-openai" headers);
        400, "application/json",
        {|{"error":{"type":"invalid_request_error","message":"bad request"}}|}
    | _ -> assert false
  in
  if step = 5 then (
    Printf.fprintf oc "HTTP/1.1 %d Mock\r\nContent-Type: %s\r\nConnection: keep-alive\r\n\r\n%s"
      status content_type body;
    flush oc;
    ignore (Unix.select [] [] [] 3.))
  else if step = 6 || step = 7 then (
    Printf.fprintf oc "HTTP/1.1 %d OK\r\nContent-Type: %s\r\nContent-Length: 4096\r\nConnection: keep-alive\r\n\r\n%s"
      status content_type body;
    flush oc;
    if step = 6 then ignore (Unix.write_substring signal_write "r" 0 1);
    let readable, _, _ = Unix.select [client] [] [] 2. in
    assert (readable <> []);
    let byte = Bytes.create 1 in
    assert (Unix.read client byte 0 1 = 0);
    ignore (Unix.write_substring closed_write "c" 0 1))
  else (
    Printf.fprintf oc "HTTP/1.1 %d Mock\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
      status content_type (String.length body) body;
    flush oc);
  close_in_noerr ic; close_out_noerr oc

let () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 8;
  let port = match Unix.getsockname socket with Unix.ADDR_INET (_, p) -> p | _ -> assert false in
  let signal_read, signal_write = Unix.pipe () in
  let closed_read, closed_write = Unix.pipe () in
  let child = Unix.fork () in
  if child = 0 then (
    Unix.close signal_read;
    Unix.close closed_read;
    (try for step = 0 to 31 do
       let client, _ = Unix.accept socket in serve client step signal_write closed_write
     done with exn -> prerr_endline (Printexc.to_string exn); exit 2);
    exit 0);
  Unix.close signal_write;
  Unix.close closed_write;
  Unix.close socket;
  Fun.protect ~finally:(fun () ->
    (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
    ignore (Unix.waitpid [] child);
    Unix.close signal_read;
    Unix.close closed_read) (fun () ->
    let endpoint = Printf.sprintf "http://127.0.0.1:%d/complete" port in
    let openai : Pave.Provider.config = { endpoint; api_key = "mock-openai";
      model = "mock"; api = Pave.Provider.Openai_completions } in
    let anthropic : Pave.Provider.config = { endpoint; api_key = "mock-anthropic";
      model = "mock-claude"; api = Pave.Provider.Anthropic_messages } in
    let system : Pave.Protocol.message = { role = "system"; content = Some "mobile system";
      tool_calls = []; tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] } in
    let user = Pave.Protocol.user "inspect" in
    let credential_read = ref false in
    List.iter (fun endpoint ->
      let exposed = { openai with endpoint } in
      (match Pave.Provider.complete exposed [user] [] with
       | exception Pave.Provider.Provider_error reason ->
           assert (String.starts_with ~prefix:"completion endpoint must use HTTPS" reason)
       | _ -> failwith "remote HTTP completion exposed a bearer token");
      (match Pave.Provider.complete ~on_text:(fun _ -> ())
        exposed [user] [] with
       | exception Pave.Provider.Provider_error reason ->
           assert (String.starts_with ~prefix:"completion endpoint must use HTTPS" reason)
       | _ -> failwith "streamed remote HTTP completion exposed a bearer token")) [
      "http://api.moonshot.ai/v1/chat/completions";
      "http://127.0.0.1:443@attacker.example/v1/chat/completions";
      "http://127.0.0.1.evil.example/v1/chat/completions" ];
    let foreign = { anthropic with endpoint = "https://attacker.example/v1/messages" } in
    (match Pave.Provider.complete ~authentication:Pave.Provider.OAuth
      ~resolve_credential:(fun () -> credential_read := true;
        { Pave.Provider.access = "sensitive"; account_id = None; residency = None }) foreign
      [ system; user ] [] with
     | exception Pave.Provider.Provider_error _ -> assert (not !credential_read)
     | _ -> failwith "OAuth credential accepted by a foreign HTTPS endpoint");
    let codex = { anthropic with
      api = Pave.Provider.Codex_responses;
      endpoint = "https://chatgpt.com/backend-api/codex/responses" } in
    (match Pave.Provider.complete codex [ system; user ] [] with
     | exception Pave.Provider.Provider_error _ -> ()
     | _ -> failwith "Codex accepted an API key in place of its OAuth grant");
    credential_read := false;
    (match Pave.Provider.complete ~authentication:Pave.Provider.OAuth
      ~resolve_credential:(fun () -> credential_read := true;
        { Pave.Provider.access = "sensitive";
          account_id = Some "workspace"; residency = None })
      { codex with endpoint = "https://attacker.example/codex/responses" }
      [ system; user ] [] with
     | exception Pave.Provider.Provider_error _ -> assert (not !credential_read)
     | _ -> failwith "Codex bearer accepted by a foreign HTTPS endpoint");
    let copilot = { anthropic with api = Pave.Provider.Copilot_chat;
      endpoint = Pave.Github_copilot_wire.endpoint; model = "gpt-4.1" } in
    credential_read := false;
    (match Pave.Provider.complete ~authentication:Pave.Provider.OAuth
      ~resolve_credential:(fun () -> credential_read := true;
        { Pave.Provider.access = "sensitive"; account_id = None; residency = None })
      { copilot with endpoint = "https://attacker.example/chat/completions" }
      [ system; user ] [] with
     | exception Pave.Provider.Provider_error _ -> assert (not !credential_read)
     | _ -> failwith "Copilot bearer accepted by a foreign HTTPS endpoint");
    (match Pave.Provider.complete copilot [ system; user ] [] with
     | exception Pave.Provider.Provider_error _ -> ()
     | _ -> failwith "Copilot accepted an API key instead of a device grant");
    let image_user = { (Pave.Protocol.user "inspect") with attachments = [
      { name = "display.png"; mime_type = "image/png"; data = "aGVsbG8=" } ] } in
    let devin = { anthropic with api = Pave.Provider.Devin_connect;
      endpoint = Pave.Devin_api.chat_url } in
    (match Pave.Provider.complete ~authentication:Pave.Provider.OAuth
      ~resolve_credential:(fun () -> credential_read := true;
        { Pave.Provider.access = "sensitive"; account_id = None; residency = None })
      devin [image_user] [] with
     | exception Pave.Provider.Provider_error message ->
         assert (not !credential_read);
         assert (String.starts_with ~prefix:
           "this provider route does not support user image attachments" message)
     | _ -> failwith "Devin accepted user image attachments");
    let deltas = ref [] in
    let streamed = Pave.Provider.complete ~on_text:(fun delta -> deltas := delta :: !deltas)
      openai [ image_user ] [] in
    assert (!deltas = [ "Hello " ]);
    assert (streamed.content = Some "Hello ");
    assert (streamed.tool_calls = [ { Pave.Protocol.id = "call-1"; name = "read_file";
      arguments = `Assoc [ "path", `String "App.swift" ] } ]);
    let reply = Pave.Provider.complete anthropic [ system; user ] [] in
    assert (reply.content = Some "Inspected.");
    (match Pave.Provider.complete openai [ system; user ] [] with
     | exception Pave.Provider.Provider_error message ->
         assert (String.starts_with ~prefix:"provider rate limited" message);
         assert (not (leaks_key message))
     | _ -> failwith "expected HTTP error");
    let emitted = ref false in
    (match Pave.Provider.complete ~on_text:(fun _ -> emitted := true)
      openai [ system; user ] [] with
     | exception Pave.Provider.Provider_error message ->
         assert (String.starts_with ~prefix:"provider rate limited" message);
         assert (not (leaks_key message))
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
    assert (Unix.gettimeofday () -. started < 2.5);
    let cancelled = ref false in
    let signalled_at = ref None in
    let cancel () =
      if !cancelled then true
      else
        let readable, _, _ = Unix.select [signal_read] [] [] 0. in
        if readable = [] then false else (
          let byte = Bytes.create 1 in
          assert (Unix.read signal_read byte 0 1 = 1);
          signalled_at := Some (Unix.gettimeofday ());
          cancelled := true;
          true) in
    (match Pave.Provider.complete ~cancel openai [ system; user ] [] with
     | exception Pave.Provider.Cancelled -> ()
     | _ -> failwith "buffered request returned despite cancellation");
    (match !signalled_at with
     | Some started -> assert (Unix.gettimeofday () -. started < 1.)
     | None -> failwith "buffered request was not cancelled after reaching server");
    let expect_disconnect () =
      let readable, _, _ = Unix.select [closed_read] [] [] 1. in
      assert (readable <> []);
      let byte = Bytes.create 1 in
      assert (Unix.read closed_read byte 0 1 = 1) in
    expect_disconnect ();
    let streamed_text = ref [] in
    let streamed_cancelled = ref false in
    (match Pave.Provider.complete
      ~on_text:(fun text ->
        streamed_text := text :: !streamed_text;
        streamed_cancelled := true)
      ~cancel:(fun () -> !streamed_cancelled)
      openai [ system; user ] [] with
     | exception Pave.Provider.Cancelled -> ()
     | _ -> failwith "streamed request returned despite cancellation");
    assert (!streamed_text = [ "partial" ]);
    expect_disconnect ();
    List.iter (fun (id, url, env) ->
      let descriptor = Option.get (Pave.Provider_catalog.find id) in
      assert (descriptor.api_key_env = Some env);
      let route = Option.get (Pave.Provider_catalog.route descriptor "") in
      assert (route.endpoint = url);
      let config : Pave.Provider.config = {
        endpoint = Printf.sprintf "http://127.0.0.1:%d/chat/completions" port;
        api_key = "mock-" ^ id;
        model = "discovered-next-model";
        api = route.wire } in
      let tools = [`Assoc ["type", `String "function";
        "function", `Assoc ["name", `String "lookup";
          "parameters", `Assoc ["type", `String "object"]]]] in
      let first = Pave.Provider.complete config
        [Pave.Protocol.user "look up detail"] tools in
      assert (first.tool_calls = [{ Pave.Protocol.id = "call-" ^ id;
        name = "lookup"; arguments = `Assoc ["query", `String "detail"] }]);
      let second = Pave.Provider.complete config
        [Pave.Protocol.user "look up detail"; first;
         Pave.Protocol.tool_result ("call-" ^ id) "found"] tools in
      assert (second.content = Some (id ^ "-reply"));
      assert (second.tool_calls = [])) [
        "together", "https://api.together.ai/v1/chat/completions", "TOGETHER_API_KEY";
        "cerebras", "https://api.cerebras.ai/v1/chat/completions", "CEREBRAS_API_KEY";
        "venice", "https://api.venice.ai/api/v1/chat/completions", "VENICE_API_KEY";
        "deepinfra", "https://api.deepinfra.com/v1/openai/chat/completions", "DEEPINFRA_API_KEY";
        "fireworks", "https://api.fireworks.ai/inference/v1/chat/completions", "FIREWORKS_API_KEY";
        "baseten", "https://inference.baseten.co/v1/chat/completions", "BASETEN_API_KEY";
        "huggingface", "https://router.huggingface.co/v1/chat/completions", "HF_TOKEN";
        "nanogpt", "https://api.nano-gpt.com/api/v1/chat/completions", "NANO_GPT_API_KEY" ];
    List.iter (fun expected ->
      match Pave.Provider.complete openai [system; user] [] with
      | exception Pave.Provider.Provider_error message ->
          assert (String.starts_with ~prefix:expected message);
          assert (not (leaks_key message))
      | _ -> failwith ("expected classified provider error: " ^ expected)) [
        "provider authentication failed";
        "provider permission denied";
        "provider model or endpoint not found";
        "provider request exceeds its context or size limit";
        "provider rate limited";
        "provider unavailable";
        "provider context limit exceeded";
        "invalid provider request" ];
    curl_timeout_smoke ());
  print_endline "provider HTTP: ok"
