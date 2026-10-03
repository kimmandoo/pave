open Pave

let field = Protocol.member
let invalid f = match f () with
  | exception Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid response"
let assistant ?(content = None) ?state calls : Protocol.message =
  { role = "assistant"; content; tool_calls = calls; tool_call_id = None;
    tool_result_content = None; provider_state = state; attachments = [] }
let call : Protocol.tool_call =
  { id = "call-1"; name = "lookup"; arguments = `Assoc [] }
let chat ?(finish = "stop") message = `Assoc ["choices", `List [`Assoc [
  "index", `Int 0; "finish_reason", `String finish; "message", message]]]
let chunk ?(finish = `Null) delta = Yojson.Basic.to_string (`Assoc [
  "choices", `List [`Assoc ["index", `Int 0; "delta", delta;
    "finish_reason", finish]]])
let event data = "data: " ^ data ^ "\n\n"
let tool_delta ?(index = Some 0) ?(kind = "function") () =
  `Assoc ((match index with None -> [] | Some n -> ["index", `Int n]) @ [
    "id", `String call.id; "type", `String kind;
    "function", `Assoc ["name", `String call.name; "arguments", `String "{}"]])
let tools calls = `Assoc ["tool_calls", `List calls]
let stream wire =
  let parser = Openai_stream.create ~on_text:(fun _ -> ()) () in
  Openai_stream.feed parser wire;
  Openai_stream.finish parser

let sse_bom () =
  let wire = "\xEF\xBB\xBFdata: first\r\n\r\ndata: second\n\n" in
  for width = 1 to String.length wire do
    let events = ref [] in
    let parser = Sse.create ~on_event:(fun name data -> events := (name, data) :: !events) in
    Sse.feed parser "";
    let rec feed offset =
      if offset < String.length wire then (
        let size = min width (String.length wire - offset) in
        Sse.feed parser (String.sub wire offset size);
        feed (offset + size)) in
    feed 0;
    Sse.finish parser;
    assert (List.rev !events = [None, "first"; None, "second"])
  done

let sse_eof () =
  List.iter (fun tail ->
    let delivered = ref false in
    let parser = Sse.create ~on_event:(fun _ _ -> delivered := true) in
    Sse.feed parser tail;
    invalid (fun () -> Sse.finish parser);
    assert (not !delivered)) ["data: terminal"; "data: terminal\n"; "event: terminal\n"]

let chat_mismatch () =
  let message = Protocol.message_to_json (assistant [call]) in
  invalid (fun () -> Protocol.parse_completion (chat message));
  let wire = event (chunk (tools [tool_delta ()])) ^
    event (chunk ~finish:(`String "stop") (`Assoc [])) ^ event "[DONE]" in
  invalid (fun () -> stream wire);
  invalid (fun () -> stream (event (chunk ~finish:(`String "tool_calls")
    (`Assoc ["content", `String "not a tool turn"])) ^ event "[DONE]"))

let chat_index () =
  invalid (fun () -> stream (event (chunk (tools [tool_delta ~index:None ()])) ^
    event (chunk ~finish:(`String "tool_calls") (`Assoc [])) ^ event "[DONE]"));
  invalid (fun () -> stream (event (chunk (tools [tool_delta ~kind:"custom" ()])) ^
    event (chunk ~finish:(`String "tool_calls") (`Assoc [])) ^ event "[DONE]"))

let chat_after_finish () =
  invalid (fun () -> stream (
    event (chunk ~finish:(`String "tool_calls") (tools [tool_delta ()])) ^
    event (chunk (`Assoc ["content", `String "late output"])) ^ event "[DONE]"))

let chat_object_preview () =
  let arguments = `Assoc ["query", `String "Tokyo"] in
  let fragments = Buffer.create 32 in
  let parser = Openai_stream.create ~on_text:(fun _ -> ())
    ~on_tool_arguments:(fun (delta : Protocol.tool_argument_delta) ->
      Buffer.add_string fragments delta.fragment) () in
  let delta = `Assoc ["index", `Int 0; "id", `String call.id;
    "type", `String "function"; "function", `Assoc [
      "name", `String call.name; "arguments", arguments]] in
  Openai_stream.feed parser (event (chunk (tools [delta])) ^
    event (chunk ~finish:(`String "tool_calls") (`Assoc [])) ^ event "[DONE]");
  let completed = Openai_stream.finish parser in
  assert ((List.hd completed.tool_calls).arguments = arguments);
  assert (Yojson.Basic.from_string (Buffer.contents fragments) = arguments)

let codex_orphan () =
  let orphan = Protocol.tool_result_blocks call.id [
    Protocol.Image { mime_type = "image/png"; data = "private-base64" }] in
  invalid (fun () -> Codex_wire.request ~model:"gpt-5" [Protocol.user "request"; orphan] []);
  invalid (fun () -> Codex_wire.request ~model:"gpt-5" [assistant [call]] [])

let anthropic_reply ?(reason = "tool_use") () = `Assoc [
  "type", `String "message"; "role", `String "assistant";
  "stop_reason", `String reason;
  "content", `List [
    `Assoc ["type", `String "thinking"; "thinking", `String "private reasoning";
      "signature", `String "signed-state"];
    `Assoc ["type", `String "tool_use"; "id", `String call.id;
      "name", `String call.name; "input", call.arguments]]]
let anthropic_event kind fields = "event: " ^ kind ^ "\ndata: " ^
  Yojson.Basic.to_string (`Assoc (("type", `String kind) :: fields)) ^ "\n\n"
let anthropic_start = anthropic_event "message_start" ["message", `Assoc [
  "type", `String "message"; "role", `String "assistant"]]

let anthropic_stop () =
  List.iter (fun reason -> invalid (fun () ->
    Anthropic_wire.parse_response (anthropic_reply ~reason ())))
    ["pause_turn"; "future_failure"; "stop_sequence"];
  let parser = Anthropic_stream.create ~on_text:(fun _ -> ()) () in
  let block = `Assoc ["type", `String "tool_use"; "id", `String call.id;
    "name", `String call.name; "input", call.arguments] in
  invalid (fun () ->
    Anthropic_stream.feed parser (anthropic_start ^
      anthropic_event "content_block_start" ["index", `Int 0; "content_block", block] ^
      anthropic_event "content_block_stop" ["index", `Int 0] ^
      anthropic_event "message_delta" ["delta", `Assoc ["stop_reason", `String "future_failure"]] ^
      anthropic_event "message_stop" []);
    Anthropic_stream.finish parser)

let anthropic_block () =
  let text = `Assoc ["type", `String "text"; "text", `String "answer"] in
  let unsupported = `Assoc ["type", `String "future_result"; "data", `String "opaque"] in
  let parser = Anthropic_stream.create ~on_text:(fun _ -> ()) () in
  invalid (fun () ->
    Anthropic_stream.feed parser (anthropic_start ^
      anthropic_event "content_block_start" ["index", `Int 0; "content_block", text] ^
      anthropic_event "content_block_stop" ["index", `Int 0] ^
      anthropic_event "content_block_start" ["index", `Int 1; "content_block", unsupported] ^
      anthropic_event "content_block_stop" ["index", `Int 1] ^
      anthropic_event "message_delta" ["delta", `Assoc ["stop_reason", `String "end_turn"]] ^
      anthropic_event "message_stop" []);
    Anthropic_stream.finish parser)

let anthropic_signature () =
  let parser = Anthropic_stream.create ~model:"claude-test" ~on_text:(fun _ -> ()) () in
  Anthropic_stream.feed parser (anthropic_start ^
    anthropic_event "content_block_start" ["index", `Int 0; "content_block", `Assoc [
      "type", `String "thinking"; "thinking", `String "initial thought";
      "signature", `String "initial signature"]] ^
    anthropic_event "content_block_stop" ["index", `Int 0] ^
    anthropic_event "message_delta" ["delta", `Assoc ["stop_reason", `String "end_turn"]] ^
    anthropic_event "message_stop" []);
  let reply = Anthropic_stream.finish parser in
  match Anthropic_wire.replay_native_content ~provider:"anthropic" ~model:"claude-test" reply with
  | Some (`List [block]) -> assert (field "signature" block = `String "initial signature")
  | _ -> failwith "native thinking was not retained"

let anthropic_bound () =
  let parser = Anthropic_stream.create ~model:"claude-test" ~on_text:(fun _ -> ()) () in
  Anthropic_stream.feed parser anthropic_start;
  let data = String.make 1_000_000 'x' in
  invalid (fun () ->
    for index = 0 to 17 do
      Anthropic_stream.feed parser (
        anthropic_event "content_block_start" ["index", `Int index; "content_block", `Assoc [
          "type", `String "redacted_thinking"; "data", `String data]] ^
        anthropic_event "content_block_stop" ["index", `Int index])
    done)

let sanitize_native () =
  let parts = [`Assoc ["functionCall", `Assoc ["id", `String call.id;
    "name", `String call.name; "args", call.arguments]; "thoughtSignature", `String "signed"]] in
  let json = `Assoc ["candidates", `List [`Assoc ["finishReason", `String "STOP";
    "content", `Assoc ["role", `String "model"; "parts", `List parts]]]] in
  let native = Gemini_wire.parse_completion ~model:"gemini-2.5-flash" json in
  let transcript = [Protocol.user "first"; native; Protocol.tool_result call.id "one";
    Protocol.user "second"; native; Protocol.tool_result call.id "two"] in
  let sanitized = Protocol.sanitize_messages transcript in
  assert (sanitized = transcript);
  ignore (Gemini_wire.request ~model:"gemini-2.5-flash" sanitized []);
  let malformed = { native with tool_calls = [{call with name = ""}] } in
  invalid (fun () -> Protocol.sanitize_messages [malformed]);
  assert (Protocol.sanitize_messages [assistant []; { (Protocol.tool_result call.id "orphan")
    with tool_call_id = None }; Protocol.user "next"] = [Protocol.user "next"])

let gemini_candidates () =
  let candidate = `Assoc ["finishReason", `String "STOP";
    "content", `Assoc ["role", `String "model"; "parts", `List [`Assoc ["text", `String "one"]]]] in
  invalid (fun () -> Gemini_wire.parse_completion ~model:"gemini-2.5-flash"
    (`Assoc ["candidates", `List [candidate; candidate]]))

let bedrock_usage () =
  List.iter (fun usage -> assert (Bedrock_wire.usage (`Assoc ["usage", usage]) = None))
    [`Assoc []; `Assoc ["inputTokens", `Int 2];
     `Assoc ["inputTokens", `Int (-1); "outputTokens", `Int 2]];
  let measured = Bedrock_wire.usage (`Assoc ["usage", `Assoc [
    "inputTokens", `Int 3; "outputTokens", `Int 2; "cacheReadInputTokens", `Int 1]]) in
  match measured with
  | Some usage -> assert (usage.input_tokens = 3 && usage.output_tokens = 2 &&
      usage.cached_input_tokens = Some 1)
  | None -> failwith "complete usage was lost"

let bedrock_content () =
  let citations = `Assoc ["content", `List [`Assoc ["text", `String "answer with citations"]];
    "citations", `List []] in
  let message = `Assoc ["role", `String "assistant";
    "content", `List [`Assoc ["citationsContent", citations]]] in
  invalid (fun () -> Bedrock_wire.parse_response (`Assoc [
    "stopReason", `String "end_turn"; "output", `Assoc ["message", message]]))

let aws_region () =
  match Aws_auth.region ~getenv:(fun _ -> None) () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith "missing region selected an unrequested region"

(* This server uses the production curl executable and public Provider.complete
   loopback route. It counts real requests, and never supplies a fake transport. *)
let read_request input =
  ignore (input_line input);
  let rec headers length =
    let line = String.trim (input_line input) in
    if line = "" then length else
    let length = if String.starts_with ~prefix:"content-length:" (String.lowercase_ascii line)
      then int_of_string (String.trim (String.sub line 15 (String.length line - 15))) else length in
    headers length in
  let length = headers 0 in
  assert (length >= 0 && length <= 16_777_216);
  Yojson.Basic.from_string (really_input_string input length)

let with_server respond f =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.set_close_on_exec listener;
  Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen listener 4;
  let port = match Unix.getsockname listener with Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let stop_read, stop_write = Unix.pipe ~cloexec:true () in
  let report_read, report_write = Unix.pipe ~cloexec:true () in
  let pid = Unix.fork () in
  if pid = 0 then (
    Unix.close stop_write; Unix.close report_read;
    ignore (Unix.alarm 15);
    let count = ref 0 in
    let result = try
      let rec serve () =
        let ready, _, _ = Unix.select [listener; stop_read] [] [] 10. in
        if List.mem stop_read ready then ()
        else if List.mem listener ready then (
          let socket, _ = Unix.accept listener in
          let input = Unix.in_channel_of_descr socket in
          let output = Unix.out_channel_of_descr (Unix.dup socket) in
          Fun.protect ~finally:(fun () -> close_in_noerr input; close_out_noerr output) (fun () ->
            let request = read_request input in
            incr count;
            let status, body = respond !count request in
            Printf.fprintf output "HTTP/1.1 %d Fixture\r\nConnection: close\r\nContent-Length: %d\r\n\r\n%s"
              status (String.length body) body;
            flush output);
          serve ())
        else failwith "loopback fixture timed out" in
      serve (); string_of_int !count
    with exn -> "error: " ^ Printexc.to_string exn in
    let report = Unix.out_channel_of_descr report_write in
    output_string report result; close_out report;
    Unix.close listener; Unix.close stop_read;
    exit (if String.starts_with ~prefix:"error:" result then 1 else 0));
  Unix.close listener; Unix.close stop_read; Unix.close report_write;
  let report = Unix.in_channel_of_descr report_read in
  let count = ref 0 in
  let answer = Fun.protect ~finally:(fun () ->
    ignore (Unix.write_substring stop_write "x" 0 1);
    Unix.close stop_write;
    let _, status = Unix.waitpid [] pid in
    let text = input_line report in
    close_in report;
    if status <> Unix.WEXITED 0 then failwith text;
    count := int_of_string text) (fun () -> f (Printf.sprintf "http://127.0.0.1:%d/v1/chat/completions" port)) in
  answer, !count

let answer = Yojson.Basic.to_string (chat (`Assoc ["role", `String "assistant";
  "content", `String "complete"]))
let stream_answer = event (chunk ~finish:(`String "stop") (`Assoc ["content", `String "complete"])) ^ event "[DONE]"
let config endpoint : Provider.config =
  { api = Provider.Local_chat; endpoint; model = "fixture"; api_key = "" }
let cancel_deadline () =
  let deadline = Unix.gettimeofday () +. 5. in
  fun () -> Unix.gettimeofday () >= deadline

let no_retry streaming () =
  let (_, count) = with_server (fun n _ ->
    if n = 1 then 500, "{\"error\":{\"message\":\"ambiguous server failure\"}}"
    else 200, (if streaming then stream_answer else answer)) (fun endpoint ->
      let usage = ref 0 in
      let on_text = if streaming then Some (fun _ -> ()) else None in
      match Provider.complete ?on_text ~cancel:(cancel_deadline ())
        ~on_usage:(fun _ -> incr usage) (config endpoint) [Protocol.user "request"] [] with
      | exception Provider.Provider_error _ -> assert (!usage = 0)
      | _ -> failwith "completion was automatically replayed after HTTP 500") in
  assert (count = 1)

let loopback_chat_effort () =
  let (_, count) = with_server (fun _ request ->
    if field "reasoning_effort" request <> `String "high" then 400, "{}" else 200, answer)
    (fun endpoint ->
      let reply = Provider.complete ~thinking:"high" ~cancel:(cancel_deadline ())
        (config endpoint) [Protocol.user "request"] [] in
      assert (reply.content = Some "complete")) in
  assert (count = 1)

let loopback_local_usage () =
  let (_, count) = with_server (fun _ request ->
    if field "stream_options" request <> `Null then 400, "{}" else 200, stream_answer)
    (fun endpoint ->
      let reply = Provider.complete ~on_text:(fun _ -> ()) ~cancel:(cancel_deadline ())
        (config endpoint) [Protocol.user "request"] [] in
      assert (reply.content = Some "complete")) in
  assert (count = 1)

let loopback_invalid_tool () =
  let wire = event (chunk (tools [tool_delta ()])) ^
    event (chunk ~finish:(`String "stop") (`Assoc [])) ^ event "[DONE]" in
  let (_, count) = with_server (fun _ _ -> 200, wire) (fun endpoint ->
    match Provider.complete ~on_text:(fun _ -> ()) ~cancel:(cancel_deadline ())
      (config endpoint) [Protocol.user "request"] [] with
    | exception Provider.Provider_error _ -> ()
    | _ -> failwith "invalid terminal outcome exposed a tool call") in
  assert (count = 1)

let send_request body validate =
  let (_, count) = with_server (fun _ request ->
    if validate request then 200, "{\"accepted\":true}" else 400, "{}") (fun endpoint ->
      let reply = Provider.post_json ~local:true ~cancel:(cancel_deadline ())
        ~endpoint ~headers:[] ~secret:"" body in
      assert (field "accepted" reply = `Bool true)) in
  assert (count = 1)

let loopback_anthropic_budget () =
  send_request (Anthropic_wire.request ~thinking:"medium" ~model:"claude-sonnet-4-5" ~max_tokens:4096
    [Protocol.user "request"] []) (fun request ->
      match field "budget_tokens" (field "thinking" request), field "max_tokens" request with
      | `Int budget, `Int limit -> budget >= 1024 && budget < limit
      | _ -> false)

let loopback_anthropic_adaptive () =
  send_request (Anthropic_wire.request ~thinking:"high" ~model:"claude-opus-4-7" ~max_tokens:4096
    [Protocol.user "request"] []) (fun request ->
      field "type" (field "thinking" request) = `String "adaptive" &&
      field "effort" (field "output_config" request) = `String "high")

let loopback_gemini_thinking () =
  send_request (Gemini_wire.request ~thinking:"low" ~model:"gemini-2.5-flash"
    [Protocol.user "request"] []) (fun request ->
      let thinking = field "thinkingConfig" (field "generationConfig" request) in
      field "thinkingLevel" thinking = `Null && field "thinkingBudget" thinking = `Int 2048)

let loopback_ollama_thinking () =
  send_request (Ollama_wire.request ~thinking:"max" ~model:"gpt-oss"
    [Protocol.user "request"] []) (fun request -> field "think" request = `String "high")

let loopback_ollama_boolean () =
  send_request (Ollama_wire.request ~thinking:"high" ~model:"qwen3"
    [Protocol.user "request"] []) (fun request -> field "think" request = `Bool true)

let cache_native_integrity () =
  let model = "claude-sonnet-4-5" in
  let content = `List [
    `Assoc ["type", `String "text"; "text", `String "answer"];
    `Assoc ["type", `String "redacted_thinking"; "data", `String "opaque-state"]] in
  let response = `Assoc ["type", `String "message"; "role", `String "assistant";
    "stop_reason", `String "end_turn"; "content", content] in
  let native = Anthropic_wire.parse_native_completion ~provider:"anthropic" ~model response in
  let body = Anthropic_wire.request ~allow_prompt_caching:true ~model ~max_tokens:4096
    ~replay_assistant_content:(Anthropic_wire.replay_native_content ~provider:"anthropic" ~model)
    [Protocol.user "request"; native] [] in
  send_request body (fun request ->
    match field "messages" request with
    | `List [_; replayed] -> field "content" replayed = content
    | _ -> false)

let read_file path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
    really_input_string input (in_channel_length input))
let write_file path text =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output) (fun () -> output_string output text)

let fixture_curl () =
  let input = Buffer.create 1024 in
  (try while true do Buffer.add_string input (input_line stdin); Buffer.add_char input '\n' done
   with End_of_file -> ());
  let options name =
    let prefix = name ^ " = " in
    String.split_on_char '\n' (Buffer.contents input) |> List.filter_map (fun line ->
      if String.starts_with ~prefix line then
        match Yojson.Basic.from_string (String.sub line (String.length prefix)
            (String.length line - String.length prefix)) with
        | `String value -> Some value | _ -> assert false
      else None) in
  let body_path = match options "data-binary" with
    | [path] -> String.sub path 1 (String.length path - 1) | _ -> assert false in
  write_file (Sys.getenv "PAVE_DATED_REQUEST") (read_file body_path);
  let response = read_file (Sys.getenv "PAVE_DATED_RESPONSE") in
  let status = if Sys.getenv_opt "PAVE_DATED_EXPECT_VISION" = Some "1" &&
    not (List.mem "Copilot-Vision-Request: true" (options "header"))
    then 400 else 200 in
  (match options "dump-header" with
   | [path] ->
       write_file path (Printf.sprintf
         "HTTP/1.1 %d Fixture\r\nContent-Type: text/event-stream\r\n\r\n" status);
       print_string response
   | [] -> print_string response; print_string (string_of_int status)
   | _ -> assert false);
  flush stdout

let with_native_fixture f =
  let directory = Filename.temp_file "pave-native-replay-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let request = Filename.concat directory "request.json" in
  let response = Filename.concat directory "response.json" in
  let entries = ["PAVE_DATED_REQUEST", request; "PAVE_DATED_RESPONSE", response] in
  let previous = List.map (fun (name, _) -> name, Sys.getenv_opt name) entries in
  Provider.Test.use_curl_helper Sys.executable_name;
  Fun.protect ~finally:(fun () ->
    List.iter (fun (name, value) -> Unix.putenv name (Option.value ~default:"" value)) previous;
    List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [request; response];
    Unix.rmdir directory) (fun () ->
      List.iter (fun (name, value) -> Unix.putenv name value) entries;
      f ~request ~response)

let custom_native_replay () =
  with_native_fixture (fun ~request ~response ->
    let configuration : Provider.config = { api = Provider.Anthropic_messages;
      endpoint = "https://custom-a.example/v1/messages"; model = "claude-test";
      api_key = "fixture-key" } in
    let native_response = anthropic_reply () in
    write_file response (Yojson.Basic.to_string native_response);
    let first = Provider.complete configuration [Protocol.user "lookup"] [] in
    let transcript = [Protocol.user "lookup"; first; Protocol.tool_result call.id "found"] in
    let filtered = Interaction.history_for_model ~provider:"anthropic" ~route:"messages"
      ~wire:Provider.Anthropic_messages ~model:configuration.model transcript in
    let final = `Assoc ["type", `String "message"; "role", `String "assistant";
      "stop_reason", `String "end_turn"; "content", `List [
        `Assoc ["type", `String "text"; "text", `String "found"]]] in
    write_file response (Yojson.Basic.to_string final);
    let continued = Provider.complete configuration filtered [] in
    assert (continued.content = Some "found");
    let captured = Yojson.Basic.from_string (read_file request) in
    (match field "messages" captured with
     | `List [_; native; _] -> assert (field "content" native = field "content" native_response)
     | _ -> failwith "custom native continuation was lost");
    ignore (Provider.complete { configuration with endpoint = "https://custom-b.example/v1/messages" }
      filtered []);
    let foreign = Yojson.Basic.from_string (read_file request) in
    (match field "messages" foreign with
     | `List [_; native; _] ->
         (match field "content" native with
          | `List blocks -> assert (not (List.exists Anthropic_wire.native_thinking_block blocks))
          | _ -> assert false)
     | _ -> assert false))


let custom_native_stream () =
  with_native_fixture (fun ~request ~response ->
    let configuration : Provider.config = { api = Provider.Anthropic_messages;
      endpoint = "https://custom-a.example/v1/messages"; model = "claude-test";
      api_key = "fixture-key" } in
    let thinking = `Assoc ["type", `String "thinking"; "thinking", `String "private";
      "signature", `String "signed-stream"] in
    let text = `Assoc ["type", `String "text"; "text", `String "answer"] in
    write_file response (anthropic_start ^
      anthropic_event "content_block_start" ["index", `Int 0; "content_block", thinking] ^
      anthropic_event "content_block_stop" ["index", `Int 0] ^
      anthropic_event "content_block_start" ["index", `Int 1; "content_block", text] ^
      anthropic_event "content_block_stop" ["index", `Int 1] ^
      anthropic_event "message_delta" ["delta", `Assoc ["stop_reason", `String "end_turn"]] ^
      anthropic_event "message_stop" []);
    let first = Provider.complete ~on_text:(fun _ -> ()) configuration [Protocol.user "request"] [] in
    let transcript = Interaction.history_for_model ~provider:"anthropic" ~route:"messages"
      ~wire:Provider.Anthropic_messages ~model:configuration.model
      [Protocol.user "request"; first; Protocol.user "continue"] in
    write_file response (Yojson.Basic.to_string (`Assoc [
      "type", `String "message"; "role", `String "assistant"; "stop_reason", `String "end_turn";
      "content", `List [`Assoc ["type", `String "text"; "text", `String "done"]]]));
    let continued = Provider.complete configuration transcript [] in
    assert (continued.content = Some "done");
    let captured = Yojson.Basic.from_string (read_file request) in
    match field "messages" captured with
    | `List [_; native; _] ->
        (match field "content" native with
         | `List [thought; _] ->
             assert (field "thinking" thought = `String "private");
             assert (field "signature" thought = `String "signed-stream")
         | _ -> assert false)
    | _ -> assert false)
let vertex_native_replay () =
  let model = "claude-sonnet-4-5" in
  let native_response = anthropic_reply () in
  let first = Vertex_anthropic_wire.parse_completion ~model native_response in
  let transcript = Interaction.history_for_model ~provider:"google-vertex" ~route:"messages"
    ~wire:Provider.Vertex_anthropic ~model
    [Protocol.user "lookup"; first; Protocol.tool_result call.id "found"] in
  let body = Vertex_anthropic_wire.request ~model ~max_tokens:4096 ~streaming:false transcript [] in
  let (_, count) = with_server (fun _ request ->
    let valid = match field "messages" request with
      | `List [_; native; _] -> field "content" native = field "content" native_response
      | _ -> false in
    if valid then 200, {|{"accepted":true}|} else 400, "{}") (fun endpoint ->
      let accepted = Provider.post_json ~local:true ~cancel:(cancel_deadline ())
        ~endpoint ~headers:[] ~secret:"" body in
      assert (field "accepted" accepted = `Bool true)) in
  assert (count = 1)

let copilot_tool_vision () =
  let previous = Sys.getenv_opt "PAVE_DATED_EXPECT_VISION" in
  Fun.protect ~finally:(fun () ->
    Unix.putenv "PAVE_DATED_EXPECT_VISION" (Option.value ~default:"" previous)) (fun () ->
    Unix.putenv "PAVE_DATED_EXPECT_VISION" "1";
    with_native_fixture (fun ~request:_ ~response ->
      write_file response answer;
      let config : Provider.config = { api = Provider.Copilot_chat;
        endpoint = Github_copilot_wire.endpoint; model = "gpt-4.1";
        api_key = "fixture-token" } in
      let image = Protocol.tool_result_blocks call.id [
        Protocol.Image { mime_type = "image/png"; data = "AQID" }] in
      let reply = Provider.complete ~authentication:Provider.OAuth config
        [Protocol.user "inspect"; assistant [call]; image] [] in
      assert (reply.content = Some "complete")))

let curl_fd_ownership () =
  with_native_fixture (fun ~request:_ ~response ->
    write_file response "descriptor-safe";
    let unrelated = ref [] in
    Fun.protect ~finally:(fun () ->
      List.iter (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ()) !unrelated)
      (fun () ->
        let on_chunk _ =
          if !unrelated = [] then
            for _ = 1 to 8 do
              unrelated := Unix.openfile response [Unix.O_RDONLY] 0 :: !unrelated
            done in
        let configuration = Printf.sprintf "data-binary = %s\n"
          (Yojson.Basic.to_string (`String ("@" ^ response))) in
        let received = Provider.run_curl ~on_chunk configuration in
        assert (received = "descriptor-safe200");
        List.iter (fun fd ->
          let bytes = Bytes.create 15 in
          assert (Unix.read fd bytes 0 15 = 15);
          assert (Bytes.to_string bytes = "descriptor-safe")) !unrelated))

let cases = [
  "anthropic-block", anthropic_block;
  "sse-bom", sse_bom; "sse-eof", sse_eof;
  "chat-mismatch", chat_mismatch; "chat-index", chat_index;
  "chat-object-preview", chat_object_preview; "codex-orphan", codex_orphan;
  "chat-after-finish", chat_after_finish; "anthropic-stop", anthropic_stop;
  "anthropic-signature", anthropic_signature; "anthropic-bound", anthropic_bound;
  "sanitize-native", sanitize_native; "gemini-candidates", gemini_candidates;
  "bedrock-usage", bedrock_usage; "aws-region", aws_region;
  "bedrock-content", bedrock_content;
  "no-retry-buffered", no_retry false; "no-retry-stream", no_retry true;
  "loopback-chat-effort", loopback_chat_effort; "loopback-local-usage", loopback_local_usage;
  "loopback-invalid-tool", loopback_invalid_tool;
  "loopback-anthropic-budget", loopback_anthropic_budget;
  "loopback-anthropic-adaptive", loopback_anthropic_adaptive;
  "loopback-gemini-thinking", loopback_gemini_thinking;
  "loopback-ollama-thinking", loopback_ollama_thinking;
  "loopback-ollama-boolean", loopback_ollama_boolean;
  "cache-native-integrity", cache_native_integrity;
  "vertex-native-replay", vertex_native_replay; "custom-native-replay", custom_native_replay;
  "custom-native-stream", custom_native_stream;
  "copilot-tool-vision", copilot_tool_vision;
  "curl-fd-ownership", curl_fd_ownership]

let () =
  if Array.to_list Sys.argv |> List.mem "--disable" then fixture_curl ()
  else (
    let selected = if Array.length Sys.argv = 1 then cases else
      let name = Sys.argv.(1) in [name, List.assoc name cases] in
    List.iter (fun (name, run) -> run (); print_endline (name ^ ": ok")) selected)
