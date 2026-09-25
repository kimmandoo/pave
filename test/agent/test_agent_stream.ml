let event data = "data: " ^ data ^ "\n\n"
let tool_call = event
  {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"call-mobile","function":{"name":"read_file","arguments":"{\"path\":\"App.swift\"}"}}]},"finish_reason":null}]}|}
  ^ event {|{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}]}|}
  ^ event "[DONE]"
let answer = event
  {|{"choices":[{"index":0,"delta":{"content":"Swift source verified."},"finish_reason":"stop"}]}|}
  ^ event "[DONE]"
let side_effect_call = event
  {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"write-mobile","function":{"name":"write_file","arguments":"{\"path\":\"MUST_NOT_EXIST\",\"content\":\"bad\"}"}}]},"finish_reason":null}]}|}
  ^ event {|{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}]}|}
  ^ event "[DONE]"
let two_calls = event
  {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"read-once","function":{"name":"read_file","arguments":"{\"path\":\"App.swift\"}"}},{"index":1,"id":"write-twice","function":{"name":"write_file","arguments":"{\"path\":\"MUST_NOT_EXIST\",\"content\":\"bad\"}"}}]},"finish_reason":null}]}|}
  ^ event {|{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}]}|}
  ^ event "[DONE]"
let shell_call = event
  {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"shell-wait","function":{"name":"run_command","arguments":"{\"command\":\"printf early; sleep 5\",\"timeout_seconds\":20}"}}]},"finish_reason":null}]}|}
  ^ event {|{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}]}|}
  ^ event "[DONE]"
let dynamic_first_call = event
  {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"dynamic-first","function":{"name":"read_file","arguments":"{\"path\":\"App.swift\"}"}}]},"finish_reason":null}]}|}
  ^ event {|{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}]}|}
  ^ event "[DONE]"
let stale_dynamic_call = event
  {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"dynamic-stale","function":{"name":"write_file","arguments":"{\"path\":\"MUST_NOT_EXIST\",\"content\":\"blocked\"}"}}]},"finish_reason":null}]}|}
  ^ event {|{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}]}|}
  ^ event "[DONE]"
let dynamic_answer = event
  {|{"choices":[{"index":0,"delta":{"content":"Dynamic availability enforced."},"finish_reason":"stop"}]}|}
  ^ event "[DONE]"
let invalid_shell_call = event
  {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"invalid-shell","function":{"name":"run_command","arguments":"{\"command\":\"touch SHOULD_NOT_EXIST\",\"unexpected\":true}"}}]},"finish_reason":null}]}|}
  ^ event {|{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}]}|}
  ^ event "[DONE]"
let invalid_shell_answer = event
  {|{"choices":[{"index":0,"delta":{"content":"Invalid arguments rejected before approval."},"finish_reason":"stop"}]}|}
  ^ event "[DONE]"
let parallel_read_calls = event
  {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"parallel-first","function":{"name":"read_file","arguments":"{\"path\":\"App.swift\"}"}},{"index":1,"id":"parallel-second","function":{"name":"read_file","arguments":"{\"path\":\"App.swift\"}"}}]},"finish_reason":null}]}|}
  ^ event {|{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}]}|}
  ^ event "[DONE]"
let parallel_read_answer = event
  {|{"choices":[{"index":0,"delta":{"content":"Parallel read results stayed ordered."},"finish_reason":"stop"}]}|}
  ^ event "[DONE]"



let serve client step =
  let ic = Unix.in_channel_of_descr client in
  let oc = Unix.out_channel_of_descr client in
  let length = ref 0 in
  let rec headers () =
    let line = input_line ic in
    if line <> "\r" && line <> "" then (
      let lower = String.lowercase_ascii line in
      if String.starts_with ~prefix:"content-length:" lower then
        length := int_of_string (String.trim (String.sub line 15 (String.length line - 15)));
      headers ()) in
  headers ();
  let request = Yojson.Basic.from_string (really_input_string ic !length) in
  let contains_tool name =
    match Pave.Protocol.member "tools" request with
    | `List tools -> List.exists (fun tool ->
        Pave.Protocol.member "name" (Pave.Protocol.member "function" tool)
          = `String name) tools
    | _ -> failwith "missing tool definitions" in
  (match Pave.Protocol.member "messages" request with
   | `List messages ->
       let results = List.filter (fun item -> Pave.Protocol.member "role" item = `String "tool") messages in
       let result call_id = List.find_opt (fun item ->
         Pave.Protocol.member "tool_call_id" item = `String call_id) results in
       (match step with
        | 1 ->
            assert (match result "call-mobile" with
              | Some item -> (match Pave.Protocol.member "content" item with
                  | `String text -> String.starts_with ~prefix:"struct App {}\n" text
                  | _ -> false)
              | None -> false)
        | 5 -> assert (results = [])
        | 6 ->
            assert (List.length results = 1);
            assert (result "dynamic-first" <> None)
        | 7 ->
            assert (List.length results = 2);
            assert (match result "dynamic-stale" with
              | Some item -> (match Pave.Protocol.member "content" item with
                  | `String text -> String.starts_with ~prefix:"Error: tool is no longer available" text
                  | _ -> false)
              | None -> false)
        | 8 -> assert (results = [])
        | 9 ->
            assert (List.length results = 1);
            assert (match result "invalid-shell" with
              | Some item -> (match Pave.Protocol.member "content" item with
                  | `String text -> String.starts_with ~prefix:"Error: unexpected argument" text
                  | _ -> false)
              | None -> false)
        | 10 -> assert (results = [])
        | 11 ->
            assert (List.length results = 2);
            assert (List.map (fun item -> Pave.Protocol.member "tool_call_id" item)
              results = [`String "parallel-first"; `String "parallel-second"]);
            List.iter (fun call_id ->
              assert (match result call_id with
                | Some item -> (match Pave.Protocol.member "content" item with
                    | `String text -> String.starts_with ~prefix:"struct App {}\n" text
                    | _ -> false)
                | None -> false)) ["parallel-first"; "parallel-second"]
        | _ -> assert (results = []));
       if step = 5 then assert (contains_tool "write_file");
       if step = 6 || step = 7 then assert (not (contains_tool "write_file"));
       if step = 8 || step = 9 then assert (contains_tool "run_command");
       if step = 10 || step = 11 then assert (contains_tool "read_file")
   | _ -> failwith "missing messages");
  let body = match step with
    | 0 -> tool_call | 1 -> answer | 2 -> side_effect_call
    | 3 -> two_calls | 4 -> shell_call | 5 -> dynamic_first_call
    | 6 -> stale_dynamic_call | 7 -> dynamic_answer
    | 8 -> invalid_shell_call | 9 -> invalid_shell_answer
    | 10 -> parallel_read_calls | _ -> parallel_read_answer in
  Printf.fprintf oc "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
    (String.length body) body;
  flush oc;
  close_in_noerr ic; close_out_noerr oc

let () =
  let root = Filename.temp_file "pave-agent-stream-" "" in
  Sys.remove root; Unix.mkdir root 0o700;
  let file = Filename.concat root "App.swift" in
  let oc = open_out file in output_string oc "struct App {}\n"; close_out oc;
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)); Unix.listen socket 2;
  let port = match Unix.getsockname socket with Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    (try for step = 0 to 11 do
      let client, _ = Unix.accept socket in serve client step
    done with exn -> prerr_endline (Printexc.to_string exn); exit 2);
    exit 0);
  Unix.close socket;
  Fun.protect ~finally:(fun () ->
    (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
    ignore (Unix.waitpid [] child);
    Sys.remove file; Unix.rmdir root) (fun () ->
    let provider : Pave.Provider.config = { api = Pave.Provider.Openai_completions;
      endpoint = Printf.sprintf "http://127.0.0.1:%d/chat/completions" port;
      api_key = "mock"; model = "mock" } in
    let events = ref [] and deltas = ref [] and tool_events = ref [] in
    let outcome_order = ref [] in
    let agent = Pave.Agent.create ~provider ~root ~system:"inspect the mobile repo"
      ~stream:true ~on_event:(fun text -> events := text :: !events)
      ~on_delta:(fun text -> deltas := text :: !deltas)
      ~on_change:(fun message ->
        if message.Pave.Protocol.role = "tool" then
          outcome_order := "result" :: !outcome_order)
      ~on_tool_event:(fun event ->
        tool_events := event :: !tool_events;
        match event with
        | Pave.Agent.Tool_started _ -> outcome_order := "started" :: !outcome_order
        | Pave.Agent.Tool_settled _ -> outcome_order := "settled" :: !outcome_order
        | Pave.Agent.Tool_updated _ | Pave.Agent.Tool_aborted _ -> ()) () in
    assert (Pave.Agent.run agent "Read App.swift" = "Swift source verified.");
    assert (List.rev !deltas = [ "Swift source verified."; "\n" ]);
    assert (not (List.mem "Swift source verified." !events));
    assert (List.rev !outcome_order = ["started"; "settled"; "result"]);
    assert (List.length (Pave.Agent.messages agent) = 4);
    (match List.rev !tool_events with
     | [ Pave.Agent.Tool_started { call_id = "call-mobile"; name = "read_file" };
         Pave.Agent.Tool_settled {
           call_id = "call-mobile"; name = "read_file"; result; is_error = false
         } ] ->
         assert (String.starts_with ~prefix:"struct App {}\n" result)
     | _ -> failwith "tool call did not emit ordered typed lifecycle events");
    let cancelled = ref false in
    let changes = ref [] and cancel_events = ref [] and cancel_order = ref [] in
    let cancelled_agent = Pave.Agent.create ~provider ~root ~system:"inspect the mobile repo"
      ~stream:true ~on_event:(fun _ -> ())
      ~on_tool_event:(fun event ->
        cancel_events := event :: !cancel_events;
        (match event with
         | Pave.Agent.Tool_started { call_id = "write-mobile"; _ } ->
             cancelled := true;
             cancel_order := "started" :: !cancel_order
         | Pave.Agent.Tool_aborted { call_id = "write-mobile"; _ } ->
             cancel_order := "aborted" :: !cancel_order
         | _ -> ()))
      ~on_change:(fun message ->
        changes := message :: !changes;
        if message.Pave.Protocol.role = "tool" then
          cancel_order := "result" :: !cancel_order) () in
    (match Pave.Agent.run ~cancel:(fun () -> !cancelled) cancelled_agent "Write file" with
     | exception Pave.Provider.Cancelled -> ()
     | _ -> failwith "agent executed a cancelled tool call");
    assert (not (Sys.file_exists (Filename.concat root "MUST_NOT_EXIST")));
    (match Pave.Agent.messages cancelled_agent with
     | [ user; assistant; result ] ->
         assert (user.Pave.Protocol.role = "user");
         assert (List.map (fun (call : Pave.Protocol.tool_call) -> call.id)
           assistant.tool_calls = ["write-mobile"]);
         assert (result.Pave.Protocol.role = "tool");
         assert (result.tool_call_id = Some "write-mobile");
         assert (match result.content with
           | Some text -> String.starts_with ~prefix:"Error:" text
           | None -> false)
     | _ -> failwith "canceled tool turn was not persisted with a paired result");
    (match List.rev !cancel_events with
     | [ Pave.Agent.Tool_started { call_id = "write-mobile"; name = "write_file" };
         Pave.Agent.Tool_aborted {
           call_id = "write-mobile"; side_effects_may_have_occurred = false; result; _
         } ] ->
         assert (String.starts_with ~prefix:"Error:" result)
     | _ -> failwith "canceled unstarted tool did not settle as aborted");
    assert (List.rev !cancel_order = ["started"; "aborted"; "result"]);
    let cancelled_after_tool = ref false and partial_events = ref [] in
    let partial_agent = Pave.Agent.create ~provider ~root ~system:"inspect the mobile repo"
      ~stream:true ~on_event:(fun _ -> ())
      ~on_tool_event:(fun event ->
        partial_events := event :: !partial_events;
        match event with
        | Pave.Agent.Tool_settled { call_id = "read-once"; _ } ->
            cancelled_after_tool := true
        | _ -> ()) () in
    (match Pave.Agent.run ~cancel:(fun () -> !cancelled_after_tool)
      partial_agent "Read then write" with
     | exception Pave.Provider.Cancelled -> ()
     | _ -> failwith "agent continued after cancellation between tools");
    (match Pave.Agent.messages partial_agent with
     | [ user; assistant; first; skipped ] ->
         assert (user.role = "user");
         assert (List.length assistant.tool_calls = 2);
         assert (first.tool_call_id = Some "read-once");
         assert (match first.content with
           | Some text -> String.starts_with ~prefix:"struct App {}\n" text
           | None -> false);
         assert (skipped.tool_call_id = Some "write-twice");
         assert (match skipped.content with
           | Some text -> String.starts_with ~prefix:"Error:" text
           | None -> false)
     | _ -> failwith "cancelled tool sequence left a dangling tool call");
    (match List.rev !partial_events with
     | [ Pave.Agent.Tool_started { call_id = "read-once"; name = "read_file" };
         Pave.Agent.Tool_settled { call_id = "read-once"; is_error = false; _ };
         Pave.Agent.Tool_started { call_id = "write-twice"; name = "write_file" };
         Pave.Agent.Tool_aborted {
           call_id = "write-twice"; side_effects_may_have_occurred = false; _
         } ] -> ()
     | _ -> failwith "multi-tool lifecycle lost call IDs or provider order");
    assert (not (Sys.file_exists (Filename.concat root "MUST_NOT_EXIST")));
    let shell_started = ref None and shell_events = ref [] in
    let shell_agent = Pave.Agent.create ~provider ~root ~system:"inspect the mobile repo"
      ~stream:true ~allow_shell:true ~on_event:(fun _ -> ())
      ~on_tool_event:(fun event -> shell_events := event :: !shell_events)
      ~approve_command:(fun _ -> shell_started := Some (Unix.gettimeofday ()); true) () in
    let started = Unix.gettimeofday () in
    (match Pave.Agent.run ~cancel:(fun () -> match !shell_started with
      | Some accepted -> Unix.gettimeofday () -. accepted > 0.25
      | None -> false) shell_agent "Run shell" with
     | exception Pave.Provider.Cancelled -> ()
     | _ -> failwith "agent did not cancel an active command");
    assert (Unix.gettimeofday () -. started < 2.);
    (match Pave.Agent.messages shell_agent with
     | [ user; assistant; stopped ] ->
         assert (user.role = "user");
         assert (List.length assistant.tool_calls = 1);
         assert (stopped.tool_call_id = Some "shell-wait");
         assert (match stopped.content with
           | Some text -> String.starts_with ~prefix:"Error:" text
           | None -> false)
     | _ -> failwith "cancelled command did not settle the tool call");
    (match List.rev !shell_events with
     | Pave.Agent.Tool_started { call_id = "shell-wait"; name = "run_command" } :: tail ->
         assert (List.exists (function
           | Pave.Agent.Tool_updated {
               call_id = "shell-wait"; received_bytes; _
             } -> received_bytes >= String.length "early"
           | _ -> false) tail);
         assert (List.exists (function
           | Pave.Agent.Tool_aborted {
               call_id = "shell-wait";
               side_effects_may_have_occurred = true; _
             } -> true
           | _ -> false) tail);
         assert (not (List.exists (function
           | Pave.Agent.Tool_settled { call_id = "shell-wait"; _ } -> true
           | _ -> false) tail))
     | _ -> failwith "running shell tool emitted no progress/abort lifecycle");
    let write_enabled = ref true and dynamic_events = ref [] in
    let dynamic_agent = Pave.Agent.create ~provider ~root
      ~system:"inspect the mobile repo" ~stream:true ~on_event:(fun _ -> ())
      ~tool_available:(fun name ->
        name <> "write_file" || !write_enabled)
      ~on_tool_event:(fun event ->
        dynamic_events := event :: !dynamic_events;
        match event with
        | Pave.Agent.Tool_settled {
            call_id = "dynamic-first"; is_error = false; _
          } -> write_enabled := false
        | _ -> ()) () in
    assert (Pave.Agent.run dynamic_agent "Check dynamic tool availability"
      = "Dynamic availability enforced.");
    (match Pave.Agent.messages dynamic_agent with
     | [ user; first; first_result; stale; stale_result; final ] ->
         assert (user.role = "user");
         assert (List.map (fun (call : Pave.Protocol.tool_call) -> call.id)
           first.tool_calls = ["dynamic-first"]);
         assert (first_result.tool_call_id = Some "dynamic-first");
         assert (match first_result.content with
           | Some text -> String.starts_with ~prefix:"struct App {}\n" text
           | None -> false);
         assert (List.map (fun (call : Pave.Protocol.tool_call) -> call.id)
           stale.tool_calls = ["dynamic-stale"]);
         assert (List.map (fun (call : Pave.Protocol.tool_call) -> call.name)
           stale.tool_calls = ["write_file"]);
         assert (stale_result.tool_call_id = Some "dynamic-stale");
         assert (match stale_result.content with
           | Some text -> String.starts_with ~prefix:"Error: tool is no longer available" text
           | None -> false);
         assert (final.role = "assistant")
     | _ -> failwith "dynamic tool turn did not preserve call/result ordering");
    assert (not (Sys.file_exists (Filename.concat root "MUST_NOT_EXIST")));
    (match List.rev !dynamic_events with
     | [ Pave.Agent.Tool_started { call_id = "dynamic-first"; name = "read_file" };
         Pave.Agent.Tool_settled {
           call_id = "dynamic-first"; is_error = false; _
         };
         Pave.Agent.Tool_started { call_id = "dynamic-stale"; name = "write_file" };
         Pave.Agent.Tool_settled {
           call_id = "dynamic-stale"; is_error = true; _
         } ] -> ()
     | _ -> failwith "dynamic tool availability was not checked at dispatch");
    let approval_count = ref 0 in
    let invalid_shell_agent = Pave.Agent.create ~provider ~root
      ~system:"inspect the mobile repo" ~stream:true ~allow_shell:true
      ~on_event:(fun _ -> ())
      ~approve_command:(fun _ -> incr approval_count; true) () in
    assert (Pave.Agent.run invalid_shell_agent "Reject invalid shell arguments"
      = "Invalid arguments rejected before approval.");
    assert (!approval_count = 0);
    assert (not (Sys.file_exists (Filename.concat root "SHOULD_NOT_EXIST")));
    (match Pave.Agent.messages invalid_shell_agent with
     | [ user; assistant; result; final ] ->
         assert (user.role = "user");
         assert (List.map (fun (call : Pave.Protocol.tool_call) -> call.id)
           assistant.tool_calls = ["invalid-shell"]);
         assert (result.tool_call_id = Some "invalid-shell");
         assert (match result.content with
           | Some text -> String.starts_with ~prefix:"Error: unexpected argument" text
           | None -> false);
         assert (result.tool_result_content =
           Some [Pave.Protocol.Text (Option.get result.content)]);
         assert (final.role = "assistant")
     | _ -> failwith "invalid tool arguments reached approval or execution");
    let parallel_events = ref [] in
    let parallel_agent = Pave.Agent.create ~provider ~root
      ~system:"inspect the mobile repo" ~stream:true ~on_event:(fun _ -> ())
      ~on_tool_event:(fun event -> parallel_events := event :: !parallel_events) () in
    assert (Pave.Agent.run parallel_agent "Read the workspace twice"
      = "Parallel read results stayed ordered.");
    (match Pave.Agent.messages parallel_agent with
     | [ user; assistant; first; second; final ] ->
         assert (user.role = "user");
         assert (List.map (fun (call : Pave.Protocol.tool_call) -> call.id)
           assistant.tool_calls = ["parallel-first"; "parallel-second"]);
         assert (first.tool_call_id = Some "parallel-first");
         assert (second.tool_call_id = Some "parallel-second");
         assert (first.tool_result_content =
           Some [Pave.Protocol.Text (Option.get first.content)]);
         assert (second.tool_result_content =
           Some [Pave.Protocol.Text (Option.get second.content)]);
         assert (final.role = "assistant")
     | _ -> failwith "parallel tool results did not preserve provider order");
    (match List.rev !parallel_events with
     | [ Pave.Agent.Tool_started { call_id = "parallel-first"; name = "read_file" };
         Pave.Agent.Tool_started { call_id = "parallel-second"; name = "read_file" };
         Pave.Agent.Tool_settled {
           call_id = "parallel-first"; is_error = false; _
         };
         Pave.Agent.Tool_settled {
           call_id = "parallel-second"; is_error = false; _
         } ] -> ()
     | _ -> failwith "shared tool lifecycle events lost provider order")
  );
  print_endline "streamed agent loop: ok"
