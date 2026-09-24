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
  {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"shell-wait","function":{"name":"run_command","arguments":"{\"command\":\"sleep 5\",\"timeout_seconds\":20}"}}]},"finish_reason":null}]}|}
  ^ event {|{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}]}|}
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
  assert (Pave.Protocol.member "stream" request = `Bool true);
  (match Pave.Protocol.member "messages" request with
   | `List messages ->
       let results = List.filter (fun item -> Pave.Protocol.member "role" item = `String "tool") messages in
       if step <> 1 then assert (results = [])
       else assert (List.exists (fun item ->
         Pave.Protocol.member "tool_call_id" item = `String "call-mobile" &&
         (match Pave.Protocol.member "content" item with
          | `String text -> String.starts_with ~prefix:"struct App {}\n" text
          | _ -> false)) results)
   | _ -> failwith "missing messages");
  let body = match step with
    | 0 -> tool_call | 1 -> answer | 2 -> side_effect_call
    | 3 -> two_calls | _ -> shell_call in
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
    (try for step = 0 to 4 do
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
    let events = ref [] and deltas = ref [] in
    let agent = Pave.Agent.create ~provider ~root ~system:"inspect the mobile repo"
      ~stream:true ~on_event:(fun text -> events := text :: !events)
      ~on_delta:(fun text -> deltas := text :: !deltas) () in
    assert (Pave.Agent.run agent "Read App.swift" = "Swift source verified.");
    assert (List.rev !deltas = [ "Swift source verified."; "\n" ]);
    assert (not (List.mem "Swift source verified." !events));
    assert (List.length (Pave.Agent.messages agent) = 4);
    let cancelled = ref false in
    let changes = ref [] in
    let cancelled_agent = Pave.Agent.create ~provider ~root ~system:"inspect the mobile repo"
      ~stream:true
      ~on_event:(fun event -> if event = "[write_file]" then cancelled := true)
      ~on_change:(fun message -> changes := message :: !changes) () in
    (match Pave.Agent.run ~cancel:(fun () -> !cancelled) cancelled_agent "Write file" with
     | exception Pave.Provider.Cancelled -> ()
     | _ -> failwith "agent executed a cancelled tool call");
    assert (not (Sys.file_exists (Filename.concat root "MUST_NOT_EXIST")));
    (match Pave.Agent.messages cancelled_agent with
     | [ message ] -> assert (message.Pave.Protocol.role = "user")
     | _ -> failwith "cancelled agent journal contains an incomplete assistant turn");
    assert (List.rev !changes = Pave.Agent.messages cancelled_agent);
    let cancelled_after_tool = ref false in
    let partial_agent = Pave.Agent.create ~provider ~root ~system:"inspect the mobile repo"
      ~stream:true
      ~on_event:(fun event ->
        if String.starts_with ~prefix:"[read_file] struct App" event then
          cancelled_after_tool := true) () in
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
    assert (not (Sys.file_exists (Filename.concat root "MUST_NOT_EXIST")));
    let shell_started = ref None in
    let shell_agent = Pave.Agent.create ~provider ~root ~system:"inspect the mobile repo"
      ~stream:true ~allow_shell:true
      ~on_event:(fun _ -> ())
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
     | _ -> failwith "cancelled command did not settle the tool call"));
  print_endline "streamed agent loop: ok"
