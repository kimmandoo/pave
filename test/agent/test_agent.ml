let tool_reply = {|{"choices":[{"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"tool_calls":[{"id":"call-1","type":"function","function":{"name":"read_file","arguments":"{\"path\":\"App.swift\"}"}}]}}]}|}
let final_reply = {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"Swift file inspected.","tool_calls":[]}}]}|}
let shell_reply = {|{"choices":[{"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"tool_calls":[{"id":"call-2","type":"function","function":{"name":"run_command","arguments":"{\"command\":\"touch MUST_NOT_EXIST\"}"}}]}}]}|}

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
  let tools = Pave.Protocol.member "tools" request in
  (match tools with
   | `List schemas ->
       let has_shell = List.exists (fun schema ->
         Pave.Protocol.member "name" (Pave.Protocol.member "function" schema)
         = `String "run_command") schemas in
       assert (has_shell = (step >= 2))
   | _ -> failwith "missing tools");
  (match Pave.Protocol.member "messages" request with
   | `List messages when step = 1 ->
       assert (List.exists (fun msg ->
         Pave.Protocol.member "tool_call_id" msg = `String "call-1"
         && (match Pave.Protocol.member "content" msg with
            | `String text -> String.starts_with ~prefix:"struct App {}\n" text
            | _ -> false)) messages)
   | `List messages when step = 3 ->
       assert (List.exists (fun msg ->
         Pave.Protocol.member "tool_call_id" msg = `String "call-2"
         && (match Pave.Protocol.member "content" msg with
             | `String text -> String.starts_with ~prefix:"Error:" text
             | _ -> false)) messages)
   | _ -> ());
  let body = match step with
    | 0 -> tool_reply | 2 -> shell_reply | _ -> final_reply in
  Printf.fprintf oc "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s" (String.length body) body;
  flush oc;
  close_in_noerr ic; close_out_noerr oc

let () =
  let root = Filename.temp_file "pave-agent-" "" in
  Sys.remove root; Unix.mkdir root 0o700;
  let file = Filename.concat root "App.swift" in
  let oc = open_out file in output_string oc "struct App {}\n"; close_out oc;
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 4;
  let port = match Unix.getsockname socket with Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    (try for step = 0 to 3 do
      let client, _ = Unix.accept socket in serve client step
    done with exn -> prerr_endline (Printexc.to_string exn); exit 2);
    exit 0);
  Unix.close socket;
  Fun.protect ~finally:(fun () ->
    (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
    ignore (Unix.waitpid [] child);
    Sys.remove file; Unix.rmdir root) (fun () ->
    let provider : Pave.Provider.config = {
      endpoint = Printf.sprintf "http://127.0.0.1:%d/v1/chat/completions" port;
      api_key = "test-key"; model = "mock"; api = Pave.Provider.Openai_completions } in
    let events = ref [] in
    let agent = Pave.Agent.create ~provider ~root ~system:"mobile agent"
      ~on_event:(fun message -> events := message :: !events) () in
    assert (Pave.Agent.run agent "Inspect App.swift" = "Swift file inspected.");
    let messages = Pave.Agent.messages agent in
    assert (List.length messages = 4);
    assert (List.exists (fun msg -> msg.Pave.Protocol.tool_call_id = Some "call-1") messages);
    assert (List.mem "Swift file inspected." !events);
    let shell_agent = Pave.Agent.create ~provider ~root ~system:"mobile agent"
      ~allow_shell:true ~approve_command:(fun _ -> false)
      ~on_event:(fun _ -> ()) () in
    assert (Pave.Agent.run shell_agent "Run test" = "Swift file inspected.");
    assert (not (Sys.file_exists (Filename.concat root "MUST_NOT_EXIST"))));
  print_endline "agent loop: ok"
