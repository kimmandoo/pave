let event data = "data: " ^ data ^ "\n\n"
let tool_call = event
  {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"call-mobile","function":{"name":"read_file","arguments":"{\"path\":\"App.swift\"}"}}]},"finish_reason":null}]}|}
  ^ event {|{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}]}|}
  ^ event "[DONE]"
let answer = event
  {|{"choices":[{"index":0,"delta":{"content":"Swift source verified."},"finish_reason":"stop"}]}|}
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
       if step = 0 then assert (results = [])
       else assert (List.exists (fun item ->
         Pave.Protocol.member "tool_call_id" item = `String "call-mobile" &&
         Pave.Protocol.member "content" item = `String "struct App {}\n") results)
   | _ -> failwith "missing messages");
  let body = if step = 0 then tool_call else answer in
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
    (try for step = 0 to 1 do
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
    assert (List.length (Pave.Agent.messages agent) = 4));
  print_endline "streamed agent loop: ok"
