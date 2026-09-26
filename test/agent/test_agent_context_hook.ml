let response_tool = {|{"choices":[{"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"tool_calls":[{"id":"call-1","type":"function","function":{"name":"read_file","arguments":"{\"path\":\"App.swift\"}"}}]}}]}|}
let response_final = {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"done","tool_calls":[]}}]}|}

let contains_substring needle text =
  let needle_length = String.length needle in
  let last_start = String.length text - needle_length in
  let rec find index =
    index <= last_start &&
    (String.sub text index needle_length = needle || find (index + 1)) in
  needle_length > 0 && find 0

let contains text json =
  let rec visit = function
    | `String value -> contains_substring text value
    | `List values -> List.exists visit values
    | `Assoc fields -> List.exists (fun (_, value) -> visit value) fields
    | _ -> false in
  visit json

let serve client step =
  let ic = Unix.in_channel_of_descr client in
  let oc = Unix.out_channel_of_descr client in
  let length = ref 0 in
  let rec headers () =
    let line = input_line ic in
    if line <> "\r" && line <> "" then (
      let lower = String.lowercase_ascii line in
      if String.starts_with ~prefix:"content-length:" lower then
        length := int_of_string (String.trim
          (String.sub line 15 (String.length line - 15)));
      headers ()) in
  headers ();
  let request = Yojson.Basic.from_string (really_input_string ic !length) in
  let messages = match Pave.Protocol.member "messages" request with
    | `List messages -> messages
    | _ -> failwith "provider request omitted messages" in
  (match step, messages with
   | 0, [system; summary; latest] ->
       assert (Pave.Protocol.member "role" system = `String "system");
       assert (Pave.Protocol.member "content" summary = `String "prior summary");
       assert (Pave.Protocol.member "role" latest = `String "user");
       assert (contains "data:image/png;base64,aGVsbG8=" latest)
   | 1, [_system; summary; latest; call; result] ->
       assert (Pave.Protocol.member "role" latest = `String "user");
       assert (contains "data:image/png;base64,aGVsbG8=" latest);
       assert (Pave.Protocol.member "content" summary = `String "prior summary");
       assert (Pave.Protocol.member "tool_calls" call <> `Null);
       assert (Pave.Protocol.member "tool_call_id" result = `String "call-1");
       assert (match Pave.Protocol.member "content" result with
         | `String text -> String.starts_with ~prefix:"struct App {}" text
         | _ -> false)
   | _ -> failwith "request history did not preserve its turn boundary");
  let body = if step = 0 then response_tool else response_final in
  Printf.fprintf oc
    "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
    (String.length body) body;
  flush oc;
  close_in_noerr ic;
  close_out_noerr oc

let () =
  let root = Filename.temp_file "pave-agent-context-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let file = Filename.concat root "App.swift" in
  let output = open_out file in
  output_string output "struct App {}\n";
  close_out output;
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 2;
  let port = match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    (try for step = 0 to 1 do
       let client, _ = Unix.accept socket in
       serve client step
     done with exn -> prerr_endline (Printexc.to_string exn); exit 2);
    exit 0);
  Unix.close socket;
  let reaped = ref false in
  Fun.protect ~finally:(fun () ->
    if not !reaped then (
      (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
      ignore (Unix.waitpid [] child));
    Sys.remove file;
    Unix.rmdir root) (fun () ->
      let provider : Pave.Provider.config = {
        endpoint = Printf.sprintf "http://127.0.0.1:%d/v1/chat/completions" port;
        api_key = "test-key"; model = "fixture";
        api = Pave.Provider.Openai_completions } in
      let attachment : Pave.Protocol.attachment = {
        name = "fixture.png"; mime_type = "image/png"; data = "aGVsbG8=" } in
      let prepared = ref 0 in
      let before_request ~cancel ~system:_ ~messages ~tools =
        assert (cancel = None);
        assert (List.exists (fun tool ->
          Pave.Protocol.member "name" (Pave.Protocol.member "function" tool)
          = `String "read_file") tools);
        incr prepared;
        match !prepared, messages with
        | 1, [_old_user; _old_answer; latest] ->
            assert (latest.Pave.Protocol.attachments = [attachment]);
            Some [Pave.Protocol.user "prior summary"; latest]
        | 2, [summary; latest; call; result] ->
            assert (summary.Pave.Protocol.content = Some "prior summary");
            assert (latest.Pave.Protocol.attachments = [attachment]);
            assert (call.Pave.Protocol.tool_calls <> []);
            assert (result.Pave.Protocol.tool_call_id = Some "call-1");
            None
        | _ -> failwith "request hook did not receive current ordered history" in
      let agent = Pave.Agent.create ~provider ~root ~system:"fixture"
        ~history:[Pave.Protocol.user "obsolete"; {
          role = "assistant"; content = Some "old answer"; tool_calls = [];
          tool_call_id = None; tool_result_content = None; provider_state = None;
          attachments = [] }]
        ~before_request ~on_event:ignore () in
      assert (Pave.Agent.run ~attachments:[attachment] agent "latest request" = "done");
      assert (!prepared = 2);
      assert (not (List.exists (fun (message : Pave.Protocol.message) ->
        message.content = Some "obsolete") (Pave.Agent.messages agent)));
      let _, status = Unix.waitpid [] child in
      reaped := true;
      assert (status = Unix.WEXITED 0);
      print_endline "agent context hook: ok")
