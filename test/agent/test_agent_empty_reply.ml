let response_empty = {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":""}}],"usage":{"prompt_tokens":11,"completion_tokens":3}}|}
let response_final = {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"done"}}]}|}

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
  let roles = match Pave.Protocol.member "messages" request with
    | `List messages -> List.map (Pave.Protocol.member "role") messages
    | _ -> failwith "provider request omitted messages" in
  (match step, roles with
   | 0, [`String "system"; `String "user"] -> ()
   | 1, [`String "system"; `String "user"; `String "user"] -> ()
   | _ -> failwith "empty assistant reply was replayed to the provider");
  let body = if step = 0 then response_empty else response_final in
  Printf.fprintf oc
    "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
    (String.length body) body;
  flush oc;
  close_in_noerr ic;
  close_out_noerr oc

let () =
  let root = Filename.temp_file "pave-agent-empty-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
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
    Unix.rmdir root) (fun () ->
      let provider : Pave.Provider.config = {
        endpoint = Printf.sprintf "http://127.0.0.1:%d/v1/chat/completions" port;
        api_key = "test-key"; model = "fixture";
        api = Pave.Provider.Openai_completions } in
      let usage = ref 0 and tools = ref 0 in
      let agent = Pave.Agent.create ~provider ~root ~system:"fixture"
        ~on_usage:(fun _ -> incr usage)
        ~on_tool_event:(fun _ -> incr tools) ~on_event:ignore () in
      (match Pave.Agent.run agent "first" with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> failwith "empty completion was accepted as success");
      assert (!usage = 0 && !tools = 0);
      assert (List.for_all (fun (message : Pave.Protocol.message) ->
        message.role <> "assistant") (Pave.Agent.messages agent));
      (* Anthropic and Responses serialization reject an empty assistant turn;
         the retained history must stay replayable on every route. *)
      ignore (Pave.Anthropic_wire.request ~model:"fixture" ~max_tokens:16
        (Pave.Agent.messages agent) []);
      ignore (Pave.Openai_responses_wire.request ~model:"fixture"
        (Pave.Agent.messages agent) []);
      assert (Pave.Agent.run agent "second" = "done");
      let _, status = Unix.waitpid [] child in
      reaped := true;
      assert (status = Unix.WEXITED 0);
      print_endline "agent empty reply: ok")
