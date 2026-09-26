let field name json = Pave.Protocol.member name json

let request_body client step =
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
  (match field "tools" request with
   | `Null | `List [] -> ()
   | _ -> failwith "summary request advertised tools");
  let messages = match field "messages" request with
    | `List messages -> messages
    | _ -> failwith "summary request omitted messages" in
  (match messages with
   | [_system; user] ->
       let payload = match field "content" user with
         | `String text -> Yojson.Basic.from_string text
         | _ -> failwith "summary request content was not JSON" in
       (match field "messages" payload with
        | `List [first; second] ->
            assert (field "role" first = `String "user");
            assert (field "role" second = `String "assistant");
            let first_content = field "content" first in
            assert (match first_content with
              | `String text -> String.length text = 1600
              | _ -> false);
            (match step, field "priorSummary" payload with
             | 0, `Null -> ()
             | 1, `String "summary-1" -> ()
             | _ -> failwith "summary chunks lost their ordered carry")
        | _ -> failwith "summary split a complete turn")
   | _ -> failwith "unexpected summary transcript shape");
  let summary = Printf.sprintf "summary-%d" (step + 1) in
  let body = Yojson.Basic.to_string (`Assoc [
    "choices", `List [`Assoc ["finish_reason", `String "stop";
      "message", `Assoc ["role", `String "assistant";
        "content", `String summary; "tool_calls", `List []]]];
    "usage", `Assoc ["prompt_tokens", `Int 12; "completion_tokens", `Int 3]
  ]) in
  Printf.fprintf oc
    "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
    (String.length body) body;
  flush oc;
  close_in_noerr ic;
  close_out_noerr oc

let () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 4;
  let port = match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    (try
       for step = 0 to 1 do
         let client, _ = Unix.accept socket in
         request_body client step
       done
     with exn -> prerr_endline (Printexc.to_string exn); exit 2);
    exit 0);
  Unix.close socket;
  let child_reaped = ref false in
  Fun.protect ~finally:(fun () ->
    if not !child_reaped then (
      (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
      ignore (Unix.waitpid [] child);
      child_reaped := true)) (fun () ->
      let provider : Pave.Provider.config = {
        endpoint = Printf.sprintf "http://127.0.0.1:%d/v1/chat/completions" port;
        api_key = "test-key"; model = "fixture";
        api = Pave.Provider.Openai_completions } in
      let turn user answer = [Pave.Protocol.user user; {
        role = "assistant"; content = Some answer; tool_calls = [];
        tool_call_id = None; tool_result_content = None; provider_state = None;
        attachments = [] }] in
      let source = turn (String.make 1600 'u') (String.make 200 'a') @
        turn (String.make 1600 'v') (String.make 200 'b') in
      let usages = ref [] in
      let summary = Pave.Context_compaction.summarize ~provider
        ~authentication:Pave.Provider.Api_key ~window_tokens:8192 source
        ~on_usage:(fun usage -> usages := usage :: !usages) in
      assert (summary = "summary-2");
      let _, status = Unix.waitpid [] child in
      child_reaped := true;
      assert (status = Unix.WEXITED 0);
      assert (List.map (fun (usage : Pave.Protocol.usage) ->
        usage.input_tokens, usage.output_tokens) (List.rev !usages) =
          [12, 3; 12, 3]);
      let oversized : Pave.Provider.config = {
        provider with endpoint = "http://127.0.0.1:1/v1/chat/completions" } in
      (match Pave.Context_compaction.summarize ~provider:oversized
        ~authentication:Pave.Provider.Api_key ~window_tokens:8192
        [Pave.Protocol.user (String.make 10_000 'x')]
        ~on_usage:ignore with
       | exception Failure reason when String.starts_with
           ~prefix:"one complete conversation turn exceeds" reason -> ()
       | exception exn -> raise exn
       | _ -> failwith "oversized single turn reached a provider");
      print_endline "context compaction: ok")
