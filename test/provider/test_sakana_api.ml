let member = Pave.Protocol.member
module Gateway = Pave.Sakana_api

let test_listing () =
  let open Gateway in
  let calls = ref 0 in
  let http ~url ~headers =
    incr calls;
    assert (url = sakana_models_url);
    assert (headers = ["Authorization", "Bearer secret-for-sakana"]);
    Ok (200, {|{"data":[{"id":"account-enabled-model"},{"id":"other-model"},{"id":"chat-model"}]}|}) in
  assert (discover_sakana ~http ~credential:"secret-for-sakana" () =
    Ok ["account-enabled-model"; "other-model"; "chat-model"]);
  assert (!calls = 1);
  assert (discover_sakana ~http ~credential:"bad\nInjected: value" () =
    Error Invalid_credential);
  assert (!calls = 1);
  assert (discover_sakana ~http:(fun ~url:_ ~headers:_ ->
    Ok (200, {|{"data":[{"id":"account-enabled-model"},{"id":""}]}|}))
    ~credential:"secret-for-sakana" () =
    Error (Invalid_response "invalid model ID"));
  assert (discover_sakana ~http:(fun ~url:_ ~headers:_ ->
    Ok (200, {|{"data":[{"id":"same-model"},{"id":"same-model"}]}|}))
    ~credential:"secret-for-sakana" () =
    Error (Invalid_response "invalid model ID"));
  assert (discover_sakana ~http:(fun ~url:_ ~headers:_ ->
    Ok (200, String.make (max_response_bytes + 1) 'x'))
    ~credential:"secret-for-sakana" () =
    Error (Invalid_response "listing exceeds size limit"));
  assert (discover_sakana ~http:(fun ~url:_ ~headers:_ ->
    Ok (403, "not authorized")) ~credential:"secret-for-sakana" () =
    Error (Http_error 403))

let request ic =
  let request_line = input_line ic in
  let headers = ref [] and length = ref 0 in
  let rec read_headers () =
    let line = input_line ic in
    if line <> "\r" && line <> "" then (
      let lower = String.lowercase_ascii line in
      headers := lower :: !headers;
      if String.starts_with ~prefix:"content-length:" lower then
        length := int_of_string (String.trim
          (String.sub line 15 (String.length line - 15)));
      read_headers ()) in
  read_headers ();
  let path = match String.split_on_char ' ' request_line with
    | "POST" :: path :: _ -> path
    | _ -> failwith "expected POST request" in
  path, !headers, Yojson.Basic.from_string (really_input_string ic !length)

let serve socket step =
  let client, _ = Unix.accept socket in
  let ic = Unix.in_channel_of_descr client in
  let oc = Unix.out_channel_of_descr client in
  let path, headers, body = request ic in
  assert (path = "/v1/responses");
  assert (List.mem "authorization: bearer secret-for-sakana\r" headers);
  assert (member "model" body = `String "account-enabled-model");
  let user = `Assoc ["role", `String "user"; "content", `List [
    `Assoc ["type", `String "input_text"; "text", `String "look up detail"]]] in
  let tool = `Assoc ["type", `String "function"; "name", `String "lookup";
    "parameters", `Assoc ["type", `String "object"]; "strict", `Bool false] in
  assert (member "tools" body = `List [tool]);
  let call = `Assoc ["type", `String "function_call";
    "call_id", `String "call-from-sakana"; "name", `String "lookup";
    "arguments", `String {|{"query":"detail"}|}] in
  let response = if step = 0 then (
    assert (member "input" body = `List [user]);
    `Assoc ["status", `String "completed"; "output", `List [call]])
  else (
    (match member "input" body with
    | `List [first; assistant; result] ->
        assert (first = user);
        assert (assistant = call);
        assert (member "type" result = `String "function_call_output");
        assert (member "call_id" result = `String "call-from-sakana");
        assert (member "output" result = `String "found")
    | _ -> failwith "tool result not included in second request");
    `Assoc ["status", `String "completed"; "output", `List [
      `Assoc ["type", `String "message"; "role", `String "assistant";
        "content", `List [`Assoc ["type", `String "output_text";
          "text", `String "detail found"]]]]]) in
  let response = Yojson.Basic.to_string response in
  Printf.fprintf oc
    "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
    (String.length response) response;
  flush oc;
  close_in_noerr ic;
  close_out_noerr oc

let test_tool_round_trip () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 2;
  let port = match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    (try for step = 0 to 1 do serve socket step done with exn ->
      prerr_endline (Printexc.to_string exn); exit 2);
    exit 0);
  Unix.close socket;
  let completed = ref false in
  Fun.protect ~finally:(fun () ->
    if not !completed then (try Unix.kill child Sys.sigkill
      with Unix.Unix_error _ -> ());
    let _, status = Unix.waitpid [] child in
    if !completed then assert (status = Unix.WEXITED 0)) (fun () ->
    let config : Pave.Provider.config = {
      endpoint = Printf.sprintf "http://127.0.0.1:%d/v1/responses" port;
      api_key = "secret-for-sakana"; model = "account-enabled-model";
      api = Pave.Provider.Openai_responses } in
    let user = Pave.Protocol.user "look up detail" in
    let tools = [`Assoc ["type", `String "function";
      "function", `Assoc ["name", `String "lookup";
        "parameters", `Assoc ["type", `String "object"]]]] in
    let first = Pave.Provider.complete config [user] tools in
    assert (first.tool_calls = [{ Pave.Protocol.id = "call-from-sakana";
      name = "lookup"; arguments = `Assoc ["query", `String "detail"] }]);
    let second = Pave.Provider.complete config
      [user; first; Pave.Protocol.tool_result "call-from-sakana" "found"] tools in
    assert (second.content = Some "detail found");
    assert (second.tool_calls = []);
    completed := true)

let () =
  test_listing ();
  test_tool_round_trip ();
  print_endline "Sakana Responses and listing: ok"
