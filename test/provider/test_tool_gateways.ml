module Gateway = Pave.Tool_gateways
module Discovery = Pave.Model_discovery
let member = Pave.Protocol.member

let expect_models expected = function
  | Ok actual when actual = expected -> ()
  | Ok actual -> failwith ("wrong model list: " ^ String.concat ", " actual)
  | Error detail -> failwith detail

let invalid provider body =
  match Gateway.parse_models ~provider body with
  | Error _ -> ()
  | Ok _ -> failwith ("invalid " ^ provider ^ " model listing accepted")

let check_listing () =
  let aiml = {|{"object":"list","data":[{"id":"tool/a","type":"openai/chat-completions","capabilities":["tools","streaming"]},{"id":"image/a","type":"openai/image-generations","capabilities":[]},{"id":"chat/no-tools","type":"openai/chat-completions","capabilities":["streaming"]},{"id":"tool/a","type":"openai/chat-completions","capabilities":["tools"]},{"id":"tool/b","type":"openai/chat-completions","capabilities":["tools"]}]}|} in
  let aiand = {|{"object":"list","data":[{"id":"lab/chat-a","object":"model","capabilities":["reasoning","tool_calling"]},{"id":"lab/no-tools","object":"model","capabilities":["reasoning"]},{"id":"lab/chat-b","object":"model","capabilities":["tool_calling"]}]}|} in
  List.iter (fun (provider, response, expected) ->
    let spec = Option.get (Gateway.find provider) in
    let descriptor = Option.get (Pave.Provider_catalog.find spec.id) in
    assert (descriptor.api_key_env = Some spec.api_key_env);
    let route = Option.get (Pave.Provider_catalog.route descriptor "") in
    assert (route.endpoint = spec.chat_url);
    assert (route.wire = Pave.Provider.Openai_completions);
    let key = "private-" ^ provider in
    let calls = ref 0 in
    let http ~url ~headers =
      incr calls;
      assert (url = spec.models_url);
      assert (headers = ["Authorization", "Bearer " ^ key]);
      Ok (200, response) in
    let actual = Discovery.discover ~http ~provider
      ~credential:(Discovery.Api_key key) () in
    (match actual with
     | Ok actual when actual = expected -> ()
     | Ok _ -> failwith ("wrong discovered models for " ^ provider)
     | Error error -> failwith (Discovery.message error));
    assert (!calls = 1);
    let unused ~url:_ ~headers:_ = failwith "credential reached unsafe listing" in
    (match Discovery.discover ~http:unused ~provider () with
     | Error Discovery.Missing_credential -> ()
     | _ -> failwith "credentialless listing allowed");
    (match Discovery.discover ~http:unused ~provider
      ~credential:(Discovery.Copilot_oauth key) () with
     | Error Discovery.Invalid_credential -> ()
     | _ -> failwith "foreign OAuth token accepted");
    (match Discovery.discover ~http:unused ~provider
      ~credential:(Discovery.Api_key "bad\nheader") () with
     | Error Discovery.Invalid_credential -> ()
     | _ -> failwith "invalid credential accepted");
    let redirect ~url ~headers:_ =
      assert (url = spec.models_url);
      Ok (302, {|{"Location":"https://attacker.example/models"}|}) in
    (match Discovery.discover ~http:redirect ~provider
      ~credential:(Discovery.Api_key key) () with
     | Error (Discovery.Http_error 302) -> ()
     | _ -> failwith "followed untrusted listing redirect");
    expect_models expected (Gateway.parse_models ~provider response)) [
      "aimlapi", aiml, ["tool/a"; "tool/b"];
      "aiand", aiand, ["lab/chat-a"; "lab/chat-b"] ];
  invalid "aimlapi" {|{"object":"list","data":[{"id":"unknown","type":"openai/chat-completions"}]}|};
  invalid "aimlapi" {|{"object":"list","data":[{"id":"unknown","type":"openai/chat-completions","capabilities":[true]}]}|};
  invalid "aimlapi" {|{"object":"list","data":[{"id":"unknown","capabilities":["tools"]}]}|};
  invalid "aiand" {|{"object":"list","data":[{"id":"unknown","capabilities":null}]}|};
  assert (Gateway.find "gmi-cloud" = None);
  invalid "aiand" {|{"object":"list","data":[{"id":"bad\nmodel","capabilities":["tool_calling"]}]}|};
  invalid "aiand" {|{"data":[{"id":"unknown","capabilities":["tool_calling"]}]}|};
  invalid "aiand" {|{"object":"list","data":{}}|};
  invalid "aiand" {|{"object":"list","data":[{}]}|};
  invalid "aiand" (String.make (1_048_576 + 1) 'x');
  invalid "aimlapi" (String.make (4 * 1_048_576 + 1) 'x');
  let oversized_rows =
    {|{"object":"list","data":[|} ^
    String.concat "," (List.init 4097 (fun _ ->
      {|{"id":"another","capabilities":["tool_calling"]}|})) ^ "]}" in
  invalid "aiand" oversized_rows;
  invalid "gmi-cloud" aiand

let request ic =
  let first_line = input_line ic in
  let headers = ref [] and length = ref 0 in
  let rec read_headers () =
    match input_line ic with
    | "" | "\r" -> ()
    | line ->
        let lower = String.lowercase_ascii line in
        headers := lower :: !headers;
        if String.starts_with ~prefix:"content-length:" lower then
          length := int_of_string (String.trim
            (String.sub line 15 (String.length line - 15)));
        read_headers () in
  read_headers ();
  assert (String.starts_with ~prefix:"POST /chat/completions HTTP/1." first_line);
  !headers, Yojson.Basic.from_string (really_input_string ic !length)

let has_header prefix headers = List.exists
  (String.starts_with ~prefix) headers

let serve socket () =
  List.iter (fun (spec : Gateway.spec) ->
    for turn = 0 to 1 do
      let client, _ = Unix.accept socket in
      let ic = Unix.in_channel_of_descr client in
      let oc = Unix.out_channel_of_descr client in
      let headers, body = request ic in
      assert (has_header ("authorization: bearer private-" ^ spec.Gateway.id) headers);
      assert (member "model" body = `String "discovered-live-model");
      let user = Pave.Protocol.message_to_json (Pave.Protocol.user "look up detail") in
      let call = { Pave.Protocol.id = "call-" ^ spec.id;
        name = "lookup"; arguments = `Assoc ["query", `String "detail"] } in
      if turn = 0 then (
        assert (member "messages" body = `List [user]);
        assert (member "tools" body = `List [`Assoc [
          "type", `String "function";
          "function", `Assoc ["name", `String "lookup";
            "parameters", `Assoc ["type", `String "object"]]]]))
      else (match member "messages" body with
        | `List [first; assistant; tool] ->
            assert (first = user);
            assert (member "role" assistant = `String "assistant");
            assert (member "tool_calls" assistant =
              `List [Pave.Protocol.call_to_json call]);
            assert (member "role" tool = `String "tool");
            assert (member "tool_call_id" tool = `String call.id);
            assert (member "content" tool = `String "found")
        | _ -> failwith "tool result not replayed into second HTTP turn");
      let response =
        if turn = 0 then `Assoc ["choices", `List [`Assoc [
          "finish_reason", `String "tool_calls";
          "message", `Assoc ["role", `String "assistant";
            "content", `Null;
            "tool_calls", `List [Pave.Protocol.call_to_json call]]]]]
        else `Assoc ["choices", `List [`Assoc [
          "finish_reason", `String "stop";
          "message", `Assoc ["role", `String "assistant";
            "content", `String (spec.id ^ "-reply")]]]] in
      let response = Yojson.Basic.to_string response in
      Printf.fprintf oc
        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
        (String.length response) response;
      flush oc;
      close_in_noerr ic;
      close_out_noerr oc
    done) Gateway.all

let check_http () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 8;
  let port = match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    try serve socket (); Unix.close socket; exit 0
    with error -> prerr_endline (Printexc.to_string error); exit 1);
  Unix.close socket;
  let finished = ref false in
  Fun.protect ~finally:(fun () ->
    if not !finished then Unix.kill child Sys.sigterm;
    let _, result = Unix.waitpid [] child in
    if !finished then assert (result = Unix.WEXITED 0)) (fun () ->
    List.iter (fun (spec : Gateway.spec) ->
      let config : Pave.Provider.config = {
        endpoint = Printf.sprintf "http://127.0.0.1:%d/chat/completions" port;
        api_key = "private-" ^ spec.id;
        model = "discovered-live-model";
        api = Pave.Provider.Openai_completions } in
      let tools = [`Assoc ["type", `String "function";
        "function", `Assoc ["name", `String "lookup";
          "parameters", `Assoc ["type", `String "object"]]]] in
      let user = Pave.Protocol.user "look up detail" in
      let first = Pave.Provider.complete config [user] tools in
      assert (first.tool_calls = [{ Pave.Protocol.id = "call-" ^ spec.id;
        name = "lookup"; arguments = `Assoc ["query", `String "detail"] }]);
      let second = Pave.Provider.complete config
        [user; first; Pave.Protocol.tool_result ("call-" ^ spec.id) "found"] tools in
      assert (second.content = Some (spec.id ^ "-reply"));
      assert (second.tool_calls = [])) Gateway.all;
    finished := true)

let () =
  check_listing ();
  check_http ();
  print_endline "tool-capable gateways: ok"
