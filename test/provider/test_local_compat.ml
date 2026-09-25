module Local = Pave.Local_compat
module Provider = Pave.Provider
module Protocol = Pave.Protocol

let member = Protocol.member

let invalid_endpoint endpoint =
  match Provider.local_endpoint endpoint with
  | exception Provider.Provider_error _ -> ()
  | _ -> failwith ("unsafe local endpoint accepted: " ^ endpoint)

let read_request ic =
  let request_line = input_line ic in
  let size = ref 0 and headers = ref [] in
  let rec read_headers () =
    let line = input_line ic in
    if line <> "\r" && line <> "" then (
      let line = String.trim line in
      let lowered = String.lowercase_ascii line in
      headers := lowered :: !headers;
      if String.starts_with ~prefix:"content-length:" lowered then
        size := int_of_string (String.trim (String.sub line 15
          (String.length line - 15)));
      read_headers ()) in
  read_headers ();
  let body = really_input_string ic !size in
  let method_, path = match String.split_on_char ' ' request_line with
    | method_ :: path :: _ -> method_, path
    | _ -> failwith "invalid HTTP request" in
  method_, path, !headers, body

let respond ?(content_type = "application/json") oc status body extra_headers =
  Printf.fprintf oc "HTTP/1.1 %d Fixture\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n%s\r\n%s"
    status content_type (String.length body) extra_headers body;
  flush oc

let has_header prefix headers = List.exists (String.starts_with ~prefix) headers

let providers = ["lm-studio"; "llama.cpp"; "vllm"]

let serve socket =
  List.iter (fun provider ->
    for step = 0 to 5 do
      let client, _ = Unix.accept socket in
      let ic = Unix.in_channel_of_descr client in
      let oc = Unix.out_channel_of_descr client in
      let method_, path, headers, body = read_request ic in
      let key = "local-" ^ provider in
      if step = 0 || step = 4 then (
        assert (method_ = "GET" && path = "/v1/models");
        assert (has_header ("authorization: bearer " ^ key) headers);
        if step = 0 then
          respond oc 200
            {|{"data":[{"id":"discovered/local-model"},{"id":"discovered/local-model"},{"id":"next/chat"}]}|} ""
        else respond oc 302 "" "Location: http://127.0.0.1:1/stolen\r\n")
      else (
        assert (method_ = "POST" && path = "/v1/chat/completions");
        assert (has_header "content-type: application/json" headers);
        assert (has_header ("authorization: bearer " ^ key) headers = (step <> 3));
        let json = Yojson.Basic.from_string body in
        assert (member "model" json = `String "discovered/local-model");
        (match step with
        | 1 ->
            assert (member "stream" json = `Bool true);
            assert (member "tools" json <> `Null);
            assert (member "messages" json = `List [Protocol.message_to_json
              (Protocol.user "lookup")]);
            let chunk =
              "data: {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"local-call\",\"type\":\"function\",\"function\":{\"name\":\"lookup\",\"arguments\":\"{\\\"query\\\":\\\"hello\\\"}\"}}]},\"finish_reason\":null}]}\n\n" ^
              "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}]}\n\n" ^
              "data: [DONE]\n\n" in
            respond ~content_type:"text/event-stream" oc 200 chunk ""
        | 2 ->
            (match member "messages" json with
            | `List [user; assistant; tool] ->
                assert (user = Protocol.message_to_json (Protocol.user "lookup"));
                assert (member "tool_calls" assistant <> `Null);
                assert (member "role" tool = `String "tool");
                assert (member "tool_call_id" tool = `String "local-call");
                assert (member "content" tool = `String "found")
            | _ -> failwith "local tool-result turn was not replayed");
            respond oc 200
              {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"local reply"}}]}|} ""
        | 3 ->
            assert (member "stream" json = `Null);
            respond oc 200
              {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"keyless reply"}}]}|} ""
        | 5 ->
            respond oc 302 "" "Location: http://127.0.0.1:1/stolen\r\n"
        | _ -> assert false));
      close_in_noerr ic;
      close_out_noerr oc
    done) providers

let () =
  assert (Local.endpoint ~provider:"lm-studio" ~base_url:"http://localhost:1234/v1" () =
    "http://127.0.0.1:1234/v1/chat/completions");
  assert (Local.endpoint ~provider:"llama.cpp" ~base_url:"http://127.0.0.1:8080" () =
    "http://127.0.0.1:8080/v1/chat/completions");
  assert (Local.endpoint ~provider:"vllm" ~base_url:"http://[::1]:8000/v1" () =
    "http://[::1]:8000/v1/chat/completions");
  assert (Local.listing_url ~endpoint:"http://[::1]:8000/v1/chat/completions" =
    "http://[::1]:8000/v1/models");
  assert (Provider.local_endpoint "http://192.168.1.9:8000/v1/chat/completions" =
    "http://192.168.1.9:8000/v1/chat/completions");
  assert (Provider.local_endpoint "http://[fd00::1]:8000/v1/chat/completions" =
    "http://[fd00::1]:8000/v1/chat/completions");
  List.iter invalid_endpoint [
    "https://api.openai.com/v1/chat/completions";
    "http://localhost.evil.test/v1/chat/completions";
    "http://127.0.0.1@attacker.test/v1/chat/completions";
    "http://127.0.0.1:0/v1/chat/completions";
    "http://127.0.0.1:70000/v1/chat/completions";
    "http://127.0.0.01:8000/v1/chat/completions";
    "http://127.0.0.1:8000/v1/chat/completions?redirect=evil";
    "http://127.0.0.1:8000/v1/../chat/completions";
    "http://127.0.0.1:8000/v1/%2f/chat/completions";
    "http://8.8.8.8:8000/v1/chat/completions";
    "http://[2606:4700::1111]:8000/v1/chat/completions";
    "http://[::ffff:8.8.8.8]:8000/v1/chat/completions";
    "http://127.0.0.1:8000/v1/responses" ];
  invalid_endpoint ("http://127.0.0.1:8000/" ^
    String.make 2050 'a' ^ "/chat/completions");
  let contacted = ref false in
  let http ~url:_ ~headers:_ =
    contacted := true; Ok (200, {|{"data":[]}|}) in
  (match Local.discover ~http ~provider:"lm-studio"
    ~endpoint:"http://attacker.example/v1/chat/completions"
    ~key:"private-local-key" () with
   | Error Local.Invalid_endpoint -> assert (not !contacted)
   | _ -> failwith "discovery sent credentials to a remote host");
  (match Local.discover ~http ~provider:"vllm"
    ~endpoint:"http://127.0.0.1:8000/v1/chat/completions"
    ~key:"bad\nkey" () with
   | Error Local.Invalid_credential -> assert (not !contacted)
   | _ -> failwith "discovery accepted a malformed local key");
  let local_http ~url ~headers =
    assert (url = "http://127.0.0.1:8000/v1/models");
    assert (headers = []);
    Ok (200, {|{"data":[{"id":"first/chat"},{"id":"first/chat"},{"id":"second/chat"}]}|}) in
  assert (Local.discover ~http:local_http ~provider:"vllm"
    ~endpoint:"http://localhost:8000/v1/chat/completions" () =
    Ok ["first/chat"; "second/chat"]);
  let sent = ref false in
  let config : Provider.config = {
    api = Provider.Local_chat;
    endpoint = "http://attacker.example/v1/chat/completions";
    api_key = "private-local-key";
    model = "discovered/local-model" } in
  (match Provider.complete ~resolve_credential:(fun () -> sent := true;
      { Provider.access = "oauth-secret"; account_id = None; residency = None })
    config [Protocol.user "lookup"] [] with
   | exception Provider.Provider_error _ -> assert (not !sent)
   | _ -> failwith "unsafe host accepted");
  (match Provider.complete ~authentication:Provider.OAuth
    ~resolve_credential:(fun () -> sent := true;
      { Provider.access = "oauth-secret"; account_id = None; residency = None })
    { config with endpoint = "http://127.0.0.1:8000/v1/chat/completions" }
    [Protocol.user "lookup"] [] with
   | exception Provider.Provider_error _ -> assert (not !sent)
   | _ -> failwith "OAuth accepted by local Chat Completions");
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 8;
  let port = match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    try serve socket; exit 0 with exn ->
      prerr_endline (Printexc.to_string exn); exit 2);
  Unix.close socket;
  Fun.protect ~finally:(fun () ->
    (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
    match Unix.waitpid [] child with
    | _, Unix.WEXITED 0 -> ()
    | _ -> failwith "local fixture server failed") (fun () ->
      Unix.putenv "HTTP_PROXY" "http://127.0.0.1:1";
      Unix.putenv "ALL_PROXY" "http://127.0.0.1:1";
      List.iter (fun provider ->
        let endpoint = Printf.sprintf
          "http://127.0.0.1:%d/v1/chat/completions" port in
        let key = "local-" ^ provider in
        let config = { config with api_key = key; endpoint } in
        let models = Local.discover ~provider ~endpoint ~key () in
        assert (models = Ok ["discovered/local-model"; "next/chat"]);
        let tools = [`Assoc ["type", `String "function";
          "function", `Assoc ["name", `String "lookup";
            "parameters", `Assoc ["type", `String "object"]]]] in
        let emitted = ref [] in
        let first = Provider.complete ~on_text:(fun text -> emitted := text :: !emitted)
          config [Protocol.user "lookup"] tools in
        assert (!emitted = []);
        assert (first.tool_calls = [{ Protocol.id = "local-call"; name = "lookup";
          arguments = `Assoc ["query", `String "hello"] }]);
        let second = Provider.complete config
          [Protocol.user "lookup"; first; Protocol.tool_result "local-call" "found"]
          tools in
        assert (second.content = Some "local reply");
        let keyless = Provider.complete { config with api_key = "" }
          [Protocol.user "lookup"] [] in
        assert (keyless.content = Some "keyless reply");
        (match Local.discover ~provider ~endpoint ~key () with
        | Error (Local.Http_error 302) -> ()
        | _ -> failwith "redirected local listing was followed");
        (match Provider.complete config [Protocol.user "lookup"] [] with
        | exception Provider.Provider_error "HTTP 302" -> ()
        | _ -> failwith "redirected local completion was followed")) providers);
  print_endline "local Chat Completions and model discovery: ok"
