module Flow = Pave.Oauth_flow
module OAuth = Pave.Devin_oauth

let reject f = match f () with
  | exception Flow.OAuth_error _ -> ()
  | _ -> failwith "unsafe Devin grant accepted"

let () =
  let auth = OAuth.start ~now:1000. () in
  let uri = Flow.parse_uri auth.url in
  let params = Flow.query_params uri.query in
  assert (uri.scheme = "https" && uri.authority = "app.devin.ai");
  assert (uri.path = "/auth/cli/continue");
  assert (auth.redirect_uri = "http://127.0.0.1:59653/callback");
  assert (String.length auth.state = 36 && auth.state.[14] = '4');
  assert (List.assoc "response_type" params = "code");
  assert (List.assoc "redirect_uri" params = auth.redirect_uri);
  assert (List.assoc "state" params = auth.state);
  assert (List.assoc "code_challenge" params = Flow.pkce_challenge auth.verifier);
  assert (List.assoc "code_challenge_method" params = "S256");
  assert (List.assoc "prompt" params = "select_account");
  assert (not (List.mem_assoc "client_id" params));
  assert (not (List.mem_assoc "scope" params));
  let callback = auth.redirect_uri ^ "?code=fixture-code&state=" ^ auth.state in
  let sent = ref 0 in
  let send ~url ~headers ~body =
    incr sent;
    assert (url = "https://api.devin.ai/auth/cli/token");
    assert (headers = ["Content-Type", "application/json";
      "Accept", "application/json"]);
    assert (Yojson.Basic.from_string body = `Assoc [
      "code", `String "fixture-code";
      "code_verifier", `String auth.verifier]);
    200, {|{"token":"session-credential"}|} in
  let credential = OAuth.exchange ~http:send ~now:1001. auth ~response:callback in
  assert (credential.access = "session-credential");
  assert (credential.refresh = None && credential.expires_at = None);
  assert (credential.metadata = [] && credential.account_id = None);
  assert (Pave.Devin_api.session_key credential.access =
    "devin-session-token$session-credential");
  assert ((OAuth.exchange_code ~http:send ~now:1001. auth ~code:"fixture-code").access =
    credential.access);
  assert (!sent = 2);
  let reject_before_http response =
    let previous = !sent in
    reject (fun () -> OAuth.exchange ~http:send ~now:1001. auth ~response);
    assert (!sent = previous) in
  reject_before_http (auth.redirect_uri ^ "?code=fixture-code");
  reject_before_http (callback ^ "&state=duplicate");
  reject_before_http (auth.redirect_uri ^ "?code=fixture-code&state=wrong");
  reject_before_http "http://localhost:59653/callback?code=fixture-code&state=wrong";
  reject_before_http ("http://127.0.0.1:59654/callback?code=fixture-code&state=" ^ auth.state);
  reject_before_http ("https://attacker.example/callback?code=fixture-code&state=" ^ auth.state);
  reject_before_http ("/elsewhere?code=fixture-code&state=" ^ auth.state);
  reject_before_http "fixture-code";
  reject (fun () -> OAuth.exchange ~http:send ~now:1300. auth ~response:callback);
  assert (!sent = 2);
  let forged = { auth with url = "https://attacker.example/authorize" } in
  reject (fun () -> OAuth.exchange ~http:send ~now:1001. forged ~response:callback);
  let forged = { auth with challenge = "forged" } in
  reject (fun () -> OAuth.exchange ~http:send ~now:1001. forged ~response:callback);
  let forged = { auth with redirect_uri = "http://attacker.example/callback" } in
  reject (fun () -> OAuth.exchange ~http:send ~now:1001. forged ~response:callback);
  assert (!sent = 2);
  List.iter (fun response ->
    reject (fun () -> OAuth.exchange ~http:(fun ~url:_ ~headers:_ ~body:_ ->
      200, response) ~now:1001. auth ~response:callback))
    [{|{}|}; {|{"access_token":"not-a-Devin-token"}|};
      {|{"token":""}|}; {|{"token":null}|}; {|{"token":123}|};
      {|{"token":"bad\nheader"}|}; {|{"token":"devin-session-token$"}|};
      {|{"error":"invalid_grant","token":"session-credential"}|}];
  reject (fun () -> OAuth.exchange ~http:(fun ~url:_ ~headers:_ ~body:_ ->
    401, {|{"token":"session-credential"}|}) ~now:1001. auth ~response:callback);
  assert (OAuth.start ~now:1000. ~ttl:600. () |> fun grant ->
    grant.deadline = 1600.);
  reject (fun () -> OAuth.start ~now:1000. ~ttl:601. ());
  let browser_auth, listener = OAuth.listen_loopback ~ttl:10. () in
  let child = Unix.fork () in
  if child = 0 then (
    Unix.close listener;
    let request ~host ~state =
      let client = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect ~finally:(fun () -> Unix.close client) (fun () ->
        Unix.connect client (Unix.ADDR_INET (Unix.inet_addr_loopback, 59653));
        let request = "GET /callback?code=fixture-code&state=" ^ state ^
          " HTTP/1.1\r\nHost: " ^ host ^ "\r\n\r\n" in
        let rec send offset =
          if offset < String.length request then
            let n = Unix.write_substring client request offset
              (String.length request - offset) in
            if n <= 0 then failwith "callback socket closed";
            send (offset + n) in
        send 0;
        let bytes = Bytes.create 512 in
        let n = Unix.read client bytes 0 (Bytes.length bytes) in
        Bytes.sub_string bytes 0 n) in
    (try
       assert (String.starts_with ~prefix:"HTTP/1.1 400"
         (request ~host:"attacker.invalid" ~state:browser_auth.state));
       assert (String.starts_with ~prefix:"HTTP/1.1 400"
         (request ~host:"127.0.0.1:59653" ~state:"forged"));
       assert (String.starts_with ~prefix:"HTTP/1.1 200"
         (request ~host:"127.0.0.1:59653" ~state:browser_auth.state));
       exit 0
     with _ -> exit 2));
  let code = OAuth.await_callback browser_auth listener in
  let _, status = Unix.waitpid [] child in
  assert (status = Unix.WEXITED 0 && code = "fixture-code");
  let browser_credential = OAuth.exchange_code ~http:(fun ~url ~headers:_ ~body ->
    assert (url = OAuth.token_url);
    let fields = Yojson.Basic.from_string body in
    assert (Flow.member "code" fields = `String code);
    assert (Flow.member "code_verifier" fields = `String browser_auth.verifier);
    200, {|{"token":"devin-session-token$already"}|}) browser_auth ~code in
  assert (Pave.Devin_api.session_key browser_credential.access =
    "devin-session-token$already");
  print_endline "Devin PKCE session grant: ok"
