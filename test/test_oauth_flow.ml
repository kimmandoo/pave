module Flow = Pave.Oauth_flow

let expect_error f =
  match f () with
  | exception Flow.OAuth_error _ -> ()
  | _ -> failwith "expected OAuth rejection"

let contains_substring text word =
  let rec find i =
    i + String.length word <= String.length text &&
    (String.sub text i (String.length word) = word || find (i + 1))
  in find 0

let policy : Flow.policy = {
  client_id = "public-client";
  authorize_url = "https://login.example.test/authorize";
  token_url = "https://login.example.test/token";
  redirect_uri = "http://127.0.0.1:54546/callback";
  scopes = ["read"; "offline"];
  token_body = Flow.Form;
  extra_authorize_params = ["prompt", "consent"];
  extra_token_params = [];
  extra_token_headers = ["X-OAuth-App", "fixture"];
  refresh_url = None;
  refresh_body = None;
  extra_refresh_params = [];
  extra_refresh_headers = [];
  account_path = ["account"; "id"];
  metadata_paths = ["email", ["account"; "email"]];
  expiry_skew = 30.;
}

let callback auth code = auth.Flow.redirect_uri ^ "?code=" ^ code ^ "&state=" ^ auth.Flow.state

let port_available () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port = match Unix.getsockname socket with Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  Unix.close socket;
  port

let () =
  assert (Flow.pkce_challenge "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk" =
    "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM");
  let auth = Flow.start ~now:1000. policy in
  let other = Flow.start ~now:1000. policy in
  assert (auth.state <> other.state && auth.verifier <> other.verifier);
  assert (String.length auth.state >= 43 && String.length auth.verifier >= 43);
  assert (auth.challenge = Flow.pkce_challenge auth.verifier);
  assert (String.contains auth.url '?');
  assert (String.contains auth.url '%');
  assert (String.contains auth.url '&');
  assert (Flow.parse_callback ~now:1001. auth ~response:(callback auth "good-code") = "good-code");
  assert (Flow.parse_callback ~now:1001. auth ~response:("good-code#" ^ auth.state) = "good-code");
  expect_error (fun () -> Flow.parse_callback ~now:1001. auth ~response:"good-code");
  expect_error (fun () -> Flow.parse_callback ~now:1001. auth
    ~response:(policy.redirect_uri ^ "?code=good-code&state=incorrect"));
  expect_error (fun () -> Flow.parse_callback ~now:1001. auth
    ~response:("http://localhost:54546/callback?code=good-code&state=" ^ auth.state));
  expect_error (fun () -> Flow.parse_callback ~now:1001. auth
    ~response:("http://127.0.0.1:54546/else?code=good-code&state=" ^ auth.state));
  expect_error (fun () -> Flow.parse_callback ~now:1001. auth
    ~response:(policy.redirect_uri ^ "?error=access_denied&state=" ^ auth.state));
  expect_error (fun () -> Flow.parse_callback ~now:1001. auth
    ~response:(callback auth "good-code" ^ "&state=" ^ auth.state));
  expect_error (fun () -> Flow.parse_callback ~now:1300. auth ~response:(callback auth "good-code"));
  let calls = ref 0 in
  let send ~url ~headers ~body =
    incr calls;
    assert (url = policy.token_url);
    assert (List.mem ("Content-Type", "application/x-www-form-urlencoded") headers);
    assert (List.mem ("X-OAuth-App", "fixture") headers);
    if !calls = 1 then (
      assert (String.contains body '%');
      assert (String.contains body '&');
      assert (String.contains body '=');
      assert (String.length body > String.length auth.verifier);
      assert (List.mem ("code_verifier=" ^ auth.verifier) (String.split_on_char '&' body));
      assert (List.mem "code=good-code" (String.split_on_char '&' body));
      200, {|{"access_token":"access-one","refresh_token":"refresh-one","expires_in":600,"account":{"id":"acct","email":"a@example.test"}}|})
    else (
      assert (List.mem "refresh_token=refresh-one" (String.split_on_char '&' body));
      200, {|{"access_token":"access-two","refresh_token":"refresh-two","expires_in":300}|}) in
  let initial = Flow.exchange ~http:send ~now:1001. policy auth ~response:(callback auth "good-code") in
  assert (initial.access = "access-one" && initial.refresh = Some "refresh-one");
  assert (initial.expires_at = Some 1571.);
  assert (initial.account_id = Some "acct" && initial.metadata = ["email", "a@example.test"]);
  let rotated = Flow.refresh ~http:send ~now:1100. policy initial in
  assert (rotated.access = "access-two" && rotated.refresh = Some "refresh-two");
  assert (rotated.account_id = initial.account_id && rotated.metadata = initial.metadata);
  assert (rotated.expires_at = Some 1370. && !calls = 2);
  ignore (Flow.refresh ~now:1100. ~http:(fun ~url:_ ~headers ~body:_ ->
    assert (List.mem ("x-oauth-app", "refresh-app") headers);
    assert (not (List.mem ("X-OAuth-App", "fixture") headers));
    200, {|{"access_token":"access-three","expires_in":300}|})
    {policy with extra_refresh_headers = ["x-oauth-app", "refresh-app"]} initial);
  let ignored = ref false in
  let never ~url:_ ~headers:_ ~body:_ = ignored := true; 200, "{}" in
  expect_error (fun () -> Flow.exchange ~http:never ~now:1001. policy auth ~response:
    (policy.redirect_uri ^ "?code=bad&state=wrong"));
  assert (not !ignored);
  (match Flow.exchange ~http:(fun ~url:_ ~headers:_ ~body:_ -> 400,
      {|{"error":"invalid_grant","message":"sensitive authorization code"}|})
      ~now:1001. policy auth ~response:(callback auth "sensitive-code") with
  | exception Flow.OAuth_error message ->
      assert (not (contains_substring message "sensitive-code"));
      assert (not (contains_substring message "sensitive authorization code"))
  | _ -> failwith "expected denied grant");
  expect_error (fun () -> Flow.exchange ~http:(fun ~url:_ ~headers:_ ~body:_ ->
    200, {|{"refresh_token":"refresh-only"}|}) ~now:1001. policy auth ~response:(callback auth "good-code"));
  expect_error (fun () -> Flow.exchange ~http:(fun ~url:_ ~headers:_ ~body:_ ->
    200, {|{"access_token":"token","expires_in":0}|}) ~now:1001. policy auth ~response:(callback auth "good-code"));
  expect_error (fun () -> Flow.exchange ~http:(fun ~url:_ ~headers:_ ~body:_ ->
    200, {|{"access_token":"token","expires_in":null}|}) ~now:1001. policy auth
    ~response:(callback auth "good-code"));
  expect_error (fun () -> Flow.refresh ~http:never ~now:1100. policy
    {initial with refresh = None});
  assert (not !ignored);
  expect_error (fun () -> Flow.start {policy with
    authorize_url = "https://login.example.test/authorize?%73tate=attacker"});
  ignore (Flow.start ~now:1000.
    {policy with authorize_url = "https://login.example.test/authorize?audience=assistant"});
  let mock_local = {policy with token_url = "http://127.0.0.1:7777/token"} in
  expect_error (fun () -> Flow.start mock_local);
  ignore (Flow.start ~http:never ~now:1000. mock_local);
  expect_error (fun () -> Flow.start {policy with token_url = "http://evil.example.test/token"});
  expect_error (fun () -> Flow.exchange ~http:never ~now:1001.
    {policy with token_url = "http://evil.example.test/token"} auth ~response:(callback auth "good-code"));
  let anth = Flow.anthropic ~sdk_version:"test-supplied-version" () in
  assert (anth.client_id = "9d1c250a-e61b-44d9-88ed-5944d1962f5e");
  assert (anth.authorize_url = "https://claude.ai/oauth/authorize");
  assert (anth.token_url = "https://api.anthropic.com/v1/oauth/token");
  assert (List.mem "user:inference" anth.scopes);
  assert (anth.extra_token_params = ["state", "{state}"]);
  let anth_auth = Flow.start ~now:1000. anth in
  let anth_http ~url:_ ~headers ~body =
    assert (List.mem ("Content-Type", "application/json") headers);
    let json = Yojson.Basic.from_string body in
    assert (Yojson.Basic.Util.member "state" json = `String anth_auth.state);
    200, {|{"access_token":"anthropic-access","refresh_token":"anthropic-refresh","expires_in":3600,"organization":{"uuid":"org-fixed","name":"Old"}}|} in
  let anth_token = Flow.exchange ~http:anth_http ~now:1001. anth anth_auth
    ~response:(callback anth_auth "anth-code") in
  let anth_refreshed = Flow.refresh ~http:(fun ~url:_ ~headers ~body:_ ->
    assert (List.mem ("anthropic-beta", "oauth-2025-04-20") headers);
    assert (List.mem ("User-Agent", "anthropic-sdk-typescript/test-supplied-version userOAuthProvider") headers);
    200, {|{"access_token":"anthropic-rotated","expires_in":3600,"organization":{"uuid":"org-new","name":"New"}}|})
    ~now:1100. anth anth_token in
  assert (anth_refreshed.refresh = Some "anthropic-refresh");
  assert (List.assoc "org_id" anth_refreshed.metadata = "org-fixed");
  expect_error (fun () -> Flow.exchange ~http:(fun ~url:_ ~headers:_ ~body:_ ->
    200, {|{"access_token":"short-lived","expires_in":120}|})
    ~now:1001. anth anth_auth ~response:(callback anth_auth "anth-code"));
  let port = port_available () in
  let local = {policy with redirect_uri = Printf.sprintf "http://127.0.0.1:%d/callback" port} in
  let listening, server = Flow.listen_loopback ~ttl:2. local in
  let child = Unix.fork () in
  if child = 0 then (
    Unix.close server;
    let make_request host =
      let client = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.connect client (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
      let request = Printf.sprintf "GET /callback?code=listener-code&state=%s HTTP/1.1\r\nHost: %s\r\n\r\n"
        listening.state host in
      ignore (Unix.write_substring client request 0 (String.length request));
      let buffer = Bytes.create 256 in
      let count = Unix.read client buffer 0 (Bytes.length buffer) in
      let response = Bytes.sub_string buffer 0 count in
      Unix.close client;
      response in
    assert (String.starts_with ~prefix:"HTTP/1.1 400" (make_request "evil.example.test"));
    assert (String.starts_with ~prefix:"HTTP/1.1 200"
      (make_request (Printf.sprintf "127.0.0.1:%d" port)));
    exit 0);
  let code = Flow.await_callback listening server in
  assert (code = "listener-code");
  ignore (Unix.waitpid [] child);
  let timed, server = Flow.listen_loopback ~ttl:0.05 local in
  expect_error (fun () -> Flow.await_callback timed server);
  print_endline "oauth flow: ok"
