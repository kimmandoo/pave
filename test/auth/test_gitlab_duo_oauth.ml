module Flow = Pave.Oauth_flow
module Gitlab = Pave.Gitlab_duo_oauth

let expect_error f = match f () with
  | exception Flow.OAuth_error _ -> ()
  | _ -> failwith "expected GitLab OAuth rejection"

let with_config ~client ~redirect f =
  let prior_client = Sys.getenv_opt "GITLAB_CLIENT_ID" in
  let prior_redirect = Sys.getenv_opt "GITLAB_REDIRECT_URI" in
  let set name value = Unix.putenv name value in
  Fun.protect ~finally:(fun () ->
    set "GITLAB_CLIENT_ID" (Option.value prior_client ~default:"");
    set "GITLAB_REDIRECT_URI" (Option.value prior_redirect ~default:"")) (fun () ->
      set "GITLAB_CLIENT_ID" client;
      set "GITLAB_REDIRECT_URI" redirect;
      f ())

let parameters body = Flow.query_params body
let field name params = List.assoc name params

let () =
  with_config ~client:"" ~redirect:"" (fun () ->
    expect_error Gitlab.policy);
  with_config ~client:"user-client" ~redirect:"" (fun () ->
    expect_error Gitlab.policy);
  with_config ~client:"" ~redirect:"http://localhost:8080/callback" (fun () ->
    expect_error Gitlab.policy);
  List.iter (fun client ->
    with_config ~client ~redirect:"http://localhost:8080/callback" (fun () ->
      expect_error Gitlab.policy)) ["bad client"; "x\r\nHost:evil.test"; "a/b"];
  List.iter (fun redirect ->
    with_config ~client:"registered-client" ~redirect (fun () ->
      expect_error Gitlab.policy)) [
        "https://localhost:8080/callback";
        "http://evil.test:8080/callback";
        "http://localhost.evil.test:8080/callback";
        "http://localhost:8080@evil.test/callback";
        "http://localhost:0/callback";
        "http://localhost:8080/callback?next=https://evil.test";
        "http://localhost:8080//evil.test";
        "http://localhost:8080/callback#fragment";
        "http://localhost:8080/%2Fcallback";
        "http://localhost:8080";
      ];
  with_config ~client:"registered-client" ~redirect:"http://127.0.0.1:18080/oauth-cb" (fun () ->
    let policy = Gitlab.policy () in
    assert (policy.client_id = "registered-client");
    assert (policy.redirect_uri = "http://127.0.0.1:18080/oauth-cb");
    assert (policy.authorize_url = "https://gitlab.com/oauth/authorize");
    assert (policy.token_url = "https://gitlab.com/oauth/token");
    assert (policy.scopes = ["api"] && policy.token_body = Flow.Form);
    assert (policy.expiry_skew = 300.);
    let auth = Flow.start ~now:1_700_000_000. policy in
    let authorize = Flow.parse_uri auth.url in
    assert (authorize.scheme = "https" && authorize.authority = "gitlab.com");
    assert (authorize.path = "/oauth/authorize");
    let authorize_params = parameters authorize.query in
    assert (field "client_id" authorize_params = policy.client_id);
    assert (field "redirect_uri" authorize_params = policy.redirect_uri);
    assert (field "scope" authorize_params = "api");
    assert (field "response_type" authorize_params = "code");
    assert (field "state" authorize_params = auth.state);
    assert (field "code_challenge" authorize_params = auth.challenge);
    assert (field "code_challenge_method" authorize_params = "S256");
    assert (auth.challenge = Flow.pkce_challenge auth.verifier);
    let callback = auth.redirect_uri ^ "?code=gitlab-code&state=" ^ auth.state in
    let posts = ref 0 in
    let send ~url ~headers ~body =
      incr posts;
      assert (url = "https://gitlab.com/oauth/token");
      assert (headers = ["Content-Type", "application/x-www-form-urlencoded"]);
      let params = parameters body in
      assert (field "client_id" params = "registered-client");
      if !posts = 1 then (
        assert (field "grant_type" params = "authorization_code");
        assert (field "code" params = "gitlab-code");
        assert (field "code_verifier" params = auth.verifier);
        assert (field "redirect_uri" params = auth.redirect_uri);
        200, {|{"access_token":"access-one","refresh_token":"refresh-one","created_at":1700000000,"expires_in":3600}|})
      else (
        assert (field "grant_type" params = "refresh_token");
        assert (field "refresh_token" params = "refresh-one");
        assert (not (List.mem_assoc "redirect_uri" params));
        200, {|{"access_token":"access-two","expires_in":1800}|}) in
    let credential = Gitlab.exchange ~http:send ~now:1_700_000_001.
      auth ~response:callback in
    assert (!posts = 1);
    assert (credential.access = "access-one");
    assert (credential.refresh = Some "refresh-one");
    assert (credential.expires_at = Some 1_700_003_301.);
    assert (credential.metadata = ["gitlab_client_id", "registered-client"]);
    let refreshed = Gitlab.refresh ~http:send ~now:1_700_000_100. credential in
    assert (!posts = 2);
    assert (refreshed.access = "access-two");
    assert (refreshed.refresh = Some "refresh-one");
    assert (refreshed.expires_at = Some 1_700_001_600.);
    assert (refreshed.metadata = credential.metadata);
    let rotated = Gitlab.refresh ~http:(fun ~url ~headers:_ ~body ->
      assert (url = "https://gitlab.com/oauth/token");
      assert (field "refresh_token" (parameters body) = "refresh-one");
      200, {|{"access_token":"access-three","refresh_token":"refresh-two","expires_in":1800}|})
      ~now:1_700_000_200. refreshed in
    assert (rotated.refresh = Some "refresh-two");
    let never ~url:_ ~headers:_ ~body:_ = failwith "unexpected token request" in
    expect_error (fun () -> Gitlab.exchange ~http:never ~now:1_700_000_001.
      auth ~response:(auth.redirect_uri ^ "?code=gitlab-code&state=wrong"));
    expect_error (fun () -> Gitlab.exchange ~http:never ~now:1_700_000_001.
      {auth with url = "https://evil.test/oauth/authorize"} ~response:callback);
    expect_error (fun () -> Gitlab.exchange ~http:never ~now:1_700_000_001.
      {auth with challenge = "tampered"} ~response:callback);
    expect_error (fun () -> Gitlab.exchange ~http:never ~now:1_700_000_001.
      {auth with verifier = "tampered"} ~response:callback);
    expect_error (fun () -> Gitlab.exchange ~http:never ~now:1_700_000_300.
      auth ~response:callback);
    expect_error (fun () -> Gitlab.refresh ~http:never ~now:1_700_000_200.
      {credential with metadata = []});
    expect_error (fun () -> Gitlab.refresh ~http:never ~now:1_700_000_200.
      {credential with refresh = None});
    Unix.putenv "GITLAB_CLIENT_ID" "different-client";
    expect_error (fun () -> Gitlab.exchange ~http:never ~now:1_700_000_001.
      auth ~response:callback);
    expect_error (fun () -> Gitlab.refresh ~http:never ~now:1_700_000_200. credential);
    Unix.putenv "GITLAB_CLIENT_ID" "registered-client";
    Unix.putenv "GITLAB_REDIRECT_URI" "http://127.0.0.1:18081/oauth-cb";
    expect_error (fun () -> Gitlab.exchange ~http:never ~now:1_700_000_001.
      auth ~response:callback));
  print_endline "gitlab duo oauth: ok"
