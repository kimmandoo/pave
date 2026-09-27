module Flow = Pave.Oauth_flow
module Codex = Pave.Codex_oauth

let expect_error f =
  match f () with
  | exception Flow.OAuth_error _ -> ()
  | _ -> failwith "expected Codex account rejection"

let jwt_payload payload =
  Flow.base64url {|{"alg":"RS256","typ":"JWT"}|} ^ "." ^
  Flow.base64url payload ^ "." ^ Flow.base64url "signature"

let jwt claims = jwt_payload (Yojson.Basic.to_string claims)

let account_claim id =
  `Assoc ["https://api.openai.com/auth",
    `Assoc ["chatgpt_account_id", `String id]]

let without_account = jwt (`Assoc ["sub", `String "user-1"])
let with_account id = jwt (account_claim id)
let with_residency entries = jwt (`Assoc [
  "https://api.openai.com/auth", `Assoc (
    ("chatgpt_account_id", `String "workspace-1") :: entries) ])

let token_response ?id_token ?(refresh_token = "refresh-one") access =
  let fields = ["access_token", `String access;
    "refresh_token", `String refresh_token; "expires_in", `Int 600] in
  let fields = match id_token with
    | None -> fields
    | Some token -> ("id_token", `String token) :: fields in
  Yojson.Basic.to_string (`Assoc fields)

let policy = Codex.policy ()

let () =
  assert (policy.client_id = "app_EMoamEEZ73f0CkXaXp7hrann");
  assert (policy.authorize_url = "https://auth.openai.com/oauth/authorize");
  assert (policy.token_url = "https://auth.openai.com/oauth/token");
  assert (policy.redirect_uri = "http://localhost:1455/auth/callback");
  assert (policy.scopes = ["openid"; "profile"; "email"; "offline_access";
    "api.connectors.read"; "api.connectors.invoke"]);
  assert (policy.token_body = Flow.Form && policy.refresh_body = None);
  assert (policy.expiry_skew = 0.);
  let auth = Flow.start ~now:1000. policy in
  let params = Flow.query_params (Flow.parse_uri auth.url).query in
  let param key = List.assoc key params in
  assert (param "response_type" = "code");
  assert (param "client_id" = policy.client_id);
  assert (param "redirect_uri" = policy.redirect_uri);
  assert (param "scope" = String.concat " " policy.scopes);
  assert (param "state" = auth.state);
  assert (param "code_challenge" = Flow.pkce_challenge auth.verifier);
  assert (param "code_challenge_method" = "S256");
  assert (param "id_token_add_organizations" = "true");
  assert (param "codex_cli_simplified_flow" = "true");
  let callback = auth.redirect_uri ^ "?code=fixture-code&state=" ^ auth.state in
  let send body ~url ~headers ~body:request =
    assert (url = policy.token_url);
    assert (List.mem ("Content-Type", "application/x-www-form-urlencoded") headers);
    assert (List.mem ("client_id=" ^ policy.client_id) (String.split_on_char '&' request));
    200, body in
  let access = with_account "workspace-1" in
  let credential = Codex.exchange ~http:(send (token_response access)) ~now:1001.
    auth ~response:callback in
  assert (credential.account_id = Some "workspace-1");
  assert (credential.refresh = Some "refresh-one");
  assert (credential.expires_at = Some 1601.);
  assert (credential.metadata = []);
  assert (Codex.account_id credential = "workspace-1");
  let id_only = Codex.exchange ~http:(send
    (token_response ~id_token:access without_account)) ~now:1001. auth
    ~response:callback in
  assert (id_only.account_id = Some "workspace-1");
  assert (id_only.metadata = []);
  assert (Codex.account_id id_only = "workspace-1");
  let send_response response = send response in
  let invalid_claims = [
    jwt (`Assoc []);
    jwt (`Assoc ["https://api.openai.com/auth", `Assoc []]);
    jwt (`Assoc ["https://api.openai.com/auth", `Assoc ["chatgpt_account_id", `Null]]);
    jwt (`Assoc ["https://api.openai.com/auth", `Assoc ["chatgpt_account_id", `Int 12]]);
    jwt (`Assoc ["https://api.openai.com/auth", `Assoc ["chatgpt_account_id", `String ""]]);
    jwt (`Assoc ["https://api.openai.com/auth", `Assoc ["chatgpt_account_id", `String "bad\nheader"]]);
    "not-a-jwt";
    jwt_payload "not-json";
    "x.!!!!.y";
  ] in
  List.iter (fun token ->
    expect_error (fun () -> Codex.exchange ~http:(send_response (token_response token))
      ~now:1001. auth ~response:callback)) invalid_claims;
  expect_error (fun () -> Codex.exchange ~http:(send_response
    (token_response ~id_token:(with_account "workspace-2") access))
    ~now:1001. auth ~response:callback);
  expect_error (fun () -> Codex.exchange ~http:(send_response
    (token_response ~id_token:"invalid" without_account))
    ~now:1001. auth ~response:callback);
  let refreshed = Codex.refresh ~http:(send (token_response
    ~refresh_token:"refresh-two" without_account)) ~now:1100. credential in
  assert (refreshed.account_id = credential.account_id);
  assert (refreshed.refresh = Some "refresh-two");
  assert (refreshed.expires_at = Some 1700.);
  assert (refreshed.metadata = []);
  assert (Codex.account_id refreshed = "workspace-1");
  let data_region = { credential with access = with_residency [
    "chatgpt_data_residency", `String "eu";
    "chatgpt_compute_residency", `String "us" ] } in
  assert (Codex.identity data_region = ("workspace-1", Some "eu"));
  let compute_region = { credential with access = with_residency [
    "chatgpt_compute_residency", `String "us" ] } in
  assert (Codex.identity compute_region = ("workspace-1", Some "us"));
  expect_error (fun () -> Codex.identity { credential with
    access = with_residency [ "chatgpt_data_residency", `String "evil\r\nheader" ] });
  expect_error (fun () -> Codex.refresh ~http:(send (token_response
    (with_account "workspace-2"))) ~now:1100. credential);
  expect_error (fun () -> Codex.refresh ~http:(send (token_response
    ~id_token:(with_account "workspace-2") without_account)) ~now:1100. credential);
  expect_error (fun () -> Codex.refresh ~http:(send (token_response "bad-jwt"))
    ~now:1100. credential);
  let invalid_prior = {credential with account_id = None} in
  expect_error (fun () -> Codex.refresh ~http:(send (token_response without_account))
    ~now:1100. invalid_prior);
  expect_error (fun () -> Codex.account_id {credential with
    account_id = Some "workspace-2"});
  expect_error (fun () -> Codex.account_id {credential with
    access = "not-a-jwt"});
  let device_clock = ref 2000. in
  let prompted = ref false and device_polls = ref 0
  and device_sleeps = ref [] in
  let device_verifier = "device-verifier" in
  let device_challenge = Flow.pkce_challenge device_verifier in
  let json_field key body =
    Yojson.Basic.Util.member key (Yojson.Basic.from_string body) in
  let device_http ~url ~headers ~body =
    if url = "https://auth.openai.com/api/accounts/deviceauth/usercode" then (
      assert (headers = ["Content-Type", "application/json"]);
      assert (json_field "client_id" body = `String policy.client_id);
      200, {|{"device_auth_id":"device-1","user_code":"ABCD-EFGH"}|})
    else if url = "https://auth.openai.com/api/accounts/deviceauth/token" then (
      assert !prompted;
      assert (headers = ["Content-Type", "application/json"]);
      assert (json_field "device_auth_id" body = `String "device-1");
      assert (json_field "user_code" body = `String "ABCD-EFGH");
      incr device_polls;
      match !device_polls with
      | 1 -> 403, ""
      | 2 -> 404, "not-json"
      | _ -> 200, Yojson.Basic.to_string (`Assoc [
          "authorization_code", `String "device-code";
          "code_verifier", `String device_verifier;
          "code_challenge", `String device_challenge ]))
    else if url = policy.token_url then (
      assert (headers = ["Content-Type", "application/x-www-form-urlencoded"]);
      let params = Flow.query_params body in
      assert (List.assoc "grant_type" params = "authorization_code");
      assert (List.assoc "client_id" params = policy.client_id);
      assert (List.assoc "code" params = "device-code");
      assert (List.assoc "redirect_uri" params =
        "https://auth.openai.com/deviceauth/callback");
      assert (List.assoc "code_verifier" params = device_verifier);
      200, token_response (with_account "workspace-1"))
    else failwith ("unexpected Codex device URL: " ^ url) in
  let device_credential = Codex.device_login ~http:device_http
    ~now:(fun () -> !device_clock)
    ~sleep:(fun seconds ->
      device_sleeps := seconds :: !device_sleeps;
      device_clock := !device_clock +. seconds)
    ~timeout:60.
    ~on_authorization:(fun (auth : Pave.Oauth_device.authorization) ->
      assert (auth.verification_uri = "https://auth.openai.com/codex/device");
      assert (auth.verification_uri_complete = None);
      assert (auth.user_code = "ABCD-EFGH");
      prompted := true) () in
  assert !prompted;
  assert (!device_polls = 3);
  assert (List.rev !device_sleeps = [5.; 5.]);
  assert (device_credential.account_id = Some "workspace-1");
  assert (device_credential.refresh = Some "refresh-one");
  assert (device_credential.expires_at = Some 2610.);
  assert (device_credential.metadata = []);
  assert (Codex.account_id device_credential = "workspace-1");
  let mismatch_http ~url ~headers:_ ~body:_ =
    if url = "https://auth.openai.com/api/accounts/deviceauth/usercode" then
      200, {|{"device_auth_id":"device-2","user_code":"WXYZ-1234"}|}
    else if url = "https://auth.openai.com/api/accounts/deviceauth/token" then
      200, Yojson.Basic.to_string (`Assoc [
        "authorization_code", `String "device-code";
        "code_verifier", `String device_verifier;
        "code_challenge", `String "wrong-challenge" ])
    else failwith ("unexpected Codex device URL: " ^ url) in
  expect_error (fun () -> Codex.device_login ~http:mismatch_http
    ~now:(fun () -> 3000.) ~timeout:30. ~on_authorization:(fun _ -> ()) ());
  let denied_http ~url ~headers:_ ~body:_ =
    if url = "https://auth.openai.com/api/accounts/deviceauth/usercode" then
      200, {|{"device_auth_id":"device-3","user_code":"NOPE-1234"}|}
    else if url = "https://auth.openai.com/api/accounts/deviceauth/token" then
      400, {|{"error":"access_denied"}|}
    else failwith ("unexpected Codex device URL: " ^ url) in
  expect_error (fun () -> Codex.device_login ~http:denied_http
    ~now:(fun () -> 4000.) ~timeout:30. ~on_authorization:(fun _ -> ()) ());
  let timeout_clock = ref 5000. and timeout_polls = ref 0 in
  let pending_http ~url ~headers:_ ~body:_ =
    if url = "https://auth.openai.com/api/accounts/deviceauth/usercode" then
      200, {|{"device_auth_id":"device-4","user_code":"WAIT-1234"}|}
    else if url = "https://auth.openai.com/api/accounts/deviceauth/token" then (
      incr timeout_polls;
      403, "")
    else failwith ("unexpected Codex device URL: " ^ url) in
  expect_error (fun () -> Codex.device_login ~http:pending_http
    ~now:(fun () -> !timeout_clock)
    ~sleep:(fun seconds -> timeout_clock := !timeout_clock +. seconds)
    ~timeout:6. ~on_authorization:(fun _ -> ()) ());
  assert (!timeout_polls = 2);
  print_endline "codex oauth: ok"
