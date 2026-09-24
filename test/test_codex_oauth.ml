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
  print_endline "codex oauth: ok"
