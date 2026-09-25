(* The bundled GitLab client has a stale registered callback. Only a user's
   explicitly registered OAuth application may request a GitLab Duo token. *)
let fail message = raise (Oauth_flow.OAuth_error message)

let client_id () = match Sys.getenv_opt "GITLAB_CLIENT_ID" with
  | Some id when id <> "" && String.length id <= 256 &&
      String.for_all (function
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' -> true
        | _ -> false) id -> id
  | _ -> fail "Set a valid GITLAB_CLIENT_ID for your registered GitLab OAuth application"

let redirect_uri () = match Sys.getenv_opt "GITLAB_REDIRECT_URI" with
  | None -> fail "Set GITLAB_REDIRECT_URI to the loopback callback registered with your GitLab OAuth application"
  | Some value ->
      let uri = Oauth_flow.validate_url ~local:true value in
      if uri.scheme <> "http" || not (Oauth_flow.loopback_host uri.authority) ||
         uri.query <> "" || uri.path = "/" ||
         not (String.for_all (function
           | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '/' -> true
           | _ -> false) uri.path) ||
         (String.length uri.path > 1 && uri.path.[1] = '/') ||
         value <> "http://" ^ uri.authority ^ uri.path then
        fail "GITLAB_REDIRECT_URI must be a fixed HTTP loopback URL with a callback path";
      value

let policy () : Oauth_flow.policy = {
  client_id = client_id ();
  authorize_url = "https://gitlab.com/oauth/authorize";
  token_url = "https://gitlab.com/oauth/token";
  redirect_uri = redirect_uri ();
  scopes = ["api"];
  token_body = Oauth_flow.Form;
  extra_authorize_params = [];
  extra_token_params = [];
  extra_token_headers = [];
  refresh_url = None;
  refresh_body = None;
  extra_refresh_params = [];
  extra_refresh_headers = [];
  account_path = [];
  metadata_paths = [];
  expiry_skew = 300.;
}

let exchange ?http ?now (auth : Oauth_flow.authorization) ~response =
  let config = policy () in
  if auth.redirect_uri <> config.redirect_uri ||
     String.length auth.state < 43 || String.length auth.verifier < 43 ||
     auth.challenge <> Oauth_flow.pkce_challenge auth.verifier ||
     auth.url <> config.authorize_url ^ "?" ^ Oauth_flow.encode_params [
       "response_type", "code"; "client_id", config.client_id;
       "redirect_uri", config.redirect_uri; "scope", "api";
       "state", auth.state; "code_challenge", auth.challenge;
       "code_challenge_method", "S256" ] then
    fail "GitLab authorization does not match the registered OAuth application";
  let credential = Oauth_flow.exchange ?http ?now config auth ~response in
  { credential with metadata = ("gitlab_client_id", config.client_id) :: credential.metadata }

let refresh ?http ?now (prior : Oauth_store.credential) =
  let config = policy () in
  if List.assoc_opt "gitlab_client_id" prior.metadata <> Some config.client_id then
    fail "GitLab OAuth application changed; log in again with the registered client ID";
  Oauth_flow.refresh ?http ?now config prior
