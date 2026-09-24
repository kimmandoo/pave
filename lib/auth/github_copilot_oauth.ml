(* Personal github.com device grants use the registered public Copilot CLI client.
   A GitHub OAuth bearer token is used directly; there is no refresh exchange. *)
let device_url = "https://github.com/login/device/code"
let token_url = "https://github.com/login/oauth/access_token"
let verification_url = "https://github.com/login/device"

let policy () : Oauth_device.policy = {
  client_id = "Ov23ctDVkRmgkPke0Mmm";
  device_url;
  token_url;
  scopes = ["read:user"];
  scope_separator = " ";
  device_body = Form;
  token_body = Form;
  device_headers = ["Accept", "application/json";
    "User-Agent", "copilot-developer-action/0.0.1"];
  token_headers = ["Accept", "application/json";
    "User-Agent", "copilot-developer-action/0.0.1"];
  extra_device_params = [];
  extra_token_params = [];
  account_path = [];
  metadata_paths = [];
  expiry_skew = 0.;
}

let fail message = raise (Oauth_device.OAuth_error message)

let check_verification_url ~complete url =
  let uri = try Oauth_flow.validate_url url
    with Oauth_flow.OAuth_error _ -> fail "invalid GitHub device verification URL" in
  if uri.authority <> "github.com" || uri.path <> "/login/device" ||
     (not complete && uri.query <> "") then
    fail "invalid GitHub device verification URL"

let login ?http ?now ?sleep ?timeout ~on_authorization () : Oauth_store.credential =
  let on_authorization (authorization : Oauth_device.authorization) =
    check_verification_url ~complete:false authorization.verification_uri;
    Option.iter (check_verification_url ~complete:true)
      authorization.verification_uri_complete;
    on_authorization authorization in
  let credential = Oauth_device.login ?http ?now ?sleep ?timeout (policy ())
    ~on_authorization in
  if credential.access = "" || String.length credential.access > 8192 ||
     String.exists (fun c -> Char.code c <= 32 || Char.code c = 127)
       credential.access then fail "invalid GitHub OAuth access token";
  if credential.refresh <> None || credential.expires_at <> None then
    fail "expiring GitHub Copilot device grant requires unsupported refresh semantics";
  credential
