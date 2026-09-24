(* OpenRouter has no registered OAuth client and does not echo state. Its
   authorization code is bound to this login's random S256 PKCE verifier.
   Never weaken the standard state checks used by other providers. *)
let redirect_uri = "http://localhost:54549/callback"
let authorize_url = "https://openrouter.ai/auth"
let key_url = "https://openrouter.ai/api/v1/auth/keys"

let start ?(now = Unix.gettimeofday ()) ?(ttl = 300.) () =
  if not (Float.is_finite now) || not (Float.is_finite ttl) || ttl <= 0. || ttl > 600.
  then raise (Oauth_flow.OAuth_error "invalid OpenRouter authorization deadline");
  let verifier = Oauth_flow.base64url (Oauth_flow.random_bytes 32) in
  let challenge = Oauth_flow.pkce_challenge verifier in
  let url = authorize_url ^ "?" ^ Oauth_flow.encode_params [
    "callback_url", redirect_uri;
    "code_challenge", challenge;
    "code_challenge_method", "S256" ] in
  ({ Oauth_flow.state = ""; verifier; challenge; url; redirect_uri;
     deadline = now +. ttl } : Oauth_flow.authorization)

let listen_loopback ?now ?ttl () =
  let listener = Oauth_flow.bind_loopback_callback redirect_uri in
  try start ?now ?ttl (), listener
  with exn -> Unix.close listener; raise exn

let await_callback ?now auth listener =
  Oauth_flow.await_callback ?now ~require_state:false auth listener

let exchange ?http ?now (auth : Oauth_flow.authorization) ~response =
  if auth.redirect_uri <> redirect_uri ||
     not (String.starts_with ~prefix:(authorize_url ^ "?") auth.url) ||
     auth.challenge <> Oauth_flow.pkce_challenge auth.verifier then
    raise (Oauth_flow.OAuth_error "invalid OpenRouter authorization");
  let code = Oauth_flow.parse_callback ?now ~require_state:false auth ~response in
  let json = Oauth_flow.post ?http ~url:key_url ~headers:[] ~format:Oauth_flow.Json [
    "code", code; "code_verifier", auth.verifier;
    "code_challenge_method", "S256" ] in
  let key = match Oauth_flow.member "key" json with
    | `String key when String.starts_with ~prefix:"sk-or-" key &&
        String.length key <= 8192 && not (Oauth_flow.has_controls key) -> key
    | _ -> raise (Oauth_flow.OAuth_error "OpenRouter did not issue a valid API key") in
  ({ Oauth_store.access = key; refresh = None; expires_at = None;
     account_id = None; metadata = [] } : Oauth_store.credential)
