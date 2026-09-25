(* Devin's CLI grant has no client ID and its token endpoint accepts only the
   authorization code and PKCE verifier. Never use a caller-selected endpoint
   for either the exchange or the session-bound Connect credential. *)
let authorize_url = "https://app.devin.ai/auth/cli/continue"
let token_url = "https://api.devin.ai/auth/cli/token"
let redirect_uri = "http://127.0.0.1:59653/callback"

let fail message = raise (Oauth_flow.OAuth_error message)

let uuid () =
  let source = Oauth_flow.random_bytes 16 in
  let hex = "0123456789abcdef" in
  let out = Bytes.create 36 in
  let position = ref 0 in
  for i = 0 to 15 do
    if i = 4 || i = 6 || i = 8 || i = 10 then (
      Bytes.set out !position '-'; incr position);
    let byte = Char.code source.[i] in
    let n = if i = 6 then (byte land 0x0f) lor 0x40
      else if i = 8 then (byte land 0x3f) lor 0x80
      else byte in
    Bytes.set out !position hex.[n lsr 4]; incr position;
    Bytes.set out !position hex.[n land 0x0f]; incr position
  done;
  Bytes.unsafe_to_string out

let authorization_url ~state ~challenge =
  authorize_url ^ "?" ^ Oauth_flow.encode_params [
    "response_type", "code";
    "redirect_uri", redirect_uri;
    "code_challenge", challenge;
    "code_challenge_method", "S256";
    "state", state;
    "prompt", "select_account"]

let start ?(now = Unix.gettimeofday ()) ?(ttl = 300.) () : Oauth_flow.authorization =
  if not (Float.is_finite now) || not (Float.is_finite ttl) || ttl <= 0. || ttl > 600.
  then fail "invalid Devin authorization deadline";
  let deadline = now +. ttl in
  if not (Float.is_finite deadline) then fail "invalid Devin authorization deadline";
  let state = uuid () in
  let verifier = Oauth_flow.base64url (Oauth_flow.random_bytes 32) in
  let challenge = Oauth_flow.pkce_challenge verifier in
  { state; verifier; challenge; url = authorization_url ~state ~challenge;
    redirect_uri; deadline }

let listen_loopback ?now ?ttl () =
  let listener = Oauth_flow.bind_loopback_callback redirect_uri in
  try start ?now ?ttl (), listener
  with exn -> Unix.close listener; raise exn

let await_callback ?now auth listener =
  Oauth_flow.await_callback ?now auth listener

let valid_uuid state =
  if String.length state <> 36 || state.[14] <> '4' ||
     not (String.contains "89ab" state.[19]) then false
  else
    let valid = ref true in
    for i = 0 to 35 do
      let separator = i = 8 || i = 13 || i = 18 || i = 23 in
      if separator then valid := !valid && state.[i] = '-'
      else valid := !valid && String.contains "0123456789abcdef" state.[i]
    done;
    !valid

let check_authorization (auth : Oauth_flow.authorization) =
  if auth.redirect_uri <> redirect_uri || not (valid_uuid auth.state) ||
     String.length auth.verifier <> 43 ||
     not (String.for_all (fun c -> String.contains
       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" c)
       auth.verifier) ||
     auth.challenge <> Oauth_flow.pkce_challenge auth.verifier ||
     auth.url <> authorization_url ~state:auth.state ~challenge:auth.challenge
  then fail "invalid Devin authorization"

let exchange ?http ?now auth ~response : Oauth_store.credential =
  check_authorization auth;
  let code = Oauth_flow.parse_callback ?now auth ~response in
  let json = Oauth_flow.post ?http ~url:token_url
    ~headers:["Accept", "application/json"] ~format:Oauth_flow.Json
    ["code", code; "code_verifier", auth.verifier] in
  let token = match Oauth_flow.member "token" json with
    | `String token when Devin_api.valid_text token &&
      token <> "devin-session-token$" -> token
    | _ -> fail "Devin did not issue a valid session token" in
  (* Devin_api.session_key supplies the wire prefix exactly once, on the
     fixed Codeium Connect transport; store the actual returned token. *)
  { access = token; refresh = None; expires_at = None;
    account_id = None; metadata = [] }

let exchange_code ?http ?now auth ~code =
  let response = redirect_uri ^ "?" ^ Oauth_flow.encode_params [
    "code", code; "state", auth.Oauth_flow.state] in
  exchange ?http ?now auth ~response

let login ?http ?ttl ~on_authorization () =
  let auth, listener = listen_loopback ?ttl () in
  (try on_authorization auth
   with exn -> Unix.close listener; raise exn);
  let code = await_callback auth listener in
  exchange_code ?http auth ~code

let login_manual ?http ?ttl ~on_authorization ~on_code () =
  let auth = start ?ttl () in
  on_authorization auth;
  exchange ?http auth ~response:(on_code ())
