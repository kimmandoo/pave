(* Codex subscription credentials belong to auth.openai.com and chatgpt.com,
   not the public OpenAI API. JWT claims are decoded, not locally verified: only
   tokens obtained from the registered HTTPS token endpoint are authoritative. *)

let policy () : Oauth_flow.policy = {
  client_id = "app_EMoamEEZ73f0CkXaXp7hrann";
  authorize_url = "https://auth.openai.com/oauth/authorize";
  token_url = "https://auth.openai.com/oauth/token";
  redirect_uri = "http://localhost:1455/auth/callback";
  scopes = ["openid"; "profile"; "email"; "offline_access";
    "api.connectors.read"; "api.connectors.invoke"];
  token_body = Oauth_flow.Form;
  extra_authorize_params = ["id_token_add_organizations", "true";
    "codex_cli_simplified_flow", "true"; "originator", "pave"];
  extra_token_params = [];
  extra_token_headers = [];
  refresh_url = None;
  refresh_body = None;
  extra_refresh_params = [];
  extra_refresh_headers = [];
  account_path = [];
  (* Only a temporary exchange transport for id_token-only account claims.
     The wrappers remove it before credentials reach persistent storage. *)
  metadata_paths = ["id_token", ["id_token"]];
  expiry_skew = 0.;
}

let fail () = raise (Oauth_flow.OAuth_error "Codex token has no valid account ID")

let decode_payload segment =
  let length = String.length segment in
  if length = 0 || length mod 4 = 1 || length > 1_048_576 then fail ();
  let decoded = Bytes.create (length * 3 / 4) in
  let digit = function
    | 'A'..'Z' as c -> Char.code c - Char.code 'A'
    | 'a'..'z' as c -> Char.code c - Char.code 'a' + 26
    | '0'..'9' as c -> Char.code c - Char.code '0' + 52
    | '-' -> 62
    | '_' -> 63
    | _ -> fail () in
  let rec loop pos target =
    if pos + 4 <= length then (
      let a = digit segment.[pos] and b = digit segment.[pos + 1]
      and c = digit segment.[pos + 2] and d = digit segment.[pos + 3] in
      Bytes.set decoded target (Char.chr ((a lsl 2) lor (b lsr 4)));
      Bytes.set decoded (target + 1) (Char.chr (((b land 15) lsl 4) lor (c lsr 2)));
      Bytes.set decoded (target + 2) (Char.chr (((c land 3) lsl 6) lor d));
      loop (pos + 4) (target + 3))
    else match length - pos with
    | 0 -> Bytes.sub_string decoded 0 target
    | 2 ->
        let a = digit segment.[pos] and b = digit segment.[pos + 1] in
        if b land 15 <> 0 then fail ();
        Bytes.set decoded target (Char.chr ((a lsl 2) lor (b lsr 4)));
        Bytes.sub_string decoded 0 (target + 1)
    | 3 ->
        let a = digit segment.[pos] and b = digit segment.[pos + 1]
        and c = digit segment.[pos + 2] in
        if c land 3 <> 0 then fail ();
        Bytes.set decoded target (Char.chr ((a lsl 2) lor (b lsr 4)));
        Bytes.set decoded (target + 1) (Char.chr (((b land 15) lsl 4) lor (c lsr 2)));
        Bytes.sub_string decoded 0 (target + 2)
    | _ -> fail () in
  loop 0 0

let auth_claims token =
  let payload = match String.split_on_char '.' token with
    | [header; payload; signature] when header <> "" && signature <> "" -> payload
    | _ -> fail () in
  let json = try Yojson.Basic.from_string (decode_payload payload)
    with _ -> fail () in
  match json with
  | `Assoc fields ->
      (match List.assoc_opt "https://api.openai.com/auth" fields with
      | None -> None
      | Some (`Assoc auth) -> Some auth
      | _ -> fail ())
  | _ -> fail ()

let account_claim = function
  | None -> None
  | Some auth ->
      (match List.assoc_opt "chatgpt_account_id" auth with
      | None -> None
      | Some (`String id) when id <> "" && String.trim id = id &&
          not (String.exists (fun c -> Char.code c <= 32 || Char.code c = 127) id) ->
          Some id
      | _ -> fail ())

let residency_claim = function
  | None -> None
  | Some auth ->
      let value key = match List.assoc_opt key auth with
        | Some (`String value) ->
            let value = String.trim value in
            if value = "" then None
            else if String.length value > 256 || Oauth_flow.has_controls value then
              fail ()
            else Some value
        | _ -> None in
      (match value "chatgpt_data_residency" with
       | Some _ as value -> value
       | None -> value "chatgpt_compute_residency")

let identity (credential : Oauth_store.credential) =
  let access_auth = auth_claims credential.access in
  let access = account_claim access_auth in
  let id_token = Option.bind
    (List.assoc_opt "id_token" credential.metadata)
    (fun token -> account_claim (auth_claims token)) in
  (match access, id_token with
  | Some a, Some b when a <> b -> fail ()
  | _ -> ());
  let account_id = match access, id_token, credential.account_id with
    | Some id, _, _ | None, Some id, _ | None, None, Some id -> id
    | None, None, None -> fail () in
  (match credential.account_id with
  | Some stored when stored <> account_id -> fail ()
  | _ -> ());
  account_id, residency_claim access_auth

let account_id credential = fst (identity credential)

let with_account (credential : Oauth_store.credential) =
  let id, _ = identity credential in
  { credential with account_id = Some id;
    metadata = List.remove_assoc "id_token" credential.metadata }

let exchange ?http ?now authorization ~response =
  with_account (Oauth_flow.exchange ?http ?now (policy ()) authorization ~response)

let refresh ?http ?now (credential : Oauth_store.credential) =
  (* Never let Oauth_flow's metadata inheritance reuse a previous id_token. *)
  let prior = { credential with
    metadata = List.remove_assoc "id_token" credential.metadata } in
  with_account (Oauth_flow.refresh ?http ?now (policy ()) prior)

let device_usercode_url =
  "https://auth.openai.com/api/accounts/deviceauth/usercode"

let device_token_url =
  "https://auth.openai.com/api/accounts/deviceauth/token"

let device_verification_uri = "https://auth.openai.com/codex/device"
let device_redirect_uri = "https://auth.openai.com/deviceauth/callback"

let device_post ?http ~url params =
  ignore (Oauth_flow.validate_url ~local:(Option.is_some http) url);
  Oauth_flow.unique_params params;
  let body = Yojson.Basic.to_string (`Assoc
    (List.map (fun (key, value) -> key, `String value) params)) in
  if String.length body > 65536 then
    raise (Oauth_flow.OAuth_error "Codex device request too large");
  let send = match http with
    | Some send -> send
    | None -> Oauth_flow.default_http in
  let status, response = send ~url ~headers:["Content-Type", "application/json"]
    ~body in
  if String.length response > 1_048_576 then
    raise (Oauth_flow.OAuth_error "Codex device response too large");
  status, response

let device_json response =
  let json = try Yojson.Basic.from_string response with _ ->
    raise (Oauth_flow.OAuth_error "invalid Codex device response") in
  match json with
  | `Assoc _ -> json
  | _ -> raise (Oauth_flow.OAuth_error "invalid Codex device response")

let device_value key json =
  match Oauth_flow.string_at [key] json with
  | Some value when String.length value <= 8192
      && not (Oauth_flow.has_controls value) -> value
  | _ -> raise (Oauth_flow.OAuth_error "invalid Codex device response")

let device_error json = Oauth_flow.string_at ["error"] json

let device_login ?http ?(now = Unix.gettimeofday) ?(sleep = fun seconds ->
    ignore (Unix.select [] [] [] seconds)) ?(timeout = 900.)
    ~on_authorization () =
  if not (Float.is_finite timeout) || timeout <= 0. || timeout > 900. then
    raise (Oauth_flow.OAuth_error "invalid Codex device authorization timeout");
  let start = now () in
  if not (Float.is_finite start) then
    raise (Oauth_flow.OAuth_error "invalid Codex device authorization clock");
  let deadline = start +. timeout in
  if not (Float.is_finite deadline) then
    raise (Oauth_flow.OAuth_error "invalid Codex device authorization timeout");
  let check_deadline () =
    let current = now () in
    if not (Float.is_finite current) || current >= deadline then
      raise (Oauth_flow.OAuth_error "Codex device authorization timed out");
    current in
  ignore (check_deadline ());
  let policy = policy () in
  let status, response = device_post ?http ~url:device_usercode_url
    ["client_id", policy.client_id] in
  ignore (check_deadline ());
  if status < 200 || status >= 300 then
    raise (Oauth_flow.OAuth_error "Codex device authorization request rejected");
  let device = device_json response in
  let device_auth_id = device_value "device_auth_id" device in
  let user_code = device_value "user_code" device in
  let authorization : Oauth_device.authorization = {
    verification_uri = device_verification_uri;
    verification_uri_complete = None;
    user_code;
  } in
  on_authorization authorization;
  let rec poll polls_left =
    ignore (check_deadline ());
    if polls_left <= 0 then
      raise (Oauth_flow.OAuth_error "Codex device authorization timed out");
    let status, response = device_post ?http ~url:device_token_url
      ["device_auth_id", device_auth_id; "user_code", user_code] in
    let current = check_deadline () in
    if status = 403 || status = 404 then (
      if polls_left <= 1 then
        raise (Oauth_flow.OAuth_error "Codex device authorization timed out");
      let remaining = deadline -. current in
      if remaining <= 0. then
        raise (Oauth_flow.OAuth_error "Codex device authorization timed out");
      sleep (min 5. remaining);
      poll (polls_left - 1))
    else if status < 200 || status >= 300 then (
      let error = try device_error (device_json response) with
        | Oauth_flow.OAuth_error _ -> None in
      match error with
      | Some "access_denied" ->
          raise (Oauth_flow.OAuth_error "Codex device authorization denied")
      | Some "expired_token" ->
          raise (Oauth_flow.OAuth_error "Codex device authorization expired")
      | _ -> raise (Oauth_flow.OAuth_error "Codex device token request rejected"))
    else
      let result = device_json response in
      (match device_error result with
       | Some "access_denied" ->
           raise (Oauth_flow.OAuth_error "Codex device authorization denied")
       | Some "expired_token" ->
           raise (Oauth_flow.OAuth_error "Codex device authorization expired")
       | Some _ ->
           raise (Oauth_flow.OAuth_error "Codex device token request rejected")
       | None -> ());
      let code = device_value "authorization_code" result in
      let verifier = device_value "code_verifier" result in
      (match Oauth_flow.response_field "code_challenge" result with
       | None | Some `Null -> ()
       | Some (`String challenge)
         when challenge = Oauth_flow.pkce_challenge verifier -> ()
       | _ -> raise (Oauth_flow.OAuth_error "Codex device proof key mismatch"));
      let token = Oauth_flow.post ?http ~url:policy.token_url
        ~headers:policy.extra_token_headers ~format:policy.token_body
        ["grant_type", "authorization_code"; "client_id", policy.client_id;
         "code", code; "redirect_uri", device_redirect_uri;
         "code_verifier", verifier] in
      let token_now = now () in
      if not (Float.is_finite token_now) then
        raise (Oauth_flow.OAuth_error "invalid Codex device authorization clock");
      with_account (Oauth_flow.credential ~now:token_now policy token)
  in
  let max_polls = int_of_float (ceil (timeout /. 5.)) + 1 in
  poll max_polls
