module Device = Pave.Oauth_device

let policy : Device.policy = {
  client_id = "public-id";
  device_url = "http://127.0.0.1:9876/device";
  token_url = "http://127.0.0.1:9876/token";
  scopes = ["openid"; "offline_access"];
  scope_separator = " ";
  device_body = Form;
  token_body = Form;
  device_headers = ["Accept", "application/json"];
  token_headers = ["Accept", "application/json"];
  extra_device_params = [];
  extra_token_params = [];
  account_path = ["identity"; "subject"];
  metadata_paths = ["org", ["identity"; "org"]];
  expiry_skew = 0.;
}

let device_response =
  {|{"device_code":"private-device","user_code":"USER-CODE","verification_uri":"http://127.0.0.1:9876/verify","verification_uri_complete":"http://127.0.0.1:9876/verify?user_code=USER-CODE","expires_in":120,"interval":2}|}

let success =
  {|{"access_token":"private-access","refresh_token":"rotated-refresh","expires_in":60,"identity":{"subject":"account-1","org":"team-1"}}|}

let expect_error expected f =
  match f () with
  | _ -> failwith ("expected device OAuth error: " ^ expected)
  | exception Device.OAuth_error actual ->
      if actual <> expected then failwith ("wrong device OAuth error: " ^ actual)

let fixture ?(config = policy) ?(timeout = 40.) ?(device = device_response) responses =
  let time = ref 1000. in
  let sleeps = ref [] in
  let presented = ref None in
  let sent = ref [] in
  let next = ref responses in
  let http ~url ~headers ~body =
    sent := (url, headers, body) :: !sent;
    if url = config.device_url then 200, device
    else if url = config.token_url then
      match !next with
      | [] -> failwith "unexpected extra device token request"
      | result :: rest -> next := rest; result
    else failwith "unexpected device URL" in
  let run () = Device.login ~http ~now:(fun () -> !time)
    ~sleep:(fun seconds -> sleeps := seconds :: !sleeps; time := !time +. seconds)
    ~timeout config ~on_authorization:(fun auth -> presented := Some auth) in
  run, presented, sleeps, sent

let () =
  let run, presented, sleeps, sent = fixture [
    400, {|{"error":"authorization_pending"}|};
    400, {|{"error":"slow_down"}|};
    400, {|{"error":"authorization_pending"}|};
    200, success] in
  let credential = run () in
  assert (credential.access = "private-access");
  assert (credential.refresh = Some "rotated-refresh");
  assert (credential.expires_at = Some 1076.);
  assert (credential.account_id = Some "account-1");
  assert (credential.metadata = ["org", "team-1"]);
  assert (List.rev !sleeps = [2.; 7.; 7.]);
  (match !presented with
   | Some auth ->
       assert (auth.user_code = "USER-CODE");
       assert (auth.verification_uri = "http://127.0.0.1:9876/verify");
       assert (auth.verification_uri_complete =
         Some "http://127.0.0.1:9876/verify?user_code=USER-CODE")
   | None -> failwith "missing device authorization callback");
  (match List.rev !sent with
   | (device_url, headers, body) :: (token_url, _, token_body) :: _ ->
       assert (device_url = policy.device_url);
       assert (token_url = policy.token_url);
       assert (List.mem ("Content-Type", "application/x-www-form-urlencoded") headers);
       assert (body = "client_id=public-id&scope=openid%20offline_access");
       assert (token_body =
         "grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Adevice_code&client_id=public-id&device_code=private-device")
   | _ -> failwith "missing device grant requests");

  (* The policy, not a provider name, determines per-endpoint encoding and headers. *)
  let config = { policy with scopes = []; device_body = Json; token_body = Json;
    device_headers = ["x-api-version", "1.0.0"];
    token_headers = ["x-api-version", "1.0.0"] } in
  let run, _, _, sent = fixture ~config [200, {|{"access_token":"no-refresh"}|}] in
  let result = run () in
  assert (result.refresh = None && result.expires_at = None);
  (match List.rev !sent with
   | (_, headers, body) :: (_, token_headers, token_body) :: _ ->
       assert (List.mem ("x-api-version", "1.0.0") headers);
       assert (List.mem ("Content-Type", "application/json") token_headers);
       assert (body = {|{"client_id":"public-id"}|});
       assert (Yojson.Basic.Util.member "device_code" (Yojson.Basic.from_string token_body)
         = `String "private-device")
   | _ -> failwith "missing JSON device grant requests");

  let run, _, _, _ = fixture [400, {|{"error":"access_denied","error_description":"secret"}|}] in
  expect_error "OAuth device authorization denied" run;
  let run, _, _, _ = fixture [400, {|{"error":"expired_token"}|}] in
  expect_error "OAuth device authorization expired" run;
  let run, _, _, _ = fixture [500, {|{"error":"internal","debug":"private-token"}|}] in
  expect_error "OAuth device token request rejected" run;
  let run, _, _, _ = fixture [200, {|{"error":"authorization_pending"}|}] in
  expect_error "OAuth device token request rejected" run;
  let run, _, _, _ = fixture [200, {|{"access_token":""}|}] in
  expect_error "OAuth device response missing required field" run;
  let run, _, _, _ = fixture [200, {|{"access_token":"valid","expires_in":-3}|}] in
  expect_error "invalid OAuth device numeric field" run;
  let run, presented, _, _ = fixture ~device:
    {|{"device_code":"private-device","verification_uri":"http://127.0.0.1:9876/verify","expires_in":120}|}
    [200, success] in
  expect_error "OAuth device response missing required field" run;
  assert (!presented = None);
  let run, _, _, _ = fixture [200, {|{"access_token":"private-access","refresh_token":false}|}] in
  expect_error "invalid OAuth device response field" run;
  let run, _, sleeps, _ = fixture ~timeout:4. [
    400, {|{"error":"authorization_pending"}|};
    400, {|{"error":"authorization_pending"}|}] in
  expect_error "OAuth device authorization timed out" run;
  assert (List.rev !sleeps = [2.; 2.]);
  let run, _, sleeps, _ = fixture ~device:
    {|{"device_code":"private-device","user_code":"USER-CODE","verification_uri":"http://127.0.0.1:9876/verify","expires_in":3,"interval":2}|} [
    400, {|{"error":"authorization_pending"}|};
    400, {|{"error":"authorization_pending"}|}] in
  expect_error "OAuth device authorization timed out" run;
  assert (List.rev !sleeps = [2.; 1.]);
  let run, presented, _, _ = fixture ~device:
    {|{"device_code":"private-device","user_code":"USER-CODE","verification_uri":"http://evil.example/verify","expires_in":120}|}
    [200, success] in
  expect_error "OAuth device URL must use HTTPS (or injected loopback HTTP)" run;
  assert (!presented = None);
  expect_error "OAuth device URL must use HTTPS (or injected loopback HTTP)"
    (fun () -> Device.login policy ~on_authorization:(fun _ -> failwith "unsafe callback"));
  (* Never forward a transport exception that might contain request credentials. *)
  let http ~url:_ ~headers:_ ~body:_ = failwith "private-device private-secret" in
  expect_error "OAuth device request failed" (fun () ->
    Device.login ~http policy ~on_authorization:(fun _ -> ()));
  print_endline "oauth device: ok"
