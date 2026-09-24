module OAuth = Pave.Github_copilot_oauth
module Device = Pave.Oauth_device
module Store = Pave.Oauth_store

let device_response =
  {|{"device_code":"private-device","user_code":"ABCD-1234","verification_uri":"https://github.com/login/device","expires_in":30,"interval":2}|}

let expect_error run = match run () with
  | exception Device.OAuth_error _ -> ()
  | _ -> failwith "accepted invalid device grant"

let fixture ?(device = device_response) ?(timeout = 30.) replies =
  let now = ref 1000. and sleeps = ref [] and requests = ref [] in
  let callback = ref None and remaining = ref replies in
  let policy = OAuth.policy () in
  let http ~url ~headers ~body =
    requests := (url, headers, body) :: !requests;
    if url = policy.device_url then 200, device
    else if url = policy.token_url then
      match !remaining with
      | reply :: rest -> remaining := rest; reply
      | [] -> failwith "unexpected token poll"
    else failwith "unexpected device grant endpoint" in
  let run () = OAuth.login ~http ~now:(fun () -> !now)
    ~sleep:(fun seconds -> sleeps := seconds :: !sleeps; now := !now +. seconds)
    ~timeout ~on_authorization:(fun auth -> callback := Some auth) () in
  run, requests, sleeps, callback

let () =
  let policy = OAuth.policy () in
  assert (policy.client_id = "Ov23ctDVkRmgkPke0Mmm");
  assert (policy.device_url = "https://github.com/login/device/code");
  assert (policy.token_url = "https://github.com/login/oauth/access_token");
  assert (policy.scopes = ["read:user"]);
  assert (policy.device_body = Device.Form && policy.token_body = Device.Form);
  let run, requests, sleeps, callback = fixture [
    400, {|{"error":"authorization_pending"}|};
    400, {|{"error":"slow_down"}|};
    200, {|{"access_token":"ghu_fixture-token"}|} ] in
  let credential = run () in
  assert (credential.access = "ghu_fixture-token" && credential.refresh = None);
  assert (credential.expires_at = None && credential.metadata = []);
  assert (List.rev !sleeps = [2.; 7.]);
  (match !callback with
   | Some auth ->
       assert (auth.verification_uri = "https://github.com/login/device");
       assert (auth.user_code = "ABCD-1234")
   | None -> failwith "verification callback missing");
  (match List.rev !requests with
   | (device_url, device_headers, body) :: (token_url, token_headers, grant) :: _ ->
       assert (device_url = policy.device_url && token_url = policy.token_url);
       assert (List.mem ("Accept", "application/json") device_headers);
       assert (List.mem ("Content-Type", "application/x-www-form-urlencoded") token_headers);
       let fields body = List.sort String.compare (String.split_on_char '&' body) in
       assert (fields body =
         fields "client_id=Ov23ctDVkRmgkPke0Mmm&scope=read%3Auser");
       assert (fields grant =
         fields "grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Adevice_code&client_id=Ov23ctDVkRmgkPke0Mmm&device_code=private-device")
   | _ -> failwith "device and token grants missing");
  let root = Filename.temp_file "pave-copilot-grant-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () ->
    Array.iter (fun name -> Sys.remove (Filename.concat root name)) (Sys.readdir root);
    Unix.rmdir root) (fun () ->
      let path = Filename.concat root "oauth.json" in
      Store.put ~path ~provider:"github-copilot" credential;
      assert (Store.get ~path ~provider:"github-copilot" = Some credential);
      assert ((Unix.stat path).Unix.st_perm = 0o600));
  let run, _, _, _ = fixture [
    200, {|{"access_token":"ghu_expiring-fixture","expires_in":60}|}] in
  expect_error run;
  let run, _, _, _ = fixture [
    200, {|{"access_token":"ghu_refresh-fixture","refresh_token":"not-supported"}|}] in
  expect_error run;
  let run, _, _, callback = fixture ~device:
    {|{"device_code":"private-device","user_code":"ABCD-1234","verification_uri":"https://github.com.evil.example/login/device","expires_in":30}|}
    [200, {|{"access_token":"ghu_fixture-token"}|}] in
  expect_error run;
  assert (!callback = None);
  let run, _, _, callback = fixture ~device:
    {|{"device_code":"private-device","user_code":"ABCD-1234","verification_uri":"https://github.com/login/device","verification_uri_complete":"https://evil.example/login/device","expires_in":30}|}
    [200, {|{"access_token":"ghu_fixture-token"}|}] in
  expect_error run;
  assert (!callback = None);
  let run, _, _, _ = fixture [400, {|{"error":"access_denied"}|}] in
  expect_error run;
  let run, _, _, _ = fixture [400, {|{"error":"expired_token"}|}] in
  expect_error run;
  let run, _, sleeps, _ = fixture ~timeout:3. [
    400, {|{"error":"authorization_pending"}|};
    400, {|{"error":"authorization_pending"}|}] in
  expect_error run;
  assert (List.rev !sleeps = [2.; 1.]);
  let run, _, _, _ = fixture [200, {|{"access_token":"bad\ntoken"}|}] in
  expect_error run;
  print_endline "GitHub Copilot device grant: ok"
