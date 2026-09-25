module Login = Pave.Kilo_oauth

let reject action = match action () with
  | exception Pave.Oauth_device.OAuth_error _ -> ()
  | _ -> failwith "unsafe device approval was accepted"

let () =
  let send ~url ~headers ~body =
    assert (url = "https://api.kilo.ai/api/device-auth/codes");
    assert (headers = ["Content-Type", "application/json"]);
    assert (body = "{}");
    200, {|{"code":"ABC-123","verificationUrl":"https://kilo.ai/device","expiresIn":120}|} in
  let auth = Login.start ~post:send ~now:100. () in
  assert (auth.code = "ABC-123" && auth.expires_at = 220.);
  let requests = ref 0 and current = ref 101. in
  let get ~url ~headers =
    assert (url = "https://api.kilo.ai/api/device-auth/codes/ABC-123");
    assert (headers = ["Accept", "application/json"]);
    incr requests;
    Ok (if !requests = 1 then 202, ""
      else 200, {|{"status":"approved","token":"gateway-session-key"}|}) in
  let credential = Login.poll ~get ~now:(fun () -> !current)
    ~sleep:(fun seconds -> assert (seconds = 5.); current := !current +. seconds)
    auth in
  assert (!requests = 2 && credential.access = "gateway-session-key");
  assert (credential.refresh = None && credential.expires_at = None);
  let denied ~url:_ ~headers:_ = Ok (403, "") in
  reject (fun () -> Login.poll ~get:denied ~now:(fun () -> 101.) auth);
  reject (fun () -> Login.poll ~get ~now:(fun () -> 220.) auth);
  reject (fun () -> Login.start ~now:100.
    ~post:(fun ~url:_ ~headers:_ ~body:_ ->
      200, {|{"code":"ABC-123","verificationUrl":"https://evil.example/device","expiresIn":120}|}) ());
  reject (fun () -> Login.start ~now:100.
    ~post:(fun ~url:_ ~headers:_ ~body:_ ->
      200, {|{"code":"ABC/123","verificationUrl":"https://kilo.ai/device","expiresIn":120}|}) ());
  print_endline "Kilo device approval: ok"
