module OAuth = Pave.Openrouter_oauth
module Flow = Pave.Oauth_flow

let reject f = match f () with
  | exception Flow.OAuth_error _ -> ()
  | _ -> failwith "unsafe OpenRouter grant accepted"

let () =
  let auth = OAuth.start ~now:1000. () in
  let uri = Flow.parse_uri auth.url in
  let params = Flow.query_params uri.query in
  assert (uri.scheme = "https" && uri.authority = "openrouter.ai");
  assert (List.assoc "callback_url" params = auth.redirect_uri);
  assert (List.assoc "code_challenge" params = Flow.pkce_challenge auth.verifier);
  assert (List.assoc "code_challenge_method" params = "S256");
  assert (not (List.mem_assoc "state" params));
  let callback = auth.redirect_uri ^ "?code=fixture-code" in
  let sent = ref 0 in
  let send ~url ~headers ~body =
    incr sent;
    assert (url = OAuth.key_url);
    assert (List.mem ("Content-Type", "application/json") headers);
    let json = Yojson.Basic.from_string body in
    assert (Flow.member "code" json = `String "fixture-code");
    assert (Flow.member "code_verifier" json = `String auth.verifier);
    200, {|{"key":"sk-or-fixture"}|} in
  let credential = OAuth.exchange ~http:send ~now:1001. auth ~response:callback in
  assert (credential.access = "sk-or-fixture" && credential.refresh = None);
  assert (credential.expires_at = None && credential.account_id = None);
  assert (!sent = 1);
  reject (fun () -> OAuth.exchange ~http:send ~now:1001. auth
    ~response:(callback ^ "&state=forged"));
  reject (fun () -> OAuth.exchange ~http:send ~now:1001. auth
    ~response:"http://attacker.example/callback?code=fixture-code");
  reject (fun () -> OAuth.exchange ~http:send ~now:1300. auth ~response:callback);
  assert (!sent = 1);
  reject (fun () -> OAuth.exchange ~http:(fun ~url:_ ~headers:_ ~body:_ ->
    200, {|{"key":"wrong-provider-key"}|}) ~now:1001. auth ~response:callback);
  let browser_auth, listener = OAuth.listen_loopback () in
  let child = Unix.fork () in
  if child = 0 then (
    Unix.close listener;
    let client = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    (try
       Unix.connect client (Unix.ADDR_INET (Unix.inet_addr_loopback, 54549));
       let request = "GET /callback?code=browser-code HTTP/1.1\r\n" ^
         "Host: localhost:54549\r\n\r\n" in
       ignore (Unix.write_substring client request 0 (String.length request));
       let response = Bytes.create 512 in
       let count = Unix.read client response 0 (Bytes.length response) in
       assert (count > 0 && String.starts_with ~prefix:"HTTP/1.1 200"
         (Bytes.sub_string response 0 count));
       Unix.close client; exit 0
     with _ -> Unix.close client; exit 2));
  let code = OAuth.await_callback browser_auth listener in
  let _, status = Unix.waitpid [] child in
  assert (status = Unix.WEXITED 0 && code = "browser-code");
  print_endline "OpenRouter PKCE key grant: ok"
