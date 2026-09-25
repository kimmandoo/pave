module Discovery = Pave.Model_discovery

let expect_models expected = function
  | Ok models when models = expected -> ()
  | Ok models -> failwith ("unexpected models: " ^ String.concat ", " models)
  | Error failure -> failwith (Discovery.message failure)

let expect_error check = function
  | Error failure when check failure -> ()
  | Error failure -> failwith ("unexpected error: " ^ Discovery.message failure)
  | Ok _ -> failwith "invalid listing was accepted"

let is_invalid_response = function Discovery.Invalid_response _ -> true | _ -> false
let unavailable = function Discovery.Http_error _ -> true | _ -> false
let no_credential = function Discovery.Missing_credential -> true | _ -> false
let wrong_credential = function Discovery.Invalid_credential -> true | _ -> false

let fixed_http expected_url expected_headers response =
  let calls = ref 0 in
  let http ~url ~headers =
    incr calls;
    assert (url = expected_url);
    assert (headers = expected_headers);
    response in
  http, calls

let () =
  let open Discovery in
  let openai_key = "private-openai" and gemini_key = "private-gemini" in
  let github_token = "ghu_private-oauth" in
  let openai_headers = ["Authorization", "Bearer " ^ openai_key] in
  let gemini_headers = ["x-goog-api-key", gemini_key] in
  let copilot_headers =
    ["Authorization", "Bearer " ^ github_token;
     "User-Agent", "copilot/1.0.82";
     "Editor-Version", "copilot/1.0.82";
     "Copilot-Integration-Id", "copilot-chat";
     "Copilot-Harness-Id", "copilot-sdk"] in
  let http, calls = fixed_http openai_url openai_headers (Ok (200,
    {|{"object":"list","data":[{"id":"gpt-4.1","object":"model"},{"id":"o3"},{"id":"gpt-4.1"}]}|})) in
  expect_models ["gpt-4.1"; "o3"]
    (discover ~http ~provider:"openai" ~credential:(Api_key openai_key) ());
  assert (!calls = 1);
  let http, _ = fixed_http ollama_url [] (Ok (200,
    {|{"models":[{"name":"qwen2.5:7b","size":500},{"name":"llama3:latest"},{"name":"qwen2.5:7b"}]}|})) in
  expect_models ["qwen2.5:7b"; "llama3:latest"]
    (discover ~http ~provider:"ollama" ());
  let http, _ = fixed_http copilot_url copilot_headers (Ok (200,
    {|{"data":[{"id":"gpt-4.1","capabilities":{"type":"chat"}},{"id":"embed","capabilities":{"type":"embeddings"}},{"id":"gpt-4o"}]}|})) in
  expect_models ["gpt-4.1"; "gpt-4o"]
    (discover ~http ~provider:"github-copilot"
       ~credential:(Copilot_oauth github_token) ());
  let calls = ref 0 in
  let http ~url ~headers =
    incr calls;
    assert (headers = gemini_headers);
    if !calls = 1 then (
      assert (url = google_url);
      Ok (200, {|{"models":[{"name":"models/gemini-2.5-pro","supportedGenerationMethods":["generateContent"]},{"name":"models/text-embedding-004","supportedGenerationMethods":["embedContent"]}],"nextPageToken":"next /?"}|}))
    else (
      assert (!calls = 2);
      assert (url = google_url ^ "?pageToken=next%20%2F%3F");
      Ok (200, {|{"models":[{"name":"models/gemini-2.5-pro","supportedGenerationMethods":["generateContent"]},{"name":"models/gemini-2.5-flash","supportedGenerationMethods":["generateContent"]}]}|})) in
  expect_models ["models/gemini-2.5-pro"; "models/gemini-2.5-flash"]
    (discover ~http ~provider:"google" ~credential:(Api_key gemini_key) ());
  assert (!calls = 2);
  let codex_credential = Codex_oauth ("private-codex", "account-123") in
  let codex_headers = [
    "Authorization", "Bearer private-codex";
    "chatgpt-account-id", "account-123";
    "OpenAI-Beta", "responses=experimental";
    "originator", "pave";
    "version", "0.155.1";
    "Accept", "application/json" ] in
  let http, calls = fixed_http (List.hd codex_urls) codex_headers
    (Ok (200, {|{"models":[{"slug":"gpt-6-sol"},{"slug":"internal","visibility":"hide"},{"id":"gpt-5.1-codex"},{"slug":"gpt-6-sol"}]}|})) in
  expect_models ["gpt-6-sol"; "gpt-5.1-codex"]
    (discover ~http ~provider:"openai-codex"
      ~credential:codex_credential ());
  assert (!calls = 1);
  let calls = ref 0 in
  let http ~url ~headers =
    assert (headers = codex_headers);
    incr calls;
    if !calls = 1 then (assert (url = List.hd codex_urls); Ok (404, ""))
    else (assert (url = List.nth codex_urls 1);
      Ok (200, {|{"data":[{"slug":"gpt-6-luna"}]}|})) in
  expect_models ["gpt-6-luna"]
    (discover ~http ~provider:"openai-codex"
      ~credential:codex_credential ());
  assert (!calls = 2);
  let http, calls = fixed_http (List.hd codex_urls) codex_headers
    (Ok (401, "revoked")) in
  expect_error unavailable (discover ~http ~provider:"openai-codex"
    ~credential:codex_credential ());
  assert (!calls = 1);
  let http, calls = fixed_http (List.hd codex_urls) codex_headers
    (Ok (302, {|{"location":"https://untrusted.example/models"}|})) in
  expect_error unavailable (discover ~http ~provider:"openai-codex"
    ~credential:codex_credential ());
  assert (!calls = 1);
  let http, _ = fixed_http (List.hd codex_urls) codex_headers
    (Ok (200, {|{"models":[{"slug":12}]}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"openai-codex"
    ~credential:codex_credential ());
  let unused ~url:_ ~headers:_ = failwith "invalid credentials initiated a request" in
  expect_error no_credential (discover ~http:unused ~provider:"openai" ());
  expect_error no_credential (discover ~http:unused ~provider:"github-copilot" ());
  expect_error wrong_credential
    (discover ~http:unused ~provider:"github-copilot" ~credential:(Api_key openai_key) ());
  expect_error wrong_credential
    (discover ~http:unused ~provider:"openai" ~credential:(Copilot_oauth github_token) ());
  expect_error wrong_credential
    (discover ~http:unused ~provider:"ollama" ~credential:(Api_key openai_key) ());
  expect_error wrong_credential
    (discover ~http:unused ~provider:"openai" ~credential:(Api_key "bad\nheader") ());
  expect_error no_credential
    (discover ~http:unused ~provider:"openai-codex" ());
  expect_error wrong_credential
    (discover ~http:unused ~provider:"openai-codex"
       ~credential:(Copilot_oauth github_token) ());
  expect_error wrong_credential
    (discover ~http:unused ~provider:"openai-codex"
       ~credential:(Codex_oauth ("bad\nheader", "account-123")) ());
  let http, _ = fixed_http openai_url openai_headers (Ok (200, {|{"data":[{"id":"ok"}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"openai" ~credential:(Api_key openai_key) ());
  let http, _ = fixed_http openai_url openai_headers (Ok (200, {|{"data":[{"id":12}]}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"openai" ~credential:(Api_key openai_key) ());
  let http, _ = fixed_http ollama_url [] (Ok (200, {|{"models":{}}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"ollama" ());
  let http, _ = fixed_http google_url gemini_headers (Ok (200,
    {|{"models":[{"name":"models/a","supportedGenerationMethods":[]}],"nextPageToken":"bad\npage"}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"google" ~credential:(Api_key gemini_key) ());
  let http, _ = fixed_http openai_url openai_headers
    (Error (Transport_error "network unavailable")) in
  expect_error (function Transport_error _ -> true | _ -> false)
    (discover ~http ~provider:"openai" ~credential:(Api_key openai_key) ());
  let http, _ = fixed_http ollama_url []
    (Ok (200, String.make (max_response_bytes + 1) 'x')) in
  expect_error is_invalid_response (discover ~http ~provider:"ollama" ());
  let http, _ = fixed_http copilot_url copilot_headers (Ok (401,
    {|{"error":{"message":"ghu_private-oauth denied"}}|})) in
  let failure = discover ~http ~provider:"github-copilot"
    ~credential:(Copilot_oauth github_token) () in
  expect_error unavailable failure;
  (match failure with
  | Error reason ->
      assert (Discovery.message reason =
        "Model discovery access denied; check the account or API key")
  | _ -> assert false);
  (* Redirects from an authenticated endpoint are failures, not follow-up calls
     with credentials attached to a Location: supplied by untrusted JSON/HTTP. *)
  let http, calls = fixed_http copilot_url copilot_headers (Ok (302,
    {|{"Location":"https://github.com.attacker.example/steal"}|})) in
  expect_error unavailable (discover ~http ~provider:"github-copilot"
    ~credential:(Copilot_oauth github_token) ());
  assert (!calls = 1);
  let http, calls = fixed_http openai_url openai_headers (Ok (200,
    {|{"data":[{"id":"gpt-4.1"}],"next":"https://attacker.example/models"}|})) in
  expect_models ["gpt-4.1"]
    (discover ~http ~provider:"openai" ~credential:(Api_key openai_key) ());
  assert (!calls = 1);
  let http, calls = fixed_http google_url gemini_headers (Ok (200,
    {|{"models":[{"name":"models/a","supportedGenerationMethods":["generateContent"]}],"nextPageToken":"repeat"}|})) in
  let http ~url ~headers =
    if !calls = 0 then http ~url ~headers
    else (
      assert (url = google_url ^ "?pageToken=repeat");
      assert (headers = gemini_headers);
      Ok (200, {|{"models":[],"nextPageToken":"repeat"}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"google" ~credential:(Api_key gemini_key) ());
  print_endline "credentialed model discovery: ok"
