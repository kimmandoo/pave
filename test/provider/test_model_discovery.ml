module Discovery = Pave.Model_discovery
let expect_models expected = function
  | Ok listing when Discovery.model_ids listing = expected -> ()
  | Ok listing ->
      failwith ("unexpected models: " ^
        String.concat ", " (Discovery.model_ids listing))
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
let copilot_oauth access =
  Discovery.OAuth { service = "github-copilot"; access; account_id = None;
    selection_id = None }
let copilot_local_oauth access selection_id =
  Discovery.OAuth { service = "github-copilot"; access; account_id = None;
    selection_id = Some selection_id }
let codex_oauth (access, account_id) =
  Discovery.OAuth { service = "openai-codex"; access;
    account_id = Some account_id; selection_id = None }

let () =
  let open Discovery in
  assert (message Missing_credential =
    "No credential configured for model listing; sign in or configure an API key");
  assert (message (Unsupported_provider "fixture") =
    "Model listing unsupported for fixture");
  assert (String.starts_with ~prefix:
    "Model listing connection failed or is offline:"
    (message (Transport_error "network unavailable")));

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
    {|{"object":"list","data":[{"id":"gpt-4.1","object":"model"},{"id":"o3"}]}|})) in
  expect_models ["gpt-4.1"; "o3"]
    (discover ~http ~provider:"openai" ~credential:(Api_key openai_key) ());
  assert (!calls = 1);
  let duplicate_http, _ = fixed_http openai_url openai_headers (Ok (200,
    {|{"data":[{"id":"same"},{"id":"same"}]}|})) in
  expect_error is_invalid_response
    (discover ~http:duplicate_http ~provider:"openai"
      ~credential:(Api_key openai_key) ());

  let generations = ref 0 in
  let refresh_http ~url ~headers =
    incr generations;
    assert (url = openai_url && headers = openai_headers);
    Ok (200, Printf.sprintf {|{"data":[{"id":"fresh-%d"}]}|} !generations) in
  let refresh () =
    discover ~http:refresh_http ~provider:"openai"
      ~credential:(Api_key openai_key) () in
  expect_models ["fresh-1"] (refresh ());
  expect_models ["fresh-2"] (refresh ());
  assert (!generations = 2);
  let http, _ = fixed_http ollama_url [] (Ok (200,
    {|{"models":[{"name":"qwen2.5:7b","size":500},{"name":"llama3:latest"}]}|})) in
  expect_models ["qwen2.5:7b"; "llama3:latest"]
    (discover ~http ~provider:"ollama" ());
  List.iter (fun (provider, key) ->
    let endpoint = Pave.Local_compat.endpoint ~provider () in
    let url = Pave.Local_compat.listing_url ~endpoint in
    let descriptor = Option.get (Pave.Provider_catalog.find provider) in
    let route = Option.get (Pave.Provider_catalog.route descriptor "") in
    assert (route.wire = Pave.Provider.Local_chat);
    assert (route.endpoint = endpoint);
    let headers = if key = "" then [] else ["Authorization", "Bearer " ^ key] in
    let http, calls = fixed_http url headers (Ok (200,
      {|{"data":[{"id":"served-now"},{"id":"served-next"}]}|})) in
    expect_models ["served-now"; "served-next"]
      (if key = "" then discover ~http ~provider ()
       else discover ~http ~provider ~credential:(Api_key key) ());
    assert (!calls = 1);
    expect_error wrong_credential (discover ~http ~provider
      ~credential:(copilot_oauth "not-a-local-key") ());
    assert (!calls = 1)) [
      "lm-studio", "";
      "llama.cpp", "local-private";
      "vllm", "vllm-private" ];
  let local_url = Pave.Local_compat.listing_url
    ~endpoint:(Pave.Local_compat.endpoint ~provider:"llama.cpp" ()) in
  let local_headers = ["Authorization", "Bearer local-private"] in
  let http, calls = fixed_http local_url local_headers (Ok (401, "unauthorized")) in
  expect_error unavailable (discover ~http ~provider:"llama.cpp"
    ~credential:(Api_key "local-private") ());
  assert (!calls = 1);
  let http, _ = fixed_http local_url local_headers (Ok (200,
    {|{"data":[{"id":"broken\nmodel"}]}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"llama.cpp"
    ~credential:(Api_key "local-private") ());
  let http, _ = fixed_http local_url local_headers (Ok (200,
    {|{"data":[{"id":"same"},{"id":"same"}]}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"llama.cpp"
    ~credential:(Api_key "local-private") ());
  let unused ~url:_ ~headers:_ = failwith "unsafe local credential sent" in
  expect_error wrong_credential (discover ~http:unused ~provider:"llama.cpp"
    ~credential:(Api_key "bad\nheader") ());
  let http, _ = fixed_http copilot_url copilot_headers (Ok (200,
    {|{"data":[{"id":"gpt-4.1","capabilities":{"type":"chat"}},{"id":"embed","capabilities":{"type":"embeddings"}},{"id":"gpt-4o"}]}|})) in
  expect_models ["gpt-4.1"; "gpt-4o"]
    (discover ~http ~provider:"github-copilot"
       ~credential:(copilot_oauth github_token) ());
  let calls = ref 0 in
  let http ~url ~headers =
    incr calls;
    assert (headers = gemini_headers);
    if !calls = 1 then (
      assert (url = google_url);
      Ok (200, {|{"models":[{"name":"models/gemini-2.5-pro","inputTokenLimit":131072,"supportedGenerationMethods":["generateContent"]},{"name":"models/text-embedding-004","supportedGenerationMethods":["embedContent"]}],"nextPageToken":"next /?"}|}))
    else (
      assert (!calls = 2);
      assert (url = google_url ^ "?pageToken=next%20%2F%3F");
      Ok (200, {|{"models":[{"name":"models/gemini-2.5-pro-next","inputTokenLimit":999999,"supportedGenerationMethods":["generateContent"]},{"name":"models/gemini-2.5-flash","inputTokenLimit":65536,"supportedGenerationMethods":["generateContent"]}]}|})) in
  let google_listing = match
      discover ~http ~provider:"google" ~credential:(Api_key gemini_key) () with
    | Ok listing -> listing
    | Error failure -> failwith (message failure) in
  assert (model_ids google_listing =
    ["models/gemini-2.5-pro"; "models/gemini-2.5-pro-next";
     "models/gemini-2.5-flash"]);
  assert (List.map (fun (model : Pave.Model_catalog.model) ->
    model.identity.provider, model.identity.route, model.identity.account_id,
    model.identity.upstream_id, model.display_name,
    model.capabilities.context_window_tokens) google_listing.models = [
      "google", "generate", None, "models/gemini-2.5-pro", None, Some 131072;
      "google", "generate", None, "models/gemini-2.5-pro-next", None,
        Some 999999;
      "google", "generate", None, "models/gemini-2.5-flash", None, Some 65536 ]);
  assert (google_listing.source.endpoint = Some google_url);
  assert (google_listing.source.id_source =
    Pave.Model_catalog.Pinned_account_listing);
  assert (Option.is_some google_listing.source.retrieved_at);
  assert (!calls = 2);
  let router_key = "sk-or-fixture" in
  let router_headers = ["Authorization", "Bearer " ^ router_key] in
  let http, calls = fixed_http openrouter_url router_headers
    (Ok (200, {|{"data":[{"id":"anthropic/claude-sonnet-4"},{"id":"openai/gpt-6-sol"},{"id":"deepseek/deepseek-chat"}]}|})) in
  expect_models ["anthropic/claude-sonnet-4"; "openai/gpt-6-sol";
    "deepseek/deepseek-chat"]
    (discover ~http ~provider:"openrouter"
      ~credential:(Api_key router_key) ());
  assert (!calls = 1);
  let router_metadata_http, _ = fixed_http openrouter_url router_headers
    (Ok (200, {|{"data":[{"id":"openai/with-metadata","context_length":128000,"top_provider":{"max_completion_tokens":16384}},{"id":"openai/without-metadata","context_length":null,"top_provider":{"max_completion_tokens":null}},{"id":"openai/context-only","context_length":4096}]}|})) in
  (match discover ~http:router_metadata_http ~provider:"openrouter"
      ~credential:(Api_key router_key) () with
   | Error failure -> failwith (message failure)
   | Ok listing ->
       assert (model_ids listing = ["openai/with-metadata";
         "openai/without-metadata"; "openai/context-only"]);
       assert (List.map (fun (model : Pave.Model_catalog.model) ->
         model.identity.provider, model.identity.upstream_id,
         model.capabilities.context_window_tokens,
         model.capabilities.max_output_tokens) listing.models = [
           "openrouter", "openai/with-metadata", Some 128000, Some 16384;
           "openrouter", "openai/without-metadata", None, None;
           "openrouter", "openai/context-only", Some 4096, None]);
       assert (listing.source.endpoint = Some openrouter_url);
       assert (listing.source.id_source =
         Pave.Model_catalog.Pinned_account_listing));
  let router_bad_metadata = [
    {|{"data":[{"id":"bad","context_length":0}]}|};
    {|{"data":[{"id":"bad","context_length":-1}]}|};
    {|{"data":[{"id":"bad","context_length":1.5}]}|};
    {|{"data":[{"id":"bad","context_length":999999999999999999999999999999}]}|};
    {|{"data":[{"id":"bad","top_provider":{"max_completion_tokens":0}}]}|};
    {|{"data":[{"id":"bad","top_provider":{"max_completion_tokens":-1}}]}|};
    {|{"data":[{"id":"bad","top_provider":{"max_completion_tokens":"4096"}}]}|};
    {|{"data":[{"id":"bad","top_provider":[] }]}|};
  ] in
  List.iter (fun body ->
    let http, _ = fixed_http openrouter_url router_headers (Ok (200, body)) in
    expect_error is_invalid_response
      (discover ~http ~provider:"openrouter"
        ~credential:(Api_key router_key) ())) router_bad_metadata;
  let router_duplicate_http, _ = fixed_http openrouter_url router_headers
    (Ok (200, {|{"data":[{"id":"same","context_length":4096},{"id":"same","context_length":8192}]}|})) in
  expect_error is_invalid_response
    (discover ~http:router_duplicate_http ~provider:"openrouter"
      ~credential:(Api_key router_key) ());
  let local_account = "pave-local:0123456789abcdef0123456789abcdef" in
  let http, _ = fixed_http openrouter_url router_headers
    (Ok (200, {|{"data":[{"id":"account-model"}]}|})) in
  (match discover ~http ~provider:"openrouter" ~account_id:local_account
      ~credential:(Account_api_key {
        key = router_key; account_id = Some local_account }) () with
   | Ok listing ->
       assert ((List.hd listing.models).identity.account_id = Some local_account)
   | Error failure -> failwith (message failure));
  let foreign_account = "pave-local:ffffffffffffffffffffffffffffffff" in
  let mismatch_http, mismatch_calls = fixed_http openrouter_url router_headers
    (Ok (200, {|{"data":[{"id":"must-not-list"}]}|})) in
  expect_error wrong_credential
    (discover ~http:mismatch_http ~provider:"openrouter"
      ~account_id:foreign_account
      ~credential:(Account_api_key {
        key = router_key; account_id = Some local_account }) ());
  assert (!mismatch_calls = 0);
  let http, calls = fixed_http copilot_url copilot_headers (Ok (200,
    {|{"data":[{"id":"copilot-account-model"}]}|})) in
  let copilot_account =
    "pave-local:abcdef0123456789abcdef0123456789" in
  let listing = discover ~http ~provider:"github-copilot"
    ~account_id:copilot_account
    ~credential:(copilot_local_oauth github_token copilot_account) () in
  (match listing with
   | Ok listing ->
       assert ((List.hd listing.models).identity.account_id = Some copilot_account)
   | Error failure -> failwith (message failure));
  assert (!calls = 1);
  let key = "new-provider-private-key" in
  let bearer = ["Authorization", "Bearer " ^ key] in
  List.iter (fun (provider, url) ->
    let http, calls = fixed_http url bearer (Ok (200,
      {|{"data":[{"id":"future-chat-1"},{"id":"future-chat-2"}]}|})) in
    expect_models ["future-chat-1"; "future-chat-2"]
      (discover ~http ~provider ~credential:(Api_key key) ());
    assert (!calls = 1);
    expect_error no_credential (discover ~http ~provider ());
    expect_error wrong_credential (discover ~http ~provider
      ~credential:(copilot_oauth key) ());
    assert (!calls = 1)) [
      "deepseek", deepseek_url;
      "groq", groq_url;
      "mistral", mistral_url ];
  let together_key = "private-together" in
  let together_headers = ["Authorization", "Bearer " ^ together_key] in
  let http, calls = fixed_http together_url together_headers (Ok (200,
    {|[{"id":"chat-next/1","type":"chat"},{"id":"embed-next/2","type":"embedding"},{"id":"chat-next/3","type":"chat"}]|})) in
  expect_models ["chat-next/1"; "chat-next/3"]
    (discover ~http ~provider:"together" ~credential:(Api_key together_key) ());
  assert (!calls = 1);
  let http, _ = fixed_http together_url together_headers
    (Ok (200, {|[{"id":"unclassified"}]|})) in
  expect_error is_invalid_response
    (discover ~http ~provider:"together" ~credential:(Api_key together_key) ());
  let http, _ = fixed_http together_url together_headers
    (Ok (200, {|{"data":[{"id":"not-an-array","type":"chat"}]}|})) in
  expect_error is_invalid_response
    (discover ~http ~provider:"together" ~credential:(Api_key together_key) ());
  let http, calls = fixed_http cerebras_url bearer (Ok (200,
    {|{"object":"list","data":[{"id":"live-chat/1"},{"id":"live-chat/2"}]}|})) in
  expect_models ["live-chat/1"; "live-chat/2"]
    (discover ~http ~provider:"cerebras" ~credential:(Api_key key) ());
  assert (!calls = 1);
  let http, calls = fixed_http venice_url bearer (Ok (200,
    {|{"object":"list","type":"text","data":[{"id":"live-chat/1","type":"text","model_spec":{"capabilities":{"supportsFunctionCalling":true}}},{"id":"chat-no-tools","type":"text","model_spec":{"capabilities":{"supportsFunctionCalling":false}}},{"id":"image-model","type":"image"},{"id":"live-chat/2","type":"text","model_spec":{"capabilities":{"supportsFunctionCalling":true}}}]}|})) in
  expect_models ["live-chat/1"; "live-chat/2"]
    (discover ~http ~provider:"venice" ~credential:(Api_key key) ());
  assert (!calls = 1);
  let http, _ = fixed_http venice_url bearer (Ok (200,
    {|{"data":[{"id":"unsafe-chat","type":"text","model_spec":{"capabilities":{}}}]}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"venice"
    ~credential:(Api_key key) ());
  let unused ~url:_ ~headers:_ = failwith "unauthorized listing attempted" in
  List.iter (fun provider ->
    expect_error no_credential (discover ~http:unused ~provider ());
    expect_error wrong_credential (discover ~http:unused ~provider
      ~credential:(copilot_oauth key) ())) ["cerebras"; "venice"];
  expect_error no_credential (discover ~http:unused ~provider:"together" ());
  expect_error wrong_credential (discover ~http:unused ~provider:"together"
    ~credential:(codex_oauth (together_key, "account")) ());
  let http, calls = fixed_http venice_url bearer
    (Ok (302, {|{"Location":"https://attacker.example/models"}|})) in
  expect_error unavailable (discover ~http ~provider:"venice"
    ~credential:(Api_key key) ());
  assert (!calls = 1);
  let http, calls = fixed_http deepinfra_url bearer (Ok (200,
    {|{"data":[{"id":"chat/one","metadata":{"tags":["chat","reasoning"]}},{"id":"image/one","metadata":{"tags":["image-gen"]}},{"id":"chat/two","metadata":{"tags":["chat"]}}]}|})) in
  expect_models ["chat/one"; "chat/two"]
    (discover ~http ~provider:"deepinfra" ~credential:(Api_key key) ());
  assert (!calls = 1);
  let http, _ = fixed_http deepinfra_url bearer (Ok (200,
    {|{"data":[{"id":"unknown","metadata":{}}]}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"deepinfra"
    ~credential:(Api_key key) ());
  let http, calls = fixed_http baseten_url bearer (Ok (200,
    {|{"data":[{"id":"tool/one","supported_features":["tools","reasoning"]},{"id":"embed/one","supported_features":["embeddings"]},{"id":"tool/two","supported_features":["tools"]}]}|})) in
  expect_models ["tool/one"; "tool/two"]
    (discover ~http ~provider:"baseten" ~credential:(Api_key key) ());
  assert (!calls = 1);
  let http, _ = fixed_http baseten_url bearer (Ok (200,
    {|{"data":[{"id":"unknown"}]}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"baseten"
    ~credential:(Api_key key) ());
  let hf_key = "private-huggingface" in
  let hf_headers = ["Authorization", "Bearer " ^ hf_key] in
  let http, calls = fixed_http huggingface_url hf_headers (Ok (200,
    {|{"data":[{"id":"vendor/chat-with-tools","providers":[{"provider":"novita","status":"live","supports_tools":true}]},{"id":"vendor/text-only","providers":[{"provider":"novita","status":"live","supports_tools":false}]},{"id":"vendor/unavailable","providers":[{"provider":"novita","status":"error","supports_tools":true}]},{"id":"vendor/other-chat","providers":[{"provider":"together","status":"live","supports_tools":true}]}]}|})) in
  expect_models ["vendor/chat-with-tools"; "vendor/other-chat"]
    (discover ~http ~provider:"huggingface" ~credential:(Api_key hf_key) ());
  assert (!calls = 1);
  let nano_key = "private-nanogpt" in
  let nano_headers = ["Authorization", "Bearer " ^ nano_key] in
  let http, calls = fixed_http nanogpt_url nano_headers (Ok (200,
    {|{"object":"list","data":[{"id":"vendor/chat-tool-1","capabilities":{"tool_calling":true}},{"id":"vendor/text-only","capabilities":{"tool_calling":false}},{"id":"vendor/chat-tool-2","capabilities":{"tool_calling":true}},{"id":"vendor/chat-tool-3","capabilities":{"tool_calling":true}}]}|})) in
  expect_models ["vendor/chat-tool-1"; "vendor/chat-tool-2";
    "vendor/chat-tool-3"]
    (discover ~http ~provider:"nanogpt" ~credential:(Api_key nano_key) ());
  assert (!calls = 1);
  List.iter (fun (provider, url, headers) ->
    let http, calls = fixed_http url headers (Ok (200,
      {|{"data":[{"id":"unknown"}]}|})) in
    expect_error is_invalid_response
      (discover ~http ~provider ~credential:(Api_key
        (if provider = "huggingface" then hf_key else nano_key)) ());
    assert (!calls = 1);
    let unused ~url:_ ~headers:_ = failwith "unsafe discovery credential sent" in
    expect_error no_credential (discover ~http:unused ~provider ());
    expect_error wrong_credential (discover ~http:unused ~provider
      ~credential:(copilot_oauth "foreign-oauth") ());
    expect_error wrong_credential (discover ~http:unused ~provider
      ~credential:(Api_key "unsafe\nkey") ());
    let http, calls = fixed_http url headers (Ok (302,
      {|{"Location":"https://untrusted.example/models"}|})) in
    expect_error unavailable
      (discover ~http ~provider ~credential:(Api_key
        (if provider = "huggingface" then hf_key else nano_key)) ());
    assert (!calls = 1))
    ["huggingface", huggingface_url, hf_headers;
     "nanogpt", nanogpt_url, nano_headers];
  let fireworks_first =
    fireworks_url ^ "?pageSize=200&filter=supports_serverless%3Dtrue" in
  let calls = ref 0 in
  let http ~url ~headers =
    incr calls;
    assert (headers = bearer);
    if !calls = 1 then (
      assert (url = fireworks_first);
      Ok (200, {|{"models":[{"name":"accounts/fireworks/models/tool-one","supportsServerless":true,"supportsTools":true,"state":"READY"},{"name":"accounts/fireworks/models/embed-one","supportsServerless":true,"supportsTools":false},{"name":"accounts/other/models/private","supportsServerless":true,"supportsTools":true}],"nextPageToken":"next /?"}|}))
    else (
      assert (!calls = 2);
      assert (url = fireworks_first ^ "&pageToken=next%20%2F%3F");
      Ok (200, {|{"models":[{"name":"accounts/fireworks/models/tool-two","supportsServerless":true,"supportsTools":true},{"name":"accounts/fireworks/models/tool-three","supportsServerless":true,"supportsTools":true},{"name":"accounts/fireworks/models/not-serving","supportsServerless":false,"supportsTools":true}]}|})) in
  expect_models ["accounts/fireworks/models/tool-one";
    "accounts/fireworks/models/tool-two"; "accounts/fireworks/models/tool-three"]
    (discover ~http ~provider:"fireworks" ~credential:(Api_key key) ());
  assert (!calls = 2);
  let duplicate_calls = ref 0 in
  let duplicate_pages ~url ~headers =
    incr duplicate_calls;
    assert (headers = bearer);
    if !duplicate_calls = 1 then (
      assert (url = fireworks_first);
      Ok (200, {|{"models":[{"name":"accounts/fireworks/models/tool-one","supportsServerless":true,"supportsTools":true}],"nextPageToken":"next /?"}|}))
    else (
      assert (!duplicate_calls = 2);
      assert (url = fireworks_first ^ "&pageToken=next%20%2F%3F");
      Ok (200, {|{"models":[{"name":"accounts/fireworks/models/tool-one","supportsServerless":true,"supportsTools":true}]}|})) in
  expect_error is_invalid_response
    (discover ~http:duplicate_pages ~provider:"fireworks"
      ~credential:(Api_key key) ());
  assert (!duplicate_calls = 2);
  let http, _ = fixed_http fireworks_first bearer (Ok (200,
    {|{"models":[{"name":"accounts/fireworks/models/tool-one","supportsServerless":true,"supportsTools":true}],"nextPageToken":"bad\npage"}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"fireworks"
    ~credential:(Api_key key) ());
  let unused ~url:_ ~headers:_ = failwith "unauthorized new provider listing attempted" in
  List.iter (fun provider ->
    expect_error no_credential (discover ~http:unused ~provider ());
    expect_error wrong_credential (discover ~http:unused ~provider
      ~credential:(codex_oauth (key, "account")) ());
    expect_error wrong_credential (discover ~http:unused ~provider
      ~credential:(Api_key "unsafe\nkey") ()))
    ["deepinfra"; "fireworks"; "baseten"];
  List.iter (fun (provider, url) ->
    let http, calls = fixed_http url bearer (Ok (401, "private")) in
    expect_error unavailable (discover ~http ~provider
      ~credential:(Api_key key) ());
    assert (!calls = 1);
    let http, calls = fixed_http url bearer (Ok (302,
      {|{"Location":"https://attacker.example/models"}|})) in
    expect_error unavailable (discover ~http ~provider
      ~credential:(Api_key key) ());
    assert (!calls = 1)) [
      "deepinfra", deepinfra_url;
      "fireworks", fireworks_first;
      "baseten", baseten_url ];
  let headers = ["x-api-key", key; "anthropic-version", "2023-06-01";
    "anthropic-beta", "compact-2026-09-04"] in
  let calls = ref 0 in
  let http ~url ~headers:actual =
    incr calls;
    assert (actual = headers);
    if !calls = 1 then (
      assert (url = anthropic_url ^ "?limit=100");
      Ok (200, {|{"data":[{"id":"future-Claude/1","display_name":"Claude Future 1","max_input_tokens":200000,"capabilities":{"compaction":{"supported":true,"summarize":{"supported":true}}}},{"id":"next/?"}],"has_more":true,"last_id":"next/?"}|}))
    else (
      assert (url = anthropic_url ^ "?limit=100&after_id=next%2F%3F");
      Ok (200, {|{"data":[{"id":"future-Claude/2","display_name":"Claude Future 2","max_input_tokens":196000,"capabilities":{"compaction":{"supported":false}}},{"id":"future-Claude/3","display_name":"Claude Future 3","max_input_tokens":192000,"capabilities":{"compaction":{"supported":false}}}],"has_more":false,"last_id":"future-Claude/3"}|})) in
  let anthropic_listing = match
      discover ~http ~provider:"anthropic" ~credential:(Api_key key) () with
    | Ok listing -> listing
    | Error failure -> failwith (message failure) in
  assert (model_ids anthropic_listing =
    ["future-Claude/1"; "next/?"; "future-Claude/2"; "future-Claude/3"]);
  assert (List.map (fun (model : Pave.Model_catalog.model) ->
    model.identity.provider, model.identity.route, model.identity.account_id,
    model.identity.upstream_id, model.display_name,
    model.capabilities.context_window_tokens,
    model.capabilities.native_compaction_supported) anthropic_listing.models = [
      "anthropic", "messages", None, "future-Claude/1", Some "Claude Future 1",
        Some 200000, Some true;
      "anthropic", "messages", None, "next/?", None, None, None;
      "anthropic", "messages", None, "future-Claude/2", Some "Claude Future 2",
        Some 196000, Some false;
      "anthropic", "messages", None, "future-Claude/3", Some "Claude Future 3",
        Some 192000, Some false ]);
  assert (anthropic_listing.source.endpoint = Some anthropic_url);
  assert (anthropic_listing.source.id_source =
    Pave.Model_catalog.Pinned_account_listing);
  let duplicate_page_calls = ref 0 in
  let duplicate_anthropic_pages ~url ~headers:actual_headers =
    incr duplicate_page_calls;
    assert (actual_headers = headers);
    if !duplicate_page_calls = 1 then (
      assert (url = anthropic_url ^ "?limit=100");
      Ok (200, {|{"data":[{"id":"same"}],"has_more":true,"last_id":"same"}|}))
    else (
      assert (!duplicate_page_calls = 2);
      assert (url = anthropic_url ^ "?limit=100&after_id=same");
      Ok (200, {|{"data":[{"id":"same"}],"has_more":false,"last_id":"same"}|})) in
  expect_error is_invalid_response
    (discover ~http:duplicate_anthropic_pages ~provider:"anthropic"
      ~credential:(Api_key key) ());
  assert (!duplicate_page_calls = 2);
  assert (Option.is_some anthropic_listing.source.retrieved_at);
  assert (!calls = 2);
  let http, _ = fixed_http (anthropic_url ^ "?limit=100") headers
    (Ok (200, {|{"data":[{"id":"partial"}],"has_more":true,"last_id":"mismatch"}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"anthropic"
    ~credential:(Api_key key) ());
  let http, _ = fixed_http (anthropic_url ^ "?limit=100") headers
    (Ok (200, {|{"data":[{"id":"partial"}]}|})) in
  expect_error is_invalid_response (discover ~http ~provider:"anthropic"
    ~credential:(Api_key key) ());
  let http, calls = fixed_http openrouter_url router_headers
    (Ok (403, "forbidden")) in
  expect_error unavailable (discover ~http ~provider:"openrouter"
    ~credential:(Api_key router_key) ());
  assert (!calls = 1);
  let http, calls = fixed_http openrouter_url router_headers
    (Ok (302, {|{"location":"https://other.example/"}|})) in
  expect_error unavailable (discover ~http ~provider:"openrouter"
    ~credential:(Api_key router_key) ());
  assert (!calls = 1);
  let codex_credential = codex_oauth ("private-codex", "account-123") in
  let codex_headers = [
    "Authorization", "Bearer private-codex";
    "chatgpt-account-id", "account-123";
    "OpenAI-Beta", "responses=experimental";
    "originator", "pave";
    "version", "0.155.1";
    "Accept", "application/json" ] in
  let http, calls = fixed_http (List.hd codex_urls) codex_headers
    (Ok (200, {|{"models":[{"slug":"gpt-6-sol","context_window":196608},{"slug":"internal","visibility":"hide","context_window":1000000},{"id":"gpt-5.1-codex"},{"slug":"gpt-6-sol-next","context_window":999999}]}|})) in
  let codex_listing = match
      discover ~http ~provider:"openai-codex"
        ~credential:codex_credential () with
    | Ok listing -> listing
    | Error failure -> failwith (message failure) in
  assert (model_ids codex_listing =
    ["gpt-6-sol"; "gpt-5.1-codex"; "gpt-6-sol-next"]);
  assert (List.map (fun (model : Pave.Model_catalog.model) ->
    model.identity.provider, model.identity.route, model.identity.account_id,
    model.identity.upstream_id, model.capabilities.context_window_tokens)
      codex_listing.models = [
        "openai-codex", "responses", Some "account-123", "gpt-6-sol",
          Some 196608;
        "openai-codex", "responses", Some "account-123", "gpt-5.1-codex",
          None;
        "openai-codex", "responses", Some "account-123", "gpt-6-sol-next",
          Some 999999 ]);
  assert (codex_listing.source.id_source =
    Pave.Model_catalog.Pinned_account_listing);
  assert (Option.is_some codex_listing.source.retrieved_at);
  assert (!calls = 1);
  let duplicate_codex, _ = fixed_http (List.hd codex_urls) codex_headers
    (Ok (200, {|{"models":[{"slug":"hidden-model","visibility":"hidden"},{"slug":"hidden-model"}]}|})) in
  expect_error is_invalid_response
    (discover ~http:duplicate_codex ~provider:"openai-codex"
      ~credential:codex_credential ());
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
    (discover ~http:unused ~provider:"openai" ~credential:(copilot_oauth github_token) ());
  expect_error wrong_credential
    (discover ~http:unused ~provider:"ollama" ~credential:(Api_key openai_key) ());
  expect_error wrong_credential
    (discover ~http:unused ~provider:"openai" ~credential:(Api_key "bad\nheader") ());
  expect_error no_credential
    (discover ~http:unused ~provider:"openai-codex" ());
  expect_error wrong_credential
    (discover ~http:unused ~provider:"openai-codex"
       ~credential:(copilot_oauth github_token) ());
  expect_error wrong_credential
    (discover ~http:unused ~provider:"openai-codex"
       ~credential:(codex_oauth ("bad\nheader", "account-123")) ());
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
    ~credential:(copilot_oauth github_token) () in
  expect_error unavailable failure;
  (match failure with
  | Error reason ->
      assert (Discovery.message reason =
        "Provider rejected the credential (possibly expired or revoked); sign in again or check the API key")
  | _ -> assert false);
  let http, _ = fixed_http copilot_url copilot_headers
    (Ok (403, {|{"error":"insufficient_scope"}|})) in
  (match discover ~http ~provider:"github-copilot"
      ~credential:(copilot_oauth github_token) () with
   | Error (Http_error 403 as reason) ->
       assert (Discovery.message reason =
         "Provider denied access; check the required scope, account entitlement, or policy")
   | _ -> failwith "permission denial was not classified");
  (* Redirects from an authenticated endpoint are failures, not follow-up calls
     with credentials attached to a Location: supplied by untrusted JSON/HTTP. *)
  let http, calls = fixed_http copilot_url copilot_headers (Ok (302,
    {|{"Location":"https://github.com.attacker.example/steal"}|})) in
  expect_error unavailable (discover ~http ~provider:"github-copilot"
    ~credential:(copilot_oauth github_token) ());
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
  let abliteration_headers =
    ["Authorization", "Bearer private-abliteration"] in
  let http, calls = fixed_http abliteration_url abliteration_headers (Ok (200,
    {|{"object":"list","data":[{"id":"future-ablit-701"},{"id":"future-ablit-702"},{"id":"future-ablit-703"}]}|})) in
  expect_models ["future-ablit-701"; "future-ablit-702"; "future-ablit-703"]
    (discover ~http ~provider:"abliteration"
      ~credential:(Api_key "private-abliteration") ());
  assert (!calls = 1);
  let duplicate_http, _ = fixed_http abliteration_url abliteration_headers
    (Ok (200, {|{"object":"list","data":[{"id":"same"},{"id":"same"}]}|})) in
  expect_error is_invalid_response
    (discover ~http:duplicate_http ~provider:"abliteration"
      ~credential:(Api_key "private-abliteration") ());
  let unused ~url:_ ~headers:_ = failwith "invalid key reached vendor endpoint" in
  expect_error wrong_credential
    (discover ~http:unused ~provider:"abliteration"
      ~credential:(Api_key "injected\r\nHeader: stolen") ());
  let http, _ = fixed_http abliteration_url abliteration_headers (Ok (200,
    {|{"object":"list","data":[{"id":"future-ablit-701"},{"id":null}]}|})) in
  expect_error is_invalid_response
    (discover ~http ~provider:"abliteration"
      ~credential:(Api_key "private-abliteration") ());
  let gmi_headers = ["Authorization", "Bearer private-gmi"] in
  let http, calls = fixed_http gmi_cloud_url gmi_headers (Ok (200,
    {|{"object":"list","data":[{"id":"account-model-alpha"},{"id":"account-model-beta"}]}|})) in
  expect_models ["account-model-alpha"; "account-model-beta"]
    (discover ~http ~provider:"gmi-cloud"
      ~credential:(Api_key "private-gmi") ());
  assert (!calls = 1);
  let dynamic_provider = Pave.Custom_provider.parse
    (Yojson.Basic.from_string
      {|{"id":"custom-dynamic","display_name":"Custom Dynamic","default_route":"chat","routes":[{"name":"chat","api":"openai-chat","endpoint":"https://api.example.test/v1/chat/completions","account_id":"team-7","api_key_env":"CUSTOM_DISCOVERY_KEY","models_endpoint":"https://api.example.test/v1/models"}]}|}) in
  let explicit_provider = Pave.Custom_provider.parse
    (Yojson.Basic.from_string
      {|{"id":"custom-explicit","display_name":"Custom Explicit","default_route":"chat","routes":[{"name":"chat","api":"openai-chat","endpoint":"https://manual.example.test/v1/chat/completions","models":[{"id":"exact-model","tools":true},{"id":"text-only","tools":false}]}]}|}) in
  let no_auth_provider = Pave.Custom_provider.parse
    (Yojson.Basic.from_string
      {|{"id":"custom-anonymous","display_name":"Custom Anonymous","default_route":"chat","routes":[{"name":"chat","api":"openai-chat","endpoint":"https://public.example.test/v1/chat/completions","models_endpoint":"https://public.example.test/v1/models"}]}|}) in
  let registry = match Pave.Provider_catalog.create_registry
      [dynamic_provider; explicit_provider; no_auth_provider] with
    | Ok registry -> registry
    | Error message -> failwith message in
  let dynamic_key = "private-custom-discovery" in
  let dynamic_route = List.hd dynamic_provider.routes in
  let dynamic_revision = Pave.Custom_provider.fingerprint dynamic_route in
  let dynamic_url = Option.get dynamic_route.models_endpoint in
  let dynamic_headers = ["Authorization", "Bearer " ^ dynamic_key] in
  let http, calls = fixed_http dynamic_url dynamic_headers (Ok (200,
    {|{"object":"list","data":[{"id":"model/future"}]}|})) in
  let custom_listing = match discover ~http ~registry
      ~provider:"custom-dynamic" ~route_name:"chat"
      ~credential:(Api_key dynamic_key) () with
    | Ok listing -> listing
    | Error error -> failwith (message error) in
  assert (model_ids custom_listing = ["model/future"]);
  assert (!calls = 1);
  assert (custom_listing.source.id_source =
    Pave.Model_catalog.Pinned_account_listing);
  assert (custom_listing.source.endpoint = Some dynamic_url);
  assert (Option.is_some custom_listing.source.retrieved_at);
  let dynamic_model = List.hd custom_listing.models in
  assert (dynamic_model.identity.provider = "custom-dynamic");
  assert (dynamic_model.identity.route = "chat");
  assert (dynamic_model.identity.account_id = Some "team-7");
  assert (dynamic_model.identity.config_revision = Some dynamic_revision);
  assert (model_supports_endpoint ~registry ~provider:"custom-dynamic"
    dynamic_model ~endpoint:dynamic_route.endpoint);
  let no_request ~url:_ ~headers:_ = failwith "custom account mismatch made a request" in
  expect_error wrong_credential
    (discover ~http:no_request ~registry ~provider:"custom-dynamic"
      ~route_name:"chat" ~account_id:"other-team"
      ~credential:(Api_key dynamic_key) ());
  expect_error no_credential
    (discover ~http:no_request ~registry ~provider:"custom-dynamic"
      ~route_name:"chat" ());
  expect_error wrong_credential
    (discover ~http:no_request ~registry ~provider:"custom-dynamic"
      ~route_name:"chat" ~credential:(copilot_oauth dynamic_key) ());
  let redirect_http, redirect_calls = fixed_http dynamic_url dynamic_headers
    (Ok (302, {|{"Location":"https://attacker.example/models"}|})) in
  expect_error unavailable
    (discover ~http:redirect_http ~registry ~provider:"custom-dynamic"
      ~route_name:"chat" ~credential:(Api_key dynamic_key) ());
  assert (!redirect_calls = 1);
  let malformed_http, _ = fixed_http dynamic_url dynamic_headers
    (Ok (200, {|{"object":"list","data":[{"id":"same"},{"id":"same"}]}|})) in
  expect_error is_invalid_response
    (discover ~http:malformed_http ~registry ~provider:"custom-dynamic"
      ~route_name:"chat" ~credential:(Api_key dynamic_key) ());
  let explicit_listing = match discover ~registry
      ~provider:"custom-explicit" ~route_name:"chat" () with
    | Ok listing -> listing
    | Error error -> failwith (message error) in
  assert (model_ids explicit_listing = ["exact-model"; "text-only"]);
  assert (explicit_listing.source.id_source =
    Pave.Model_catalog.Explicit_user_input);
  assert (List.map (fun (model : Pave.Model_catalog.model) ->
    model.capabilities.tools) explicit_listing.models =
      [Some true; Some false]);
  assert (List.map (fun (model : Pave.Model_catalog.model) ->
    model.provenance.capability_source) explicit_listing.models =
      [Some Pave.Model_catalog.Explicit_user_input;
       Some Pave.Model_catalog.Explicit_user_input]);
  assert ((List.hd explicit_listing.models).identity.config_revision =
    Some (Pave.Custom_provider.fingerprint
      (List.hd explicit_provider.routes)));
  assert (Pave.Provider_catalog.unclassified_models ~registry
    "custom-explicit");
  let anonymous_route = List.hd no_auth_provider.routes in
  let anonymous_url = Option.get anonymous_route.models_endpoint in
  let anonymous_http, anonymous_calls = fixed_http anonymous_url [] (Ok (200,
    {|{"object":"list","data":[{"id":"public-model"}]}|})) in
  let anonymous = match discover ~http:anonymous_http ~registry
      ~provider:"custom-anonymous" () with
    | Ok listing -> listing
    | Error error -> failwith (message error) in
  assert (!anonymous_calls = 1);
  assert (model_ids anonymous = ["public-model"]);
  expect_error wrong_credential
    (discover ~http:no_request ~registry ~provider:"custom-anonymous"
      ~credential:(Api_key "unexpected") ());
  expect_error (function Unsupported_route _ -> true | _ -> false)
    (discover ~http:no_request ~registry ~provider:"custom-dynamic"
      ~route_name:"unknown" ~credential:(Api_key dynamic_key) ());
  let identity_custom = List.hd explicit_listing.models in
  assert (not (Pave.Provider_catalog.unclassified_models "openai"));
  assert (identity_custom.identity.config_revision <> None);
  print_endline "credentialed model discovery: ok"
