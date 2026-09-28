module D = Pave.Devin_api
module P = Pave.Protocol
let fail what = failwith ("Devin fixture: " ^ what)
let expect_error predicate = function
  | Error error when predicate error -> ()
  | _ -> fail "expected protocol rejection"
let key = "account-session-token"
let model_uid = "live-discovered-model-uid"
let router_uid = "account-router-uid"
let assigned_uid = "MODEL_GOOGLE_GEMINI_3_7_FLASH"
let cascade = D.cascade_id [P.user "What is six times seven?"]
let call_id = "call_devin_1"
let tool = `Assoc ["type", `String "function"; "function", `Assoc [
  "name", `String "multiply_seven";
  "description", `String "Multiply a number by seven";
  "parameters", `Assoc ["type", `String "object";
    "properties", `Assoc [
      "number", `Assoc ["type", `String "integer"];
      "hint", `Assoc ["type", `List [`String "string"; `String "null"]]];
    "required", `List [`String "number"]]]]
let lookup field fs = D.text field fs
let bytes field fs = match D.entries field fs with
  | `Bytes bytes :: _ -> bytes | _ -> fail ("missing protobuf field " ^ string_of_int field)
let object_field field fs = D.fields (bytes field fs)
let assert_metadata ~jwt ~discovery fs =
  let metadata = object_field 1 fs in
  assert (lookup 3 metadata = "devin-session-token$" ^ key);
  assert (lookup 21 metadata = jwt);
  if discovery then (
    assert (lookup 1 metadata = "chisel");
    assert (D.entries 30 metadata <> []))
  else (
    assert (lookup 1 metadata = "devin-cli");
    assert (lookup 28 metadata = "chisel"))
let response f = D.buf f
let model_config ?(router=false) ?context_window ?tokenizer
    ?(max_tokens=Some 4096) ?(feature_metadata=true)
    ?(supports_tools=true) id label =
  response (fun b ->
    D.string b 1 label; D.string b 22 id;
    D.bytes b 23 (response (fun info ->
      Option.iter (D.number info 4) context_window;
      Option.iter (D.string info 5) tokenizer;
      Option.iter (D.number info 13) max_tokens;
      if router then D.number info 22 3;
      if feature_metadata then
        D.bytes info 6 (response (fun features ->
          D.boolean features 12 supports_tools; D.boolean features 21 true)))))
let stream_reply ?(gzip=false) payload =
  let value = if gzip then D.gzip payload else payload in
  D.frame (if gzip then 1 else 0) value ^ D.frame 2 "{}"
let initial_reply =
  let first = response (fun b ->
    D.string b 1 "server-message-id";
    D.string b 9 "need calculator"; D.string b 10 "opaque-signature";
    D.bytes b 6 (response (fun call ->
      D.string call 1 call_id;
      D.string call 2 "multiply_seven";
      D.string call 3 {|{"number":|}))) in
  let continuation = response (fun b ->
    D.bytes b 6 (response (fun call -> D.string call 3 "6}"));
    D.number b 5 10) in
  D.frame 1 (D.gzip first) ^ D.frame 0 continuation ^ D.frame 2 "{}"
let make_reply product = stream_reply (response (fun b ->
  D.string b 1 "server-response-two";
  D.string b 3 (Printf.sprintf "Six times seven is %d." product);
  D.number b 5 2;
  D.bytes b 7 (response (fun usage ->
    D.number usage 2 76; D.number usage 3 12))))
let () =
  assert (Pave.Devin_binary_http.curl_path = "/usr/bin/curl");
  assert (Array.to_list Pave.Devin_binary_http.curl_environment =
    ["LANG=C"; "LC_ALL=C"]);

  let steps = ref [] in
  let seen url = steps := url :: !steps in
  let http ~url ~headers ~body ~on_chunk =
    seen url;
    if url = D.models_url then (
      assert (List.mem ("Content-Type", "application/proto") headers);
      let meta = object_field 1 (D.fields body) in
      if lookup 1 meta = "chisel" then (
        assert_metadata ~jwt:"" ~discovery:true (D.fields body);
        let wire = response (fun b ->
          D.bytes b 1 (model_config ~context_window:131072
            ~tokenizer:"devin-tokenizer-v1" model_uid "Model from account");
          D.bytes b 1 (model_config ~router:true ~feature_metadata:false
            router_uid "Router from account");
          D.bytes b 1 (model_config ~max_tokens:None
            ~feature_metadata:false ~tokenizer:"bad\nlabel"
            "unsafe-tokenizer-model" "Unsafe tokenizer label");
          D.bytes b 1 (model_config ~supports_tools:false
            "explicit-no-tools" "Text only")) in
        on_chunk wire)
      else (
        assert (lookup 1 meta = "windsurf");
        assert (lookup 3 meta = key);
        on_chunk (response (fun _ -> ())));
      Ok 200)
    else fail "unexpected discovery RPC" in
  let models = match D.discover ~http ~api_key:key () with
    | Ok models -> models | Error _ -> fail "catalog request failed" in
  (match models with
  | [{D.id = first; router = false; supports_tools = Some true;
      supports_parallel_tool_calls = Some true; max_tokens = Some 4096;
      context_window_tokens = Some 131072;
      tokenizer_type = Some "devin-tokenizer-v1"; _};
     {D.id = second; router = true; supports_tools = Some true;
      supports_parallel_tool_calls = None; context_window_tokens = None;
      tokenizer_type = None; _};
     {D.id = unsafe; tokenizer_type = None; max_tokens = None;
      supports_tools = Some true; supports_parallel_tool_calls = None; _};
     {D.id = no_tools; supports_tools = Some false; _}] ->
       assert (first = model_uid && second = router_uid &&
         unsafe = "unsafe-tokenizer-model" && no_tools = "explicit-no-tools")
  | _ -> fail "dynamic catalog metadata or router flag not retained");
  let duplicate_calls = ref 0 in
  let duplicate_http ~url ~headers:_ ~body:_ ~on_chunk =
    assert (url = D.models_url);
    incr duplicate_calls;
    if !duplicate_calls = 1 then
      on_chunk (response (fun b ->
        D.bytes b 1 (model_config "duplicate-model" "First");
        D.bytes b 1 (model_config "duplicate-model" "Second")))
    else on_chunk (response (fun _ -> ()));
    Ok 200 in
  expect_error (function D.Invalid_response _ -> true | _ -> false)
    (D.discover ~http:duplicate_http ~api_key:key ());
  assert (!duplicate_calls = 1);
  let messages = [P.user "What is six times seven?"] in
  let independent_cascade = D.cascade_id messages in
  assert (String.length cascade = 36);
  assert (String.length independent_cascade = 36);
  assert (cascade <> independent_cascade);
  if not (D.valid_uuid cascade && D.valid_uuid independent_cascade) then
    fail (Printf.sprintf "invalid fresh Cascade IDs %S %S"
      cascade independent_cascade);
  let turn = ref 0 in
  let http ~url ~headers ~body ~on_chunk =
    if url = D.auth_url then (
      incr turn;
      assert (List.mem ("Connect-Protocol-Version", "1") headers);
      assert_metadata ~jwt:"" ~discovery:false (D.fields body);
      on_chunk (response (fun b -> D.string b 1 "jwt-from-account")); Ok 200)
    else if url = D.assign_url then (
      let fs = D.fields body in
      assert_metadata ~jwt:"" ~discovery:false fs;
      assert (lookup 2 fs = router_uid);
      assert (lookup 3 fs = cascade);
      assert (lookup 3 (object_field 5 fs) = "What is six times seven?");
      on_chunk (response (fun b -> D.bytes b 1 (response (fun assign ->
        D.string assign 1 "signed-router-assignment";
        D.string assign 2 assigned_uid)))); Ok 200)
    else if url = D.chat_url then (
      assert (List.mem ("Connect-Content-Encoding", "gzip") headers);
      assert (Char.code body.[0] = 1);
      let length = String.length body - 5 in
      assert (length > 0);
      let fs = D.fields (D.gzip ~decode:true (String.sub body 5 length)) in
      assert_metadata ~jwt:"jwt-from-account" ~discovery:false fs;
      assert (lookup 21 fs = assigned_uid);
      assert (lookup 16 fs = cascade);
      assert (D.integer 7 fs = 5);
      assert (D.integer 2 (object_field 8 fs) = 4096);
      assert (not (D.flag 11 fs));
      assert (lookup 26 fs = "signed-router-assignment");
      assert (lookup 1 (object_field 12 fs) = "auto");
      assert (lookup 1 (object_field 10 fs) = "multiply_seven");
      let schema = Yojson.Basic.from_string (lookup 3 (object_field 10 fs)) in
      let hint = P.member "hint" (P.member "properties" schema) in
      assert (P.member "type" hint = `String "string");
      assert (P.member "nullable" hint = `Bool true);
      assert (P.member "required" schema = `List [`String "number"]);
      let prompts = D.submessages 3 fs in
      let value = if !turn = 1 then (
        assert (List.length prompts = 1);
        assert (lookup 3 (List.hd prompts) = "What is six times seven?");
        initial_reply)
      else (
        let result = match prompts with
          | [user; assistant; result] ->
              assert (lookup 3 user = "What is six times seven?");
              assert (lookup 1 assistant = "server-message-id");
              assert (D.integer 2 assistant = 2);
              assert (lookup 11 assistant = "need calculator");
              assert (lookup 12 assistant = "opaque-signature");
              let call = object_field 6 assistant in
              assert (lookup 1 call = call_id);
              assert (lookup 2 call = "multiply_seven");
              assert (lookup 3 call = {|{"number":6}|});
              assert (D.integer 2 result = 4);
              assert (lookup 7 result = call_id);
              Yojson.Basic.from_string (lookup 3 result)
          | _ -> fail "tool result lost from second Cascade turn" in
        let product = match P.member "product" result with
          | `Int product -> product | _ -> fail "tool product not transmitted" in
        make_reply product) in
      (* Exercise fragmented HTTP chunks, even across Connect frame headers. *)
      on_chunk (String.sub value 0 3);
      on_chunk (String.sub value 3 (String.length value - 3));
      Ok 200)
    else fail "unexpected inference RPC" in
  let first = match D.complete ~http ~api_key:key ~model:router_uid
    ~max_tokens:4096 ~supports_parallel_tool_calls:true
    ~cascade_id:cascade ~router:true messages [tool] with
    | Ok (message, _) -> message | Error _ -> fail "first turn failed" in
  assert (first.tool_calls = [{P.id = call_id; name = "multiply_seven";
    arguments = `Assoc ["number", `Int 6]}]);
  assert (P.member "actual_model" (Option.get first.provider_state) = `String assigned_uid);
  assert (P.member "cascade_id" (Option.get first.provider_state) = `String cascade);
  let continued = messages @ [first; P.tool_result call_id {|{"product":42}|}] in
  let resumed_cascade = D.cascade_id continued in
  assert (resumed_cascade = cascade);
  let invalid_state = { first with provider_state = Some (`Assoc [
    "provider", `String "devin"; "model", `String router_uid;
    "cascade_id", `String "attacker-controlled-id";
    "message_id", `String "server-message-id";
    "thinking", `String ""; "signature", `String ""]) } in
  let regenerated = D.cascade_id (messages @ [invalid_state]) in
  assert (D.valid_uuid regenerated && regenerated <> cascade);
  let second = match D.complete ~http ~api_key:key ~model:router_uid
    ~max_tokens:4096 ~supports_parallel_tool_calls:true
    ~cascade_id:resumed_cascade ~router:true
    continued [tool] with
    | Ok (message, usage) ->
        assert (usage = Some (76,12)); message
    | Error _ -> fail "continuation failed" in
  assert (second.content = Some "Six times seven is 42.");
  assert (D.cascade_id (continued @ [second]) = cascade);
  assert (!turn = 2);
  let plain = D.request ~api_key:key ~jwt:"jwt-from-account"
    ~model:model_uid ~selected_model:model_uid ~cascade_id:cascade messages [tool] in
  let plain_schema = Yojson.Basic.from_string
    (lookup 3 (object_field 10 (D.fields plain))) in
  assert (P.member "type" (P.member "hint" (P.member "properties" plain_schema)) =
    `List [`String "string"; `String "null"]);
  assert (P.member "nullable" (P.member "hint" (P.member "properties" plain_schema)) =
    `Null);
  let routed_gemini = D.request ~api_key:key ~jwt:"jwt-from-account"
    ~model:"opaque-assigned-uid" ~selected_model:"gemini-3.7"
    ~cascade_id:cascade messages [tool] in
  let routed_schema = Yojson.Basic.from_string
    (lookup 3 (object_field 10 (D.fields routed_gemini))) in
  let routed_hint = P.member "hint" (P.member "properties" routed_schema) in
  assert (P.member "type" routed_hint = `String "string");
  assert (P.member "nullable" routed_hint = `Bool true);
  let image_attempts = ref 0 in
  let image_http ~url:_ ~headers:_ ~body:_ ~on_chunk:_ =
    incr image_attempts; Ok 200 in
  let image_result = P.tool_result_blocks call_id [
    P.Text "chart attached";
    P.Image { mime_type = "image/png"; data = "c2VjcmV0LWJhc2U2NA==" }] in
  expect_error (function
    | D.Invalid_response message ->
        message = "Devin protobuf transport does not support image tool results"
    | _ -> false)
    (D.complete ~http:image_http ~api_key:key ~model:router_uid
      ~cascade_id:cascade ~router:false [P.user "Inspect this"; image_result] [tool]);
  assert (!image_attempts = 0);
  let attempts = ref 0 in
  let hostile_http ~url ~headers:_ ~body:_ ~on_chunk =
    incr attempts;
    assert (url = D.auth_url);
    on_chunk (response (fun b ->
      D.string b 1 "jwt-from-account";
      D.string b 2 "https://server.codeium.com.evil.example"));
    Ok 200 in
  expect_error (function D.Invalid_response _ -> true | _ -> false)
    (D.complete ~http:hostile_http ~api_key:key ~model:model_uid
      ~cascade_id:cascade ~router:false messages [tool]);
  assert (!attempts = 1);
  expect_error (function D.Invalid_credential -> true | _ -> false)
    (D.discover ~http:hostile_http ~api_key:"bad\r\nsecret" ());
  assert (!attempts = 1);
  expect_error (function D.Invalid_response _ -> true | _ -> false)
    (D.rpc ~http:hostile_http
      ~url:"https://server.codeium.com.evil.example/exa.auth_pb.AuthService/GetUserJwt"
      ~headers:D.unary_headers ~body:(D.unary_request (D.metadata key)) ());
  assert (!attempts = 1);
  List.iter (fun hostile ->
    match D.authenticate ~http:(fun ~url:_ ~headers:_ ~body:_ ~on_chunk ->
      on_chunk (response (fun b -> D.string b 1 "jwt"; D.string b 2 hostile));
      Ok 200) ~api_key:key () with
    | Error (D.Invalid_response _) -> ()
    | _ -> fail "untrusted auth host accepted")
    ["http://server.codeium.com"; "https://evil.example/path";
     "https://server.codeium.com@evil.example"];
  expect_error (function D.Invalid_response _ -> true | _ -> false)
    (D.protect (fun () -> Ok (D.parse_stream ("\001\255\255\255\255"))));
  let trailer_error = D.frame 2
    {|{"error":{"code":"invalid_argument","message":"an internal error occurred (trace ID: 800c570aa8eb6576879b5fb52211a521) private-token","details":[{"debug":"private-token"}]}}|} in
  let error_steps = ref [] in
  let error_http ~url ~headers:_ ~body:_ ~on_chunk =
    error_steps := url :: !error_steps;
    if url = D.auth_url then
      on_chunk (response (fun b -> D.string b 1 "jwt-from-account"))
    else if url = D.chat_url then
      on_chunk (D.frame 0 (response (fun b -> D.string b 3 "partial answer")) ^
        trailer_error)
    else fail "trailer error escaped pinned auth/chat endpoints";
    Ok 200 in
  expect_error (function D.Invalid_response message ->
    message = "Devin Connect error invalid_argument: an internal error occurred" ^
      " (trace ID: 800c570aa8eb6576879b5fb52211a521)" | _ -> false)
    (D.complete ~http:error_http ~api_key:key ~model:model_uid
      ~cascade_id:cascade ~router:false messages []);
  assert (List.rev !error_steps = [D.auth_url; D.chat_url]);
  expect_error (function D.Invalid_response message ->
    message = "Devin Connect error unknown" | _ -> false)
    (D.protect (fun () -> Ok (D.parse_stream (D.frame 2
      {|{"error":{"code":"invalid_argument\nprivate-token","message":"private-token"}}|}))));
  print_endline "Devin Connect dynamic models, routed two-turn tool result, hostile host: ok"
