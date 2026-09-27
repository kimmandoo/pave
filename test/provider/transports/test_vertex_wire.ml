open Pave

let field = Protocol.member
let invalid f = match f () with
  | exception Invalid_argument _ | exception Protocol.Invalid_response _ -> ()
  | _ -> failwith "unsafe Vertex wire value accepted"
let contains haystack needle =
  let haystack_length = String.length haystack
  and needle_length = String.length needle in
  let rec search index =
    index + needle_length <= haystack_length &&
    (String.sub haystack index needle_length = needle || search (index + 1)) in
  search 0


let text value = `Assoc ["text", `String value]
let content role parts = `Assoc ["role", `String role; "parts", `List parts]
let completion parts = `Assoc ["candidates", `List [ `Assoc [
  "content", content "model" parts; "finishReason", `String "STOP" ] ] ]
let user = Protocol.user "Find both records"
let result id value = Protocol.tool_result id value
let parameters = `Assoc ["type", `String "object";
  "properties", `Assoc ["key", `Assoc ["type", `String "string"]]]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "lookup";
    "parameters", parameters]]
let fn id key signature = `Assoc [
  "functionCall", `Assoc ["id", `String id; "name", `String "lookup";
    "args", `Assoc ["key", `String key]];
  "thoughtSignature", `String signature ]

let () =
  let project = "research-123" and model = "gemini-3.1-pro-preview" in
  let endpoint location = Vertex_wire.endpoint ~project ~location ~model in
  assert (endpoint "global" =
    "https://aiplatform.googleapis.com/v1/projects/research-123/locations/global/publishers/google/models/gemini-3.1-pro-preview:streamGenerateContent?alt=sse");
  assert (String.starts_with ~prefix:"https://aiplatform.us.rep.googleapis.com/v1/"
    (endpoint "us"));
  assert (String.starts_with ~prefix:"https://aiplatform.eu.rep.googleapis.com/v1/"
    (endpoint "eu"));
  assert (String.starts_with ~prefix:"https://us-central1-aiplatform.googleapis.com/v1/"
    (endpoint "us-central1"));
  List.iter (fun location ->
    invalid (fun () -> endpoint location)) [
    ""; "us..example"; "us-central1.evil.example"; "us-central1/path";
    "us-central1@evil"; "us-central1:443"; "US-central1"; "us-central1\r\n" ];
  List.iter (fun project -> invalid (fun () ->
    Vertex_wire.endpoint ~project ~location:"global" ~model)) [
    ""; "abc"; "project/path"; "project.evil"; "project@evil"; "a--b#evil" ];
  List.iter (fun model -> invalid (fun () ->
    Vertex_wire.endpoint ~project ~location:"global" ~model)) [
    ""; "other/model"; "../models/evil"; "model?query"; "model%2Fescape" ];
  let parts = [text "Looking up "; fn "first-id" "alpha" "c2lnbmVkLTE=";
    fn "second-id" "beta" "c2lnbmVkLTI="] in
  let parsed = Vertex_wire.parse_completion ~model (completion parts) in
  let first, second = match parsed.tool_calls with
    | [first; second] -> first, second
    | _ -> failwith "Vertex failed to parse parallel tool calls" in
  assert (first.id = "first-id" && second.id = "second-id");
  assert (first.arguments = `Assoc ["key", `String "alpha"]);
  assert (field "provider" (Option.get parsed.provider_state) =
    `String "google-vertex");
  let request = Vertex_wire.request ~model
    [user; parsed; result second.id "beta result";
      result first.id "alpha result"] [tool] in
  let expected_parts = [text "Looking up ";
    `Assoc ["functionCall", `Assoc ["name", `String "lookup";
      "args", `Assoc ["key", `String "alpha"]];
      "thoughtSignature", `String "c2lnbmVkLTE="];
    `Assoc ["functionCall", `Assoc ["name", `String "lookup";
      "args", `Assoc ["key", `String "beta"]];
      "thoughtSignature", `String "c2lnbmVkLTI="]] in
  assert (field "contents" request = `List [
    content "user" [text "Find both records"];
    content "model" expected_parts;
    content "user" [
      `Assoc ["functionResponse", `Assoc ["name", `String "lookup";
        "response", `Assoc ["output", `String "alpha result"]]];
      `Assoc ["functionResponse", `Assoc ["name", `String "lookup";
        "response", `Assoc ["output", `String "beta result"]]] ] ]);
  assert (field "tools" request <> `Null);
  invalid (fun () -> Gemini_wire.request ~model [user; parsed] []);
  let direct = Gemini_wire.parse_completion ~model (completion parts) in
  invalid (fun () -> Vertex_wire.request ~model [user; direct] []);
  invalid (fun () -> Vertex_wire.request ~model:"another-model"
    [user; parsed] []);
  let stream = Gemini_stream.create ~model ~on_text:(fun _ -> ()) in
  let event = completion parts in
  Gemini_stream.feed stream ("data: " ^ Yojson.Basic.to_string event ^ "\n\n");
  let streamed = Vertex_wire.finish_stream ~model stream in
  assert (streamed.tool_calls = parsed.tool_calls);
  let streamed_request = Vertex_wire.request ~model
    [user; streamed; result second.id "beta result";
      result first.id "alpha result"] [tool] in
  assert (field "contents" streamed_request = field "contents" request);
  let answer = Vertex_wire.parse_completion ~model (completion [text "Both found."]) in
  assert (answer.content = Some "Both found.");
  let claude = "claude-sonnet-4-5" in
  let raw = Vertex_anthropic_wire.endpoint ~project ~location:"us-central1"
    ~model:claude ~streaming:false in
  assert (raw =
    "https://us-central1-aiplatform.googleapis.com/v1/projects/research-123/locations/us-central1/publishers/anthropic/models/claude-sonnet-4-5:rawPredict");
  let streamed = Vertex_anthropic_wire.endpoint ~project ~location:"global"
    ~model:claude ~streaming:true in
  assert (String.ends_with ~suffix:":streamRawPredict" streamed);
  invalid (fun () -> Vertex_anthropic_wire.endpoint ~project ~location:"global"
    ~model:"gemini-2.5-pro" ~streaming:false);
  invalid (fun () -> Vertex_anthropic_wire.endpoint ~project ~location:"global"
    ~model:"claude/evil" ~streaming:false);
  let claude_request = Vertex_anthropic_wire.request ~model:claude
    ~max_tokens:128 ~streaming:false [Protocol.user "Hello"] [] in
  assert (field "anthropic_version" claude_request =
    `String "vertex-2023-10-16");
  assert (field "stream" claude_request = `Bool false);
  assert (field "max_tokens" claude_request = `Int 128);
  assert (field "model" claude_request = `Null);
  let streaming_request = Vertex_anthropic_wire.request ~model:claude
    ~max_tokens:128 ~streaming:true [Protocol.user "Hello"] [] in
  assert (field "stream" streaming_request = `Bool true);
  let answer = `Assoc [
    "id", `String "msg_1"; "type", `String "message";
    "role", `String "assistant"; "model", `String claude;
    "content", `List [`Assoc ["type", `String "text"; "text", `String "Hello"]];
    "stop_reason", `String "end_turn";
    "usage", `Assoc ["input_tokens", `Int 2; "output_tokens", `Int 3] ] in
  assert ((Vertex_anthropic_wire.parse_completion ~model:claude answer).content =
    Some "Hello");
  invalid (fun () -> Vertex_anthropic_wire.parse_completion ~model:claude
    (`Assoc ["type", `String "error"]));
  invalid (fun () -> Vertex_anthropic_wire.parse_completion ~model:claude
    (`Assoc ["type", `String "message"; "role", `String "assistant";
      "content", `List []; "stop_reason", `String "end_turn"]));
  let chunks = ref [] in
  let stream = Vertex_anthropic_wire.create_stream
    ~on_text:(fun part -> chunks := part :: !chunks) in
  let event kind data = "event: " ^ kind ^ "\r\ndata: " ^ data ^ "\r\n\r\n" in
  Vertex_anthropic_wire.feed_stream stream
    (event "message_start"
      {|{"type":"message_start","message":{"id":"msg_1","type":"message","role":"assistant","usage":{"input_tokens":2}}}|} ^
     event "content_block_start"
      {|{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}|} ^
     event "content_block_delta"
      {|{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hello"}}|} ^
     event "content_block_stop" {|{"type":"content_block_stop","index":0}|} ^
     event "message_delta"
      {|{"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":3}}|} ^
     event "message_stop" {|{"type":"message_stop"}|});
  assert (Vertex_anthropic_wire.stream_is_finished stream);
  assert ((Vertex_anthropic_wire.finish_stream ~model:claude stream).content =
    Some "Hello");
  assert (String.concat "" (List.rev !chunks) = "Hello");
  let empty = Vertex_anthropic_wire.create_stream ~on_text:(fun _ -> ()) in
  Vertex_anthropic_wire.feed_stream empty
    (event "message_start"
       {|{"type":"message_start","message":{"id":"msg_2","type":"message","role":"assistant"}}|} ^
     event "message_delta"
       {|{"type":"message_delta","delta":{"stop_reason":"end_turn"}}|} ^
     event "message_stop" {|{"type":"message_stop"}|});
  invalid (fun () -> Vertex_anthropic_wire.finish_stream ~model:claude empty);
  let incomplete = Vertex_anthropic_wire.create_stream ~on_text:(fun _ -> ()) in
  Vertex_anthropic_wire.feed_stream incomplete
    (event "message_start"
      {|{"type":"message_start","message":{"id":"msg_1","type":"message","role":"assistant","usage":{"input_tokens":2}}}|});
  invalid (fun () -> Vertex_anthropic_wire.finish_stream ~model:claude incomplete);
  let failed = Vertex_anthropic_wire.create_stream ~on_text:(fun _ -> ()) in
  invalid (fun () -> Vertex_anthropic_wire.feed_stream failed
    (event "error" {|{"type":"error","error":{"type":"api_error","message":"denied"}}|}));
  invalid (fun () -> Vertex_anthropic_wire.finish_stream ~model:claude failed);
  let adc_invalid json =
    match Vertex_auth.validate_credential_json json with
    | exception Vertex_auth.Authentication_error _ -> ()
    | _ -> failwith "invalid or unsupported ADC credential accepted" in
  assert (Vertex_auth.validate_credential_json
    {|{"type":"authorized_user","client_id":"client","client_secret":"secret","refresh_token":"refresh"}|}
    = "authorized_user");
  assert (Vertex_auth.validate_credential_json
    {|{"type":"service_account","client_email":"sa@example.iam.gserviceaccount.com","private_key":"key","token_uri":"https://oauth2.googleapis.com/token"}|}
    = "service_account");
  adc_invalid {|{"type":"authorized_user","client_id":"client"}|};
  adc_invalid {|{"type":"service_account","client_email":"sa@example.com","private_key":"key","token_uri":"https://attacker.example/token"}|};
  adc_invalid {|{"type":"external_account","audience":"//iam.googleapis.com/","token_url":"https://sts.googleapis.com/v1/token"}|};
  assert (Vertex_auth.validate_credential_json
    {|{"type":"authorized_user","client_id":"client","client_secret":"secret","refresh_token":"refresh","service_account_impersonation_url":"https://iamcredentials.googleapis.com/v1/projects/-/serviceAccounts/sa:generateAccessToken"}|}
    = "impersonated_service_account");
  adc_invalid {|{"type":"unknown"}|};
  let auth_args, auth_body = Vertex_auth.authorized_user_request
    ~client_id:"id+&" ~client_secret:"secret =?" ~refresh_token:"refresh/+" in
  assert (List.mem "https://oauth2.googleapis.com/token" auth_args);
  assert (List.mem "Content-Type: application/x-www-form-urlencoded" auth_args);
  assert (List.mem "--proto" auth_args && List.mem "=https" auth_args);
  assert (List.mem "--max-redirs" auth_args && List.mem "0" auth_args);
  assert (List.mem "--data-binary" auth_args && List.mem "@-" auth_args);
  assert (not (List.exists (fun arg ->
    contains arg "id+&" || contains arg "secret =?" ||
    contains arg "refresh/+") auth_args));
  assert (auth_body =
    "client_id=id%2B%26&client_secret=secret%20%3D%3F&refresh_token=refresh%2F%2B&grant_type=refresh_token");
  assert (Vertex_auth.parse_token_response
    {|{"access_token":"fixture-token","token_type":"Bearer","expires_in":3600}|}
    = "fixture-token");
  List.iter (fun response ->
    match Vertex_auth.parse_token_response response with
    | exception Vertex_auth.Authentication_error message ->
        assert (not (contains message "secret" || contains message "fixture-token"))
    | _ -> failwith "malformed or expired OAuth token response accepted") [
    {|{"access_token":"fixture-token","token_type":"Bearer","expires_in":0}|};
    {|{"access_token":"fixture-token","token_type":"Bearer","expires_in":-1}|};
    {|{"access_token":"bad\nsecret","token_type":"Bearer","expires_in":3600}|};
    {|{"access_token":"fixture-token","token_type":"Basic","expires_in":3600}|};
    {|not-json|} ];
  let directory = Filename.temp_file "pave-vertex-adc-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let cli = Filename.concat directory "gcloud" in
  let credentials = Filename.concat directory "adc.json" in
  let output = open_out cli in
  output_string output
    "#!/bin/sh\n[ \"$1 $2 $3\" = 'auth application-default print-access-token' ] || exit 1\nprintf 'fixture-adc-token\\n'\n";
  close_out output;
  Unix.chmod cli 0o700;
  let output = open_out credentials in
  output_string output
    {|{"type":"authorized_user","client_id":"client","client_secret":"secret","refresh_token":"refresh","service_account_impersonation_url":"https://iamcredentials.googleapis.com/v1/projects/-/serviceAccounts/sa:generateAccessToken"}|};
  close_out output;
  let pid = Unix.fork () in
  if pid = 0 then (
    Unix.putenv "GOOGLE_CLOUD_ACCESS_TOKEN" "";
    Unix.putenv "CLOUDSDK_AUTH_ACCESS_TOKEN" "";
    Unix.putenv "GOOGLE_APPLICATION_CREDENTIALS" credentials;
    Unix.putenv "PATH" (directory ^ ":" ^ (match Sys.getenv_opt "PATH" with
      | Some path -> path | None -> "/usr/bin:/bin"));
    assert (Vertex_auth.access_token () = "fixture-adc-token");
    Unix.putenv "GOOGLE_CLOUD_ACCESS_TOKEN" "explicit-token";
    assert (Vertex_auth.access_token () = "explicit-token");
    Unix.putenv "GOOGLE_CLOUD_ACCESS_TOKEN" "invalid\nheader";
    (match Vertex_auth.access_token () with
     | exception Vertex_auth.Authentication_error _ -> ()
     | _ -> failwith "invalid bearer token accepted");
    exit 0);
  let _, status = Unix.waitpid [] pid in
  Sys.remove cli;
  Sys.remove credentials;
  Unix.rmdir directory;
  assert (status = Unix.WEXITED 0);
  print_endline "Vertex AI endpoint and signed parallel function roundtrip: ok"
