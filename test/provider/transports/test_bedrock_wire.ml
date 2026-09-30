let field = Pave.Protocol.member
module Aws = Pave.Aws_auth
module Wire = Pave.Bedrock_wire
module Discovery = Pave.Model_discovery

let expect_invalid f = match f () with
  | exception Pave.Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid Bedrock Converse response"

let tool_parameters =
  `Assoc ["type", `String "object";
    "properties", `Assoc ["key", `Assoc ["type", `String "string"]]]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "lookup";
    "description", `String "Look up an item";
    "parameters", tool_parameters]]
let arguments = `Assoc ["key", `String "alpha"]
let call : Pave.Protocol.tool_call = { id = "use-1"; name = "lookup"; arguments }
let answer stop blocks = `Assoc ["output", `Assoc ["message", `Assoc [
  "role", `String "assistant"; "content", `List blocks]];
  "stopReason", `String stop; "usage", `Assoc ["inputTokens", `Int 12;
    "outputTokens", `Int 7]]
let tool_answer = answer "tool_use" [`Assoc ["text", `String "Looking up."];
  `Assoc ["toolUse", `Assoc ["toolUseId", `String call.id;
    "name", `String call.name; "input", arguments]]]
let final_answer = answer "end_turn" [`Assoc ["text", `String "Found "];
  `Assoc ["text", `String "alpha."]]

let header headers name =
  match List.assoc_opt (String.lowercase_ascii name) headers with
  | Some value -> value
  | None -> failwith ("missing header " ^ name)

module Event = Pave.Aws_event_stream

let event_u16 value =
  String.init 2 (fun index ->
    Char.chr ((value lsr (8 * (1 - index))) land 0xff))

let event_u32_int32 value =
  String.init 4 (fun index ->
    let shift = 8 * (3 - index) in
    Char.chr (Int32.to_int
      (Int32.logand (Int32.shift_right_logical value shift) 0xffl)))

let event_u32 value = event_u32_int32 (Int32.of_int value)
let event_header name value =
  String.make 1 (Char.chr (String.length name)) ^ name ^
  String.make 1 (Char.chr 7) ^ event_u16 (String.length value) ^ value

let event_frame ?(message_type="event") ?event_type ?exception_type
    ?headers_length payload =
  let headers = Buffer.create 128 in
  Buffer.add_string headers (event_header ":message-type" message_type);
  (match event_type with Some name ->
     Buffer.add_string headers (event_header ":event-type" name) | None -> ());
  (match exception_type with Some name ->
     Buffer.add_string headers (event_header ":exception-type" name) | None -> ());
  if message_type = "event" then
    Buffer.add_string headers (event_header ":content-type" "application/json");
  let headers = Buffer.contents headers in
  let total = 16 + String.length headers + String.length payload in
  let header_length = Option.value headers_length ~default:(String.length headers) in
  let prelude = event_u32 total ^ event_u32 header_length in
  let prelude = prelude ^ event_u32_int32 (Event.crc32 prelude 0 8) in
  let body = prelude ^ headers ^ payload in
  body ^ event_u32_int32 (Event.crc32 body 0 (String.length body))

let bedrock_event name body =
  event_frame ~event_type:name
    (Yojson.Basic.to_string (`Assoc [name, body]))

let event_tool_start index id name =
  bedrock_event "contentBlockStart" (`Assoc [
    "contentBlockIndex", `Int index;
    "start", `Assoc ["toolUse", `Assoc [
      "toolUseId", `String id; "name", `String name]]])

let event_tool_delta index fragment =
  bedrock_event "contentBlockDelta" (`Assoc [
    "contentBlockIndex", `Int index;
    "delta", `Assoc ["toolUse", `Assoc ["input", `String fragment]]])

let event_block_stop index =
  bedrock_event "contentBlockStop" (`Assoc ["contentBlockIndex", `Int index])

let expect_event_invalid f = match f () with
  | exception Event.Invalid_message _ -> ()
  | _ -> failwith "expected invalid AWS EventStream frame"

let test_converse_stream_decoder () =
  let frames = [
    bedrock_event "messageStart" (`Assoc ["role", `String "assistant"]);
    bedrock_event "contentBlockDelta" (`Assoc [
      "contentBlockIndex", `Int 0; "delta", `Assoc ["text", `String "hello"]]);
    event_block_stop 0;
    event_tool_start 1 "tool-a" "lookup";
    event_tool_delta 1 "{\"key\":";
    event_tool_delta 1 "\"alpha\"}";
    event_block_stop 1;
    event_tool_start 2 "tool-b" "finish";
    event_tool_delta 2 "{}";
    event_block_stop 2;
    bedrock_event "messageStop" (`Assoc ["stopReason", `String "tool_use"]);
    bedrock_event "metadata" (`Assoc ["usage", `Assoc [
      "inputTokens", `Int 9; "outputTokens", `Int 3]])] in
  let drafts = ref [] in
  let stream = Wire.create_converse_stream
    ~on_tool_arguments:(fun delta -> drafts := delta :: !drafts) () in
  let events = List.concat_map (fun frame ->
    let pieces = List.init (String.length frame) (fun index ->
      String.sub frame index 1) in
    List.concat_map (Wire.feed_converse_stream stream) pieces) frames in
  Wire.finish_converse_stream stream;
  let fragments key = List.rev !drafts
    |> List.filter (fun (d : Pave.Protocol.tool_argument_delta) -> d.key = key)
    |> List.map (fun (d : Pave.Protocol.tool_argument_delta) -> d.fragment)
    |> String.concat "" in
  let key_for_id id =
    (List.find (fun (delta : Pave.Protocol.tool_argument_delta) ->
      delta.call_id = Some id) !drafts).key in
  let first_key = key_for_id "tool-a" and second_key = key_for_id "tool-b" in
  assert (first_key <> second_key);
  assert (fragments first_key = {|{"key":"alpha"}|});
  assert (fragments second_key = "{}");
  let expected_usage = { Pave.Protocol.input_tokens = 9; output_tokens = 3;
    cached_input_tokens = None; cache_creation_input_tokens = None;
    reasoning_output_tokens = None; input_modality_tokens = None;
    cached_input_modality_tokens = None; output_modality_tokens = None } in
  assert (events = [
    Wire.Text_delta "hello";
    Wire.Tool_call { id = "tool-a"; name = "lookup";
      arguments = `Assoc ["key", `String "alpha"] };
    Wire.Tool_call { id = "tool-b"; name = "finish"; arguments = `Assoc [] };
    Wire.Message_stop "tool_use";
    Wire.Usage expected_usage]);
  let joined = String.concat "" frames in
  let multi = Event.create () in
  assert (List.length (Event.feed multi joined) = List.length frames);
  Event.finish multi;
  let first = List.hd frames in
  let corrupted_crc = Bytes.of_string first in
  let last = Bytes.length corrupted_crc - 1 in
  Bytes.set corrupted_crc last (Char.chr (Char.code (Bytes.get corrupted_crc last) lxor 1));
  expect_event_invalid (fun () ->
    Event.feed (Event.create ()) (Bytes.to_string corrupted_crc));
  let corrupted_prelude = Bytes.of_string first in
  Bytes.set corrupted_prelude 8 (Char.chr (Char.code (Bytes.get corrupted_prelude 8) lxor 1));
  expect_event_invalid (fun () ->
    Event.feed (Event.create ()) (Bytes.to_string corrupted_prelude));
  expect_event_invalid (fun () ->
    let decoder = Event.create () in
    Event.feed decoder (event_frame ~headers_length:100 "{}"));
  expect_event_invalid (fun () ->
    let decoder = Event.create () in
    ignore (Event.feed decoder (String.sub first 0 (String.length first - 1)));
    Event.finish decoder);
  expect_event_invalid (fun () ->
    Event.feed (Event.create ~max_frame_bytes:16 ()) first);
  (match Wire.feed_converse_stream (Wire.create_converse_stream ())
      (event_frame ~message_type:"exception" ~exception_type:"ThrottlingException"
        {|{"message":"rate limited"}|}) with
   | exception Pave.Protocol.Invalid_response detail
       when String.starts_with ~prefix:"invalid Bedrock Converse response: Bedrock ConverseStream ThrottlingException"
         detail -> ()
   | _ -> failwith "Bedrock provider exception was not surfaced")
let read_request ic =
  let line = input_line ic in
  let method_, path = match String.split_on_char ' ' line with
    | method_ :: path :: _ -> method_, path
    | _ -> failwith "malformed HTTP request" in
  let rec headers acc =
    let line = input_line ic in
    if line = "\r" || line = "" then acc else
    let index = String.index line ':' in
    let name = String.sub line 0 index |> String.lowercase_ascii in
    let value = String.sub line (index + 1) (String.length line - index - 1) |> String.trim in
    headers ((name, value) :: acc) in
  let headers = headers [] in
  let length = int_of_string (header headers "content-length") in
  method_, path, headers, really_input_string ic length

let check_signed_request ~port ~step ic =
  let method_, path, headers, body = read_request ic in
  let target = Wire.endpoint ~base_url:(Printf.sprintf "http://127.0.0.1:%d" port)
    ~region:"eu-west-1" ~model:"fixture-model" () in
  assert (method_ = "POST");
  assert (path = target.path);
  assert (header headers "host" = target.host);
  assert (header headers "content-type" = "application/json");
  assert (header headers "accept" = "application/json");
  let keys = Aws.credentials ~access_key_id:"TESTACCESS" ~secret_access_key:"secretTEST"
    ~session_token:(Some "SESSIONTEST") in
  let expected = Aws.sign ~credentials:keys ~region:"eu-west-1"
    ~amz_date:(header headers "x-amz-date") ~method_:method_ ~host:target.host
    ~path ~body () in
  List.iter (fun (name, value) ->
    assert (header headers name = value)) expected;
  assert (header headers "x-amz-security-token" = "SESSIONTEST");
  let json = Yojson.Basic.from_string body in
  assert (field "model" json = `Null);
  let messages = match field "messages" json with
    | `List messages -> messages | _ -> failwith "missing Converse messages" in
  let first = `Assoc ["role", `String "user"; "content", `List [`Assoc [
    "text", `String "Find alpha"]]] in
  let assistant = `Assoc ["role", `String "assistant"; "content", `List [
    `Assoc ["text", `String "Looking up."];
    `Assoc ["toolUse", `Assoc ["toolUseId", `String call.id;
      "name", `String call.name; "input", arguments]]]] in
  let result = `Assoc ["role", `String "user"; "content", `List [
    `Assoc ["toolResult", `Assoc ["toolUseId", `String call.id;
      "content", `List [`Assoc ["text", `String "value-alpha"]]]]]] in
  assert (messages = (if step = 0 then [first] else [first; assistant; result]));
  let config = field "toolConfig" json in
  (match field "tools" config with
   | `List [definition] ->
       assert (field "name" (field "toolSpec" definition) = `String "lookup");
       assert (field "json" (field "inputSchema" (field "toolSpec" definition)) =
         field "parameters" (field "function" tool))
   | _ -> failwith "Converse tool schema missing")

let with_env entries fn =
  let old = List.map (fun (key, _) -> key, Sys.getenv_opt key) entries in
  Fun.protect ~finally:(fun () -> List.iter (fun (key, prior) ->
    Unix.putenv key (Option.value prior ~default:"")) old) (fun () ->
    List.iter (fun (key, value) -> Unix.putenv key value) entries;
    fn ())

let write_test_file path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel contents)

let future_expiration () =
  let tm = Unix.gmtime (Unix.gettimeofday () +. 3600.) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let test_aws_credential_sources () =
  let directory = Filename.temp_file "pave-aws-sources-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let credentials_file = Filename.concat directory "credentials" in
  let config_file = Filename.concat directory "config" in
  let token_file = Filename.concat directory "web-token" in
  let cache_dir = Filename.concat directory "sso-cache" in
  Unix.mkdir cache_dir 0o700;
  Fun.protect ~finally:(fun () ->
    List.iter (fun path -> if Sys.file_exists path then Sys.remove path)
      [credentials_file; config_file; token_file;
       Filename.concat cache_dir "ee0bfd2552fbd840c02cc48b6e823320543c450f.json"];
    Unix.rmdir cache_dir;
    Unix.rmdir directory) (fun () ->
    let env entries name = List.assoc_opt name entries in
    let future = future_expiration () in
    let sts_response = Printf.sprintf
      "<AssumeRoleResponse><Credentials><AccessKeyId>TEMPACCESS</AccessKeyId><SecretAccessKey>TEMPSECRET</SecretAccessKey><SessionToken>TEMPTOKEN</SessionToken><Expiration>%s</Expiration></Credentials></AssumeRoleResponse>"
      future in
    let write path content = write_test_file path content in

    (* Environment keys override all configured profiles and dynamic sources. *)
    write credentials_file "[default]\naws_access_key_id=PROFILEKEY\naws_secret_access_key=PROFILESECRET\n";
    write config_file "[default]\ncredential_process=/usr/bin/false\n";
    let environment = [
      "AWS_ACCESS_KEY_ID", "ENVKEY"; "AWS_SECRET_ACCESS_KEY", "ENVSECRET";
      "AWS_ROLE_ARN", "arn:aws:iam::123456789012:role/web";
      "AWS_WEB_IDENTITY_TOKEN_FILE", token_file;
      "AWS_PROFILE", "default"] in
    let never_http ~method_:_ ~url:_ ~headers:_ ~body:_ =
      failwith "AWS environment credentials unexpectedly made a request" in
    let resolved = Aws.resolve ~getenv:(env environment)
      ~credentials_file ~config_file ~http:never_http () in
    assert (resolved.access_key_id = "ENVKEY");

    (* Credential-process config is never executed unless explicitly allowed. *)
    write credentials_file "[process]\n";
    write config_file
      "[profile process]\ncredential_process = /usr/bin/printf %s '{\"Version\":1,\"AccessKeyId\":\"PROCESS\",\"SecretAccessKey\":\"PROCESSSECRET\",\"SessionToken\":\"PROCESS$HOME\"}'\n";
    let process_env = env ["AWS_PROFILE", "process"] in
    (match Aws.resolve ~getenv:process_env ~credentials_file ~config_file () with
     | exception Invalid_argument reason
         when String.starts_with ~prefix:"AWS credential_process is configured" reason -> ()
     | _ -> failwith "credential_process ran without explicit opt-in");
    let process_keys = Aws.resolve ~getenv:process_env ~credentials_file ~config_file
      ~credential_process_policy:Aws.Allow () in
    assert (process_keys.access_key_id = "PROCESS");
    assert (process_keys.session_token = Some "PROCESS$HOME");

    (* Web identity uses the token file and unsigned STS query protocol. *)
    write token_file "fake+token\n";
    write credentials_file "[default]\n";
    write config_file "[default]\naws_access_key_id=PROFILEKEY\naws_secret_access_key=PROFILESECRET\n";
    let web_http ~method_ ~url ~headers ~body =
      assert (method_ = "POST");
      assert (url = "https://sts.us-west-2.amazonaws.com/");
      assert (List.mem_assoc "content-type" headers);
      assert (String.starts_with ~prefix:"Action=AssumeRoleWithWebIdentity&Version=2011-06-15&RoleArn=arn%3Aaws%3Aiam%3A%3A123456789012%3Arole%2Fweb"
        body);
      assert (String.ends_with ~suffix:"WebIdentityToken=fake%2Btoken" body);
      200, sts_response in
    let web_keys = Aws.resolve ~getenv:(env [
        "AWS_REGION", "us-west-2";
        "AWS_ROLE_ARN", "arn:aws:iam::123456789012:role/web";
        "AWS_WEB_IDENTITY_TOKEN_FILE", token_file])
      ~credentials_file ~config_file ~http:web_http () in
    assert (web_keys.access_key_id = "TEMPACCESS" &&
      web_keys.session_token = Some "TEMPTOKEN");

    (* source_profile credentials are signed for STS AssumeRole, not reused directly. *)
    write credentials_file
      "[base]\naws_access_key_id=SOURCEKEY\naws_secret_access_key=SOURCESECRET\n";
    write config_file
      "[profile work]\nrole_arn=arn:aws:iam::123456789012:role/target\nsource_profile=base\nrole_session_name=fixture\n";
    let role_http ~method_ ~url ~headers ~body =
      assert (method_ = "POST" && url = "https://sts.us-west-2.amazonaws.com/");
      let authorization = List.assoc "authorization" headers in
      assert (String.starts_with ~prefix:"AWS4-HMAC-SHA256 Credential=SOURCEKEY/" authorization);
      let credential_scope = List.hd (String.split_on_char ',' authorization) in
      assert (String.ends_with ~suffix:"/us-west-2/sts/aws4_request" credential_scope);
      assert (body = "Action=AssumeRole&Version=2011-06-15&RoleArn=arn%3Aaws%3Aiam%3A%3A123456789012%3Arole%2Ftarget&RoleSessionName=fixture");
      200, sts_response in
    let role_keys = Aws.resolve ~getenv:(env [
        "AWS_REGION", "us-west-2"; "AWS_PROFILE", "work"])
      ~credentials_file ~config_file ~http:role_http () in
    assert (role_keys.access_key_id = "TEMPACCESS");

    (* IAM Identity Center cache lookup is keyed by the named SSO session. *)
    write config_file
      "[profile sso]\nsso_session=corp\nsso_account_id=123456789012\nsso_role_name=Developer\n[sso-session corp]\nsso_start_url=https://example.awsapps.com/start\nsso_region=us-east-1\n";
    write (Filename.concat cache_dir "ee0bfd2552fbd840c02cc48b6e823320543c450f.json")
      (Yojson.Basic.to_string (`Assoc [
        "accessToken", `String "cached-sso-token";
        "expiresAt", `String future]));
    let expiration_ms = int_of_float ((Unix.gettimeofday () +. 3600.) *. 1000.) in
    let sso_response = Yojson.Basic.to_string (`Assoc [
      "roleCredentials", `Assoc ["accessKeyId", `String "SSOACCESS";
        "secretAccessKey", `String "SSOSECRET"; "sessionToken", `String "SSOTOKEN";
        "expiration", `Int expiration_ms]]) in
    let sso_http ~method_ ~url ~headers ~body =
      assert (method_ = "GET" && body = "");
      assert (String.starts_with ~prefix:"https://portal.sso.us-east-1.amazonaws.com/federation/credentials?"
        url);
      assert (List.assoc "x-amz-sso_bearer_token" headers = "cached-sso-token");
      200, sso_response in
    let sso_keys = Aws.resolve ~getenv:(env ["AWS_PROFILE", "sso"; "HOME", directory])
      ~credentials_file ~config_file ~http:sso_http ~sso_cache_dir:cache_dir () in
    assert (sso_keys.access_key_id = "SSOACCESS");

    (* Container credentials use the documented relative URI and authorization header. *)
    write config_file "[default]\n";
    let container_response = Yojson.Basic.to_string (`Assoc [
      "AccessKeyId", `String "CONTAINERACCESS";
      "SecretAccessKey", `String "CONTAINERSECRET";
      "Token", `String "CONTAINERTOKEN";
      "Expiration", `String future]) in
    let container_http ~method_ ~url ~headers ~body =
      assert (method_ = "GET" && body = "");
      assert (url = "http://169.254.170.2/v2/credentials/fixture");
      assert (List.assoc "authorization" headers = "Bearer fixture");
      200, container_response in
    let container_keys = Aws.resolve ~getenv:(env [
        "AWS_REGION", "us-west-2";
        "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI", "/v2/credentials/fixture";
        "AWS_CONTAINER_AUTHORIZATION_TOKEN", "Bearer fixture"])
      ~credentials_file ~config_file ~http:container_http () in
    assert (container_keys.access_key_id = "CONTAINERACCESS");

    (* IMDSv2 is a bounded token, role, credential sequence with no v1 fallback. *)
    let step = ref 0 in
    let imds_http ~method_ ~url ~headers ~body =
      assert (body = "");
      match !step with
      | 0 ->
          incr step;
          assert (method_ = "PUT" && url = "http://169.254.169.254/latest/api/token");
          assert (List.assoc "x-aws-ec2-metadata-token-ttl-seconds" headers = "21600");
          200, "metadata-token"
      | 1 ->
          incr step;
          assert (method_ = "GET" &&
            url = "http://169.254.169.254/latest/meta-data/iam/security-credentials/");
          assert (List.assoc "x-aws-ec2-metadata-token" headers = "metadata-token");
          200, "instance-role\n"
      | 2 ->
          incr step;
          assert (method_ = "GET" &&
            url = "http://169.254.169.254/latest/meta-data/iam/security-credentials/instance-role");
          assert (List.assoc "x-aws-ec2-metadata-token" headers = "metadata-token");
          200, Yojson.Basic.to_string (`Assoc [
            "Code", `String "Success"; "AccessKeyId", `String "IMDSACCESS";
            "SecretAccessKey", `String "IMDSSECRET"; "Token", `String "IMDSTOKEN";
            "Expiration", `String future])
      | _ -> failwith "unexpected extra IMDS request" in
    let imds_keys = Aws.resolve ~getenv:(env ["AWS_REGION", "us-west-2"])
      ~credentials_file ~config_file ~http:imds_http () in
    assert (!step = 3 && imds_keys.access_key_id = "IMDSACCESS"))
let fixture () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 2;
  let port = match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    try
      for step = 0 to 1 do
        let client, _ = Unix.accept socket in
        let ic = Unix.in_channel_of_descr client in
        let oc = Unix.out_channel_of_descr client in
        check_signed_request ~port ~step ic;
        let body = Yojson.Basic.to_string (if step = 0 then tool_answer else final_answer) in
        Printf.fprintf oc "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
          (String.length body) body;
        flush oc;
        close_in_noerr ic;
        close_out_noerr oc
      done;
      Unix.close socket;
      exit 0
    with exn -> prerr_endline (Printexc.to_string exn); exit 2);
  Unix.close socket;
  let reaped = ref false in
  Fun.protect ~finally:(fun () ->
    if not !reaped then (
      (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
      ignore (Unix.waitpid [] child))) (fun () ->
    with_env ["AWS_REGION", "eu-west-1";
      "AWS_ACCESS_KEY_ID", "TESTACCESS";
      "AWS_SECRET_ACCESS_KEY", "secretTEST";
      "AWS_SESSION_TOKEN", "SESSIONTEST";
      "PAVE_AWS_CREDENTIAL_PROCESS", "disabled"] (fun () ->
      let config : Pave.Provider.config = {
        endpoint = Printf.sprintf "http://127.0.0.1:%d" port;
        api_key = ""; model = "fixture-model"; api = Pave.Provider.Bedrock_converse } in
      let original = [Pave.Protocol.user "Find alpha"] in
      let incompatible = { config with api = Pave.Provider.Openai_completions } in
      (match Pave.Provider.complete
          ~authentication:Pave.Provider.Cloud_identity incompatible original [] with
       | exception Pave.Provider.Provider_error
           "cloud identity authentication requires an AWS Bedrock, Google Vertex, or Azure route" -> ()
       | _ -> failwith "cloud identity was accepted on an unrelated route");
      (match Pave.Provider.complete config original [tool] with
       | exception Pave.Provider.Provider_error
           "Bedrock Converse requires AWS cloud identity, not an API key" -> ()
       | _ -> failwith "Bedrock accepted API-key authentication");
      let first = Pave.Provider.complete
        ~authentication:Pave.Provider.Cloud_identity config original [tool] in
      assert (first.content = Some "Looking up.");
      assert (first.tool_calls = [call]);
      let continuation = original @ [first; Pave.Protocol.tool_result call.id "value-alpha"] in
      let callbacks = ref [] in
      let second = Pave.Provider.complete
        ~authentication:Pave.Provider.Cloud_identity
        ~on_text:(fun content -> callbacks := content :: !callbacks)
        config continuation [tool] in
      assert (second.content = Some "Found alpha.");
      assert (second.tool_calls = []);
      assert (!callbacks = ["Found alpha."]);
      let _, status = Unix.waitpid [] child in
      reaped := true;
      assert (status = Unix.WEXITED 0)))

let check_signed_stream_request ~port ~step ic =
  let method_, path, headers, body = read_request ic in
  let target = Wire.converse_stream_endpoint
    ~base_url:(Printf.sprintf "http://127.0.0.1:%d" port)
    ~region:"eu-west-1" ~model:"fixture-model" () in
  assert (method_ = "POST" && path = target.path);
  assert (header headers "host" = target.host);
  assert (header headers "accept" = "application/vnd.amazon.eventstream");
  assert (header headers "x-amzn-bedrock-accept" = "application/json");
  assert (header headers "content-type" = "application/json");
  let keys = Aws.credentials ~access_key_id:"TESTACCESS"
    ~secret_access_key:"secretTEST" ~session_token:(Some "SESSIONTEST") in
  let expected = Aws.sign_converse_stream ~credentials:keys ~region:"eu-west-1"
    ~amz_date:(header headers "x-amz-date") ~method_ ~host:target.host
    ~path ~body () in
  List.iter (fun (name, value) ->
    assert (header headers name = value)) expected;
  assert (header headers "x-amz-security-token" = "SESSIONTEST");
  let messages = field "messages" (Yojson.Basic.from_string body) in
  let user = `Assoc ["role", `String "user"; "content", `List [
    `Assoc ["text", `String "Find alpha"]]] in
  let assistant = `Assoc ["role", `String "assistant"; "content", `List [
    `Assoc ["text", `String "Looking up."];
    `Assoc ["toolUse", `Assoc ["toolUseId", `String call.id;
      "name", `String call.name; "input", arguments]]]] in
  let result = `Assoc ["role", `String "user"; "content", `List [
    `Assoc ["toolResult", `Assoc ["toolUseId", `String call.id;
      "content", `List [`Assoc ["text", `String "value-alpha"]]]]]] in
  assert (messages = `List (if step = 0 then [user]
    else [user; assistant; result]))

let converse_stream_fixture () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 2;
  let port = match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    try
      for step = 0 to 1 do
        let client, _ = Unix.accept socket in
        let ic = Unix.in_channel_of_descr client in
        let oc = Unix.out_channel_of_descr client in
        check_signed_stream_request ~port ~step ic;
        let frames = if step = 0 then [
          bedrock_event "messageStart" (`Assoc ["role", `String "assistant"]);
          bedrock_event "contentBlockDelta" (`Assoc [
            "contentBlockIndex", `Int 0;
            "delta", `Assoc ["text", `String "Looking up."]]);
          event_block_stop 0;
          event_tool_start 1 call.id call.name;
          event_tool_delta 1 {|{"key":|};
          event_tool_delta 1 {|"alpha"}|};
          event_block_stop 1;
          bedrock_event "messageStop" (`Assoc ["stopReason", `String "tool_use"]);
          bedrock_event "metadata" (`Assoc ["usage", `Assoc [
            "inputTokens", `Int 9; "outputTokens", `Int 3]])]
        else [
          bedrock_event "messageStart" (`Assoc ["role", `String "assistant"]);
          bedrock_event "contentBlockDelta" (`Assoc [
            "contentBlockIndex", `Int 0;
            "delta", `Assoc ["text", `String "Found "]]);
          bedrock_event "contentBlockDelta" (`Assoc [
            "contentBlockIndex", `Int 0;
            "delta", `Assoc ["text", `String "alpha."]]);
          event_block_stop 0;
          bedrock_event "messageStop" (`Assoc ["stopReason", `String "end_turn"]);
          bedrock_event "metadata" (`Assoc ["usage", `Assoc [
            "inputTokens", `Int 11; "outputTokens", `Int 4]])] in
        let body = String.concat "" frames in
        Printf.fprintf oc
          "HTTP/1.1 200 OK\r\nContent-Type: application/vnd.amazon.eventstream\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
          (String.length body) body;
        flush oc;
        close_in_noerr ic;
        close_out_noerr oc
      done;
      Unix.close socket;
      exit 0
    with exn -> prerr_endline (Printexc.to_string exn); exit 2);
  Unix.close socket;
  let reaped = ref false in
  Fun.protect ~finally:(fun () ->
    if not !reaped then (
      (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
      ignore (Unix.waitpid [] child))) (fun () ->
    with_env ["AWS_REGION", "eu-west-1";
      "AWS_ACCESS_KEY_ID", "TESTACCESS";
      "AWS_SECRET_ACCESS_KEY", "secretTEST";
      "AWS_SESSION_TOKEN", "SESSIONTEST";
      "PAVE_AWS_CREDENTIAL_PROCESS", "disabled"] (fun () ->
      let config : Pave.Provider.config = {
        endpoint = Printf.sprintf "http://127.0.0.1:%d" port;
        api_key = ""; model = "fixture-model";
        api = Pave.Provider.Bedrock_converse_stream } in
      let original = [Pave.Protocol.user "Find alpha"] in
      let first_text = ref [] and first_usage = ref None in
      let first = Pave.Provider.complete
        ~authentication:Pave.Provider.Cloud_identity
        ~on_text:(fun text -> first_text := text :: !first_text)
        ~on_usage:(fun usage -> first_usage := Some usage)
        config original [tool] in
      assert (first.content = Some "Looking up.");
      assert (first.tool_calls = [call]);
      assert (List.rev !first_text = ["Looking up."]);
      assert (!first_usage = Some { Pave.Protocol.input_tokens = 9;
        output_tokens = 3; cached_input_tokens = None;
        cache_creation_input_tokens = None; reasoning_output_tokens = None;
        input_modality_tokens = None; cached_input_modality_tokens = None;
        output_modality_tokens = None });
      let continuation = original @
        [first; Pave.Protocol.tool_result call.id "value-alpha"] in
      let final_text = ref [] and final_usage = ref None in
      let final = Pave.Provider.complete
        ~authentication:Pave.Provider.Cloud_identity
        ~on_text:(fun text -> final_text := text :: !final_text)
        ~on_usage:(fun usage -> final_usage := Some usage)
        config continuation [tool] in
      assert (final.content = Some "Found alpha.");
      assert (final.tool_calls = []);
      assert (String.concat "" (List.rev !final_text) = "Found alpha.");
      assert (!final_usage = Some { Pave.Protocol.input_tokens = 11;
        output_tokens = 4; cached_input_tokens = None;
        cache_creation_input_tokens = None; reasoning_output_tokens = None;
        input_modality_tokens = None; cached_input_modality_tokens = None;
        output_modality_tokens = None });
      let _, status = Unix.waitpid [] child in
      reaped := true;
      assert (status = Unix.WEXITED 0)))

let () =
  assert (Aws.curl_path = "/usr/bin/curl");
  assert (Array.to_list Aws.curl_environment = ["LANG=C"; "LC_ALL=C"]);
  (* Reap the direct helper once, then drain a pipe held by its descendant. *)
  assert (Aws.run_child ~timeout:3. ~output_limit:128 ~stdin:""
    "/bin/sh" ["-c"; "printf ready; sleep 0.2 &"] = "ready");
  let keys = Aws.credentials ~access_key_id:"TESTACCESS" ~secret_access_key:"secretTEST"
    ~session_token:(Some "SESSIONTEST") in
  let signed = Aws.sign ~credentials:keys ~region:"eu-west-1" ~amz_date:"20250925T123456Z"
    ~method_:"POST" ~host:"bedrock-runtime.eu-west-1.amazonaws.com"
    ~path:"/model/sample%3Aprofile/converse" ~body:"{\"messages\":[]}" () in
  assert (List.assoc "x-amz-security-token" signed = "SESSIONTEST");
  assert (List.assoc "x-amz-content-sha256" signed =
    "5e4ce7b36ba37b78a5d5f9fd08e6b7b54ba6879d651aa46ec9e1d6fa24ebe30a");
  let signature = List.assoc "authorization" signed in
  assert (signature =
    "AWS4-HMAC-SHA256 Credential=TESTACCESS/20250925/eu-west-1/bedrock/aws4_request, SignedHeaders=accept;content-type;host;x-amz-content-sha256;x-amz-date;x-amz-security-token, Signature=a0fa4cef7d8422c1576c358e611918c1507faee77a1773027252d8b9280da738");
  let env name = List.assoc_opt name ["AWS_ACCESS_KEY_ID", "ENVKEY";
    "AWS_SECRET_ACCESS_KEY", "ENVSECRET"; "AWS_SESSION_TOKEN", "ENVTOKEN";
    "AWS_REGION", "eu-west-1"; "AWS_PROFILE", "work"] in
  let resolved = Aws.resolve ~getenv:env () in
  assert (resolved.access_key_id = "ENVKEY" && resolved.session_token = Some "ENVTOKEN");
  assert (Aws.region ~getenv:env () = "eu-west-1");
  (match Aws.resolve ~getenv:(fun name -> if name = "AWS_PROFILE" then Some "work" else None)
    ~credentials_file:"/dev/null" ~config_file:"/dev/null" () with
   | exception Invalid_argument _ -> ()
   | _ -> failwith "empty profile unexpectedly resolved credentials");
  let credentials_file = Filename.temp_file "pave-aws-creds-" ".ini" in
  let config_file = Filename.temp_file "pave-aws-config-" ".ini" in
  Fun.protect ~finally:(fun () ->
    Sys.remove credentials_file; Sys.remove config_file) (fun () ->
    let write path content =
      let channel = open_out path in
      output_string channel content;
      close_out channel in
    write credentials_file
      "[work]\naws_access_key_id = PROFILEKEY\naws_secret_access_key = PROFILESECRET\naws_session_token = PROFILETOKEN\n";
    write config_file
      "[profile work]\naws_access_key_id = CONFIGKEY\naws_secret_access_key = CONFIGSECRET\n";
    let profile = Aws.resolve ~getenv:(fun key ->
      if key = "AWS_PROFILE" then Some "work" else None)
      ~credentials_file ~config_file () in
    assert (profile.access_key_id = "PROFILEKEY");
    assert (profile.session_token = Some "PROFILETOKEN");
    let fallback = Aws.resolve ~getenv:(fun key ->
      if key = "AWS_PROFILE" then Some "work" else None)
      ~credentials_file:config_file ~config_file () in
    assert (fallback.access_key_id = "CONFIGKEY"));
  let discovery = Wire.discovery_endpoint ~region:"eu-west-1" () in
  assert (discovery.host = "bedrock.eu-west-1.amazonaws.com" &&
    discovery.path = "/foundation-models");
  let signed_get = Aws.sign ~content_type:false ~credentials:keys
    ~region:"eu-west-1" ~amz_date:"20250925T123456Z" ~method_:"GET"
    ~host:discovery.host ~path:discovery.path ~body:"" () in
  assert (not (List.mem_assoc "content-type" signed_get));
  assert (List.assoc "x-amz-content-sha256" signed_get =
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
  let summary id output inference status = `Assoc [
    "modelId", `String id; "outputModalities", `List [`String output];
    "inferenceTypesSupported", `List [`String inference];
    "modelLifecycle", `Assoc ["status", `String status]] in
  assert (Wire.parse_models (`Assoc ["modelSummaries", `List [
    summary "discovered-model" "TEXT" "ON_DEMAND" "ACTIVE";
    summary "unavailable" "TEXT" "PROVISIONED" "ACTIVE";
    summary "obsolete" "TEXT" "ON_DEMAND" "LEGACY";
    summary "embed" "EMBEDDING" "ON_DEMAND" "ACTIVE"]]) = ["discovered-model"]);
  let profile id status profile_type model_arn = `Assoc [
    "inferenceProfileId", `String id;
    "status", `String status;
    "type", `String profile_type;
    "models", `List [`Assoc ["modelArn", `String model_arn]]] in
  let profile_token = "page+/1=" in
  let profile_target = Wire.inference_profiles_endpoint
    ~region:"eu-west-1" () in
  let next_profile_target = Wire.inference_profiles_endpoint
    ~region:"eu-west-1" ~next_token:profile_token () in
  assert (Aws.query_string ["nextToken", profile_token; "maxResults", "100"] =
    "maxResults=100&nextToken=page%2B%2F1%3D");
  assert (profile_target.url =
    "https://bedrock.eu-west-1.amazonaws.com/inference-profiles?maxResults=100");
  assert (next_profile_target.url =
    "https://bedrock.eu-west-1.amazonaws.com/inference-profiles?maxResults=100&nextToken=page%2B%2F1%3D");
  let sign_query query = Aws.sign ~content_type:false ~query
    ~credentials:keys ~region:"eu-west-1" ~amz_date:"20250925T123456Z"
    ~method_:"GET" ~host:profile_target.host ~path:profile_target.path ~body:"" () in
  let signed_page = sign_query next_profile_target.query in
  assert (List.assoc "authorization" signed_page =
    List.assoc "authorization"
      (sign_query (List.rev next_profile_target.query)));
  assert (List.assoc "authorization" signed_page <>
    List.assoc "authorization"
      (sign_query ["maxResults", "101"; "nextToken", profile_token]));
  let parsed_profiles, parsed_token = Wire.parse_inference_profiles
    (`Assoc ["inferenceProfileSummaries", `List [
      profile "us-profile" "ACTIVE" "SYSTEM_DEFINED" "arn:aws:bedrock:us-east-1::foundation-model/model";
      profile "application-profile" "ACTIVE" "APPLICATION" "arn:aws:bedrock:us-east-1::foundation-model/model";
      profile "deleting-profile" "DELETING" "APPLICATION" "arn:aws:bedrock:us-east-1::foundation-model/model"];
      "nextToken", `String profile_token]) in
  assert (parsed_profiles = ["us-profile"; "application-profile"] &&
    parsed_token = Some profile_token);
  expect_invalid (fun () -> Wire.parse_inference_profiles
    (`Assoc ["inferenceProfileSummaries", `List [
      profile "duplicate" "ACTIVE" "SYSTEM_DEFINED" "arn";
      profile "duplicate" "ACTIVE" "APPLICATION" "arn"]]));
  let foundation_body = Yojson.Basic.to_string (`Assoc [
    "modelSummaries", `List [
      summary "foundation-model" "TEXT" "ON_DEMAND" "ACTIVE";
      summary "provisioned-model" "TEXT" "PROVISIONED" "ACTIVE"]]) in
  let profile_page_one = Yojson.Basic.to_string (`Assoc [
    "inferenceProfileSummaries", `List [
      profile "us.anthropic.claude" "ACTIVE" "SYSTEM_DEFINED" "arn:aws:bedrock:us-east-1::foundation-model/claude"];
    "nextToken", `String profile_token]) in
  let profile_page_two = Yojson.Basic.to_string (`Assoc [
    "inferenceProfileSummaries", `List [
      profile "application-inference-profile" "ACTIVE" "APPLICATION" "arn:aws:bedrock:us-east-1::foundation-model/model"]]) in
  let foundation_target = Wire.discovery_endpoint ~region:"eu-west-1" () in
  let requests = ref [] in
  let discovery_http ~url ~headers =
    requests := url :: !requests;
    assert (List.assoc "x-amz-security-token" headers = "SESSIONTEST");
    assert (String.starts_with
      ~prefix:"AWS4-HMAC-SHA256 Credential=TESTACCESS/"
      (List.assoc "authorization" headers));
    assert (not (List.mem_assoc "content-type" headers));
    match url with
    | url when url = foundation_target.url -> Ok (200, foundation_body)
    | url when url = profile_target.url -> Ok (200, profile_page_one)
    | url when url = next_profile_target.url -> Ok (200, profile_page_two)
    | _ -> failwith ("unexpected Bedrock listing URL: " ^ url) in
  let aws_env = ["AWS_ACCESS_KEY_ID", "TESTACCESS";
    "AWS_SECRET_ACCESS_KEY", "secretTEST"; "AWS_SESSION_TOKEN", "SESSIONTEST";
    "AWS_REGION", "eu-west-1"; "AWS_EC2_METADATA_DISABLED", "true";
    "PAVE_AWS_CREDENTIAL_PROCESS", "disabled"] in
  with_env aws_env (fun () ->
    (match Discovery.discover ~provider:"amazon-bedrock"
        ~http:discovery_http () with
     | Ok listing ->
         assert (Discovery.model_ids listing =
           ["foundation-model"; "us.anthropic.claude";
            "application-inference-profile"])
     | Error error ->
         failwith ("Bedrock inference profile discovery failed: " ^
           Discovery.message error));
    assert (List.rev !requests =
      [foundation_target.url; profile_target.url; next_profile_target.url]));
  let repeated_calls = ref 0 in
  let repeated_http ~url ~headers:_ =
    incr repeated_calls;
    if url = foundation_target.url then Ok (200, foundation_body)
    else Ok (200, Yojson.Basic.to_string (`Assoc [
      "inferenceProfileSummaries", `List [];
      "nextToken", `String "repeated"])) in
  with_env aws_env (fun () ->
    (match Discovery.discover ~provider:"amazon-bedrock"
        ~http:repeated_http () with
     | Error (Discovery.Invalid_response detail)
       when String.starts_with
         ~prefix:"inference profile listing repeated a pagination token" detail -> ()
     | Error error -> failwith ("unexpected pagination failure: " ^
         Discovery.message error)
     | Ok _ -> failwith "repeated Bedrock inference profile token accepted");
    assert (!repeated_calls = 3));
  let target = Wire.endpoint ~region:"eu-west-1" ~model:"arn:aws:bedrock:eu-west-1:123:profile/a" () in
  assert (String.contains target.path '%');
  let stream_target = Wire.converse_stream_endpoint
    ~region:"eu-west-1" ~model:"sample" () in
  assert (String.ends_with ~suffix:"/converse-stream" stream_target.path);
  let stream_signed = Aws.sign_converse_stream ~credentials:keys
    ~region:"eu-west-1" ~amz_date:"20250925T123456Z" ~method_:"POST"
    ~host:stream_target.host ~path:stream_target.path ~body:"{}" () in
  assert (List.assoc "accept" stream_signed = "application/vnd.amazon.eventstream");
  assert (List.assoc "x-amzn-bedrock-accept" stream_signed = "application/json");
  (match Wire.endpoint ~base_url:"http://example.test" ~region:"eu-west-1" ~model:"sample" () with
   | exception Invalid_argument _ -> ()
   | _ -> failwith "untrusted endpoint accepted");
  expect_invalid (fun () -> Wire.parse_response (answer "max_tokens" [`Assoc ["text", `String "partial"]]));
  expect_invalid (fun () -> Wire.parse_response (answer "end_turn" [
    `Assoc ["reasoningContent", `Assoc ["reasoningText", `Assoc [
      "text", `String "unpreserved"; "signature", `String "signed"]]]]));
  let history : Pave.Protocol.message = { role = "assistant"; content = None; tool_calls = [call];
  tool_call_id = None; tool_result_content = None; provider_state = None;
  attachments = [] } in
  expect_invalid (fun () -> Wire.request
    [Pave.Protocol.user "Find alpha"; history;
      Pave.Protocol.tool_result call.id "value-alpha"] []);
  assert (Wire.usage final_answer = Some { Pave.Protocol.input_tokens = 12;
    output_tokens = 7; cached_input_tokens = None;
    cache_creation_input_tokens = None; reasoning_output_tokens = None;
    input_modality_tokens = None; cached_input_modality_tokens = None;
    output_modality_tokens = None });
  let typed_result blocks = Pave.Protocol.tool_result_blocks call.id blocks in
  let assistant_message : Pave.Protocol.message = {
    role = "assistant"; content = None; tool_calls = [call]; tool_call_id = None;
    tool_result_content = None; provider_state = None; attachments = [] } in
  let png = "iVBORw0KGgo=" in
  let mixed = Wire.request
    [Pave.Protocol.user "Find alpha"; assistant_message;
     typed_result [Pave.Protocol.Text "before";
       Pave.Protocol.Image { mime_type = "image/png"; data = png };
       Pave.Protocol.Text "after"]] [tool] in
  assert (field "messages" mixed = `List [
    `Assoc ["role", `String "user"; "content", `List [
      `Assoc ["text", `String "Find alpha"]]];
    `Assoc ["role", `String "assistant"; "content", `List [
      `Assoc ["toolUse", `Assoc ["toolUseId", `String call.id;
        "name", `String call.name; "input", arguments]]]];
    `Assoc ["role", `String "user"; "content", `List [
      `Assoc ["toolResult", `Assoc ["toolUseId", `String call.id;
        "content", `List [
          `Assoc ["text", `String "before"];
          `Assoc ["image", `Assoc ["format", `String "png";
            "source", `Assoc ["bytes", `String png]]];
          `Assoc ["text", `String "after"]]]]]]]);
  let image_only = Wire.request
    [assistant_message;
     typed_result [Pave.Protocol.Image { mime_type = "image/jpeg"; data = "/9j/2Q==" }]] [tool] in
  assert (field "messages" image_only = `List [
    `Assoc ["role", `String "assistant"; "content", `List [
      `Assoc ["toolUse", `Assoc ["toolUseId", `String call.id;
        "name", `String call.name; "input", arguments]]]];
    `Assoc ["role", `String "user"; "content", `List [
      `Assoc ["toolResult", `Assoc ["toolUseId", `String call.id;
        "content", `List [`Assoc ["image", `Assoc ["format", `String "jpeg";
          "source", `Assoc ["bytes", `String "/9j/2Q=="]]]]]]]]]);
  expect_invalid (fun () -> Wire.request
    [assistant_message; typed_result [Pave.Protocol.Image {
      mime_type = "image/bmp"; data = "AA==" }]] [tool]);
  let attached = { (Pave.Protocol.user "describe") with attachments = [
    { name = "secret.jpg"; mime_type = "image/jpeg"; data = "/9j/2Q==" } ] } in
  assert (field "messages" (Wire.request [attached] []) = `List [
    `Assoc ["role", `String "user"; "content", `List [
      `Assoc ["text", `String "describe"];
      `Assoc ["image", `Assoc ["format", `String "jpeg";
        "source", `Assoc ["bytes", `String "/9j/2Q=="]]]]]]);
  assert (Aws.credential_process_policy
    ~getenv:(fun _ -> None) () = Aws.Disabled);
  assert (Pave.Provider_catalog.unclassified_models "amazon-bedrock");
  assert (Aws.credential_process_policy
    ~getenv:(fun _ -> Some "allow") () = Aws.Allow);
  (match Aws.credential_process_policy
      ~getenv:(fun _ -> Some "yes") () with
   | exception Invalid_argument _ -> ()
   | _ -> failwith "unknown credential_process policy was accepted");
  test_aws_credential_sources ();
  test_converse_stream_decoder ();
  fixture ();
  converse_stream_fixture ();
  print_endline "bedrock converse wire: ok"
