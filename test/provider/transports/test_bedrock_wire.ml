let field = Pave.Protocol.member
module Aws = Pave.Aws_auth
module Wire = Pave.Bedrock_wire

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
      "AWS_SESSION_TOKEN", "SESSIONTEST"] (fun () ->
      let config : Pave.Provider.config = {
        endpoint = Printf.sprintf "http://127.0.0.1:%d" port;
        api_key = ""; model = "fixture-model"; api = Pave.Provider.Bedrock_converse } in
      let original = [Pave.Protocol.user "Find alpha"] in
      let first = Pave.Provider.complete config original [tool] in
      assert (first.content = Some "Looking up.");
      assert (first.tool_calls = [call]);
      let continuation = original @ [first; Pave.Protocol.tool_result call.id "value-alpha"] in
      let callbacks = ref [] in
      let second = Pave.Provider.complete
        ~on_text:(fun content -> callbacks := content :: !callbacks)
        config continuation [tool] in
      assert (second.content = Some "Found alpha.");
      assert (second.tool_calls = []);
      assert (!callbacks = ["Found alpha."]);
      let _, status = Unix.waitpid [] child in
      reaped := true;
      assert (status = Unix.WEXITED 0)))

let () =
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
  let target = Wire.endpoint ~region:"eu-west-1" ~model:"arn:aws:bedrock:eu-west-1:123:profile/a" () in
  assert (String.contains target.path '%');
  (match Wire.endpoint ~base_url:"http://example.test" ~region:"eu-west-1" ~model:"sample" () with
   | exception Invalid_argument _ -> ()
   | _ -> failwith "untrusted endpoint accepted");
  expect_invalid (fun () -> Wire.parse_response (answer "max_tokens" [`Assoc ["text", `String "partial"]]));
  expect_invalid (fun () -> Wire.parse_response (answer "end_turn" [
    `Assoc ["reasoningContent", `Assoc ["reasoningText", `Assoc [
      "text", `String "unpreserved"; "signature", `String "signed"]]]]));
  let history : Pave.Protocol.message = {
    role = "assistant"; content = None; tool_calls = [call];
    tool_call_id = None; provider_state = None } in
  expect_invalid (fun () -> Wire.request
    [Pave.Protocol.user "Find alpha"; history;
      Pave.Protocol.tool_result call.id "value-alpha"] []);
  assert (Wire.usage final_answer = Some { Pave.Protocol.input_tokens = 12; output_tokens = 7 });
  fixture ();
  print_endline "bedrock converse wire: ok"
