open Pave

let field name json = Protocol.member name json
let invalid f = match f () with
  | exception Invalid_argument _ | exception Provider.Provider_error _ -> ()
  | _ -> failwith "unsafe Azure configuration was accepted"

let completed_call = `Assoc [ "id", `String "fc_72";
  "type", `String "function_call"; "status", `String "completed";
  "call_id", `String "call_72"; "name", `String "lookup";
  "arguments", `String {|{"query":"hello"}|} ]
let response = `Assoc [ "status", `String "completed";
  "output", `List [completed_call] ]
let event kind fields =
  let json = `Assoc (("type", `String kind) :: fields) in
  "event: " ^ kind ^ "\r\ndata: " ^ Yojson.Basic.to_string json ^ "\r\n\r\n"

let expect_auth_failure f = match f () with
  | exception Azure_auth.Authentication_error _ -> ()
  | _ -> failwith "Azure Entra authentication accepted an unsafe result"

let expect_cancelled f = match f () with
  | exception Azure_auth.Cancelled -> ()
  | _ -> failwith "Azure Entra authentication ignored cancellation"

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect ~finally:(fun () ->
    match previous with
    | Some value -> Unix.putenv name value
    | None -> Unix.putenv name "") f

let read_file path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let with_fake_az f =
  let directory = Filename.temp_file "pave-azure-auth-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let executable = Filename.concat directory "az" in
  let args_path = Filename.concat directory "arguments" in
  let script = {|#!/bin/sh
printf '%s\n' "$@" > "$AZURE_TEST_ARGS"
case "$AZURE_TEST_MODE" in
  token) printf '%s\n' "$AZURE_TEST_TOKEN" ;;
  overflow) printf '%020000d' 0 ;;
  busy) while :; do :; done ;;
esac
|} in
  let output = open_out_bin executable in
  output_string output script;
  close_out output;
  Unix.chmod executable 0o700;
  Fun.protect ~finally:(fun () ->
    List.iter (fun path -> try Sys.remove path with Sys_error _ -> ())
      [executable; args_path];
    try Unix.rmdir directory with Unix.Unix_error _ -> ())
    (fun () ->
      with_env "PATH" directory (fun () ->
        with_env "AZURE_TEST_ARGS" args_path (fun () -> f args_path)))

let azure_auth_fixtures () =
  let openai_endpoint =
    "https://project-7.openai.azure.com/openai/v1/responses?api-version=v1" in
  let foundry_endpoint =
    "https://project-7.services.ai.azure.com/openai/v1/chat/completions" in
  with_fake_az (fun args_path ->
    with_env "AZURE_TEST_MODE" "token" (fun () ->
      with_env "AZURE_TEST_TOKEN" "eyJfake.azure.token" (fun () ->
        assert (Azure_auth.access_token ~endpoint:openai_endpoint () =
          "eyJfake.azure.token");
        assert (read_file args_path =
          "account\nget-access-token\n--scope\nhttps://cognitiveservices.azure.com/.default\n--query\naccessToken\n--output\ntsv\n");
        assert (Azure_auth.access_token ~endpoint:foundry_endpoint () =
          "eyJfake.azure.token");
        assert (read_file args_path =
          "account\nget-access-token\n--scope\nhttps://ai.azure.com/.default\n--query\naccessToken\n--output\ntsv\n")));
    (try Sys.remove args_path with Sys_error _ -> ());
    expect_auth_failure (fun () ->
      Azure_auth.access_token
        ~endpoint:"https://project-7.openai.azure.com.evil.example/openai/v1/responses" ());
    assert (not (Sys.file_exists args_path));
    with_env "AZURE_TEST_MODE" "overflow" (fun () ->
      expect_auth_failure (fun () ->
        Azure_auth.access_token ~endpoint:openai_endpoint ()));
    with_env "AZURE_TEST_MODE" "busy" (fun () ->
      expect_auth_failure (fun () ->
        Azure_auth.access_token ~endpoint:openai_endpoint ~timeout:0.05 ()));
    with_env "AZURE_TEST_MODE" "busy" (fun () ->
      let checks = ref 0 in
      expect_cancelled (fun () ->
        Azure_auth.access_token ~endpoint:openai_endpoint
          ~cancel:(fun () -> incr checks; !checks >= 2) ())))

let () =
  (* Microsoft REST reference: v1 routes take the deployment alias in the
     JSON `model` field; API-key and Entra bearer headers are separate. *)
  let base = "https://project-7.openai.azure.com" in
  let endpoint = base ^ "/openai/v1/responses?api-version=v1" in
  assert (Azure_wire.resource_name ~endpoint = "project-7");
  let resolved, headers = Azure_wire.resolve ~route:Azure_wire.Responses
    ~endpoint ~deployment:"my-deployment"
    ~authentication:(Azure_wire.Api_key "private-azure-key") in
  assert (resolved = endpoint);
  assert (headers = ["api-key: private-azure-key"]);
  assert (Azure_wire.deployment_model ~deployment:"my-deployment" |> Yojson.Basic.to_string
    = {|{"model":"my-deployment"}|});
  assert (Yojson.Basic.to_string
    (Azure_wire.deployment_model ~deployment:"deployment-alias")
    = {|{"model":"deployment-alias"}|});
  let foundry_base = "https://project-7.services.ai.azure.com" in
  let chat_endpoint = foundry_base ^ "/openai/v1/chat/completions" in
  assert (Azure_wire.resource_name ~endpoint:chat_endpoint = "project-7");
  let chat_url, chat_headers = Azure_wire.resolve ~route:Azure_wire.Chat_completions
    ~endpoint:chat_endpoint ~deployment:"deployment-alias"
    ~authentication:(Azure_wire.Api_key "private-foundry-key") in
  assert (chat_url = chat_endpoint);
  assert (chat_headers = ["api-key: private-foundry-key"]);
  let _, foundry_bearer = Azure_wire.resolve ~route:Azure_wire.Responses
    ~endpoint:(foundry_base ^ "/openai/v1/responses")
    ~deployment:"deployment-alias"
    ~authentication:(Azure_wire.Entra_token "entra-token") in
  assert (foundry_bearer = ["Authorization: Bearer entra-token"]);
  let _, openai_bearer = Azure_wire.resolve ~route:Azure_wire.Responses
    ~endpoint:(base ^ "/openai/v1/responses")
    ~deployment:"my-deployment"
    ~authentication:(Azure_wire.Entra_token "entra-token") in
  assert (openai_bearer = ["Authorization: Bearer entra-token"]);
  assert (Azure_wire.resolve ~route:Azure_wire.Responses
    ~endpoint:(base ^ "/openai/v1/responses?api-version=preview")
    ~deployment:"my-deployment"
    ~authentication:(Azure_wire.Api_key "private-azure-key") |> fst
    = base ^ "/openai/v1/responses?api-version=preview");
  let child = Unix.fork () in
  if child = 0 then (
    Unix.putenv "AZURE_OPENAI_ENDPOINT" (foundry_base ^ "/");
    assert (Azure_wire.endpoint ~route:Azure_wire.Chat_completions () =
      foundry_base ^ "/openai/v1/chat/completions?api-version=v1");
    Unix.putenv "AZURE_OPENAI_ENDPOINT" (base ^ "/");
    Unix.putenv "AZURE_OPENAI_API_VERSION" "preview";
    assert (Azure_wire.endpoint () =
      base ^ "/openai/v1/responses?api-version=preview");
    assert (Azure_wire.endpoint ~route:Azure_wire.Chat_completions () =
      base ^ "/openai/v1/chat/completions?api-version=preview");
    Unix.putenv "AZURE_OPENAI_API_VERSION" "";
    assert (Azure_wire.endpoint () = base ^ "/openai/v1/responses?api-version=v1");
    Unix.putenv "AZURE_OPENAI_API_VERSION" "2025-04-01-preview";
    invalid Azure_wire.endpoint;
    Unix.putenv "AZURE_OPENAI_ENDPOINT" "https://project-7.openai.azure.com.evil.example";
    invalid Azure_wire.endpoint;
    exit 0);
  (match snd (Unix.waitpid [] child) with
   | Unix.WEXITED 0 -> ()
   | _ -> failwith "Azure endpoint environment fixture failed");
  azure_auth_fixtures ();
  let user : Protocol.message = { role = "user"; content = Some "Find greeting";
    tool_calls = []; tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] } in
  let call : Protocol.tool_call = { id = "call_72"; name = "lookup";
    arguments = `Assoc ["query", `String "hello"] } in
  let expected : Protocol.message = { role = "assistant"; content = None;
    tool_calls = [call]; tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] } in
  let parameters = `Assoc ["type", `String "object"; "properties",
    `Assoc ["query", `Assoc ["type", `String "string"]]] in
  let tool = `Assoc ["type", `String "function"; "function", `Assoc [
    "name", `String "lookup"; "description", `String "Look up a greeting";
    "parameters", parameters]] in
  let first_request = Openai_responses_wire.request ~model:"my-deployment"
    [user] [tool] in
  assert (field "model" first_request = `String "my-deployment");
  assert (field "tools" first_request = `List [`Assoc [
    "type", `String "function"; "name", `String "lookup";
    "parameters", `Assoc ["type", `String "object"; "properties", `Assoc [
      "query", `Assoc ["type", `String "string"]]];
    "strict", `Bool false; "description", `String "Look up a greeting" ]]);
  let buffered = Openai_responses_wire.parse_completion response in
  assert (buffered = expected);
  let streamed = Openai_responses_stream.create ~on_text:(fun _ -> ()) () in
  let initial_call = `Assoc [ "id", `String "fc_72";
    "type", `String "function_call"; "call_id", `String "call_72";
    "name", `String "lookup"; "arguments", `String "" ] in
  let wire = event "response.output_item.added" [
      "output_index", `Int 0; "item", initial_call ]
    ^ event "response.function_call_arguments.delta" [
      "output_index", `Int 0; "item_id", `String "fc_72";
      "delta", `String {|{"query":"hel|} ]
    ^ event "response.function_call_arguments.delta" [
      "output_index", `Int 0; "item_id", `String "fc_72";
      "delta", `String {|lo"}|} ]
    ^ event "response.function_call_arguments.done" [
      "output_index", `Int 0; "item_id", `String "fc_72";
      "name", `String "lookup"; "arguments", `String {|{"query":"hello"}|} ]
    ^ event "response.output_item.done" [
      "output_index", `Int 0; "item", completed_call ]
    ^ event "response.completed" ["response", response]
    ^ "data: [DONE]\r\n\r\n" in
  String.iter (fun c -> Openai_responses_stream.feed streamed (String.make 1 c)) wire;
  assert (Openai_responses_stream.finish streamed = expected);
  let tool_result = Protocol.tool_result "call_72" "Found greeting" in
  let continued = Openai_responses_wire.request ~stream:true
    ~model:"my-deployment" [user; buffered; tool_result] [] in
  assert (field "stream" continued = `Bool true);
  assert (field "input" continued = `List [
    `Assoc ["role", `String "user"; "content", `List [
      `Assoc ["type", `String "input_text"; "text", `String "Find greeting"] ]];
    `Assoc ["type", `String "function_call"; "call_id", `String "call_72";
      "name", `String "lookup"; "arguments", `String {|{"query":"hello"}|} ];
    `Assoc ["type", `String "function_call_output"; "call_id", `String "call_72";
      "output", `String "Found greeting"] ]);
  let answer = `Assoc [ "status", `String "completed"; "output", `List [
    `Assoc [ "type", `String "message"; "role", `String "assistant";
      "content", `List [ `Assoc [ "type", `String "output_text";
        "text", `String "Hello from lookup" ] ] ] ] ] in
  assert ((Openai_responses_wire.parse_completion answer).content =
    Some "Hello from lookup");
  let emitted = ref [] in
  let final_stream = Openai_responses_stream.create ~on_text:(fun text -> emitted := text :: !emitted) () in
  Openai_responses_stream.feed final_stream
    (event "response.completed" ["response", answer] ^ "data: [DONE]\r\n\r\n");
  assert ((Openai_responses_stream.finish final_stream).content =
    Some "Hello from lookup");
  assert (List.rev !emitted = ["Hello from lookup"]);
  let config endpoint : Provider.config = {
    api = Provider.Azure_responses; model = "my-deployment";
    api_key = "private-azure-key"; endpoint } in
  let unsafe_endpoint endpoint =
    invalid (fun () -> Azure_wire.resolve ~route:Azure_wire.Responses
      ~endpoint ~deployment:"my-deployment"
      ~authentication:(Azure_wire.Api_key "private-azure-key"));
    invalid (fun () -> Provider.complete (config endpoint) [user] []) in
  List.iter unsafe_endpoint [
    "http://project-7.openai.azure.com/openai/v1/responses";
    "https://project-7.openai.azure.com.evil.example/openai/v1/responses";
    "https://project-7.openai.azure.com@evil.example/openai/v1/responses";
    "https://project-7.openai.azure.com:443/openai/v1/responses";
    "https://project-7.openai.azure.com/openai/v1/responses#evil";
    "https://project-7.openai.azure.com/openai/v1/responses?api-version=bad";
    "https://project-7.openai.azure.com/openai/v1/responses/../evil";
    "https://project-7.openai.azure.us/openai/v1/responses";
    "https://project-7.services.ai.azure.us/openai/v1/responses";
    "https://project-7.openai.chinacloudapi.cn/openai/v1/responses";
    "https://project-7.services.ai.azure.com.evil.example/openai/v1/responses" ];
  invalid (fun () -> Azure_wire.resolve ~route:Azure_wire.Responses
    ~endpoint ~deployment:""
    ~authentication:(Azure_wire.Api_key "private-azure-key"));
  invalid (fun () -> Azure_wire.resolve ~route:Azure_wire.Responses
    ~endpoint ~deployment:"my-deployment"
    ~authentication:(Azure_wire.Api_key ""));
  invalid (fun () -> Azure_wire.resolve ~route:Azure_wire.Responses
    ~endpoint ~deployment:"my-deployment"
    ~authentication:(Azure_wire.Api_key "private-azure-key\r\nHost: evil.example"));
  invalid (fun () -> Azure_wire.resolve ~route:Azure_wire.Responses
    ~endpoint:chat_endpoint ~deployment:"deployment-alias"
    ~authentication:(Azure_wire.Api_key "private-azure-key"));
  invalid (fun () -> Provider.complete ~authentication:Provider.OAuth
    (config endpoint) [user] []);
  print_endline "Azure OpenAI Responses endpoint and tool turn: ok"
