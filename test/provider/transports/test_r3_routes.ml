module P = Pave.Provider
module Protocol = Pave.Protocol
module Discovery = Pave.Model_discovery

let key = "fixture-private-key-42"
let args = `Assoc ["path", `String "README.md"]
let call_id = "call_r3_readme"
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "read_file";
    "description", `String "Read a workspace file";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["path", `Assoc ["type", `String "string"]];
      "required", `List [`String "path"]]]]
let user = Protocol.user "Read README.md"
let tool_call = { Protocol.id = call_id; name = "read_file"; arguments = args }
let tool_call_json = Protocol.call_to_json tool_call
let tool_result = Protocol.tool_result call_id "README body from the fixture"
let fail reason = failwith ("R3 route fixture: " ^ reason)
let field = Protocol.member
let assoc fields = `Assoc fields

type scenario = {
  id : string;
  endpoint : string;
  api : P.api;
  model : string;
  key : string;
  auth_header : string;
  thinking : string option;
  anthropic_messages : bool;
}

let scenarios = [
  { id = "minimax"; endpoint = Pave.Minimax_api.chat_url;
    api = P.Minimax_chat; model = "MiniMax-M3"; key;
    auth_header = "Authorization: Bearer " ^ key; thinking = None;
    anthropic_messages = false };
  { id = "deepseek"; endpoint = Pave.Deepseek_api.chat_url;
    api = P.Deepseek_chat; model = "deepseek-fixture-model"; key;
    auth_header = "Authorization: Bearer " ^ key; thinking = Some "max";
    anthropic_messages = false };
  { id = "mistral"; endpoint = Pave.Mistral_api.chat_url;
    api = P.Mistral_chat; model = "mistral-fixture-model"; key;
    auth_header = "Authorization: Bearer " ^ key; thinking = Some "high";
    anthropic_messages = false };
  { id = "openrouter"; endpoint = Pave.Openrouter_api.chat_url;
    api = P.Openrouter_chat; model = "vendor/model-with-a-slash"; key;
    auth_header = "Authorization: Bearer " ^ key; thinking = Some "xhigh";
    anthropic_messages = false };
  { id = "fireworks"; endpoint = Pave.Fireworks_api.chat_url;
    api = P.Fireworks_chat; model = "accounts/fireworks/models/fixture-reasoning";
    key = "fireworks-fixture-key";
    auth_header = "Authorization: Bearer fireworks-fixture-key";
    thinking = Some "minimal"; anthropic_messages = false };
  { id = "umans-chat"; endpoint = Pave.Umans_api.chat_url;
    api = P.Umans_chat; model = "umans-fixture-model"; key;
    auth_header = "Authorization: Bearer " ^ key; thinking = Some "max";
    anthropic_messages = false };
  { id = "umans-messages"; endpoint = Pave.Umans_api.messages_url;
    api = P.Umans_messages; model = "umans-fixture-model"; key;
    auth_header = "x-api-key: " ^ key; thinking = Some "max";
    anthropic_messages = true };
  { id = "cline-pass"; endpoint = Pave.Cline_pass_api.chat_url;
    api = P.Cline_pass_chat; model = "vendor/cline-model"; key;
    auth_header = "Authorization: Bearer " ^ key; thinking = None;
    anthropic_messages = false };
  { id = "alibaba-token-plan"; endpoint = Pave.Alibaba_token_plan_api.chat_url;
    api = P.Alibaba_token_plan_chat; model = "token-plan-fixture-model";
    key = "sk-sp-fixture-private-key";
    auth_header = "Authorization: Bearer sk-sp-fixture-private-key";
    thinking = Some "medium"; anthropic_messages = false };
  { id = "alibaba-coding-plan-cn";
    endpoint = Pave.Alibaba_coding_api.china_chat_url;
    api = P.Alibaba_coding_chat; model = "qwen-coding-fixture";
    key = "sk-sp-coding-fixture";
    auth_header = "Authorization: Bearer sk-sp-coding-fixture";
    thinking = Some "high"; anthropic_messages = false };
  { id = "alibaba-coding-plan-intl";
    endpoint = Pave.Alibaba_coding_api.intl_chat_url;
    api = P.Alibaba_coding_chat; model = "qwen-coding-fixture";
    key = "sk-sp-coding-fixture";
    auth_header = "Authorization: Bearer sk-sp-coding-fixture";
    thinking = Some "none"; anthropic_messages = false };
  { id = "kimi-code"; endpoint = Pave.Kimi_code_api.intl_openai_chat_url;
    api = P.Kimi_code_chat; model = "k3"; key = "kimi-fixture-key";
    auth_header = "Authorization: Bearer kimi-fixture-key"; thinking = None;
    anthropic_messages = false };
  { id = "kimi-code-cn"; endpoint = Pave.Kimi_code_api.china_openai_chat_url;
    api = P.Kimi_code_cn_chat; model = "kimi-for-coding"; key = "kimi-cn-fixture-key";
    auth_header = "Authorization: Bearer kimi-cn-fixture-key"; thinking = None;
    anthropic_messages = false };
  { id = "kimi-code-messages"; endpoint = Pave.Kimi_code_api.intl_messages_url;
    api = P.Kimi_code_messages; model = "k3-256k"; key = "kimi-fixture-key";
    auth_header = "x-api-key: kimi-fixture-key"; thinking = None;
    anthropic_messages = true };
  { id = "kimi-code-cn-messages"; endpoint = Pave.Kimi_code_api.china_messages_url;
    api = P.Kimi_code_cn_messages; model = "kimi-for-coding-highspeed";
    key = "kimi-cn-fixture-key";
    auth_header = "x-api-key: kimi-cn-fixture-key"; thinking = None;
    anthropic_messages = true };
]

let scenario id = match List.find_opt (fun entry -> entry.id = id) scenarios with
  | Some entry -> entry
  | None -> fail "unknown fixture provider"

let read_json_file path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
    Yojson.Basic.from_string (really_input_string input (in_channel_length input)))

let write_file path body =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output) (fun () ->
    output_string output body)

let response_chat ~model ~message ~finish =
  assoc ["id", `String "r3-completion";
    "object", `String "chat.completion";
    "model", `String model;
    "choices", `List [assoc ["index", `Int 0;
      "finish_reason", `String finish; "message", message]];
    "usage", assoc ["prompt_tokens", `Int 5; "completion_tokens", `Int 3]]

let chat_message ~content ?(extra = []) ?(calls = false) () =
  let fields = ["role", `String "assistant"; "content", content] @ extra in
  assoc (fields @ if calls then ["tool_calls", `List [tool_call_json]] else [])

let response_messages ~model ~content ~stop =
  assoc ["type", `String "message"; "role", `String "assistant";
    "model", `String model; "content", content; "stop_reason", `String stop;
    "usage", assoc ["input_tokens", `Int 5; "output_tokens", `Int 3]]

let response_for entry step =
  if entry.anthropic_messages then
    if step = 0 then (
      let tool_use = assoc ["type", `String "tool_use";
        "id", `String call_id; "name", `String "read_file"; "input", args] in
      let content = if List.mem entry.id ["umans-messages"; "kimi-code-messages";
          "kimi-code-cn-messages"] then
        [assoc ["type", `String "thinking";
          "thinking", `String "Read the requested file";
          "signature", `String "fixture-signed-thinking"]; tool_use]
        else [tool_use] in
      response_messages ~model:entry.model ~stop:"tool_use"
        ~content:(`List content))
    else response_messages ~model:entry.model ~stop:"end_turn"
      ~content:(`List [assoc ["type", `String "text";
        "text", `String "README read complete"]])
  else if step = 1 then response_chat ~model:entry.model ~finish:"stop"
      ~message:(chat_message ~content:(`String "README read complete") ())
  else
    let content, extra = match entry.id with
      | "minimax" -> `String "", ["reasoning_details", `List [assoc [
          "type", `String "reasoning.text"; "id", `String "mm-reasoning-1";
          "text", `String "Read the requested file"]]]
      | "deepseek" -> `Null, ["reasoning_content", `String "Read the requested file"]
      | "mistral" -> `List [
          assoc ["type", `String "thinking";
            "thinking", `List [assoc ["type", `String "text";
              "text", `String "Read the requested file"]]];
          assoc ["type", `String "text"; "text", `String "Checking README"]], []
      | "openrouter" -> `Null, ["reasoning_details", `List [assoc [
          "type", `String "reasoning.text"; "text", `String "Read the requested file"]]]
      | "umans-chat" -> `Null, ["reasoning_content", `String "Read the requested file"]
      | "fireworks" | "alibaba-coding-plan-cn"
      | "alibaba-coding-plan-intl" | "alibaba-token-plan" ->
          `Null, ["reasoning_content", `String "Read the requested file"]
      | _ -> `String "", [] in
    response_chat ~model:entry.model ~finish:"tool_calls"
      ~message:(chat_message ~content ~extra ~calls:true ())

let check_chat_request entry step body =
  assert (field "model" body = `String entry.model);
  assert (field "stream" body = `Bool false);
  assert (field "tools" body = `List [tool]);
  (match field "messages" body with
   | `List [first] when step = 0 ->
       assert (field "role" first = `String "user");
       assert (field "content" first = `String "Read README.md")
   | `List [first; assistant; result] when step = 1 ->
       assert (field "role" first = `String "user");
       assert (field "role" assistant = `String "assistant");
       assert (field "tool_calls" assistant = `List [tool_call_json]);
       assert (field "role" result = `String "tool");
       assert (field "tool_call_id" result = `String call_id);
       assert (field "content" result = `String "README body from the fixture");
       (match entry.id with
        | "minimax" ->
            assert (field "reasoning_details" assistant =
              `List [assoc ["type", `String "reasoning.text";
                "id", `String "mm-reasoning-1";
                "text", `String "Read the requested file"]])
        | "deepseek" ->
            assert (field "reasoning_content" assistant =
              `String "Read the requested file")
        | "mistral" ->
            assert (field "content" assistant = `List [
              assoc ["type", `String "thinking";
                "thinking", `List [assoc ["type", `String "text";
                  "text", `String "Read the requested file"]]];
              assoc ["type", `String "text"; "text", `String "Checking README"]])
        | "openrouter" ->
            assert (field "reasoning_details" assistant =
              `List [assoc ["type", `String "reasoning.text";
                "text", `String "Read the requested file"]])
        | "umans-chat" ->
            assert (field "reasoning_content" assistant =
              `String "Read the requested file")
        | "fireworks" | "alibaba-coding-plan-cn"
        | "alibaba-coding-plan-intl" | "alibaba-token-plan" ->
            assert (field "reasoning_content" assistant =
              `String "Read the requested file")
        | _ -> ())
   | _ -> fail "Chat transcript did not contain the matching tool result");
  (match entry.id with
   | "minimax" -> assert (field "reasoning_split" body = `Bool true)
   | "deepseek" ->
       assert (field "thinking" body = assoc ["type", `String "enabled"]);
       assert (field "reasoning_effort" body = `String "max")
   | "mistral" -> assert (field "reasoning_effort" body = `String "high")
   | "openrouter" ->
       assert (field "reasoning_effort" body = `String "xhigh");
       (match field "tools" body with
        | `List [`Assoc ["type", `String "function"; "function", `Assoc fields]] ->
            assert (not (List.mem_assoc "strict" fields))
        | _ -> fail "OpenRouter tool schema changed")
   | "umans-chat" -> assert (field "reasoning_effort" body = `String "max")
   | "fireworks" ->
       assert (field "reasoning_effort" body = `String "none");
       assert (field "thinking" body = `Null)
   | "alibaba-coding-plan-cn" | "alibaba-token-plan" ->
       assert (field "enable_thinking" body = `Bool true);
       assert (field "reasoning_effort" body = `Null)
   | "alibaba-coding-plan-intl" ->
       assert (field "enable_thinking" body = `Bool false);
       assert (field "reasoning_effort" body = `Null)
   | _ ->
       assert (field "reasoning_effort" body = `Null);
       assert (field "reasoning_split" body = `Null))

let check_messages_request entry step body =
  assert (field "model" body = `String entry.model);
  assert (field "max_tokens" body = `Int 4096);
  assert (field "stream" body = `Null);
  assert (field "reasoning_effort" body =
    (if entry.id = "umans-messages" then `String "max" else `Null));
  match field "messages" body with
  | `List [first] when step = 0 ->
      assert (field "role" first = `String "user");
      assert (field "content" first = `String "Read README.md")
  | `List [first; assistant; result] when step = 1 ->
      assert (field "role" first = `String "user");
      assert (field "role" assistant = `String "assistant");
      (match field "content" assistant with
       | `List [thinking; use] when List.mem entry.id
            ["umans-messages"; "kimi-code-messages"; "kimi-code-cn-messages"] ->
           assert (field "type" thinking = `String "thinking");
           assert (field "thinking" thinking =
             `String "Read the requested file");
           assert (field "signature" thinking =
             `String "fixture-signed-thinking");
           assert (field "type" use = `String "tool_use");
           assert (field "id" use = `String call_id);
           assert (field "name" use = `String "read_file");
           assert (field "input" use = args)
       | `List [use] ->
           assert (field "type" use = `String "tool_use");
           assert (field "id" use = `String call_id);
           assert (field "name" use = `String "read_file");
           assert (field "input" use = args)
       | _ -> fail "Messages assistant tool-use turn was not replayed");
      assert (field "role" result = `String "user");
      (match field "content" result with
       | `List [result] ->
           assert (field "type" result = `String "tool_result");
           assert (field "tool_use_id" result = `String call_id);
           assert (field "content" result =
             `String "README body from the fixture")
       | _ -> fail "Messages tool result was not replayed")
  | _ -> fail "Messages transcript did not contain the matching tool result"

let fake_curl () =
  let config = ref [] in
  (try while true do config := input_line stdin :: !config done
   with End_of_file -> ());
  let config = List.rev !config in
  let values name = List.filter_map (fun line ->
    let prefix = name ^ " = " in
    if String.starts_with ~prefix line then
      match Yojson.Basic.from_string
        (String.sub line (String.length prefix)
          (String.length line - String.length prefix)) with
      | `String value -> Some value
      | _ -> fail "invalid curl configuration"
    else None) config in
  let one name = match values name with
    | [value] -> value | _ -> fail ("missing or repeated curl " ^ name) in
  let entry = scenario (Sys.getenv "PAVE_R3_CASE") in
  assert (one "url" = entry.endpoint);
  assert (one "request" = "POST");
  assert (one "proto" = "=https");
  assert (not (List.mem "location" config));
  assert (not (List.mem "location-trusted" config));
  assert (List.mem entry.auth_header (values "header"));
  if String.starts_with ~prefix:"kimi-code" entry.id then
    assert (List.mem "User-Agent: Pave" (values "header"));
  assert (List.mem "Content-Type: application/json" (values "header"));
  if entry.anthropic_messages then
    assert (List.mem "anthropic-version: 2023-06-01" (values "header"));
  let body_path = one "data-binary" in
  assert (String.starts_with ~prefix:"@" body_path);
  let body = read_json_file
    (String.sub body_path 1 (String.length body_path - 1)) in
  let state_path = Sys.getenv "PAVE_R3_STATE" in
  let step = if Sys.file_exists state_path then
    let input = open_in state_path in
    Fun.protect ~finally:(fun () -> close_in_noerr input)
      (fun () -> int_of_string (input_line input))
    else 0 in
  if step > 1 then fail "unexpected third provider request";
  if entry.anthropic_messages then check_messages_request entry step body
  else check_chat_request entry step body;
  let response = Yojson.Basic.to_string (response_for entry step) in
  write_file (one "output") response;
  write_file state_path (string_of_int (step + 1));
  print_string "200";
  flush stdout

let preserve_env name action =
  let old = Sys.getenv_opt name in
  Fun.protect ~finally:(fun () ->
    match old with None -> Unix.putenv name "" | Some value -> Unix.putenv name value)
    action

let with_fixture entry action =
  let directory = Filename.temp_file "pave-r3-route-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let binary = Filename.concat directory "curl" in
  let state = Filename.concat directory "state" in
  Unix.symlink (Unix.realpath Sys.executable_name) binary;
  let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
  preserve_env "PATH" (fun () ->
    preserve_env "PAVE_R3_CASE" (fun () ->
      preserve_env "PAVE_R3_STATE" (fun () ->
        Unix.putenv "PATH" (directory ^ ":" ^ old_path);
        Unix.putenv "PAVE_R3_CASE" entry.id;
        Unix.putenv "PAVE_R3_STATE" state;
        Fun.protect ~finally:(fun () ->
          List.iter (fun path -> if Sys.file_exists path then Sys.remove path)
            [binary; state];
          Unix.rmdir directory) action)))

let expect_provider_error action = match action () with
  | exception P.Provider_error _ -> ()
  | _ -> fail "invalid endpoint or thinking level was accepted"

let run_provider_route entry =
  let config : P.config = { endpoint = entry.endpoint; api_key = entry.key;
    model = entry.model; api = entry.api } in
  with_fixture entry (fun () ->
    let hostile = { config with endpoint = "https://attacker.invalid/chat/completions" } in
    expect_provider_error (fun () -> P.complete hostile [user] [tool]);
    let state = Sys.getenv "PAVE_R3_STATE" in
    assert (not (Sys.file_exists state));
    if Option.is_some entry.thinking then
      expect_provider_error (fun () -> P.complete ~thinking:"unsupported" config
        [user] [tool]);
    assert (not (Sys.file_exists state));
    let emitted = ref [] in
    let first = P.complete ?thinking:entry.thinking
      ~on_text:(fun text -> emitted := text :: !emitted)
      config [user] [tool] in
    assert (first.tool_calls = [tool_call]);
    if List.mem entry.id ["umans-messages"; "kimi-code-messages";
        "kimi-code-cn-messages"] then (
      assert (first.content = None);
      assert (first.provider_state <> None));
    if entry.id = "mistral" then (
      assert (first.content = Some "Checking README");
      assert (List.rev !emitted = ["Checking README"]);
      assert (first.provider_state <> None));
    let final = P.complete ?thinking:entry.thinking config
      [user; first; tool_result] [tool] in
    assert (final.tool_calls = []);
    assert (final.content = Some "README read complete");
    let input = open_in state in
    let requests = Fun.protect ~finally:(fun () -> close_in_noerr input)
      (fun () -> int_of_string (input_line input)) in
    assert (requests = 2))

let test_discovery () =
  let minimax_body = {|{"data":[{"id":"fixture-minimax-model"}]}|} in
  let minimax_calls = ref 0 in
  let minimax_http ~url ~headers =
    incr minimax_calls;
    assert (url = Pave.Minimax_api.models_url);
    assert (headers = ["Authorization", "Bearer fixture-minimax-list-key"]);
    Ok (200, minimax_body) in
  let minimax = match Discovery.discover ~provider:"minimax"
      ~credential:(Discovery.Api_key "fixture-minimax-list-key")
      ~http:minimax_http () with
    | Ok listing -> listing
    | Error _ -> fail "MiniMax model listing failed" in
  assert (!minimax_calls = 1);
  assert (Discovery.model_ids minimax = ["fixture-minimax-model"]);
  assert (minimax.source.id_source = Pave.Model_catalog.Pinned_account_listing);
  assert (minimax.source.endpoint = Some Pave.Minimax_api.models_url);
  assert (Pave.Provider_catalog.unclassified_models "minimax");

  let umans_body = Yojson.Basic.to_string (`Assoc [
    ("fixture-vision-model", assoc [
      "name", `String "fixture-vision-model";
      "display_name", `String "Fixture Vision";
      "capabilities", assoc [
        "context_window", `Int 65536;
        "max_completion_tokens", `Int 8192;
        "supports_vision", `Bool true;
        "supports_tools", `Bool true;
        "reasoning", assoc [
          "supported", `Bool true;
          "levels", `List [`String "none"; `String "low"; `String "max"]]]]);
    ("fixture-text-model", assoc [
      "name", `String "fixture-text-model";
      "display_name", `String "Fixture Text";
      "capabilities", assoc [
        "context_window", `Int 32768;
        "max_completion_tokens", `Int 4096;
        "supports_vision", `Bool false;
        "supports_tools", `Bool false;
        "reasoning", assoc [
          "supported", `Bool false;
          "levels", `List [`String "low"]]]])
  ]) in
  let umans_calls = ref 0 in
  let umans_http ~url ~headers =
    incr umans_calls;
    assert (url = Pave.Umans_api.models_url);
    assert (headers = []);
    Ok (200, umans_body) in
  let umans = match Discovery.discover ~provider:"umans" ~http:umans_http () with
    | Ok listing -> listing
    | Error _ -> fail "Umans public model info failed" in
  assert (!umans_calls = 1);
  assert (Discovery.model_ids umans =
    ["fixture-vision-model"; "fixture-text-model"]);
  assert (umans.source.id_source = Pave.Model_catalog.Provider_listing);
  assert (umans.source.endpoint = Some Pave.Umans_api.models_url);
  let vision = List.hd umans.models in
  assert (vision.display_name = Some "Fixture Vision");
  assert (vision.capabilities.tools = Some true);
  assert (vision.capabilities.context_window_tokens = Some 65536);
  assert (vision.capabilities.max_output_tokens = Some 8192);
  assert (vision.capabilities.input_modalities =
    Some [Pave.Model_catalog.Text; Pave.Model_catalog.Image]);
  assert (vision.capabilities.effort_levels = Some ["none"; "low"; "max"]);
  assert (vision.provenance.capability_source =
    Some Pave.Model_catalog.Capability_response);
  let text = List.nth umans.models 1 in
  assert (text.capabilities.tools = Some false);
  assert (text.capabilities.input_modalities = Some [Pave.Model_catalog.Text]);
  assert (text.capabilities.effort_levels = None);
  assert (Pave.Provider_catalog.route
    (Option.get (Pave.Provider_catalog.find "umans")) "chat" <> None);

  List.iter (fun provider ->
    match Discovery.discover ~provider () with
    | Error (Discovery.Unsupported_provider actual) when actual = provider -> ()
    | _ -> fail (provider ^ " unexpectedly inferred a model listing"))
    ["cline-pass"; "alibaba-token-plan"; "kimi-code"; "kimi-code-cn"];
  let deepseek = Option.get (Pave.Provider_catalog.find "deepseek") in
  assert ((Option.get (Pave.Provider_catalog.route deepseek "chat")).wire =
    P.Deepseek_chat);
  let mistral = Option.get (Pave.Provider_catalog.find "mistral") in
  assert ((Option.get (Pave.Provider_catalog.route mistral "chat")).wire =
    P.Mistral_chat);
  let openrouter = Option.get (Pave.Provider_catalog.find "openrouter") in
  assert ((Option.get (Pave.Provider_catalog.route openrouter "chat")).wire =
    P.Openrouter_chat);
  let fireworks = Option.get (Pave.Provider_catalog.find "fireworks") in
  assert ((Option.get (Pave.Provider_catalog.route fireworks "chat")).wire =
    P.Fireworks_chat);
  let kimi_intl = Option.get (Pave.Provider_catalog.find "kimi-code") in
  let kimi_cn = Option.get (Pave.Provider_catalog.find "kimi-code-cn") in
  assert ((Option.get (Pave.Provider_catalog.route kimi_intl "chat")).endpoint =
    Pave.Kimi_code_api.intl_openai_chat_url);
  assert ((Option.get (Pave.Provider_catalog.route kimi_cn "chat")).endpoint =
    Pave.Kimi_code_api.china_openai_chat_url);
  assert ((Option.get (Pave.Provider_catalog.route kimi_intl "messages")).endpoint =
    Pave.Kimi_code_api.intl_messages_url);
  assert ((Option.get (Pave.Provider_catalog.route kimi_cn "messages")).endpoint =
    Pave.Kimi_code_api.china_messages_url);
  (match Pave.Umans_api.parse_models
    {|{"m":{"name":"m","capabilities":{"supports_tools":true,"supports_tools":false}}}|} with
   | Error _ -> ()
   | Ok _ -> fail "duplicate Umans capability field was accepted");
  (match Pave.Umans_api.parse_models
    {|{"m":{"name":"m","capabilities":{"supports_tools":true}}}|} with
   | Ok [_] -> ()
   | _ -> fail "valid minimal Umans capability entry was rejected")

let () =
  if Array.length Sys.argv >= 3 && Sys.argv.(1) = "--disable" &&
      Sys.argv.(2) = "--config" then
    (try fake_curl () with exn ->
      prerr_endline (Printexc.to_string exn); exit 2)
  else (
    test_discovery ();
    List.iter run_provider_route scenarios;
    print_endline "R3 provider routes, model discovery, reasoning replay, and tool-result turns: ok")
