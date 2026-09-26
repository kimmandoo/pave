module Command = Pave.Commandcode_api
module Protocol = Pave.Protocol
let field = Protocol.member
let key = "user_private-fixture"
let model = "fixture-server-model"
let call_id = "call_command_7"
let args = `Assoc ["left", `Int 8; "right", `Int 13]
let properties = `Assoc ["left", `Assoc ["type", `String "integer"];
  "right", `Assoc ["type", `String "integer"]]
let parameters = `Assoc ["type", `String "object"; "properties", properties]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "add";
    "description", `String "Add two integers";
    "parameters", parameters]]
let fail text = failwith ("Command Code native fixture: " ^ text)
let expect_invalid f = match f () with
  | exception Invalid_argument _ | exception Protocol.Invalid_response _ -> ()
  | _ -> fail "unsafe endpoint or transcript accepted"
let expect_provider_rejected f = match f () with
  | exception Pave.Provider.Provider_error _ -> ()
  | _ -> fail "unsafe provider config accepted"

(* curl is intercepted by this binary so POST goes through the real HTTPS
   executor; no live Studio key or external connection is needed. *)
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
    | [value] -> value | _ -> fail ("missing curl " ^ name) in
  let endpoint = Sys.getenv "PAVE_COMMANDCODE_FIXTURE_ENDPOINT" in
  assert (List.mem endpoint [Command.chat_url; Command.responses_url; Command.messages_url]);
  assert (one "url" = endpoint);
  assert (one "proto" = "=https");
  assert (one "request" = "POST");
  assert (not (List.mem "location" config));
  assert (not (List.mem "location-trusted" config));
  assert (List.mem ("Authorization: Bearer " ^ key) (values "header"));
  assert (List.mem "Content-Type: application/json" (values "header"));
  if endpoint = Command.messages_url then
    assert (List.mem "anthropic-version: 2023-06-01" (values "header"));
  let state_path = Sys.getenv "PAVE_COMMANDCODE_FIXTURE_STATE" in
  let step = if Sys.file_exists state_path then (
    let input = open_in state_path in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let body_path = one "data-binary" in
  assert (String.starts_with ~prefix:"@" body_path);
  let input = open_in_bin
    (String.sub body_path 1 (String.length body_path - 1)) in
  let body = Fun.protect ~finally:(fun () -> close_in input)
    (fun () -> Yojson.Basic.from_string
      (really_input_string input (in_channel_length input))) in
  assert (field "model" body = `String model);
  let tool_call = `Assoc ["id", `String call_id;
    "type", `String "function";
    "function", `Assoc ["name", `String "add";
      "arguments", `String (Yojson.Basic.to_string args)]] in
  let response = if endpoint = Command.chat_url then (
    assert (field "stream" body = `Bool false);
    assert (field "tools" body = `List [tool]);
    (match step with
    | 0 ->
        assert (field "messages" body = `List [
          `Assoc ["role", `String "user"; "content", `String "Eight plus thirteen?"]]);
        {|{"choices":[{"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"Add 8 and 13.","tool_calls":[{"id":"call_command_7","type":"function","function":{"name":"add","arguments":"{\"left\":8,\"right\":13}"}}]}}]}|}
    | 1 ->
        assert (field "messages" body = `List [
          `Assoc ["role", `String "user"; "content", `String "Eight plus thirteen?"];
          `Assoc ["role", `String "assistant";
            "tool_calls", `List [tool_call]; "content", `Null;
            "reasoning_content", `String "Add 8 and 13."];
          `Assoc ["role", `String "tool";
            "content", `String "21"; "tool_call_id", `String call_id]]);
        {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"21"}}]}|}
    | _ -> fail "unexpected Chat request"))
  else if endpoint = Command.messages_url then (
    assert (field "max_tokens" body = `Int 4096);
    let expected_tool = `Assoc ["name", `String "add";
      "input_schema", field "parameters" (field "function" tool);
      "description", `String "Add two integers"] in
    assert (field "tools" body = `List [expected_tool]);
    (match step with
    | 0 ->
        assert (field "messages" body = `List [
          `Assoc ["role", `String "user"; "content", `String "Eight plus thirteen?"]]);
        {|{"type":"message","role":"assistant","content":[{"type":"thinking","thinking":"Add 8 and 13.","signature":"sig-opaque"},{"type":"tool_use","id":"call_command_7","name":"add","input":{"left":8,"right":13}}],"stop_reason":"tool_use"}|}
    | 1 ->
        assert (field "messages" body = `List [
          `Assoc ["role", `String "user"; "content", `String "Eight plus thirteen?"];
          `Assoc ["role", `String "assistant"; "content", `List [
            `Assoc ["type", `String "thinking";
              "thinking", `String "Add 8 and 13.";
              "signature", `String "sig-opaque"];
            `Assoc ["type", `String "tool_use";
              "id", `String call_id; "name", `String "add"; "input", args]]];
          `Assoc ["role", `String "user"; "content", `List [
            `Assoc ["type", `String "tool_result";
              "tool_use_id", `String call_id; "content", `String "21"]]]]);
        {|{"type":"message","role":"assistant","content":[{"type":"text","text":"21"}],"stop_reason":"end_turn"}|}
    | _ -> fail "unexpected Messages request"))
  else (
    assert (field "store" body = `Bool false);
    assert (field "include" body = `List [`String "reasoning.encrypted_content"]);
    assert (field "tools" body = `List [`Assoc [
      "type", `String "function"; "name", `String "add";
      "parameters", field "parameters" (field "function" tool);
      "strict", `Bool false;
      "description", `String "Add two integers"]]);
    let user = `Assoc ["role", `String "user"; "content", `List [
      `Assoc ["type", `String "input_text";
        "text", `String "Eight plus thirteen?"]]] in
    (match step with
    | 0 ->
        assert (field "input" body = `List [user]);
        {|{"status":"completed","model":"fixture-server-model","output":[{"type":"reasoning","encrypted_content":"cipher-opaque","summary":[]},{"type":"function_call","call_id":"call_command_7","name":"add","arguments":"{\"left\":8,\"right\":13}"}]}|}
    | 1 ->
        assert (field "input" body = `List [user;
          `Assoc ["type", `String "reasoning";
            "encrypted_content", `String "cipher-opaque";
            "summary", `List []];
          `Assoc ["type", `String "function_call";
            "call_id", `String call_id; "name", `String "add";
            "arguments", `String (Yojson.Basic.to_string args)];
          `Assoc ["type", `String "function_call_output";
            "call_id", `String call_id; "output", `String "21"]]);
        {|{"status":"completed","model":"fixture-server-model","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"21"}]}]}|}
    | _ -> fail "unexpected Responses request")) in
  let state = open_out state_path in
  output_string state (string_of_int (step + 1)); close_out state;
  let output = open_out_bin (one "output") in
  output_string output response; close_out output;
  print_string "200"; flush stdout

let () =
  if Array.length Sys.argv >= 3 && Sys.argv.(1) = "--disable" && Sys.argv.(2) = "--config" then
    (try fake_curl () with exn ->
       prerr_endline (Printexc.to_string exn); exit 2)
  else (
    let endpoints = [Command.chat_url; Command.messages_url; Command.responses_url] in
    List.iter (fun (url, make_headers) ->
      assert (List.mem ("Authorization: Bearer " ^ key)
        (make_headers ~endpoint:url ~api_key:key));
      List.iter (fun api_key ->
        expect_invalid (fun () -> make_headers ~endpoint:url ~api_key))
        [""; "user_fault\ninjection"; "user_fault\rheader"];
      List.iter (fun endpoint ->
        if endpoint <> url then
          expect_invalid (fun () -> make_headers ~endpoint ~api_key:key))
        (endpoints @ [Command.models_url;
          "https://api.commandcode.ai/provider/v1/chat/completions?redirect=https://evil.test";
          "https://api.commandcode.ai.evil.test/provider/v1/chat/completions";
          "http://api.commandcode.ai/provider/v1/messages";
          "https://evil.test/provider/v1/messages"]))
      [Command.chat_url, Command.chat_headers;
       Command.messages_url, Command.messages_headers;
       Command.responses_url, Command.responses_headers];
    assert (Command.discover ~api_key:key ~http:(fun ~url ~headers ->
      assert (url = Command.models_url);
      assert (List.mem ("Authorization", "Bearer " ^ key) headers);
      Ok (200, {|{"object":"list","data":[{"id":"claude-impostor","name":"Chat model","context_length":120000,"supported_endpoints":["/chat/completions","/responses"]},{"id":"mystery-model","name":"Messages model","supported_endpoints":["/messages"]},{"id":"future-systemone","name":"Decision","supported_endpoints":["/systemone"]}]}|})) () =
      Ok [{Command.id = "claude-impostor"; name = "Chat model";
        context_length = Some 120000;
        supported_endpoints = ["/chat/completions"; "/responses"]};
        {Command.id = "mystery-model"; name = "Messages model";
          context_length = None; supported_endpoints = ["/messages"]}]);
    (match Command.discover ~api_key:key ~http:(fun ~url:_ ~headers:_ ->
      Ok (200, {|{"object":"list","data":[{"id":"repeated","name":"First","supported_endpoints":["/chat/completions"]},{"id":"repeated","name":"Second","supported_endpoints":["/messages"]}]}|})) () with
    | Error (Command.Invalid_response _) -> ()
    | _ -> fail "duplicate Command Code model IDs accepted");
    let listing_started_at = Unix.gettimeofday () in
    let listing = match Pave.Model_discovery.discover
        ~provider:"commandcode" ~route_name:"chat"
        ~credential:(Pave.Model_discovery.Api_key key)
        ~http:(fun ~url ~headers ->
          assert (url = Command.models_url);
          assert (List.mem ("Authorization", "Bearer " ^ key) headers);
          Ok (200, {|{"object":"list","data":[{"id":"claude-impostor","name":"Chat model","context_length":120000,"supported_endpoints":["/chat/completions","/responses"]},{"id":"mystery-model","name":"Messages model","supported_endpoints":["/messages"]}]}|}))
        () with
      | Ok listing -> listing
      | Error _ -> fail "provider listing capability metadata was lost" in
    let listing_finished_at = Unix.gettimeofday () in
    assert (List.map (fun (model : Pave.Model_catalog.model) ->
      model.identity.provider, model.identity.route, model.identity.account_id,
      model.identity.upstream_id, model.display_name,
      model.capabilities.context_window_tokens,
      model.capabilities.supported_endpoints) listing.models = [
        "commandcode", "chat", None, "claude-impostor", Some "Chat model",
          Some 120000, Some ["/chat/completions"; "/responses"];
        "commandcode", "chat", None, "mystery-model", Some "Messages model",
          None, Some ["/messages"] ]);
    let retrieved_at = Option.get listing.source.retrieved_at in
    assert (listing.source.id_source =
      Pave.Model_catalog.Pinned_account_listing &&
      listing.source.endpoint = Some Command.models_url &&
      retrieved_at >= listing_started_at &&
      retrieved_at <= listing_finished_at);
    let chat_model = List.hd listing.models in
    let messages_model = List.nth listing.models 1 in
    assert (Pave.Model_discovery.model_supports_endpoint
      ~provider:"commandcode" chat_model ~endpoint:Command.chat_url);
    assert (Pave.Model_discovery.model_supports_endpoint
      ~provider:"commandcode" chat_model ~endpoint:Command.responses_url);
    assert (not (Pave.Model_discovery.model_supports_endpoint
      ~provider:"commandcode" chat_model ~endpoint:Command.messages_url));
    assert (Pave.Model_discovery.model_supports_endpoint
      ~provider:"commandcode" messages_model ~endpoint:Command.messages_url);
    assert (not (Pave.Model_discovery.model_supports_endpoint
      ~provider:"commandcode" messages_model ~endpoint:Command.responses_url));
    assert (Command.discover ~api_key:"bad\nheader" ~http:(fun ~url:_ ~headers:_ ->
      fail "invalid key reached discovery executor") () = Error Command.Invalid_credential);
    expect_invalid (fun () -> Command.parse_messages_completion ~model
      (`Assoc ["type", `String "message"; "role", `String "assistant";
        "stop_reason", `String "tool_use";
        "content", `List [
          `Assoc ["type", `String "thinking"; "thinking", `String "opaque"];
          `Assoc ["type", `String "tool_use"; "id", `String call_id;
            "name", `String "add"; "input", args]]]));
    expect_invalid (fun () -> Command.parse_responses_completion ~model
      (`Assoc ["status", `String "completed"; "model", `String model;
        "output", `List [
          `Assoc ["type", `String "reasoning"; "summary", `List []];
          `Assoc ["type", `String "function_call";
            "call_id", `String call_id; "name", `String "add";
            "arguments", `String (Yojson.Basic.to_string args)]]]));
    let child = Unix.fork () in
    if child = 0 then (
      Unix.putenv "COMMAND_CODE_API_KEY" key;
      Unix.putenv "COMMANDCODE_API_KEY" "user_legacy";
      assert (Command.env_api_key () = Some key);
      Unix.putenv "COMMAND_CODE_API_KEY" "invalid\nheader";
      assert (Command.env_api_key () = Some "user_legacy");
      exit 0);
    (match Unix.waitpid [] child with
    | _, Unix.WEXITED 0 -> ()
    | _ -> fail "Studio key environment lookup failed");
    let directory = Filename.temp_file "pave-commandcode-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_COMMANDCODE_FIXTURE_STATE" "";
      Unix.putenv "PAVE_COMMANDCODE_FIXTURE_ENDPOINT" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_COMMANDCODE_FIXTURE_STATE" state;
      List.iter (fun (endpoint, api, request) ->
        Unix.putenv "PAVE_COMMANDCODE_FIXTURE_ENDPOINT" endpoint;
        let config : Pave.Provider.config = {endpoint; api_key = key; model; api} in
        let send messages = Pave.Provider.complete config messages [tool] in
        (* Guard is checked by the public dispatch before curl sees the key. *)
        expect_provider_rejected (fun () ->
          Pave.Provider.complete
            {config with endpoint = "https://evil.test/provider/v1/messages"}
            [Protocol.user "Do not leak my key"] [tool]);
        assert (not (Sys.file_exists state));
        let first = send [Protocol.user "Eight plus thirteen?"] in
        let call = match first.tool_calls with
          | [call] when call.id = call_id && call.name = "add" && call.arguments = args -> call
          | _ -> fail "native tool call was lost" in
        let result = match field "left" call.arguments, field "right" call.arguments with
          | `Int left, `Int right -> string_of_int (left + right)
          | _ -> fail "invalid tool arguments" in
        assert (first.provider_state <> None);
        expect_invalid (fun () -> request ~model:"different-model"
          [Protocol.user "Eight plus thirteen?"; first;
           Protocol.tool_result call.id result] [tool]);
        let final = send [Protocol.user "Eight plus thirteen?"; first;
          Protocol.tool_result call.id result] in
        assert (final.content = Some "21");
        let input = open_in state in
        assert (Fun.protect ~finally:(fun () -> close_in input)
          (fun () -> int_of_string (input_line input)) = 2);
        Sys.remove state)
        [Command.chat_url, Pave.Provider.Commandcode_chat,
          Command.chat_request;
         Command.messages_url, Pave.Provider.Commandcode_messages,
          (fun ~model messages tools ->
            Command.messages_request ~model ~max_tokens:4096 messages tools);
         Command.responses_url, Pave.Provider.Commandcode_responses,
          Command.responses_request]);
    print_endline "Command Code pinned HTTPS Chat/Messages/Responses native tool continuation: ok")
