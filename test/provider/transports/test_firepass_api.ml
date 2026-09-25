module Firepass = Pave.Firepass_api
module Protocol = Pave.Protocol
let field = Protocol.member

let key = "fpk_private-test-key"
let model = "accounts/fireworks/routers/fixture-selected-router"
let call_id = "call_firepass_42"
let arguments = `Assoc ["number", `Int 6]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let user = `Assoc ["role", `String "user";
  "content", `String "What is six times seven?"]
let fail reason = failwith ("Fire Pass fixture: " ^ reason)

(* The fake curl is a real child process in place of curl. It asserts the
   production HTTP request, host, method, headers and both native tool turns. *)
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
      | _ -> fail "invalid curl config"
    else None) config in
  let one name = match values name with
    | [value] -> value | _ -> fail ("missing curl " ^ name) in
  assert (one "proto" = "=https");
  assert (one "url" = Firepass.chat_url);
  assert (one "request" = "POST");
  assert (not (List.mem "location" config));
  assert (not (List.mem "location-trusted" config));
  assert (List.sort String.compare (values "header") =
    List.sort String.compare ["Authorization: Bearer " ^ key;
      "Content-Type: application/json"]);
  let body_path = one "data-binary" in
  assert (String.starts_with ~prefix:"@" body_path);
  let input = open_in_bin
    (String.sub body_path 1 (String.length body_path - 1)) in
  let body = Fun.protect ~finally:(fun () -> close_in input)
    (fun () -> Yojson.Basic.from_string
      (really_input_string input (in_channel_length input))) in
  assert (field "model" body = `String model);
  assert (field "stream" body = `Bool false);
  assert (field "tools" body = `List [tool]);
  let state = Sys.getenv "PAVE_FIREPASS_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 ->
        assert (field "messages" body = `List [user]);
        {|{"id":"firepass-1","object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"Call the calculator first.","tool_calls":[{"id":"call_firepass_42","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|}
    | 1 ->
        (match field "messages" body with
        | `List [first; assistant; result] ->
            assert (first = user);
            assert (field "role" assistant = `String "assistant");
            assert (field "content" assistant = `Null);
            assert (field "reasoning_content" assistant =
              `String "Call the calculator first.");
            assert (field "tool_calls" assistant = `List [Protocol.call_to_json
              { id = call_id; name = "multiply_seven"; arguments }]);
            assert (result = `Assoc ["role", `String "tool";
              "content", `String {|{"product":42}|};
              "tool_call_id", `String call_id])
        | _ -> fail "assistant tool call and computed tool result must precede completion");
        {|{"id":"firepass-2","object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Six times seven is 42."}}]}|}
    | _ -> fail "unexpected GET or additional HTTP request" in
  let output = open_out state in
  output_string output (string_of_int (step + 1)); close_out output;
  let output = open_out_bin (one "output") in
  output_string output response; close_out output;
  print_string "200"; flush stdout

let () =
  if Array.length Sys.argv >= 3 && Sys.argv.(1) = "--disable" && Sys.argv.(2) = "--config" then
    (try fake_curl () with exn ->
      prerr_endline (Printexc.to_string exn); exit 2)
  else (
    assert (Firepass.valid_key key);
    List.iter (fun invalid -> assert (not (Firepass.valid_key invalid)))
      [""; "fpk_"; "fw_standard-key"; "fpk_bad\r\nAuthorization: Bearer stolen"];
    let previous_pass = Sys.getenv_opt "FIREPASS_API_KEY" in
    let previous_standard = Sys.getenv_opt "FIREWORKS_API_KEY" in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "FIREPASS_API_KEY" (Option.value ~default:"" previous_pass);
      Unix.putenv "FIREWORKS_API_KEY" (Option.value ~default:"" previous_standard))
      (fun () ->
        Unix.putenv "FIREWORKS_API_KEY" "fw_standard-key";
        Unix.putenv "FIREPASS_API_KEY" "";
        assert (Firepass.env_api_key () = None);
        Unix.putenv "FIREPASS_API_KEY" key;
        assert (Firepass.env_api_key () = Some key);
        Unix.putenv "FIREPASS_API_KEY" "fw_standard-key";
        assert (Firepass.env_api_key () = None));
    assert (Firepass.chat_headers ~endpoint:Firepass.chat_url ~api_key:key =
      ["Authorization: Bearer " ^ key]);
    List.iter (fun (endpoint, api_key) ->
      match Firepass.chat_headers ~endpoint ~api_key with
      | exception Invalid_argument _ -> ()
      | _ -> fail "untrusted host or non-pass credential accepted") [
      "https://evil.example/inference/v1/chat/completions", key;
      "https://api.fireworks.ai.attacker.example/inference/v1/chat/completions", key;
      "http://api.fireworks.ai/inference/v1/chat/completions", key;
      Firepass.chat_url, "fw_standard-key";
      Firepass.chat_url, "fpk_bad\nheader: injected"];
    assert (Firepass.valid_model model);
    List.iter (fun invalid ->
      match Firepass.request ~model:invalid [Protocol.user "test"] [] with
      | exception Invalid_argument _ -> ()
      | _ -> fail "non-router or malformed model ID accepted") [
      "fixture-selected-router";
      "accounts/fireworks/models/fixture-selected-router";
      "accounts/fireworks/routers/";
      "accounts/fireworks/routers/fixture/other";
      "accounts/fireworks/routers/fixture\nheader"];
    let config : Pave.Provider.config = {
      endpoint = Firepass.chat_url; api_key = key; model;
      api = Pave.Provider.Firepass_chat } in
    let directory = Filename.temp_file "pave-firepass-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_FIREPASS_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_FIREPASS_FIXTURE_STATE" state;
      List.iter (fun unsafe ->
        match Pave.Provider.complete unsafe [Protocol.user "Keep the key private"] [] with
        | exception Pave.Provider.Provider_error _ -> ()
        | _ -> fail "unsafe configuration reached transport") [
        { config with endpoint = "https://evil.example/inference/v1/chat/completions" };
        { config with api_key = "fw_standard-key" };
        { config with model = "accounts/fireworks/models/fixture-selected-router" } ];
      assert (not (Sys.file_exists state));
      let first = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"] [tool] in
      let call = match first.tool_calls with
        | [call] when call.id = call_id && call.name = "multiply_seven" &&
          call.arguments = arguments -> call
        | _ -> fail "native Fire Pass tool call not decoded" in
      assert (first.provider_state = Some (`Assoc
        ["reasoning_content", `String "Call the calculator first."]));
      let result = match field "number" call.arguments with
        | `Int number -> Yojson.Basic.to_string
            (`Assoc ["product", `Int (number * 7)])
        | _ -> fail "unexpected tool argument" in
      let final = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"; first;
          Protocol.tool_result call.id result] [tool] in
      assert (final.content = Some "Six times seven is 42.");
      assert (final.provider_state = None);
      let input = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in input)
        (fun () -> int_of_string (input_line input)) = 2));
    print_endline "Fire Pass pinned HTTPS two-turn native tool continuation: ok")
