module Go = Pave.Opencode_go_api
module Protocol = Pave.Protocol
let field = Protocol.member

let key = "opencode-go-fixture-key"
let chat_model = "glm-5.3"
let non_chat_model = "minimax-m3"
let call_id = "call_go_42"
let arguments = `Assoc ["number", `Int 6]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let user = `Assoc ["role", `String "user";
  "content", `String "What is six times seven?"]
let fail message = failwith ("OpenCode Go fixture: " ^ message)
let expect_error pred = function
  | Error err when pred err -> ()
  | _ -> fail "invalid model listing accepted"
let invalid = function Go.Invalid_response _ -> true | _ -> false

(* Fake curl checks the actual HTTPS executor, Go-specific route, real
   Provider tool calls and a computed tool-result continuation. *)
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
  let has name value = assert (one name = value) in
  has "proto" "=https";
  assert (not (List.mem "location" config));
  assert (not (List.mem "location-trusted" config));
  assert (List.mem "User-Agent: pave" (values "header"));
  let state = Sys.getenv "PAVE_OPENCODE_GO_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 ->
        has "url" Go.models_url;
        has "request" "GET";
        has "max-filesize" (string_of_int Go.max_response_bytes);
        assert (not (List.exists (String.starts_with ~prefix:"Authorization:") (values "header")));
        assert (List.mem "Accept: application/json" (values "header"));
        {|{"object":"list","data":[{"id":"glm-5.3","object":"model","created":1790329858,"owned_by":"opencode"},{"id":"minimax-m3","object":"model","created":1790329858,"owned_by":"opencode"}]}|}
    | 1 | 2 ->
        assert (List.mem ("Authorization: Bearer " ^ key) (values "header"));
        has "url" Go.chat_url;
        has "request" "POST";
        assert (List.mem "Content-Type: application/json" (values "header"));
        let body_path = one "data-binary" in
        assert (String.starts_with ~prefix:"@" body_path);
        let input = open_in_bin
          (String.sub body_path 1 (String.length body_path - 1)) in
        let body = Fun.protect ~finally:(fun () -> close_in input)
          (fun () -> Yojson.Basic.from_string
            (really_input_string input (in_channel_length input))) in
        assert (field "model" body = `String chat_model);
        assert (field "stream" body = `Bool false);
        assert (field "tools" body = `List [tool]);
        if step = 1 then (
          assert (field "messages" body = `List [user]);
          {|{"object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"Use the provided tool to calculate.","tool_calls":[{"id":"call_go_42","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|})
        else (
          (match field "messages" body with
           | `List [first; assistant; result] ->
               assert (first = user);
               assert (field "role" assistant = `String "assistant");
               assert (field "content" assistant = `Null);
               assert (field "reasoning_content" assistant =
                 `String "Use the provided tool to calculate.");
               assert (field "tool_calls" assistant = `List [Protocol.call_to_json
                 { id = call_id; name = "multiply_seven"; arguments }]);
               assert (result = `Assoc ["role", `String "tool";
                 "content", `String {|{"product":42}|};
                 "tool_call_id", `String call_id])
           | _ -> fail "computed tool result missing from Go continuation");
          {|{"object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Six times seven is 42."}}]}|})
    | _ -> fail "unexpected network request" in
  let count = open_out state in
  output_string count (string_of_int (step + 1)); close_out count;
  let output = open_out_bin (one "output") in
  output_string output response; close_out output;
  print_string "200"; flush stdout

let () =
  if Array.length Sys.argv >= 3 && Sys.argv.(1) = "--disable" && Sys.argv.(2) = "--config" then
    (try fake_curl () with exn ->
       prerr_endline (Printexc.to_string exn); exit 2)
  else (
    let calls = ref 0 in
    let http ~url ~headers =
      incr calls;
      assert (url = Go.models_url);
      assert (headers = ["Accept", "application/json"; "User-Agent", "pave"]);
      Ok (200, {|{"object":"list","data":[{"id":"glm-5.3","object":"model","created":1790329858,"owned_by":"opencode"},{"id":"minimax-m3","object":"model","created":1790329858,"owned_by":"opencode"},{"id":"glm-5.3","object":"model"}]}|}) in
    (* Even the documented non-Chat ID is returned: the listing cannot certify
       a route or tool capability for any of its rows. *)
    assert (Go.discover ~http ~api_key:key () =
      Ok [chat_model; non_chat_model]);
    assert (Go.discover ~http ~api_key:"" () =
      Ok [chat_model; non_chat_model]);
    assert (Go.discover ~http ~api_key:"bad\r\nHeader: injected" () =
      Ok [chat_model; non_chat_model]);
    assert (!calls = 3);
    List.iter (fun body -> expect_error invalid
      (Go.discover ~http:(fun ~url:_ ~headers:_ -> Ok (200, body))
         ~api_key:key ())) [
      {|{"object":"list","data":[{"id":"bad\nmodel","object":"model"}]}|};
      {|{"object":"list","data":[{"id":null,"object":"model"}]}|};
      {|{"object":"list","data":[{"id":"glm-5.3","object":"route"}]}|};
      {|{"object":"list","data":[{}]}|};
      {|{"data":[]}|}; "not json"];
    expect_error invalid (Go.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        String.make (Go.max_response_bytes + 1) 'x')) ~api_key:key ());
    expect_error ((=) (Go.Http_error 302)) (Go.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect")) ~api_key:key ());
    assert (Go.chat_headers ~endpoint:Go.chat_url ~api_key:key =
      ["Authorization: Bearer " ^ key; "User-Agent: pave"]);
    List.iter (fun endpoint ->
      match Go.chat_headers ~endpoint ~api_key:key with
      | exception Invalid_argument _ -> ()
      | _ -> fail "Go credential escaped pinned Chat endpoint") [
        "https://opencode.ai/zen/v1/chat/completions";
        "https://opencode.ai/zen/go/v1/responses";
        "https://opencode.ai/zen/go/v1/messages";
        "https://evil.example/zen/go/v1/chat/completions"];
    (match Go.chat_headers ~endpoint:Go.chat_url ~api_key:"bad\nvalue" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "header injection accepted");
    let rejected_reply body =
      match Go.parse_completion (Yojson.Basic.from_string body) with
      | exception Protocol.Invalid_response _ -> ()
      | _ -> fail "invalid function call accepted" in
    List.iter rejected_reply [
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":null,"tool_calls":[{"id":"call_1","type":"function","function":{"name":"multiply_seven","arguments":"{"}}]}}]}|};
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":null,"tool_calls":[{"id":"call_1","type":"function","function":{"name":"multiply_seven","arguments":"[1]"}}]}}]}|};
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":null,"tool_calls":[{"id":"call_1","type":"web_search","function":{"name":"multiply_seven","arguments":"{}"}}]}}]}|}];
    let directory = Filename.temp_file "pave-opencode-go-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_OPENCODE_GO_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_OPENCODE_GO_FIXTURE_STATE" state;
      let discovered = match Pave.Model_discovery.discover
        ~provider:"opencode-go" ~credential:(Pave.Model_discovery.Api_key key) () with
        | Ok ids when ids = [chat_model; non_chat_model] -> chat_model
        | _ -> fail "Go discovery lost unclassified IDs" in
      let config : Pave.Provider.config = {
        endpoint = Go.chat_url; api_key = key; model = discovered;
        api = Pave.Provider.Opencode_go_chat } in
      (match Pave.Provider.complete
        { config with endpoint = "https://opencode.ai/zen/v1/chat/completions" }
        [Protocol.user "Do not leak credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> fail "Go credential sent to Zen instead of Go");
      let first = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"] [tool] in
      let call = match first.tool_calls with
        | [call] when call.id = call_id && call.name = "multiply_seven" &&
          call.arguments = arguments -> call
        | _ -> fail "Go function call not decoded" in
      assert (first.provider_state = Some (`Assoc [
        "reasoning_content", `String "Use the provided tool to calculate."]));
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
        (fun () -> int_of_string (input_line input)) = 3));
    print_endline "OpenCode Go pinned listing and native tool-result continuation: ok")
