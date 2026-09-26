module Xiaomi = Pave.Xiaomi_api
module Protocol = Pave.Protocol
let field = Protocol.member

let key = "sk-xiaomi-fixture"
let model = "example/new-chat-model"
let speech_model = "example/speech-only-model"
let call_id = "call_xiaomi_47"
let reasoning = "I will multiply the requested number using the tool."
let arguments = `Assoc ["number", `Int 6]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let user = `Assoc ["role", `String "user";
  "content", `String "What is six times seven?"]
let fail reason = failwith ("Xiaomi fixture: " ^ reason)
let expect_error predicate = function
  | Error error when predicate error -> ()
  | _ -> fail "unsafe model listing accepted"
let invalid = function Xiaomi.Invalid_response _ -> true | _ -> false

(* Fake curl observes the production HTTPS executor's pinned GET and POST
   requests, including the actual computed tool result on the second turn. *)
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
  assert (List.mem ("api-key: " ^ key) (values "header"));
  let state = Sys.getenv "PAVE_XIAOMI_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 ->
        has "url" Xiaomi.models_url;
        has "request" "GET";
        has "max-filesize" (string_of_int Xiaomi.max_response_bytes);
        assert (List.mem "Accept: application/json" (values "header"));
        {|{"object":"list","data":[{"id":"example/new-chat-model","object":"model","owned_by":"xiaomi"},{"id":"example/speech-only-model","object":"model","owned_by":"xiaomi"}]}|}
    | 1 | 2 ->
        has "url" Xiaomi.chat_url;
        has "request" "POST";
        assert (List.mem "Content-Type: application/json" (values "header"));
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
        (match body with
         | `Assoc fields ->
             assert (not (List.mem_assoc "thinking" fields));
             assert (not (List.mem_assoc "max_completion_tokens" fields))
         | _ -> fail "Chat request is not an object");
        if step = 1 then (
          assert (field "messages" body = `List [user]);
          {|{"object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"I will multiply the requested number using the tool.","tool_calls":[{"id":"call_xiaomi_47","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|})
        else (
          (match field "messages" body with
           | `List [first; assistant; result] ->
               assert (first = user);
               assert (field "role" assistant = `String "assistant");
               (match assistant with
                | `Assoc fields ->
                    assert (List.assoc_opt "content" fields = Some `Null)
                | _ -> fail "assistant continuation is not an object");
               assert (field "reasoning_content" assistant = `String reasoning);
               assert (field "tool_calls" assistant = `List [Protocol.call_to_json
                 { id = call_id; name = "multiply_seven"; arguments }]);
               assert (result = `Assoc ["role", `String "tool";
                 "content", `String {|{"product":42}|};
                 "tool_call_id", `String call_id])
           | _ -> fail "tool result missing from continuation");
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
      assert (url = Xiaomi.models_url);
      assert (headers = ["api-key", key; "Accept", "application/json"]);
      Ok (200, {|{"object":"list","data":[{"id":"example/new-chat-model","object":"model","owned_by":"xiaomi"},{"id":"example/speech-only-model","object":"model","owned_by":"xiaomi"},{"id":"example/third-model","object":"model","owned_by":"xiaomi"}]}|}) in
    (match Xiaomi.discover ~http ~api_key:key () with
     | Ok ids when ids = [model; speech_model; "example/third-model"] -> ()
     | _ -> fail "authenticated model listing failed");
    expect_error invalid (Xiaomi.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        {|{"object":"list","data":[{"id":"same-model","object":"model"},{"id":"same-model","object":"unsupported"}]}|}))
      ~api_key:key ());
    assert (!calls = 1);
    List.iter (fun api_key ->
      expect_error ((=) Xiaomi.Invalid_credential)
        (Xiaomi.discover ~http ~api_key ()))
      [""; "bad\r\nInjected: true"; "tp-subscription-key"; "ttp-team-key"];
    assert (!calls = 1);
    List.iter (fun body -> expect_error invalid
      (Xiaomi.discover ~http:(fun ~url:_ ~headers:_ -> Ok (200, body))
         ~api_key:key ())) [
      {|{"object":"list","data":[{"id":"bad\nmodel","object":"model"}]}|};
      {|{"object":"list","data":[{"id":null,"object":"model"}]}|};
      {|{"object":"list","data":[{"id":"x","object":"not-model"}]}|};
      {|{"object":"list","data":[{}]}|};
      {|{"data":[]}|}; "not json"];
    expect_error invalid (Xiaomi.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        String.make (Xiaomi.max_response_bytes + 1) 'x')) ~api_key:key ());
    expect_error ((=) (Xiaomi.Http_error 302)) (Xiaomi.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect")) ~api_key:key ());
    assert (Xiaomi.chat_headers ~endpoint:Xiaomi.chat_url ~api_key:key =
      ["api-key: " ^ key]);
    List.iter (fun endpoint ->
      match Xiaomi.chat_headers ~endpoint ~api_key:key with
      | exception Invalid_argument _ -> ()
      | _ -> fail "credential escaped pinned MiMo host") [
        "https://evil.example/v1/chat/completions";
        "https://token-plan-cn.xiaomimimo.com/v1/chat/completions"];
    (match Xiaomi.chat_headers ~endpoint:Xiaomi.chat_url ~api_key:"bad\nkey" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "header injection accepted");
    let invalid_completion body =
      match Xiaomi.parse_completion (Yojson.Basic.from_string body) with
      | exception Protocol.Invalid_response _ -> ()
      | _ -> fail "incomplete MiMo tool state accepted" in
    List.iter invalid_completion [
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":null,"tool_calls":[{"id":"call_1","type":"function","function":{"name":"calculate","arguments":"{}"}}]}}]}|};
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":null,"reasoning_content":3,"tool_calls":[{"id":"call_1","type":"function","function":{"name":"calculate","arguments":"{}"}}]}}]}|};
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":null,"reasoning_content":"thinking","tool_calls":[{"id":"call_1","type":"function","function":{"name":"calculate","arguments":"{"}}]}}]}|}];
    let previous = Protocol.user "hello" in
    let unsourced = { previous with role = "assistant";
      content = None; tool_calls = [{id = call_id; name = "multiply_seven"; arguments}] } in
    (match Xiaomi.request ~model [unsourced] [tool] with
     | exception Protocol.Invalid_response _ -> ()
     | _ -> fail "missing reasoning fabricated for tool continuation");
    let directory = Filename.temp_file "pave-xiaomi-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_XIAOMI_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_XIAOMI_FIXTURE_STATE" state;
      let discovered = match Result.map Pave.Model_discovery.model_ids
        (Pave.Model_discovery.discover
          ~provider:"xiaomi" ~credential:(Pave.Model_discovery.Api_key key) ()) with
        | Ok ids when ids = [model; speech_model] -> model
        | _ -> fail "production model discovery lost unclassified IDs" in
      let config : Pave.Provider.config = {
        endpoint = Xiaomi.chat_url; api_key = key; model = discovered;
        api = Pave.Provider.Xiaomi_chat } in
      (match Pave.Provider.complete
        { config with endpoint = "https://evil.example/v1/chat/completions" }
        [Protocol.user "Do not leak credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> fail "credential sent to hostile endpoint");
      let before = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in before)
        (fun () -> input_line before) = "1");
      let first = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"] [tool] in
      let call = match first.tool_calls with
        | [call] when call.id = call_id && call.name = "multiply_seven" &&
          call.arguments = arguments -> call
        | _ -> fail "MiMo function call not decoded" in
      assert (first.provider_state = Some (`Assoc [
        "reasoning_content", `String reasoning]));
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
    print_endline "Xiaomi authenticated listing and native reasoning/tool continuation: ok")
