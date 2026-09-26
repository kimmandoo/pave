module Synthetic = Pave.Synthetic_api
module Protocol = Pave.Protocol
let field = Protocol.member

let key = "synthetic-fixture-token"
let model = "hf:example-org/chat-model"
let embedding = "hf:example-org/embedding-model"
let call_id = "call_synthetic_47"
let arguments = `Assoc ["number", `Int 6]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let user = `Assoc ["role", `String "user";
  "content", `String "What is six times seven?"]
let fail message = failwith ("Synthetic fixture: " ^ message)
let expect_models expected = function
  | Ok models when models = expected -> ()
  | _ -> fail "incorrect model listing"
let expect_error pred = function
  | Error err when pred err -> ()
  | _ -> fail "unsafe Synthetic listing accepted"
let invalid = function Synthetic.Invalid_response _ -> true | _ -> false

(* Fake curl validates the production HTTPS executor and two real Provider
   completions, rather than merely serializing transport functions. *)
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
  assert (List.mem ("Authorization: Bearer " ^ key) (values "header"));
  let state = Sys.getenv "PAVE_SYNTHETIC_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 ->
        has "url" Synthetic.models_url;
        has "request" "GET";
        has "max-filesize" (string_of_int Synthetic.max_response_bytes);
        assert (List.mem "Accept: application/json" (values "header"));
        {|{"object":"list","data":[{"id":"hf:example-org/chat-model","object":"model"},{"id":"hf:example-org/embedding-model","object":"model"}]}|}
    | 1 | 2 ->
        has "url" Synthetic.chat_url;
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
             assert (not (List.mem_assoc "reasoning_effort" fields));
             assert (not (List.mem_assoc "max_completion_tokens" fields))
         | _ -> fail "Chat request is not an object");
        if step = 1 then (
          assert (field "messages" body = `List [user]);
          {|{"object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"Compute using the supplied function.","tool_calls":[{"id":"call_synthetic_47","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|})
        else (
          (match field "messages" body with
           | `List [first; assistant; result] ->
               assert (first = user);
               assert (field "role" assistant = `String "assistant");
               assert (field "content" assistant = `Null);
               assert (field "reasoning_content" assistant =
                 `String "Compute using the supplied function.");
               assert (field "tool_calls" assistant = `List [Protocol.call_to_json
                 { id = call_id; name = "multiply_seven"; arguments }]);
               assert (result = `Assoc ["role", `String "tool";
                 "content", `String {|{"product":42}|};
                 "tool_call_id", `String call_id])
           | _ -> fail "computed tool result missing from continuation");
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
      assert (url = Synthetic.models_url);
      assert (headers = ["Authorization", "Bearer " ^ key;
        "Accept", "application/json"]);
      Ok (200, {|{"object":"list","data":[{"id":"hf:example-org/chat-model","object":"model"},{"id":"hf:example-org/embedding-model","object":"model"},{"id":"hf:example-org/chat-model","object":"model"}]}|}) in
    (* /models does not document capability fields, so neither the chat-shaped
       ID nor the embedding ID can be certified as tool-capable from listing. *)
    expect_models [model; embedding] (Synthetic.discover ~http ~api_key:key ());
    assert (!calls = 1);
    expect_error ((=) Synthetic.Invalid_credential)
      (Synthetic.discover ~http ~api_key:"" ());
    expect_error ((=) Synthetic.Invalid_credential)
      (Synthetic.discover ~http ~api_key:"bad\r\nAuthorization: Bearer stolen" ());
    assert (!calls = 1);
    List.iter (fun body -> expect_error invalid
      (Synthetic.discover ~http:(fun ~url:_ ~headers:_ -> Ok (200, body))
         ~api_key:key ())) [
      {|{"data":[{"id":"bad\nmodel"}]}|};
      {|{"data":[{"id":null}]}|};
      {|{"data":[{}]}|}; {|{"models":[]}|}; "not json"];
    expect_error invalid (Synthetic.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        String.make (Synthetic.max_response_bytes + 1) 'x')) ~api_key:key ());
    expect_error ((=) (Synthetic.Http_error 302)) (Synthetic.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect")) ~api_key:key ());
    assert (Synthetic.chat_headers ~endpoint:Synthetic.chat_url ~api_key:key =
      ["Authorization: Bearer " ^ key]);
    List.iter (fun endpoint ->
      match Synthetic.chat_headers ~endpoint ~api_key:key with
      | exception Invalid_argument _ -> ()
      | _ -> fail "credential escaped pinned OpenAI endpoint") [
        "https://api.synthetic.new/v1/chat/completions";
        "https://api.synthetic.new/anthropic/v1/messages";
        "https://evil.example/openai/v1/chat/completions"];
    (match Synthetic.chat_headers ~endpoint:Synthetic.chat_url ~api_key:"bad\nvalue" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "header injection accepted");
    let rejected_reply body =
      match Synthetic.parse_completion (Yojson.Basic.from_string body) with
      | exception Protocol.Invalid_response _ -> ()
      | _ -> fail "incomplete Synthetic function call accepted" in
    List.iter rejected_reply [
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":null,"tool_calls":[{"id":"call_1","type":"function","function":{"name":"calculate","arguments":"{"}}]}}]}|};
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":null,"tool_calls":[{"id":"call_1","type":"web_search","function":{"name":"calculate","arguments":"{}"}}]}}]}|};
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":null,"tool_calls":[{"id":"call_1","type":"function","function":{"name":"calculate","arguments":"[1]"}}]}}]}|};
      {|{"choices":[{"finish_reason":"stop","message":{"content":null,"tool_calls":[{"id":"call_1","type":"function","function":{"name":"calculate","arguments":"{}"}}]}}]}|};
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":null,"tool_calls":[{"id":"call_1","type":"function","function":{"arguments":"{}"}}]}}]}|}];
    let directory = Filename.temp_file "pave-synthetic-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_SYNTHETIC_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_SYNTHETIC_FIXTURE_STATE" state;
      let discovered = match Result.map Pave.Model_discovery.model_ids
        (Pave.Model_discovery.discover
          ~provider:"synthetic" ~credential:(Pave.Model_discovery.Api_key key) ()) with
        | Ok ids when ids = [model; embedding] -> model
        | _ -> fail "production discovery lost unclassified IDs" in
      let config : Pave.Provider.config = {
        endpoint = Synthetic.chat_url; api_key = key; model = discovered;
        api = Pave.Provider.Synthetic_chat } in
      (match Pave.Provider.complete
        { config with endpoint = "https://evil.example/openai/v1/chat/completions" }
        [Protocol.user "Do not leak credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> fail "credential sent to a foreign host");
      let before = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in before)
        (fun () -> input_line before) = "1");
      let first = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"] [tool] in
      let call = match first.tool_calls with
        | [call] when call.id = call_id && call.name = "multiply_seven" &&
          call.arguments = arguments -> call
        | _ -> fail "Synthetic function call not decoded" in
      assert (first.provider_state = Some (`Assoc [
        "reasoning_content", `String "Compute using the supplied function."]));
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
    print_endline "Synthetic authenticated listing and native tool continuation: ok")
