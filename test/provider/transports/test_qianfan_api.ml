module Qianfan = Pave.Qianfan_api
module Protocol = Pave.Protocol
let field = Protocol.member

let key = "bce-v3/ALTAK-fixture/fixture"
let model = "fixture/available-chat"
let call_id = "call_qianfan_47"
let arguments = `Assoc ["number", `Int 6]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let user = `Assoc ["role", `String "user";
  "content", `String "What is six times seven?"]
let fail reason = failwith ("Qianfan fixture: " ^ reason)
let expect_models expected = function
  | Ok actual when actual = expected -> ()
  | _ -> fail "unexpected Chat model listing"
let expect_error check = function
  | Error failure when check failure -> ()
  | _ -> fail "unsafe Qianfan listing accepted"
let invalid = function Qianfan.Invalid_response _ -> true | _ -> false

(* Fake curl inspects the production HTTPS executor: one authenticated GET
   and two native Chat turns with a locally computed tool result. *)
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
  let state = Sys.getenv "PAVE_QIANFAN_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 ->
        has "url" Qianfan.models_url;
        has "request" "GET";
        has "max-filesize" (string_of_int Qianfan.max_response_bytes);
        assert (List.mem "Accept: application/json" (values "header"));
        {|{"object":"list","data":[{"id":"fixture/available-chat","object":"model","type":"chat"},{"id":"fixture/embedding","object":"model","type":"embeddings"},{"id":"fixture/unknown","object":"model"}]}|}
    | 1 | 2 ->
        has "url" Qianfan.chat_url;
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
        assert (field "tool_choice" body = `String "auto");
        if step = 1 then (
          assert (field "messages" body = `List [user]);
          {|{"object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":"","tool_calls":[{"id":"call_qianfan_47","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|})
        else (
          (match field "messages" body with
           | `List [first; assistant; result] ->
               assert (first = user);
               assert (field "role" assistant = `String "assistant");
               assert (field "content" assistant = `String "");
               assert (field "tool_calls" assistant = `List [Protocol.call_to_json
                 { id = call_id; name = "multiply_seven"; arguments }]);
               assert (result = `Assoc ["role", `String "tool";
                 "content", `String {|{"product":42}|};
                 "tool_call_id", `String call_id])
           | _ -> fail "native function result missing from continuation");
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
      assert (url = Qianfan.models_url);
      assert (headers = ["Authorization", "Bearer " ^ key;
        "Accept", "application/json"]);
      Ok (200, {|{"object":"list","data":[{"id":"fixture/available-chat","object":"model","type":"chat"},{"id":"fixture/embedding","object":"model","type":"embeddings"},{"id":"fixture/unknown","object":"model"},{"id":"fixture/available-chat","object":"model","type":"chat"}]}|}) in
    expect_models [model] (Qianfan.discover ~http ~api_key:key ());
    assert (!calls = 1);
    expect_models [] (Qianfan.parse_models
      {|{"data":[{"id":"unclassified","object":"model"},{"id":"image","object":"model","type":"text2image"}]}|});
    expect_error ((=) Qianfan.Invalid_credential)
      (Qianfan.discover ~http ~api_key:"" ());
    expect_error ((=) Qianfan.Invalid_credential)
      (Qianfan.discover ~http ~api_key:"bad\r\nAuthorization: Bearer stolen" ());
    assert (!calls = 1);
    List.iter (fun body -> expect_error invalid
      (Qianfan.discover ~http:(fun ~url:_ ~headers:_ -> Ok (200, body))
         ~api_key:key ())) [
      {|{"data":[{"id":"bad\nmodel","object":"model","type":"chat"}]}|};
      {|{"data":[{"id":null,"object":"model","type":"chat"}]}|};
      {|{"data":[{"id":"x","object":"wrong","type":"chat"}]}|};
      {|{"data":[{"id":"x","object":"model","type":null}]}|};
      {|{"data":[{}]}|}; {|{"models":[]}|}; "not json"];
    expect_error invalid (Qianfan.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        String.make (Qianfan.max_response_bytes + 1) 'x')) ~api_key:key ());
    expect_error invalid (Qianfan.parse_models
      ("{\"data\":[" ^ String.concat ","
        (List.init (Qianfan.max_models + 1)
          (fun _ -> {|{"id":"x","object":"model","type":"chat"}|})) ^ "]}"));
    expect_error ((=) (Qianfan.Http_error 302)) (Qianfan.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect")) ~api_key:key ());
    assert (Qianfan.chat_headers ~endpoint:Qianfan.chat_url ~api_key:key =
      ["Authorization: Bearer " ^ key]);
    List.iter (fun endpoint ->
      match Qianfan.chat_headers ~endpoint ~api_key:key with
      | exception Invalid_argument _ -> ()
      | _ -> fail "Qianfan credential escaped pinned V2 endpoint") [
        "https://evil.example/v2/chat/completions";
        "https://qianfan.baidubce.com/v2/models";
        "https://aip.baidubce.com/rpc/2.0/ai_custom/v1/wenxinworkshop/chat/completions"];
    (match Qianfan.chat_headers ~endpoint:Qianfan.chat_url ~api_key:"bad\nvalue" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "header injection accepted");
    let rejected_reply body =
      match Qianfan.parse_completion (Yojson.Basic.from_string body) with
      | exception Protocol.Invalid_response _ -> ()
      | _ -> fail "incomplete Qianfan tool call accepted" in
    List.iter rejected_reply [
      {|{"choices":[{"finish_reason":"stop","message":{"content":"","tool_calls":[{"id":"c","type":"function","function":{"name":"multiply_seven","arguments":"{}"}}]}}]}|};
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":"","tool_calls":[{"id":"c","type":"function","function":{"name":"multiply_seven","arguments":"{"}}]}}]}|};
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":"","tool_calls":[{"id":"c","type":"not_function","function":{"name":"multiply_seven","arguments":"{}"}}]}}]}|};
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":"","tool_calls":[{"id":"c","type":"function","function":{"name":"multiply_seven","arguments":"[6]"}}]}}]}|}];
    let reasoning = Qianfan.parse_completion
      (Yojson.Basic.from_string
        {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"ok","reasoning_content":"thinking"}}]}|}) in
    assert (reasoning.provider_state =
      Some (`Assoc ["reasoning_content", `String "thinking"]));
    (* The official request schema omits reasoning_content: do not invent it
       in assistant history, but retain it in the local provider state. *)
    assert (field "messages" (Qianfan.request ~model [reasoning] []) =
      `List [`Assoc ["role", `String "assistant"; "content", `String "ok"]]);
    let directory = Filename.temp_file "pave-qianfan-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_QIANFAN_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_QIANFAN_FIXTURE_STATE" state;
      let discovered = match Result.map Pave.Model_discovery.model_ids
        (Pave.Model_discovery.discover
          ~provider:"qianfan" ~credential:(Pave.Model_discovery.Api_key key) ()) with
        | Ok ids when ids = [model] -> model
        | _ -> fail "production model discovery lost Chat-only classification" in
      let config : Pave.Provider.config = {
        endpoint = Qianfan.chat_url; api_key = key; model = discovered;
        api = Pave.Provider.Qianfan_chat } in
      (match Pave.Provider.complete
        { config with endpoint = "https://evil.example/v2/chat/completions" }
        [Protocol.user "Do not leak credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> fail "credential sent to hostile host");
      let before = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in before)
        (fun () -> input_line before) = "1");
      let first = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"] [tool] in
      let call = match first.tool_calls with
        | [call] when call.id = call_id && call.name = "multiply_seven" &&
          call.arguments = arguments -> call
        | _ -> fail "native Qianfan tool call was not decoded" in
      let result = match field "number" call.arguments with
        | `Int number -> Yojson.Basic.to_string
            (`Assoc ["product", `Int (number * 7)])
        | _ -> fail "unexpected function argument" in
      let final = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"; first;
          Protocol.tool_result call.id result] [tool] in
      assert (final.content = Some "Six times seven is 42.");
      let input = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in input)
        (fun () -> int_of_string (input_line input)) = 3));
    print_endline "Qianfan authenticated Chat listing and native tool continuation: ok")
