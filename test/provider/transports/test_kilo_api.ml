module Kilo = Pave.Kilo_api
module Protocol = Pave.Protocol
let field = Protocol.member

let key = "private-kilo-gateway-fixture"
let model = "example/future-chat-model"
let call_id = "call_kilo_42"
let arguments = `Assoc ["number", `Int 6]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let user = `Assoc ["role", `String "user";
  "content", `String "What is six times seven?"]
let reasoning = `List [`Assoc ["type", `String "reasoning.text";
  "text", `String "Use the calculator tool first.";
  "signature", `String "opaque-signature";
  "provider_extension", `Assoc ["preserve", `Bool true]]]
let fail reason = failwith ("Kilo Gateway fixture: " ^ reason)
let invalid = function Kilo.Invalid_response _ -> true | _ -> false
let expect_error check = function
  | Error failure when check failure -> ()
  | _ -> fail "unsafe model listing accepted"

(* Replacement curl verifies the production GET and both POST turns, without
   making any network request or using a production model ID. *)
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
  let state = Sys.getenv "PAVE_KILO_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 ->
        has "url" Kilo.models_url;
        has "request" "GET";
        has "max-filesize" (string_of_int Kilo.max_response_bytes);
        assert (values "header" = ["Accept: application/json"]);
        {|{"data":[{"id":"example/future-chat-model","object":"model","context_length":100000,"pricing":{"prompt":"0.01","completion":"0.02"}}]}|}
    | 1 | 2 ->
        has "url" Kilo.chat_url;
        has "request" "POST";
        assert (List.mem ("Authorization: Bearer " ^ key) (values "header"));
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
             assert (not (List.mem_assoc "max_tokens" fields))
         | _ -> fail "Chat request is not an object");
        if step = 1 then (
          assert (field "messages" body = `List [user]);
          {|{"id":"kilo-1","object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_details":[{"type":"reasoning.text","text":"Use the calculator tool first.","signature":"opaque-signature","provider_extension":{"preserve":true}}],"tool_calls":[{"id":"call_kilo_42","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|})
        else (
          (match field "messages" body with
           | `List [first; assistant; result] ->
               assert (first = user);
               assert (field "role" assistant = `String "assistant");
               (match assistant with
                | `Assoc fields ->
                    assert (List.assoc_opt "content" fields = Some `Null)
                | _ -> fail "assistant continuation is not an object");
               assert (field "reasoning_details" assistant = reasoning);
               assert (field "tool_calls" assistant = `List [Protocol.call_to_json
                 { id = call_id; name = "multiply_seven"; arguments }]);
               assert (result = `Assoc ["role", `String "tool";
                 "content", `String {|{"product":42}|};
                 "tool_call_id", `String call_id])
           | _ -> fail "tool result missing from continuation");
          {|{"id":"kilo-2","object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Six times seven is 42."}}]}|})
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
      assert (url = Kilo.models_url);
      assert (headers = ["Accept", "application/json"]);
      Ok (200, {|{"data":[{"id":"example/future-chat-model","object":"model"},{"id":"another/model","object":"model"},{"id":"third-valid-model","object":"model"}]}|}) in
    assert (Kilo.discover ~http ~api_key:key () =
      Ok [model; "another/model"; "third-valid-model"]);
    assert (Kilo.discover ~http ~api_key:"" () =
      Ok [model; "another/model"; "third-valid-model"]);
    assert (!calls = 2);
    expect_error invalid (Kilo.parse_models
      {|{"data":[{"id":"duplicate","object":"model"},{"id":"duplicate","object":"model"}]}|});
    List.iter (fun body -> expect_error invalid
      (Kilo.discover ~http:(fun ~url:_ ~headers:_ -> Ok (200, body))
         ~api_key:key ())) [
      {|{"data":[{"id":"bad\nmodel","object":"model"}]}|};
      {|{"data":[{"id":null,"object":"model"}]}|};
      {|{"data":[{"id":"not-a-model"}]}|};
      {|{"data":{}}|}; "not json"];
    expect_error invalid (Kilo.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        String.make (Kilo.max_response_bytes + 1) 'x')) ~api_key:key ());
    expect_error invalid (Kilo.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        Yojson.Basic.to_string (`Assoc ["data", `List
          (List.init (Kilo.max_models + 1) (fun _ ->
            `Assoc ["id", `String model; "object", `String "model"]))])))
      ~api_key:key ());
    expect_error ((=) (Kilo.Http_error 302)) (Kilo.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect")) ~api_key:key ());
    assert (Kilo.chat_headers ~endpoint:Kilo.chat_url ~api_key:key =
      ["Authorization: Bearer " ^ key]);
    (match Kilo.chat_headers ~endpoint:Kilo.chat_url ~api_key:"" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "anonymous listing incorrectly allowed unauthenticated Chat");
    (match Kilo.chat_headers ~endpoint:"https://evil.example/api/gateway/chat/completions" ~api_key:key with
     | exception Invalid_argument _ -> ()
     | _ -> fail "credential escaped pinned host");
    (match Kilo.chat_headers ~endpoint:Kilo.chat_url ~api_key:"bad\nvalue" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "header injection accepted");
    let directory = Filename.temp_file "pave-kilo-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_KILO_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_KILO_FIXTURE_STATE" state;
      let discovered = match Result.map Pave.Model_discovery.model_ids
        (Pave.Model_discovery.discover ~provider:"kilo" ()) with
      | Ok [id] -> id
      | _ -> fail "keyless production model discovery failed" in
      assert (discovered = model);
      let config : Pave.Provider.config = {
        endpoint = Kilo.chat_url; api_key = key; model = discovered;
        api = Pave.Provider.Kilo_chat } in
      (match Pave.Provider.complete
        { config with endpoint = "https://evil.example/api/gateway/chat/completions" }
        [Protocol.user "Do not leak credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> fail "credential sent to untrusted endpoint");
      let before = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in before)
        (fun () -> input_line before) = "1");
      let first = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"] [tool] in
      let call = match first.tool_calls with
        | [call] when call.id = call_id && call.name = "multiply_seven" &&
          call.arguments = arguments -> call
        | _ -> fail "Kilo tool call not decoded" in
      assert (first.provider_state = Some (`Assoc ["reasoning_details", reasoning]));
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
    print_endline "Kilo anonymous listing and authenticated tool continuation: ok")
