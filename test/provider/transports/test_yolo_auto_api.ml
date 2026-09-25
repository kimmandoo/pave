module Yolo = Pave.Yolo_auto_api
module Protocol = Pave.Protocol
let field = Protocol.member

let key = "yolo_fixture-secret"
let model = "example/new-chat-model"
let other_model = "example/unknown-model"
let call_id = "call_yolo_47"
let reasoning = "Compute the product using the function."
let arguments = `Assoc ["number", `Int 6]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let user = `Assoc ["role", `String "user";
  "content", `String "What is six times seven?"]
let fail reason = failwith ("Yolo-Auto fixture: " ^ reason)
let invalid = function Yolo.Invalid_response _ -> true | _ -> false
let expect_error predicate = function
  | Error error when predicate error -> ()
  | _ -> fail "unsafe listing accepted"

(* Substituted curl subprocess checks the actual HTTPS executor, its bounded
   authenticated GET, and both POST bodies with the locally computed result. *)
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
  let state = Sys.getenv "PAVE_YOLO_AUTO_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 ->
        has "url" Yolo.models_url;
        has "request" "GET";
        has "max-filesize" (string_of_int Yolo.max_response_bytes);
        assert (List.mem "Accept: application/json" (values "header"));
        {|{"object":"list","data":[{"id":"example/new-chat-model","object":"model","context_length":4096,"thinking":["low","medium"]},{"id":"example/unknown-model","object":"model","context_length":2048}]}|}
    | 1 | 2 ->
        has "url" Yolo.chat_url;
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
        if step = 1 then (
          assert (field "messages" body = `List [user]);
          {|{"object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"Compute the product using the function.","tool_calls":[{"id":"call_yolo_47","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|})
        else (
          (match field "messages" body with
           | `List [first; assistant; result] ->
               assert (first = user);
               assert (field "role" assistant = `String "assistant");
               (match assistant with
                | `Assoc fields -> assert (List.assoc_opt "content" fields = Some `Null)
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
      assert (url = Yolo.models_url);
      assert (List.mem ("Authorization", "Bearer " ^ key) headers);
      Ok (200, {|{"object":"list","data":[{"id":"example/new-chat-model"},{"id":"example/unknown-model"},{"id":"example/new-chat-model"}]}|}) in
    (match Yolo.discover ~http ~api_key:key () with
     | Ok ids when ids = [model; other_model] -> ()
     | _ -> fail "authenticated model listing failed");
    assert (!calls = 1);
    List.iter (fun api_key -> expect_error ((=) Yolo.Invalid_credential)
      (Yolo.discover ~http ~api_key ())) [""; "bad\r\nInjected: true"];
    assert (!calls = 1);
    List.iter (fun body -> expect_error invalid
      (Yolo.discover ~http:(fun ~url:_ ~headers:_ -> Ok (200, body))
         ~api_key:key ())) [
      {|{"object":"list","data":[{"id":"bad\nheader"}]}|};
      {|{"object":"list","data":[{"id":"ok","object":"not-model"}]}|};
      {|{"object":"list","data":[{"object":"model"}]}|};
      {|{"data":[]}|}; "not json"];
    expect_error invalid (Yolo.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        String.make (Yolo.max_response_bytes + 1) 'x')) ~api_key:key ());
    expect_error invalid (Yolo.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        Yojson.Basic.to_string (`Assoc ["object", `String "list";
          "data", `List (List.init (Yolo.max_models + 1)
            (fun _ -> `Assoc ["id", `String "x"]))]))) ~api_key:key ());
    expect_error ((=) (Yolo.Http_error 302)) (Yolo.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect")) ~api_key:key ());
    assert (Yolo.chat_headers ~endpoint:Yolo.chat_url ~api_key:key =
      ["Authorization: Bearer " ^ key]);
    List.iter (fun endpoint ->
      match Yolo.chat_headers ~endpoint ~api_key:key with
      | exception Invalid_argument _ -> ()
      | _ -> fail "credential escaped pinned Yolo-Auto host") [
        "https://evil.example/v1/chat/completions";
        "https://yolo-auto.com.evil.example/v1/chat/completions";
        "http://yolo-auto.com/v1/chat/completions";
        "https://yolo-auto.com/v1/chat/completions?next=https://evil.example"];
    (match Yolo.chat_headers ~endpoint:Yolo.chat_url ~api_key:"bad\nkey" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "header injection accepted");
    (match Yolo.parse_completion (Yojson.Basic.from_string
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":null,"reasoning_content":42,"tool_calls":[{"id":"call_1","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|}) with
     | exception Protocol.Invalid_response _ -> ()
     | _ -> fail "invalid reasoning field accepted");
    let unsourced = { (Protocol.user "question") with role = "assistant";
      content = None; tool_calls = [{ id = call_id;
        name = "multiply_seven"; arguments }];
      provider_state = Some (`Assoc ["untrusted", `String "do not forward"]) } in
    (match field "messages" (Yolo.request ~model [unsourced] []) with
     | `List [assistant] ->
         assert (field "reasoning_content" assistant = `Null);
         assert (field "untrusted" assistant = `Null)
     | _ -> fail "unexpected assistant request");
    let directory = Filename.temp_file "pave-yolo-auto-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_YOLO_AUTO_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_YOLO_AUTO_FIXTURE_STATE" state;
      let discovered = match Pave.Model_discovery.discover
        ~provider:"yolo-auto" ~credential:(Pave.Model_discovery.Api_key key) () with
        | Ok ids when ids = [model; other_model] -> model
        | _ -> fail "production model discovery lost unclassified IDs" in
      let config : Pave.Provider.config = {
        endpoint = Yolo.chat_url; api_key = key; model = discovered;
        api = Pave.Provider.Yolo_auto_chat } in
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
        | _ -> fail "function call not decoded" in
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
    print_endline "Yolo-Auto authenticated catalog and native function continuation: ok")
