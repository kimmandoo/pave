module Singularity = Pave.Singularity_dev_api
module Protocol = Pave.Protocol
let field = Protocol.member

let key = "private-singularity-dev-fixture"
let model = "future-vendor/chat-model"
let call_id = "call_sapi_9"
let arguments = `Assoc ["number", `Int 6]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let user = `Assoc ["role", `String "user";
  "content", `String "What is six times seven?"]
let fail reason = failwith ("SingularityAPI .dev fixture: " ^ reason)
let expect_models expected = function
  | Ok actual when actual = expected -> ()
  | _ -> fail "unexpected chat-capable model listing"
let expect_error check = function
  | Error failure when check failure -> ()
  | _ -> fail "unsafe SingularityAPI listing accepted"
let invalid = function Singularity.Invalid_response _ -> true | _ -> false

(* This executable doubles as a fake curl: inspect the authenticated GET and
   both HTTPS Chat calls, including computed tool output replay. *)
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
  let state = Sys.getenv "PAVE_SINGULARITY_DEV_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 ->
        has "url" Singularity.models_url;
        has "request" "GET";
        has "max-filesize" (string_of_int Singularity.max_response_bytes);
        assert (List.mem "Accept: application/json" (values "header"));
        {|{"object":"list","data":[{"id":"future-vendor/chat-model","object":"model","capabilities":[{"endpoint":"/v1/chat/completions","context_window_tokens":123456,"pricing":{"input_per_million_usd":"0.081000000000","output_per_million_usd":"0.162000000000"}}]},{"id":"image-only","object":"model","capabilities":[{"endpoint":"/v1/images/generations"}]}]}|}
    | 1 | 2 ->
        has "url" Singularity.chat_url;
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
             assert (not (List.mem_assoc "max_tokens" fields))
         | _ -> fail "Chat request is not an object");
        if step = 1 then (
          assert (field "messages" body = `List [user]);
          {|{"id":"sapi-1","object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"Calculate the product.","tool_calls":[{"id":"call_sapi_9","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|})
        else (
          (match field "messages" body with
           | `List [first; assistant; result] ->
               assert (first = user);
               assert (field "role" assistant = `String "assistant");
               assert (field "content" assistant = `Null);
               assert (field "reasoning_content" assistant = `String "Calculate the product.");
               assert (field "tool_calls" assistant = `List [Protocol.call_to_json
                 { id = call_id; name = "multiply_seven"; arguments }]);
               assert (result = `Assoc ["role", `String "tool";
                 "content", `String {|{"product":42}|};
                 "tool_call_id", `String call_id])
           | _ -> fail "tool result missing from continuation");
          {|{"id":"sapi-2","object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Six times seven is 42."}}]}|})
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
    let previous = Sys.getenv_opt "SINGULARITYAPI_DEV_API_KEY" in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "SINGULARITYAPI_DEV_API_KEY" (Option.value ~default:"" previous)) (fun () ->
      Unix.putenv "SINGULARITYAPI_DEV_API_KEY" key;
      assert (Singularity.env_api_key () = Some key);
      Unix.putenv "SINGULARITYAPI_DEV_API_KEY" "";
      assert (Singularity.env_api_key () = None);
      Unix.putenv "SINGULARITYAPI_DEV_API_KEY" "bad\nkey";
      assert (Singularity.env_api_key () = None));
    let calls = ref 0 in
    let http ~url ~headers =
      incr calls;
      assert (url = Singularity.models_url);
      assert (headers = ["Authorization", "Bearer " ^ key;
        "Accept", "application/json"]);
      Ok (200, {|{"object":"list","data":[{"id":"image-only","object":"model","capabilities":[{"endpoint":"/v1/images/generations"}]},{"id":"future-vendor/chat-model","object":"model","capabilities":[{"endpoint":"/v1/chat/completions"},{"endpoint":"/v1/responses"}]},{"id":"future-vendor/chat-model","object":"model","capabilities":[{"endpoint":"/v1/chat/completions"}]}]}|}) in
    expect_models [model] (Singularity.discover ~http ~api_key:key ());
    assert (!calls = 1);
    expect_error ((=) Singularity.Invalid_credential)
      (Singularity.discover ~http ~api_key:"" ());
    expect_error ((=) Singularity.Invalid_credential)
      (Singularity.discover ~http ~api_key:"bad\r\nAuthorization: Bearer stolen" ());
    assert (!calls = 1);
    List.iter (fun body -> expect_error invalid
      (Singularity.discover ~http:(fun ~url:_ ~headers:_ -> Ok (200, body))
         ~api_key:key ())) [
      {|{"object":"list","data":[{"id":"bad\nmodel","object":"model","capabilities":[{"endpoint":"/v1/chat/completions"}]}]}|};
      {|{"object":"list","data":[{"id":null,"object":"model","capabilities":[]}]}|};
      {|{"object":"list","data":[{"id":"missing-capabilities","object":"model"}]}|};
      {|{"object":"list","data":[{"id":"bad-endpoint","object":"model","capabilities":[{"endpoint":null}]}]}|};
      {|{"data":[]}|}; {|{"models":[]}|}; "not json"];
    expect_error invalid (Singularity.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        String.make (Singularity.max_response_bytes + 1) 'x')) ~api_key:key ());
    expect_error ((=) (Singularity.Http_error 302)) (Singularity.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect")) ~api_key:key ());
    assert (Singularity.chat_headers ~endpoint:Singularity.chat_url ~api_key:key =
      ["Authorization: Bearer " ^ key]);
    List.iter (fun endpoint ->
      match Singularity.chat_headers ~endpoint ~api_key:key with
      | exception Invalid_argument _ -> ()
      | _ -> fail "credential escaped pinned .dev host") [
      "https://evil.example/v1/chat/completions";
      "https://api.singularityapi.tech/v1/chat/completions";
      "https://api.singularityapi.dev.evil.example/v1/chat/completions"];
    (match Singularity.chat_headers ~endpoint:Singularity.chat_url ~api_key:"bad\nvalue" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "header injection accepted");
    let directory = Filename.temp_file "pave-singularity-dev-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_SINGULARITY_DEV_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_SINGULARITY_DEV_FIXTURE_STATE" state;
      let discovered = match Pave.Model_discovery.discover
        ~provider:"singularityapi-dev" ~credential:(Pave.Model_discovery.Api_key key) () with
        | Ok [id] -> id | _ -> fail "production Chat model discovery failed" in
      assert (discovered = model);
      let config : Pave.Provider.config = {
        endpoint = Singularity.chat_url; api_key = key; model = discovered;
        api = Pave.Provider.Singularity_dev_chat } in
      (match Pave.Provider.complete
        { config with endpoint = "https://evil.example/v1/chat/completions" }
        [Protocol.user "Do not leak credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> fail "credential sent to untrusted endpoint");
      (match Pave.Provider.complete
        { config with endpoint = "https://api.singularityapi.tech/v1/chat/completions" }
        [Protocol.user "Do not leak credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> fail "credential sent to .tech lane");
      let before = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in before)
        (fun () -> input_line before) = "1");
      let first = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"] [tool] in
      let call = match first.tool_calls with
        | [call] when call.id = call_id && call.name = "multiply_seven" &&
          call.arguments = arguments -> call
        | _ -> fail "native tool call not decoded" in
      assert (first.provider_state = Some (`Assoc [
        "reasoning_content", `String "Calculate the product."]));
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
    print_endline "SingularityAPI .dev Chat listing and tool continuation: ok")
