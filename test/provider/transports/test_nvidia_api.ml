module Nvidia = Pave.Nvidia_api
module Protocol = Pave.Protocol
let field = Protocol.member
let key = "private-nvidia-fixture"
let model = "newvendor/future-tool-model"
let call_id = "call_nvidia_72"
let arguments = `Assoc ["item", `String "current"]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "lookup";
    "description", `String "Look up an item";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["item", `Assoc ["type", `String "string"]]]]]

let fail reason = failwith ("NVIDIA fixture: " ^ reason)
let expect_models expected = function
  | Ok actual when actual = expected -> ()
  | _ -> fail "unexpected model listing"
let expect_error check = function
  | Error failure when check failure -> ()
  | _ -> fail "unsafe NVIDIA listing accepted"
let invalid = function Nvidia.Invalid_response _ -> true | _ -> false

(* Curl impersonation inspects the production GET/POST wire, rather than
   mocking Provider.complete or its JSON serializer. *)
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
  let state = Sys.getenv "PAVE_NVIDIA_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let output = open_out state in
  output_string output (string_of_int (step + 1)); close_out output;
  let response = match step with
    | 0 ->
        has "url" Nvidia.models_url;
        has "request" "GET";
        has "max-filesize" (string_of_int Nvidia.max_response_bytes);
        {|{"object":"list","data":[{"id":"newvendor/future-tool-model","object":"model"}]}|}
    | 1 | 2 ->
        has "url" Nvidia.chat_url;
        has "request" "POST";
        assert (List.mem "Content-Type: application/json" (values "header"));
        let body_path = one "data-binary" in
        assert (String.starts_with ~prefix:"@" body_path);
        let input = open_in_bin (String.sub body_path 1 (String.length body_path - 1)) in
        let body = Fun.protect ~finally:(fun () -> close_in input)
          (fun () -> Yojson.Basic.from_string
            (really_input_string input (in_channel_length input))) in
        assert (field "model" body = `String model);
        assert (field "stream" body = `Bool false);
        assert (field "tools" body = `List [tool]);
        (match body with
         | `Assoc fields ->
             assert (not (List.mem_assoc "tool_choice" fields));
             assert (not (List.mem_assoc "max_tokens" fields))
         | _ -> fail "Chat request is not an object");
        let user = `Assoc ["role", `String "user";
          "content", `String "Look up this item"] in
        if step = 1 then (
          assert (field "messages" body = `List [user]);
          {|{"id":"chatcmpl-nvidia-1","object":"chat.completion","choices":[{"index":0,"message":{"role":"assistant","content":null,"tool_calls":[{"id":"call_nvidia_72","type":"function","function":{"name":"lookup","arguments":"{\"item\":\"current\"}"}}]},"finish_reason":"tool_calls"}]}|})
        else (
          let messages = match field "messages" body with
            | `List messages -> messages | _ -> fail "missing Chat history" in
          (match messages with
          | [first; assistant; result] ->
              assert (first = user);
              assert (field "role" assistant = `String "assistant");
              assert (field "content" assistant = `Null);
              (match assistant with
               | `Assoc fields -> assert (List.mem_assoc "content" fields)
               | _ -> fail "assistant history malformed");
              assert (field "tool_calls" assistant = `List [Protocol.call_to_json
                { id = call_id; name = "lookup"; arguments }]);
              assert (result = `Assoc ["role", `String "tool";
                "content", `String {|{"value":"September"}|};
                "tool_call_id", `String call_id])
          | _ -> fail "tool result missing from continuation");
          {|{"id":"chatcmpl-nvidia-2","object":"chat.completion","choices":[{"index":0,"message":{"role":"assistant","content":"The item is September."},"finish_reason":"stop"}]}|})
    | _ -> fail "unexpected network request" in
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
      assert (url = Nvidia.models_url);
      assert (headers = ["Authorization", "Bearer " ^ key;
        "Accept", "application/json"]);
      Ok (200, {|{"object":"list","data":[{"id":"newvendor/future-tool-model"},{"id":"another/model"},{"id":"newvendor/future-tool-model"}]}|}) in
    expect_models [model; "another/model"] (Nvidia.discover ~http ~api_key:key ());
    assert (!calls = 1);
    expect_error ((=) Nvidia.Invalid_credential)
      (Nvidia.discover ~http ~api_key:"" ());
    expect_error ((=) Nvidia.Invalid_credential)
      (Nvidia.discover ~http ~api_key:"bad\r\nAuthorization: Bearer stolen" ());
    assert (!calls = 1);
    List.iter (fun body -> expect_error invalid
      (Nvidia.discover ~http:(fun ~url:_ ~headers:_ -> Ok (200, body))
         ~api_key:key ())) [
      {|{"data":[{"id":"bad\nmodel"}]}|};
      {|{"data":[{"id":null}]}|};
      {|{"data":[{}]}|};
      {|{"models":[]}|}; "not json"];
    expect_error invalid (Nvidia.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        String.make (Nvidia.max_response_bytes + 1) 'x')) ~api_key:key ());
    expect_error ((=) (Nvidia.Http_error 302)) (Nvidia.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect")) ~api_key:key ());
    assert (Nvidia.chat_headers ~endpoint:Nvidia.chat_url ~api_key:key =
      ["Authorization: Bearer " ^ key]);
    (match Nvidia.chat_headers ~endpoint:"https://evil.example/v1/chat/completions" ~api_key:key with
     | exception Invalid_argument _ -> ()
     | _ -> fail "credential escaped pinned host");
    (match Nvidia.chat_headers ~endpoint:Nvidia.chat_url ~api_key:"bad\nvalue" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "header injection accepted");
    let directory = Filename.temp_file "pave-nvidia-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_NVIDIA_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_NVIDIA_FIXTURE_STATE" state;
      let discovered = match Result.map Pave.Model_discovery.model_ids
        (Pave.Model_discovery.discover
          ~provider:"nvidia" ~credential:(Pave.Model_discovery.Api_key key) ()) with
      | Ok [id] -> id
      | _ -> fail "production model discovery failed" in
      assert (discovered = model);
      let config : Pave.Provider.config = {
        endpoint = Nvidia.chat_url; api_key = key; model = discovered;
        api = Pave.Provider.Nvidia_chat } in
      (match Pave.Provider.complete
        { config with endpoint = "https://evil.example/v1/chat/completions" }
        [Protocol.user "Do not leak credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> fail "credential sent to untrusted endpoint");
      let before = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in before)
        (fun () -> input_line before) = "1");
      let first = Pave.Provider.complete config
        [Protocol.user "Look up this item"] [tool] in
      let call = match first.tool_calls with
        | [call] when call.id = call_id && call.name = "lookup" &&
          call.arguments = arguments -> call
        | _ -> fail "NVIDIA tool call not decoded" in
      let result = match field "item" call.arguments with
        | `String "current" -> Yojson.Basic.to_string
            (`Assoc ["value", `String "September"])
        | _ -> fail "unexpected lookup argument" in
      let final = Pave.Provider.complete config
        [Protocol.user "Look up this item"; first;
          Protocol.tool_result call.id result] [tool] in
      assert (final.content = Some "The item is September.");
      let input = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in input)
        (fun () -> int_of_string (input_line input)) = 3));
    print_endline "NVIDIA authenticated discovery and tool continuation: ok")
