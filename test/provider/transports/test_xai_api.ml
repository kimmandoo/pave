module Xai = Pave.Xai_api
module Protocol = Pave.Protocol
let field = Protocol.member

let key = "private-xai-fixture"
let model = "account-model-2099"
let parameters = `Assoc ["type", `String "object";
  "properties", `Assoc ["item", `Assoc ["type", `String "string"]]]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "lookup";
    "description", `String "Look up a value";
    "parameters", parameters]]
let args = `Assoc ["item", `String "current"]
let native_call = `Assoc ["id", `String "call_xai_1";
  "type", `String "function";
  "function", `Assoc ["name", `String "lookup";
    "arguments", `String {|{"item":"current"}|}]]
let user = `Assoc ["role", `String "user";
  "content", `String "Look this up"]

let expect_models expected = function
  | Ok actual when actual = expected -> ()
  | Ok _ -> failwith "incorrect xAI account model IDs"
  | Error _ -> failwith "xAI account discovery failed"
let expect_invalid = function
  | Error (Xai.Invalid_response _) -> ()
  | _ -> failwith "invalid xAI account listing accepted"

(* curl is replaced by this executable to inspect the actual production
   Provider.complete/Model_discovery HTTP path, not a mocked serializer. *)
let fake_curl () =
  let configs = ref [] in
  (try while true do configs := input_line stdin :: !configs done
   with End_of_file -> ());
  let config = List.rev !configs in
  let values name = List.filter_map (fun line ->
    let prefix = name ^ " = " in
    if String.starts_with ~prefix line then
      match Yojson.Basic.from_string
        (String.sub line (String.length prefix)
          (String.length line - String.length prefix)) with
      | `String value -> Some value | _ -> failwith "invalid curl option"
    else None) config in
  let one name = match values name with
    | [value] -> value | _ -> failwith ("missing curl " ^ name) in
  let has name value = assert (one name = value) in
  has "proto" "=https";
  assert (not (List.mem "location" config));
  assert (not (List.mem "location-trusted" config));
  assert (List.mem ("Authorization: Bearer " ^ key) (values "header"));
  let state_path = Sys.getenv "PAVE_XAI_FIXTURE_STATE" in
  let step = if not (Sys.file_exists state_path) then 0 else
    let input = open_in state_path in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input)) in
  let response = match step with
    | 0 ->
        has "url" Xai.models_url;
        has "request" "GET";
        has "max-filesize" (string_of_int Xai.max_response_bytes);
        assert (List.mem "Accept: application/json" (values "header"));
        {|{"models":[{"id":"account-model-2099","aliases":["latest-alias"]},{"id":"second-account-model"}]}|}
    | 1 | 2 ->
        has "url" Xai.chat_url;
        has "request" "POST";
        assert (List.mem "Content-Type: application/json" (values "header"));
        let body_path = one "data-binary" in
        assert (String.starts_with ~prefix:"@" body_path);
        let input = open_in_bin
          (String.sub body_path 1 (String.length body_path - 1)) in
        let request = Fun.protect ~finally:(fun () -> close_in input)
          (fun () -> Yojson.Basic.from_string
            (really_input_string input (in_channel_length input))) in
        assert (field "model" request = `String model);
        assert (field "tools" request = `List [tool]);
        if step = 1 then (
          assert (field "messages" request = `List [user]);
          {|{"id":"completion-1","object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"I should look up the current value.","tool_calls":[{"id":"call_xai_1","type":"function","function":{"name":"lookup","arguments":"{\"item\":\"current\"}"}}]}}]}|})
        else (
          assert (field "messages" request = `List [user;
            `Assoc ["role", `String "assistant";
              "tool_calls", `List [native_call];
              "reasoning_content", `String "I should look up the current value."];
            `Assoc ["role", `String "tool";
              "content", `String "found";
              "tool_call_id", `String "call_xai_1"]]);
          {|{"id":"completion-2","object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"The answer is found."}}]}|})
    | _ -> failwith "unexpected xAI network request" in
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
    let calls = ref 0 in
    let http ~url ~headers =
      incr calls;
      assert (url = Xai.models_url);
      assert (headers = ["Authorization", "Bearer " ^ key;
                         "Accept", "application/json"]);
      Ok (200, {|{"models":[{"id":"new-model-2099","aliases":["alias"]},{"id":"other-model"},{"id":"new-model-2099"}]}|}) in
    expect_models ["new-model-2099"; "other-model"]
      (Xai.discover ~http ~api_key:key ());
    assert (!calls = 1);
    assert (Xai.discover ~http ~api_key:"" () = Error Xai.Invalid_credential);
    assert (Xai.discover ~http ~api_key:"bad\r\nAuthorization: Bearer stolen" () =
      Error Xai.Invalid_credential);
    assert (!calls = 1);
    List.iter (fun payload ->
      expect_invalid (Xai.discover
        ~http:(fun ~url:_ ~headers:_ -> Ok (200, payload)) ~api_key:key ())) [
      {|{"models":[{"id":"bad\nname"}]}|};
      {|{"models":[{"id":null,"name":"fallback"}]}|};
      {|{"models":[{"id":"ok"},{}]}|};
      {|{"data":[{"id":"wrong-endpoint"}]}|}; "not json" ];
    expect_invalid (Xai.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        String.make (Xai.max_response_bytes + 1) 'x')) ~api_key:key ());
    assert (Xai.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect")) ~api_key:key () =
      Error (Xai.Http_error 302));
    assert (Xai.chat_headers ~endpoint:Xai.chat_url ~api_key:key =
      ["Authorization: Bearer " ^ key]);
    (match Xai.chat_headers ~endpoint:"https://evil.example/v1/chat/completions" ~api_key:key with
     | exception Invalid_argument _ -> ()
     | _ -> failwith "xAI credential escaped pinned host");
    (match Xai.chat_headers ~endpoint:Xai.chat_url ~api_key:"bad\nvalue" with
     | exception Invalid_argument _ -> ()
     | _ -> failwith "xAI header injection accepted");
    let child = Unix.fork () in
    if child = 0 then (
      Unix.putenv "XAI_API_KEY" "fixture-env-key";
      assert (Xai.env_api_key () = Some "fixture-env-key");
      Unix.putenv "XAI_API_KEY" "bad\nkey";
      assert (Xai.env_api_key () = None);
      exit 0);
    (match Unix.waitpid [] child with
     | _, Unix.WEXITED 0 -> ()
     | _ -> failwith "xAI API key environment resolution failed");
    let directory = Filename.temp_file "pave-xai-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_XAI_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_XAI_FIXTURE_STATE" state;
      let discovered = match Result.map Pave.Model_discovery.model_ids
        (Pave.Model_discovery.discover
          ~provider:"xai" ~credential:(Pave.Model_discovery.Api_key key) ()) with
        | Ok [id; "second-account-model"] -> id
        | _ -> failwith "production xAI account discovery failed" in
      assert (discovered = model);
      let config : Pave.Provider.config = {
        endpoint = Xai.chat_url; api_key = key; model = discovered;
        api = Pave.Provider.Xai_chat } in
      (match Pave.Provider.complete
        { config with endpoint = "https://evil.example/v1/chat/completions" }
        [Protocol.user "Do not leak credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> failwith "xAI credential sent to an untrusted endpoint");
      let before = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in before)
        (fun () -> input_line before) = "1");
      let streamed = ref [] in
      let first = Pave.Provider.complete ~on_text:(fun text ->
        streamed := text :: !streamed) config [Protocol.user "Look this up"] [tool] in
      assert (!streamed = []);
      let call = match first.tool_calls with
        | [call] when call.name = "lookup" && call.arguments = args -> call
        | _ -> failwith "xAI did not return its native tool call" in
      assert (first.provider_state = Some (`Assoc [
        "reasoning_content", `String "I should look up the current value."]));
      let final = Pave.Provider.complete ~on_text:(fun text ->
        streamed := text :: !streamed) config [Protocol.user "Look this up";
        first; Protocol.tool_result call.id "found"] [tool] in
      assert (final.content = Some "The answer is found.");
      assert (List.rev !streamed = ["The answer is found."]);
      let input = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in input)
        (fun () -> int_of_string (input_line input)) = 3));
    print_endline "xAI account listing and native reasoning/tool round-trip: ok")
