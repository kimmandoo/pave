module Hyper = Pave.Charm_hyper_api
module Protocol = Pave.Protocol
let field = Protocol.member

let key = "sk-hyper-private-fixture"
let model = "future-hyper-model"
let call_id = "call_hyper_tool_01"
let arguments = `Assoc ["number", `Int 6]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let reasoning = "Use the calculator tool first."
let user = `Assoc ["role", `String "user";
  "content", `String "What is six times seven?"]
let fail reason = failwith ("Charm Hyper fixture: " ^ reason)
let expect_models expected = function
  | Ok actual when actual = expected -> ()
  | _ -> fail "unexpected model listing"
let expect_error check = function
  | Error failure when check failure -> ()
  | _ -> fail "unsafe Hyper listing accepted"
let invalid = function Hyper.Invalid_response _ -> true | _ -> false

(* Replacement curl exercises the production HTTPS executor for public model
   discovery and both turns of an assistant-tool-result-assistant exchange. *)
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
  let state = Sys.getenv "PAVE_CHARM_HYPER_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 ->
        has "url" Hyper.models_url;
        has "request" "GET";
        has "max-filesize" (string_of_int Hyper.max_response_bytes);
        assert (List.mem "Accept: application/json" (values "header"));
        assert (not (List.exists (String.starts_with ~prefix:"Authorization:")
          (values "header")));
        {|{"object":"list","data":[{"id":"future-hyper-model","object":"model","capabilities":{"vision":true},"reasoning":{"effort_levels":[{"value":"high"}]}}]}|}
    | 1 | 2 ->
        has "url" Hyper.chat_url;
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
          {|{"id":"hyper-1","object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"Use the calculator tool first.","tool_calls":[{"id":"call_hyper_tool_01","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|})
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
          {|{"id":"hyper-2","object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Six times seven is 42."}}]}|})
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
      assert (url = Hyper.models_url);
      assert (headers = ["Accept", "application/json"]);
      Ok (200, {|{"object":"list","data":[{"id":"future-hyper-model","object":"model"},{"id":"another/model","object":"model"},{"id":"third-valid-hyper-model","object":"model"}]}|}) in
    expect_models [model; "another/model"; "third-valid-hyper-model"]
      (Hyper.discover ~http ~api_key:key ());
    expect_models [model; "another/model"; "third-valid-hyper-model"]
      (Hyper.discover ~http ~api_key:"" ());
    assert (!calls = 2);
    expect_error invalid (Hyper.parse_models
      {|{"object":"list","data":[{"id":"duplicate","object":"model"},{"id":"duplicate","object":"model"}]}|});
    List.iter (fun body -> expect_error invalid
      (Hyper.discover ~http:(fun ~url:_ ~headers:_ -> Ok (200, body))
         ~api_key:key ())) [
      {|{"object":"list","data":[{"id":"bad\nmodel","object":"model"}]}|};
      {|{"object":"list","data":[{"id":null,"object":"model"}]}|};
      {|{"object":"list","data":[{"id":"not-a-model"}]}|};
      {|{"data":[]}|}; "not json"];
    expect_error invalid (Hyper.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        String.make (Hyper.max_response_bytes + 1) 'x')) ~api_key:key ());
    expect_error invalid (Hyper.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        Yojson.Basic.to_string (`Assoc ["object", `String "list";
          "data", `List (List.init (Hyper.max_models + 1)
            (fun _ -> `Assoc ["id", `String model; "object", `String "model"]))])))
      ~api_key:key ());
    expect_error ((=) (Hyper.Http_error 302)) (Hyper.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect")) ~api_key:key ());
    assert (Hyper.chat_headers ~endpoint:Hyper.chat_url ~api_key:key =
      ["Authorization: Bearer " ^ key]);
    (match Hyper.chat_headers ~endpoint:"https://evil.example/v1/chat/completions" ~api_key:key with
     | exception Invalid_argument _ -> ()
     | _ -> fail "credential escaped pinned host");
    (match Hyper.chat_headers ~endpoint:Hyper.chat_url ~api_key:"sk-hyper-bad\nvalue" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "header injection accepted");
    (match Hyper.chat_headers ~endpoint:Hyper.chat_url ~api_key:"sk-other-gateway" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "non-Hyper key accepted");
    let old_primary = Sys.getenv_opt "CHARM_HYPER_API_KEY" in
    let old_official = Sys.getenv_opt "HYPER_API_KEY" in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "CHARM_HYPER_API_KEY" (Option.value ~default:"" old_primary);
      Unix.putenv "HYPER_API_KEY" (Option.value ~default:"" old_official))
      (fun () ->
        Unix.putenv "CHARM_HYPER_API_KEY" "";
        Unix.putenv "HYPER_API_KEY" key;
        assert (Hyper.env_api_key () = Some key);
        Unix.putenv "CHARM_HYPER_API_KEY" "sk-hyper-preferred";
        assert (Hyper.env_api_key () = Some "sk-hyper-preferred");
        Unix.putenv "CHARM_HYPER_API_KEY" "sk-other-gateway";
        assert (Hyper.env_api_key () = Some key));
    let directory = Filename.temp_file "pave-charm-hyper-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_CHARM_HYPER_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_CHARM_HYPER_FIXTURE_STATE" state;
      let listing = match Pave.Model_discovery.discover
        ~provider:"charm-hyper" ~credential:(Pave.Model_discovery.Api_key key) () with
        | Ok listing -> listing
        | Error _ -> fail "production model discovery failed" in
      let discovered = match Pave.Model_discovery.model_ids listing with
      | [id] -> id
      | _ -> fail "production discovery returned an unexpected roster" in
      assert (listing.source.id_source = Pave.Model_catalog.Provider_listing);
      assert ((List.hd listing.models).identity.account_id = None);
      assert (discovered = model);
      let config : Pave.Provider.config = {
        endpoint = Hyper.chat_url; api_key = key; model = discovered;
        api = Pave.Provider.Charm_hyper_chat } in
      (match Pave.Provider.complete
        { config with endpoint = "https://evil.example/v1/chat/completions" }
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
        | _ -> fail "Hyper tool call not decoded" in
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
    print_endline "Charm Hyper public listing and reasoning-preserving tool continuation: ok")
