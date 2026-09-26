module Zen = Pave.Opencode_zen_api
module Protocol = Pave.Protocol
let field = Protocol.member
let key = "private-zen-key"
(* A caller must explicitly choose the Responses route for this model ID. *)
let model = "manually-selected-responses-model"
let call_id = "call_zen_signed_1"
let arguments = `Assoc ["number", `Int 6]
let signed_reasoning = `Assoc ["type", `String "reasoning";
  "id", `String "rs_zen_1"; "summary", `List [];
  "encrypted_content", `String "opaque-signed-reasoning-v1";
  "extension", `Assoc ["preserve", `Bool true]]
let signed_message = `Assoc ["type", `String "message";
  "role", `String "assistant"; "status", `String "completed";
  "content", `List [`Assoc ["type", `String "output_text";
    "text", `String "Let me calculate that."]]]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let user = `Assoc ["role", `String "user";
  "content", `List [`Assoc ["type", `String "input_text";
    "text", `String "What is six times seven?"]]]
let fail reason = failwith ("Zen fixture: " ^ reason)
let invalid = function Zen.Invalid_response _ -> true | _ -> false
let expect_error check = function
  | Error error when check error -> ()
  | _ -> fail "invalid listing was accepted"

(* The fake curl exercises the real pinned HTTPS executor and both POST turns;
   the tool output is calculated from the decoded function arguments. *)
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
  let state = Sys.getenv "PAVE_ZEN_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 ->
        has "url" Zen.models_url;
        has "request" "GET";
        has "max-filesize" (string_of_int Zen.max_response_bytes);
        assert (List.mem "Accept: application/json" (values "header"));
        assert (values "header" = ["Accept: application/json"]);
        {|{"object":"list","data":[{"id":"manually-selected-responses-model","object":"model","created":1700000000,"owned_by":"opencode"}]}|}
    | 1 | 2 ->
        has "url" Zen.responses_url;
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
        assert (field "store" body = `Bool false);
        assert (field "include" body = `List [`String "reasoning.encrypted_content"]);
        assert (field "tools" body = `List [`Assoc [
          "type", `String "function"; "name", `String "multiply_seven";
          "parameters", `Assoc ["type", `String "object";
            "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
            "required", `List [`String "number"]];
          "strict", `Bool false;
          "description", `String "Multiply a number by seven"]]);
        (match body with
         | `Assoc fields -> assert (not (List.mem_assoc "reasoning" fields))
         | _ -> fail "Responses request is not an object");
        if step = 1 then (
          assert (field "input" body = `List [user]);
          {|{"id":"resp_zen_1","status":"completed","output":[{"type":"reasoning","id":"rs_zen_1","summary":[],"encrypted_content":"opaque-signed-reasoning-v1","extension":{"preserve":true}},{"type":"message","role":"assistant","status":"completed","content":[{"type":"output_text","text":"Let me calculate that."}]},{"type":"function_call","id":"fc_zen_1","status":"completed","call_id":"call_zen_signed_1","name":"multiply_seven","arguments":"{\"number\": 6}"}]}|})
        else (
          (match field "input" body with
           | `List [first; thinking; message; call; result] ->
               assert (first = user);
               assert (thinking = signed_reasoning);
               assert (message = signed_message);
               assert (call = `Assoc ["type", `String "function_call";
                 "id", `String "fc_zen_1"; "status", `String "completed";
                 "call_id", `String call_id;
                 "name", `String "multiply_seven";
                 "arguments", `String {|{"number": 6}|}]);
               assert (result = `Assoc ["type", `String "function_call_output";
                 "call_id", `String call_id; "output", `String {|{"product":42}|}])
           | _ -> fail "signed reasoning or computed tool result not replayed");
          {|{"id":"resp_zen_2","status":"completed","output":[{"type":"message","role":"assistant","status":"completed","content":[{"type":"output_text","text":"Six times seven is 42."}]}]}|})
    | _ -> fail "unexpected request" in
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
      assert (url = Zen.models_url);
      assert (headers = ["Accept", "application/json"]);
      Ok (200, {|{"object":"list","data":[{"id":"manually-selected-responses-model","object":"model"},{"id":"other-model-with-unclassified-route","object":"model"},{"id":"third-valid-responses-model","object":"model"}]}|}) in
    assert (Zen.discover ~http ~api_key:key () =
      Ok [model; "other-model-with-unclassified-route"; "third-valid-responses-model"]);
    assert (!calls = 1);
    assert (Zen.discover ~http ~api_key:"" () =
      Ok [model; "other-model-with-unclassified-route"; "third-valid-responses-model"]);
    assert (Zen.discover ~http ~api_key:"bad\r\nAuthorization: Bearer stolen" () =
      Ok [model; "other-model-with-unclassified-route"; "third-valid-responses-model"]);
    assert (!calls = 3);
    expect_error invalid (Zen.parse_models
      {|{"object":"list","data":[{"id":"duplicate","object":"model"},{"id":"duplicate","object":"model"}]}|});
    List.iter (fun body -> expect_error invalid (Zen.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200, body)) ~api_key:key ())) [
      {|{"object":"list","data":[{"id":"bad\nmodel","object":"model"}]}|};
      {|{"object":"list","data":[{"id":null,"object":"model"}]}|};
      {|{"data":[]}|}; "not json"];
    expect_error invalid (Zen.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        String.make (Zen.max_response_bytes + 1) 'x')) ~api_key:key ());
    expect_error ((=) (Zen.Http_error 302)) (Zen.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect")) ~api_key:key ());
    assert (Zen.responses_headers ~endpoint:Zen.responses_url ~api_key:key =
      ["Authorization: Bearer " ^ key]);
    (match Zen.responses_headers
      ~endpoint:"https://evil.example/zen/v1/responses" ~api_key:key with
     | exception Invalid_argument _ -> ()
     | _ -> fail "credential escaped pinned host");
    (match Zen.responses_headers ~endpoint:Zen.responses_url ~api_key:"bad\nvalue" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "header injection accepted");
    (match Zen.parse_completion ~model (`Assoc [
      "status", `String "completed";
      "output", `List [
        `Assoc ["type", `String "reasoning"; "summary", `List []];
        `Assoc ["type", `String "function_call";
          "call_id", `String call_id; "name", `String "multiply_seven";
          "arguments", `String {|{"number":6}|}]]]) with
     | exception Protocol.Invalid_response _ -> ()
     | _ -> fail "unsigned reasoning accepted for stateless tool replay");
    let directory = Filename.temp_file "pave-zen-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_ZEN_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_ZEN_FIXTURE_STATE" state;
      let listing = match Pave.Model_discovery.discover
        ~provider:"opencode-zen" ~credential:(Pave.Model_discovery.Api_key key) () with
        | Ok listing -> listing
        | Error _ -> fail "production Zen model discovery failed" in
      let discovered = match Pave.Model_discovery.model_ids listing with
        | [id] -> id | _ -> fail "production Zen returned an unexpected roster" in
      assert (listing.source.id_source = Pave.Model_catalog.Provider_listing);
      assert ((List.hd listing.models).identity.account_id = None);
      assert (discovered = model);
      let config : Pave.Provider.config = {
        endpoint = Zen.responses_url; api_key = key; model = discovered;
        api = Pave.Provider.Opencode_zen_responses } in
      (match Pave.Provider.complete
        { config with endpoint = "https://evil.example/zen/v1/responses" }
        [Protocol.user "Do not leak credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> fail "credential sent to untrusted endpoint");
      let first = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"] [tool] in
      let call = match first.tool_calls with
        | [call] when call.id = call_id && call.name = "multiply_seven" &&
          call.arguments = arguments -> call
        | _ -> fail "Zen Responses tool call not decoded" in
      assert (first.content = Some "Let me calculate that.");
      assert (first.provider_state = Some (`Assoc [
        "provider", `String "opencode-zen"; "model", `String model;
        "output", `List [
        signed_reasoning;
        signed_message;
        `Assoc ["type", `String "function_call";
          "id", `String "fc_zen_1"; "status", `String "completed";
          "call_id", `String call_id; "name", `String "multiply_seven";
          "arguments", `String {|{"number": 6}|}]] ]));
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
    print_endline "Zen pinned listing and stateless signed Responses continuation: ok")
