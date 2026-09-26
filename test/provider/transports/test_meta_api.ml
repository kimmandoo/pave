module Meta = Pave.Meta_api
module Protocol = Pave.Protocol
let field = Protocol.member
let key = "private-meta-model-key"
let model = "operator-selected-responses-model"
let other_model = "catalog-entry-with-unknown-route"
let call_id = "call_meta_1"
let arguments = `Assoc ["number", `Int 6]
let reasoning = `Assoc ["type", `String "reasoning";
  "id", `String "rs_meta_1"; "status", `String "completed";
  "summary", `List []; "encrypted_content", `String "opaque-signed-reasoning";
  "meta_extension", `Assoc ["keep", `Bool true]]
let commentary = `Assoc ["type", `String "message";
  "role", `String "assistant"; "phase", `String "commentary";
  "status", `String "completed";
  "content", `List [`Assoc ["type", `String "output_text";
    "text", `String "I will calculate that."]]]
let raw_call = `Assoc ["type", `String "function_call";
  "id", `String "fc_meta_1"; "status", `String "completed";
  "call_id", `String call_id; "name", `String "multiply_seven";
  "arguments", `String {|{"number": 6}|}]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let user = `Assoc ["role", `String "user";
  "content", `List [`Assoc ["type", `String "input_text";
    "text", `String "What is six times seven?"]]]
let fail reason = failwith ("Meta fixture: " ^ reason)
let expect_error predicate = function
  | Error error when predicate error -> ()
  | _ -> fail "expected error"

(* Fake curl inspects the real HTTPS GET/POST executor config and the wire
   body. Its second answer is computed from the actual tool result, not an echo
   or a preselected static answer. *)
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
  let has name expected = assert (one name = expected) in
  has "proto" "=https";
  assert (not (List.mem "location" config));
  assert (not (List.mem "location-trusted" config));
  let state = Sys.getenv "PAVE_META_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 ->
        has "url" Meta.models_url;
        has "request" "GET";
        has "max-filesize" (string_of_int Meta.max_response_bytes);
        assert (values "header" = [
          "Authorization: Bearer " ^ key; "Accept: application/json"]);
        `Assoc ["object", `String "list"; "data", `List [
          `Assoc ["object", `String "model"; "id", `String model;
            "created", `Int 1750000000; "owned_by", `String "meta"];
          `Assoc ["object", `String "model"; "id", `String other_model;
            "created", `Int 1750000000; "owned_by", `String "meta"]]]
    | 1 | 2 ->
        has "url" Meta.responses_url;
        has "request" "POST";
        assert (values "header" = ["Content-Type: application/json";
          "Authorization: Bearer " ^ key]);
        let body_path = one "data-binary" in
        assert (String.starts_with ~prefix:"@" body_path);
        let file = open_in_bin (String.sub body_path 1
          (String.length body_path - 1)) in
        let body = Fun.protect ~finally:(fun () -> close_in file)
          (fun () -> Yojson.Basic.from_string
            (really_input_string file (in_channel_length file))) in
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
         | `Assoc fields ->
             assert (not (List.mem_assoc "previous_response_id" fields));
             assert (not (List.mem_assoc "reasoning" fields))
         | _ -> fail "non-object request");
        if step = 1 then (
          assert (field "input" body = `List [user]);
          `Assoc ["model", `String model; "status", `String "completed";
            "output", `List [reasoning; commentary; raw_call]])
        else (
          let output = match field "input" body with
            | `List [first; signed_reasoning; signed_commentary; signed_call; result]
              when first = user && signed_reasoning = reasoning &&
                signed_commentary = commentary && signed_call = raw_call -> result
            | _ -> fail "raw signed output or commentary not replayed" in
          assert (field "type" output = `String "function_call_output");
          assert (field "call_id" output = `String call_id);
          let result = match field "output" output with
            | `String value -> Yojson.Basic.from_string value
            | _ -> fail "tool result is not a string" in
          let product = match field "product" result with
            | `Int number -> number | _ -> fail "tool result missing product" in
          `Assoc ["model", `String model; "status", `String "completed";
            "output", `List [`Assoc ["type", `String "message";
              "role", `String "assistant"; "status", `String "completed";
              "content", `List [`Assoc ["type", `String "output_text";
                "text", `String (Printf.sprintf "Six times seven is %d." product)]]]]])
    | _ -> fail "unexpected request" in
  let count = open_out state in
  output_string count (string_of_int (step + 1)); close_out count;
  let file = open_out_bin (one "output") in
  output_string file (Yojson.Basic.to_string response); close_out file;
  print_string "200"; flush stdout

let () =
  if Array.length Sys.argv >= 3 && Sys.argv.(1) = "--disable" &&
    Sys.argv.(2) = "--config" then
    (try fake_curl () with exn ->
      prerr_endline (Printexc.to_string exn); exit 2)
  else (
    let old_primary = Sys.getenv_opt "MODEL_API_KEY" in
    let old_alias = Sys.getenv_opt "META_API_KEY" in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "MODEL_API_KEY" (Option.value ~default:"" old_primary);
      Unix.putenv "META_API_KEY" (Option.value ~default:"" old_alias)) (fun () ->
      Unix.putenv "META_API_KEY" key;
      Unix.putenv "MODEL_API_KEY" "primary-key";
      assert (Meta.env_api_key () = Some "primary-key");
      Unix.putenv "MODEL_API_KEY" "";
      assert (Meta.env_api_key () = Some key);
      Unix.putenv "META_API_KEY" "bad\nheader";
      assert (Meta.env_api_key () = None));
    let count = ref 0 in
    let http ~url ~headers =
      incr count;
      assert (url = Meta.models_url);
      assert (headers = ["Authorization", "Bearer " ^ key;
        "Accept", "application/json"]);
      Ok (200, {|{"object":"list","data":[{"id":"operator-selected-responses-model","object":"model","created":1750000000,"owned_by":"meta"},{"id":"catalog-entry-with-unknown-route","object":"model","created":1750000000,"owned_by":"meta"},{"id":"third-valid-meta-model","object":"model","created":1750000000,"owned_by":"meta"}]}|}) in
    assert (Meta.discover ~http ~api_key:key () =
      Ok [model; other_model; "third-valid-meta-model"]);
    assert (!count = 1);
    expect_error (function Meta.Invalid_response _ -> true | _ -> false)
      (Meta.parse_models
        {|{"object":"list","data":[{"id":"duplicate","object":"model","created":1,"owned_by":"meta"},{"id":"duplicate","object":"model","created":2,"owned_by":"meta"}]}|});
    List.iter (fun body ->
      expect_error (function Meta.Invalid_response _ -> true | _ -> false)
        (Meta.discover ~http:(fun ~url:_ ~headers:_ -> Ok (200, body))
          ~api_key:key ())) [
        {|{"object":"list","data":[{"id":"bad\nmodel","object":"model","created":1,"owned_by":"meta"}]}|};
        {|{"object":"list","data":[{"id":"model","object":"image"}]}|};
        {|{"data":[]}|}; "not json";
        String.make (Meta.max_response_bytes + 1) 'x'];
    expect_error ((=) (Meta.Http_error 302))
      (Meta.discover ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect"))
        ~api_key:key ());
    assert (Meta.responses_headers ~endpoint:Meta.responses_url ~api_key:key =
      ["Authorization: Bearer " ^ key]);
    (match Meta.responses_headers
      ~endpoint:"https://evil.example/v1/responses" ~api_key:key with
     | exception Invalid_argument _ -> ()
     | _ -> fail "key escaped pinned host");
    (match Meta.responses_headers
      ~endpoint:Meta.responses_url ~api_key:"bad\r\nheader" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "header injection accepted");
    let bare = `Assoc ["model", `String model; "status", `String "completed";
      "output", `List [`Assoc ["type", `String "reasoning";
        "id", `String "rs_unsigned"; "summary", `List []]; raw_call]] in
    (match Meta.parse_completion ~model bare with
     | exception Protocol.Invalid_response _ -> ()
     | _ -> fail "reasoning without signed encrypted state accepted");
    let signed = Meta.parse_completion ~model (`Assoc [
      "model", `String model; "status", `String "completed";
      "output", `List [reasoning; commentary; raw_call]]) in
    let tampered = { signed with content = Some "altered" } in
    (match Meta.request ~model [Protocol.user "What is six times seven?";
      tampered; Protocol.tool_result call_id {|{"product":42}|}] [tool] with
     | exception Protocol.Invalid_response _ -> ()
     | _ -> fail "edited signed transcript accepted");
    (match Meta.request ~model:other_model
      [Protocol.user "What is six times seven?"; signed;
       Protocol.tool_result call_id {|{"product":42}|}] [tool] with
     | exception Protocol.Invalid_response _ -> ()
     | _ -> fail "signed output replayed against another model");
    let directory = Filename.temp_file "pave-meta-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_META_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_META_FIXTURE_STATE" state;
      let listed = match Result.map Pave.Model_discovery.model_ids
        (Pave.Model_discovery.discover ~provider:"meta"
          ~credential:(Pave.Model_discovery.Api_key key) ()) with
        | Ok ids -> ids
        | _ -> fail "production authenticated Meta model discovery failed" in
      assert (listed = [model; other_model]);
      (* Catalog rows do not imply a Responses capability. The operator must
         explicitly select the pinned route and a supported model ID. *)
      let config : Pave.Provider.config = {
        endpoint = Meta.responses_url; api_key = key; model;
        api = Pave.Provider.Meta_responses } in
      (match Pave.Provider.complete
        { config with endpoint = "https://evil.example/v1/responses" }
        [Protocol.user "Do not leak credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> fail "production route sent key to hostile host");
      let complete messages = Pave.Provider.complete config messages [tool] in
      let first = complete [Protocol.user "What is six times seven?"] in
      let call = match first.tool_calls with
        | [call] when call.id = call_id && call.name = "multiply_seven" &&
          call.arguments = arguments -> call
        | _ -> fail "Meta Responses tool call not decoded" in
      assert (first.content = Some "I will calculate that.");
      assert (first.provider_state = Some (`Assoc [
        "provider", `String "meta"; "model", `String model;
        "output", `List [reasoning; commentary; raw_call]]));
      let result = match field "number" call.arguments with
        | `Int number -> Yojson.Basic.to_string
            (`Assoc ["product", `Int (number * 7)])
        | _ -> fail "unexpected tool argument" in
      let final = complete [Protocol.user "What is six times seven?";
        first; Protocol.tool_result call.id result] in
      assert (final.content = Some "Six times seven is 42.");
      assert (final.provider_state = None);
      let file = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in file)
        (fun () -> int_of_string (input_line file)) = 3));
    print_endline "Meta authenticated listing and stateless signed Responses continuation: ok")
