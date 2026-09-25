open Pave

module Mantle = Bedrock_mantle
let field = Protocol.member
let supported = "us-east-2"
let target = Mantle.endpoint ~region:supported ()
let listing = Mantle.discovery_endpoint ~region:supported ()
let model = "fixture-model-from-account"
let key = "bedrock-private-test-key"
let call : Protocol.tool_call = {
  id = "call_lookup"; name = "lookup";
  arguments = `Assoc ["file", `String "alpha.txt"] }
let schema = `Assoc ["type", `String "object";
  "properties", `Assoc ["file", `Assoc ["type", `String "string"]]]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "lookup";
    "description", `String "Read a file"; "parameters", schema]]
let tool_answer = `Assoc ["status", `String "completed"; "output", `List [
  `Assoc ["type", `String "function_call"; "status", `String "completed";
    "call_id", `String call.id; "name", `String call.name;
    "arguments", `String {|{"file":"alpha.txt"}|}]]]
let final_answer = `Assoc ["status", `String "completed"; "output", `List [
  `Assoc ["type", `String "message"; "role", `String "assistant";
    "content", `List [`Assoc ["type", `String "output_text";
      "text", `String "alpha.txt contains: contents-alpha"]]]]]

let invalid f = match f () with
  | exception Invalid_argument _ | exception Provider.Provider_error _ -> ()
  | _ -> failwith "unsafe Mantle configuration was accepted"
let invalid_listing f = match f () with
  | exception Protocol.Invalid_response _ -> ()
  | _ -> failwith "malformed Mantle model listing was accepted"
let with_env entries fn =
  let previous = List.map (fun (name, _) -> name, Sys.getenv_opt name) entries in
  Fun.protect ~finally:(fun () -> List.iter (fun (name, value) ->
    Unix.putenv name (Option.value ~default:"" value)) previous) (fun () ->
    List.iter (fun (name, value) -> Unix.putenv name value) entries;
    fn ())

(* curl runs this same test executable in an isolated process. It receives
   the actual Provider curl config and serialized Responses request. *)
let config_entry key line =
  let prefix = key ^ " = " in
  if String.starts_with ~prefix line then
    let encoded = String.sub line (String.length prefix)
      (String.length line - String.length prefix) in
    Some (Scanf.sscanf encoded "%S" Fun.id)
  else None
let fake_curl () =
  let rec input acc = match input_line stdin with
    | line -> input (line :: acc)
    | exception End_of_file -> List.rev acc in
  let lines = input [] in
  let required key = match List.find_map (config_entry key) lines with
    | Some value -> value
    | None -> failwith ("missing curl option " ^ key) in
  let headers = List.filter_map (config_entry "header") lines in
  let url = required "url" in
  assert (url = target.url);
  assert (required "request" = "POST");
  assert (List.mem ("Authorization: Bearer " ^ key) headers);
  assert (List.mem "Content-Type: application/json" headers);
  let body_path = required "data-binary" in
  assert (String.starts_with ~prefix:"@" body_path);
  let ic = open_in_bin (String.sub body_path 1 (String.length body_path - 1)) in
  let body = Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    really_input_string ic (in_channel_length ic)) |> Yojson.Basic.from_string in
  assert (field "model" body = `String model);
  let input = match field "input" body with `List items -> items | _ -> assert false in
  let continued = List.exists (fun item -> field "type" item = `String "function_call_output") input in
  if continued then (
    assert (List.exists (fun item -> field "type" item = `String "function_call" &&
      field "call_id" item = `String call.id && field "name" item = `String call.name &&
      field "arguments" item = `String {|{"file":"alpha.txt"}|}) input);
    assert (List.exists (fun item -> field "type" item = `String "function_call_output" &&
      field "call_id" item = `String call.id &&
      field "output" item = `String "contents-alpha") input))
  else (
    assert (field "tools" body = `List [`Assoc ["type", `String "function";
      "name", `String "lookup"; "parameters", schema; "strict", `Bool false;
      "description", `String "Read a file"]]);
    assert (List.length input = 1));
  let output = open_out_bin (required "output") in
  Fun.protect ~finally:(fun () -> close_out_noerr output) (fun () ->
    output_string output (Yojson.Basic.to_string
      (if continued then final_answer else tool_answer)));
  print_string "200";
  flush stdout

let test_http_fixture () =
  let temp = Filename.temp_file "pave-bedrock-mantle" "" in
  Sys.remove temp;
  Unix.mkdir temp 0o700;
  let stub = Filename.concat temp "curl" in
  let output = open_out stub in
  output_string output ("#!/bin/sh\nPAVE_MANTLE_FAKE_CURL=1 exec " ^
    Filename.quote Sys.executable_name ^ " \"$@\"\n");
  close_out output;
  Unix.chmod stub 0o700;
  Fun.protect ~finally:(fun () -> Sys.remove stub; Unix.rmdir temp) (fun () ->
    with_env ["PATH", temp ^ ":" ^ Option.value ~default:"" (Sys.getenv_opt "PATH");
      "AWS_REGION", supported; "AWS_DEFAULT_REGION", "";
      "AWS_BEARER_TOKEN_BEDROCK", key;
      "AWS_ACCESS_KEY_ID", "not-a-bearer-key"] (fun () ->
      let config : Provider.config = {
        endpoint = target.url; model; api_key = "";
        api = Provider.Bedrock_mantle_responses } in
      let original = [Protocol.user "Look up alpha.txt"] in
      let first = Provider.complete config original [tool] in
      assert (first.tool_calls = [call]);
      let result = Protocol.tool_result call.id "contents-alpha" in
      let second = Provider.complete config (original @ [first; result]) [tool] in
      assert (second.content = Some "alpha.txt contains: contents-alpha");
      assert (second.tool_calls = [])))

let () =
  if Sys.getenv_opt "PAVE_MANTLE_FAKE_CURL" = Some "1" then fake_curl ()
  else (
    assert (target.host = "bedrock-mantle.us-east-2.api.aws");
    assert (target.path = "/openai/v1/responses");
    assert (listing.url = "https://bedrock-mantle.us-east-2.api.aws/v1/models");
    assert (Mantle.region ~getenv:(function "AWS_REGION" -> Some supported
      | "AWS_DEFAULT_REGION" -> Some "eu-west-1" | _ -> None) () = supported);
    assert (Mantle.region ~getenv:(function "AWS_DEFAULT_REGION" -> Some "eu-west-1"
      | _ -> None) () = "eu-west-1");
    invalid (fun () -> Mantle.region ~getenv:(fun _ -> None) ());
    List.iter (fun unsafe -> invalid (fun () -> Mantle.endpoint ~region:unsafe ())) [
      "us-east-3"; "us-east-1.evil.invalid"; "us-east-1/path"; "us-east-1\nHost:evil"];
    with_env ["AWS_REGION", supported; "AWS_BEARER_TOKEN_BEDROCK", "env-key"] (fun () ->
      assert (Mantle.resolve ~endpoint:target.url ~api_key:"explicit-key" () =
        (target.url, ["Authorization: Bearer explicit-key"]));
      assert (Mantle.resolve ~endpoint:target.url ~api_key:"" () =
        (target.url, ["Authorization: Bearer env-key"]));
      List.iter (fun unsafe -> invalid (fun () ->
        Mantle.resolve ~endpoint:unsafe ~api_key:"explicit-key" ())) [
        "http://bedrock-mantle.us-east-2.api.aws/openai/v1/responses";
        "https://bedrock-mantle.us-east-2.api.aws.evil.invalid/openai/v1/responses";
        "https://bedrock-mantle.us-east-2.api.aws@evil.invalid/openai/v1/responses";
        "https://bedrock-mantle.us-east-2.api.aws:443/openai/v1/responses";
        "https://bedrock-mantle.us-east-2.api.aws/v1/responses";
        "https://bedrock-mantle.us-east-2.api.aws/openai/v1/responses?x=1";
        "https://bedrock-mantle.us-east-2.api.aws/openai/v1/responses#evil"];
      invalid (fun () -> Mantle.resolve ~endpoint:target.url
        ~api_key:"evil\r\nHost:steal.example" ()));
    with_env ["AWS_REGION", supported; "AWS_BEARER_TOKEN_BEDROCK", "";
      "AWS_ACCESS_KEY_ID", "static-credentials-are-not-bearer"] (fun () ->
      invalid (fun () -> Mantle.resolve ~endpoint:target.url ~api_key:"" ()));
    assert (Mantle.parse_models (`Assoc ["data", `List [
      `Assoc ["id", `String model; "status", `String "available"];
      `Assoc ["id", `String "unavailable-model"; "status", `String "unavailable"];
      `Assoc ["id", `String model];
      `Assoc ["id", `String "another-account-model"]]]) =
      [model; "another-account-model"]);
    invalid_listing (fun () -> Mantle.parse_models (`Assoc ["data", `List [
      `Assoc ["id", `String "x\nHeader:unsafe"]]]));
    invalid_listing (fun () -> Mantle.parse_models (`Assoc ["data", `String "bad"]));
    with_env ["AWS_REGION", supported; "AWS_DEFAULT_REGION", "";
      "AWS_BEARER_TOKEN_BEDROCK", "env-key"] (fun () ->
      let requests = ref 0 in
      let http ~url ~headers =
        incr requests;
        assert (url = listing.url);
        assert (List.mem ("Authorization", "Bearer " ^ key) headers);
        Ok (200, Yojson.Basic.to_string (`Assoc ["data", `List [
          `Assoc ["id", `String model; "status", `String "available"];
          `Assoc ["id", `String "disabled"; "status", `String "unavailable"];
          `Assoc ["id", `String "newly-enabled-model"]]])) in
      assert (Model_discovery.discover ~provider:"bedrock-mantle"
        ~credential:(Model_discovery.Api_key key) ~http () =
        Ok [model; "newly-enabled-model"]);
      assert (!requests = 1);
      (match Model_discovery.discover ~provider:"bedrock-mantle"
         ~credential:(Model_discovery.Api_key key)
         ~http:(fun ~url ~headers ->
           assert (url = listing.url);
           assert (List.mem ("Authorization", "Bearer " ^ key) headers);
           Ok (200, String.make 1_048_577 'x')) () with
       | Error (Model_discovery.Invalid_response _) -> ()
       | _ -> failwith "unbounded Mantle listing was accepted"));
    test_http_fixture ();
    print_endline "Bedrock Mantle bearer Responses and account model listing: ok")
