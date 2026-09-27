open Pave

let model = "claude-sonnet-4-5"
let token = "vertex-fixture-token"
let call_id = "call-vertex-17"
let arguments = `Assoc ["key", `String "alpha"]
let key_schema = `Assoc ["type", `String "string"]
let parameters = `Assoc [
  "type", `String "object";
  "properties", `Assoc ["key", key_schema] ]
let tool = `Assoc [
  "type", `String "function";
  "function", `Assoc [
    "name", `String "lookup";
    "description", `String "Look up an item";
    "parameters", parameters] ]
let expected_tool = `Assoc [
  "name", `String "lookup";
  "input_schema", parameters;
  "description", `String "Look up an item" ]
let fail detail = failwith ("Vertex Claude fixture: " ^ detail)
let field name json = Protocol.member name json

let write_file path contents =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output contents)

let read_file path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let read_stdin () =
  let buffer = Buffer.create 512 in
  let rec read first =
    match input_line stdin with
    | line ->
        if not first then Buffer.add_char buffer '\n';
        Buffer.add_string buffer line;
        read false
    | exception End_of_file -> Buffer.contents buffer in
  read true

let config_values configuration name =
  let prefix = name ^ " = " in
  List.filter_map (fun line ->
    if String.starts_with ~prefix line then
      match Yojson.Basic.from_string
        (String.sub line (String.length prefix) (String.length line - String.length prefix)) with
      | `String value -> Some value
      | _ -> fail "curl option was not a string"
    else None) configuration

let one configuration name = match config_values configuration name with
  | [value] -> value
  | _ -> fail ("expected one curl " ^ name)

let user = Protocol.user "Look up alpha"

let fake_curl () =
  try
    if List.mem "--config" (Array.to_list Sys.argv) then (
      let configuration = String.split_on_char '\n' (read_stdin ()) in
      assert (one configuration "proto" = "=https");
      assert (one configuration "url" =
        "https://us-central1-aiplatform.googleapis.com/v1/projects/research-123/locations/us-central1/publishers/anthropic/models/claude-sonnet-4-5:streamRawPredict"
        || one configuration "url" =
        "https://us-central1-aiplatform.googleapis.com/v1/projects/research-123/locations/us-central1/publishers/anthropic/models/claude-sonnet-4-5:rawPredict");
      assert (List.mem ("Authorization: Bearer " ^ token)
        (config_values configuration "header"));
      let body_path = one configuration "data-binary" in
      assert (String.starts_with ~prefix:"@" body_path);
      let body = Yojson.Basic.from_string
        (read_file (String.sub body_path 1 (String.length body_path - 1))) in
      assert (field "model" body = `Null);
      assert (field "anthropic_version" body = `String "vertex-2023-10-16");
      let state = Sys.getenv "PAVE_VERTEX_CLAUDE_STATE" in
      let step = if Sys.file_exists state then int_of_string (read_file state) else 0 in
      write_file state (string_of_int (step + 1));
      (match step with
       | 0 ->
           assert (String.ends_with ~suffix:":streamRawPredict" (one configuration "url"));
           assert (field "stream" body = `Bool true);
           assert (field "tools" body = `List [expected_tool]);
           assert (field "messages" body = `List [
             `Assoc ["role", `String "user"; "content", `String "Look up alpha"]]);
           let headers = one configuration "dump-header" in
           write_file headers
             "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n";
           let event kind data =
             "event: " ^ kind ^ "\r\ndata: " ^ data ^ "\r\n\r\n" in
           print_string (
             event "message_start"
               {|{"type":"message_start","message":{"id":"msg_1","type":"message","role":"assistant","usage":{"input_tokens":2}}}|} ^
             event "content_block_start"
               {|{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}|} ^
             event "content_block_delta"
               {|{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Searching."}}|} ^
             event "content_block_stop" {|{"type":"content_block_stop","index":0}|} ^
             event "content_block_start"
               {|{"type":"content_block_start","index":1,"content_block":{"type":"tool_use","id":"call-vertex-17","name":"lookup","input":{}}}|} ^
             event "content_block_delta"
               {|{"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"{\"key\":\"alpha\"}"}}|} ^
             event "content_block_stop" {|{"type":"content_block_stop","index":1}|} ^
             event "message_delta"
               {|{"type":"message_delta","delta":{"stop_reason":"tool_use"},"usage":{"output_tokens":3}}|} ^
             event "message_stop" {|{"type":"message_stop"}|});
           flush stdout
       | 1 ->
           assert (String.ends_with ~suffix:":rawPredict" (one configuration "url"));
           assert (field "stream" body = `Bool false);
           (match field "messages" body with
            | `List [first; assistant; result] ->
                assert (first = `Assoc ["role", `String "user";
                  "content", `String "Look up alpha"]);
                assert (field "role" assistant = `String "assistant");
                assert (field "content" assistant = `List [
                  `Assoc ["type", `String "text"; "text", `String "Searching."];
                  `Assoc ["type", `String "tool_use"; "id", `String call_id;
                    "name", `String "lookup"; "input", arguments]]);
                assert (field "content" result = `List [
                  `Assoc ["type", `String "tool_result";
                    "tool_use_id", `String call_id;
                    "content", `String "alpha result"]])
            | _ -> fail "tool result was not sent as native Anthropic history");
           assert (field "tools" body = `List [expected_tool]);
           let response = {|{"type":"message","id":"msg_2","role":"assistant","content":[{"type":"text","text":"The result is alpha."}],"stop_reason":"end_turn","usage":{"input_tokens":4,"output_tokens":5}}|} in
           write_file (one configuration "output") response;
           print_string "200";
           flush stdout
       | _ -> fail "unexpected extra Vertex request")
    ) else (
      let arguments = Array.to_list Sys.argv |> List.tl in
      assert (List.mem "--request" arguments);
      assert (List.mem "POST" arguments);
      assert (List.mem "https://oauth2.googleapis.com/token" arguments);
      assert (not (List.exists (fun argument ->
        argument = "fixture-client-secret" || argument = "fixture-refresh-token") arguments));
      let body = read_stdin () in
      assert (body = "client_id=fixture-client&client_secret=fixture-client-secret&refresh_token=fixture-refresh-token&grant_type=refresh_token");
      print_endline
        {|{"access_token":"vertex-fixture-token","token_type":"Bearer","expires_in":3600}|}
    )
  with exn ->
    prerr_endline (Printexc.to_string exn);
    exit 2

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect ~finally:(fun () ->
    Unix.putenv name (Option.value previous ~default:"")) f

let () =
  if Array.length Sys.argv >= 2 && Sys.argv.(1) = "--disable" then fake_curl ()
  else (
    let directory = Filename.temp_file "pave-vertex-claude-" "" in
    Sys.remove directory;
    Unix.mkdir directory 0o700;
    let credentials = Filename.concat directory "adc.json" in
    let curl = Filename.concat directory "curl" in
    let state = Filename.concat directory "requests" in
    let response = Filename.concat directory "unused-response" in
    Fun.protect ~finally:(fun () ->
      List.iter (fun path -> try Sys.remove path with Sys_error _ -> ())
        [credentials; curl; state; response];
      Unix.rmdir directory) (fun () ->
        let executable = Filename.quote Sys.executable_name in
        write_file curl ("#!/bin/sh\nexec " ^ executable ^ " \"$@\"\n");
        Unix.chmod curl 0o700;
        write_file credentials
          {|{"type":"authorized_user","client_id":"fixture-client","client_secret":"fixture-client-secret","refresh_token":"fixture-refresh-token"}|};
        let original_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
        let config : Provider.config = {
          endpoint = ""; api_key = ""; model;
          api = Provider.Vertex_anthropic } in
        with_env "PATH" (directory ^ ":" ^ original_path) (fun () ->
          with_env "GOOGLE_APPLICATION_CREDENTIALS" credentials (fun () ->
            with_env "GOOGLE_CLOUD_ACCESS_TOKEN" "" (fun () ->
              with_env "CLOUDSDK_AUTH_ACCESS_TOKEN" "" (fun () ->
                with_env "GOOGLE_CLOUD_PROJECT" "research-123" (fun () ->
                  with_env "GOOGLE_CLOUD_LOCATION" "us-central1" (fun () ->
                    with_env "PAVE_VERTEX_CLAUDE_STATE" state (fun () ->
                      let streamed_text = ref [] and usage = ref None in
                      let first = Provider.complete
                        ~authentication:Provider.Cloud_identity
                        ~on_text:(fun text -> streamed_text := text :: !streamed_text)
                        ~on_usage:(fun reported -> usage := Some reported)
                        config [user] [tool] in
                      assert (List.rev !streamed_text = ["Searching."]);
                      assert (first.content = Some "Searching.");
                      assert (first.tool_calls = [{ Protocol.id = call_id;
                        name = "lookup"; arguments }]);
                      (match !usage with
                       | Some reported ->
                           assert (reported.input_tokens = 2);
                           assert (reported.output_tokens = 3)
                       | None -> fail "streamed provider usage was lost");
                      let final = Provider.complete
                        ~authentication:Provider.Cloud_identity config
                        [user; first; Protocol.tool_result call_id "alpha result"] [tool] in
                      assert (final.content = Some "The result is alpha.");
                      assert (final.tool_calls = []);
                      assert (read_file state = "2")))))))));
    print_endline "Vertex Claude ADC streaming and native tool continuation: ok")
