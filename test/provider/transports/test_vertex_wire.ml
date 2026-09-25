open Pave

let field = Protocol.member
let invalid f = match f () with
  | exception Invalid_argument _ | exception Protocol.Invalid_response _ -> ()
  | _ -> failwith "unsafe Vertex wire value accepted"

let text value = `Assoc ["text", `String value]
let content role parts = `Assoc ["role", `String role; "parts", `List parts]
let completion parts = `Assoc ["candidates", `List [ `Assoc [
  "content", content "model" parts; "finishReason", `String "STOP" ] ] ]
let user = Protocol.user "Find both records"
let result id value = Protocol.tool_result id value
let parameters = `Assoc ["type", `String "object";
  "properties", `Assoc ["key", `Assoc ["type", `String "string"]]]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "lookup";
    "parameters", parameters]]
let fn id key signature = `Assoc [
  "functionCall", `Assoc ["id", `String id; "name", `String "lookup";
    "args", `Assoc ["key", `String key]];
  "thoughtSignature", `String signature ]

let () =
  let project = "research-123" and model = "gemini-3.1-pro-preview" in
  let endpoint location = Vertex_wire.endpoint ~project ~location ~model in
  assert (endpoint "global" =
    "https://aiplatform.googleapis.com/v1/projects/research-123/locations/global/publishers/google/models/gemini-3.1-pro-preview:streamGenerateContent?alt=sse");
  assert (String.starts_with ~prefix:"https://aiplatform.us.rep.googleapis.com/v1/"
    (endpoint "us"));
  assert (String.starts_with ~prefix:"https://aiplatform.eu.rep.googleapis.com/v1/"
    (endpoint "eu"));
  assert (String.starts_with ~prefix:"https://us-central1-aiplatform.googleapis.com/v1/"
    (endpoint "us-central1"));
  List.iter (fun location ->
    invalid (fun () -> endpoint location)) [
    ""; "us..example"; "us-central1.evil.example"; "us-central1/path";
    "us-central1@evil"; "us-central1:443"; "US-central1"; "us-central1\r\n" ];
  List.iter (fun project -> invalid (fun () ->
    Vertex_wire.endpoint ~project ~location:"global" ~model)) [
    ""; "abc"; "project/path"; "project.evil"; "project@evil"; "a--b#evil" ];
  List.iter (fun model -> invalid (fun () ->
    Vertex_wire.endpoint ~project ~location:"global" ~model)) [
    ""; "other/model"; "../models/evil"; "model?query"; "model%2Fescape" ];
  let parts = [text "Looking up "; fn "first-id" "alpha" "c2lnbmVkLTE=";
    fn "second-id" "beta" "c2lnbmVkLTI="] in
  let parsed = Vertex_wire.parse_completion ~model (completion parts) in
  let first, second = match parsed.tool_calls with
    | [first; second] -> first, second
    | _ -> failwith "Vertex failed to parse parallel tool calls" in
  assert (first.id = "first-id" && second.id = "second-id");
  assert (first.arguments = `Assoc ["key", `String "alpha"]);
  assert (field "provider" (Option.get parsed.provider_state) =
    `String "google-vertex");
  let request = Vertex_wire.request ~model
    [user; parsed; result second.id "beta result";
      result first.id "alpha result"] [tool] in
  let expected_parts = [text "Looking up ";
    `Assoc ["functionCall", `Assoc ["name", `String "lookup";
      "args", `Assoc ["key", `String "alpha"]];
      "thoughtSignature", `String "c2lnbmVkLTE="];
    `Assoc ["functionCall", `Assoc ["name", `String "lookup";
      "args", `Assoc ["key", `String "beta"]];
      "thoughtSignature", `String "c2lnbmVkLTI="]] in
  assert (field "contents" request = `List [
    content "user" [text "Find both records"];
    content "model" expected_parts;
    content "user" [
      `Assoc ["functionResponse", `Assoc ["name", `String "lookup";
        "response", `Assoc ["output", `String "alpha result"]]];
      `Assoc ["functionResponse", `Assoc ["name", `String "lookup";
        "response", `Assoc ["output", `String "beta result"]]] ] ]);
  assert (field "tools" request <> `Null);
  invalid (fun () -> Gemini_wire.request ~model [user; parsed] []);
  let direct = Gemini_wire.parse_completion ~model (completion parts) in
  invalid (fun () -> Vertex_wire.request ~model [user; direct] []);
  invalid (fun () -> Vertex_wire.request ~model:"another-model"
    [user; parsed] []);
  let stream = Gemini_stream.create ~model ~on_text:(fun _ -> ()) in
  let event = completion parts in
  Gemini_stream.feed stream ("data: " ^ Yojson.Basic.to_string event ^ "\n\n");
  let streamed = Vertex_wire.finish_stream ~model stream in
  assert (streamed.tool_calls = parsed.tool_calls);
  let streamed_request = Vertex_wire.request ~model
    [user; streamed; result second.id "beta result";
      result first.id "alpha result"] [tool] in
  assert (field "contents" streamed_request = field "contents" request);
  let answer = Vertex_wire.parse_completion ~model (completion [text "Both found."]) in
  assert (answer.content = Some "Both found.");
  let directory = Filename.temp_file "pave-vertex-adc-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let cli = Filename.concat directory "gcloud" in
  let credentials = Filename.concat directory "adc.json" in
  let output = open_out cli in
  output_string output
    "#!/bin/sh\n[ \"$1 $2 $3\" = 'auth application-default print-access-token' ] || exit 1\nprintf 'fixture-adc-token\\n'\n";
  close_out output;
  Unix.chmod cli 0o700;
  let output = open_out credentials in
  output_string output "{\"type\":\"authorized_user\"}";
  close_out output;
  let pid = Unix.fork () in
  if pid = 0 then (
    Unix.putenv "GOOGLE_CLOUD_ACCESS_TOKEN" "";
    Unix.putenv "CLOUDSDK_AUTH_ACCESS_TOKEN" "";
    Unix.putenv "GOOGLE_APPLICATION_CREDENTIALS" credentials;
    Unix.putenv "PATH" (directory ^ ":" ^ (match Sys.getenv_opt "PATH" with
      | Some path -> path | None -> "/usr/bin:/bin"));
    assert (Vertex_auth.access_token () = "fixture-adc-token");
    Unix.putenv "GOOGLE_CLOUD_ACCESS_TOKEN" "explicit-token";
    assert (Vertex_auth.access_token () = "explicit-token");
    Unix.putenv "GOOGLE_CLOUD_ACCESS_TOKEN" "invalid\nheader";
    (match Vertex_auth.access_token () with
     | exception Vertex_auth.Authentication_error _ -> ()
     | _ -> failwith "invalid bearer token accepted");
    exit 0);
  let _, status = Unix.waitpid [] pid in
  Sys.remove cli;
  Sys.remove credentials;
  Unix.rmdir directory;
  assert (status = Unix.WEXITED 0);
  print_endline "Vertex AI endpoint and signed parallel function roundtrip: ok"
