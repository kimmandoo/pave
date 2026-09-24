open Pave

let field = Protocol.member
let item kind fields = `Assoc (("type", `String kind) :: fields)
let invalid f = match f () with
  | exception Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid Codex response"
let system text : Protocol.message =
  { role = "system"; content = Some text; tool_calls = []; tool_call_id = None;
    provider_state = None }
let developer text : Protocol.message =
  { role = "developer"; content = Some text; tool_calls = []; tool_call_id = None;
    provider_state = None }
let completed model output = `Assoc ["status", `String "completed";
  "model", `String model; "output", `List output]
let reasoning = item "reasoning" ["id", `String "rs_1";
  "status", `String "completed";
  "encrypted_content", `String "encrypted-turn-state";
  "summary", `List [item "summary_text" ["text", `String "Considering the file"]]]
let call = item "function_call" ["id", `String "fc_1";
  "status", `String "completed"; "call_id", `String "call_1";
  "name", `String "read_file";
  "arguments", `String {|{"path":"README.md"}|}]
let message = item "message" ["id", `String "msg_1";
  "status", `String "completed"; "role", `String "assistant";
  "content", `List [item "output_text" ["text", `String "Reading"]]]
let without_id = function
  | `Assoc fields -> `Assoc (List.remove_assoc "id" fields)
  | _ -> assert false

let () =
  let model = "gpt-5.1-codex" in
  let schema = `Assoc ["type", `String "object"] in
  let tool = item "function" ["function", `Assoc [
    "name", `String "read_file"; "description", `String "Read a file";
    "parameters", schema]] in
  let answer = Codex_wire.parse_completion ~model
    (completed model [reasoning; message; call]) in
  assert (answer.content = Some "Reading");
  assert (answer.tool_calls = [{ Protocol.id = "call_1"; name = "read_file";
    arguments = `Assoc ["path", `String "README.md"] }]);
  let result = Protocol.tool_result "call_1" "README contents" in
  let conversation = [system "Be exact"; developer "Read before replying";
    Protocol.user "Read the file"; answer; result; Protocol.user "Summarize"] in
  let body = Codex_wire.request ~model conversation [tool] in
  assert (field "model" body = `String model);
  assert (field "store" body = `Bool false);
  assert (field "stream" body = `Bool true);
  assert (field "include" body = `List [`String "reasoning.encrypted_content"]);
  assert (field "instructions" body = `String "Be exact");
  assert (field "tools" body = `List [item "function" [
    "name", `String "read_file"; "parameters", `Assoc [
      "type", `String "object"; "properties", `Assoc []];
    "description", `String "Read a file"]]);
  assert (field "input" body = `List [
    `Assoc ["role", `String "developer"; "content", `List [
      item "input_text" ["text", `String "Read before replying"]]];
    `Assoc ["role", `String "user"; "content", `List [
      item "input_text" ["text", `String "Read the file"]]];
    without_id reasoning; without_id message; without_id call;
    item "function_call_output" ["call_id", `String "call_1";
      "output", `String "README contents"];
    `Assoc ["role", `String "user"; "content", `List [
      item "input_text" ["text", `String "Summarize"]]]]);
  let strict_tool = item "function" ["function", `Assoc [
    "name", `String "read_file"; "parameters", schema; "strict", `Bool false]] in
  assert (field "strict" (match field "tools" (Codex_wire.request ~model [] [strict_tool]) with
    | `List [value] -> value | _ -> assert false) = `Bool false);
  let tmp = Filename.temp_file "pave-codex-wire-" ".jsonl" in
  Sys.remove tmp;
  Fun.protect ~finally:(fun () -> try Sys.remove tmp with Sys_error _ -> ()) (fun () ->
    let session = Session.open_file tmp in
    List.iter (fun msg -> ignore (Session.append session msg))
      [Protocol.user "Read the file"; answer; result; Protocol.user "Summarize"];
    let reopened = Session.open_file tmp in
    let restored = Session.history reopened in
    assert (restored = [Protocol.user "Read the file"; answer; result;
      Protocol.user "Summarize"]);
    let replay = Codex_wire.request ~model restored [tool] in
    assert (field "input" replay = `List [
      `Assoc ["role", `String "user"; "content", `List [
        item "input_text" ["text", `String "Read the file"]]];
      without_id reasoning; without_id message; without_id call;
      item "function_call_output" ["call_id", `String "call_1";
        "output", `String "README contents"];
      `Assoc ["role", `String "user"; "content", `List [
        item "input_text" ["text", `String "Summarize"]]]]);
    invalid (fun () -> Codex_wire.request ~model:"other-model" restored []));
  invalid (fun () -> Codex_wire.parse_completion ~model (completed model [
    item "message" ["role", `String "assistant"; "content", `List [
      item "refusal" ["refusal", `String "Denied"]]] ]));
  invalid (fun () -> Codex_wire.parse_completion ~model (`Assoc [
    "status", `String "incomplete"; "output", `List [message]]));
  invalid (fun () -> Codex_wire.parse_completion ~model (completed model [
    item "custom_tool_call" ["call_id", `String "custom_1";
      "name", `String "apply_patch"; "input", `String "patch"]]));
  invalid (fun () -> Codex_wire.parse_completion ~model (completed "different" [message]));
  invalid (fun () -> Codex_wire.request ~model [
    { answer with content = Some "Changed opaque assistant text" }; result] []);
  let long_id = String.make 65 'x' in
  let long_call = { Protocol.id = long_id; name = "read_file";
    arguments = `Assoc [] } in
  let long_body = Codex_wire.request ~model [
    { role = "assistant"; content = None; tool_calls = [long_call];
      tool_call_id = None; provider_state = None };
    Protocol.tool_result long_id "out"] [] in
  (match field "input" long_body with
  | `List [call; output] ->
      let id = field "call_id" call in
      assert (id = field "call_id" output);
      (match id with `String id ->
        assert (String.length id <= 64 && id <> long_id)
      | _ -> assert false)
  | _ -> assert false);
  invalid (fun () -> Codex_wire.request ~model [
    { role = "assistant"; content = None; tool_calls = [
      { long_call with id = "same|first" };
      { long_call with id = "same|second" } ];
      tool_call_id = None; provider_state = None };
    Protocol.tool_result "same|first" "out";
    Protocol.tool_result "same|second" "out"] []);
  print_endline "Codex wire: ok"
