let field name json = Pave.Protocol.member name json
let expect_invalid f = match f () with
  | exception Pave.Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid Ollama response"

let assistant content calls : Pave.Protocol.message =
  { role = "assistant"; content; tool_calls = calls; tool_call_id = None; tool_result_content = None; provider_state = None }
let system content : Pave.Protocol.message =
  { role = "system"; content = Some content; tool_calls = [];
    tool_call_id = None; tool_result_content = None; provider_state = None }
let native_call name arguments = `Assoc [ "type", `String "function";
  "function", `Assoc [ "name", `String name; "arguments", arguments ] ]
let native_message role content extras =
  `Assoc ([ "role", `String role; "content", `String content ] @ extras)
let completion ?(reason="stop") message =
  `Assoc [ "done", `Bool true; "done_reason", `String reason; "message", message ]

let () =
  let open Pave.Protocol in
  let first = { id = "ollama:0:search"; name = "search";
    arguments = `Assoc [ "query", `String "東京" ] } in
  let second = { id = "ollama:1:search"; name = "search";
    arguments = `Assoc [ "query", `String "京都" ] } in
  let schema = `Assoc [ "type", `String "object";
    "properties", `Assoc [ "query", `Assoc [ "type", `String "string" ] ];
    "required", `List [ `String "query" ] ] in
  let definition = `Assoc [ "type", `String "function";
    "function", `Assoc [ "name", `String "search";
      "description", `String "Search"; "parameters", schema ] ] in
  let request = Pave.Ollama_wire.request ~model:"qwen" [
    system "Be helpful"; user "東京と京都";
    assistant (Some "Searching") [ first; second ];
    tool_result second.id "京都 result"; tool_result first.id "東京 result";
    assistant (Some "完了") [] ] [ definition ] in
  assert (field "model" request = `String "qwen");
  assert (field "stream" request = `Bool false);
  assert (field "tools" request = `List [ definition ]);
  assert (field "messages" request = `List [
    native_message "system" "Be helpful" [];
    native_message "user" "東京と京都" [];
    native_message "assistant" "Searching" [ "tool_calls", `List [
      native_call "search" first.arguments; native_call "search" second.arguments ] ];
    native_message "tool" "京都 result" [ "tool_name", `String "search" ];
    native_message "tool" "東京 result" [ "tool_name", `String "search" ];
    native_message "assistant" "完了" [] ]);
  let image_request = Pave.Ollama_wire.request ~model:"qwen" [
    assistant (Some "Searching") [ first; second ];
    tool_result_blocks second.id [ Text "京都 result"; Image {
      mime_type = "image/png"; data = "c2Vjb25k" }; Text "continued";
      Image { mime_type = "image/jpeg"; data = "dGhpcmQ=" } ];
    tool_result_blocks first.id [ Image { mime_type = "image/jpeg"; data = "Zmlyc3Q=" } ] ] [] in
  assert (field "messages" image_request = `List [
    native_message "assistant" "Searching" [ "tool_calls", `List [
      native_call "search" first.arguments; native_call "search" second.arguments ] ];
    native_message "tool" "京都 result\ncontinued" [
      "tool_name", `String "search";
      "images", `List [ `String "c2Vjb25k"; `String "dGhpcmQ=" ] ];
    native_message "tool" "Tool result contained image(s)." [
      "tool_name", `String "search";
      "images", `List [ `String "Zmlyc3Q=" ] ] ]);
  expect_invalid (fun () -> Pave.Ollama_wire.request ~model:"qwen"
    [ assistant None [first; second]; tool_result first.id "one" ] []);
  expect_invalid (fun () -> Pave.Ollama_wire.request ~model:"qwen"
    [ assistant None [first]; tool_result first.id "one";
      tool_result first.id "again" ] []);
  expect_invalid (fun () -> Pave.Ollama_wire.request ~model:"qwen"
    [ assistant None [first; first] ] []);
  let invalid_tool = `Assoc [ "type", `String "function";
    "function", `Assoc [ "name", `String "search";
      "parameters", `Assoc [ "type", `String "string" ] ] ] in
  expect_invalid (fun () -> Pave.Ollama_wire.request ~model:"qwen"
    [user "hello"] [ invalid_tool ]);
  let message = native_message "assistant" "こんにちは" [] in
  assert (Pave.Ollama_wire.parse_completion (completion message) =
    assistant (Some "こんにちは") []);
  let reported = `Assoc [ "prompt_eval_count", `Int 18;
    "eval_count", `Int 7 ] in
  assert (Pave.Ollama_wire.usage reported =
    Some { input_tokens = 18; output_tokens = 7 });
  assert (Pave.Ollama_wire.usage (completion message) = None);
  assert (Pave.Ollama_wire.usage (`Assoc [ "prompt_eval_count", `Int 18 ]) = None);
  assert (Pave.Ollama_wire.usage (`Assoc [ "prompt_eval_count", `Int (-1);
    "eval_count", `Int 7 ]) = None);
  let calls = native_message "assistant" "" [ "tool_calls", `List [
    native_call "search" first.arguments; native_call "search" second.arguments ] ] in
  assert (Pave.Ollama_wire.parse_completion (completion calls) =
    assistant (Some "") [ first; second ]);
  let string_args = native_message "assistant" "" [ "tool_calls", `List [
    native_call "search" (`String {|{"query":"東京"}|}) ] ] in
  assert (Pave.Ollama_wire.parse_completion (completion string_args) =
    assistant (Some "") [ first ]);
  expect_invalid (fun () -> Pave.Ollama_wire.parse_completion
    (`Assoc [ "error", `String "model not found" ]));
  expect_invalid (fun () -> Pave.Ollama_wire.parse_completion
    (`Assoc [ "done", `Bool false; "message", message ]));
  expect_invalid (fun () -> Pave.Ollama_wire.parse_completion
    (completion ~reason:"length" message));
  expect_invalid (fun () -> Pave.Ollama_wire.parse_completion
    (completion ~reason:"load" message));
  expect_invalid (fun () -> Pave.Ollama_wire.parse_completion
    (completion ~reason:"tool_calls" message));
  expect_invalid (fun () -> Pave.Ollama_wire.parse_completion
    (completion (native_message "assistant" "" [])));
  expect_invalid (fun () -> Pave.Ollama_wire.parse_completion
    (completion (native_message "assistant" "" [ "tool_calls", `List [
      native_call "search" (`String "{broken") ] ])));
  print_endline "Ollama native wire: ok"
