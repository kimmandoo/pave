open Pave

let field = Protocol.member
let invalid f = match f () with
  | exception Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid Responses response"
let assistant content calls : Protocol.message =
  { role = "assistant"; content; tool_calls = calls; tool_call_id = None; tool_result_content = None; provider_state = None }
let system content : Protocol.message =
  { role = "system"; content = Some content; tool_calls = [];
    tool_call_id = None; tool_result_content = None; provider_state = None }
let call id name arguments : Protocol.tool_call = { id; name; arguments }
let item kind fields = `Assoc (("type", `String kind) :: fields)
let completed outputs = `Assoc [ "status", `String "completed"; "output", `List outputs ]
let text value = item "output_text" [ "text", `String value ]
let message content = item "message" [ "role", `String "assistant";
  "status", `String "completed"; "content", `List content ]
let function_call id name arguments = item "function_call" [
  "status", `String "completed"; "call_id", `String id;
  "name", `String name; "arguments", `String arguments ]
let image_result id blocks = Protocol.tool_result_blocks id blocks



let () =
  let args = `Assoc [ "path", `String "alpha.txt" ] in
  let use = call "call_A" "read_file" args in
  let schema = `Assoc [ "type", `String "object";
    "properties", `Assoc [ "path", `Assoc [ "type", `String "string" ] ] ] in
  let tool = `Assoc [ "type", `String "function";
    "function", `Assoc [ "name", `String "read_file";
      "description", `String "Read file"; "parameters", schema ] ] in
  let transcript = [ system "Act carefully"; Protocol.user "Read the file";
    assistant (Some "Reading") [ use ]; Protocol.tool_result use.id "Contents";
    assistant (Some "Found it") [] ] in
  let wire = Openai_responses_wire.request ~model:"gpt-test" transcript [ tool ] in
  assert (field "model" wire = `String "gpt-test");
  assert (field "instructions" wire = `String "Act carefully");
  assert (field "stream" wire = `Null);
  assert (field "tools" wire = `List [ item "function" [
    "name", `String "read_file"; "parameters", schema; "strict", `Bool false;
    "description", `String "Read file" ] ]);
  assert (field "input" wire = `List [
    `Assoc [ "role", `String "user"; "content", `List [ item "input_text" [ "text", `String "Read the file" ] ] ];
    `Assoc [ "role", `String "assistant"; "content", `String "Reading" ];
    item "function_call" [ "call_id", `String use.id; "name", `String use.name;
      "arguments", `String (Yojson.Basic.to_string args) ];
    item "function_call_output" [ "call_id", `String use.id; "output", `String "Contents" ];
    `Assoc [ "role", `String "assistant"; "content", `String "Found it" ] ]);
  let typed_result = image_result use.id [
    Protocol.Text "before"; Protocol.Image { mime_type = "image/png"; data = "AQID" };
    Protocol.Text "after" ] in
  let typed_wire = Openai_responses_wire.request ~model:"gpt-test"
    [assistant None [use]; typed_result] [] in
  assert (field "input" typed_wire = `List [
    item "function_call" [ "call_id", `String use.id; "name", `String use.name;
      "arguments", `String (Yojson.Basic.to_string args) ];
    item "function_call_output" [ "call_id", `String use.id; "output", `List [
      item "input_text" ["text", `String "before"];
      item "input_image" ["image_url", `String "data:image/png;base64,AQID"];
      item "input_text" ["text", `String "after"] ] ] ]);
  let image_only = image_result use.id [
    Protocol.Image { mime_type = "image/jpeg"; data = "BAUG" } ] in
  let image_only_wire = Openai_responses_wire.request ~model:"gpt-test"
    [assistant None [use]; image_only] [] in
  assert (field "output" (match field "input" image_only_wire with
    | `List [_; output] -> output | _ -> assert false) = `List [
      item "input_image" ["image_url", `String "data:image/jpeg;base64,BAUG"];
      item "input_text" ["text", `String "(see attached image)"] ]);
  assert (field "stream" (Openai_responses_wire.request ~stream:true
    ~model:"gpt-test" [] []) = `Bool true);
  let response = completed [ message [ text "Hello "; text "world" ] ] in
  assert (Openai_responses_wire.parse_completion response = assistant (Some "Hello world") []);
  let reported = `Assoc [
    "input_tokens", `Int 21; "output_tokens", `Int 9;
    "output_tokens_details", `Assoc ["reasoning_tokens", `Int 4] ] in
  assert (Openai_responses_wire.usage (`Assoc ["usage", reported]) =
    Some { Protocol.input_tokens = 21; output_tokens = 9 });
  assert (Openai_responses_wire.usage response = None);
  assert (Openai_responses_wire.usage (`Assoc [
    "usage", `Assoc ["input_tokens", `Int 21] ]) = None);
  assert (Openai_responses_wire.usage (`Assoc [
    "usage", `Assoc ["input_tokens", `Int 21;
      "output_tokens", `Int (-1)] ]) = None);
  assert (Openai_responses_wire.parse_completion (completed [
    item "reasoning" [ "status", `String "completed" ];
    message [ text "Checking" ]; function_call "call_A" "read_file" {|{"path":"alpha.txt"}|} ])
    = assistant (Some "Checking") [ use ]);
  invalid (fun () -> Openai_responses_wire.request ~model:"gpt-test"
    [ assistant None [ use ] ] []);
  invalid (fun () -> Openai_responses_wire.request ~model:"gpt-test"
    [ assistant None [ use ]; Protocol.tool_result "wrong" "result" ] []);
  invalid (fun () -> Openai_responses_wire.parse_completion
    (`Assoc [ "status", `String "incomplete"; "output", `List [ message [ text "partial" ] ] ]));
  invalid (fun () -> Openai_responses_wire.parse_completion
    (completed [ function_call "call_A" "read_file" "{broken" ]));
  invalid (fun () -> Openai_responses_wire.parse_completion
    (completed [ function_call "call_A" "read_file" "{}";
      function_call "call_A" "read_file" "{}" ]));
  invalid (fun () -> Openai_responses_wire.parse_completion
    (completed [ item "message" [ "role", `String "user";
      "content", `List [ text "bad role" ] ] ]));
  invalid (fun () -> Openai_responses_wire.parse_completion
    (completed [ message [ item "refusal" [ "refusal", `String "No" ] ] ]));
  print_endline "OpenAI Responses wire: ok"
