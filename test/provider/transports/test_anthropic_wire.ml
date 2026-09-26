let field name json = Pave.Protocol.member name json

let expect_invalid f =
  match f () with
  | exception Pave.Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid Anthropic response"

let call id name arguments : Pave.Protocol.tool_call = { id; name; arguments }
let assistant content calls : Pave.Protocol.message =
  { role = "assistant"; content; tool_calls = calls; tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] }
let system text : Pave.Protocol.message =
  { role = "system"; content = Some text; tool_calls = [];
    tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] }
let block kind fields = `Assoc (("type", `String kind) :: fields)
let message role blocks = `Assoc [ "role", `String role; "content", `List blocks ]
let response stop blocks =
  `Assoc [ "stop_reason", `String stop; "content", `List blocks ]

let () =
  let open Pave.Protocol in
  let first = call "use-1" "read_file" (`Assoc [ "path", `String "alpha" ]) in
  let second = call "use-2" "read_file" (`Assoc [ "path", `String "beta" ]) in
  let schema = `Assoc [ "type", `String "object";
                        "properties", `Assoc [ "path", `Assoc [ "type", `String "string" ] ];
                        "required", `List [ `String "path" ] ] in
  let definition = `Assoc [ "type", `String "function";
                            "function", `Assoc [ "name", `String "read_file";
                              "description", `String "Read a file";
                              "parameters", schema ] ] in
  let transcript = [ system "Follow instructions"; user "Inspect both files";
    assistant (Some "Checking.") [ first; second ];
    tool_result second.id "beta body"; tool_result first.id "alpha body";
    assistant (Some "Done") [] ] in
  let wire = Pave.Anthropic_wire.request ~model:"claude-test" ~max_tokens:4096
    transcript [ definition ] in
  assert (field "system" wire = `String "Follow instructions");
  assert (field "model" wire = `String "claude-test");
  assert (field "max_tokens" wire = `Int 4096);
  assert (field "tools" wire = `List [ `Assoc [ "name", `String "read_file";
    "input_schema", schema; "description", `String "Read a file" ] ]);
  assert (field "messages" wire = `List [
    `Assoc [ "role", `String "user"; "content", `String "Inspect both files" ];
    message "assistant" [ block "text" [ "text", `String "Checking." ];
      block "tool_use" [ "id", `String "use-1"; "name", `String "read_file";
        "input", first.arguments ];
      block "tool_use" [ "id", `String "use-2"; "name", `String "read_file";
        "input", second.arguments ] ];
    message "user" [ block "tool_result" [ "tool_use_id", `String "use-2";
      "content", `String "beta body" ];
      block "tool_result" [ "tool_use_id", `String "use-1";
        "content", `String "alpha body" ] ];
    message "assistant" [ block "text" [ "text", `String "Done" ] ] ]);
  let png = "iVBORw0KGgo=" in
  let typed_result id blocks = tool_result_blocks id blocks in
  let image_request = Pave.Anthropic_wire.request ~model:"claude-test" ~max_tokens:4096
    [assistant None [first; second];
     typed_result second.id [Text "before"; Image { mime_type = "image/png"; data = png };
       Text "after"];
     typed_result first.id [Image { mime_type = "image/jpeg"; data = "/9j/2Q==" }]] [] in
  let expected_images = `List [
    message "assistant" [
      block "tool_use" ["id", `String first.id; "name", `String first.name;
        "input", first.arguments];
      block "tool_use" ["id", `String second.id; "name", `String second.name;
        "input", second.arguments]];
    message "user" [
      block "tool_result" [
        "tool_use_id", `String second.id;
        "content", `List [
          block "text" ["text", `String "before"];
          block "image" ["source", `Assoc ["type", `String "base64";
            "media_type", `String "image/png"; "data", `String png]];
          block "text" ["text", `String "after"]]];
      block "tool_result" [
        "tool_use_id", `String first.id;
        "content", `List [
          block "text" ["text", `String "(see attached image)"];
          block "image" ["source", `Assoc ["type", `String "base64";
            "media_type", `String "image/jpeg"; "data", `String "/9j/2Q=="]]]]]]
  in
  assert (field "messages" image_request = expected_images);
  expect_invalid (fun () -> Pave.Anthropic_wire.request ~model:"claude-test"
    ~max_tokens:4096 [assistant None [first];
      typed_result first.id [Image { mime_type = "image/bmp"; data = "AA==" }]] []);
  expect_invalid (fun () -> Pave.Anthropic_wire.request ~model:"claude-test"
    ~max_tokens:4096 [ assistant None [ first; second ]; tool_result first.id "ok" ] []);
  expect_invalid (fun () -> Pave.Anthropic_wire.request ~model:"claude-test"
    ~max_tokens:4096 [ assistant None [ first ]; tool_result first.id "ok";
      tool_result first.id "again" ] []);
  let attached = { (user "describe") with attachments = [
    { name = "ignored"; mime_type = "image/png"; data = png } ] } in
  assert (field "messages" (Pave.Anthropic_wire.request ~model:"claude-test"
    ~max_tokens:1024 [attached] []) = `List [
      message "user" [block "text" ["text", `String "describe"]; block "image" [
        "source", `Assoc ["type", `String "base64";
          "media_type", `String "image/png"; "data", `String png]]]]);
  let text s = block "text" [ "text", `String s ] in
  let use id name input = block "tool_use" [ "id", `String id;
    "name", `String name; "input", input ] in
  let answer = Pave.Anthropic_wire.parse_response
    (response "end_turn" [ text "Hello "; text "world" ]) in
  assert (answer = assistant (Some "Hello world") []);
  let counted = `Assoc [
    "usage", `Assoc [
      "input_tokens", `Int 2; "cache_creation_input_tokens", `Int 3;
      "cache_read_input_tokens", `Int 4; "output_tokens", `Int 7 ] ] in
  assert (Pave.Anthropic_wire.usage counted =
    Some { input_tokens = 9; output_tokens = 7 });
  assert (Pave.Anthropic_wire.usage (response "end_turn" [text "Hello"]) = None);
  assert (Pave.Anthropic_wire.usage (`Assoc [
    "usage", `Assoc ["input_tokens", `Int 2; "output_tokens", `Int 7;
      "cache_read_input_tokens", `Int (-1)] ]) = None);
  assert (Pave.Anthropic_wire.usage (`Assoc [
    "usage", `Assoc ["input_tokens", `Int 2] ]) = None);
  let reply = Pave.Anthropic_wire.parse_response
    (response "tool_use" [ text "Checking.";
      use first.id first.name first.arguments; use second.id second.name second.arguments ]) in
  assert (reply = assistant (Some "Checking.") [ first; second ]);
  expect_invalid (fun () -> Pave.Anthropic_wire.parse_response
    (response "max_tokens" [ text "Partial" ]));
  expect_invalid (fun () -> Pave.Anthropic_wire.parse_response
    (response "refusal" [ text "Cannot comply" ]));
  expect_invalid (fun () -> Pave.Anthropic_wire.parse_response
    (response "end_turn" [ use first.id first.name first.arguments ]));
  expect_invalid (fun () -> Pave.Anthropic_wire.parse_response
    (response "tool_use" [ text "No call" ]));
  expect_invalid (fun () -> Pave.Anthropic_wire.parse_response
    (response "tool_use" [ use first.id first.name first.arguments;
      use first.id first.name first.arguments ]));
  expect_invalid (fun () -> Pave.Anthropic_wire.parse_response
    (response "tool_use" [ use "bad" "read_file" (`String "not JSON object") ]));
  expect_invalid (fun () -> Pave.Anthropic_wire.parse_response
    (`Assoc [ "stop_reason", `String "end_turn"; "content", `String "wrong shape" ]));
  print_endline "anthropic wire: ok"
