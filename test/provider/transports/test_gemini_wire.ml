let field = Pave.Protocol.member
let expect_invalid f = match f () with
  | exception Pave.Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid Gemini response"
let call id name arguments : Pave.Protocol.tool_call = { id; name; arguments }
let assistant content tool_calls : Pave.Protocol.message =
  { role = "assistant"; content; tool_calls; tool_call_id = None;
    provider_state = None }
let system text : Pave.Protocol.message =
  { role = "system"; content = Some text; tool_calls = [];
    tool_call_id = None; provider_state = None }
let item role parts = `Assoc [ "role", `String role; "parts", `List parts ]
let text value = `Assoc [ "text", `String value ]
let fn name args = `Assoc [ "functionCall", `Assoc [ "name", `String name; "args", args ] ]
let response parts reason = `Assoc [ "candidates", `List [ `Assoc [
  "content", item "model" parts; "finishReason", `String reason ] ] ]

let () =
  let open Pave.Protocol in
  let first = call "invoke-1" "read_file" (`Assoc [ "path", `String "日本語.txt" ]) in
  let second = call "invoke-2" "read_file" (`Assoc [ "path", `String "café.txt" ]) in
  let schema = `Assoc [ "type", `String "object";
    "properties", `Assoc [
      "path", `Assoc [ "type", `String "string" ];
      "limit", `Assoc [ "type", `String "integer";
        "description", `String "Result limit"; "minimum", `Int 1; "maximum", `Int 10 ] ];
    "required", `List [ `String "path" ]; "additionalProperties", `Bool false ] in
  let normalized_schema = `Assoc [ "type", `String "object";
    "properties", `Assoc [
      "path", `Assoc [ "type", `String "string" ];
      "limit", `Assoc [ "description", `String "minimum: 1; maximum: 10; Result limit";
        "type", `String "integer" ] ];
    "required", `List [ `String "path" ] ] in
  let tool = `Assoc [ "type", `String "function"; "function", `Assoc [
    "name", `String "read_file"; "description", `String "Read UTF-8 file";
    "parameters", schema ] ] in
  let transcript = [ system "Respond concisely"; user "Read two files";
    assistant (Some "Reading…") [ first; second ]; tool_result second.id "café content";
    tool_result first.id "日本語 content"; assistant (Some "All done") [] ] in
  let request = Pave.Gemini_wire.request ~model:"gemini-2.5-flash" transcript [ tool ] in
  assert (field "model" request = `Null);
  assert (field "systemInstruction" request = `Assoc [ "parts", `List [ text "Respond concisely" ] ]);
  assert (field "tools" request = `List [ `Assoc [ "functionDeclarations", `List [
    `Assoc [ "name", `String "read_file"; "description", `String "Read UTF-8 file";
      "parametersJsonSchema", normalized_schema ] ] ] ]);
  assert (field "contents" request = `List [
    item "user" [ text "Read two files" ];
    item "model" [ text "Reading…"; fn "read_file" first.arguments;
      fn "read_file" second.arguments ];
    item "user" [
      `Assoc [ "functionResponse", `Assoc [ "name", `String "read_file";
        "response", `Assoc [ "output", `String "日本語 content" ] ] ];
      `Assoc [ "functionResponse", `Assoc [ "name", `String "read_file";
        "response", `Assoc [ "output", `String "café content" ] ] ] ];
    item "model" [ text "All done" ] ]);
  expect_invalid (fun () -> Pave.Gemini_wire.request ~model:"gemini-3-pro"
    [ user "Inspect"; assistant None [ first ]; tool_result first.id "ok" ] []);
  assert (field "contents" (Pave.Gemini_wire.request ~model:"gemini-3-pro"
    [ user "Text only" ] []) = `List [ item "user" [ text "Text only" ] ]);
  expect_invalid (fun () -> Pave.Gemini_wire.request ~model:"gemini-2.5-flash"
    [ assistant None [ first; second ]; tool_result first.id "only one" ] []);
  expect_invalid (fun () -> Pave.Gemini_wire.request ~model:"gemini-2.5-flash"
    [ assistant None [ first ]; tool_result first.id "ok"; tool_result first.id "twice" ] []);
  expect_invalid (fun () -> Pave.Gemini_wire.request ~model:"gemini-2.5-flash"
    [ user "hello" ] [ `Assoc [ "type", `String "function";
      "function", `Assoc [ "name", `String "bad";
        "parameters", `Assoc [ "type", `String "string" ] ] ] ]);
  let reply = Pave.Gemini_wire.parse_completion ~model:"gemini-2.5-flash"
    (response [ text "こんにちは "; text "世界 🌍";
      `Assoc [ "functionCall", `Assoc [ "id", `String first.id;
        "name", `String first.name; "args", first.arguments ] ] ] "STOP") in
  assert (reply = assistant (Some "こんにちは 世界 🌍") [ first ]);
  let generated = Pave.Gemini_wire.parse_completion ~model:"gemini-2.5-flash"
    (response [ fn "read_file" second.arguments ] "STOP") in
  (match generated.tool_calls with
   | [ invocation ] ->
       assert (invocation.id <> "");
       assert (invocation.name = "read_file");
       assert (invocation.arguments = second.arguments)
   | _ -> failwith "missing generated call");
  (match generated.tool_calls with
   | [ invocation ] ->
       let followup = Pave.Gemini_wire.request ~model:"gemini-2.5-flash"
         [ user "Read"; generated; tool_result invocation.id "read result" ] [] in
       (match field "contents" followup with
        | `List [ _; _; tool_turn ] ->
            assert (field "parts" tool_turn = `List [ `Assoc [
              "functionResponse", `Assoc [ "name", `String "read_file";
                "response", `Assoc [ "output", `String "read result" ] ] ] ])
        | _ -> failwith "missing recovered tool response")
   | _ -> failwith "missing generated call");
  let signed_part = `Assoc [ "functionCall", `Assoc [
    "name", `String "read_file"; "args", first.arguments ];
    "thoughtSignature", `String "c2ln" ] in
  let native_parts = [ text "Opening file"; signed_part ] in
  let signed = Pave.Gemini_wire.parse_completion ~model:"gemini-3-pro"
    (response native_parts "STOP") in
  (match signed.tool_calls with
   | [ invocation ] ->
       let followup = Pave.Gemini_wire.request ~model:"gemini-3-pro"
         [ user "Read"; signed; tool_result invocation.id "file contents" ] [ tool ] in
       assert (field "contents" followup = `List [
         item "user" [ text "Read" ]; item "model" native_parts;
         item "user" [ `Assoc [ "functionResponse", `Assoc [
           "name", `String "read_file";
           "response", `Assoc [ "output", `String "file contents" ] ] ] ] ]);
       assert (field "tools" followup <> `Null);
       let replay altered = Pave.Gemini_wire.request ~model:"gemini-3-pro"
         [ user "Read"; altered; tool_result invocation.id "file contents" ] [ tool ] in
       expect_invalid (fun () -> replay { signed with content = Some "Different text" });
       expect_invalid (fun () -> replay { signed with tool_calls =
         [ { invocation with name = "other" } ] });
       expect_invalid (fun () -> replay { signed with tool_calls =
         [ { invocation with arguments = `Assoc [] } ] });
       expect_invalid (fun () -> Pave.Gemini_wire.request ~model:"gemini-3-flash"
         [ user "Read"; signed; tool_result invocation.id "file contents" ] [ tool ]);
       expect_invalid (fun () -> replay { signed with provider_state = Some (`Assoc [
         "provider", `String "other"; "model", `String "gemini-3-pro";
         "parts", `List native_parts ]) });
       expect_invalid (fun () -> replay { signed with provider_state = Some (`Assoc [
         "provider", `String "google"; "model", `String "gemini-3-pro";
         "parts", `List [ `Assoc [ "functionCall", `Assoc [
           "name", `String "read_file"; "args", first.arguments ];
           "thoughtSignature", `String "c2ln"; "inlineData", `Assoc [] ] ] ]) });
       expect_invalid (fun () -> replay { signed with provider_state = None })
   | _ -> failwith "signed call was not parsed");
  let parallel_parts = [ signed_part; fn "read_file" second.arguments ] in
  let parallel = Pave.Gemini_wire.parse_completion ~model:"gemini-3-pro"
    (response parallel_parts "STOP") in
  (match parallel.tool_calls with
   | [ left; right ] ->
       let followup = Pave.Gemini_wire.request ~model:"gemini-3-pro"
         [ user "Read both"; parallel; tool_result right.id "second";
           tool_result left.id "first" ] [ tool ] in
       (match field "contents" followup with
        | `List [ _; model_turn; result_turn ] ->
            assert (field "parts" model_turn = `List parallel_parts);
            assert (field "parts" result_turn = `List [
              `Assoc [ "functionResponse", `Assoc [
                "name", `String "read_file";
                "response", `Assoc [ "output", `String "first" ] ] ];
              `Assoc [ "functionResponse", `Assoc [
                "name", `String "read_file";
                "response", `Assoc [ "output", `String "second" ] ] ] ]);
        | _ -> failwith "missing parallel tool results");
       expect_invalid (fun () -> Pave.Gemini_wire.request ~model:"gemini-3-pro"
         [ user "Read both"; { parallel with tool_calls = [ right; left ] };
           tool_result right.id "second"; tool_result left.id "first" ] [ tool ])
   | _ -> failwith "missing parallel calls");
  let explicit_part = `Assoc [ "functionCall", `Assoc [
    "id", `String "native-id"; "name", `String "read_file"; "args", first.arguments ];
    "thoughtSignature", `String "c2ln" ] in
  let explicit = Pave.Gemini_wire.parse_completion ~model:"gemini-3-pro"
    (response [ explicit_part ] "STOP") in
  (match explicit.tool_calls with
   | [ invocation ] ->
       expect_invalid (fun () -> Pave.Gemini_wire.request ~model:"gemini-3-pro"
         [ user "Read"; { explicit with tool_calls =
           [ { invocation with id = "changed-id" } ] };
           tool_result "changed-id" "contents" ] [ tool ])
   | _ -> failwith "missing native call id");
  expect_invalid (fun () -> Pave.Gemini_wire.parse_completion ~model:"gemini-3-pro"
    (response [ fn "read_file" first.arguments ] "STOP"));
  expect_invalid (fun () -> Pave.Gemini_wire.parse_completion ~model:"gemini-2.5-flash"
    (response [] "STOP"));
  expect_invalid (fun () -> Pave.Gemini_wire.parse_completion ~model:"gemini-2.5-flash"
    (response [ text "partial" ] "MAX_TOKENS"));
  expect_invalid (fun () -> Pave.Gemini_wire.parse_completion ~model:"gemini-2.5-flash"
    (`Assoc [ "error", `Assoc [ "message", `String "denied" ] ]));
  expect_invalid (fun () -> Pave.Gemini_wire.parse_completion ~model:"gemini-2.5-flash"
    (`Assoc [ "promptFeedback", `Assoc [ "blockReason", `String "SAFETY" ];
      "candidates", `List [] ]));
  expect_invalid (fun () -> Pave.Gemini_wire.parse_completion ~model:"gemini-2.5-flash"
    (response [ `Assoc [ "inlineData", `Assoc [] ] ] "STOP"));
  print_endline "Gemini request/response wire: ok"
