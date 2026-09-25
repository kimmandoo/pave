let event data = "data: " ^ data ^ "\r\n\r\n"
let chunk parts finish =
  let candidate = [ "index", `Int 0 ] @
    (if parts = [] then [] else [ "content", `Assoc [ "role", `String "model";
      "parts", `List parts ] ]) @
    (if finish then [ "finishReason", `String "STOP" ] else []) in
  event (Yojson.Basic.to_string (`Assoc [ "candidates", `List [ `Assoc candidate ] ]))
let text value = `Assoc [ "text", `String value ]
let tool id name arguments = `Assoc [ "functionCall", `Assoc [
  "id", `String id; "name", `String name; "args", arguments ];
  "thoughtSignature", `String "c2ln" ]
let invalid ?(model="gemini-2.5-flash") wire =
  let stream = Pave.Gemini_stream.create ~model ~on_text:(fun _ -> ()) in
  match Pave.Gemini_stream.feed stream wire; Pave.Gemini_stream.finish stream with
  | exception Pave.Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid Gemini stream"

let () =
  let deltas = ref [] in
  let stream = Pave.Gemini_stream.create ~model:"gemini-2.5-flash" ~on_text:(fun fragment ->
    deltas := fragment :: !deltas) in
  let args = `Assoc [ "path", `String "日本語.txt" ] in
  let wire = ": keepalive\r\n\r\n" ^ chunk [ text "你好，" ] false ^
    chunk [ text "世界 🌍"; tool "fc-1" "read_file" args ] false ^
    chunk [ text "!" ] false ^ chunk [] true in
  String.iter (fun char -> Pave.Gemini_stream.feed stream (String.make 1 char)) wire;
  assert (Pave.Gemini_stream.is_finished stream);
  let result = Pave.Gemini_stream.finish stream in
  assert (List.rev !deltas = [ "你好，"; "世界 🌍"; "!" ]);
  assert (result.content = Some "你好，世界 🌍!");
  assert (result.tool_calls = [ { Pave.Protocol.id = "fc-1";
    name = "read_file"; arguments = args } ]);
  assert (Pave.Gemini_stream.usage stream = None);
  let reported = `Assoc [
    "promptTokenCount", `Int 12; "candidatesTokenCount", `Int 5;
    "thoughtsTokenCount", `Int 3 ] in
  let final_with_usage = event (Yojson.Basic.to_string (`Assoc [
    "candidates", `List [ `Assoc [
      "finishReason", `String "STOP";
      "content", `Assoc ["parts", `List [text "metered"]] ] ];
    "usageMetadata", reported ])) in
  let measured = Pave.Gemini_stream.create ~model:"gemini-2.5-flash"
    ~on_text:(fun _ -> ()) in
  Pave.Gemini_stream.feed measured final_with_usage;
  ignore (Pave.Gemini_stream.finish measured);
  assert (Pave.Gemini_stream.is_done measured);
  assert (Pave.Gemini_stream.usage measured =
    Some { Pave.Protocol.input_tokens = 12; output_tokens = 8 });
  let trailing = Pave.Gemini_stream.create ~model:"gemini-2.5-flash"
    ~on_text:(fun _ -> ()) in
  Pave.Gemini_stream.feed trailing
    (chunk [text "metered"] true ^
     event (Yojson.Basic.to_string (`Assoc [
       "usageMetadata", reported ])));
  ignore (Pave.Gemini_stream.finish trailing);
  assert (Pave.Gemini_stream.usage trailing =
    Some { Pave.Protocol.input_tokens = 12; output_tokens = 8 });
  let result_without_id = Pave.Gemini_stream.create ~model:"gemini-2.5-flash" ~on_text:(fun _ -> ()) in
  Pave.Gemini_stream.feed result_without_id
    (chunk [ `Assoc [ "functionCall", `Assoc [ "name", `String "read_file";
      "args", args ]; "thoughtSignature", `String "c2ln" ] ] true);
  (match (Pave.Gemini_stream.finish result_without_id).tool_calls with
   | [ invocation ] -> assert (invocation.id <> "" && invocation.arguments = args)
   | _ -> failwith "missing streamed tool call");
  let text_only = Pave.Gemini_stream.create ~model:"gemini-2.5-flash" ~on_text:(fun _ -> ()) in
  Pave.Gemini_stream.feed text_only (chunk [ text "éclair" ] true);
  assert ((Pave.Gemini_stream.finish text_only).content = Some "éclair");
  invalid (chunk [ text "partial" ] false);
  invalid (chunk [] true);
  invalid (chunk [ text "partial" ] false ^
    event {|{"candidates":[{"finishReason":"MAX_TOKENS"}]}|});
  invalid (event {|{"error":{"message":"quota exhausted"}}|});
  invalid (event {|{"promptFeedback":{"blockReason":"SAFETY"},"candidates":[]}|});
  invalid (chunk [ tool "fc-1" "read_file" args ] false ^
    chunk [ tool "fc-1" "read_file" args ] true);
  let signed_part = `Assoc [ "thoughtSignature", `String "c2ln";
    "functionCall", `Assoc [ "name", `String "read_file"; "args", args ] ] in
  let signed_stream = Pave.Gemini_stream.create ~model:"gemini-3-pro"
    ~on_text:(fun _ -> ()) in
  Pave.Gemini_stream.feed signed_stream
    (chunk [ text "Reading " ] false ^ chunk [ signed_part ] true);
  let signed = Pave.Gemini_stream.finish signed_stream in
  assert (signed.content = Some "Reading ");
  (match signed.tool_calls with
   | [ invocation ] ->
       let request = Pave.Gemini_wire.request ~model:"gemini-3-pro"
         [ Pave.Protocol.user "Read"; signed;
           Pave.Protocol.tool_result invocation.id "contents" ] [] in
       assert (Pave.Protocol.member "contents" request = `List [
         `Assoc [ "role", `String "user"; "parts", `List [ text "Read" ] ];
         `Assoc [ "role", `String "model";
           "parts", `List [ text "Reading "; signed_part ] ];
         `Assoc [ "role", `String "user"; "parts", `List [
           `Assoc [ "functionResponse", `Assoc [
             "name", `String "read_file";
             "response", `Assoc [ "output", `String "contents" ] ] ] ] ] ])
   | _ -> failwith "signed stream lost call");
  invalid (chunk [ `Assoc [ "functionCall", `Assoc [
    "id", `String "fc-3"; "name", `String "read_file"; "args", args ] ] ] true);
  invalid ~model:"gemini-3-pro" (chunk [ signed_part ] false ^
    chunk [ signed_part ] true);
  invalid ~model:"gemini-3-pro"
    (chunk [ `Assoc [ "text", `String "Hello";
      "thoughtSignature", `String "c2ln" ] ] false ^
     chunk [ `Assoc [ "text", `String "Hello again";
       "thoughtSignature", `String "c2ln" ] ] true);
  invalid (chunk [ text "finished" ] true ^ chunk [ text "extra" ] true);
  invalid "data: {\"candidates\": [\r\n\r\n";
  invalid "data: {\"candidates\":[]}";
  let oversized = Pave.Gemini_stream.create ~model:"gemini-3-pro"
    ~on_text:(fun _ -> ()) in
  let repeated = chunk [ `Assoc [ "thought", `Bool true;
    "text", `String (String.make 850_000 'x') ] ] false in
  let bounded = try
    for _ = 1 to 22 do Pave.Gemini_stream.feed oversized repeated done;
    false
  with Pave.Protocol.Invalid_response _ -> true in
  if not bounded then failwith "native Gemini thought state exceeded the response bound";
  print_endline "Gemini incremental SSE: ok"
