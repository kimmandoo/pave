let event data = "data: " ^ data ^ "\r\n\r\n"
let chunk parts finish =
  let candidate = [ "index", `Int 0 ] @
    (if parts = [] then [] else [ "content", `Assoc [ "role", `String "model";
      "parts", `List parts ] ]) @
    (if finish then [ "finishReason", `String "STOP" ] else []) in
  event (Yojson.Basic.to_string (`Assoc [ "candidates", `List [ `Assoc candidate ] ]))
let text value = `Assoc [ "text", `String value ]
let tool id name arguments = `Assoc [ "functionCall", `Assoc [
  "id", `String id; "name", `String name; "args", arguments ] ]
let invalid wire =
  let stream = Pave.Gemini_stream.create ~on_text:(fun _ -> ()) in
  match Pave.Gemini_stream.feed stream wire; Pave.Gemini_stream.finish stream with
  | exception Pave.Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid Gemini stream"

let () =
  let deltas = ref [] in
  let stream = Pave.Gemini_stream.create ~on_text:(fun fragment ->
    deltas := fragment :: !deltas) in
  let args = `Assoc [ "path", `String "日本語.txt" ] in
  let wire = ": keepalive\r\n\r\n" ^ chunk [ text "你好，" ] false ^
    chunk [ text "世界 🌍"; tool "fc-1" "read_file" args ] false ^
    chunk [ text "!" ] false ^ chunk [] true in
  String.iter (fun char -> Pave.Gemini_stream.feed stream (String.make 1 char)) wire;
  assert (Pave.Gemini_stream.is_done stream);
  assert (Pave.Gemini_stream.is_finished stream);
  let result = Pave.Gemini_stream.finish stream in
  assert (List.rev !deltas = [ "你好，"; "世界 🌍"; "!" ]);
  assert (result.content = Some "你好，世界 🌍!");
  assert (result.tool_calls = [ { Pave.Protocol.id = "fc-1";
    name = "read_file"; arguments = args } ]);
  let result_without_id = Pave.Gemini_stream.create ~on_text:(fun _ -> ()) in
  Pave.Gemini_stream.feed result_without_id
    (chunk [ `Assoc [ "functionCall", `Assoc [ "name", `String "read_file";
      "args", args ] ] ] true);
  (match (Pave.Gemini_stream.finish result_without_id).tool_calls with
   | [ invocation ] -> assert (invocation.id <> "" && invocation.arguments = args)
   | _ -> failwith "missing streamed tool call");
  let text_only = Pave.Gemini_stream.create ~on_text:(fun _ -> ()) in
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
  invalid (chunk [ `Assoc [ "thoughtSignature", `String "c2ln";
    "functionCall", `Assoc [ "name", `String "read_file"; "args", args ] ] ] true);
  invalid (chunk [ `Assoc [ "text", `String "signed";
    "thoughtSignature", `String "c2ln" ] ] false ^
    chunk [ tool "fc-2" "read_file" args ] true);
  invalid (chunk [ text "finished" ] true ^ chunk [ text "extra" ] true);
  invalid "data: {\"candidates\": [\r\n\r\n";
  invalid "data: {\"candidates\":[]}";
  print_endline "Gemini incremental SSE: ok"
