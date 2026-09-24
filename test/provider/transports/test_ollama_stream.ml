let frame text = text ^ "\n"
let chunk content =
  frame (Yojson.Basic.to_string (`Assoc [ "message", `Assoc [
    "role", `String "assistant"; "content", `String content ]; "done", `Bool false ]))
let tool ?index name arguments =
  `Assoc [ "type", `String "function";
    "function", `Assoc ([ "name", `String name; "arguments", arguments ] @
      match index with None -> [] | Some n -> [ "index", `Int n ]) ]
let call_chunk calls =
  frame (Yojson.Basic.to_string (`Assoc [ "message", `Assoc [
    "role", `String "assistant"; "content", `String "";
    "tool_calls", `List calls ]; "done", `Bool false ]))
let done_frame ?(reason="stop") ?(counts=[]) () =
  frame (Yojson.Basic.to_string (`Assoc ([ "message", `Assoc [
    "role", `String "assistant"; "content", `String "" ];
    "done_reason", `String reason; "done", `Bool true ] @ counts)))
let parse wire =
  let t = Pave.Ollama_stream.create ~on_text:(fun _ -> ()) in
  Pave.Ollama_stream.feed t wire;
  Pave.Ollama_stream.finish t
let expect_invalid wire = match parse wire with
  | exception Pave.Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid Ollama NDJSON"

let () =
  let text = ref [] in
  let t = Pave.Ollama_stream.create ~on_text:(fun fragment -> text := fragment :: !text) in
  let args = `Assoc [ "query", `String "東京" ] in
  let wire = chunk "你" ^ chunk "好、東京" ^
    call_chunk [ tool "search" args ] ^ done_frame () in
  String.iter (fun c -> Pave.Ollama_stream.feed t (String.make 1 c)) wire;
  assert (Pave.Ollama_stream.is_done t && Pave.Ollama_stream.is_finished t);
  let result = Pave.Ollama_stream.finish t in
  assert (List.rev !text = [ "你"; "好、東京" ]);
  assert (result.content = Some "你好、東京");
  assert (result.tool_calls = [ { Pave.Protocol.id = "ollama:0:search";
    name = "search"; arguments = args } ]);
  assert (Pave.Ollama_stream.usage t = None);
  let measured = Pave.Ollama_stream.create ~on_text:(fun _ -> ()) in
  Pave.Ollama_stream.feed measured (chunk "ok" ^
    done_frame ~counts:["prompt_eval_count", `Int 18;
      "eval_count", `Int 7] ());
  ignore (Pave.Ollama_stream.finish measured);
  assert (Pave.Ollama_stream.usage measured =
    Some { Pave.Protocol.input_tokens = 18; output_tokens = 7 });
  let fragments =
    call_chunk [ tool ~index:0 "search" (`String {|{"query":"東|}) ] ^
    call_chunk [ tool ~index:0 "search" (`String {|京"}|}) ] ^
    done_frame ~reason:"tool_calls" () in
  let partial = parse fragments in
  assert (partial.tool_calls = [ { Pave.Protocol.id = "ollama:0:search";
    name = "search"; arguments = args } ]);
  let two = call_chunk [ tool "search" args; tool "read" (`Assoc []) ] ^
    done_frame () in
  let result = parse two in
  assert (List.map (fun (call : Pave.Protocol.tool_call) -> call.id) result.tool_calls =
    [ "ollama:0:search"; "ollama:1:read" ]);
  let no_newline = chunk "完成" ^ String.trim (done_frame ()) in
  assert ((parse no_newline).content = Some "完成");
  let t = Pave.Ollama_stream.create ~on_text:(fun _ -> ()) in
  Pave.Ollama_stream.feed t (chunk "hello");
  assert (not (Pave.Ollama_stream.is_done t));
  expect_invalid (chunk "partial");
  expect_invalid (chunk "partial" ^ "{\"done\":tr");
  expect_invalid (frame {|{"error":"model not found"}|});
  expect_invalid (frame "not-json");
  expect_invalid (chunk "partial" ^ done_frame ~reason:"length" ());
  expect_invalid (chunk "partial" ^ done_frame ~reason:"load" ());
  expect_invalid (done_frame ());
  expect_invalid (done_frame () ^ chunk "too late");
  expect_invalid (call_chunk [ tool ~index:0 "search" (`String "{broken") ] ^ done_frame ());
  expect_invalid (call_chunk [ tool "search" (`String "{broken") ] ^ done_frame ());
  expect_invalid (call_chunk [ tool ~index:0 "search" args ] ^
    call_chunk [ tool ~index:0 "search" args ] ^ done_frame ());
  print_endline "Ollama native stream: ok"
