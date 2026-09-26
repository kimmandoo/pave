open Pave

let invalid f =
  match f () with
  | exception Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid streamed response"

let chunk ?(finish = `Null) delta =
  Yojson.Basic.to_string (`Assoc [
    "choices", `List [ `Assoc [
      "index", `Int 0; "delta", delta; "finish_reason", finish ] ] ])

let event data = "data: " ^ data ^ "\r\n\r\n"
let done_event = "data: [DONE]\r\n\r\n"
let text value = `Assoc [ "content", `String value ]
let call index ?id ?name ?arguments () =
  let function_fields =
    (match name with Some value -> [ "name", `String value ] | None -> []) @
    (match arguments with Some value -> [ "arguments", `String value ] | None -> []) in
  `Assoc ([ "index", `Int index ] @
    (match id with Some value -> [ "id", `String value ] | None -> []) @
    (if function_fields = [] then [] else [ "function", `Assoc function_fields ]))
let calls entries = `Assoc [ "tool_calls", `List entries ]
let stream ?(on_text = fun _ -> ()) wire =
  let parser = Openai_stream.create ~on_text in
  Openai_stream.feed parser wire;
  Openai_stream.finish parser

let () =
  let deltas = ref [] in
  let parser = Openai_stream.create ~on_text:(fun part -> deltas := part :: !deltas) in
  let wire = ": heartbeat\r\n\r\n"
    ^ event (chunk (text "Hel"))
    ^ "data: {\"choices\": [\r\ndata: {\"index\": 0, \"delta\": {\"content\": \"lo\"}, \"finish_reason\": null}]}\r\n\r\n"
    ^ event (chunk ~finish:(`String "stop") (`Assoc []))
    ^ done_event in
  (* Every possible CRLF and JSON boundary is crossed by a separate feed. *)
  String.iter (fun c -> Openai_stream.feed parser (String.make 1 c)) wire;
  assert (List.rev !deltas = [ "Hel"; "lo" ]);
  assert ((Openai_stream.finish parser).content = Some "Hello");
  let counted = Openai_stream.create ~on_text:(fun _ -> ()) in
  let usage = `Assoc [
    "prompt_tokens", `Int 19; "completion_tokens", `Int 7;
    "prompt_tokens_details", `Assoc ["cached_tokens", `Int 5];
    "completion_tokens_details", `Assoc ["reasoning_tokens", `Int 2] ] in
  let usage_only = event (Yojson.Basic.to_string (`Assoc [
    "choices", `List []; "usage", usage ])) in
  Openai_stream.feed counted
    (usage_only ^ event (chunk (text "metered")) ^
     event (chunk ~finish:(`String "stop") (`Assoc [])) ^
     usage_only ^ done_event);
  ignore (Openai_stream.finish counted);
  assert (Openai_stream.usage counted =
    Some { Protocol.input_tokens = 19; output_tokens = 7;
      cached_input_tokens = Some 5; cache_creation_input_tokens = None;
      reasoning_output_tokens = Some 2 });
  let gated = Openai_stream.create ~on_text:(fun _ -> ()) in
  Openai_stream.feed gated
    (event (chunk (text "metered")) ^
     event (chunk ~finish:(`String "stop") (`Assoc [])) ^ usage_only);
  assert (Openai_stream.usage gated = None);
  Openai_stream.feed gated done_event;
  assert (Openai_stream.usage gated =
    Some { Protocol.input_tokens = 19; output_tokens = 7;
      cached_input_tokens = Some 5; cache_creation_input_tokens = None;
      reasoning_output_tokens = Some 2 });
  ignore (Openai_stream.finish gated);
  invalid (fun () -> stream
    (event (chunk (text "metered")) ^
     event (chunk ~finish:(`String "stop") (`Assoc [])) ^
     usage_only ^ usage_only ^ done_event));
  let incomplete = Openai_stream.create ~on_text:(fun _ -> ()) in
  Openai_stream.feed incomplete
    (usage_only ^ event (chunk (text "no final usage")) ^ done_event);
  invalid (fun () -> Openai_stream.finish incomplete);
  let wire =
    event (chunk (calls [
      call 1 ~id:"call-" ~name:"wri" ~arguments:"{\"value\":" ();
      call 0 ~id:"first" ~name:"read" ~arguments:"{\"path\":" () ]))
    ^ event (chunk (calls [
      call 0 ~arguments:"\"a.txt\"}" ();
      call 1 ~id:"second" ~name:"te" ~arguments:"42}" () ]))
    ^ event (chunk ~finish:(`String "tool_calls") (`Assoc []))
    ^ done_event in
  let response = stream wire in
  assert (response.content = None);
  assert (response.tool_calls = [
    { Protocol.id = "first"; name = "read";
      arguments = `Assoc [ "path", `String "a.txt" ] };
    { Protocol.id = "call-second"; name = "write";
      arguments = `Assoc [ "value", `Int 42 ] } ]);
  invalid (fun () -> stream (event (chunk (text "partial"))));
  assert ((stream (event (chunk ~finish:(`String "stop") (text "complete"))
    ^ done_event)).content = Some "complete");
  invalid (fun () -> stream
    (event (chunk ~finish:(`String "stop") (text "truncated"))));
  invalid (fun () -> stream (event (chunk (text "done only")) ^ done_event));
  invalid (fun () -> stream
    (event (chunk (calls [ call 0 ~id:"call" ~name:"read" ~arguments:"{}" () ]))
     ^ event (chunk ~finish:(`String "stop") (`Assoc []))));
  invalid (fun () -> stream (event (chunk ~finish:(`String "length") (text "short")) ^ done_event));
  invalid (fun () -> stream
    (event "{\"error\":{\"message\":\"rate limited\"}}" ^ done_event));
  invalid (fun () -> stream ("event: error\ndata: {\"message\":\"rate limited\"}\n\n"));
  invalid (fun () -> stream
    (event (chunk (calls [ call 0 ~id:"duplicate" ~name:"a" ~arguments:"{}" ();
                           call 1 ~id:"duplicate" ~name:"b" ~arguments:"{}" () ]))
     ^ event (chunk ~finish:(`String "tool_calls") (`Assoc [])) ^ done_event));
  invalid (fun () -> stream
    (event (chunk (calls [ call 0 ~id:"call" ~name:"read" ~arguments:"[]" () ]))
     ^ event (chunk ~finish:(`String "tool_calls") (`Assoc [])) ^ done_event));
  invalid (fun () -> stream
    (event (chunk (`Assoc ["refusal", `String "No"])) ^
     event (chunk ~finish:(`String "stop") (`Assoc [])) ^ done_event));
  invalid (fun () -> stream ("data: " ^ String.make 1_048_577 'a'));
  let poisoned = Openai_stream.create ~on_text:(fun _ -> ()) in
  Openai_stream.feed poisoned
    (event (chunk ~finish:(`String "stop") (text "complete")) ^ done_event);
  invalid (fun () -> Openai_stream.feed poisoned (event (chunk (text "after terminal"))));
  invalid (fun () -> Openai_stream.finish poisoned);
  assert (Openai_stream.usage poisoned = None);
  print_endline "OpenAI incremental stream: ok"
