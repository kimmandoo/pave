let event kind data = "event: " ^ kind ^ "\r\ndata: " ^ data ^ "\r\n\r\n"
let start = event "message_start"
  {|{"type":"message_start","message":{"id":"msg_1","role":"assistant","usage":{"input_tokens":2}}}|}
let text_start = event "content_block_start"
  {|{"type":"content_block_start","index":0,"content_block":{"type":"text","text":"Hi "}}|}
let text_delta = event "content_block_delta"
  {|{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"there"}}|}
let text_stop = event "content_block_stop" {|{"type":"content_block_stop","index":0}|}
let tool_start = event "content_block_start"
  {|{"type":"content_block_start","index":1,"content_block":{"type":"tool_use","id":"toolu_1","name":"read_file","input":{}}}|}
let tool_delta = event "content_block_delta"
  {|{"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"{\"path\":\"App.swift\"}"}}|}
let tool_stop = event "content_block_stop" {|{"type":"content_block_stop","index":1}|}
let ending reason = event "message_delta"
  (Yojson.Basic.to_string (`Assoc [ "type", `String "message_delta";
    "delta", `Assoc [ "stop_reason", `String reason ] ]))
let stop = event "message_stop" {|{"type":"message_stop"}|}

let invalid wire =
  let parser = Pave.Anthropic_stream.create ~on_text:(fun _ -> ()) in
  match Pave.Anthropic_stream.feed parser wire; Pave.Anthropic_stream.finish parser with
  | exception Pave.Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid Anthropic stream"

let () =
  let deltas = ref [] in
  let parser = Pave.Anthropic_stream.create ~on_text:(fun delta -> deltas := delta :: !deltas) in
  let wire = ": heartbeat\r\n\r\n" ^ start ^ text_start ^ text_delta ^ text_stop ^
    tool_start ^ tool_delta ^ tool_stop ^ ending "tool_use" ^ stop in
  String.iter (fun byte -> Pave.Anthropic_stream.feed parser (String.make 1 byte)) wire;
  let message = Pave.Anthropic_stream.finish parser in
  assert (List.rev !deltas = [ "Hi "; "there" ]);
  assert (message.content = Some "Hi there");
  assert (message.tool_calls = [ { Pave.Protocol.id = "toolu_1"; name = "read_file";
    arguments = `Assoc [ "path", `String "App.swift" ] } ]);
  let text_only = start ^ text_start ^ text_delta ^ text_stop ^ ending "end_turn" ^ stop in
  let parser = Pave.Anthropic_stream.create ~on_text:(fun _ -> ()) in
  Pave.Anthropic_stream.feed parser text_only;
  assert ((Pave.Anthropic_stream.finish parser).content = Some "Hi there");
  invalid (start ^ text_start ^ text_delta ^ text_stop);
  invalid (start ^ tool_start ^ tool_delta ^ tool_stop ^ ending "end_turn" ^ stop);
  invalid (start ^ text_start ^ text_stop ^ ending "max_tokens" ^ stop);
  invalid (start ^ event "error"
    {|{"type":"error","error":{"type":"overloaded_error","message":"Overloaded"}}|});
  print_endline "Anthropic incremental stream: ok"
