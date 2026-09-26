open Pave

let item kind fields = `Assoc (("type", `String kind) :: fields)
let response_item id kind fields = item kind (("id", `String id) :: fields)
let text value = item "output_text" [ "text", `String value ]
let message id value = response_item id "message" [ "role", `String "assistant";
  "status", `String "completed"; "content", `List [ text value ] ]
let call id call_id name arguments = response_item id "function_call" [
  "status", `String "completed"; "call_id", `String call_id;
  "name", `String name; "arguments", `String arguments ]
let event kind fields =
  let json = item kind fields |> Yojson.Basic.to_string in
  "event: " ^ kind ^ "\r\ndata: " ^ json ^ "\r\n\r\n"
let added n output = event "response.output_item.added" [
  "output_index", `Int n; "item", output ]
let done_item n output = event "response.output_item.done" [
  "output_index", `Int n; "item", output ]
let indexed kind n id fields = event kind
  ([ "output_index", `Int n; "item_id", `String id ] @ fields)
let completion outputs = event "response.completed" [ "response", `Assoc [
  "status", `String "completed"; "output", `List outputs ] ]
let failed = event "response.failed" [ "response", `Assoc [
  "status", `String "failed"; "error", `Assoc [ "message", `String "unavailable" ] ] ]
let stream wire =
  let t = Openai_responses_stream.create ~on_text:(fun _ -> ()) in
  Openai_responses_stream.feed t wire;
  Openai_responses_stream.finish t
let invalid_cases = ref 0
let invalid wire =
  incr invalid_cases;
  match stream wire with
  | exception Protocol.Invalid_response _ -> ()
  | _ -> failwith ("expected invalid Responses stream case " ^ string_of_int !invalid_cases)

let () =
  let initial_message = response_item "msg_1" "message" [
    "role", `String "assistant"; "content", `List [] ] in
  let final_message = message "msg_1" "Hello world" in
  let initial_call = response_item "fc_1" "function_call" [
    "call_id", `String "call_1"; "name", `String "read_file";
    "arguments", `String "" ] in
  let final_call = call "fc_1" "call_1" "read_file" {|{"path":"a.txt"}|} in
  let wire = ": heartbeat\r\n\r\n"
    ^ added 0 initial_message
    ^ indexed "response.output_text.delta" 0 "msg_1" [
      "content_index", `Int 0; "delta", `String "Hello " ]
    ^ indexed "response.output_text.delta" 0 "msg_1" [
      "content_index", `Int 0; "delta", `String "world" ]
    ^ done_item 0 final_message
    ^ added 1 initial_call
    ^ indexed "response.function_call_arguments.delta" 1 "fc_1" [
      "delta", `String {|{"path":"|} ]
    ^ indexed "response.function_call_arguments.delta" 1 "fc_1" [
      "delta", `String {|a.txt"}|} ]
    ^ indexed "response.function_call_arguments.done" 1 "fc_1" [
      "name", `String "read_file"; "arguments", `String {|{"path":"a.txt"}|} ]
    ^ done_item 1 final_call
    ^ completion [ final_message; final_call ]
    ^ "data: [DONE]\r\n\r\n" in
  let deltas = ref [] in
  let t = Openai_responses_stream.create ~on_text:(fun part -> deltas := part :: !deltas) in
  String.iter (fun byte -> Openai_responses_stream.feed t (String.make 1 byte)) wire;
  assert (Openai_responses_stream.is_done t);
  assert (Openai_responses_stream.is_finished t);
  let response = Openai_responses_stream.finish t in
  assert (List.rev !deltas = [ "Hello "; "world" ]);
  assert (response = { Protocol.role = "assistant"; content = Some "Hello world";
    tool_calls = [ { Protocol.id = "call_1"; name = "read_file";
      arguments = `Assoc [ "path", `String "a.txt" ] } ];
    tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] });
  assert (Openai_responses_stream.usage t = None);
  let metered = Openai_responses_stream.create ~on_text:(fun _ -> ()) in
  let measured = event "response.completed" [ "response", `Assoc [
    "status", `String "completed"; "output", `List [final_message];
    "usage", `Assoc ["input_tokens", `Int 21; "output_tokens", `Int 9] ] ] in
  Openai_responses_stream.feed metered measured;
  ignore (Openai_responses_stream.finish metered);
  assert (Openai_responses_stream.usage metered =
    Some { Protocol.input_tokens = 21; output_tokens = 9 });
  let without_deltas = added 0 initial_message ^ done_item 0 final_message
    ^ completion [ final_message ] in
  let deltas = ref [] in
  let t = Openai_responses_stream.create ~on_text:(fun part -> deltas := part :: !deltas) in
  Openai_responses_stream.feed t without_deltas;
  assert ((Openai_responses_stream.finish t).content = Some "Hello world");
  assert (List.rev !deltas = [ "Hello world" ]);
  let only_completed = ref [] in
  let t = Openai_responses_stream.create
    ~on_text:(fun part -> only_completed := part :: !only_completed) in
  Openai_responses_stream.feed t (completion [ final_message ]);
  assert ((Openai_responses_stream.finish t).content = Some "Hello world");
  assert (List.rev !only_completed = [ "Hello world" ]);
  invalid (added 1 initial_call ^ indexed "response.function_call_arguments.delta" 1 "fc_1"
    [ "delta", `String "{" ] ^ completion [ final_message; final_call ]);
  invalid (added 1 initial_call ^ indexed "response.function_call_arguments.done" 1 "fc_1"
    [ "name", `String "read_file"; "arguments", `String "{" ]
    ^ done_item 1 final_call ^ completion [ final_message; final_call ]);
  invalid (added 0 initial_message ^ indexed "response.output_text.delta" 0 "msg_1"
    [ "content_index", `Int 0; "delta", `String "mismatch" ]
    ^ done_item 0 final_message ^ completion [ final_message ]);
  invalid (added 0 initial_message ^ done_item 0 final_message ^
    completion [ message "msg_1" "silently changed" ]);
  invalid (added 1 initial_call ^ done_item 1 final_call ^
    completion [ final_message; call "fc_1" "call_1" "read_file" {|{"path":"other.txt"}|} ]);
  invalid (added 0 initial_message ^ done_item 0 final_message ^ failed);
  invalid (added 1 initial_call ^ done_item 1
    (call "fc_1" "call_1" "read_file" "{bad") ^ completion [ final_message; final_call ]);
  invalid (added 0 initial_message ^ done_item 0 final_message);
  invalid ("data: [DONE]\r\n\r\n");
  invalid ("event: error\r\ndata: {\"type\":\"error\",\"message\":\"bad\"}\r\n\r\n");
  invalid ("event: response.completed\r\ndata: {bad}\r\n\r\n");
  print_endline "OpenAI Responses stream: ok"
