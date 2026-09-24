open Pave

let item kind fields = `Assoc (("type", `String kind) :: fields)
let event kind fields =
  "data: " ^ Yojson.Basic.to_string (item kind fields) ^ "\n\n"
let added ?index output = event "response.output_item.added"
  ((match index with None -> [] | Some n -> ["output_index", `Int n]) @ ["item", output])
let done_item ?index output = event "response.output_item.done"
  ((match index with None -> [] | Some n -> ["output_index", `Int n]) @ ["item", output])
let indexed kind index id fields = event kind
  (["output_index", `Int index; "item_id", `String id] @ fields)
let completed ?(status = "completed") ?output
    ?(usage = Some (`Assoc ["input_tokens", `Int 5; "output_tokens", `Int 3])) () =
  event "response.completed" ["response", `Assoc (
    (["id", `String "resp_1"; "status", `String status] @
     (match usage with None -> [] | Some reported -> ["usage", reported]) @
     (match output with None -> [] | Some output -> ["output", `List output])))]
let text value = item "output_text" ["text", `String value]
let message id value = item "message" ["id", `String id;
  "role", `String "assistant"; "status", `String "completed";
  "content", `List [text value]]
let call id call_id name arguments = item "function_call" [
  "id", `String id; "call_id", `String call_id; "name", `String name;
  "status", `String "completed"; "arguments", `String arguments]
let model = "gpt-5.1-codex"
let stream wire =
  let t = Codex_stream.create ~model ~on_text:(fun _ -> ()) in
  Codex_stream.feed t wire;
  Codex_stream.finish t
let rejects = ref 0
let invalid wire =
  incr rejects;
  match stream wire with
  | exception Protocol.Invalid_response _ -> ()
  | _ -> failwith ("expected invalid Codex SSE case " ^ string_of_int !rejects)

let () =
  let initial_reasoning = item "reasoning" ["id", `String "rs_1";
    "summary", `List []] in
  let final_reasoning = item "reasoning" ["id", `String "rs_1";
    "status", `String "completed"; "encrypted_content", `String "native-opaque";
    "summary", `List [item "summary_text" ["text", `String "Thinking"]]] in
  let initial_message = item "message" ["id", `String "msg_1";
    "role", `String "assistant"; "status", `String "in_progress";
    "content", `List []] in
  let final_message = message "msg_1" "Reading file" in
  let initial_call = item "function_call" ["id", `String "fc_1";
    "call_id", `String "call_1"; "name", `String "read_file";
    "arguments", `String ""] in
  let final_call = call "fc_1" "call_1" "read_file" {|{"path":"README.md"}|} in
  (* Codex's real SSE fixtures omit response.output and often omit output_index,
     item_id and content_index; the completed items carry the native replay. *)
  let sse = ": heartbeat\n\n"
    ^ event "response.created" ["response", `Assoc ["id", `String "resp_1"]]
    ^ added initial_reasoning
    ^ event "response.reasoning_summary_text.delta" ["item_id", `String "rs_1";
        "delta", `String "Thinking"]
    ^ done_item final_reasoning
    ^ added initial_message
    ^ event "response.content_part.added" ["part", text ""]
    ^ event "response.output_text.delta" ["delta", `String "Reading "]
    ^ event "response.output_text.delta" ["delta", `String "file"]
    ^ done_item final_message
    ^ added initial_call
    ^ event "response.function_call_arguments.delta" ["item_id", `String "fc_1";
        "delta", `String {|{"path":"|}]
    ^ event "response.function_call_arguments.delta" ["item_id", `String "fc_1";
        "delta", `String {|README.md"}|}]
    ^ done_item final_call ^ completed () in
  let chunks = ref [] in
  let t = Codex_stream.create ~model ~on_text:(fun chunk -> chunks := chunk :: !chunks) in
  String.iter (fun char -> Codex_stream.feed t (String.make 1 char)) sse;
  assert (Codex_stream.is_done t && Codex_stream.is_finished t);
  let result = Codex_stream.finish t in
  assert (Codex_stream.usage t =
    Some { Protocol.input_tokens = 5; output_tokens = 3 });
  assert (List.rev !chunks = ["Reading "; "file"]);
  assert (result.content = Some "Reading file");
  assert (result.tool_calls = [{ Protocol.id = "call_1"; name = "read_file";
    arguments = `Assoc ["path", `String "README.md"] }]);
  assert (Protocol.member "output" (Option.get result.provider_state) =
    `List [final_reasoning; final_message; final_call]);
  let body = Codex_wire.request ~model
    [Protocol.user "Read"; result; Protocol.tool_result "call_1" "file contents";
     Protocol.user "Summarize"] [] in
  (match Protocol.member "input" body with
  | `List (_user :: native_reasoning :: _message :: _call :: _result :: _next :: []) ->
      assert (Protocol.member "type" native_reasoning = `String "reasoning");
      assert (Protocol.member "encrypted_content" native_reasoning = `String "native-opaque")
  | _ -> failwith "Codex native reasoning not replayed");
  let no_delta = added initial_message ^ done_item final_message ^ completed () in
  let chunks = ref [] in
  let t = Codex_stream.create ~model ~on_text:(fun chunk -> chunks := chunk :: !chunks) in
  Codex_stream.feed t no_delta;
  assert ((Codex_stream.finish t).content = Some "Reading file");
  assert (List.rev !chunks = ["Reading file"]);
  let final_only = completed ~output:[final_message] () in
  let chunks = ref [] in
  let t = Codex_stream.create ~model ~on_text:(fun chunk -> chunks := chunk :: !chunks) in
  Codex_stream.feed t final_only;
  assert ((Codex_stream.finish t).content = Some "Reading file");
  assert (List.rev !chunks = ["Reading file"]);
  let unmetered = Codex_stream.create ~model ~on_text:(fun _ -> ()) in
  Codex_stream.feed unmetered (completed ~output:[final_message] ~usage:None ());
  ignore (Codex_stream.finish unmetered);
  assert (Codex_stream.usage unmetered = None);
  let malformed = Codex_stream.create ~model ~on_text:(fun _ -> ()) in
  Codex_stream.feed malformed (completed ~output:[final_message]
    ~usage:(Some (`Assoc ["input_tokens", `Int 5;
      "output_tokens", `Int (-1)])) ());
  ignore (Codex_stream.finish malformed);
  assert (Codex_stream.usage malformed = None);
  let indexed_call = added ~index:0 initial_call
    ^ indexed "response.function_call_arguments.delta" 0 "fc_1" [
        "delta", `String {|{"path":"README.md"}|}]
    ^ done_item ~index:0 final_call ^ completed () in
  assert ((stream indexed_call).tool_calls = result.tool_calls);
  let idless_call = item "function_call" ["call_id", `String "call_1";
    "name", `String "read_file"; "arguments", `String ""] in
  let idless_final = item "function_call" ["call_id", `String "call_1";
    "name", `String "read_file"; "arguments", `String {|{"path":"README.md"}|}] in
  assert ((stream (added ~index:0 idless_call ^ done_item ~index:0 idless_final ^
    completed ())).tool_calls = result.tool_calls);
  invalid (added initial_message ^ event "response.output_text.delta" [
    "item_id", `String "wrong_id"; "delta", `String "oops"]
    ^ done_item final_message ^ completed ());
  invalid (added initial_message ^ event "response.output_text.delta" [
    "delta", `String "corrupt"] ^ done_item final_message ^ completed ());
  invalid (added initial_call ^ event "response.function_call_arguments.delta" [
    "delta", `String "{}"] ^ done_item final_call ^ completed ());
  invalid (added initial_message ^ done_item final_message ^ completed ~status:"incomplete" ());
  invalid (added initial_message ^ done_item final_message ^
    event "response.failed" ["response", `Assoc ["status", `String "failed"]]);
  invalid (added initial_message ^ event "response.refusal.delta" [
    "delta", `String "No"] ^ done_item final_message ^ completed ());
  invalid (added initial_message ^ done_item final_message ^
    completed ~output:[message "msg_1" "changed in completed response"] ());
  invalid (added initial_message ^ added (item "message" ["id", `String "msg_2";
    "role", `String "assistant"; "content", `List []]) ^
    event "response.output_text.delta" ["delta", `String "ambiguous"]);
  invalid (added initial_message ^ done_item final_message);
  invalid ("data: [DONE]\n\n");
  invalid (event "response.created" [
      "response", `Assoc ["id", `String "resp_1"]] ^
    added initial_message ^ done_item final_message ^
    event "response.completed" ["response", `Assoc [
      "id", `String "wrong-response-id"; "status", `String "completed"]]);
  print_endline "Codex stream: ok"
