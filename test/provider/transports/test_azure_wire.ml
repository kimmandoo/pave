open Pave

let field name json = Protocol.member name json
let invalid f = match f () with
  | exception Invalid_argument _ | exception Provider.Provider_error _ -> ()
  | _ -> failwith "unsafe Azure configuration was accepted"

let completed_call = `Assoc [ "id", `String "fc_72";
  "type", `String "function_call"; "status", `String "completed";
  "call_id", `String "call_72"; "name", `String "lookup";
  "arguments", `String {|{"query":"hello"}|} ]
let response = `Assoc [ "status", `String "completed";
  "output", `List [completed_call] ]
let event kind fields =
  let json = `Assoc (("type", `String kind) :: fields) in
  "event: " ^ kind ^ "\r\ndata: " ^ Yojson.Basic.to_string json ^ "\r\n\r\n"

let () =
  let base = "https://project-7.openai.azure.com" in
  let endpoint = base ^ "/openai/v1/responses?api-version=v1" in
  let endpoint, headers = Azure_wire.resolve ~endpoint ~deployment:"my-deployment"
    ~api_key:"private-azure-key" in
  assert (endpoint = base ^ "/openai/v1/responses?api-version=v1");
  assert (headers = ["api-key: private-azure-key"]);
  assert (Azure_wire.resolve ~endpoint:(base ^ "/openai/v1/responses?api-version=preview")
    ~deployment:"my-deployment" ~api_key:"private-azure-key" |> fst
    = base ^ "/openai/v1/responses?api-version=preview");
  let child = Unix.fork () in
  if child = 0 then (
    Unix.putenv "AZURE_OPENAI_ENDPOINT" (base ^ "/");
    Unix.putenv "AZURE_OPENAI_API_VERSION" "preview";
    assert (Azure_wire.endpoint () =
      base ^ "/openai/v1/responses?api-version=preview");
    Unix.putenv "AZURE_OPENAI_API_VERSION" "";
    assert (Azure_wire.endpoint () = base ^ "/openai/v1/responses");
    Unix.putenv "AZURE_OPENAI_API_VERSION" "2025-04-01-preview";
    invalid Azure_wire.endpoint;
    Unix.putenv "AZURE_OPENAI_ENDPOINT" "https://project-7.openai.azure.com.evil.example";
    invalid Azure_wire.endpoint;
    exit 0);
  (match snd (Unix.waitpid [] child) with
   | Unix.WEXITED 0 -> ()
   | _ -> failwith "Azure endpoint environment fixture failed");
  let user : Protocol.message = { role = "user"; content = Some "Find greeting";
    tool_calls = []; tool_call_id = None; provider_state = None } in
  let call : Protocol.tool_call = { id = "call_72"; name = "lookup";
    arguments = `Assoc ["query", `String "hello"] } in
  let expected : Protocol.message = { role = "assistant"; content = None;
    tool_calls = [call]; tool_call_id = None; provider_state = None } in
  let parameters = `Assoc ["type", `String "object"; "properties",
    `Assoc ["query", `Assoc ["type", `String "string"]]] in
  let tool = `Assoc ["type", `String "function"; "function", `Assoc [
    "name", `String "lookup"; "description", `String "Look up a greeting";
    "parameters", parameters]] in
  let first_request = Openai_responses_wire.request ~model:"my-deployment"
    [user] [tool] in
  assert (field "model" first_request = `String "my-deployment");
  assert (field "tools" first_request = `List [`Assoc [
    "type", `String "function"; "name", `String "lookup";
    "parameters", `Assoc ["type", `String "object"; "properties", `Assoc [
      "query", `Assoc ["type", `String "string"]]];
    "strict", `Bool false; "description", `String "Look up a greeting" ]]);
  let buffered = Openai_responses_wire.parse_completion response in
  assert (buffered = expected);
  let streamed = Openai_responses_stream.create ~on_text:(fun _ -> ()) in
  let initial_call = `Assoc [ "id", `String "fc_72";
    "type", `String "function_call"; "call_id", `String "call_72";
    "name", `String "lookup"; "arguments", `String "" ] in
  let wire = event "response.output_item.added" [
      "output_index", `Int 0; "item", initial_call ]
    ^ event "response.function_call_arguments.delta" [
      "output_index", `Int 0; "item_id", `String "fc_72";
      "delta", `String {|{"query":"hel|} ]
    ^ event "response.function_call_arguments.delta" [
      "output_index", `Int 0; "item_id", `String "fc_72";
      "delta", `String {|lo"}|} ]
    ^ event "response.function_call_arguments.done" [
      "output_index", `Int 0; "item_id", `String "fc_72";
      "name", `String "lookup"; "arguments", `String {|{"query":"hello"}|} ]
    ^ event "response.output_item.done" [
      "output_index", `Int 0; "item", completed_call ]
    ^ event "response.completed" ["response", response]
    ^ "data: [DONE]\r\n\r\n" in
  String.iter (fun c -> Openai_responses_stream.feed streamed (String.make 1 c)) wire;
  assert (Openai_responses_stream.finish streamed = expected);
  let tool_result = Protocol.tool_result "call_72" "Found greeting" in
  let continued = Openai_responses_wire.request ~stream:true
    ~model:"my-deployment" [user; buffered; tool_result] [] in
  assert (field "stream" continued = `Bool true);
  assert (field "input" continued = `List [
    `Assoc ["role", `String "user"; "content", `List [
      `Assoc ["type", `String "input_text"; "text", `String "Find greeting"] ]];
    `Assoc ["type", `String "function_call"; "call_id", `String "call_72";
      "name", `String "lookup"; "arguments", `String {|{"query":"hello"}|} ];
    `Assoc ["type", `String "function_call_output"; "call_id", `String "call_72";
      "output", `String "Found greeting"] ]);
  let answer = `Assoc [ "status", `String "completed"; "output", `List [
    `Assoc [ "type", `String "message"; "role", `String "assistant";
      "content", `List [ `Assoc [ "type", `String "output_text";
        "text", `String "Hello from lookup" ] ] ] ] ] in
  assert ((Openai_responses_wire.parse_completion answer).content =
    Some "Hello from lookup");
  let emitted = ref [] in
  let final_stream = Openai_responses_stream.create
    ~on_text:(fun text -> emitted := text :: !emitted) in
  Openai_responses_stream.feed final_stream
    (event "response.completed" ["response", answer] ^ "data: [DONE]\r\n\r\n");
  assert ((Openai_responses_stream.finish final_stream).content =
    Some "Hello from lookup");
  assert (List.rev !emitted = ["Hello from lookup"]);
  let config endpoint : Provider.config = {
    api = Provider.Azure_responses; model = "my-deployment";
    api_key = "private-azure-key"; endpoint } in
  List.iter (fun hostile ->
    invalid (fun () -> Azure_wire.resolve ~endpoint:hostile ~deployment:"my-deployment"
      ~api_key:"private-azure-key");
    invalid (fun () -> Provider.complete (config hostile) [user] [])) [
    "http://project-7.openai.azure.com/openai/v1/responses";
    "https://project-7.openai.azure.com.evil.example/openai/v1/responses";
    "https://project-7.openai.azure.com@evil.example/openai/v1/responses";
    "https://project-7.openai.azure.com:443/openai/v1/responses";
    "https://project-7.openai.azure.com/openai/v1/responses#evil";
    "https://project-7.openai.azure.com/openai/v1/responses?api-version=bad";
    "https://project-7.openai.azure.com/openai/v1/responses/../evil" ];
  invalid (fun () -> Azure_wire.resolve ~endpoint ~deployment:"" ~api_key:"private-azure-key");
  invalid (fun () -> Azure_wire.resolve ~endpoint ~deployment:"my-deployment" ~api_key:"");
  invalid (fun () -> Azure_wire.resolve ~endpoint ~deployment:"my-deployment"
    ~api_key:"private-azure-key\r\nHost: evil.example");
  invalid (fun () -> Provider.complete ~authentication:Provider.OAuth
    (config endpoint) [user] []);
  print_endline "Azure OpenAI Responses endpoint and tool turn: ok"
