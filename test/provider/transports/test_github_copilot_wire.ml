module Wire = Pave.Github_copilot_wire
module Protocol = Pave.Protocol

let rejected f = match f () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith "unsupported Copilot Chat route accepted"

let header key headers =
  let prefix = key ^ ": " in
  let line = List.find (String.starts_with ~prefix) headers in
  String.sub line (String.length prefix) (String.length line - String.length prefix)

let () =
  assert (Wire.endpoint = "https://api.githubcopilot.com/chat/completions");
  List.iter (fun model -> assert (not (Wire.supported_model model)))
    ["gpt-5"; "claude-sonnet-4"; "GPT-4.1"; "gpt-4o-mini"; ""];
  let token = "ghu_fixture-private-token" in
  let messages = [Protocol.user "question"] in
  let headers = Wire.headers ~endpoint:Wire.endpoint ~model:"gpt-4.1" ~token ~messages in
  assert (header "Authorization" headers = "Bearer " ^ token);
  assert (header "Copilot-Integration-Id" headers = "copilot-chat");
  assert (header "Openai-Intent" headers = "conversation-agent");
  assert (header "X-Initiator" headers = "user");
  assert (header "X-Interaction-Type" headers = "conversation-user");
  let tool_messages = messages @ [Protocol.tool_result "call-1" "result"] in
  let headers = Wire.headers ~endpoint:Wire.endpoint ~model:"gpt-4o" ~token
    ~messages:tool_messages in
  assert (header "X-Initiator" headers = "agent");
  assert (header "X-Interaction-Type" headers = "conversation-agent");
  rejected (fun () -> Wire.headers ~endpoint:"https://evil.example/chat/completions"
    ~model:"gpt-4.1" ~token ~messages);
  rejected (fun () -> Wire.headers
    ~endpoint:"https://api.githubcopilot.com/chat/completions/../v1/responses"
    ~model:"gpt-4.1" ~token ~messages);
  rejected (fun () -> Wire.headers ~endpoint:Wire.endpoint
    ~model:"gpt-5" ~token ~messages);
  rejected (fun () -> Wire.headers ~endpoint:Wire.endpoint
    ~model:"gpt-4o" ~token:"ghu_forged\r\nHost:evil.example" ~messages);
  rejected (fun () -> Wire.headers ~endpoint:Wire.endpoint
    ~model:"gpt-4o" ~token:"" ~messages);
  print_endline "GitHub Copilot Chat wire: ok"
