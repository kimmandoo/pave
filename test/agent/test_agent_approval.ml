let expect label condition =
  if not condition then failwith ("agent approval: " ^ label)

let send_json oc body =
  Printf.fprintf oc
    "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
    (String.length body) body;
  flush oc

let tool_reply name arguments =
  let call = `Assoc [
    "id", `String "approval-call";
    "type", `String "function";
    "function", `Assoc [
      "name", `String name;
      "arguments", `String (Yojson.Basic.to_string arguments)
    ]
  ] in
  Yojson.Basic.to_string (`Assoc [
    "choices", `List [`Assoc [
      "finish_reason", `String "tool_calls";
      "message", `Assoc [
        "role", `String "assistant";
        "content", `Null;
        "tool_calls", `List [call]
      ]
    ]]
  ])

let final_reply = Yojson.Basic.to_string (`Assoc [
  "choices", `List [`Assoc [
    "finish_reason", `String "stop";
    "message", `Assoc [
      "role", `String "assistant";
      "content", `String "Approval scenario completed.";
      "tool_calls", `List []
    ]
  ]]
])

let serve_client client step ~allow_shell ~first_reply =
  let ic = Unix.in_channel_of_descr client in
  let oc = Unix.out_channel_of_descr client in
  let length = ref 0 in
  let rec headers () =
    let line = input_line ic in
    if line <> "\r" && line <> "" then (
      let lower = String.lowercase_ascii line in
      if String.starts_with ~prefix:"content-length:" lower then
        length := int_of_string
          (String.trim (String.sub line 15 (String.length line - 15)));
      headers ()) in
  headers ();
  let request = Yojson.Basic.from_string (really_input_string ic !length) in
  (match Pave.Protocol.member "tools" request with
   | `List schemas ->
       let has_shell = List.exists (fun schema ->
         Pave.Protocol.member "name" (Pave.Protocol.member "function" schema)
         = `String "run_command") schemas in
       expect "shell tool availability matches configuration"
         (has_shell = allow_shell)
   | _ -> failwith "agent approval: tool schemas missing");
  if step = 1 then (
    match Pave.Protocol.member "messages" request with
    | `List messages ->
        expect "tool result was sent back to the provider"
          (List.exists (fun message ->
            Pave.Protocol.member "tool_call_id" message =
              `String "approval-call") messages)
    | _ -> failwith "agent approval: messages missing");
  send_json oc (if step = 0 then first_reply else final_reply);
  close_in_noerr ic;
  close_out_noerr oc

let with_agent ~root ~name ~arguments ~allow_shell ~approval_mode
    ~tool_approval ~command_patterns ?approve_tool () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 2;
  let port = match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> assert false in
  let first_reply = tool_reply name arguments in
  let child = Unix.fork () in
  if child = 0 then (
    (try
       for step = 0 to 1 do
         let client, _ = Unix.accept socket in
         serve_client client step ~allow_shell ~first_reply
       done;
       Unix.close socket;
       exit 0
     with exn ->
       prerr_endline (Printexc.to_string exn);
       exit 2));
  Unix.close socket;
  let completed = ref false in
  Fun.protect ~finally:(fun () ->
    if not !completed then
      (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
    let _, status = Unix.waitpid [] child in
    match status with
    | Unix.WEXITED 0 -> ()
    | _ -> failwith "agent approval: local provider fixture failed") (fun () ->
    let provider : Pave.Provider.config = {
      endpoint = Printf.sprintf "http://127.0.0.1:%d/v1/chat/completions" port;
      api_key = "test-key"; model = "mock";
      api = Pave.Provider.Openai_completions } in
    let events = ref [] in
    let agent = Pave.Agent.create ~provider ~root ~system:"approval integration"
      ~allow_shell ~approval_mode ~tool_approval ~command_patterns ?approve_tool
      ~on_event:(fun event -> events := event :: !events) () in
    let result = Pave.Agent.run agent "Exercise approval policy" in
    completed := true;
    result, List.rev !events)

let new_root () =
  let root = Filename.temp_file "pave-approval-agent-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  root

let remove_root root =
  Array.iter (fun name -> Sys.remove (Filename.concat root name))
    (Sys.readdir root);
  Unix.rmdir root

let () =
  let module A = Pave.Approval in
  let root = new_root () in
  Fun.protect ~finally:(fun () -> remove_root root) (fun () ->
    let prompt_calls = ref 0 and captured = ref None in
    let result, events = with_agent ~root ~name:"write_file"
      ~arguments:(`Assoc ["path", `String "blocked.txt";
        "content", `String "secret content"])
      ~allow_shell:false ~approval_mode:A.Ask_writes ~tool_approval:[]
      ~command_patterns:[]
      ~approve_tool:(fun request ->
        incr prompt_calls;
        captured := Some request;
        false) () in
    expect "write request was prompted and rejected" (!prompt_calls = 1);
    expect "rejected write has no side effect"
      (not (Sys.file_exists (Filename.concat root "blocked.txt")));
    expect "approval preview names tool, tier, impact and path"
      (match !captured with
       | Some request -> request.tool_name = "write_file" &&
           request.tier = A.Write &&
           String.starts_with ~prefix:"Creates or replaces" request.impact &&
           List.exists (String.starts_with ~prefix:"Path: \"blocked.txt\"")
             request.details
       | None -> false);
    expect "denial is returned to the model"
      (List.exists (String.starts_with ~prefix:"[write_file] Error:") events);
    expect "model turn completed after rejection"
      (result = "Approval scenario completed.");
    let denied_calls = ref 0 in
    let _, denied_events = with_agent ~root ~name:"write_file"
      ~arguments:(`Assoc ["path", `String "policy-denied.txt";
        "content", `String "must not be written"])
      ~allow_shell:false ~approval_mode:A.Auto_all
      ~tool_approval:["write_file", A.Deny] ~command_patterns:[]
      ~approve_tool:(fun _ -> incr denied_calls; true) () in
    expect "tool deny overrides yolo without prompting" (!denied_calls = 0);
    expect "tool deny prevents side effect"
      (not (Sys.file_exists (Filename.concat root "policy-denied.txt")));
    expect "tool deny is reported to the model"
      (List.exists (String.starts_with ~prefix:"[write_file] Error:") denied_events);
    ignore (with_agent ~root ~name:"write_file"
      ~arguments:(`Assoc ["path", `String "headless-prompt.txt";
        "content", `String "must not be written"])
      ~allow_shell:false ~approval_mode:A.Auto_all
      ~tool_approval:["write_file", A.Prompt] ~command_patterns:[] ());
    expect "headless prompt-required write fails closed"
      (not (Sys.file_exists (Filename.concat root "headless-prompt.txt")));
    let allowed_calls = ref 0 in
    ignore (with_agent ~root ~name:"write_file"
      ~arguments:(`Assoc ["path", `String "allowed.txt";
        "content", `String "approved by write mode"])
      ~allow_shell:false ~approval_mode:A.Ask_exec ~tool_approval:[]
      ~command_patterns:[]
      ~approve_tool:(fun _ -> incr allowed_calls; true) ());
    expect "write mode allows a write without prompting" (!allowed_calls = 0);
    expect "allowed write changed the target file"
      (let input = open_in (Filename.concat root "allowed.txt") in
       Fun.protect ~finally:(fun () -> close_in input) (fun () ->
         input_line input = "approved by write mode"));
    let shell_prompts = ref 0 and shell_preview = ref None in
    ignore (with_agent ~root ~name:"run_command"
      ~arguments:(`Assoc ["command", `String "touch mandatory-prompt.txt"])
      ~allow_shell:true ~approval_mode:A.Auto_all ~tool_approval:[]
      ~command_patterns:[]
      ~approve_tool:(fun request ->
        incr shell_prompts;
        shell_preview := Some request;
        false) ());
    expect "shell still prompts under yolo" (!shell_prompts = 1);
    expect "shell preview discloses unsandboxed execution"
      (match !shell_preview with
       | Some request ->
           (let text = request.impact in
            let fragment = "not sandboxed" in
            let n = String.length text and m = String.length fragment in
            let rec contains i = i + m <= n &&
              (String.sub text i m = fragment || contains (i + 1)) in
            contains 0) &&
           List.exists (fun detail ->
             String.starts_with ~prefix:"Command: touch mandatory-prompt.txt" detail)
             request.details
       | None -> false);
    expect "rejected shell command has no side effect"
      (not (Sys.file_exists (Filename.concat root "mandatory-prompt.txt")));
    let compound_calls = ref 0 in
    ignore (with_agent ~root ~name:"run_command"
      ~arguments:(`Assoc ["command", `String
        "echo harmless && touch compound-denied.txt"])
      ~allow_shell:true ~approval_mode:A.Auto_all ~tool_approval:[]
      ~command_patterns:[{ A.match_text = "touch compound-denied.txt";
        policy = A.Deny }]
      ~approve_tool:(fun _ -> incr compound_calls; true) ());
    expect "compound deny blocks before prompting" (!compound_calls = 0);
    expect "compound deny blocks every side effect"
      (not (Sys.file_exists (Filename.concat root "compound-denied.txt"))));
  print_endline "agent approval prompts, denies, previews and side effects: ok"
