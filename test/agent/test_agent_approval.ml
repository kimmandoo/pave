let expect label condition =
  if not condition then failwith ("agent approval: " ^ label)
let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec find index = index + m <= n &&
    (String.sub text index m = fragment || find (index + 1)) in
  find 0

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
    ~tool_approval ~command_patterns ?approve_tool ?delegate_task
    ?(external_tools = []) ?execute_external ?validate_external_tool
    ?on_workspace_effect ?workspace_context () =
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
      ?workspace_context ~allow_shell ~approval_mode ~tool_approval ~command_patterns
      ?approve_tool ?delegate_task ?on_workspace_effect
      ~external_tools ?execute_external ?validate_external_tool
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
    let allowed_calls = ref 0 and tracked_effects = ref [] in
    ignore (with_agent ~root ~name:"write_file"
      ~arguments:(`Assoc ["path", `String "allowed.txt";
        "content", `String "approved by write mode"])
      ~allow_shell:false ~approval_mode:A.Ask_exec ~tool_approval:[]
      ~command_patterns:[]
      ~approve_tool:(fun _ -> incr allowed_calls; true)
      ~on_workspace_effect:(fun workspace_effect ->
        tracked_effects := workspace_effect :: !tracked_effects) ());
    expect "write mode allows a write without prompting" (!allowed_calls = 0);
    expect "allowed write changed the target file"
      (let input = open_in (Filename.concat root "allowed.txt") in
       Fun.protect ~finally:(fun () -> close_in input) (fun () ->
         input_line input = "approved by write mode"));
    expect "approved write exposes a durable pre/post rewind snapshot"
      (match !tracked_effects with
       | [Pave.Session_rewind.File_change {
            tool_name = "write_file"; path = "allowed.txt";
            before = Pave.Session_rewind.Missing;
            after = Pave.Session_rewind.Captured { data; _ } }] ->
           data = "approved by write mode"
       | _ -> false);
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
    let shell_effects = ref [] and shell_events = ref [] in
    let _, events = with_agent ~root ~name:"run_command"
      ~arguments:(`Assoc ["command", `String "touch approved-shell.txt"])
      ~allow_shell:true ~approval_mode:A.Auto_all ~tool_approval:[]
      ~command_patterns:[]
      ~approve_tool:(fun request ->
        expect "approved shell uses the exact command approval"
          (request.tool_name = "run_command");
        true)
      ~on_workspace_effect:(fun workspace_effect ->
        (match workspace_effect with
         | Pave.Session_rewind.Non_reversible_effect
             { tool_name = "run_command"; detail } ->
             expect "shell effects are recorded before execution"
               (not (Sys.file_exists (Filename.concat root "approved-shell.txt")));
             shell_effects := ("run_command", detail) :: !shell_effects
         | _ -> failwith "agent approval: shell was not marked non-reversible")) () in
    shell_events := events;
    expect "approved shell command ran"
      (Sys.file_exists (Filename.concat root "approved-shell.txt"));
    expect "shell attempt was recorded as non-reversible"
      (match !shell_effects with
       | [("run_command", detail)] ->
           String.starts_with ~prefix:"Shell command was attempted" detail
       | _ -> false);
    expect "shell non-reversible warning reached the user"
      (List.exists (fun event ->
        contains event "non-reversible" && contains event "/rewind") !shell_events);
    let process_manager = Pave.Workspace_process.create_manager () in
    let process_context = Pave.Tools.create_session_context
      ~owner:"approval-session" ~root ~process_manager
      ~read_artifact:(fun _ -> None)
      ~record_file_change:(fun ~path:_ ~before:_ ~after:_ -> ()) () in
    let process_prompts = ref 0 and process_request = ref None in
    let process_effects = ref [] in
    Fun.protect
      ~finally:(fun () ->
        Pave.Tools.close_session_context process_context;
        Pave.Workspace_process.close_manager process_manager)
      (fun () ->
        let _, process_events = with_agent ~root ~name:"start_process"
          ~arguments:(`Assoc [
            "id", `String "approved-process";
            "program", `String "/usr/bin/printf";
            "arguments", `List [`String "managed-process"]])
          ~allow_shell:true ~approval_mode:A.Auto_all ~tool_approval:[]
          ~command_patterns:[] ~workspace_context:process_context
          ~approve_tool:(fun request ->
            incr process_prompts;
            process_request := Some request;
            true)
          ~on_workspace_effect:(fun workspace_effect ->
            process_effects := workspace_effect :: !process_effects) () in
    let process_request_text = match !process_request with
      | None -> "(none)"
      | Some request -> String.concat " | "
          (request.tool_name :: request.impact :: request.details) in
    let process_events_text = String.concat " | " process_events in
    expect (Printf.sprintf
      "managed process start requires approval even in yolo (prompts=%d; request=%s; events=%s)"
      !process_prompts process_request_text process_events_text)
      (!process_prompts = 1 &&
       (match !process_request with
        | Some request -> request.tool_name = "start_process" &&
            String.starts_with ~prefix:"Starts an unsandboxed background executable"
              request.impact &&
            List.exists (String.starts_with ~prefix:"Program and arguments:")
              request.details
        | None -> false));
    expect "managed process attempt was recorded before execution"
      (match !process_effects with
       | [Pave.Session_rewind.Non_reversible_effect
            { tool_name = "start_process"; detail }] ->
           String.starts_with ~prefix:"start_process was attempted" detail
       | _ -> false);
    expect "managed process non-reversible warning reached the user"
      (List.exists (fun event ->
        contains event "non-reversible" && contains event "/rewind") process_events);
    expect "approved managed process exited successfully"
      (Pave.Workspace_process.wait_job process_manager ~id:"approved-process"
        ~timeout_seconds:5 () =
       Pave.Workspace_process.Completed (Pave.Workspace_process.Exited 0));
    expect "approved managed process executed argv"
      ((Pave.Workspace_process.read_output process_manager
          ~id:"approved-process" ()).output = "managed-process")
      );
    let task_calls = ref 0 and task_approval = ref None in
    let _, task_events = with_agent ~root ~name:"task"
      ~arguments:(`Assoc ["label", `String "review";
        "task", `String "Inspect the selected workspace read-only"])
      ~allow_shell:false ~approval_mode:A.Auto_all ~tool_approval:[]
      ~command_patterns:[]
      ~approve_tool:(fun request ->
        task_approval := Some request;
        true)
      ~delegate_task:(fun ~cancel ~label ~task ->
        expect "child delegation has not been cancelled" (not (cancel ()));
        expect "child label reached the delegate" (label = "review");
        expect "child task reached the delegate"
          (task = "Inspect the selected workspace read-only");
        incr task_calls;
        "0123456789abcdef0123456789abcdef") () in
    expect "child agent always requires explicit approval under yolo"
      (!task_calls = 1 &&
       (match !task_approval with
        | Some request -> request.tool_name = "task" &&
            String.starts_with ~prefix:"Starts a bounded read-only child agent"
              request.impact
        | None -> false));
    expect "delegation result identifies the durable child job"
      (List.exists (String.starts_with ~prefix:
        "[task] Started read-only child job 0123456789abcdef0123456789abcdef")
        task_events);
    let invalid_task_calls = ref 0 and invalid_task_prompts = ref 0 in
    ignore (with_agent ~root ~name:"task"
      ~arguments:(`Assoc ["label", `String "review"; "task", `String "inspect";
        "unexpected", `String "must be rejected"])
      ~allow_shell:false ~approval_mode:A.Auto_all ~tool_approval:[]
      ~command_patterns:[]
      ~approve_tool:(fun _ -> incr invalid_task_prompts; true)
      ~delegate_task:(fun ~cancel:_ ~label:_ ~task:_ ->
        incr invalid_task_calls; "unused") ());
    expect "task schema rejects unknown arguments before approval or delegation"
      (!invalid_task_calls = 0 && !invalid_task_prompts = 0);
    let external_schema = `Assoc ["type", `String "function";
      "function", `Assoc [
        "name", `String "local_review";
        "description", `String "Private local review";
        "parameters", `Assoc [
          "type", `String "object";
          "properties", `Assoc ["text", `Assoc [
            "type", `String "string"; "maxLength", `Int 64]];
          "required", `List [`String "text"];
          "additionalProperties", `Bool false]]] in
    let external_calls = ref 0 and external_prompts = ref 0 in
    let execute_external ~name:_ ~args:_ ~cancel:_ =
      incr external_calls; Ok "reviewed" in
    let validate_external_tool ~name:_ ~args =
      match Pave.Protocol.member "text" args with
      | `String value when String.length value <= 64 -> Ok ()
      | _ -> Error "invalid private tool input" in
    let call arguments approve_tool =
      with_agent ~root ~name:"local_review" ~arguments
        ~allow_shell:false ~approval_mode:A.Auto_all ~tool_approval:[]
        ~command_patterns:[] ~external_tools:[external_schema]
        ~execute_external ~validate_external_tool ~approve_tool () in
    let _, invalid_external = call (`Assoc ["other", `String "x"])
      (fun _ -> incr external_prompts; true) in
    expect "invalid extension arguments settle before approval or execution"
      (!external_prompts = 0 && !external_calls = 0 &&
       List.exists (String.starts_with ~prefix:
         "[local_review] Error: invalid private tool input") invalid_external);
    let _, denied_external = call (`Assoc ["text", `String "review"])
      (fun _ -> incr external_prompts; false) in
    expect "per-call external approval cannot be skipped by yolo"
      (!external_prompts = 1 && !external_calls = 0 &&
       List.exists (String.starts_with ~prefix:
         "[local_review] Error: tool approval denied") denied_external);
    let _, approved_external = call (`Assoc ["text", `String "review"])
      (fun _ -> incr external_prompts; true) in
    expect "approved extension settles one canonical provider result"
      (!external_prompts = 2 && !external_calls = 1 &&
       List.exists (String.starts_with ~prefix:
         "[local_review] reviewed") approved_external);
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
