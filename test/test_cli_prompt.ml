let contains haystack needle =
  let haystack_length = String.length haystack
  and needle_length = String.length needle in
  let rec search index =
    index + needle_length <= haystack_length &&
    (String.sub haystack index needle_length = needle || search (index + 1)) in
  search 0

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun name ->
        remove_tree (Filename.concat path name));
      Unix.rmdir path)
    else Sys.remove path

      
let fail message = failwith message

let check condition message = if not condition then fail message
let member name = function
  | `Assoc fields -> List.assoc name fields
  | _ -> failwith ("expected object when reading " ^ name)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel contents)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    really_input_string channel (in_channel_length channel))

let read_request channel =
  let request_line = input_line channel in
  let length = ref 0 in
  let rec headers () =
    match input_line channel with
    | "" | "\r" -> ()
    | line ->
        let lower = String.lowercase_ascii line in
        if String.starts_with ~prefix:"content-length:" lower then
          length := int_of_string (String.trim
            (String.sub line 15 (String.length line - 15)));
        headers () in
  headers ();
  let body = Yojson.Basic.from_string (really_input_string channel !length) in
  request_line, body

let response status content_type body = status, content_type, body

let with_server ?(before_reply = fun _ -> ()) responses check_request run =
  let listener = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt listener Unix.SO_REUSEADDR true;
  Unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen listener 8;
  let port = match Unix.getsockname listener with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> assert false in
  let error = ref None in
  let server = Thread.create (fun () ->
    (try
       List.iteri (fun index (status, content_type, body) ->
         let readable, _, _ = Unix.select [listener] [] [] 15. in
         if readable = [] then failwith "fake provider timed out waiting for request";
         let client, _ = Unix.accept listener in
         let input = Unix.in_channel_of_descr client in
         let output = Unix.out_channel_of_descr (Unix.dup client) in
        let check_error =
          try
            let request_line, request = read_request input in
            check_request index request_line request;
            before_reply index;
            None
          with exn -> Some exn in
        (try
           Printf.fprintf output
             "HTTP/1.1 %d OK\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n"
             status content_type (String.length body);
           output_string output body;
           flush output
         with Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) |
              Sys_error _ -> ());
        close_in_noerr input;
        close_out_noerr output;
        Option.iter raise check_error) responses
     with exn -> error := Some exn);
    Unix.close listener) () in
  let endpoint = Printf.sprintf
    "http://127.0.0.1:%d/v1/chat/completions" port in
  let result = try Ok (run endpoint) with exn -> Error exn in
  Thread.join server;
  Option.iter raise !error;
  match result with Ok value -> value | Error exn -> raise exn

type result = { status : Unix.process_status; stdout : string; stderr : string }

let run binary root ?(input = "") ?(before_wait = fun _ -> ()) arguments =
  let stdin_path = Filename.temp_file "pave-cli-stdin" ".txt"
  and stdout_path = Filename.temp_file "pave-cli-stdout" ".txt"
  and stderr_path = Filename.temp_file "pave-cli-stderr" ".txt" in
  write_file stdin_path input;
  let stdin = Unix.openfile stdin_path [Unix.O_RDONLY] 0
  and stdout = Unix.openfile stdout_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
  and stderr = Unix.openfile stderr_path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600 in
  let home = Filename.concat root "home" in
  List.iter (fun path -> if not (Sys.file_exists path) then Unix.mkdir path 0o700)
    [home; Filename.concat home "config"; Filename.concat home "data";
     Filename.concat home "state"; Filename.concat home "cache"];
  let environment = Array.to_list (Unix.environment ())
    |> List.filter (fun item ->
      not (List.exists (fun prefix -> String.starts_with ~prefix item)
        ["HOME="; "XDG_CONFIG_HOME="; "XDG_DATA_HOME="; "XDG_STATE_HOME=";
         "XDG_CACHE_HOME="; "OPENAI_API_KEY="; "LM_STUDIO_API_KEY=";
         "DEVIN_API_KEY=";
         "HTTP_PROXY="; "http_proxy="; "HTTPS_PROXY="; "https_proxy=";
         "ALL_PROXY="; "all_proxy="]))
    |> fun inherited -> inherited @ [
      "HOME=" ^ home;
      "XDG_CONFIG_HOME=" ^ Filename.concat home "config";
      "XDG_DATA_HOME=" ^ Filename.concat home "data";
      "XDG_STATE_HOME=" ^ Filename.concat home "state";
      "XDG_CACHE_HOME=" ^ Filename.concat home "cache";
      "LM_STUDIO_API_KEY=fixture-secret";
      "DEVIN_API_KEY=";
      "TERM=dumb";
      "NO_COLOR=1"]
    |> Array.of_list in
  let argv = Array.of_list (binary :: "--root" :: root :: arguments) in
  let pid = Unix.create_process_env binary argv environment stdin stdout stderr in
  Unix.close stdin;
  Unix.close stdout;
  Unix.close stderr;
  before_wait pid;
  let _, status = Unix.waitpid [] pid in
  let result = { status; stdout = read_file stdout_path;
    stderr = read_file stderr_path } in
  List.iter Sys.remove [stdin_path; stdout_path; stderr_path];
  result

let code = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal -> 128 + signal
  | Unix.WSTOPPED signal -> 128 + signal

let base_arguments endpoint = [
  "--provider"; "lm-studio"; "--api"; "chat"; "--model"; "fixture-model";
  "--endpoint"; endpoint]

let user_content request =
  let messages = match member "messages" request with
    | `List messages -> messages
    | _ -> failwith "provider request omitted messages" in
  let user = List.find (fun message ->
    try member "role" message = `String "user" with Not_found -> false) messages in
  member "content" user

let assert_success result label =
  check (code result.status = 0)
    (label ^ " failed: " ^ result.stderr)

let records output =
  String.split_on_char '\n' output
  |> List.filter (fun line -> line <> "")
  |> List.map Yojson.Basic.from_string

let record_type record = member "type" record
let record_status record = member "status" record

let failed_compaction_usage binary root =
  let assistant text : Pave.Protocol.message = {
    role = "assistant"; content = Some text; tool_calls = [];
    tool_call_id = None; tool_result_content = None; provider_state = None;
    attachments = [] } in
  let original = [
    Pave.Protocol.user (String.make 1600 'u'); assistant (String.make 200 'a');
    Pave.Protocol.user (String.make 1600 'v'); assistant (String.make 200 'b')] in
  let completed summary input output cached reasoning =
    response 200 "application/json" (Yojson.Basic.to_string (`Assoc [
      "choices", `List [`Assoc [
        "finish_reason", `String "stop";
        "message", `Assoc ["role", `String "assistant";
          "content", `String summary]]];
      "usage", `Assoc ["prompt_tokens", `Int input;
        "completion_tokens", `Int output;
        "prompt_tokens_details", `Assoc ["cached_tokens", `Int cached];
        "completion_tokens_details", `Assoc ["reasoning_tokens", `Int reasoning]]])) in
  let scenarios = [
    "later-failure", [
      completed "summary-1" 12 3 7 2;
      response 500 "application/json"
        {|{"error":{"message":"summary unavailable"}}|}],
      (12, 3, 7, 2);
    "oversized-summary", [completed (String.make 2049 's') 17 5 11 3],
      (17, 5, 11, 3)] in
  List.iter (fun (name, responses, (input, output, cached, reasoning)) ->
    let path = Filename.concat root (name ^ ".jsonl") in
    let session = Pave.Session.open_file path in
    List.iter (fun message -> ignore (Pave.Session.append session message)) original;
    let result = with_server responses
      (fun index _ request ->
        if index = 1 then
          match member "messages" request with
          | `List [_system; user] ->
              let payload = match member "content" user with
                | `String text -> Yojson.Basic.from_string text
                | _ -> fail "summary request omitted its transcript" in
              check (member "priorSummary" payload = `String "summary-1")
                "second summary request lost the successful first chunk"
          | _ -> fail "unexpected summary request shape")
      (fun endpoint -> run binary root
        (base_arguments endpoint @ ["--session"; path;
          "--context-window"; "8192"; "--prompt"; "latest request"])) in
    check (code result.status = 1)
      ("failed compaction returned success: " ^ result.stderr);
    let reopened = Pave.Session.open_file path in
    check (Pave.Session.history reopened =
      original @ [Pave.Protocol.user "latest request"])
      "failed compaction changed or dropped original conversation history";
    check (not (List.exists (fun (entry : Pave.Session.entry) ->
      match entry.kind with Pave.Session.Compaction _ -> true | _ -> false)
      (Pave.Session.entries reopened)))
      "failed compaction left a phantom context marker";
    let recorded = List.filter_map (fun (entry : Pave.Session.entry) ->
      match entry.kind with
      | Pave.Session.Usage { provider; account_id; route; model; tokens } ->
          check (provider = "lm-studio" && account_id = None &&
            route = Some "chat" && model = "fixture-model")
            "failed compaction lost actual usage provenance";
          Some tokens
      | _ -> None) (Pave.Session.entries reopened) in
    (match recorded with
     | [usage] ->
         check (usage.input_tokens = input && usage.output_tokens = output &&
           usage.cached_input_tokens = Some cached &&
           usage.reasoning_output_tokens = Some reasoning)
           "validated compaction usage was lost, duplicated or altered"
     | _ -> fail "failed compaction did not retain exactly one billed request");
    let again = Pave.Session.open_file path in
    check (Pave.Session.usage again = Pave.Session.usage reopened)
      "resuming failed compaction recorded billed usage again") scenarios

let subagent_admission binary root =
  let tool_reply = Yojson.Basic.to_string (`Assoc [
    "choices", `List [`Assoc ["finish_reason", `String "tool_calls";
      "message", `Assoc ["role", `String "assistant"; "content", `Null;
        "tool_calls", `List [`Assoc ["id", `String "child-request";
          "type", `String "function"; "function", `Assoc [
            "name", `String "task";
            "arguments", `String {|{"label":"review","task":"inspect files"}|}]]]]]]]) in
  let final_reply = Yojson.Basic.to_string (`Assoc [
    "choices", `List [`Assoc ["finish_reason", `String "stop";
      "message", `Assoc ["role", `String "assistant";
        "content", `String "Child request was not executed."]]]]) in
  List.iter (fun (name, enabled, saved) ->
    let path = Filename.concat root (name ^ ".jsonl") in
    let extra = (if enabled then ["--enable-subagents"] else []) @
      (if saved then ["--session"; path] else []) in
    let result = with_server [
      response 200 "application/json" tool_reply;
      response 200 "application/json" final_reply]
      (fun index _ request ->
        if index = 0 then (
          let definitions = match member "tools" request with
            | `List definitions -> definitions | _ -> fail "missing tool roster" in
          let advertised = List.exists (fun definition ->
            member "name" (member "function" definition) = `String "task") definitions in
          check (advertised = (enabled && saved))
            "child capability escaped opt-in or saved-session boundary")
        else
          match member "messages" request with
          | `List messages ->
              check (List.exists (fun message ->
                Pave.Protocol.member "role" message = `String "tool" &&
                Pave.Protocol.member "tool_call_id" message = `String "child-request")
                messages) "denied child call lost its ordered tool result"
          | _ -> fail "missing denied child result")
      (fun endpoint -> run binary root
        (base_arguments endpoint @ extra @
          ["--approval-mode"; "yolo"; "--prompt"; "inspect source"])) in
    check (code result.status = 2)
      "disabled or unapproved headless delegation was reported successful";
    if saved then (
      let session = Pave.Session.open_file path in
      check (not (List.exists (fun (entry : Pave.Session.entry) ->
        match entry.kind with
        | Pave.Session.Job_started _ -> true
        | _ -> false) (Pave.Session.entries session)))
        "unapproved or disabled delegation started a child job"))
    ["single-default", false, true; "child-headless", true, true;
     "child-unsaved", true, false]

let () =
  let root = Filename.temp_file "pave-cli-prompt" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let source_dir = Filename.concat root "src" in
  Unix.mkdir source_dir 0o700;
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    failed_compaction_usage Sys.argv.(1) root;
    subagent_admission Sys.argv.(1) root;
    let piped_prompt = "  /help\nthinkdeep\r\n" in
    let plain = with_server
      [response 200 "application/json"
        {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"ok"}}]}|}]
      (fun index request_line request ->
        check (index = 0 && String.starts_with ~prefix:"POST " request_line)
          "stdin prompt request was not a POST";
        check (user_content request = `String piped_prompt)
          "redirected stdin bytes were changed or parsed as a slash command")
      (fun endpoint -> run Sys.argv.(1) root ~input:piped_prompt
        (base_arguments endpoint)) in
    assert_success plain "redirected prompt";
    check (contains plain.stdout "ok") "provider reply was not printed";

    let prompt_file = Filename.concat source_dir "prompt.txt" in
    let file_prompt = "/help\nkept exactly\n" in
    write_file prompt_file file_prompt;
    let from_file = with_server
      [response 200 "application/json"
        {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"file-ok"}}]}|}]
      (fun _ _ request -> check (user_content request = `String file_prompt)
        "--prompt-file did not preserve the checked file contents")
      (fun endpoint -> run Sys.argv.(1) root
        (base_arguments endpoint @ ["--prompt-file"; "src/prompt.txt"])) in
    assert_success from_file "workspace prompt file";

    let png = "\137PNG\r\n\026\n" in
    write_file (Filename.concat source_dir "photo.png") png;
    let image = with_server
      [response 200 "application/json"
        {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"image-ok"}}]}|}]
      (fun _ _ request ->
        match user_content request with
        | `List [text; image] ->
            check (member "text" text = `String "inspect")
              "image prompt text changed";
            let url = member "url" (member "image_url" image) in
            check (url = `String
              ("data:image/png;base64," ^ Pave.Session_attachment.base64 png))
              "image was not routed as its native MIME/data payload"
        | _ -> fail "image request did not use native chat media content")
      (fun endpoint -> run Sys.argv.(1) root
        (base_arguments endpoint @ ["--prompt"; "inspect";
          "--image"; "src/photo.png"])) in
    assert_success image "native image input";
    check (not (contains image.stdout (Pave.Session_attachment.base64 png)))
      "headless image input printed its base64 payload";

    let shortcut = with_server
      [response 200 "application/json"
        {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"shortcut-ok"}}]}|}]
      (fun _ _ request -> check (user_content request = `String
        "Please reason carefully through this request")
        "opt-in shortcut did not change prose before the provider request")
      (fun endpoint -> run Sys.argv.(1) root
        (base_arguments endpoint @ ["--prompt"; "thinkdeep";
          "--shortcut"; "thinkdeep"])) in
    assert_success shortcut "opt-in prose shortcut";
    check (contains shortcut.stderr "Shortcut expansion: thinkdeep")
      "shortcut expansion was not visible";
    let disabled_shortcut = with_server
      [response 200 "application/json"
        {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"shortcut-ok"}}]}|}]
      (fun _ _ request -> check (user_content request = `String "thinkdeep")
        "explicitly disabled shortcut changed the prompt")
      (fun endpoint -> run Sys.argv.(1) root
        (base_arguments endpoint @ ["--prompt"; "thinkdeep";
          "--shortcut"; "thinkdeep"; "--disable-shortcut"; "thinkdeep"])) in
    assert_success disabled_shortcut "disabled prose shortcut";
    check (not (contains disabled_shortcut.stderr "Shortcut expansion"))
      "disabled shortcut was reported as active";

    let jsonl_response =
      "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"safe\"},\"finish_reason\":null}]}\n\n" ^
      "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\n" ^
      "data: [DONE]\n\n" in
    let jsonl = with_server
      [response 200 "text/event-stream" jsonl_response]
      (fun _ _ request -> check (member "stream" request = `Bool true)
        "JSONL did not request a streamed provider response")
      (fun endpoint -> run Sys.argv.(1) root ~input:"input"
        (base_arguments endpoint @ ["--output"; "jsonl"])) in
    assert_success jsonl "JSONL one-shot";
    check (not (String.contains jsonl.stdout '\027') &&
      not (String.contains jsonl.stdout '\r'))
      "JSONL stdout contained terminal control characters";
    let jsonl_records = records jsonl.stdout in
    check (List.length jsonl_records >= 3) "JSONL omitted turn/text/outcome records";
    List.iteri (fun index record ->
      check (member "turn_id" record = `String "turn-1" &&
        member "sequence" record = `Int (index + 1))
        "JSONL owner or sequence was not stable and monotonic") jsonl_records;
    check (record_type (List.hd jsonl_records) = `String "turn" &&
      List.exists (fun record -> record_type record = `String "text_delta" &&
        member "text" record = `String "safe") jsonl_records &&
      record_type (List.hd (List.rev jsonl_records)) = `String "outcome" &&
      record_status (List.hd (List.rev jsonl_records)) = `String "completed")
      "JSONL success event sequence or final outcome was incorrect";
    let long_text = String.make 4095 'x' ^ "🙂" ^ String.make 1000 'y' in
    let long_event = Yojson.Basic.to_string (`Assoc [
      "choices", `List [`Assoc [
        "index", `Int 0;
        "delta", `Assoc ["content", `String long_text];
        "finish_reason", `Null]]]) in
    let long_stream = "data: " ^ long_event ^ "\n\n" ^
      "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\n" ^
      "data: [DONE]\n\n" in
    let long_jsonl = with_server
      [response 200 "text/event-stream" long_stream]
      (fun _ _ request -> check (member "stream" request = `Bool true)
        "long JSONL response did not stream")
      (fun endpoint -> run Sys.argv.(1) root ~input:"bounded"
        (base_arguments endpoint @ ["--output"; "jsonl"])) in
    assert_success long_jsonl "bounded JSONL text";
    let text_records = records long_jsonl.stdout |> List.filter (fun record ->
      record_type record = `String "text_delta") in
    check (List.for_all (fun record ->
      match member "text" record with
      | `String text -> String.length text <= 4096 &&
          Pave.Session_attachment.valid_utf8 text
      | _ -> false) text_records &&
      String.concat "" (List.map (fun record ->
        match member "text" record with `String text -> text | _ -> "")
        text_records) = long_text ^ "\n")
      "JSONL text chunks exceeded their UTF-8 byte bound";

    let unsafe_text = "\027[31mred\027[0m\nplain" in
    let unsafe_event = Yojson.Basic.to_string (`Assoc [
      "choices", `List [`Assoc [
        "index", `Int 0;
        "delta", `Assoc ["content", `String unsafe_text];
        "finish_reason", `Null]]]) in
    let unsafe_stream = "data: " ^ unsafe_event ^ "\n\n" ^
      "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}\n\n" ^
      "data: [DONE]\n\n" in
    let safe_jsonl = with_server
      [response 200 "text/event-stream" unsafe_stream]
      (fun _ _ _ -> ())
      (fun endpoint -> run Sys.argv.(1) root ~input:"inspect"
        (base_arguments endpoint @ ["--output"; "jsonl"])) in
    assert_success safe_jsonl "control-safe JSONL text";
    let safe_text = records safe_jsonl.stdout
      |> List.filter (fun record -> record_type record = `String "text_delta")
      |> List.map (fun record -> member "text" record) in
    check (not (List.exists (function
      | `String text -> String.contains text '\027'
      | _ -> true) safe_text) &&
      List.exists (function
        | `String text -> contains text "red" && contains text "\nplain"
        | _ -> false) safe_text)
      "JSONL retained terminal escape bytes or dropped ordinary text";

    let tool_call =
      "data: {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call-private\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"missing.txt\\\"}\"}}]},\"finish_reason\":null}]}\n\n" ^
      "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}]}\n\n" ^
      "data: [DONE]\n\n" in
    let tool_result = with_server
      [response 200 "text/event-stream" tool_call;
       response 200 "text/event-stream" jsonl_response]
      (fun _ _ request -> check (member "stream" request = `Bool true)
        "JSONL tool turn did not use streaming")
      (fun endpoint -> run Sys.argv.(1) root ~input:"inspect"
        (base_arguments endpoint @ ["--output"; "jsonl"])) in
    check (code tool_result.status = 2)
      ("tool failure did not produce the distinct tool-error status: " ^
       tool_result.stderr);
    let tool_records = records tool_result.stdout in
    check (List.exists (fun record ->
      record_type record = `String "tool" &&
      member "state" record = `String "started") tool_records &&
      List.exists (fun record ->
        record_type record = `String "tool" &&
        member "state" record = `String "settled" &&
        member "is_error" record = `Bool true) tool_records &&
      record_status (List.hd (List.rev tool_records)) = `String "tool_error")
      "JSONL tool events or tool-error outcome was missing";
    List.iter (fun record ->
      List.iter (fun private_field ->
        check (match record with
          | `Assoc fields -> not (List.mem_assoc private_field fields)
          | _ -> false)
          ("JSONL leaked private tool field " ^ private_field))
        ["call_id"; "arguments"; "result"]) tool_records;
    check (not (contains tool_result.stdout "call-private") &&
      not (contains tool_result.stdout "missing.txt"))
      "JSONL exposed tool-call data";
    List.iteri (fun index record ->
      check (member "turn_id" record = `String "turn-1" &&
        member "sequence" record = `Int (index + 1))
        "tool JSONL records lost ordering or ownership") tool_records;

    let text_tool_result = with_server
      [response 200 "text/event-stream" tool_call;
       response 200 "text/event-stream" jsonl_response]
      (fun _ _ request -> check (member "stream" request = `Bool true)
        "text tool turn did not use streaming")
      (fun endpoint -> run Sys.argv.(1) root
        (base_arguments endpoint @ ["--prompt"; "inspect"; "--stream"])) in
    check (code text_tool_result.status = 2 &&
      contains text_tool_result.stderr "[read_file]" &&
      not (contains text_tool_result.stdout "missing.txt"))
      "text-mode tool failure lacked stderr diagnostics or distinct exit status";

    let shell_call =
      "data: {\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call-shell\",\"type\":\"function\",\"function\":{\"name\":\"run_command\",\"arguments\":\"{\\\"command\\\":\\\"touch forbidden.txt\\\"}\"}}]},\"finish_reason\":null}]}\n\n" ^
      "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}]}\n\n" ^
      "data: [DONE]\n\n" in
    let shell_result = with_server
      [response 200 "text/event-stream" shell_call;
       response 200 "text/event-stream" jsonl_response]
      (fun _ _ request -> check (member "stream" request = `Bool true)
        "headless shell approval fixture did not stream")
      (fun endpoint -> run Sys.argv.(1) root
        (base_arguments endpoint @ ["--prompt"; "inspect"; "--stream";
          "--allow-shell"])) in
    check (code shell_result.status = 2 &&
      not (Sys.file_exists (Filename.concat root "forbidden.txt")) &&
      contains shell_result.stderr "[run_command]")
      "headless shell command escaped interactive approval";

    let malformed_stream =
      "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"partial\"},\"finish_reason\":null}]}\n\n" ^
      "data: {not-json}\n\n" in
    let text_failure = with_server
      [response 200 "text/event-stream" malformed_stream]
      (fun _ _ request -> check (member "stream" request = `Bool true)
        "text failure did not request streaming")
      (fun endpoint -> run Sys.argv.(1) root
        (base_arguments endpoint @ ["--prompt"; "inspect"; "--stream"])) in
    check (code text_failure.status = 1 &&
      contains text_failure.stdout "partial" &&
      contains text_failure.stderr "Error:")
      "partial text stream failure returned success or lost diagnostics";
    let jsonl_failure = with_server
      [response 200 "text/event-stream" malformed_stream]
      (fun _ _ request -> check (member "stream" request = `Bool true)
        "JSONL failure did not request streaming")
      (fun endpoint -> run Sys.argv.(1) root ~input:"inspect"
        (base_arguments endpoint @ ["--output"; "jsonl"])) in
    check (code jsonl_failure.status = 1 &&
      List.exists (fun record ->
        record_type record = `String "text_delta" &&
        member "text" record = `String "partial") (records jsonl_failure.stdout) &&
      record_status (List.hd (List.rev (records jsonl_failure.stdout))) =
        `String "failed")
      "partial JSONL provider failure was not explicit";

    let signal_mutex = Mutex.create () in
    let request_seen = ref false and release_response = ref false in
    let before_reply _ =
      Mutex.lock signal_mutex;
      request_seen := true;
      while not !release_response do
        Mutex.unlock signal_mutex;
        Thread.delay 0.01;
        Mutex.lock signal_mutex
      done;
      Mutex.unlock signal_mutex in
    let interrupt pid =
      let deadline = Unix.gettimeofday () +. 5. in
      let rec wait_request () =
        Mutex.lock signal_mutex;
        let seen = !request_seen in
        Mutex.unlock signal_mutex;
        if seen then true
        else if Unix.gettimeofday () >= deadline then false
        else (Thread.delay 0.01; wait_request ()) in
      if not (wait_request ()) then (
        Mutex.lock signal_mutex;
        release_response := true;
        Mutex.unlock signal_mutex;
        fail "fake provider did not receive the cancellation turn");
      Unix.kill pid Sys.sigint;
      Thread.delay 0.1;
      Mutex.lock signal_mutex;
      release_response := true;
      Mutex.unlock signal_mutex in
    let cancelled = with_server ~before_reply
      [response 200 "text/event-stream" jsonl_response]
      (fun _ _ request -> check (member "stream" request = `Bool true)
        "cancellation turn did not use JSONL streaming")
      (fun endpoint -> run Sys.argv.(1) root ~input:"cancel me"
        ~before_wait:interrupt
        (base_arguments endpoint @ ["--output"; "jsonl"])) in
    let cancelled_records = records cancelled.stdout in
    check (code cancelled.status = 130 &&
      record_status (List.hd (List.rev cancelled_records)) = `String "cancelled" &&
      contains cancelled.stderr "Cancelled")
      "interrupted JSONL turn did not report the cancellation outcome";
    Mutex.lock signal_mutex;
    request_seen := false;
    release_response := false;
    Mutex.unlock signal_mutex;
    let text_cancelled = with_server ~before_reply
      [response 200 "text/event-stream" jsonl_response]
      (fun _ _ request -> check (member "stream" request = `Bool true)
        "text cancellation turn did not stream")
      (fun endpoint -> run Sys.argv.(1) root
        ~before_wait:interrupt
        (base_arguments endpoint @ ["--prompt"; "cancel me"; "--stream"])) in
    check (code text_cancelled.status = 130 &&
      contains text_cancelled.stderr "Cancelled")
      "interrupted text turn did not use the cancellation exit status";
    

    let conflict = run Sys.argv.(1) root ~input:"also-piped"
      (base_arguments "http://127.0.0.1:1/v1/chat/completions" @
       ["--prompt"; "explicit"]) in
    check (code conflict.status <> 0 &&
      contains conflict.stderr "conflicts")
      "conflicting prompt and stdin sources were not rejected";
    write_file (Filename.concat source_dir "binary.txt") "\000not text";
    let binary_file = run Sys.argv.(1) root
      (base_arguments "http://127.0.0.1:1/v1/chat/completions" @
       ["--prompt-file"; "src/binary.txt"]) in
    check (code binary_file.status <> 0 &&
      contains binary_file.stderr "without NUL bytes")
      "binary workspace prompt file reached provider setup";
    write_file (Filename.concat source_dir "oversized.txt")
      (String.make (Pave.Workspace_path.max_read_bytes + 1) 'x');
    let oversized_file = run Sys.argv.(1) root
      (base_arguments "http://127.0.0.1:1/v1/chat/completions" @
       ["--prompt-file"; "src/oversized.txt"]) in
    check (code oversized_file.status <> 0 &&
      contains oversized_file.stderr "file exceeds")
      "oversized workspace prompt file was not rejected";
    let file_conflict = run Sys.argv.(1) root ~input:"piped too"
      (base_arguments "http://127.0.0.1:1/v1/chat/completions" @
       ["--prompt-file"; "src/prompt.txt"]) in
    check (code file_conflict.status <> 0 &&
      contains file_conflict.stderr "conflicts with --prompt-file")
      "prompt-file and nonempty stdin sources were not rejected";
    let oversized_stdin = run Sys.argv.(1) root
      ~input:(String.make 1_048_577 'x')
      (base_arguments "http://127.0.0.1:1/v1/chat/completions") in
    check (code oversized_stdin.status <> 0 &&
      contains oversized_stdin.stderr "stdin prompt exceeds 1048576-byte limit")
      "oversized redirected stdin was not bounded";
    let empty = run Sys.argv.(1) root
      (base_arguments "http://127.0.0.1:1/v1/chat/completions") in
    check (code empty.status <> 0 &&
      String.starts_with ~prefix:"Error: redirected stdin prompt is empty"
        empty.stderr)
      "empty redirected stdin was accepted";
    let unsupported = run Sys.argv.(1) root
      ["--provider"; "devin"; "--api"; "connect"; "--model"; "fixture-model";
       "--prompt"; "inspect"; "--image"; "src/photo.png"] in
    check (code unsupported.status <> 0 &&
      String.starts_with ~prefix:"Error: this provider route does not support user media attachments"
        unsupported.stderr)
      ("unsupported media was not rejected before provider authentication: " ^
       unsupported.stderr);
    print_endline "CLI prompt, file/media input, JSONL, and preflight verified")
