let active_mask = Pave.Secret_mask.create
  ["mobile agent"; "private prompt"; "App.swift"; "struct App {}";
   "Swift file inspected."; "command not approved"]

let tool_reply = {|{"choices":[{"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"tool_calls":[{"id":"call-1","type":"function","function":{"name":"read_file","arguments":"{\"path\":\"App.swift\"}"}}]}}]}|}
let final_reply = {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"Swift file inspected.","tool_calls":[]}}]}|}
let shell_reply = {|{"choices":[{"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"tool_calls":[{"id":"call-2","type":"function","function":{"name":"run_command","arguments":"{\"command\":\"touch MUST_NOT_EXIST\"}"}}]}}]}|}

let serve client step =
  let ic = Unix.in_channel_of_descr client in
  let oc = Unix.out_channel_of_descr client in
  let length = ref 0 in
  let rec headers () =
    let line = input_line ic in
    if line <> "\r" && line <> "" then (
      let lower = String.lowercase_ascii line in
      if String.starts_with ~prefix:"content-length:" lower then
        length := int_of_string (String.trim (String.sub line 15 (String.length line - 15)));
      headers ()) in
  headers ();
  let request = Yojson.Basic.from_string (really_input_string ic !length) in
  let tools = Pave.Protocol.member "tools" request in
  (match tools with
   | `List schemas ->
       let has_shell = List.exists (fun schema ->
         Pave.Protocol.member "name" (Pave.Protocol.member "function" schema)
         = `String "run_command") schemas in
       assert (has_shell = (step >= 2))
   | _ -> failwith "missing tools");
  (match Pave.Protocol.member "messages" request with
   | `List messages when step = 0 ->
       let system = List.find (fun msg ->
         Pave.Protocol.member "role" msg = `String "system") messages in
       let user = List.find (fun msg ->
         Pave.Protocol.member "role" msg = `String "user") messages in
       let system_text = Pave.Protocol.member "content" system in
       let user_text = Pave.Protocol.member "content" user in
       assert (system_text = `String (Pave.Secret_mask.mask active_mask "mobile agent"));
       assert (user_text = `String (Pave.Secret_mask.mask active_mask "private prompt"))
   | `List messages when step = 1 ->
       assert (List.exists (fun msg ->
         Pave.Protocol.member "tool_call_id" msg = `String "call-1"
         && (match Pave.Protocol.member "content" msg with
            | `String text ->
                String.starts_with ~prefix:(Pave.Secret_mask.mask active_mask
                  "struct App {}\n") text
            | _ -> false)) messages)
   | `List messages when step = 3 ->
       assert (List.exists (fun msg ->
         Pave.Protocol.member "tool_call_id" msg = `String "call-2"
         && (match Pave.Protocol.member "content" msg with
             | `String text -> String.starts_with ~prefix:"Error:" text
             | _ -> false)) messages)
   | _ -> ());
  let body = match step with
    | 0 -> tool_reply | 2 -> shell_reply | _ -> final_reply in
  Printf.fprintf oc "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s" (String.length body) body;
  flush oc;
  close_in_noerr ic; close_out_noerr oc

let contains text needle =
  let n = String.length needle and length = String.length text in
  let rec search i = i + n <= length &&
    (String.sub text i n = needle || search (i + 1)) in
  search 0

let test_secret_mask () =
  let mask = Pave.Secret_mask.create ["abc"; "abcd"] in
  let masked = Pave.Secret_mask.mask mask "abcd abc" in
  assert (not (contains masked "abcd"));
  assert (not (contains masked "abc"));
  assert (Pave.Secret_mask.restore_tool_arguments mask (`String masked)
    = `String "abcd abc");
  let overlap_mask = Pave.Secret_mask.create ["secret"; "long-secret"] in
  let overlap = Pave.Secret_mask.mask overlap_mask "long-secret" in
  assert (not (contains overlap "long-secret"));
  assert (not (contains overlap "secret"));
  assert (Pave.Secret_mask.mask overlap_mask overlap = overlap);
  assert (Pave.Secret_mask.restore_tool_arguments overlap_mask
    (`String overlap) = `String "long-secret");
  let evolving = Pave.Secret_mask.create ["first-token"] in
  ignore (Pave.Secret_mask.mask evolving "first-token");
  Pave.Secret_mask.add evolving ["rotated-token"];
  assert (Pave.Secret_mask.redact evolving "rotated-token" = "[redacted]");
  let rotated = Pave.Secret_mask.mask evolving "rotated-token" in
  assert (Pave.Secret_mask.restore_tool_arguments evolving (`String rotated)
    = `String "rotated-token");
  let secret = "collision-secret" in
  let collision_mask = Pave.Secret_mask.create [secret] in
  let base = Pave.Secret_mask.mask collision_mask secret in
  let text = base ^ " " ^ secret in
  let masked = Pave.Secret_mask.mask collision_mask text in
  assert (String.starts_with ~prefix:(base ^ " ") masked);
  assert (not (contains masked secret));
  let placeholder = Pave.Secret_mask.mask collision_mask secret in
  let args = `Assoc [placeholder, `String placeholder;
    "nested", `List [`String placeholder]] in
  (match Pave.Secret_mask.restore_tool_arguments collision_mask args with
   | `Assoc [(key, `String restored); ("nested", `List [`String nested])] ->
       assert (key = placeholder && restored = secret && nested = secret)
   | _ -> assert false);
  let raw_args = `Assoc ["path", `String secret;
    "nested", `List [`String ("before " ^ secret)]] in
  let masked_args = Pave.Secret_mask.mask_tool_arguments collision_mask raw_args in
  assert (not (contains (Yojson.Basic.to_string masked_args) secret));
  assert (Pave.Secret_mask.restore_tool_arguments collision_mask masked_args
    = raw_args);
  let safe = Pave.Secret_mask.redact collision_mask secret in
  assert (safe = "[redacted]");
  assert (Pave.Secret_mask.redact collision_mask ("Error: " ^ secret) =
    "Error: [redacted]");
  assert (Pave.Secret_mask.redact (Pave.Secret_mask.create []) secret = secret)
let () =
  test_secret_mask ();
  let root = Filename.temp_file "pave-agent-" "" in
  Sys.remove root; Unix.mkdir root 0o700;
  let file = Filename.concat root "App.swift" in
  let oc = open_out file in output_string oc "struct App {}\n"; close_out oc;
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 4;
  let port = match Unix.getsockname socket with Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    List.iter (fun secret -> ignore (Pave.Secret_mask.mask active_mask secret))
      ["private prompt"; "mobile agent"; "App.swift"; "struct App {}\n"];
    (try for step = 0 to 3 do
      let client, _ = Unix.accept socket in serve client step
    done with exn -> prerr_endline (Printexc.to_string exn); exit 2);
    exit 0);
  Unix.close socket;
  Fun.protect ~finally:(fun () ->
    (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
    ignore (Unix.waitpid [] child);
    Sys.remove file; Unix.rmdir root) (fun () ->
    let provider : Pave.Provider.config = {
      endpoint = Printf.sprintf "http://127.0.0.1:%d/v1/chat/completions" port;
      api_key = "test-key"; model = "mock"; api = Pave.Provider.Openai_completions } in
    let events = ref [] and persisted = ref [] in
    let assert_safe text =
      List.iter (fun secret -> assert (not (contains text secret)))
        ["mobile agent"; "private prompt"; "App.swift"; "struct App {}";
         "Swift file inspected."; "command not approved"] in
    let agent = Pave.Agent.create ~provider ~root ~system:"mobile agent"
      ~secret_mask:active_mask
      ~before_request:(fun ~cancel:_ ~system ~messages ~tools:_ ->
        assert_safe system;
        List.iter (fun (message : Pave.Protocol.message) ->
          Option.iter assert_safe message.content;
          Option.iter (List.iter (function
            | Pave.Protocol.Text text -> assert_safe text
            | Pave.Protocol.Image _ -> ())) message.tool_result_content;
          List.iter (fun (call : Pave.Protocol.tool_call) ->
            assert_safe (Yojson.Basic.to_string call.arguments))
            message.tool_calls) messages;
        Some messages)
      ~on_change:(fun message -> persisted := message :: !persisted)
      ~on_event:(fun message -> events := message :: !events) () in
    assert (Pave.Agent.run agent "private prompt" = "[redacted]");
    let messages = Pave.Agent.messages agent in
    assert (List.length messages = 4);
    assert (List.exists (fun msg -> msg.Pave.Protocol.tool_call_id = Some "call-1") messages);
    let call_message = List.find (fun msg ->
      msg.Pave.Protocol.role = "assistant" &&
      msg.Pave.Protocol.tool_calls <> []) messages in
    let call = List.hd call_message.Pave.Protocol.tool_calls in
    assert (not (contains (Yojson.Basic.to_string call.arguments) "App.swift"));
    assert (Pave.Secret_mask.restore_tool_arguments active_mask call.arguments =
      `Assoc ["path", `String "App.swift"]);
    let user_message = List.find (fun msg ->
      msg.Pave.Protocol.role = "user") messages in
    assert (user_message.Pave.Protocol.content =
      Some (Pave.Secret_mask.mask active_mask "private prompt"));
    List.iter (fun (message : Pave.Protocol.message) ->
      Option.iter assert_safe message.content;
      List.iter (fun (call : Pave.Protocol.tool_call) ->
        assert_safe (Yojson.Basic.to_string call.arguments)) message.tool_calls)
      !persisted;
    assert (not (List.mem "Swift file inspected." !events));
    assert (List.mem "[redacted]" !events);
    assert (List.exists (fun msg ->
      msg.Pave.Protocol.role = "assistant" &&
      msg.Pave.Protocol.content = Some (Pave.Secret_mask.mask active_mask
        "Swift file inspected.") &&
      not (contains (Option.value msg.Pave.Protocol.content ~default:"")
        "Swift file inspected.")) messages);
    let shell_events = ref [] in
    let shell_agent = Pave.Agent.create ~provider ~root ~system:"mobile agent"
      ~secret_mask:active_mask ~allow_shell:true ~approve_command:(fun _ -> false)
      ~on_event:(fun message -> shell_events := message :: !shell_events) () in
    assert (Pave.Agent.run shell_agent "Run test" = "[redacted]");
    assert (List.exists (fun message ->
      contains message "Error:" && contains message "[redacted]") !shell_events);
    assert (not (Sys.file_exists (Filename.concat root "MUST_NOT_EXIST"))));
  print_endline "agent loop: ok"
