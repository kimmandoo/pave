module Dap = Pave.Workspace_dap

let fail label = failwith ("workspace DAP: " ^ label)
let expect label condition = if not condition then fail label

let expect_error label action =
  match action () with
  | _ -> fail (label ^ " was accepted")
  | exception Dap.Error _ -> ()

let expect_denied label action =
  match action () with
  | _ -> fail (label ^ " was approved")
  | exception Dap.Not_approved _ -> ()

let root_with_target () =
  let root = Filename.temp_file "pave-workspace-dap-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  let root = Unix.realpath root in
  let target = Filename.concat root "main.py" in
  let output = open_out_bin target in
  output_string output "print('dap')\n";
  close_out output;
  root, target

let cleanup_root (root, target) =
  (try Unix.unlink target with _ -> ());
  (try Unix.rmdir root with _ -> ())

let frame message =
  let body = Yojson.Basic.to_string message in
  Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body

let body_of_frame frame =
  match String.index_opt frame '\n' with
  | None -> fail "test mock received a malformed request frame"
  | Some _ ->
      let marker = "\r\n\r\n" in
      let rec find index =
        if index + String.length marker > String.length frame then
          fail "test mock received a malformed request frame"
        else if String.sub frame index (String.length marker) = marker then index
        else find (index + 1)
      in
      let start = find 0 + String.length marker in
      Yojson.Basic.from_string
        (String.sub frame start (String.length frame - start))

let json_member key = function
  | `Assoc fields -> (match List.assoc_opt key fields with Some value -> value | None -> `Null)
  | _ -> `Null

let json_int = function `Int value -> value | _ -> fail "test request had no integer"
let json_string = function `String value -> value | _ -> fail "test request had no string"
let python3 () =
  let path_dirs = match Sys.getenv_opt "PATH" with
    | None -> []
    | Some path -> String.split_on_char ':' path |> List.filter (( <> ) "") in
  let candidates =
    ["/usr/bin/python3"; "/usr/local/bin/python3"; "/opt/homebrew/bin/python3"] @
    List.map (fun directory -> Filename.concat directory "python3") path_dirs in
  let executable path =
    Sys.file_exists path &&
    try Unix.access path [Unix.X_OK]; true with Unix.Unix_error _ -> false in
  match List.find_opt executable candidates with
  | Some path -> path
  | None -> fail "Python 3 is required for the stdio DAP regression"

type mock = {
  transport : Dap.transport;
  sent_frames : string list ref;
  close_count : int ref;
}

let mock ?(fragment = false) responder =
  let sent_frames = ref [] and close_count = ref 0 in
  let chunks = Queue.create () and adapter_seq = ref 0 in
  let emit (kind, fields) =
    incr adapter_seq;
    `Assoc (["seq", `Int !adapter_seq; "type", `String kind] @ fields)
    |> frame
  in
  let push response_frames =
    let bytes = String.concat "" response_frames in
    if fragment && String.length bytes > 2 then (
      let first = min 7 (String.length bytes) in
      let second = min (first + 13) (String.length bytes) in
      Queue.add (String.sub bytes 0 first) chunks;
      Queue.add (String.sub bytes first (second - first)) chunks;
      Queue.add (String.sub bytes second (String.length bytes - second)) chunks)
    else if bytes <> "" then Queue.add bytes chunks
  in
  let transport = {
    Dap.send = (fun ~timeout_seconds:_ ~cancelled:_ request_frame ->
      sent_frames := request_frame :: !sent_frames;
      let request = body_of_frame request_frame in
      let outgoing_seq = json_int (json_member "seq" request) in
      let command = json_string (json_member "command" request) in
      let responses = responder ~emit ~request ~outgoing_seq ~command in
      push responses);
    receive = (fun ~timeout_seconds:_ ~cancelled ->
      if cancelled () || Queue.is_empty chunks then None else Some (Queue.take chunks));
    close = (fun () -> incr close_count);
  } in
  { transport; sent_frames; close_count }

let response emit request_seq command ?(success = true) ?message body =
  emit ("response", [
    "request_seq", `Int request_seq; "command", `String command;
    "success", `Bool success;
    "body", body
  ] @ (match message with None -> [] | Some value -> ["message", `String value]))

let event emit name body = emit ("event", ["event", `String name; "body", body])

let standard_responder_with_path malicious_stack_path ~emit ~request ~outgoing_seq ~command =
  let answer ?(events = []) body =
    let event_frames =
      List.map (fun (name, value) -> event emit name value) events in
    let response_frame = response emit outgoing_seq command body in
    event_frames @ [response_frame]
  in
  match command with
  | "initialize" ->
      let initialized = event emit "initialized" (`Assoc []) in
      let response = response emit outgoing_seq command
          (`Assoc ["supportsConfigurationDoneRequest", `Bool true]) in
      [initialized; response]
  | "configurationDone" ->
      answer ~events:["stopped", `Assoc ["reason", `String "entry"]] (`Assoc [])
  | "setBreakpoints" ->
      let breakpoints = json_member "breakpoints" (json_member "arguments" request) in
      let count = match breakpoints with `List values -> List.length values | _ -> 0 in
      answer (`Assoc ["breakpoints", `List (List.init count (fun _ ->
        `Assoc ["verified", `Bool true; "line", `Int 1]))])
  | "threads" -> answer ~events:["stopped", `Assoc ["reason", `String "pause"]] (`Assoc ["threads", `List [`Assoc ["id", `Int 7; "name", `String "main"]]])
  | "stackTrace" ->
      let path = if malicious_stack_path then "../secret" else "main.py" in
      answer (`Assoc ["stackFrames", `List [`Assoc [
        "id", `Int 11; "name", `String "entry"; "line", `Int 1;
        "column", `Int 1; "source", `Assoc ["name", `String "main.py"; "path", `String path]
      ]]; "totalFrames", `Int 1])
  | "scopes" -> answer (`Assoc ["scopes", `List [`Assoc [
      "name", `String "Locals"; "variablesReference", `Int 23; "expensive", `Bool false
    ]]])
  | "variables" -> answer (`Assoc ["variables", `List [`Assoc [
      "name", `String "answer"; "value", `String "42"; "variablesReference", `Int 0
    ]]])
  | "evaluate" -> answer (`Assoc ["result", `String "42"; "variablesReference", `Int 0])
  | "continue" -> answer ~events:["continued", `Assoc ["threadId", `Int 7]] (`Assoc [])
  | "next" | "stepIn" | "stepOut" ->
      answer ~events:["stopped", `Assoc ["reason", `String "step"]] (`Assoc [])
  | _ -> answer (`Assoc [])
let standard_responder ~emit ~request ~outgoing_seq ~command =
  standard_responder_with_path false ~emit ~request ~outgoing_seq ~command

let manager root =
  Dap.create_manager ~owner:"owner-1" ~workspace_root:root ~authorize:(fun _ -> ())
let initialize_and_launch dap_manager ~id =
  ignore (Dap.initialize dap_manager ~id ~adapter_id:"python");
  ignore (Dap.launch dap_manager ~id ~target:"main.py" ~arguments:["--fixture"]);
  ignore (Dap.configuration_done dap_manager ~id)
let test_stdio_transport_framing_lifecycle_and_environment () =
  let fixture = root_with_target () in
  let names = ["PAVE_WORKSPACE_DAP_SECRET"; "OPENAI_API_KEY"] in
  let previous = List.map (fun name -> name, Sys.getenv_opt name) names in
  List.iter (fun name -> Unix.putenv name "must-not-reach-debug-adapter") names;
  Fun.protect ~finally:(fun () ->
    List.iter (fun (name, value) -> Unix.putenv name (Option.value value ~default:"")) previous;
    cleanup_root fixture) (fun () ->
    let root, _ = fixture in
    let effects = ref [] in
    let dap_manager = Dap.create_manager ~owner:"stdio-owner" ~workspace_root:root
        ~authorize:(fun authorization -> effects := authorization :: !effects) in
    let script =
      "import json, os, sys\n" ^
      "header = sys.stdin.buffer.readline()\n" ^
      "length = int(header.split(b':', 1)[1])\n" ^
      "sys.stdin.buffer.readline()\n" ^
      "request = json.loads(sys.stdin.buffer.read(length))\n" ^
      "body = {'argv': sys.argv[1], 'cwd': os.getcwd(), 'pid': os.getpid(), " ^
      "'pave': os.getenv('PAVE_WORKSPACE_DAP_SECRET'), " ^
      "'provider': os.getenv('OPENAI_API_KEY')}\n" ^
      "payload = json.dumps({'seq': 1, 'type': 'response', " ^
      "'request_seq': request['seq'], 'command': request['command'], " ^
      "'success': True, 'body': body}, separators=(',', ':')).encode()\n" ^
      "sys.stdout.buffer.write(b'Content-Length: ' + str(len(payload)).encode() + " ^
      "b'\\r\\n\\r\\n' + payload)\n" ^
      "sys.stdout.buffer.flush()\n" in
    let transport_ref = ref None in
    Fun.protect ~finally:(fun () ->
      Dap.close_session dap_manager ~id:"stdio";
      Dap.close_manager dap_manager;
      Option.iter (fun transport -> transport.Dap.close ()) !transport_ref) (fun () ->
      let stdio = Dap.stdio_transport dap_manager ~program:(python3 ())
          ~arguments:["-u"; "-c"; script; "literal ; argv"] ~cwd:root in
      transport_ref := Some stdio;
      ignore (Dap.create_session dap_manager ~id:"stdio" ~transport:stdio);
      let response = Dap.initialize dap_manager ~id:"stdio" ~adapter_id:"python" in
      expect "stdio transport preserves exact argv"
        (json_member "argv" response = `String "literal ; argv");
      expect "stdio transport uses canonical workspace cwd"
        (json_member "cwd" response = `String root);
      expect "stdio child receives no Pave secret"
        (json_member "pave" response = `Null);
      expect "stdio child receives no provider credential"
        (json_member "provider" response = `Null);
      expect "stdio adapter process has its independent approval effect"
        (!effects = [Dap.Adapter_process]);
      let pid = json_int (json_member "pid" response) in
      Dap.close_session dap_manager ~id:"stdio";
      let gone = try Unix.kill pid 0; false
        with Unix.Unix_error (Unix.ESRCH, _, _) -> true in
      expect "stdio close terminates and reaps the child" gone))

let sent_count mock = List.length !(mock.sent_frames)
let sent_requests mock =
  List.rev !(mock.sent_frames)
  |> List.map (fun sent ->
       let request = body_of_frame sent in
       json_int (json_member "seq" request), json_string (json_member "command" request))

let test_fragmented_flow_and_transitions () =
  let fixture = root_with_target () in
  Fun.protect ~finally:(fun () -> cleanup_root fixture) (fun () ->
    let root, _ = fixture in
    let effects = ref [] in
    let dap_manager = Dap.create_manager ~owner:"owner-1" ~workspace_root:root
        ~authorize:(fun authorization -> effects := authorization :: !effects) in
    let adapter = mock ~fragment:true standard_responder in
    ignore (Dap.create_session dap_manager ~id:"flow" ~transport:adapter.transport);
    initialize_and_launch dap_manager ~id:"flow";
    let events = Dap.take_events dap_manager ~id:"flow" in
    expect "fragmented initialized and stopped events retained" (List.length events = 2);
    ignore (Dap.set_breakpoints dap_manager ~id:"flow" ~source:"main.py"
      ~breakpoints:[{ Dap.line = 1; condition = None; hit_condition = None; log_message = None }]);
    let threads = Dap.threads dap_manager ~id:"flow" in
    expect "threads response is available" (json_member "threads" threads <> `Null);
    let stack = Dap.stack_trace dap_manager ~id:"flow" ~thread_id:7 () in
    expect "stack trace response is available" (json_member "stackFrames" stack <> `Null);
    ignore (Dap.scopes dap_manager ~id:"flow" ~frame_id:11);
    ignore (Dap.variables dap_manager ~id:"flow" ~reference:23 ());
    ignore (Dap.evaluate dap_manager ~id:"flow" ~expression:"answer" ~frame_id:(Some 11));
    ignore (Dap.continue_ dap_manager ~id:"flow" ~thread_id:7 ());
    ignore (Dap.threads dap_manager ~id:"flow");
    ignore (Dap.next dap_manager ~id:"flow" ~thread_id:7 ());
    ignore (Dap.step_in dap_manager ~id:"flow" ~thread_id:7 ());
    ignore (Dap.step_out dap_manager ~id:"flow" ~thread_id:7 ());
    expect "DAP request sequence IDs are correlated and monotonic"
      (sent_requests adapter = List.mapi (fun index (_, command) -> index + 1, command)
         (sent_requests adapter));
    expect "all DAP requests used expected commands"
      (List.map snd (sent_requests adapter) =
       ["initialize"; "launch"; "configurationDone"; "setBreakpoints"; "threads";
        "stackTrace"; "scopes"; "variables"; "evaluate"; "continue"; "threads";
        "next"; "stepIn"; "stepOut"]);
    expect "separate boundary effects are observable"
      (List.mem Dap.Launch !effects && List.mem Dap.Breakpoints !effects &&
       List.mem Dap.Evaluate !effects && List.mem Dap.Debug_execution !effects);
    ignore (Dap.disconnect dap_manager ~id:"flow");
    expect "disconnect closes the transport exactly once" (!(adapter.close_count) = 1);
    expect_error "disconnected session is removed"
      (fun () -> Dap.threads dap_manager ~id:"flow");
    Dap.close_manager dap_manager)

let test_denied_effects_do_not_send () =
  let fixture = root_with_target () in
  Fun.protect ~finally:(fun () -> cleanup_root fixture) (fun () ->
    let root, _ = fixture in
    let denied_launch = ref true and denied_eval = ref true and denied_write = ref true in
    let denied_execution = ref false in
    let effects = ref [] in
    let authorize authorization =
      effects := authorization :: !effects;
      match authorization with
      | Dap.Launch when !denied_launch -> raise (Dap.Not_approved "launch denied")
      | Dap.Evaluate when !denied_eval -> raise (Dap.Not_approved "evaluate denied")
      | Dap.Breakpoints when !denied_write -> raise (Dap.Not_approved "breakpoints denied")
      | Dap.Debug_execution when !denied_execution ->
          raise (Dap.Not_approved "execution denied")
      | _ -> () in
    let dap_manager = Dap.create_manager ~owner:"owner-1" ~workspace_root:root ~authorize in
    let adapter = mock standard_responder in
    ignore (Dap.create_session dap_manager ~id:"denials" ~transport:adapter.transport);
    ignore (Dap.initialize dap_manager ~id:"denials" ~adapter_id:"python");
    let after_initialize = sent_count adapter in
    expect_denied "launch approval" (fun () ->
      Dap.launch dap_manager ~id:"denials" ~target:"main.py" ~arguments:[]);
    expect "denied launch wrote no DAP frame" (sent_count adapter = after_initialize);
    denied_launch := false;
    ignore (Dap.launch dap_manager ~id:"denials" ~target:"main.py" ~arguments:[]);
    ignore (Dap.configuration_done dap_manager ~id:"denials");
    let after_setup = sent_count adapter in
    expect_denied "breakpoint mutation approval" (fun () ->
      Dap.set_breakpoints dap_manager ~id:"denials" ~source:"main.py"
        ~breakpoints:[{ Dap.line = 1; condition = None; hit_condition = None; log_message = None }]);
    expect "denied breakpoint mutation wrote no DAP frame" (sent_count adapter = after_setup);
    (* Inspection is permitted independently from evaluate and execution. *)
    let effect_count = List.length !effects in
    ignore (Dap.threads dap_manager ~id:"denials");
    expect "thread inspection did not request approval" (List.length !effects = effect_count);
    let after_inspection = sent_count adapter in
    expect_denied "evaluate approval" (fun () ->
      Dap.evaluate dap_manager ~id:"denials" ~expression:"answer" ~frame_id:None);
    expect "denied evaluate wrote no DAP frame" (sent_count adapter = after_inspection);
    denied_execution := true;
    expect_denied "debug execution approval" (fun () ->
      Dap.continue_ dap_manager ~id:"denials" ~thread_id:7 ());
    expect "denied execution wrote no DAP frame" (sent_count adapter = after_inspection);
    Dap.close_manager dap_manager)

let test_failed_execution_keeps_stopped_state () =
  let fixture = root_with_target () in
  Fun.protect ~finally:(fun () -> cleanup_root fixture) (fun () ->
    let root, _ = fixture in
    let dap_manager = manager root in
    let responder ~emit ~request ~outgoing_seq ~command =
      if command = "continue" then
        [response emit outgoing_seq command ~success:false ~message:"continue rejected" (`Assoc [])]
      else standard_responder ~emit ~request ~outgoing_seq ~command in
    let adapter = mock responder in
    ignore (Dap.create_session dap_manager ~id:"failed-step" ~transport:adapter.transport);
    initialize_and_launch dap_manager ~id:"failed-step";
    expect_error "failed continue response" (fun () ->
      Dap.continue_ dap_manager ~id:"failed-step" ~thread_id:7 ());
    ignore (Dap.stack_trace dap_manager ~id:"failed-step" ~thread_id:7 ());
    expect "failed execution did not close the DAP session" (!(adapter.close_count) = 0);
    Dap.close_manager dap_manager)

let test_remote_host_trust_is_separate () =
  let fixture = root_with_target () in
  Fun.protect ~finally:(fun () -> cleanup_root fixture) (fun () ->
    let root, _ = fixture in
    let denied_host = ref true and effects = ref [] in
    let authorize authorization =
      match authorization with
      | Dap.Remote_host _ when !denied_host -> raise (Dap.Not_approved "host denied")
      | _ -> effects := authorization :: !effects in
    let dap_manager = Dap.create_manager ~owner:"owner-1" ~workspace_root:root ~authorize in
    let adapter = mock standard_responder in
    let connect_count = ref 0 in
    ignore (Dap.create_remote_session dap_manager ~id:"remote" ~host:"debug.example"
      ~transport_factory:(fun ~timeout_seconds:_ ~cancelled:_ ->
        incr connect_count;
        adapter.transport));
    expect_error "remote initialization before host consent" (fun () ->
      Dap.initialize dap_manager ~id:"remote" ~adapter_id:"python");
    expect "untrusted remote host caused no connection or I/O"
      (sent_count adapter = 0 && !connect_count = 0);
    expect_denied "remote host trust approval" (fun () ->
      Dap.trust_attach_host dap_manager ~id:"remote" ~host:"DEBUG.EXAMPLE");
    expect "denied host trust caused no connection or consent state"
      (sent_count adapter = 0 && !connect_count = 0 && !effects = []);
    denied_host := false;
    Dap.trust_attach_host dap_manager ~id:"remote" ~host:"DEBUG.EXAMPLE";
    expect "host consent is distinct from launch approval"
      (!effects = [Dap.Remote_host "debug.example"]);
    expect "approved host trust still does not connect eagerly" (!connect_count = 0);
    ignore (Dap.initialize dap_manager ~id:"remote" ~adapter_id:"python");
    expect "remote transport connects only after host trust" (!connect_count = 1);
    ignore (Dap.attach dap_manager ~id:"remote" ~host:"debug.example" ~port:5678);
    ignore (Dap.configuration_done dap_manager ~id:"remote");
    expect "attach sent only after host consent and without launch"
      (List.map snd (sent_requests adapter) = ["initialize"; "attach"; "configurationDone"] &&
       not (List.mem Dap.Launch !effects));
    Dap.close_manager dap_manager)

let test_cancel_close_and_session_isolation () =
  let fixture = root_with_target () in
  Fun.protect ~finally:(fun () -> cleanup_root fixture) (fun () ->
    let root, _ = fixture in
    let dap_manager = manager root in
    let canceled_adapter = mock standard_responder in
    ignore (Dap.create_session dap_manager ~id:"cancelled" ~transport:canceled_adapter.transport);
    initialize_and_launch dap_manager ~id:"cancelled";
    let calls = ref 0 in
    let cancel () = incr calls; !calls >= 3 in
    (match Dap.threads ~cancel dap_manager ~id:"cancelled" with
     | _ -> fail "in-flight cancellation was ignored"
     | exception Dap.Cancelled -> ());
    expect "cancel closes its adapter transport" (!(canceled_adapter.close_count) = 1);
    expect_error "cancelled session is no longer addressable"
      (fun () -> Dap.threads dap_manager ~id:"cancelled");

    let left = mock standard_responder and right = mock standard_responder in
    ignore (Dap.create_session dap_manager ~id:"left" ~transport:left.transport);
    ignore (Dap.create_session dap_manager ~id:"right" ~transport:right.transport);
    ignore (Dap.initialize dap_manager ~id:"left" ~adapter_id:"python");
    ignore (Dap.initialize dap_manager ~id:"right" ~adapter_id:"python");
    expect "independent sessions have independent request sequences"
      (sent_requests left = [1, "initialize"] && sent_requests right = [1, "initialize"]);
    Dap.close_session dap_manager ~id:"left";
    ignore (Dap.launch dap_manager ~id:"right" ~target:"main.py" ~arguments:[]);
    expect "closing one session leaves its sibling usable"
      (sent_requests right = [1, "initialize"; 2, "launch"]);
    Dap.close_manager dap_manager;
    expect "manager close cleans remaining session" (!(right.close_count) = 1))

let test_bounds_and_malicious_paths () =
  let fixture = root_with_target () in
  Fun.protect ~finally:(fun () -> cleanup_root fixture) (fun () ->
    let root, _ = fixture in
    let dap_manager = manager root in
    let adapter = mock (standard_responder_with_path true) in
    ignore (Dap.create_session dap_manager ~id:"malicious" ~transport:adapter.transport);
    ignore (Dap.initialize dap_manager ~id:"malicious" ~adapter_id:"python");
    let before_bad_target = sent_count adapter in
    let too_many_breakpoints = List.init 129 (fun _ ->
      { Dap.line = 1; condition = None; hit_condition = None; log_message = None }) in
    expect_error "breakpoint count limit" (fun () ->
      Dap.set_breakpoints dap_manager ~id:"malicious" ~source:"main.py"
        ~breakpoints:too_many_breakpoints);
    expect "breakpoint bound rejected before transport I/O"
      (sent_count adapter = before_bad_target);
    expect_error "workspace escape launch target" (fun () ->
      Dap.launch dap_manager ~id:"malicious" ~target:"../secret" ~arguments:[]);
    expect "bad target rejected before transport I/O" (sent_count adapter = before_bad_target);
    ignore (Dap.launch dap_manager ~id:"malicious" ~target:"main.py" ~arguments:[]);
    ignore (Dap.configuration_done dap_manager ~id:"malicious");
    expect_error "adapter traversal source path" (fun () ->
      Dap.stack_trace dap_manager ~id:"malicious" ~thread_id:7 ());
    expect "malicious source response closes its session" (!(adapter.close_count) = 1);
    expect_error "malicious response session is removed"
      (fun () -> Dap.threads dap_manager ~id:"malicious");

    let bounded_manager = manager root in
    let malformed = {
      Dap.send = (fun ~timeout_seconds:_ ~cancelled:_ _ -> ());
      receive = (fun ~timeout_seconds:_ ~cancelled:_ ->
        Some "Content-Length: 1048577\r\n\r\n");
      close = (fun () -> ());
    } in
    ignore (Dap.create_session bounded_manager ~id:"frame-bound" ~transport:malformed);
    expect_error "oversized incoming DAP frame" (fun () ->
      Dap.initialize bounded_manager ~id:"frame-bound" ~adapter_id:"python");
    expect_error "bad-frame session is cleaned up" (fun () ->
      Dap.initialize bounded_manager ~id:"frame-bound" ~adapter_id:"python");
    Dap.close_manager dap_manager;
    Dap.close_manager bounded_manager)

let test_unsolicited_response_and_event_bound () =
  let fixture = root_with_target () in
  Fun.protect ~finally:(fun () -> cleanup_root fixture) (fun () ->
    let root, _ = fixture in
    let dap_manager = manager root in
    let wrong_id = mock (fun ~emit ~request:_ ~outgoing_seq:_ ~command ->
      [emit ("response", ["request_seq", `Int 900; "command", `String command;
                           "success", `Bool true; "body", `Assoc []])]) in
    ignore (Dap.create_session dap_manager ~id:"wrong-id" ~transport:wrong_id.transport);
    expect_error "late unsolicited response" (fun () ->
      Dap.initialize dap_manager ~id:"wrong-id" ~adapter_id:"python");
    expect "unsolicited response tears down its session" (!(wrong_id.close_count) = 1);
    let wrong_command = mock (fun ~emit ~request:_ ~outgoing_seq ~command:_ ->
      [response emit outgoing_seq "launch" (`Assoc [])]) in
    ignore (Dap.create_session dap_manager ~id:"wrong-command" ~transport:wrong_command.transport);
    expect_error "mismatched DAP response command" (fun () ->
      Dap.initialize dap_manager ~id:"wrong-command" ~adapter_id:"python");
    expect "command mismatch tears down its session" (!(wrong_command.close_count) = 1);
    let bad_sequence = mock (fun ~emit:_ ~request:_ ~outgoing_seq:_ ~command:_ ->
      [frame (`Assoc ["seq", `Int 0; "type", `String "response";
        "request_seq", `Int 1; "command", `String "initialize";
        "success", `Bool true; "body", `Assoc []])]) in
    ignore (Dap.create_session dap_manager ~id:"bad-sequence" ~transport:bad_sequence.transport);
    expect_error "invalid adapter sequence number" (fun () ->
      Dap.initialize dap_manager ~id:"bad-sequence" ~adapter_id:"python");
    expect "invalid sequence tears down its session" (!(bad_sequence.close_count) = 1);

    let duplicate_top_level = mock (fun ~emit:_ ~request:_ ~outgoing_seq:_ ~command:_ ->
      [frame (`Assoc ["seq", `Int 1; "type", `String "response";
        "request_seq", `Int 1; "command", `String "initialize";
        "success", `Bool true; "success", `Bool false; "body", `Assoc []])]) in
    ignore (Dap.create_session dap_manager ~id:"duplicate-top" ~transport:duplicate_top_level.transport);
    expect_error "duplicate top-level DAP key" (fun () ->
      Dap.initialize dap_manager ~id:"duplicate-top" ~adapter_id:"python");
    expect "duplicate top-level key closes its session" (!(duplicate_top_level.close_count) = 1);

    let duplicate_nested = mock (fun ~emit:_ ~request:_ ~outgoing_seq:_ ~command:_ ->
      [frame (`Assoc ["seq", `Int 1; "type", `String "response";
        "request_seq", `Int 1; "command", `String "initialize"; "success", `Bool true;
        "body", `Assoc ["capabilities", `Assoc ["supportsFoo", `Bool true;
                                                "supportsFoo", `Bool false]]])]) in
    ignore (Dap.create_session dap_manager ~id:"duplicate-nested" ~transport:duplicate_nested.transport);
    expect_error "duplicate nested DAP key" (fun () ->
      Dap.initialize dap_manager ~id:"duplicate-nested" ~adapter_id:"python");
    expect "duplicate nested key closes its session" (!(duplicate_nested.close_count) = 1);
    let too_many_events = mock (fun ~emit ~request:_ ~outgoing_seq ~command ->
      let events = List.init 65 (fun index -> event emit "output"
        (`Assoc ["category", `String "console"; "output", `String (string_of_int index)])) in
      events @ [response emit outgoing_seq command (`Assoc [])]) in
    ignore (Dap.create_session dap_manager ~id:"event-bound" ~transport:too_many_events.transport);
    expect_error "retained DAP event count limit" (fun () ->
      Dap.initialize dap_manager ~id:"event-bound" ~adapter_id:"python");
    expect "event overflow closes the affected session" (!(too_many_events.close_count) = 1);

    let too_many_bytes = mock (fun ~emit ~request:_ ~outgoing_seq ~command ->
      let events = List.init 5 (fun _ -> event emit "output"
        (`Assoc ["output", `String (String.make 60_000 'x')])) in
      events @ [response emit outgoing_seq command (`Assoc [])]) in
    ignore (Dap.create_session dap_manager ~id:"event-byte-bound" ~transport:too_many_bytes.transport);
    expect_error "retained DAP event byte limit" (fun () ->
      Dap.initialize dap_manager ~id:"event-byte-bound" ~adapter_id:"python");
    expect "event byte overflow closes its session" (!(too_many_bytes.close_count) = 1);

    let timeout_adapter = mock (fun ~emit:_ ~request:_ ~outgoing_seq:_ ~command:_ -> []) in
    ignore (Dap.create_session dap_manager ~id:"timeout" ~transport:timeout_adapter.transport);
    expect_error "DAP request timeout" (fun () ->
      Dap.initialize ~timeout_seconds:0.001 dap_manager ~id:"timeout" ~adapter_id:"python");
    expect "timeout closes its session" (!(timeout_adapter.close_count) = 1);
    let bad_type = mock (fun ~emit ~request:_ ~outgoing_seq:_ ~command:_ ->
      [emit ("not-a-message", ["event", `String "initialized"])]) in
    ignore (Dap.create_session dap_manager ~id:"bad-type" ~transport:bad_type.transport);
    expect_error "invalid DAP message type" (fun () ->
      Dap.initialize dap_manager ~id:"bad-type" ~adapter_id:"python");
    expect "invalid type tears down its session" (!(bad_type.close_count) = 1);
    Dap.close_manager dap_manager)

let test_frame_boundaries_across_chunks () =
  let fixture = root_with_target () in
  Fun.protect ~finally:(fun () -> cleanup_root fixture) (fun () ->
    let root, _ = fixture in
    let dap_manager = manager root in
    Fun.protect ~finally:(fun () -> Dap.close_manager dap_manager) (fun () ->
      let adapter = mock standard_responder in
      let session = Dap.create_session dap_manager ~id:"frame-boundary"
          ~transport:adapter.transport in
      let header body_length =
        let prefix = "Content-Length:" and value = string_of_int body_length in
        prefix ^ String.make (Dap.max_header_bytes - String.length prefix -
          String.length value) ' ' ^ value in
      let body = String.make Dap.max_frame_bytes 'x' in
      let frame_header = header (String.length body) in
      List.iter (fun delimiter_bytes ->
        session.receive_buffer <- frame_header ^ String.sub "\r\n\r\n" 0 delimiter_bytes;
        expect "bounded header accepts a split delimiter" (Dap.take_frame session = None))
        [0; 1; 2; 3];
      session.receive_buffer <- frame_header ^ "\r\n\r\n" ^
        String.sub body 0 (String.length body - 1);
      expect "bounded body waits for its final byte" (Dap.take_frame session = None);
      session.receive_buffer <- session.receive_buffer ^ "x";
      expect "fragmented maximum frame retains exact body"
        (Dap.take_frame session = Some body && session.receive_buffer = "");
      session.receive_buffer <- frame_header ^ " \r\n\r\n" ^ body;
      expect_error "oversized complete header" (fun () -> Dap.take_frame session);
      session.receive_buffer <- String.make (Dap.max_header_bytes + 4) ' ';
      expect_error "oversized unterminated header" (fun () -> Dap.take_frame session)))

let () =
  test_fragmented_flow_and_transitions ();
  test_frame_boundaries_across_chunks ();
  test_stdio_transport_framing_lifecycle_and_environment ();
  test_denied_effects_do_not_send ();
  test_failed_execution_keeps_stopped_state ();
  test_remote_host_trust_is_separate ();
  test_cancel_close_and_session_isolation ();
  test_bounds_and_malicious_paths ();
  test_unsolicited_response_and_event_bound ();
  print_endline "workspace DAP: ok"
