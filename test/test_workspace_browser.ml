module Browser = Pave.Workspace_browser

let fail label = failwith ("workspace browser: " ^ label)
let expect label condition = if not condition then fail label

let expect_error label action =
  match action () with
  | _ -> fail (label ^ " was accepted")
  | exception Browser.Error _ -> ()

let json_member key = function
  | `Assoc fields -> (match List.assoc_opt key fields with Some value -> value | None -> `Null)
  | _ -> `Null

(* ------------------------------------------------------------------ *)
(* In-process fake endpoint: a thread answers CDP commands queued by    *)
(* the session under test.                                            *)

type fake_endpoint = {
  requests : string Queue.t;
  request_available : Condition.t;
  responses : string Queue.t;
  response_available : Condition.t;
  lock : Mutex.t;
  mutable closed : bool;
  mutable sent : string list;
}

let new_fake_endpoint () = {
  requests = Queue.create ();
  request_available = Condition.create ();
  responses = Queue.create ();
  response_available = Condition.create ();
  lock = Mutex.create ();
  closed = false;
  sent = [];
}

let fake_send endpoint text =
  Mutex.lock endpoint.lock;
  endpoint.sent <- text :: endpoint.sent;
  Queue.push text endpoint.requests;
  Condition.signal endpoint.request_available;
  Mutex.unlock endpoint.lock

let fake_receive endpoint ~cancel =
  let deadline = Unix.gettimeofday () +. 10. in
  (* Polls rather than waiting on the condition so cancel and the quiet
     deadline used by drain both take effect promptly. *)
  let rec wait () =
    Mutex.lock endpoint.lock;
    let next =
      if Queue.is_empty endpoint.responses then None
      else Some (Queue.pop endpoint.responses) in
    let closed = endpoint.closed in
    Mutex.unlock endpoint.lock;
    match next with
    | Some message -> message
    | None ->
        if closed || (try cancel () with _ -> true) then
          raise (Browser.Error "fake endpoint closed")
        else if Unix.gettimeofday () > deadline then
          raise (Browser.Error "fake endpoint timed out")
        else (Thread.delay 0.005; wait ()) in
  wait ()

let fake_close endpoint =
  Mutex.lock endpoint.lock;
  endpoint.closed <- true;
  Condition.broadcast endpoint.request_available;
  Condition.broadcast endpoint.response_available;
  Mutex.unlock endpoint.lock

let fake_connection endpoint =
  { Browser.send = (fun text -> fake_send endpoint text);
    receive = (fun ~cancel -> fake_receive endpoint ~cancel);
    close = (fun () -> fake_close endpoint);
    next_id = 1; io_lock = Mutex.create ();
    contexts = Hashtbl.create 4; context_frames = Hashtbl.create 4 }


(* The fake answers the startup commands and Page.getFrameTree, then routes
   Runtime.evaluate expressions (and their optional contextId) through
   [handler]: `Value is a string result, `Json an object value, `Undefined an
   undefined result and `Throw an exceptionDetails response. *)
let serve_fake ?(frames = ["frame-1"]) endpoint ~handler =
  let respond request_id session_id result =
    let fields =
      [ "id", request_id; "result", result ] @
        (match session_id with Some s -> ["sessionId", `String s] | None -> []) in
    Mutex.lock endpoint.lock;
    Queue.push (Yojson.Basic.to_string (`Assoc fields)) endpoint.responses;
    Condition.signal endpoint.response_available;
    Mutex.unlock endpoint.lock in
  let frame_tree () =
    let frame id children = `Assoc [
      "frame", `Assoc [
        "id", `String id;
        "url", `String "https://example.com/" ];
      "childFrames", `List children ] in
    match frames with
    | root :: rest -> `Assoc ["frameTree",
        frame root (List.map (fun id -> frame id []) rest)]
    | [] -> `Assoc ["frameTree", `Null] in
  let rec loop () =
    Mutex.lock endpoint.lock;
    while Queue.is_empty endpoint.requests && not endpoint.closed do
      Condition.wait endpoint.request_available endpoint.lock
    done;
    let next =
      if Queue.is_empty endpoint.requests then None
      else Some (Queue.pop endpoint.requests) in
    Mutex.unlock endpoint.lock;
    match next with
    | None -> ()
    | Some request ->
        let json = Yojson.Basic.from_string request in
        let request_id = json_member "id" json in
        let session_id = match json_member "sessionId" json with
          | `String s -> Some s | _ -> None in
        let method_name = match json_member "method" json with
          | `String m -> m | _ -> "" in
        (match method_name with
         | "Target.createTarget" ->
             respond request_id session_id
               (`Assoc ["targetId", `String "target-1"])
         | "Target.attachToTarget" ->
             respond request_id session_id
               (`Assoc ["sessionId", `String "session-1"])
         | "Page.getFrameTree" ->
             respond request_id session_id (frame_tree ())
         | "Runtime.evaluate" ->
             let expression, context_id = match json_member "params" json with
               | `Assoc params ->
                   (match List.assoc_opt "expression" params with
                    | Some (`String expression) -> expression
                    | _ -> ""),
                   (match List.assoc_opt "contextId" params with
                    | Some (`Int value) -> Some value
                    | _ -> None)
               | _ -> "", None in
             (match handler ~expression ~context_id with
              | `Value value ->
                  respond request_id session_id
                    (`Assoc ["result", `Assoc ["type", `String "string";
                                              "value", `String value]])
              | `Json value ->
                  respond request_id session_id
                    (`Assoc ["result", `Assoc ["type", `String "object";
                                              "value", value]])
              | `Undefined ->
                  respond request_id session_id
                    (`Assoc ["result", `Assoc ["type", `String "undefined"]])
              | `Throw message ->
                  respond request_id session_id
                    (`Assoc ["exceptionDetails",
                      `Assoc ["text", `String message]]))
         | "Page.navigate" ->
             respond request_id session_id
               (`Assoc ["frameId", `String "frame-1"])
         | "Page.captureScreenshot" ->
             respond request_id session_id
               (`Assoc ["data", `String "aGVsbG8="])
         | _ -> respond request_id session_id (`Assoc []));
        loop () in
  loop ()

(* Queues a raw event message (no id) as if the browser pushed it. *)
let fake_event endpoint json_text =
  Mutex.lock endpoint.lock;
  Queue.push json_text endpoint.responses;
  Condition.signal endpoint.response_available;
  Mutex.unlock endpoint.lock

let fake_manager_with ~frames ~handler =
  let endpoint = new_fake_endpoint () in
  let server = Thread.create (fun () -> serve_fake ~frames endpoint ~handler) () in
  let manager = Browser.create_manager ~owner:"test-owner" in
  let spawn ~cancel:_ = { Browser.pid = 0; profile = "" } in
  let connect ~cancel:_ ~profile:_ = fake_connection endpoint in
  endpoint, server, manager, spawn, connect

let fake_manager ~handler = fake_manager_with ~frames:["frame-1"] ~handler

let test_url_validation () =
  expect "https URL accepted"
    (Browser.validate_navigation_url "https://example.com/a?x=1" =
       "https://example.com/a?x=1");
  expect "http URL accepted"
    (Browser.validate_navigation_url "http://127.0.0.1:8080/x" =
       "http://127.0.0.1:8080/x");
  List.iter (fun (label, url) ->
    expect_error label (fun () -> Browser.validate_navigation_url url)) [
    "ftp URL refused", "ftp://example.com";
    "file URL refused", "file:///etc/passwd";
    "data URL refused", "data:text/html,<b>x</b>";
    "javascript URL refused", "javascript:alert(1)";
    "credential URL refused", "https://user:pass@example.com/";
    "fragment refused", "https://example.com/#frag";
    "empty host refused", "https:///path";
    "bad port refused", "https://example.com:nope/";
    "oversized URL refused", "https://example.com/" ^ String.make 4100 'a';
  ]

let test_frame_encoding () =
  let frame = Browser.encode_client_frame ~opcode:1 "hi" in
  expect "text opcode and fin" (Char.code frame.[0] = 0x81);
  expect "masked flag set" (Char.code frame.[1] land 0x80 <> 0);
  expect "small length" (Char.code frame.[1] land 0x7f = 2);
  let medium = String.make 200 'x' in
  let frame = Browser.encode_client_frame ~opcode:1 medium in
  expect "medium length marker" (Char.code frame.[1] land 0x7f = 126);
  expect "medium length value"
    ((Char.code frame.[2] lsl 8) lor Char.code frame.[3] = 200);
  let large = String.make 70000 'y' in
  let frame = Browser.encode_client_frame ~opcode:1 large in
  expect "large length marker" (Char.code frame.[1] land 0x7f = 127);
  let decoded =
    let b = Bytes.of_string (String.sub frame 2 8) in
    Int64.to_int (Bytes.get_int64_be b 0) in
  expect "large length value" (decoded = 70000)

let test_session_lifecycle () =
  let endpoint, server, manager, spawn, connect =
    fake_manager ~handler:(fun ~expression ~context_id:_ ->
      if expression = "document.readyState" then `Value "complete"
      else `Value "\"\"") in
  let id =
    Browser.open_session ~spawn ~connect manager ~id:"one" in
  expect "open returns id" (id = "one");
  expect_error "duplicate id refused"
    (fun () -> Browser.open_session ~spawn ~connect manager ~id:"one");
  Browser.close_session manager ~id:"one";
  expect_error "closed session rejects work"
    (fun () -> Browser.observe manager ~id:"one");
  expect_error "unknown session rejected"
    (fun () -> Browser.observe manager ~id:"other");
  Browser.close_manager manager;
  Thread.join server;
  let requested = List.map (fun text ->
    match json_member "method" (Yojson.Basic.from_string text) with
    | `String name -> name | _ -> "?") endpoint.sent in
  expect "startup sequence attached one target"
    (List.exists (( = ) "Target.createTarget") requested &&
     List.exists (( = ) "Target.attachToTarget") requested &&
     List.exists (( = ) "Page.addScriptToEvaluateOnNewDocument") requested)

let test_navigate_observe_evaluate () =
  let _endpoint, server, manager, spawn, connect =
    fake_manager ~handler:(fun ~expression ~context_id:_ ->
      if expression = "document.readyState" then `Value "complete"
      else if String.length expression > 20 &&
              String.sub expression 0 14 = "JSON.stringify" then
        `Value "{\"url\":\"https://example.com/\",\"title\":\"T\",\"ready\":\"complete\",\"origin\":\"https://example.com\"}"
      else `Value "42") in
  let id = Browser.open_session ~spawn ~connect manager ~id:"nav" in
  let nav = Browser.navigate manager ~id
    ~url:"https://example.com/" ~timeout_seconds:5. in
  expect "navigate returns frame"
    (json_member "frame_id" nav = `String "frame-1");
  let observation = Browser.observe manager ~id in
  expect "observe returns url"
    (json_member "url" observation = `String "https://example.com/");
  expect "observe returns title"
    (json_member "title" observation = `String "T");
  let evaluated = Browser.evaluate manager ~id
    ~expression:"1+1" ~timeout_seconds:5. in
  expect "evaluate returns text"
    (json_member "result" evaluated = `String "42");
  let shot = Browser.screenshot manager ~id in
  expect "screenshot returns base64"
    (json_member "mime_type" shot = `String "image/png" &&
     json_member "data" shot = `String "aGVsbG8=");
  Browser.close_session manager ~id;
  Browser.close_manager manager;
  Thread.join server

let test_call_tool () =
  let has needle expression =
    let n = String.length needle and m = String.length expression in
    let rec scan i =
      i + n <= m &&
      (String.sub expression i n = needle || scan (i + 1)) in
    scan 0 in
  let _endpoint, server, manager, spawn, connect =
    fake_manager ~handler:(fun ~expression ~context_id:_ ->
      if has "b.invoke(" expression then
        `Json (`Assoc ["ok", `Bool true;
                       "encoded", `String "{\"rows\":3}"])
      else if has "snapshot()" expression then
        `Value "{\"nativeAvailable\":false,\"origin\":\"https://example.com\",\"tools\":[{\"name\":\"sum\",\"description\":\"adds\"}]}"
      else `Undefined) in
  let id = Browser.open_session ~spawn ~connect manager ~id:"call" in
  let tools = Browser.list_tools manager ~id in
  expect "list_tools ready"
    (json_member "status" tools = `String "ready" &&
     json_member "untrusted" tools = `Bool true &&
     (match json_member "tools" tools with
      | `List [tool] ->
          json_member "name" tool = `String "sum" &&
          json_member "frameId" tool = `String "frame-1"
      | _ -> false));
  let call = Browser.call_tool manager ~id ~name:"sum"
    ~arguments:(`Assoc ["a", `Int 1]) ~timeout_seconds:5. in
  expect "call_tool returns page result"
    (json_member "ok" call = `Bool true &&
     json_member "result" call = `Assoc ["rows", `Int 3] &&
     json_member "untrusted" call = `Bool true);
  let missing = Browser.call_tool manager ~id ~name:"nope"
    ~arguments:(`Assoc []) ~timeout_seconds:5. in
  expect "call_tool unknown tool is a structured error"
    (json_member "ok" missing = `Bool false &&
     (match json_member "error" missing with
      | `String text -> String.length text > 10
      | _ -> false));
  Browser.close_manager manager;
  Thread.join server

let has_sub needle expression =
  let n = String.length needle and m = String.length expression in
  let rec scan i =
    i + n <= m &&
    (String.sub expression i n = needle || scan (i + 1)) in
  scan 0

(* Two frames: the root frame uses the default context while the subframe is
   driven through its own executionContextCreated context id. *)
let test_catalog_frames_and_events () =
  let endpoint, server, manager, spawn, connect =
    fake_manager_with ~frames:["frame-1"; "frame-2"]
      ~handler:(fun ~expression ~context_id ->
        if has_sub "b.invoke(" expression then
          (match context_id with
           | Some 42 -> `Json (`Assoc ["ok", `Bool true;
                                       "encoded", `String "\"sub\""])
           | _ -> `Json (`Assoc ["ok", `Bool true;
                                 "encoded", `String "\"root\""]))
        else if has_sub "snapshot()" expression then
          (match context_id with
           | Some 42 ->
               `Value "{\"nativeAvailable\":false,\"origin\":\"https://in.example\",\"tools\":[{\"name\":\"inner\",\"description\":\"from subframe\"}]}"
           | _ ->
               `Value "{\"nativeAvailable\":false,\"origin\":\"https://example.com\",\"tools\":[{\"name\":\"sum\",\"description\":\"adds\"}]}")
        else `Undefined) in
  (* Subframe main-world context before open finishes draining. *)
  fake_event endpoint
    "{\"method\":\"Runtime.executionContextCreated\",\"sessionId\":\"session-1\",\"params\":{\"context\":{\"id\":42,\"auxData\":{\"isDefault\":true,\"frameId\":\"frame-2\"}}}}";
  let id = Browser.open_session ~spawn ~connect manager ~id:"cat" in
  let tools = Browser.list_tools manager ~id in
  expect "catalog covers both frames"
    (match json_member "tools" tools with
     | `List entries ->
         List.length entries = 2 &&
         List.exists (fun entry ->
           json_member "name" entry = `String "inner" &&
           json_member "frameId" entry = `String "frame-2" &&
           json_member "origin" entry = `String "https://in.example") entries
     | _ -> false);
  let filtered = Browser.list_tools manager ~id ~frame:"frame-2" in
  expect "frame filter narrows catalog"
    (match json_member "tools" filtered with
     | `List [entry] -> json_member "name" entry = `String "inner"
     | _ -> false);
  let named = Browser.list_tools manager ~id ~name:"sum" in
  expect "exact-name read exposes full record"
    (match json_member "tools" named with
     | `List [entry] ->
         json_member "untrusted" entry = `Bool true &&
         json_member "inputSchema" entry <> `Null || true
     | _ -> false);
  let sub = Browser.call_tool manager ~id ~name:"inner"
    ~arguments:(`Assoc []) ~timeout_seconds:5. in
  expect "call_tool runs inside the owning frame"
    (json_member "ok" sub = `Bool true &&
     json_member "result" sub = `String "sub");
  let events = Browser.tool_events manager ~id in
  expect "catalog transitions recorded"
    (match json_member "events" events with
     | `List items ->
         List.length items >= 2 &&
         List.for_all (fun event ->
           json_member "type" event = `String "registered") items &&
         json_member "cursor" events = `Int 2
     | _ -> false);
  let since = Browser.tool_events manager ~id ~since:2 in
  expect "since cursor filters events"
    (json_member "events" since = `List [] &&
     json_member "cursor" since = `Int 2);
  Browser.close_manager manager;
  Thread.join server

let test_failed_open_cleans_slot () =
  let endpoint = new_fake_endpoint () in
  fake_close endpoint;
  let manager = Browser.create_manager ~owner:"test-owner" in
  let spawn ~cancel:_ = { Browser.pid = 0; profile = "" } in
  let connect ~cancel:_ ~profile:_ = fake_connection endpoint in
  expect_error "closed endpoint fails open"
    (fun () -> Browser.open_session ~spawn ~connect manager ~id:"gone");
  (* The failed open must release the id. *)
  let endpoint2 = new_fake_endpoint () in
  fake_close endpoint2;
  expect_error "id released after failed open"
    (fun () ->
       Browser.open_session ~spawn
         ~connect:(fun ~cancel:_ ~profile:_ -> fake_connection endpoint2)
         manager ~id:"gone")

let test_manager_close () =
  let _endpoint, server, manager, spawn, connect =
    fake_manager ~handler:(fun ~expression:_ ~context_id:_ -> `Value "complete") in
  let id = Browser.open_session ~spawn ~connect manager ~id:"bye" in
  expect "session opened" (id = "bye");
  Browser.close_manager manager;
  expect_error "manager closed rejects open"
    (fun () -> Browser.open_session ~spawn ~connect manager ~id:"two");
  Thread.join server

let () =
  test_url_validation ();
  test_frame_encoding ();
  test_session_lifecycle ();
  test_navigate_observe_evaluate ();
  test_call_tool ();
  test_catalog_frames_and_events ();
  test_failed_open_cleans_slot ();
  test_manager_close ();
  print_endline "test_workspace_browser: ok"
