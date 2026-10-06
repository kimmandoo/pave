(* Spins a real Session_hub server on port 0 and exercises the HTTP surface
   over raw loopback sockets. *)

module Hub = Pave.Session_hub

let token = "test-token-0123456789abcdef"
let session_id = "sess-test"
let title = "hub test session"

let () = Printexc.record_backtrace true
(* The server legitimately answers and closes while a large request is still
   in flight; on macOS the follow-up write can raise SIGPIPE. Convert the
   signal to EPIPE so tests observe a closed connection, not a signal exit. *)
let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let fail message = failwith ("test_session_hub: " ^ message)

let check condition message = if not condition then fail message

let write_all fd bytes =
  let total = Bytes.length bytes in
  let rec loop offset =
    if offset < total then
      let n =
        try Unix.write fd bytes offset (total - offset)
        with Unix.Unix_error (Unix.EINTR, _, _) -> 0
        (* The server may answer and close before consuming a large request;
           a broken pipe ends the write; the response is still readable. *)
        | Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) ->
            total - offset in
      loop (offset + n) in
  loop 0

let read_all fd =
  let buffer = Buffer.create 512 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    match
      (try Unix.read fd chunk 0 (Bytes.length chunk)
       with
       | Unix.Unix_error (Unix.EINTR, _, _) -> -1
       (* RST (server shutdown with unread data) counts as EOF *)
       | Unix.Unix_error ((Unix.ECONNRESET | Unix.EPIPE
                          | Unix.ETIMEDOUT | Unix.EAGAIN
                          | Unix.EWOULDBLOCK), _, _) -> 0)
    with
    | -1 -> loop ()
    | 0 -> Buffer.contents buffer
    | n ->
      Buffer.add_subbytes buffer chunk 0 n;
      loop () in
  loop ()

let contains text needle =
  let length = String.length text and width = String.length needle in
  let rec seek i =
    i + width <= length &&
    (String.sub text i width = needle || seek (i + 1)) in
  width = 0 || seek 0

let has_header headers name =
  List.exists (fun (key, _) -> String.lowercase_ascii key = name) headers

let request ?(meth = "GET") ?(headers = []) ?content_length ?(body = "")
    ~port target =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      Unix.setsockopt_float fd Unix.SO_RCVTIMEO 10.;
      Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
      let headers =
        if meth = "POST" && not (has_header headers "content-length") then
          ("content-length",
           string_of_int (match content_length with
               | Some n -> n | None -> String.length body)) :: headers
        else headers in
      let head = Printf.sprintf "%s %s HTTP/1.1\r\nhost: 127.0.0.1\r\n%s\r\n"
          meth target
          (String.concat ""
             (List.map (fun (k, v) -> k ^ ": " ^ v ^ "\r\n") headers)) in
      write_all fd (Bytes.unsafe_of_string head);
      write_all fd (Bytes.unsafe_of_string body);
      let response = read_all fd in
      (* Server closes the connection; response must contain a status line. *)
      match String.split_on_char '\n' response with
      | status_line :: _ ->
        let parts = List.filter (fun part -> part <> "")
            (String.split_on_char ' ' (String.trim status_line)) in
        let status = match parts with
          | _version :: code :: _ -> (match int_of_string_opt code with
              | Some code -> code | None -> fail ("bad status line: " ^ status_line))
          | _ -> fail ("bad status line: " ^ status_line) in
        let response_body =
          let rec find i =
            if i + 3 >= String.length response then None
            else if String.sub response i 4 = "\r\n\r\n" then Some (i + 4)
            else find (i + 1) in
          match find 0 with
          | Some index -> String.sub response index
                            (String.length response - index)
          | None -> "" in
        status, response_body
      | [] -> fail "empty response")
let auth = [ "x-pave-csrf-token", token ]

let json_member json key =
  Yojson.Basic.Util.(json |> member key)

let () =
  let entries = ref [
      `Assoc [ "step", `Int 1; "kind", `String "user" ];
      `Assoc [ "step", `Int 2; "kind", `String "assistant" ];
      `Assoc [ "step", `Int 3; "kind", `String "tool" ];
    ] in
  let pending = ref 0 in
  let submissions = ref [] in
  let received = ref [] in
  let fail_next = ref false in
  let current_title = ref title in
  let record submission = received := submission :: !received in
  let hub = Hub.create ~port:0 ~token ~session_id
      ~read_title:(fun () -> !current_title)
      ~read_entries:(fun () -> !entries)
      ~read_pending:(fun () -> !pending)
      ~submit:(fun submission ->
          record submission;
          if !fail_next then begin
            fail_next := false;
            Error "queue-full"
          end
          else begin
            submissions := submission :: !submissions;
            Ok ()
          end)
      () in
  let port = Hub.port hub in
  check (port > 0) "server did not bind an ephemeral port";
  let stuck = ref None in
  Fun.protect ~finally:(fun () -> Hub.close hub; Hub.close hub) (fun () ->
      (* healthz is unauthenticated *)
      let status, body = request ~port "/healthz" in
      check (status = 200) "healthz status";
      check (json_member (Yojson.Basic.from_string body) "ok" = `Bool true)
        "healthz body";

      (* /api/session token enforcement *)
      let status, _ = request ~port "/api/session" in
      check (status = 403) "session without token must be 403";
      let status, body = request ~port "/api/session"
          ~headers:[ "x-pave-csrf-token", "wrong" ] in
      check (status = 403) "session with wrong token must be 403";
      check (contains body "csrf") "403 body must name csrf";
      let status, _ = request ~port ("/api/session?token=" ^ token) in
      check (status = 403) "query token must not authenticate";
      pending := 2;
      let status, body = request ~port "/api/session" ~headers:auth in
      check (status = 200) "session with token must be 200";
      let json = Yojson.Basic.from_string body in
      check (json_member json "sessionId" = `String session_id) "sessionId";
      check (json_member json "title" = `String title) "title";
      check (json_member json "pending" = `Int 2) "pending count";
      pending := 0;
      current_title := "renamed live session";
      let status, body = request ~port "/api/session" ~headers:auth in
      check (status = 200) "renamed session status";
      check (json_member (Yojson.Basic.from_string body) "title" =
             `String "renamed live session") "session title must stay live";

      (* /api/entries returns injected entries and honors since *)
      let status, body = request ~port "/api/entries" ~headers:auth in
      check (status = 200) "entries status";
      let json = Yojson.Basic.from_string body in
      check (match json_member json "entries" with
          | `List list -> List.length list = 3 | _ -> false)
        "entries count";
      check (json_member json "next" = `Int 3) "entries next";
      let status, body = request ~port "/api/entries?since=2" ~headers:auth in
      check (status = 200) "entries since status";
      let json = Yojson.Basic.from_string body in
      (match json_member json "entries" with
       | `List [ `Assoc fields ] ->
         check (List.assoc "step" fields = `Int 3) "since filter kept step 3"
       | _ -> fail "since filter result");
      check (json_member json "next" = `Int 3) "entries since next";
      let status, _ = request ~port "/api/entries?since=bogus" ~headers:auth in
      check (status = 400) "invalid since must be 400";

      (* POST /api/prompt delivers exactly one Submit *)
      let status, body = request ~meth:"POST" ~port "/api/prompt"
          ~headers:auth ~body:{|{"text":"hello hub"}|} in
      check (status = 202) "prompt must be 202";
      check (contains body "queued") "prompt response must report queued";
      check (match !received with
          | [ Hub.Submit text ] -> text = "hello hub"
          | _ -> false) "exactly one Submit delivered";

      (* submit Error surfaces an error response *)
      fail_next := true;
      let status, body = request ~meth:"POST" ~port "/api/prompt"
          ~headers:auth ~body:{|{"text":"again"}|} in
      check (status >= 400 && status < 600) "submit error must be 4xx/5xx";
      check (contains body "error") "submit error body must carry error";
      check (contains body "queue-full") "submit error propagates message";

      (* POST /api/cancel *)
      let status, _ = request ~meth:"POST" ~port "/api/cancel" ~headers:auth in
      check (status = 202) "cancel must be 202";

      (* /api/poll is read-only and must not count as a submission *)
      let before = List.length !received in
      let status, body = request ~port "/api/poll?since=0" ~headers:auth in
      check (status = 200) "poll status";
      let json = Yojson.Basic.from_string body in
      check (match json_member json "entries" with
          | `List list -> List.length list = 3 | _ -> false)
        "poll entries";
      check (List.length !received = before) "poll must not submit";

      check (match !submissions with
          | [ Hub.Cancel; Hub.Submit "hello hub" ] -> true
          | _ -> false) "submission order";

      (* malformed requests *)
      let status, _ = request ~meth:"POST" ~port "/api/prompt"
          ~headers:auth ~body:"not json" in
      check (status = 400) "bad json must be 400";
      List.iter (fun body ->
        let status, _ = request ~meth:"POST" ~port "/api/prompt"
            ~headers:auth ~body in
        check (status = 400) "invalid or empty prompt JSON must be 400")
        ["[]"; "null"; {|{"text":3}|}; {|{"text":""}|}; {|{"text":" \n\t"}|}];
      let before = List.length !received in
      let framing_body = {|{"text":"framing test"}|} in
      let framing_length = String.length framing_body in
      let status, _ = request ~meth:"POST" ~port "/api/prompt"
          ~headers:(auth @ [
            "content-length", string_of_int framing_length;
            "Content-Length", string_of_int (framing_length + 10)])
          ~body:framing_body in
      check (status = 400) "conflicting content-length headers must be 400";
      let status, _ = request ~port "/api/session" ~headers:(auth @ auth) in
      check (status = 400) "duplicate CSRF headers must not authenticate";
      let status, _ = request ~meth:"POST" ~port "/api/prompt"
          ~headers:(auth @ ["content-length", Printf.sprintf "0x%x" framing_length])
          ~body:framing_body in
      check (status = 400) "hexadecimal HTTP content-length must be rejected";
      check (List.length !received = before) "malformed framing must never submit";
      let status, _ = request ~meth:"POST" ~port "/api/entries"
          ~headers:auth ~body:"{}" in
      check (status = 405) "POST /api/entries must be 405";
      let status, _ = request ~port "/nope" ~headers:auth in
      check (status = 404) "unknown path must be 404";

      (* oversized body -> 413 *)
      let status, _ = request ~meth:"POST" ~port "/api/prompt" ~headers:auth
          ~content_length:(64 * 1024 + 1) in
      check (status = 413) "oversized body must be 413";

      (* oversized request head -> 431 *)
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
          Unix.setsockopt_float fd Unix.SO_RCVTIMEO 10.;
          Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
          let junk = "GET /healthz HTTP/1.1\r\nx: "
                     ^ String.make (20 * 1024) 'a' ^ "\r\n\r\n" in
          write_all fd (Bytes.unsafe_of_string junk);
          let response = read_all fd in
          check (String.length response >= 15 &&
                 String.sub response 9 3 = "431")
            "oversized head must be 431");

      (* connection still open (server busy) gets torn down by close() *)
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
      Unix.setsockopt_float fd Unix.SO_RCVTIMEO 10.;
      stuck := Some fd;
      ());

  (* close() wedged the half-open connection *)
  (match !stuck with
   | Some fd ->
     let leftover =
       try read_all fd
       with Unix.Unix_error _ -> "" in
     check (leftover = "")
       "server must not emit data on the wedged connection";
     Unix.close fd
   | None -> fail "stuck connection was not opened");

  (* close() freed the listen port: it can be bound again *)
  let probe = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close probe) (fun () ->
      Unix.setsockopt probe Unix.SO_REUSEADDR true;
      Unix.bind probe (Unix.ADDR_INET (Unix.inet_addr_loopback, port)));

  (* nothing is listening on the port anymore *)
  let refused =
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
        try Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
          false
        with Unix.Unix_error _ -> true) in
  check refused "connection accepted after close";

  (* Exercise the reader with an explicit shared deadline: a client-connect
     timestamp predates accept/worker scheduling and cannot measure the
     production handler's budget. Activity must not refresh this deadline. *)
  let reader, writer = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close reader; Unix.close writer) (fun () ->
    Unix.set_nonblock reader;
    let deadline = Unix.gettimeofday () +. 0.5 in
    let outcome = ref None in
    let worker = Thread.create (fun () ->
      let status = try ignore (Hub.read_request ~deadline reader); None
        with Hub.Http_error (status, _) -> Some status in
      outcome := Some (status, Unix.gettimeofday ())) () in
    Fun.protect ~finally:(fun () ->
      Unix.shutdown writer Unix.SHUTDOWN_SEND;
      Thread.join worker) (fun () ->
      write_all writer (Bytes.of_string "GET /healthz HTTP/1.1\r\nx-drip: ");
      let rec drip () =
        if Unix.gettimeofday () < deadline then (
          write_all writer (Bytes.of_string "a");
          Thread.delay 0.025;
          drip ()) in
      drip ();
      Thread.join worker;
      check (match !outcome with
        | Some (Some 408, finished) ->
            finished -. deadline < Hub.io_timeout /. 2.
        | _ -> false)
        "slow drip must reach the shared absolute deadline, not the later idle timeout"));

  print_endline "test_session_hub: ok"
