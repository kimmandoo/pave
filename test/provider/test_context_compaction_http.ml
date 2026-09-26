let member = Pave.Protocol.member

let read_request ic =
  let first = input_line ic in
  let headers = ref [] and length = ref 0 in
  let rec consume () =
    let line = input_line ic in
    if line <> "\r" && line <> "" then (
      let lower = String.lowercase_ascii line in
      headers := lower :: !headers;
      if String.starts_with ~prefix:"content-length:" lower then
        length := int_of_string
          (String.trim (String.sub lower 15 (String.length lower - 15)));
      consume ()) in
  consume ();
  let path = match String.split_on_char ' ' first with
    | _method :: path :: _ -> path
    | _ -> failwith "malformed compact request" in
  path, !headers, Yojson.Basic.from_string (really_input_string ic !length)

let has_header prefix headers = List.exists (String.starts_with ~prefix) headers

let serve socket =
  for attempt = 0 to 1 do
    let client, _ = Unix.accept socket in
    let ic = Unix.in_channel_of_descr client in
    let oc = Unix.out_channel_of_descr client in
    let path, headers, request = read_request ic in
    assert (path = "/v1/responses/compact");
    assert (has_header "authorization: bearer mock-openai" headers);
    assert (member "model" request = `String "fixture-model");
    assert (member "instructions" request = `String "compact safely");
    assert (member "input" request = `List [`Assoc [
      "role", `String "user";
      "content", `List [`Assoc ["type", `String "input_text";
        "text", `String "older turn"]]]]);
    let message = `Assoc ["type", `String "message"; "role", `String "assistant";
      "status", `String "completed"; "content", `List [
        `Assoc ["type", `String "output_text";
          "text", `String "retained provider item"]]] in
    let compact_item = `Assoc ["type", `String "compaction";
      "encrypted_content", `String "opaque compact payload"] in
    let output = if attempt = 0 then [message; compact_item] else [message] in
    let body = Yojson.Basic.to_string (`Assoc [
      "output", `List output;
      "usage", `Assoc ["input_tokens", `Int 7; "output_tokens", `Int 3]]) in
    Printf.fprintf oc
      "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
      (String.length body) body;
    flush oc;
    close_in_noerr ic;
    close_out_noerr oc
  done;
  Unix.close socket

let () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 2;
  let port = match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    try serve socket; exit 0 with exn ->
      prerr_endline ("compact fixture: " ^ Printexc.to_string exn);
      exit 2);
  Unix.close socket;
  let reap () =
    let _, status = Unix.waitpid [] child in
    match status with
    | Unix.WEXITED 0 -> ()
    | _ -> failwith "native compaction HTTP fixture failed" in
  try
    let provider : Pave.Provider.config = {
      endpoint = Printf.sprintf "http://127.0.0.1:%d/v1/responses" port;
      api_key = "mock-openai"; model = "fixture-model";
      api = Pave.Provider.Openai_responses } in
    let usage = ref None in
    let compacted = Pave.Provider.compact_openai_responses
      ~on_usage:(fun value -> usage := Some value) provider
      ~instructions:"compact safely" [Pave.Protocol.user "older turn"] in
    assert (compacted.summary = "OpenAI Responses compacted context");
    assert (member "provider" compacted.provider_state = `String "openai");
    assert (member "route" compacted.provider_state = `String "responses");
    assert (member "model" compacted.provider_state = `String "fixture-model");
    assert (match member "items" compacted.provider_state with
      | `List [_; item] -> member "encrypted_content" item =
          `String "opaque compact payload"
      | _ -> false);
    (match Pave.Provider.compact_openai_responses
        ~on_usage:(fun _ -> failwith "invalid native response recorded usage")
        provider ~instructions:"compact safely"
        [Pave.Protocol.user "older turn"] with
     | exception Pave.Protocol.Invalid_response _ -> ()
     | _ -> failwith "native compaction accepted output without a compaction item");
    reap ();
    (match Pave.Provider.compact_openai_responses
        { provider with endpoint = "https://api.openai.com/v1/chat/completions" }
        ~instructions:"compact safely" [Pave.Protocol.user "older turn"] with
     | exception Pave.Provider.Provider_error _ -> ()
     | _ -> failwith "native compaction accepted a non-Responses endpoint");
    print_endline "native context compaction HTTP: ok"
  with exn ->
    (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
    (try ignore (Unix.waitpid [] child) with Unix.Unix_error _ -> ());
    raise exn
