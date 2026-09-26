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
let contains text needle =
  let text_length = String.length text and needle_length = String.length needle in
  let rec search index =
    index + needle_length <= text_length &&
    (String.sub text index needle_length = needle || search (index + 1)) in
  needle_length = 0 || search 0

let read_file path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
    really_input_string input (in_channel_length input))

let write_file path contents =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output) (fun () ->
    output_string output contents)

let anthropic_curl_smoke () =
  let directory = Filename.temp_file "pave-anthropic-curl-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let curl = Filename.concat directory "curl" in
  let captured_config = Filename.concat directory "config" in
  let captured_body = Filename.concat directory "body.json" in
  let calls = Filename.concat directory "calls" in
  write_file curl {|#!/bin/sh
set -eu
[ "$1" = "--disable" ] && [ "$2" = "--config" ] && [ "$3" = "-" ]
printf 'request\n' >> "$PAVE_ANTHROPIC_CALLS"
cat > "$PAVE_ANTHROPIC_CONFIG"
output=$(sed -n 's/^output = "\(.*\)"/\1/p' "$PAVE_ANTHROPIC_CONFIG" | tail -n 1)
body=$(sed -n 's/^data-binary = "@\(.*\)"/\1/p' "$PAVE_ANTHROPIC_CONFIG")
cp "$body" "$PAVE_ANTHROPIC_BODY"
printf '%s' "$PAVE_ANTHROPIC_RESPONSE" > "$output"
printf '200'
|};
  Unix.chmod curl 0o700;
  let old_path = Sys.getenv_opt "PATH"
  and old_config = Sys.getenv_opt "PAVE_ANTHROPIC_CONFIG"
  and old_body = Sys.getenv_opt "PAVE_ANTHROPIC_BODY"
  and old_calls = Sys.getenv_opt "PAVE_ANTHROPIC_CALLS"
  and old_response = Sys.getenv_opt "PAVE_ANTHROPIC_RESPONSE" in
  let restore name = function
    | Some value -> Unix.putenv name value
    | None -> Unix.putenv name "" in
  Fun.protect
    ~finally:(fun () ->
      restore "PATH" old_path;
      restore "PAVE_ANTHROPIC_CONFIG" old_config;
      restore "PAVE_ANTHROPIC_BODY" old_body;
      restore "PAVE_ANTHROPIC_CALLS" old_calls;
      restore "PAVE_ANTHROPIC_RESPONSE" old_response;
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path)
        [curl; captured_config; captured_body; calls];
      Unix.rmdir directory)
    (fun () ->
      Unix.putenv "PATH"
        (directory ^ ":" ^ Option.value ~default:"" old_path);
      Unix.putenv "PAVE_ANTHROPIC_CONFIG" captured_config;
      Unix.putenv "PAVE_ANTHROPIC_BODY" captured_body;
      Unix.putenv "PAVE_ANTHROPIC_CALLS" calls;
      let set_response json =
        Unix.putenv "PAVE_ANTHROPIC_RESPONSE" (Yojson.Basic.to_string json) in
      let captured_request () =
        Yojson.Basic.from_file captured_body,
        read_file captured_config in
      let verify_headers config ~compaction_beta =
        assert (contains config
          "url = \"https://api.anthropic.com/v1/messages\"");
        assert (contains config "header = \"anthropic-version: 2023-06-01\"");
        assert (contains config "header = \"x-api-key: fixture-anthropic-key\"");
        assert (contains config "header = \"anthropic-beta: compact-2026-09-04\""
          = compaction_beta) in
      let native_response = `Assoc [
        "type", `String "message"; "role", `String "assistant";
        "stop_reason", `String "compaction";
        "content", `List [`Assoc ["type", `String "compaction";
          "content", `String "signed summary";
          "signature", `String "signed-by-anthropic"]];
        "usage", `Assoc ["input_tokens", `Int 7; "output_tokens", `Int 3]] in
      set_response native_response;
      let provider : Pave.Provider.config = {
        endpoint = "https://api.anthropic.com/v1/messages";
        api_key = "fixture-anthropic-key"; model = "claude-fixture";
        api = Pave.Provider.Anthropic_messages } in
      let system : Pave.Protocol.message = {
        role = "system"; content = Some "stable system prompt"; tool_calls = [];
        tool_call_id = None; tool_result_content = None;
        provider_state = None; attachments = [] } in
      let tool = `Assoc ["type", `String "function"; "function", `Assoc [
        "name", `String "read_file"; "parameters", `Assoc [
          "type", `String "object"]]] in
      let usage = ref None in
      let compacted = Pave.Provider.compact_anthropic_messages
        ~on_usage:(fun value -> usage := Some value) provider
        ~instructions:"preserve decisions"
        ~messages:[system; Pave.Protocol.user "older turn"] ~tools:[tool] in
      assert (compacted.summary = "signed summary");
      assert (member "signature" compacted.provider_state =
        `String "signed-by-anthropic");
      assert (!usage = Some { Pave.Protocol.input_tokens = 7;
        output_tokens = 3 });
      let request, config = captured_request () in
      verify_headers config ~compaction_beta:true;
      assert (member "system" request = `String "stable system prompt");
      assert (member "compaction" request = `Assoc [
        "type", `String "summarize";
        "instructions", `String "preserve decisions"]);
      assert (member "tools" request <> `Null);
      assert (member "messages" request = `List [
        `Assoc ["role", `String "user"; "content", `String "older turn"]]);
      let signed_marker = { (Pave.Protocol.user compacted.summary) with
        provider_state = Some compacted.provider_state } in
      let normal_response = `Assoc [
        "type", `String "message"; "role", `String "assistant";
        "stop_reason", `String "end_turn";
        "content", `List [`Assoc ["type", `String "text";
          "text", `String "ok"]]] in
      set_response normal_response;
      let reply = Pave.Provider.complete provider
        [Pave.Protocol.user "ordinary turn"] [] in
      assert (reply.content = Some "ok");
      let ordinary, config = captured_request () in
      verify_headers config ~compaction_beta:false;
      assert (member "compaction" ordinary = `Null);
      ignore (Pave.Provider.complete provider
        [signed_marker; Pave.Protocol.user "after summary"] []);
      let replay, config = captured_request () in
      verify_headers config ~compaction_beta:true;
      assert (member "messages" replay = `List [
        `Assoc ["role", `String "assistant"; "content", `List [
          `Assoc ["type", `String "compaction";
            "content", `String "signed summary";
            "signature", `String "signed-by-anthropic"]]];
        `Assoc ["role", `String "user"; "content", `String "after summary"]]);
      let gateway = { provider with
        endpoint = "https://gateway.example/v1/messages" } in
      ignore (Pave.Provider.complete gateway
        [signed_marker; Pave.Protocol.user "after summary"] []);
      let fallback, config = captured_request () in
      assert (contains config
        "url = \"https://gateway.example/v1/messages\"");
      assert (not (contains config "anthropic-beta: compact-2026-09-04"));
      assert (member "messages" fallback = `List [
        `Assoc ["role", `String "user"; "content", `String "signed summary"];
        `Assoc ["role", `String "user"; "content", `String "after summary"]]);
      (match Pave.Provider.compact_anthropic_messages gateway
          ~instructions:"preserve decisions"
          ~messages:[system; Pave.Protocol.user "older turn"] ~tools:[] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> failwith "Anthropic compaction accepted a custom endpoint");
      (match Pave.Provider.compact_anthropic_messages
          ~authentication:Pave.Provider.OAuth provider
          ~instructions:"preserve decisions"
          ~messages:[system; Pave.Protocol.user "older turn"] ~tools:[] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> failwith "Anthropic compaction accepted OAuth authentication");
      assert (List.length (String.split_on_char '\n' (read_file calls)) = 5))

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
    anthropic_curl_smoke ();
    print_endline "native context compaction HTTP: ok"
  with exn ->
    (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
    (try ignore (Unix.waitpid [] child) with Unix.Unix_error _ -> ());
    raise exn
