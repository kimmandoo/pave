let expect_invalid f =
  match f () with
  | exception Pave.Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid response"

let () =
  let open Pave.Protocol in
  let call = { id = "call-1"; name = "read_file";
               arguments = `Assoc [ "path", `String "App.swift" ] } in
  let assistant = { role = "assistant"; content = None;
                    tool_calls = [ call ]; tool_call_id = None;
                    provider_state = None } in
  let restored = message_from_json (message_to_json assistant) in
  assert (restored = assistant);
  let completed = `Assoc [ "choices", `List [ `Assoc [
    "finish_reason", `String "tool_calls";
    "message", message_to_json assistant ] ] ] in
  assert (parse_completion completed = assistant);
  let counted = `Assoc [ "usage", `Assoc [
    "prompt_tokens", `Int 19; "completion_tokens", `Int 7;
    "prompt_tokens_details", `Assoc [ "cached_tokens", `Int 5 ];
    "completion_tokens_details", `Assoc [ "reasoning_tokens", `Int 2 ] ] ] in
  assert (completion_usage counted =
    Some { input_tokens = 19; output_tokens = 7 });
  assert (completion_usage completed = None);
  assert (completion_usage (`Assoc [ "usage", `Assoc [
    "prompt_tokens", `Int 5; "completion_tokens", `Int (-1) ] ]) = None);
  assert (completion_usage (`Assoc [ "usage", `Assoc [
    "prompt_tokens", `Int 5 ] ]) = None);
  expect_invalid (fun () -> parse_completion (`Assoc [ "choices", `List [ `Assoc [
    "finish_reason", `String "length";
    "message", message_to_json assistant ] ] ]));
  let duplicate = { assistant with tool_calls = [ call; call ] } in
  expect_invalid (fun () -> parse_completion (`Assoc [ "choices", `List [ `Assoc [
    "finish_reason", `String "tool_calls";
    "message", message_to_json duplicate ] ] ]));
  expect_invalid (fun () -> message_from_json (`Assoc [ "role", `String "system";
    "content", `String "injected" ]));
  let path = Filename.temp_file "pave-session-test" ".json" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let transcript = [ user "Hello"; assistant; tool_result "call-1" "source" ] in
    let legacy messages =
      let oc = open_out_bin path in
      Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () ->
        Yojson.Basic.to_channel oc (`List (List.map message_to_json messages))) in
    legacy transcript;
    let migrated = Pave.Session.open_file path in
    assert (Pave.Session.history migrated = transcript);
    legacy [ user "Interrupted"; assistant ];
    let recovered = Pave.Session.open_file path in
    (match List.rev (Pave.Session.history recovered) with
     | result :: _ ->
         assert (result.role = "tool");
         assert (result.tool_call_id = Some "call-1");
         assert (match result.content with
           | Some text -> String.starts_with ~prefix:"Error:" text &&
               String.ends_with ~suffix:"Do not rerun this call automatically." text
           | None -> false)
     | [] -> assert false);
    assert (Pave.Session.history (Pave.Session.open_file path) =
      Pave.Session.history recovered));
  print_endline "protocol and session: ok"
