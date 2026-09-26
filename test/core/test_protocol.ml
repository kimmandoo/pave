let expect_invalid f =
  match f () with
  | exception Pave.Protocol.Invalid_response _ -> ()
  | _ -> failwith "expected invalid response"

let () =
  let open Pave.Protocol in
  let call = { id = "call-1"; name = "read_file";
               arguments = `Assoc [ "path", `String "App.swift" ] } in
  let assistant = { role = "assistant"; content = None;
                    tool_calls = [ call ]; tool_call_id = None; tool_result_content = None; provider_state = None; attachments = [] } in
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
  let attachment = {
    name = "private-name.png"; mime_type = "image/png"; data = "aGVsbG8="
  } in
  let attached = user ~attachments:[attachment] "inspect" in
  assert (message_to_json attached = `Assoc [
    "role", `String "user";
    "content", `List [
      `Assoc ["type", `String "text"; "text", `String "inspect"];
      `Assoc ["type", `String "image_url";
        "image_url", `Assoc ["url", `String
          "data:image/png;base64,aGVsbG8="]]]]);
  let stored_attachment = message_to_json ~stored:true attached in
  assert (stored_attachment = `Assoc [
    "role", `String "user"; "content", `String "inspect"]);
  expect_invalid (fun () -> validate_attachments [
    { attachment with data = "a===" }]);
  expect_invalid (fun () -> validate_attachments [
    { attachment with mime_type = "image/gif" }]);
  expect_invalid (fun () -> validate_attachments (List.init (max_attachments + 1)
    (fun _ -> attachment)));
  expect_invalid (fun () -> validate_attachments [
    { attachment with data = String.make (max_attachment_bytes + 4) 'A' }]);
  let attachments_with_size size =
    let data = String.make size 'A' in
    List.init 4 (fun index ->
      { attachment with name = string_of_int index; data }) in
  validate_attachments (attachments_with_size (max_attachment_bytes / 4));
  expect_invalid (fun () -> validate_attachments
    (attachments_with_size (max_attachment_bytes / 4 + 4)));
  let image = Image { mime_type = "image/png"; data = "aGVsbG8=" } in
  let mixed = tool_result_blocks "call-1" [Text "before"; image; Text "after"] in
  assert (mixed.content = Some "before\nafter");
  assert (display_content_blocks (content_blocks_of_tool_result mixed) =
    "before\n[image/png image]\nafter");
  let stored = message_to_json ~stored:true mixed in
  assert (message_from_json stored = mixed);
  assert (member "tool_result_content" (message_to_json mixed) = `Null);
  expect_invalid (fun () -> message_to_json { mixed with content = Some "wrong" });
  let image_only = tool_result_blocks "call-2" [image] in
  let second_call = { call with id = "call-2" } in
  let assistant_with_two_calls = { assistant with tool_calls = [call; second_call] } in
  let final = { assistant with content = Some "done"; tool_calls = [] } in
  (match chat_messages_to_json
      [assistant_with_two_calls; mixed; image_only; final] with
   | `List [assistant_json; first_result; second_result; images; final_json] ->
       assert (assistant_json = message_to_json assistant_with_two_calls);
       assert (member "content" first_result = `String "before\nafter");
       assert (member "content" second_result = `String "(see attached image)");
       assert (member "role" images = `String "user");
       assert (member "content" images = `List [
         `Assoc ["type", `String "text";
           "text", `String "Attached image(s) from tool result:"];
         `Assoc ["type", `String "image_url";
           "image_url", `Assoc ["url", `String "data:image/png;base64,aGVsbG8="]];
         `Assoc ["type", `String "image_url";
           "image_url", `Assoc ["url", `String "data:image/png;base64,aGVsbG8="]]
       ]);
       assert (final_json = message_to_json final)
   | _ -> failwith "Chat result images were not grouped after tool messages");
  assert (chat_messages_to_json [assistant; tool_result "call-1" "source"] =
    `List [message_to_json assistant; message_to_json (tool_result "call-1" "source")]);
  expect_invalid (fun () -> message_from_json (`Assoc [
    "role", `String "tool"; "content", `String "";
    "tool_call_id", `String "call-1";
    "tool_result_content", `List [
      `Assoc ["type", `String "image"; "mimeType", `String "text/plain";
        "data", `String "aGVsbG8="]
    ]
  ]));
  expect_invalid (fun () -> tool_result_blocks "call-1" [
    Image { mime_type = "image/"; data = "aGVsbG8=" }]);
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
