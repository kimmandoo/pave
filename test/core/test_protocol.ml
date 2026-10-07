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
  let direct = direct_tool_message call in
  assert (is_direct_tool_message direct);
  assert (message_from_json (message_to_json ~stored:true direct) = direct);
  let direct_result = tool_result_blocks call.id [
    Text "before"; Image { mime_type = "image/png"; data = "aGVsbG8=" };
    Text "after"] in
  let local_history = [user "/read_file"; direct; direct_result; user "continue"] in
  let projected = replay_messages local_history in
  assert (replay_messages projected == projected);
  assert (sanitize_messages local_history = local_history);
  (match projected with
   | command :: before :: image :: after :: follow_up :: [] ->
       assert (command = user "/read_file" && follow_up = user "continue");
       assert (List.for_all (fun message ->
         message.role = "user" && message.tool_calls = [] &&
         message.tool_call_id = None && message.provider_state = None) projected);
       assert (String.ends_with ~suffix:"\"before\"" (Option.get before.content));
       assert (String.ends_with ~suffix:"\"after\"" (Option.get after.content));
       assert (image.attachments = [{
         name = "direct-tool-image-2"; mime_type = "image/png"; data = "aGVsbG8=" }]);
       assert (not (String.contains (Option.get image.content) '='))
   | _ -> failwith "direct results lost canonical text/image order");
  expect_invalid (fun () -> replay_messages [direct]);
  expect_invalid (fun () -> replay_messages [direct; tool_result "wrong" "output"]);
  expect_invalid (fun () -> replay_messages [
    { direct with tool_calls = [{ call with name = "write_file" }] };
    tool_result call.id "output"]);
  let forged_id = { assistant with tool_calls = [{ call with id = "direct-forged" }] } in
  let model_history = [forged_id; tool_result "direct-forged" "output"] in
  assert (replay_messages model_history == model_history);
  let completed = `Assoc [ "choices", `List [ `Assoc [
    "finish_reason", `String "tool_calls";
    "message", message_to_json assistant ] ] ] in
  assert (parse_completion completed = assistant);
  let text_completion content = `Assoc ["choices", `List [`Assoc [
    "finish_reason", `String "stop";
    "message", `Assoc ["role", `String "assistant"; "content", content]]]] in
  expect_invalid (fun () -> parse_completion (text_completion `Null));
  expect_invalid (fun () -> parse_completion (text_completion (`String "")));
  assert ((parse_completion (text_completion (`String "answer"))).content =
    Some "answer");
  assert (decode_tool_arguments "\"\"" = `Assoc []);
  assert (decode_tool_arguments "\"   \"" = `Assoc []);
  assert (decode_tool_arguments (Yojson.Basic.to_string (`String "{}")) = `Assoc []);
  List.iter (fun raw ->
    assert (member invalid_arguments_key (decode_tool_arguments raw) <> `Null))
    ["\"not arguments\""; Yojson.Basic.to_string (`String "\"still not arguments\"");
     Yojson.Basic.to_string (`String (Yojson.Basic.to_string
       (`String (Yojson.Basic.to_string (`String "{}")))))];
  let counted = `Assoc [ "usage", `Assoc [
    "prompt_tokens", `Int 19; "completion_tokens", `Int 7;
    "prompt_tokens_details", `Assoc [ "cached_tokens", `Int 5 ];
    "completion_tokens_details", `Assoc [ "reasoning_tokens", `Int 2 ] ] ] in
  assert (completion_usage counted =
    Some { input_tokens = 19; output_tokens = 7;
      cached_input_tokens = Some 5; cache_creation_input_tokens = None;
      reasoning_output_tokens = Some 2; input_modality_tokens = None;
      cached_input_modality_tokens = None; output_modality_tokens = None });
  let usage = Option.get (completion_usage counted) in
  let usage = { usage with
    input_modality_tokens = Some [
      { modality = "IMAGE"; token_count = 4 };
      { modality = "SPATIAL"; token_count = 1 } ];
    cached_input_modality_tokens = Some [
      { modality = "IMAGE"; token_count = 5 } ];
    output_modality_tokens = Some [
      { modality = "TEXT"; token_count = 2 }] } in
  let total = add_usage usage usage in
  assert (total.input_modality_tokens = Some [
    { modality = "IMAGE"; token_count = 8 };
    { modality = "SPATIAL"; token_count = 2 } ]);
  assert (total.cached_input_modality_tokens = Some [
    { modality = "IMAGE"; token_count = 10 } ]);

  assert (completion_usage completed = None);
  assert (completion_usage (`Assoc [ "usage", `Assoc [
    "prompt_tokens", `Int 5; "completion_tokens", `Int (-1) ] ]) = None);
  assert (completion_usage (`Assoc [ "usage", `Assoc [
    "prompt_tokens", `Int 5 ] ]) = None);
  expect_invalid (fun () -> parse_completion (`Assoc [ "choices", `List [ `Assoc [
    "finish_reason", `String "length";
    "message", message_to_json assistant ] ] ]));
  (match parse_completion (`Assoc [ "choices", `List [ `Assoc [
    "finish_reason", `String "length";
    "message", message_to_json assistant ] ] ]) with
   | exception Invalid_response message ->
       assert (String.starts_with ~prefix:truncated_prefix message)
   | _ -> assert false);
  (match parse_completion (`Assoc [ "choices", `List [ `Assoc [
    "finish_reason", `String "tool_calls";
    "message", `Assoc [ "role", `String "assistant"; "tool_calls", `List [ `Assoc [
      "id", `String "call"; "type", `String "function";
      "function", `Assoc [ "name", `String "list_files" ] ] ] ] ] ] ]) with
   | { tool_calls = [ { arguments = `Assoc []; _ } ]; _ } -> ()
   | _ -> assert false);
  let choice = match member "choices" completed with
    | `List [choice] -> choice | _ -> assert false in
  expect_invalid (fun () -> parse_completion (`Assoc [
    "choices", `List [choice; choice]]));
  expect_invalid (fun () -> parse_completion (`Assoc ["choices", `List [`Assoc [
    "index", `Int 1; "finish_reason", `String "tool_calls";
    "message", message_to_json assistant]]]));
  expect_invalid (fun () -> parse_completion (`Assoc ["choices", `List [`Assoc [
    "index", `Int 0; "finish_reason", `String "stop";
    "message", `Assoc ["content", `String "No"; "refusal", `String "No"]]]]));
  expect_invalid (fun () -> parse_completion (`Assoc ["choices", `List [`Assoc [
    "index", `Int 0; "finish_reason", `String "stop";
    "message", `Assoc ["role", `String "user"; "content", `String "No"]]]]));
  expect_invalid (fun () -> parse_completion (`Assoc ["choices", `List [`Assoc [
    "index", `Int 0; "finish_reason", `String "stop";
    "message", `Assoc ["content", `String "No"]]]]));
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
  let audio = { attachment with name = "voice.wav"; mime_type = "audio/wav" } in
  let video = { attachment with name = "clip.mp4"; mime_type = "video/mp4" } in
  let media = user ~attachments:[audio; video] "summarize" in
  expect_invalid (fun () -> message_to_json media);
  assert (message_to_json ~stored:true media = `Assoc [
    "role", `String "user"; "content", `String "summarize"]);

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
