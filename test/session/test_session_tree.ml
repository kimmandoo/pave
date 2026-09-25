let message role text : Pave.Protocol.message =
  { role; content = Some text; tool_calls = []; tool_call_id = None;
    tool_result_content = None; provider_state = None }

let entry id parent_id message : Pave.Session.entry =
  { id; parent_id; timestamp = "2026-09-25T00:00:00Z";
    kind = Pave.Session.Message message }

let () =
  let first = entry "a" None (message "user" "First prompt") in
  let answered = entry "b" (Some "a") (message "assistant" "First reply") in
  let abandoned = entry "c" (Some "b") (message "user" "Abandoned") in
  let alternate = entry "d" (Some "a")
    (message "user" "東京👩‍💻\027[31m\nignored second line") in
  let choices, omitted = Pave.Session_tree.choices ~leaf:(Some "d")
    [first; answered; abandoned; alternate] in
  let labels = List.map (fun (choice : Pave.Session_tree.choice) -> choice.label) choices in
  assert (not omitted);
  assert (List.map (fun (choice : Pave.Session_tree.choice) -> choice.id) choices =
    ["a"; "b"; "c"; "d"]);
  assert (String.starts_with ~prefix:"◆" (List.hd (List.rev labels)));
  assert (List.exists (String.starts_with ~prefix:"    ↳") labels);
  assert (List.for_all (fun text -> not (String.contains text '\027')) labels);
  assert (not (String.contains (List.hd (List.rev labels)) '\n'));
  assert (Pave.Session_tree.first_line "safe\226\128\174unsafe" =
    "safe unsafe");
  let tool = {
    Pave.Session.id = "tool-entry"; parent_id = Some "a";
    timestamp = "2026-09-25T00:00:01Z";
    kind = Pave.Session.Tool_lifecycle {
      call_id = "call-1"; name = "read_file";
      state = Pave.Session.Tool_started
    }
  } in
  let exit = {
    Pave.Session.id = "exit-entry"; parent_id = Some "tool-entry";
    timestamp = "2026-09-25T00:00:02Z";
    kind = Pave.Session.Session_exit {
      kind = Pave.Session.Fatal;
      pending_tool_calls = [{
        call_id = "call-1"; name = "read_file"; state = Pave.Session.Started
      }]
    }
  } in
  assert (Pave.Session_tree.summary tool = "tool · read_file · started");
  assert (Pave.Session_tree.summary exit =
    "session exit · fatal · 1 pending tool");
  let image_result = Pave.Protocol.tool_result_blocks "image-call" [
    Pave.Protocol.Image { mime_type = "image/png"; data = "secret-base64" }
  ] in
  assert (Pave.Session_tree.summary (entry "image" None image_result) =
    "tool · [image/png image]");
  let entries = List.init 1030 (fun n -> entry (string_of_int n)
    (if n = 0 then None else Some (string_of_int (n - 1)))
    (message "user" (String.make 512 'x'))) in
  let recent, truncated = Pave.Session_tree.choices ~leaf:(Some "1029") entries in
  assert (truncated && List.length recent = Pave.Session_tree.max_choices);
  assert ((List.hd recent).id = "6" && (List.hd (List.rev recent)).id = "1029");
  assert (String.length (List.hd recent).label < 200);
  print_endline "journal lineage picker and bounded previews: ok"
