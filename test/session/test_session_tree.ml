let message role text : Pave.Protocol.message =
  { role; content = Some text; tool_calls = []; tool_call_id = None;
    tool_result_content = None; provider_state = None; attachments = [] }

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
  let labeled, _ = Pave.Session_tree.choices ~labels:["d", "Release review"]
    ~leaf:(Some "d") [first; answered; abandoned; alternate] in
  assert (String.ends_with ~suffix:" · Release review"
    (List.hd (List.rev labeled)).label);
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
  let pin : Pave.Session.entry = {
    id = "pin-entry"; parent_id = Some "a";
    timestamp = "2026-09-25T00:00:03Z"; kind = Pave.Session.Pin true
  } in
  assert (Pave.Session_tree.summary pin = "pin · pinned");
  let attached_prompt = Pave.Protocol.user ~attachments:[{
    Pave.Protocol.name = "private.png";
    mime_type = "image/png"; data = "aGVsbG8="
  }] "Look at this" in
  assert (Pave.Session_tree.summary
    (entry "attached-prompt" None attached_prompt) =
    "user · Look at this · private.png");
  let image_result = Pave.Protocol.tool_result_blocks "image-call" [
    Pave.Protocol.Image { mime_type = "image/png"; data = "secret-base64" }
  ] in
  assert (Pave.Session_tree.summary (entry "image" None image_result) =
    "tool · [image/png image]");
  let owner = "0123456789abcdef0123456789abcdef"
  and artifact_id = "fedcba9876543210fedcba9876543210" in
  let reference : Pave.Session.attachment_reference = {
    owner; id = artifact_id; name = "screenshot.png"; mime_type = "image/png";
    size = 9; sha256 = String.make 64 'a' } in
  let artifact_message : Pave.Session.entry = {
    id = "artifact-message"; parent_id = None;
    timestamp = "2026-09-25T00:00:04Z";
    kind = Pave.Session.Message_artifact
      (message "user" "Look at this", [reference]) } in
  assert (Pave.Session_tree.summary artifact_message =
    "user · Look at this · screenshot.png");
  let started : Pave.Session.entry = {
    id = "job-started"; parent_id = None;
    timestamp = "2026-09-25T00:00:05Z";
    kind = Pave.Session.Job_started {
      owner; job_id = artifact_id; label = "Plan"; job_kind = "plan" } } in
  assert (Pave.Session_tree.summary started = "job · Plan · plan · started");
  let delivered : Pave.Session.entry = {
    id = "job-delivery"; parent_id = Some "job-started";
    timestamp = "2026-09-25T00:00:06Z";
    kind = Pave.Session.Job_delivery {
      owner; job_id = artifact_id; label = "Plan";
      status = Pave.Session.Completed; summary = "Saved result";
      artifact = Some (owner, artifact_id) } } in
  assert (Pave.Session_tree.summary delivered =
    "job · Plan · completed · Saved result · artifact " ^ artifact_id);
  let goal : Pave.Session.entry = {
    id = "goal"; parent_id = None; timestamp = "2026-09-25T00:00:07Z";
    kind = Pave.Session.Workflow_goal (Some "Inspect the workflow") } in
  assert (Pave.Session_tree.summary goal = "goal · Inspect the workflow");
  let interruption_rule : Pave.Session.entry = {
    id = "rule"; parent_id = Some "goal"; timestamp = "2026-09-25T00:00:08Z";
    kind = Pave.Session.Interruption_rule (Some "Stop before irreversible changes") } in
  assert (Pave.Session_tree.summary interruption_rule =
    "interruption rule · Stop before irreversible changes");
  let entries = List.init 1030 (fun n -> entry (string_of_int n)
    (if n = 0 then None else Some (string_of_int (n - 1)))
    (message "user" (String.make 512 'x'))) in
  let recent, truncated = Pave.Session_tree.choices ~leaf:(Some "1029") entries in
  assert (truncated && List.length recent = Pave.Session_tree.max_choices);
  assert ((List.hd recent).id = "6" && (List.hd (List.rev recent)).id = "1029");
  assert (String.length (List.hd recent).label < 200);
  print_endline "journal lineage picker and bounded previews: ok"
