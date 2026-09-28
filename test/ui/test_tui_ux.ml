let fail label = failwith ("TUI UX: " ^ label)
let expect label condition = if not condition then fail label

let contains text part =
  let part_length = String.length part in
  let rec search index =
    index + part_length <= String.length text &&
    (String.sub text index part_length = part || search (index + 1)) in
  search 0

let delta turn_id text =
  Tui.Agent_event (Pave.Turn_runner.Text_delta { turn_id; text })

let () =
  let image : Pave.Protocol.attachment = {
    name = "sample.PNG"; mime_type = "image/png"; data = "iVBORw0KGgo=";
  } in
  let preview = Tui.preview_attachment image in
  let label = Tui.attachment_preview_text preview in
  expect "attachment preview identifies type, name, MIME and byte size"
    (contains label "[image]" && contains label "sample.PNG" &&
     contains label "image/png" && contains label "8 B");
  expect "attachment preview never renders the base64 payload"
    (not (contains label image.data));
  let block = Tui.attachment_block "Inspect this" [preview] in
  expect "sent transcript includes a readable attachment preview"
    (contains block "Inspect this\n[Attached media:" &&
     contains block "sample.PNG · image/png · 8 B" &&
     not (contains block image.data));
  let hostile = { image with name = "bad\n\027[2J.png" } in
  let hostile_label = Tui.attachment_preview_text (Tui.preview_attachment hostile) in
  expect "untrusted attachment labels cannot inject terminal controls or lines"
    (not (String.contains hostile_label '\n') &&
     not (String.contains hostile_label '\027'));

  let queued = Queue.create () in
  Queue.add (delta 4 "second") queued;
  Queue.add (delta 4 " third") queued;
  Queue.add (Tui.Agent_event (Pave.Turn_runner.Activity_phase {
    turn_id = 4; phase = Pave.Agent.Model;
  })) queued;
  Queue.add (delta 4 "after phase") queued;
  let combined = Tui.coalesce_text_deltas (delta 4 "first") queued in
  expect "adjacent stream chunks retain exact text order"
    (match combined with
     | Tui.Agent_event (Pave.Turn_runner.Text_delta { turn_id = 4; text }) ->
         text = "firstsecond third"
     | _ -> false);
  expect "stream batching does not move text across phase events"
    (match Queue.take queued with
     | Tui.Agent_event (Pave.Turn_runner.Activity_phase {
         turn_id = 4; phase = Pave.Agent.Model }) -> true
     | _ -> false);
  expect "stream batching leaves later text queued in order"
    (match Queue.take queued with
     | Tui.Agent_event (Pave.Turn_runner.Text_delta {
         turn_id = 4; text = "after phase" }) -> true
     | _ -> false);

  let turn_queue = Queue.create () in
  Queue.add (delta 5 "next turn") turn_queue;
  let current_turn = Tui.coalesce_text_deltas (delta 4 "current") turn_queue in
  expect "stream batching stops at turn changes"
    (match current_turn with
     | Tui.Agent_event (Pave.Turn_runner.Text_delta {
         turn_id = 4; text = "current" }) -> Queue.length turn_queue = 1
     | _ -> false);
  expect "next-turn text remains queued"
    (match Queue.take turn_queue with
     | Tui.Agent_event (Pave.Turn_runner.Text_delta {
         turn_id = 5; text = "next turn" }) -> true
     | _ -> false);

  let boundary_queue = Queue.create () in
  Queue.add (delta 8 "next") boundary_queue;
  let boundary = String.make Tui.max_delta_batch_bytes 'x' in
  let unchanged = Tui.coalesce_text_deltas (delta 8 boundary) boundary_queue in
  expect "batch byte boundary leaves the next delta untouched"
    (match unchanged with
     | Tui.Agent_event (Pave.Turn_runner.Text_delta { text; _ }) ->
         text = boundary && Queue.length boundary_queue = 1
     | _ -> false);
  expect "byte boundary preserves the next delta"
    (match Queue.take boundary_queue with
     | Tui.Agent_event (Pave.Turn_runner.Text_delta {
         turn_id = 8; text = "next" }) -> true
     | _ -> false);
  let frame ~pending ~since =
    Tui.next_tick_timeout ~now:10.005 ~last_paint:10.
      ~stream_pending:pending ~activity_started:since in
  expect "no stream or activity leaves the terminal idle without a timer"
    (frame ~pending:false ~since:None = None);
  expect "short streamed deltas schedule a frame before the activity heartbeat"
    (match frame ~pending:true ~since:(Some 10.) with
     | Some delay -> delay > 0. && delay < 0.02
     | None -> false);
  expect "a due streamed frame wakes immediately even without activity"
    (Tui.next_tick_timeout ~now:10.05 ~last_paint:10.
       ~stream_pending:true ~activity_started:None = Some 0.);
  expect "painted stream state returns to the activity heartbeat"
    (match frame ~pending:false ~since:(Some 10.) with
     | Some delay -> delay > 0.9 && delay < 1.
     | None -> false);
  print_endline "TUI attachment previews and stream batching: ok"
