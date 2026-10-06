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
  let blocked_label = "Observe app — unavailable" in
  let blocked_reason =
    "requires a running selected app; current app state is selected." in
  let blocked_chooser : Tui.chooser = {
    title = "Mobile";
    intro = [||];
    plain = [];
    detail_rows = 2;
    choices = Tui.initial_candidates ~dynamic:false
      ~details:[blocked_label, blocked_reason]
      [|blocked_label; "Back"|];
    allow_custom = false;
    dynamic = false;
    segmented = false;
    empty_message = "No choices";
    count_label = "available";
    scope_action = None;
    status = None;
    status_pages = [||];
    status_page = 0;
    filter = "";
    selected = 0;
    offset = 0;
    touched = false;
    filtered = None;
    matched_models = 0;
  } in
  let _, _, _, selected_detail, _ = Tui.chooser_sections
      ~cols:100 ~height:8 blocked_chooser in
  expect "static chooser carries an exact gated-action reason"
    (Array.to_list selected_detail = [blocked_reason]);
  let form = Tui.create_text_form ~title:"Intent"
    ~intro:["Describe the app interaction without changing your message draft"] () in
  let key key = `Key (key, []) in
  let handle ?(can_accept = true) event =
    Tui.text_form_event ~bindings:Keybindings.bindings ~columns:18
      ~can_accept form event in
  expect "empty form cannot confirm" (handle (key `Enter) = `Continue);
  expect "blank text is not a completed intent"
    (handle (key (`ASCII ' ')) = `Continue &&
     handle (key `Enter) = `Continue);
  ignore (handle (key `Backspace));
  let intent = String.make 304 'x' ^ " 한글/" in
  ignore (handle (`Paste `Start));
  Uutf.String.fold_utf_8 (fun () _ -> function
    | `Uchar uchar -> ignore (handle (key (`Uchar uchar)))
    | `Malformed _ -> fail "test intent is valid UTF-8") () intent;
  expect "paste data is not committed until its closing delimiter"
    (Pave.Composer.text form.composer = "");
  ignore (handle (`Paste `End));
  expect "long Unicode and trailing slash survive as complete intent text"
    (handle (key `Enter) = `Confirm intent);
  ignore (handle (key `Home));
  ignore (handle (key `Delete));
  ignore (handle (key (`ASCII 'y')));
  ignore (handle (key `End));
  ignore (handle (key `Backspace));
  expect "home, delete, end and backspace edit rather than select a model"
    (Pave.Composer.text form.composer = "y" ^ String.sub intent 1
      (String.length intent - 2));
  ignore (handle (key (`Arrow `Left)));
  let cursor = Pave.Composer.cursor form.composer in
  ignore (handle (key (`Uchar (Uchar.of_int 0x754c))));
  expect "Unicode insertion follows the movable cursor"
    (Pave.Composer.cursor form.composer = cursor + String.length "界");
  expect "small-screen confirmation is disabled even with complete text"
    (handle ~can_accept:false (key `Enter) = `Continue);
  expect "escape cancels a text form" (handle (key `Escape) = `Cancel);
  ignore (handle (`Paste `Start));
  ignore (handle (key `Enter));
  expect "pasted newline cannot confirm" (form.pasting);
  ignore (handle (`Paste `End));
  expect "pasted newline remains editable text"
    (String.contains (Pave.Composer.text form.composer) '\n');
  let bounded = Tui.create_text_form ~max_bytes:8 ~title:"Intent" () in
  let bound event = Tui.text_form_event ~bindings:Keybindings.bindings
    ~columns:18 ~can_accept:true bounded event in
  ignore (bound (key (`Uchar (Uchar.of_int 0x754c))));
  ignore (bound (`Paste `Start));
  List.iter (fun char -> ignore (bound (key (`ASCII char))))
    ['1'; '2'; '3'; '4'; '5'; '6'];
  ignore (bound (`Paste `End));
  expect "over-budget paste rejects atomically and reports failure"
    (Pave.Composer.text bounded.composer = "界" && bounded.notice <> "" &&
     bound (key `Enter) = `Continue);
  ignore (bound (`Paste `Start));
  List.iter (fun char -> ignore (bound (key (`ASCII char))))
    ['1'; '2'; '3'; '4'; '5'];
  ignore (bound (`Paste `End));
  expect "a complete retry can consume the exact byte budget"
    (bound (key `Enter) = `Confirm "界12345");
  ignore (bound (key (`Uchar (Uchar.of_int 0x754c))));
  expect "Unicode over the boundary is never shortened or accepted"
    (Pave.Composer.text bounded.composer = "界12345" &&
     bound (key `Enter) = `Continue);
  ignore (bound (key `Backspace));
  expect "editing recovers from over-limit feedback"
    (bound (key `Enter) = `Confirm "界1234");
  ignore (bound (`Paste `Start));
  ignore (bound (key `Escape));
  expect "pasted Escape is data, not modal cancellation"
    (bounded.pasting && bounded.blocked);
  ignore (bound (`Paste `End));
  expect "unsupported controls never silently change a pasted intent"
    (Pave.Composer.text bounded.composer = "界1234" &&
     bound (key `Enter) = `Continue);
  let default_limit = Tui.create_text_form ~title:"Intent" () in
  let limit event = Tui.text_form_event ~bindings:Keybindings.bindings
    ~columns:18 ~can_accept:true default_limit event in
  ignore (limit (`Paste `Start));
  for _ = 1 to 4096 do ignore (limit (key (`ASCII 'a'))) done;
  ignore (limit (`Paste `End));
  expect "default budget accepts the whole 4096-byte paste"
    (limit (key `Enter) = `Confirm (String.make 4096 'a'));
  List.iter (fun cols -> List.iter (fun rows ->
    let screen, cursor = Tui.text_form_screen ~cols ~rows form in
    expect "text form rows and widths stay inside every viewport"
      (Array.length screen = rows &&
       Array.for_all (fun image -> Notty.I.width image <= cols &&
         Notty.I.height image <= 1) screen);
    expect "text form confirmation requires a visible editor and controls"
      (Option.is_some cursor = Tui.text_form_can_accept ~cols ~rows);
    Option.iter (fun (row, col) ->
      expect "text form caret remains inside the resized viewport"
        (row >= 0 && row < rows && col >= 0 && col < cols)) cursor)
    [1; 2; 3; 4; 5; 8; 24]) [1; 4; 9; 12; 24; 80];
  let reflow = Transcript_view.create () in
  let paragraph = "abcdefghijklmnopqrstuvwx" in
  Transcript_view.assistant reflow paragraph;
  let before = Transcript_view.snapshot reflow ~columns:6 ~measure:Tui.measure_text in
  let entry = Option.get (Array.find_opt (fun (entry : Transcript_view.entry) ->
    entry.row.text = paragraph) before.entries) in
  let after = Transcript_view.snapshot reflow ~columns:4 ~measure:Tui.measure_text in
  let next = Option.get (Array.find_opt (fun (next : Transcript_view.entry) ->
    next.source = entry.source) after.entries) in
  expect "resize retains the visible position inside a wrapped transcript row"
    (Tui.reflow_anchor before after (entry.start + 2) = Some (next.start + 3));
  let bidi_draft = Pave.Composer.create () in
  Pave.Composer.insert bidi_draft "a\226\128\174b";
  let bidi_lines = Pave.Composer.layout ~columns:16 ~measure:Tui.measure_text
    bidi_draft in
  expect "composer caret measures the sanitized cells shown on screen"
    (Pave.Composer.position ~measure:Tui.measure_text bidi_draft bidi_lines = (0, 3));
  List.iter (fun rows -> List.iter (fun activity ->
    List.iter (fun attachments -> List.iter (fun lines ->
      let media, editor, body = Tui.editor_geometry ~rows ~activity
        ~attachments ~modal:false ~lines in
      expect "editor and activity remain within every supported small viewport"
        (editor >= 1 && editor <= 4 && media >= 0 && body >= 0 &&
         (rows < 6 || media + editor + body + activity + 4 = rows));
      expect "compact terminals retain their single editable row"
        (rows >= 6 || editor = 1 && media = 0))
      [1; 2; 5]) [0; 1; 8]) [0; 1]) [3; 6; 8; 12];
  expect "busy seven-row layout cannot focus a hidden hint list"
    (Tui.editor_geometry ~rows:7 ~activity:1 ~attachments:0 ~modal:false
       ~lines:1 = (0, 1, 1));
  expect "media consumes hint space rather than granting invisible hint focus"
    (Tui.editor_geometry ~rows:12 ~activity:0 ~attachments:8 ~modal:false
       ~lines:1 = (6, 1, 1));
  List.iter (fun width ->
    let first = Tui.compact_model_label ~width ~provider:"openai" ~name:"same-model"
    and second = Tui.compact_model_label ~width ~provider:"ollama" ~name:"same-model" in
    expect "compact model identity still distinguishes its provider"
      (first <> second && contains first "openai" && contains second "ollama" &&
       Tui.measure_text first <= width && Tui.measure_text second <= width))
    [16; 22; 34];
  expect "a compound shell command cannot offer an ineffective permanent grant"
    (not (Tui.persistent_command_grant "echo first && echo second") &&
     not (Tui.persistent_command_grant "echo first | cat") &&
     Tui.persistent_command_grant "printf '%s' 'a && b'");
  let mouse_event button =
    (`Mouse (`Press (`Scroll button), (0, 0), []) : Notty.Unescape.event) in
  expect "mouse-wheel up scrolls toward earlier transcript rows"
    (Tui.mouse_scroll_delta (mouse_event `Up) = Some 1);
  expect "mouse-wheel down scrolls toward recent transcript rows"
    (Tui.mouse_scroll_delta (mouse_event `Down) = Some (-1));
  expect "ordinary mouse clicks do not scroll the transcript"
    (Tui.mouse_scroll_delta
      (`Mouse (`Press `Left, (0, 0), []) : Notty.Unescape.event) = None);
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
  expect "event backlog and per-pump fairness limits are documented"
    (Tui.max_ui_events = 4096 && Tui.max_ui_event_bytes = 4_194_304 &&
     Tui.reserved_ui_events = 256 && Tui.reserved_ui_bytes = 1_048_576 &&
     Tui.ui_pump_event_limit = 128);
  let flood = Queue.create () in
  for _ = 1 to Tui.max_delta_batch_events do Queue.add (delta 9 "x") flood done;
  let batched = Tui.coalesce_text_deltas (delta 9 "x") flood in
  expect "sustained stream batching retains byte order within a bounded batch"
    (match batched with
     | Tui.Agent_event (Pave.Turn_runner.Text_delta { text; _ }) ->
         text = String.make Tui.max_delta_batch_events 'x' &&
         Queue.length flood = 1
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
  expect "painted stream state returns to the 80 ms spinner cadence"
    (match frame ~pending:false ~since:(Some 10.) with
     | Some delay -> delay > 0.07 && delay < 0.08
     | None -> false);
  let diff = Transcript_view.create () in
  Transcript_view.assistant diff
    "```diff\n@@ -1 +1 @@\n-old\n+new\n```";
  let rows = Transcript_view.layout diff ~columns:24
    ~measure:(fun text -> Notty.I.width (Notty.I.string Notty.A.empty text)) in
  let visual text = Option.get (Array.find_opt
    (fun (row : Transcript_view.visual) -> row.text = text) rows) in
  let removed = visual "-old" and added = visual "+new" in
  expect "printed diff has distinct foreground and surface for each change"
    (not (Notty.A.equal (Tui.style_attr removed.row)
      (Tui.style_attr added.row)) &&
     (Tui.no_color || not (Notty.A.equal
       (Tui.row_surface removed.row) (Tui.row_surface added.row))));
  let output = Buffer.create 128 in
  Notty.Render.to_buffer output Notty.Cap.ansi (0, 0) (24, 1)
    (Tui.styled_visual 24 removed);
  expect "terminal rendering preserves the original deletion and gutter"
    (contains (Buffer.contents output) "-old" &&
     contains (Buffer.contents output) "│");
  let reads = Transcript_view.create () in
  let group = Transcript_view.start_tool ~target:"docs/one.md" reads
    "read_file" in
  Transcript_view.tool_result ~group reads "read_file"
    "---\n# A readable heading\nbody";
  let read = Option.get (Array.find_opt (fun (visual : Transcript_view.visual) ->
    visual.row.style = Transcript_view.Tool_summary)
    (Transcript_view.layout reads ~columns:60
      ~measure:(fun text -> Notty.I.width
        (Notty.I.string Notty.A.empty text)))) in
  let rendered_read = Buffer.create 160 in
  Notty.Render.to_buffer rendered_read Notty.Cap.ansi (0, 0) (60, 1)
    (Tui.styled_visual 60 read);
  expect "settled file-read card uses one identifiable line, not a raw preview"
    (contains (Buffer.contents rendered_read) "●" &&
     contains (Buffer.contents rendered_read) "docs/one.md" &&
     not (contains (Buffer.contents rendered_read) "⎿") &&
     not (contains (Buffer.contents rendered_read) "---"));
  (match Sys.getenv_opt "PAVE_REAL_DIFF_TUI" with
   | Some "1" ->
       let screen = Tui.create ~root:(Unix.getcwd ()) ~model:"diff-smoke"
         ~session:false () in
       Fun.protect ~finally:(fun () -> Tui.close screen) (fun () ->
         Tui.sent screen "Show changed lines";
         Tui.tool_started ~target:"docs/one.md" screen "read-one" "read_file";
         Tui.tool_settled screen "read-one" "read_file"
           "---\n# One\nbody" false;
         Tui.tool_started ~target:"docs/two.md" screen "read-two" "read_file";
         Tui.tool_settled screen "read-two" "read_file"
           "---\n# Two\nbody" false;
         Tui.event screen
           "[run_command] Status: exit 0\ndiff --git a/a.txt b/a.txt\n--- a/a.txt\n+++ b/a.txt\n@@ -1 +1 @@\n-old\n+new";
         ignore (Transcript_view.toggle screen.transcript ~first:0
           ~last:(screen.transcript.count - 1));
         Tui.delta screen
           "```diff\n@@ -1 +1 @@\n-before\n+after\n```";
         Transcript_view.finish screen.transcript;
         Tui.paint screen;
         Thread.delay 0.2;
         let call : Pave.Protocol.tool_call = {
           id = "restored-read"; name = "read_file";
           arguments = `Assoc ["path", `String "docs/restored.md"] } in
         let assistant : Pave.Protocol.message = {
           role = "assistant"; content = None; tool_result_content = None;
           tool_calls = [call]; tool_call_id = None; provider_state = None;
           attachments = [] } in
         Tui.show_history screen [assistant;
           Pave.Protocol.tool_result call.id "---\n# Restored\ncontent"];
         expect "session replay restores the named compact file-read row"
           (Array.exists (fun (entry : Transcript_view.visual) ->
             entry.text = "read_file · docs/restored.md · 3 lines · collapsed")
             (Transcript_view.layout screen.transcript ~columns:80
               ~measure:(fun text -> Notty.I.width
                 (Notty.I.string Notty.A.empty text))));
         let failed = { call with id = "failed-read";
           arguments = `Assoc ["path", `String "docs/unavailable.md"] } in
         let assistant = { assistant with tool_calls = [call; failed] } in
         Tui.show_history ~tool_outcomes:[call.id, false; failed.id, true] screen
           [assistant; Pave.Protocol.tool_result call.id "Error: ordinary file content";
            Pave.Protocol.tool_result failed.id "Permission refused"];
         let restored = Transcript_view.layout screen.transcript ~columns:80
           ~measure:Tui.measure_text in
         expect "typed successful read replay does not infer failure from Error prefix"
           (Array.exists (fun (row : Transcript_view.visual) ->
             row.row.kind = Transcript_view.Tool &&
             row.row.style = Transcript_view.Tool_summary) restored);
         expect "typed failed read replay retains its failure without a text prefix"
           (Array.exists (fun (row : Transcript_view.visual) ->
             row.row.kind = Transcript_view.Error &&
             contains row.text "Permission refused") restored);
         let content = String.concat "\n"
           (List.init 40 (fun index -> Printf.sprintf "Error: ordinary line %d" index)) in
         Tui.show_history ~tool_outcomes:[call.id, false] screen
           [{ assistant with tool_calls = [call] }; Pave.Protocol.tool_result call.id content];
         screen.scroll <- max_int;
         Tui.paint screen;
         Tui.toggle_tool_detail screen;
         expect "read detail expansion keeps the compact summary in view"
           (let _, _, layout = Option.get screen.layout_cache in
            let first = max 0 (layout.total - Tui.view_height screen - screen.scroll) in
            let row = Transcript_view.visual_at layout first in
            row.row.style = Transcript_view.Tool_summary);
         Thread.delay 0.8)
   | _ -> ());
  print_endline "TUI attachment, stream and diff rendering: ok"
