open Transcript_view

let fail message = failwith message
let expect label condition = if not condition then fail label
let measure cluster = Notty.I.width (Notty.I.string Notty.A.empty cluster)
let rendered t width = layout t ~columns:width ~measure
let lines t width = Array.to_list (Array.map (fun visual -> visual.text) (rendered t width))
let has text lines = List.exists (String.equal text) lines
let has_tool_state t name status =
  let prefix = name ^ " · " ^ status in
  let rec find index =
    if index = t.count then false
    else
      let row = t.rows.(index) in
      (row.kind = Tool && row.style = Tool_state &&
       String.starts_with ~prefix row.text) || find (index + 1) in
  find 0
let heading_count t kind =
  let count = ref 0 in
  for i = 0 to t.count - 1 do
    let row = t.rows.(i) in
    if row.kind = kind && row.style = Heading then incr count
  done;
  !count

let row_visual view source =
  match Array.find_opt (fun (entry : entry) -> entry.source = source)
      view.entries with
  | None -> None
  | Some entry ->
      Some (Array.init entry.length (fun i ->
        (visual_at view (entry.start + i)).text))

let () =
  let transcript = create () in
  sent transcript "Ship the HTTP client";
  delta transcript "Building a client\nthat retries";
  event transcript "[http_request]";
  let result = "HTTP/1.1 200 OK\nline two\nsecret later line\nfourth line" in
  event transcript ("[http_request] " ^ result);
  delta transcript "Final **response**";
  let initial = lines transcript 64 in
  expect "distinct user block" (heading_count transcript User = 1);
  expect "streamed assistant grouped with tool activity" (heading_count transcript Assistant = 2);
  expect "tool settled state and first preview visible"
    (has "http_request · completed" initial &&
    not (has "http_request · running" initial) &&
    has_tool_state transcript "http_request" "done" &&
    has "HTTP/1.1 200 OK" initial);
  expect "collapsed output hides remaining lines" (not (has "secret later line" initial));
  expect "tool expansion chooses current tool"
    (Option.is_some (toggle transcript ~first:0 ~last:(transcript.count - 1)));
  let expanded = lines transcript 64 in
  expect "expanded output contains full result" (has "secret later line" expanded &&
    has "fourth line" expanded);
  expect "tool collapse restores compact result"
    (Option.is_some (toggle transcript ~first:0 ~last:(transcript.count - 1)));
  expect "collapsed result hidden again" (not (has "secret later line" (lines transcript 64)));
  rollback transcript;
  let after = lines transcript 64 in
  expect "cancel retracts both assistant segments" (heading_count transcript Assistant = 0 &&
    not (has "Building a client" after) &&
    not (has "Final **response**" after));
  expect "completed tool execution remains visible"
    (has_tool_state transcript "http_request" "done");
  error transcript "Error: provider unavailable";
  expect "errors distinguished" (heading_count transcript Error = 1);
  let narrow = rendered transcript 8 in
  let user_body = Array.to_list narrow
    |> List.filter (fun (visual : visual) -> visual.source = 1)
    |> List.map (fun (visual : visual) -> visual.text)
    |> String.concat "" in
  expect "narrow layout preserves complete user text"
    (user_body = "Ship the HTTP client");
  Array.iter (fun v ->
    expect "each grapheme-safe line fits viewport"
      (Notty.I.width (Notty.I.string Notty.A.empty v.text) <= 8)) narrow;
  let unicode = create () in
  sent unicode "e\204\129 👩‍💻";
  let unicode_rows = rendered unicode 8 in
  let unicode_body = Array.to_list unicode_rows
    |> List.filter (fun (visual : visual) -> visual.source = 1)
    |> List.map (fun (visual : visual) -> visual.text)
    |> String.concat "" in
  expect "combining sequence and emoji survive reflow"
    (unicode_body = "e\204\129 👩‍💻");

  let prose = "Please inspect this workspace and summarize the completed command." in
  let prose_lines = wrap ~columns:26 ~measure prose in
  expect "narrow prose wraps at word boundaries when possible"
    (Array.to_list prose_lines = [
      "Please inspect this "; "workspace and summarize "; "the completed command."]);
  expect "word wrapping preserves text and viewport row count"
    (String.concat "" (Array.to_list prose_lines) = prose &&
     wrapped_count ~columns:26 ~measure prose = Array.length prose_lines);
  let markdown = create () in
  delta markdown "# Heading\n```ocaml\nlet x = 1\n```\n- bullet\n> quote\n";
  finish markdown;
  expect "streamed headings have semantic styling"
    (Array.exists (fun (visual : visual) -> visual.row.style = Subheading)
      (rendered markdown 20));
  expect "code fences remain code, not terminal controls"
    (has "╶ code · ocaml" (lines markdown 20) &&
     has "╴ end code" (lines markdown 20));
  expect "markdown prefixes become styling, not duplicated visible punctuation"
    (has "Heading" (lines markdown 20) &&
     has "bullet" (lines markdown 20) &&
     has "quote" (lines markdown 20) &&
     not (has "- bullet" (lines markdown 20)));
  let rich = create () in
  assistant rich
    "A **bold** `code` and [docs](https://example.test).\n| Name | Status |\n| ---- | ------ |\n| API | ready |";
  finish rich;
  let rich_text = rich.rows.(1) in
  expect "inline Markdown removes syntax while retaining linked destination"
    (rich_text.text = "A bold code and docs (https://example.test).");
  expect "inline bold, code and links keep distinct semantic spans"
    (Array.exists (fun run -> run.content = "bold" && run.style = Bold)
       rich_text.runs &&
     Array.exists (fun run -> run.content = "code" && run.style = Inline_code)
       rich_text.runs &&
     Array.exists (fun run -> run.content = "docs" && run.style = Link)
       rich_text.runs);
  expect "pipe table promotes header, separator and data rows"
    (rich.rows.(2).style = Table_header &&
     rich.rows.(2).text = "Name | Status" &&
     rich.rows.(3).style = Table_separator &&
     rich.rows.(4).style = Table_row &&
     rich.rows.(4).text = "API | ready");
  let rich_layout = rendered rich 13 in
  expect "inline styles survive grapheme-safe wrapping"
    (Array.exists (fun visual ->
       Array.exists (fun (run : inline_run) -> run.style = Bold) visual.runs) rich_layout &&
     Array.exists (fun visual ->
       Array.exists (fun (run : inline_run) -> run.style = Link) visual.runs) rich_layout);
  let streamed_markdown = create () in
  delta streamed_markdown
    "A **bold** `code` [docs](https://example.test).\n| Name | Status |\n| ---- | ------ |\n| API | ready |\nStill **streaming**";
  let provisional = snapshot streamed_markdown ~columns:40 ~measure in
  expect "streamed Markdown spans are styled before the turn settles"
    (Array.exists (fun (entry : entry) ->
       entry.row.provisional &&
       Array.exists (fun (run : inline_run) ->
         run.content = "streaming" && run.style = Bold) entry.row.runs)
       provisional.entries);
  finish streamed_markdown;
  expect "settled streamed prose retains Markdown spans"
    (Array.exists (fun (run : inline_run) ->
       run.content = "bold" && run.style = Bold)
       streamed_markdown.rows.(1).runs &&
     Array.exists (fun (run : inline_run) ->
       run.content = "code" && run.style = Inline_code)
       streamed_markdown.rows.(1).runs &&
     Array.exists (fun (run : inline_run) ->
       run.content = "docs" && run.style = Link)
       streamed_markdown.rows.(1).runs);
  expect "settled streamed pipe tables remain semantic"
    (streamed_markdown.rows.(2).style = Table_header &&
     streamed_markdown.rows.(3).style = Table_separator &&
     streamed_markdown.rows.(4).style = Table_row);
  expect "final provisional row settles without losing Markdown"
    (not streamed_markdown.rows.(5).provisional &&
     Array.exists (fun (run : inline_run) ->
       run.content = "streaming" && run.style = Bold)
       streamed_markdown.rows.(5).runs);
  let settled = create () in
  delta settled "Committed reply";
  finish settled;
  rollback settled;
  expect "finished stream survives later cancellation"
    (has "Committed reply" (lines settled 40) &&
      heading_count settled Assistant = 1);
  let aborted_tool = create () in
  event aborted_tool "[run_command]";
  rollback aborted_tool;
  expect "interrupted tool is not falsely shown as running"
    (has "run_command · interrupted (outcome unknown)"
      (lines aborted_tool 60));
  let failed_tool = create () in
  event failed_tool "[http_request]";
  event failed_tool "[http_request] Error: HTTP 503";
  expect "tool failure has error semantics"
    (Array.exists (fun (visual : visual) ->
      visual.row.kind = Error && visual.row.style = Tool_state)
      (rendered failed_tool 60));
  let approval_block = create () in
  approval approval_block "pwd";
  let malicious = create () in
  sent malicious "safe\027[31m\194\155unsafe\226\128\174rtl";
  expect "transcript strips C0, C1 and bidi display controls while preserving prose"
    (has "safe [31m unsafe rtl" (lines malicious 64));
  expect "approval retains exact reviewable command"
    (heading_count approval_block Approval = 1 &&
      has "pwd" (lines approval_block 60));
  let tool_approval = create () in
  approval ~title:"TOOL APPROVAL · review before deciding" tool_approval
    "Tool: write_file\nTier: WRITE\nImpact: Replaces a workspace file.\nPath: Sources/App.swift";
  expect "typed approval presents the impact and exact target"
    (heading_count tool_approval Approval = 1 &&
     has "TOOL APPROVAL · review before deciding" (lines tool_approval 64) &&
     has "Path: Sources/App.swift" (lines tool_approval 64));
  let large = create () in
  sent large (String.make 4096 'A');
  let compact = snapshot large ~columns:1 ~measure:(fun _ -> 1) in
  expect "tiny viewport layout cache remains logical-row bounded"
    (Array.length compact.entries = 2 && compact.total >= 4096 &&
      (visual_at compact 2000).text = "A");
  let different_measure = snapshot large ~columns:1 ~measure:(fun _ -> 2) in
  expect "same-width snapshots honor a changed grapheme measurer"
    ((visual_at different_measure 2000).text = "?");
  let history = create () in
  for i = 1 to 3200 do notice history (string_of_int i) done;
  let initial_history = snapshot history ~columns:8 ~measure in
  expect "long history begins with its original rows"
    (row_visual initial_history 1 = Some [|"1"|]);
  event history "[http_request]";
  let running = snapshot history ~columns:8 ~measure in
  expect "cached running tool heading is visible"
    (Array.exists (fun (entry : entry) ->
      entry.row.text = "http_request · running") running.entries);
  event history "[http_request] first output\nsecond output\nsecret output";
  let completed = snapshot history ~columns:8 ~measure in
  expect "tool heading updates after a cached running snapshot"
    (Array.exists (fun (entry : entry) ->
      entry.row.text = "http_request · completed") completed.entries &&
    not (Array.exists (fun (entry : entry) ->
      entry.row.text = "http_request · running") completed.entries));
  expect "old history and collapsed preview survive tool settlement"
    (row_visual completed 1 = Some [|"1"|] &&
    not (Array.exists (fun (entry : entry) ->
      entry.row.text = "secret output") completed.entries));
  delta history "e\204\129 👩‍💻";
  let live_source = history.count in
  let first_delta = snapshot history ~columns:8 ~measure in
  expect "first live row wraps at a grapheme boundary"
    (row_visual first_delta live_source = Some [|"e\204\129 👩‍💻"|]);
  delta history " and longer response";
  let narrow_history = snapshot history ~columns:8 ~measure in
  let wider_history = snapshot history ~columns:18 ~measure in
  let narrow_again = snapshot history ~columns:8 ~measure in
  let complete_text = "e\204\129 👩‍💻 and longer response" in
  let joined view = match row_visual view live_source with
    | None -> ""
    | Some segments -> String.concat "" (Array.to_list segments) in
  expect "long-history streaming delta replaces stale live measurements"
    (joined narrow_history = complete_text &&
    joined wider_history = complete_text &&
    joined narrow_again = complete_text &&
    Array.length (Option.get (row_visual narrow_history live_source)) >
      Array.length (Option.get (row_visual wider_history live_source)) &&
    Array.for_all (fun text -> measure text <= 8)
      (Option.get (row_visual narrow_again live_source)));
  expect "tool expansion reflows hidden details at current width"
    (Option.is_some (toggle history ~first:0 ~last:(history.count - 1)));
  let expanded_narrow = snapshot history ~columns:8 ~measure in
  let expanded_wide = snapshot history ~columns:18 ~measure in
  expect "expanded result replaces preview and preserves its text"
    (Array.exists (fun (entry : entry) ->
      entry.row.text = "secret output") expanded_narrow.entries &&
    Array.exists (fun (entry : entry) ->
      entry.row.text = "secret output") expanded_wide.entries &&
    row_visual expanded_wide live_source <> None);
  expect "collapsing after resize hides details again"
    (Option.is_some (toggle history ~first:0 ~last:(history.count - 1)));
  let collapsed_again = snapshot history ~columns:8 ~measure in
  expect "collapsed result has no stale expanded details"
    (not (Array.exists (fun (entry : entry) ->
      entry.row.text = "secret output") collapsed_again.entries) &&
    joined collapsed_again = complete_text);
  finish history;
  let finished = snapshot history ~columns:8 ~measure in
  expect "settled stream retains its final content after a cached live row"
    (joined finished = complete_text &&
    (Option.get (Array.find_opt (fun (entry : entry) ->
      entry.source = live_source) finished.entries)).row.provisional = false);
  delta history "retract this";
  ignore (snapshot history ~columns:8 ~measure);
  rollback history;
  let cancelled = snapshot history ~columns:8 ~measure in
  expect "cancellation evicts only provisional tail"
    (joined cancelled = complete_text &&
    not (Array.exists (fun (entry : entry) ->
      entry.row.text = "retract this") cancelled.entries));
  while history.count < 9_990 do notice history "fill" done;
  let before_trim = snapshot history ~columns:8 ~measure in
  expect "snapshot is near the bounded storage limit"
    (history.count < max_rows &&
    row_visual before_trim 1000 = Some [|"334"|]);
  for _ = 1 to 10 do notice history "fill" done;
  let after_trim = snapshot history ~columns:8 ~measure in
  expect "eviction shifts cached source offsets without retaining old rows"
    (history.count <= max_rows &&
    row_visual after_trim 0 = row_visual before_trim 1000);
  let many = create () in
  for i = 1 to 11_000 do notice many (string_of_int i) done;
  expect "bounded logical transcript storage" (many.count <= max_rows);
  print_endline "semantic transcript, cancellation, expansion and resize: ok"
