open Transcript_view

let fail message = failwith message
let expect label condition = if not condition then fail label
let measure cluster = Notty.I.width (Notty.I.string Notty.A.empty cluster)
let rendered t width = layout t ~columns:width ~measure
let lines t width = Array.to_list (Array.map (fun visual -> visual.text) (rendered t width))
let has text lines = List.exists (String.equal text) lines
let has_tool_state t kind status =
  let prefix = status ^ " · " in
  let rec find index =
    if index = t.count then false
    else
      let row = t.rows.(index) in
      (row.kind = kind && row.style = Tool_state &&
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
  expect "settled tool has one title, outcome, and a compact preview"
    (has "http_request" initial &&
    not (has "http_request · running" initial) &&
    has_tool_state transcript Tool "completed" &&
    has "HTTP/1.1 200 OK" initial &&
    not (has "line two" initial));
  expect "collapsed output hides remaining lines" (not (has "secret later line" initial));
  expect "tool expansion chooses current tool"
    (Option.is_some (toggle transcript ~first:0 ~last:(transcript.count - 1)));
  let expanded = lines transcript 64 in
  expect "expanded output contains full result"
    (has "secret later line" expanded && has "fourth line" expanded);
  expect "tool collapse restores compact result"
    (Option.is_some (toggle transcript ~first:0 ~last:(transcript.count - 1)));
  expect "collapsed result hidden again" (not (has "secret later line" (lines transcript 64)));
  let read_cards = create () in
  let first_read = start_tool ~target:"src/one.md" read_cards "read_file" in
  let second_read = start_tool ~target:"src/two.md" read_cards "read_file" in
  tool_result ~group:second_read read_cards "read_file" "---\n# Two\nbody";
  tool_result ~group:first_read read_cards "read_file" "---\n# One\nbody";
  let compact = rendered read_cards 80 in
  let read_rows = Array.to_list compact
    |> List.filter (fun (visual : visual) ->
      visual.row.style <> Divider) in
  expect "repeated successful file reads show one distinguishable row each"
    (List.length read_rows = 2 &&
     List.exists (fun (visual : visual) ->
       visual.text = "read_file · src/one.md · 3 lines · collapsed" &&
       visual.row.style = Tool_summary) read_rows &&
     List.exists (fun (visual : visual) ->
       visual.text = "read_file · src/two.md · 3 lines · collapsed" &&
       visual.row.style = Tool_summary) read_rows &&
     not (has "---" (lines read_cards 80)));
  expect "last read remains independently expandable"
    (Option.value ~default:0
      (toggle read_cards ~first:0 ~last:(read_cards.count - 1)) =
       second_read &&
     has "Two" (lines read_cards 80) &&
     not (has "One" (lines read_cards 80)));
  ignore (toggle read_cards ~first:0 ~last:(read_cards.count - 1));
  expect "collapsed reads hide returned text again"
    (not (has "Two" (lines read_cards 80)));
  let failed_read = start_tool ~target:"src/missing.md" read_cards
    "read_file" in
  tool_result ~group:failed_read ~is_error:true read_cards "read_file"
    "Error: missing file";
  expect "failed read keeps its file identity and visible error"
    (has "read_file · src/missing.md" (lines read_cards 80) &&
     has "Error: missing file" (lines read_cards 80));
  let public_url = create () in
  let url_read = start_tool ~target:"https://example.test/file?token=secret"
    public_url "read_file" in
  tool_result ~group:url_read public_url "read_file" "safe content";
  expect "compact file reads do not expose URL query strings as targets"
    (has "read_file · 1 line · collapsed" (lines public_url 80) &&
     not (List.exists (fun line -> String.contains line '?')
       (lines public_url 80)));
  rollback transcript;
  let after = lines transcript 64 in
  expect "cancel retracts both assistant segments" (heading_count transcript Assistant = 0 &&
    not (has "Building a client" after) &&
    not (has "Final **response**" after));
  expect "completed tool execution remains visible"
    (has_tool_state transcript Tool "completed");
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
  expect "code fences retain language and fence boundary semantics"
    (has "code · ocaml" (lines markdown 20) &&
     has "end code" (lines markdown 20));
  expect "markdown prefixes become styling, not duplicated visible punctuation"
    (has "Heading" (lines markdown 20) &&
     has "bullet" (lines markdown 20) &&
     has "quote" (lines markdown 20) &&
     not (has "- bullet" (lines markdown 20)));
  let live_markdown = create () in
  delta live_markdown "> **quoted**";
  let live_quote = rendered live_markdown 30 in
  expect "live quote removes markup but retains semantic inline style"
    (Array.exists (fun (visual : visual) ->
       visual.row.style = Quote && visual.text = "quoted" &&
       Array.exists (fun (run : inline_run) -> run.style = Bold &&
         run.content = "quoted") visual.runs) live_quote);
  delta live_markdown "\n```ocaml";
  expect "live code fence uses the same language label as settled output"
    (Array.exists (fun (visual : visual) ->
       visual.row.style = Code && visual.text = "code · ocaml")
       (rendered live_markdown 30));
  let fenced_diff = create () in
  delta fenced_diff
    "Patch:\n```diff\ndiff --git a/a.txt b/a.txt\n--- a/a.txt\n+++ b/a.txt\n@@ -1,2 +1,2 @@\n-old\n+new\n unchanged\n```\n- ordinary item";
  finish fenced_diff;
  let patch_rows = Array.sub fenced_diff.rows 0 fenced_diff.count in
  let patch_row text =
    Option.get (Array.find_opt (fun (row : row) -> row.text = text) patch_rows) in
  expect "fenced diff preserves markers and styles its header, hunk and edits"
    ((patch_row "diff --git a/a.txt b/a.txt").style = Diff_header &&
     (patch_row "@@ -1,2 +1,2 @@").style = Diff_hunk &&
     (patch_row "-old").style = Diff_remove &&
     (patch_row "+new").style = Diff_add &&
     (patch_row " unchanged").style = Diff_context);
  expect "ordinary Markdown after a diff remains a list"
    ((patch_row "ordinary item").style = List_item);
  let raw_diff = create () in
  event raw_diff
    "[run_command] Status: exit 0\ndiff --git a/a.txt b/a.txt\nindex 123..456 100644\n--- a/a.txt\n+++ b/a.txt\n@@ -1 +1 @@\n- old\n+ new\n unchanged\n\n- later prose";
  let collapsed_diff = rendered raw_diff 64 in
  expect "collapsed command diff previews a file header, not the shell status"
    (Array.exists (fun (visual : visual) ->
      visual.row.preview && visual.text = "diff --git a/a.txt b/a.txt")
      collapsed_diff);
  ignore (toggle raw_diff ~first:0 ~last:(raw_diff.count - 1));
  let raw_rows = Array.sub raw_diff.rows 0 raw_diff.count in
  let raw_row text =
    Option.get (Array.find_opt (fun (row : row) -> row.text = text) raw_rows) in
  expect "raw diff preserves removed markers and resets for later Markdown"
    ((raw_row "- old").style = Diff_remove &&
     (raw_row "+ new").style = Diff_add &&
     (raw_row " unchanged").style = Diff_context &&
     (raw_row "later prose").style = List_item);
  let compact_diff = rendered raw_diff 13 in
  expect "narrow diff wraps without dropping change markers"
    (Array.for_all (fun (visual : visual) -> measure visual.text <= 13)
      compact_diff &&
     Array.exists (fun (visual : visual) ->
       visual.text = "- old" && not visual.continuation) compact_diff);
  let bare_diff = create () in
  event bare_diff
    "[read_file] --- a/old.txt\n+++ b/new.txt\n@@ -1 +1 @@\n-before\n+after";
  expect "bare unified diff without git header keeps file and change roles"
    (Array.exists (fun (row : row) ->
       row.text = "--- a/old.txt" && row.style = Diff_header)
       (Array.sub bare_diff.rows 0 bare_diff.count) &&
     Array.exists (fun (row : row) ->
       row.text = "-before" && row.style = Diff_remove)
       (Array.sub bare_diff.rows 0 bare_diff.count));
  let fragmented = create () in
  delta fragmented "```di";
  delta fragmented "ff\n+after\n";
  finish fragmented;
  expect "stream fragments keep diff fence semantics"
    (Array.exists (fun (row : row) ->
       row.text = "+after" && row.style = Diff_add)
       (Array.sub fragmented.rows 0 fragmented.count));
  let ordinary_code = create () in
  assistant ordinary_code "```text\n- not a diff\n+ not a diff\n```\n- item";
  expect "regular code and prose do not acquire diff styling"
    (Array.exists (fun (row : row) ->
       row.text = "- not a diff" && row.style = Code)
       (Array.sub ordinary_code.rows 0 ordinary_code.count) &&
     Array.exists (fun (row : row) ->
       row.text = "item" && row.style = List_item)
       (Array.sub ordinary_code.rows 0 ordinary_code.count));
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
  expect "tool failure has distinct error outcome and preserved diagnostics"
    (has_tool_state failed_tool Error "failed" &&
     has "Error: HTTP 503" (lines failed_tool 60));
  let compact_tool = create () in
  event compact_tool ("[run_command] \n```text\n" ^
    String.make 280 'x' ^ "\n```\nprivate diagnostics");
  expect "collapsed tool skips empty and fence-only previews"
    (not (has "private diagnostics" (lines compact_tool 32)) &&
     Array.exists (fun (visual : visual) ->
       visual.row.preview && visual.row.style = Code)
       (rendered compact_tool 32));
  ignore (toggle compact_tool ~first:0 ~last:(compact_tool.count - 1));
  let full_tool = rendered compact_tool 16 in
  expect "expanded long code wraps without losing original content"
    (Array.for_all (fun (visual : visual) -> measure visual.text <= 16)
       full_tool &&
     has "private diagnostics" (lines compact_tool 32) &&
     Array.exists (fun (row : row) ->
       row.detail && row.style = Code && row.text = String.make 280 'x')
       (Array.sub compact_tool.rows 0 compact_tool.count));
  let final_answer = create () in
  event final_answer "[run_command] done";
  delta final_answer "Final answer";
  finish final_answer;
  let final_rows = rendered final_answer 30 in
  let last = final_rows.(Array.length final_rows - 1) in
  expect "completed answer stays the last visible row at 30 by 3"
    (last.row.kind = Assistant && last.row.style = Text &&
     last.text = "Final answer" && not last.row.provisional &&
     final_rows.(Array.length final_rows - 2).row.style = Heading);
  let persisted_answer = create () in
  assistant persisted_answer "Persisted answer\n\n";
  let persisted_rows = rendered persisted_answer 30 in
  expect "saved answer omits terminal blank rows but retains its content"
    (Array.length persisted_rows = 2 &&
     persisted_rows.(1).row.kind = Assistant &&
     persisted_rows.(1).text = "Persisted answer");
  let streamed_answer = create () in
  delta streamed_answer "Streamed answer\n\n";
  finish streamed_answer;
  let streamed_rows = rendered streamed_answer 30 in
  expect "settled stream leaves the answer, not a blank line, at viewport end"
    (Array.length streamed_rows = 2 &&
     streamed_rows.(1).row.kind = Assistant &&
     streamed_rows.(1).text = "Streamed answer");
  let malicious = create () in
  sent malicious "safe\027[31m\194\155unsafe\226\128\174rtl";
  expect "transcript strips C0, C1 and bidi display controls while preserving prose"
    (has "safe [31m unsafe rtl" (lines malicious 64));
  let hostile_tool = create () in
  event hostile_tool "[run_command] safe\027[31m\194\155unsafe\nprivate detail";
  expect "collapsed tool preview sanitizes output without revealing later lines"
    (has "safe [31m unsafe" (lines hostile_tool 64) &&
     not (has "private detail" (lines hostile_tool 64)));
  ignore (toggle hostile_tool ~first:0 ~last:(hostile_tool.count - 1));
  expect "expanded tool diagnostics stay sanitized"
    (has "private detail" (lines hostile_tool 64) &&
     Array.for_all (fun (visual : visual) ->
       not (String.contains visual.text '\027'))
       (rendered hostile_tool 64));
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
  expect "settled tool shows its identity and a collapsed successful outcome"
    (Array.exists (fun (entry : entry) ->
      entry.row.kind = Tool && entry.row.style = Heading &&
      entry.row.text = "http_request") completed.entries &&
    Array.exists (fun (entry : entry) ->
      entry.row.kind = Tool && entry.row.style = Tool_state &&
      String.starts_with ~prefix:"completed" entry.row.text &&
      String.ends_with ~suffix:"collapsed" entry.row.text)
      completed.entries &&
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
  let writes = create () in
  let first = start_write writes and second = start_write writes in
  let one = Pave.Write_preview.of_values ~path:"one.ml" ~content:"one\nlive" in
  let two = Pave.Write_preview.of_values ~path:"two.ml" ~content:"two" in
  write_preview writes first one "generating draft · not written";
  write_preview writes second two "queued · not written";
  let first_card = Hashtbl.find writes.writes first in
  let second_card = Hashtbl.find writes.writes second in
  expect "both independent write drafts are visible before completion"
    (first_card.path = Some "one.ml" && second_card.path = Some "two.ml" &&
     List.exists (fun (row : row) ->
       String.ends_with ~suffix:"one" row.text) first_card.code &&
     List.exists (fun (row : row) ->
       String.ends_with ~suffix:"live" row.text) first_card.code);
  write_state writes second "writing";
  tool_result ~group:second writes "write_file" "Wrote two.ml";
  finish_write writes second ~aborted:false ~is_error:false;
  expect "out-of-order settlement preserves the other live card"
    (Hashtbl.mem writes.writes first && not (Hashtbl.mem writes.writes second) &&
     second_card.executing && heading_count writes Tool = 2);
  finish_write writes first ~aborted:true ~is_error:false;
  expect "queued cancellation clears the live card without executing it"
    (not first_card.executing && not (Hashtbl.mem writes.writes first));
  let denied = start_write writes in
  write_preview writes denied one "queued · not written";
  let denied_card = Hashtbl.find writes.writes denied in
  finish_write writes denied ~aborted:false ~is_error:true;
  expect "denial and pre-execution error clear the card without executing it"
    (not denied_card.executing && not (Hashtbl.mem writes.writes denied));
  print_endline "semantic transcript, cancellation, expansion and resize: ok"
