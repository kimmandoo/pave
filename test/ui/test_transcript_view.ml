open Transcript_view

let fail message = failwith message
let expect label condition = if not condition then fail label
let rendered t width = layout t ~columns:width ~measure:(fun cluster ->
  Notty.I.width (Notty.I.string Notty.A.empty cluster))
let lines t width = Array.to_list (Array.map (fun visual -> visual.text) (rendered t width))
let has text lines = List.exists (String.equal text) lines
let heading_count t kind =
  let count = ref 0 in
  for i = 0 to t.count - 1 do
    let row = t.rows.(i) in
    if row.kind = kind && row.style = Heading then incr count
  done;
  !count

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
    has "http_request · done · Alt+O expand" initial &&
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
  expect "completed tool execution remains visible" (has "http_request · done · Alt+O expand" after);
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
  sent malicious "safe\027[31m\194\155unsafe";
  expect "transcript strips C0 and C1 terminal controls while preserving prose"
    (has "safe [31m unsafe" (lines malicious 64));
  expect "approval retains exact reviewable command"
    (heading_count approval_block Approval = 1 &&
      has "pwd" (lines approval_block 60));
  let large = create () in
  sent large (String.make 4096 'A');
  let compact = snapshot large ~columns:1 ~measure:(fun _ -> 1) in
  expect "tiny viewport layout cache remains logical-row bounded"
    (Array.length compact.entries = 2 && compact.total >= 4096 &&
      (visual_at compact 2000).text = "A");
  let many = create () in
  for i = 1 to 11_000 do notice many (string_of_int i) done;
  expect "bounded logical transcript storage" (many.count <= max_rows);
  print_endline "semantic transcript, cancellation, expansion and resize: ok"
