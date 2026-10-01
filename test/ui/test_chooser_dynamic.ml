let model value label : Tui.candidate = {
  value; label; custom = false; verified = true; action = false;
  detail = None;
}

let chooser : Tui.chooser = {
  title = "Model";
  intro = [||];
  plain = ["Back · authentication"; "Skip setup"];
  choices = [| model "unverified/suggestion" "Suggestion" |];
  allow_custom = true;
  dynamic = true;
  segmented = false;
  empty_message = "No available models yet";
  count_label = "available";
  scope_action = None;
  status = Some "Loading available models";
  status_pages = [||];
  status_page = 0;
  filter = "";
  selected = 0;
  offset = 0;
  touched = false;
  filtered = None;
  matched_models = 0;
}

let values chooser =
  Array.to_list (Array.map (fun (item : Tui.candidate) -> item.value)
    (Tui.matches chooser))

let () =
  Tui.update_chooser chooser ~verified:[ "openai/o3"; "openai/gpt-4.1";
    "openai/o3" ]
    ~details:["openai/gpt-4.1", "context 120000 tokens · APIs chat/responses"]
    ~labels:["openai/gpt-4.1", "GPT-4.1 · exact"]
    ~status:(Some "Available IDs loaded");
  assert (values chooser = ["openai/o3"; "openai/gpt-4.1";
    "Back · authentication"; "Skip setup"]);
  assert (chooser.matched_models = 2 && chooser.selected = 0);
  assert ((Tui.matches chooser).(1).label = "GPT-4.1 · exact");
  chooser.filter <- "gpt-4";
  assert (values chooser = ["openai/gpt-4.1"]);
  assert (chooser.matched_models = 1);
  chooser.filter <- "unverified/suggestion";
  assert (values chooser = [] && chooser.matched_models = 0);
  chooser.filter <- "openai/gpt-4";
  assert (values chooser = ["openai/gpt-4.1"]);
  chooser.filter <- "custom/provider-model";
  assert (values chooser = []);
  chooser.filter <- "";
  Tui.update_chooser chooser ~preferred:"openai/gpt-4.1"
    ~verified:["openai/o3"; "openai/gpt-4.1"] ~details:[] ~labels:[]
    ~status:None;
  assert ((Tui.matches chooser).(chooser.selected).value = "openai/gpt-4.1");
  Tui.update_chooser chooser ~preferred:"openai/missing"
    ~verified:["openai/o3"; "openai/gpt-4.1"] ~details:[] ~labels:[]
    ~status:None;
  assert (chooser.selected = 0);
  chooser.filter <- "";
  chooser.selected <- 1;
  chooser.touched <- true;
  Tui.update_chooser chooser ~verified:["openai/o3"; "openai/gpt-4.1";
    "openai/gpt-5"] ~details:[]
    ~labels:["openai/gpt-4.1", "GPT 4.1"] ~status:None;
  assert ((Tui.matches chooser).(chooser.selected).value = "openai/gpt-4.1");
  chooser.filter <- "gpt-4";
  chooser.filtered <- None;
  chooser.selected <- 0;
  assert ((Tui.matches chooser).(chooser.selected).value = "openai/gpt-4.1");
  Tui.update_chooser chooser ~verified:["openai/gpt-4.2"; "openai/gpt-5";
    "openai/gpt-4.1"] ~details:[] ~labels:[] ~status:None;
  assert (chooser.filter = "gpt-4" && chooser.selected = 1 &&
    (Tui.matches chooser).(chooser.selected).value = "openai/gpt-4.1");
  chooser.filter <- "";
  chooser.filtered <- None;
  chooser.selected <- 3;
  Tui.update_chooser chooser ~verified:["openai/gpt-5"] ~details:[]
    ~labels:[] ~status:(Some "One available model");
  assert (values chooser = ["openai/gpt-5"; "Back · authentication";
    "Skip setup"]);
  assert ((Tui.matches chooser).(chooser.selected).value = "Back · authentication");
  Tui.update_chooser chooser ~verified:[] ~details:[] ~labels:[]
    ~status:(Some "Provider offline");
  assert (values chooser = ["Back · authentication"; "Skip setup"]);
  assert (chooser.matched_models = 0 && chooser.status = Some "Provider offline");
  assert (Array.for_all (fun (item : Tui.candidate) -> item.action)
    (Tui.matches chooser));
  let static = { chooser with dynamic = false; plain = [];
    choices = [| model "openai/o3" "openai/o3" |];
    filter = "other/model"; filtered = None } in
  assert (values static = ["other/model"]);
  assert ((Tui.matches static).(0).custom);
  let scoped = { chooser with plain = []; choices = [||];
    filter = ""; filtered = None; touched = false } in
  let id_a = "openai@chat#team-a/company/very-long-model-name-suffix-A" in
  let id_b = "openai@chat#team-b/company/very-long-model-name-suffix-B" in
  Tui.update_chooser scoped ~verified:[id_a; id_b]
    ~labels:[id_a, "openai@chat#team-a · company/very-long-model-name-suffix-A";
      id_b, "openai@chat#team-b · company/very-long-model-name-suffix-B"]
    ~details:[id_a, "exact identity " ^ id_a;
      id_b, "exact identity " ^ id_b] ~status:None;
  let a = (Tui.matches scoped).(0) and b = (Tui.matches scoped).(1) in
  let visible width (item : Tui.candidate) =
    Tui.shorten_model_label width item.label in
  let measure text = Notty.I.width (Notty.I.string Notty.A.empty text) in
  let contains_ellipsis text =
    let rec find i =
      i + String.length "…" <= String.length text &&
      (String.sub text i (String.length "…") = "…" || find (i + 1)) in
    find 0 in
  assert (a.value = id_a && b.value = id_b &&
    a.detail = Some ("exact identity " ^ id_a) &&
    b.detail = Some ("exact identity " ^ id_b));
  List.iter (fun width ->
    let first = visible width a and second = visible width b in
    assert (measure first <= width && measure second <= width &&
      String.ends_with ~suffix:"A" first &&
      String.ends_with ~suffix:"B" second &&
      first <> second &&
      (width > 24 || (contains_ellipsis first &&
        contains_ellipsis second)))) [24; 62];
  scoped.filter <- "team-b";
  assert (values scoped = [id_b]);
  scoped.filter <- "suffix-A";
  assert (values scoped = [id_a]);
  scoped.filter <- "";
  scoped.selected <- 1;
  assert ((Tui.matches scoped).(scoped.selected).value = id_b);
  let review : Tui.approval_view = {
    heading = "Tool permission"; context = "One action";
    lines = ["Tool: write_file"; "Tier: WRITE"; "Path: reviewed.txt"; "Content: exact"];
    primary = 1; wrap_lines = true; notice = ""; preview_cache = None;
    buttons = [| "n", "Deny", ""; "y", "Allow once", "" |]; focus = 0
  } in
  assert (Tui.approval_fits ~cols:24 ~rows:11 ~activity:0 review);
  assert (not (Tui.approval_fits ~cols:24 ~rows:10 ~activity:0 review));
  assert (not (Tui.approval_fits ~cols:24 ~rows:11 ~activity:1 review));
  assert (Tui.approval_fits ~cols:24 ~rows:12 ~activity:1 review);
  assert (not (Tui.approval_fits ~cols:23 ~rows:30 ~activity:0 review));
  let unwrapped = { review with lines = [String.make 40 'x'];
    wrap_lines = false; preview_cache = None } in
  assert (not (Tui.approval_fits ~cols:24 ~rows:30 ~activity:0 unwrapped));
  assert (Tui.approval_fits ~cols:44 ~rows:10 ~activity:0 unwrapped);
  let glyph status = String.sub status 0 (String.index status ' ') in
  let idle_progress = Tui.activity_status ~state:"Thinking" ~elapsed:0. () in
  let frames = List.init 10 (fun frame ->
    glyph (Tui.activity_status ~state:"Thinking"
      ~elapsed:(0.04 +. 0.08 *. float_of_int frame) ())) in
  assert (idle_progress =
    Tui.activity_status ~state:"Thinking" ~elapsed:0.05 () &&
    glyph idle_progress <>
      glyph (Tui.activity_status ~state:"Thinking" ~elapsed:0.125 ()) &&
    String.ends_with ~suffix:"Thinking · 0s"
      (Tui.activity_status ~state:"Thinking" ~elapsed:0.9 ()) &&
    String.ends_with ~suffix:"Thinking · 0s" idle_progress &&
    String.ends_with ~suffix:"Tool: read_file · 1m05s"
      (Tui.activity_status ~state:"Tool: read_file" ~elapsed:65. ()) &&
    List.length (List.sort_uniq String.compare frames) = 10 &&
    List.for_all (fun frame ->
      measure frame = 1 &&
      not (List.mem frame ["◐"; "◓"; "◑"; "◒"])) frames);
  let base_status =
    Tui.activity_status ~state:"Tool: run_command" ~elapsed:1.25 () in
  let with_bytes = Tui.activity_status ~state:"Tool: run_command"
    ~elapsed:1.25 ~received_bytes:1536 ~width:80 () in
  assert (String.ends_with ~suffix:"Tool: run_command · 1s · 1.5 KiB"
    with_bytes && not (String.contains with_bytes '%'));
  assert (Tui.activity_status ~state:"Tool: run_command" ~elapsed:1.25
    ~received_bytes:1536 ~width:(measure base_status) () = base_status);
  let compact = Tui.activity_status ~state:"Tool: run_command"
    ~elapsed:1.25 ~received_bytes:1536 ~width:18 () in
  assert (measure compact <= 18 &&
    String.ends_with ~suffix:" · 1s" compact &&
    String.starts_with ~prefix:"Tool: ru"
      (String.sub compact (String.length (glyph compact) + 1)
        (String.length compact - String.length (glyph compact) - 1)));
  assert (Tui.activity_tick_delay 0. = 0.08 &&
    abs_float (Tui.activity_tick_delay 0.125 -. 0.035) < 0.000001);
  let turn_started =
    Tui.activity_started_at None (Some "Thinking") 100. in
  let tool_started =
    Tui.activity_started_at turn_started (Some "Tool: run_command") 101. in
  let model_resumed =
    Tui.activity_started_at tool_started (Some "Thinking") 103. in
  assert (turn_started = Some 100. && tool_started = turn_started &&
    model_resumed = turn_started);
  let idle = Tui.activity_started_at model_resumed None 105. in
  assert (idle = None &&
    Tui.activity_started_at idle (Some "Thinking") 110. = Some 110.);
  let full_draft = Pave.Composer.create () in
  Pave.Composer.insert full_draft (String.make 16_384 'x');
  assert (Tui.draft_paste_capacity full_draft = 0);
  Pave.Composer.select_left full_draft;
  assert (Tui.draft_paste_capacity full_draft = 1);
  Pave.Composer.begin_paste full_draft;
  Pave.Composer.insert full_draft "Y";
  Pave.Composer.end_paste full_draft;
  assert (String.length (Pave.Composer.text full_draft) = 16_384 &&
    String.ends_with ~suffix:"Y" (Pave.Composer.text full_draft));
  let progress : Tui.tool_progress = {
    call_id = "call-1"; name = "run_command"; received_bytes = None;
  } in
  let active = Some progress in
  Tui.update_tool_progress active "other-call" "run_command" 512;
  Tui.update_tool_progress active "call-1" "read_file" 512;
  assert (progress.received_bytes = None);
  Tui.update_tool_progress active "call-1" "run_command" 4096;
  Tui.update_tool_progress active "call-1" "run_command" 1024;
  assert (progress.received_bytes = Some 4096 &&
    Tui.reset_tool_progress active "other-call" = active &&
    Tui.reset_tool_progress active "call-1" = None);
  assert (Tui.transcript_prefix Transcript_view.Heading false = "  ● ");
  assert (Tui.transcript_prefix Transcript_view.Heading true = "    ");
  assert (Tui.transcript_prefix Transcript_view.Code false = "  │ ");
  assert (Tui.transcript_prefix Transcript_view.List_item true = "    ");
  print_endline "dynamic chooser availability and navigation: ok"

