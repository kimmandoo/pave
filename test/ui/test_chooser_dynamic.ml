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
  assert (Tui.candidate_label chooser (Tui.matches chooser).(1) =
    "• GPT-4.1 · exact");
  assert (Tui.candidate_label chooser (Tui.matches chooser).(2) =
    "↩ Back · authentication");
  chooser.filter <- "gpt-4";
  assert (values chooser = ["openai/gpt-4.1"]);
  assert (chooser.matched_models = 1);
  chooser.filter <- "unverified/suggestion";
  assert (values chooser = [] && chooser.matched_models = 0);
  assert (Tui.chooser_empty_message chooser =
    "No available models match this search");
  chooser.filter <- "openai/gpt-4";
  assert (values chooser = ["openai/gpt-4.1"]);
  chooser.filter <- "custom/provider-model";
  assert (values chooser = []);
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
  assert (Tui.chooser_empty_message chooser = "No available models yet");
  assert (Array.for_all (fun (item : Tui.candidate) -> item.action)
    (Tui.matches chooser));
  let static = { chooser with dynamic = false; plain = [];
    choices = [| model "openai/o3" "openai/o3" |];
    filter = "other/model"; filtered = None } in
  assert (values static = ["other/model"]);
  assert ((Tui.matches static).(0).custom);
  let detail = "context 120000 tokens · APIs chat/responses" in
  let rows = Tui.wrap_chooser_text ~columns:18 ~max_rows:4 detail in
  assert (String.concat " " (Array.to_list rows) = detail);
  assert (Array.for_all (fun row ->
    Notty.I.width (Notty.I.string Notty.A.empty row) <= 18) rows);
  let clipped = Tui.wrap_chooser_text ~columns:18 ~max_rows:2 detail in
  assert (Array.length clipped = 2 && String.ends_with ~suffix:"…" clipped.(1));
  let approval_rows = Tui.approval_body_rows ~columns:8
    ~measure:(fun text ->
      Notty.I.width (Notty.I.string Notty.A.empty text))
    "Tier: WRITE\nPath: ok" in
  assert (approval_rows = 3);
  assert (Tui.activity_status ~state:"Thinking" ~elapsed:0. () =
    "◐ Thinking · 0s");
  assert (Tui.activity_status ~state:"Thinking" ~elapsed:0.125 () =
    "◓ Thinking · 0s");
  assert (Tui.activity_status ~state:"Tool: read_file" ~elapsed:65. () =
    "◐ Tool: read_file · 1m05s");
  let base_status =
    Tui.activity_status ~state:"Tool: run_command" ~elapsed:1.25 () in
  let with_bytes = Tui.activity_status ~state:"Tool: run_command"
    ~elapsed:1.25 ~received_bytes:1536 ~width:80 () in
  assert (with_bytes = "◑ Tool: run_command · 1s · 1.5 KiB" &&
    not (String.contains with_bytes '%'));
  assert (Tui.activity_status ~state:"Tool: run_command" ~elapsed:1.25
    ~received_bytes:1536 ~width:(Notty.I.width
      (Notty.I.string Notty.A.empty base_status)) () = base_status);
  assert (Tui.activity_tick_delay 0. = 0.125 &&
    abs_float (Tui.activity_tick_delay 0.124 -. 0.001) < 0.000000001);
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
  assert (Tui.transcript_prefix Transcript_view.Heading false = "  ▌ ");
  assert (Tui.transcript_prefix Transcript_view.Heading true = "    ");
  assert (Tui.transcript_prefix Transcript_view.Code false = "    ");
  assert (Tui.transcript_prefix Transcript_view.List_item true = "    ");
  print_endline "dynamic chooser availability and navigation: ok"

