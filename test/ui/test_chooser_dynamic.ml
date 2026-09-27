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
  assert (Tui.activity_status ~state:"Thinking" ~elapsed:0 =
    "◐ Thinking · 0s");
  assert (Tui.activity_status ~state:"Tool: read_file" ~elapsed:65 =
    "◓ Tool: read_file · 1m05s");
  assert (Tui.transcript_prefix Transcript_view.Heading false = "  ▌ ");
  assert (Tui.transcript_prefix Transcript_view.Heading true = "    ");
  assert (Tui.transcript_prefix Transcript_view.Code false = "    ");
  assert (Tui.transcript_prefix Transcript_view.List_item true = "    ");
  print_endline "dynamic chooser availability and navigation: ok"
