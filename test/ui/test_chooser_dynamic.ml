let chooser : Tui.chooser = {
  title = "Model";
  intro = [||];
  plain = [];
  suggestions = [| "openai/gpt-4.1"; "google/gemini-2.5-pro" |];
  choices = [| { Tui.value = "openai/gpt-4.1"; label = "GPT-4.1 · exact";
      custom = false; verified = false; listed = false; detail = None };
    { Tui.value = "google/gemini-2.5-pro"; label = "Gemini Pro";
      custom = false; verified = false; listed = false; detail = None } |];
  allow_custom = true;
  dynamic = true;
  status = None;
  status_pages = [||];
  status_page = 0;
  filter = "gpt";
  selected = 0;
  offset = 0;
  touched = false;
  filtered = None;
}

let values () = Array.to_list (Array.map (fun (item : Tui.candidate) -> item.value)
  (Tui.matches chooser))

let () =
  Tui.update_chooser chooser
    ~verified:[ "openai/o3"; "openai/gpt-4.1"; "openai/o3" ]
    ~listed:[]
    ~details:["openai/gpt-4.1", "context 120000 tokens · APIs chat/responses"]
    ~labels:["openai/gpt-4.1", "GPT-4.1 · exact"]
    ~status:(Some "Verified IDs loaded");
  assert (values () = [ "openai/gpt-4.1" ]);
  assert (chooser.selected = 0 && chooser.filter = "gpt");
  assert ((Tui.matches chooser).(0).value = "openai/gpt-4.1" &&
    (Tui.matches chooser).(0).label = "GPT-4.1 · exact");
  chooser.filter <- "GPT-4.1 · exact";
  assert (values () = ["openai/gpt-4.1"]);
  chooser.filter <- "gpt";
  let detail = "context 120000 tokens · APIs chat/responses" in
  let rows = Tui.wrap_chooser_text ~columns:18 ~max_rows:4 detail in
  assert (String.concat " " (Array.to_list rows) = detail);
  assert (Array.for_all (fun row ->
    Notty.I.width (Notty.I.string Notty.A.empty row) <= 18) rows);
  let clipped = Tui.wrap_chooser_text ~columns:18 ~max_rows:2 detail in
  assert (Array.length clipped = 2 &&
    String.ends_with ~suffix:"…" clipped.(1));
  chooser.filter <- "openai/";
  chooser.selected <- 1;
  chooser.touched <- true;
  Tui.update_chooser chooser ~verified:[ "openai/o3"; "openai/gpt-4.1";
    "openai/gpt-5" ] ~listed:[] ~details:[]
    ~labels:["openai/gpt-4.1", "GPT 4.1 (readable)"] ~status:None;
  assert (values () = [ "openai/o3"; "openai/gpt-4.1"; "openai/gpt-5" ]);
  assert (chooser.selected = 1);
  assert ((Tui.matches chooser).(chooser.selected).value = "openai/gpt-4.1");
  assert ((Tui.matches chooser).(chooser.selected).label = "GPT 4.1 (readable)");
  chooser.filter <- "openai/gpt-4";
  chooser.selected <- 0;
  let prefix = Tui.matches chooser in
  assert (prefix.(0).value = "openai/gpt-4" && prefix.(0).custom);
  assert (prefix.(1).value = "openai/gpt-4.1");
  chooser.filter <- "openai/gpt-4.1";
  let exact = Tui.matches chooser in
  assert (Array.length exact = 1 && not exact.(0).custom);
  chooser.filter <- "local/custom";
  chooser.selected <- 0;
  Tui.update_chooser chooser ~verified:[ "openai/gpt-4.1" ]
    ~listed:[] ~details:[] ~labels:[] ~status:None;
  assert (values () = [ "local/custom" ]);
  assert ((Tui.matches chooser).(0).custom);
  assert (chooser.filter = "local/custom");
  chooser.filter <- "gemini";
  Tui.update_chooser chooser ~verified:[] ~listed:[] ~details:[] ~labels:[]
    ~status:(Some "Offline");
  assert (values () = [ "google/gemini-2.5-pro" ]);
  assert (not (Tui.matches chooser).(0).verified &&
    not (Tui.matches chooser).(0).listed);
  assert (chooser.status = Some "Offline");
  let onboarding = { chooser with
    plain = ["Back · authentication"; "Skip setup"];
    suggestions = [| "ollama/offline"; "Back · authentication"; "Skip setup" |];
    choices = Array.map (fun value ->
      { Tui.value; label = value; custom = false; verified = false;
        listed = false; detail = None })
      [| "ollama/offline"; "Back · authentication"; "Skip setup" |];
    filter = ""; selected = 0; offset = 0; touched = false;
    filtered = None } in
  Tui.update_chooser onboarding
    ~verified:["ollama/llama3.2:latest"; "ollama/qwen2.5-coder:7b"]
    ~listed:[] ~details:[] ~labels:[] ~status:(Some "2 live models");
  let shown = Tui.matches onboarding in
  assert (shown.(0).value = "ollama/llama3.2:latest" && shown.(0).verified);
  assert (shown.(2).value = "ollama/offline");
  assert (onboarding.selected = 0);
  assert (shown.(3).value = "Back · authentication");
  onboarding.selected <- 3;
  onboarding.touched <- true;
  Tui.update_chooser onboarding ~verified:["ollama/another"]
    ~listed:[] ~details:[] ~labels:[] ~status:None;
  assert ((Tui.matches onboarding).(onboarding.selected).value =
    "Back · authentication");
  let navigated = { onboarding with
    choices = [| { Tui.value = "ollama/offline"; label = "Offline";
      custom = false; verified = false; listed = false; detail = None } |];
    selected = 0; touched = true; filtered = None } in
  Tui.update_chooser navigated ~verified:["ollama/another"]
    ~listed:[] ~details:[] ~labels:[] ~status:None;
  assert ((Tui.matches navigated).(navigated.selected).value = "ollama/offline");
  let mixed = { chooser with filter = ""; touched = false; selected = 0 } in
  Tui.update_chooser mixed ~verified:["openai/new-chat"]
    ~listed:["stepfun/new-audio"; "stepfun/new-chat"]
    ~details:[]
    ~labels:["openai/new-chat", "New chat";
      "stepfun/new-audio", "Audio model"]
    ~status:(Some "3 models");
  let found = Tui.matches mixed in
  assert (Array.to_list (Array.map (fun (item : Tui.candidate) -> item.value) found) =
    ["openai/new-chat"; "stepfun/new-audio"; "stepfun/new-chat";
     "openai/gpt-4.1"; "google/gemini-2.5-pro"]);
  assert (found.(1).listed && found.(1).label = "Audio model");
  let approval_rows = Tui.approval_body_rows ~columns:8
    ~measure:(fun text ->
      Notty.I.width (Notty.I.string Notty.A.empty text))
    "Tier: WRITE\nPath: ok" in
  assert (approval_rows = 3);
  print_endline "dynamic chooser filtering and source labels: ok"
