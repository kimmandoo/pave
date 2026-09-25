let chooser : Tui.chooser = {
  title = "Model";
  intro = [||];
  plain = [];
  suggestions = [| "openai/gpt-4.1"; "google/gemini-2.5-pro" |];
  choices = [| { Tui.value = "openai/gpt-4.1"; custom = false; verified = false; listed = false };
    { Tui.value = "google/gemini-2.5-pro"; custom = false; verified = false; listed = false } |];
  allow_custom = true;
  dynamic = true;
  status = None;
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
    ~listed:[] ~status:(Some "Verified IDs loaded");
  assert (values () = [ "openai/gpt-4.1" ]);
  assert (chooser.selected = 0 && chooser.filter = "gpt");
  assert (Tui.candidate_label chooser (Tui.matches chooser).(0)
    = "[verified] openai/gpt-4.1");
  chooser.filter <- "openai/";
  chooser.selected <- 1;
  chooser.touched <- true;
  Tui.update_chooser chooser ~verified:[ "openai/o3"; "openai/gpt-4.1";
    "openai/gpt-5" ] ~listed:[] ~status:None;
  assert (values () = [ "openai/o3"; "openai/gpt-4.1"; "openai/gpt-5" ]);
  assert (chooser.selected = 1);
  assert (Tui.candidate_label chooser (Tui.matches chooser).(0)
    = "[verified] openai/o3");
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
  Tui.update_chooser chooser ~verified:[ "openai/gpt-4.1" ] ~listed:[] ~status:None;
  assert (values () = [ "local/custom" ]);
  assert ((Tui.matches chooser).(0).custom);
  assert (Tui.candidate_label chooser (Tui.matches chooser).(0) = "Use: local/custom");
  assert (chooser.filter = "local/custom");
  chooser.filter <- "gemini";
  Tui.update_chooser chooser ~verified:[] ~listed:[] ~status:(Some "Offline");
  assert (values () = [ "google/gemini-2.5-pro" ]);
  assert (Tui.candidate_label chooser (Tui.matches chooser).(0)
    = "[suggested] google/gemini-2.5-pro");
  assert (chooser.status = Some "Offline");
  let onboarding = { chooser with
    plain = ["Back · authentication"; "Skip setup"];
    suggestions = [| "ollama/offline"; "Back · authentication"; "Skip setup" |];
    choices = Array.map (fun value ->
      { Tui.value; custom = false; verified = false; listed = false })
      [| "ollama/offline"; "Back · authentication"; "Skip setup" |];
    filter = ""; selected = 0; offset = 0; touched = false;
    filtered = None } in
  Tui.update_chooser onboarding
    ~verified:["ollama/llama3.2:latest"; "ollama/qwen2.5-coder:7b"]
    ~listed:[] ~status:(Some "2 live models");
  let shown = Tui.matches onboarding in
  assert (shown.(0).value = "ollama/llama3.2:latest");
  assert (Tui.candidate_label onboarding shown.(0) =
    "[verified] ollama/llama3.2:latest");
  assert (shown.(2).value = "ollama/offline");
  assert (onboarding.selected = 0);
  assert (Tui.candidate_label onboarding shown.(3) = "Back · authentication");
  onboarding.selected <- 3;
  onboarding.touched <- true;
  Tui.update_chooser onboarding ~verified:["ollama/another"]
    ~listed:[] ~status:None;
  assert ((Tui.matches onboarding).(onboarding.selected).value =
    "Back · authentication");
  let navigated = { onboarding with
    choices = [| { Tui.value = "ollama/offline"; custom = false;
      verified = false; listed = false } |];
    selected = 0; touched = true; filtered = None } in
  Tui.update_chooser navigated ~verified:["ollama/another"]
    ~listed:[] ~status:None;
  assert ((Tui.matches navigated).(navigated.selected).value = "ollama/offline");
  let mixed = { chooser with filter = ""; touched = false; selected = 0 } in
  Tui.update_chooser mixed ~verified:["openai/new-chat"]
    ~listed:["stepfun/new-audio"; "stepfun/new-chat"]
    ~status:(Some "3 models");
  let found = Tui.matches mixed in
  assert (Array.to_list (Array.map (fun (item : Tui.candidate) -> item.value) found) =
    ["openai/new-chat"; "stepfun/new-audio"; "stepfun/new-chat";
     "openai/gpt-4.1"; "google/gemini-2.5-pro"]);
  assert (Tui.candidate_label mixed found.(1) =
    "[listed · API unverified] stepfun/new-audio");
  let approval_rows = Tui.approval_body_rows ~columns:8
    ~measure:(fun text ->
      Notty.I.width (Notty.I.string Notty.A.empty text))
    "Tier: WRITE\nPath: ok" in
  assert (approval_rows = 3);
  print_endline "dynamic chooser filtering and source labels: ok"
