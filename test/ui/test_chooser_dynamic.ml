let chooser : Tui.chooser = {
  title = "Model";
  suggestions = [| "openai/gpt-4.1"; "google/gemini-2.5-pro" |];
  choices = [| { Tui.value = "openai/gpt-4.1"; custom = false; verified = false };
    { Tui.value = "google/gemini-2.5-pro"; custom = false; verified = false } |];
  allow_custom = true;
  dynamic = true;
  status = None;
  filter = "gpt";
  selected = 0;
  offset = 0;
}

let values () = Array.to_list (Array.map (fun (item : Tui.candidate) -> item.value)
  (Tui.matches chooser))

let () =
  Tui.update_chooser chooser
    ~verified:[ "openai/o3"; "openai/gpt-4.1"; "openai/o3" ]
    ~status:(Some "Verified IDs loaded");
  assert (values () = [ "openai/gpt-4.1" ]);
  assert (chooser.selected = 0 && chooser.filter = "gpt");
  assert (Tui.candidate_label chooser (Tui.matches chooser).(0)
    = "[verified] openai/gpt-4.1");
  chooser.filter <- "openai/";
  chooser.selected <- 1;
  Tui.update_chooser chooser ~verified:[ "openai/o3"; "openai/gpt-4.1";
    "openai/gpt-5" ] ~status:None;
  assert (values () = [ "openai/gpt-4.1"; "openai/o3"; "openai/gpt-5" ]);
  assert (chooser.selected = 1);
  assert (Tui.candidate_label chooser (Tui.matches chooser).(1)
    = "[verified] openai/o3");
  chooser.filter <- "local/custom";
  chooser.selected <- 0;
  Tui.update_chooser chooser ~verified:[ "openai/gpt-4.1" ] ~status:None;
  assert (values () = [ "local/custom" ]);
  assert ((Tui.matches chooser).(0).custom);
  assert (Tui.candidate_label chooser (Tui.matches chooser).(0) = "Use: local/custom");
  assert (chooser.filter = "local/custom");
  chooser.filter <- "gemini";
  Tui.update_chooser chooser ~verified:[] ~status:(Some "Offline");
  assert (values () = [ "google/gemini-2.5-pro" ]);
  assert (Tui.candidate_label chooser (Tui.matches chooser).(0)
    = "[suggested] google/gemini-2.5-pro");
  assert (chooser.status = Some "Offline");
  print_endline "dynamic chooser filtering and source labels: ok"
