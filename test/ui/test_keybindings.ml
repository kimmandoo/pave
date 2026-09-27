let () =
  assert (Keybindings.conflicts Keybindings.bindings = []);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Chooser
    (`Key (`Escape, [])) = Some Keybindings.Cancel);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Chooser
    (`Key (`Escape, [`Shift])) = Some Keybindings.Cancel);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Chooser
    (`Key (`Enter, [`Meta])) = Some Keybindings.Accept);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Composer
    (`Key (`ASCII 'M', [`Meta; `Ctrl])) = Some Keybindings.Follow_up);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Composer
    (`Key (`Enter, [`Shift])) = Some Keybindings.Newline);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Approval
    (`Key (`ASCII 'y', [])) = Some Keybindings.Approve);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Approval
    (`Key (`ASCII 'n', [])) = Some Keybindings.Reject);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Paste
    (`Key (`ASCII 'C', [`Ctrl])) = Some Keybindings.Ignore);
  assert (Keybindings.focus ~paste:true ~overlay:(Some Keybindings.Approval)
    ~search:true ~hints:true = Keybindings.Paste);
  assert (Keybindings.focus ~paste:false ~overlay:(Some Keybindings.Chooser)
    ~search:true ~hints:true = Keybindings.Chooser);
  assert (Keybindings.focus ~paste:false ~overlay:None ~search:true ~hints:true
    = Keybindings.Search);
  assert (Keybindings.focus ~paste:false ~overlay:None ~search:false ~hints:true
    = Keybindings.Hints);
  let valid = Keybindings.override ~target:"composer.ctrl-p"
    ~event:(`Key (`ASCII 'Q', [`Ctrl])) () in
  assert (Result.is_ok (Keybindings.apply_overrides Keybindings.bindings [valid]));
  let conflicting = Keybindings.override ~target:"composer.enter"
    ~event:(`Key (`ASCII 'C', [`Ctrl])) () in
  assert (Result.is_error
    (Keybindings.apply_overrides Keybindings.bindings [conflicting]));
  let unknown = Keybindings.override ~target:"missing" ~event:(`Key (`Enter, [])) () in
  let duplicate = [
    Keybindings.override ~target:"composer.ctrl-p"
      ~event:(`Key (`ASCII 'Q', [`Ctrl])) ();
    Keybindings.override ~target:"composer.ctrl-p"
      ~event:(`Key (`ASCII 'R', [`Ctrl])) () ] in
  assert (Result.is_error
    (Keybindings.apply_overrides Keybindings.bindings duplicate));
  assert (Result.is_error (Keybindings.apply_overrides Keybindings.bindings [unknown]));
  let help = Keybindings.hotkeys Keybindings.bindings in
  assert (List.length help = List.length (List.sort_uniq String.compare help));
  print_endline "keybinding focus, variants and conflicts: ok"
