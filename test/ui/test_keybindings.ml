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
  (* Alt shortcuts typed under a Korean input method keep their meaning. *)
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Composer
    (`Key (`Uchar (Uchar.of_int 0x3150), [`Meta])) = Some Keybindings.Toggle_details);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Composer
    (`Key (`Uchar (Uchar.of_int 0x3160), [`Meta])) = Some Keybindings.Word_left);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Composer
    (`Key (`Uchar (Uchar.of_int 0x3150), [])) = Some (Keybindings.Insert_uchar (Uchar.of_int 0x3150)));
  (* Korean input sends ㅛ for the y key; other IME text must not decide. *)
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Approval
    (`Key (`Uchar (Uchar.of_int 0x315B), [])) = Some Keybindings.Approve);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Approval
    (`Key (`Uchar (Uchar.of_int 0x315C), [])) = Some Keybindings.Ignore);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Approval
    (`Key (`Escape, [])) = Some Keybindings.Reject);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Approval
    (`Key (`Uchar (Uchar.of_int 0x315B), [`Meta])) = Some Keybindings.Reject);
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
  let focused ~hints =
    Keybindings.focus ~paste:false ~overlay:None ~search:false ~hints in
  List.iter (fun (event, action) ->
    assert (Keybindings.resolve Keybindings.bindings (focused ~hints:true) event =
      Some action);
    assert (Keybindings.resolve Keybindings.bindings (focused ~hints:false) event =
      Some action)) [
    (`Key (`Backspace, []), Keybindings.Erase);
    (`Key (`Backspace, [`Meta]), Keybindings.Erase_word);
    (`Key (`Backspace, [`Ctrl]), Keybindings.Erase);
    (`Key (`Arrow `Left, []), Keybindings.Move_left);
    (`Key (`Delete, []), Keybindings.Delete);
    (`Key (`ASCII 'Z', [`Ctrl]), Keybindings.Undo) ];
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Hints
    (`Key (`Arrow `Down, [])) = Some Keybindings.Move_down);
  assert (Keybindings.resolve Keybindings.bindings Keybindings.Hints
    (`Key (`Enter, [])) = Some Keybindings.Accept_hint);
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
