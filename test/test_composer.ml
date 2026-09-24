let () =
  let editor = Pave.Composer.create () in
  Pave.Composer.insert editor "Swift 앱";
  assert (Pave.Composer.text editor = "Swift 앱");
  Pave.Composer.left editor;
  Pave.Composer.erase editor;
  assert (Pave.Composer.text editor = "Swift앱");
  Pave.Composer.home editor;
  Pave.Composer.insert editor "모바일 ";
  assert (Pave.Composer.text editor = "모바일 Swift앱");
  assert (Pave.Composer.submit editor = Some "모바일 Swift앱");
  Pave.Composer.insert editor "draft";
  Pave.Composer.older editor;
  assert (Pave.Composer.text editor = "모바일 Swift앱");
  Pave.Composer.newer editor;
  assert (Pave.Composer.text editor = "draft");
  Pave.Composer.clear editor;
  assert (Pave.Composer.submit editor = None);
  Pave.Composer.insert editor "한글語";
  Pave.Composer.left editor;
  Pave.Composer.erase editor;
  assert (Pave.Composer.text editor = "한語");
  Pave.Composer.clear editor;
  Pave.Composer.insert editor "👩‍💻개발";
  Pave.Composer.home editor;
  Pave.Composer.delete editor;
  assert (Pave.Composer.text editor = "개발");
  Pave.Composer.clear editor;
  print_endline "terminal composer: ok"
