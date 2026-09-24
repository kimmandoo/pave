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

  let measure = function "語" | "👩‍💻" -> 2 | _ -> 1 in
  Pave.Composer.insert editor "A語bc\nZ";
  let lines = Pave.Composer.layout ~columns:4 ~measure editor in
  assert (Array.length lines = 3);
  assert (Pave.Composer.position ~measure editor lines = (2, 1));
  assert (Pave.Composer.vertical ~columns:4 ~measure editor (-1));
  assert (Pave.Composer.cursor editor = String.length "A語bc");
  assert (Pave.Composer.vertical ~columns:4 ~measure editor (-1));
  assert (Pave.Composer.cursor editor = String.length "A");
  let before_resize = Pave.Composer.cursor editor in
  let narrow = Pave.Composer.layout ~columns:3 ~measure editor in
  assert (Array.length narrow = 3);
  assert (Pave.Composer.cursor editor = before_resize);
  assert (Pave.Composer.position ~measure editor narrow = (0, 1));
  Pave.Composer.clear editor;
  Pave.Composer.insert editor "語a\nb";
  let exact = Pave.Composer.layout ~columns:3 ~measure editor in
  assert (Array.length exact = 3);
  Pave.Composer.home editor;
  Pave.Composer.right editor;
  Pave.Composer.right editor;
  assert (Pave.Composer.position ~measure editor exact = (1, 0));
  Pave.Composer.clear editor;
  Pave.Composer.insert editor "abcde";
  assert (Pave.Composer.vertical ~columns:3 ~measure editor (-1));
  assert (Pave.Composer.position ~measure editor
    (Pave.Composer.layout ~columns:3 ~measure editor) = (0, 2));
  assert (Pave.Composer.vertical ~columns:3 ~measure editor 1);
  assert (Pave.Composer.cursor editor = String.length "abcde");
  Pave.Composer.clear editor;
  Pave.Composer.insert editor "echo, 語字 go";
  Pave.Composer.erase_word editor;
  assert (Pave.Composer.text editor = "echo, 語字 ");
  Pave.Composer.erase_word editor;
  assert (Pave.Composer.text editor = "echo, ");
  Pave.Composer.clear editor;
  Pave.Composer.insert editor "é 👩‍💻";
  Pave.Composer.erase_word editor;
  assert (Pave.Composer.text editor = "é ");
  Pave.Composer.erase_word editor;
  assert (Pave.Composer.text editor = "");
  Pave.Composer.insert editor "first alpha";
  ignore (Pave.Composer.submit editor);
  Pave.Composer.insert editor "second beta";
  ignore (Pave.Composer.submit editor);
  Pave.Composer.insert editor "👩‍💻 draft";
  Pave.Composer.home editor;
  Pave.Composer.right editor;
  let draft_cursor = Pave.Composer.cursor editor in
  Pave.Composer.older editor;
  Pave.Composer.newer editor;
  assert (Pave.Composer.text editor = "👩‍💻 draft");
  assert (Pave.Composer.cursor editor = draft_cursor);
  Pave.Composer.search_older editor;
  assert (Pave.Composer.search_match editor = Some "second beta");
  Pave.Composer.search_older editor;
  assert (Pave.Composer.search_match editor = Some "first alpha");
  Pave.Composer.search_cancel editor;
  assert (Pave.Composer.cursor editor = draft_cursor);
  Pave.Composer.search_older editor;
  Pave.Composer.search_insert editor "ALPHA";
  assert (Pave.Composer.text editor = "👩‍💻 draft");
  assert (Pave.Composer.cursor editor = draft_cursor);
  assert (Pave.Composer.search_match editor = Some "first alpha");
  Pave.Composer.search_accept editor;
  assert (Pave.Composer.text editor = "first alpha");
  Pave.Composer.newer editor;
  Pave.Composer.newer editor;
  assert (Pave.Composer.text editor = "👩‍💻 draft");
  assert (Pave.Composer.cursor editor = draft_cursor);
  Pave.Composer.search_older editor;
  Pave.Composer.search_insert editor "not present";
  assert (Pave.Composer.search_match editor = None);
  Pave.Composer.search_cancel editor;
  assert (Pave.Composer.text editor = "👩‍💻 draft");
  assert (Pave.Composer.cursor editor = draft_cursor);
  Pave.Composer.clear editor;
  print_endline "terminal composer: ok"
