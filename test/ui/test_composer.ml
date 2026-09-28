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

  let editor = Pave.Composer.create () in
  Pave.Composer.insert editor "語";
  Pave.Composer.insert editor "👩";
  Pave.Composer.insert editor "‍";
  Pave.Composer.insert editor "💻";
  Pave.Composer.insert editor "e";
  Pave.Composer.insert editor "́";
  assert (Pave.Composer.text editor = "語👩‍💻é");
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "");
  Pave.Composer.redo editor;
  assert (Pave.Composer.text editor = "語👩‍💻é");
  Pave.Composer.erase editor;
  assert (Pave.Composer.text editor = "語👩‍💻");
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "語👩‍💻é");
  Pave.Composer.home editor;
  Pave.Composer.delete editor;
  assert (Pave.Composer.text editor = "👩‍💻é");
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "語👩‍💻é");
  Pave.Composer.redo editor;
  assert (Pave.Composer.text editor = "👩‍💻é");
  Pave.Composer.undo editor;
  Pave.Composer.right editor;
  Pave.Composer.right editor;
  Pave.Composer.erase_word editor;
  assert (Pave.Composer.text editor = "é");
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "語👩‍💻é");
  Pave.Composer.clear editor;

  Pave.Composer.insert editor "甲👩‍💻尾\n第二行";
  Pave.Composer.kill_before editor;
  assert (Pave.Composer.text editor = "甲👩‍💻尾\n");
  Pave.Composer.yank editor;
  assert (Pave.Composer.text editor = "甲👩‍💻尾\n第二行");
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "甲👩‍💻尾\n");
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "甲👩‍💻尾\n第二行");
  Pave.Composer.redo editor;
  Pave.Composer.redo editor;
  assert (Pave.Composer.text editor = "甲👩‍💻尾\n第二行");
  Pave.Composer.beginning_of_line editor;
  assert (Pave.Composer.cursor editor = String.length "甲👩‍💻尾\n");
  Pave.Composer.home editor;
  Pave.Composer.right editor;
  Pave.Composer.kill_to_end editor;
  assert (Pave.Composer.text editor = "甲\n第二行");
  Pave.Composer.yank editor;
  assert (Pave.Composer.text editor = "甲👩‍💻尾\n第二行");
  Pave.Composer.end_of_line editor;
  assert (Pave.Composer.cursor editor = String.length "甲👩‍💻尾");
  Pave.Composer.kill_to_end editor;
  assert (Pave.Composer.text editor = "甲👩‍💻尾第二行");
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "甲👩‍💻尾\n第二行");
  Pave.Composer.clear editor;

  Pave.Composer.insert editor "prefix ";
  Pave.Composer.begin_paste editor;
  List.iter (Pave.Composer.insert editor)
    ["e"; "́"; "\n"; "👩"; "‍"; "💻"; "\027"; "\127"; "\194\155"; "\t"];
  Pave.Composer.end_paste editor;
  assert (Pave.Composer.text editor = "prefix é\n👩‍💻 ");
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "prefix ");
  Pave.Composer.redo editor;
  assert (Pave.Composer.text editor = "prefix é\n👩‍💻 ");
  Pave.Composer.clear editor;
  Pave.Composer.insert editor "👩💻";
  Pave.Composer.home editor;
  Pave.Composer.right editor;
  Pave.Composer.begin_paste editor;
  Pave.Composer.insert editor "‍";
  Pave.Composer.insert editor "字";
  Pave.Composer.end_paste editor;
  assert (Pave.Composer.text editor = "👩‍💻字");
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "👩💻");
  Pave.Composer.redo editor;
  assert (Pave.Composer.text editor = "👩‍💻字");
  Pave.Composer.clear editor;
  Pave.Composer.insert editor (String.make 16_380 'a');
  Pave.Composer.begin_paste editor;
  Pave.Composer.insert editor "👩";
  Pave.Composer.insert editor "not accepted";
  Pave.Composer.end_paste editor;
  assert (String.length (Pave.Composer.text editor) = 16_384);
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = String.make 16_380 'a');
  Pave.Composer.clear editor;

  Pave.Composer.insert editor "first";
  ignore (Pave.Composer.submit editor);
  Pave.Composer.insert editor "second";
  ignore (Pave.Composer.submit editor);
  Pave.Composer.insert editor "draft 👩‍💻";
  Pave.Composer.left editor;
  let saved = Pave.Composer.cursor editor in
  Pave.Composer.older editor;
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "second");
  Pave.Composer.erase editor;
  assert (Pave.Composer.text editor = "secon");
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "second");
  Pave.Composer.older editor;
  assert (Pave.Composer.text editor = "first");
  Pave.Composer.newer editor;
  Pave.Composer.newer editor;
  assert (Pave.Composer.text editor = "draft 👩‍💻");
  assert (Pave.Composer.cursor editor = saved);
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "");
  Pave.Composer.redo editor;
  assert (Pave.Composer.text editor = "draft 👩‍💻");
  Pave.Composer.search_older editor;
  Pave.Composer.search_cancel editor;
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "");
  let editor = Pave.Composer.create () in
  Pave.Composer.insert editor "draft 👩‍💻";
  Pave.Composer.home editor;
  Pave.Composer.right editor;
  Pave.Composer.right editor;
  let draft_cursor = Pave.Composer.cursor editor in
  let prefix = "queued\n\n" in
  assert (Pave.Composer.prepend editor prefix);
  assert (Pave.Composer.text editor = prefix ^ "draft 👩‍💻");
  assert (Pave.Composer.cursor editor = String.length prefix + draft_cursor);
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "draft 👩‍💻");
  assert (Pave.Composer.cursor editor = draft_cursor);
  Pave.Composer.redo editor;
  assert (Pave.Composer.text editor = prefix ^ "draft 👩‍💻");
  let restored = Pave.Composer.text editor
  and restored_cursor = Pave.Composer.cursor editor in
  assert (not (Pave.Composer.prepend editor (String.make 16_384 'x')));
  assert (Pave.Composer.text editor = restored);
  assert (Pave.Composer.cursor editor = restored_cursor);

  let editor = Pave.Composer.create () in
  let measure = function "👩‍💻" -> 2 | _ -> 1 in
  Pave.Composer.insert editor "á👩‍💻z";
  Pave.Composer.home editor;
  Pave.Composer.select_right editor;
  Pave.Composer.select_right editor;
  assert (Pave.Composer.selection editor =
    Some (0, String.length "á👩‍💻"));
  Pave.Composer.insert editor "X";
  assert (Pave.Composer.text editor = "Xz");
  assert (Pave.Composer.selection editor = None);
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "á👩‍💻z");
  assert (Pave.Composer.selection editor =
    Some (0, String.length "á👩‍💻"));
  Pave.Composer.redo editor;
  assert (Pave.Composer.text editor = "Xz");
  Pave.Composer.clear editor;

  Pave.Composer.insert editor "abc";
  Pave.Composer.home editor;
  Pave.Composer.select_right editor;
  Pave.Composer.select_right editor;
  Pave.Composer.delete editor;
  assert (Pave.Composer.text editor = "c");
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "abc");
  assert (Pave.Composer.selection editor = Some (0, 2));
  Pave.Composer.right editor;
  assert (Pave.Composer.cursor editor = 2);
  assert (Pave.Composer.selection editor = None);
  Pave.Composer.clear editor;

  Pave.Composer.insert editor "ab\ncd";
  Pave.Composer.home editor;
  Pave.Composer.select_vertical ~columns:8 ~measure editor 1;
  assert (Pave.Composer.cursor editor = 3);
  assert (Pave.Composer.selection editor = Some (0, 3));
  Pave.Composer.select_end_of_line ~columns:8 ~measure editor;
  assert (Pave.Composer.cursor editor = 5);
  assert (Pave.Composer.selection editor = Some (0, 5));
  Pave.Composer.select_beginning_of_line ~columns:8 ~measure editor;
  assert (Pave.Composer.cursor editor = 3);
  assert (Pave.Composer.selection editor = Some (0, 3));
  Pave.Composer.left editor;
  assert (Pave.Composer.cursor editor = 0);
  assert (Pave.Composer.selection editor = None);

  Pave.Composer.home editor;
  Pave.Composer.select_right editor;
  Pave.Composer.erase editor;
  assert (Pave.Composer.text editor = "b\ncd");
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = "ab\ncd");
  assert (Pave.Composer.selection editor = Some (0, 1));
  Pave.Composer.clear editor;

  Pave.Composer.insert editor "Review @src/no after";
  let before_completion = Pave.Composer.text editor in
  let first = String.length "Review " and last =
    String.length "Review @src/no" in
  assert (Pave.Composer.replace_range editor ~start:first ~stop:last
    ~value:"@src/notes.md");
  assert (Pave.Composer.text editor = "Review @src/notes.md after");
  assert (Pave.Composer.cursor editor =
    first + String.length "@src/notes.md");
  Pave.Composer.undo editor;
  assert (Pave.Composer.text editor = before_completion);
  Pave.Composer.redo editor;
  assert (Pave.Composer.text editor = "Review @src/notes.md after");
  Pave.Composer.clear editor;
  Pave.Composer.insert editor "e\204\129";
  (match Pave.Composer.replace_range editor ~start:1 ~stop:2 ~value:"x" with
   | _ -> failwith "range replacement split a grapheme cluster"
   | exception Invalid_argument _ -> ());

  let pasted_editor = Pave.Composer.create () in
  Pave.Composer.insert pasted_editor "typed ";
  Pave.Composer.begin_paste pasted_editor;
  Pave.Composer.insert pasted_editor "thinkdeep";
  Pave.Composer.end_paste pasted_editor;
  assert (Pave.Composer.pasted_ranges pasted_editor =
    [String.length "typed ", String.length "typed thinkdeep"]);
  Pave.Composer.home pasted_editor;
  Pave.Composer.insert pasted_editor "prefix ";
  assert (Pave.Composer.pasted_ranges pasted_editor =
    [String.length "prefix typed ",
     String.length "prefix typed thinkdeep"]);
  Pave.Composer.undo pasted_editor;
  assert (Pave.Composer.pasted_ranges pasted_editor =
    [0, String.length "typed thinkdeep"]);
  let repeated_editor = Pave.Composer.create () in
  Pave.Composer.insert repeated_editor "thinkdeep";
  Pave.Composer.home repeated_editor;
  Pave.Composer.begin_paste repeated_editor;
  Pave.Composer.insert repeated_editor "thinkdeep ";
  Pave.Composer.end_paste repeated_editor;
  assert (Pave.Composer.pasted_ranges repeated_editor = [0, 10]);
  let expanded, names = Pave.Prompt_shortcuts.expand
    ~enabled:["thinkdeep"] ~disabled:[]
    ~paste_ranges:(Pave.Composer.pasted_ranges repeated_editor)
    (Pave.Composer.text repeated_editor) in
  assert (expanded =
    "thinkdeep Please reason carefully through this request" &&
    names = ["thinkdeep"]);
  Pave.Composer.undo repeated_editor;
  assert (Pave.Composer.text repeated_editor = "thinkdeep");
  Pave.Composer.redo repeated_editor;
  assert (Pave.Composer.text repeated_editor = "thinkdeep thinkdeep");
  Pave.Composer.clear repeated_editor;
  Pave.Composer.insert repeated_editor "thinkdeep";
  Pave.Composer.home repeated_editor;
  for _ = 1 to 9 do Pave.Composer.select_right repeated_editor done;
  Pave.Composer.begin_paste repeated_editor;
  Pave.Composer.insert repeated_editor "thinkdeep";
  Pave.Composer.end_paste repeated_editor;
  assert (Pave.Composer.pasted_ranges repeated_editor = [0, 9]);
  let expanded, names = Pave.Prompt_shortcuts.expand
    ~enabled:["thinkdeep"] ~disabled:[]
    ~paste_ranges:(Pave.Composer.pasted_ranges repeated_editor)
    (Pave.Composer.text repeated_editor) in
  assert (expanded = "thinkdeep" && names = []);
  Pave.Composer.clear repeated_editor;
  Pave.Composer.insert repeated_editor "e";
  Pave.Composer.begin_paste repeated_editor;
  Pave.Composer.insert repeated_editor "\204\129";
  Pave.Composer.end_paste repeated_editor;
  assert (Pave.Composer.pasted_ranges repeated_editor = [0, 3]);
  let restored_editor = Pave.Composer.create () in
  Pave.Composer.set restored_editor (Pave.Composer.text repeated_editor);
  Pave.Composer.restore_pasted_ranges restored_editor
    (Pave.Composer.pasted_ranges repeated_editor);
  assert (Pave.Composer.pasted_ranges restored_editor = [0, 3]);
  print_endline "terminal composer: ok"
