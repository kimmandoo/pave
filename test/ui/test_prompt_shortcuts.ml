let fail label = failwith ("prompt shortcuts: " ^ label)
let expect label condition = if not condition then fail label

let expand ?(enabled = ["thinkdeep"]) ?(disabled = []) ?(paste_ranges = []) text =
  Pave.Prompt_shortcuts.expand ~enabled ~disabled ~paste_ranges text

let () =
  let prose, names = expand "Please thinkdeep." in
  expect "prose expands to ordinary request language"
    (prose = "Please Please reason carefully through this request.");
  expect "expanded name is returned for visible notice" (names = ["thinkdeep"]);

  let unchanged, names = expand ~enabled:["thinkdeep"] ~disabled:["thinkdeep"]
      "thinkdeep" in
  expect "disabled takes precedence" (unchanged = "thinkdeep" && names = []);
  let unchanged, names = expand ~enabled:[] "thinkdeep verifyfirst" in
  expect "non-opt-in tokens stay unchanged" (unchanged = "thinkdeep verifyfirst" && names = []);

  let text = "thinkdeep `thinkdeep` and ``thinkdeep``\n" ^
    "```text\nthinkdeep\n```\n~~~\nverifyfirst\n~~~\n" ^
    "./thinkdeep thinkdeep.md .thinkdeep foo.thinkdeep @thinkdeep" in
  let expanded, names = expand ~enabled:["thinkdeep"; "verifyfirst"] text in
  expect "code, fences, paths, and mentions are excluded"
    (expanded = "Please reason carefully through this request `thinkdeep` and ``thinkdeep``\n" ^
     "```text\nthinkdeep\n```\n~~~\nverifyfirst\n~~~\n" ^
     "./thinkdeep thinkdeep.md .thinkdeep foo.thinkdeep @thinkdeep");
  expect "only visible shortcut is reported" (names = ["thinkdeep"]);

  let repeated, names = expand "thinkdeep, then thinkdeep; finally verifyfirst." 
      ~enabled:["thinkdeep"; "verifyfirst"] in
  expect "all repeated tokens expand"
    (repeated = "Please reason carefully through this request, then Please reason carefully through this request; finally Please verify the relevant details before answering.");
  expect "visible names are unique and in first-seen order"
    (names = ["thinkdeep"; "verifyfirst"]);

  let prefix = "世界 says " in
  let text = prefix ^ "thinkdeep remains" in
  let start = String.length prefix in
  let pasted, names = expand ~paste_ranges:[(start, start + String.length "thinkdeep")]
      text in
  expect "pasted token uses byte offsets after multibyte text"
    (pasted = text && names = []);
  let outside_paste, names = expand ~paste_ranges:[(0, String.length prefix)] text in
  expect "non-overlapping multibyte paste range preserves prose token"
    (outside_paste = prefix ^ "Please reason carefully through this request remains" &&
     names = ["thinkdeep"])
