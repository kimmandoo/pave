let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec loop i = i + m <= n &&
    (String.sub text i m = fragment || loop (i + 1)) in
  loop 0

let args fields = `Assoc (List.map (fun (k, v) -> k, `String v) fields)
let tool_json root name fields =
  Pave.Tools.execute ~root ~name ~args:(`Assoc fields) ()
let tool root name fields = Pave.Tools.execute ~root ~name ~args:(args fields) ()
let rejected f =
  try contains (String.lowercase_ascii (f ())) "error"
  with _ -> true

let () =
  let root = Filename.temp_file "pave-tools-" "" in
  Sys.remove root; Unix.mkdir root 0o700;
  let outside = Filename.temp_file "pave-outside-" ".swift" in
  let files = ref [] and directories = ref [] in
  let create path text =
    let absolute = Filename.concat root path in
    files := absolute :: !files;
    let oc = open_out absolute in
    Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc text) in
  let directory path =
    let absolute = Filename.concat root path in
    Unix.mkdir absolute 0o700;
    directories := absolute :: !directories in
  let oc = open_out outside in output_string oc "outside secret\n"; close_out oc;
  Fun.protect ~finally:(fun () ->
    List.iter (fun path -> try Sys.remove path with Sys_error _ -> ()) !files;
    List.iter (fun path -> try Unix.rmdir path with Unix.Unix_error _ -> ()) !directories;
    Sys.remove outside; Unix.rmdir root) (fun () ->
    create "App.swift" "one\none\n";
    assert (contains (tool root "read_file" ["path", "App.swift"]) "one");
    assert (rejected (fun () -> tool root "edit_file"
      ["path", "App.swift"; "old_string", "one"; "new_string", "two"]));
    ignore (tool root "edit_file" ["path", "App.swift";
      "old_string", "one\none\n"; "new_string", "two\n"]);
    assert (contains (tool root "read_file" ["path", "App.swift"]) "two");
    let link = Filename.concat root "escape.swift" in
    Unix.symlink outside link; files := link :: !files;
    let folder = Filename.temp_file "pave-outside-dir-" "" in
    Sys.remove folder; Unix.mkdir folder 0o700;
    let external_file = Filename.concat folder "hidden.swift" in
    let oc = open_out external_file in output_string oc "outside secret\n"; close_out oc;
    Fun.protect ~finally:(fun () -> Sys.remove external_file; Unix.rmdir folder) (fun () ->
      let link = Filename.concat root "external" in
      Unix.symlink folder link; files := link :: !files;
      assert (rejected (fun () -> tool root "read_file" ["path", "../escape.swift"]));
      assert (rejected (fun () -> tool root "read_file" ["path", "escape.swift"]));
      assert (rejected (fun () -> tool root "read_file" ["path", "external/hidden.swift"]));
      assert (rejected (fun () -> tool root "glob" ["pattern", "*.swift"; "path", "external"]));
      assert (rejected (fun () -> tool root "grep" ["pattern", "outside"; "path", "external"]));
      assert (rejected (fun () -> tool root "glob" ["pattern", "../*.swift"]));
      assert (rejected (fun () -> tool root "write_file"
        ["path", "escape.swift"; "content", "bad"]));
      assert (rejected (fun () -> tool root "write_file"
        ["path", "../outside.swift"; "content", "bad"])));
    directory "src"; directory "ignored"; directory "build";
    create ".gitignore" "ignored/\n*.tmp\n!keep.tmp\n";
    create "src/.gitignore" "*.log\n!keep.log\n";
    create "src/Match.swift" "needle-123\nquiet\nneedle-456\n";
    create "src/hidden.log" "needle-789\n";
    create "src/keep.log" "needle-555\n";
    create "ignored/Omit.swift" "needle-999\n";
    create "build/Omit.swift" "needle-888\n";
    create "hidden.tmp" "needle-777\n";
    create "keep.tmp" "needle-444\n";
    let glob = tool root "glob" ["pattern", "**/*.swift"] in
    assert (contains glob "App.swift" && contains glob "src/Match.swift");
    assert (not (contains glob "ignored/Omit.swift"));
    assert (not (contains glob "build/Omit.swift"));
    assert (not (contains glob "escape.swift"));
    assert (contains (tool root "glob" ["pattern", "*.tmp"]) "keep.tmp");
    assert (not (contains (tool root "glob" ["pattern", "*.tmp"]) "hidden.tmp"));
    let matches = tool root "grep" ["pattern", "needle-[0-9][0-9][0-9]"] in
    assert (contains matches "src/Match.swift:1:needle-123");
    assert (contains matches "src/keep.log:1:needle-555");
    assert (not (contains matches "src/hidden.log"));
    assert (not (contains matches "ignored/Omit.swift"));
    assert (not (contains matches "build/Omit.swift"));
    assert (not (contains matches "outside secret"));
    assert (contains (tool root "search" ["pattern", "needle-123"]) "src/Match.swift");
    assert (not (contains (tool root "search" ["pattern", "needle-[0-9]"]) "src/Match.swift"));
    assert (contains (tool_json root "grep"
      ["pattern", `String "needle"; "limit", `Int 1]) "[truncated;");
    assert (contains (tool_json root "glob"
      ["pattern", `String "*.swift"; "limit", `Int 1]) "[truncated;");
    assert (rejected (fun () -> tool root "glob" ["pattern", "*.swift"; "path", "ignored"]));
    assert (List.exists (function
      | `Assoc fields -> (match List.assoc_opt "function" fields with
          | Some (`Assoc desc) -> List.assoc_opt "name" desc = Some (`String "glob")
          | _ -> false)
      | _ -> false) Pave.Tools.definitions);
    create "pages.txt" "first\nsecond\nthird\n";
    let first = tool_json root "read_file"
      ["path", `String "pages.txt"; "max_lines", `Int 1] in
    assert (contains first "first\n");
    assert (not (contains first "second\n"));
    assert (contains first "bytes: 6; lines: 1-1; next offset: 6; next line: 2; file size: 19; truncated");
    let second = tool_json root "read_file"
      ["path", `String "pages.txt"; "line", `Int 2; "max_bytes", `Int 3] in
    assert (contains second "sec");
    assert (contains second "offset: 6; bytes: 3; lines: 2-2; next offset: 9; next line: 2");
    assert (contains (tool_json root "read_file"
      ["path", `String "pages.txt"; "offset", `Int 9; "max_lines", `Int 1])
      "ond\n");
    assert (rejected (fun () -> tool_json root "read_file"
      ["path", `String "pages.txt"; "offset", `Int 3; "line", `Int 2]));
    assert (rejected (fun () -> tool_json root "read_file"
      ["path", `String "pages.txt"; "line", `Int 9]));
    create "large.txt" (String.make 70_000 'x' ^ "\nTARGET-END\n");
    let first_page = tool root "read_file" ["path", "large.txt"] in
    assert (contains first_page "next offset: 16384; next line: 1; file size: 70012; truncated");
    assert (String.length first_page < 65_536);
    let page = tool_json root "read_file"
      ["path", `String "large.txt"; "offset", `Int 69995; "max_bytes", `Int 32] in
    assert (contains page "TARGET-END");
    assert (contains page "offset: 69995; bytes: 17; lines: 1-2; next offset: 70012");
    assert (contains (tool_json root "read_file"
      ["path", `String "large.txt"; "line", `Int 2]) "TARGET-END");
    assert (rejected (fun () -> tool_json root "read_file"
      ["path", `String "large.txt"; "offset", `Int 70013]));
    create "settings.gradle.kts" "";
    assert (contains (String.lowercase_ascii (tool root "mobile_project" [])) "android");
    let command = tool root "run_command" ["command", "printf 'command-ok\\n'"] in
    assert (contains command "Status: exit 0");
    assert (contains command "command-ok");
    let timeout = Pave.Tools.execute ~root ~name:"run_command"
      ~args:(`Assoc ["command", `String "sleep 3"; "timeout_seconds", `Int 1]) () in
    assert (contains timeout "Status: timed out");
    let started = Unix.gettimeofday () in
    let cancelled = try
      ignore (Pave.Tools.execute
        ~cancel:(fun () -> Unix.gettimeofday () -. started > 0.1)
        ~root ~name:"run_command"
        ~args:(`Assoc ["command", `String "sleep 5";
          "timeout_seconds", `Int 20]) ());
      false
    with Pave.Tools.Cancelled -> true in
    assert (cancelled && Unix.gettimeofday () -. started < 2.);
    let ic = open_in outside in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
      assert (input_line ic = "outside secret")));
  print_endline "workspace tools: ok"
