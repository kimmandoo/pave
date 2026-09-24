let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec loop i = i + m <= n &&
    (String.sub text i m = fragment || loop (i + 1)) in
  loop 0

let args fields = `Assoc (List.map (fun (k, v) -> k, `String v) fields)
let tool root name fields = Pave.Tools.execute ~root ~name ~args:(args fields)
let rejected f =
  try contains (String.lowercase_ascii (f ())) "error"
  with _ -> true

let () =
  let root = Filename.temp_file "pave-tools-" "" in
  Sys.remove root; Unix.mkdir root 0o700;
  let outside = Filename.temp_file "pave-outside-" ".swift" in
  let oc = open_out outside in output_string oc "outside"; close_out oc;
  let link = Filename.concat root "escape.swift" in
  Unix.symlink outside link;
  Fun.protect ~finally:(fun () ->
    Sys.remove link; Sys.remove outside;
    (try Sys.remove (Filename.concat root "App.swift") with Sys_error _ -> ());
    (try Sys.remove (Filename.concat root "settings.gradle.kts") with Sys_error _ -> ());
    Unix.rmdir root) (fun () ->
    ignore (tool root "write_file" [ "path", "App.swift"; "content", "one\none\n" ]);
    assert (contains (tool root "read_file" [ "path", "App.swift" ]) "one");
    assert (rejected (fun () -> tool root "edit_file" [ "path", "App.swift";
      "old_string", "one"; "new_string", "two" ]));
    ignore (tool root "edit_file" [ "path", "App.swift";
      "old_string", "one\none\n"; "new_string", "two\n" ]);
    assert (contains (tool root "read_file" [ "path", "App.swift" ]) "two");
    assert (rejected (fun () -> tool root "read_file" [ "path", "../escape.swift" ]));
    assert (rejected (fun () -> tool root "read_file" [ "path", "escape.swift" ]));
    assert (rejected (fun () -> tool root "write_file" [ "path", "escape.swift"; "content", "bad" ]));
    assert (rejected (fun () -> tool root "write_file" [ "path", "../outside.swift"; "content", "bad" ]));
    assert (contains (tool root "search" [ "pattern", "two" ]) "App.swift");
    ignore (tool root "write_file" [ "path", "settings.gradle.kts"; "content", "" ]);
    assert (contains (String.lowercase_ascii (tool root "mobile_project" [])) "android");
    let command = tool root "run_command" [ "command", "printf 'command-ok\\n'" ] in
    assert (contains command "Status: exit 0");
    assert (contains command "command-ok");
    let timeout = Pave.Tools.execute ~root ~name:"run_command"
      ~args:(`Assoc [ "command", `String "sleep 3"; "timeout_seconds", `Int 1 ]) in
    assert (contains timeout "Status: timed out");
    let ic = open_in outside in
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
      assert (input_line ic = "outside")));
  print_endline "workspace tools: ok"
