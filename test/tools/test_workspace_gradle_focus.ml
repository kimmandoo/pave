module Gradle = Pave.Workspace_gradle_focus

let expect label value = if not value then failwith ("workspace gradle: " ^ label)
let rejects label operation =
  try ignore (operation ()); failwith ("workspace gradle accepted " ^ label)
  with Gradle.Error _ -> ()

let write path text =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel text)

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.iter (fun name ->
        remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let sample =
  "> Task :tasks\n\n" ^
  "------------------------------------------------------------\n" ^
  "All tasks runnable from root project 'mobile'\n" ^
  "------------------------------------------------------------\n\n" ^
  "Build tasks\n-----------\n" ^
  "assembleDebug - Assembles a debug build.\n" ^
  ":app:assembleDebug - Assembles an app debug build.\n" ^
  ":lib:check - Checks the library.\n\n" ^
  "Help tasks\n----------\n" ^
  "tasks - Displays the tasks runnable from root project.\n" ^
  "wrapper\n\n" ^
  "Rules\n-----\nPattern: clean<TaskName>: Cleans a task.\n\n" ^
  "BUILD SUCCESSFUL in 1s\n1 actionable task: 1 executed\n"

let () =
  let found = Gradle.tasks sample in
  expect "root and qualified tasks, not headings or Gradle rule patterns"
    (found = [":app:assembleDebug"; ":assembleDebug"; ":lib:check";
              ":tasks"; ":wrapper"]);
  let prefix = "All tasks runnable from root project 'mobile'\n" ^
    "------------------------------------------------------------\n" ^
    "Build tasks\n-----------\n" in
  rejects "task-like stack traces" (fun () ->
    Gradle.tasks (prefix ^ "assembleDebug - real\n" ^
      "FAILURE: Build failed with an exception.\nBUILD SUCCESSFUL in 1s\n"));
  rejects "unterminated output" (fun () ->
    Gradle.tasks (String.sub sample 0 (String.length sample - 1)));
  rejects "no build success" (fun () -> Gradle.tasks prefix);
  rejects "missing listing" (fun () ->
    Gradle.tasks "Build tasks\n-----------\nassembleDebug - fake\nBUILD SUCCESSFUL in 1s\n");
  rejects "malformed quoted name" (fun () ->
    Gradle.tasks (prefix ^ ":app:assembleDebug;touch - unsafe\nBUILD SUCCESSFUL in 1s\n"));
  rejects "malformed task row" (fun () ->
    Gradle.tasks (prefix ^ "assembleDebug bogus description\nBUILD SUCCESSFUL in 1s\n"));
  rejects "stack trace camouflaged as task" (fun () ->
    Gradle.tasks (prefix ^ "at :app:assembleDebug - frame\nBUILD SUCCESSFUL in 1s\n"));
  rejects "oversized output" (fun () ->
    Gradle.tasks (sample ^ String.make 65_536 'x' ^ "\n"));
  let root = Filename.temp_file "pave-gradle-focus-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    let android = Filename.concat root "android" in
    Unix.mkdir android 0o700;
    write (Filename.concat android "settings.gradle.kts")
      "rootProject.name = \"mobile\"\ninclude(\":app\", \":lib\")\n";
    write (Filename.concat android "gradlew") "#!/bin/sh\nexit 7\n";
    let tasks, cwd = Gradle.command ~root ~subroot:"android"
      ~action:"tasks" ~task:"" in
    expect "system Gradle offline listing; never wrapper" 
      (tasks = "gradle --offline tasks --all" && cwd = android);
    let run, cwd = Gradle.command ~root ~subroot:"android"
      ~action:"run" ~task:":app:assembleDebug" in
    expect "exact quoted qualified task and selected settings root"
      (run = "gradle --offline ':app:assembleDebug'" && cwd = android);
    expect "root task allowed" (fst (Gradle.command ~root ~subroot:"android"
      ~action:"run" ~task:":check") = "gradle --offline ':check'");
    rejects "absent statically declared module" (fun () ->
      Gradle.command ~root ~subroot:"android" ~action:"run"
        ~task:":missing:assembleDebug");
    rejects "ambiguous unqualified task" (fun () ->
      Gradle.command ~root ~subroot:"android" ~action:"run"
        ~task:"assembleDebug");
    rejects "Gradle abbreviation" (fun () ->
      Gradle.command ~root ~subroot:"android" ~action:"run"
        ~task:":app:build --offline");
    rejects "task discovery accepts no target" (fun () ->
      Gradle.command ~root ~subroot:"android" ~action:"tasks" ~task:":check");
    rejects "parent traversal" (fun () ->
      Gradle.command ~root ~subroot:"android/../android" ~action:"tasks" ~task:"");
    Unix.symlink android (Filename.concat root "linked");
    rejects "symlinked selected directory" (fun () ->
      Gradle.command ~root ~subroot:"linked" ~action:"tasks" ~task:"");
    let original = Filename.concat android "settings.gradle.kts" in
    Unix.unlink original;
    Unix.symlink (Filename.concat root "outside-settings") original;
    rejects "symlinked settings" (fun () ->
      Gradle.command ~root ~subroot:"android" ~action:"tasks" ~task:"");
    Unix.unlink original;
    write original (String.make (Pave.Workspace_path.max_write_bytes + 1) 'x');
    rejects "oversized manifest" (fun () ->
      Gradle.command ~root ~subroot:"android" ~action:"tasks" ~task:"");
    write original "include(projectsFromPlugin)\n";
    expect "dynamic include cannot establish static absence"
      (fst (Gradle.command ~root ~subroot:"android" ~action:"run"
        ~task:":external:assembleDebug") =
         "gradle --offline ':external:assembleDebug'");
    print_endline "workspace Gradle focused discovery: ok")
