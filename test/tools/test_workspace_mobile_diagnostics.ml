module Diagnostics = Pave.Workspace_mobile_diagnostics
module Run = Pave.Workspace_mobile_run

let expect label condition = if not condition then failwith label
let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec find i = i + m <= n &&
    (String.sub text i m = fragment || find (i + 1)) in
  find 0

let session ?(platform = Run.Android) ?(variant = Some "debug") () = {
  Run.id = "mobile-1"; root = "/tmp"; subroot = "app"; platform;
  device = (if platform = Run.Android then "emulator-5554" else "simulator-uuid");
  app_id = "com.example.fixture"; app_path = "app/build/outputs/apk/debug/app.apk";
  scheme = (if platform = Run.Ios then Some "Fixture" else None); variant;
  activity = None; ios_device_binding = None; screen_size = None; state = Run.Running;
}

let () =
  let android = session () in
  let logs = Diagnostics.command "logs" android in
  expect "Android app-scoped log command" (contains logs "pidof -s" &&
    contains logs "com.example.fixture");
  let crashes = Diagnostics.command "crashes" android in
  expect "Android app-specific process-exit diagnostics"
    (contains crashes "dumpsys activity exit-info" &&
     contains crashes "com.example.fixture");
  let anr = Diagnostics.command "anr" android in
  expect "Android last ANR command" (contains anr "dumpsys activity lastanr");
  let crash_text = String.concat "\n" [
    "E AndroidRuntime: Process: com.other.app, PID: 1"; "foreign crash frame";
    "E AndroidRuntime: FATAL EXCEPTION: main";
    "E AndroidRuntime: Process: com.example.fixture, PID: 2";
    "E AndroidRuntime: at com.example.fixture.MainActivity.onCreate(MainActivity.java:42)";
    "E AndroidRuntime: Process: com.example.fixture.evil, PID: 3"; "must not leak";
    "Process: com.other.app, PID: 4"; "another foreign frame"] in
  let crash = Diagnostics.result ~action:"crashes" android ~output:crash_text ~truncated:false
    |> Yojson.Basic.from_string in
  let member key = match crash with
    | `Assoc fields -> List.assoc key fields
    | _ -> failwith "diagnostic result is not an object" in
  let evidence = match member "evidence" with `String value -> value | _ -> "" in
  expect "matching crash found" (contains evidence "FATAL EXCEPTION");
  expect "verified Android source frame retained" (contains evidence "MainActivity.java:42");
  expect "foreign package crash frame omitted" (not (contains evidence "foreign crash frame"));
  expect "package-prefix collision omitted" (not (contains evidence "must not leak"));
  expect "no automatic Android symbolication" (contains (Yojson.Basic.to_string crash)
    "No automatic symbolication");
  let exit_record = Diagnostics.result ~action:"crashes" android
    ~output:(String.concat "\n" [
      "Historical Process Exit for package com.example.fixture:";
      "        ApplicationExitInfo #0:";
      "          process=com.example.fixture reason=4 (CRASH)";
      "        ApplicationExitInfo #1:";
      "          process=com.example.fixture reason=10 (USER REQUESTED)"])
    ~truncated:false |> Yojson.Basic.from_string in
  (match exit_record with
   | `Assoc fields ->
       expect "package-specific crash classification"
         (List.assoc "status" fields = `String "evidence_available");
       let evidence = match List.assoc "evidence" fields with `String s -> s | _ -> "" in
       expect "actual crash reason retained" (contains evidence "reason=4 (CRASH)");
       expect "non-crash process exit omitted" (not (contains evidence "reason=10"))
   | _ -> failwith "process-exit result is not an object");
  let anr_text = String.concat "\n" [
    "ANR in com.other.app (pid 1)"; "foreign ANR";
    "ANR in com.example.fixture (pid 2)"; "Input dispatching timed out";
    "at com.example.fixture.MainActivity.onResume(MainActivity.java:55)";
    "ANR in com.other.app (pid 3)"; "next record"] in
  let anr_report = Diagnostics.result ~action:"anr" android ~output:anr_text ~truncated:false
    |> Yojson.Basic.from_string in
  let anr_evidence = match anr_report with
    | `Assoc fields -> (match List.assoc "evidence" fields with `String s -> s | _ -> "")
    | _ -> "" in
  expect "selected ANR retained" (contains anr_evidence "Input dispatching timed out");
  expect "ANR source frame retained" (contains anr_evidence "MainActivity.java:55");
  expect "foreign ANR record omitted" (not (contains anr_evidence "next record"));
  let truncated = Diagnostics.result ~action:"crashes" android
    ~output:crash_text ~truncated:true |> Yojson.Basic.from_string in
  (match truncated with
   | `Assoc fields ->
       expect "truncated evidence status" (List.assoc "status" fields = `String "incomplete");
       expect "truncated output is not treated as evidence" (List.assoc "evidence" fields = `String "")
   | _ -> failwith "truncated result is not an object");
  let ios = session ~platform:Run.Ios ~variant:None () in
  expect "iOS log command scopes the simulator process"
    (contains (Diagnostics.command "logs" ios) "process == \"com.example.fixture\"");
  expect "iOS crash command scopes report evidence"
    (contains (Diagnostics.command "crashes" ios) "process == \"ReportCrash\"");
  (try ignore (Diagnostics.command "anr" ios); failwith "iOS ANR unexpectedly accepted"
   with Diagnostics.Error _ -> ());
  let root = Filename.temp_file "pave-mobile-symbols-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  let root = Unix.realpath root in
  let remove_tree path =
    let rec remove path =
      try match (Unix.lstat path).Unix.st_kind with
        | Unix.S_DIR ->
            Sys.readdir path |> Array.iter (fun name ->
              remove (Filename.concat path name));
            Unix.rmdir path
        | _ -> Unix.unlink path
      with Unix.Unix_error (Unix.ENOENT, _, _) -> () in
    remove path in
  let write_relative path =
    let absolute = Filename.concat root path in
    let rec make_dirs path =
      if path <> root && not (Sys.file_exists path) then (
        make_dirs (Filename.dirname path);
        Unix.mkdir path 0o700) in
    make_dirs (Filename.dirname absolute);
    let channel = open_out_bin absolute in
    output_string channel "symbol fixture";
    close_out channel in
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    write_relative "app/build/outputs/mapping/debug/mapping.txt";
    let android = { android with Run.root = root;
      app_path = "app/build/outputs/apk/debug/app-debug.apk" } in
    let android_symbols = Diagnostics.result ~action:"crashes" android
      ~output:"" ~truncated:false |> Yojson.Basic.from_string in
    let symbol_status result =
      Yojson.Basic.Util.(result |> member "symbol_artifact" |> member "status" |> to_string) in
    expect ("Android mapping file reported when present: " ^
      Yojson.Basic.to_string android_symbols)
      (symbol_status android_symbols = "available");
    write_relative "Fixture.app.dSYM/Contents/Resources/DWARF/Fixture";
    let ios = { ios with Run.root = root; app_path = "Fixture.app" } in
    let ios_symbols = Diagnostics.result ~action:"crashes" ios
      ~output:"" ~truncated:false |> Yojson.Basic.from_string in
    expect "iOS dSYM reported when its DWARF file is present"
      (symbol_status ios_symbols = "available"));
  print_endline "workspace mobile runtime diagnostics: ok"
