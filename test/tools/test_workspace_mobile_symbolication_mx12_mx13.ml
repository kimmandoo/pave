module Diagnostics = Pave.Workspace_mobile_diagnostics
module Run = Pave.Workspace_mobile_run

let expect label condition = if not condition then failwith label
let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec find i = i + m <= n &&
    (String.sub text i m = fragment || find (i + 1)) in
  find 0
let session ?(platform = Run.Android) ?(variant = Some "debug") ?(root = "/tmp")
    ?(app_path = "app/build/outputs/apk/debug/app-debug.apk") () = {
  Run.id = "mx12-mx13-fixture"; root; subroot = "app"; platform;
  device = "simulator-uuid"; app_id = "com.example.fixture"; app_path;
  scheme = (if platform = Run.Ios then Some "Fixture" else None); variant;
  activity = None; screen_size = None; state = Run.Built;
}
let fails label fn =
  try ignore (fn ()); failwith (label ^ " unexpectedly succeeded")
  with Diagnostics.Error _ -> ()

let () =
  let original_path = Sys.getenv_opt "PATH" and original_home = Sys.getenv_opt "HOME" in
  let restore name = function
    | Some value -> Unix.putenv name value
    | None -> Unix.putenv name "" in
  let temp = Filename.temp_file "pave-mx12-mx13-" "" in
  Unix.unlink temp;
  Unix.mkdir temp 0o700;
  let temp = Unix.realpath temp in
  let rec remove_tree path =
    try match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path |> Array.iter (fun name -> remove_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Unix.unlink path
    with Unix.Unix_error (Unix.ENOENT, _, _) -> () in
  let rec mkdirs path =
    if not (Sys.file_exists path) then (mkdirs (Filename.dirname path); Unix.mkdir path 0o700) in
  Fun.protect ~finally:(fun () ->
    restore "PATH" original_path;
    restore "HOME" original_home;
    remove_tree temp) (fun () ->
      mkdirs (Filename.concat temp "app/build/outputs/mapping/debug");
      let mapping = open_out_bin (Filename.concat temp
        "app/build/outputs/mapping/debug/mapping.txt") in
      output_string mapping "# mapping fixture\n"; close_out mapping;
      Unix.putenv "PATH" temp;
      let android = session ~root:temp () in
      fails "Android retrace approval" (fun () ->
        ignore (Diagnostics.android_deobfuscate ~approved:false
          ~run:(fun ~program:_ ~arguments:_ ~stdin:_ -> "") android ~crash:"at a.b.c(Unknown Source)"));
      fails "missing installed retrace" (fun () ->
        Diagnostics.android_deobfuscate ~approved:true
          ~run:(fun ~program:_ ~arguments:_ ~stdin:_ -> "") android ~crash:"at a.b.c(Unknown Source)");
      let retrace = open_out_bin (Filename.concat temp "retrace") in
      output_string retrace "#!/bin/sh\nexit 0\n"; close_out retrace;
      Unix.chmod (Filename.concat temp "retrace") 0o700;
      let raw = "at a.b.c(Unknown Source)" in
      let resolved = Diagnostics.android_deobfuscate ~approved:true
        ~run:(fun ~program ~arguments ~stdin ->
          expect "installed allowlisted retrace path" (Filename.basename program = "retrace");
          expect "mapping argument is build-variant-bound"
            (List.exists (fun value -> contains value "outputs/mapping/debug/mapping.txt") arguments);
          expect "raw obfuscated stack passed unchanged" (stdin = raw);
          "at com.example.Main.run(Main.java:7)") android ~crash:raw in
      (match resolved with
       | `Assoc fields ->
           expect "transformed frame retained"
             (List.assoc "transformed_evidence" fields = `String "at com.example.Main.run(Main.java:7)");
           expect "original frame provenance retained"
             (List.assoc "raw_evidence" fields = `String raw)
       | _ -> failwith "Android symbolication result is not an object");
      let ios = session ~platform:Run.Ios ~variant:None ~root:temp
        ~app_path:"Fixture.app" () in
      fails "iOS retrieval approval" (fun () ->
        ignore (Diagnostics.ios_retrieve_crashes ~approved:false ios));
      let run_called = ref false in
      let run ~program:_ ~arguments:_ ~stdin:_ = run_called := true; "symbolicated" in
      fails "wrong iOS UUID" (fun () -> ignore (Diagnostics.ios_symbolicate ~approved:true ~run ios
        ~report:"{\"Identifier\":\"com.example.fixture\", \"uuid\":\"1111\", \"arm64\"}"
        ~binary_uuid:"1111" ~dsym_uuid:"2222" ~binary_architecture:"arm64" ~dsym_architecture:"arm64"));
      expect "UUID mismatch does not invoke symbolicator" (not !run_called);
      fails "wrong iOS architecture" (fun () -> ignore (Diagnostics.ios_symbolicate ~approved:true ~run ios
        ~report:"{\"Identifier\":\"com.example.fixture\", \"uuid\":\"1111\", \"arm64\"}"
        ~binary_uuid:"1111" ~dsym_uuid:"1111" ~binary_architecture:"arm64" ~dsym_architecture:"x86_64"));
      expect "architecture mismatch does not invoke symbolicator" (not !run_called);
      fails "foreign iOS report" (fun () -> ignore (Diagnostics.ios_symbolicate ~approved:true ~run ios
        ~report:"{\"Identifier\":\"com.other.app\", \"uuid\":\"1111\", \"arm64\"}"
        ~binary_uuid:"1111" ~dsym_uuid:"1111" ~binary_architecture:"arm64" ~dsym_architecture:"arm64"));
      expect "foreign report does not invoke symbolicator" (not !run_called);
      Unix.putenv "HOME" temp;
      let reports_dir = Filename.concat temp
        "Library/Developer/CoreSimulator/Devices/simulator-uuid/data/Library/Logs/CrashReporter" in
      mkdirs reports_dir;
      let report_file = open_out_bin (Filename.concat reports_dir "Fixture.ips") in
      output_string report_file "{\"Identifier\":\"com.example.fixture\",\"uuid\":\"1111\"}";
      close_out report_file;
      let foreign_file = open_out_bin (Filename.concat reports_dir "Foreign.ips") in
      output_string foreign_file "{\"Identifier\":\"com.other.app\",\"uuid\":\"2222\"}";
      close_out foreign_file;
      (match Diagnostics.ios_retrieve_crashes ~approved:true ios with
       | `List reports ->
           expect "selected simulator report retrieved"
             (List.exists (function
               | `Assoc fields -> List.assoc_opt "status" fields = Some (`String "available")
               | _ -> false) reports);
           expect "foreign report is not attributed"
             (List.exists (function
               | `Assoc fields -> List.assoc_opt "status" fields = Some (`String "foreign_report")
               | _ -> false) reports)
       | _ -> failwith "simulator crash retrieval result is not a list");
      mkdirs (Filename.concat temp "Fixture.app");
      mkdirs (Filename.concat temp "Fixture.app.dSYM/Contents/Resources/DWARF");
      let dwarf = open_out_bin (Filename.concat temp
        "Fixture.app.dSYM/Contents/Resources/DWARF/Fixture") in
      output_string dwarf "DWARF fixture"; close_out dwarf;
      let symbolicatecrash = open_out_bin (Filename.concat temp "symbolicatecrash") in
      output_string symbolicatecrash "#!/bin/sh\nexit 0\n"; close_out symbolicatecrash;
      Unix.chmod (Filename.concat temp "symbolicatecrash") 0o700;
      let ios_report = "{\"Identifier\":\"com.example.fixture\", \"uuid\":\"1111\", \"arm64\"}" in
      let ios_resolved = Diagnostics.ios_symbolicate ~approved:true
        ~run:(fun ~program ~arguments ~stdin ->
          expect "installed symbolicator selected" (Filename.basename program = "symbolicatecrash");
          expect "symbolicator does not receive fabricated stdin" (stdin = "");
          let channel = open_in_bin (List.hd arguments) in
          let supplied = Fun.protect ~finally:(fun () -> close_in_noerr channel)
            (fun () -> really_input_string channel (in_channel_length channel)) in
          expect "raw iOS report passed unchanged" (supplied = ios_report);
          expect "selected dSYM passed to symbolicator"
            (List.exists (fun value -> contains value "Fixture.app.dSYM") arguments);
          "FixtureController.viewDidLoad") ios ~report:ios_report ~binary_uuid:"1111"
        ~dsym_uuid:"1111" ~binary_architecture:"arm64" ~dsym_architecture:"arm64" in
      (match ios_resolved with
       | `Assoc fields ->
           expect "resolved iOS frame retained"
             (List.assoc "transformed_evidence" fields = `String "FixtureController.viewDidLoad");
           expect "raw iOS report provenance retained"
             (List.assoc "raw_evidence" fields = `String ios_report)
       | _ -> failwith "iOS symbolication result is not an object");
      let output = Diagnostics.android_mapping_path android in
      expect "variant-bound map path" (contains output "outputs/mapping/debug/mapping.txt");
      print_endline "mobile symbolication safety regressions: ok")
