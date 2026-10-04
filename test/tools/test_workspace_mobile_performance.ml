module Performance = Pave.Workspace_mobile_performance
module Run = Pave.Workspace_mobile_run

let expect label condition = if not condition then failwith label
let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec find i = i + m <= n &&
    (String.sub text i m = fragment || find (i + 1)) in
  find 0
let session ?(platform = Run.Android) ?(state = Run.Running) ?activity () = {
  Run.id = "mobile-perf-1"; root = "/tmp"; subroot = "app"; platform;
  device = (if platform = Run.Android then "emulator-5554" else "simulator-uuid");
  app_id = "com.example.fixture"; app_path = "app/build/outputs/apk/debug/app.apk";
  scheme = (if platform = Run.Ios then Some "Fixture" else None);
  variant = Some "debug"; activity; ios_device_binding = None;
  screen_size = None; state;
}
let metric name json =
  let fields = match Yojson.Basic.from_string json with
    | `Assoc fields -> fields | _ -> failwith "expected report object" in
  let metrics = List.assoc "metrics" fields in
  match metrics with
  | `List values -> List.find (fun value -> Yojson.Basic.Util.(member "name" value |> to_string) = name) values
  | _ -> failwith "expected metrics list"
let fails f = try f (); false with Performance.Error _ -> true

let () =
  let android = session ~activity:".MainActivity" () in
  let launch = Performance.android_command ~action:"launch" android ~condition:"cold" () in
  List.iter (fun fragment ->
    expect ("cold launch command includes " ^ fragment) (contains launch fragment))
    ["am force-stop"; "com.example.fixture"; "am start -W";
     "PAVE_PRECONDITION_PID=stopped"; "PAVE_SELECTED_PID"];
  let warm_launch = Performance.android_command ~action:"launch" android ~condition:"warm" () in
  expect "warm launch requires and preserves an existing selected-app PID"
    (contains warm_launch "selected-app-not-warm" &&
     contains warm_launch "PAVE_PRECONDITION_PID=%s");
  let launch_report = Performance.parse_android_launch android ~condition:"cold"
    ~output:"PAVE_PRECONDITION_PID=stopped\nStatus: ok\nThisTime: 119\nTotalTime: 123\nWaitTime: 130\nPAVE_SELECTED_PID=42\n"
    ~truncated:false ~exit_code:0 |> Performance.report_json in
  expect "launch milliseconds retained" (Yojson.Basic.Util.(metric "TotalTime" launch_report |> member "unit" |> to_string) = "ms");
  expect "launch pid retained" (contains launch_report "\"pid\":42");
  expect "launch provenance includes cold condition and complete one-sample window"
    (contains launch_report "\"condition\":\"cold\"" &&
     contains launch_report "\"sample_count\":1" &&
     contains launch_report "\"complete\":true" &&
     contains launch_report "force-stop then one am start -W launch invocation");
  let revalidate = Performance.android_revalidate_pid_command android ~pid:42 in
  let expected_remote = Printf.sprintf
    "pid=$(pidof -s %s); test \"$pid\" = 42 || { echo selected-app-pid-changed >&2; exit 4; }; printf 'PAVE_SELECTED_PID=%%s\\n' \"$pid\""
    (Filename.quote android.app_id) in
  let expected_command = "adb -s " ^ Filename.quote android.device ^
    " shell " ^ Filename.quote expected_remote in
  expect "fresh PID revalidation helper binds device, package and exact PID"
    (revalidate = expected_command);
  expect "malformed launch condition rejected"
    (fails (fun () -> ignore (Performance.parse_android_launch android ~condition:"hot"
      ~output:"Status: ok\nTotalTime: 1\nPAVE_SELECTED_PID=42\n" ~truncated:false ~exit_code:0)));
  expect "warm launch must retain the pre-existing PID"
    (fails (fun () -> ignore (Performance.parse_android_launch android ~condition:"warm"
      ~output:"PAVE_PRECONDITION_PID=41\nStatus: ok\nTotalTime: 1\nPAVE_SELECTED_PID=42\n"
      ~truncated:false ~exit_code:0)));
  let memory_output = "PAVE_SELECTED_PID=42\n** MEMINFO in pid 42 [com.example.fixture] **\nApplications Memory Usage (in Kilobytes):\nTOTAL PSS: 321\n" in
  let memory_command = Performance.android_command ~action:"memory" android ~pid:42 ~condition:"warm" () in
  expect "memory command binds exact running package pid" (contains memory_command "pidof -s" && contains memory_command "dumpsys meminfo 42");
  let memory = Performance.parse_android ~action:"memory" android ~pid:42 ~condition:"warm"
    ~output:memory_output ~truncated:false ~exit_code:0 |> Performance.report_json in
  expect "PSS has known kB unit" (Yojson.Basic.Util.(metric "total_pss" memory |> member "unit" |> to_string) = "kB");
  expect "PSS value parsed" (Yojson.Basic.Util.(metric "total_pss" memory |> member "value" |> to_float) = 321.);
  expect "memory provenance records warm condition and one complete snapshot"
    (contains memory "\"condition\":\"warm\"" && contains memory "\"sample_count\":1" &&
     contains memory "\"complete\":true" && contains memory "single dumpsys meminfo snapshot");
  let frames_command = Performance.android_command ~action:"frames" android ~pid:42 ~condition:"warm" () in
  expect "frame command uses gfxinfo framestats" (contains frames_command "dumpsys gfxinfo" && contains frames_command "framestats");
  let frames_output = String.concat "\n" [
    "PAVE_SELECTED_PID=42";
    "** Graphics info for pid 42 [com.example.fixture] **";
    "Flags,IntendedVsync,Vsync,FrameCompleted";
    "0,1000000,1000000,18000000";
    "0,20000000,20000000,36000000";
  ] in
  let frames = Performance.parse_android ~action:"frames" android ~pid:42 ~condition:"warm"
    ~output:frames_output ~truncated:false ~exit_code:0 |> Performance.report_json in
  expect "frame durations converted nanoseconds to milliseconds" (Yojson.Basic.Util.(metric "frame_duration_p50" frames |> member "value" |> to_float) = 16.);
  expect "frame metric unit is ms" (Yojson.Basic.Util.(metric "frame_duration_p90" frames |> member "unit" |> to_string) = "ms");
  expect "frame provenance includes sample window and observed sample count"
    (contains frames "\"condition\":\"warm\"" && contains frames "\"sample_count\":2" &&
     contains frames "\"complete\":true" && contains frames "2 frame samples");
  expect "foreign selected pid rejected" (fails (fun () -> ignore (Performance.parse_android ~action:"memory" android ~pid:42 ~condition:"warm" ~output:(String.concat "" ["PAVE_SELECTED_PID=43\n"; "** MEMINFO in pid 43 [com.example.fixture] **\nTOTAL PSS: 1 kB\n"]) ~truncated:false ~exit_code:0)));
  expect "truncated data is incomplete with no metrics and zero samples"
    (let report = Performance.parse_android ~action:"memory" android ~pid:42 ~condition:"warm" ~output:memory_output ~truncated:true ~exit_code:0 |> Performance.report_json in
     contains report "\"status\":\"incomplete\"" && contains report "\"metrics\":[]" &&
     contains report "\"sample_count\":0" && contains report "\"complete\":false");
  expect "malformed PSS sample rejected" (fails (fun () -> ignore (Performance.parse_android ~action:"memory" android ~pid:42 ~condition:"warm" ~output:"PAVE_SELECTED_PID=42\n** MEMINFO in pid 42 [com.example.fixture] **\nApplications Memory Usage (in Kilobytes):\nTOTAL PSS: unknown\n" ~truncated:false ~exit_code:0)));
  expect "duplicate PSS sample window rejected" (fails (fun () -> ignore (Performance.parse_android ~action:"memory" android ~pid:42 ~condition:"warm" ~output:(memory_output ^ "TOTAL PSS: 9 kB\n") ~truncated:false ~exit_code:0)));
  expect "empty frame window rejected" (fails (fun () -> ignore (Performance.parse_android ~action:"frames" android ~pid:42 ~condition:"warm" ~output:"PAVE_SELECTED_PID=42\n** Graphics info for pid 42 [com.example.fixture] **\nFlags,IntendedVsync,Vsync,FrameCompleted\n" ~truncated:false ~exit_code:0)));
  expect "mixed frame PID window rejected" (fails (fun () -> ignore (Performance.parse_android ~action:"frames" android ~pid:42 ~condition:"warm" ~output:(frames_output ^ "\n** Graphics info for pid 43 [com.example.fixture] **\n") ~truncated:false ~exit_code:0)));
  expect "truncated frame sample rejected" (fails (fun () -> ignore (Performance.parse_android ~action:"frames" android ~pid:42 ~condition:"warm" ~output:(frames_output ^ "0,30000000,30000000\n") ~truncated:false ~exit_code:0)));
  let ios = session ~platform:Run.Ios () in
  expect "template list command uses xctrace" (Performance.ios_templates_command = "xcrun xctrace list templates");
  let templates = Performance.ios_templates "Available Instruments templates:\n    Time Profiler\n    Allocations\n" ~truncated:false in
  expect "installed templates retained" (templates = ["Time Profiler"; "Allocations"]);
  expect "truncated template list rejected" (Performance.ios_templates "Time Profiler" ~truncated:true = []);
  let command = Performance.ios_command ~template:"Time Profiler" ~installed_templates:templates
      ~pid:51 ~output_path:"/tmp/fixture.trace" ios in
  expect "xctrace binds simulator, PID, duration and trace output"
    (contains command ("--template " ^ Filename.quote "Time Profiler") &&
     contains command ("--device " ^ Filename.quote ios.device) &&
     contains command "--attach 51" && contains command "--time-limit 30s" &&
     contains command (Filename.quote "/tmp/fixture.trace"));
  expect "relative Instruments trace path rejected"
    (fails (fun () -> ignore (Performance.ios_command ~template:"Time Profiler"
      ~installed_templates:templates ~pid:51 ~output_path:"fixture.trace" ios)));
  expect "unsupported or unconfirmed template rejected"
    (fails (fun () -> ignore (Performance.ios_command ~template:"Energy Log" ~installed_templates:templates ~pid:51 ~output_path:"/tmp/fixture.trace" ios)));
  let capture = Performance.parse_ios_capture ios ~template:"Time Profiler" ~pid:51 ~condition:"warm"
      ~output_path:"/tmp/fixture.trace" ~output:"Recording completed"
      ~truncated:false ~exit_code:0 |> Performance.report_json in
  expect "simulator PID and raw capture output retained"
    (contains capture "\"pid\":51" && contains capture "Recording completed" &&
     contains capture "\"trace_path\":\"/tmp/fixture.trace\"" &&
     contains capture "\"condition\":\"warm\"" && contains capture "\"complete\":true");
  let incomplete = Performance.parse_ios_capture ios ~template:"Time Profiler" ~pid:51 ~condition:"cold"
      ~output_path:"/tmp/fixture.trace" ~output:"partial"
      ~truncated:true ~exit_code:0 |> Performance.report_json in
  expect "truncated Instruments capture is incomplete"
    (contains incomplete "\"status\":\"incomplete\"");
  print_endline "workspace mobile performance diagnostics: ok"
