exception Error of string
let fail message = raise (Error message)

let max_output_bytes = 1_048_576
let max_frames = 20_000
let max_capture_seconds = 30

type metric = { name : string; value : float; unit : string }
type report = {
  status : string;
  action : string;
  session_id : string;
  app_id : string;
  device : string;
  pid : int option;
  build : string;
  condition : string;
  sample_window : string;
  sample_count : int;
  complete : bool;
  metrics : metric list;
  raw_evidence : string;
  trace_path : string option;
}

let valid_pid pid = pid > 0 && pid <= 4_194_304
let pid_text pid =
  if not (valid_pid pid) then fail "selected process ID is invalid";
  string_of_int pid
let quote = Filename.quote
let build_label (session : Workspace_mobile_run.session) =
  session.app_path ^ " (" ^ Option.value ~default:"unspecified" session.variant ^ ")"
let validate_condition condition =
  if condition <> "cold" && condition <> "warm" then
    fail "measurement condition must be explicitly cold or warm";
  condition

(* The caller must execute this command immediately before capture and reject
   any nonzero result. It confirms that the selected package still resolves
   to the exact PID being measured on the selected device. *)
let android_revalidate_pid_command (session : Workspace_mobile_run.session) ~pid =
  if session.platform <> Workspace_mobile_run.Android then
    fail "Android PID revalidation requires an Android session";
  let pid = pid_text pid in
  let remote = Printf.sprintf
    "pid=$(pidof -s %s); test \"$pid\" = %s || { echo selected-app-pid-changed >&2; exit 4; }; printf 'PAVE_SELECTED_PID=%%s\\n' \"$pid\""
    (quote session.app_id) pid in
  "adb -s " ^ quote session.device ^ " shell " ^ quote remote

let android_command ~action (session : Workspace_mobile_run.session) ?pid ?condition () =
  if session.platform <> Workspace_mobile_run.Android then
    fail "Android measurements require an Android session";
  let device = quote session.device and package = quote session.app_id in
  match action, pid with
  | "launch", _ ->
      let condition = validate_condition (Option.value ~default:"" condition) in
      let activity = match session.activity with
        | Some activity when activity <> "" -> activity
        | _ -> fail "Android launch measurement requires the selected activity" in
      let component = if String.contains activity '/' then activity
        else session.app_id ^ "/" ^ activity in
      let precondition = match condition with
        | "cold" ->
            "am force-stop " ^ package ^
            "; status=$?; test $status -eq 0 || exit $status; " ^
            "pid=$(pidof -s " ^ package ^
            "); test -z \"$pid\" || { echo selected-app-did-not-stop >&2; exit 4; }; " ^
            "printf 'PAVE_PRECONDITION_PID=stopped\\n'; "
        | "warm" ->
            "pid=$(pidof -s " ^ package ^
            "); case \"$pid\" in ''|*[!0-9]*) echo selected-app-not-warm >&2; exit 4;; esac; " ^
            "printf 'PAVE_PRECONDITION_PID=%s\\n' \"$pid\"; "
        | _ -> fail "launch condition must be cold or warm" in
      let remote = precondition ^
        Printf.sprintf
          "am start -W -n %s; status=$?; test $status -eq 0 || exit $status; pid=$(pidof -s %s); case \"$pid\" in ''|*[!0-9]*) echo selected-app-not-running >&2; exit 4;; esac; printf 'PAVE_SELECTED_PID=%%s\\n' \"$pid\""
          (quote component) package in
      "adb -s " ^ device ^ " shell " ^ quote remote
  | ("frames" | "memory"), Some pid ->
      let condition = validate_condition (Option.value ~default:"" condition) in
      if condition <> "warm" then
        fail "frame and memory captures require an explicitly warm selected app";
      let pid = pid_text pid in
      if session.state <> Workspace_mobile_run.Running then
        fail "Android measurement requires a running selected-app session";
      let verify = Printf.sprintf
        "pid=$(pidof -s %s); test \"$pid\" = %s || { echo selected-app-pid-changed >&2; exit 4; }; printf 'PAVE_SELECTED_PID=%%s\\n' \"$pid\"; "
        package pid in
      let capture = if action = "frames" then
        "dumpsys gfxinfo " ^ package ^ " framestats"
        else "dumpsys meminfo " ^ pid in
      "adb -s " ^ device ^ " shell " ^ quote (verify ^ capture)
  | ("frames" | "memory"), None ->
      fail "Android measurement requires the exact selected process ID"
  | _ -> fail "Android measurement action must be launch, frames or memory"

let ios_templates_command = "xcrun xctrace list templates"

let ios_templates output ~truncated =
  if String.length output > max_output_bytes then fail "Instruments template output exceeds its byte limit";
  if truncated then [] else
  let lines = String.split_on_char '\n' output in
  let known = ["Time Profiler"; "Allocations"; "Leaks"; "Activity Monitor"] in
  List.filter (fun name -> List.exists (fun line -> String.trim line = name) lines) known

let ios_command ~template ~installed_templates ~pid ~output_path (session : Workspace_mobile_run.session) =
  if session.platform <> Workspace_mobile_run.Ios then fail "Instruments capture requires an iOS Simulator session";
  if session.state <> Workspace_mobile_run.Running then fail "Instruments capture requires a running simulator app session";
  if not (List.mem template installed_templates) then
    fail "Instruments template was not confirmed as installed";
  if not (List.mem template ["Time Profiler"; "Allocations"; "Leaks"; "Activity Monitor"]) then
    fail "Instruments template is not an allowlisted template";
  let pid = pid_text pid in
  if Filename.is_relative output_path ||
     not (Filename.check_suffix output_path ".trace") ||
     String.contains output_path '\000' then
    fail "Instruments output must be an absolute .trace path";
  Printf.sprintf
    "xcrun xctrace record --template %s --device %s --attach %s --time-limit %ds --output %s"
    (quote template) (quote session.device) pid max_capture_seconds (quote output_path)

let json_metric (metric : metric) = `Assoc [
  "name", `String metric.name; "value", `Float metric.value;
  "unit", `String metric.unit]

let report_json report = Yojson.Basic.to_string (`Assoc [
  "status", `String report.status; "action", `String report.action;
  "session_id", `String report.session_id; "app_id", `String report.app_id;
  "device", `String report.device;
  "pid", (match report.pid with None -> `Null | Some pid -> `Int pid);
  "build", `String report.build; "condition", `String report.condition;
  "sample_window", `String report.sample_window;
  "sample_count", `Int report.sample_count;
  "complete", `Bool report.complete;
  "metrics", `List (List.map json_metric report.metrics);
  "raw_evidence", `String report.raw_evidence;
  "trace_path", (match report.trace_path with None -> `Null | Some path -> `String path)])

let empty_report ~status ~action (session : Workspace_mobile_run.session) ?pid
    ?(condition = "") ?(sample_window = "") ?(sample_count = 0)
    ?(complete = false) ?(raw_evidence = "") ?trace_path metrics = {
  status; action; session_id = session.id; app_id = session.app_id;
  device = session.device; pid; build = build_label session; condition;
  sample_window; sample_count; complete; metrics; raw_evidence; trace_path }

let parse_pid_marker pid output =
  let expected = "PAVE_SELECTED_PID=" ^ string_of_int pid in
  let found = String.split_on_char '\n' output |> List.filter (fun line ->
    String.starts_with ~prefix:"PAVE_SELECTED_PID=" line) in
  match found with
  | [line] when line = expected -> ()
  | _ -> fail "measurement output does not identify the exact selected process ID"

let parse_float label value =
  match float_of_string_opt value with
  | Some value when Float.is_finite value && value >= 0. -> value
  | _ -> fail ("malformed " ^ label ^ " sample")

let after_prefix prefix line =
  if String.starts_with ~prefix line then
    Some (String.trim (String.sub line (String.length prefix) (String.length line - String.length prefix)))
  else None

let launch_metrics output =
  let names = ["ThisTime"; "TotalTime"; "WaitTime"] in
  let lines = String.split_on_char '\n' output in
  let metrics = List.filter_map (fun name ->
    match List.filter_map (after_prefix (name ^ ":")) lines with
    | [] -> None
    | [value] ->
        let length = String.length value in
        let amount = if length >= 2 && String.sub value (length - 2) 2 = "ms"
          then String.trim (String.sub value 0 (length - 2)) else value in
        Some { name; value = parse_float name amount; unit = "ms" }
    | _ -> fail ("launch output contains duplicate " ^ name ^ " samples")) names in
  if not (List.exists (fun metric -> metric.name = "TotalTime") metrics) then
    fail "launch output is missing TotalTime";
  metrics

let parse_android_launch (session : Workspace_mobile_run.session) ~condition ~output ~truncated ~exit_code =
  let condition = validate_condition condition in
  if session.platform <> Workspace_mobile_run.Android then fail "Android launch parser requires an Android session";
  if String.length output > max_output_bytes then fail "Android launch output exceeds its byte limit";
  if truncated || exit_code <> 0 then empty_report ~status:"incomplete" ~action:"launch" session ~condition ~raw_evidence:output []
  else
    let selected_pid = match String.split_on_char '\n' output |> List.find_map (after_prefix "PAVE_SELECTED_PID=") with
      | Some value -> (match int_of_string_opt value with Some pid when valid_pid pid -> pid | _ -> fail "malformed selected process ID")
      | None -> fail "launch output is missing the selected process ID" in
    parse_pid_marker selected_pid output;
    let preconditions = String.split_on_char '\n' output
      |> List.filter_map (after_prefix "PAVE_PRECONDITION_PID=") in
    (match condition, preconditions with
     | "cold", ["stopped"] -> ()
     | "warm", [pid] when int_of_string_opt pid = Some selected_pid -> ()
     | "warm", [_] -> fail "warm launch did not preserve the pre-existing selected-app process"
     | _ -> fail "launch output does not prove its requested cold/warm precondition");
    let metrics = launch_metrics output in
    if not (List.exists (fun line -> String.trim line = "Status: ok") (String.split_on_char '\n' output)) then
      fail "launch output does not report successful ActivityManager completion";
    empty_report ~status:"available" ~action:"launch" session ~pid:selected_pid ~condition
      ~sample_window:(if condition = "cold" then
        "force-stop then one am start -W launch invocation"
      else "pre-existing PID continuity through one am start -W launch invocation")
      ~sample_count:1 ~complete:true ~raw_evidence:output metrics

let csv_fields line = String.split_on_char ',' line |> List.map String.trim
let int64_field label text = match Int64.of_string_opt text with
  | Some value when value >= 0L -> value
  | _ -> fail ("malformed frame " ^ label)

let frame_metrics output =
  let lines = String.split_on_char '\n' output in
  let headers = List.filter (fun line -> String.starts_with ~prefix:"Flags,IntendedVsync," line) lines in
  let header = match headers with
    | [header] -> header
    | [] -> fail "gfxinfo output is missing the framestats header"
    | _ -> fail "gfxinfo output contains mixed frame sample windows" in
  let columns = csv_fields header in
  let index name =
    let rec find i = function
      | [] -> fail ("gfxinfo header is missing " ^ name)
      | value :: rest -> if value = name then i else find (i + 1) rest in
    find 0 columns in
  let intended = index "IntendedVsync" and completed = index "FrameCompleted" in
  let rec after_header = function
    | [] -> fail "gfxinfo header disappeared"
    | line :: rest when line = header -> rest
    | _ :: rest -> after_header rest in
  let rows = after_header lines |> List.filter (fun line ->
    let line = String.trim line in
    line <> "" && String.contains line ',' &&
    not (String.starts_with ~prefix:"---" line)) in
  if rows = [] then fail "gfxinfo output contains no frame samples";
  if List.length rows > max_frames then fail "gfxinfo output exceeds its frame sample limit";
  let samples = List.map (fun row ->
    let fields = csv_fields row in
    if List.length fields <> List.length columns then fail "malformed or truncated gfxinfo frame sample";
    let start = int64_field "start" (List.nth fields intended)
    and finish = int64_field "completion" (List.nth fields completed) in
    if start = 0L && finish = 0L then fail "malformed empty gfxinfo frame sample";
    if finish < start then fail "malformed gfxinfo frame interval";
    start, finish, Int64.to_float (Int64.sub finish start) /. 1_000_000.) rows in
  let sorted = List.map (fun (_, _, duration) -> duration) samples |> List.sort Float.compare in
  let percentile p = List.nth sorted (min (List.length sorted - 1) (int_of_float (ceil (p *. float (List.length sorted))) - 1)) in
  let first = List.fold_left (fun current (start, _, _) -> min current start) Int64.max_int samples
  and last = List.fold_left (fun current (_, finish, _) -> max current finish) 0L samples in
  let count = List.length samples in
  [{ name = "frame_count"; value = float count; unit = "frames" };
   { name = "frame_duration_p50"; value = percentile 0.50; unit = "ms" };
   { name = "frame_duration_p90"; value = percentile 0.90; unit = "ms" }],
  Printf.sprintf "%Ld-%Ld ns IntendedVsync/FrameCompleted; %d frame samples" first last count,
  count

let contains text needle =
  let rec find i =
    i + String.length needle <= String.length text &&
    (String.sub text i (String.length needle) = needle || find (i + 1)) in
  find 0
let memory_metrics output =
  if not (contains output "Applications Memory Usage (in Kilobytes)") then
    fail "dumpsys meminfo output is missing its kilobyte unit declaration";
  let lines = String.split_on_char '\n' output in
  let totals = List.filter_map (fun line ->
    match after_prefix "TOTAL PSS:" line with
    | None -> None
    | Some value ->
        let words = String.split_on_char ' ' value |> List.filter ((<>) "") in
        (match words with
         | amount :: rest ->
             let amount = String.concat "" (String.split_on_char ',' amount) in
             if rest <> [] && rest <> ["kB"] then fail "malformed TOTAL PSS unit";
             Some (parse_float "TOTAL PSS" amount)
         | [] -> fail "malformed TOTAL PSS sample")) lines in
  match totals with
  | [value] -> [{ name = "total_pss"; value; unit = "kB" }]
  | [] -> fail "dumpsys meminfo output is missing TOTAL PSS"
  | _ -> fail "dumpsys meminfo output contains mixed or duplicate PSS samples"

let parse_android ~action (session : Workspace_mobile_run.session) ~pid ~condition ~output ~truncated ~exit_code =
  let condition = validate_condition condition in
  if condition <> "warm" then fail "frame and memory reports require an explicitly warm selected app";
  if session.platform <> Workspace_mobile_run.Android then fail "Android parser requires an Android session";
  let pid_text = pid_text pid in
  if String.length output > max_output_bytes then fail "Android measurement output exceeds its byte limit";
  if truncated || exit_code <> 0 then empty_report ~status:"incomplete" ~action session ~pid ~condition ~raw_evidence:output []
  else (
    parse_pid_marker pid output;
    let metrics, sample_window, sample_count = match action with
      | "frames" ->
          let markers = List.filter (fun line -> String.starts_with ~prefix:"** Graphics info for pid " (String.trim line)) (String.split_on_char '\n' output) in
          if markers <> ["** Graphics info for pid " ^ pid_text ^ " [" ^ session.app_id ^ "] **"] then
            fail "gfxinfo output does not match exactly one selected process and app";
          frame_metrics output
      | "memory" ->
          let markers = List.filter (fun line -> String.starts_with ~prefix:"** MEMINFO in pid " (String.trim line)) (String.split_on_char '\n' output) in
          if markers <> ["** MEMINFO in pid " ^ pid_text ^ " [" ^ session.app_id ^ "] **"] then
            fail "meminfo output does not match exactly one selected process and app";
          memory_metrics output, "single dumpsys meminfo snapshot", 1
      | _ -> fail "Android measurement action must be frames or memory" in
    empty_report ~status:"available" ~action session ~pid ~condition
      ~sample_window ~sample_count ~complete:true ~raw_evidence:output metrics)

let parse_ios_capture (session : Workspace_mobile_run.session) ~template ~pid ~condition ~output_path ~output ~truncated ~exit_code =
  let condition = validate_condition condition in
  if session.platform <> Workspace_mobile_run.Ios then fail "Instruments parser requires an iOS Simulator session";
  ignore (pid_text pid);
  if String.length output > max_output_bytes then fail "Instruments output exceeds its byte limit";
  let valid_path = not (Filename.is_relative output_path) && Filename.check_suffix output_path ".trace" in
  if not valid_path then fail "Instruments output path is invalid";
  let status = if truncated || exit_code <> 0 then "incomplete" else "available" in
  empty_report ~status ~action:("instruments:" ^ template) session ~pid ~condition
    ~sample_window:(Printf.sprintf "single xctrace capture capped at %ds" max_capture_seconds)
    ~sample_count:1 ~complete:(status = "available")
    ~raw_evidence:output ~trace_path:output_path []
