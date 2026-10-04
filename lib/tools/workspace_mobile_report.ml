exception Error of string
let fail message = raise (Error message)

let max_sources = 4096
let max_artifacts = 256
let max_checks = 256
let max_report_bytes = 262_144
let max_build_bytes = 67_108_864
let max_artifact_bytes = 16_777_216

type status = Passed | Failed | Not_run | Incomplete
type identity = {
  platform : string; device : string; app_id : string; app_path : string;
  scheme : string option; variant : string option; build_hash : string;
}
type source = { path : string; sha256 : string }
type artifact_kind = Visual | Scenario | Diagnostic
type artifact = { kind : artifact_kind; path : string; sha256 : string; identity : identity }
type check_result = { name : string; status : status }
type verified_check = { result : check_result }
type report = { identity : identity; source_chain : string; sources : source list;
  checks : check_result list; artifacts : (artifact_kind * string * string) list }

let valid_text label maximum value =
  if value = "" || String.length value > maximum ||
     String.exists (fun c -> let n = Char.code c in n < 32 || n = 127) value then
    fail ("invalid " ^ label);
  value
let digest text = Digestif.SHA256.(to_hex (digest_string text))
let read_bounded ?(private_file=false) path limit =
  let st = try Unix.lstat path with _ -> fail "report evidence is unavailable" in
  if st.Unix.st_kind <> Unix.S_REG || st.Unix.st_uid <> Unix.geteuid () ||
     (private_file && st.Unix.st_perm land 0o077 <> 0) ||
     st.Unix.st_size < 0 || st.Unix.st_size > limit then
    fail "report evidence must be a bounded owner-controlled regular file";
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    let opened = Unix.fstat (Unix.descr_of_in_channel ic) in
    if opened.Unix.st_dev <> st.Unix.st_dev || opened.Unix.st_ino <> st.Unix.st_ino ||
       opened.Unix.st_kind <> Unix.S_REG then fail "report evidence changed while opening";
    really_input_string ic st.Unix.st_size)
let checked ?(allow_missing=false) root relative =
  if not (Filename.is_relative relative) || relative = "" then fail "report evidence path must be workspace-relative";
  let root = try Unix.realpath root with _ -> fail "report workspace is unavailable" in
  let parts = String.split_on_char '/' relative in
  if List.exists (fun part -> part = "" || part = "." || part = "..") parts then
    fail "report evidence path must not contain dot components";
  let rec walk parent = function
    | [] -> parent
    | [last] ->
        let path = Filename.concat parent last in
        (try
           if (Unix.lstat path).Unix.st_kind = Unix.S_LNK then fail "report evidence cannot follow symbolic links";
           path
         with Unix.Unix_error (Unix.ENOENT,_,_) when allow_missing -> path
            | Unix.Unix_error _ -> fail "report evidence path is unavailable")
    | part::rest ->
        let path = Filename.concat parent part in
        let st = try Unix.lstat path with _ -> fail "report evidence path is unavailable" in
        if st.Unix.st_kind <> Unix.S_DIR then fail "report evidence path traverses a non-directory";
        walk path rest in
  walk root parts
let hash_build root app_path =
  let path = checked root app_path in
  let stat = try Unix.lstat path with _ -> fail "selected build artifact is unavailable" in
  let entries = ref 0 and total = ref 0 in
  let rec walk prefix depth =
    if depth > 32 then fail "selected build directory is too deep";
    let absolute = checked root prefix in
    let directory_stat = Unix.lstat absolute in
    if directory_stat.Unix.st_size > max_build_bytes then fail "selected build directory exceeds its traversal limit";
    let children = Sys.readdir absolute |> Array.to_list |> List.sort String.compare in
    entries := !entries + List.length children;
    if !entries > max_sources then fail "selected build contains too many entries";
    List.concat_map (fun name ->
      let relative = Filename.concat prefix name in
      let child = checked root relative in
      match (Unix.lstat child).Unix.st_kind with
      | Unix.S_REG ->
          let bytes = read_bounded child max_build_bytes in
          total := !total + String.length bytes;
          if !total > max_build_bytes then fail "selected build exceeds its byte limit";
          [relative ^ "\000file\000" ^ digest bytes]
      | Unix.S_DIR -> (relative ^ "\000directory") :: walk relative (depth + 1)
      | _ -> fail "selected build contains an unsupported filesystem entry") children in
  let parts = if stat.Unix.st_kind = Unix.S_REG then (
    let bytes = read_bounded path max_build_bytes in
    [app_path ^ "\000file\000" ^ digest bytes])
    else if stat.Unix.st_kind = Unix.S_DIR then walk app_path 0
    else fail "selected build artifact has an unsupported type" in
  digest (String.concat "\n" parts)
let same_identity a b =
  a.platform=b.platform && a.device=b.device && a.app_id=b.app_id &&
  a.app_path=b.app_path && a.scheme=b.scheme && a.variant=b.variant && a.build_hash=b.build_hash
let kind_name = function Visual -> "visual" | Scenario -> "scenario" | Diagnostic -> "diagnostic"
let status_name = function Passed -> "passed" | Failed -> "failed" | Not_run -> "not_run" | Incomplete -> "incomplete"
let make_identity (session : Workspace_mobile_run.session) ~build_hash = {
  platform=Workspace_mobile_run.platform_name session.platform; device=session.device;
  app_id=session.app_id; app_path=session.app_path; scheme=session.scheme;
  variant=session.variant; build_hash }
let build_identity root (session : Workspace_mobile_run.session) =
  make_identity session ~build_hash:(hash_build root session.app_path)
let process_check ~name (result : Workspace_process.result) =
  let status = match result.Workspace_process.termination with
    | Workspace_process.Exited 0 when not result.truncated -> Passed
    | Workspace_process.Exited _ | Workspace_process.Signaled _ -> Failed
    | Workspace_process.Timed_out | Workspace_process.Cancelled -> Incomplete in
  let status = if result.truncated then Incomplete else status in
  {result={name=valid_text "check name" 128 name; status}}
let not_run_check ~name =
  {result={name=valid_text "check name" 128 name; status=Not_run}}
let source_json (source : source) =
  `Assoc ["path", `String source.path; "sha256", `String source.sha256]
let status_json status = `String (status_name status)
let kind_string kind = `String (kind_name kind)
let to_json report = `Assoc [
  "version", `Int 1;
  "identity", `Assoc ["platform", `String report.identity.platform;
    "device", `String report.identity.device; "app_id", `String report.identity.app_id;
    "app_path", `String report.identity.app_path;
    "scheme", (match report.identity.scheme with None -> `Null | Some x -> `String x);
    "variant", (match report.identity.variant with None -> `Null | Some x -> `String x);
    "build_sha256", `String report.identity.build_hash];
  "source_chain_sha256", `String report.source_chain;
  "sources", `List (List.map source_json report.sources);
  "checks", `List (List.map (fun c -> `Assoc ["name", `String c.name; "status", status_json c.status]) report.checks);
  "artifacts", `List (List.map (fun (kind,path,sha) -> `Assoc ["kind",kind_string kind; "path",`String path; "sha256",`String sha]) report.artifacts)]
let projection report =
  let text = Yojson.Basic.to_string (to_json report) in
  if String.length text > max_report_bytes then fail "verification report exceeds its size limit";
  text
let create ~root ~(session : Workspace_mobile_run.session) ~sources ~checks ~artifacts =
  if List.length sources > max_sources || List.length checks > max_checks || List.length artifacts > max_artifacts then
    fail "verification report exceeds its item limit";
  let identity = build_identity root session in
  let source_total = ref 0 in
  let sources : source list = List.map (fun (preview : Workspace_edit.preview) ->
    if not preview.changed || Workspace_edit.sha256 preview.content <> preview.result_sha256 then
      fail "source list must contain unchanged-integrity guarded-edit previews";
    let path = valid_text "source path" 4096 preview.path in
    let bytes = read_bounded (checked root path) max_build_bytes in
    if digest bytes <> preview.result_sha256 then fail "guarded-edit source changed after its preview";
    source_total := !source_total + String.length bytes;
    if !source_total > max_build_bytes then fail "source evidence exceeds its aggregate byte limit";
    {path; sha256=preview.result_sha256}) sources |> List.sort (fun (a : source) (b : source) -> String.compare a.path b.path) in
  let rec unique (sources : source list) =
    match sources with
    | a::b::_ when a.path=b.path -> fail "duplicate guarded-edit source path"
    | _::rest -> unique rest
    | [] -> () in
  unique sources;
  let source_chain = digest (String.concat "\n" (List.map (fun (s : source) -> s.path ^ "\000" ^ s.sha256) sources)) in
  let checks = List.map (fun check -> check.result) checks in
  let artifact_refs = List.map (fun (artifact : artifact) ->
    if not (same_identity identity artifact.identity) then fail "report artifact belongs to another app build or device";
    let path = valid_text "artifact path" 4096 artifact.path in
    let bytes = read_bounded ~private_file:true (checked root path) max_artifact_bytes in
    let actual = digest bytes in
    if actual <> artifact.sha256 then fail "report artifact is stale or no longer owned";
    (artifact.kind,path,actual)) artifacts in
  let report = {identity; source_chain; sources; checks; artifacts=artifact_refs} in
  ignore (projection report); report
let write ~root ~path ~cancelled report =
  if (try cancelled () with _ -> true) then fail "verification report write cancelled";
  let relative = valid_text "report output path" 4096 path in
  let destination = checked ~allow_missing:true root relative in
  let data = projection report in
  let directory = Filename.dirname destination in
  let temp = Filename.temp_file ~temp_dir:directory ".mobile-report-" ".tmp" in
  Fun.protect ~finally:(fun () -> try Unix.unlink temp with _ -> ()) (fun () ->
    Unix.chmod temp 0o600;
    let oc = open_out_bin temp in
    Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc data; flush oc);
    if (try cancelled () with _ -> true) then fail "verification report write cancelled";
    try Unix.link temp destination with
    | Unix.Unix_error (Unix.EEXIST,_,_) -> fail "verification report output already exists"
    | Unix.Unix_error _ -> fail "verification report could not be published")
