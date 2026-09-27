type provenance = Rule_produced | Untrusted_claim

type finding = {
  rule_id : string; tool_name : string; tool_version : string; message : string;
  path : string; start_line : int; start_column : int; end_line : int; end_column : int;
  file_sha256 : string; evidence_sha256 : string; provenance : provenance;
}
type scan_result = { findings : finding list; files_scanned : int; truncated : bool }
exception Error of string
let fail message = raise (Error message)
let tool_name = "pave-repository-security"
let tool_version = "1.0.0"
let max_file_bytes = 1_048_576
let max_files = 2_000
let max_findings = 500
let sha256 = Workspace_edit.sha256
let check_cancel cancel = if cancel () then fail "repository security scan cancelled"

let assignment_secret line =
  let text = String.lowercase_ascii line in
  let names = ["password"; "passwd"; "secret"; "api_key"; "apikey";
               "access_token"; "auth_token"; "private_key"] in
  List.exists (fun name ->
    let rec find from =
      try
        let at = Str.search_forward (Str.regexp_string name) text from in
        let after = at + String.length name in
        let rec skip i = if i < String.length line &&
          (line.[i] = ' ' || line.[i] = '\t') then skip (i + 1) else i in
        let i = skip after in
        if i < String.length line && (line.[i] = '=' || line.[i] = ':') then
          let value = skip (i + 1) in
          value < String.length line && line.[value] <> ' ' && line.[value] <> '\t' &&
          not (value + 4 <= String.length line && String.sub text value 4 = "null") &&
          not (value + 2 <= String.length line &&
            ((line.[value] = '"' && line.[value + 1] = '"') ||
             (line.[value] = '\'' && line.[value + 1] = '\'')))
        else find after
      with Not_found -> false
    in find 0) names

let aws_key line =
  let rec find i =
    if i + 20 > String.length line then false
    else if String.sub line i 4 = "AKIA" then
      let rec valid j = j = i + 20 ||
        ((line.[j] >= 'A' && line.[j] <= 'Z' || line.[j] >= '2' && line.[j] <= '7') && valid (j + 1)) in
      valid (i + 4)
    else find (i + 1)
  in find 0

let rule_specs = [
  ("RS001", "Hard-coded credential assignment", assignment_secret);
  ("RS002", "Cloud access key identifier", aws_key);
]
let excluded_directory = function
  | ".git" | ".hg" | ".svn" | "_build" | "build" | ".build" | "dist"
  | "node_modules" | "DerivedData" | ".gradle" | ".dart_tool" | "Pods" -> true
  | _ -> false
let read_file path =
  try Workspace_path.read_bounded path max_file_bytes
  with Workspace_path.Error message -> fail message

let path_excluded relative =
  List.exists excluded_directory (String.split_on_char '/' relative)


let make_finding ~path ~file_sha256 ~line_number ~line rule_id message =
  { rule_id; tool_name; tool_version; message; path; start_line = line_number;
    start_column = 1; end_line = line_number; end_column = max 2 (String.length line + 1);
    file_sha256; evidence_sha256 = sha256 line; provenance = Rule_produced }

let scan ?(cancel = fun () -> false) ?(file_limit = max_files)
    ?(finding_limit = max_findings) ~root () =
  if file_limit < 0 || file_limit > max_files then fail "file limit exceeds scanner bound";
  if finding_limit < 0 || finding_limit > max_findings then fail "finding limit exceeds scanner bound";
  let root = try Workspace_path.root_path root with Workspace_path.Error e -> fail e in
  let files = ref 0 and findings = ref [] and truncated = ref false and entries = ref 0 in
  let max_walk_entries = 10_000 in
  let rec walk relative =
    check_cancel cancel;
    let absolute = if relative = "" then root else Filename.concat root relative in
    let directory = Unix.opendir absolute in
    let names = Fun.protect ~finally:(fun () -> Unix.closedir directory) (fun () ->
      let rec collect acc =
        check_cancel cancel;
        if !entries >= max_walk_entries then (truncated := true; List.sort String.compare acc)
        else match Unix.readdir directory with
          | "." | ".." -> collect acc
          | name -> incr entries; collect (name :: acc)
          | exception End_of_file -> List.sort String.compare acc
      in collect []) in
    List.iter (fun name ->
      check_cancel cancel;
      let rel = if relative = "" then name else relative ^ "/" ^ name in
      if name <> ".gitignore" then
      try
        let stat = Unix.lstat (Filename.concat root rel) in
        if stat.Unix.st_kind <> Unix.S_LNK then (
          let path = Workspace_path.checked_path root rel in
          match stat.Unix.st_kind with
          | Unix.S_DIR when not (path_excluded rel) -> walk rel
          | Unix.S_REG when not (path_excluded rel) ->
              if !files >= file_limit then truncated := true else (
                incr files;
                let contents = read_file path in
                let digest = sha256 contents in
                String.split_on_char '\n' contents |> List.iteri (fun index line ->
                  check_cancel cancel;
                  List.iter (fun (id, message, matches) ->
                    if matches line then
                      if List.length !findings >= finding_limit then truncated := true
                      else findings := make_finding ~path:rel ~file_sha256:digest
                        ~line_number:(index + 1) ~line id message :: !findings
                  ) rule_specs))
          | _ -> ())
      with Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    ) names
  in
  walk "";
  { findings = List.rev !findings; files_scanned = !files; truncated = !truncated }

let proposed_finding ~rule_id ~path ~start_line ~start_column ~end_line ~end_column
    ~file_sha256 ~evidence_sha256 ~message =
  { rule_id; tool_name; tool_version; message; path; start_line; start_column;
    end_line; end_column; file_sha256; evidence_sha256; provenance = Untrusted_claim }

let validate_finding ~root finding =
  match finding.provenance with
  | Untrusted_claim -> false
  | Rule_produced ->
      try
        if finding.tool_name <> tool_name || finding.tool_version <> tool_version ||
           finding.start_line < 1 || finding.end_line <> finding.start_line || finding.start_column <> 1 then false
        else
          let root = Workspace_path.root_path root in
          if path_excluded finding.path then false else
          let path = Workspace_path.regular_path root finding.path in
          let contents = read_file path in
          if sha256 contents <> finding.file_sha256 then false else
          match List.nth_opt (String.split_on_char '\n' contents) (finding.start_line - 1) with
          | None -> false
          | Some line ->
              sha256 line = finding.evidence_sha256 &&
              finding.end_column = max 2 (String.length line + 1) &&
              (match List.find_opt (fun (id, _, _) -> id = finding.rule_id) rule_specs with
               | Some (_, description, matches) -> finding.message = description && matches line
               | None -> false)
      with _ -> false

let sarif ~root scan_result =
  let rec take count = function
    | _ when count = 0 -> []
    | [] -> []
    | finding :: rest -> finding :: take (count - 1) rest
  in
  let valid = take max_findings scan_result.findings |> List.filter (validate_finding ~root) in
  let locations finding = `Assoc ["physicalLocation", `Assoc [
    "artifactLocation", `Assoc ["uri", `String finding.path];
    "region", `Assoc ["startLine", `Int finding.start_line;
      "startColumn", `Int finding.start_column; "endLine", `Int finding.end_line;
      "endColumn", `Int finding.end_column]]] in
  let result finding = `Assoc ["ruleId", `String finding.rule_id; "level", `String "error";
    "message", `Assoc ["text", `String finding.message]; "locations", `List [locations finding];
    "properties", `Assoc ["evidenceSha256", `String finding.evidence_sha256;
      "fileSha256", `String finding.file_sha256; "provenance", `String "deterministic-rule"]] in
  let ids = List.sort_uniq String.compare (List.map (fun f -> f.rule_id) valid) in
  let rules = List.map (fun id ->
    let (_, description, _) = List.find (fun (rule_id, _, _) -> rule_id = id) rule_specs in
    `Assoc ["id", `String id; "shortDescription", `Assoc ["text", `String description]]) ids in
  let incomplete = scan_result.truncated in
  let invocation = `Assoc [
    "executionSuccessful", `Bool (not incomplete);
    "toolExecutionNotifications", (if incomplete then `List [`Assoc [
      "level", `String "warning";
      "message", `Assoc ["text", `String "Repository scan coverage is incomplete because a scan bound was reached."]]]
      else `List [])] in
  Yojson.Basic.to_string (`Assoc ["$schema", `String "https://json.schemastore.org/sarif-2.1.0.json";
    "version", `String "2.1.0"; "runs", `List [`Assoc [
      "tool", `Assoc ["driver", `Assoc ["name", `String tool_name;
        "semanticVersion", `String tool_version; "rules", `List rules]];
      "invocations", `List [invocation];
      "properties", `Assoc ["files_scanned", `Int scan_result.files_scanned;
        "truncated", `Bool scan_result.truncated];
      "results", `List (List.map result valid)]]])
