module Security = Pave.Repository_security
module Workspace_path = Pave.Workspace_path

let fail label = failwith ("repository security: " ^ label)
let expect label condition = if not condition then fail label
let write path contents =
  Workspace_path.with_fd path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600
    (fun fd -> Workspace_path.write_all fd contents)
let mkdir path = Unix.mkdir path 0o700
let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let () =
  let root = Filename.temp_file "pave-repository-security-" "" in
  Unix.unlink root;
  mkdir root;
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    let secret = "NEVER-EMIT-this-test-secret-94821" in
    let credential_path = Filename.concat root ".env" in
    write credential_path ("password=\"" ^ secret ^ "\"\nordinary=value\n");
    write (Filename.concat root ".gitignore") ".env\nignored.txt\nignored-dir/\n";
    write (Filename.concat root "ignored.txt") ("api_key=" ^ secret ^ "\n");
    mkdir (Filename.concat root "ignored-dir");
    write (Filename.concat (Filename.concat root "ignored-dir") "credentials")
      ("secret=" ^ secret ^ "\n");
    mkdir (Filename.concat root ".git");
    write (Filename.concat (Filename.concat root ".git") "config") ("secret=" ^ secret ^ "\n");
    let result = Security.scan ~root () in
    expect "credential fixtures produce deterministic findings" (List.length result.findings = 3);
    let finding = List.find (fun finding -> finding.Security.path = ".env") result.findings in
    let serialized = Security.sarif ~root result in
    expect "rule-produced provenance" (finding.provenance = Security.Rule_produced);
    expect "hidden .env credential assignment is found"
      (finding.path = ".env" && finding.start_line = 1 && finding.start_column = 1 &&
       finding.end_line = 1);
    expect "raw secret never appears in finding or SARIF"
      (not (try ignore (Str.search_forward (Str.regexp_string secret) finding.message 0); true with Not_found -> false) &&
       not (try ignore (Str.search_forward (Str.regexp_string secret) serialized 0); true with Not_found -> false));
    let sarif = Yojson.Basic.from_string serialized in
    let run = List.hd (Yojson.Basic.Util.to_list (Yojson.Basic.Util.member "runs" sarif)) in
    let invocation = List.hd (Yojson.Basic.Util.to_list (Yojson.Basic.Util.member "invocations" run)) in
    let properties = Yojson.Basic.Util.member "properties" run in
    expect "SARIF 2.1.0 shape"
      (Yojson.Basic.Util.member "version" sarif = `String "2.1.0" &&
       List.length (Yojson.Basic.Util.to_list (Yojson.Basic.Util.member "runs" sarif)) = 1);
    expect "complete SARIF reports successful complete coverage and file count"
      (Yojson.Basic.Util.member "executionSuccessful" invocation = `Bool true &&
       Yojson.Basic.Util.member "files_scanned" properties = `Int 3 &&
       Yojson.Basic.Util.member "truncated" properties = `Bool false);
    expect "gitignored secret files and directories are scanned"
      (List.exists (fun f -> f.Security.path = "ignored.txt") result.findings &&
       List.exists (fun f -> f.Security.path = "ignored-dir/credentials") result.findings);
    let claimed = Security.proposed_finding ~rule_id:finding.rule_id ~path:finding.path
      ~start_line:finding.start_line ~start_column:finding.start_column ~end_line:finding.end_line
      ~end_column:finding.end_column ~file_sha256:finding.file_sha256
      ~evidence_sha256:finding.evidence_sha256 ~message:"model claims secret" in
    expect "model proposal is unvalidated" (not (Security.validate_finding ~root claimed));
    let forged = { finding with Security.provenance = Security.Rule_produced; message = secret } in
    expect "forged rule provenance and secret claim remain unvalidated"
      (not (Security.validate_finding ~root forged));
    expect "forged secret claim is omitted from SARIF"
      (not (try ignore (Str.search_forward (Str.regexp_string secret)
        (Security.sarif ~root { result with Security.findings = [forged] }) 0); true with Not_found -> false));
    write credential_path "password=\"changed-value\"\nordinary=value\n";
    expect "stale file snapshot rejected" (not (Security.validate_finding ~root finding));
    let stale_export = Yojson.Basic.from_string (Security.sarif ~root result) in
    let stale_run = List.hd (Yojson.Basic.Util.to_list (Yojson.Basic.Util.member "runs" stale_export)) in
    let stale_results = Yojson.Basic.Util.to_list (Yojson.Basic.Util.member "results" stale_run) in
    expect "stale finding omitted from export"
      (not (List.exists (fun json ->
        let location = Yojson.Basic.Util.member "physicalLocation"
          (List.hd (Yojson.Basic.Util.to_list (Yojson.Basic.Util.member "locations" json))) in
        Yojson.Basic.Util.member "uri" (Yojson.Basic.Util.member "artifactLocation" location) = `String ".env")
        stale_results));
    let limited = Security.scan ~root ~file_limit:0 () in
    let limited_sarif = Yojson.Basic.from_string (Security.sarif ~root limited) in
    let limited_run = List.hd (Yojson.Basic.Util.to_list (Yojson.Basic.Util.member "runs" limited_sarif)) in
    let limited_invocation = List.hd (Yojson.Basic.Util.to_list
      (Yojson.Basic.Util.member "invocations" limited_run)) in
    let limited_properties = Yojson.Basic.Util.member "properties" limited_run in
    let notification = List.hd (Yojson.Basic.Util.to_list
      (Yojson.Basic.Util.member "toolExecutionNotifications" limited_invocation)) in
    expect "file cap is reported in incomplete SARIF"
      (limited.files_scanned = 0 && limited.truncated &&
       Yojson.Basic.Util.member "executionSuccessful" limited_invocation = `Bool false &&
       Yojson.Basic.Util.member "files_scanned" limited_properties = `Int 0 &&
       Yojson.Basic.Util.member "truncated" limited_properties = `Bool true &&
       Yojson.Basic.Util.member "level" notification = `String "warning" &&
       Yojson.Basic.Util.member "text" (Yojson.Basic.Util.member "message" notification) =
         `String "Repository scan coverage is incomplete because a scan bound was reached.");
    let finding_limited = Security.scan ~root ~finding_limit:0 () in
    expect "finding cap is reported" (finding_limited.findings = [] && finding_limited.truncated);
    let cancelled = try ignore (Security.scan ~root ~cancel:(fun () -> true) ()); false
      with Security.Error _ -> true in
    expect "pre-cancelled scan aborts" cancelled;
    expect "VCS directories remain excluded"
      (not (List.exists (fun f -> String.starts_with ~prefix:".git/" f.Security.path) result.findings));
    print_endline "repository security boundaries: ok")
