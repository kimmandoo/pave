open Pave

let mkdir path = Unix.mkdir path 0o700
let write path text =
  let out = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out out) (fun () -> output_string out text)
let rec remove path =
  let stat = Unix.lstat path in
  if stat.Unix.st_kind = Unix.S_DIR then (
    Array.iter (fun child -> remove (Filename.concat path child)) (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path
let check condition message = if not condition then failwith message
let contains_code code diagnostics =
  List.exists (fun (diagnostic : Local_content.diagnostic) ->
    diagnostic.code = code) diagnostics
let skill name instructions resources =
  "---\nname: " ^ name ^ "\ndescription: Review a change\n" ^
  (if resources = "" then "" else "resources: " ^ resources ^ "\n") ^
  "---\n" ^ instructions
let command name prompt =
  Yojson.Basic.to_string (`Assoc [
    "name", `String name; "description", `String "Review a change";
    "prompt", `String prompt])

let () =
  let root = Filename.temp_file "pave-local-content-" "" in
  Sys.remove root;
  mkdir root;
  Fun.protect ~finally:(fun () -> remove root) (fun () ->
    let user = Filename.concat root "user" in
    let project = Filename.concat root "project" in
    mkdir user; mkdir project;
    let user_skills = Filename.concat user "skills" in
    let user_commands = Filename.concat user "commands" in
    let pave = Filename.concat project ".pave" in
    let project_skills = Filename.concat pave "skills" in
    let project_commands = Filename.concat pave "commands" in
    List.iter mkdir [user_skills; user_commands; pave; project_skills; project_commands];
    let user_skill = Filename.concat user_skills "review" in
    let project_skill = Filename.concat project_skills "review" in
    mkdir user_skill; mkdir project_skill;
    let refs = Filename.concat user_skill "references" in
    mkdir refs;
    let resource = Filename.concat refs "checklist.txt" in
    write resource "Known checklist";
    write (Filename.concat user_skill "SKILL.md")
      (skill "review" "User instructions" "references/checklist.txt");
    write (Filename.concat project_skill "SKILL.md")
      (skill "review" "Project instructions" "");
    write (Filename.concat user_commands "review.json") (command "review" "User command");
    write (Filename.concat project_commands "review.json") (command "review" "Project command");
    let scan ?(enable_user = true) ?(enable_project = true) ?(builtin_names = []) () =
      Local_content.scan ~user_root:user ~project_root:project
        ~enable_user ~enable_project ~builtin_names () in
    let snapshot = scan () in
    check (Local_content.skill_names snapshot = ["review"]) "skill precedence";
    check (Local_content.command_names snapshot = ["review"]) "command precedence";
    let (selected_skill : Local_content.skill) = List.hd snapshot.skills in
    let (selected_command : Local_content.prompt_command) = List.hd snapshot.commands in
    check (selected_skill.source.origin = Local_content.Project &&
      selected_skill.instructions = "Project instructions") "project skill must take precedence";
    check (selected_command.source.origin = Local_content.Project &&
      selected_command.prompt = "Project command") "project command must take precedence";
    check (contains_code "shadowed" snapshot.diagnostics) "shadowing must be attributed";
    let user_only = scan ~enable_project:false () in
    check (List.length user_only.diagnostics = 0) "disabled project must not be inspected";
    let (safe_skill : Local_content.skill) = List.hd user_only.skills in
    check (Local_content.read_resource safe_skill "references/checklist.txt" = Ok "Known checklist")
      "declared resource should be readable";
    (match Local_content.read_resource safe_skill "../outside" with
     | Error { code = "unknown_resource"; _ } -> ()
     | _ -> failwith "unlisted traversal should be refused");
    check (Local_content.command_names (scan ~builtin_names:["/review"] ()) = [])
      "built-in command should reserve its name";
    write (Filename.concat project_skill "SKILL.md")
      (skill "review" "Invalid instructions" "../outside");
    write (Filename.concat project_commands "review.json")
      {|{"name":"review","name":"review","description":"Duplicate key","prompt":"Bad"}|};
    let invalid = scan () in
    check (contains_code "invalid_manifest" invalid.diagnostics)
      "unsafe resource and duplicate JSON fields must be reported";
    check ((List.hd invalid.skills).source.origin = Local_content.User)
      "unsafe project resource must not shadow valid user skill";
    check ((List.hd invalid.commands).source.origin = Local_content.User)
      "duplicate project command fields must not shadow valid user command";
    Unix.unlink (Filename.concat project_skill "SKILL.md");
    Unix.symlink (Filename.concat user_skill "SKILL.md")
      (Filename.concat project_skill "SKILL.md");
    Unix.unlink (Filename.concat project_commands "review.json");
    Unix.symlink (Filename.concat user_commands "review.json")
      (Filename.concat project_commands "review.json");
    let fallback = scan () in
    check (contains_code "unsafe_path" fallback.diagnostics) "symlinks must be reported";
    check ((List.hd fallback.skills).source.origin = Local_content.User)
      "symlink must not shadow user skill";
    check ((List.hd fallback.commands).source.origin = Local_content.User)
      "symlink must not shadow user command";
    Unix.unlink resource;
    Unix.symlink (Filename.concat project "outside") resource;
    (match Local_content.read_resource safe_skill "references/checklist.txt" with
     | Error { code = "unsafe_path"; _ } -> ()
     | _ -> failwith "resource symlink swap must fail closed");
    Unix.unlink resource;
    write resource (String.make (Local_content.max_resource_bytes + 1) 'x');
    (match Local_content.read_resource safe_skill "references/checklist.txt" with
     | Error { code = "file_limit"; _ } -> ()
     | _ -> failwith "oversized resource must not be read");
    check (Local_content.skill_names (scan ~enable_user:false ()) = [])
      "disabled user source must not load through a project symlink";
    for number = 1 to Local_content.max_entries do
      let filename = Printf.sprintf "extra-%02d.json" number in
      write (Filename.concat user_commands filename) "invalid JSON"
    done;
    let capped = scan ~enable_project:false () in
    check (contains_code "entry_limit" capped.diagnostics &&
      Local_content.command_names capped = [])
      "directory cap must reject the whole collection, not select a partial roster")
