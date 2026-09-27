exception Error of string

type process_result = Workspace_process.result

let max_output_bytes = 65_536
let max_commit_message_bytes = 2_000
let default_timeout_seconds = 10

let process_ok result = result.Workspace_process.termination = Workspace_process.Exited 0

let describe_process result =
  let termination = match result.Workspace_process.termination with
    | Workspace_process.Exited code -> Printf.sprintf "exit %d" code
    | Workspace_process.Signaled signal -> Printf.sprintf "signal %d" signal
    | Workspace_process.Timed_out -> "timed out"
    | Workspace_process.Cancelled -> "cancelled"
  in
  Printf.sprintf "git command %s%s" termination
    (if result.output = "" then "" else ": " ^ result.output)

let git_environment () =
  Unix.environment ()
  |> Array.to_list
  |> List.filter_map (fun entry ->
       match String.index_opt entry '=' with
       | Some index ->
           let name = String.sub entry 0 index in
           if String.length name >= 4 && String.sub name 0 4 = "GIT_" then Some name
           else None
       | None -> None)

let run_git ?(timeout_seconds = default_timeout_seconds) ?(read_only = false)
    ?(output_limit = max_output_bytes) ?cancel ?on_progress ~cwd arguments =
  if output_limit < 0 || output_limit > max_output_bytes then
    raise (Error "Git output limit must be between 0 and 65536 bytes");
  let unset = List.concat_map (fun variable -> ["-u"; variable]) (git_environment ()) in
  let safe_options =
    if read_only then
      ["--no-optional-locks"; "-c"; "core.fsmonitor=false";
       "-c"; "core.quotePath=true"]
    else []
  in
  let arguments = unset @ ["/usr/bin/git"] @ safe_options @ arguments in
  Workspace_process.run ?cancel ?on_progress ~timeout_seconds ~output_limit
    ~cwd:(Some cwd) ~program:"/usr/bin/env" ~arguments ()

let require_success result =
  if not (process_ok result) then raise (Error (describe_process result));
  result

let remove_line_ending text =
  let length = String.length text in
  if length > 0 && text.[length - 1] = '\n' then String.sub text 0 (length - 1)
  else text

let git_dir ?cancel ?on_progress ?(output_limit = max_output_bytes) base =
  let result = run_git ~read_only:true ?cancel ?on_progress ~output_limit ~cwd:base
      ["rev-parse"; "--git-common-dir"] |> require_success in
  let path = remove_line_ending result.output in
  if path = "" || result.truncated then raise (Error "Git returned an invalid common directory");
  if Filename.is_relative path then Filename.concat base path else path



let canonical_directory path =
  try Workspace_path.root_path path
  with Unix.Unix_error _ -> raise (Error ("not an existing directory: " ^ path))

let validate_repo ?cancel ?on_progress ?(output_limit = max_output_bytes) path =
  let path = canonical_directory path in
  let result = run_git ~read_only:true ?cancel ?on_progress ~output_limit ~cwd:path
      ["rev-parse"; "--show-toplevel"] |> require_success in
  if result.truncated then raise (Error "Git repository path output was truncated");
  canonical_directory (remove_line_ending result.output)



let validate_branch branch =
  if String.length branch = 0 || String.length branch > 200 ||
     String.contains branch '\000' || String.contains branch '\n' || String.contains branch '\r' then
    raise (Error "invalid task branch name");
  branch

let exists path =
  try ignore (Unix.lstat path); true
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false

let validate_token label token =
  if token = "" || String.length token > 256 || String.contains token '\000' then
    raise (Error (label ^ " must be nonempty and at most 256 bytes"));
  token

let registry_path common_dir worktree =
  Filename.concat (Filename.concat common_dir "pave-managed-worktrees")
    (Digest.to_hex (Digest.string worktree) ^ ".owner")

let write_registry common_dir id owner base worktree branch =
  let directory = Filename.concat common_dir "pave-managed-worktrees" in
  (try Unix.mkdir directory 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) ->
     try
       if (Unix.lstat directory).Unix.st_kind <> Unix.S_DIR then
         raise (Error "managed-worktree registry is not a directory")
     with Unix.Unix_error _ -> raise (Error "managed-worktree registry is unavailable"));
  let target = registry_path common_dir worktree in
  if exists target then raise (Error "worktree ownership record already exists");
  Workspace_path.atomic_write target
    (id ^ "\000" ^ owner ^ "\000" ^ base ^ "\000" ^ worktree ^ "\000" ^ branch ^ "\000")

let read_registry common_dir worktree =
  let path = registry_path common_dir worktree in
  try
    if (Unix.lstat path).Unix.st_kind <> Unix.S_REG then None
    else
      let contents = Workspace_path.read_bounded path 16_384 in
      match String.split_on_char '\000' contents with
      | [id; owner; base; recorded_path; branch; ""] when recorded_path = worktree ->
          Some (id, owner, base, branch)
      | _ -> None
  with
  | Unix.Unix_error _ -> None
  | Workspace_path.Error _ -> None

type managed_worktree = { id : string; path : string; branch : string }

let starts_with prefix text =
  String.length text >= String.length prefix &&
  String.sub text 0 (String.length prefix) = prefix

let parse_worktree_records output =
  let finish current records = match current with
    | Some (path, Some branch) -> (path, branch) :: records
    | _ -> records
  in
  let set_branch current branch = match current with
    | Some (path, _) -> Some (path, Some branch)
    | None -> None
  in
  let current, records =
    List.fold_left (fun (current, records) field ->
      if field = "" then (None, finish current records)
      else if starts_with "worktree " field then
        (Some (String.sub field 9 (String.length field - 9), None), finish current records)
      else if starts_with "branch refs/heads/" field then
        (set_branch current (String.sub field 18 (String.length field - 18)), records)
      else (current, records))
      (None, []) (String.split_on_char '\000' output)
  in
  List.rev (finish current records)

let list_worktrees ?cancel ?on_progress ?(output_limit = max_output_bytes) ~base ~owner () =
  let owner = validate_token "owner" owner in
  let base = validate_repo ?cancel ?on_progress ~output_limit base in
  let common_dir = git_dir ?cancel ?on_progress ~output_limit base in
  let result = run_git ~read_only:true ?cancel ?on_progress ~output_limit ~cwd:base
      ["worktree"; "list"; "--porcelain"; "-z"] in
  if not (process_ok result) then raise (Error (describe_process result));
  if result.truncated then raise (Error "Git worktree metadata exceeded the output limit");
  parse_worktree_records result.output
  |> List.filter_map (fun (raw_path, branch) ->
       try
         let path = canonical_directory raw_path in
         match read_registry common_dir path with
         | Some (id, recorded_owner, recorded_base, recorded_branch)
           when recorded_base = base && recorded_owner = owner && recorded_branch = branch ->
             Some { id; path; branch }
         | _ -> None
       with Error _ -> None)

let find_worktree ?cancel ?on_progress ?(output_limit = max_output_bytes)
    ~base ~owner ~id () =
  let id = validate_token "worktree id" id in
  match List.filter (fun item -> item.id = id)
      (list_worktrees ?cancel ?on_progress ~output_limit ~base ~owner ()) with
  | [item] -> item
  | [] -> raise (Error "no Pave worktree with this id belongs to the owner")
  | _ -> raise (Error "managed worktree id is ambiguous")

let within root path =
  let prefix = if root = "/" then root else root ^ "/" in
  path = root ||
  (String.length path >= String.length prefix &&
   String.sub path 0 (String.length prefix) = prefix)

let path_for_creation ~base path =
  if path = "" || String.contains path '\000' then raise (Error "invalid worktree path");
  if Filename.is_relative path then raise (Error "worktree path must be absolute");
  let parent = try Unix.realpath (Filename.dirname path) with Unix.Unix_error _ ->
    raise (Error "worktree parent directory does not exist") in
  let path = Filename.concat parent (Filename.basename path) in
  if within base path then raise (Error "worktree path must be outside the base repository");
  if exists path then raise (Error "worktree path already exists");
  path

let create_worktree ?cancel ?on_progress ?(output_limit = max_output_bytes)
    ~base ~path ~branch ~id ~owner ~approved () =
  if not approved then raise (Error "worktree creation requires explicit approval");
  let id = validate_token "worktree id" id in
  let owner = validate_token "owner" owner in
  let base = validate_repo ?cancel ?on_progress ~output_limit base in
  let path = path_for_creation ~base path in
  let branch = validate_branch branch in
  let common_dir = git_dir ?cancel ?on_progress ~output_limit base in
  if List.exists (fun item -> item.id = id)
       (list_worktrees ?cancel ?on_progress ~output_limit ~base ~owner ()) then
    raise (Error "worktree id is already managed for this owner");
  let check = run_git ~read_only:true ?cancel ?on_progress ~output_limit ~cwd:base
      ["check-ref-format"; "--branch"; branch] in
  (match check.Workspace_process.termination with
   | Workspace_process.Exited 0 -> ()
   | Workspace_process.Exited _ -> raise (Error "invalid Git branch name")
   | _ -> raise (Error (describe_process check)));
  let record = registry_path common_dir path in
  if exists record then raise (Error "worktree path is already managed");
  let result = run_git ?cancel ?on_progress ~output_limit ~cwd:base
      ["worktree"; "add"; "-b"; branch; "--"; path; "HEAD"] in
  if process_ok result then (
    try write_registry common_dir id owner base path branch
    with
    | Error detail ->
        raise (Error (Printf.sprintf
          "worktree was created at %s on branch %s but could not be marked as Pave-managed: %s"
          path branch detail))
    | Workspace_path.Error detail ->
        raise (Error (Printf.sprintf
          "worktree was created at %s on branch %s but could not be marked as Pave-managed: %s"
          path branch detail))
    | Unix.Unix_error (error, operation, argument) ->
        raise (Error (Printf.sprintf
          "worktree was created at %s on branch %s but could not be marked as Pave-managed: %s (%s %s)"
          path branch (Unix.error_message error) operation argument))
  );
  result

let ensure_managed ?cancel ?on_progress ~base ~owner ~id () =
  let owner = validate_token "owner" owner in
  let id = validate_token "worktree id" id in
  let base = validate_repo ?cancel ?on_progress base in
  let common_dir = git_dir ?cancel ?on_progress base in
  let worktree = find_worktree ?cancel ?on_progress ~base ~owner ~id () in
  let result = run_git ~read_only:true ?cancel ?on_progress ~cwd:worktree.path
      ["symbolic-ref"; "--short"; "HEAD"] |> require_success in
  if String.trim result.output <> worktree.branch then
    raise (Error "managed worktree branch has changed");
  (base, worktree.path, common_dir, worktree.branch)

let status ?cancel ?on_progress ?(output_limit = max_output_bytes) ~base ~owner ~id () =
  let _base, path, _common_dir, _branch =
    ensure_managed ?cancel ?on_progress ~base ~owner ~id () in
  run_git ~read_only:true ?cancel ?on_progress ~output_limit ~cwd:path
    ["status"; "--porcelain=v1"; "--untracked-files=all"]

let diff ?cancel ?on_progress ?(output_limit = max_output_bytes) ~base ~owner ~id () =
  let _base, path, _common_dir, _branch =
    ensure_managed ?cancel ?on_progress ~base ~owner ~id () in
  run_git ~read_only:true ?cancel ?on_progress ~output_limit ~cwd:path
    ["diff"; "HEAD"; "--no-ext-diff"; "--no-textconv"; "--no-color"; "--"]

let history ?cancel ?on_progress ?(output_limit = max_output_bytes) ?(count = 20)
    ~base ~owner ~id () =
  if count < 1 || count > 100 then raise (Error "history count must be between 1 and 100");
  let _base, path, _common_dir, _branch =
    ensure_managed ?cancel ?on_progress ~base ~owner ~id () in
  run_git ~read_only:true ?cancel ?on_progress ~output_limit ~cwd:path
    ["log"; "--no-color"; "--format=%h %aI %s"; "-n";
     string_of_int count; "--"]

let remove_worktree ?cancel ?on_progress ?(output_limit = max_output_bytes)
    ~base ~owner ~id () =
  let base, path, common_dir, _branch =
    ensure_managed ?cancel ?on_progress ~base ~owner ~id () in
  let state = run_git ~read_only:true ?cancel ?on_progress ~output_limit ~cwd:path
      ["status"; "--porcelain=v1"; "-z"; "--untracked-files=all"; "--ignored=matching"]
    |> require_success in
  if state.output <> "" || state.truncated || state.bytes_received <> 0 then
    raise (Error "refusing to remove a dirty worktree");
  let result = run_git ?cancel ?on_progress ~output_limit ~cwd:base
      ["worktree"; "remove"; "--"; path] in
  if process_ok result then (
    try Unix.unlink (registry_path common_dir path)
    with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  result


let validate_commit_paths paths =
  if paths = [] || List.length paths > 256 then
    raise (Error "commit requires between 1 and 256 approved paths");
  let seen = Hashtbl.create (List.length paths) in
  let total_bytes = List.fold_left (fun total path ->
    if path = "" || String.length path > 4096 || String.contains path '\000' ||
       not (Filename.is_relative path) ||
       List.exists (fun part -> part = "" || part = "." || part = "..")
         (String.split_on_char '/' path) then
      raise (Error "commit paths must be normalized relative paths without '..'");
    if Hashtbl.mem seen path then raise (Error "duplicate approved commit path");
    Hashtbl.add seen path ();
    total + String.length path + 1) 0 paths in
  if total_bytes > 32_768 then raise (Error "approved commit paths exceed the 32768-byte limit");
  paths


type commit_result = {
  process : process_result;
  files : string list;
  commit_id : string option;
}

let commit ?cancel ?on_progress ?(output_limit = max_output_bytes)
    ~base ~owner ~id ~approved ~paths ~message () =
  if not approved then raise (Error "commit requires explicit approval");
  if message = "" || String.length message > max_commit_message_bytes ||
     String.contains message '\n' || String.contains message '\r' || String.contains message '\000' then
    raise (Error "commit message must be a nonempty single line of at most 2000 bytes");
  let paths = validate_commit_paths paths in
  let _base, worktree, _common_dir, _branch =
    ensure_managed ?cancel ?on_progress ~base ~owner ~id () in

  let add = run_git ?cancel ?on_progress ~output_limit ~cwd:worktree
      ("--literal-pathspecs" :: "add" :: "-A" :: "--" :: paths) in
  if not (process_ok add) then { process = add; files = []; commit_id = None }
  else
    let committed = run_git ?cancel ?on_progress ~output_limit ~cwd:worktree
        ("--literal-pathspecs" :: "commit" :: "--only" :: "-m" :: message :: "--" :: paths) in
    if not (process_ok committed) then { process = committed; files = []; commit_id = None }
    else
      let head = run_git ~read_only:true ?cancel ?on_progress ~output_limit ~cwd:worktree ["rev-parse"; "HEAD"] in
      if not (process_ok head) || head.truncated then
        { process = head; files = []; commit_id = None }
      else
        let commit_id = String.trim head.output in
        let changed = run_git ~read_only:true ?cancel ?on_progress ~output_limit ~cwd:worktree
            ["diff-tree"; "--no-commit-id"; "--name-only"; "-r"; "-z"; "HEAD"] in
        if not (process_ok changed) || changed.truncated then
          { process = changed; files = []; commit_id = Some commit_id }
        else
          { process = changed; files = List.filter (( <> ) "") (String.split_on_char '\000' changed.output);
            commit_id = Some commit_id }

