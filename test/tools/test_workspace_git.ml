module Workspace_git = Pave.Workspace_git
module Workspace_path = Pave.Workspace_path
module Workspace_process = Pave.Workspace_process

let fail label = failwith ("workspace git: " ^ label)
let rejects label action =
  try action (); fail (label ^ " was accepted")
  with Workspace_git.Error _ -> ()
let expect label value = if not value then fail label

let run program arguments =
  let result = Workspace_process.run ~timeout_seconds:10 ~output_limit:65_536
      ~program ~arguments () in
  match result.Workspace_process.termination with
  | Workspace_process.Exited 0 -> result.Workspace_process.output
  | _ -> fail ("command failed: " ^ program ^ " " ^ String.concat " " arguments ^
               " / " ^ result.Workspace_process.output)

let git dir args = ignore (run "git" ("-C" :: dir :: args))
let write path contents =
  Workspace_path.with_fd path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600
    (fun fd -> Workspace_path.write_all fd contents)
let read path = Workspace_path.read_bounded path 65_536
let mkdir path = Unix.mkdir path 0o700

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | _ -> Unix.unlink path

let () =
  let root = Filename.temp_file "pave-workspace-git-" "" in
  Unix.unlink root;
  mkdir root;
  let root = Unix.realpath root in
  Fun.protect ~finally:(fun () -> remove_tree root) (fun () ->
    let base = Filename.concat root "base" in
    let task = Filename.concat root "task" in
    let owner = "session-test" in
    let id = "task-one" in
    mkdir base;
    git base ["init"; "-q"; "-b"; "main"];
    git base ["config"; "user.name"; "Pave Test"];
    git base ["config"; "core.excludesFile"; "/dev/null"];
    git base ["config"; "core.hooksPath"; "/dev/null"];
    git base ["config"; "commit.gpgsign"; "false"];
    git base ["config"; "user.email"; "pave@example.invalid"];
    write (Filename.concat base "tracked.txt") "initial\n";
    git base ["add"; "tracked.txt"];
    git base ["commit"; "-qm"; "initial"];
    let base_head = String.trim (run "git" ["-C"; base; "rev-parse"; "HEAD"]) in
    write (Filename.concat base "tracked.txt") "base staged\n";
    git base ["add"; "tracked.txt"];
    write (Filename.concat base "tracked.txt") "base unstaged\n";
    write (Filename.concat base "base-only.txt") "leave me\n";
    let staged_before = run "git" ["-C"; base; "diff"; "--cached"; "--binary"] in
    let unstaged_before = run "git" ["-C"; base; "diff"; "--binary"] in
    rejects "unapproved worktree creation" (fun () ->
      ignore (Workspace_git.create_worktree ~base ~path:task ~branch:"pave/denied"
        ~id ~owner ~approved:false ()));
    rejects "worktree path inside base repository" (fun () ->
      ignore (Workspace_git.create_worktree ~base ~path:(Filename.concat base "inside")
        ~branch:"pave/inside" ~id:"inside" ~owner ~approved:true ()));
    rejects "relative worktree path" (fun () ->
      ignore (Workspace_git.create_worktree ~base ~path:"relative-worktree"
        ~branch:"pave/relative" ~id:"relative" ~owner ~approved:true ()));
    mkdir task;
    rejects "worktree path collision" (fun () ->
      ignore (Workspace_git.create_worktree ~base ~path:task ~branch:"pave/task-one"
        ~id ~owner ~approved:true ()));
    Unix.rmdir task;
    git base ["branch"; "pave/task-one"; "HEAD"];
    let collision = Workspace_git.create_worktree ~base ~path:task ~branch:"pave/task-one"
        ~id ~owner ~approved:true () in
    expect "branch collision refused" (not (Workspace_git.process_ok collision));
    git base ["branch"; "-D"; "pave/task-one"];
    let made = Workspace_git.create_worktree ~base ~path:task ~branch:"pave/task-one"
        ~id ~owner ~approved:true () in
    expect "worktree created successfully" (Workspace_git.process_ok made);
    let listed = Workspace_git.list_worktrees ~base ~owner () in
    rejects "managed ID collision" (fun () ->
      ignore (Workspace_git.create_worktree ~base ~path:(Filename.concat root "task-two")
        ~branch:"pave/task-two" ~id ~owner ~approved:true ()));
    expect "ID collision did not create path"
      (not (Sys.file_exists (Filename.concat root "task-two")));
    let listed_detail = String.concat " | "
      (List.map (fun item ->
        item.Workspace_git.id ^ " " ^ item.branch ^ " " ^ item.path) listed) in
    expect ("listing exposes verified managed ID, path, and branch: " ^ listed_detail)
      (List.exists (fun item -> item.Workspace_git.id = id && item.path = task &&
                                item.branch = "pave/task-one") listed);
    let found = Workspace_git.find_worktree ~base ~owner ~id () in
    expect "ID lookup returns canonical worktree" (found.path = task && found.branch = "pave/task-one");
    expect "different owner cannot address the worktree"
      (try ignore (Workspace_git.find_worktree ~base ~owner:"other-session" ~id ()); false
       with Workspace_git.Error _ -> true);
    expect "task worktree is clean initially"
      ((Workspace_git.status ~base ~owner ~id ()).Workspace_process.output = "");
    expect "base staged state preserved" (staged_before =
      run "git" ["-C"; base; "diff"; "--cached"; "--binary"]);
    expect "base unstaged state preserved" (unstaged_before =
      run "git" ["-C"; base; "diff"; "--binary"]);
    expect "base untracked content preserved" (read (Filename.concat base "base-only.txt") = "leave me\n");
    write (Filename.concat task "unrelated.txt") "must not be committed\n";
    git task ["add"; "unrelated.txt"];
    write (Filename.concat task "literal*.txt") "task change\n";
    let state = Workspace_git.status ~base ~owner ~id () in
    expect "status reports task changes" (Workspace_git.process_ok state && String.length state.output > 0);
    let patch = Workspace_git.diff ~base ~owner ~id () in
    expect "diff exposes changed file" (Workspace_git.process_ok patch && String.contains patch.output 't');
    rejects "multiline commit message" (fun () ->
      ignore (Workspace_git.commit ~base ~owner ~id ~approved:true
        ~paths:["literal*.txt"] ~message:"line one\nline two" ()));
    rejects "oversized commit message" (fun () ->
      ignore (Workspace_git.commit ~base ~owner ~id ~approved:true
        ~paths:["literal*.txt"] ~message:(String.make 2_001 'x') ()));
    rejects "empty approved paths" (fun () ->
      ignore (Workspace_git.commit ~base ~owner ~id ~approved:true
        ~paths:[] ~message:"invalid" ()));
    rejects "duplicate approved paths" (fun () ->
      ignore (Workspace_git.commit ~base ~owner ~id ~approved:true
        ~paths:["literal*.txt"; "literal*.txt"] ~message:"invalid" ()));
    rejects "absolute approved paths" (fun () ->
      ignore (Workspace_git.commit ~base ~owner ~id ~approved:true
        ~paths:["/tmp/other"] ~message:"invalid" ()));
    rejects "parent traversal paths" (fun () ->
      ignore (Workspace_git.commit ~base ~owner ~id ~approved:true
        ~paths:["../outside"] ~message:"invalid" ()));
    rejects "unapproved commit" (fun () ->
      ignore (Workspace_git.commit ~base ~owner ~id ~approved:false
        ~paths:["literal*.txt"] ~message:"denied" ()));
    let committed = Workspace_git.commit ~base ~owner ~id ~approved:true
        ~paths:["literal*.txt"] ~message:"isolated task" () in
    expect "approved commit succeeds" (Workspace_git.process_ok committed.process);
    expect "commit result names exact path and ID"
      (committed.files = ["literal*.txt"] && committed.commit_id <> None);
    expect "commit is on task branch"
      (String.trim (run "git" ["-C"; task; "branch"; "--show-current"]) = "pave/task-one");
    expect "base branch head unchanged"
      (String.trim (run "git" ["-C"; base; "rev-parse"; "HEAD"]) = base_head);
    expect "task commit contains only approved change"
      (String.trim (run "git" ["-C"; task; "show"; "--pretty=format:"; "--name-only"; "HEAD"]) = "literal*.txt");
    expect "unrelated staged change remains uncommitted"
      (String.trim (run "git" ["-C"; task; "diff"; "--cached"; "--name-only"]) = "unrelated.txt");
    git task ["reset"; "--hard"; "-q"; "HEAD"];
    expect "history inspection succeeds"
      (Workspace_git.process_ok (Workspace_git.history ~base ~owner ~id ()));
    let user_worktree = Filename.concat root "user-worktree" in
    git base ["worktree"; "add"; "-b"; "user/branch"; "--"; user_worktree; "HEAD"];
    expect "unmanaged Git worktree is not listed as Pave-owned"
      (not (List.exists (fun item -> item.Workspace_git.path = user_worktree)
        (Workspace_git.list_worktrees ~base ~owner ())));
    rejects "unmanaged Git worktree cannot be removed by ID" (fun () ->
      ignore (Workspace_git.remove_worktree ~base ~owner ~id:"user-id" ()));
    rejects "unmanaged ID removal" (fun () ->
      ignore (Workspace_git.remove_worktree ~base ~owner ~id:"unmanaged" ()));
    rejects "other owner's ID removal" (fun () ->
      ignore (Workspace_git.remove_worktree ~base ~owner:"other-session" ~id ()));
    write (Filename.concat task "dirty.txt") "dirty\n";
    rejects "dirty managed worktree removal" (fun () ->
      ignore (Workspace_git.remove_worktree ~base ~owner ~id ()));
    Unix.unlink (Filename.concat task "dirty.txt");
    let mutable_files = List.init 80 (fun index -> "large-" ^ string_of_int index) in
    List.iter (fun name -> write (Filename.concat task name) (String.make 80 'x')) mutable_files;
    let bounded = Workspace_git.status ~output_limit:32 ~base ~owner ~id () in
    expect "subprocess output is bounded" (String.length bounded.output <= 32 && bounded.truncated);
    List.iter (fun name -> Unix.unlink (Filename.concat task name)) mutable_files;
    expect "base dirty/index state remains after lifecycle"
      (staged_before = run "git" ["-C"; base; "diff"; "--cached"; "--binary"] &&
       unstaged_before = run "git" ["-C"; base; "diff"; "--binary"]);
    let inherited_git_dir = Filename.concat root "not-a-repository" in
    let git_dir_before = Sys.getenv_opt "GIT_DIR" in
    Unix.putenv "GIT_DIR" inherited_git_dir;
    let isolated_state =
      Fun.protect
        ~finally:(fun () ->
          match git_dir_before with
          | Some value -> Unix.putenv "GIT_DIR" value
          | None -> Unix.putenv "GIT_DIR" "")
        (fun () -> Workspace_git.status ~base ~owner ~id ())
    in
    expect "workspace Git ignores inherited GIT_DIR"
      (Workspace_git.process_ok isolated_state);
    let removed = Workspace_git.remove_worktree ~base ~owner ~id () in
    expect "clean managed worktree removed"
      (Workspace_git.process_ok removed && not (Sys.file_exists task));
    print_endline "workspace Git isolation and policy: ok")
