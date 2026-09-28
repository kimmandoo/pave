let child = Filename.concat

let rec remove path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter (fun name -> remove (child path name)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let read_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    really_input_string channel (in_channel_length channel))

let complete binary ~root kind prefix =
  let output = Filename.temp_file "pave-completion" ".txt" in
  Fun.protect ~finally:(fun () -> Sys.remove output) (fun () ->
    let input = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
    let destination = Unix.openfile output [Unix.O_WRONLY; Unix.O_TRUNC] 0 in
    let pid = Unix.create_process binary
      [| binary; "__complete"; kind; prefix; root |]
      input destination destination in
    Unix.close input;
    Unix.close destination;
    let _, status = Unix.waitpid [] pid in
    if status <> Unix.WEXITED 0 then
      failwith ("completion failed: " ^ read_file output);
    String.split_on_char '\n' (read_file output)
    |> List.filter (( <> ) ""))

let () =
  let base = Filename.temp_file "pave-completion-fixture" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  let previous_home = Sys.getenv_opt "HOME"
  and previous_state = Sys.getenv_opt "XDG_STATE_HOME" in
  Fun.protect ~finally:(fun () ->
    (match previous_home with Some value -> Unix.putenv "HOME" value
     | None -> Unix.putenv "HOME" "");
    (match previous_state with Some value -> Unix.putenv "XDG_STATE_HOME" value
     | None -> Unix.putenv "XDG_STATE_HOME" "");
    remove base) (fun () ->
    let root = child base "project with spaces" in
    let other = child base "other" in
    let state = child base "private state" in
    List.iter (fun path -> Unix.mkdir path 0o700) [root; other; state];
    Unix.putenv "HOME" base;
    Unix.putenv "XDG_STATE_HOME" state;
    let saved = Pave.Session_store.create ~root in
    ignore (Pave.Session.append saved (Pave.Protocol.user "completion fixture"));
    let foreign = Pave.Session_store.create ~root:other in
    ignore (Pave.Session.append foreign (Pave.Protocol.user "foreign fixture"));
    let binary = Sys.argv.(1) in
    let sessions = complete binary ~root "session" "" in
    if sessions <> [saved.Pave.Session.path] then
      failwith "session completion dropped a path with spaces or crossed workspaces";
    if complete binary ~root:other "session" "" <>
         [foreign.Pave.Session.path] then
      failwith "workspace-root completion ignored the selected root";
    let model = Pave.Model_identity.make ~provider:"ollama" ~route:"chat"
      ~upstream_id:"offline-model" () in
    Pave.Recent_model.save ~root model;
    if complete binary ~root "model" "ollama@" <>
       [Pave.Model_identity.selector model] ||
       complete binary ~root:other "model" "" <> [] then
      failwith "model completion crossed workspaces or changed exact identity";
    print_endline "workspace-scoped local shell completion: ok")
