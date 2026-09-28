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

let command_output executable args =
  let output = Filename.temp_file "pave-completion-command" ".txt" in
  Fun.protect ~finally:(fun () -> Sys.remove output) (fun () ->
    let input = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
    let destination = Unix.openfile output [Unix.O_WRONLY; Unix.O_TRUNC] 0 in
    let pid = Unix.create_process executable args input destination destination in
    Unix.close input;
    Unix.close destination;
    let _, status = Unix.waitpid [] pid in
    let result = read_file output in
    if status <> Unix.WEXITED 0 then
      failwith ("command failed: " ^ executable ^ ": " ^ result);
    result)

let output_lines output =
  String.split_on_char '\n' output |> List.filter (( <> ) "")

let shell_quote value =
  "'" ^ String.concat "'\\''" (String.split_on_char '\'' value) ^ "'"

let require label expected actual =
  if actual <> expected then
    failwith (label ^ ": expected " ^ String.concat ", " expected ^
      "; got " ^ String.concat ", " actual)

let shell_available shell =
  match Sys.getenv_opt "PATH" with
  | None -> false
  | Some path ->
      String.split_on_char ':' path
      |> List.exists (fun dir ->
        let binary = Filename.concat dir shell in
        try Unix.access binary [Unix.X_OK]; true with Unix.Unix_error _ -> false)

let script binary root shell =
  let path = child root ("completion." ^ shell) in
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel (command_output binary
      [|binary; "completions"; shell|]));
  path

let bash_candidates script arguments =
  let words = String.concat " " (List.map shell_quote arguments) in
  (* Bash's compopt is only available within a Readline completion callback. *)
  let command = "source " ^ shell_quote script ^ "; compopt() { :; }; " ^
    "COMP_WORDS=(" ^ words ^ "); COMP_CWORD=" ^
    string_of_int (List.length arguments - 1) ^
    "; _pave_completion; printf '%s\\n' \"${COMPREPLY[@]}\"" in
  output_lines (command_output "bash" [|"bash"; "-c"; command|])

let fish_candidates script commandline =
  let command = "source " ^ shell_quote script ^ "; complete -C " ^
    shell_quote commandline in
  output_lines (command_output "fish" [|"fish"; "-c"; command|])


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
  and previous_state = Sys.getenv_opt "XDG_STATE_HOME"
  and previous_path = Sys.getenv_opt "PATH" in
  Fun.protect ~finally:(fun () ->
    (match previous_home with Some value -> Unix.putenv "HOME" value
     | None -> Unix.putenv "HOME" "");
    (match previous_state with Some value -> Unix.putenv "XDG_STATE_HOME" value
     | None -> Unix.putenv "XDG_STATE_HOME" "");
    (match previous_path with Some value -> Unix.putenv "PATH" value
     | None -> Unix.putenv "PATH" "");
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
    let binary = Unix.realpath Sys.argv.(1) in
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
    Unix.symlink binary (child base "pave");
    Unix.putenv "PATH" (base ^ ":" ^ Option.value ~default:"" previous_path);
    if shell_available "bash" then (
      let bash = script binary base "bash" in
      require "Bash task voice prefix" ["alloy"; "ash"]
        (bash_candidates bash ["pave"; "task"; "speak"; "--voice"; "a"]);
      require "Bash model completion respects @ word boundaries"
        ["chat/offline-model"]
        (bash_candidates bash ["pave"; "--root"; root; "--model"; "ollama@ch"]);
      require "Bash task input consumes next word" []
        (bash_candidates bash ["pave"; "task"; "speak"; "--input"; "--"]);
      require "Bash update has only its own option" ["--check"]
        (bash_candidates bash ["pave"; "update"; "--"]);
      require "Bash update has no further options" []
        (bash_candidates bash ["pave"; "update"; "--check"; "--"]));
    if shell_available "fish" then (
      let fish = script binary base "fish" in
      require "Fish output choice prefix" ["jsonl"]
        (fish_candidates fish "pave --output j");
      require "Fish task voice prefix" ["alloy"; "ash"]
        (fish_candidates fish "pave task speak --voice a");
      require "Fish task voice consumes a dash-prefixed value" []
        (fish_candidates fish "pave task speak --voice --");
      require "Fish output consumes a dash-prefixed value" []
        (fish_candidates fish "pave --output --");
      require "Fish update has only its own option" ["--check"]
        (fish_candidates fish "pave update --");
      require "Fish model value honors a quoted workspace root"
        [Pave.Model_identity.selector model]
        (fish_candidates fish ("pave --root " ^ shell_quote root ^
          " --model ollama@")));
    print_endline "workspace-scoped local shell completion: ok")
