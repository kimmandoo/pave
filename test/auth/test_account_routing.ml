module Store = Pave.Oauth_store

let child = Filename.concat

let rec remove path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter (fun name -> remove (child path name)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let write path text =
  let out = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out out)
    (fun () -> output_string out text)

let read path = In_channel.with_open_bin path In_channel.input_all

let contains text part =
  let length = String.length part in
  let rec search index =
    index + length <= String.length text &&
    (String.sub text index length = part || search (index + 1)) in
  search 0

let () =
  let binary = Sys.argv.(1) in
  let base = Filename.temp_file "pave-account-routing-" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  let previous_config = Sys.getenv_opt "XDG_CONFIG_HOME"
  and previous_state = Sys.getenv_opt "XDG_STATE_HOME" in
  Fun.protect ~finally:(fun () ->
    (match previous_config with Some value -> Unix.putenv "XDG_CONFIG_HOME" value
     | None -> Unix.putenv "XDG_CONFIG_HOME" "");
    (match previous_state with Some value -> Unix.putenv "XDG_STATE_HOME" value
     | None -> Unix.putenv "XDG_STATE_HOME" "");
    remove base) (fun () ->
    let workspace = child base "workspace"
    and config = child base "config"
    and state = child base "state" in
    List.iter (fun path -> Unix.mkdir path 0o700) [workspace; config; state];
    Unix.putenv "XDG_CONFIG_HOME" config;
    Unix.putenv "XDG_STATE_HOME" state;
    let descriptor = Option.get (Pave.Provider_catalog.find "devin") in
    let route = Option.get (Pave.Provider_catalog.route descriptor "connect") in
    let binding routes : Store.binding = {
      provider = "devin"; grant_type = Store.Provider_session; routes } in
    let path = Store.default_path () in
    let put id access routes =
      Store.put_account ~path ~provider:"devin" ~binding:(binding routes)
        ({ access; refresh = None; expires_at = None;
           account_id = Some id; metadata = [] } : Store.credential) in
    put "account-a" "fixture-account-a" ["other", "https://example.test/other"];
    put "account-b" "devin-session-token$" ["connect", route.endpoint];
    let run ?(input_text = "") args =
      let input_path = child base "input"
      and output = child base "output"
      and errors = child base "errors" in
      write input_path input_text;
      let stdin = Unix.openfile input_path [Unix.O_RDONLY] 0
      and stdout = Unix.openfile output [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600
      and stderr = Unix.openfile errors [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
      let env = Unix.environment ()
        |> Array.to_list
        |> List.filter (fun item ->
          not (String.starts_with ~prefix:"DEVIN_API_KEY=" item) &&
          not (String.starts_with ~prefix:"TERM=" item)) in
      let env = Array.of_list ("DEVIN_API_KEY=" :: "TERM=dumb" :: env) in
      let argv = Array.of_list (binary :: "--root" :: workspace :: args) in
      let pid = Unix.create_process_env binary argv env stdin stdout stderr in
      List.iter Unix.close [stdin; stdout; stderr];
      let _, status = Unix.waitpid [] pid in
      status, read output, read errors in
    let expect_failure args message = match run args with
      | Unix.WEXITED 1, _, errors when contains errors message -> ()
      | status, _, errors ->
          let status = match status with
            | Unix.WEXITED code -> "exit " ^ string_of_int code
            | Unix.WSIGNALED signal -> "signal " ^ string_of_int signal
            | Unix.WSTOPPED signal -> "stopped " ^ string_of_int signal in
          failwith ("unexpected routing result (" ^ status ^ ") for " ^
            String.concat " " args ^ ": " ^ errors) in
    expect_failure [] "redirected stdin prompt is empty";
    expect_failure ["--provider"; "devin"; "--model"; "fixture-model";
      "--prompt"; "hello"] "multiple saved accounts for devin";
    expect_failure ["--model"; "devin@connect#account-b/fixture-model";
      "--prompt"; "hello"] "Devin session credential is invalid";
    expect_failure ["--model"; "devin@connect#account-b/fixture-model";
      "--context-window"; "auto"; "--prompt"; "hello"]
      "Devin session credential is invalid";
    expect_failure ["--provider"; "devin"; "--account"; "account-a";
      "--model"; "fixture-model"; "--prompt"; "hello"]
      "saved credential is not authorized";
    let user_config = child config "pave" in
    write (child user_config "settings.json")
      {|{"default_provider":"devin","default_model":"fixture-model","default_api":"connect"}|};
    let identity = Pave.Model_identity.make ~provider:"devin"
      ~account_id:"account-b" ~route:"connect" ~upstream_id:"fixture-model" () in
    let journal_path = child workspace "conversation.jsonl" in
    let journal = Pave.Session.open_file ~cwd:workspace journal_path in
    Pave.Session.set_model journal identity;
    expect_failure ["--session"; journal_path] "redirected stdin prompt is empty";
    Store.remove_account ~path ~provider:"devin" ~account_id:(Some "account-b");
    expect_failure ["--provider"; "devin"; "--model"; "fixture-model";
      "--prompt"; "hello"] "saved credential is not authorized");
  print_endline "multi-account routing: ok"
