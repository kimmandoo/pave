module Prompt = Pave.System_prompt

let child = Filename.concat
let write path text =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out output) (fun () -> output_string output text)

let rec remove path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter (fun name -> remove (child path name)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let fixture test =
  let previous_home = Sys.getenv_opt "HOME" and previous_xdg = Sys.getenv_opt "XDG_CONFIG_HOME" in
  let root = Filename.temp_file "pave-system-prompt-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () ->
    (match previous_home with None -> Unix.putenv "HOME" "" | Some v -> Unix.putenv "HOME" v);
    (match previous_xdg with None -> Unix.putenv "XDG_CONFIG_HOME" "" | Some v -> Unix.putenv "XDG_CONFIG_HOME" v);
    remove root) (fun () ->
    let config_home = child root "xdg" in
    Unix.mkdir config_home 0o700;
    Unix.putenv "HOME" root;
    Unix.putenv "XDG_CONFIG_HOME" config_home;
    let user = child config_home "pave" and project = child root ".pave" in
    Unix.mkdir user 0o700;
    Unix.mkdir project 0o700;
    test ~root ~user ~project)

let contains text term =
  let n = String.length term in
  let rec seek i = i + n <= String.length text &&
    (String.sub text i n = term || seek (i + 1)) in
  seek 0

let index text term =
  let n = String.length term in
  let rec seek i =
    if i + n > String.length text then failwith ("missing " ^ term)
    else if String.sub text i n = term then i else seek (i + 1) in
  seek 0

let invalid action =
  match action () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith "expected Invalid_argument"

let () =
  fixture (fun ~root ~user ~project ->
    write (child user "SYSTEM.md") "USER_CUSTOM";
    write (child user "APPEND_SYSTEM.md") "USER_APPEND";
    write (child project "SYSTEM_TEMPLATE.md") "Project template: {{root}} PROJECT_TEMPLATE";
    write (child project "APPEND_SYSTEM.md") "PROJECT_APPEND";
    let loaded = Prompt.load ~root ~mobile:"MOBILE_SAFETY" ~project:"AGENTS_RULES" () in
    assert (loaded.diagnostics = []);
    assert (contains loaded.text "PROJECT_TEMPLATE");
    assert (contains loaded.text ("Project template: " ^ root));
    assert (contains loaded.text "PROJECT_APPEND");
    assert (not (contains loaded.text "USER_CUSTOM"));
    assert (not (contains loaded.text "USER_APPEND"));
    assert (index loaded.text "MOBILE_SAFETY" < index loaded.text "AGENTS_RULES");
    assert (index loaded.text "AGENTS_RULES" < index loaded.text "PROJECT_TEMPLATE");
    write (child project "SYSTEM.md") "PROJECT_CUSTOM";
    let literal = Prompt.load ~root ~mobile:"SAFE" ~project:"AGENTS" () in
    assert (contains literal.text "PROJECT_CUSTOM");
    assert (not (contains literal.text "PROJECT_TEMPLATE"));
    let explicit = Prompt.load ~root ~mobile:"SAFE" ~project:"AGENTS"
      ~custom_text:"EXPLICIT" ~append_text:"EXPLICIT_APPEND" () in
    assert (contains explicit.text "EXPLICIT");
    assert (contains explicit.text "EXPLICIT_APPEND");
    assert (not (contains explicit.text "PROJECT_CUSTOM"));
    assert (not (contains explicit.text "PROJECT_APPEND"));
    assert (index explicit.text "SAFE" < index explicit.text "EXPLICIT");
    assert (index explicit.text "AGENTS" < index explicit.text "EXPLICIT");
    invalid (fun () -> Prompt.load ~root ~mobile:"SAFE" ~project:"AGENTS"
      ~custom_text:"both" ~template_file:"missing" ()));
  fixture (fun ~root ~user ~project ->
    write (child user "SYSTEM.md") "USER_FALLBACK";
    write (child project "SYSTEM_TEMPLATE.md") "MALFORMED {{other}}";
    let recovered = Prompt.load ~root ~mobile:"SAFETY" ~project:"AGENTS" () in
    assert (contains recovered.text "USER_FALLBACK");
    assert (List.length recovered.diagnostics = 1);
    let strict_file = child project "strict.md" in
    write strict_file "MALFORMED {{other}}";
    invalid (fun () -> Prompt.load ~root ~mobile:"SAFETY" ~project:"AGENTS"
      ~template_file:strict_file ());
    write strict_file "GOOD {{root}} END";
    let strict = Prompt.load ~root ~mobile:"SAFETY" ~project:"AGENTS"
      ~template_file:strict_file () in
    assert (contains strict.text ("GOOD " ^ root ^ " END"));
    assert (not (contains strict.text "USER_FALLBACK"));
    Unix.unlink (child project "SYSTEM_TEMPLATE.md");
    Unix.symlink strict_file (child project "SYSTEM_TEMPLATE.md");
    let ignored = Prompt.load ~root ~mobile:"SAFETY" ~project:"AGENTS" () in
    assert (contains ignored.text "USER_FALLBACK");
    assert (List.length ignored.diagnostics = 1);
    write strict_file (String.make (Prompt.max_file_bytes + 1) 'x');
    invalid (fun () -> Prompt.load ~root ~mobile:"SAFETY" ~project:"AGENTS"
      ~template_file:strict_file ());
    invalid (fun () -> Prompt.load ~root ~mobile:"SAFETY" ~project:"AGENTS"
      ~append_text:"bad\000append" ()));
  print_endline "system prompt: ok"
