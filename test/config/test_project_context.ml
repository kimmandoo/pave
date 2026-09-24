module Context = Pave.Project_context

let write path content =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out output)
    (fun () -> output_string output content)

let directory path = Unix.mkdir path 0o700
let child = Filename.concat

let rec remove path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter (fun name -> remove (child path name)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let fixture f =
  let previous_home = Sys.getenv_opt "HOME" in
  let previous_xdg = Sys.getenv_opt "XDG_CONFIG_HOME" in
  let top = Filename.temp_file "pave-project-context-" "" in
  Sys.remove top;
  directory top;
  let top = Unix.realpath top in
  Fun.protect ~finally:(fun () ->
    (match previous_home with None -> Unix.putenv "HOME" "" | Some value -> Unix.putenv "HOME" value);
    (match previous_xdg with None -> Unix.putenv "XDG_CONFIG_HOME" "" | Some value -> Unix.putenv "XDG_CONFIG_HOME" value);
    remove top)
    (fun () ->
      Unix.putenv "HOME" top;
      let xdg = child top "xdg" in
      directory xdg;
      Unix.putenv "XDG_CONFIG_HOME" xdg;
      let config = child xdg "pave" in
      directory config;
      let project = child top "project" in
      directory project;
      let workspace = child project "workspace" in
      directory workspace;
      f ~top ~config ~project ~workspace)

let contains haystack needle =
  let len = String.length needle in
  let rec seek i =
    i + len <= String.length haystack &&
    (String.sub haystack i len = needle || seek (i + 1)) in
  seek 0

let codes (result : Context.result) =
  List.map (fun (d : Context.diagnostic) -> d.code) result.diagnostics
let paths (result : Context.result) =
  List.map (fun (s : Context.source) -> s.path) result.provenance

let () =
  fixture (fun ~top ~config ~project ~workspace ->
    write (child config "AGENTS.md") "USER\n";
    write (child top "AGENTS.md") "HOME\n";
    write (child project "AGENTS.md") "PROJECT\n";
    write (child workspace "AGENTS.md") "WORKSPACE\n";
    let rules = child (child workspace ".pave") "rules" in
    directory (child workspace ".pave");
    directory rules;
    write (child rules "alpha.md") "---\npaths: src/**/*.ml\n---\nRULE_ALPHA\n";
    write (child rules "beta.md") "---\npaths: docs/**\n---\nRULE_BETA\n";
    let context = Context.load ~root:workspace ~path:"src/App.ml" () in
    assert (paths context = [child config "AGENTS.md"; child top "AGENTS.md";
      child project "AGENTS.md"; child workspace "AGENTS.md";
      child rules "alpha.md"]);
    assert (context.text = "USER\n\n\nHOME\n\n\nPROJECT\n\n\nWORKSPACE\n\n\nRULE_ALPHA\n");
    assert (context.diagnostics = []);
    let without_target = Context.load ~root:workspace () in
    assert (not (contains without_target.text "RULE_ALPHA"));
    let docs = Context.load ~root:workspace ~path:"docs/guide.md" () in
    assert (contains docs.text "RULE_BETA" && not (contains docs.text "RULE_ALPHA"));
    let outside = Context.load ~root:workspace ~path:"../outside.ml" () in
    assert (List.mem "unsafe_target" (codes outside));
    assert (not (contains outside.text "RULE_ALPHA")));
  fixture (fun ~top:_ ~config:_ ~project:_ ~workspace ->
    let shared = child workspace "shared.md" in
    write shared "IMPORTED\n";
    write (child workspace "AGENTS.md") "BEFORE\n@shared.md\nAFTER";
    let first = Context.load ~root:workspace () in
    assert (first.text = "BEFORE\nIMPORTED\n\nAFTER");
    assert (paths first = [child workspace "AGENTS.md"; shared]);
    write shared "@AGENTS.md\n";
    let cycle = Context.load ~root:workspace () in
    assert (List.mem "import_cycle" (codes cycle));
    assert (not (contains cycle.text "IMPORTED"));
    write shared "IMPORTED\n";
    write (child workspace "AGENTS.md") "@shared.md\n@shared.md";
    let shadowed = Context.load ~root:workspace () in
    assert (List.mem "shadowed" (codes shadowed));
    assert (paths shadowed = [child workspace "AGENTS.md"; shared]));
  fixture (fun ~top ~config:_ ~project:_ ~workspace ->
    let outside = child top "outside.md" in
    write outside "ESCAPED_CONTENT";
    let symlink = child workspace "link.md" in
    Unix.symlink outside symlink;
    write (child workspace "AGENTS.md") "SAFE\n@../outside.md\n@link.md\n";
    let context = Context.load ~root:workspace () in
    assert (contains context.text "SAFE");
    assert (not (contains context.text "ESCAPED_CONTENT"));
    assert (List.mem "unsafe_import" (codes context));
    assert (List.mem "unsafe_path" (codes context));
    let via = child workspace "via" in
    Unix.symlink top via;
    write (child workspace "AGENTS.md") "@via/outside.md";
    let directory_link = Context.load ~root:workspace () in
    assert (List.mem "unsafe_path" (codes directory_link));
    assert (not (contains directory_link.text "ESCAPED_CONTENT"));
    write (child workspace "AGENTS.md") "\255";
    let invalid = Context.load ~root:workspace () in
    assert (List.mem "invalid_utf8" (codes invalid));
    assert (invalid.text = "");
    write (child workspace "AGENTS.md") (String.make (Context.max_file_bytes + 1) 'a');
    let oversized = Context.load ~root:workspace () in
    assert (List.mem "file_limit" (codes oversized));
    assert (oversized.text = ""));
  fixture (fun ~top:_ ~config:_ ~project:_ ~workspace ->
    let rules = child (child workspace ".pave") "rules" in
    directory (child workspace ".pave");
    directory rules;
    write (child rules "one.md") "---\npaths: src/**\n---\nONE";
    write (child rules "two.md") "---\npaths: src/**/*.ml\n---\nTWO";
    let matched = Context.load ~root:workspace ~path:"src/a/b.ml" () in
    assert (contains matched.text "ONE\n\nTWO");
    assert (List.mem "rule_conflict" (codes matched));
    let not_matched = Context.load ~root:workspace ~path:"test/a.ml" () in
    assert (not_matched.text = "");
    Unix.symlink (child workspace "src") (child workspace "linked");
    let unsafe_target = Context.load ~root:workspace ~path:"linked/a.ml" () in
    assert (List.mem "unsafe_target" (codes unsafe_target));
    assert (not (contains unsafe_target.text "ONE")));
  fixture (fun ~top ~config:_ ~project:_ ~workspace ->
    let fallback = child top ".config" in
    directory fallback;
    directory (child fallback "pave");
    write (child (child fallback "pave") "AGENTS.md") "FALLBACK_USER";
    Unix.putenv "XDG_CONFIG_HOME" "relative-not-a-config-root";
    let loaded = Context.load ~root:workspace () in
    assert (loaded.text = "FALLBACK_USER");
    assert (List.map (fun (s : Context.source) -> s.kind) loaded.provenance = [Context.User]));
  fixture (fun ~top ~config ~project ~workspace ->
    let text = String.make Context.max_file_bytes 'x' in
    write (child config "AGENTS.md") text;
    write (child top "AGENTS.md") text;
    write (child project "AGENTS.md") text;
    write (child workspace "AGENTS.md") text;
    directory (child workspace ".pave");
    let rules = child (child workspace ".pave") "rules" in
    directory rules;
    write (child rules "extra.md") "---\npaths: **\n---\nEXTRA";
    let bounded = Context.load ~root:workspace ~path:"a.ml" () in
    assert (List.mem "total_limit" (codes bounded));
    assert (not (contains bounded.text "EXTRA"));
    assert (List.length bounded.provenance = 4));
  fixture (fun ~top:_ ~config:_ ~project:_ ~workspace ->
    write (child workspace "AGENTS.md") "@0.md";
    for i = 0 to 8 do
      write (child workspace (string_of_int i ^ ".md"))
        (if i = 8 then "TOO_DEEP" else "@" ^ string_of_int (i + 1) ^ ".md")
    done;
    let bounded = Context.load ~root:workspace () in
    assert (List.mem "import_depth" (codes bounded));
    assert (not (contains bounded.text "TOO_DEEP")));
  print_endline "project context: ok"
