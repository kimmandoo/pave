let child = Filename.concat

let rec remove path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter (fun name -> remove (child path name)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let invalid action = match action () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith "unsafe or foreign session was accepted"

let () =
  let previous_home = Sys.getenv_opt "HOME"
  and previous_state = Sys.getenv_opt "XDG_STATE_HOME" in
  let base = Filename.temp_file "pave-session-store-" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  Fun.protect ~finally:(fun () ->
    (match previous_home with Some path -> Unix.putenv "HOME" path
     | None -> Unix.putenv "HOME" "");
    (match previous_state with Some path -> Unix.putenv "XDG_STATE_HOME" path
     | None -> Unix.putenv "XDG_STATE_HOME" "");
    remove base) (fun () ->
    let root = child base "workspace" in
    let other = child base "another-workspace" in
    let state = child base "state" in
    Unix.mkdir root 0o700;
    Unix.mkdir other 0o700;
    Unix.mkdir state 0o700;
    Unix.putenv "HOME" base;
    Unix.putenv "XDG_STATE_HOME" state;
    let module Store = Pave.Session_store in
    assert (Store.recent ~root = []);
    let unmanaged = Pave.Session.open_file
      ~cwd:(Unix.realpath root) (child root "external.jsonl") in
    ignore (Pave.Session.append unmanaged (Pave.Protocol.user "outside store"));
    invalid (fun () -> Store.toggle_pin ~root unmanaged);
    Store.set_title ~root unmanaged "External journal";
    assert (Pave.Session.title unmanaged = Some "External journal");
    let unmanaged_id = match Pave.Protocol.member "id"
      unmanaged.Pave.Session.header with
      | `String id -> id | _ -> assert false in
    assert (not (Sys.file_exists (child (Store.directory ~root)
      (unmanaged_id ^ ".title"))));
    let first = Store.create ~root in
    let second = Store.create ~root in
    assert (first.Pave.Session.path <> second.Pave.Session.path);
    Pave.Session.set_model first ~provider:"ollama" ~model:"fixture";
    let prompt = "Fix SwiftUI navigation\027[31m\226\128\174spoof" in
    ignore (Pave.Session.append first (Pave.Protocol.user prompt));
    ignore (Pave.Session.append second (Pave.Protocol.user "Inspect Android lifecycle"));
    assert ((Unix.stat first.path).Unix.st_perm land 0o077 = 0);
    assert ((Unix.stat (Filename.dirname first.path)).Unix.st_perm land 0o077 = 0);
    let recent = Store.recent ~root in
    assert (List.length recent = 2);
    assert (List.exists (fun (item : Pave.Session_store.recent) ->
      item.path = first.path && item.title = "Fix SwiftUI navigation [31m spoof") recent);
    assert (List.exists (fun (item : Pave.Session_store.recent) ->
      item.path = second.path && item.title = "Inspect Android lifecycle") recent);
    assert (Store.recent ~root:other = []);
    invalid (fun () -> Store.open_existing ~root:other first.path);
    let reopened = Store.open_existing ~root first.path in
    assert (Pave.Session.history reopened = [Pave.Protocol.user prompt]);
    assert (Pave.Session.model reopened = Some ("ollama", "fixture"));
    Store.set_title ~root first "Release review";
    assert ((Store.search ~root "release" |> List.map
      (fun (item : Store.recent) -> item.path)) = [first.path]);
    assert ((Store.search ~root "RELEASE" |> List.map
      (fun (item : Store.recent) -> item.path)) = [first.path]);
    let first_id = match Pave.Protocol.member "id" first.Pave.Session.header with
      | `String id -> id | _ -> assert false in
    assert ((Store.search ~root (String.sub first_id 0 8) |> List.map
      (fun (item : Store.recent) -> item.path)) = [first.path]);
    assert (Store.toggle_pin ~root second);
    assert (Pave.Session.pinned second);
    assert (List.exists (fun (entry : Pave.Session.entry) ->
      match entry.kind with Pave.Session.Pin true -> true | _ -> false)
      (Pave.Session.entries second));
    assert (Pave.Session.history second =
      [Pave.Protocol.user "Inspect Android lifecycle"]);
    let pinned_recent = Store.recent ~root in
    assert ((List.hd pinned_recent).path = second.path);
    let pins_file = Filename.concat (Store.directory ~root) "pins.json" in
    assert ((Unix.stat pins_file).Unix.st_perm land 0o077 = 0);
    assert ((List.hd pinned_recent).pinned);
    assert (not (Store.toggle_pin ~root second));
    assert (not (Pave.Session.pinned second));
    assert (List.exists (fun (entry : Pave.Session.entry) ->
      match entry.kind with Pave.Session.Pin false -> true | _ -> false)
      (Pave.Session.entries second));
    assert (not (Pave.Session.pinned (Store.open_existing ~root second.path)));
    assert (not (List.hd (Store.recent ~root)).pinned);
    let forked = Store.fork ~root first in
    let parent_id = match Pave.Protocol.member "id" first.Pave.Session.header with
      | `String id -> id | _ -> assert false in
    assert (Pave.Session.parent_session forked = Some parent_id);
    assert (Pave.Session.history forked = Pave.Session.history first);
    assert (Pave.Session.title forked = Some "Release review");
    let forked_id = match Pave.Protocol.member "id" forked.Pave.Session.header with
      | `String id -> id | _ -> assert false in
    assert (List.exists (fun (item : Store.recent) ->
      item.id = forked_id && item.title = "Release review") (Store.recent ~root));
    let link = child base "alias" in
    Unix.symlink root link;
    assert (Store.directory ~root:link = Store.directory ~root);
    let symlink = child (Filename.dirname first.path)
      (Pave.Session.fresh_id () ^ ".jsonl") in
    Unix.symlink first.path symlink;
    assert (List.length (Store.recent ~root) = 3);
    invalid (fun () -> Store.open_existing ~root symlink);
    Unix.chmod second.path 0o644;
    assert (List.length (Store.recent ~root) = 2);
    invalid (fun () -> Store.open_existing ~root second.path));
  print_endline "private session store: ok"
