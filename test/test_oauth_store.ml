module Store = Pave.Oauth_store

let credential ?(metadata = ["org", "organization"; "project", "project-1"]) access
    : Store.credential =
  { access; refresh = Some "refresh-secret"; expires_at = Some 1_750_000_000.;
    account_id = Some "user-1"; metadata }

let rejects f =
  try ignore (f ()); false with
  | Store.Storage_error _ -> true

let write_text path text =
  let out = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out out)
    (fun () -> output_string out text)

let mode path = (Unix.stat path).Unix.st_perm

let () =
  let root = Filename.temp_file "pave-oauth-store-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let dir = Filename.concat root "config" in
  let path = Filename.concat dir "oauth.json" in
  Fun.protect ~finally:(fun () ->
    (try Array.iter (fun file -> Sys.remove (Filename.concat dir file))
       (Sys.readdir dir); Unix.rmdir dir with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
    Unix.rmdir root) (fun () ->
    let first = credential "first-secret" in
    assert (Store.get ~path ~provider:"alpha" = None);
    Store.put ~path ~provider:"alpha" first;
    assert (mode dir = 0o700);
    assert (mode path = 0o600);
    assert (mode (path ^ ".lock") = 0o600);
    assert (Store.get ~path ~provider:"alpha" = Some first);
    let second = credential "second-secret" in
    Store.put ~path ~provider:"beta" second;
    let previous_inode = (Unix.stat path).Unix.st_ino in
    let refreshed : Store.credential = { first with access = "new-secret";
      refresh = Some "rotated-secret"; expires_at = Some 1_760_000_000.;
      metadata = ["org", "organization"; "project", "project-2"] } in
    Store.with_lock ~path (fun () ->
      assert (Store.get ~path ~provider:"alpha" = Some first);
      Store.put ~path ~provider:"alpha" refreshed);
    assert (Store.get ~path ~provider:"alpha" = Some refreshed);
    assert (Store.get ~path ~provider:"beta" = Some second);
    assert ((Unix.stat path).Unix.st_ino <> previous_inode);
    assert (mode path = 0o600);
    let json = Yojson.Basic.from_file path in
    let metadata = Yojson.Basic.Util.(
      json |> member "providers" |> member "alpha" |> member "metadata"
      |> to_assoc |> List.map fst) in
    assert (metadata = ["org"; "project"]);
    Store.remove ~path ~provider:"alpha";
    assert (Store.get ~path ~provider:"alpha" = None);
    assert (Store.get ~path ~provider:"beta" = Some second);
    Unix.chmod path 0o644;
    assert (rejects (fun () -> Store.get ~path ~provider:"beta"));
    Unix.chmod path 0o600;

    write_text path "{broken token: secret-value}";
    (match Store.get ~path ~provider:"beta" with
     | _ -> failwith "corrupt credentials were accepted"
     | exception Store.Storage_error message ->
         assert (not (String.contains message ':' || String.contains message '{')));
    assert (rejects (fun () -> Store.put ~path ~provider:"beta" first));
    assert (rejects (fun () -> Store.remove ~path ~provider:"beta"));
    assert (rejects (fun () -> Store.get ~path ~provider:"beta"));
    write_text path {|{"version":1,"providers":{"beta":{"access":null}}}|};
    assert (rejects (fun () -> Store.get ~path ~provider:"beta"));
    write_text path (String.make (1024 * 1024 + 1) 'x');
    assert (rejects (fun () -> Store.get ~path ~provider:"beta"));
    Sys.remove path;

    let outside = Filename.concat root "outside" in
    write_text outside "must not change";
    Unix.symlink outside path;
    assert (rejects (fun () -> Store.get ~path ~provider:"beta"));
    assert (rejects (fun () -> Store.put ~path ~provider:"beta" first));
    Sys.remove path;
    Sys.remove (path ^ ".lock");
    Unix.symlink outside (path ^ ".lock");
    assert (rejects (fun () -> Store.put ~path ~provider:"beta" first));
    Sys.remove (path ^ ".lock");
    let old_dir = Filename.concat root "real-config" in
    Unix.rename dir old_dir;
    Unix.symlink old_dir dir;
    assert (rejects (fun () -> Store.get ~path ~provider:"beta"));
    assert (rejects (fun () -> Store.put ~path ~provider:"beta" first));
    Sys.remove dir;
    Unix.rename old_dir dir;
    let input = open_in outside in
    assert (input_line input = "must not change");
    close_in input;
    Sys.remove outside;
    Unix.chmod dir 0o755;
    assert (Store.get ~path ~provider:"beta" = None);
    assert (mode dir = 0o700);

    let previous_xdg = Sys.getenv_opt "XDG_CONFIG_HOME" in
    let previous_home = Sys.getenv_opt "HOME" in
    Fun.protect ~finally:(fun () ->
      (match previous_xdg with None -> Unix.putenv "XDG_CONFIG_HOME" "" | Some value -> Unix.putenv "XDG_CONFIG_HOME" value);
      (match previous_home with None -> Unix.putenv "HOME" "" | Some value -> Unix.putenv "HOME" value))
      (fun () ->
        Unix.putenv "HOME" root;
        Unix.putenv "XDG_CONFIG_HOME" root;
        assert (Store.default_path () = Filename.concat root "pave/oauth.json");
        Unix.putenv "XDG_CONFIG_HOME" "relative";
        assert (Store.default_path () = Filename.concat root ".config/pave/oauth.json"));


    Store.put ~path ~provider:"counter" (credential ~metadata:[] "0");
    let children = List.init 4 (fun _ ->
      match Unix.fork () with
      | 0 ->
          (try
             for _ = 1 to 12 do
               Store.with_lock ~path (fun () ->
                 let old = match Store.get ~path ~provider:"counter" with
                   | Some value -> value
                   | None -> failwith "missing counter" in
                 let next = string_of_int (int_of_string old.access + 1) in
                 Store.put ~path ~provider:"counter" { old with access = next })
             done;
             exit 0
           with _ -> exit 1)
      | pid -> pid) in
    List.iter (fun pid ->
      match snd (Unix.waitpid [] pid) with
      | Unix.WEXITED 0 -> ()
      | _ -> failwith "concurrent credential writer failed") children;
    assert ((Store.get ~path ~provider:"counter" |> Option.get).access = "48");
    assert (Store.get ~path ~provider:"beta" = None);
    assert (mode path = 0o600));
  print_endline "oauth store: ok"
