module Store = Pave.Oauth_store

let credential ?(metadata = ["org", "organization"; "project", "project-1"])
    ?(account_id = Some "user-1") access : Store.credential =
  { access; refresh = Some "refresh-secret"; expires_at = Some 1_750_000_000.;
    account_id; metadata }

let binding provider : Store.binding = {
  provider;
  grant_type = Store.Authorization_code;
  routes = ["token", "https://provider.test/token"];
}

let rejects f =
  try ignore (f ()); false with
  | Store.Storage_error _ -> true

let write_text path text =
  let out = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out out)
    (fun () -> output_string out text)

let mode path = (Unix.stat path).Unix.st_perm

let find path provider account_id =
  Store.account ~path ~provider ~account_id
  |> Option.map (fun entry -> entry.Store.credential)

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
    assert (Store.accounts ~path ~provider:"alpha" = []);
    Store.put_account ~path ~provider:"alpha" ~binding:(binding "alpha") first;
    assert (mode dir = 0o700);
    assert (mode path = 0o600);
    assert (mode (path ^ ".lock") = 0o600);
    assert (find path "alpha" (Some "user-1") = Some first);
    let second = credential ~account_id:(Some "user-2") "second-secret" in
    Store.put_account ~path ~provider:"alpha" ~binding:(binding "alpha") second;
    assert (List.length (Store.accounts ~path ~provider:"alpha") = 2);
    let rows = Yojson.Basic.Util.to_list
      (Yojson.Basic.Util.member "accounts" (Yojson.Basic.from_file path)) in
    write_text path (Yojson.Basic.to_string (`Assoc [
      "version", `Int 2; "accounts", `List (List.rev rows)]));
    assert (List.map (fun account -> account.Store.selection_id)
      (Store.accounts ~path ~provider:"alpha") = ["user-1"; "user-2"]);
    let no_id = credential ~account_id:None "no-id-secret" in
    let local_id_1 = Store.put_account_with_selection ~path ~provider:"alpha"
      ~binding:(binding "alpha") no_id in
    let local_id_2 = Store.put_account_with_selection ~path ~provider:"alpha"
      ~binding:(binding "alpha") (credential ~account_id:None "second-no-id") in
    assert (local_id_1 <> local_id_2);
    assert (String.starts_with ~prefix:"pave-local:" local_id_1);
    assert (String.starts_with ~prefix:"pave-local:" local_id_2);
    assert (Store.account ~path ~provider:"alpha" ~account_id:None = None);
    assert (List.map (fun account -> account.Store.selection_id)
      (Store.accounts ~path ~provider:"alpha") =
      List.sort String.compare ["user-1"; "user-2"; local_id_1; local_id_2]);
    assert (find path "alpha" (Some local_id_1) = Some no_id);
    let refreshed_anonymous = { no_id with access = "rotated-anonymous" } in
    Store.put_account ~path ~provider:"alpha" ~binding:(binding "alpha")
      ~selection_id:local_id_1 refreshed_anonymous;
    assert (find path "alpha" (Some local_id_1) = Some refreshed_anonymous);
    assert (find path "alpha" (Some local_id_2) =
      Some (credential ~account_id:None "second-no-id"));
    let anonymous_selector = Store.put_account_with_selection ~path
      ~provider:"mixed-account-identities"
      ~binding:(binding "mixed-account-identities") no_id in
    assert (find path "mixed-account-identities" None = Some no_id);
    let identified = credential ~account_id:(Some "user-3") "identified-secret" in
    Store.put_account ~path ~provider:"mixed-account-identities"
      ~binding:(binding "mixed-account-identities") identified;
    assert (find path "mixed-account-identities" (Some "user-3") = Some identified);
    assert (Store.account ~path ~provider:"mixed-account-identities"
      ~account_id:None = None);
    assert (Store.account ~path ~provider:"mixed-account-identities"
      ~account_id:(Some anonymous_selector) <> None);
    let other = credential "beta-secret" in
    Store.put_account ~path ~provider:"beta" ~binding:(binding "beta") other;
    assert (rejects (fun () ->
      Store.put_account ~path ~provider:"beta" ~binding:(binding "alpha") other));
    let previous_inode = (Unix.stat path).Unix.st_ino in
    let refreshed : Store.credential = { first with access = "new-secret";
      refresh = Some "rotated-secret"; expires_at = Some 1_760_000_000.;
      metadata = ["org", "organization"; "project", "project-2"] } in
    Store.with_lock ~path (fun () ->
      assert (find path "alpha" (Some "user-1") = Some first);
      Store.put_account ~path ~provider:"alpha" ~binding:(binding "alpha") refreshed);
    assert (find path "alpha" (Some "user-1") = Some refreshed);
    assert (find path "alpha" (Some "user-2") = Some second);
    assert (find path "beta" (Some "user-1") = Some other);
    assert ((Unix.stat path).Unix.st_ino <> previous_inode);
    assert (mode path = 0o600);
    let json = Yojson.Basic.from_file path in
    assert (Yojson.Basic.Util.(json |> member "version" |> to_int) = 3);
    let metadata = Yojson.Basic.Util.(
      json |> member "accounts" |> to_list |> List.hd
      |> member "credential" |> member "metadata" |> to_assoc |> List.map fst) in
    assert (metadata = ["org"; "project"]);
    Store.remove_account ~path ~provider:"alpha" ~account_id:(Some "user-1");
    assert (find path "alpha" (Some "user-1") = None);
    assert (find path "alpha" (Some "user-2") = Some second);
    assert (rejects (fun () ->
      Store.remove_account ~path ~provider:"alpha" ~account_id:None));
    Store.remove_account ~path ~provider:"alpha" ~account_id:(Some local_id_1);
    Store.remove_account ~path ~provider:"alpha" ~account_id:(Some local_id_2);
    assert (List.map (fun account -> account.Store.selection_id)
      (Store.accounts ~path ~provider:"alpha") = ["user-2"]);
    Store.remove_provider ~path ~provider:"alpha";
    assert (Store.accounts ~path ~provider:"alpha" = []);
    assert (find path "beta" (Some "user-1") = Some other);
    Unix.chmod path 0o644;
    assert (rejects (fun () -> Store.accounts ~path ~provider:"beta"));
    Unix.chmod path 0o600;

    (* v1 and v2 accountless rows migrate to provider-neutral local selectors. *)
    write_text path {|{"version":1,"providers":{"legacy":{"access":"legacy-token","refresh":null,"expires_at":null,"account_id":"legacy-user","metadata":{}}}}|};
    let inode = (Unix.stat path).Unix.st_ino in
    assert (find path "legacy" (Some "legacy-user") =
      Some { access = "legacy-token"; refresh = None; expires_at = None;
        account_id = Some "legacy-user"; metadata = [] });
    assert ((Unix.stat path).Unix.st_ino <> inode);
    assert (Yojson.Basic.Util.to_int
      (Yojson.Basic.Util.member "version" (Yojson.Basic.from_file path)) = 3);
    assert (mode path = 0o600);
    write_text path {|{"version":1,"providers":{"legacy-anon":{"access":"legacy-token","refresh":null,"expires_at":null,"account_id":null,"metadata":{}}}}|};
    let legacy_local = Store.accounts ~path ~provider:"legacy-anon" |> List.hd in
    assert (legacy_local.credential.account_id = None);
    assert (String.starts_with ~prefix:"pave-local:" legacy_local.selection_id);
    assert (Store.account ~path ~provider:"legacy-anon"
      ~account_id:(Some legacy_local.selection_id) = Some legacy_local);
    write_text path {|{"version":2,"accounts":[{"provider":"legacy-v2","credential":{"access":"legacy-token","refresh":null,"expires_at":null,"account_id":null,"metadata":{}},"binding":null}]}|};
    let legacy_v2 = Store.accounts ~path ~provider:"legacy-v2" |> List.hd in
    assert (legacy_v2.credential.account_id = None);
    assert (String.starts_with ~prefix:"pave-local:" legacy_v2.selection_id);
    assert (Yojson.Basic.Util.to_int
      (Yojson.Basic.Util.member "version" (Yojson.Basic.from_file path)) = 3);

    write_text path "{broken token: secret-value}";
    (match Store.accounts ~path ~provider:"beta" with
     | _ -> failwith "corrupt credentials were accepted"
     | exception Store.Storage_error message ->
         assert (not (String.contains message ':' || String.contains message '{')));
    assert (rejects (fun () -> Store.put_account ~path ~provider:"beta" ~binding:(binding "beta") first));
    assert (rejects (fun () -> Store.remove_provider ~path ~provider:"beta"));
    assert (rejects (fun () -> Store.accounts ~path ~provider:"beta"));
    write_text path {|{"version":2,"accounts":[{"provider":"x","credential":{"access":"a","refresh":null,"expires_at":null,"account_id":"id","metadata":{}},"binding":null},{"provider":"x","credential":{"access":"b","refresh":null,"expires_at":null,"account_id":"id","metadata":{}},"binding":null}]}|};
    assert (rejects (fun () -> Store.accounts ~path ~provider:"x"));
    write_text path {|{"version":2,"accounts":[{"provider":"x","credential":{"access":"a","refresh":null,"expires_at":null,"account_id":"id","metadata":{}},"binding":{"provider":"x","grant_type":"bogus","routes":{}}}]}|};
    assert (rejects (fun () -> Store.accounts ~path ~provider:"x"));
    write_text path {|{"version":1,"providers":{"beta":{"access":null}}}|};
    assert (rejects (fun () -> Store.accounts ~path ~provider:"beta"));
    write_text path (String.make (1024 * 1024 + 1) 'x');
    assert (rejects (fun () -> Store.accounts ~path ~provider:"beta"));
    Sys.remove path;

    let outside = Filename.concat root "outside" in
    write_text outside "must not change";
    Unix.symlink outside path;
    assert (rejects (fun () -> Store.accounts ~path ~provider:"beta"));
    assert (rejects (fun () -> Store.put_account ~path ~provider:"beta" ~binding:(binding "beta") first));
    Sys.remove path;
    Sys.remove (path ^ ".lock");
    Unix.symlink outside (path ^ ".lock");
    assert (rejects (fun () -> Store.put_account ~path ~provider:"beta" ~binding:(binding "beta") first));
    Sys.remove (path ^ ".lock");
    let old_dir = Filename.concat root "real-config" in
    Unix.rename dir old_dir;
    Unix.symlink old_dir dir;
    assert (rejects (fun () -> Store.accounts ~path ~provider:"beta"));
    assert (rejects (fun () -> Store.put_account ~path ~provider:"beta" ~binding:(binding "beta") first));
    Sys.remove dir;
    Unix.rename old_dir dir;
    let input = open_in outside in
    assert (input_line input = "must not change");
    close_in input;
    Sys.remove outside;
    Unix.chmod dir 0o755;
    assert (Store.accounts ~path ~provider:"beta" = []);
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

    Store.put_account ~path ~provider:"counter" ~binding:(binding "counter")
      (credential ~metadata:[] "0");
    let children = List.init 4 (fun _ ->
      match Unix.fork () with
      | 0 ->
          (try
             for _ = 1 to 12 do
               Store.with_lock ~path (fun () ->
                 let old = find path "counter" (Some "user-1") |> Option.get in
                 let next = string_of_int (int_of_string old.access + 1) in
                 Store.put_account ~path ~provider:"counter" ~binding:(binding "counter")
                   { old with access = next })
             done;
             exit 0
           with _ -> exit 1)
      | pid -> pid) in
    List.iter (fun pid ->
      match snd (Unix.waitpid [] pid) with
      | Unix.WEXITED 0 -> ()
      | _ -> failwith "concurrent credential writer failed") children;
    assert ((find path "counter" (Some "user-1") |> Option.get).access = "48");
    let invoked = ref false in
    assert (rejects (fun () ->
      Store.with_lock ~path ~cancel:(fun () -> true) (fun () -> invoked := true)));
    assert (not !invoked);
    assert (rejects (fun () ->
      Store.with_lock ~path ~deadline:(Unix.gettimeofday () -. 1.)
        (fun () -> invoked := true)));
    assert (not !invoked);
    let lock_fd = Unix.openfile (path ^ ".lock") [Unix.O_RDWR] 0 in
    Unix.lockf lock_fd Unix.F_LOCK 0;
    let waiter = Unix.fork () in
    if waiter = 0 then (
      let started = Unix.gettimeofday () in
      (try
         Store.with_lock ~path
           ~cancel:(fun () -> Unix.gettimeofday () -. started > 0.1)
           (fun () -> ());
         exit 3
       with Store.Storage_error _ -> exit 0 | _ -> exit 4));
    (match snd (Unix.waitpid [] waiter) with
     | Unix.WEXITED 0 -> ()
     | _ -> failwith "cancelled cross-process OAuth lock waiter did not stop");
    let contender = Unix.fork () in
    if contender = 0 then (
      let fd = Unix.openfile (path ^ ".lock") [Unix.O_RDWR] 0 in
      (try Unix.lockf fd Unix.F_TLOCK 0; exit 2
       with Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) -> exit 0));
    (match snd (Unix.waitpid [] contender) with
     | Unix.WEXITED 0 -> ()
     | _ -> failwith "cancelled lock waiter released another process lock");
    Unix.lockf lock_fd Unix.F_ULOCK 0;
    Unix.close lock_fd;
    Store.with_lock ~path (fun () ->
      Store.with_lock ~path (fun () -> invoked := true));
    assert !invoked;
    let contender_entered = Atomic.make false in
    Store.with_lock ~path (fun () ->
      let contender = Thread.create (fun () ->
        try Store.with_lock ~path
          ~deadline:(Unix.gettimeofday () +. 0.1)
          (fun () -> Atomic.set contender_entered true)
        with Store.Storage_error _ -> ()) () in
      Thread.join contender);
    assert (not (Atomic.get contender_entered));
    let thread_writers = List.init 4 (fun _ ->
      Thread.create (fun () ->
        for _ = 1 to 12 do
          Store.with_lock ~path (fun () ->
            let old = find path "counter" (Some "user-1") |> Option.get in
            Thread.yield ();
            Store.put_account ~path ~provider:"counter" ~binding:(binding "counter")
              { old with access = string_of_int (int_of_string old.access + 1) })
        done) ()) in
    List.iter Thread.join thread_writers;
    assert ((find path "counter" (Some "user-1") |> Option.get).access = "96");
    assert (Store.accounts ~path ~provider:"beta" = []);
    assert (mode path = 0o600));
  print_endline "oauth store: ok"
