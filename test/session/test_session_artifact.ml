let child path name = Filename.concat path name

let rec remove path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter (fun name -> remove (child path name)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let rejected action = match action () with
  | exception Pave.Session_artifact.Error _ -> ()
  | _ -> failwith "unsafe or invalid artifact operation was accepted"
let write_file path text =
  let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_TRUNC] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    ignore (Unix.write_substring fd text 0 (String.length text)))

let () =
  let previous_home = Sys.getenv_opt "HOME"
  and previous_state = Sys.getenv_opt "XDG_STATE_HOME" in
  let base = Filename.temp_file "pave-artifact-store-" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  Fun.protect ~finally:(fun () ->
    (match previous_home with Some value -> Unix.putenv "HOME" value
     | None -> Unix.putenv "HOME" "");
    (match previous_state with Some value -> Unix.putenv "XDG_STATE_HOME" value
     | None -> Unix.putenv "XDG_STATE_HOME" "");
    remove base) (fun () ->
    let root = child base "workspace" and state = child base "state" in
    Unix.mkdir root 0o700;
    Unix.mkdir state 0o700;
    Unix.putenv "HOME" base;
    Unix.putenv "XDG_STATE_HOME" state;
    let module Store = Pave.Session_artifact in
    let store = Store.open_for_workspace ~root in
    let owner = "0123456789abcdef0123456789abcdef"
    and other = "fedcba9876543210fedcba9876543210" in
    let dir = Filename.concat
      (Filename.concat (Filename.concat (Filename.concat state "pave") "sessions")
        (Digestif.SHA256.(to_hex (digest_string (Unix.realpath root))))) "artifacts" in
    let permissions = (Unix.stat dir).Unix.st_perm land 0o777 in
    assert (permissions land 0o077 = 0);
    Unix.chmod dir 0o755;
    rejected (fun () -> Store.open_for_workspace ~root);
    Unix.chmod dir 0o700;
    let payload = "\000\255binary\000payload" in
    let item = Store.put store ~owner ~name:"binary.dat" ~mime_type:"application/octet-stream" payload in
    assert (Store.read store ~owner ~id:item.id = payload);
    assert (item.size = String.length payload);
    assert (item.sha256 = Digestif.SHA256.(to_hex (digest_string payload)));
    assert (Store.list store ~owner () = [item]);
    assert (Store.list store ~owner:other () = []);
    rejected (fun () -> Store.read store ~owner:other ~id:item.id);
    rejected (fun () -> Store.read store ~owner ~id:(String.make 32 'Z'));
    rejected (fun () -> Store.begin_write store ~owner:"bad" ~name:"x" ~mime_type:"text/plain");
    let writer = Store.begin_write store ~owner ~name:"stream" ~mime_type:"text/plain" in
    Store.write writer "part-";
    Store.write writer "two";
    let streamed = Store.finish writer in
    assert (Store.read store ~owner ~id:streamed.id = "part-two");
    let incomplete = Store.begin_write store ~owner ~name:"incomplete" ~mime_type:"text/plain" in
    Store.write incomplete "hidden";
    Store.abort incomplete;
    assert (not (Sys.file_exists (Filename.concat dir ("." ^ incomplete.id ^ ".tmp"))));
    assert (List.length (Store.list store ()) = 2);
    let corrupt = Store.put store ~owner ~name:"corrupt" ~mime_type:"text/plain" "valid" in
    let data = Filename.concat dir (corrupt.id ^ ".data") in
    write_file data "wrong";
    rejected (fun () -> Store.read store ~owner ~id:corrupt.id);
    let metadata_item = Store.put store ~owner ~name:"metadata" ~mime_type:"text/plain" "ok" in
    let metadata = Filename.concat dir (metadata_item.id ^ ".json") in
    write_file metadata "{not json";
    assert (not (List.exists (fun (entry : Store.item) ->
      entry.id = metadata_item.id) (Store.list store ())));
    rejected (fun () -> Store.read store ~owner ~id:metadata_item.id);
    let unsafe = Store.put store ~owner ~name:"unsafe" ~mime_type:"text/plain" "safe" in
    let unsafe_path = Filename.concat dir (unsafe.id ^ ".data") in
    let linked = Filename.concat dir "hardlink-fixture" in
    Unix.link unsafe_path linked;
    rejected (fun () -> Store.read store ~owner ~id:unsafe.id);
    Unix.unlink linked;
    Unix.unlink unsafe_path;
    Unix.symlink (Filename.concat dir (item.id ^ ".data")) unsafe_path;
    rejected (fun () -> Store.read store ~owner ~id:unsafe.id);
    Unix.unlink unsafe_path;
    let max_data = String.make Store.max_artifact_bytes 'x' in
    let largest = Store.put store ~owner ~name:"largest" ~mime_type:"application/octet-stream" max_data in
    assert (largest.size = Store.max_artifact_bytes);
    let overflow = Store.begin_write store ~owner ~name:"overflow" ~mime_type:"text/plain" in
    Store.write overflow "x";
    rejected (fun () -> Store.write overflow max_data);
    Store.abort overflow;
    (* Eight maximum-sized artifacts fill the aggregate quota. *)
    let quota_root = child base "quota-workspace" in
    Unix.mkdir quota_root 0o700;
    let quota_store = Store.open_for_workspace ~root:quota_root in
    for index = 1 to 8 do
      ignore (Store.put quota_store ~owner ~name:(string_of_int index)
        ~mime_type:"application/octet-stream" max_data)
    done;
    rejected (fun () -> Store.put quota_store ~owner ~name:"over-quota"
      ~mime_type:"text/plain" "x");
    let count_root = child base "count-workspace" in
    Unix.mkdir count_root 0o700;
    let count_store = Store.open_for_workspace ~root:count_root in
    for index = 1 to Store.max_artifacts do
      ignore (Store.put count_store ~owner ~name:(string_of_int index)
        ~mime_type:"text/plain" "")
    done;
    rejected (fun () -> Store.put count_store ~owner ~name:"too-many"
      ~mime_type:"text/plain" "");
    print_endline "private session artifact store: ok")
