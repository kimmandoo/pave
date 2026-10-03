open Pave

let mkdir path = Unix.mkdir path 0o700
let write path text =
  let out = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out out) (fun () -> output_string out text)
let rec remove path =
  let stat = Unix.lstat path in
  if stat.Unix.st_kind = Unix.S_DIR then (
    Array.iter (fun child -> remove (Filename.concat path child)) (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path
let check condition message = if not condition then failwith message
let codes diagnostics = List.map fst diagnostics
let has_code code diagnostics = List.exists (fun (c, _) -> c = code) diagnostics
let memory_dir root = Filename.concat (Filename.concat root ".pave") "memory"

let () =
  let root = Filename.temp_file "pave-project-memory-" "" in
  Sys.remove root;
  mkdir root;
  Fun.protect ~finally:(fun () -> remove root) (fun () ->
    (* Missing .pave/memory is a normal empty state, not an error. *)
    let mem, diags = Project_memory.scan ~root in
    check (Project_memory.entries mem = [] && diags = [])
      "missing memory dir must scan empty with no diagnostics";
    let pave = Filename.concat root ".pave" in
    let dir = memory_dir root in
    mkdir pave; mkdir dir;

    (* Valid entries: summaries come from the first nonblank line. *)
    write (Filename.concat dir "alpha.md")
      "\n\n  Alpha summary line  \nrest of the body\n";
    write (Filename.concat dir "zeta.md") "Zeta first line\nbody";
    let long_line = String.make 200 's' in
    write (Filename.concat dir "beta.md") (long_line ^ "\nbody");

    (* Invalid entries, each skipped with a diagnostic. *)
    write (Filename.concat dir "Big.md") "uppercase name";
    write (Filename.concat dir "notes.txt") "not a markdown name";
    write (Filename.concat dir ".hidden.md") "hidden name";
    write (Filename.concat dir "huge.md") (String.make (33 * 1024) 'x');
    write (Filename.concat dir "binary.md") ("ok\n" ^ String.make 1 '\x01');
    Unix.mkdir (Filename.concat dir "subdir.md") 0o700;
    Unix.symlink "alpha.md" (Filename.concat dir "link.md");

    let mem, diags = Project_memory.scan ~root in
    let names = List.map (fun (e : Project_memory.entry) -> e.name)
      (Project_memory.entries mem) in
    check (names = ["alpha"; "beta"; "zeta"])
      "only valid .md entries must be indexed";
    let by_name name =
      List.find (fun (e : Project_memory.entry) -> e.name = name)
        (Project_memory.entries mem) in
    check ((by_name "alpha").summary = "Alpha summary line")
      "summary must be the first nonblank line, trimmed";
    check (String.length (by_name "beta").summary = 96)
      "summary must be truncated to 96 bytes";
    check ((by_name "zeta").bytes = String.length "Zeta first line\nbody")
      "entry bytes must record file size";
    check (String.length (Filename.concat dir "alpha.md") =
           String.length (by_name "alpha").path &&
           Filename.basename (by_name "alpha").path = "alpha.md")
      "entry path must point at the memory file";
    check (has_code "invalid_name" diags && has_code "file_limit" diags &&
           has_code "invalid_text" diags && has_code "unsafe_path" diags)
      ("skipped files must report diagnostics, got: " ^
       String.concat "," (codes diags));
    check (List.for_all (fun (n : string) -> n <> "link" && n <> "huge" &&
             n <> "binary" && n <> "subdir") names)
      "skipped files must not appear in entries";

    (* get returns full text, errors on unknown names. *)
    (match Project_memory.get mem ~name:"zeta" with
     | Ok text -> check (text = "Zeta first line\nbody") "get must return full text"
     | Error _ -> failwith "get on a scanned entry must succeed");
    (match Project_memory.get mem ~name:"missing" with
     | Error _ -> ()
     | Ok _ -> failwith "get on a missing name must error");

    (* index_text: sorted "name — summary" lines, bounded. *)
    let index = Project_memory.index_text mem in
    check (index = "alpha — Alpha summary line\nbeta — " ^
           String.make 96 's' ^ "\nzeta — Zeta first line\n")
      ("index_text must list sorted name/summary lines, got: " ^ index);

    (* Unsafe directories fail closed. *)
    let symlink_root = Filename.concat root "linkroot" in
    Unix.symlink root symlink_root;
    let mem2, diags2 = Project_memory.scan ~root:symlink_root in
    check (Project_memory.entries mem2 = [] && has_code "unsafe_path" diags2)
      "symlinked root must fail closed";
    Unix.unlink symlink_root;
    let fake = Filename.concat root "fake" in
    mkdir fake;
    write (Filename.concat fake ".pave") "not a directory";
    let mem3, diags3 = Project_memory.scan ~root:fake in
    check (Project_memory.entries mem3 = [] && has_code "unsafe_path" diags3)
      "non-directory .pave must fail closed";

    (* index_text bound: 64 maximal entries stay under 8 KiB. *)
    let big = Filename.concat root "big" in
    let big_mem = memory_dir big in
    mkdir big; mkdir (Filename.concat big ".pave"); mkdir big_mem;
    for i = 0 to 69 do
      write (Filename.concat big_mem (Printf.sprintf "m%047d.md" i))
        (String.make 96 's' ^ "\nbody")
    done;
    let mem4, diags4 = Project_memory.scan ~root:big in
    check (List.length (Project_memory.entries mem4) = 64)
      "entry cap must accept the first 64 valid files";
    check (has_code "entry_limit" diags4)
      "extra valid files must report entry_limit";
    let index4 = Project_memory.index_text mem4 in
    check (String.length index4 <= 8 * 1024)
      "index_text must stay within 8 KiB";
    let index_lines = String.split_on_char '\n' index4 in
    check (List.for_all (fun (item : Project_memory.entry) ->
      List.exists (fun line -> line = item.name ||
        String.starts_with ~prefix:(item.name ^ " — ") line) index_lines)
      (Project_memory.entries mem4))
      "bounded memory index must not silently drop valid names";
    List.iter (fun (item : Project_memory.entry) ->
      write item.path (String.concat "" (List.init 32 (fun _ -> "가"))))
      (Project_memory.entries mem4);
    let unicode_memory, _ = Project_memory.scan ~root:big in
    let unicode_index = Project_memory.index_text unicode_memory in
    check (String.length unicode_index <= Project_memory.max_index_bytes &&
           Local_content.valid_utf8 unicode_index)
      "summary budgeting must keep the complete index within its UTF-8 byte bound";

    (* get re-reads: a file replaced after scan still returns content. *)
    write (Filename.concat dir "zeta.md") "Zeta updated\n";
    (match Project_memory.get mem ~name:"zeta" with
     | Ok text -> check (text = "Zeta updated\n")
       "get must re-read current file contents"
     | Error _ -> failwith "get after in-place rewrite must succeed"))

let () =
  let root = Filename.temp_file "pave-memory-writes-" "" in
  Sys.remove root; mkdir root;
  let reject action =
    match action () with
    | exception Workspace_memory.Error _ -> ()
    | _ -> failwith "unsafe or over-quota memory write was accepted" in
  Fun.protect ~finally:(fun () -> remove root) (fun () ->
    let workspace = Filename.concat root "workspace" in
    mkdir workspace;
    let path = Workspace_memory.put ~root:workspace ~name:"first" "private text\n" in
    check ((Unix.stat path).Unix.st_perm land 0o777 = 0o600)
      "memory publication must be private";
    let previous_umask = Unix.umask 0o600 in
    Fun.protect ~finally:(fun () -> ignore (Unix.umask previous_umask)) (fun () ->
      ignore (Workspace_memory.put ~root:workspace ~name:"first" "private text\n"));
    check ((Unix.stat path).Unix.st_perm land 0o777 = 0o600)
      "memory staging must retain usable private permissions under a restrictive umask";
    for i = 1 to Project_memory.max_entries - 1 do
      ignore (Workspace_memory.put ~root:workspace
        ~name:(Printf.sprintf "entry_%d" i) "entry\n")
    done;
    reject (fun () -> Workspace_memory.put ~root:workspace ~name:"overflow" "too many");
    check (not (Sys.file_exists (Filename.concat (memory_dir workspace) "overflow.md")))
      "over-quota memory put must not publish an unindexed file";
    ignore (Workspace_memory.put ~root:workspace ~name:"first" "replacement\n");
    check (Workspace_memory.forget ~root:workspace ~name:"entry_1")
      "forget must free a memory slot";
    ignore (Workspace_memory.put ~root:workspace ~name:"replacement" "new slot\n");

    let linkroot = Filename.concat root "linked-workspace" in
    Unix.symlink workspace linkroot;
    reject (fun () -> Workspace_memory.put ~root:linkroot ~name:"first" "escaped write");
    reject (fun () -> Workspace_memory.forget ~root:linkroot ~name:"first");
    let memory, _ = Project_memory.scan ~root:workspace in
    check (Project_memory.get memory ~name:"first" = Ok "replacement\n")
      "symlinked workspace mutation must preserve real memory";
    Unix.unlink linkroot;
    let external_path = Filename.concat root "external.md" in
    write external_path "external data";
    let linked = Filename.concat (memory_dir workspace) "linked.md" in
    Unix.symlink external_path linked;
    reject (fun () -> Workspace_memory.put ~root:workspace ~name:"linked" "changed");
    reject (fun () -> Workspace_memory.forget ~root:workspace ~name:"linked");
    check ((Unix.lstat linked).Unix.st_kind = Unix.S_LNK)
      "rejected symlink update must preserve the link";
    let ic = open_in_bin external_path in
    let text = Fun.protect ~finally:(fun () -> close_in ic)
        (fun () -> really_input_string ic (in_channel_length ic)) in
    check (text = "external data") "memory tools must not modify external data";
    Unix.chmod (memory_dir workspace) 0o777;
    reject (fun () -> Workspace_memory.put ~root:workspace ~name:"first" "unsafe directory");
    Unix.chmod (memory_dir workspace) 0o700;
    Unix.chmod workspace 0o777;
    reject (fun () -> Workspace_memory.put ~root:workspace ~name:"first" "unsafe root");
    Unix.chmod workspace 0o700;

    (* The last quota slot is serialized across processes, not only threads. *)
    let concurrent = Filename.concat root "concurrent" in
    mkdir concurrent;
    for i = 1 to Project_memory.max_entries - 1 do
      ignore (Workspace_memory.put ~root:concurrent
        ~name:(Printf.sprintf "entry_%d" i) "entry")
    done;
    let read_fd, write_fd = Unix.pipe () in
    let child name =
      match Unix.fork () with
      | 0 ->
          Unix.close write_fd;
          let byte = Bytes.create 1 in
          ignore (Unix.read read_fd byte 0 1);
          Unix.close read_fd;
          (match Workspace_memory.put ~root:concurrent ~name "racing final slot" with
           | _ -> Unix._exit 0
           | exception Workspace_memory.Error _ -> Unix._exit 2
           | exception _ -> Unix._exit 3)
      | pid -> pid in
    let one = child "racer_one" and two = child "racer_two" in
    Unix.close read_fd;
    ignore (Unix.write_substring write_fd "xx" 0 2);
    Unix.close write_fd;
    let statuses = List.map (fun pid -> snd (Unix.waitpid [] pid)) [one; two] in
    check (List.sort compare statuses = [Unix.WEXITED 0; Unix.WEXITED 2])
      "exactly one concurrent writer must acquire the final memory slot";
    let memory, diagnostics = Project_memory.scan ~root:concurrent in
    check (List.length (Project_memory.entries memory) = Project_memory.max_entries &&
           not (has_code "entry_limit" diagnostics))
      "concurrent memory updates must not exceed the reader's entry cap")
