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

    (* get re-reads: a file replaced after scan still returns content. *)
    write (Filename.concat dir "zeta.md") "Zeta updated\n";
    (match Project_memory.get mem ~name:"zeta" with
     | Ok text -> check (text = "Zeta updated\n")
       "get must re-read current file contents"
     | Error _ -> failwith "get after in-place rewrite must succeed"))
