module Workspace_reader = Pave.Workspace_reader
module Workspace_process = Pave.Workspace_process
module Workspace_path = Pave.Workspace_path

let fail label = failwith ("workspace reader: " ^ label)
let expect label condition = if not condition then fail label

let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec search index =
    index + m <= n &&
    (String.sub text index m = fragment || search (index + 1))
  in
  search 0

let read ?cancel ?read_artifact ~root path =
  Workspace_reader.read ?cancel ?read_artifact ~root ~path ()

let expect_error label fragment fn =
  match fn () with
  | _ -> fail (label ^ " was accepted")
  | exception Workspace_reader.Error message ->
      expect (label ^ " error message") (contains message fragment);
      message
  | exception error -> fail (label ^ " raised the wrong exception: " ^ Printexc.to_string error)

let write path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () -> output_string channel contents)

let rec remove_tree path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Sys.readdir path |> Array.iter (fun name -> remove_tree (Filename.concat path name));
        Unix.rmdir path
    | _ -> Unix.unlink path
  with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let run program arguments =
  let result = Workspace_process.run ~timeout_seconds:15 ~output_limit:65_536
      ~program ~arguments () in
  match result.Workspace_process.termination with
  | Workspace_process.Exited 0 -> result.Workspace_process.output
  | _ -> fail ("fixture command failed: " ^ program ^ " " ^ String.concat " " arguments ^
               " (" ^ result.Workspace_process.output ^ ")")

let () =
  let root = Filename.temp_file "pave-workspace-reader-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let root = Unix.realpath root in
  let outside = Filename.temp_file "pave-workspace-reader-outside-" ".txt" in
  write outside "outside secret\n";
  Fun.protect ~finally:(fun () -> remove_tree root; (try Sys.remove outside with _ -> ())) (fun () ->
    let create name content = write (Filename.concat root name) content in
    create "lines.txt" "one\ntwo\nthree\nfour\n";
    expect "single-line selector" (read ~root "lines.txt:2" = "two\n");
    expect "range selector" (read ~root "lines.txt:2-3" = "two\nthree\n");
    expect "tail selector" (read ~root "lines.txt:-2" = "three\nfour\n");
    expect "raw selector" (read ~root "lines.txt:raw" = "one\ntwo\nthree\nfour\n");
    expect "multiple ranges" (read ~root "lines.txt:1-1,4-4" = "one\nfour\n");
    create "literal.txt:2" "literal filename wins\n";
    expect "literal file selector precedence" (read ~root "literal.txt:2" = "literal filename wins\n");
    expect "local URI" (read ~root "local://lines.txt:2" = "two\n");
    expect "directory listing" (contains (read ~root ".") "lines.txt");

    Unix.symlink outside (Filename.concat root "escape.txt");
    ignore (expect_error "parent traversal" "workspace-relative" (fun () -> read ~root "../outside.txt"));
    ignore (expect_error "symlink escape" "escapes workspace" (fun () -> read ~root "escape.txt"));

    let marker = Filename.concat root "notebook-executed" in
    let code = Printf.sprintf "open(%S, 'w').write('executed')\n" marker in
    let notebook = Yojson.Safe.to_string (`Assoc [
      "cells", `List [
        `Assoc ["cell_type", `String "code"; "source", `List [`String code];
          "outputs", `List [`Assoc ["text", `List [`String "DO_NOT_RETURN"]]]];
        `Assoc ["cell_type", `String "markdown"; "source", `String "# Notes"]];
      "metadata", `Assoc []]) in
    create "sample.ipynb" notebook;
    let converted = read ~root "sample.ipynb" in
    expect "notebook code cell" (contains converted "open(");
    expect "notebook markdown cell" (contains converted "# Notes");
    expect "notebook output omitted" (not (contains converted "DO_NOT_RETURN"));
    expect "notebook code not executed" (not (Sys.file_exists marker));
    expect "raw notebook selector" (read ~root "sample.ipynb:raw" = notebook);

    create "member.txt" "archive first\narchive second\n";
    let archive = Filename.concat root "sample.tar" in
    ignore (run "/usr/bin/tar" ["-cf"; archive; "-C"; root; "member.txt"]);
    expect "archive listing" (contains (read ~root "sample.tar") "member.txt");
    expect "archive member" (read ~root "sample.tar:member.txt" = "archive first\narchive second\n");
    expect "archive member selector" (read ~root "sample.tar:member.txt:2" = "archive second\n");
    ignore (expect_error "unsafe archive member" "without '..'" (fun () -> read ~root "sample.tar:../outside.txt"));

    let db = Filename.concat root "items.sqlite" in
    let inserts = List.init 205 (fun index ->
      Printf.sprintf "INSERT INTO items(value) VALUES('row-%d')" index) in
    let fixture_sql = "CREATE TABLE items(id INTEGER PRIMARY KEY, value TEXT);" ^
      String.concat ";" inserts ^ ";" in
    let setup =
      try Some (Workspace_process.run ~timeout_seconds:5 ~output_limit:65_536
                  ~program:"/usr/bin/sqlite3" ~arguments:["-batch"; db; fixture_sql] ())
      with _ -> None in
    let sqlite_ready = match setup with
      | Some result ->
          (match result.Workspace_process.termination with
           | Workspace_process.Exited 0 -> true
           | Workspace_process.Exited 127 -> false
           | _ -> fail ("SQLite fixture creation failed: " ^ result.Workspace_process.output))
      | None -> false in
    let sqlite_arguments =
      ["-batch"; "-bail"; "-readonly"; "-nofollow"; "-safe"; "-json";
       "-cmd"; ".limit length 4096"; "-cmd"; ".limit column 64";
       "-cmd"; ".limit sql_length 8192"; "-cmd"; "PRAGMA temp_store=MEMORY";
       "-init"; "/dev/null"; db; "SELECT 1"] in
    let sqlite_supported =
      sqlite_ready &&
      (try
         match (Workspace_process.run ~timeout_seconds:5 ~output_limit:65_536
                  ~program:"/usr/bin/sqlite3" ~arguments:sqlite_arguments ()).Workspace_process.termination with
         | Workspace_process.Exited 0 -> true
         | _ -> false
       with _ -> false) in
    if sqlite_supported then (
      let before = Workspace_path.read_bounded db 65_536 in
      let schema = read ~root "items.sqlite:schema" in
      expect "SQLite schema" (contains schema "items");
      let table = read ~root "items.sqlite:table:items" in
      expect "SQLite table rows" (contains table "row-0");
      expect "SQLite table last bounded row" (contains table "row-199");
      expect "SQLite row cap" (contains table "rows truncated at 200");
      let query = read ~root "items.sqlite:SELECT value FROM items WHERE id = 2" in
      expect "SQLite SELECT" (contains query "row-1");
      ignore (expect_error "SQLite write statement" "single SELECT" (fun () -> read ~root "items.sqlite:UPDATE items SET value='bad'"));
      ignore (expect_error "SQLite multiple statements" "multiple statements" (fun () -> read ~root "items.sqlite:SELECT 1; DROP TABLE items"));
      let forbidden = Filename.temp_file "pave-sqlite-read-only-" "" in
      Sys.remove forbidden;
      let escaped = "items.sqlite:SELECT writefile('" ^ forbidden ^ "','bad')" in
      ignore (expect_error "SQLite file-writing function" "unsafe writefile" (fun () -> read ~root escaped));
      expect "SQLite writer function did not write" (not (Sys.file_exists forbidden));
      expect "SQLite database stayed unchanged" (Workspace_path.read_bounded db 65_536 = before))
    else if sqlite_ready then
      ignore (expect_error "SQLite helper unavailable or unsafe" "helper" (fun () -> read ~root "items.sqlite:schema"));

    create "tail.txt" (String.make 65_537 'x' ^ "\nlast\n");
    expect "tail ignores oversized earlier lines" (read ~root "tail.txt:-1" = "last\n");
    ignore (expect_error "tail rejects oversized selected line" "output limit"
      (fun () -> read ~root "tail.txt:-2"));

    let large = String.make 65_537 'x' in
    create "large.txt" large;
    ignore (expect_error "output bound" "output limit" (fun () -> read ~root "large.txt"));
    ignore (expect_error "raw input bound" "exceeds 65536-byte limit" (fun () -> read ~root "large.txt:raw"));

    ignore (expect_error "HTTP URL policy" "HTTPS" (fun () -> read ~root "http://example.invalid/file.txt"));
    ignore (expect_error "URL credentials" "credentials" (fun () -> read ~root "https://user:secret@example.invalid/file.txt"));
    ignore (expect_error "loopback URL" "private, local" (fun () -> read ~root "https://127.0.0.1/file.txt"));
    ignore (expect_error "private URL" "private, local" (fun () -> read ~root "https://10.1.2.3/file.txt"));
    ignore (expect_error "IPv6 loopback URL" "private, local" (fun () -> read ~root "https://[::1]/file.txt"));
    ignore (expect_error "nonstandard HTTPS port" "port 443" (fun () -> read ~root "https://example.invalid:8443/file.txt"));
    ignore (expect_error "unsupported internal URI" "unsupported workspace URI" (fun () -> read ~root "agent://session/file"));

    expect "owner-scoped artifact" (read ~root ~read_artifact:(fun id -> if id = "owned" then Some "alpha\nbeta\n" else None) "artifact://owned:2" = "beta\n");
    ignore (expect_error "unowned artifact" "not owned" (fun () -> read ~root ~read_artifact:(fun _ -> None) "artifact://foreign"));
    ignore (expect_error "artifact callback required" "callback" (fun () -> read ~root "artifact://owned"));
    ignore (expect_error "pre-cancelled read" "cancelled" (fun () -> read ~cancel:(fun () -> true) ~root "lines.txt"));
    let checks = ref 0 in
    let cancel_during_read () = incr checks; !checks >= 3 in
    ignore (expect_error "mid-read cancellation" "cancelled" (fun () -> read ~cancel:cancel_during_read ~root "lines.txt"));

    print_endline "workspace reader selective reads and safety: ok")
