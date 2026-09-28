let fail label = failwith ("file mentions: " ^ label)
let expect label condition = if not condition then fail label

let write path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let rec remove path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let rejected label f =
  match f () with
  | _ -> fail (label ^ " was accepted")
  | exception Invalid_argument _ | exception Pave.Workspace_path.Error _
  | exception Unix.Unix_error _ -> ()

let contains text part =
  let part_length = String.length part in
  let rec search index =
    index + part_length <= String.length text &&
    (String.sub text index part_length = part || search (index + 1)) in
  search 0

let () =
  let base = Filename.temp_file "pave-file-mentions-" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  Fun.protect ~finally:(fun () -> remove base) (fun () ->
    let root = Filename.concat base "workspace" in
    let outside = Filename.concat base "outside" in
    Unix.mkdir root 0o700;
    Unix.mkdir outside 0o700;
    Unix.mkdir (Filename.concat root "src") 0o700;
    Unix.mkdir (Filename.concat root "ignored-dir") 0o700;
    write (Filename.concat root ".gitignore") "ignored.txt\nignored-dir/\n";
    write (Filename.concat root "src/notes.md") "hello 世界\n";
    write (Filename.concat root "src/notes extra.txt") "quoted text";
    write (Filename.concat root "ignored.txt") "must not complete";
    write (Filename.concat root "ignored-dir/hidden.txt") "must not complete";
    let text = Pave.File_mentions.expand ~root
      "Summarize @src/notes.md please" in
    expect "text mention label" (contains text.prompt
      "[Attached text file: src/notes.md]");
    expect "Unicode text content" (contains text.prompt "hello 世界\n");
    expect "prompt suffix retained" (String.ends_with ~suffix:" please" text.prompt);
    expect "text file is not a media attachment" (text.attachments = []);
    let sentence = Pave.File_mentions.expand ~root
      "Summarize @src/notes.md." in
    expect "sentence-final mention attaches without consuming punctuation"
      (contains sentence.prompt "[Attached text file: src/notes.md]" &&
       String.ends_with ~suffix:"]." sentence.prompt);
    let quoted = Pave.File_mentions.expand ~root
      "Read @\"src/notes extra.txt\" now" in
    expect "quoted path content" (contains quoted.prompt "quoted text");
    expect "quoted path label" (contains quoted.prompt
      "[Attached text file: src/notes extra.txt]");
    List.iter (fun (name, contents) ->
      let path = "src/" ^ name in
      write (Filename.concat root path) contents;
      let rendered = Pave.File_mentions.render_reference ~directory:false path in
      (match Pave.File_mentions.references rendered with
      | [reference] ->
          expect ("completed mention retains filename " ^ name)
            (reference.path = path && reference.stop = String.length rendered)
      | _ -> fail ("completed mention cannot be parsed: " ^ rendered));
      expect ("completed mention attaches filename " ^ name)
        (contains (Pave.File_mentions.expand ~root rendered).prompt contents))
      ["a,b.txt", "file containing comma";
       "terminal.", "file ending in period";
       "single'quote.txt", "file containing apostrophe";
       "double\"quote.txt", "file containing quote"];
    expect "unterminated quoted path cannot consume a later line's quote"
      (Pave.File_mentions.references "Read @\"src/notes.md\n\" later" = []);
    let prose = "literal @person@host and @missing/path; code `@src/notes.md`" in
    let unchanged = Pave.File_mentions.expand ~root prose in
    expect "email and unresolved mentions stay prose" (unchanged.prompt = prose);
    expect "code span stays prose"
      (Pave.File_mentions.references "`@src/notes.md`" = []);
    let multiline_code = "Literal `first line\n@src/notes.md` outside" in
    expect "multiline inline code does not attach its mention"
      (Pave.File_mentions.references multiline_code = [] &&
       (Pave.File_mentions.expand ~root multiline_code).prompt = multiline_code);
    expect "multiline inline code mention is not completable"
      (Pave.File_mentions.completion_context multiline_code
         (String.length "Literal `first line\n@src/no") = None);
    expect "email is not a file reference"
      (Pave.File_mentions.references "person@host" = []);
    let tilde_fence = "~~~text\n@src/notes.md\n~~~" in
    expect "tilde code fence stays prose"
      ((Pave.File_mentions.expand ~root tilde_fence).prompt = tilde_fence &&
       Pave.File_mentions.references tilde_fence = []);
    expect "cursor inside tilde fence is not completable"
      (Pave.File_mentions.completion_context "~~~\n@src/no\n~~~" 10 = None);
    let invalid_name = "invalid-\255.txt" in
    let invalid_name_created =
      try write (Filename.concat root invalid_name) "not a completion candidate";
        true
      with Sys_error message when
        String.ends_with ~suffix:"Illegal byte sequence" message -> false in
    if invalid_name_created then
      expect "invalid UTF-8 filesystem name is filtered"
        (not (List.exists (fun item ->
          item.Pave.File_mentions.path = invalid_name)
          (Pave.File_mentions.complete_paths ~root "invalid-").candidates));

    let png = "\137PNG\r\n\026\n" in
    write (Filename.concat root "src/photo.png") png;
    let media = Pave.File_mentions.expand ~root
      "Compare @src/photo.png and @src/photo.png" in
    expect "media mention stays readable in prompt"
      (media.prompt = "Compare @src/photo.png and @src/photo.png");
    expect "repeated media mention is deduplicated"
      (List.length media.attachments = 1 &&
       media.attachment_names = ["photo.png"]);
    expect "media MIME is retained"
      ((List.hd media.attachments).Pave.Protocol.mime_type = "image/png");

    let missing = Pave.File_mentions.expand ~root "Use @src/not-yet-created.txt" in
    expect "missing safe reference stays prose"
      (missing.prompt = "Use @src/not-yet-created.txt");
    write (Filename.concat outside "secret.txt") "secret";
    rejected "parent traversal" (fun () ->
      Pave.File_mentions.expand ~root "Read @../outside/secret.txt");
    Unix.symlink (Filename.concat outside "secret.txt")
      (Filename.concat root "src/escape.txt");
    rejected "escaping symlink" (fun () ->
      Pave.File_mentions.expand ~root "Read @src/escape.txt");
    write (Filename.concat root "src/wrong.png") "not a PNG";
    rejected "bad media signature" (fun () ->
      Pave.File_mentions.expand ~root "Read @src/wrong.png");
    write (Filename.concat root "src/binary.txt") "\000data";
    rejected "NUL binary text" (fun () ->
      Pave.File_mentions.expand ~root "Read @src/binary.txt");
    write (Filename.concat root "src/invalid.txt") "\255";
    rejected "invalid UTF-8" (fun () ->
      Pave.File_mentions.expand ~root "Read @src/invalid.txt");
    let limit = Pave.Workspace_path.max_read_bytes in
    write (Filename.concat root "src/exact.txt") (String.make limit 'x');
    expect "text size boundary accepted"
      (contains (Pave.File_mentions.expand ~root "@src/exact.txt").prompt
        (String.make 128 'x'));
    write (Filename.concat root "src/oversized.txt") (String.make (limit + 1) 'x');
    rejected "oversized text" (fun () ->
      Pave.File_mentions.expand ~root "@src/oversized.txt");
    rejected "aggregate expanded text limit" (fun () ->
      Pave.File_mentions.expand ~root
        (String.concat " " (List.init 17 (fun _ -> "@src/exact.txt"))));
    rejected "reference count limit" (fun () ->
      Pave.File_mentions.expand ~root (String.concat " "
        (List.init (Pave.File_mentions.max_references + 1)
          (fun index -> "@missing-" ^ string_of_int index))));

    (match Pave.File_mentions.completion_context
        "Review @src/notes.md after" (String.length "Review @src/no") with
     | Some context ->
         expect "cursor-local prefix" (context.prefix = "src/no");
         expect "cursor-local token bounds"
           (context.start = String.length "Review " &&
            context.stop = String.length "Review @src/notes.md")
     | None -> fail "cursor in an @ token was not recognized");
    (match Pave.File_mentions.completion_context "Read (@src/no"
        (String.length "Read (@src/no") with
     | Some context ->
         expect "parenthesized reference completion" (context.prefix = "src/no")
     | None -> fail "parenthesized @ token was not recognized");
    (match Pave.File_mentions.completion_context "Read,@src/no"
        (String.length "Read,@src/no") with
     | Some context ->
         expect "comma-separated reference completion" (context.prefix = "src/no")
     | None -> fail "comma-separated @ token was not recognized");
    (match Pave.File_mentions.completion_context "Read @\"src/notes extra"
        (String.length "Read @\"src/notes") with
     | Some context -> expect "quoted path prefix" (context.prefix = "src/notes")
     | None -> fail "quoted @ token was not recognized");
    expect "code token is not completable"
      (Pave.File_mentions.completion_context "`@src/no`" 7 = None);
    (match Pave.File_mentions.completion_context
        "Read @\"src/no\n\" later" (String.length "Read @\"src/no") with
     | Some context ->
         expect "unfinished quoted completion stays on its line"
           (context.prefix = "src/no" &&
            context.stop = String.length "Read @\"src/no")
     | None -> fail "unfinished quoted completion disappeared");
    (match Pave.File_mentions.completion_context
        "`unclosed @src/no" (String.length "`unclosed @src/no") with
     | Some context ->
         expect "unmatched backtick does not hide ordinary mention"
           (context.prefix = "src/no")
     | None -> fail "unmatched backtick hid ordinary mention");
    (match Pave.File_mentions.completion_context
        "Read @\"missing\n@src/no" (String.length "Read @\"missing\n@src/no") with
     | Some context ->
         expect "unterminated quote does not hide next line's mention"
           (context.prefix = "src/no")
     | None -> fail "unterminated quote hid next line's mention");
    let listing = Pave.File_mentions.complete_paths ~root "src/no" in
    expect "completion finds exact workspace paths"
      (List.exists (fun item -> item.Pave.File_mentions.path = "src/notes.md")
        listing.candidates);
    expect "selectable text exposes its bounded preview"
      (List.exists (fun item ->
        item.Pave.File_mentions.path = "src/notes.md" &&
        item.preview = Some ("text/plain", String.length "hello 世界\n"))
        listing.candidates);
    let images = Pave.File_mentions.complete_paths ~root "src/pho" in
    expect "selectable image exposes verified MIME and byte count"
      (List.exists (fun item ->
        item.Pave.File_mentions.path = "src/photo.png" &&
        item.preview = Some ("image/png", String.length png))
        images.candidates);
    let fuzzy = Pave.File_mentions.suggest_paths ~root "pho" in
    expect "typing @pho surfaces nested attachable image without Tab"
      (List.exists (fun item ->
        item.Pave.File_mentions.path = "src/photo.png" &&
        item.preview = Some ("image/png", String.length png))
        fuzzy.candidates);
    expect "invalid media stays absent from fuzzy suggestions"
      (not (List.exists (fun item ->
        item.Pave.File_mentions.path = "src/wrong.png")
        (Pave.File_mentions.suggest_paths ~root "wrong").candidates));

    List.iter (fun path ->
      expect ("unattachable file omitted from selector: " ^ path)
        (not (List.exists (fun item -> item.Pave.File_mentions.path = path)
          (Pave.File_mentions.complete_paths ~root
            (Filename.concat "src" (Filename.basename path))).candidates)))
      ["src/wrong.png"; "src/binary.txt"; "src/invalid.txt";
       "src/oversized.txt"; "src/escape.txt"];
    let control_path = "src/bad\nname.txt" in
    write (Filename.concat root control_path) "not a safe mention";
    expect "control-character filenames cannot enter the selector"
      (not (List.exists (fun item ->
        item.Pave.File_mentions.path = control_path)
        (Pave.File_mentions.complete_paths ~root "src/bad").candidates));

    expect "unfinished nonexistent directory has no completions"
      ((Pave.File_mentions.complete_paths ~root "src/not-yet/").candidates = []);
    expect "file cannot be completed as directory"
      ((Pave.File_mentions.complete_paths ~root "src/notes.md/").candidates = []);
    expect "completion obeys ignore rules"
      (not (List.exists (fun item ->
         String.starts_with ~prefix:"ignored" item.Pave.File_mentions.path)
        listing.candidates));
    expect "completion omits symlinks"
      (not (List.exists (fun item -> item.Pave.File_mentions.path = "src/escape.txt")
        listing.candidates));
    expect "directory completion is marked"
      (List.exists (fun item -> item.Pave.File_mentions.path = "src" &&
        item.is_directory)
        (Pave.File_mentions.complete_paths ~root "s").candidates);
    expect "root selector shows directories before file attachments"
      (match (Pave.File_mentions.suggest_paths ~root "").candidates with
       | first :: _ -> first.is_directory
       | [] -> false);

    expect "traversal completion returns no candidates"
      ((Pave.File_mentions.complete_paths ~root "../" ).candidates = []);
    expect "quoted completion closes path and escapes delimiter"
      (Pave.File_mentions.render_reference ~quote:'"' ~directory:false
        "src/a\"b.txt" = "@\"src/a\\\"b.txt\"");
    for index = 1 to 101 do
      write (Filename.concat root
        (Printf.sprintf "candidate-%03d.txt" index)) "x"
    done;
    let capped = Pave.File_mentions.complete_paths ~root "" in
    expect "candidate cap is surfaced"
      (List.length capped.candidates = Pave.File_mentions.max_candidates &&
       capped.truncated);
    print_endline "inline file mentions: ok")
