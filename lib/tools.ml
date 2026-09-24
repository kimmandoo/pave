
let max_read_bytes = 65_536
let max_write_bytes = 1_048_576
let max_command_bytes = 65_536
let max_walk_entries = 10_000
let max_search_bytes = 16_777_216
let max_matches = 100

exception Tool_error of string

let fail message = raise (Tool_error message)

let field name = function
  | `Assoc fields -> (match List.assoc_opt name fields with Some value -> value | None -> `Null)
  | _ -> fail "arguments must be a JSON object"

let required_string name args =
  match field name args with
  | `String value -> value
  | `Null -> fail ("missing required string argument: " ^ name)
  | _ -> fail (name ^ " must be a string")

let optional_string name default args =
  match field name args with
  | `Null -> default
  | `String value -> value
  | _ -> fail (name ^ " must be a string")

let optional_int name default ~minimum ~maximum args =
  match field name args with
  | `Null -> default
  | `Int n when n >= minimum && n <= maximum -> n
  | _ -> fail (Printf.sprintf "%s must be an integer between %d and %d" name minimum maximum)

let within root path =
  path = root ||
  (let prefix = if root = "/" then root else root ^ "/" in
   String.length path >= String.length prefix &&
   String.sub path 0 (String.length prefix) = prefix)

let root_path root =
  let root = Unix.realpath root in
  if (Unix.stat root).Unix.st_kind <> Unix.S_DIR then fail "workspace root is not a directory";
  root

let checked_path root relative =
  if relative = "" || String.contains relative '\000' || not (Filename.is_relative relative) ||
     List.exists (( = ) "..") (String.split_on_char '/' relative) then
    fail "path must be a nonempty workspace-relative path without '..'";
  let path = Filename.concat root relative in
  (* Checking the canonical parent also handles a new file, for which realpath
     on the final component cannot yet succeed. *)
  let parent = Unix.realpath (Filename.dirname path) in
  if not (within root parent) then fail ("path escapes workspace: " ^ relative);
  let path = Filename.concat parent (Filename.basename path) in
  (try
     let canonical = Unix.realpath path in
     if not (within root canonical) then fail ("path escapes workspace: " ^ relative)
   with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  path

let regular_path root relative =
  let path = checked_path root relative in
  if (Unix.stat path).Unix.st_kind <> Unix.S_REG then fail ("not a regular file: " ^ relative);
  path

let writable_path root relative =
  if relative = "." || Filename.basename relative = "." then fail "a file path is required";
  let path = checked_path root relative in
  (try
     let stat = Unix.lstat path in
     if stat.Unix.st_kind <> Unix.S_REG then fail ("not a regular file (or is a symlink): " ^ relative)
   with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  path

let with_fd path flags permissions fn =
  let fd = Unix.openfile path flags permissions in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> fn fd)

let read_bounded path limit =
  let size = (Unix.stat path).Unix.st_size in
  if size > limit then fail (Printf.sprintf "file exceeds %d-byte limit: %s" limit path);
  with_fd path [Unix.O_RDONLY] 0 (fun fd ->
    let buffer = Bytes.create 8192 in
    let result = Buffer.create (min size limit) in
    let rec loop () =
      let count = Unix.read fd buffer 0 (min 8192 (limit + 1 - Buffer.length result)) in
      if count <> 0 then (
        Buffer.add_subbytes result buffer 0 count;
        if Buffer.length result > limit then fail (Printf.sprintf "file exceeds %d-byte limit: %s" limit path);
        loop ())
    in
    loop ();
    Buffer.contents result)

let write_all fd text =
  let bytes = Bytes.unsafe_of_string text in
  let rec loop offset =
    if offset < Bytes.length bytes then (
      let written = Unix.write fd bytes offset (Bytes.length bytes - offset) in
      if written = 0 then fail "could not write file";
      loop (offset + written))
  in
  loop 0

let atomic_write path text =
  if String.length text > max_write_bytes then
    fail (Printf.sprintf "content exceeds %d-byte write limit" max_write_bytes);
  let mode = try (Unix.stat path).Unix.st_perm with Unix.Unix_error (Unix.ENOENT, _, _) -> 0o600 in
  let temp = Filename.temp_file ~temp_dir:(Filename.dirname path) ".pave-" ".tmp" in
  Fun.protect ~finally:(fun () -> try Unix.unlink temp with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
    (fun () ->
       with_fd temp [Unix.O_WRONLY] 0 (fun fd ->
         Unix.fchmod fd mode;
         write_all fd text;
         Unix.fsync fd);
       Unix.rename temp path)

let find_from text needle start =
  let text_len = String.length text and needle_len = String.length needle in
  let rec matches i j =
    j = needle_len || (text.[i + j] = needle.[j] && matches i (j + 1))
  in
  let rec scan i =
    if i + needle_len > text_len then None
    else if text.[i] = needle.[0] && matches i 0 then Some i
    else scan (i + 1)
  in
  scan start

let skip_directory = function
  | ".git" | ".hg" | ".svn" | "_build" | "build" | ".build" | "dist"
  | "node_modules" | "DerivedData" | ".gradle" | ".dart_tool" | "Pods" -> true
  | _ -> false

let walk root relative visit =
  if List.exists skip_directory (String.split_on_char '/' relative) then
    fail ("directory is excluded from listing/search: " ^ relative);
  let starting = checked_path root relative in
  if (Unix.stat starting).Unix.st_kind <> Unix.S_DIR then fail ("not a directory: " ^ relative);
  let entries = ref 0 in
  let truncated = ref false in
  let rec directory absolute prefix =
    let dir = Unix.opendir absolute in
    Fun.protect ~finally:(fun () -> Unix.closedir dir) (fun () ->
      let rec next () =
        if !entries >= max_walk_entries then truncated := true
        else
          match Unix.readdir dir with
          | exception End_of_file -> ()
          | "." | ".." -> next ()
          | name ->
              incr entries;
              let child = Filename.concat absolute name in
              let relative = if prefix = "" then name else prefix ^ "/" ^ name in
              let kind = (Unix.lstat child).Unix.st_kind in
              (match kind with
               | Unix.S_DIR when not (skip_directory name) -> directory child relative
               | Unix.S_REG -> visit relative child
               | _ -> ());
              next ()
      in next ())
  in
  directory starting (if relative = "." then "" else relative);
  !truncated

let append_bounded output text limit =
  if Buffer.length output + String.length text <= limit then (Buffer.add_string output text; true)
  else false

let list_files root args =
  let relative = optional_string "path" "." args in
  let output = Buffer.create 4096 in
  let count = ref 0 and overflow = ref false in
  let walk_limit = walk root relative (fun name _ ->
    if !count < 500 && not !overflow then
      if append_bounded output (name ^ "\n") max_read_bytes then incr count
      else overflow := true
    else overflow := true) in
  if walk_limit || !overflow then Buffer.add_string output "[truncated; narrow the path]\n";
  if !count = 0 && not (walk_limit || !overflow) then "No files found" else Buffer.contents output

let search root args =
  let query = required_string "pattern" args in
  if query = "" then fail "pattern must not be empty";
  let relative = optional_string "path" "." args in
  let output = Buffer.create 4096 in
  let matches = ref 0 and scanned = ref 0 and truncated = ref false in
  let walk_limit = walk root relative (fun name path ->
    let size = (Unix.stat path).Unix.st_size in
    if size > max_write_bytes then ()
    else if !scanned + size > max_search_bytes then truncated := true
    else if !matches >= max_matches then truncated := true
    else (
      scanned := !scanned + size;
      let contents = read_bounded path max_write_bytes in
      if not (String.contains contents '\000') then (
        let length = String.length contents in
        let rec lines start number =
          if start < length && !matches < max_matches && not !truncated then (
            let finish = try String.index_from contents start '\n' with Not_found -> length in
            let line = String.sub contents start (finish - start) in
            if find_from line query 0 <> None then (
              let preview = if String.length line > 240 then String.sub line 0 240 ^ "…" else line in
              if append_bounded output (Printf.sprintf "%s:%d:%s\n" name number preview) max_read_bytes then incr matches
              else truncated := true);
            lines (finish + 1) (number + 1))
        in lines 0 1))) in
  if walk_limit || !truncated then Buffer.add_string output "[truncated; narrow the path or query]\n";
  if !matches = 0 && not (walk_limit || !truncated) then "No matches found" else Buffer.contents output

let read_file root args =
  let relative = required_string "path" args in
  let path = regular_path root relative in
  let offset = optional_int "offset" 0 ~minimum:0 ~maximum:max_int args in
  let count = optional_int "max_bytes" 16_384 ~minimum:1 ~maximum:max_read_bytes args in
  with_fd path [Unix.O_RDONLY] 0 (fun fd ->
    let size = (Unix.fstat fd).Unix.st_size in
    if offset > size then fail (Printf.sprintf "offset %d exceeds file size %d" offset size);
    ignore (Unix.lseek fd offset Unix.SEEK_SET);
    let remaining = min count (size - offset) in
    let bytes = Bytes.create remaining in
    let rec read_at position =
      if position < remaining then (
        let n = Unix.read fd bytes position (remaining - position) in
        if n <> 0 then read_at (position + n) else position)
      else position
    in
    let actual = read_at 0 in
    let text = Bytes.sub_string bytes 0 actual in
    if String.contains text '\000' then fail "binary file; read_file supports text only";
    if offset + actual < size then
      Printf.sprintf "%s\n[truncated; next offset: %d; file size: %d]" text (offset + actual) size
    else text)

let write_file root args =
  let relative = required_string "path" args in
  let content = required_string "content" args in
  let path = writable_path root relative in
  atomic_write path content;
  Printf.sprintf "Wrote %d bytes to %s" (String.length content) relative

let edit_file root args =
  let relative = required_string "path" args in
  let old_text = required_string "old_string" args in
  let new_text = required_string "new_string" args in
  if old_text = "" then fail "old_string must not be empty";
  let path = writable_path root relative in
  let contents = read_bounded path max_write_bytes in
  let index = match find_from contents old_text 0 with
    | Some index -> index
    | None -> fail "old_string was not found; read_file to check the exact text" in
  (match find_from contents old_text (index + 1) with
   | Some _ -> fail "old_string matches more than once; provide a longer unique excerpt"
   | None -> ());
  let result = String.sub contents 0 index ^ new_text ^
    String.sub contents (index + String.length old_text)
      (String.length contents - index - String.length old_text) in
  atomic_write path result;
  Printf.sprintf "Edited %s" relative

let shell_quote text =
  "'" ^ String.concat "'\\''" (String.split_on_char '\'' text) ^ "'"

let mobile_project root =
  let manifest name =
    let path = Filename.concat root name in
    try (Unix.lstat path).Unix.st_kind = Unix.S_REG
    with Unix.Unix_error (Unix.ENOENT, _, _) -> false
  in
  let directory name =
    let path = Filename.concat root name in
    try (Unix.lstat path).Unix.st_kind = Unix.S_DIR
    with Unix.Unix_error (Unix.ENOENT, _, _) -> false
  in
  let xcode_items suffix =
    let dir = Unix.opendir root in
    Fun.protect ~finally:(fun () -> Unix.closedir dir) (fun () ->
      let rec collect visited found =
        if visited >= max_walk_entries || List.length found >= 20 then List.rev found
        else match Unix.readdir dir with
          | exception End_of_file -> List.rev found
          | name ->
              if Filename.check_suffix name suffix && directory name then
                collect (visited + 1) (name :: found)
              else collect (visited + 1) found
      in collect 0 [])
  in
  let output = Buffer.create 1024 in
  let add stack commands =
    Buffer.add_string output (stack ^ "\n");
    List.iter (fun command -> Buffer.add_string output ("  " ^ command ^ "\n")) commands
  in
  let workspaces = xcode_items ".xcworkspace" in
  let projects = xcode_items ".xcodeproj" in
  List.iter (fun name ->
    let quoted = shell_quote name in
    add ("Xcode workspace: " ^ name)
      ["xcodebuild -list -workspace " ^ quoted;
       "xcodebuild -workspace " ^ quoted ^ " -scheme '<scheme-from-list>' build";
       "xcodebuild -workspace " ^ quoted ^ " -scheme '<scheme-from-list>' test"])
    workspaces;
  List.iter (fun name ->
    let quoted = shell_quote name in
    add ("Xcode project: " ^ name)
      ["xcodebuild -list -project " ^ quoted;
       "xcodebuild -project " ^ quoted ^ " -scheme '<scheme-from-list>' build";
       "xcodebuild -project " ^ quoted ^ " -scheme '<scheme-from-list>' test"])
    projects;
  if manifest "Package.swift" then
    add "Swift Package Manager: Package.swift" ["swift build"; "swift test"];
  let android_root =
    if manifest "settings.gradle" || manifest "settings.gradle.kts" || manifest "gradlew" then Some ""
    else if directory "android" &&
            (manifest "android/settings.gradle" || manifest "android/settings.gradle.kts" ||
             manifest "android/gradlew")
    then Some "android/" else None
  in
  (match android_root with
   | None -> ()
   | Some prefix ->
       let gradle = if manifest (prefix ^ "gradlew") then "./gradlew" else "gradle" in
       let command action = (if prefix = "" then "" else "cd android && ") ^ gradle ^ " " ^ action in
       add ("Android Gradle: " ^ prefix ^ "settings.gradle[.kts] / gradlew")
         [command "tasks"; command "assembleDebug"; command "test"]);
  if manifest "pubspec.yaml" then (
    let flutter =
      try
        let pubspec = read_bounded (Filename.concat root "pubspec.yaml") max_write_bytes in
        Some (find_from pubspec "flutter:" 0 <> None)
      with Tool_error _ -> None
    in
    match flutter with
    | Some true ->
        add "Flutter: pubspec.yaml" ["flutter pub get"; "flutter test"; "flutter build apk"; "flutter build ios"]
    | Some false -> add "Dart: pubspec.yaml" ["dart pub get"; "dart test"]
    | None -> add "Dart/Flutter: pubspec.yaml (too large to inspect dependencies)"
        ["inspect pubspec.yaml to identify the SDK"]);
  if manifest "package.json" then (
    let package =
      try Some (Yojson.Basic.from_string (read_bounded (Filename.concat root "package.json") max_write_bytes))
      with Tool_error _ | Yojson.Json_error _ -> None
    in
    match package with
    | None -> Buffer.add_string output "package.json could not be parsed (or exceeds 1 MiB).\n"
    | Some (`Assoc _ as json) ->
        let has_dependency name =
          List.exists (fun section ->
            match field section json with
            | `Assoc entries -> List.mem_assoc name entries
            | _ -> false) ["dependencies"; "devDependencies"]
        in
        let scripts = field "scripts" json in
        let has_script name =
          match scripts with
          | `Assoc entries -> List.mem_assoc name entries
          | _ -> false
        in
        let script_commands =
          (if has_script "build" then ["npm run build"] else []) @
          (if has_script "test" then ["npm test"] else [])
        in
        if has_dependency "expo" then
          add "Expo: package.json" (["npx expo export"] @ script_commands)
        else if has_dependency "react-native" then
          add "React Native: package.json (use Xcode/Gradle above for native builds)"
            script_commands
    | Some _ -> Buffer.add_string output "package.json must contain a JSON object.\n");
  if Buffer.length output = 0 then
    "No supported mobile project manifests found at workspace root."
  else "Detected mobile project stacks and suggested commands (not executed):\n" ^ Buffer.contents output

let run_command root args =
  let command = required_string "command" args in
  if command = "" then fail "command must not be empty";
  let timeout = optional_int "timeout_seconds" 60 ~minimum:1 ~maximum:300 args in
  let reader, writer = Unix.pipe () in
  let child =
    try Unix.fork ()
    with exn -> Unix.close reader; Unix.close writer; raise exn
  in
  if child = 0 then (
    try
      Unix.close reader;
      ignore (Unix.setsid ());
      Unix.chdir root;
      let input = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
      Unix.dup2 input Unix.stdin; Unix.close input;
      Unix.dup2 writer Unix.stdout;
      Unix.dup2 writer Unix.stderr;
      Unix.close writer;
      Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; command |]
    with _ -> Unix._exit 127);
  Unix.close writer;
  let captured = Bytes.create max_command_bytes in
  let used = ref 0 and truncated = ref false and timed_out = ref false in
  let deadline = Unix.gettimeofday () +. float_of_int timeout in
  let chunk = Bytes.create 8192 in
  let status = ref None and eof = ref false in
  let reap () =
    if !status = None then
      match Unix.waitpid [Unix.WNOHANG] child with
      | 0, _ -> ()
      | _, result -> status := Some result
  in
  let terminate () =
    (try Unix.kill (-child) Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
    (try Unix.kill child Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
    if !status = None then status := Some (snd (Unix.waitpid [] child))
  in
  Fun.protect ~finally:(fun () -> Unix.close reader; if !status = None then terminate ()) (fun () ->
    while not !eof && not !timed_out do
      reap ();
      let remaining = deadline -. Unix.gettimeofday () in
      if remaining <= 0. then timed_out := true
      else (
        let readable, _, _ = Unix.select [reader] [] [] (min remaining 0.2) in
        if readable <> [] then (
          let n = Unix.read reader chunk 0 (Bytes.length chunk) in
          if n = 0 then eof := true
          else (
            if !used + n > max_command_bytes then truncated := true;
            let retained = min n max_command_bytes in
            let overflow = max 0 (!used + retained - max_command_bytes) in
            if overflow > 0 then Bytes.blit captured overflow captured 0 (!used - overflow);
            Bytes.blit chunk (n - retained) captured (!used - overflow) retained;
            used := !used - overflow + retained)))
    done;
    if !timed_out then terminate ()
    else while !status = None && not !timed_out do
      reap ();
      if !status = None then
        if Unix.gettimeofday () >= deadline then (timed_out := true; terminate ())
        else ignore (Unix.select [] [] [] 0.05)
    done;
    let result =
      if !timed_out then "timed out"
      else match !status with
        | Some (Unix.WEXITED code) -> Printf.sprintf "exit %d" code
        | Some (Unix.WSIGNALED signal) -> Printf.sprintf "signal %d" signal
        | Some (Unix.WSTOPPED signal) -> Printf.sprintf "stopped %d" signal
        | None -> "unknown status"
    in
    Printf.sprintf "Status: %s%s\n%s" result
      (if !truncated then " (output truncated to last 65536 bytes)" else "")
      (Bytes.sub_string captured 0 !used))

let schema name description properties required =
  `Assoc ["type", `String "function";
          "function", `Assoc ["name", `String name; "description", `String description;
                              "parameters", `Assoc ["type", `String "object";
                                                    "properties", `Assoc properties;
                                                    "required", `List (List.map (fun s -> `String s) required);
                                                    "additionalProperties", `Bool false]]]

let string_field description = `Assoc ["type", `String "string"; "description", `String description]
let integer_field description minimum maximum =
  `Assoc ["type", `String "integer"; "description", `String description;
          "minimum", `Int minimum; "maximum", `Int maximum]

let definitions = [
  schema "mobile_project" "Detect root mobile project manifests and suggest relevant build/test commands without executing anything."
    [] [];
  schema "read_file" "Read workspace text, at most 65536 bytes per request. Use offset to continue."
    ["path", string_field "Workspace-relative file path";
     "offset", integer_field "Byte offset (default 0)" 0 max_int;
     "max_bytes", integer_field "Bytes to read (default 16384)" 1 max_read_bytes] ["path"];
  schema "list_files" "Recursively list workspace files; skips build, dependency and Git directories. Output is bounded."
    ["path", string_field "Workspace-relative directory (default .)"] [];
  schema "search" "Find literal, case-sensitive text in workspace files. Skips binary, large, build, dependency and Git files/directories. Results are bounded."
    ["pattern", string_field "Literal text to search for";
     "path", string_field "Workspace-relative directory (default .)"] ["pattern"];
  schema "write_file" "Atomically create or replace a workspace file (maximum 1 MiB); parent directory must exist."
    ["path", string_field "Workspace-relative file path";
     "content", string_field "Complete replacement file contents"] ["path"; "content"];
  schema "edit_file" "Atomically replace exactly one occurrence of old_string in a workspace file (maximum 1 MiB)."
    ["path", string_field "Workspace-relative file path";
     "old_string", string_field "Exact, unique original text";
     "new_string", string_field "Replacement text"] ["path"; "old_string"; "new_string"];
  schema "run_command" "Run a shell command with workspace as cwd, returning exit status and bounded output/time. NOT SANDBOXED: the shell can access or modify files outside the workspace."
    ["command", string_field "Shell command to execute (not sandboxed)";
     "timeout_seconds", integer_field "Deadline in seconds (default 60, maximum 300)" 1 300] ["command"]
]

let execute ~root ~name ~args =
  try
    let root = root_path root in
    (match args with `Assoc _ -> () | _ -> fail "arguments must be a JSON object");
    match name with
    | "read_file" -> read_file root args
    | "list_files" -> list_files root args
    | "search" -> search root args
    | "write_file" -> write_file root args
    | "edit_file" -> edit_file root args
    | "run_command" -> run_command root args
    | "mobile_project" -> mobile_project root
    | _ -> fail ("unknown tool: " ^ name)
  with
  | Tool_error message -> "Error: " ^ message
  | Unix.Unix_error (code, operation, path) ->
      Printf.sprintf "Error: %s %s: %s" operation path (Unix.error_message code)
  | Sys_error message -> "Error: " ^ message
