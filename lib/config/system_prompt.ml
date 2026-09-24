type loaded = { text : string; diagnostics : string list }

type candidate = Plain of string | Template of string

let max_file_bytes = 65_536
let max_prompt_bytes = 262_144

let check_text ~source text =
  if String.length text > max_file_bytes then
    invalid_arg (source ^ " exceeds 64 KiB");
  if String.contains text '\000' || not (Project_context.valid_utf8 text) then
    invalid_arg (source ^ " must be valid UTF-8 without NUL bytes");
  text

let read_file path =
  let directory = Unix.lstat (Filename.dirname path) in
  if directory.Unix.st_kind <> Unix.S_DIR then
    invalid_arg "prompt directory must be real, not a symlink";
  let before = Unix.lstat path in
  if before.Unix.st_kind <> Unix.S_REG || before.Unix.st_size > max_file_bytes then
    invalid_arg "prompt file must be a regular file of at most 64 KiB";
  let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_NONBLOCK; Unix.O_CLOEXEC] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let current = Unix.lstat path and stats = Unix.fstat fd in
    if stats.Unix.st_kind <> Unix.S_REG ||
       stats.Unix.st_dev <> before.Unix.st_dev ||
       stats.Unix.st_ino <> before.Unix.st_ino ||
       current.Unix.st_dev <> stats.Unix.st_dev ||
       current.Unix.st_ino <> stats.Unix.st_ino then
      invalid_arg "prompt file changed during opening";
    let buffer = Bytes.create stats.Unix.st_size in
    let rec fill offset =
      if offset < Bytes.length buffer then (
        let count = Unix.read fd buffer offset (Bytes.length buffer - offset) in
        if count = 0 then invalid_arg "prompt file changed during reading";
        fill (offset + count)) in
    fill 0;
    check_text ~source:path (Bytes.to_string buffer))

let render_template ~root source =
  let marker = "{{root}}" in
  let marker_length = String.length marker in
  let n = String.length source in
  let output = Buffer.create n in
  let rec render index =
    if index < n then
      if index + marker_length <= n &&
         String.sub source index marker_length = marker then (
        Buffer.add_string output root;
        render (index + marker_length))
      else if index + 1 < n &&
              (String.sub source index 2 = "{{" ||
               String.sub source index 2 = "}}") then
        invalid_arg "template has an unknown placeholder"
      else (Buffer.add_char output source.[index]; render (index + 1)) in
  render 0;
  let result = Buffer.contents output in
  if String.trim result = "" then invalid_arg "template is empty";
  result

let load ?custom_text ?template_file ?append_text ~root ~mobile ~project () =
  if Option.is_some custom_text && Option.is_some template_file then
    invalid_arg "--system-prompt and --system-prompt-template are mutually exclusive";
  let user_home, base_diagnostics = Settings.config_home () in
  let user = Filename.concat user_home "pave" in
  let workspace = Filename.concat root ".pave" in
  let diagnostics = ref base_diagnostics in
  let attempt path =
    try Some (read_file path) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> None
    | (Unix.Unix_error _ | Sys_error _ | Invalid_argument _) as exn ->
        diagnostics := !diagnostics @ [path ^ ": " ^ Printexc.to_string exn];
        None in
  let custom = match custom_text, template_file with
    | Some text, None -> check_text ~source:"--system-prompt" text
    | None, Some path -> render_template ~root (read_file path)
    | None, None ->
        let rec discover = function
          | [] -> ""
          | Plain path :: rest -> (match attempt path with
              | Some text -> text | None -> discover rest)
          | Template path :: rest ->
              (match attempt path with
               | None -> discover rest
               | Some text -> try render_template ~root text with Invalid_argument why ->
                   diagnostics := !diagnostics @ [path ^ ": " ^ why];
                   discover rest) in
        discover [Plain (Filename.concat workspace "SYSTEM.md");
          Template (Filename.concat workspace "SYSTEM_TEMPLATE.md");
          Plain (Filename.concat user "SYSTEM.md");
          Template (Filename.concat user "SYSTEM_TEMPLATE.md")]
    | Some _, Some _ -> assert false in
  let appended = match append_text with
    | Some text -> check_text ~source:"--append-system-prompt" text
    | None ->
        let project_append = Filename.concat workspace "APPEND_SYSTEM.md" in
        (match attempt project_append with
         | Some text -> text
         | None -> Option.value ~default:"" (attempt
             (Filename.concat user "APPEND_SYSTEM.md"))) in
  let parts = [mobile;
    (if project = "" then "" else "Project instructions (lower priority than mobile safety):\n" ^ project);
    (if custom = "" then "" else "Custom instructions (lower priority than mobile safety):\n" ^ custom);
    (if appended = "" then "" else "Additional instructions (lower priority than mobile safety):\n" ^ appended)] in
  let text = String.concat "\n\n" (List.filter ((<>) "") parts) in
  if String.length text > max_prompt_bytes then
    invalid_arg "assembled system prompt exceeds 256 KiB";
  { text; diagnostics = !diagnostics }
