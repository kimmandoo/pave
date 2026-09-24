(* Precedence (least to most specific): $XDG_CONFIG_HOME/pave/AGENTS.md
   (or $HOME/.config/pave/AGENTS.md), then ancestor AGENTS.md from the
   filesystem root/home boundary toward the workspace, then matching
   workspace .pave/rules/*.md in lexical order. Pave.Mobile_prompt.text is
   NOT included: callers must put it BEFORE [text], as higher-priority safety
   instructions. No settings, overrides, or executable rule formats are read.

   Each rule starts with a deliberately small header:
     ---
     paths: src/**/*.ml, test/**
     ---
   A rule applies only when [?path] is supplied (absolute within [root], or
   root-relative). Patterns use slash-separated *, ?, and ** components.
   Imports are full lines of the form @relative/file.md, expanded inline.
   Every import stays within its source's instruction directory (for rules,
   the .pave/rules directory); symlinks are not followed. [text] is the
   precedence-ordered payload to append after mobile safety; [provenance]
   lists accepted source files in encounter order, including imports.
   [diagnostics] is ordered and contains source paths, stable codes, and
   actionable descriptions for skipped or overlapping instructions. *)

type kind = User | Project | Rule | Import

type source = { path : string; kind : kind; scope : string option }
type diagnostic = { path : string; code : string; message : string }
type result = { text : string; diagnostics : diagnostic list; provenance : source list }

let max_file_bytes = 64 * 1024
let max_total_bytes = 256 * 1024
let max_import_depth = 8
let max_files = 64

let within ~base path =
  path = base || (String.length path > String.length base &&
    String.sub path 0 (String.length base) = base &&
    (base = "/" || path.[String.length base] = '/'))

let normalize path =
  let absolute = not (Filename.is_relative path) in
  let parts = String.split_on_char '/' path in
  let rec fold acc = function
    | [] -> List.rev acc
    | "" :: rest | "." :: rest -> fold acc rest
    | ".." :: rest -> fold (match acc with [] -> [] | _ :: xs -> xs) rest
    | part :: rest -> fold (part :: acc) rest in
  let joined = String.concat "/" (fold [] parts) in
  if absolute then "/" ^ joined else joined

let split_components path = String.split_on_char '/' path |> List.filter ((<>) "")

let valid_utf8 s =
  let n = String.length s in
  let byte i = Char.code s.[i] in
  let cont i = i < n && byte i land 0xc0 = 0x80 in
  let rec check i =
    if i = n then true else
    let b = byte i in
    if b < 0x80 then check (i + 1)
    else if b >= 0xc2 && b <= 0xdf then
      cont (i + 1) && check (i + 2)
    else if b = 0xe0 then
      i + 2 < n && byte (i + 1) >= 0xa0 && byte (i + 1) <= 0xbf &&
      cont (i + 2) && check (i + 3)
    else if (b >= 0xe1 && b <= 0xec) || (b >= 0xee && b <= 0xef) then
      cont (i + 1) && cont (i + 2) && check (i + 3)
    else if b = 0xed then
      i + 2 < n && byte (i + 1) >= 0x80 && byte (i + 1) <= 0x9f &&
      cont (i + 2) && check (i + 3)
    else if b = 0xf0 then
      i + 3 < n && byte (i + 1) >= 0x90 && byte (i + 1) <= 0xbf &&
      cont (i + 2) && cont (i + 3) && check (i + 4)
    else if b >= 0xf1 && b <= 0xf3 then
      cont (i + 1) && cont (i + 2) && cont (i + 3) && check (i + 4)
    else if b = 0xf4 then
      i + 3 < n && byte (i + 1) >= 0x80 && byte (i + 1) <= 0x8f &&
      cont (i + 2) && cont (i + 3) && check (i + 4)
    else false in
  check 0

let wildcard pattern text =
  let p = String.length pattern and n = String.length text in
  let rec walk pi ti star retry =
    if ti = n then
      let rec trailing i = i = p || (pattern.[i] = '*' && trailing (i + 1)) in
      trailing pi
    else if pi < p && (pattern.[pi] = '?' || pattern.[pi] = text.[ti]) then
      walk (pi + 1) (ti + 1) star retry
    else if pi < p && pattern.[pi] = '*' then
      walk (pi + 1) ti pi ti
    else if star >= 0 then walk (star + 1) (retry + 1) star (retry + 1)
    else false in
  walk 0 0 (-1) 0

let glob_matches pattern path =
  let pattern = Array.of_list (split_components pattern) in
  let target = Array.of_list (split_components path) in
  let memo = Hashtbl.create 32 in
  let rec walk i j =
    match Hashtbl.find_opt memo (i, j) with
    | Some matched -> matched
    | None ->
      let matched =
        if i = Array.length pattern then j = Array.length target
        else if pattern.(i) = "**" then walk (i + 1) j ||
          (j < Array.length target && walk i (j + 1))
        else j < Array.length target &&
          wildcard pattern.(i) target.(j) && walk (i + 1) (j + 1) in
      Hashtbl.add memo (i, j) matched; matched in
  walk 0 0

let parse_rule text =
  match String.split_on_char '\n' text with
  | first :: header :: third :: body
    when String.trim first = "---" && String.trim third = "---" ->
      let header = String.trim header in
      let prefix = "paths:" in
      if String.length header < String.length prefix ||
         String.sub header 0 (String.length prefix) <> prefix then None
      else
        let patterns = String.sub header (String.length prefix)
          (String.length header - String.length prefix)
          |> String.split_on_char ',' |> List.map String.trim
          |> List.filter ((<>) "") in
        if patterns = [] || List.length patterns > 64 ||
           List.exists (fun p -> String.length p > 256 ||
             not (Filename.is_relative p) ||
             List.mem ".." (split_components p) ||
             String.contains p '\\' || String.contains p '\000') patterns
        then None else Some (patterns, String.concat "\n" body)
  | _ -> None

let load ?path ?(scoped_only = false) ~root () : result =
  let diagnostics = ref [] and provenance = ref [] and chunks = ref [] in
  let diag path code message =
    diagnostics := { path; code; message } :: !diagnostics in
  let canonical dir =
    try let real = Unix.realpath dir in
      if (Unix.stat real).Unix.st_kind = Unix.S_DIR then Some real else None
    with Unix.Unix_error _ | Sys_error _ | Invalid_argument _ -> None in
  match canonical root with
  | None -> { text = ""; diagnostics = [{ path = root; code = "invalid_root";
      message = "Workspace root is not an accessible directory" }]; provenance = [] }
  | Some root ->
    let safe_relative ~base candidate =
      let candidate = normalize candidate in
      if String.contains candidate '\000' then Error "Instruction path contains a NUL byte"
      else if not (within ~base candidate) then Error "Path escapes its instruction boundary"
      else
        let relative = if candidate = base then [] else
          String.sub candidate (String.length base + (if base = "/" then 0 else 1))
            (String.length candidate - String.length base - (if base = "/" then 0 else 1))
          |> split_components in
        let rec inspect dir = function
          | [] -> Ok candidate
          | part :: rest ->
              let current = Filename.concat dir part in
              match Unix.lstat current with
              | { Unix.st_kind = Unix.S_LNK; _ } -> Error "Symlink is not allowed in instruction path"
              | { Unix.st_kind = Unix.S_DIR; _ } when rest <> [] -> inspect current rest
              | { Unix.st_kind = Unix.S_REG; _ } when rest = [] -> Ok current
              | _ -> Error "Instruction path is not a regular file"
              | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Error "Instruction file is missing"
              | exception Unix.Unix_error _ -> Error "Cannot inspect instruction path" in
        inspect base relative in
    let bytes = ref 0 and files = ref 0 in
    let read_file ~base file =
      match safe_relative ~base file with
      | Error reason -> diag file "unsafe_path" reason; None
      | Ok file ->
        try
          let fd = Unix.openfile file [Unix.O_RDONLY; Unix.O_CLOEXEC; Unix.O_NONBLOCK] 0 in
          Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
            let stat = Unix.fstat fd and current = Unix.lstat file in
            if safe_relative ~base file <> Ok file ||
               stat.Unix.st_kind <> Unix.S_REG || current.Unix.st_kind <> Unix.S_REG ||
               stat.Unix.st_dev <> current.Unix.st_dev ||
               stat.Unix.st_ino <> current.Unix.st_ino then (
              diag file "unsafe_path" "Instruction changed during opening"; None)
            else if stat.Unix.st_size > max_file_bytes then (
              diag file "file_limit" "Instruction exceeds 64 KiB per-file limit"; None)
            else if !files >= max_files || !bytes + stat.Unix.st_size > max_total_bytes then (
              diag file "total_limit" "Instruction count or total 256 KiB limit reached"; None)
            else (
              let text = Bytes.create stat.Unix.st_size in
              let rec fill offset =
                if offset < Bytes.length text then
                  let n = Unix.read fd text offset (Bytes.length text - offset) in
                  if n = 0 then raise End_of_file else fill (offset + n) in
              fill 0;
              let text = Bytes.to_string text in
              if not (valid_utf8 text) then (
                diag file "invalid_utf8" "Instruction must contain valid UTF-8"; None)
              else (
                incr files; bytes := !bytes + String.length text;
                Some text)))
        with End_of_file ->
          diag file "read_error" "Instruction was truncated while reading"; None
        | Unix.Unix_error _ | Sys_error _ ->
          diag file "read_error" "Cannot read instruction file"; None in
    let present file =
      try ignore (Unix.lstat file); true with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> false
      | Unix.Unix_error _ -> true in
    let seen = Hashtbl.create 32 in
    let rec expand ~base ~kind ~scope ~depth ~stack file text =
      if List.mem file stack then (
        diag file "import_cycle" "Import cycle detected; remove the repeated @ import";
        "")
      else if depth > max_import_depth then (
        diag file "import_depth" "Import exceeds maximum nesting depth of 8"; "")
      else if Hashtbl.mem seen file then (
        diag file "shadowed" "Instruction already loaded; duplicate import ignored"; "")
      else (
        Hashtbl.add seen file ();
        provenance := { path = file; kind; scope } :: !provenance;
        let stack = file :: stack in
        String.split_on_char '\n' text |> List.map (fun line ->
          let trimmed = String.trim line in
          if String.length trimmed > 1 && trimmed.[0] = '@' &&
             not (String.contains trimmed ' ' || String.contains trimmed '\t') then
            let imported = String.sub trimmed 1 (String.length trimmed - 1) in
            if not (Filename.is_relative imported) ||
               String.contains imported '\\' || String.contains imported '\000' then (
              diag file "unsafe_import" ("Import must be a relative path: " ^ imported); "")
            else
              let target = normalize (Filename.concat (Filename.dirname file) imported) in
              if not (within ~base target) then (
                diag file "unsafe_import" ("Import escapes instruction directory: " ^ imported); "")
              else if List.mem target stack then (
                diag target "import_cycle" "Import cycle detected; remove the repeated @ import"; "")
              else match read_file ~base target with
                | None -> ""
                | Some imported_text ->
                  expand ~base ~kind:Import ~scope ~depth:(depth + 1)
                    ~stack target imported_text
          else line) |> String.concat "\n") in
    let add ?(scope = None) ~base ~kind file =
      if present file then match read_file ~base file with
        | None -> ()
        | Some text ->
          let expanded = expand ~base ~kind ~scope ~depth:0 ~stack:[] file text in
          chunks := expanded :: !chunks in
    if not scoped_only then (
      let home = match Sys.getenv_opt "HOME" with
        | Some value when value <> "" && not (Filename.is_relative value) -> canonical value
        | _ -> None in
      let config_dir = match Sys.getenv_opt "XDG_CONFIG_HOME" with
        | Some value when value <> "" && not (Filename.is_relative value) -> Some value
        | _ -> Option.map (fun dir -> Filename.concat dir ".config") home in
      (match config_dir with
       | None -> diag root "no_user_config" "HOME is unavailable; user instructions skipped"
       | Some dir ->
         (match canonical dir with
          | None -> if present dir then
              diag dir "unsafe_config" "User config directory is not accessible"
          | Some dir -> add ~base:dir ~kind:User (Filename.concat dir "pave/AGENTS.md")));
      let boundary = match home with
        | Some home when within ~base:home root -> home
        | _ -> "/" in
      let rec ancestors dir acc =
        if dir = boundary || dir = "/" then dir :: acc
        else ancestors (Filename.dirname dir) (dir :: acc) in
      List.iter (fun dir ->
        add ~base:dir ~kind:Project (Filename.concat dir "AGENTS.md"))
        (ancestors root []));
    let target = match path with
      | None -> None
      | Some path ->
        let absolute = if Filename.is_relative path then Filename.concat root path else path in
        let absolute = normalize absolute in
        if String.contains absolute '\000' || not (within ~base:root absolute) then (
          diag path "unsafe_target" "Rule target is outside workspace"; None)
        else
          let relative = if absolute = root then "" else
            String.sub absolute (String.length root + (if root = "/" then 0 else 1))
              (String.length absolute - String.length root - (if root = "/" then 0 else 1)) in
          let rec safe_target dir = function
            | [] -> true
            | part :: rest ->
              let next = Filename.concat dir part in
              (match Unix.lstat next with
               | { Unix.st_kind = Unix.S_LNK; _ } -> false
               | { Unix.st_kind = Unix.S_DIR; _ } -> safe_target next rest
               | { Unix.st_kind = Unix.S_REG; _ } -> rest = []
               | _ -> false
               | exception Unix.Unix_error (Unix.ENOENT, _, _) -> true
               | exception Unix.Unix_error _ -> false) in
          if safe_target root (split_components relative) then Some relative
          else (diag path "unsafe_target" "Rule target passes through a symlink or non-directory"; None) in
    let rules_dir = Filename.concat root ".pave/rules" in
    let first_matching_rule = ref None in
    let rule_entries = ref 0 in
    let rec visit_rules depth dir =
      if depth > 8 then diag dir "rule_depth" "Rule directory exceeds nesting limit"
      else
        let entries =
          try Array.to_list (Sys.readdir dir) |> List.sort String.compare
          with Sys_error _ | Unix.Unix_error _ ->
            diag dir "read_error" "Cannot list rule directory"; [] in
        List.iter (fun entry ->
          incr rule_entries;
          let file = Filename.concat dir entry in
          if !rule_entries > 256 then (
            if !rule_entries = 257 then
              diag dir "rule_limit" "Rule directory exceeds 256 entries; remaining rules skipped")
          else
          try
            let stat = Unix.lstat file in
            match stat.Unix.st_kind with
            | Unix.S_LNK -> diag file "unsafe_path" "Symlink is not allowed in rule directory"
            | Unix.S_DIR -> visit_rules (depth + 1) file
            | Unix.S_REG when Filename.check_suffix file ".md" ->
              (match read_file ~base:rules_dir file with
               | None -> ()
               | Some text ->
                 (match parse_rule text with
                  | None -> diag file "invalid_rule"
                      "Rule needs --- / paths: comma-separated/globs / --- header"
                  | Some (patterns, body) ->
                    (match target with
                     | None -> ()
                     | Some path when List.exists (fun pattern -> glob_matches pattern path) patterns ->
                       (match !first_matching_rule with
                        | None -> first_matching_rule := Some file
                        | Some previous ->
                          diag file "rule_conflict"
                            ("Also matches " ^ path ^ "; ordered after " ^ previous));
                       let scope = Some (String.concat ", " patterns) in
                       chunks := expand ~base:rules_dir ~kind:Rule ~scope ~depth:0
                         ~stack:[] file body :: !chunks
                     | Some _ -> ())))
            | _ -> ()
          with Unix.Unix_error _ ->
            diag file "read_error" "Cannot inspect rule entry")
          entries in
    (match Unix.lstat (Filename.concat root ".pave") with
     | { Unix.st_kind = Unix.S_DIR; _ } ->
       (match Unix.lstat rules_dir with
        | { Unix.st_kind = Unix.S_DIR; _ } -> visit_rules 0 rules_dir
        | _ -> diag rules_dir "unsafe_path" "Rule root must be a real directory"
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
        | exception Unix.Unix_error _ ->
          diag rules_dir "read_error" "Cannot inspect rule directory")
     | _ -> diag rules_dir "unsafe_path" "Rule parent must be a real directory"
     | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
     | exception Unix.Unix_error _ ->
       diag rules_dir "read_error" "Cannot inspect rule parent");
    { text = String.concat "\n\n" (List.rev !chunks);
      diagnostics = List.rev !diagnostics; provenance = List.rev !provenance }

(* Resolve just the per-file instructions. The caller must put [text] into a
   system message, never into a tool result. Incomplete rule sets fail closed:
   a skipped rule or import could otherwise silently omit restrictions. Two
   matching rules are deterministic (lexical order), so their conflict is
   reported but both are supplied. Do not cache this result: rules and symlink
   targets may change between model turns. *)
type scoped = { text : string; diagnostics : diagnostic list; safe : bool }

let resolve_scoped ~root ~path () : scoped =
  let result = load ~root ~path ~scoped_only:true () in
  let safe = List.for_all (fun diagnostic ->
    diagnostic.code = "rule_conflict") result.diagnostics in
  { text = if safe then result.text else "";
    diagnostics = result.diagnostics; safe }
