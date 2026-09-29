(* EX01/EX03 local content format and trust boundary.
   [user_root] is Pave's user configuration directory; [project_root] is the
   selected workspace. Their layouts are respectively skills/<name>/SKILL.md,
   commands/<name>.json and .pave/skills/<name>/SKILL.md,
   .pave/commands/<name>.json. Disabled sources are never inspected.

   SKILL.md is UTF-8 text, at most 64 KiB, beginning with exactly:
     ---
     name: review-code
     description: Review a change
     resources: references/checklist.txt, references/example.txt
     ---
     Instructions after the closing delimiter are inert data until explicitly
     activated by the session owner. [resources] is optional; it names at most
     16 explicit, relative, ordinary files inside that skill directory.
   Command JSON (at most 32 KiB) has exactly the string fields [name],
   [description], [prompt]. E.g. {"name":"review-code","description":
   "Review code","prompt":"Review the selected change"}. Prompt is data,
   not shell, script, provider request, or an implicitly submitted message.

   Names are lowercase ASCII [a-z][a-z0-9_-]* (up to 48 bytes); command
   callers supply built-in names (with or without '/'). The snapshot never
   registers or executes anything. Invalid candidates cannot shadow valid
   content. Two valid sources of the same name resolve to project precedence;
   duplicate/ambiguous entries in one source fail closed, not first-wins.
   All counts and text reads have fixed limits; symlinks are never followed. *)

type origin = User | Project

type source = { origin : origin; path : string }
type diagnostic = { source : source; code : string; message : string }
type resource = { name : string; path : string }
type skill = {
  name : string;
  description : string;
  instructions : string;
  resources : resource list;
  source : source;
  directory : string;
  directory_identity : int * int;
}
type prompt_command = {
  name : string;
  description : string;
  prompt : string;
  source : source;
}
type snapshot = {
  skills : skill list;
  commands : prompt_command list;
  diagnostics : diagnostic list;
}

let max_entries = 64
let max_skill_bytes = 64 * 1024
let max_command_bytes = 32 * 1024
let max_resource_bytes = 32 * 1024
let max_resources = 16
let max_description_bytes = 256
let max_prompt_bytes = 16 * 1024

let id stat = stat.Unix.st_dev, stat.Unix.st_ino
let same a b = id a = id b && a.Unix.st_kind = b.Unix.st_kind
let regular stat = stat.Unix.st_kind = Unix.S_REG
let directory stat = stat.Unix.st_kind = Unix.S_DIR
let name_ok text =
  let n = String.length text in
  n > 0 && n <= 48 && text.[0] >= 'a' && text.[0] <= 'z' &&
  String.for_all (function
    | 'a'..'z' | '0'..'9' | '_' | '-' -> true | _ -> false) text
let component_ok text =
  let n = String.length text in
  n > 0 && n <= 96 && text <> "." && text <> ".." &&
  text.[0] <> '.' &&
  String.for_all (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' | '.' -> true
    | _ -> false) text
let resource_parts text =
  if String.length text > 256 || not (Filename.is_relative text) ||
     String.contains text '\\' || String.contains text '\000' then None
  else let parts = String.split_on_char '/' text in
    if List.length parts > 8 || not (List.for_all component_ok parts)
    then None else Some parts
let valid_utf8 = Project_context.valid_utf8
let plain_text text = valid_utf8 text &&
  not (String.exists (fun c -> c = '\000' || (Char.code c < 32 &&
    c <> '\n' && c <> '\r' && c <> '\t') || Char.code c = 127) text)
let label text = text <> "" && String.length text <= max_description_bytes &&
  plain_text text && not (String.contains text '\n') &&
  not (String.contains text '\r')
let source origin path = { origin; path }
let diagnostic origin path code message =
  { source = source origin path; code; message }

(* Check each component before opening and the file identity during reading.
   This prevents ordinary symlink escapes and replacement of the selected
   file; configuration directories must not be writable by a concurrent
   hostile actor (Unix.openat/O_NOFOLLOW is unavailable in this interface). *)
let rec inspect_path dir parts = match parts with
  | [] -> Ok (dir, Unix.lstat dir)
  | part :: rest ->
    let path = Filename.concat dir part in
    (try let stat = Unix.lstat path in
       if stat.Unix.st_kind = Unix.S_LNK then Error "symlink in content path"
       else if rest <> [] && not (directory stat) then
         Error "content path component is not a directory"
       else inspect_path path rest
     with Unix.Unix_error _ | Sys_error _ -> Error "cannot inspect content path")

let checked_directory origin path =
  try
    let stat = Unix.lstat path in
    if directory stat then Ok stat
    else Error (diagnostic origin path "unsafe_path" "expected a directory, not a symlink or file")
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    Error (diagnostic origin path "missing" "content directory does not exist")
  | Unix.Unix_error _ | Sys_error _ ->
    Error (diagnostic origin path "unsafe_path" "cannot inspect content directory")

let checked_file ~origin ~base ~parts ~limit =
  match checked_directory origin base with
  | Error issue -> Error issue
  | Ok base_stat ->
    (match inspect_path base parts with
     | Error message ->
       Error (diagnostic origin (List.fold_left Filename.concat base parts)
         "unsafe_path" message)
     | Ok (path, before) ->
       if not (regular before) then
         Error (diagnostic origin path "unsafe_path" "expected an ordinary regular file")
       else if before.Unix.st_size < 0 || before.Unix.st_size > limit then
         Error (diagnostic origin path "file_limit" "content file exceeds its byte limit")
       else try
         let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC; Unix.O_NONBLOCK] 0 in
         Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
           let opened = Unix.fstat fd in
           let current = Unix.lstat path in
           if not (regular opened && same opened before && same opened current &&
             same base_stat (Unix.lstat base)) then
             Error (diagnostic origin path "unsafe_path" "content changed while opening")
           else
             let capacity = min (opened.Unix.st_size + 1) (limit + 1) in
             let buffer = Bytes.create capacity in
             let rec fill offset =
               if offset = capacity then offset else
               let count = Unix.read fd buffer offset (capacity - offset) in
               if count = 0 then offset else fill (offset + count) in
             let length = fill 0 in
             if length > limit then
               Error (diagnostic origin path "file_limit" "content file exceeds its byte limit")
             else
               let still_safe = match inspect_path base parts with
                 | Ok (_, final) -> same opened final &&
                   same base_stat (Unix.lstat base)
                 | Error _ -> false in
               if not still_safe || opened.Unix.st_size <> length then
                 Error (diagnostic origin path "unsafe_path" "content changed during reading")
               else let text = Bytes.sub_string buffer 0 length in
                 if not (plain_text text) then
                   Error (diagnostic origin path "invalid_text"
                     "content must be UTF-8 text without control bytes")
                 else Ok text)
       with Unix.Unix_error _ | Sys_error _ ->
         Error (diagnostic origin path "read_error" "cannot read content file"))

let directory_entries origin path =
  try
    let dir = Unix.opendir path in
    Fun.protect ~finally:(fun () -> Unix.closedir dir) (fun () ->
      let rec read acc count =
        match Unix.readdir dir with
        | "." | ".." -> read acc count
        | name ->
          if count >= max_entries then
            Error (diagnostic origin path "entry_limit" "more than 64 entries; no content loaded from this directory")
          else read (name :: acc) (count + 1)
        | exception End_of_file -> Ok (List.sort String.compare acc) in
      read [] 0)
  with Unix.Unix_error _ | Sys_error _ ->
    Error (diagnostic origin path "read_error" "cannot enumerate content directory")

let skill_metadata text =
  match String.split_on_char '\n' text with
  | "---" :: rest ->
    let rec headers fields count = function
      | [] -> Error "missing closing metadata delimiter"
      | "---" :: body -> Ok (fields, String.concat "\n" body)
      | line :: remaining ->
        if count >= 4 then Error "too many metadata fields"
        else match String.index_opt line ':' with
          | None -> Error "invalid metadata field"
          | Some index ->
            let key = String.sub line 0 index in
            let value = String.sub line (index + 1) (String.length line - index - 1)
              |> String.trim in
            if not (List.mem key ["name"; "description"; "resources"]) ||
               List.mem_assoc key fields then Error "unknown or duplicate metadata field"
            else headers ((key, value) :: fields) (count + 1) remaining in
    (match headers [] 0 rest with
     | Error _ as error -> error
     | Ok (fields, instructions) ->
       let get key = List.assoc_opt key fields in
       match get "name", get "description" with
       | Some name, Some description when name_ok name && label description ->
         let resources = match get "resources" with
           | None -> []
           | Some text -> String.split_on_char ',' text |> List.map String.trim in
         if List.length resources > max_resources ||
            List.exists (fun name -> resource_parts name = None) resources ||
            List.length resources <> List.length (List.sort_uniq String.compare resources)
         then Error "resource list contains an unsafe, duplicate, or excessive path"
         else Ok (name, description, instructions, resources)
       | _ -> Error "name or description is missing or invalid")
  | _ -> Error "SKILL.md must start with --- metadata delimiter"

let command_manifest text =
  try
    let fields = match Yojson.Basic.from_string text with
      | `Assoc fields -> fields
      | _ -> invalid_arg "command manifest must be a JSON object" in
    let keys = List.map fst fields in
    if List.sort String.compare keys <> ["description"; "name"; "prompt"] then
      invalid_arg "command manifest requires exactly name, description and prompt";
    match List.assoc "name" fields, List.assoc "description" fields,
          List.assoc "prompt" fields with
    | `String name, `String description, `String prompt
      when name_ok name && label description && String.trim prompt <> "" &&
           String.length prompt <= max_prompt_bytes && plain_text prompt ->
        Ok (name, description, prompt)
    | _ -> Error "command fields must be valid bounded text and a strict name"
  with Yojson.Json_error _ | Invalid_argument _ ->
    Error "invalid command JSON or unexpected manifest fields"

let inspect_resource origin base name =
  match resource_parts name with
  | None -> Error (diagnostic origin (Filename.concat base name) "unsafe_path" "invalid resource path")
  | Some parts ->
    (match inspect_path base parts with
     | Error message -> Error (diagnostic origin (Filename.concat base name) "unsafe_path" message)
     | Ok (path, stat) ->
       if not (regular stat) then Error (diagnostic origin path "unsafe_path" "resource is not a regular file")
       else if stat.Unix.st_size < 0 || stat.Unix.st_size > max_resource_bytes then
         Error (diagnostic origin path "file_limit" "resource exceeds 32 KiB")
       else Ok { name; path })

let scan_skill origin skills_root dirname =
  let base = Filename.concat skills_root dirname in
  if not (name_ok dirname) then
    Error [diagnostic origin base "invalid_name" "skill directory must have a strict lowercase name"]
  else match checked_directory origin base with
    | Error issue -> Error [issue]
    | Ok stat ->
      (match checked_file ~origin ~base ~parts:["SKILL.md"] ~limit:max_skill_bytes with
       | Error issue -> Error [issue]
       | Ok text ->
         match skill_metadata text with
         | Error message -> Error [diagnostic origin (Filename.concat base "SKILL.md") "invalid_manifest" message]
         | Ok (name, description, instructions, paths) ->
           if name <> dirname then Error [diagnostic origin base "invalid_name" "skill name differs from directory"]
           else
             let resources = List.map (inspect_resource origin base) paths in
             let failures = List.filter_map (function Error issue -> Some issue | Ok _ -> None) resources in
             if failures <> [] then Error failures
             else (try
               if id (Unix.lstat base) <> id stat then
                 Error [diagnostic origin base "unsafe_path" "skill directory changed during discovery"]
               else Ok { name; description; instructions;
                 resources = List.filter_map (function Ok item -> Some item | Error _ -> None) resources;
                 source = source origin (Filename.concat base "SKILL.md");
                 directory = base; directory_identity = id stat }
             with Unix.Unix_error _ | Sys_error _ ->
               Error [diagnostic origin base "unsafe_path" "skill directory disappeared"]))

let scan_command origin command_root filename =
  let base = Filename.remove_extension filename in
  let path = Filename.concat command_root filename in
  if Filename.extension filename <> ".json" || not (name_ok base) then
    Error [diagnostic origin path "invalid_name" "command filename must be <strict-name>.json"]
  else match checked_file ~origin ~base:command_root ~parts:[filename] ~limit:max_command_bytes with
    | Error issue -> Error [issue]
    | Ok text ->
      (match command_manifest text with
       | Error message -> Error [diagnostic origin path "invalid_manifest" message]
       | Ok (name, description, prompt) ->
         if name <> base then Error [diagnostic origin path "invalid_name" "command name differs from filename"]
         else Ok { name; description; prompt; source = source origin path })

let scan_collection ~origin ~root ~parts ~reader =
  let path = List.fold_left Filename.concat root parts in
  let rec inspect dir parents = function
    | [] -> Ok (List.rev parents)
    | part :: remaining ->
      let next = Filename.concat dir part in
      match checked_directory origin next with
      | Ok stat -> inspect next ((next, stat) :: parents) remaining
      | Error issue -> Error issue in
  match checked_directory origin root with
  | Error issue -> [], [issue]
  | Ok root_stat ->
    (match inspect root [root, root_stat] parts with
     | Error { code = "missing"; _ } -> [], []
     | Error issue -> [], [issue]
     | Ok parents ->
       let unchanged () =
         List.for_all (fun (dir, before) ->
           try same before (Unix.lstat dir)
           with Unix.Unix_error _ | Sys_error _ -> false) parents in
       (match directory_entries origin path with
        | Error issue -> [], [issue]
        | Ok entries ->
          let accepted = ref [] and issues = ref [] and invalidated = ref false in
          List.iter (fun entry ->
            if not (unchanged ()) then invalidated := true
            else if not !invalidated then
              match reader origin path entry with
              | Ok item -> accepted := item :: !accepted
              | Error errors -> issues := List.rev_append errors !issues) entries;
          if !invalidated || not (unchanged ()) then
            [], [diagnostic origin path "unsafe_path"
              "content directory changed during discovery; no entries selected"]
          else List.rev !accepted, List.rev !issues))

let prefer_project ~name ~source entries =
  let groups = Hashtbl.create 64 in
  List.iter (fun item ->
    let key = name item in
    let items = match Hashtbl.find_opt groups key with Some xs -> xs | None -> [] in
    Hashtbl.replace groups key (item :: items)) entries;
  let selected = ref [] and issues = ref [] in
  Hashtbl.iter (fun name items ->
    let project = List.filter (fun item -> (source item).origin = Project) items in
    let user = List.filter (fun item -> (source item).origin = User) items in
    let one origin entries = match entries with
      | [] -> None
      | [item] -> Some item
      | items ->
        List.iter (fun item -> issues := diagnostic origin (source item).path
          "duplicate_name" ("ambiguous duplicate name " ^ name) :: !issues) items;
        None in
    let project = one Project project and user = one User user in
    match project, user with
    | Some p, Some u ->
      selected := p :: !selected;
      issues := diagnostic User (source u).path "shadowed"
        ("project content takes precedence for " ^ name) :: !issues
    | Some p, None -> selected := p :: !selected
    | None, Some u -> selected := u :: !selected
    | None, None -> ()) groups;
  List.sort (fun a b -> String.compare (name a) (name b)) !selected,
  List.sort (fun (a : diagnostic) b -> String.compare a.source.path b.source.path) !issues

let scan ~user_root ~project_root ~enable_user ~enable_project ~builtin_names () =
  let sources = [User, user_root, enable_user; Project, project_root, enable_project] in
  let all_skills = ref [] and all_commands = ref [] and issues = ref [] in
  List.iter (fun (origin, root, enabled) -> if enabled then (
    match checked_directory origin root with
    | Error issue -> issues := issue :: !issues
    | Ok _ ->
      let prefix = if origin = Project then [".pave"] else [] in
      let skills, skill_issues = scan_collection ~origin ~root
        ~parts:(prefix @ ["skills"]) ~reader:scan_skill in
      let commands, command_issues = scan_collection ~origin ~root
        ~parts:(prefix @ ["commands"]) ~reader:scan_command in
      all_skills := List.rev_append skills !all_skills;
      all_commands := List.rev_append commands !all_commands;
      issues := List.rev_append (skill_issues @ command_issues) !issues)) sources;
  let skills, skill_issues = prefer_project ~name:(fun (s : skill) -> s.name)
    ~source:(fun s -> s.source) !all_skills in
  let commands, command_issues = prefer_project
    ~name:(fun (c : prompt_command) -> c.name) ~source:(fun c -> c.source)
    !all_commands in
  let builtin_names = List.map (fun name ->
    if String.length name > 0 && name.[0] = '/' then
      String.sub name 1 (String.length name - 1) else name) builtin_names in
  let commands, collision_issues = List.fold_right (fun command (selected, issues) ->
    if List.mem command.name builtin_names then
      selected, diagnostic command.source.origin command.source.path
        "builtin_collision" ("built-in command reserves /" ^ command.name) :: issues
    else command :: selected, issues) commands ([], []) in
  { skills; commands;
    diagnostics = List.rev !issues @ skill_issues @ command_issues @ collision_issues }

let skill_names snapshot = List.map (fun (skill : skill) -> skill.name) snapshot.skills
let command_names snapshot = List.map (fun (command : prompt_command) -> command.name) snapshot.commands

(* Only an explicitly listed resource from an accepted skill may be read.
   A later changed/disabled snapshot must not be used for new requests. *)
let read_resource skill name =
  match List.find_opt (fun (item : resource) -> item.name = name) skill.resources with
  | None -> Error (diagnostic skill.source.origin skill.source.path "unknown_resource"
      "resource is not declared by this skill")
  | Some item ->
    (try
      if id (Unix.lstat skill.directory) <> skill.directory_identity then
        Error (diagnostic skill.source.origin item.path "unsafe_path" "skill directory changed")
      else match resource_parts name with
        | None -> Error (diagnostic skill.source.origin item.path "unsafe_path" "invalid resource name")
        | Some parts ->
          let result = checked_file ~origin:skill.source.origin ~base:skill.directory
            ~parts ~limit:max_resource_bytes in
          if id (Unix.lstat skill.directory) <> skill.directory_identity then
            Error (diagnostic skill.source.origin item.path "unsafe_path" "skill directory changed")
          else result
    with Unix.Unix_error _ | Sys_error _ ->
      Error (diagnostic skill.source.origin item.path "unsafe_path" "skill directory disappeared"))
