(* Command/task provenance is supplied by the caller, never inferred from Gradle output. *)
let locations ~root ~cwd ~subroot ~task output =
  let max_locations = 16 in
  let forbidden_directory = function
    | "build" | ".gradle" | "node_modules" | ".m2" | ".dart_tool"
    | "vendor" | "third_party" | "Pods" | "out" -> true
    | _ -> false in
  let no_controls text =
    not (String.exists (fun c -> Char.code c < 32 || Char.code c = 127) text) in
  let positive text =
    if text = "" || not (String.for_all (function '0' .. '9' -> true | _ -> false) text)
    then None
    else match int_of_string_opt text with
      | Some n when n > 0 && n <= 1_000_000 -> Some n
      | _ -> None in
  let find_from text start needle =
    let length = String.length needle in
    let rec loop i =
      if i + length > String.length text then None
      else if String.sub text i length = needle then Some i
      else loop (i + 1) in
    loop start in
  let slice text start stop = String.sub text start (stop - start) in
  let relative root path = if path = root then ""
    else slice path (String.length root + (if root = "/" then 0 else 1))
      (String.length path) in
  let components path = String.split_on_char '/' path in
  let clean_segments path =
    not (List.exists (fun segment -> segment = ".." || forbidden_directory segment)
      (components path)) in
  (* realpath alone would permit a symlink into the selected module. Check each
     lexical component as well as the canonical containment of the final file. *)
  let no_symlinks_from root path =
    let suffix = relative root path in
    let rec walk base = function
      | [] -> true
      | "" :: rest | "." :: rest -> walk base rest
      | segment :: rest ->
          let next = Filename.concat base segment in
          (Unix.lstat next).Unix.st_kind <> Unix.S_LNK && walk next rest in
    walk root (components suffix) in
  try
    let root = Workspace_path.root_path root in
    let project = if subroot = "." then root
      else Workspace_path.checked_path root subroot in
    if not (Workspace_gradle_focus.qualified_task task) ||
       not (Workspace_path.within root project) ||
       (subroot <> "." &&
        not (no_symlinks_from root (Filename.concat root subroot))) ||
       not (clean_segments (relative root project)) ||
       not (no_symlinks_from root project) ||
       (Unix.stat project).Unix.st_kind <> Unix.S_DIR ||
       Unix.realpath cwd <> Unix.realpath project then []
    else
      let segments = String.split_on_char ':' task in
      let module_segments = match segments with
        | "" :: rest -> List.rev (List.tl (List.rev rest))
        | _ -> [] in
      let module_root = List.fold_left Filename.concat project module_segments in
      if not (Workspace_path.within project module_root) ||
         not (no_symlinks_from root module_root) ||
         (Unix.stat module_root).Unix.st_kind <> Unix.S_DIR then []
      else
        let valid_path path =
          try
            let absolute = if Filename.is_relative path then Filename.concat cwd path
              else path in
            if not (no_controls path) ||
               not (Workspace_path.within root absolute) ||
               not (Workspace_path.within module_root absolute) ||
               not (clean_segments (relative root absolute)) ||
               not (Filename.check_suffix path ".kt" || Filename.check_suffix path ".java") ||
               not (no_symlinks_from root absolute) ||
               (Unix.lstat absolute).Unix.st_kind <> Unix.S_REG then None
            else
              let canonical = Unix.realpath absolute in
              if Workspace_path.within module_root canonical &&
                 (Unix.stat canonical).Unix.st_kind = Unix.S_REG then
                Some (relative root canonical)
              else None
          with Unix.Unix_error _ | Invalid_argument _ -> None in
        let decode_uri uri =
          if not (String.starts_with ~prefix:"file:///" uri) then None
          else
            let hex = function
              | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
              | 'a' .. 'f' as c -> Some (Char.code c - Char.code 'a' + 10)
              | 'A' .. 'F' as c -> Some (Char.code c - Char.code 'A' + 10)
              | _ -> None in
            let buffer = Buffer.create (String.length uri - 7) in
            let rec copy i =
              if i = String.length uri then Some (Buffer.contents buffer)
              else if uri.[i] = '%' then
                if i + 2 >= String.length uri then None
                else (match hex uri.[i + 1], hex uri.[i + 2] with
                  | Some a, Some b ->
                      Buffer.add_char buffer (Char.chr (a * 16 + b));
                      copy (i + 3)
                  | _ -> None)
              else (Buffer.add_char buffer uri.[i]; copy (i + 1)) in
            copy 7 in
        let render path row column message =
          match valid_path path with
          | Some path -> Some (match column with
              | Some column ->
                  Printf.sprintf "%s:%d:%d: %s" path row column message
              | None -> Printf.sprintf "%s:%d: %s" path row message)
          | None -> None in
        let parenthetical line =
          match find_from line 0 ": (" with
          | None -> None
          | Some at ->
              (match find_from line (at + 3) ", " with
              | None -> None
              | Some comma ->
                  (match find_from line (comma + 2) "): " with
                  | None -> None
                  | Some stop ->
                      let path = slice line 0 at in
                      let path = if String.starts_with ~prefix:"file:///" path
                        then decode_uri path else Some path in
                      match path, positive (slice line (at + 3) comma),
                        positive (slice line (comma + 2) stop) with
                      | Some path, Some row, Some column ->
                          render path row (Some column)
                            (slice line (stop + 3) (String.length line))
                      | _ -> None)) in
        let colon_location line =
          match find_from line 0 ": error: " with
          | None -> None
          | Some stop ->
              let prefix = slice line 0 stop in
              let message = slice line (stop + 9) (String.length line) in
              (try
                 let last = String.rindex prefix ':' in
                 let final = slice prefix (last + 1) (String.length prefix) in
                 match positive final with
                 | None -> None
                 | Some final ->
                     (try
                        let before = String.rindex_from prefix (last - 1) ':' in
                        let row = slice prefix (before + 1) last in
                        match positive row with
                        | Some row -> render (slice prefix 0 before) row
                            (Some final) message
                        | None -> render (slice prefix 0 last) final None message
                      with Not_found | Invalid_argument _ ->
                        render (slice prefix 0 last) final None message)
               with Not_found -> None) in
        let parse line =
          if String.length line > 4096 || not (no_controls line) then None
          else if String.starts_with ~prefix:"e: " line then
            let location = slice line 3 (String.length line) in
            if String.starts_with ~prefix:"file:///" location then
              parenthetical location
            else match parenthetical location with
              | Some _ as parsed -> parsed
              | None -> colon_location location
          else colon_location line in
        let seen = Hashtbl.create max_locations in
        let found = ref [] and count = ref 0 in
        let length = String.length output in
        let rec scan start i =
          if i = length || output.[i] = '\n' then (
            if i - start <= 4096 && !count < max_locations then
              (match parse (slice output start i) with
               | Some location when not (Hashtbl.mem seen location) ->
                   Hashtbl.add seen location ();
                   found := location :: !found;
                   incr count
               | _ -> ());
            if i < length && !count < max_locations then scan (i + 1) (i + 1))
          else scan start (i + 1) in
        scan 0 0;
        List.rev !found
  with Unix.Unix_error _ | Workspace_path.Error _ | Invalid_argument _ -> []
