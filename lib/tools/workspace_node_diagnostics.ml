(* Output locations are hints from a failed command, not proof of its exit status. *)
let locations ~root ~cwd ~subroot output =
  let root = Workspace_path.root_path root in
  let max_locations = 16 in
  let forbidden_directory = function
    | "node_modules" | "build" | "dist" | "coverage" | "out" | "generated"
    | "__generated__" | "gen" | ".expo" | ".next" | ".turbo" | ".cache"
    | "_build" | ".build" | ".git" -> true
    | _ -> false in
  let components path = String.split_on_char '/' path in
  let safe_components path =
    let parts = components path in
    not (List.exists (fun part -> part = ".." || forbidden_directory part) parts) in
  let source_file path =
    List.exists (Filename.check_suffix path)
      [".js"; ".jsx"; ".mjs"; ".cjs"; ".ts"; ".tsx"; ".mts"; ".cts"] in
  let rec no_symlinks base = function
    | [] -> true
    | "" :: parts | "." :: parts -> no_symlinks base parts
    | part :: parts ->
        let path = Filename.concat base part in
        (Unix.lstat path).Unix.st_kind <> Unix.S_LNK &&
        no_symlinks path parts in
  let selected =
    try
      if subroot <> "" && not (Filename.is_relative subroot) then None
      else if not (safe_components subroot) then None
      else
        let path = if subroot = "" || subroot = "." then root
          else Workspace_path.checked_path root subroot in
        if (Unix.stat path).Unix.st_kind <> Unix.S_DIR ||
           not (no_symlinks root (components subroot)) then None
        else Some (Unix.realpath path)
    with Workspace_path.Error _ | Unix.Unix_error _ | Invalid_argument _ -> None in
  let cwd =
    try
      let canonical = Unix.realpath cwd in
      match selected with
      | Some project when Workspace_path.within project canonical &&
          (Unix.stat canonical).Unix.st_kind = Unix.S_DIR -> Some canonical
      | _ -> None
    with Unix.Unix_error _ | Invalid_argument _ -> None in
  let valid_path path =
    match selected, cwd with
    | Some project, Some cwd when path <> "" && source_file path &&
        safe_components path && not (String.contains path ':') ->
        (try
           let absolute = if Filename.is_relative path then Filename.concat cwd path
             else path in
           if not (Workspace_path.within project absolute) ||
              not (no_symlinks project
                (components (String.sub absolute (String.length project + 1)
                   (String.length absolute - String.length project - 1)))) ||
              (Unix.lstat absolute).Unix.st_kind <> Unix.S_REG then None
           else
             let canonical = Unix.realpath absolute in
             if not (Workspace_path.within project canonical) ||
                not (Workspace_path.within root canonical) ||
                (Unix.stat canonical).Unix.st_kind <> Unix.S_REG then None
             else Some (String.sub canonical (String.length root + 1)
               (String.length canonical - String.length root - 1))
         with Unix.Unix_error _ | Invalid_argument _ -> None)
    | _ -> None in
  let positive text = match int_of_string_opt text with
    | Some n when n > 0 && n <= 1_000_000 -> Some n
    | _ -> None in
  let span text start stop = String.sub text start (stop - start) in
  let parse_coordinates text ~start ~stop =
    try
      let column_sep = String.rindex_from text (stop - 1) ':' in
      let row = positive (span text start column_sep) in
      let column = positive (span text (column_sep + 1) stop) in
      match row, column with
      | Some row, Some column -> Some (row, column)
      | _ -> None
    with Not_found | Invalid_argument _ -> None in
  let colon_location text =
    try
      let column_sep = String.rindex text ':' in
      let row_sep = String.rindex_from text (column_sep - 1) ':' in
      match parse_coordinates text ~start:(row_sep + 1)
        ~stop:(String.length text) with
      | Some (row, column) -> Some (span text 0 row_sep, row, column)
      | None -> None
    with Not_found | Invalid_argument _ -> None in
  let paren_location text =
    let length = String.length text in
    if length = 0 || text.[length - 1] <> ')' then None
    else try
      let open_paren = String.rindex text '(' in
      if open_paren = 0 || text.[open_paren - 1] <> ' ' then None
      else match parse_coordinates text ~start:(open_paren + 1)
          ~stop:(length - 1) with
        | Some (row, column) ->
            Some (span text 0 (open_paren - 1), row, column)
        | None -> None
    with Not_found | Invalid_argument _ -> None in
  let parse line =
    let line = String.trim line in
    let length = String.length line in
    let candidate =
      if length >= 3 && String.sub line 0 3 = "at " then (
        let frame = span line 3 length in
        let frame_length = String.length frame in
        if frame_length > 0 && frame.[frame_length - 1] = ')' then
          match paren_location frame with
          | Some _ as found -> found
          | None ->
              (try
                 let start = String.rindex frame '(' in
                 colon_location (span frame (start + 1) (frame_length - 1))
               with Not_found -> None)
        else colon_location frame)
      else match colon_location line with
        | Some _ as found -> found
        | None -> paren_location line in
    match candidate with
    | Some (path, row, column) ->
        Option.map (fun relative -> Printf.sprintf "%s:%d:%d" relative row column)
          (valid_path path)
    | None -> None in
  let seen = Hashtbl.create max_locations in
  let found = ref [] in
  let rec scan start =
    if start < String.length output && List.length !found < max_locations then (
      let stop = match String.index_from_opt output start '\n' with
        | Some index -> index | None -> String.length output in
      let length = stop - start in
      if length <= 4096 then (
        let line = span output start stop in
        if not (String.exists (fun c -> Char.code c < 32 || Char.code c = 127) line) then
          match parse line with
          | Some location when not (Hashtbl.mem seen location) ->
              Hashtbl.add seen location ();
              found := location :: !found
          | _ -> ());
      scan (stop + 1)) in
  scan 0;
  List.rev !found
