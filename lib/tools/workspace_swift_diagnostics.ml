(* Locations are hints over an executed command's bounded output, never authority to edit. *)
let locations ?(within_cwd = false) ~root ~cwd output =
  let root = Workspace_path.root_path root in
  let max_lines = 16 in
  let valid_path path =
    try
      let absolute = if Filename.is_relative path then Filename.concat cwd path else path in
      let canonical = Unix.realpath absolute in
      if not (Workspace_path.within root canonical) ||
         (within_cwd && not (Workspace_path.within cwd canonical)) ||
         not (Filename.check_suffix canonical ".swift") ||
         (Unix.lstat absolute).Unix.st_kind <> Unix.S_REG ||
         (Unix.stat canonical).Unix.st_kind <> Unix.S_REG then None
      else Some (String.sub canonical (String.length root + 1)
        (String.length canonical - String.length root - 1))
    with Unix.Unix_error _ | Invalid_argument _ -> None in
  let positive text = match int_of_string_opt text with
    | Some n when n > 0 && n <= 1_000_000 -> Some n
    | _ -> None in
  let parse line =
    let line = String.trim line in
    (* Swift diagnostics end their source location with :line:column: error: .
       Parse from the right so absolute paths and filenames containing ':' stay data. *)
    let marker = ": error: " in
    let rec marker_at i =
      if i + String.length marker > String.length line then None
      else if String.sub line i (String.length marker) = marker then Some i
      else marker_at (i + 1) in
    match marker_at 0 with
    | None -> None
    | Some stop ->
        let prefix = String.sub line 0 stop in
        (try
           let column_sep = String.rindex prefix ':' in
           let column = String.sub prefix (column_sep + 1)
             (String.length prefix - column_sep - 1) in
           let line_sep = String.rindex_from prefix (column_sep - 1) ':' in
           let row = String.sub prefix (line_sep + 1)
             (column_sep - line_sep - 1) in
           let path = String.sub prefix 0 line_sep in
           match positive row, positive column, valid_path path with
           | Some row, Some column, Some path ->
               Some (Printf.sprintf "%s:%d:%d: %s" path row column
                 (String.sub line (stop + String.length marker)
                    (String.length line - stop - String.length marker)))
           | _ -> None
         with Not_found | Invalid_argument _ -> None) in
  let seen = Hashtbl.create max_lines in
  let found = ref [] in
  String.split_on_char '\n' output |> List.iter (fun line ->
    if List.length !found < max_lines && String.length line <= 4096 &&
       not (String.exists (fun c -> Char.code c < 32 || Char.code c = 127) line) then
      match parse line with
      | Some location when not (Hashtbl.mem seen location) ->
          Hashtbl.add seen location ();
          found := location :: !found
      | _ -> ());
  List.rev !found
