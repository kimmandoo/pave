(* Diagnostic locations are hints from bounded command output, not authority to edit. *)
let locations ~root ~cwd ~subroot output =
  let root = Workspace_path.root_path root in
  let max_locations = 16 in
  let prefix text prefix = String.starts_with ~prefix text in
  let prohibited_component = function
    | ".dart_tool" | ".pub-cache" | "build" | "generated" | "gen"
    | "node_modules" | "packages" | "vendor" -> true
    | _ -> false in
  let generated_file name =
    List.exists (Filename.check_suffix name)
      [".g.dart"; ".freezed.dart"; ".gr.dart"; ".gen.dart"; ".mocks.dart"] in
  let no_controls text =
    not (String.exists (fun c -> Char.code c < 32 || Char.code c = 127) text) in
  let components text =
    let parts = String.split_on_char '/' text in
    if List.exists (fun part -> part = "" || part = "." || part = "..") parts
    then None else Some parts in
  let selected =
    let relative = if subroot = "" || subroot = "." then "" else subroot in
    if not (Filename.is_relative relative) || not (no_controls relative) then None
    else match (if relative = "" then Some [] else components relative) with
      | None -> None
      | Some parts ->
          (try
             let path = List.fold_left (fun parent part ->
               let path = Filename.concat parent part in
               if (Unix.lstat path).Unix.st_kind <> Unix.S_DIR then raise Not_found;
               path) root parts in
             if Workspace_path.within root (Unix.realpath path) &&
                (Unix.realpath path) = path then Some path else None
           with Unix.Unix_error _ | Not_found -> None) in
  let cwd = try Some (Unix.realpath cwd) with Unix.Unix_error _ -> None in
  let valid_path path =
    match selected, cwd with
    | Some selected, Some cwd when cwd = selected && path <> "" && no_controls path ->
        let path = if prefix path "./" then String.sub path 2 (String.length path - 2)
          else path in
        let absolute = if Filename.is_relative path then Filename.concat cwd path else path in
        if absolute = selected || not (Workspace_path.within selected absolute) then None
        else
          let relative_to base =
            let start = String.length base + (if base = "/" then 0 else 1) in
            String.sub absolute start (String.length absolute - start) in
          let relative = relative_to root in
          (match components (relative_to selected), components relative with
           | Some selected_parts, Some parts
             when not (List.exists prohibited_component selected_parts) &&
                  not (generated_file (List.hd (List.rev parts))) &&
                  Filename.check_suffix absolute ".dart" ->
               (try
                  (* Workspace_path verifies the canonical workspace boundary;
                     lstat every component to exclude internal symlink aliases. *)
                  let checked = Workspace_path.regular_path root relative in
                  let _ = List.fold_left (fun parent part ->
                    let path = Filename.concat parent part in
                    if (Unix.lstat path).Unix.st_kind = Unix.S_LNK then raise Not_found;
                    path) root parts in
                  let _ = List.fold_left (fun parent part ->
                    let path = Filename.concat parent part in
                    if path <> absolute && Sys.file_exists
                         (Filename.concat path "pubspec.yaml") then raise Not_found;
                    path) selected selected_parts in
                  if checked <> absolute then None else Some relative
                with Workspace_path.Error _ | Unix.Unix_error _ | Not_found -> None)
           | _ -> None)
    | _ -> None in
  let decimal number =
    number <> "" && String.for_all (fun c -> c >= '0' && c <= '9') number in
  let positive number = if not (decimal number) then None else
    match int_of_string_opt number with
    | Some n when n > 0 && n <= 1_000_000 -> Some n
    | _ -> None in
  let location path row column message =
    match positive row, positive column, valid_path path with
    | Some row, Some column, Some path ->
        Some (Printf.sprintf "%s:%d:%d%s" path row column
          (if message = "" then "" else ": " ^ message))
    | _ -> None in
  let from_colons text =
    try
      let column_sep = String.rindex text ':' in
      let line_sep = String.rindex_from text (column_sep - 1) ':' in
      let path = String.sub text 0 line_sep in
      let row = String.sub text (line_sep + 1) (column_sep - line_sep - 1) in
      let column = String.sub text (column_sep + 1) (String.length text - column_sep - 1) in
      Some (path, row, column)
    with Not_found | Invalid_argument _ -> None in
  let split_bullets line =
    let separator = " • " in
    let rec loop start fields =
      let rec find i =
        if i + String.length separator > String.length line then None
        else if String.sub line i (String.length separator) = separator then Some i
        else find (i + 1) in
      match find start with
      | None -> List.rev (String.sub line start (String.length line - start) :: fields)
      | Some stop ->
          loop (stop + String.length separator)
            (String.sub line start (stop - start) :: fields) in
    loop 0 [] in
  let parse_machine line =
    let rec take count start fields =
      if count = 0 then Some (List.rev (String.sub line start (String.length line - start) :: fields))
      else match String.index_from_opt line start '|' with
        | None -> None
        | Some stop ->
            take (count - 1) (stop + 1)
              (String.sub line start (stop - start) :: fields) in
    match take 7 0 [] with
    | Some ["ERROR"; kind; code; path; row; column; length; message]
      when kind <> "" && code <> "" && message <> "" ->
        (if not (decimal length) then None else
         match int_of_string_opt length with
         | Some n when n >= 0 && n <= 1_000_000 -> location path row column message
         | _ -> None)
    | _ -> None in
  let parse_human line =
    match split_bullets line with
    | ["error"; message; position; code] when message <> "" && code <> "" ->
        (match from_colons position with
         | Some (path, row, column) -> location path row column message
         | None -> None)
    | _ -> None in
  let parse_widget line =
    (* Accept either an explicit path:line:column: message, or a Dart path
       followed by a line:column stack token and optional frame context. *)
    let rec message_separator i =
      if i + 2 > String.length line then None
      else if String.sub line i 2 = ": " then Some i
      else message_separator (i + 1) in
    let with_message = match message_separator 0 with
      | None -> None
      | Some stop ->
          (match from_colons (String.sub line 0 stop) with
           | Some (path, row, column) when Filename.check_suffix path ".dart" &&
                                          stop + 2 < String.length line ->
               location path row column
                 (String.sub line (stop + 2) (String.length line - stop - 2))
           | _ -> None) in
    match with_message with
    | Some _ -> with_message
    | None ->
        let tokens = String.split_on_char ' ' line |> List.filter ((<>) "") in
        let tokens = match tokens with
          | frame :: rest when String.length frame > 1 && frame.[0] = '#' &&
              String.for_all (fun c -> c >= '0' && c <= '9')
                (String.sub frame 1 (String.length frame - 1)) -> rest
          | tokens -> tokens in
        match tokens with
        | path :: position :: _ when Filename.check_suffix path ".dart" ->
            (match String.split_on_char ':' position with
             | [row; column] -> location path row column ""
             | _ -> None)
        | _ -> None in
  let found = ref [] and seen = Hashtbl.create max_locations in
  let output = if String.length output > 65_536 then String.sub output 0 65_536 else output in
  String.split_on_char '\n' output |> List.iter (fun line ->
    let line = if String.ends_with ~suffix:"\r" line then
        String.sub line 0 (String.length line - 1) else line in
    if List.length !found < max_locations && String.length line <= 4096 &&
       no_controls line then (
      let line = String.trim line in
      let parsed = if prefix line "ERROR|" then parse_machine line
        else if prefix line "error • " then parse_human line
        else parse_widget line in
      match parsed with
      | Some value when not (Hashtbl.mem seen value) ->
          Hashtbl.add seen value ();
          found := value :: !found
      | _ -> ()));
  List.rev !found
