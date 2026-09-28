type reference = {
  start : int;
  stop : int;
  path : string;
  quote : char option;
}

type expansion = {
  prompt : string;
  attachments : Protocol.attachment list;
  attachment_names : string list;
}

type completion_context = {
  start : int;
  stop : int;
  prefix : string;
  quote : char option;
}

type candidate = { path : string; is_directory : bool }
type listing = { candidates : candidate list; truncated : bool }

let max_references = 32
let max_expanded_text_bytes = 1_048_576
let max_candidates = 100

let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false
let is_punctuation = function ',' | ';' | ':' | '!' | '?' | ')' | ']' | '}' -> true | _ -> false
let is_quote = function '\'' | '"' -> true | _ -> false

let reference_boundary text index =
  index = 0 || is_space text.[index - 1] ||
  match text.[index - 1] with '(' | '[' | '{' | ',' | ';' | ':' -> true | _ -> false

let decode_quoted text start stop =
  let output = Buffer.create (stop - start) in
  let rec loop index =
    if index < stop then
      if text.[index] = '\\' && index + 1 < stop then (
        Buffer.add_char output text.[index + 1];
        loop (index + 2))
      else (Buffer.add_char output text.[index]; loop (index + 1)) in
  loop start;
  Buffer.contents output

let line_end text start =
  try String.index_from text start '\n' with Not_found -> String.length text

let parse_reference text start =
  let length = String.length text in
  if start >= length || text.[start] <> '@' ||
     not (reference_boundary text start) then None
  else if start + 1 < length && is_quote text.[start + 1] then (
    let quote = text.[start + 1] in
    let stop = line_end text (start + 2) in
    let rec close index escaped =
      if index >= stop then None
      else if escaped then close (index + 1) false
      else if text.[index] = '\\' then close (index + 1) true
      else if text.[index] = quote then Some index
      else close (index + 1) false in
    match close (start + 2) false with
    | None -> None
    | Some finish ->
        let path = decode_quoted text (start + 2) finish in
        if path = "" || String.exists (fun c -> Char.code c < 32) path then None
        else Some { start; stop = finish + 1; path; quote = Some quote })
  else (
    let rec finish index =
      if index < length && not (is_space text.[index]) &&
         not (is_punctuation text.[index]) then finish (index + 1)
      else index in
    let stop = ref (finish (start + 1)) in
    while !stop > start + 1 && text.[!stop - 1] = '.' do decr stop done;
    let stop = !stop in
    if stop = start + 1 then None
    else
      let path = String.sub text (start + 1) (stop - start - 1) in
      Some { start; stop; path; quote = None })

let fence_marker text start stop =
  let index = ref start in
  while !index < stop && !index - start < 4 && text.[!index] = ' ' do
    incr index
  done;
  if !index - start > 3 || !index >= stop ||
     (text.[!index] <> '`' && text.[!index] <> '~') then None
  else
    let marker = text.[!index] in
    let first = !index in
    while !index < stop && text.[!index] = marker do incr index done;
    let count = !index - first in
    if count < 3 then None else Some (marker, count, !index)

let only_spaces text start stop =
  let rec scan index =
    index >= stop ||
    ((text.[index] = ' ' || text.[index] = '\t') && scan (index + 1)) in
  scan start

let closing_ticks text start stop count =
  let rec scan index =
    if index >= stop then None
    else if text.[index] <> '`' then scan (index + 1)
    else
      let finish = ref index in
      while !finish < stop && text.[!finish] = '`' do incr finish done;
      if !finish - index = count then Some !finish else scan !finish in
  scan start

let references text =
  let length = String.length text in
  let found = ref [] and inline_end = ref 0 in
  let scan_line start stop =
    let rec scan index =
      if index >= stop then ()
      else if index < !inline_end then scan (min stop !inline_end)
      else if text.[index] = '`' then (
        let finish = ref index in
        while !finish < stop && text.[!finish] = '`' do incr finish done;
        let count = !finish - index in
        (match closing_ticks text !finish length count with
         | Some close -> inline_end := close; scan (min stop close)
         | None -> scan !finish))
      else if text.[index] = '@' then
        (match parse_reference text index with
         | Some reference ->
             found := reference :: !found;
             scan (max (index + 1) reference.stop)
         | None -> scan (index + 1))
      else scan (index + 1) in
    scan start in
  let rec lines start fenced =
    if start < length then (
      let stop = line_end text start in
      let next = if stop < length then stop + 1 else length in
      let marker = if !inline_end > start then None
        else fence_marker text start stop in
      match fenced, marker with
      | Some (fence_char, fence_count), Some (char, count, marker_end)
        when char = fence_char && count >= fence_count &&
             only_spaces text marker_end stop ->
          lines next None
      | Some _, _ -> lines next fenced
      | None, Some (char, count, _) -> lines next (Some (char, count))
      | None, None ->
          scan_line start stop;
          lines next None) in
  lines 0 None;
  List.rev !found

let inside_fenced_code text cursor =
  let length = String.length text in
  let rec lines start fenced =
    if start >= length then Option.is_some fenced
    else
      let stop = line_end text start in
      let next = if stop < length then stop + 1 else length in
      let marker = fence_marker text start stop in
      let in_code, next_fenced = match fenced, marker with
        | Some (fence_char, fence_count), Some (char, count, marker_end)
          when char = fence_char && count >= fence_count &&
               only_spaces text marker_end stop -> true, None
        | Some _, _ -> true, fenced
        | None, Some (char, count, _) -> true, Some (char, count)
        | None, None -> false, None in
      if cursor <= stop || cursor < next then in_code
      else lines next next_fenced in
  lines 0 None

let missing_path = function
  | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> true
  | _ -> false

let text_file_block path text =
  let contents = if text = "" || text.[String.length text - 1] = '\n'
    then text else text ^ "\n" in
  "[Attached text file: " ^ path ^ "]\n" ^ contents ^
  "[End attached text file: " ^ path ^ "]"

let expand ~root prompt =
  let refs = references prompt in
  if List.length refs > max_references then
    invalid_arg (Printf.sprintf "a prompt may reference at most %d files"
      max_references);
  let output = Buffer.create (String.length prompt) in
  let cursor = ref 0 and text_bytes = ref 0 in
  let attachments = ref [] and attachment_names = ref [] in
  let media_paths = Hashtbl.create 8 in
  List.iter (fun (reference : reference) ->
    Buffer.add_substring output prompt !cursor (reference.start - !cursor);
    let original = String.sub prompt reference.start
      (reference.stop - reference.start) in
    let loaded = try
      Some (Session_attachment.load_reference ~root reference.path)
    with exn when missing_path exn -> None in
    (match loaded with
     | None -> Buffer.add_string output original
     | Some (Session_attachment.Text text) ->
         if String.length text > max_expanded_text_bytes - !text_bytes then
           invalid_arg (Printf.sprintf
             "attached text files exceed the %d-byte aggregate limit"
             max_expanded_text_bytes);
         text_bytes := !text_bytes + String.length text;
         Buffer.add_string output (text_file_block reference.path text)
     | Some (Session_attachment.Media attachment) ->
         Buffer.add_string output original;
         if not (Hashtbl.mem media_paths reference.path) then (
           Hashtbl.add media_paths reference.path ();
           attachments := attachment :: !attachments;
           attachment_names := attachment.name :: !attachment_names));
    cursor := reference.stop) refs;
  Buffer.add_substring output prompt !cursor (String.length prompt - !cursor);
  { prompt = Buffer.contents output; attachments = List.rev !attachments;
    attachment_names = List.rev !attachment_names }


let completion_context text cursor =
  let length = String.length text in
  if cursor < 0 || cursor > length || inside_fenced_code text cursor then None
  else
    let token_start = ref 0 and in_code = ref false and quote = ref None in
    let index = ref 0 in
    while !index < cursor do
      let char = text.[!index] in
      if char = '`' && !quote = None then (
        let run_end = ref !index in
        while !run_end < cursor && text.[!run_end] = '`' do incr run_end done;
        let count = !run_end - !index in
        match closing_ticks text !run_end length count with
        | Some closing ->
            if closing > cursor then in_code := true;
            index := min cursor closing
        | None ->
            token_start := !run_end;
            index := !run_end)
      else if not !in_code then
        (match !quote with
         | Some _ when char = '\n' ->
             quote := None; token_start := !index + 1; incr index
         | Some _ when char = '\\' && !index + 1 < length &&
                       text.[!index + 1] <> '\n' ->
             index := min cursor (!index + 2)
         | Some delimiter when char = delimiter ->
             quote := None; incr index
         | Some _ -> incr index
         | None when is_space char ->
             token_start := !index + 1; incr index
         | None when (match char with
             | ',' | ';' | ':' -> true
             | '(' | '[' | '{' -> text.[!token_start] <> '@'
             | _ -> false) ->
             token_start := !index + 1; incr index
         | None when is_quote char && !index = !token_start + 1 &&
                    text.[!token_start] = '@' ->
             quote := Some char; incr index
         | None -> incr index)
      else incr index
    done;
    let start = !token_start in
    if !in_code || start >= length || text.[start] <> '@' ||
       not (reference_boundary text start) || cursor < start + 1 then None
    else
      let quote_char = if start + 1 < length && is_quote text.[start + 1]
        then Some text.[start + 1] else None in
      let path_start = start + (if quote_char = None then 1 else 2) in
      match quote_char with
      | Some _ when cursor < path_start -> None
      | Some delimiter ->
          let stop_line = line_end text path_start in
          let rec close index escaped =
            if index >= stop_line then None
            else if escaped then close (index + 1) false
            else if text.[index] = '\\' then close (index + 1) true
            else if text.[index] = delimiter then Some index
            else close (index + 1) false in
          let closing = close path_start false in
          let stop = match closing with
            | Some position -> position + 1
            | None -> stop_line in
          if cursor > stop then None
          else
            let prefix_end = min cursor (Option.value ~default:stop closing) in
            Some { start; stop;
              prefix = decode_quoted text path_start prefix_end;
              quote = quote_char }
      | None ->
          let rec finish index =
            if index < length && not (is_space text.[index]) &&
               not (is_punctuation text.[index]) then finish (index + 1)
            else index in
          let stop = finish path_start in
          if cursor > stop then None
          else Some { start; stop;
            prefix = String.sub text path_start (cursor - path_start);
            quote = None }

let has_parent_component path =
  List.exists (( = ) "..") (String.split_on_char '/' path)

let complete_paths ~root prefix =
  if prefix <> "" &&
     (not (Filename.is_relative prefix) || has_parent_component prefix) then
    { candidates = []; truncated = false }
  else
    let root = Workspace_path.root_path root in
    let directory, partial =
      if prefix = "" || String.ends_with ~suffix:"/" prefix then
        (if prefix = "" then "." else String.sub prefix 0
          (String.length prefix - 1)), ""
      else
        Filename.dirname prefix, Filename.basename prefix in
    let directory = if directory = "" then "." else directory in
    let found = ref [] and overflow = ref false in
    let add path is_directory =
      if Session_attachment.valid_utf8 path &&
         Filename.dirname path = directory &&
         String.starts_with ~prefix:partial (Filename.basename path) then
        if List.length !found < max_candidates then
          found := { path; is_directory } :: !found
        else overflow := true in
    let walked = try
      if (Unix.lstat (Workspace_path.checked_path root directory)).Unix.st_kind
         <> Unix.S_DIR then false
      else Tools.walk ~hidden:false
        ~visit_directory:(fun path _ -> add path true)
        root directory (fun path _ -> add path false)
    with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> false in
    { candidates = List.sort (fun left right ->
        String.compare left.path right.path) !found;
      truncated = walked || !overflow }

let render_reference ?quote ~directory path =
  let needs_quote = quote <> None ||
    String.exists (fun char -> is_space char || is_punctuation char ||
      is_quote char) path ||
    (path <> "" && path.[String.length path - 1] = '.') in
  if not needs_quote then "@" ^ path ^ (if directory then "/" else "")
  else
    let quote_char = Option.value quote ~default:'"' in
    let escaped = Buffer.create (String.length path + 8) in
    String.iter (fun char ->
      if char = quote_char || char = '\\' then Buffer.add_char escaped '\\';
      Buffer.add_char escaped char) path;
    let escaped = Buffer.contents escaped in
    "@" ^ String.make 1 quote_char ^ escaped ^
      (if directory then "/" else String.make 1 quote_char)
