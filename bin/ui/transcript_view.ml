(* UI-only transcript. History is supplied explicitly; these rows are never journaled. *)
type kind = User | Assistant | Tool | Notice | Error | Approval
type style =
  Heading | Text | Code | Quote | List_item | Subheading | Tool_state
  | Divider | Table_header | Table_row | Table_separator
  | Diff_header | Diff_hunk | Diff_add | Diff_remove | Diff_context | Diff_meta
type inline_style = Plain | Bold | Inline_code | Link
type inline_run = { content : string; style : inline_style }

type row = {
  mutable kind : kind;
  mutable style : style;
  mutable text : string;
  mutable runs : inline_run array;
  mutable provisional : bool;
  group : int;
  detail : bool;
  preview : bool;
}

type visual = {
  source : int;
  row : row;
  text : string;
  continuation : bool;
  runs : inline_run array;
}
type wrapped_segment = { rendered : string; start_byte : int; end_byte : int }
type entry = { source : int; row : row; start : int; length : int }
type snapshot = {
  entries : entry array;
  total : int;
  columns : int;
  measure : string -> int;
  mutable recent : (int * wrapped_segment array) option;
}

type t = {
  mutable rows : row array;
  mutable count : int;
  mutable next_group : int;
  expanded : (int, bool) Hashtbl.t;
  mutable pending_tool : (string * int) option;
  mutable live : string;
  mutable streaming : bool;
  mutable fenced : bool;
  mutable diff_fenced : bool;
  mutable diff_raw : bool;
  mutable table_active : bool;
  mutable revision : int;
  mutable cached : snapshot option;
  mutable dirty : int;
}

let max_rows = 10_000
let max_line_bytes = 4096
let max_tool_lines = 3000
let blank = { kind = Notice; style = Text; text = ""; runs = [||];
  provisional = false; group = 0; detail = false; preview = false }

let create () = { rows = [||]; count = 0; next_group = 1;
  expanded = Hashtbl.create 32; pending_tool = None; live = "";
  streaming = false; fenced = false; diff_fenced = false; diff_raw = false;
  table_active = false; revision = 0;
  cached = None; dirty = 0 }

let sanitize text =
  let buffer = Buffer.create (String.length text) in
  let append () _ = function
    | `Malformed _ -> Buffer.add_utf_8_uchar buffer Uutf.u_rep
    | `Uchar uchar ->
        let code = Uchar.to_int uchar in
        if code = 10 then Buffer.add_char buffer '\n'
        else if code = 9 then Buffer.add_char buffer ' '
        else if code < 32 || (code >= 127 && code <= 159) ||
          code = 0x61c || code = 0x200e || code = 0x200f ||
          (code >= 0x202a && code <= 0x202e) ||
          (code >= 0x2066 && code <= 0x2069) then Buffer.add_char buffer ' '
        else Buffer.add_utf_8_uchar buffer uchar in
  ignore (Uutf.String.fold_utf_8 append () text);
  Buffer.contents buffer

let single_line text =
  String.map (function '\n' -> ' ' | char -> char) (sanitize text)

let fit text =
  if String.length text <= max_line_bytes then text else (
    let size = ref max_line_bytes in
    while !size > 0 && Char.code text.[!size] land 0xc0 = 0x80 do decr size done;
    String.sub text 0 !size ^ "…")

let group t = let id = t.next_group in t.next_group <- id + 1; id
let mark_dirty t index = t.dirty <- min t.dirty index

let add t row =
  if t.count = max_rows then (
    mark_dirty t 0;
    let removed = 1000 in
    Array.blit t.rows removed t.rows 0 (t.count - removed);
    Array.fill t.rows (t.count - removed) removed blank;
    t.count <- t.count - removed;
    (* Group IDs increase monotonically; trimming cannot retain an older ID. *)
    let first = t.rows.(0).group in
    Hashtbl.filter_map_inplace (fun id expanded ->
      if id < first then None else Some expanded) t.expanded);
  if t.count = Array.length t.rows then (
    let grown = Array.make (min max_rows (max 128 (2 * t.count))) blank in
    Array.blit t.rows 0 grown 0 t.count;
    t.rows <- grown);
  mark_dirty t t.count;
  t.rows.(t.count) <- row;
  t.count <- t.count + 1;
  t.revision <- t.revision + 1

let plain_runs text =
  if text = "" then [||] else [| { content = text; style = Plain } |]
let set_text (row : row) text =
  row.text <- text;
  row.runs <- plain_runs text

let inline_markdown text =
  let length = String.length text in
  let runs : inline_run list ref = ref [] and plain = Buffer.create length in
  let push style text =
    if text <> "" then
      match !runs with
      | previous :: rest when previous.style = style ->
          runs := { content = previous.content ^ text; style } :: rest
      | _ -> runs := { content = text; style } :: !runs in
  let flush () =
    if Buffer.length plain > 0 then (
      push Plain (Buffer.contents plain);
      Buffer.clear plain) in
  let starts marker index =
    let size = String.length marker in
    index >= 0 && index + size <= length &&
    (let rec equal offset =
       offset = size ||
       (text.[index + offset] = marker.[offset] && equal (offset + 1)) in
     equal 0) in
  let find marker start =
    let size = String.length marker in
    let rec loop index =
      if index + size > length then None
      else if starts marker index then Some index
      else loop (index + 1) in
    loop start in
  let rec scan index =
    if index >= length then flush ()
    else if text.[index] = '\\' && index + 1 < length &&
      String.contains "\\`*[]()|" text.[index + 1] then (
      Buffer.add_char plain text.[index + 1];
      scan (index + 2))
    else if starts "**" index || starts "__" index then (
      let marker = if text.[index] = '*' then "**" else "__" in
      match find marker (index + 2) with
      | Some close when close > index + 2 ->
          flush ();
          push Bold (String.sub text (index + 2) (close - index - 2));
          scan (close + 2)
      | _ -> Buffer.add_char plain text.[index]; scan (index + 1))
    else if text.[index] = '`' then
      (match find "`" (index + 1) with
       | Some close when close > index + 1 ->
           flush ();
           push Inline_code (String.sub text (index + 1) (close - index - 1));
           scan (close + 1)
       | _ -> Buffer.add_char plain text.[index]; scan (index + 1))
    else if text.[index] = '[' then
      (match find "](" (index + 1) with
       | Some middle ->
           (match String.index_from_opt text (middle + 2) ')' with
            | Some close when middle > index + 1 && close > middle + 2 ->
                flush ();
                push Link (String.sub text (index + 1) (middle - index - 1));
                push Plain (" (" ^ String.sub text (middle + 2)
                  (close - middle - 2) ^ ")");
                scan (close + 1)
            | _ -> Buffer.add_char plain text.[index]; scan (index + 1))
       | None -> Buffer.add_char plain text.[index]; scan (index + 1))
    else (
      Buffer.add_char plain text.[index];
      scan (index + 1)) in
  scan 0;
  let runs = Array.of_list (List.rev !runs) in
  let visible = Buffer.create length in
  Array.iter (fun (run : inline_run) ->
    Buffer.add_string visible run.content) runs;
  Buffer.contents visible, runs

let table_cells text =
  if not (String.contains text '|') then None
  else
    let cells = List.map String.trim (String.split_on_char '|' text) in
    let cells = match cells with "" :: rest -> rest | _ -> cells in
    let cells = match List.rev cells with "" :: rest -> List.rev rest
      | _ -> cells in
    if List.length cells >= 2 then Some cells else None

let table_separator cells =
  List.length cells >= 2 &&
  List.for_all (fun cell ->
    let dashes = ref 0 and valid = ref true in
    String.iter (function
      | '-' -> incr dashes
      | ':' -> ()
      | _ -> valid := false) cell;
    !valid && !dashes >= 3) cells

let table_row_text cells = String.concat " | " cells
let table_rule cells =
  String.concat "+" (List.map (fun cell ->
    String.make (max 3 (String.length cell)) '-') cells)

let set_row t index ~style ~markdown text =
  let text = fit text in
  let text, runs = if markdown then inline_markdown text
    else text, plain_runs text in
  let row = t.rows.(index) in
  mark_dirty t index;
  row.style <- style;
  row.text <- text;
  row.runs <- runs

let add_line t ~kind ~group ~provisional ?(detail = false)
    ?(preview = false) ?(markdown = false) ?(style = Text) text =
  let text = fit text in
  let text, runs = if markdown then inline_markdown text
    else text, plain_runs text in
  add t { kind; style; text; runs; provisional; group; detail; preview }

let heading t ~kind ~group ~provisional text =
  if t.count > 0 && t.rows.(t.count - 1).style <> Divider then
    add_line t ~kind ~group ~provisional ~style:Divider "";
  t.fenced <- false;
  t.diff_fenced <- false;
  t.diff_raw <- false;
  t.table_active <- false;
  add_line t ~kind ~group ~provisional ~style:Heading text

let markdown_prefix line =
  let length = String.length line in
  let rec hashes n =
    if n < 6 && n < length && line.[n] = '#' then hashes (n + 1)
    else n in
  let count = hashes 0 in
  if count > 0 && count < length && line.[count] = ' ' then
    Subheading, String.sub line (count + 1) (length - count - 1)
  else if line = ">" then Quote, ""
  else if String.starts_with ~prefix:"> " line then
    Quote, String.sub line 2 (length - 2)
  else if String.starts_with ~prefix:"- " line ||
    String.starts_with ~prefix:"* " line then
    List_item, String.sub line 2 (length - 2)
  else Text, line

let display_line ~fenced line =
  if String.starts_with ~prefix:"```" line then
    Code, (if fenced then "end code" else
      let language = String.trim
        (String.sub line 3 (String.length line - 3)) in
      "code" ^ (if language = "" then "" else " · " ^ language))
  else if fenced then Code, line
  else markdown_prefix line

let diff_style line =
  let starts prefix = String.starts_with ~prefix line in
  if starts "diff --git " || starts "--- " || starts "+++ " ||
     starts "Index: " then Some Diff_header
  else if starts "@@ " || starts "@@@ " then Some Diff_hunk
  else if starts "index " || starts "new file mode " ||
          starts "deleted file mode " || starts "old mode " ||
          starts "new mode " || starts "similarity index " ||
          starts "rename from " || starts "rename to " ||
          starts "copy from " || starts "copy to " ||
          starts "Binary files " || starts "GIT binary patch" ||
          starts "\\ No newline at end of file" then Some Diff_meta
  else if line = "" then None
  else match line.[0] with
    | '+' -> Some Diff_add
    | '-' -> Some Diff_remove
    | ' ' -> Some Diff_context
    | _ -> None

let is_diff_style = function
  | Diff_header | Diff_hunk | Diff_add | Diff_remove | Diff_context
  | Diff_meta -> true
  | _ -> false

let content_line t ~kind ~group ~provisional ?(detail = false)
    ?(start_diff = false) line =
  let line = fit line in
  let fence = String.starts_with ~prefix:"```" line in
  let diff = not fence && (t.diff_fenced ||
    (not t.fenced && (t.diff_raw || start_diff ||
      String.starts_with ~prefix:"diff --git " line))) in
  let diff_kind = if diff then diff_style line else None in
  let style, visible = match diff_kind with
    | Some style -> style, line
    | None when diff && t.diff_fenced -> Code, line
    | None -> display_line ~fenced:t.fenced line in
  if fence then (
    if t.fenced then t.diff_fenced <- false
    else t.diff_fenced <- String.equal
      (String.lowercase_ascii (String.trim
        (String.sub line 3 (String.length line - 3)))) "diff";
    t.fenced <- not t.fenced;
    t.diff_raw <- false)
  else if not t.fenced then
    t.diff_raw <- diff && Option.is_some diff_kind;
  if style <> Text then (
    t.table_active <- false;
    add_line t ~kind ~group ~provisional ~detail
      ~markdown:(style <> Code && not (is_diff_style style))
      ~style visible)
  else
    match table_cells line with
    | Some cells when table_separator cells ->
        let previous = if t.count = 0 then None else
            Some (t.count - 1, t.rows.(t.count - 1)) in
        (match previous with
         | Some (index, row) when row.kind = kind && row.group = group &&
             row.style = Text ->
             (match table_cells row.text with
              | Some header when List.length header = List.length cells ->
                  set_row t index ~style:Table_header ~markdown:true
                    (table_row_text header);
                  t.table_active <- true;
                  add_line t ~kind ~group ~provisional ~detail
                    ~style:Table_separator (table_rule cells)
              | _ ->
                  t.table_active <- false;
                  add_line t ~kind ~group ~provisional ~detail ~markdown:true
                    line)
         | _ when t.table_active ->
             add_line t ~kind ~group ~provisional ~detail
               ~style:Table_separator (table_rule cells)
         | _ ->
             t.table_active <- false;
             add_line t ~kind ~group ~provisional ~detail ~markdown:true line)
    | Some cells when t.table_active ->
        add_line t ~kind ~group ~provisional ~detail ~markdown:true
          ~style:Table_row (table_row_text cells)
    | _ ->
        t.table_active <- false;
        add_line t ~kind ~group ~provisional ~detail ~markdown:true line

let add_block t kind title text =
  let id = group t in
  heading t ~kind ~group:id ~provisional:false title;
  let text = sanitize text in
  let last = ref (String.length text) in
  while !last > 0 && text.[!last - 1] = '\n' do decr last done;
  let body = if !last = String.length text then text
    else String.sub text 0 !last in
  String.split_on_char '\n' body
  |> List.iter (content_line t ~kind ~group:id ~provisional:false)

let sent t text = add_block t User "You" text
let assistant t text = add_block t Assistant "Pave" text
let notice t text = add_block t Notice "Note" text
let error t text = add_block t Error "Error" text
let approval ?(title = "SHELL APPROVAL · review before deciding") t text =
  add_block t Approval (single_line title) text

let valid_tool_name name =
  name <> "" && String.length name <= 64 &&
  String.for_all (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' -> true
    | _ -> false) name

let parse_tool text =
  if not (String.starts_with ~prefix:"[" text) then None
  else match String.index_opt text ']' with
  | Some close when close > 1 && close <= 65 ->
      let name = String.sub text 1 (close - 1) in
      if not (valid_tool_name name) then None
      else if close = String.length text - 1 then Some (name, None)
      else if text.[close + 1] = ' ' then
        Some (name, Some (String.sub text (close + 2)
          (String.length text - close - 2)))
      else None
  | _ -> None

let start_tool t name =
  let name = single_line name in
  let id = group t in
  t.pending_tool <- Some (name, id);
  heading t ~kind:Tool ~group:id ~provisional:false
    (name ^ " · running");
  id

let tool_result ?group:existing ?(aborted = false) ?(is_error = false) t name result =
  let name = single_line name in
  let id = match existing, t.pending_tool with
    | Some id, _ -> id
    | None, Some (current, id) when current = name -> id
    | None, _ -> start_tool t name in
  t.pending_tool <- None;
  t.fenced <- false;
  t.diff_fenced <- false;
  t.diff_raw <- false;
  t.table_active <- false;
  let failed = is_error || String.starts_with ~prefix:"Error:" result in
  let error = failed || aborted in
  let outcome = if aborted then "aborted" else if failed then "failed" else "completed" in
  for i = 0 to t.count - 1 do
    let row = t.rows.(i) in
    if row.group = id && row.kind = Tool && row.style = Heading then (
      mark_dirty t i;
      if error then row.kind <- Error;
      set_text row name)
  done;
  let length = String.fold_left (fun count char ->
    if char = '\n' then count + 1 else count) 1 result in
  add_line t ~kind:(if error then Error else Tool) ~group:id
    ~provisional:false ~style:Tool_state
    (Printf.sprintf "%s · %d %s · collapsed" outcome length
      (if length = 1 then "line" else "lines"));
  let position = ref 0 and previewed = ref false in
  let status_preview = ref None in
  let starts_at text index prefix =
    let size = String.length prefix in
    index + size <= String.length text &&
    String.sub text index size = prefix in
  let excerpt text =
    if String.length text <= 160 then text
    else let size = ref 160 in
      while !size > 0 && Char.code text.[!size] land 0xc0 = 0x80 do
        decr size done;
      String.sub text 0 !size ^ "…" in
  for _index = 0 to min (length - 1) (max_tool_lines - 1) do
    let stop = match String.index_from_opt result !position '\n' with
      | Some stop -> stop | None -> String.length result in
    let bytes = stop - !position in
    let line = sanitize (String.sub result !position
      (min bytes max_line_bytes)) in
    let line = if bytes > max_line_bytes then fit (line ^ "…") else fit line in
    let starts_diff = (not t.fenced || t.diff_fenced) &&
      (String.starts_with ~prefix:"diff --git " line ||
       (String.starts_with ~prefix:"--- " line &&
        starts_at result (stop + 1) "+++ ")) in
    if starts_diff then (
      match !status_preview with
      | Some index ->
          set_row t index ~style:Diff_header ~markdown:false (excerpt line);
          status_preview := None
      | None -> ());
    if not !previewed && String.trim line <> "" &&
      not (String.starts_with ~prefix:"```" line) then (
      previewed := true;
      let style, preview =
        if starts_diff || t.diff_raw || t.diff_fenced then
          match diff_style line with
          | Some style -> style, line
          | None -> display_line ~fenced:t.fenced line
        else display_line ~fenced:t.fenced line in
      let index = t.count in
      add_line t ~kind:(if error then Error else Tool) ~group:id
        ~provisional:false ~preview:true ~style
        ~markdown:(style <> Code && not (is_diff_style style))
        (excerpt preview);
      if String.starts_with ~prefix:"Status:" line then
        status_preview := Some index);
    content_line t ~kind:(if error then Error else Tool)
      ~group:id ~provisional:false ~detail:true ~start_diff:starts_diff line;
    position := stop + 1
  done;
  if length > max_tool_lines then
    add_line t ~kind:Notice ~group:id ~provisional:false ~detail:true
      (Printf.sprintf "… %d additional lines omitted (transcript limit)"
        (length - max_tool_lines));
  t.fenced <- false;
  t.diff_fenced <- false;
  t.diff_raw <- false

let flush_live t =
  if t.live <> "" then (
    let id = match t.streaming with
      | true -> t.next_group - 1
      | false -> group t in
    content_line t ~kind:Assistant ~group:id ~provisional:true t.live;
    t.live <- "")

let event t text =
  (* A tool event terminates the current assistant segment, not its provisional
     status. Only successful completion can settle that streamed text. *)
  flush_live t;
  t.streaming <- false;
  match parse_tool text with
  | Some (name, None) -> ignore (start_tool t name)
  | Some (name, Some result) -> tool_result t name result
  | None -> if String.starts_with ~prefix:"Error:" text then error t text
      else notice t text

let delta t chunk =
  let chunk = sanitize chunk in
  if chunk <> "" then (
    if not t.streaming then (
      let id = group t in
      heading t ~kind:Assistant ~group:id ~provisional:true "Pave";
      t.streaming <- true);
    let id = t.next_group - 1 in
    match String.split_on_char '\n' chunk with
    | [] -> ()
    | first :: rest ->
        mark_dirty t t.count;
        t.live <- fit (t.live ^ first);
        List.iter (fun part ->
          content_line t ~kind:Assistant ~group:id ~provisional:true t.live;
          t.live <- fit part) rest;
    t.revision <- t.revision + 1)

let finish t =
  flush_live t;
  t.streaming <- false;
  t.fenced <- false;
  t.diff_fenced <- false;
  t.diff_raw <- false;
  t.table_active <- false;
  while t.count > 0 &&
    (let row = t.rows.(t.count - 1) in
     row.kind = Assistant && row.provisional &&
     row.style = Text && row.text = "") do
    mark_dirty t (t.count - 1);
    t.count <- t.count - 1;
    t.rows.(t.count) <- blank
  done;
  for i = 0 to t.count - 1 do t.rows.(i).provisional <- false done;
  t.revision <- t.revision + 1

let interrupt_tool t name id =
  for i = 0 to t.count - 1 do
    let row = t.rows.(i) in
    if row.group = id && row.kind = Tool && row.style = Heading then (
      mark_dirty t i;
      set_text row (name ^ " · interrupted (outcome unknown)"))
  done;
  t.revision <- t.revision + 1

let rollback t =
  mark_dirty t t.count;
  t.live <- "";
  t.streaming <- false;
  (match t.pending_tool with
  | None -> ()
  | Some (name, id) -> interrupt_tool t name id);
  t.pending_tool <- None;
  t.fenced <- false;
  t.diff_fenced <- false;
  t.diff_raw <- false;
  t.table_active <- false;
  let kept = ref 0 in
  for i = 0 to t.count - 1 do
    if not t.rows.(i).provisional then (
      t.rows.(!kept) <- t.rows.(i); incr kept)
    else mark_dirty t i
  done;
  Array.fill t.rows !kept (t.count - !kept) blank;
  t.count <- !kept;
  t.revision <- t.revision + 1

let clear t =
  mark_dirty t 0;
  t.count <- 0;
  t.pending_tool <- None;
  t.live <- "";
  t.streaming <- false;
  t.fenced <- false;
  t.diff_fenced <- false;
  t.diff_raw <- false;
  t.table_active <- false;
  Hashtbl.clear t.expanded;
  t.revision <- t.revision + 1

let visible t row =
  let expanded = Option.value (Hashtbl.find_opt t.expanded row.group)
    ~default:false in
  (not row.detail || expanded) && (not row.preview || not expanded)

let toggle t ~first:_ ~last =
  let chosen = ref None in
  for i = 0 to t.count - 1 do
    let row = t.rows.(i) in
    if row.style = Tool_state && i <= last then chosen := Some row.group
  done;
  match !chosen with
  | None -> None
  | Some id ->
      let expanded =
        not (Option.value (Hashtbl.find_opt t.expanded id) ~default:false) in
      Hashtbl.replace t.expanded id expanded;
      for i = 0 to t.count - 1 do
        if t.rows.(i).group = id then mark_dirty t i
      done;
      for i = 0 to t.count - 1 do
        let row = t.rows.(i) in
        if row.group = id && row.style = Tool_state &&
          String.ends_with ~suffix:" · collapsed" row.text then
          set_text row (String.sub row.text 0
            (String.length row.text - String.length " · collapsed") ^
            " · expanded")
        else if row.group = id && row.style = Tool_state &&
          String.ends_with ~suffix:" · expanded" row.text then
          set_text row (String.sub row.text 0
            (String.length row.text - String.length " · expanded") ^
            " · collapsed")
      done;
      t.revision <- t.revision + 1;
      Some id

let wrap_lines ~columns ~measure ~on_line ?on_range text =
  let columns = max 1 columns in
  let buffer = Buffer.create (min max_line_bytes columns) in
  let used = ref 0 and break_at = ref None in
  let line_start = ref 0 and source_end = ref 0 in
  let notify range_end =
    on_line buffer;
    Option.iter (fun emit ->
      emit (Buffer.contents buffer) !line_start range_end) on_range;
    line_start := range_end in
  let push () =
    notify !source_end;
    Buffer.clear buffer;
    used := 0;
    break_at := None in
  let add_chunk chunk width =
    if !used > 0 && !used + width > columns then push ();
    if width > columns then (
      Buffer.add_char buffer '?';
      used := !used + 1)
    else (
      Buffer.add_string buffer chunk;
      used := !used + width);
    source_end := !source_end + String.length chunk in
  let split_at byte_count prefix_width prefix_end =
    let content = Buffer.contents buffer in
    let suffix_width = !used - prefix_width in
    let suffix_length = String.length content - byte_count in
    Buffer.clear buffer;
    Buffer.add_substring buffer content 0 byte_count;
    used := prefix_width;
    notify prefix_end;
    Buffer.clear buffer;
    Buffer.add_substring buffer content byte_count suffix_length;
    used := suffix_width;
    break_at := None in
  ignore (Uuseg_string.fold_utf_8 `Grapheme_cluster
    (fun () chunk ->
      let width = max 0 (measure chunk) in
      if !used > 0 && !used + width > columns then
        (match !break_at with
         | Some (byte_count, prefix_width, prefix_end)
           when byte_count < Buffer.length buffer ->
             split_at byte_count prefix_width prefix_end
         | _ -> push ());
      add_chunk chunk width;
      if chunk = " " || chunk = "\t" then
        break_at := Some (Buffer.length buffer, !used, !source_end))
    () text);
  push ()

let wrap ~columns ~measure text =
  let segments = ref [] in
  wrap_lines ~columns ~measure ~on_line:(fun buffer ->
    segments := Buffer.contents buffer :: !segments) text;
  Array.of_list (List.rev !segments)

let wrap_ranges ~columns ~measure text =
  let segments = ref [] in
  wrap_lines ~columns ~measure ~on_line:(fun _ -> ())
    ~on_range:(fun text start_byte end_byte ->
      segments := { rendered = text; start_byte; end_byte } :: !segments) text;
  Array.of_list (List.rev !segments)

let wrapped_count ~columns ~measure text =
  let count = ref 0 in
  wrap_lines ~columns ~measure ~on_line:(fun _ -> incr count) text;
  !count

let visual_runs (row : row) (segment : wrapped_segment) =
  let runs : inline_run list ref = ref [] in
  let position = ref 0 and size = ref 0 in
  Array.iter (fun (run : inline_run) ->
    let start = max segment.start_byte !position in
    let stop = min segment.end_byte (!position + String.length run.content) in
    if start < stop then (
      let text = String.sub run.content (start - !position) (stop - start) in
      runs := { run with content = text } :: !runs;
      size := !size + String.length text);
    position := !position + String.length run.content) row.runs;
  let runs = Array.of_list (List.rev !runs) in
  if !size = String.length segment.rendered then runs
  else
    let style = if Array.length runs = 0 then Plain else runs.(0).style in
    if segment.rendered = "" then [||]
    else [| { content = segment.rendered; style } |]

let snapshot t ~columns ~measure =
  let previous = match t.cached with
    | Some cached when cached.columns = columns && cached.measure == measure ->
        Some cached
    | _ -> None in
  match previous with
  | Some cached when t.dirty = max_int -> cached
  | _ ->
      let old_entries = match previous with
        | Some cached -> cached.entries
        | None -> [||] in
      let prefix, start = match previous with
        | None -> 0, 0
        | Some _ ->
            let low = ref 0 and high = ref (Array.length old_entries) in
            while !low < !high do
              let middle = (!low + !high) / 2 in
              if old_entries.(middle).source < t.dirty then
                low := middle + 1
              else high := middle
            done;
            !low, (if !low = 0 then 0 else
              let entry = old_entries.(!low - 1) in
              entry.start + entry.length) in
      let entries = ref [] and total = ref start in
      let push source (row : row) =
        let length = wrapped_count ~columns ~measure row.text in
        entries := { source; row; start = !total; length } :: !entries;
        total := !total + length in
      let first = if Option.is_some previous then min t.dirty t.count else 0 in
      for i = first to t.count - 1 do
        let row = t.rows.(i) in
        if visible t row then push i row
      done;
      if t.live <> "" then (
        let style, line = display_line ~fenced:t.fenced t.live in
        let style, line = if style <> Text then style, line
          else match table_cells line with
            | Some cells when table_separator cells ->
                Table_separator, table_rule cells
            | Some cells when t.table_active ->
                Table_row, table_row_text cells
            | _ -> Text, line in
        let text, runs = if style = Code || style = Table_separator then
          line, plain_runs line else inline_markdown line in
        push t.count { kind = Assistant; style; text; runs;
          provisional = true; group = t.next_group - 1; detail = false;
          preview = false });
      let suffix = Array.of_list (List.rev !entries) in
      let entries = Array.init (prefix + Array.length suffix) (fun i ->
        if i < prefix then old_entries.(i)
        else suffix.(i - prefix)) in
      let view = { entries; total = !total; columns; measure; recent = None } in
      t.cached <- Some view;
      t.dirty <- max_int;
      view

let visual_at layout position =
  if position < 0 || position >= layout.total then invalid_arg "visual_at";
  let low = ref 0 and high = ref (Array.length layout.entries - 1) in
  while !low < !high do
    let middle = (!low + !high + 1) / 2 in
    if layout.entries.(middle).start <= position then low := middle
    else high := middle - 1
  done;
  let entry = layout.entries.(!low) in
  let segments = match layout.recent with
    | Some (index, chunks) when index = !low -> chunks
    | _ ->
        let chunks = wrap_ranges ~columns:layout.columns ~measure:layout.measure
          entry.row.text in
        layout.recent <- Some (!low, chunks);
        chunks in
  let index = position - entry.start in
  let segment = segments.(index) in
  { source = entry.source; row = entry.row; text = segment.rendered;
    continuation = index > 0; runs = visual_runs entry.row segment }
(* Convenience for focused tests and short transcripts. Tui uses snapshot
   directly and allocates only the viewport's visual rows. *)
let layout t ~columns ~measure =
  let view = snapshot t ~columns ~measure in
  Array.init view.total (visual_at view)
