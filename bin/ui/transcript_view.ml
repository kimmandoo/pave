(* UI-only transcript. History is supplied explicitly; these rows are never journaled. *)
type kind = User | Assistant | Tool | Notice | Error
type style =
  Heading | Text | Code | Quote | List_item | Subheading | Tool_state
  | Tool_summary | Divider | Table_header | Table_row | Table_separator
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
  mutable detail : bool;
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

type write_card = {
  title : row;
  state : row;
  mutable code : row list;
  mutable path : string option;
  mutable executing : bool;
}

type t = {
  mutable rows : row array;
  mutable count : int;
  mutable next_group : int;
  expanded : (int, bool) Hashtbl.t;
  writes : (int, write_card) Hashtbl.t;
  mutable pending_tool : (string * int) option;
  mutable live : string;
  mutable streaming : bool;
  mutable fenced : bool;
  mutable fence_marker : char;
  mutable fence_length : int;
  mutable diff_fenced : bool;
  mutable diff_raw : bool;
  mutable table_active : bool;
  mutable table_header : string option;
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
  expanded = Hashtbl.create 32; writes = Hashtbl.create 8;
  pending_tool = None; live = "";
  streaming = false; fenced = false; diff_fenced = false; diff_raw = false;
  fence_marker = '`'; fence_length = 3;
  table_active = false; table_header = None; revision = 0;
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
    let retained = Hashtbl.create 128 in
    for index = 0 to t.count - 1 do
      Hashtbl.replace retained t.rows.(index).group ()
    done;
    Hashtbl.filter_map_inplace (fun id expanded ->
      if Hashtbl.mem retained id then Some expanded else None) t.expanded;
    Hashtbl.filter_map_inplace (fun id card ->
      if Hashtbl.mem retained id then Some card else None) t.writes);
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
    let cells = ref [] and cell = Buffer.create (String.length text) in
    let code = ref 0 and index = ref 0 in
    let push () =
      cells := String.trim (Buffer.contents cell) :: !cells;
      Buffer.clear cell in
    while !index < String.length text do
      let char = text.[!index] in
      if char = '\\' && !code = 0 && !index + 1 < String.length text then (
        Buffer.add_substring cell text !index 2;
        index := !index + 2)
      else if char = '`' then (
        let stop = ref (!index + 1) in
        while !stop < String.length text && text.[!stop] = '`' do incr stop done;
        let length = !stop - !index in
        if !code = 0 then code := length else if !code = length then code := 0;
        Buffer.add_substring cell text !index length;
        index := !stop)
      else (
        if char = '|' && !code = 0 then push () else Buffer.add_char cell char;
        incr index)
    done;
    push ();
    let cells = List.rev !cells in
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
  t.table_header <- None;
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

type fence = Open_fence of char * int * string | Close_fence

let fence_line t line =
  let size = String.length line in
  let rec indent index =
    if index < size && index < 3 && line.[index] = ' ' then indent (index + 1)
    else index in
  let start = indent 0 in
  if start = size || (line.[start] <> '`' && line.[start] <> '~') then None
  else
    let marker = line.[start] in
    let stop = ref start in
    while !stop < size && line.[!stop] = marker do incr stop done;
    let count = !stop - start in
    let info = String.trim (String.sub line !stop (size - !stop)) in
    if t.fenced then
      if marker = t.fence_marker && count >= t.fence_length && info = ""
      then Some Close_fence else None
    else if count >= 3 && (marker <> '`' || not (String.contains info '`')) then
      Some (Open_fence (marker, count, info))
    else None

let display_line ~fence ~fenced line =
  match fence with
  | Some Close_fence -> Code, "end code"
  | Some (Open_fence (_, _, language)) ->
      Code, "code" ^ (if language = "" then "" else " · " ^ language)
  | None when fenced -> Code, line
  | None -> markdown_prefix line

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
let classify_line t ?(start_diff = false) line =
  let fence = fence_line t line in
  let diff = Option.is_none fence && (t.diff_fenced ||
    (not t.fenced && (t.diff_raw || start_diff ||
      String.starts_with ~prefix:"diff --git " line))) in
  let diff_kind = if diff then diff_style line else None in
  let style, visible = match diff_kind with
    | Some style -> style, line
    | None when diff && t.diff_fenced -> Code, line
    | None -> display_line ~fence ~fenced:t.fenced line in
  style, visible, fence, diff_kind


let content_line t ~kind ~group ~provisional ?(detail = false)
    ?(start_diff = false) line =
  let line = fit line in
  let style, visible, fence, diff_kind = classify_line t ~start_diff line in
  let table_header = t.table_header in
  t.table_header <- None;
  (match fence with
  | Some (Open_fence (marker, length, language)) ->
      t.fenced <- true;
      t.fence_marker <- marker;
      t.fence_length <- length;
      t.diff_fenced <- String.equal (String.lowercase_ascii language) "diff";
      t.diff_raw <- false
  | Some Close_fence ->
      t.fenced <- false;
      t.diff_fenced <- false;
      t.diff_raw <- false
  | None when not t.fenced ->
      t.diff_raw <- Option.is_some diff_kind
  | None -> ());
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
             (match Option.bind table_header table_cells with
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
    | cells ->
        t.table_active <- false;
        t.table_header <- (if Option.is_some cells then Some line else None);
        add_line t ~kind ~group ~provisional ~detail ~markdown:true line

let flush_live t =
  if t.live <> "" then (
    let id = if t.streaming then t.next_group - 1 else group t in
    content_line t ~kind:Assistant ~group:id ~provisional:true t.live;
    t.live <- "")

let end_segment t =
  flush_live t;
  t.streaming <- false

(* Results can settle out of provider order. Keep each call's rows next to its
   title rather than attaching its outcome to the last call that started. *)
let gather_group t id =
  let first = ref t.count and last = ref (-1) and count = ref 0 in
  for index = 0 to t.count - 1 do
    if t.rows.(index).group = id then (
      first := min !first index; last := index; incr count)
  done;
  if !count > 0 && !last - !first + 1 <> !count then (
    let grouped = Array.make !count blank in
    let target = ref !last and next = ref (!count - 1) in
    for index = !last downto !first do
      let row = t.rows.(index) in
      if row.group = id then (
        grouped.(!next) <- row; decr next)
      else (
        t.rows.(!target) <- row; decr target)
    done;
    Array.blit grouped 0 t.rows !first !count;
    mark_dirty t !first)

let add_block t kind title text =
  end_segment t;
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

let denied_results =
  ["Error: command not approved"; "Error: tool approval denied"]

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

let excerpt_bytes limit text =
  if String.length text <= limit then text else (
    let size = ref limit in
    while !size > 0 && Char.code text.[!size] land 0xc0 = 0x80 do
      decr size
    done;
    String.sub text 0 !size ^ "…")

(* A card names what the call acts on, so a denial or failure is readable
   without expanding it. *)
let start_tool ?target t name =
  end_segment t;
  let name = single_line name in
  let target = Option.map (fun value -> String.trim (single_line value)) target in
  let label = match name, target with
    | ("read_file" | "write_file" | "edit_file" | "apply_edits" | "list_files"),
      Some path when path <> "" &&
        Filename.is_relative path && not (String.contains path ':') ->
        name ^ " · " ^ excerpt_bytes 96 path
    | ("run_command" | "glob" | "search" | "grep"), Some value when value <> "" ->
        name ^ " · " ^ excerpt_bytes 72 value
    | "web_search", Some query when query <> "" ->
        name ^ " · \"" ^ excerpt_bytes 72 query ^ "\""
    | "web_fetch", Some url when String.starts_with ~prefix:"https://" url ->
        name ^ " · " ^ excerpt_bytes 96 url
    | _ -> name in
  let id = group t in
  t.pending_tool <- Some (name, id);
  heading t ~kind:Tool ~group:id ~provisional:false
    (label ^ " · running");
  id
let write_label path =
  "write_file" ^ Option.fold ~none:""
    ~some:(fun path -> " · " ^ single_line path) path

let start_write t =
  end_segment t;
  let id = group t in
  heading t ~kind:Tool ~group:id ~provisional:false
    "write_file";
  let title = t.rows.(t.count - 1) in
  add_line t ~kind:Tool ~group:id ~provisional:false ~style:Tool_state
    "generating draft · not written";
  let state = t.rows.(t.count - 1) in
  Hashtbl.add t.writes id { title; state; code = []; path = None;
    executing = false };
  id

let dirty_write t id =
  for index = 0 to t.count - 1 do
    if t.rows.(index).group = id then mark_dirty t index
  done;
  t.revision <- t.revision + 1

let write_state t id state =
  Option.iter (fun card ->
    set_text card.title (write_label card.path);
    set_text card.state state;
    if state = "writing" then card.executing <- true;
    dirty_write t id) (Hashtbl.find_opt t.writes id)

let write_preview t id (preview : Pave.Write_preview.snapshot) state =
  Option.iter (fun card ->
    card.path <- preview.path;
    set_text card.title (write_label card.path);
    let omitted =
      (if preview.omitted_lines = 0 then "" else
        Printf.sprintf " · %d earlier lines omitted" preview.omitted_lines) ^
      (if preview.omitted_bytes = 0 then "" else
        Printf.sprintf " · %d bytes omitted" preview.omitted_bytes) in
    set_text card.state (Printf.sprintf "%s · %d %s%s"
      state preview.total_lines
      (if preview.total_lines = 1 then "line" else "lines") omitted);
    let rec update rows lines = match rows, lines with
      | row :: rows, (number, text) :: lines ->
          set_text row (Printf.sprintf "%7d │ %s" number text);
          row :: update rows lines
      | [], (number, text) :: lines ->
          add_line t ~kind:Tool ~group:id ~provisional:false ~style:Code
            (Printf.sprintf "%7d │ %s" number text);
          let row = t.rows.(t.count - 1) in
          (* Keep simultaneous calls contiguous and the live status below the tail. *)
          let index = ref 0 in
          while !index < t.count - 1 && t.rows.(!index) != card.state do
            incr index
          done;
          if !index < t.count - 1 then (
            Array.blit t.rows !index t.rows (!index + 1) (t.count - !index - 1);
            t.rows.(!index) <- row;
            mark_dirty t !index);
          row :: update [] lines
      | rows, [] ->
          if rows <> [] then (
            let kept = ref 0 in
            for index = 0 to t.count - 1 do
              let row = t.rows.(index) in
              if not (List.exists (fun removed -> removed == row) rows) then (
                t.rows.(!kept) <- row; incr kept)
              else mark_dirty t index
            done;
            Array.fill t.rows !kept (t.count - !kept) blank;
            t.count <- !kept);
          [] in
    card.code <- update card.code preview.lines;
    dirty_write t id) (Hashtbl.find_opt t.writes id)

let finish_write t id ~aborted ~is_error ~denied =
  if Hashtbl.mem t.writes id then (
    let card = Hashtbl.find t.writes id in
    write_state t id (if aborted then
      (if card.executing then "cancelled · write unconfirmed" else "cancelled · not written")
      else if denied then "denied by you · not written"
      else if is_error then "failed · not written" else "completed · written");
    Hashtbl.remove t.writes id)


let tool_result ?group:existing ?(aborted = false) ?is_error t name result =
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
  t.table_header <- None;
  let failed = Option.value
    ~default:(String.starts_with ~prefix:"Error:" result) is_error in
  let error = failed || aborted in
  let denied = failed && List.mem result denied_results in
  let outcome = if aborted then "aborted" else if denied then "denied by you · not run"
    else if failed then "failed" else "completed" in
  let length = if result = "" then 0 else
    String.fold_left (fun count char ->
      if char = '\n' then count + 1 else count)
      (if result.[String.length result - 1] = '\n' then 0 else 1) result in
  let compact_read = name = "read_file" && not error in
  let write = Hashtbl.find_opt t.writes id in
  Option.iter (fun card ->
    card.state.kind <- if error then Error else Tool;
    List.iter (fun (row : row) -> row.detail <- true) card.code;
    dirty_write t id) write;
  for i = 0 to t.count - 1 do
    let row = t.rows.(i) in
    if row.group = id && row.kind = Tool && row.style = Heading then (
      mark_dirty t i;
      let label = if String.ends_with ~suffix:" · running" row.text then
        String.sub row.text 0 (String.length row.text -
          String.length " · running")
        else if Option.is_some write then row.text else name in
      if error then row.kind <- Error;
      if compact_read then (
        row.style <- Tool_summary;
        set_text row (Printf.sprintf "%s · %d %s%s"
          label length (if length = 1 then "line" else "lines")
          (if length = 0 then "" else " · collapsed")))
      else set_text row label)
  done;
  if error then
    for i = 0 to t.count - 1 do
      let row = t.rows.(i) in
      if row.group = id && row.kind = Tool && row.style = Quote then (
        mark_dirty t i; row.kind <- Error)
    done;
  if not compact_read && Option.is_none write then
    add_line t ~kind:(if error then Error else Tool) ~group:id
      ~provisional:false ~style:Tool_state
      (if length <= 1 then outcome
       else Printf.sprintf "%s · %d lines · collapsed" outcome length);
  (* The outcome row already says a denial happened; one line needs no preview. *)
  let position = ref 0 and previewed = ref denied in
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
    let retained = ref (min bytes max_line_bytes) in
    while !retained > 0 && !retained < bytes &&
      Char.code result.[!position + !retained] land 0xc0 = 0x80 do
      decr retained
    done;
    let line = sanitize (String.sub result !position !retained) in
    let line = fit (line ^ (if bytes > !retained then "…" else "")) in
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
    if not compact_read &&
      not !previewed && String.trim line <> "" &&
      Option.is_none (fence_line t line) then (
      previewed := true;
      let style, preview, _, _ = classify_line t ~start_diff:starts_diff line in
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
  t.diff_raw <- false;
  gather_group t id;
  t.revision <- t.revision + 1

(* Notices raised while a call runs belong to its card, not a separate block. *)
let tool_note t group text =
  if text <> "" then (
    add_line t ~kind:Tool ~group ~provisional:false ~style:Quote
      (single_line text);
    gather_group t group)


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
  t.table_header <- None;
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
  t.table_header <- None;
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
  t.table_header <- None;
  Hashtbl.clear t.expanded;
  Hashtbl.clear t.writes;
  t.revision <- t.revision + 1

let visible t row =
  let expanded = Option.value (Hashtbl.find_opt t.expanded row.group)
    ~default:false in
  (not row.detail || expanded) && (not row.preview || not expanded)

let expandable_style = function
  | Tool_state | Tool_summary -> true
  | _ -> false

let toggle t ~first ~last =
  let expandable = Hashtbl.create 16 in
  for i = 0 to t.count - 1 do
    let row = t.rows.(i) in
    if row.detail then Hashtbl.replace expandable row.group ()
  done;
  let chosen = ref None in
  for i = max 0 first to min last (t.count - 1) do
    let row = t.rows.(i) in
    if visible t row && row.style <> Divider &&
      Hashtbl.mem expandable row.group then chosen := Some row.group
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
        if row.group = id && expandable_style row.style &&
          String.ends_with ~suffix:" · collapsed" row.text then
          set_text row (String.sub row.text 0
            (String.length row.text - String.length " · collapsed") ^
            " · expanded")
        else if row.group = id && expandable_style row.style &&
          String.ends_with ~suffix:" · expanded" row.text then
          set_text row (String.sub row.text 0
            (String.length row.text - String.length " · expanded") ^
            " · collapsed")
      done;
      t.revision <- t.revision + 1;
      Some id

let ascii_clusters = Array.init 95 (fun i -> String.make 1 (Char.chr (i + 32)))

let rec printable_ascii text index =
  index = String.length text ||
  (let code = Char.code text.[index] in
   code >= 32 && code <= 126 && printable_ascii text (index + 1))

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
  let add_cluster () chunk =
    let width = max 0 (measure chunk) in
    if !used > 0 && !used + width > columns then
      (match !break_at with
       | Some (byte_count, prefix_width, prefix_end)
         when byte_count < Buffer.length buffer ->
           split_at byte_count prefix_width prefix_end
       | _ -> push ());
    add_chunk chunk width;
    if chunk = " " || chunk = "\t" then
      break_at := Some (Buffer.length buffer, !used, !source_end) in
  (* Only an entirely printable-ASCII line has one grapheme per byte:
     a later combining mark can otherwise extend an earlier ASCII letter. *)
  if printable_ascii text 0 then (
    for index = 0 to String.length text - 1 do
      add_cluster () ascii_clusters.(Char.code text.[index] - 32)
    done)
  else
    ignore (Uuseg_string.fold_utf_8 `Grapheme_cluster add_cluster () text);
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
        let style, line, _, _ = classify_line t t.live in
        let style, line = if style <> Text then style, line
          else match table_cells line with
            | Some cells when table_separator cells ->
                Table_separator, table_rule cells
            | Some cells when t.table_active ->
                Table_row, table_row_text cells
            | _ -> Text, line in
        let text, runs =
          if style = Code || style = Table_separator || is_diff_style style then
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
