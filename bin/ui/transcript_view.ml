(* UI-only transcript. History is supplied explicitly; these rows are never journaled. *)
type kind = User | Assistant | Tool | Notice | Error | Approval
type style = Heading | Text | Code | Quote | List_item | Subheading | Tool_state | Divider

type row = {
  kind : kind;
  style : style;
  mutable text : string;
  mutable provisional : bool;
  group : int;
  detail : bool;
  preview : bool;
}

(* The snapshot stores logical rows, never all their wrapped visual lines. *)
type visual = { source : int; row : row; text : string; continuation : bool }
type entry = { source : int; row : row; start : int; length : int }
type snapshot = {
  entries : entry array;
  total : int;
  columns : int;
  measure : string -> int;
  mutable recent : (int * string array) option;
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
  mutable revision : int;
  mutable cached : snapshot option;
  mutable dirty : int;
}

let max_rows = 10_000
let max_line_bytes = 4096
let max_tool_lines = 3000
let blank = { kind = Notice; style = Text; text = ""; provisional = false;
  group = 0; detail = false; preview = false }

let create () = { rows = [||]; count = 0; next_group = 1;
  expanded = Hashtbl.create 32; pending_tool = None; live = "";
  streaming = false; fenced = false; revision = 0;
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

let add_line t ~kind ~group ~provisional ?(detail = false)
    ?(preview = false) ?(style = Text) text =
  add t { kind; style; text = fit text; provisional; group; detail; preview }

let heading t ~kind ~group ~provisional text =
  if t.count > 0 && t.rows.(t.count - 1).style <> Divider then
    add_line t ~kind ~group ~provisional ~style:Divider "";
  t.fenced <- false;
  add_line t ~kind ~group ~provisional ~style:Heading text

let content_line t ~kind ~group ~provisional ?(detail = false) line =
  let line = fit line in
  let style, line =
    if String.starts_with ~prefix:"```" line then (
      let opening = not t.fenced in
      t.fenced <- opening;
      Code, (if opening then
        let language = String.trim
          (String.sub line 3 (String.length line - 3)) in
        "╶ code" ^ (if language = "" then "" else " · " ^ language)
      else "╴ end code"))
    else if t.fenced then Code, line
    else if String.starts_with ~prefix:"# " line then
      Subheading, String.sub line 2 (String.length line - 2)
    else if String.starts_with ~prefix:"## " line then
      Subheading, String.sub line 3 (String.length line - 3)
    else if String.starts_with ~prefix:"### " line then
      Subheading, String.sub line 4 (String.length line - 4)
    else if String.starts_with ~prefix:"> " line then
      Quote, String.sub line 2 (String.length line - 2)
    else if String.starts_with ~prefix:"- " line ||
      String.starts_with ~prefix:"* " line then
      List_item, String.sub line 2 (String.length line - 2)
    else Text, line in
  add_line t ~kind ~group ~provisional ~detail ~style line

let add_block t kind title text =
  let id = group t in
  heading t ~kind ~group:id ~provisional:false title;
  String.split_on_char '\n' (sanitize text)
  |> List.iter (content_line t ~kind ~group:id ~provisional:false)

let sent t text = add_block t User "YOU" text
let assistant t text = add_block t Assistant "PAVE" text
let notice t text = add_block t Notice "NOTICE" text
let error t text = add_block t Error "ERROR" text
let approval t text = add_block t Approval "SHELL APPROVAL · review before deciding" text

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
  let failed = is_error || String.starts_with ~prefix:"Error:" result in
  let error = failed || aborted in
  let outcome = if aborted then "aborted" else if failed then "failed" else "completed" in
  for i = 0 to t.count - 1 do
    let row = t.rows.(i) in
    if row.group = id && row.kind = Tool && row.style = Heading then (
      mark_dirty t i;
      row.text <- name ^ " · " ^ outcome)
  done;
  let length = String.fold_left (fun count char ->
    if char = '\n' then count + 1 else count) 1 result in
  add_line t ~kind:(if error then Error else Tool) ~group:id
    ~provisional:false ~style:Tool_state
    (name ^ (if aborted then " · aborted" else
      if failed then " · error" else " · done") ^ " · Alt+O expand");
  let position = ref 0 in
  for index = 0 to min (length - 1) (max_tool_lines - 1) do
    let stop = match String.index_from_opt result !position '\n' with
      | Some stop -> stop | None -> String.length result in
    let bytes = stop - !position in
    let line = sanitize (String.sub result !position
      (min bytes max_line_bytes)) in
    let line = if bytes > max_line_bytes then fit (line ^ "…") else fit line in
    if index < 2 then (
      let excerpt =
        if String.length line <= 160 then line
        else let prefix = ref 160 in
          while !prefix > 0 && Char.code line.[!prefix] land 0xc0 = 0x80 do
            decr prefix done;
          String.sub line 0 !prefix ^ "…" in
      add_line t ~kind:(if error then Error else Tool) ~group:id
        ~provisional:false ~preview:true excerpt);
    content_line t ~kind:(if error then Error else Tool)
      ~group:id ~provisional:false ~detail:true line;
    position := stop + 1
  done;
  if length > max_tool_lines then
    add_line t ~kind:Notice ~group:id ~provisional:false ~detail:true
      (Printf.sprintf "… %d additional lines omitted (transcript limit)"
        (length - max_tool_lines))

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
      heading t ~kind:Assistant ~group:id ~provisional:true "PAVE";
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
  for i = 0 to t.count - 1 do t.rows.(i).provisional <- false done;
  t.revision <- t.revision + 1

let interrupt_tool t name id =
  for i = 0 to t.count - 1 do
    let row = t.rows.(i) in
    if row.group = id && row.kind = Tool && row.style = Heading then (
      mark_dirty t i;
      row.text <- name ^ " · interrupted (outcome unknown)")
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
          String.ends_with ~suffix:" · Alt+O expand" row.text then
          row.text <- String.sub row.text 0
            (String.length row.text - String.length " · Alt+O expand") ^
            " · Alt+O collapse"
        else if row.group = id && row.style = Tool_state &&
          String.ends_with ~suffix:" · Alt+O collapse" row.text then
          row.text <- String.sub row.text 0
            (String.length row.text - String.length " · Alt+O collapse") ^
            " · Alt+O expand"
      done;
      t.revision <- t.revision + 1;
      Some id

let wrap ~columns ~measure text =
  let columns = max 1 columns in
  let segments = ref [] and buffer = Buffer.create (min max_line_bytes columns) in
  let used = ref 0 in
  let push () = segments := Buffer.contents buffer :: !segments; Buffer.clear buffer;
    used := 0 in
  ignore (Uuseg_string.fold_utf_8 `Grapheme_cluster
    (fun () chunk ->
      let width = max 0 (measure chunk) in
      if !used > 0 && !used + width > columns then push ();
      if width > columns then (
        Buffer.add_char buffer '?'; used := !used + 1)
      else (Buffer.add_string buffer chunk; used := !used + width))
    () text);
  push ();
  Array.of_list (List.rev !segments)


let wrapped_count ~columns ~measure text =
  let columns = max 1 columns in
  fst (Uuseg_string.fold_utf_8 `Grapheme_cluster
    (fun (lines, used) cluster ->
      let width = max 0 (measure cluster) in
      let rendered_width = if width > columns then 1 else width in
      if used > 0 && used + width > columns then
        lines + 1, rendered_width
      else lines, used + rendered_width) (1, 0) text)

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
        let style =
          if String.starts_with ~prefix:"```" t.live || t.fenced then Code
          else if String.starts_with ~prefix:"# " t.live ||
            String.starts_with ~prefix:"## " t.live then Subheading
          else if String.starts_with ~prefix:"> " t.live then Quote
          else if String.starts_with ~prefix:"- " t.live ||
            String.starts_with ~prefix:"* " t.live then List_item
          else Text in
        push t.count { kind = Assistant; style; text = t.live;
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
        let chunks = wrap ~columns:layout.columns ~measure:layout.measure
          entry.row.text in
        layout.recent <- Some (!low, chunks);
        chunks in
  let index = position - entry.start in
  { source = entry.source; row = entry.row; text = segments.(index);
    continuation = index > 0 }

(* Convenience for focused tests and short transcripts. Tui uses snapshot
   directly and allocates only the viewport's visual rows. *)
let layout t ~columns ~measure =
  let view = snapshot t ~columns ~measure in
  Array.init view.total (visual_at view)
