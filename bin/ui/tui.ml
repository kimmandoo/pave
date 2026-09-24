open Notty

let max_line_bytes = 4096
let max_transcript_rows = 10_000
let prompt = "  ❯ "

type candidate = { value : string; custom : bool; verified : bool }

type chooser = {
  title : string;
  suggestions : string array;
  mutable choices : candidate array;
  allow_custom : bool;
  dynamic : bool;
  mutable status : string option;
  mutable filter : string;
  mutable selected : int;
  mutable offset : int;
}

type row = { text : string; attr : A.t; mutable provisional : bool }
type t = {
  mutable term : Notty_unix.Term.t;
  mutable input : Terminal_input.t;
  root : string;
  mutable model : string;
  session : bool;
  editor : Pave.Composer.t;
  mutable lines : row array;
  mutable line_count : int;
  mutable scroll : int;
  mutable chooser : chooser option;
  mutable live : string;
  mutable stream_start : int option;
  mutable revision : int;
  mutable body_cache : (int * int * int * I.t) option;
  mutable previous : I.t array option;
  mutable status : string;
  mutable last_paint : float;
  mutable paste : bool;
}

let text_attr = A.(fg lightwhite)
let accent = A.(fg lightcyan ++ st bold)
let muted = A.(fg lightblack)
let warning = A.(fg lightyellow)
let error = A.(fg lightred)

let idle_status =
  "Enter send · Shift+Enter newline · ↑↓ edit/history · Ctrl+R search · PgUp/Dn scroll"

(* Two ASCII columns per 8px SVG pixel keep the mark square in a terminal. *)
let startup_logo =
  let mint = I.string accent "##" and shadow = I.string muted "++"
  and cursor = I.string warning "**" and blank = I.string A.empty "  " in
  let pixel = function
    | '#' -> mint | '+' -> shadow | '*' -> cursor | _ -> blank in
  let mark = I.vcat (List.map (fun row ->
    I.hcat (List.init (String.length row) (fun index -> pixel row.[index])))
    [ "#######"; "########"; "##+++++##"; "##+    ##+";
      "##+    ##+"; "##+    ##+"; "########++"; "#######++";
      "##++++++"; "##+"; "##+     *"; " ++" ]) in
  I.(mark <-> void 1 1 <-> string accent "      P A V E")

let sanitize text =
  let buffer = Buffer.create (min max_line_bytes (String.length text)) in
  let append () _ = function
    | `Malformed _ -> Buffer.add_utf_8_uchar buffer Uutf.u_rep
    | `Uchar uchar ->
        let code = Uchar.to_int uchar in
        if code = 10 then Buffer.add_char buffer '\n'
        else if code = 9 then Buffer.add_char buffer ' '
        else if code < 32 || code = 127 then Buffer.add_char buffer ' '
        else Buffer.add_utf_8_uchar buffer uchar in
  ignore (Uutf.String.fold_utf_8 append () text);
  Buffer.contents buffer

let fit_bytes text =
  if String.length text <= max_line_bytes then text
  else (
    let size = ref max_line_bytes in
    while !size > 0 && Char.code text.[!size] land 0xc0 = 0x80 do decr size done;
    String.sub text 0 !size ^ "…")

let add_row ?(provisional = false) t attr text =
  if t.line_count = max_transcript_rows then (
    let removed = 1_000 in
    let remaining = t.line_count - removed in
    Array.blit t.lines removed t.lines 0 remaining;
    Array.fill t.lines remaining removed
      { attr = A.empty; text = ""; provisional = false };
    t.line_count <- remaining;
    t.stream_start <- Option.map (fun index -> max 0 (index - removed))
      t.stream_start);
  if t.line_count = Array.length t.lines then (
    let grown = Array.make (max 128 (2 * t.line_count))
      { attr = A.empty; text = ""; provisional = false } in
    Array.blit t.lines 0 grown 0 t.line_count;
    t.lines <- grown);
  t.lines.(t.line_count) <- { attr; text = fit_bytes text; provisional };
  t.line_count <- t.line_count + 1;
  if t.scroll > 0 then t.scroll <- t.scroll + 1;
  t.revision <- t.revision + 1

let add_lines t attr text =
  List.iter (add_row t attr) (String.split_on_char '\n' (sanitize text))

let styled_line width attr text =
  I.hsnap ~align:`Left width (I.string attr text)

let matches chooser =
  let query = String.lowercase_ascii chooser.filter in
  let found = ref [] in
  Array.iter (fun item ->
    let value = item.value in
    let n = String.length value and m = String.length query in
    let rec at pos j =
      j = m || (Char.lowercase_ascii value.[pos + j] = query.[j]
        && at pos (j + 1)) in
    let rec find pos =
      pos + m <= n && (at pos 0 || find (pos + 1)) in
    if find 0 then found := item :: !found) chooser.choices;
  match !found with
  | [] when chooser.allow_custom && String.contains chooser.filter '/' ->
      [| { value = chooser.filter; custom = true; verified = false } |]
  | _ -> Array.of_list (List.rev !found)

let candidate_label chooser item =
  let source =
    if item.custom then "Use: "
    else if not chooser.dynamic then ""
    else if item.verified then "[verified] " else "[suggested] " in
  source ^ sanitize item.value

let view_height t =
  let _, rows = Notty_unix.Term.size t.term in
  max 1 (rows - 6)

let paint t =
  let cols, rows = Notty_unix.Term.size t.term in
  let cols = max 1 cols and rows = max 1 rows in
  let prompt_width = I.width (I.string accent prompt) in
  let prompt = if cols <= prompt_width then "" else prompt in
  let prefix_width = if prompt = "" then 0 else prompt_width in
  let field_width = max 1 (cols - prefix_width) in
  let measure cluster = I.width (I.string text_attr cluster) in
  let editor_lines = Pave.Composer.layout ~columns:field_width ~measure t.editor in
  let editor_row, editor_col =
    Pave.Composer.position ~measure t.editor editor_lines in
  let editor_height = match t.chooser, Pave.Composer.search_query t.editor with
    | Some _, _ | None, Some _ -> 1
    | None, None -> min 4 (max 1 (min (rows - 4) (Array.length editor_lines))) in
  let body_height = max 0 (rows - 4 - editor_height) in
  let header = styled_line cols accent "  ◆  PAVE  /  mobile workspace" in
  let location = styled_line cols muted
    ("  " ^ sanitize t.model ^
      (if t.session then "   ·   journal on" else "   ·   journal off") ^
      "   ·   " ^ sanitize t.root) in
  let divider = I.uchar A.(fg lightblack) (Uchar.of_int 0x2500) cols 1 in
  let total = t.line_count + if t.live = "" then 0 else 1 in
  t.scroll <- min t.scroll (max 0 (total - body_height));
  let first = max 0 (total - body_height - t.scroll) in
  let last = min total (first + body_height) in
  let body = match t.chooser with
    | Some chooser ->
        let found = matches chooser in
        let count = Array.length found in
        chooser.selected <- max 0 (min (count - 1) chooser.selected);
        let page = max 0 (body_height - 1) in
        if chooser.selected < chooser.offset then chooser.offset <- chooser.selected;
        if page > 0 && chooser.selected >= chooser.offset + page then
          chooser.offset <- chooser.selected - page + 1;
        chooser.offset <- min chooser.offset (max 0 (count - page));
        I.vcat (List.init body_height (fun i ->
          if i = 0 then styled_line cols accent ("  " ^ chooser.title)
          else
            let index = chooser.offset + i - 1 in
            if index >= count then I.void cols 1
            else let choice = found.(index) in
              styled_line cols
                (if index = chooser.selected then accent else text_attr)
                ((if index = chooser.selected then "  ❯ " else "    ")
                 ^ candidate_label chooser choice)))
    | None ->
        (match t.body_cache with
        | Some (width, height, revision, body)
          when width = cols && height = body_height && revision = t.revision -> body
        | _ ->
            let body =
              if t.line_count = 0 && t.live = "" then (
                let logo_width = I.width startup_logo
                and logo_height = I.height startup_logo in
                if cols < logo_width || body_height < logo_height then
                  I.vsnap ~align:`Bottom body_height
                    (styled_line cols accent "  PAVE")
                else
                  let left = (cols - logo_width) / 2
                  and top = (body_height - logo_height) / 2 in
                  I.(void cols top
                    <-> hsnap ~align:`Left cols (void left 1 <|> startup_logo)
                    <-> void cols (body_height - top - logo_height)))
              else
                I.vsnap ~align:`Bottom body_height
                  (I.vcat (List.init (last - first) (fun index ->
                    let index = first + index in
                    let row = if index < t.line_count then t.lines.(index)
                      else { text = "PAVE › " ^ fit_bytes t.live;
                        attr = text_attr; provisional = true } in
                    styled_line cols row.attr row.text))) in
            t.body_cache <- Some (cols, body_height, t.revision, body);
            body) in
  let footer_text = match t.chooser with
    | Some chooser ->
        let found = matches chooser in
        let number = if Array.length found = 0 then 0 else chooser.selected + 1 in
        let status = match chooser.status with None -> "" | Some text -> text ^ " · " in
        if body_height < 2 then
          Printf.sprintf "  %s%d/%d %s · Enter select · Esc cancel" status number
            (Array.length found)
            (if Array.length found = 0 then "(no match)"
             else candidate_label chooser found.(chooser.selected))
        else
          Printf.sprintf "  %s%d/%d · ↑↓/PgUp/PgDn move · Enter select · Esc cancel"
            status number (Array.length found)
    | None ->
        let status = match Pave.Composer.search_query t.editor with
          | None -> t.status
          | Some _ ->
              "reverse search: " ^
              (match Pave.Composer.search_match t.editor with
              | None -> "(no match)"
              | Some value -> sanitize (String.split_on_char '\n' value |> List.hd)) ^
              " · Ctrl+R older · Enter recall · Esc cancel" in
        (if total = 0 then "  "
         else if body_height = 0 then Printf.sprintf "  [0/%d] " total
         else Printf.sprintf "  [%d-%d/%d] " (first + 1) last total) ^ status in
  let footer = styled_line cols muted footer_text in
  let first_line = max 0 (min (editor_row - editor_height + 1)
    (Array.length editor_lines - editor_height)) in
  let prompt_rows, cursor_row, cursor_col =
    match t.chooser, Pave.Composer.search_query t.editor with
    | Some chooser, _ ->
        let filter = sanitize chooser.filter in
        let col = measure filter in
        let left_crop = max 0 (col - field_width + 1) in
        [| I.(string accent prompt <|>
            hsnap ~align:`Left field_width (hcrop left_crop 0 (string text_attr filter))) |],
        0, min (cols - 1) (prefix_width + col - left_crop)
    | None, Some query ->
        let query = sanitize query in
        let col = measure query in
        let left_crop = max 0 (col - field_width + 1) in
        let marker = if prompt = "" then "" else "  ? " in
        [| I.(string accent marker <|>
            hsnap ~align:`Left field_width (hcrop left_crop 0 (string text_attr query))) |],
        0, min (cols - 1) (prefix_width + col - left_crop)
    | None, None ->
        Array.init editor_height (fun index ->
          let line_index = first_line + index in
          let line = editor_lines.(line_index) in
          let gutter = if line_index = 0 then prompt
            else if prompt = "" then "" else "    " in
          let content = String.sub (Pave.Composer.text t.editor)
            line.start (line.stop - line.start) in
          let content = sanitize content in
          let content = if field_width = 1 && measure content > 1 then "?"
            else content in
          I.(string accent gutter <|>
            hsnap ~align:`Left field_width (string text_attr content))),
        editor_row - first_line, min (cols - 1) (prefix_width + editor_col) in
  let screen = if rows < 6 then
    let candidates = Array.append [| footer |] prompt_rows in
    if Array.length candidates >= rows then
      Array.sub candidates (Array.length candidates - rows) rows
    else Array.append
      (Array.make (rows - Array.length candidates) (I.void cols 1))
      candidates
  else Array.concat [
    [| header; location; divider |];
    Array.init body_height (fun row -> I.vcrop row (body_height - row - 1) body);
    [| footer |]; prompt_rows ] in
  let output = Buffer.create 512 in
  Buffer.add_string output "\027[?25l";
  for row = 0 to rows - 1 do
    if (match t.previous with
      | Some previous when Array.length previous = rows ->
          not (I.equal previous.(row) screen.(row))
      | _ -> true) then (
      Buffer.add_string output (Printf.sprintf "\027[%d;1H\027[0m\027[2K" (row + 1));
      Render.to_buffer output Cap.ansi (0, 0) (cols, 1) screen.(row))
  done;
  t.previous <- Some screen;
  let y = if rows < 6 then rows - 1
    else rows - editor_height + cursor_row in
  Buffer.add_string output (Printf.sprintf "\027[%d;%dH\027[?25h"
    (max 0 y + 1) (max 0 cursor_col + 1));
  Buffer.output_buffer stdout output;
  flush stdout;
  t.last_paint <- Unix.gettimeofday ()

let scroll_by t delta =
  t.scroll <- max 0 (t.scroll + delta);
  t.revision <- t.revision + 1;
  paint t

let create ~root ~model ~session =
  let term = Notty_unix.Term.create ~mouse:false ~bpaste:true () in
  let t = { term; input = Terminal_input.create term;
    root; model; session; editor = Pave.Composer.create ();
    lines = [||]; line_count = 0; scroll = 0; chooser = None;
    live = ""; stream_start = None; revision = 0; body_cache = None;
    previous = None; status = idle_status; last_paint = 0.; paste = false } in
  (try paint t with exn -> Notty_unix.Term.release term; raise exn);
  t

let close t = Notty_unix.Term.release t.term

let suspend t callback =
  Notty_unix.Term.release t.term;
  Fun.protect callback ~finally:(fun () ->
    let term = Notty_unix.Term.create ~mouse:false ~bpaste:true () in
    t.term <- term;
    t.input <- Terminal_input.create term;
    t.previous <- None;
    t.body_cache <- None;
    t.paste <- false;
    paint t)

let reset_status t =
  t.status <- idle_status;
  paint t

let set_model t model =
  t.model <- model;
  reset_status t

let settle_live t =
  if t.live <> "" then (
    add_lines t text_attr ("PAVE › " ^ t.live);
    t.live <- "";
    if t.scroll > 0 then t.scroll <- t.scroll - 1);
  (match t.stream_start with
  | None -> ()
  | Some start ->
      for i = start to t.line_count - 1 do
        t.lines.(i).provisional <- false
      done);
  t.stream_start <- None

let finish_live t =
  settle_live t;
  paint t

let sent t text =
  add_lines t accent ("YOU › " ^ text);
  t.scroll <- 0;
  paint t

let event t text =
  settle_live t;
  let attr = if String.starts_with ~prefix:"Error:" text then error
    else if String.starts_with ~prefix:"[" text then warning else text_attr in
  add_lines t attr text;
  paint t

let events t lines =
  List.iter (add_lines t text_attr) lines;
  paint t

let delta t chunk =
  if t.stream_start = None then t.stream_start <- Some t.line_count;
  let old_lines = t.line_count in
  let old_total = old_lines + if t.live = "" then 0 else 1 in
  let anchored = t.scroll > 0 in
  let parts = String.split_on_char '\n' (sanitize chunk) in
  (match parts with
   | [] -> ()
   | first :: rest ->
       t.live <- fit_bytes (t.live ^ first);
       List.iter (fun part ->
         add_row ~provisional:true t text_attr ("PAVE › " ^ t.live);
         t.live <- fit_bytes part) rest);
  if anchored then (
    let total = t.line_count + if t.live = "" then 0 else 1 in
    t.scroll <- max 0 (t.scroll + total - old_total - (t.line_count - old_lines)));
  t.revision <- t.revision + 1;
  if chunk = "\n" || Unix.gettimeofday () -. t.last_paint > 0.033 then paint t

let clear_live t =
  let start = Option.value t.stream_start ~default:t.line_count in
  let write = ref start in
  for i = start to t.line_count - 1 do
    if not t.lines.(i).provisional then (
      t.lines.(!write) <- t.lines.(i);
      incr write)
  done;
  let removed = t.line_count - !write + if t.live = "" then 0 else 1 in
  for i = !write to t.line_count - 1 do
    t.lines.(i) <- { attr = A.empty; text = ""; provisional = false }
  done;
  t.line_count <- !write;
  t.live <- "";
  t.stream_start <- None;
  if removed > 0 then (
    if t.scroll > 0 then t.scroll <- max 0 (t.scroll - removed);
    t.revision <- t.revision + 1;
    paint t)

let alert t message =
  t.status <- sanitize message;
  paint t

let utf8 uchar =
  let buffer = Buffer.create 4 in
  Buffer.add_utf_8_uchar buffer uchar;
  Buffer.contents buffer

let repaint_after_key t =
  if (not t.paste && not (Terminal_input.pending t.input))
    || Unix.gettimeofday () -. t.last_paint >= 0.1 then paint t

let read ?wake_fd ?on_wake ?on_interrupt t =
  let measure cluster = I.width (I.string text_attr cluster) in
  let field_width () =
    let cols, _ = Notty_unix.Term.size t.term in
    let prefix_width = I.width (I.string accent prompt) in
    max 1 (if cols <= prefix_width then cols else cols - prefix_width) in
  let changed () = repaint_after_key t in
  let paste_insert value =
    (match Pave.Composer.search_query t.editor with
    | None -> Pave.Composer.insert t.editor value
    | Some _ -> Pave.Composer.search_insert t.editor value);
    changed () in
  let rec loop () =
    match Terminal_input.event ?wake_fd t.input with
    | `End -> None
    | `Wake ->
        (match on_wake with Some callback -> callback ()
         | None -> invalid_arg "Tui.read: wake_fd requires on_wake");
        loop ()
    | `Resize _ -> paint t; loop ()
    | `Paste `Start -> t.paste <- true; loop ()
    | `Paste `End -> t.paste <- false; paint t; loop ()
    | `Key (`Enter, _) when t.paste ->
        if Pave.Composer.search_query t.editor = None then paste_insert "\n";
        loop ()
    | `Key (`ASCII c, []) when t.paste && Char.code c >= 32 ->
        paste_insert (String.make 1 c); loop ()
    | `Key (`Uchar uchar, []) when t.paste ->
        paste_insert (utf8 uchar); loop ()
    | `Key _ when t.paste -> loop ()
    | `Key (`ASCII 'C', [ `Ctrl ]) ->
        if Pave.Composer.text t.editor = "" then (
          Pave.Composer.search_cancel t.editor;
          Option.iter (fun callback -> callback ()) on_interrupt)
        else Pave.Composer.clear t.editor;
        changed (); loop ()
    | `Key key when Pave.Composer.search_query t.editor <> None ->
        (match key with
        | `Escape, _ | `ASCII 'G', [ `Ctrl ] ->
            Pave.Composer.search_cancel t.editor
        | `Enter, _ when not t.paste -> Pave.Composer.search_accept t.editor
        | `ASCII 'R', [ `Ctrl ] -> Pave.Composer.search_older t.editor
        | `Backspace, _ -> Pave.Composer.search_erase t.editor
        | `ASCII c, [] when Char.code c >= 32 ->
            Pave.Composer.search_insert t.editor (String.make 1 c)
        | `Uchar uchar, [] ->
            Pave.Composer.search_insert t.editor (utf8 uchar)
        | _ -> ());
        changed (); loop ()
    | `Key (`Enter, mods) ->
        if t.paste || List.mem `Shift mods then
          (Pave.Composer.insert t.editor "\n"; changed (); loop ())
        else (match Pave.Composer.submit t.editor with
          | None -> loop ()
          | Some value -> paint t; Some value)
    | `Key (`Page `Up, _) -> scroll_by t (view_height t); loop ()
    | `Key (`Page `Down, _) -> scroll_by t (-view_height t); loop ()
    | `Key (`Home, [ `Ctrl ]) ->
        t.scroll <- t.line_count + 1; t.revision <- t.revision + 1; paint t; loop ()
    | `Key (`End, [ `Ctrl ]) ->
        t.scroll <- 0; t.revision <- t.revision + 1; paint t; loop ()
    | `Key (`Arrow `Up, [ `Meta ]) | `Key (`ASCII 'P', [ `Ctrl ]) ->
        Pave.Composer.older t.editor; changed (); loop ()
    | `Key (`Arrow `Down, [ `Meta ]) | `Key (`ASCII 'N', [ `Ctrl ]) ->
        Pave.Composer.newer t.editor; changed (); loop ()
    | `Key (`Arrow `Up, _) ->
        if not (Pave.Composer.vertical ~columns:(field_width ()) ~measure
          t.editor (-1)) then Pave.Composer.older t.editor;
        changed (); loop ()
    | `Key (`Arrow `Down, _) ->
        if not (Pave.Composer.vertical ~columns:(field_width ()) ~measure
          t.editor 1) then Pave.Composer.newer t.editor;
        changed (); loop ()
    | `Key (`Arrow `Left, mods) ->
        (if List.mem `Meta mods || List.mem `Ctrl mods then
          Pave.Composer.word_left t.editor else Pave.Composer.left t.editor);
        changed (); loop ()
    | `Key (`Arrow `Right, mods) ->
        (if List.mem `Meta mods || List.mem `Ctrl mods then
          Pave.Composer.word_right t.editor else Pave.Composer.right t.editor);
        changed (); loop ()
    | `Key (`ASCII 'B', [ `Meta ]) ->
        Pave.Composer.word_left t.editor; changed (); loop ()
    | `Key (`ASCII 'F', [ `Meta ]) ->
        Pave.Composer.word_right t.editor; changed (); loop ()
    | `Key (`ASCII 'W', [ `Ctrl ]) | `Key (`Backspace, [ `Meta ]) ->
        Pave.Composer.erase_word t.editor; changed (); loop ()
    | `Key (`Backspace, _) -> Pave.Composer.erase t.editor; changed (); loop ()
    | `Key (`Delete, _) -> Pave.Composer.delete t.editor; changed (); loop ()
    | `Key (`Home, _) | `Key (`ASCII 'A', [ `Ctrl ]) ->
        Pave.Composer.home t.editor; changed (); loop ()
    | `Key (`End, _) | `Key (`ASCII 'E', [ `Ctrl ]) ->
        Pave.Composer.finish t.editor; changed (); loop ()
    | `Key (`ASCII 'R', [ `Ctrl ]) ->
        Pave.Composer.search_older t.editor; paint t; loop ()
    | `Key (`ASCII 'D', [ `Ctrl ]) when Pave.Composer.text t.editor = "" -> None
    | `Key (`ASCII c, []) when Char.code c >= 32 ->
        Pave.Composer.insert t.editor (String.make 1 c); changed (); loop ()
    | `Key (`Uchar uchar, []) ->
        Pave.Composer.insert t.editor (utf8 uchar); changed (); loop ()
    | _ -> loop () in
  loop ()

(* The chooser is updated only on the UI thread (typically from on_wake). The
   initial offline suggestions remain available when verified IDs arrive. *)
let update_chooser chooser ~verified ~status =
  let previous = matches chooser in
  let selected = if chooser.selected < Array.length previous then
      Some previous.(chooser.selected).value else None in
  let confirmed = Hashtbl.create (List.length verified) in
  List.iter (fun value -> Hashtbl.replace confirmed value ()) verified;
  let seen = Hashtbl.create (Array.length chooser.suggestions + List.length verified) in
  let choices = ref [] in
  let add value =
    if not (Hashtbl.mem seen value) then (
      Hashtbl.add seen value ();
      choices := { value; custom = false;
        verified = Hashtbl.mem confirmed value } :: !choices) in
  Array.iter add chooser.suggestions;
  List.iter add verified;
  chooser.choices <- Array.of_list (List.rev !choices);
  chooser.status <- Option.map sanitize status;
  let found = matches chooser in
  chooser.selected <- (match selected with
    | Some value ->
        let rec locate i =
          if i = Array.length found then 0
          else if found.(i).value = value then i else locate (i + 1) in
        locate 0
    | None -> 0);
  chooser.offset <- min chooser.offset chooser.selected

let update_choices t ~verified ?status () =
  match t.chooser with
  | Some chooser when chooser.dynamic ->
      update_chooser chooser ~verified ~status;
      paint t
  | _ -> invalid_arg "Tui.update_choices: no dynamic chooser is open"

let choose ?(allow_custom = false) ?wake_fd ?on_wake t ~title ~choices =
  let cols, rows = Notty_unix.Term.size t.term in
  if cols < 9 || rows < 2 then (
    alert t "Resize terminal (at least 9 columns × 2 rows) to select";
    None)
  else
  let suggestions = Array.of_list choices in
  let chooser = { title = sanitize title; suggestions;
    choices = Array.map (fun value ->
      { value; custom = false; verified = false }) suggestions;
    allow_custom; dynamic = Option.is_some wake_fd; status = None;
    filter = ""; selected = 0; offset = 0 } in
  let old_scroll = t.scroll in
  let selected () =
    let found = matches chooser in
    if chooser.selected >= 0 && chooser.selected < Array.length found then
      Some (found.(chooser.selected).value)
    else None in
  t.chooser <- Some chooser;
  Fun.protect ~finally:(fun () ->
    t.chooser <- None;
    t.scroll <- old_scroll;
    t.previous <- None;
    t.paste <- false;
    paint t) (fun () ->
    paint t;
    let rec loop () =
      match Terminal_input.event ?wake_fd t.input with
      | `End -> None
      | `Wake ->
          (match on_wake with Some callback -> callback ()
           | None -> invalid_arg "Tui.choose: wake_fd requires on_wake");
          loop ()
      | `Resize _ ->
          let cols, rows = Notty_unix.Term.size t.term in
          if cols < 9 || rows < 2 then (
            t.status <- "Resize terminal to select"; None)
          else (paint t; loop ())
      | `Paste `Start -> t.paste <- true; loop ()
      | `Paste `End -> t.paste <- false; paint t; loop ()
      | `Key (`Escape, _) when not t.paste -> None
      | `Key (`Enter, _) when not t.paste ->
          (match selected () with Some _ as choice -> choice | None -> loop ())
      | `Key (`Arrow `Up, _) ->
          chooser.selected <- max 0 (chooser.selected - 1);
          paint t; loop ()
      | `Key (`Arrow `Down, _) ->
          chooser.selected <- max 0 (min (Array.length (matches chooser) - 1)
            (chooser.selected + 1));
          paint t; loop ()
      | `Key (`Page direction, _) ->
          let step = view_height t in
          chooser.selected <- max 0 (min (Array.length (matches chooser) - 1)
            (chooser.selected + if direction = `Down then step else -step));
          paint t; loop ()
      | `Key (`Home, _) ->
          chooser.selected <- 0; paint t; loop ()
      | `Key (`End, _) ->
          chooser.selected <- max 0 (Array.length (matches chooser) - 1);
          paint t; loop ()
      | `Key (`Backspace, _) when chooser.filter <> "" ->
          let boundaries = Pave.Composer.segment chooser.filter in
          chooser.filter <- String.sub chooser.filter 0
            boundaries.(Array.length boundaries - 2);
          chooser.selected <- 0; chooser.offset <- 0;
          paint t; loop ()
      | `Key (`ASCII c, []) when Char.code c >= 32 ->
          if String.length chooser.filter < 256 then (
            chooser.filter <- chooser.filter ^ String.make 1 c;
            chooser.selected <- 0; chooser.offset <- 0;
            paint t);
          loop ()
      | `Key (`Uchar uchar, []) ->
          let value = utf8 uchar in
          if String.length chooser.filter + String.length value <= 256 then (
            chooser.filter <- chooser.filter ^ value;
            chooser.selected <- 0; chooser.offset <- 0;
            paint t);
          loop ()
      | _ -> loop () in
    loop ())

let confirm t command =
  let command_lines = String.split_on_char '\n' (sanitize command) in
  let fits () =
    let cols, rows = Notty_unix.Term.size t.term in
    List.length command_lines <= max 1 (rows - 7)
      && List.for_all (fun line ->
        I.width (I.string text_attr line) <= max 1 (cols - 22)) command_lines in
  if not (fits ()) then (
    alert t "Shell command denied: too large to review on screen";
    false)
  else (
    add_lines t warning ("SHELL APPROVAL  ·  " ^ command);
    alert t "Run the command above without a sandbox?  y approve   ·   any other key deny";
    let rec decision () = match Terminal_input.event t.input with
      | `Resize _ -> if fits () then (paint t; decision ()) else false
      | `Key (`ASCII ('y' | 'Y'), []) -> true
      | _ -> false in
    let accepted = decision () in
    alert t (if accepted then "Shell command approved" else "Shell command denied");
    accepted)
