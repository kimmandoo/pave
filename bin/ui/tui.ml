open Notty

(* Transcript limits live in Transcript_view; no duplicate storage here. *)
let prompt = "  ❯ "

type candidate = { value : string; custom : bool; verified : bool }

type chooser = {
  title : string;
  intro : string array;
  suggestions : string array;
  mutable choices : candidate array;
  allow_custom : bool;
  dynamic : bool;
  mutable status : string option;
  mutable filter : string;
  mutable selected : int;
  mutable offset : int;
}

type t = {
  mutable term : Notty_unix.Term.t;
  mutable input : Terminal_input.t;
  root : string;
  mutable model : string;
  mutable session : bool;
  editor : Pave.Composer.t;
  transcript : Transcript_view.t;
  mutable scroll : int;
  mutable chooser : chooser option;
  mutable hint_draft : string;
  mutable hint_selected : int;
  mutable hint_offset : int;
  mutable hint_suppressed : string option;
  mutable revision : int;
  mutable body_cache : (int * int * int * I.t) option;
  mutable layout_cache : (int * int * Transcript_view.snapshot) option;
  mutable previous : I.t array option;
  mutable status : string;
  mutable activity : string option;
  mutable activity_started : float option;
  mutable usage_badge : string option;
  mutable queue : int;
  mutable last_paint : float;
  mutable paste : bool;
}

let no_color = match Sys.getenv_opt "NO_COLOR" with Some s -> s <> "" | None -> false
let text_attr = if no_color then A.empty else A.(fg lightwhite)
let accent = if no_color then A.empty else A.(fg lightcyan ++ st bold)
let user_attr = if no_color then A.empty else A.(fg lightblue ++ st bold)
let muted = if no_color then A.empty else A.(fg lightblack)
let warning = if no_color then A.empty else A.(fg lightyellow)
let error = if no_color then A.empty else A.(fg lightred)

let idle_status =
  "Alt+O tool details · PgUp/Dn scroll · Enter send · Ctrl+R search"

let hotkeys = [
  "Keys · Enter send · Shift+Enter newline";
  "/ · live commands; ↑/↓ select · Tab/Enter insert · Esc close";
  "Ctrl+R reverse search · Esc cancel search";
  "↑/↓ move in draft · Ctrl+P/N or Alt+↑/↓ prompt history";
  "Ctrl+A/E line ends · Alt+B/F move by word · Ctrl+W erase word";
  "Ctrl+Z/Y undo/redo · Ctrl+K/U kill line · Alt+Y yank";
  "Alt+O tool details · PgUp/Dn scroll · Ctrl+Home/End transcript";
  "Ctrl+C clear draft or interrupt if empty · Ctrl+D exit if empty";
  "Bracketed paste inserts atomically; pasted Enter does not send";
]

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

let sanitize = Transcript_view.sanitize
let single_line = Transcript_view.single_line

let transcript_changed t =
  t.revision <- t.revision + 1;
  t.layout_cache <- None

let change_transcript t action =
  let before, cols = if t.scroll = 0 then 0, 0 else (
    let cols, _ = Notty_unix.Term.size t.term in
    let content_cols = if cols <= 6 then max 1 cols else cols - 5 in
    let measure chunk = I.width (I.string text_attr chunk) in
    let layout = match t.layout_cache with
      | Some (width, revision, layout)
        when width = cols && revision = t.transcript.revision -> layout
      | _ -> Transcript_view.snapshot t.transcript ~columns:content_cols ~measure in
    layout.total, content_cols) in
  action ();
  transcript_changed t;
  if t.scroll > 0 then (
    let after = (Transcript_view.snapshot t.transcript ~columns:cols
      ~measure:(fun chunk -> I.width (I.string text_attr chunk))).total in
    t.scroll <- max 0 (t.scroll + after - before))

let style_attr (row : Transcript_view.row) =
  match row.kind, row.style with
  | Transcript_view.Error, _ -> error
  | Transcript_view.Approval, _ -> warning
  | Transcript_view.User, Transcript_view.Heading -> user_attr
  | Transcript_view.Assistant, Transcript_view.Heading -> accent
  | Transcript_view.Tool, Transcript_view.Heading -> warning
  | _, (Transcript_view.Heading | Transcript_view.Subheading) -> accent
  | _, Transcript_view.Code -> text_attr
  | _, Transcript_view.Quote -> text_attr
  | _, Transcript_view.Tool_state -> warning
  | _, _ -> text_attr

let styled_visual cols (visual : Transcript_view.visual) =
  let row = visual.row in
  let prefix = match row.style with
    | Transcript_view.Heading -> "  ╭─ "
    | Transcript_view.Divider -> ""
    | Transcript_view.Tool_state -> "  ├─ "
    | Transcript_view.Code -> "  │  "
    | Transcript_view.Quote -> "  │ › "
    | Transcript_view.List_item -> "  │ • "
    | _ -> if visual.continuation then "  │  " else "  │ "
  in
  let attr = style_attr row in
  let prefix = if cols <= I.width (I.string attr prefix) then "" else prefix in
  I.hsnap ~align:`Left cols (I.string attr (prefix ^ visual.text))

let styled_line width attr text =
  I.hsnap ~align:`Left width (I.string attr text)

let shorten_width width text =
  if width < 2 then "" else
  let measure cluster = I.width (I.string text_attr cluster) in
  if measure text <= width then text
  else (Transcript_view.wrap ~columns:(width - 1) ~measure text).(0) ^ "…"

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

(* Hint state belongs to the editor, never to the transcript or the modal chooser.
   A dismissed/inserted draft remains quiet until the user edits it again. *)
let hint_matches t =
  let draft = Pave.Composer.text t.editor in
  if t.hint_draft <> draft then (
    t.hint_draft <- draft;
    t.hint_selected <- 0;
    t.hint_offset <- 0;
    t.hint_suppressed <- None);
  if t.paste || Option.is_some t.chooser ||
    Pave.Composer.search_query t.editor <> None ||
    t.hint_suppressed = Some draft ||
    Pave.Composer.cursor t.editor <> String.length draft ||
    String.exists (fun c -> c = ' ' || c = '\t' || c = '\n') draft
  then []
  else Pave.Interaction.suggestions draft

let hint_room t =
  let cols, rows = Notty_unix.Term.size t.term in
  let prompt_width = I.width (I.string accent prompt) in
  let field_width = max 1 (cols - if cols <= prompt_width then 0
    else prompt_width) in
  let measure cluster = I.width (I.string text_attr cluster) in
  let lines = Pave.Composer.layout ~columns:field_width ~measure t.editor in
  let height = min 4 (max 1 (min (rows - 4) (Array.length lines))) in
  rows - 4 - height >= 2

let hints_visible t = hint_matches t <> [] && hint_room t

let selected_hint t =
  let matches = hint_matches t in
  if t.hint_selected < List.length matches then
    Some (List.nth matches t.hint_selected)
  else None

let dismiss_hint t =
  t.hint_suppressed <- Some (Pave.Composer.text t.editor)

let insert_hint t (item : Pave.Interaction.shortcut) =
  let draft = Pave.Composer.text t.editor in
  Pave.Composer.finish t.editor;
  Pave.Composer.insert t.editor
    (String.sub item.name (String.length draft)
      (String.length item.name - String.length draft));
  t.hint_draft <- Pave.Composer.text t.editor;
  dismiss_hint t

let hint_row cols selected (item : Pave.Interaction.shortcut) =
  let marker = if selected then "  ❯ " else "    " in
  let usage = if item.usage = "" then "" else " " ^ item.usage in
  I.hsnap ~align:`Left cols I.(
    string (if selected then accent else text_attr) (marker ^ item.name) <|>
    string (if selected then text_attr else muted) (usage ^ " · " ^ item.summary))

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
  let hints = hint_matches t in
  let hint_count = List.length hints in
  let hint_height = if body_height < 2 || hint_count = 0 then 0
    else min body_height (min 8 (hint_count + 1)) in
  let hint_page = max 0 (hint_height - 1) in
  t.hint_selected <- min t.hint_selected (max 0 (hint_count - 1));
  if t.hint_selected < t.hint_offset then t.hint_offset <- t.hint_selected;
  if hint_page > 0 && t.hint_selected >= t.hint_offset + hint_page then
    t.hint_offset <- t.hint_selected - hint_page + 1;
  t.hint_offset <- min t.hint_offset (max 0 (hint_count - hint_page));
  let activity = match t.activity with
    | None -> ""
    | Some state ->
        let elapsed = match t.activity_started with
          | Some since -> max 0 (int_of_float (Unix.gettimeofday () -. since))
          | None -> 0 in
        " · " ^ single_line state ^
        (if elapsed = 0 then "" else if elapsed < 60 then
          Printf.sprintf " · %ds" elapsed
        else Printf.sprintf " · %dm%02ds" (elapsed / 60) (elapsed mod 60)) in
  let queued = if t.queue = 0 then "" else
    Printf.sprintf " · %d queued" t.queue in
  let usage = match t.activity, t.usage_badge with
    | None, Some badge when cols >= 28 && cols >= 9 + String.length badge ->
        badge
    | _ -> "" in
  let header = styled_line cols accent
    ("  ◆  PAVE" ^ activity ^ (if cols >= 48 then queued else "") ^ usage) in
  let model = single_line t.model in
  let model =
    if cols < 60 then match String.rindex_opt model '/' with
      | None -> model
      | Some split ->
          shorten_width 6 (String.sub model 0 split) ^ "/" ^
          String.sub model (split + 1) (String.length model - split - 1)
    else model in
  let journal = if t.session then "on" else "off" in
  let location = styled_line cols text_attr
    (if cols < 22 then " " ^ shorten_width (max 2 (cols - 1)) model
    else if cols < 60 then
      " " ^ shorten_width (cols - 15) model ^ " · journal " ^ journal
    else "  " ^ model ^ "   ·   journal " ^ journal ^
      "   ·   " ^ single_line t.root) in
  let divider = I.uchar muted (Uchar.of_int 0x2500) cols 1 in
  let layout = match t.layout_cache with
    | Some (width, revision, layout)
      when width = cols && revision = t.transcript.revision -> layout
    | _ ->
        let content_cols = if cols <= 6 then cols else cols - 5 in
        let layout = Transcript_view.snapshot t.transcript ~columns:content_cols
          ~measure in
        t.layout_cache <- Some (cols, t.transcript.revision, layout);
        layout in
  let total = layout.total in
  t.scroll <- min t.scroll (max 0 (total - body_height));
  let first = max 0 (total - body_height - t.scroll) in
  let last = min total (first + body_height) in
  let body = match t.chooser with
    | Some chooser ->
        let found = matches chooser in
        let count = Array.length found in
        chooser.selected <- max 0 (min (count - 1) chooser.selected);
        let intro_height =
          if cols >= 52 && body_height >= 9 && chooser.filter = "" &&
             Array.length chooser.intro > 0 then
            min 3 (Array.length chooser.intro) + 1
          else 0 in
        let page = max 0 (body_height - 1 - intro_height) in
        if chooser.selected < chooser.offset then chooser.offset <- chooser.selected;
        if page > 0 && chooser.selected >= chooser.offset + page then
          chooser.offset <- chooser.selected - page + 1;
        chooser.offset <- min chooser.offset (max 0 (count - page));
        I.vcat (List.init body_height (fun i ->
          if i = 0 then styled_line cols accent ("  " ^ chooser.title)
          else if i <= intro_height then
            if i = intro_height then I.void cols 1
            else styled_line cols muted ("  " ^ chooser.intro.(i - 1))
          else
            let index = chooser.offset + i - 1 - intro_height in
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
              if total = 0 then (
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
                    styled_visual cols
                      (Transcript_view.visual_at layout (first + index))))) in
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
    | None when hint_height > 0 ->
        let selected = List.nth hints t.hint_selected in
        let label = selected.name ^ " · " ^ selected.summary in
        if cols < 45 then "  " ^ label
        else "  " ^ label ^ "   ·   ↑↓ move · Tab/Enter insert · Esc close"
    | None ->
        let status = match Pave.Composer.search_query t.editor with
          | None -> t.status
          | Some _ ->
              "reverse search: " ^
              (match Pave.Composer.search_match t.editor with
              | None -> "(no match)"
              | Some value -> sanitize (String.split_on_char '\n' value |> List.hd)) ^
              " · Ctrl+R older · Enter recall · Esc cancel" in
        if cols < 45 then
          (if status = idle_status then
            (if t.queue > 0 then Printf.sprintf "q%d · " t.queue else "") ^
            "Alt+O details · PgUp/Dn scroll"
           else status)
        else
          (if total = 0 then "  "
           else if body_height = 0 then Printf.sprintf "  [0/%d] " total
           else Printf.sprintf "  [%d-%d/%d] " (first + 1) last total) ^ status in
  let footer = styled_line cols text_attr footer_text in
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
    Array.init body_height (fun row ->
      if row < body_height - hint_height then
        I.vcrop row (body_height - row - 1) body
      else
        let index = row - (body_height - hint_height) in
        if index = 0 then
          styled_line cols accent
            "  / Commands · ↑↓ move · Tab/Enter insert · Esc close"
        else
          let choice = List.nth hints (t.hint_offset + index - 1) in
          hint_row cols (t.hint_offset + index - 1 = t.hint_selected) choice);
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

let paint_resized t =
  (match t.layout_cache, t.body_cache with
  | Some (_, _, old_layout), Some (_, old_height, _, _)
    when t.scroll > 0 && old_height > 0 && old_layout.total > 0 ->
      let first = max 0 (old_layout.total - old_height - t.scroll) in
      let old_entry = Transcript_view.visual_at old_layout first in
      let cols, rows = Notty_unix.Term.size t.term in
      let cols = max 1 cols and rows = max 1 rows in
      let measure chunk = I.width (I.string text_attr chunk) in
      let content_cols = if cols <= 6 then cols else cols - 5 in
      let next = Transcript_view.snapshot t.transcript
        ~columns:content_cols ~measure in
      let prefix_width = I.width (I.string accent prompt) in
      let field_width = if cols <= prefix_width then cols
        else cols - prefix_width in
      let editor_height = match t.chooser, Pave.Composer.search_query t.editor with
        | Some _, _ | None, Some _ -> 1
        | None, None ->
            let editor_lines = Pave.Composer.layout ~columns:field_width
              ~measure t.editor in
            min 4 (max 1 (min (rows - 4) (Array.length editor_lines))) in
      let height = max 0 (rows - 4 - editor_height) in
      let anchor = ref None in
      Array.iter (fun (entry : Transcript_view.entry) ->
        if entry.source = old_entry.source then anchor := Some entry.start)
        next.entries;
      Option.iter (fun position ->
        t.scroll <- max 0 (next.total - height - position))
        !anchor;
      t.layout_cache <- Some (cols, t.transcript.revision, next);
      t.revision <- t.revision + 1
  | _ -> ());
  paint t

let scroll_by t delta =
  t.scroll <- max 0 (t.scroll + delta);
  t.revision <- t.revision + 1;
  paint t

let create ~root ~model ~session =
  let term = Notty_unix.Term.create ~mouse:false ~bpaste:true () in
  let t = { term; input = Terminal_input.create term;
    root; model; session; editor = Pave.Composer.create ();
    transcript = Transcript_view.create (); scroll = 0; chooser = None;
    hint_draft = ""; hint_selected = 0; hint_offset = 0;
    hint_suppressed = None;
    revision = 0; body_cache = None; layout_cache = None;
    previous = None; status = idle_status; activity = None;
    activity_started = None; usage_badge = None; queue = 0;
    last_paint = 0.; paste = false } in
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

let set_session t session =
  t.session <- session;
  paint t

let set_activity t activity =
  if t.activity <> activity then (
    (match t.activity, activity with
     | None, Some _ -> t.activity_started <- Some (Unix.gettimeofday ())
     | _, None -> t.activity_started <- None
     | Some _, Some _ -> ());
    t.activity <- activity;
    paint t)
let set_usage t = function
  | None ->
      t.usage_badge <- None;
      paint t
  | Some (tokens : Pave.Protocol.usage) ->
      t.usage_badge <- Some (Printf.sprintf " · %d in/%d out"
        tokens.input_tokens tokens.output_tokens);
      paint t

let set_queue t count =
  t.queue <- max 0 count;
  paint t

let show_history t (messages : Pave.Protocol.message list) =
  let names = Hashtbl.create 32 in
  Transcript_view.clear t.transcript;
  List.iter (fun (message : Pave.Protocol.message) ->
    match message.role with
    | "user" ->
        Option.iter (Transcript_view.sent t.transcript) message.content
    | "assistant" ->
        (match message.content with
        | Some content when content <> "" ->
            Transcript_view.assistant t.transcript content
        | _ -> ());
        List.iter (fun (call : Pave.Protocol.tool_call) ->
          let group = Transcript_view.start_tool t.transcript call.name in
          Hashtbl.replace names call.id (call.name, group))
          message.tool_calls
    | "tool" ->
        let name, group = match message.tool_call_id with
          | Some id -> (match Hashtbl.find_opt names id with
            | Some (name, group) -> name, Some group
            | None -> "tool", None)
          | None -> "tool", None in
        Option.iter (fun result ->
          Transcript_view.tool_result ?group t.transcript name result;
          Option.iter (Hashtbl.remove names) message.tool_call_id)
          message.content
    | _ -> ()) messages;
  Hashtbl.iter (fun _ (name, group) ->
    Transcript_view.interrupt_tool t.transcript name group) names;
  t.transcript.pending_tool <- None;
  transcript_changed t;
  t.scroll <- 0;
  t.status <- idle_status;
  paint t

let finish_live t =
  change_transcript t (fun () -> Transcript_view.finish t.transcript);
  t.status <- idle_status;
  paint t

let sent t text =
  change_transcript t (fun () -> Transcript_view.sent t.transcript text);
  t.scroll <- 0;
  t.status <- idle_status;
  paint t

let event t text =
  change_transcript t (fun () -> Transcript_view.event t.transcript text);
  paint t

let events t lines =
  (match lines with
  | [] -> ()
  | _ ->
      change_transcript t (fun () ->
        Transcript_view.notice t.transcript (String.concat "\n" lines));
      t.status <- idle_status;
      paint t)

let delta t chunk =
  change_transcript t (fun () -> Transcript_view.delta t.transcript chunk);
  if chunk = "\n" || Unix.gettimeofday () -. t.last_paint > 0.033 then paint t

let clear_live t =
  change_transcript t (fun () -> Transcript_view.rollback t.transcript);
  t.status <- idle_status;
  paint t

let alert t message =
  t.status <- single_line message;
  paint t

let utf8 uchar =
  let buffer = Buffer.create 4 in
  Buffer.add_utf_8_uchar buffer uchar;
  Buffer.contents buffer

let repaint_after_key t =
  if (not t.paste && not (Terminal_input.pending t.input))
    || Unix.gettimeofday () -. t.last_paint >= 0.1 then paint t

let rec next_input ?wake_fd t =
  let timeout = match t.activity_started with
    | None -> None
    | Some since ->
        let elapsed = Unix.gettimeofday () -. since in
        Some (max 0. (1. -. (elapsed -. floor elapsed))) in
  match Terminal_input.event ?wake_fd ?timeout t.input with
  | `Tick -> paint t; next_input ?wake_fd t
  | event -> event

let read ?wake_fd ?on_wake ?on_interrupt ?on_completion t =
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
    match next_input ?wake_fd t with
    | `End -> None
    | `Wake ->
        (match on_wake with Some callback -> callback ()
         | None -> invalid_arg "Tui.read: wake_fd requires on_wake");
        loop ()
    | `Resize _ -> paint_resized t; loop ()
    | `Paste `Start ->
        t.paste <- true;
        if Pave.Composer.search_query t.editor = None then
          Pave.Composer.begin_paste t.editor;
        paint t; loop ()
    | `Paste `End ->
        t.paste <- false;
        Pave.Composer.end_paste t.editor;
        t.hint_draft <- Pave.Composer.text t.editor;
        dismiss_hint t;
        paint t; loop ()
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
    | `Key (`Arrow `Up, []) when hints_visible t ->
        t.hint_selected <- max 0 (t.hint_selected - 1);
        changed (); loop ()
    | `Key (`Arrow `Down, []) when hints_visible t ->
        t.hint_selected <- min (List.length (hint_matches t) - 1)
          (t.hint_selected + 1);
        changed (); loop ()
    | `Key (`Escape, _) when hints_visible t ->
        dismiss_hint t; changed (); loop ()
    | `Key (`Tab, _) when hints_visible t ->
        Option.iter (insert_hint t) (selected_hint t);
        changed (); loop ()
    | `Key (`Tab, _) ->
        (match on_completion with
        | None -> ()
        | Some complete ->
            let draft = Pave.Composer.text t.editor in
            if String.starts_with ~prefix:"/" draft &&
              not (String.exists (fun char ->
                char = ' ' || char = '\n' || char = '\t') draft) then
              (match complete draft with
               | Some selected when String.starts_with ~prefix:draft selected ->
                   Pave.Composer.finish t.editor;
                   Pave.Composer.insert t.editor
                     (String.sub selected (String.length draft)
                       (String.length selected - String.length draft));
                   changed ()
               | _ -> ()));
        loop ()
    | `Key (`Enter, mods) when not (List.mem `Shift mods) &&
        hints_visible t ->
        Option.iter (insert_hint t) (selected_hint t);
        changed (); loop ()
    | `Key (`Enter, mods) ->
        if t.paste || List.mem `Shift mods then
          (Pave.Composer.insert t.editor "\n"; changed (); loop ())
        else (match Pave.Composer.submit t.editor with
          | None -> loop ()
          | Some value -> paint t; Some value)
    | `Key (`Page `Up, _) -> scroll_by t (view_height t); loop ()
    | `Key (`Page `Down, _) -> scroll_by t (-view_height t); loop ()
    | `Key (`ASCII 'o', [ `Meta ]) ->
        let cols, rows = Notty_unix.Term.size t.term in
        let measure chunk = I.width (I.string text_attr chunk) in
        let layout = match t.layout_cache with
          | Some (width, revision, layout)
            when width = cols && revision = t.transcript.revision -> layout
          | _ -> Transcript_view.snapshot t.transcript
              ~columns:(if cols <= 6 then max 1 cols else cols - 5)
              ~measure in
        let visible = layout.total in
        let height = max 1 (rows - 5) in
        let first = max 0 (visible - height - t.scroll) in
        let last = min visible (first + height) in
        let source_first = if first < visible then
          (Transcript_view.visual_at layout first).source else 0 in
        let source_last = if last > 0 then
          (Transcript_view.visual_at layout (last - 1)).source else 0 in
        let selected = ref None in
        change_transcript t (fun () ->
          selected := Transcript_view.toggle t.transcript ~first:source_first
            ~last:source_last);
        (match !selected with
        | None -> ()
        | Some group ->
            let expanded = Transcript_view.snapshot t.transcript
              ~columns:(if cols <= 6 then max 1 cols else cols - 5)
              ~measure in
            let target = ref None in
            Array.iter (fun (entry : Transcript_view.entry) ->
              if entry.row.group = group &&
                entry.row.style = Transcript_view.Heading &&
                !target = None then target := Some entry.start)
              expanded.entries;
            Option.iter (fun start ->
              t.scroll <- max 0 (expanded.total - height - start))
              !target);
        paint t; loop ()
    | `Key (`Home, [ `Ctrl ]) ->
        t.scroll <- max_int; t.revision <- t.revision + 1; paint t; loop ()
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
    | `Key (`ASCII 'b', [ `Meta ]) ->
        Pave.Composer.word_left t.editor; changed (); loop ()
    | `Key (`ASCII 'f', [ `Meta ]) ->
        Pave.Composer.word_right t.editor; changed (); loop ()
    | `Key (`ASCII 'W', [ `Ctrl ]) | `Key (`Backspace, [ `Meta ]) ->
        Pave.Composer.erase_word t.editor; changed (); loop ()
    | `Key (`Backspace, _) -> Pave.Composer.erase t.editor; changed (); loop ()
    | `Key (`Delete, _) -> Pave.Composer.delete t.editor; changed (); loop ()
    | `Key (`ASCII 'Z', [ `Ctrl ]) ->
        Pave.Composer.undo t.editor; changed (); loop ()
    | `Key (`ASCII 'Y', [ `Ctrl ]) ->
        Pave.Composer.redo t.editor; changed (); loop ()
    | `Key (`ASCII 'K', [ `Ctrl ]) ->
        Pave.Composer.kill_to_end t.editor; changed (); loop ()
    | `Key (`ASCII 'U', [ `Ctrl ]) ->
        Pave.Composer.kill_before t.editor; changed (); loop ()
    | `Key (`ASCII 'y', [ `Meta ]) ->
        Pave.Composer.yank t.editor; changed (); loop ()
    | `Key (`Home, _) -> Pave.Composer.home t.editor; changed (); loop ()
    | `Key (`End, _) -> Pave.Composer.finish t.editor; changed (); loop ()
    | `Key (`ASCII 'A', [ `Ctrl ]) ->
        Pave.Composer.beginning_of_line t.editor; changed (); loop ()
    | `Key (`ASCII 'E', [ `Ctrl ]) ->
        Pave.Composer.end_of_line t.editor; changed (); loop ()
    | `Key (`ASCII 'R', [ `Ctrl ]) ->
        Pave.Composer.search_older t.editor; paint t; loop ()
    | `Key (`ASCII 'D', [ `Ctrl ]) when Pave.Composer.text t.editor = "" -> None
    | `Key (`ASCII c, []) when Char.code c >= 32 ->
        Pave.Composer.insert t.editor (String.make 1 c); changed (); loop ()
    | `Key (`Uchar uchar, []) ->
        Pave.Composer.insert t.editor (utf8 uchar); changed (); loop ()
    | _ -> loop () in
  Fun.protect ~finally:(fun () ->
    if t.paste then (
      t.paste <- false;
      Pave.Composer.end_paste t.editor)) loop

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

let choose ?(allow_custom = false) ?(intro = []) ?wake_fd ?on_wake ?dynamic t
    ~title ~choices =
  let cols, rows = Notty_unix.Term.size t.term in
  if cols < 9 || rows < 2 then (
    alert t "Resize terminal (at least 9 columns × 2 rows) to select";
    None)
  else
  let suggestions = Array.of_list choices in
  let chooser = { title = sanitize title;
    intro = Array.of_list (List.map sanitize intro); suggestions;
    choices = Array.map (fun value ->
      { value; custom = false; verified = false }) suggestions;
    allow_custom; dynamic = Option.value dynamic
      ~default:(Option.is_some wake_fd); status = None;
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
      match next_input ?wake_fd t with
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

let reviewable_command command =
  String.length command <= 4096 && sanitize command = command &&
  Uutf.String.fold_utf_8 (fun valid _ -> function
    | `Malformed _ -> false
    | `Uchar uchar ->
        let code = Uchar.to_int uchar in
        valid && (code = 10 || code >= 32) && code <> 127 &&
        not (code >= 0x80 && code <= 0x9f) &&
        not (code >= 0x200b && code <= 0x200f) &&
        not (code >= 0x202a && code <= 0x202e) &&
        not (code >= 0x2066 && code <= 0x2069) &&
        code <> 0x61c && code <> 0xad &&
        code <> 0x2060 && code <> 0xfeff) true command

let confirm t command =
  let command_lines = if String.length command > 4096 then []
    else String.split_on_char '\n' (sanitize command) in
  let fits () =
    let cols, rows = Notty_unix.Term.size t.term in
    let content_cols = max 1 (cols - 5) in
    let measure text = I.width (I.string text_attr text) in
    let editor_height = match Pave.Composer.search_query t.editor with
      | Some _ -> 1
      | None ->
          let prompt_cols = max 1 (cols - I.width (I.string accent prompt)) in
          let draft = Pave.Composer.layout ~columns:prompt_cols ~measure t.editor in
          min 4 (max 1 (min (rows - 4) (Array.length draft))) in
    let header_height = Transcript_view.wrapped_count ~columns:content_cols
      ~measure "SHELL APPROVAL · review before deciding" in
    t.chooser = None && String.length command <= 4096 &&
    cols >= 25 && rows >= 10 &&
    rows - 4 - editor_height >=
      header_height + 1 + List.length command_lines &&
    List.for_all (fun line -> measure line <= content_cols) command_lines in
  if String.length command > 4096 then (
    alert t "Shell command denied: too large to review on screen";
    false)
  else if not (reviewable_command command) then (
    alert t "Shell command denied: hidden/control text cannot be reviewed";
    false)
  else if not (fits ()) then (
    alert t "Shell command denied: too large to review on screen";
    false)
  else (
    change_transcript t (fun () -> Transcript_view.approval t.transcript command);
    t.scroll <- 0;
    alert t "SHELL: y=yes · other=no";
    let rec decision () = match next_input t with
      | `Resize _ -> if fits () then (paint t; decision ()) else false
      | `Key (`ASCII ('y' | 'Y'), []) -> true
      | _ -> false in
    let accepted = decision () in
    alert t (if accepted then "Shell command approved" else "Shell command denied");
    accepted)
