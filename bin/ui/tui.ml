open Notty

(* Transcript limits live in Transcript_view; no duplicate storage here. *)
let prompt = "  ❯ "

type candidate = {
  value : string;
  label : string;
  custom : bool;
  verified : bool;
  action : bool;
  detail : string option;
}

type chooser = {
  title : string;
  intro : string array;
  plain : string list;
  mutable choices : candidate array;
  allow_custom : bool;
  dynamic : bool;
  mutable status : string option;
  mutable status_pages : string array;
  mutable status_page : int;
  mutable filter : string;
  mutable selected : int;
  mutable offset : int;
  mutable touched : bool;
  mutable filtered : (string * candidate array) option;
  mutable matched_models : int;
}

type overlay_focus = Chooser_overlay | Approval_overlay

type submission = {
  text : string;
  follow_up : bool;
  paste_ranges : (int * int) list;
}
type completion = { start : int; stop : int; value : string }
exception Terminal_signal of int
type listing_update = {
  verified : string list;
  details : (string * string) list;
  labels : (string * string) list;
  status : string option;
  status_pages : string list;
}

type approval_request = {
  title : string;
  label : string;
  body : string;
  max_bytes : int;
  wrap : bool;
  too_large : string;
  unsafe_text : string;
  approved_text : string;
  denied_text : string;
  mutable result : bool option;
}

type terminal_event =
  [ Notty.Unescape.event | `Resize of int * int | `End | `Wake | `Tick ]

type ui_event =
  | Terminal_event of terminal_event
  | Agent_event of Pave.Turn_runner.event
  | Listing_event of listing_update
  | Approval_event of approval_request
  | Background_message of string
  | Shutdown



type tool_progress = {
  call_id : string;
  name : string;
  mutable received_bytes : int option;
}

type attachment_preview = { name : string; mime_type : string; size : int }
type inline_hint =
  | Command_hint of Pave.Interaction.shortcut
  | File_hint of Pave.File_mentions.completion_context *
      Pave.File_mentions.candidate

type t = {
  mutable term : Notty_unix.Term.t;
  mutable input : Terminal_input.t;
  root : string;
  version : string;
  mutable model : string;
  mutable model_display_name : string option;
  mutable session : bool;
  editor : Pave.Composer.t;
  transcript : Transcript_view.t;
  tool_groups : (string, int) Hashtbl.t;
  mutable scroll : int;
  mutable chooser : chooser option;
  mutable overlays : overlay_focus list;
  mutable hint_draft : string;
  mutable hint_cursor : int;
  mutable hint_results : inline_hint list;
  mutable hint_truncated : bool;
  mutable hint_selected : int;
  mutable hint_offset : int;
  mutable hint_suppressed : string option;
  mutable revision : int;
  mutable body_cache : (int * int * int * I.t) option;
  mutable layout_cache : (int * int * Transcript_view.snapshot) option;
  mutable location_cache : (int * I.t) option;
  mutable previous : I.t array option;
  mutable cursor_position : (int * int) option;
  mutable status : string;
  mutable activity : string option;
  mutable activity_started : float option;
  mutable active_tool : tool_progress option;
  mutable usage_badge : string option;
  mutable pending_attachments : attachment_preview list;

  mutable queue : int;
  mutable last_paint : float;
  mutable paste : bool;
  paste_buffer : Buffer.t;
  bindings : Keybindings.binding list;
  signals : (int * Sys.signal_behavior) list;
  ui_events : ui_event Queue.t;
  ui_lock : Mutex.t;
  ui_read_fd : Unix.file_descr;
  ui_write_fd : Unix.file_descr;
  ui_wake_byte : bytes;
  ui_drain_bytes : bytes;
  mutable ui_closed : bool;
  mutable agent_event_handler : (Pave.Turn_runner.event -> unit) option;

}
let create_ui_pipe () =
  let read_fd, write_fd = Unix.pipe () in
  try
    Unix.set_close_on_exec read_fd;
    Unix.set_close_on_exec write_fd;
    Unix.set_nonblock read_fd;
    Unix.set_nonblock write_fd;
    read_fd, write_fd
  with exn ->
    Unix.close read_fd;
    Unix.close write_fd;
    raise exn

let enqueue_ui_event t event =
  Mutex.lock t.ui_lock;
  let wake = not t.ui_closed && Queue.is_empty t.ui_events in
  if not t.ui_closed then Queue.add event t.ui_events;
  Mutex.unlock t.ui_lock;
  if wake then (
    let rec write () =
      try ignore (Unix.write t.ui_write_fd t.ui_wake_byte 0 1)
      with
      | Unix.Unix_error (Unix.EINTR, _, _) -> write ()
      | Unix.Unix_error ((Unix.EAGAIN | Unix.EPIPE | Unix.EBADF), _, _) -> () in
    write ())

let drain_ui_pipe t =
  let rec drain () =
    try
      let count = Unix.read t.ui_read_fd t.ui_drain_bytes 0
        (Bytes.length t.ui_drain_bytes) in
      if count > 0 then drain ()
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> drain ()
    | Unix.Unix_error (Unix.EAGAIN, _, _) -> () in
  drain ()

let max_delta_batch_bytes = 16_384
let max_delta_batch_events = 64

let coalesce_text_deltas first queue =
  match first with
  | Agent_event (Pave.Turn_runner.Text_delta { turn_id; text }) ->
      let length = ref (String.length text) and count = ref 1 in
      let buffer = ref None in
      let rec gather () =
        if !length < max_delta_batch_bytes &&
           !count < max_delta_batch_events && not (Queue.is_empty queue) then
          match Queue.peek queue with
          | Agent_event (Pave.Turn_runner.Text_delta
              { turn_id = next_turn; text = next_text })
            when next_turn = turn_id &&
                 String.length next_text <= max_delta_batch_bytes - !length ->
              ignore (Queue.take queue);
              let output = match !buffer with
                | Some output -> output
                | None ->
                    let output = Buffer.create
                      (min max_delta_batch_bytes
                        (!length + String.length next_text)) in
                    Buffer.add_string output text;
                    buffer := Some output;
                    output in
              Buffer.add_string output next_text;
              length := !length + String.length next_text;
              incr count;
              gather ()
          | _ -> () in
      gather ();
      (match !buffer with
       | None -> first
       | Some output ->
           Agent_event (Pave.Turn_runner.Text_delta {
             turn_id; text = Buffer.contents output
           }))
  | _ -> first

let stream_frame_interval = 1. /. 60.

let pop_ui_event t =
  drain_ui_pipe t;
  Mutex.lock t.ui_lock;
  let event =
    if Queue.is_empty t.ui_events then None
    else
      let first = Queue.take t.ui_events in
      Some (coalesce_text_deltas first t.ui_events) in
  Mutex.unlock t.ui_lock;
  event


let ui_events_pending t =
  Mutex.lock t.ui_lock;
  let pending = not (Queue.is_empty t.ui_events) in
  Mutex.unlock t.ui_lock;
  pending

let set_agent_event_handler t handler =
  t.agent_event_handler <- Some handler

let publish_agent_event t event =
  enqueue_ui_event t (Agent_event event)

let post_message t message =
  enqueue_ui_event t (Background_message message)

let shutdown t = enqueue_ui_event t Shutdown

let close_ui_pipe t =
  Mutex.lock t.ui_lock;
  let close = not t.ui_closed in
  t.ui_closed <- true;
  Queue.clear t.ui_events;
  Mutex.unlock t.ui_lock;
  if close then (
    Unix.close t.ui_read_fd;
    Unix.close t.ui_write_fd)

let listing_handler : (t -> listing_update -> unit) ref =
  ref (fun _ _ -> ())

let approval_handler : (t -> approval_request -> unit) ref =
  ref (fun _ request -> request.result <- Some false)


let no_color = match Sys.getenv_opt "NO_COLOR" with Some s -> s <> "" | None -> false
let text_attr = if no_color then A.empty else A.(fg lightwhite)
let accent = if no_color then A.(st bold) else A.(fg lightcyan ++ st bold)
let user_attr = if no_color then A.(st bold) else A.(fg lightblue ++ st bold)
let muted = if no_color then A.empty else A.(fg lightblack)
let warning = if no_color then A.empty else A.(fg lightyellow)
let error = if no_color then A.empty else A.(fg lightred)
let selected_attr = if no_color then A.(st bold)
  else A.(fg black ++ bg lightcyan ++ st bold)
let measure_text chunk = I.width (I.string text_attr chunk)

let macos = Sys.os_type = "Unix" && Sys.file_exists "/System/Library"
let meta_key = if macos then "Option" else "Alt"
let enter_key = if macos then "Return" else "Enter"

let idle_status =
  enter_key ^ " steer · " ^ meta_key ^ "+" ^ enter_key ^ " follow-up · /queue"

let hotkeys = Keybindings.hotkeys Keybindings.bindings

(* Two ASCII columns per 8px SVG pixel keep the rounded P nearly square. *)
let startup_logo version =
  let mint = I.string accent "##" and shadow = I.string muted "++"
  and cursor = I.string warning "**" and blank = I.string A.empty "  " in
  let pixel = function
    | '#' -> mint | '+' -> shadow | '*' -> cursor | _ -> blank in
  let mark = I.vcat (List.map (fun row ->
    I.hcat (List.init (String.length row) (fun index -> pixel row.[index])))
    [ "   ######  "; "  ######## "; " ##########";
      " ###    ###"; " ###    ###"; " ###    ###";
      " ##########"; " #########+";
      " ###++++++ "; " ###       "; " ###     * ";
      "  +++      " ]) in
  I.(mark <-> void 1 1 <->
    string accent ("  P A V E  " ^ version))

let sanitize = Transcript_view.sanitize
let single_line = Transcript_view.single_line

let transcript_changed t =
  t.revision <- t.revision + 1;
  t.layout_cache <- None

let change_transcript t action =
  let before, cols = if t.scroll = 0 then 0, 0 else (
    let cols, _ = Notty_unix.Term.size t.term in
    let content_cols = if cols <= 4 then max 1 cols else cols - 4 in
    let measure = measure_text in
    let layout = match t.layout_cache with
      | Some (width, revision, layout)
        when width = cols && revision = t.transcript.revision -> layout
      | _ -> Transcript_view.snapshot t.transcript ~columns:content_cols ~measure in
    layout.total, content_cols) in
  action ();
  transcript_changed t;
  if t.scroll > 0 then (
    let after = (Transcript_view.snapshot t.transcript ~columns:cols
      ~measure:measure_text).total in
    t.scroll <- max 0 (t.scroll + after - before))

let style_attr (row : Transcript_view.row) =
  match row.kind, row.style with
  | Transcript_view.Error, _ -> error
  | Transcript_view.Approval, _ -> warning
  | Transcript_view.User, Transcript_view.Heading -> user_attr
  | Transcript_view.Assistant, Transcript_view.Heading -> accent
  | Transcript_view.Tool, Transcript_view.Heading -> warning
  | _, (Transcript_view.Heading | Transcript_view.Subheading) -> accent
  | _, Transcript_view.Table_header -> accent
  | _, Transcript_view.Table_separator -> muted
  | _, Transcript_view.Code ->
      if no_color then text_attr else A.(fg lightgreen)
  | _, Transcript_view.Quote -> muted
  | _, Transcript_view.Tool_state -> muted
  | _, _ -> text_attr

let inline_attr = function
  | Transcript_view.Plain -> A.empty
  | Transcript_view.Bold -> A.(st bold)
  | Transcript_view.Inline_code ->
      if no_color then A.(st bold) else A.(fg lightyellow)
  | Transcript_view.Link ->
      if no_color then A.(st underline)
      else A.(fg lightblue ++ st underline)

let transcript_prefix style continuation =
  match style with
  | Transcript_view.Heading -> if continuation then "    " else "  ▌ "
  | Transcript_view.Divider -> ""
  | Transcript_view.Tool_state -> "  ↳ "
  | Transcript_view.Code -> "  │ "
  | Transcript_view.Quote -> if continuation then "    " else "  │ "
  | Transcript_view.List_item -> if continuation then "    " else "  • "
  | Transcript_view.Table_header
  | Transcript_view.Table_row
  | Transcript_view.Table_separator -> "  "
  | _ -> "  "

let styled_visual cols (visual : Transcript_view.visual) =
  let row = visual.row in
  let prefix = match row.style, row.kind, visual.continuation with
    | Transcript_view.Heading, Transcript_view.User, false -> "  ◆ "
    | Transcript_view.Heading, Transcript_view.Tool, false -> "  ◇ "
    | Transcript_view.Heading, Transcript_view.Error, false -> "  ! "
    | _ -> transcript_prefix row.style visual.continuation in
  let attr = style_attr row in
  let prefix = if cols <= I.width (I.string attr prefix) then "" else prefix in
  let body = if Array.length visual.runs = 0 then
    I.string attr visual.text
    else I.hcat (Array.fold_right
      (fun (run : Transcript_view.inline_run) images ->
        I.string A.(attr ++ inline_attr run.style) run.content :: images)
      visual.runs []) in
  I.hsnap ~align:`Left cols I.(string attr prefix <|> body)

let styled_line width attr text =
  I.hsnap ~align:`Left width (I.string attr text)

let activity_frames =
  [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]
let activity_tick = 1.

let activity_tick_delay elapsed =
  let phase = mod_float (max 0. elapsed) activity_tick in
  if phase = 0. then activity_tick else activity_tick -. phase
let draft_paste_capacity editor =
  let selected = match Pave.Composer.selection editor with
    | Some (start, stop) -> stop - start
    | None -> 0 in
  max 0 (16_384 - String.length (Pave.Composer.text editor) + selected)

let activity_started_at started activity now =
  match activity with
  | None -> None
  | Some _ -> Some (Option.value ~default:now started)


let received_bytes_text bytes =
  let bytes = max 0 bytes in
  if bytes < 1024 then Printf.sprintf "%d B" bytes
  else if bytes < 1024 * 1024 then
    Printf.sprintf "%.1f KiB" (float bytes /. 1024.)
  else Printf.sprintf "%.1f MiB" (float bytes /. (1024. *. 1024.))

let attachment_size data =
  let length = String.length data in
  if length = 0 then 0
  else
    let padding =
      if data.[length - 1] <> '=' then 0
      else if length > 1 && data.[length - 2] = '=' then 2
      else 1 in
    max 0 ((length / 4 * 3) - padding)

let preview_attachment (item : Pave.Protocol.attachment) =
  { name = single_line item.name; mime_type = single_line item.mime_type;
    size = attachment_size item.data }

let preview_attachments attachments = List.map preview_attachment attachments

let attachment_preview_text preview =
  let kind = match Pave.Protocol.attachment_kind preview.mime_type with
    | Some Pave.Protocol.Image_attachment -> "image"
    | Some Pave.Protocol.Audio_attachment -> "audio"
    | Some Pave.Protocol.Video_attachment -> "video"
    | None -> "media" in
  Printf.sprintf "  [%s] %s · %s · %s" kind preview.name
    preview.mime_type (received_bytes_text preview.size)

let attachment_block text previews =
  match previews with
  | [] -> text
  | previews ->
      let details = String.concat "\n"
        (List.map attachment_preview_text previews) in
      let block = "[Attached media:\n" ^ details ^ "\n]" in
      if text = "" then block else text ^ "\n" ^ block


let shorten_width width text =
  if width < 2 then "" else
  let measure = measure_text in
  if measure text <= width then text
  else (Transcript_view.wrap ~columns:(width - 1) ~measure text).(0) ^ "…"

let shorten_middle width text =
  if measure_text text <= width then text
  else if width < 5 then shorten_width width text
  else
    let boundaries = Pave.Composer.segment text in
    let count = Array.length boundaries - 1 in
    let left_width = (width - 1) / 2 in
    let rec left index used =
      if index >= count then boundaries.(count)
      else
        let start = boundaries.(index) and stop = boundaries.(index + 1) in
        let next = used + measure_text (String.sub text start (stop - start)) in
        if next > left_width then start else left (index + 1) next in
    let rec right index used =
      if index <= 0 then 0
      else
        let start = boundaries.(index - 1) and stop = boundaries.(index) in
        let next = used + measure_text (String.sub text start (stop - start)) in
        if next > width - 1 - left_width then stop
        else right (index - 1) next in
    let prefix_end = left 0 0 and suffix_start = right count 0 in
    String.sub text 0 prefix_end ^ "…" ^
    String.sub text suffix_start (String.length text - suffix_start)

let shorten_model_label width label =
  if width < 5 then shorten_width width label else
  match String.index_opt label ' ' with
  | Some split when String.length label >= split + 4 &&
      String.sub label split 4 = " · " ->
      let scope = String.sub label 0 split in
      let model = String.sub label (split + 4)
        (String.length label - split - 4) in
      let separator = " · " in
      let available = max 0 (width - measure_text separator) in
      let scope = shorten_middle (min (available / 3) (measure_text scope))
        scope in
      scope ^ separator ^ shorten_middle
        (available - measure_text scope) model
  | _ -> shorten_middle width label

let shorten_activity width text =
  if String.starts_with ~prefix:"Tool: " text then
    let name = String.sub text 6 (String.length text - 6) in
    shorten_width width
      ("Tool: " ^ shorten_middle (max 0 (width - 6)) name)
  else shorten_width width text

let activity_status ?received_bytes ?(width = max_int) ~state ~elapsed () =
  let elapsed = max 0. elapsed in
  let seconds = int_of_float elapsed in
  let duration = if seconds < 60 then Printf.sprintf "%ds" seconds
    else Printf.sprintf "%dm%02ds" (seconds / 60) (seconds mod 60) in
  let frame = activity_frames.(seconds mod Array.length activity_frames) in
  let suffix = " · " ^ duration in
  let available = max 0 (width - measure_text frame - 1 - measure_text suffix) in
  let state = shorten_activity available state in
  let base = frame ^ " " ^ state ^ suffix in
  match received_bytes with
  | None -> base
  | Some bytes ->
      let progress = base ^ " · " ^ received_bytes_text bytes in
      if measure_text progress <= width then progress else base


let wrap_chooser_text ~columns ~max_rows text =
  if max_rows <= 0 then [||]
  else
    let columns = max 1 columns in
    let words = String.split_on_char ' ' (single_line text)
      |> List.filter (fun word -> word <> "") in
    let lines = ref [] in
    let current = Buffer.create (min columns 128) and used = ref 0 in
    let push () =
      if Buffer.length current > 0 then (
        lines := Buffer.contents current :: !lines;
        Buffer.clear current;
        used := 0) in
    let add word width =
      Buffer.add_string current word;
      used := !used + width in
    let append word =
      let width = measure_text word in
      if width > columns then (
        push ();
        let parts = Transcript_view.wrap ~columns ~measure:measure_text word in
        for index = 0 to Array.length parts - 2 do
          lines := parts.(index) :: !lines
        done;
        if Array.length parts > 0 then (
          let last = parts.(Array.length parts - 1) in
          Buffer.add_string current last;
          used := measure_text last))
      else if !used = 0 then add word width
      else
        let space_width = measure_text " " in
        if !used + space_width + width > columns then (
          push ();
          add word width)
        else (
          Buffer.add_char current ' ';
          used := !used + space_width;
          add word width) in
    List.iter append words;
    push ();
    let lines = Array.of_list (List.rev !lines) in
    let count = min max_rows (Array.length lines) in
    let visible = Array.sub lines 0 count in
    if count < Array.length lines then
      visible.(count - 1) <-
        shorten_width columns (visible.(count - 1) ^ "…");
    visible

let matches chooser =
  match chooser.filtered with
  | Some (filter, found) when filter = chooser.filter -> found
  | _ ->
      let query = String.lowercase_ascii chooser.filter in
      let includes text =
        let value = String.lowercase_ascii text in
        let n = String.length value and m = String.length query in
        let rec find pos =
          pos + m <= n &&
          (String.sub value pos m = query || find (pos + 1)) in
        find 0 in
      let found = ref [] and models = ref 0 in
      Array.iter (fun (item : candidate) ->
        if includes item.value || includes item.label then (
          if chooser.dynamic && not item.action then incr models;
          found := item :: !found)) chooser.choices;
      let found = List.rev !found in
      chooser.matched_models <- !models;
      let manual =
        not chooser.dynamic && chooser.allow_custom &&
        chooser.filter <> "" &&
        chooser.filter.[String.length chooser.filter - 1] <> '/' &&
        (String.contains chooser.filter '/' || found = []) &&
        not (List.exists (fun (item : candidate) ->
          item.value = chooser.filter) found) in
      let found = Array.of_list (if manual then
        { value = chooser.filter; label = chooser.filter; custom = true;
          verified = false; action = false; detail = None } :: found
        else found) in
      chooser.filtered <- Some (chooser.filter, found);
      found

let candidate_label chooser item =
  (if chooser.dynamic then
    if item.action then "↩ " else "• "
   else if item.custom then "Use: " else "") ^ sanitize item.label

let chooser_empty_message chooser =
  if chooser.filter <> "" then "No available models match this search"
  else "No available models yet"

let view_height t =
  let _, rows = Notty_unix.Term.size t.term in
  max 1 (rows - 6)

(* Hint state belongs to the editor, never to the transcript or the modal chooser.
   A dismissed/inserted draft remains quiet until the user edits it again. *)
let hint_matches t =
  let draft = Pave.Composer.text t.editor in
  let cursor = Pave.Composer.cursor t.editor in
  if t.hint_draft <> draft || t.hint_cursor <> cursor then (
    t.hint_draft <- draft;
    t.hint_cursor <- cursor;
    t.hint_selected <- 0;
    t.hint_offset <- 0;
    t.hint_suppressed <- None;
    t.hint_truncated <- false;
    t.hint_results <-
      if t.paste || t.overlays <> [] ||
         Pave.Composer.search_query t.editor <> None ||
         Option.is_some (Pave.Composer.selection t.editor) then []
      else match Pave.File_mentions.completion_context draft cursor with
        | Some context when cursor = String.length draft ->
            let listing = Pave.File_mentions.suggest_paths ~root:t.root
              context.prefix in
            t.hint_truncated <- listing.truncated;
            List.map (fun candidate -> File_hint (context, candidate))
              listing.candidates
        | _ when cursor = String.length draft &&
                 not (String.exists
                   (fun c -> c = ' ' || c = '\t' || c = '\n') draft) ->
            List.map (fun item -> Command_hint item)
              (Pave.Interaction.suggestions
                ~session:t.session ~interactive:true draft)
        | _ -> []);
  if t.paste || t.overlays <> [] ||
     Pave.Composer.search_query t.editor <> None ||
     Option.is_some (Pave.Composer.selection t.editor) ||
     t.hint_suppressed = Some draft then []
  else t.hint_results


let hint_room t =
  let cols, rows = Notty_unix.Term.size t.term in
  let prompt_width = I.width (I.string accent prompt) in
  let field_width = max 1 (cols - if cols <= prompt_width then 0
    else prompt_width) in
  let measure = measure_text in
  let lines = Pave.Composer.layout ~columns:field_width ~measure t.editor in
  let height = min 4 (max 1 (min (rows - 4) (Array.length lines))) in
  rows - 4 - height >= 2

let hints_visible t = hint_matches t <> [] && hint_room t
let key_focus t =
  let overlay = match t.overlays with
    | Approval_overlay :: _ -> Some Keybindings.Approval
    | Chooser_overlay :: _ -> Some Keybindings.Chooser
    | [] -> None in
  Keybindings.focus ~paste:t.paste ~overlay
    ~search:(Option.is_some (Pave.Composer.search_query t.editor))
    ~hints:(hints_visible t)

let selected_hint t =
  let matches = hint_matches t in
  if t.hint_selected < List.length matches then
    Some (List.nth matches t.hint_selected)
  else None

let dismiss_hint t =
  t.hint_suppressed <- Some (Pave.Composer.text t.editor)

let insert_hint t = function
  | Command_hint item ->
      let draft = Pave.Composer.text t.editor in
      Pave.Composer.finish t.editor;
      Pave.Composer.insert t.editor
        (String.sub item.name (String.length draft)
          (String.length item.name - String.length draft));
      t.hint_draft <- Pave.Composer.text t.editor;
      t.hint_cursor <- Pave.Composer.cursor t.editor;
      dismiss_hint t
  | File_hint (context, candidate) ->
      let value = Pave.File_mentions.render_reference
        ?quote:context.quote ~directory:candidate.is_directory
        candidate.path in
      if Pave.Composer.replace_range t.editor ~start:context.start
           ~stop:context.stop ~value then (
        if candidate.is_directory then t.hint_draft <- ""
        else (
          t.hint_draft <- Pave.Composer.text t.editor;
          t.hint_cursor <- Pave.Composer.cursor t.editor;
          dismiss_hint t))
      else t.status <- "Completion exceeds the composer input limit"

let hint_row cols selected = function
  | Command_hint item ->
      let marker = if selected then "  ❯ " else "    " in
      let usage = Pave.Interaction.usage item in
      let usage = if usage = "" then "" else " " ^ usage in
      I.hsnap ~align:`Left cols I.(
        string (if selected then accent else text_attr) (marker ^ item.name) <|>
        string (if selected then text_attr else muted)
          (usage ^ " · " ^ item.summary))
  | File_hint (_, candidate) ->
      let marker = if selected then "  ❯ " else "    " in
      let label = single_line candidate.path ^
        (if candidate.is_directory then "/" else "") in
      let detail = match candidate.preview with
        | None -> "directory"
        | Some (mime, size) ->
            single_line mime ^ " · " ^ received_bytes_text size in
      I.hsnap ~align:`Left cols I.(
        string (if selected then accent else text_attr) (marker ^ label) <|>
        string (if selected then text_attr else muted) (" · " ^ detail))

let paint t =
  let cols, rows = Notty_unix.Term.size t.term in
  let cols = max 1 cols and rows = max 1 rows in
  let prompt_width = I.width (I.string accent prompt) in
  let prompt = if cols <= prompt_width then "" else prompt in
  let prefix_width = if prompt = "" then 0 else prompt_width in
  let field_width = max 1 (cols - prefix_width) in
  let measure = measure_text in
  let editor_lines = Pave.Composer.layout ~columns:field_width ~measure t.editor in
  let editor_row, editor_col =
    Pave.Composer.position ~measure t.editor editor_lines in
  let activity_height = if Option.is_some t.activity then 1 else 0 in
  let attachment_height =
    if rows < 6 || Option.is_some t.chooser ||
       Option.is_some (Pave.Composer.search_query t.editor) then 0
    else min (List.length t.pending_attachments)
      (max 0 (rows - 6 - activity_height)) in
  let editor_space =
    max 1 (rows - 4 - activity_height - attachment_height) in
  let editor_height = match t.chooser, Pave.Composer.search_query t.editor with
    | Some _, _ | None, Some _ -> 1
    | None, None ->
        min 4 (max 1 (min editor_space (Array.length editor_lines))) in
  let body_height =
    max 0 (rows - 4 - editor_height - activity_height - attachment_height) in

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
  let activity_text = match t.activity with
    | None -> None
    | Some state ->
        let elapsed = match t.activity_started with
          | Some since -> max 0. (Unix.gettimeofday () -. since)
          | None -> 0. in
        let received_bytes = match t.active_tool with
          | Some progress when state = "Tool: " ^ progress.name ->
              progress.received_bytes
          | _ -> None in
        let indent = if cols < 40 then " " else "  " in
        Some (indent ^ activity_status ~state:(single_line state) ~elapsed
          ?received_bytes ~width:(max 0 (cols - String.length indent)) ()) in
  let activity_rows = match activity_text with
    | None -> [||]
    | Some text -> [| styled_line cols accent text |] in
  let attachment_rows =
    if attachment_height = 0 then [||]
    else
      let hidden = List.length t.pending_attachments - attachment_height in
      t.pending_attachments
      |> List.mapi (fun index preview -> index, preview)
      |> List.filter_map (fun (index, preview) ->
        if index >= attachment_height then None
        else
          let label = if hidden > 0 && index = attachment_height - 1 then
              let suffix = Printf.sprintf " · +%d more" hidden in
              shorten_width (max 0 (cols - measure suffix))
                (attachment_preview_text preview) ^ suffix
            else shorten_width cols (attachment_preview_text preview) in
          Some (styled_line cols text_attr label))
      |> Array.of_list in

  let queued = if t.queue = 0 then "" else
    Printf.sprintf "  ·  %d queued" t.queue in
  let usage = match t.activity, t.usage_badge with
    | None, Some badge when cols >= 28 && cols >= 9 + String.length badge ->
        badge
    | _ -> "" in
  let attached = match t.pending_attachments with
    | [] -> ""
    | names -> Printf.sprintf " · %d media attachment%s ready"
        (List.length names) (if List.length names = 1 then "" else "s") in

  let brand = "  ◆  PAVE " ^ t.version in
  let indicators = (if cols >= 48 then queued else "") ^ usage ^ attached in
  let header = I.hsnap ~align:`Left cols I.(
    string accent (shorten_width cols brand) <|>
    string muted (shorten_width (max 0 (cols - measure brand)) indicators)) in
  let location = match t.location_cache with
    | Some (width, image) when width = cols -> image
    | _ ->
      let model = single_line t.model in
      let model_id, model_scope = match String.index_opt model '/' with
        | None -> model, ""
        | Some split ->
            String.sub model (split + 1) (String.length model - split - 1),
            String.sub model 0 split in
      let model_name = Option.value ~default:model_id t.model_display_name
        |> single_line |> String.trim in
      let model_name = if model_name = "" then single_line model_id
        else model_name in
      let display_model width = shorten_middle width model_name in
      let image =
        if cols < 28 then styled_line cols accent
          (" " ^ display_model (cols - 1))
        else
          let badge = if cols < 60 then "  MODEL " else "  [MODEL] " in
          let state = if cols < 45 then
            (if t.session then " · SAVED" else " · UNSAVED")
          else if t.session then "  ·  SAVED" else "  ·  UNSAVED" in
          let space = max 0 (cols - measure badge - measure state) in
          let root = if cols < 60 then "" else
            let prefix = "  ·  " in
            prefix ^ shorten_middle
              (min (cols / 4) (max 0 (space - measure prefix - 12)))
              (single_line t.root) in
          let identity_space = max 0 (min (space / 3)
            (space - measure root - 20)) in
          let detail = if cols < 72 || model_scope = "" ||
              identity_space < 12 then ""
            else "  ·  " ^ shorten_middle (min 28 identity_space) model_scope in
          let name_width = max 0 (space - measure root - measure detail) in
          I.hsnap ~align:`Left cols I.(
            string accent badge <|>
            string text_attr (display_model name_width) <|>
            string muted state <|>
            string muted root <|>
            string muted detail) in
      t.location_cache <- Some (cols, image);
      image in
  let divider = I.uchar muted (Uchar.of_int 0x2500) cols 1 in
  let layout = match t.layout_cache with
    | Some (width, revision, layout)
      when width = cols && revision = t.transcript.revision -> layout
    | _ ->
        let content_cols = if cols <= 4 then cols else cols - 4 in
        let layout = Transcript_view.snapshot t.transcript ~columns:content_cols
          ~measure in
        t.layout_cache <- Some (cols, t.transcript.revision, layout);
        layout in
  let total = layout.total in
  t.scroll <- min t.scroll (max 0 (total - body_height));
  let first = max 0 (total - body_height - t.scroll) in
  let last = min total (first + body_height) in
  let visible_first, visible_last =
    if rows >= 6 then first, last
    else
      let spare = max 0 (rows - activity_height - 1 - editor_height) in
      let start = max 0 (total - spare - t.scroll) in
      start, min total (start + spare) in
  let body = match t.chooser with
    | Some chooser ->
        let found = matches chooser in
        let count = Array.length found in
        chooser.selected <- max 0 (min (count - 1) chooser.selected);
        let status_prefix = "  · " in
        let status_text = match chooser.status_pages with
          | [||] -> chooser.status
          | pages ->
              let summary = Option.value ~default:"" chooser.status in
              let detail = Printf.sprintf "Provider status %d/%d: %s"
                (chooser.status_page + 1) (Array.length pages)
                pages.(chooser.status_page) in
              Some (if summary = "" then detail
                else summary ^ " · " ^ detail) in
        let intro_spacer =
          cols >= 52 && body_height >= 9 && chooser.filter = "" &&
          Array.length chooser.intro > 0 in
        let compact_intro =
          cols >= 30 && cols < 52 && body_height >= 4 &&
          chooser.filter = "" && Array.length chooser.intro > 0 in
        let intro_rows =
          if intro_spacer then min 3 (Array.length chooser.intro) + 1
          else if compact_intro then 1
          else 0 in
        let empty_height =
          if chooser.dynamic && chooser.matched_models = 0 &&
            body_height >= (if count > 0 then 3 else 2) then 1 else 0 in
        let status_max_rows = max 0
          (min 3 (body_height - 1 - empty_height -
            (if count > 0 then 1 else 0) - intro_rows)) in
        let status_lines = match status_text with
          | Some status when body_height >= 3 ->
              wrap_chooser_text
                ~columns:(max 1 (cols - measure_text status_prefix))
                ~max_rows:status_max_rows status
          | _ -> [||] in
        let status_height = Array.length status_lines in
        let detail_prefix = "  ↳ " in
        let detail_lines =
          if cols >= 45 && body_height >= 6 && count > 0 then
            match found.(chooser.selected).detail with
            | Some detail ->
                wrap_chooser_text
                  ~columns:(max 1 (cols - measure_text detail_prefix))
                  ~max_rows:(min 4 (body_height - 2 - status_height -
                    intro_rows - empty_height)) detail
            | None -> [||]
          else [||] in
        let detail_height = Array.length detail_lines in
        let minimum_choices =
          if compact_intro then min 1 count else min 3 count in
        let intro_height =
          if body_height - 1 - status_height - detail_height - empty_height -
              intro_rows >= minimum_choices
          then intro_rows else 0 in
        let page = max 0
          (body_height - 1 - intro_height - status_height -
            detail_height - empty_height) in
        if chooser.selected < chooser.offset then chooser.offset <- chooser.selected;
        if page > 0 && chooser.selected >= chooser.offset + page then
          chooser.offset <- chooser.selected - page + 1;
        chooser.offset <- min chooser.offset (max 0 (count - page));
        I.vcat (List.init body_height (fun i ->
          if i = 0 then styled_line cols accent
            (if chooser.dynamic then
              Printf.sprintf "  ▌  %s  ·  %d available"
                chooser.title chooser.matched_models
             else Printf.sprintf "  ▌  %s  ·  %d matches"
                chooser.title count)
          else if i <= intro_height then
            if intro_spacer && i = intro_height then I.void cols 1
            else styled_line cols muted ("  " ^ chooser.intro.(i - 1))
          else if i <= intro_height + status_height then
            let index = i - intro_height - 1 in
            styled_line cols muted
              ((if index = 0 then status_prefix else "     ") ^
                status_lines.(index))
          else if i <= intro_height + status_height + empty_height then
            styled_line cols text_attr ("  " ^ chooser_empty_message chooser)
          else if i <= intro_height + status_height + empty_height +
              detail_height then
            let index = i - intro_height - status_height - empty_height - 1 in
            styled_line cols muted
              ((if index = 0 then detail_prefix else "    ") ^
                detail_lines.(index))
          else
            let index = chooser.offset + i - 1 - intro_height -
              status_height - empty_height - detail_height in
            if index >= count then I.void cols 1
            else let choice = found.(index) in
              let marker = if index = chooser.selected then
                (if cols < 40 then "❯ " else "  ❯ ")
              else if cols < 40 then "  " else "    " in
              let label = candidate_label chooser choice in
              let width = max 0 (cols - measure marker) in
              let label = if chooser.dynamic && not choice.action then
                let prefix = "• " in
                prefix ^ shorten_model_label
                  (max 0 (width - measure prefix)) (sanitize choice.label)
                else shorten_width width label in
              styled_line cols
                (if index = chooser.selected then selected_attr
                 else if choice.action then muted else text_attr)
                (marker ^ label)))
    | None ->
        (match t.body_cache with
        | Some (width, height, revision, body)
          when width = cols && height = body_height && revision = t.revision -> body
        | _ ->
            let body =
              if total = 0 then (
                let logo = startup_logo t.version in
                let logo_width = I.width logo
                and logo_height = I.height logo in
                if cols < logo_width || body_height < logo_height then
                  I.vsnap ~align:`Bottom body_height
                    (styled_line cols accent
                      ("  PAVE " ^ t.version))
                else
                  let left = (cols - logo_width) / 2
                  and top = (body_height - logo_height) / 2 in
                  I.(void cols top
                    <-> hsnap ~align:`Left cols (void left 1 <|> logo)
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
        let status = match chooser.status with
          | Some text when body_height < 3 -> " · " ^ single_line text
          | _ -> "" in
        let status_page = if Array.length chooser.status_pages = 0 then ""
          else Printf.sprintf " · Tab status %d/%d"
            (chooser.status_page + 1) (Array.length chooser.status_pages) in
        if cols < 9 || rows < 2 then
          "Resize terminal to at least 9×2 · Esc cancel"
        else if chooser.dynamic && Array.length found = 0 then
          (if cols < 35 then "  No models · Esc cancel"
           else "  No available models · Esc cancel") ^ status ^ status_page
        else
        if body_height < 2 then (
          let prefix = Printf.sprintf "  %d/%d "
            number (Array.length found) in
          let select_hint = if cols >= 20 then " ↵" else "" in
          let width = max 0 (cols - measure prefix - measure select_hint) in
          let label = if Array.length found = 0 then
              shorten_width width "(no match)"
            else
              let choice = found.(chooser.selected) in
              if chooser.dynamic && not choice.action then
                shorten_model_label width (sanitize choice.label)
              else shorten_middle width (sanitize choice.label) in
          prefix ^ label ^ select_hint)
        else if cols < 55 then
          Printf.sprintf "  %d/%d · %s select · Esc cancel%s%s"
            number (Array.length found) enter_key status status_page
        else if cols < 75 then
          Printf.sprintf "  %d/%d · ↑↓ move · %s select · Esc cancel%s%s"
            number (Array.length found) enter_key status status_page
        else
          Printf.sprintf "  %d/%d · ↑↓/PgUp/PgDn move · %s select · Esc cancel%s%s"
            number (Array.length found) enter_key status status_page
    | None when hint_height > 0 ->
        (match List.nth hints t.hint_selected with
         | Command_hint selected ->
             "  " ^ selected.name ^ " · " ^ selected.summary
         | File_hint (_, selected) ->
             let detail = match selected.preview with
               | None -> "directory"
               | Some (mime, size) ->
                   single_line mime ^ " · " ^ received_bytes_text size in
             "  @" ^ single_line selected.path ^ " · " ^ detail)
    | None ->
        let status = match Pave.Composer.search_query t.editor with
          | None -> t.status
          | Some _ ->
              "reverse search: " ^
              (match Pave.Composer.search_match t.editor with
              | None -> "(no match)"
              | Some value -> sanitize (String.split_on_char '\n' value |> List.hd)) ^
              " · Ctrl+R older · " ^ enter_key ^ " recall · Esc cancel" in
        let status = if status = idle_status && Option.is_some t.activity then
          if cols < 45 then "Ctrl+C cancel · " ^ enter_key ^ " steer"
          else "Ctrl+C cancel · " ^ enter_key ^ " steer · " ^
            meta_key ^ "+" ^ enter_key ^ " queue"
        else status in
        (if cols < 45 then
          (if status = idle_status then
            (if t.queue > 0 then Printf.sprintf "q%d · " t.queue else "") ^
            (if total = 0 then
              (if cols < 20 then "  /help"
               else "  Type a prompt · /help")
             else if cols < 20 then "  PgUp/Dn"
             else if cols < 29 then "  PgUp/Dn · " ^ meta_key ^ "+O"
             else "  PgUp/Dn · " ^ meta_key ^ "+O details")
           else status)
        else
          (if total = 0 then "  "
           else if visible_last = visible_first then
             Printf.sprintf "  [0/%d] " total
           else Printf.sprintf "  [%d-%d/%d] "
             (visible_first + 1) visible_last total) ^ status) ^
        (if attachment_height > 0 then ""
         else match t.pending_attachments with
         | [] -> ""
         | first :: rest ->
             " · " ^ first.name ^
             (if rest = [] then "" else
               Printf.sprintf " · +%d more" (List.length rest))) in


  let footer = styled_line cols text_attr (shorten_width cols footer_text) in
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
        let selection = Pave.Composer.selection t.editor in
        Array.init editor_height (fun index ->
          let line_index = first_line + index in
          let line = editor_lines.(line_index) in
          let gutter = if line_index = 0 then prompt
            else if prompt = "" then "" else "    " in
          let raw = String.sub (Pave.Composer.text t.editor)
            line.start (line.stop - line.start) in
          let content = match selection with
            | Some (start, stop) when start < line.stop && stop > line.start ->
                let first = max start line.start
                and last = min stop line.stop in
                let before = String.sub raw 0 (first - line.start)
                and selected = String.sub raw (first - line.start)
                  (last - first)
                and after = String.sub raw (last - line.start)
                  (line.stop - last) in
                I.(string text_attr (sanitize before) <|>
                   string selected_attr (sanitize selected) <|>
                   string text_attr (sanitize after))
            | _ -> I.string text_attr (sanitize raw) in
          let content = if field_width = 1 &&
              measure (sanitize raw) > 1 then I.string text_attr "?"
            else content in
          I.(string accent gutter <|>
            hsnap ~align:`Left field_width content)),
        editor_row - first_line, min (cols - 1) (prefix_width + editor_col) in
  let screen = if rows < 6 then
    let candidates = Array.concat [activity_rows; [| footer |]; prompt_rows] in
    if Array.length candidates >= rows then
      Array.sub candidates (Array.length candidates - rows) rows
    else
      let spare = rows - Array.length candidates in
      Array.append (Array.init spare (fun index ->
        match t.chooser with
        | Some chooser when index = spare - 1 ->
            styled_line cols accent ("  " ^ chooser.title)
        | Some _ -> I.void cols 1
        | None when total > 0 ->
            let source = total - spare - t.scroll + index in
            if source < 0 || source >= total then I.void cols 1
            else styled_visual cols (Transcript_view.visual_at layout source)
        | None when index = spare - 1 ->
            styled_line cols accent ("  PAVE " ^ t.version)
        | None -> I.void cols 1)) candidates
  else Array.concat [
    [| header; location; divider |];
    Array.init body_height (fun row ->
      if row < body_height - hint_height then
        I.vcrop row (body_height - row - 1) body
      else
        let index = row - (body_height - hint_height) in
        if index = 0 then
          styled_line cols accent
            (match List.hd hints with
             | Command_hint _ ->
                 "  / Commands · ↑↓ move · Tab/" ^ enter_key ^
                   " insert · Esc close"
             | File_hint _ ->
                 "  @ Files" ^
                 (if t.hint_truncated then " · incomplete" else "") ^
                 " · ↑↓ move · Tab/" ^ enter_key ^
                   " insert · Esc close")
        else
          let choice = List.nth hints (t.hint_offset + index - 1) in
          hint_row cols (t.hint_offset + index - 1 = t.hint_selected) choice);
    activity_rows; [|footer|]; attachment_rows; prompt_rows ] in

  let activity_row =
    if Array.length activity_rows = 0 then -1
    else if rows >= 6 then 3 + body_height
    else
      let candidates = Array.length activity_rows + 1 + Array.length prompt_rows in
      if candidates > rows then -1 else rows - candidates in
  let output = Buffer.create 512 in
  let dirty = ref false in
  for row = 0 to rows - 1 do
    if (match t.previous with
      | Some previous when Array.length previous = rows ->
          not (I.equal previous.(row) screen.(row))
      | _ -> true) then (
      if not !dirty then Buffer.add_string output "\027[?25l";
      dirty := true;
      (match activity_text with
      | Some text when row = activity_row ->
          let text = shorten_width cols text in
          Buffer.add_string output (Printf.sprintf "\027[%d;1H\027[0m%s%s\027[0m"
            (row + 1) (if no_color then "" else "\027[96;1m") text);
          if measure_text text < cols then Buffer.add_string output "\027[K"
      | _ ->
          Buffer.add_string output (Printf.sprintf "\027[%d;1H\027[0m\027[2K"
            (row + 1));
          Render.to_buffer output Cap.ansi (0, 0) (cols, 1) screen.(row)))
  done;
  t.previous <- Some screen;
  let y = if rows < 6 then rows - 1
    else rows - editor_height + cursor_row in
  let position = max 0 y + 1, max 0 cursor_col + 1 in
  if !dirty || t.cursor_position <> Some position then (
    Buffer.add_string output (Printf.sprintf "\027[%d;%dH%s"
      (fst position) (snd position) (if !dirty then "\027[?25h" else ""));
    t.cursor_position <- Some position);
  if Buffer.length output > 0 then (
    Buffer.output_buffer stdout output;
    flush stdout);
  t.last_paint <- Unix.gettimeofday ()

let paint_resized t =
  (match t.layout_cache, t.body_cache with
  | Some (_, _, old_layout), Some (_, old_height, _, _)
    when t.scroll > 0 && old_height > 0 && old_layout.total > 0 ->
      let first = max 0 (old_layout.total - old_height - t.scroll) in
      let old_entry = Transcript_view.visual_at old_layout first in
      let cols, rows = Notty_unix.Term.size t.term in
      let cols = max 1 cols and rows = max 1 rows in
      let measure = measure_text in
      let content_cols = if cols <= 4 then cols else cols - 4 in
      let next = Transcript_view.snapshot t.transcript
        ~columns:content_cols ~measure in
      let prefix_width = I.width (I.string accent prompt) in
      let field_width = if cols <= prefix_width then cols
        else cols - prefix_width in
      let activity_height = if Option.is_some t.activity then 1 else 0 in
      let editor_space = max 1 (rows - 4 - activity_height) in
      let editor_height = match t.chooser, Pave.Composer.search_query t.editor with
        | Some _, _ | None, Some _ -> 1
        | None, None ->
            let editor_lines = Pave.Composer.layout ~columns:field_width
              ~measure t.editor in
            min 4 (max 1 (min editor_space (Array.length editor_lines))) in
      let height = max 0 (rows - 4 - editor_height - activity_height) in
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

let install_terminal_signals () =
  let previous = ref [] in
  try
    List.iter (fun (signal, number) ->
      let old = Sys.signal signal
        (Sys.Signal_handle (fun _ -> raise (Terminal_signal number))) in
      previous := (signal, old) :: !previous)
      [Sys.sigint, 2; Sys.sigterm, 15; Sys.sighup, 1; Sys.sigquit, 3;
       Sys.sigtstp, (if macos then 18 else 20)];
    !previous
  with exn ->
    List.iter (fun (signal, behavior) -> Sys.set_signal signal behavior)
      !previous;
    raise exn

let restore_terminal_signals signals =
  List.iter (fun (signal, behavior) -> Sys.set_signal signal behavior) signals

(* Preserve CR and LF separately for Terminal_input's Enter and paste decoder. *)
let create_terminal () =
  let term = Notty_unix.Term.create ~mouse:false ~bpaste:true () in
  try
    let input_fd, _ = Notty_unix.Term.fds term in
    let state = Unix.tcgetattr input_fd in
    state.Unix.c_icrnl <- false;
    state.Unix.c_inlcr <- false;
    state.Unix.c_igncr <- false;
    Unix.tcsetattr input_fd Unix.TCSANOW state;
    term
  with exn ->
    Notty_unix.Term.release term;
    raise exn

let close t =
  Fun.protect (fun () -> close_ui_pipe t)
    ~finally:(fun () ->
      Fun.protect (fun () -> Notty_unix.Term.release t.term)
        ~finally:(fun () -> restore_terminal_signals t.signals))
let create ?(keybinding_overrides = []) ?(version = "source")
    ?model_display_name ~root ~model ~session () =
  let bindings =
    match Keybindings.apply_overrides Keybindings.bindings
      keybinding_overrides with
    | Ok bindings -> bindings
    | Error message -> invalid_arg message in
  let ui_read_fd, ui_write_fd = create_ui_pipe () in
  let term = try create_terminal () with exn ->
    Unix.close ui_read_fd;
    Unix.close ui_write_fd;
    raise exn in
  let signals = try install_terminal_signals () with exn ->
    Notty_unix.Term.release term;
    Unix.close ui_read_fd;
    Unix.close ui_write_fd;
    raise exn in
  let t = try {
    term; input = Terminal_input.create term;
    root; version = single_line version; model; model_display_name; session;
    editor = Pave.Composer.create ();
    transcript = Transcript_view.create (); tool_groups = Hashtbl.create 8;
    scroll = 0; chooser = None; overlays = [];
    hint_suppressed = None;
    hint_draft = ""; hint_cursor = 0; hint_results = [];
    hint_truncated = false;
    hint_selected = 0; hint_offset = 0;
    revision = 0; body_cache = None; layout_cache = None;
    location_cache = None;
    previous = None; cursor_position = None;
    status = idle_status; activity = None;
    activity_started = None; usage_badge = None;
    active_tool = None;
    pending_attachments = []; queue = 0;
    last_paint = 0.; paste = false; paste_buffer = Buffer.create 256;
    bindings; signals;
    ui_events = Queue.create (); ui_lock = Mutex.create ();
    ui_read_fd; ui_write_fd; ui_wake_byte = Bytes.of_string "x";
    ui_drain_bytes = Bytes.create 256; ui_closed = false;
    agent_event_handler = None;
  } with exn ->
    restore_terminal_signals signals;
    Notty_unix.Term.release term;
    Unix.close ui_read_fd;
    Unix.close ui_write_fd;
    raise exn in
  (try paint t with exn -> close t; raise exn);
  t


let suspend t callback =
  let previous_sigint = Sys.signal Sys.sigint
    (Sys.Signal_handle (fun _ -> raise Sys.Break)) in
  Fun.protect (fun () ->
    Notty_unix.Term.release t.term;
    callback ()) ~finally:(fun () ->
    Sys.set_signal Sys.sigint Sys.Signal_ignore;
    Fun.protect (fun () ->
      let term = create_terminal () in
      let input_fd, _ = Notty_unix.Term.fds term in
      Unix.tcflush input_fd Unix.TCIFLUSH;
      t.term <- term;
      t.input <- Terminal_input.create term;
      t.previous <- None;
      t.cursor_position <- None;
      t.body_cache <- None;
      t.paste <- false;
      Buffer.clear t.paste_buffer;
      paint t) ~finally:(fun () ->
        Sys.set_signal Sys.sigint previous_sigint))

let show_terminal_image t ~enabled (image : Pave.Terminal_image.image) =
  let env name = Option.value ~default:"" (Sys.getenv_opt name) in
  let ssh = List.exists (fun name -> env name <> "")
    ["SSH_CONNECTION"; "SSH_CLIENT"; "SSH_TTY"] in
  let capability = Pave.Terminal_image.detect ~term:(env "TERM")
    ~term_program:(env "TERM_PROGRAM") ~ssh in
  match capability with
  | Pave.Terminal_image.Unsupported -> false
  | Pave.Terminal_image.Supported _ ->
      let commands = Pave.Terminal_image.encode_image
        ~capability ~enabled image in
      if commands = [] then false
      else (
        suspend t (fun () ->
          print_string "\027[2J\027[H";
          List.iter print_string commands;
          print_string "\r\nPress Return to return to Pave.";
          flush stdout;
          (try ignore (input_line stdin) with End_of_file -> ());
          List.iter print_string
            (Pave.Terminal_image.clear ~capability ~enabled);
          print_string "\027[0m\r\n";
          flush stdout);
        true)

let reset_status t =
  t.status <- idle_status;
  paint t

let set_model ?display_name t model =
  t.model <- model;
  t.model_display_name <- display_name;
  t.location_cache <- None;
  reset_status t

let set_session t session =
  t.session <- session;
  t.location_cache <- None;
  paint t

let set_activity t activity =
  (match t.active_tool, activity with
   | Some progress, Some state when state = "Tool: " ^ progress.name -> ()
   | Some _, _ -> t.active_tool <- None
   | None, _ -> ());
  if t.activity <> activity then (
    let now = Unix.gettimeofday () in
    t.activity_started <- activity_started_at t.activity_started activity now;
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
let set_attachments t attachments =
  t.pending_attachments <- preview_attachments attachments;
  paint t



let set_queue t count =
  t.queue <- max 0 count;
  paint t
let prepend_prompt ?(paste_ranges = []) t text =
  let draft = Pave.Composer.text t.editor in
  let prefix = text ^ (if draft = "" then "" else "\n\n") in
  if Pave.Composer.prepend t.editor prefix then (
    Pave.Composer.restore_pasted_ranges t.editor paste_ranges;
    t.hint_draft <- Pave.Composer.text t.editor;
    dismiss_hint t;
    paint t;
    true)
  else false


let show_history t (messages : Pave.Protocol.message list) =
  Hashtbl.clear t.tool_groups;
  let names = Hashtbl.create 32 in
  Transcript_view.clear t.transcript;
  List.iter (fun (message : Pave.Protocol.message) ->
    match message.role with
    | "user" ->
        let content = Option.value ~default:"" message.content in
        let content = attachment_block content
          (preview_attachments message.attachments) in


        if content <> "" then Transcript_view.sent t.transcript content
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
        let result = match message.tool_result_content with
          | Some blocks -> Some
              (Pave.Protocol.display_content_blocks blocks)
          | None -> message.content in
        Option.iter (fun result ->
          Transcript_view.tool_result ?group t.transcript name result;
          Option.iter (Hashtbl.remove names) message.tool_call_id)
          result
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
  Hashtbl.clear t.tool_groups;
  t.active_tool <- None;
  t.activity <- None;
  t.activity_started <- None;
  t.status <- idle_status;
  paint t

let sent ?attachments t text =
  let previews, clear_pending = match attachments with
    | Some attachments -> preview_attachments attachments, false
    | None -> t.pending_attachments, true in
  let text = attachment_block text previews in
  if clear_pending then t.pending_attachments <- [];

  change_transcript t (fun () -> Transcript_view.sent t.transcript text);
  t.scroll <- 0;
  t.status <- idle_status;
  paint t
let event t text =
  change_transcript t (fun () -> Transcript_view.event t.transcript text);
  paint t

let tool_started t call_id name =
  let name = single_line name in
  change_transcript t (fun () ->
    Hashtbl.replace t.tool_groups call_id
      (Transcript_view.start_tool t.transcript name));
  t.active_tool <- Some { call_id; name; received_bytes = None };
  let activity = Some ("Tool: " ^ name) in
  if t.activity = activity then paint t else set_activity t activity

let update_tool_progress active call_id name received_bytes =
  match active with
  | Some progress when progress.call_id = call_id && progress.name = name ->
      let received_bytes = max 0 received_bytes in
      let previous = Option.value ~default:0 progress.received_bytes in
      progress.received_bytes <- Some (max previous received_bytes)
  | _ -> ()

let reset_tool_progress active call_id =
  match active with
  | Some progress when progress.call_id = call_id -> None
  | _ -> active

let tool_updated t call_id name received_bytes =
  update_tool_progress t.active_tool call_id (single_line name) received_bytes

let finish_tool ?(aborted = false) ?(is_error = false)
    t call_id name result =
  let group = Hashtbl.find_opt t.tool_groups call_id in
  Hashtbl.remove t.tool_groups call_id;
  t.active_tool <- reset_tool_progress t.active_tool call_id;
  change_transcript t (fun () ->
    Transcript_view.tool_result ?group ~aborted ~is_error
      t.transcript name result);
  paint t

let tool_settled t call_id name result is_error =
  finish_tool ~is_error t call_id name result

let tool_aborted t call_id name result =
  finish_tool ~aborted:true t call_id name result

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
  if String.contains chunk '\n' ||
     Unix.gettimeofday () -. t.last_paint >= stream_frame_interval then paint t


let clear_live t =
  change_transcript t (fun () -> Transcript_view.rollback t.transcript);
  Hashtbl.clear t.tool_groups;
  t.active_tool <- None;
  t.activity <- None;
  t.activity_started <- None;
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

let process_ui_event t = function
  | Terminal_event event -> `Return event
  | Agent_event event ->
      Option.iter (fun handler -> handler event) t.agent_event_handler;
      `Continue
  | Listing_event update ->
      (!listing_handler) t update;
      `Continue
  | Background_message message ->
      event t message;
      `Continue
  | Approval_event request ->
      (!approval_handler) t request;
      `Return `Wake
  | Shutdown -> `Return `End

let rec next_input ?wake_fd t =
  match pop_ui_event t with
  | Some queued ->
      (match process_ui_event t queued with
      | `Continue -> next_input ?wake_fd t
      | `Return `Tick -> paint t; next_input ?wake_fd t
      | `Return event -> event)
  | None ->
      let timeout = match t.activity_started with
        | None -> None
        | Some since ->
            Some (activity_tick_delay (Unix.gettimeofday () -. since)) in
      let wake_fds = match wake_fd with
        | None -> [t.ui_read_fd]
        | Some fd -> [t.ui_read_fd; fd] in
      (match Terminal_input.event ~wake_fds ?timeout t.input with
      | `Wake when ui_events_pending t -> next_input ?wake_fd t
      | event ->
          enqueue_ui_event t (Terminal_event event);
          next_input ?wake_fd t)

let toggle_tool_detail t =
  let cols, rows = Notty_unix.Term.size t.term in
  let measure = measure_text in
  let layout = match t.layout_cache with
    | Some (width, revision, layout)
      when width = cols && revision = t.transcript.revision -> layout
    | _ -> Transcript_view.snapshot t.transcript
        ~columns:(if cols <= 6 then max 1 cols else cols - 5) ~measure in
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
        ~columns:(if cols <= 6 then max 1 cols else cols - 5) ~measure in
      let target = ref None in
      Array.iter (fun (entry : Transcript_view.entry) ->
        if entry.row.group = group &&
           entry.row.style = Transcript_view.Heading &&
           !target = None then target := Some entry.start) expanded.entries;
      Option.iter (fun start ->
        t.scroll <- max 0 (expanded.total - height - start)) !target);
  paint t

let read ?wake_fd ?on_wake ?on_interrupt ?on_dequeue ?on_completion t =
  let measure = measure_text in
  let field_width () =
    let cols, _ = Notty_unix.Term.size t.term in
    let prefix_width = I.width (I.string accent prompt) in
    max 1 (if cols <= prefix_width then cols else cols - prefix_width) in
  let changed () = repaint_after_key t in
  let paste_buffer = t.paste_buffer in
  let paste_truncated = ref false in
  let history_provenance_uncertain = ref false in
  let paste_limit () = match Pave.Composer.search_query t.editor with
    | None -> draft_paste_capacity t.editor
    | Some query -> max 0 (512 - String.length query) in
  let paste_append value =
    if not !paste_truncated then (
      if Buffer.length paste_buffer + String.length value <= paste_limit () then
        Buffer.add_string paste_buffer value
      else paste_truncated := true) in
  let paste_append_char char =
    if not !paste_truncated then (
      if Buffer.length paste_buffer < paste_limit () then
        Buffer.add_char paste_buffer char
      else paste_truncated := true) in
  let paste_finish () =
    t.paste <- false;
    let value = Buffer.contents paste_buffer in
    Buffer.clear paste_buffer;
    (match Pave.Composer.search_query t.editor with
    | None ->
        Pave.Composer.begin_paste t.editor;
        Pave.Composer.insert t.editor value;
        Pave.Composer.end_paste t.editor
    | Some _ -> Pave.Composer.search_insert t.editor value);
    t.hint_draft <- Pave.Composer.text t.editor;
    t.hint_cursor <- Pave.Composer.cursor t.editor;

    dismiss_hint t;
    if !paste_truncated then
      t.status <- "Paste truncated at input limit";
    paint t in
  let submit follow_up =
    let text = Pave.Composer.text t.editor in
    let paste_ranges =
      if !history_provenance_uncertain && text <> "" then
        [0, String.length text]
      else Pave.Composer.pasted_ranges t.editor in
    match Pave.Composer.submit t.editor with
    | None -> None
    | Some text ->
        paint t;
        Some { text; follow_up; paste_ranges } in
  let key_action event = Keybindings.resolve t.bindings (key_focus t) event in
  let rec loop () =
    match next_input ?wake_fd t with
    | `End -> None
    | `Wake ->
        (match on_wake with Some callback -> callback ()
         | None -> invalid_arg "Tui.read: wake_fd requires on_wake");
        loop ()
    | `Resize _ -> paint_resized t; loop ()
    | `Tick -> paint t; loop ()
    | `Mouse _ -> loop ()
    | `Paste `Start ->
        t.paste <- true;
        Buffer.clear paste_buffer;
        paste_truncated := false;
        paint t; loop ()
    | `Paste `End when t.paste ->
        paste_finish (); loop ()
    | `Paste `End -> loop ()
    | `Key _ as event -> handle_key event
  and handle_key event =
    let action = key_action event in
    let submit_result follow_up =
      match submit follow_up with
      | None -> loop ()
      | Some _ as result -> result in
    let interrupt () =
      let handled = match on_interrupt with
        | Some callback -> callback ()
        | None -> false in
      if not handled then (
        if Pave.Composer.text t.editor = "" then
          Pave.Composer.search_cancel t.editor
        else Pave.Composer.clear t.editor);
      changed ();
      loop () in
    match action with
    | Some Keybindings.Ignore
    | Some Keybindings.Cancel
    | Some Keybindings.Accept
    | Some Keybindings.Reject
    | Some Keybindings.Approve
    | Some Keybindings.Next_status
    | Some Keybindings.First
    | Some Keybindings.Last
    | None -> loop ()
    | Some Keybindings.Paste_newline ->
        if Pave.Composer.search_query t.editor = None then
          paste_append "\n";
        loop ()
    | Some Keybindings.Paste_space ->
        paste_append_char ' ';
        loop ()
    | Some (Keybindings.Paste_ascii char) ->
        paste_append_char char;
        loop ()
    | Some (Keybindings.Paste_uchar uchar) ->
        paste_append (utf8 uchar);
        loop ()
    | Some Keybindings.Interrupt -> interrupt ()
    | Some Keybindings.Cancel_search ->
        Pave.Composer.search_cancel t.editor;
        changed (); loop ()
    | Some Keybindings.Accept_search ->
        history_provenance_uncertain := true;
        Pave.Composer.search_accept t.editor;
        changed (); loop ()
    | Some Keybindings.Search_older ->
        Pave.Composer.search_older t.editor;
        changed (); loop ()
    | Some Keybindings.Search_erase ->
        Pave.Composer.search_erase t.editor;
        changed (); loop ()
    | Some (Keybindings.Search_ascii char) ->
        Pave.Composer.search_insert t.editor (String.make 1 char);
        changed (); loop ()
    | Some (Keybindings.Search_uchar uchar) ->
        Pave.Composer.search_insert t.editor (utf8 uchar);
        changed (); loop ()
    | Some Keybindings.Dismiss_hint ->
        dismiss_hint t;
        changed (); loop ()
    | Some Keybindings.Insert_hint ->
        Option.iter (insert_hint t) (selected_hint t);
        changed (); loop ()
    | Some Keybindings.Accept_hint ->
        let matches = hint_matches t in
        let draft = Pave.Composer.text t.editor in
        if List.exists (function
          | Command_hint item -> item.name = draft
          | File_hint _ -> false) matches then submit_result false
        else (
          if t.hint_selected < List.length matches then
            insert_hint t (List.nth matches t.hint_selected);
          changed (); loop ())
    | Some Keybindings.Move_up when key_focus t = Keybindings.Hints ->
        t.hint_selected <- max 0 (t.hint_selected - 1);
        changed (); loop ()
    | Some Keybindings.Move_down when key_focus t = Keybindings.Hints ->
        t.hint_selected <- min (List.length (hint_matches t) - 1)
          (t.hint_selected + 1);
        changed (); loop ()
    | Some Keybindings.Complete ->
        (match on_completion, Pave.Composer.selection t.editor with
        | Some complete, None ->
            let draft = Pave.Composer.text t.editor in
            let cursor = Pave.Composer.cursor t.editor in
            (match complete draft cursor with
             | Some completion ->
                 if Pave.Composer.replace_range t.editor
                   ~start:completion.start ~stop:completion.stop
                   ~value:completion.value then changed ()
                 else (
                   t.status <- "Completion exceeds the composer input limit";
                   paint t)
             | None -> ())
        | _ -> ());
        loop ()
    | Some Keybindings.Submit ->
        let matches = hint_matches t in
        if matches <> [] && hint_room t then
          let draft = Pave.Composer.text t.editor in
          if List.exists (function
            | Command_hint item -> item.name = draft
            | File_hint _ -> false) matches then submit_result false
          else (
            if t.hint_selected < List.length matches then
              insert_hint t (List.nth matches t.hint_selected);
            changed (); loop ())
        else submit_result false
    | Some Keybindings.Follow_up -> submit_result true
    | Some Keybindings.Newline ->
        Pave.Composer.insert t.editor "\n";
        changed (); loop ()
    | Some Keybindings.Scroll_up ->
        scroll_by t (view_height t); loop ()
    | Some Keybindings.Scroll_down ->
        scroll_by t (-view_height t); loop ()
    | Some Keybindings.Toggle_details ->
        toggle_tool_detail t;
        loop ()
    | Some Keybindings.Scroll_to_start ->
        t.scroll <- max_int;
        t.revision <- t.revision + 1;
        paint t; loop ()
    | Some Keybindings.Scroll_to_end ->
        t.scroll <- 0;
        t.revision <- t.revision + 1;
        paint t; loop ()
    | Some Keybindings.Restore_or_history ->
        if t.queue > 0 then Option.iter (fun dequeue -> dequeue ()) on_dequeue
        else (
          history_provenance_uncertain := true;
          Pave.Composer.older t.editor);
        changed (); loop ()
    | Some Keybindings.History_older ->
        history_provenance_uncertain := true;
        Pave.Composer.older t.editor;
        changed (); loop ()
    | Some Keybindings.History_newer ->
        history_provenance_uncertain := true;
        Pave.Composer.newer t.editor;
        changed (); loop ()
    | Some Keybindings.Vertical_up ->
        if not (Pave.Composer.vertical ~columns:(field_width ()) ~measure
            t.editor (-1)) then (
          history_provenance_uncertain := true;
          Pave.Composer.older t.editor);
        changed (); loop ()
    | Some Keybindings.Vertical_down ->
        if not (Pave.Composer.vertical ~columns:(field_width ()) ~measure
            t.editor 1) then (
          history_provenance_uncertain := true;
          Pave.Composer.newer t.editor);
        changed (); loop ()
    | Some Keybindings.Select_up ->
        Pave.Composer.select_vertical ~columns:(field_width ()) ~measure
          t.editor (-1);
        changed (); loop ()
    | Some Keybindings.Select_down ->
        Pave.Composer.select_vertical ~columns:(field_width ()) ~measure
          t.editor 1;
        changed (); loop ()
    | Some Keybindings.Move_left ->
        Pave.Composer.left t.editor;
        changed (); loop ()
    | Some Keybindings.Move_right ->
        Pave.Composer.right t.editor;
        changed (); loop ()
    | Some Keybindings.Select_left ->
        Pave.Composer.select_left t.editor;
        changed (); loop ()
    | Some Keybindings.Select_right ->
        Pave.Composer.select_right t.editor;
        changed (); loop ()
    | Some Keybindings.Word_left ->
        Pave.Composer.word_left t.editor;
        changed (); loop ()
    | Some Keybindings.Word_right ->
        Pave.Composer.word_right t.editor;
        changed (); loop ()
    | Some Keybindings.Erase_word ->
        Pave.Composer.erase_word t.editor;
        changed (); loop ()
    | Some Keybindings.Erase ->
        Pave.Composer.erase t.editor;
        changed (); loop ()
    | Some Keybindings.Delete ->
        Pave.Composer.delete t.editor;
        changed (); loop ()
    | Some Keybindings.Undo ->
        Pave.Composer.undo t.editor;
        changed (); loop ()
    | Some Keybindings.Redo ->
        Pave.Composer.redo t.editor;
        changed (); loop ()
    | Some Keybindings.Kill_end ->
        Pave.Composer.kill_to_end t.editor;
        changed (); loop ()
    | Some Keybindings.Kill_before ->
        Pave.Composer.kill_before t.editor;
        changed (); loop ()
    | Some Keybindings.Yank ->
        Pave.Composer.yank t.editor;
        changed (); loop ()
    | Some Keybindings.Home ->
        Pave.Composer.home t.editor;
        changed (); loop ()
    | Some Keybindings.End ->
        Pave.Composer.finish t.editor;
        changed (); loop ()
    | Some Keybindings.Select_home ->
        Pave.Composer.select_beginning_of_line
          ~columns:(field_width ()) ~measure t.editor;
        changed (); loop ()
    | Some Keybindings.Select_end ->
        Pave.Composer.select_end_of_line
          ~columns:(field_width ()) ~measure t.editor;
        changed (); loop ()
    | Some Keybindings.Beginning_of_line ->
        Pave.Composer.beginning_of_line t.editor;
        changed (); loop ()
    | Some Keybindings.End_of_line ->
        Pave.Composer.end_of_line t.editor;
        changed (); loop ()
    | Some Keybindings.End_of_input when Pave.Composer.text t.editor = "" ->
        None
    | Some Keybindings.End_of_input -> loop ()
    | Some (Keybindings.Insert_ascii char) ->
        Pave.Composer.insert t.editor (String.make 1 char);
        changed (); loop ()
    | Some (Keybindings.Insert_uchar uchar) ->
        Pave.Composer.insert t.editor (utf8 uchar);
        changed (); loop ()
    | Some (Keybindings.Filter_ascii _)
    | Some (Keybindings.Filter_uchar _) ->
        loop ()
    | Some Keybindings.Page_up
    | Some Keybindings.Page_down
    | Some Keybindings.Backspace
    | Some Keybindings.Move_up
    | Some Keybindings.Move_down ->
        loop () in
  Fun.protect ~finally:(fun () ->
    t.paste <- false;
    Buffer.clear paste_buffer) loop

(* Dynamic choices contain only fresh usable IDs and explicit navigation controls.
   Preserve a touched selection when discovery refreshes the IDs. *)
let update_chooser ?(status_pages = []) chooser ~verified
    ~details ~labels ~status =
  let previous = matches chooser in
  let selected = if chooser.selected < Array.length previous then
      Some previous.(chooser.selected).value else None in
  let annotations = Hashtbl.create (List.length details) in
  List.iter (fun (value, detail) ->
    Hashtbl.replace annotations value detail) details;
  let display_labels = Hashtbl.create (List.length labels) in
  List.iter (fun (value, label) ->
    Hashtbl.replace display_labels value label) labels;
  let seen = Hashtbl.create (List.length verified + List.length chooser.plain) in
  let choices = ref [] in
  let add ~action value =
    if not (Hashtbl.mem seen value) then (
      Hashtbl.add seen value ();
      choices := { value;
        label = if action then value else Option.value ~default:value
          (Hashtbl.find_opt display_labels value);
        custom = false; verified = not action; action;
        detail = if action then None else Hashtbl.find_opt annotations value
      } :: !choices) in
  List.iter (fun value ->
    if not (List.mem value chooser.plain) then add ~action:false value) verified;
  List.iter (add ~action:true) chooser.plain;
  chooser.choices <- Array.of_list (List.rev !choices);
  chooser.filtered <- None;
  chooser.status <- Option.map sanitize status;
  chooser.status_pages <- Array.of_list (List.map sanitize status_pages);
  chooser.status_page <- min chooser.status_page
    (max 0 (Array.length chooser.status_pages - 1));
  let found = matches chooser in
  chooser.selected <- (if
    verified <> [] && chooser.filter = "" && not chooser.touched then 0
    else match selected with
    | Some value ->
        let rec locate i =
          if i = Array.length found then 0
          else if found.(i).value = value then i else locate (i + 1) in
        locate 0
    | None -> 0);
  chooser.offset <- min chooser.offset chooser.selected

let apply_listing_update t update =
  match t.chooser with
  | Some chooser when chooser.dynamic ->
      update_chooser chooser ~verified:update.verified
        ~details:update.details ~labels:update.labels ~status:update.status
        ~status_pages:update.status_pages;
      paint t
  | _ -> ()

let () =
  listing_handler := apply_listing_update

let update_choices t ~verified
    ?(details = []) ?(labels = []) ?(status_pages = []) ~status () =
  enqueue_ui_event t (Listing_event {
    verified; details; labels; status; status_pages })

let choose ?(allow_custom = false) ?(intro = []) ?(plain = [])
    ?initial_status ?initial_filter ?wake_fd ?on_wake ?dynamic t ~title ~choices =
  let dynamic = Option.value dynamic ~default:(Option.is_some wake_fd) in
  let initial = if dynamic then Array.of_list plain
    else Array.of_list choices in
  let chooser = { title = sanitize title;
    intro = Array.of_list (List.map sanitize intro); plain;
    choices = Array.map (fun value ->
      { value; label = value; custom = false; verified = false;
        action = dynamic; detail = None }) initial;
    allow_custom; dynamic; status = initial_status;
    status_pages = [||]; status_page = 0;
    filter = Option.value ~default:"" initial_filter; selected = 0; offset = 0; touched = false;
    filtered = None; matched_models = 0 } in
  let old_scroll = t.scroll in
  let selected () =
    let found = matches chooser in
    if chooser.selected >= 0 && chooser.selected < Array.length found then
      Some (found.(chooser.selected).value)
    else None in
  let can_select () =
    let cols, rows = Notty_unix.Term.size t.term in
    cols >= 9 && rows >= 2 in
  let previous_overlays = t.overlays in
  t.overlays <- Chooser_overlay :: previous_overlays;
  t.chooser <- Some chooser;
  Fun.protect ~finally:(fun () ->
    t.chooser <- None;
    t.overlays <- previous_overlays;
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
      | `Resize _ -> paint t; loop ()
      | `Paste `Start -> t.paste <- true; loop ()
      | `Paste `End -> t.paste <- false; paint t; loop ()
      | `Key _ as event ->
          let action = Keybindings.resolve t.bindings (key_focus t) event in
          (match action with
          | Some Keybindings.Cancel -> None
          | Some Keybindings.Next_status when can_select () &&
              Array.length chooser.status_pages > 1 ->
              chooser.status_page <- (chooser.status_page + 1) mod
                Array.length chooser.status_pages;
              paint t; loop ()
          | Some Keybindings.Accept when can_select () ->
              (match selected () with Some _ as choice -> choice | None -> loop ())
          | Some Keybindings.Move_up when can_select () ->
              chooser.touched <- true;
              chooser.selected <- max 0 (chooser.selected - 1);
              paint t; loop ()
          | Some Keybindings.Move_down when can_select () ->
              chooser.touched <- true;
              chooser.selected <- max 0 (min (Array.length (matches chooser) - 1)
                (chooser.selected + 1));
              paint t; loop ()
          | Some Keybindings.Page_up when can_select () ->
              chooser.touched <- true;
              chooser.selected <- max 0 (chooser.selected - view_height t);
              paint t; loop ()
          | Some Keybindings.Page_down when can_select () ->
              chooser.touched <- true;
              chooser.selected <- max 0 (min (Array.length (matches chooser) - 1)
                (chooser.selected + view_height t));
              paint t; loop ()
          | Some Keybindings.First when can_select () ->
              chooser.touched <- true;
              chooser.selected <- 0; paint t; loop ()
          | Some Keybindings.Last when can_select () ->
              chooser.touched <- true;
              chooser.selected <- max 0 (Array.length (matches chooser) - 1);
              paint t; loop ()
          | Some Keybindings.Backspace when can_select () &&
              chooser.filter <> "" ->
              chooser.touched <- true;
              let boundaries = Pave.Composer.segment chooser.filter in
              chooser.filter <- String.sub chooser.filter 0
                boundaries.(Array.length boundaries - 2);
              chooser.selected <- 0; chooser.offset <- 0;
              paint t; loop ()
          | Some (Keybindings.Filter_ascii char) when can_select () ->
              if String.length chooser.filter < 256 then (
                chooser.touched <- true;
                chooser.filter <- chooser.filter ^ String.make 1 char;
                chooser.selected <- 0; chooser.offset <- 0;
                paint t);
              loop ()
          | Some (Keybindings.Filter_uchar uchar) when can_select () ->
              let value = utf8 uchar in
              if String.length chooser.filter + String.length value <= 256 then (
                chooser.touched <- true;
                chooser.filter <- chooser.filter ^ value;
                chooser.selected <- 0; chooser.offset <- 0;
                paint t);
              loop ()
          | _ -> loop ())
      | _ -> loop () in
    loop ())

let reviewable_text ~max_bytes text =
  String.length text <= max_bytes && sanitize text = text &&
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
        code <> 0x2060 && code <> 0xfeff) true text


let approval_body_rows ~columns ~measure body =
  String.split_on_char '\n' body
  |> List.fold_left (fun total line ->
    total + max 1 (Transcript_view.wrapped_count ~columns ~measure line)) 0

let confirm_review_now t ~title ~label ~body ~max_bytes ~wrap
    ~too_large ~unsafe_text ~approved_text ~denied_text =
  let body_lines = String.split_on_char '\n' body in
  let fits () =
    let cols, rows = Notty_unix.Term.size t.term in
    let content_cols = max 1 (cols - 5) in
    let measure text = I.width (I.string text_attr text) in
    let activity_height = if Option.is_some t.activity then 1 else 0 in
    let editor_space = max 1 (rows - 4 - activity_height) in
    let editor_height = match Pave.Composer.search_query t.editor with
      | Some _ -> 1
      | None ->
          let prompt_cols = max 1 (cols - I.width (I.string accent prompt)) in
          let draft = Pave.Composer.layout ~columns:prompt_cols ~measure t.editor in
          min 4 (max 1 (min editor_space (Array.length draft))) in
    let available = max 0 (rows - 4 - editor_height - activity_height) in
    let hint_count = List.length (hint_matches t) in
    let hint_height = if available < 2 || hint_count = 0 then 0
      else min available (min 8 (hint_count + 1)) in
    let header_height = Transcript_view.wrapped_count ~columns:content_cols
      ~measure title in
    let body_height = if wrap then
        approval_body_rows ~columns:content_cols ~measure body
      else List.length body_lines in
    t.chooser = None && String.length body <= max_bytes &&
    cols >= 25 && rows >= 10 &&
    available - hint_height >= header_height + 1 + body_height &&
    (wrap || List.for_all (fun line -> measure line <= content_cols) body_lines) in
  if String.length body > max_bytes then (
    alert t too_large;
    false)
  else if not (reviewable_text ~max_bytes body) then (
    alert t unsafe_text;
    false)
  else (
    change_transcript t (fun () ->
      Transcript_view.approval ~title t.transcript body);
    t.scroll <- 0;
    let previous_overlays = t.overlays in
    t.overlays <- Approval_overlay :: previous_overlays;
    Fun.protect ~finally:(fun () ->
      t.overlays <- previous_overlays;
      t.paste <- false) (fun () ->
      let resize_notice = "Resize to review · other=no" in
      alert t (if fits () then label else resize_notice);
      let rec decision () = match next_input t with
        | `Resize _ ->
            alert t (if fits () then label else resize_notice);
            decision ()
        | `Tick -> paint t; decision ()
        | `Mouse _ -> decision ()
        | `Paste `Start -> t.paste <- true; decision ()
        | `Paste `End ->
            t.paste <- false;
            alert t (if fits () then label else resize_notice);
            decision ()
        | `Key _ when t.paste -> decision ()
        | `Key _ as event ->
            (match Keybindings.resolve t.bindings (key_focus t) event with
            | Some Keybindings.Approve when fits () -> true
            | Some Keybindings.Approve ->
                alert t resize_notice;
                decision ()
            | Some Keybindings.Reject -> false
            | _ -> false)
        | _ -> false in
      let accepted = decision () in
      alert t (if accepted then approved_text else denied_text);
      accepted))
let confirm_review t ~title ~label ~body ~max_bytes ~wrap
    ~too_large ~unsafe_text ~approved_text ~denied_text =
  let request = {
    title; label; body; max_bytes; wrap; too_large; unsafe_text;
    approved_text; denied_text; result = None
  } in
  enqueue_ui_event t (Approval_event request);
  let rec await () = match request.result with
    | Some result -> result
    | None ->
        (match next_input t with
        | `End -> false
        | _ -> await ()) in
  await ()

let () =
  approval_handler := (fun t request ->
    let result = confirm_review_now t ~title:request.title
      ~label:request.label ~body:request.body ~max_bytes:request.max_bytes
      ~wrap:request.wrap ~too_large:request.too_large
      ~unsafe_text:request.unsafe_text ~approved_text:request.approved_text
      ~denied_text:request.denied_text in
    request.result <- Some result)


let confirm t command =
  let title = "SHELL APPROVAL · review before deciding" in
  confirm_review t ~title ~label:"SHELL: y=yes · other=no"
    ~body:command ~max_bytes:4096 ~wrap:false
    ~too_large:"Shell command denied: too large to review on screen"
    ~unsafe_text:"Shell command denied: hidden/control text cannot be reviewed"
    ~approved_text:"Shell command approved"
    ~denied_text:"Shell command denied"

let confirm_tool t (request : Pave.Approval.request) =
  let shell = request.tool_name = "run_command" in
  let title = if shell then
      "SHELL APPROVAL · review before deciding"
    else "TOOL APPROVAL · review before deciding" in
  let body = String.concat "\n" ([
    "Tool: " ^ request.tool_name;
    "Tier: " ^ String.uppercase_ascii
      (Pave.Approval.tier_name request.tier);
    "Impact: " ^ request.impact;
    "Details:"
  ] @ request.details @ [
    (match request.reason with Some reason -> "Policy: " ^ reason | None -> "")
  ]) in
  let oversized_shell_command = shell && List.exists (fun detail ->
    let prefix = "Command: " in
    String.starts_with ~prefix detail &&
    String.length detail - String.length prefix > 4096) request.details in
  if oversized_shell_command then (
    alert t "Shell command denied: too large to review on screen";
    false)
  else
    confirm_review t ~title ~label:"APPROVE: y=yes · other=no" ~body
      ~max_bytes:8192 ~wrap:true
      ~too_large:"Tool action denied: preview does not fit on screen"
      ~unsafe_text:"Tool action denied: hidden/control text cannot be reviewed"
      ~approved_text:"Tool action approved"
      ~denied_text:"Tool action denied"
