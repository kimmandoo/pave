open Notty

let max_rows = 240
let max_line_bytes = 4096

type row = { text : string; attr : A.t }
type t = {
  mutable term : Notty_unix.Term.t;
  mutable input : Terminal_input.t;
  root : string;
  mutable model : string;
  session : bool;
  editor : Pave.Composer.t;
  lines : row Queue.t;
  mutable live : string;
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
  "Enter send   ·   Shift+Enter newline   ·   ↑↓ history   ·   /login /model /help"

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

let add_row t attr text =
  Queue.add { attr; text = fit_bytes text } t.lines;
  if Queue.length t.lines > max_rows then ignore (Queue.take t.lines);
  t.revision <- t.revision + 1

let add_lines t attr text =
  List.iter (add_row t attr) (String.split_on_char '\n' (sanitize text))

let styled_line width attr text =
  I.hsnap ~align:`Left width (I.string attr text)

let paint t =
  let cols, rows = Notty_unix.Term.size t.term in
  let cols = max 1 cols and rows = max 1 rows in
  let input = Pave.Composer.text t.editor in
  let input_lines = Array.of_list (String.split_on_char '\n' input) in
  let editor_height = min 4 (max 1 (min (rows - 4) (Array.length input_lines))) in
  let body_height = max 0 (rows - 4 - editor_height) in
  let header = styled_line cols accent "  ◆  PAVE  /  mobile workspace" in
  let location = styled_line cols muted
    ("  " ^ sanitize t.root ^ "   ·   " ^ sanitize t.model
      ^ (if t.session then "   ·   session on" else "   ·   session off")) in
  let divider = I.uchar A.(fg lightblack) (Uchar.of_int 0x2500) cols 1 in
  let body = match t.body_cache with
    | Some (width, height, revision, body)
      when width = cols && height = body_height && revision = t.revision -> body
    | _ ->
        let all = Queue.to_seq t.lines |> List.of_seq in
        let all = if t.live = "" then all
          else all @ [ { text = "PAVE › " ^ fit_bytes t.live; attr = text_attr } ] in
        let total = List.length all in
        let rec drop count lines = if count <= 0 then lines else match lines with
          | [] -> [] | _ :: rest -> drop (count - 1) rest in
        let visible = drop (max 0 (total - body_height)) all in
        let body = I.vsnap ~align:`Bottom body_height
          (I.vcat (List.map (fun row -> styled_line cols row.attr row.text) visible)) in
        t.body_cache <- Some (cols, body_height, t.revision, body);
        body in
  let footer = styled_line cols muted ("  " ^ t.status) in
  let prefix = "  ❯ " in
  let prefix_width = I.width (I.string accent prefix) in
  let field_width = max 1 (cols - prefix_width) in
  let before = String.sub input 0 (Pave.Composer.cursor t.editor) in
  let before_lines = String.split_on_char '\n' before in
  let cursor_line = List.length before_lines - 1 in
  let before_line = List.hd (List.rev before_lines) in
  let cursor_col = I.width (I.string text_attr (sanitize before_line)) in
  let first_col = max 0 (cursor_col - field_width + 1) in
  let first_line = max 0 (min cursor_line (Array.length input_lines - editor_height)) in
  let prompt_rows = Array.init editor_height (fun index ->
    let line_index = first_line + index in
    let gutter = if line_index = 0 then prefix else "    " in
    let crop_left = if line_index = cursor_line then first_col else 0 in
    I.(string accent gutter <|>
      hsnap ~align:`Left field_width
        (hcrop crop_left 0 (string text_attr (sanitize input_lines.(line_index)))))) in
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
  let x = min (cols - 1) (prefix_width + cursor_col - first_col) in
  let y = if rows < 6 then rows - 1
    else rows - editor_height + cursor_line - first_line in
  Buffer.add_string output (Printf.sprintf "\027[%d;%dH\027[?25h"
    (max 0 y + 1) (max 0 x + 1));
  Buffer.output_buffer stdout output;
  flush stdout;
  t.last_paint <- Unix.gettimeofday ()

let create ~root ~model ~session =
  let term = Notty_unix.Term.create ~mouse:false ~bpaste:true () in
  let t = { term; input = Terminal_input.create term;
    root; model; session; editor = Pave.Composer.create ();
    lines = Queue.create (); live = ""; revision = 0; body_cache = None;
    previous = None;
    status = idle_status;
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

let event t text =
  if t.live <> "" then (add_lines t text_attr ("PAVE › " ^ t.live); t.live <- "");
  let attr = if String.starts_with ~prefix:"Error:" text then error
    else if String.starts_with ~prefix:"[" text then warning else text_attr in
  add_lines t attr text;
  paint t

let events t lines =
  List.iter (add_lines t text_attr) lines;
  paint t

let delta t chunk =
  let parts = String.split_on_char '\n' (sanitize chunk) in
  (match parts with
   | [] -> ()
   | first :: rest ->
       t.live <- fit_bytes (t.live ^ first);
       List.iter (fun part ->
         add_row t text_attr ("PAVE › " ^ t.live);
         t.live <- fit_bytes part) rest);
  t.revision <- t.revision + 1;
  if chunk = "\n" || Unix.gettimeofday () -. t.last_paint > 0.033 then paint t

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

let rec read t =
  match Terminal_input.event t.input with
  | `End -> None
  | `Resize _ -> paint t; read t
  | `Paste `Start -> t.paste <- true; read t
  | `Paste `End -> t.paste <- false; paint t; read t
  | `Key (`Enter, mods) ->
      if t.paste || List.mem `Shift mods then
        (Pave.Composer.insert t.editor "\n"; repaint_after_key t; read t)
      else (match Pave.Composer.submit t.editor with
        | None -> read t
        | Some value -> add_lines t accent ("YOU › " ^ value); paint t; Some value)
  | `Key (`Arrow `Up, _) -> Pave.Composer.older t.editor; repaint_after_key t; read t
  | `Key (`Arrow `Down, _) -> Pave.Composer.newer t.editor; repaint_after_key t; read t
  | `Key (`Arrow `Left, _) -> Pave.Composer.left t.editor; repaint_after_key t; read t
  | `Key (`Arrow `Right, _) -> Pave.Composer.right t.editor; repaint_after_key t; read t
  | `Key (`Backspace, _) -> Pave.Composer.erase t.editor; repaint_after_key t; read t
  | `Key (`Delete, _) -> Pave.Composer.delete t.editor; repaint_after_key t; read t
  | `Key (`Home, _) | `Key (`ASCII 'A', [ `Ctrl ]) ->
      Pave.Composer.home t.editor; repaint_after_key t; read t
  | `Key (`End, _) | `Key (`ASCII 'E', [ `Ctrl ]) ->
      Pave.Composer.finish t.editor; repaint_after_key t; read t
  | `Key (`ASCII 'C', [ `Ctrl ]) ->
      Pave.Composer.clear t.editor; repaint_after_key t; read t
  | `Key (`ASCII 'D', [ `Ctrl ]) when Pave.Composer.text t.editor = "" -> None
  | `Key (`ASCII c, []) when Char.code c >= 32 ->
      Pave.Composer.insert t.editor (String.make 1 c); repaint_after_key t; read t
  | `Key (`Uchar uchar, []) ->
      Pave.Composer.insert t.editor (utf8 uchar); repaint_after_key t; read t
  | _ -> read t

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
