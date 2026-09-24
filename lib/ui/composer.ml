type line = { start : int; stop : int; columns : int }

type change = {
  start : int;
  removed : string;
  mutable inserted : string;
  mutable pending : Buffer.t option;
  before : int;
  mutable after : int;
}

type journal = {
  mutable undo : change list;
  mutable redo : change list;
  mutable bytes : int;
  mutable count : int;
  mutable grouping : bool;
}

let empty_journal () =
  { undo = []; redo = []; bytes = 0; count = 0; grouping = false }

type t = {
  mutable text : string;
  mutable cursor : int;
  mutable boundaries : int array;
  mutable boundary_count : int;
  mutable history : string list;
  mutable recall : int option;
  mutable draft : string;
  mutable draft_cursor : int;
  mutable preferred_column : int option;
  mutable search : (string * int option) option;
  mutable journal : journal;
  mutable draft_journal : journal option;
  mutable paste : (string * int) option;
  mutable kill : string;
}

let create () = { text = ""; cursor = 0; boundaries = [| 0 |];
  boundary_count = 1; history = []; recall = None; draft = "";
  draft_cursor = 0; preferred_column = None; search = None;
  journal = empty_journal (); draft_journal = None; paste = None; kill = "" }
let text t = t.text
let cursor t = t.cursor

let segment text =
  let _, reverse = Uuseg_string.fold_utf_8 `Grapheme_cluster
    (fun (offset, reverse) cluster ->
      let finish = offset + String.length cluster in
      finish, finish :: reverse) (0, []) text in
  Array.of_list (0 :: List.rev reverse)

let at_or_after boundaries count pos =
  let low = ref 0 and high = ref (count - 1) in
  while !low < !high do
    let mid = (!low + !high) / 2 in
    if boundaries.(mid) < pos then low := mid + 1 else high := mid
  done;
  !low

let previous t =
  let index = at_or_after t.boundaries t.boundary_count t.cursor in
  if index = 0 then 0 else t.boundaries.(index - 1)

let next t =
  let index = at_or_after t.boundaries t.boundary_count t.cursor in
  t.boundaries.(min (t.boundary_count - 1) (index + 1))

let set_at t text pos =
  let boundaries = segment text in
  t.text <- text;
  t.boundaries <- boundaries;
  t.boundary_count <- Array.length boundaries;
  t.cursor <- boundaries.(at_or_after boundaries t.boundary_count pos);
  t.preferred_column <- None

let set t text =
  set_at t text (String.length text);
  t.journal <- empty_journal ()

let clear t =
  set t "";
  t.recall <- None;
  t.draft <- "";
  t.draft_cursor <- 0;
  t.draft_journal <- None;
  t.paste <- None;
  t.search <- None

let inserted_length change = match change.pending with
  | None -> String.length change.inserted
  | Some buffer -> Buffer.length buffer

let inserted_text change = match change.pending with
  | None -> change.inserted
  | Some buffer ->
      let value = Buffer.contents buffer in
      change.pending <- None;
      change.inserted <- value;
      value

let change_size change =
  String.length change.removed + inserted_length change

let rec changes_size = function
  | [] -> 0
  | change :: rest -> change_size change + changes_size rest

(* Undo and redo share this budget; new edits discard redo before trimming. *)
let trim_journal journal =
  if journal.count > 128 || journal.bytes > 131_072 then (
    let rec keep count bytes = function
      | change :: rest when count < 128 && bytes + change_size change <= 131_072 ->
          change :: keep (count + 1) (bytes + change_size change) rest
      | _ -> [] in
    journal.undo <- keep 0 0 journal.undo;
    journal.count <- List.length journal.undo;
    journal.bytes <- changes_size journal.undo)

let record t ~start ~removed ~inserted ~before =
  let journal = t.journal in
  journal.bytes <- journal.bytes - changes_size journal.redo;
  journal.redo <- [];
  (match journal.undo with
  | previous :: _ when journal.grouping && removed = ""
      && previous.removed = "" && previous.after = before
      && previous.start + inserted_length previous = start ->
      (match previous.pending with
      | Some buffer -> Buffer.add_string buffer inserted
      | None ->
          let buffer = Buffer.create
            (max 64 (String.length previous.inserted + String.length inserted)) in
          Buffer.add_string buffer previous.inserted;
          Buffer.add_string buffer inserted;
          previous.inserted <- "";
          previous.pending <- Some buffer);
      previous.after <- t.cursor;
      journal.bytes <- journal.bytes + String.length inserted
  | _ ->
      journal.undo <- { start; removed; inserted; pending = None;
        before; after = t.cursor } :: journal.undo;
      journal.bytes <- journal.bytes + String.length removed + String.length inserted;
      journal.count <- journal.count + 1);
  journal.grouping <- true;
  trim_journal journal

let replace t ~start ~stop ~value ~position =
  set_at t (String.sub t.text 0 start ^ value
    ^ String.sub t.text stop (String.length t.text - stop)) position

let undo t =
  let journal = t.journal in
  journal.grouping <- false;
  match journal.undo with
  | [] -> ()
  | change :: rest ->
      journal.undo <- rest;
      journal.count <- journal.count - 1;
      let inserted = inserted_text change in
      replace t ~start:change.start
        ~stop:(change.start + String.length inserted)
        ~value:change.removed ~position:change.before;
      journal.redo <- change :: journal.redo

let redo t =
  let journal = t.journal in
  journal.grouping <- false;
  match journal.redo with
  | [] -> ()
  | change :: rest ->
      journal.redo <- rest;
      journal.count <- journal.count + 1;
      replace t ~start:change.start
        ~stop:(change.start + String.length change.removed)
        ~value:(inserted_text change) ~position:change.after;
      journal.undo <- change :: journal.undo

let safe_input value =
  let length = String.length value in
  let control i =
    let byte = Char.code value.[i] in
    byte < 32 && byte <> 10 || byte = 127
    || byte = 0xc2 && i + 1 < length
       && let next = Char.code value.[i + 1] in next >= 0x80 && next <= 0x9f in
  let rec first i =
    if i = length then value
    else if control i then (
      let buffer = Buffer.create length in
      Buffer.add_substring buffer value 0 i;
      let rec copy i =
        if i < length then (
          let byte = Char.code value.[i] in
          if byte = 9 then (Buffer.add_char buffer ' '; copy (i + 1))
          else if byte = 0xc2 && i + 1 < length
            && let next = Char.code value.[i + 1] in next >= 0x80 && next <= 0x9f
          then copy (i + 2)
          else if control i then copy (i + 1)
          else (Buffer.add_char buffer value.[i]; copy (i + 1))) in
      copy i;
      Buffer.contents buffer)
    else first (i + 1) in
  first 0

(* One paste snapshot, not one copy or undo entry per arriving key event. *)
let begin_paste t =
  if t.paste = None then (
    t.journal.grouping <- false;
    t.paste <- Some (t.text, t.cursor))

let end_paste t =
  (match t.paste with
  | None -> ()
  | Some (before, cursor) ->
      t.paste <- None;
      if before <> t.text then (
        let old_length = String.length before
        and new_length = String.length t.text in
        let start = ref 0 in
        while !start < old_length && !start < new_length
          && before.[!start] = t.text.[!start] do
          incr start
        done;
        let old_stop = ref old_length and new_stop = ref new_length in
        while !old_stop > !start && !new_stop > !start
          && before.[!old_stop - 1] = t.text.[!new_stop - 1] do
          decr old_stop;
          decr new_stop
        done;
        record t ~start:!start
          ~removed:(String.sub before !start (!old_stop - !start))
          ~inserted:(String.sub t.text !start (!new_stop - !start))
          ~before:cursor));
  t.journal.grouping <- false

let insert t value =
  let value = safe_input value in
  if value <> "" && String.length t.text + String.length value <= 16_384 then (
    let start = t.cursor in
    if t.cursor = String.length t.text then (
      let start = t.boundaries.(max 0 (t.boundary_count - 2)) in
      let suffix = String.sub t.text start (String.length t.text - start) ^ value in
      let tail = segment suffix in
      let prefix_count = max 1 (t.boundary_count - 1) in
      let count = prefix_count + Array.length tail - 1 in
      if Array.length t.boundaries < count then (
        let grown = Array.make (max count (2 * Array.length t.boundaries)) 0 in
        Array.blit t.boundaries 0 grown 0 t.boundary_count;
        t.boundaries <- grown);
      for i = 1 to Array.length tail - 1 do
        t.boundaries.(prefix_count + i - 1) <- start + tail.(i)
      done;
      t.boundary_count <- count;
      t.text <- t.text ^ value;
      t.cursor <- String.length t.text)
    else (
      let pos = t.cursor + String.length value in
      replace t ~start:t.cursor ~stop:t.cursor ~value ~position:pos);
    if t.paste = None then
      record t ~start ~removed:"" ~inserted:value ~before:start)

let erase t =
  let start = previous t in
  if start <> t.cursor then (
    let before = t.cursor in
    let removed = String.sub t.text start (before - start) in
    replace t ~start ~stop:before ~value:"" ~position:start;
    t.journal.grouping <- false;
    record t ~start ~removed ~inserted:"" ~before;
    t.journal.grouping <- false)

let delete t =
  let finish = next t in
  if finish <> t.cursor then (
    let start = t.cursor in
    let removed = String.sub t.text start (finish - start) in
    replace t ~start ~stop:finish ~value:"" ~position:start;
    t.journal.grouping <- false;
    record t ~start ~removed ~inserted:"" ~before:start;
    t.journal.grouping <- false)

let left t =
  t.cursor <- previous t;
  t.preferred_column <- None;
  t.journal.grouping <- false
let right t =
  t.cursor <- next t;
  t.preferred_column <- None;
  t.journal.grouping <- false
let home t =
  t.cursor <- 0;
  t.preferred_column <- None;
  t.journal.grouping <- false
let finish t =
  t.cursor <- String.length t.text;
  t.preferred_column <- None;
  t.journal.grouping <- false

let line_start t =
  try String.rindex_from t.text (t.cursor - 1) '\n' + 1
  with Not_found | Invalid_argument _ -> 0

let line_stop t =
  match String.index_from_opt t.text t.cursor '\n' with
  | Some position -> position
  | None -> String.length t.text

let beginning_of_line t =
  t.cursor <- line_start t;
  t.preferred_column <- None;
  t.journal.grouping <- false

let end_of_line t =
  t.cursor <- line_stop t;
  t.preferred_column <- None;
  t.journal.grouping <- false

let kill_between t start stop =
  if start <> stop then (
    let before = t.cursor in
    let removed = String.sub t.text start (stop - start) in
    replace t ~start ~stop ~value:"" ~position:start;
    t.kill <- removed;
    t.journal.grouping <- false;
    record t ~start ~removed ~inserted:"" ~before;
    t.journal.grouping <- false)

let kill_to_end t =
  let stop = line_stop t in
  let stop = if stop = t.cursor && stop < String.length t.text
    then stop + 1 else stop in
  kill_between t t.cursor stop
let kill_before t = kill_between t (line_start t) t.cursor

let yank t =
  t.journal.grouping <- false;
  insert t t.kill;
  t.journal.grouping <- false

let layout ~columns ~measure t =
  let columns = max 1 columns in
  let lines = ref [] and start = ref 0 and used = ref 0 in
  let push stop =
    lines := { start = !start; stop; columns = !used } :: !lines in
  for i = 0 to t.boundary_count - 2 do
    let first = t.boundaries.(i) and last = t.boundaries.(i + 1) in
    if t.text.[first] = '\n' then (
      if !used = columns then (
        push first;
        start := first;
        used := 0);
      push first;
      start := last;
      used := 0)
    else (
      let width = max 0 (measure (String.sub t.text first (last - first))) in
      if !used > 0 && !used + width > columns then (
        push first;
        start := first;
        used := 0);
      used := min columns (!used + width))
  done;
  if !used = columns then (
    push (String.length t.text);
    start := String.length t.text;
    used := 0);
  push (String.length t.text);
  Array.of_list (List.rev !lines)

let position ~measure t (lines : line array) =
  let row = ref 0 in
  for i = 1 to Array.length lines - 1 do
    if lines.(i).start <= t.cursor then row := i
  done;
  let col = ref 0 in
  let index = ref (at_or_after t.boundaries t.boundary_count lines.(!row).start) in
  while !index < t.boundary_count - 1 && t.boundaries.(!index) < t.cursor do
    let first = t.boundaries.(!index) and last = t.boundaries.(!index + 1) in
    if last <= t.cursor && t.text.[first] <> '\n' then
      col := !col + max 0 (measure (String.sub t.text first (last - first)));
    incr index
  done;
  !row, min lines.(!row).columns !col

let vertical ~columns ~measure t direction =
  let lines = layout ~columns ~measure t in
  let row, col = position ~measure t lines in
  let goal = Option.value t.preferred_column ~default:col in
  let dest = max 0 (min (Array.length lines - 1) (row + direction)) in
  if dest <> row then (
    let line = lines.(dest) in
    let best = ref line.start and distance = ref max_int and col = ref 0 in
    let consider offset =
      let wraps_into_next = offset = line.stop
        && dest + 1 < Array.length lines
        && lines.(dest + 1).start = line.stop in
      if not wraps_into_next then (
        let d = abs (goal - !col) in
        if d < !distance then (best := offset; distance := d)) in
    consider line.start;
    let index = ref (at_or_after t.boundaries t.boundary_count line.start) in
    while !index < t.boundary_count - 1
      && t.boundaries.(!index + 1) <= line.stop do
      let first = t.boundaries.(!index) and last = t.boundaries.(!index + 1) in
      col := !col + max 0 (measure (String.sub t.text first (last - first)));
      consider last;
      incr index
    done;
    t.cursor <- !best;
    t.preferred_column <- Some goal;
    t.journal.grouping <- false);
  dest <> row

let word_kind text start =
  let c = text.[start] and length = String.length text in
  let space =
    c = ' ' || c = '\t' || c = '\n' || c = '\r'
    || c = '\011' || c = '\012'
    || (c = '\194' && start + 1 < length && text.[start + 1] = '\160')
    || (c = '\225' && start + 2 < length
      && text.[start + 1] = '\154' && text.[start + 2] = '\128')
    || (c = '\227' && start + 2 < length
      && text.[start + 1] = '\128' && text.[start + 2] = '\128')
    || (c = '\226' && start + 2 < length
      && ((text.[start + 1] = '\128'
          && ((text.[start + 2] >= '\128' && text.[start + 2] <= '\138')
            || text.[start + 2] = '\168'
            || text.[start + 2] = '\169'
            || text.[start + 2] = '\175'))
        || (text.[start + 1] = '\129' && text.[start + 2] = '\159'))) in
  if space then 0
  else if Char.code c < 128
    && not (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
      || ('0' <= c && c <= '9') || c = '_') then 1
  else 2

let word_left t =
  let i = ref (at_or_after t.boundaries t.boundary_count t.cursor) in
  while !i > 0 && word_kind t.text t.boundaries.(!i - 1) = 0 do decr i done;
  if !i > 0 then (
    let kind = word_kind t.text t.boundaries.(!i - 1) in
    while !i > 0 && word_kind t.text t.boundaries.(!i - 1) = kind do decr i done);
  t.cursor <- t.boundaries.(!i);
  t.preferred_column <- None;
  t.journal.grouping <- false

let word_right t =
  let i = ref (at_or_after t.boundaries t.boundary_count t.cursor) in
  let limit = t.boundary_count - 1 in
  if !i < limit then (
    let kind = word_kind t.text t.boundaries.(!i) in
    while !i < limit && word_kind t.text t.boundaries.(!i) = kind do incr i done;
    while !i < limit && word_kind t.text t.boundaries.(!i) = 0 do incr i done);
  t.cursor <- t.boundaries.(!i);
  t.preferred_column <- None;
  t.journal.grouping <- false

let erase_word t =
  let finish = t.cursor in
  word_left t;
  if t.cursor <> finish then (
    let start = t.cursor in
    let removed = String.sub t.text start (finish - start) in
    replace t ~start ~stop:finish ~value:"" ~position:start;
    record t ~start ~removed ~inserted:"" ~before:finish;
    t.journal.grouping <- false)

let older t =
  let index = match t.recall with None -> 0 | Some i -> i + 1 in
  match List.nth_opt t.history index with
  | None -> ()
  | Some value ->
      if t.recall = None then (
        t.draft <- t.text;
        t.draft_cursor <- t.cursor;
        t.draft_journal <- Some t.journal);
      t.recall <- Some index;
      t.journal <- empty_journal ();
      set_at t value (String.length value)

let newer t =
  match t.recall with
  | None -> ()
  | Some 0 ->
      t.recall <- None;
      set_at t t.draft t.draft_cursor;
      t.journal <- Option.value t.draft_journal ~default:(empty_journal ());
      t.draft_journal <- None;
      t.journal.grouping <- false
  | Some i ->
      t.recall <- Some (i - 1);
      let value = List.nth t.history (i - 1) in
      set_at t value (String.length value);
      t.journal <- empty_journal ()

let search_query t = Option.map fst t.search
let search_match t = match t.search with
  | Some (_, Some i) -> List.nth_opt t.history i
  | _ -> None

let contains text query =
  let n = String.length text and m = String.length query in
  let rec at i j =
    j = m || (Char.lowercase_ascii text.[i + j] =
      Char.lowercase_ascii query.[j] && at i (j + 1)) in
  let rec find i = i + m <= n && (at i 0 || find (i + 1)) in
  find 0

let find_history t query from =
  let rec scan index = match List.nth_opt t.history index with
    | None -> None
    | Some value -> if contains value query then Some index else scan (index + 1) in
  scan from

let search_older t = match t.search with
  | None ->
      if t.recall = None then (
        t.draft <- t.text;
        t.draft_cursor <- t.cursor);
      t.search <- Some ("", find_history t "" 0)
  | Some (query, index) ->
      let from = match index with None -> 0 | Some i -> i + 1 in
      let next = find_history t query from in
      t.search <- Some (query, if next = None then index else next)

let search_insert t value = match t.search with
  | None -> ()
  | Some (query, _) ->
      let value = safe_input value in
      if value <> "" && String.length query + String.length value <= 512 then (
        let query = query ^ value in
        t.search <- Some (query, find_history t query 0))

let search_erase t = match t.search with
  | None -> ()
  | Some (query, _) when query <> "" ->
      let boundaries = segment query in
      let query = String.sub query 0 boundaries.(Array.length boundaries - 2) in
      t.search <- Some (query, find_history t query 0)
  | Some _ -> ()

let search_accept t =
  (match t.search with
  | Some (_, Some index) ->
      (match List.nth_opt t.history index with
      | None -> ()
      | Some value ->
          if t.recall = None then t.draft_journal <- Some t.journal;
          set_at t value (String.length value);
          t.journal <- empty_journal ();
          t.recall <- Some index)
  | _ -> ());
  t.search <- None

let search_cancel t = t.search <- None

let submit t =
  if String.trim t.text = "" then None
  else (
    let value = t.text in
    if (match t.history with first :: _ -> first <> value | [] -> true) then (
      t.history <- value :: t.history;
      if List.length t.history > 100 then
        t.history <- List.rev (List.tl (List.rev t.history)));
    clear t;
    Some value)
