type t = {
  on_event : string option -> string -> unit;
  line : Buffer.t;
  data : Buffer.t;
  mutable data_present : bool;
  mutable event_name : string option;
  mutable after_cr : bool;
  mutable bom_checked : bool;
  (* Count of events delivered to on_event; keep-alives do not increment it. *)
  mutable dispatched : int;
}

let max_event_bytes = 1_048_576
let invalid text = raise (Protocol.Invalid_response text)
let create ~on_event =
  { on_event; line = Buffer.create 128; data = Buffer.create 512;
    data_present = false; event_name = None; after_cr = false;
    bom_checked = false; dispatched = 0 }

let dispatch t =
  (* An event with a `data:` field — even an empty value — is dispatchable;
     comment keep-alives set no fields and never reach the listener. *)
  if t.data_present then (
    t.on_event t.event_name (Buffer.contents t.data);
    t.dispatched <- t.dispatched + 1);
  Buffer.clear t.data;
  t.data_present <- false;
  t.event_name <- None

let events t = t.dispatched

(* A complete line in a chunk needs no intermediate line/name/value strings.
   Only a line split across chunks is accumulated in [t.line]. *)
let process_span t bytes start length =
  if length = 0 then dispatch t
  else if bytes.[start] <> ':' then (
    let stop = start + length in
    let colon = ref start in
    while !colon < stop && bytes.[!colon] <> ':' do incr colon done;
    let name_length = !colon - start in
    let value_start = if !colon = stop then stop else !colon + 1 in
    let value_start =
      if value_start < stop && bytes.[value_start] = ' ' then value_start + 1
      else value_start in
    let value_length = stop - value_start in
    if name_length = 4 && bytes.[start] = 'd' && bytes.[start + 1] = 'a' &&
      bytes.[start + 2] = 't' && bytes.[start + 3] = 'a' then (
      let extra = value_length + (if t.data_present then 1 else 0) in
      if extra > max_event_bytes - Buffer.length t.data then invalid "SSE event exceeds 1 MiB";
      if t.data_present then Buffer.add_char t.data '\n';
      Buffer.add_substring t.data bytes value_start value_length;
      t.data_present <- true)
    else if name_length = 5 && bytes.[start] = 'e' && bytes.[start + 1] = 'v' &&
      bytes.[start + 2] = 'e' && bytes.[start + 3] = 'n' &&
      bytes.[start + 4] = 't' then
      t.event_name <- Some (String.sub bytes value_start value_length))

let process_line t =
  let line = Buffer.contents t.line in
  Buffer.clear t.line;
  process_span t line 0 (String.length line)

let rec line_end bytes offset length =
  if offset = length || bytes.[offset] = '\r' || bytes.[offset] = '\n' then offset
  else line_end bytes (offset + 1) length

let rec feed_from t bytes offset length =
  if offset < length then
    if t.after_cr && bytes.[offset] = '\n' then (
      t.after_cr <- false;
      feed_from t bytes (offset + 1) length)
    else (
      t.after_cr <- false;
      let stop = line_end bytes offset length in
      let span_length = stop - offset in
      if span_length > max_event_bytes - Buffer.length t.line then
        invalid "SSE line exceeds 1 MiB";
      if stop = length then (
        if span_length = 1 then Buffer.add_char t.line bytes.[offset]
        else Buffer.add_substring t.line bytes offset span_length)
      else (
        if Buffer.length t.line = 0 then process_span t bytes offset span_length
        else (
          if span_length = 1 then Buffer.add_char t.line bytes.[offset]
          else Buffer.add_substring t.line bytes offset span_length;
          process_line t);
        t.after_cr <- bytes.[stop] = '\r';
        feed_from t bytes (stop + 1) length))

let feed t bytes =
  (* Strip a UTF-8 byte-order mark once; a BOM would otherwise poison the
     first field name and swallow the opening event. *)
  if not t.bom_checked then (
    t.bom_checked <- true;
    let offset =
      if String.length bytes >= 3 && bytes.[0] = '\xEF' &&
         bytes.[1] = '\xBB' && bytes.[2] = '\xBF' then 3 else 0 in
    feed_from t bytes offset (String.length bytes))
  else feed_from t bytes 0 (String.length bytes)

let finish t =
  (* Flush an unterminated tail line and a pending event: some services close
     without a trailing blank line after their last data frame. *)
  if Buffer.length t.line <> 0 then process_line t;
  dispatch t
