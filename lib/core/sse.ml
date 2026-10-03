type t = {
  on_event : string option -> string -> unit;
  line : Buffer.t;
  data : Buffer.t;
  mutable data_present : bool;
  mutable event_name : string option;
  mutable after_cr : bool;
  mutable bom_bytes : int;
}

let max_event_bytes = 1_048_576
let invalid text = raise (Protocol.Invalid_response text)
let create ~on_event =
  { on_event; line = Buffer.create 128; data = Buffer.create 512;
    data_present = false; event_name = None; after_cr = false;
    bom_bytes = 0 }

let dispatch t =
  (* An event with a `data:` field — even an empty value — is dispatchable;
     comment keep-alives set no fields and never reach the listener. *)
  if t.data_present then t.on_event t.event_name (Buffer.contents t.data);
  Buffer.clear t.data;
  t.data_present <- false;
  t.event_name <- None


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
  let length = String.length bytes in
  let rec prefix offset =
    if t.bom_bytes = 3 then feed_from t bytes offset length
    else if offset < length then (
      let expected = match t.bom_bytes with
        | 0 -> '\xEF' | 1 -> '\xBB' | _ -> '\xBF' in
      if bytes.[offset] = expected then (
        t.bom_bytes <- t.bom_bytes + 1;
        prefix (offset + 1))
      else (
        (* A partial BOM was ordinary input, not a byte-order mark. *)
        if t.bom_bytes >= 1 then Buffer.add_char t.line '\xEF';
        if t.bom_bytes = 2 then Buffer.add_char t.line '\xBB';
        t.bom_bytes <- 3;
        feed_from t bytes offset length)) in
  prefix 0

let finish t =
  if (t.bom_bytes > 0 && t.bom_bytes < 3) ||
     Buffer.length t.line <> 0 || t.data_present || t.event_name <> None then
    invalid "incomplete SSE event at EOF"
