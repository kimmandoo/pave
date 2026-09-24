type t = {
  on_event : string option -> string -> unit;
  line : Buffer.t;
  data : Buffer.t;
  mutable data_present : bool;
  mutable event_name : string option;
  mutable after_cr : bool;
}

let max_event_bytes = 1_048_576
let invalid text = raise (Protocol.Invalid_response text)

let create ~on_event =
  { on_event; line = Buffer.create 128; data = Buffer.create 512;
    data_present = false; event_name = None; after_cr = false }

let dispatch t =
  if t.data_present then t.on_event t.event_name (Buffer.contents t.data);
  Buffer.clear t.data;
  t.data_present <- false;
  t.event_name <- None

let process_line t =
  let line = Buffer.contents t.line in
  Buffer.clear t.line;
  if line = "" then dispatch t
  else if line.[0] <> ':' then (
    let name, value = match String.index_opt line ':' with
      | None -> line, ""
      | Some colon ->
          let start = colon + 1 in
          let start = if start < String.length line && line.[start] = ' ' then start + 1 else start in
          String.sub line 0 colon, String.sub line start (String.length line - start) in
    match name with
    | "data" ->
        let extra = String.length value + (if t.data_present then 1 else 0) in
        if extra > max_event_bytes - Buffer.length t.data then invalid "SSE event exceeds 1 MiB";
        if t.data_present then Buffer.add_char t.data '\n';
        Buffer.add_string t.data value;
        t.data_present <- true
    | "event" -> t.event_name <- Some value
    | _ -> ())

let feed t bytes =
  String.iter (fun byte ->
    if t.after_cr && byte = '\n' then t.after_cr <- false
    else (
      t.after_cr <- false;
      match byte with
      | '\r' -> process_line t; t.after_cr <- true
      | '\n' -> process_line t
      | _ ->
          if Buffer.length t.line >= max_event_bytes then invalid "SSE line exceeds 1 MiB";
          Buffer.add_char t.line byte)) bytes

let finish t =
  if Buffer.length t.line <> 0 || t.data_present || t.event_name <> None then
    invalid "incomplete SSE event at EOF"
