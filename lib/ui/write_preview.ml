(* Untrusted display-only JSON. Nothing decoded here is admitted as a tool input. *)
type snapshot = {
  path : string option;
  lines : (int * string) list;
  total_lines : int;
  omitted_lines : int;
  omitted_bytes : int;
}
type field = Key | Path | Content | Ignore
type escape = Plain | Slash | Hex of int * int
type phase = Start | Keys | Colon | Value | Comma | Done

type t = {
  mutable phase : phase;
  mutable depth : int;
  mutable string_field : field option;
  mutable escape : escape;
  mutable high : int option;
  mutable utf_value : int;
  mutable utf_left : int;
  mutable utf_min : int;
  key : Buffer.t;
  path : Buffer.t;
  mutable path_safe : bool;
  mutable field : field;
  tail : (int * string) Queue.t;
  line : bytes;
  mutable line_start : int;
  mutable line_bytes : int;
  mutable line_number : int;
  mutable omitted_bytes : int;
  mutable invalid : bool;
}
let max_lines = 16
let max_line_bytes = 256
let create () = {
  phase = Start; depth = 0; string_field = None; escape = Plain; high = None;
  utf_value = 0; utf_left = 0; utf_min = 0; key = Buffer.create 16;
  path = Buffer.create 128; path_safe = true; field = Ignore;
  tail = Queue.create (); line = Bytes.create max_line_bytes;
  line_start = 0; line_bytes = 0;
  line_number = 1; omitted_bytes = 0; invalid = false;
}
let line_text t =
  let text = Bytes.create t.line_bytes in
  let first = min t.line_bytes (max_line_bytes - t.line_start) in
  Bytes.blit t.line t.line_start text 0 first;
  Bytes.blit t.line 0 text first (t.line_bytes - first);
  Bytes.unsafe_to_string text

let discard_scalar t =
  let first = Char.code (Bytes.get t.line t.line_start) in
  let removed = if first < 128 then 1 else if first < 224 then 2
    else if first < 240 then 3 else 4 in
  t.line_start <- (t.line_start + removed) land (max_line_bytes - 1);
  t.line_bytes <- t.line_bytes - removed;
  t.omitted_bytes <- t.omitted_bytes + removed

let put_byte t byte =
  Bytes.set t.line ((t.line_start + t.line_bytes) land (max_line_bytes - 1))
    (Char.chr byte);
  t.line_bytes <- t.line_bytes + 1

let append_scalar t code =
  let size = if code < 128 then 1 else if code < 2048 then 2
    else if code < 65536 then 3 else 4 in
  while t.line_bytes + size > max_line_bytes do discard_scalar t done;
  if size = 1 then put_byte t code else (
    put_byte t (((0xff lsl (8 - size)) land 0xff) lor (code lsr (6 * (size - 1))));
    for shift = size - 2 downto 0 do
      put_byte t (0x80 lor ((code lsr (6 * shift)) land 0x3f))
    done)

(* Printable ASCII cannot split a scalar, so retain/copy only its bounded tail. *)
let append_ascii t bytes start length =
  if length >= max_line_bytes then (
    t.omitted_bytes <- t.omitted_bytes + t.line_bytes + length - max_line_bytes;
    Bytes.blit_string bytes (start + length - max_line_bytes) t.line 0 max_line_bytes;
    t.line_start <- 0;
    t.line_bytes <- max_line_bytes)
  else (
    while t.line_bytes + length > max_line_bytes do discard_scalar t done;
    let offset = (t.line_start + t.line_bytes) land (max_line_bytes - 1) in
    let first = min length (max_line_bytes - offset) in
    Bytes.blit_string bytes start t.line offset first;
    Bytes.blit_string bytes (start + first) t.line 0 (length - first);
    t.line_bytes <- t.line_bytes + length)

let ascii_span t field bytes start length =
  match field with
  | Ignore -> ()
  | Key ->
      Buffer.add_substring t.key bytes start (min length (max 0 (32 - Buffer.length t.key)))
  | Path ->
      let retained = min length (512 - Buffer.length t.path) in
      Buffer.add_substring t.path bytes start retained;
      if retained <> length then t.path_safe <- false
  | Content -> append_ascii t bytes start length

let rec ascii_end bytes offset length =
  if offset = length then offset
  else
    let byte = Char.code bytes.[offset] in
    if byte < 32 || byte >= 127 || byte = 34 || byte = 92 then offset
    else ascii_end bytes (offset + 1) length

let unsafe_scalar code = code < 32 || (code >= 127 && code <= 159) ||
  code = 0x61c || code = 0x200e || code = 0x200f ||
  (code >= 0x202a && code <= 0x202e) || (code >= 0x2066 && code <= 0x2069)
let scalar t field code =
  let code = if code < 0 || code > 0x10ffff ||
    (code >= 0xd800 && code <= 0xdfff) then 0xfffd else code in
  match field with
  | Ignore -> ()
  | Key ->
      if Buffer.length t.key < 32 then
        Buffer.add_utf_8_uchar t.key (Uchar.of_int code)
  | Path ->
      if unsafe_scalar code || code = 0xfffd then t.path_safe <- false;
      let size = if code < 128 then 1 else if code < 2048 then 2
        else if code < 65536 then 3 else 4 in
      if Buffer.length t.path + size <= 512 then
        Buffer.add_utf_8_uchar t.path (Uchar.of_int code)
      else t.path_safe <- false
  | Content ->
      if code = 10 then (
        Queue.add (t.line_number, line_text t) t.tail;
        if Queue.length t.tail >= max_lines then ignore (Queue.take t.tail);
        t.line_start <- 0; t.line_bytes <- 0;
        t.line_number <- t.line_number + 1)
      else append_scalar t (if unsafe_scalar code then 32 else code)
let unicode t field code =
  match t.high with
  | Some high when code >= 0xdc00 && code <= 0xdfff ->
      t.high <- None;
      scalar t field (0x10000 + ((high - 0xd800) lsl 10) + code - 0xdc00)
  | high ->
      t.high <- None;
      (match high with None -> () | Some _ -> scalar t field 0xfffd);
      if code >= 0xd800 && code <= 0xdbff then t.high <- Some code
      else scalar t field code
let flush_high t field =
  (match t.high with None -> () | Some _ -> scalar t field 0xfffd);
  t.high <- None
let rec utf_byte t field byte =
  if t.utf_left > 0 then
    if byte land 0xc0 = 0x80 then (
      t.utf_value <- (t.utf_value lsl 6) lor (byte land 0x3f);
      t.utf_left <- t.utf_left - 1;
      if t.utf_left = 0 then
        scalar t field (if t.utf_value < t.utf_min then 0xfffd else t.utf_value))
    else (
      t.utf_left <- 0; scalar t field 0xfffd; utf_byte t field byte)
  else if byte < 128 then scalar t field byte
  else if byte >= 0xc2 && byte <= 0xf4 then (
    let left, mask, minimum = if byte < 0xe0 then 1, 0x1f, 0x80
      else if byte < 0xf0 then 2, 0xf, 0x800 else 3, 7, 0x10000 in
    t.utf_left <- left; t.utf_value <- byte land mask; t.utf_min <- minimum)
  else scalar t field 0xfffd
let hex = function
  | '0'..'9' as c -> Char.code c - 48
  | 'a'..'f' as c -> Char.code c - 87
  | 'A'..'F' as c -> Char.code c - 55
  | _ -> -1
let rec feed_from t fragment offset length =
  if offset < length && not t.invalid then (
    let c = fragment.[offset] in
    let next = ref (offset + 1) in
    (match t.string_field with
    | Some field ->
        (match t.escape with
         | Hex (count, value) ->
             let digit = hex c in
             if digit < 0 then t.invalid <- true
             else if count = 3 then (
               t.escape <- Plain; unicode t field ((value lsl 4) lor digit))
             else t.escape <- Hex (count + 1, (value lsl 4) lor digit)
         | Slash ->
             t.escape <- Plain;
             if c = 'u' then t.escape <- Hex (0, 0)
             else (
               flush_high t field;
               match c with
               | '"' | '\\' | '/' -> scalar t field (Char.code c)
               | 'n' -> scalar t field 10 | 'r' -> scalar t field 13
               | 't' -> scalar t field 9 | 'b' -> scalar t field 8
               | 'f' -> scalar t field 12 | _ -> t.invalid <- true)
         | Plain ->
             if c = '"' || c = '\\' then (
               if t.utf_left > 0 then (t.utf_left <- 0; scalar t field 0xfffd);
               if c = '\\' then t.escape <- Slash
               else (
                 flush_high t field;
                 t.string_field <- None;
                 if t.depth = 1 then
                   if field = Key then (
                     t.field <- (match Buffer.contents t.key with
                       | "path" -> Path | "content" -> Content | _ -> Ignore);
                     t.phase <- Colon)
                   else t.phase <- Comma))
             else if Char.code c < 32 then t.invalid <- true
             else if t.utf_left = 0 && t.high = None && Char.code c < 127 then (
               let stop = ascii_end fragment (offset + 1) length in
               if stop = offset + 1 then scalar t field (Char.code c)
               else ascii_span t field fragment offset (stop - offset);
               next := stop)
             else (flush_high t field; utf_byte t field (Char.code c)))
    | None ->
        if c = '"' then (
          let field = if t.depth = 1 && t.phase = Keys then (
            Buffer.clear t.key; Key)
            else if t.depth = 1 && t.phase = Value then t.field else Ignore in
          t.string_field <- Some field; t.escape <- Plain)
        else if c = '{' || c = '[' then (
          if t.phase = Start && c = '{' then t.phase <- Keys;
          t.depth <- t.depth + 1)
        else if c = '}' || c = ']' then (
          t.depth <- t.depth - 1;
          if t.depth = 0 then t.phase <- Done
          else if t.depth = 1 then t.phase <- Comma)
        else if t.depth = 1 then (
          if c = ':' && t.phase = Colon then t.phase <- Value
          else if c = ',' then t.phase <- Keys
          else if c <> ' ' && c <> '\n' && c <> '\r' && c <> '\t' &&
            t.phase <> Value then t.invalid <- true));
    feed_from t fragment !next length)
let feed t fragment = feed_from t fragment 0 (String.length fragment)
let safe_path path =
  path <> "" && Filename.is_relative path &&
  not (String.contains path ':' || String.contains path '\\') &&
  List.for_all (fun component -> component <> ".." && component <> "")
    (String.split_on_char '/' path)
let snapshot t =
  let path = Buffer.contents t.path in
  let lines = List.of_seq (Queue.to_seq t.tail) @
    [t.line_number, line_text t] in
  { path = (if t.path_safe && safe_path path then Some path else None);
    lines; total_lines = t.line_number;
    omitted_lines = t.line_number - List.length lines;
    omitted_bytes = t.omitted_bytes }
let of_values ~path ~content =
  let t = create () in
  (* Validated complete values still pass the same terminal-safe scalar filter. *)
  let append field () _ = function
    | `Malformed _ -> scalar t field 0xfffd
    | `Uchar value -> scalar t field (Uchar.to_int value) in
  ignore (Uutf.String.fold_utf_8 (append Path) () path);
  ignore (Uutf.String.fold_utf_8 (append Content) () content);
  snapshot t
