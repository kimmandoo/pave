(* JSONL recording of session events. Each line is one frame:

     {"seq":1,"at_ms":0,"direction":"input","kind":"stdin","data":{...}}

   Frames are strictly ordered: seq is dense from 1 and at_ms is a
   monotonic millisecond clock relative to recorder creation. *)

type frame = {
  seq : int;
  at_ms : int;
  direction : [ `Input | `Output ];
  kind : string;
  data : Yojson.Basic.t;
}

(* Public conservative limits. *)
let max_data_bytes = 1024 * 1024
let max_frames = 1_000_000
let max_kind_chars = 64
(* A frame line is bounded by the serialized data plus a small fixed
   header; reject longer lines before feeding them to the parser. *)
let max_line_bytes = max_data_bytes + 1_024

let valid_kind kind =
  String.length kind >= 1 && String.length kind <= max_kind_chars &&
  String.for_all
    (function 'a'..'z' | '0'..'9' | '_' | '-' -> true | _ -> false)
    kind

type recorder = {
  out : out_channel;
  started_ms : int;
  mutable seq : int;
  mutable last_at_ms : int;
  mutable closed : bool;
  mutex : Mutex.t;
}

(* Wall clock clamped per-recorder so recorded at_ms values never go
   backward, even if the system clock steps. *)
let now_ms () = int_of_float (Unix.gettimeofday () *. 1_000.)

let create_recorder out =
  { out;
    started_ms = now_ms ();
    seq = 0;
    last_at_ms = 0;
    closed = false;
    mutex = Mutex.create () }

let record recorder ~direction ~kind data =
  Mutex.lock recorder.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock recorder.mutex) (fun () ->
    if recorder.closed then invalid_arg "session recorder is closed";
    if recorder.seq >= max_frames then
      invalid_arg "session recording exceeds frame bound";
    if not (valid_kind kind) then invalid_arg "invalid frame kind";
    let data_json = Yojson.Basic.to_string data in
    if String.length data_json > max_data_bytes then
      invalid_arg "frame data exceeds 1 MiB";
    (* Clamped so a backward clock step can never reorder timestamps. *)
    let at_ms = max recorder.last_at_ms (now_ms () - recorder.started_ms) in
    recorder.seq <- recorder.seq + 1;
    recorder.last_at_ms <- at_ms;
    let direction_json = match direction with
      | `Input -> "input"
      | `Output -> "output" in
    (* kind is charset-validated, so it embeds verbatim. *)
    (try
       Printf.ksprintf (output_string recorder.out)
         "{\"seq\":%d,\"at_ms\":%d,\"direction\":\"%s\",\"kind\":\"%s\",\"data\":%s}\n"
         recorder.seq at_ms direction_json kind data_json;
       flush recorder.out
     with exn ->
       (* A failed write may have left a partial frame. Never append another
          frame to that stream, including through an already-waiting writer. *)
       recorder.closed <- true;
       close_out_noerr recorder.out;
       raise exn))

let close_recorder recorder =
  Mutex.lock recorder.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock recorder.mutex) (fun () ->
    if not recorder.closed then (
      recorder.closed <- true;
      close_out_noerr recorder.out))

type player = {
  read_line : unit -> string;
  mutable line_no : int;
  mutable frames_seen : int;
  mutable last_seq : int;
  mutable last_at_ms : int;
  mutable eof : bool;
}

let new_player read_line =
  { read_line; line_no = 0; frames_seen = 0;
    last_seq = 0; last_at_ms = 0; eof = false }

(* Bounded line reader: aborts playback on oversize rather than
   allocating an unbounded buffer. *)
let read_line_of_channel ic =
  let buffer = Buffer.create 256 in
  let rec loop () =
    match input_char ic with
    | exception End_of_file ->
        if Buffer.length buffer = 0 then raise End_of_file;
        Buffer.contents buffer
    | '\n' -> Buffer.contents buffer
    | c ->
        if Buffer.length buffer >= max_line_bytes then
          invalid_arg "session recording line exceeds size bound";
        Buffer.add_char buffer c;
        loop () in
  loop ()

let open_player ic = new_player (fun () -> read_line_of_channel ic)

let parse_frame player line =
  let bad () =
    invalid_arg (Printf.sprintf "malformed session recording line %d"
      player.line_no) in
  let json =
    match Yojson.Basic.from_string line with
    | json -> json
    | exception _ -> bad () in
  let fields = match json with
    | `Assoc fields -> fields
    | _ -> bad () in
  if List.length fields <> 5 then bad ();
  let field name =
    match List.assoc_opt name fields with
    | Some value -> value
    | None -> bad () in
  let seq = match field "seq" with
    | `Int n when n > 0 -> n
    | _ -> bad () in
  let at_ms = match field "at_ms" with
    | `Int n when n >= 0 -> n
    | _ -> bad () in
  let direction = match field "direction" with
    | `String "input" -> `Input
    | `String "output" -> `Output
    | _ -> bad () in
  let kind = match field "kind" with
    | `String s when valid_kind s -> s
    | _ -> bad () in
  let data = field "data" in
  if String.length (Yojson.Basic.to_string data) > max_data_bytes then
    invalid_arg "session recording frame data exceeds 1 MiB";
  if seq <> player.last_seq + 1 then bad ();
  if at_ms < player.last_at_ms then bad ();
  player.last_seq <- seq;
  player.last_at_ms <- at_ms;
  { seq; at_ms; direction; kind; data }

let next player =
  if player.eof then None
  else match player.read_line () with
    | exception End_of_file ->
        player.eof <- true;
        None
    | line ->
        player.line_no <- player.line_no + 1;
        if String.length line > max_line_bytes then
          invalid_arg "session recording line exceeds size bound";
        if player.frames_seen >= max_frames then
          invalid_arg "session recording exceeds frame bound";
        player.frames_seen <- player.frames_seen + 1;
        Some (parse_frame player line)

let frames_of_string text =
  let length = String.length text in
  let pos = ref 0 in
  let read_line () =
    if !pos >= length then raise End_of_file;
    let stop = match String.index_from_opt text !pos '\n' with
      | Some index -> index
      | None -> length in
    let width = stop - !pos in
    if width > max_line_bytes then
      invalid_arg "session recording line exceeds size bound";
    let line = String.sub text !pos width in
    pos := stop + 1;
    line in
  let player = new_player read_line in
  let rec collect acc =
    match next player with
    | None -> List.rev acc
    | Some frame -> collect (frame :: acc) in
  collect []

let replay player ~on_input ~on_output =
  let rec loop () =
    match next player with
    | None -> ()
    | Some frame ->
        (match frame.direction with
         | `Input -> on_input frame
         | `Output -> on_output frame);
        loop () in
  loop ()
