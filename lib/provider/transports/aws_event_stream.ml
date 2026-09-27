(* AWS EventStream prelude/header/frame CRCs follow the documented encoding:
   https://docs.aws.amazon.com/lexv2/latest/dg/event-stream-encoding.html *)
exception Invalid_message of string

type header_value =
  | Bool of bool
  | Byte of int
  | Short of int
  | Int of int32
  | Long of int64
  | Bytes of string
  | String of string
  | Timestamp of int64
  | Uuid of string

type message = { headers : (string * header_value) list; payload : string }

type decoder = {
  pending : Buffer.t;
  mutable expected : int option;
  mutable total : int;
  mutable frame_count : int;
  max_frame_bytes : int;
  max_total_bytes : int;
  max_frames : int;
}

let invalid detail = raise (Invalid_message detail)

let create ?(max_frame_bytes = 1_048_576) ?(max_total_bytes = 16_777_216)
    ?(max_frames = 4096) () =
  if max_frame_bytes < 16 || max_total_bytes < 16 || max_frames < 1 then
    invalid_arg "invalid AWS EventStream limits";
  { pending = Buffer.create 256; expected = None; total = 0;
    frame_count = 0; max_frame_bytes; max_total_bytes; max_frames }

let uint32 value offset =
  let byte n = Int32.of_int (Char.code value.[offset + n]) in
  Int32.logor (Int32.shift_left (byte 0) 24)
    (Int32.logor (Int32.shift_left (byte 1) 16)
      (Int32.logor (Int32.shift_left (byte 2) 8) (byte 3)))

let crc32 value offset length =
  let crc = ref 0xffffffffl in
  for i = offset to offset + length - 1 do
    crc := Int32.logxor !crc (Int32.of_int (Char.code value.[i]));
    for _ = 0 to 7 do
      crc := if Int32.logand !crc 1l <> 0l then
          Int32.logxor (Int32.shift_right_logical !crc 1) 0xedb88320l
        else Int32.shift_right_logical !crc 1
    done
  done;
  Int32.lognot !crc

let int_of_u32 label value =
  if Int32.compare value 0l < 0 then invalid ("invalid " ^ label);
  Int64.to_int (Int64.logand (Int64.of_int32 value) 0xffff_ffffL)

let parse_headers source offset length =
  let limit = offset + length in
  let cursor = ref offset in
  let require count =
    if count < 0 || !cursor > limit - count then invalid "truncated event headers" in
  let take_byte () = require 1; let value = Char.code source.[!cursor] in
    incr cursor; value in
  let take_u16 () = require 2;
    let value = (Char.code source.[!cursor] lsl 8) lor Char.code source.[!cursor + 1] in
    cursor := !cursor + 2; value in
  let take_i32 () = require 4; let value = uint32 source !cursor in
    cursor := !cursor + 4; value in
  let take_i64 () = require 8;
    let value = ref 0L in
    for _ = 1 to 8 do
      value := Int64.logor (Int64.shift_left !value 8)
        (Int64.of_int (Char.code source.[!cursor]));
      incr cursor
    done;
    !value in
  let take_string size =
    if size > 4096 then invalid "event header value exceeds 4 KiB";
    require size;
    let value = String.sub source !cursor size in
    cursor := !cursor + size;
    value in
  let rec loop count acc =
    if !cursor = limit then List.rev acc
    else if !cursor > limit || count >= 64 then invalid "invalid event header count"
    else
      let name_size = take_byte () in
      if name_size = 0 || name_size > 256 then invalid "invalid event header name";
      let name = take_string name_size in
      if String.exists (fun c -> Char.code c < 33 || Char.code c > 126) name then
        invalid "invalid event header name";
      let kind = take_byte () in
      let value = match kind with
        | 0 -> Bool true
        | 1 -> Bool false
        | 2 -> Byte (take_byte ())
        | 3 -> Short (take_u16 ())
        | 4 -> Int (take_i32 ())
        | 5 -> Long (take_i64 ())
        | 6 -> Bytes (take_string (take_u16 ()))
        | 7 -> String (take_string (take_u16 ()))
        | 8 -> Timestamp (take_i64 ())
        | 9 -> Uuid (take_string 16)
        | _ -> invalid "unknown event header value type" in
      if List.mem_assoc name acc then invalid "duplicate event header";
      loop (count + 1) ((name, value) :: acc)
  in
  loop 0 []

let decode_frame frame =
  let total = String.length frame in
  let headers_length = int_of_u32 "event headers length" (uint32 frame 4) in
  if headers_length > total - 16 then invalid "event headers exceed frame length";
  if crc32 frame 0 8 <> uint32 frame 8 then invalid "event prelude CRC mismatch";
  if crc32 frame 0 (total - 4) <> uint32 frame (total - 4) then
    invalid "event message CRC mismatch";
  let headers = parse_headers frame 12 headers_length in
  let payload_start = 12 + headers_length in
  { headers; payload = String.sub frame payload_start (total - payload_start - 4) }

let feed decoder data =
  let size = String.length data in
  let offset = ref 0 and messages = ref [] in
  let append count =
    if count > 0 then Buffer.add_substring decoder.pending data !offset count;
    offset := !offset + count in
  let inspect_prelude () =
    let prelude = Buffer.contents decoder.pending in
    let total = int_of_u32 "event frame length" (uint32 prelude 0) in
    let headers_length = int_of_u32 "event headers length" (uint32 prelude 4) in
    if total < 16 then invalid "event frame is shorter than its framing";
    if total > decoder.max_frame_bytes then invalid "event frame exceeds size limit";
    if headers_length > total - 16 then invalid "event headers exceed frame length";
    if crc32 prelude 0 8 <> uint32 prelude 8 then invalid "event prelude CRC mismatch";
    if decoder.total > decoder.max_total_bytes - total then
      invalid "event stream exceeds size limit";
    decoder.expected <- Some total in
  while !offset < size do
    let target = match decoder.expected with Some total -> total | None -> 12 in
    let pending = Buffer.length decoder.pending in
    let count = min (size - !offset) (target - pending) in
    append count;
    if Buffer.length decoder.pending = target then
      match decoder.expected with
      | None -> inspect_prelude ()
      | Some total ->
          let frame = Buffer.contents decoder.pending in
          let message = decode_frame frame in
          decoder.total <- decoder.total + total;
          decoder.frame_count <- decoder.frame_count + 1;
          if decoder.frame_count > decoder.max_frames then invalid "event stream has too many frames";
          Buffer.clear decoder.pending;
          decoder.expected <- None;
          messages := message :: !messages
  done;
  List.rev !messages

let finish decoder =
  if decoder.expected <> None || Buffer.length decoder.pending <> 0 then
    invalid "truncated event stream frame"

let header headers name = List.assoc_opt name headers
let string_header headers name = match header headers name with
  | Some (String value) -> Some value
  | _ -> None
