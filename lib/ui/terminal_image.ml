type protocol = Kitty | ITerm2

type capability = Unsupported | Supported of protocol

type image = { mime_type : string; data : string }

let max_image_bytes = 1024 * 1024
let kitty_chunk_bytes = 4096

(* A generic TERM value is intentionally insufficient: only explicit vendor
   identities opt in, and SSH/dumb sessions always remain unsupported. *)
let detect ~term ~term_program ~ssh =
  if ssh || String.equal (String.lowercase_ascii term) "dumb" then Unsupported
  else
    match String.lowercase_ascii term_program with
    | "kitty" -> Supported Kitty
    | "iterm.app" | "iterm2" -> Supported ITerm2
    | _ -> Unsupported

let base64_value = function
  | 'A'..'Z' as c -> Char.code c - Char.code 'A'
  | 'a'..'z' as c -> Char.code c - Char.code 'a' + 26
  | '0'..'9' as c -> Char.code c - Char.code '0' + 52
  | '+' -> 62
  | '/' -> 63
  | _ -> -1

let decode_base64 encoded =
  let length = String.length encoded in
  if length = 0 || length mod 4 <> 0 ||
     length > ((max_image_bytes + 2) / 3 * 4) then
    invalid_arg "terminal image has invalid or oversized base64 data";
  let padding =
    if encoded.[length - 1] = '=' then
      if length > 1 && encoded.[length - 2] = '=' then 2 else 1
    else 0 in
  let decoded_length = (length / 4 * 3) - padding in
  if decoded_length = 0 || decoded_length > max_image_bytes then
    invalid_arg "terminal image is empty or oversized";
  let output = Bytes.create decoded_length in
  let target = ref 0 in
  for offset = 0 to (length / 4) - 1 do
    let i = offset * 4 in
    let last = offset = length / 4 - 1 in
    let a = base64_value encoded.[i]
    and b = base64_value encoded.[i + 1] in
    let c = if encoded.[i + 2] = '=' then -2 else base64_value encoded.[i + 2]
    and d = if encoded.[i + 3] = '=' then -2 else base64_value encoded.[i + 3] in
    if a < 0 || b < 0 || c = -1 || d = -1 ||
       (not last && (c = -2 || d = -2)) ||
       (c = -2 && d <> -2) || (c = -2 && b land 15 <> 0) ||
       (d = -2 && c >= 0 && c land 3 <> 0) then
      invalid_arg "terminal image has malformed base64 data";
    let value = (a lsl 18) lor (b lsl 12) lor
      ((max 0 c) lsl 6) lor max 0 d in
    let put byte =
      if !target >= decoded_length then
        invalid_arg "terminal image has malformed base64 data";
      Bytes.set output !target (Char.chr byte);
      incr target in
    put ((value lsr 16) land 255);
    if c <> -2 then put ((value lsr 8) land 255);
    if d <> -2 then put (value land 255)
  done;
  if !target <> decoded_length then
    invalid_arg "terminal image has malformed base64 data";
  Bytes.unsafe_to_string output

let starts bytes prefix =
  String.length bytes >= String.length prefix &&
  String.sub bytes 0 (String.length prefix) = prefix

let supported_mime mime_type bytes =
  match mime_type with
  | "image/png" -> starts bytes "\137PNG\r\n\026\n"
  | "image/jpeg" -> String.length bytes >= 3 &&
      Char.code bytes.[0] = 0xff && Char.code bytes.[1] = 0xd8 &&
      Char.code bytes.[2] = 0xff
  | "image/webp" -> String.length bytes >= 12 && starts bytes "RIFF" &&
      String.sub bytes 8 4 = "WEBP"
  | _ -> false

let validate image =
  let bytes = decode_base64 image.data in
  if not (supported_mime image.mime_type bytes) then
    invalid_arg "terminal image MIME type does not match a supported image signature";
  bytes

let apc body = "\027_G" ^ body ^ "\027\\"

let kitty_payload base64 =
  let length = String.length base64 in
  let rec chunks offset first acc =
    let remaining = length - offset in
    let count = min kitty_chunk_bytes remaining in
    let final = count = remaining in
    let header = if first then "a=T,t=d,f=100," else "" in
    let marker = if final then "0" else "1" in
    let payload = String.sub base64 offset count in
    let line = apc (header ^ "m=" ^ marker ^ ";" ^ payload) in
    if final then List.rev (line :: acc)
    else chunks (offset + count) false (line :: acc)
  in
  chunks 0 true []

let encode_image ~capability ~enabled image =
  match capability, enabled with
  | Unsupported, _ | _, false -> []
  | Supported _, _ ->
      let _bytes = validate image in
      (match capability with
       | Unsupported -> []
       | Supported Kitty -> kitty_payload image.data
       | Supported ITerm2 ->
           ["\027]1337;File=inline=1;width=auto;height=auto;preserveAspectRatio=1:"
            ^ image.data ^ "\007"])

(* iTerm2's inline-image protocol has no image-delete primitive. Do not
   approximate deletion by clearing the user's screen; only Kitty supports it. *)
let clear ~capability ~enabled =
  match capability, enabled with
  | Supported Kitty, true -> [apc "a=d,d=A"]
  | _ -> []
