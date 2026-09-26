let max_file_bytes = 7 * 1024 * 1024

let base64 data =
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let length = String.length data in
  let output = Buffer.create (((length + 2) / 3) * 4) in
  let rec encode index =
    if index < length then (
      let remaining = length - index in
      let a = Char.code data.[index] in
      let b = if remaining > 1 then Char.code data.[index + 1] else 0 in
      let c = if remaining > 2 then Char.code data.[index + 2] else 0 in
      Buffer.add_char output alphabet.[a lsr 2];
      Buffer.add_char output alphabet.[((a land 0x03) lsl 4) lor (b lsr 4)];
      Buffer.add_char output (if remaining > 1 then
        alphabet.[((b land 0x0f) lsl 2) lor (c lsr 6)] else '=');
      Buffer.add_char output (if remaining > 2 then alphabet.[c land 0x3f] else '=');
      encode (index + 3)) in
  encode 0;
  Buffer.contents output

let mime_type path data =
  let length = String.length data in
  let starts prefix = length >= String.length prefix &&
    String.sub data 0 (String.length prefix) = prefix in
  match String.lowercase_ascii (Filename.extension path) with
  | ".png" when starts "\137PNG\r\n\026\n" -> "image/png"
  | ".jpg" | ".jpeg" when length >= 3 &&
      Char.code data.[0] = 0xff && Char.code data.[1] = 0xd8 &&
      Char.code data.[2] = 0xff -> "image/jpeg"
  | ".webp" when length >= 12 && starts "RIFF" &&
      String.sub data 8 4 = "WEBP" -> "image/webp"
  | ".png" | ".jpg" | ".jpeg" | ".webp" ->
      invalid_arg "image file extension does not match its image data"
  | _ -> invalid_arg "attachments support PNG, JPEG, and WebP images"

let load ~root path =
  if not (Filename.is_relative path) || path = "" then
    invalid_arg "image attachment path must be workspace-relative";
  let root = Unix.realpath root in
  let absolute = Tools.regular_path root path in
  let bytes = Tools.read_bounded absolute max_file_bytes in
  if bytes = "" then invalid_arg "image attachment is empty";
  let attachment = {
    Protocol.name = Filename.basename path;
    mime_type = mime_type path bytes;
    data = base64 bytes;
  } in
  Protocol.validate_attachments [attachment];
  attachment
