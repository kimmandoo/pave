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
  let mp4_container = length >= 12 && String.sub data 4 4 = "ftyp" in
  let mp3 = starts "ID3" || (length >= 2 &&
    Char.code data.[0] = 0xff && Char.code data.[1] land 0xe0 = 0xe0) in
  let aac = length >= 2 && Char.code data.[0] = 0xff &&
    Char.code data.[1] land 0xf6 = 0xf0 in
  match String.lowercase_ascii (Filename.extension path) with
  | ".png" when starts "\137PNG\r\n\026\n" -> "image/png"
  | ".jpg" | ".jpeg" when length >= 3 &&
      Char.code data.[0] = 0xff && Char.code data.[1] = 0xd8 &&
      Char.code data.[2] = 0xff -> "image/jpeg"
  | ".webp" when length >= 12 && starts "RIFF" &&
      String.sub data 8 4 = "WEBP" -> "image/webp"
  | ".wav" when length >= 12 && starts "RIFF" &&
      String.sub data 8 4 = "WAVE" -> "audio/wav"
  | ".mp3" when mp3 -> "audio/mp3"
  | ".aac" when aac -> "audio/aac"
  | ".ogg" when starts "OggS" -> "audio/ogg"
  | ".opus" when starts "OggS" -> "audio/opus"
  | ".flac" when starts "fLaC" -> "audio/flac"
  | ".m4a" when mp4_container -> "audio/m4a"
  | ".mp4" when mp4_container -> "video/mp4"
  | ".webm" when starts "\026E\223\163" -> "video/webm"

  | ".png" | ".jpg" | ".jpeg" | ".webp" | ".wav" | ".mp3" | ".aac"
  | ".ogg" | ".opus" | ".flac" | ".m4a" | ".mp4" | ".webm" ->
      invalid_arg "media file extension does not match its file signature"
  | _ -> invalid_arg
      "attachments support PNG, JPEG, WebP, WAV, MP3, AAC, OGG, Opus, FLAC, M4A, MP4, and WebM"

let load ~root path =
  if not (Filename.is_relative path) || path = "" then
    invalid_arg "media attachment path must be workspace-relative";
  let root = Unix.realpath root in
  let absolute = Workspace_path.regular_path root path in
  let bytes = Workspace_path.read_bounded absolute max_file_bytes in
  if bytes = "" then invalid_arg "media attachment is empty";
  let attachment = {
    Protocol.name = Filename.basename path;
    mime_type = mime_type path bytes;
    data = base64 bytes;
  } in
  Protocol.validate_attachments [attachment];
  attachment
