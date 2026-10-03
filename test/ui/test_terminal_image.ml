open Pave.Terminal_image

let fail label = failwith label
let expect label condition = if not condition then fail label
let expect_invalid label f =
  match f () with
  | _ -> fail (label ^ ": expected Invalid_argument")
  | exception Invalid_argument _ -> ()

let base64 bytes =
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let length = String.length bytes in
  let output = Buffer.create (((length + 2) / 3) * 4) in
  let rec encode offset =
    if offset < length then (
      let remaining = length - offset in
      let a = Char.code bytes.[offset] in
      let b = if remaining > 1 then Char.code bytes.[offset + 1] else 0 in
      let c = if remaining > 2 then Char.code bytes.[offset + 2] else 0 in
      Buffer.add_char output alphabet.[a lsr 2];
      Buffer.add_char output alphabet.[((a land 3) lsl 4) lor (b lsr 4)];
      Buffer.add_char output (if remaining > 1 then
        alphabet.[((b land 15) lsl 2) lor (c lsr 6)] else '=');
      Buffer.add_char output (if remaining > 2 then alphabet.[c land 63] else '=');
      encode (offset + 3)) in
  encode 0;
  Buffer.contents output

let png_bytes size =
  if size < 8 then invalid_arg "fixture too small";
  "\137PNG\r\n\026\n" ^ String.make (size - 8) 'x'

let image bytes = { mime_type = "image/png"; data = base64 bytes }
let has_prefix prefix value = String.starts_with ~prefix value

let () =
  expect "kitty terminal recognized"
    (detect ~term:"xterm-kitty" ~term_program:"kitty" ~ssh:false =
     Supported Kitty);
  expect "iTerm terminal recognized"
    (detect ~term:"xterm-256color" ~term_program:"iTerm.app" ~ssh:false =
     Supported ITerm2);
  expect "generic TERM is not capability"
    (detect ~term:"xterm-kitty" ~term_program:"unknown" ~ssh:false = Unsupported);
  expect "dumb terminal is unsupported"
    (detect ~term:"dumb" ~term_program:"kitty" ~ssh:false = Unsupported);
  expect "SSH terminal is unsupported"
    (detect ~term:"xterm-kitty" ~term_program:"kitty" ~ssh:true = Unsupported);

  let small = image (png_bytes 8) in
  let kitty = encode_image ~capability:(Supported Kitty) ~enabled:true small in
  expect "Kitty framing and one-chunk final marker"
    (kitty = ["\027_Ga=T,t=d,f=100,m=0;iVBORw0KGgo=\027\\"]);
  let iterm = encode_image ~capability:(Supported ITerm2) ~enabled:true small in
  expect "iTerm2 OSC framing"
    (iterm = ["\027]1337;File=inline=1;width=auto;height=auto;preserveAspectRatio=1:iVBORw0KGgo=\007"]);

  let first_chunk = image (png_bytes 3072) in
  let chunks = encode_image ~capability:(Supported Kitty) ~enabled:true first_chunk in
  expect "exact Kitty chunk boundary produces one final chunk"
    (match chunks with
     | [line] -> has_prefix "\027_Ga=T,t=d,f=100,m=0;" line &&
         String.length line = String.length "\027_Ga=T,t=d,f=100,m=0;" + 4096 + 2
     | _ -> false);
  let split_chunks = encode_image ~capability:(Supported Kitty) ~enabled:true
      (image (png_bytes 3075)) in
  expect "Kitty chunks at 4096 base64 characters and closes each APC"
    (match split_chunks with
     | [first; last] ->
         has_prefix "\027_Ga=T,t=d,f=100,m=1;" first &&
         String.length first = String.length "\027_Ga=T,t=d,f=100,m=1;" + 4096 + 2 &&
         has_prefix "\027_Gm=0;" last && String.length last =
           String.length "\027_Gm=0;" + 4 + 2
     | _ -> false);

  let jpeg = { mime_type = "image/jpeg"; data = base64 "\255\216\255x" } in
  let webp = { mime_type = "image/webp"; data = base64 "RIFFxxxxWEBPx" } in
  List.iter (fun image ->
    expect "Kitty never labels JPEG or WebP bytes as PNG"
      (encode_image ~capability:(Supported Kitty) ~enabled:true image = []);
    expect "iTerm2 retains its native JPEG and WebP display support"
      (match encode_image ~capability:(Supported ITerm2) ~enabled:true image with
       | [line] -> String.ends_with ~suffix:(image.data ^ "\007") line
       | _ -> false)) [jpeg; webp];

  expect_invalid "MIME/magic mismatch"
    (fun () -> encode_image ~capability:(Supported Kitty) ~enabled:true
      { mime_type = "image/jpeg"; data = small.data });
  expect_invalid "noncanonical base64 pad bits"
    (fun () -> encode_image ~capability:(Supported Kitty) ~enabled:true
      { mime_type = "image/png"; data = "iVBORw0KGg1=" });
  expect_invalid "unsupported MIME"
    (fun () -> encode_image ~capability:(Supported ITerm2) ~enabled:true
      { mime_type = "image/gif"; data = small.data });
  expect_invalid "image size limit"
    (fun () -> encode_image ~capability:(Supported ITerm2) ~enabled:true
      (image (png_bytes (max_image_bytes + 1))));
  let max_payload = encode_image ~capability:(Supported ITerm2) ~enabled:true
      (image (png_bytes max_image_bytes)) in
  expect "exact maximum image size is accepted"
    (match max_payload with
     | [line] -> String.length line =
         String.length "\027]1337;File=inline=1;width=auto;height=auto;preserveAspectRatio=1:" +
         String.length (base64 (png_bytes max_image_bytes)) + 1
     | _ -> false);

  expect "Kitty delete-all sequence"
    (clear ~capability:(Supported Kitty) ~enabled:true = ["\027_Ga=d,d=A\027\\"]);
  expect "iTerm2 has no unsafe delete approximation"
    (clear ~capability:(Supported ITerm2) ~enabled:true = []);
  expect "disabled capability emits no bytes"
    (encode_image ~capability:(Supported Kitty) ~enabled:false small = [] &&
     clear ~capability:(Supported Kitty) ~enabled:false = []);
  expect "fallback emits no escape or image bytes"
    (encode_image ~capability:Unsupported ~enabled:true small = [] &&
     clear ~capability:Unsupported ~enabled:true = []);
  print_endline "terminal image capability and framing: ok"
