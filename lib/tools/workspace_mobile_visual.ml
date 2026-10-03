exception Error of string

let fail message = raise (Error message)

let max_png_bytes = 16 * 1024 * 1024
let max_baseline_bytes = 20 * 1024 * 1024
let max_pixels = 16_777_216
let max_dimension = 16_384
let version = 1

type rect = { x : int; y : int; width : int; height : int }
type metadata = {
  app : string;
  platform : string;
  device : string;
  os : string;
  locale : string;
  theme : string;
  width : int;
  height : int;
  masks : rect list;
}
type capture = { png : string; complete : bool; metadata : metadata }
type difference = { equal : bool; differing_pixels : int; first_difference : (int * int) option }

type pixels = { width : int; height : int; rgba : string }

let validate_dimensions width height =
  if width <= 0 || height <= 0 || width > max_dimension || height > max_dimension ||
     width > max_pixels / height then fail "invalid or oversized screenshot dimensions"

let validate_metadata metadata =
  List.iter (fun (label, value) ->
    if value = "" || String.length value > 512 || String.contains value '\000' then
      fail ("invalid " ^ label))
    ["app", metadata.app; "platform", metadata.platform; "device", metadata.device;
     "OS", metadata.os; "locale", metadata.locale; "theme", metadata.theme];
  validate_dimensions metadata.width metadata.height;
  List.iter (fun mask ->
    if mask.x < 0 || mask.y < 0 || mask.width <= 0 || mask.height <= 0 ||
       mask.x > metadata.width - mask.width || mask.y > metadata.height - mask.height then
      fail "dynamic-region mask is invalid or out of bounds") metadata.masks;
  if List.length metadata.masks > 1024 then fail "too many dynamic-region masks"

let byte value = Char.chr (value land 255)
let uint16_le data offset = Char.code data.[offset] lor (Char.code data.[offset + 1] lsl 8)
let uint32_le data offset =
  Int64.logor (Int64.of_int (uint16_le data offset))
    (Int64.shift_left (Int64.of_int (uint16_le data (offset + 2))) 16)
let int32_le data offset =
  let value = uint32_le data offset in
  if value > Int64.of_int max_int then fail "BMP field exceeds platform integer range";
  Int64.to_int value

let decode_bmp data =
  let length = String.length data in
  let require condition message = if not condition then fail message in
  require (length >= 54 && String.sub data 0 2 = "BM") "malformed or truncated BMP";
  let offset = int32_le data 10 and header_size = int32_le data 14 in
  require (header_size >= 40 && offset >= 54 && offset <= length) "unsupported BMP header";
  require (int32_le data 18 > 0) "invalid BMP width";
  let signed_height = Int64.to_int (Int64.logand (uint32_le data 22) 0xffff_ffffL) in
  let signed_height = if signed_height >= 0x8000_0000 then signed_height - 0x1_0000_0000 else signed_height in
  require (signed_height <> 0) "invalid BMP height";
  let width = int32_le data 18 and height = abs signed_height in
  validate_dimensions width height;
  let planes = uint16_le data 26 and bpp = uint16_le data 28 and compression = int32_le data 30 in
  require (planes = 1 && (bpp = 24 || bpp = 32) && compression = 0)
    "unsupported BMP encoding";
  let row_bytes = ((width * (bpp / 8) + 3) / 4) * 4 in
  let data_end = Int64.add (Int64.of_int offset)
      (Int64.mul (Int64.of_int row_bytes) (Int64.of_int height)) in
  require (Int64.compare data_end (Int64.of_int length) <= 0)
    "truncated BMP pixel data";
  let rgba = Bytes.create (width * height * 4) in
  for y = 0 to height - 1 do
    let source_y = if signed_height > 0 then height - y - 1 else y in
    for x = 0 to width - 1 do
      let source = offset + source_y * row_bytes + x * (bpp / 8) in
      let target = (y * width + x) * 4 in
      Bytes.set rgba target data.[source + 2]; Bytes.set rgba (target + 1) data.[source + 1];
      Bytes.set rgba (target + 2) data.[source];
      Bytes.set rgba (target + 3) (if bpp = 32 then data.[source + 3] else byte 255)
    done
  done;
  { width; height; rgba = Bytes.unsafe_to_string rgba }

let masked masks x y = List.exists (fun r -> x >= r.x && y >= r.y && x < r.x + r.width && y < r.y + r.height) masks

let compare_pixels ~masks left right =
  if left.width <> right.width || left.height <> right.height then fail "screenshot dimensions mismatch";
  validate_dimensions left.width left.height;
  if String.length left.rgba <> left.width * left.height * 4 ||
     String.length right.rgba <> right.width * right.height * 4 then fail "invalid pixel buffer";
  let count = ref 0 and first = ref None in
  for y = 0 to left.height - 1 do for x = 0 to left.width - 1 do
    if not (masked masks x y) then begin
      let pos = (y * left.width + x) * 4 in
      if left.rgba.[pos] <> right.rgba.[pos] || left.rgba.[pos + 1] <> right.rgba.[pos + 1] ||
         left.rgba.[pos + 2] <> right.rgba.[pos + 2] || left.rgba.[pos + 3] <> right.rgba.[pos + 3] then begin
        incr count; if !first = None then first := Some (x, y)
      end
    end
  done done;
  { equal = !count = 0; differing_pixels = !count; first_difference = !first }

let read_file path maximum =
  let stat = Unix.LargeFile.stat path in
  if stat.Unix.LargeFile.st_size < 0L || stat.Unix.LargeFile.st_size > Int64.of_int maximum then
    fail "file exceeds size limit";
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () -> really_input_string channel (Int64.to_int stat.Unix.LargeFile.st_size))

let validate_png_header png =
  if String.length png < 33 || String.length png > max_png_bytes || String.sub png 0 8 <> "\137PNG\r\n\026\n" ||
     String.sub png 12 4 <> "IHDR" then fail "incomplete or invalid PNG capture";
  let read32 pos =
    let n i = Int64.of_int (Char.code png.[pos + i]) in
    Int64.to_int (Int64.logor (Int64.shift_left (n 0) 24) (Int64.logor (Int64.shift_left (n 1) 16) (Int64.logor (Int64.shift_left (n 2) 8) (n 3)))) in
  let width = read32 16 and height = read32 20 in
  validate_dimensions width height;
  if String.sub png (String.length png - 8) 4 <> "IEND" then fail "incomplete PNG capture";
  width, height

let decode_png png =
  let width, height = validate_png_header png in
  if Sys.os_type <> "Unix" ||
     not (try Unix.access "/usr/bin/swift" [Unix.X_OK]; true with _ -> false) then
    fail "PNG pixel decoding requires macOS Swift and ImageIO";
  let input = Filename.temp_file "pave-vr01-" ".png" in
  let rgba = input ^ ".rgba" in
  Fun.protect ~finally:(fun () ->
    List.iter (fun path -> try Unix.unlink path with _ -> ()) [input; rgba])
    (fun () ->
      let out = open_out_bin input in
      Fun.protect ~finally:(fun () -> close_out_noerr out)
        (fun () -> output_string out png);
      let decoder = {|import Foundation
import ImageIO
import CoreGraphics
let sourceURL = URL(fileURLWithPath: CommandLine.arguments[1])
let outputURL = URL(fileURLWithPath: CommandLine.arguments[2])
guard let source = CGImageSourceCreateWithURL(sourceURL as CFURL, nil),
      let image = CGImageSourceCreateImageAtIndex(source, 0, nil) else { exit(2) }
let width = image.width, height = image.height
var pixels = Data(count: width * height * 4)
let colorSpace = CGColorSpaceCreateDeviceRGB()
let info = CGBitmapInfo.byteOrder32Big.rawValue | CGImageAlphaInfo.premultipliedLast.rawValue
let rendered = pixels.withUnsafeMutableBytes { bytes -> Bool in
  guard let context = CGContext(data: bytes.baseAddress, width: width, height: height,
      bitsPerComponent: 8, bytesPerRow: width * 4, space: colorSpace, bitmapInfo: info) else {
    return false
  }
  context.translateBy(x: 0, y: CGFloat(height))
  context.scaleBy(x: 1, y: -1)
  context.draw(image, in: CGRect(x: 0, y: 0, width: width, height: height))
  return true
}
guard rendered else { exit(3) }
do { try pixels.write(to: outputURL, options: .atomic) } catch { exit(4) }
|} in
      let null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0 in
      let status =
        Fun.protect ~finally:(fun () -> Unix.close null) (fun () ->
          let argv = [|"/usr/bin/swift"; "-e"; decoder; input; rgba|] in
          let pid = Unix.create_process "/usr/bin/swift" argv null null null in
          snd (Unix.waitpid [] pid)) in
      (match status with
       | Unix.WEXITED 0 -> ()
       | _ -> fail "macOS ImageIO could not decode PNG capture");
      let pixels = read_file rgba (max_baseline_bytes * 4) in
      if String.length pixels <> width * height * 4 then
        fail "PNG pixel decoder returned an incomplete buffer";
      { width; height; rgba = pixels })

let metadata_json m = `Assoc ["app", `String m.app; "platform", `String m.platform;
  "device", `String m.device; "os", `String m.os; "locale", `String m.locale;
  "theme", `String m.theme; "width", `Int m.width; "height", `Int m.height;
  "masks", `List (List.map (fun r -> `Assoc ["x", `Int r.x; "y", `Int r.y; "width", `Int r.width; "height", `Int r.height]) m.masks)]
let metadata_of_json json =
  let field key = Yojson.Basic.Util.member key json in
  let string key = Yojson.Basic.Util.to_string (field key) and integer key = Yojson.Basic.Util.to_int (field key) in
  let masks = Yojson.Basic.Util.to_list (field "masks") |> List.map (fun item ->
    let get key = Yojson.Basic.Util.to_int (Yojson.Basic.Util.member key item) in
    {x=get "x"; y=get "y"; width=get "width"; height=get "height"}) in
  {app=string "app"; platform=string "platform"; device=string "device"; os=string "os";
   locale=string "locale"; theme=string "theme"; width=integer "width"; height=integer "height"; masks}

let validate_key key =
  if key = "" || String.length key > 80 || not (String.for_all (function 'a'..'z'|'A'..'Z'|'0'..'9'|'-'|'_' -> true | _ -> false) key) then fail "invalid baseline name"

let ensure_private_dir path create private_dir =
  if create then (try Unix.mkdir path 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let st = try Unix.lstat path with _ -> fail "baseline storage directory is missing" in
  if st.Unix.st_kind <> Unix.S_DIR || st.Unix.st_uid <> Unix.geteuid () ||
     (private_dir && st.Unix.st_perm land 0o077 <> 0) then
    fail "baseline storage directory is not owner-controlled and private"

let storage_dir workspace create =
  let root = try Unix.realpath workspace with _ -> fail "workspace path cannot be resolved" in
  let pave = Filename.concat root ".pave" and dir = Filename.concat (Filename.concat root ".pave") "mobile-baselines" in
  ensure_private_dir pave create false; ensure_private_dir dir create true; dir

let write_atomic path data =
  let temp = Filename.temp_file ~temp_dir:(Filename.dirname path) ".vr01-" ".tmp" in
  Fun.protect ~finally:(fun () -> try Unix.unlink temp with _ -> ()) (fun () ->
    Unix.chmod temp 0o600;
    let ch = open_out_bin temp in output_string ch data; flush ch; close_out ch;
    Unix.rename temp path)

let save ~workspace ~name capture =
  validate_key name;
  if not capture.complete then fail "incomplete screenshot cannot be saved as a baseline";
  validate_metadata capture.metadata;
  if String.length capture.png > max_png_bytes then fail "PNG capture exceeds size limit";
  let width, height = validate_png_header capture.png in
  if width <> capture.metadata.width || height <> capture.metadata.height then fail "capture dimensions do not match metadata";
  ignore (decode_png capture.png);
  let dir = storage_dir workspace true in
  let image = Filename.concat dir (name ^ ".png") and record = Filename.concat dir (name ^ ".json") in
  let json = Yojson.Basic.to_string (`Assoc ["version", `Int version; "metadata", metadata_json capture.metadata]) in
  if String.length json + String.length capture.png > max_baseline_bytes then fail "baseline exceeds size limit";
  write_atomic image capture.png;
  write_atomic record json

let safe_baseline_file path maximum =
  let st = try Unix.lstat path with _ -> fail "baseline file is missing" in
  if st.Unix.st_kind <> Unix.S_REG || st.Unix.st_uid <> Unix.geteuid () ||
     st.Unix.st_perm land 0o077 <> 0 || st.Unix.st_size > maximum then
    fail "baseline file is not private, owner-controlled, or within size limits";
  read_file path maximum
let compare ~workspace ~name capture =
  validate_key name;
  if not capture.complete then fail "incomplete screenshot cannot be compared";
  validate_metadata capture.metadata;
  if String.length capture.png > max_png_bytes then fail "PNG capture exceeds size limit";
  let width, height = validate_png_header capture.png in
  if width <> capture.metadata.width || height <> capture.metadata.height then fail "capture dimensions do not match metadata";
  let dir = storage_dir workspace false in
  let image = Filename.concat dir (name ^ ".png") and record = Filename.concat dir (name ^ ".json") in
  let json_text = safe_baseline_file record max_baseline_bytes in
  let json = try Yojson.Basic.from_string json_text with _ -> fail "corrupt baseline record" in
  let stored = try
    if Yojson.Basic.Util.to_int (Yojson.Basic.Util.member "version" json) <> version then fail "unsupported baseline version";
    metadata_of_json (Yojson.Basic.Util.member "metadata" json)
  with Error _ as e -> raise e | _ -> fail "corrupt baseline metadata" in
  validate_metadata stored;
  if stored <> capture.metadata then fail "baseline metadata mismatch";
  let baseline_png = safe_baseline_file image max_png_bytes in
  let baseline_width, baseline_height = validate_png_header baseline_png in
  if baseline_width <> stored.width || baseline_height <> stored.height then fail "baseline image dimensions mismatch";
  let previous = decode_png baseline_png and current = decode_png capture.png in
  compare_pixels ~masks:stored.masks previous current
