exception Error of string

let fail message = raise (Error message)

let max_png_bytes = 16 * 1024 * 1024
let max_baseline_bytes = 20 * 1024 * 1024
let max_pixels = 16_777_216
let max_dimension = 16_384
let version = 2

type rect = { x : int; y : int; width : int; height : int }
type metadata = {
  app : string;
  platform : string;
  device : string;
  build_hash : string;
  os : string;
  locale : string;
  theme : string;
  width : int;
  height : int;
  masks : rect list;
}
type capture = { png : string; complete : bool; metadata : metadata }
type difference = { equal : bool; differing_pixels : int; first_difference : (int * int) option }
type report = { comparison : difference; baseline_png : string; current_png : string; difference_png : string; regions : rect list; settings_version : int; threshold : int; max_differing_pixels : int }

type pixels = { width : int; height : int; rgba : string }

let validate_dimensions width height =
  if width <= 0 || height <= 0 || width > max_dimension || height > max_dimension ||
     width > max_pixels / height then fail "invalid or oversized screenshot dimensions"
let valid_sha256 value =
  String.length value = 64 &&
  String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false) value


let validate_metadata metadata =
  List.iter (fun (label, value) ->
    if value = "" || String.length value > 512 || String.contains value '\000' then
      fail ("invalid " ^ label))
    ["app", metadata.app; "platform", metadata.platform; "device", metadata.device;
     "build SHA-256", metadata.build_hash; "OS", metadata.os;
     "locale", metadata.locale; "theme", metadata.theme];
  if not (valid_sha256 metadata.build_hash) then
    fail "invalid selected build identity";
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

let compare_pixels ~threshold ~masks left right =
  if threshold < 0 || threshold > 255 then fail "pixel threshold must be between 0 and 255";
  if left.width <> right.width || left.height <> right.height then fail "screenshot dimensions mismatch";
  validate_dimensions left.width left.height;
  if String.length left.rgba <> left.width * left.height * 4 ||
     String.length right.rgba <> right.width * right.height * 4 then fail "invalid pixel buffer";
  let count = ref 0 and first = ref None in
  for y = 0 to left.height - 1 do for x = 0 to left.width - 1 do
    if not (masked masks x y) then begin
      let pos = (y * left.width + x) * 4 in
      let changed = ref false in
      for channel=0 to 3 do
        if abs (Char.code left.rgba.[pos+channel] - Char.code right.rgba.[pos+channel]) > threshold then changed:=true
      done;
      if !changed then begin incr count; if !first = None then first := Some (x, y) end
    end
  done done;
  { equal = !count = 0; differing_pixels = !count; first_difference = !first }

let read_file path maximum =
  let stat = Unix.LargeFile.stat path in
  if stat.Unix.LargeFile.st_size < 0L || stat.Unix.LargeFile.st_size > Int64.of_int maximum then
    fail "file exceeds size limit";
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () -> really_input_string channel (Int64.to_int stat.Unix.LargeFile.st_size))

let be32 data offset =
  let n i = Int64.of_int (Char.code data.[offset + i]) in
  Int64.to_int (Int64.logor (Int64.shift_left (n 0) 24)
    (Int64.logor (Int64.shift_left (n 1) 16) (Int64.logor (Int64.shift_left (n 2) 8) (n 3))))
let validate_png_header data =
  let require condition message = if not condition then fail message in
  require (String.length data >= 33 &&
    String.sub data 0 8 = "\137PNG\r\n\026\n") "malformed or truncated PNG";
  require (be32 data 8 = 13 && String.sub data 12 4 = "IHDR")
    "PNG header is missing or invalid";
  let width = be32 data 16 and height = be32 data 20 in
  validate_dimensions width height;
  width, height

let crc32 data start length =
  let crc = ref 0xffff_ffffl in
  for i = start to start + length - 1 do
    crc := Int32.logxor !crc (Int32.of_int (Char.code data.[i]));
    for _ = 0 to 7 do
      crc := if Int32.logand !crc 1l <> 0l then
        Int32.logxor (Int32.shift_right_logical !crc 1) 0xedb8_8320l
      else Int32.shift_right_logical !crc 1
    done
  done;
  Int32.lognot !crc

let adler32 data =
  let a = ref 1 and b = ref 0 in
  String.iter (fun c -> a := (!a + Char.code c) mod 65521; b := (!b + !a) mod 65521) data;
  Int32.logor (Int32.shift_left (Int32.of_int !b) 16) (Int32.of_int !a)

type bit_reader = { input : string; mutable bit : int; limit : int }
let read_bits reader count =
  if count < 0 || count > 24 || reader.bit > reader.limit * 8 - count then fail "truncated PNG deflate stream";
  let value = ref 0 in
  for i = 0 to count - 1 do
    let pos = reader.bit + i in
    value := !value lor (((Char.code reader.input.[pos lsr 3] lsr (pos land 7)) land 1) lsl i)
  done;
  reader.bit <- reader.bit + count;
  !value

let reverse_bits value count =
  let result = ref 0 in
  for i = 0 to count - 1 do result := (!result lsl 1) lor ((value lsr i) land 1) done;
  !result

let huffman lengths =
  let counts = Array.make 16 0 in
  Array.iter (fun length -> if length < 0 || length > 15 then fail "invalid PNG Huffman code"; if length > 0 then counts.(length) <- counts.(length) + 1) lengths;
  let available=ref 1 in
  for bits=1 to 15 do
    available := (!available lsl 1) - counts.(bits);
    if !available < 0 then fail "oversubscribed PNG Huffman table"
  done;
  let next = Array.make 16 0 and code = ref 0 in
  for bits = 1 to 15 do code := (!code + counts.(bits - 1)) lsl 1; next.(bits) <- !code done;
  let table = Hashtbl.create (Array.length lengths) in
  Array.iteri (fun symbol length -> if length > 0 then begin
    let reversed = reverse_bits next.(length) length in
    Hashtbl.add table ((length lsl 16) lor reversed) symbol;
    next.(length) <- next.(length) + 1
  end) lengths;
  table

let huffman_symbol reader table =
  let code = ref 0 and found = ref None and length = ref 0 in
  while !found = None && !length < 15 do
    code := !code lor (read_bits reader 1 lsl !length);
    incr length;
    found := Hashtbl.find_opt table ((!length lsl 16) lor !code)
  done;
  match !found with Some symbol -> symbol | None -> fail "invalid PNG Huffman symbol"

let inflate_zlib compressed expected =
  if String.length compressed < 6 then fail "truncated PNG zlib stream";
  let cmf = Char.code compressed.[0] and flg = Char.code compressed.[1] in
  if cmf land 15 <> 8 || cmf lsr 4 > 7 || ((cmf lsl 8) + flg) mod 31 <> 0 || flg land 32 <> 0 then
    fail "unsupported PNG zlib header";
  let reader = {input=compressed; bit=16; limit=String.length compressed - 4} in
  let out = Bytes.create expected and used = ref 0 and finished = ref false in
  let emit value = if !used >= expected then fail "oversized PNG deflate output" else (Bytes.set out !used (Char.chr (value land 255)); incr used) in
  let length_base = [|3;4;5;6;7;8;9;10;11;13;15;17;19;23;27;31;35;43;51;59;67;83;99;115;131;163;195;227;258|] in
  let length_extra = [|0;0;0;0;0;0;0;0;1;1;1;1;2;2;2;2;3;3;3;3;4;4;4;4;5;5;5;5;0|] in
  let dist_base = [|1;2;3;4;5;7;9;13;17;25;33;49;65;97;129;193;257;385;513;769;1025;1537;2049;3073;4097;6145;8193;12289;16385;24577|] in
  let dist_extra = [|0;0;0;0;1;1;2;2;3;3;4;4;5;5;6;6;7;7;8;8;9;9;10;10;11;11;12;12;13;13|] in
  let fixed_lit = Array.init 288 (fun i -> if i <= 143 then 8 else if i <= 255 then 9 else if i <= 279 then 7 else 8) |> huffman in
  let fixed_dist = huffman (Array.make 32 5) in
  while not !finished do
    finished := read_bits reader 1 = 1;
    let kind = read_bits reader 2 in
    let lit, dist = if kind = 1 then fixed_lit, fixed_dist else if kind = 2 then begin
      let hlit = read_bits reader 5 + 257 in
      let hdist = read_bits reader 5 + 1 in
      let hclen = read_bits reader 4 + 4 in
      let order = [|16;17;18;0;8;7;9;6;10;5;11;4;12;3;13;2;14;1;15|] in
      let clen = Array.make 19 0 in
      for i=0 to hclen-1 do clen.(order.(i)) <- read_bits reader 3 done;
      let ct = huffman clen and all = ref [] in
      while List.length !all < hlit + hdist do
        let sym = huffman_symbol reader ct in
        if sym <= 15 then all := sym :: !all
        else if sym = 16 then begin
          if !all = [] then fail "invalid PNG repeat code";
          let n = read_bits reader 2 + 3 in
          let value = List.hd !all in
          for _=1 to n do all := value :: !all done
        end else if sym = 17 then (let n=read_bits reader 3+3 in for _=1 to n do all:=0::!all done)
        else if sym = 18 then (let n=read_bits reader 7+11 in for _=1 to n do all:=0::!all done)
        else fail "invalid PNG code length";
        if List.length !all > hlit + hdist then fail "PNG code lengths exceed table"
      done;
      let all = Array.of_list (List.rev !all) in
      let lt = huffman (Array.sub all 0 hlit) and dt = huffman (Array.sub all hlit hdist) in
      if Array.length all = 0 then fail "empty PNG Huffman table";
      lt, dt
    end else if kind = 0 then begin
      reader.bit <- (reader.bit + 7) land (lnot 7);
      let n=read_bits reader 16 in
      let complement=read_bits reader 16 in
      if n lxor complement <> 0xffff then fail "invalid PNG stored deflate block";
      for _=1 to n do emit (read_bits reader 8) done;
      (* Stored blocks are complete blocks and don't use Huffman symbols. *)
      huffman [||], huffman [||]
    end else fail "reserved PNG deflate block" in
    if kind <> 0 then begin
      let ended = ref false in
      while not !ended do
        let symbol = huffman_symbol reader lit in
        if symbol < 256 then emit symbol
        else if symbol = 256 then ended := true
        else if symbol <= 285 then begin
          let idx = symbol - 257 in
          let len = length_base.(idx) + read_bits reader length_extra.(idx) in
          let ds = huffman_symbol reader dist in
          if ds >= 30 then fail "invalid PNG distance code";
          let distance = dist_base.(ds) + read_bits reader dist_extra.(ds) in
          if distance > !used then fail "PNG deflate distance precedes output";
          for _=1 to len do let v=Char.code (Bytes.get out (!used-distance)) in emit v done
        end else fail "invalid PNG length code"
      done
    end
  done;
  if (reader.bit + 7) / 8 <> reader.limit then fail "trailing PNG deflate data";
  if !used <> expected then fail "PNG decompressed size mismatch";
  let raw = Bytes.unsafe_to_string out in
  let expected_adler = Int64.to_int32 (Int64.of_int (be32 compressed (String.length compressed - 4))) in
  if expected_adler <> adler32 raw then fail "PNG zlib checksum mismatch";
  raw

let decode_png png =
  let width, height = validate_png_header png in
  let length=String.length png and pos=ref 8 and idat=Buffer.create 4096 and ended=ref false in
  let depth=ref 0 and color=ref 0 and interlace=ref 0 and saw_header=ref false and saw_data=ref false in
  while !pos + 12 <= length && not !ended do
    let n=be32 png !pos in
    if n < 0 || n > max_png_bytes || !pos > length - n - 12 then fail "truncated PNG chunk";
    let typ=String.sub png (!pos+4) 4 and data_pos= !pos+8 in
    if Int32.of_int (be32 png (!pos+8+n)) <> crc32 png (!pos+4) (n+4) then fail "PNG chunk checksum mismatch";
    if typ="IHDR" then begin
      if !saw_header || n<>13 then fail "invalid PNG header";
      saw_header:=true; depth:=Char.code png.[data_pos+8]; color:=Char.code png.[data_pos+9];
      if Char.code png.[data_pos+10]<>0 || Char.code png.[data_pos+11]<>0 then fail "unsupported PNG compression or filter";
      interlace:=Char.code png.[data_pos+12]
    end else if typ="IDAT" then begin
      if not !saw_header || !ended then fail "misordered PNG image data";
      saw_data:=true; Buffer.add_substring idat png data_pos n
    end else if typ="IEND" then (if n<>0 || not !saw_data then fail "invalid PNG end"; ended:=true)
    else if typ="tRNS" then fail "unsupported PNG transparency chunk"
    else if String.length typ=4 && Char.code typ.[0] land 32=0 then fail "unsupported critical PNG chunk";
    pos:= !pos+n+12
  done;
  if not !ended || !pos <> length || not !saw_header then fail "incomplete PNG capture";
  if !depth<>8 || !interlace<>0 || not (List.mem !color [0;2;4;6]) then fail "unsupported PNG pixel format";
  let channels=match !color with 0->1|2->3|4->2|_->4 in
  let stride=width*channels in
  let raw=inflate_zlib (Buffer.contents idat) ((stride+1)*height) in
  let rgba=Bytes.create (width*height*4) in
  let previous=ref (Bytes.make stride '\000') in
  let current=ref (Bytes.create stride) in
  for y=0 to height-1 do
    let filter=Char.code raw.[y*(stride+1)] in
    if filter>4 then fail "invalid PNG row filter";
    for x=0 to stride-1 do
      let v=Char.code raw.[y*(stride+1)+1+x] in
      let left=if x>=channels then Char.code (Bytes.get !current (x-channels)) else 0 in
      let up=Char.code (Bytes.get !previous x) in
      let ul=if x>=channels then Char.code (Bytes.get !previous (x-channels)) else 0 in
      let paeth a b c =
        let p=a+b-c in
        let pa=abs (p-a) and pb=abs (p-b) and pc=abs (p-c) in
        if pa<=pb && pa<=pc then a else if pb<=pc then b else c in
      let predictor=match filter with 0->0|1->left|2->up|3->(left+up)/2|_->paeth left up ul in
      Bytes.set !current x (Char.chr ((v+predictor) land 255))
    done;
    for x=0 to width-1 do
      let src=x*channels and dst=(y*width+x)*4 in
      let c i=Char.code (Bytes.get !current (src+i)) in
      let r,g,b,a=match !color with 0->let q=c 0 in q,q,q,255|2->c 0,c 1,c 2,255|4->let q=c 0 in q,q,q,c 1|_->c 0,c 1,c 2,c 3 in
      Bytes.set rgba dst (byte r); Bytes.set rgba (dst+1) (byte g); Bytes.set rgba (dst+2) (byte b); Bytes.set rgba (dst+3) (byte a)
    done;
    let row = !previous in
    previous := !current;
    current := row
  done;
  {width;height;rgba=Bytes.unsafe_to_string rgba}

let png_chunk typ data =
  let n=String.length data in
  let b=Buffer.create (n+12) in
  let add32 x=Buffer.add_char b (Char.chr ((x lsr 24) land 255)); Buffer.add_char b (Char.chr ((x lsr 16) land 255)); Buffer.add_char b (Char.chr ((x lsr 8) land 255)); Buffer.add_char b (Char.chr (x land 255)) in
  add32 n; Buffer.add_string b typ; Buffer.add_string b data;
  add32 (Int32.to_int (crc32 (typ^data) 0 (n+4))); Buffer.contents b

let encode_png pixels =
  validate_dimensions pixels.width pixels.height;
  if String.length pixels.rgba <> pixels.width * pixels.height * 4 then fail "invalid pixel buffer";
  let w=pixels.width and h=pixels.height in
  let raw=Bytes.create ((w*4+1)*h) in
  for y=0 to h-1 do Bytes.set raw (y*(w*4+1)) '\000'; Bytes.blit_string pixels.rgba (y*w*4) raw (y*(w*4+1)+1) (w*4) done;
  let raw=Bytes.unsafe_to_string raw and compressed=Buffer.create 1024 in
  Buffer.add_string compressed "\120\001";
  let pending=ref 0 and bits=ref 0 in
  let write_bits value count =
    for i=0 to count-1 do
      pending := !pending lor (((value lsr i) land 1) lsl !bits);
      incr bits;
      if !bits=8 then (Buffer.add_char compressed (Char.chr !pending); pending:=0; bits:=0)
    done in
  let emit_symbol symbol =
    let code,length =
      if symbol<=143 then 0x30+symbol,8
      else if symbol<=255 then 0x190+symbol-144,9
      else if symbol<=279 then symbol-256,7
      else 0xc0+symbol-280,8 in
    write_bits (reverse_bits code length) length in
  write_bits 1 1; write_bits 1 2;
  let length_base=[|3;4;5;6;7;8;9;10;11;13;15;17;19;23;27;31;35;43;51;59;67;83;99;115;131;163;195;227;258|] in
  let length_extra=[|0;0;0;0;0;0;0;0;1;1;1;1;2;2;2;2;3;3;3;3;4;4;4;4;5;5;5;5;0|] in
  let emit_run length =
    let index=ref 0 in
    while !index<28 && length>length_base.(!index)+(1 lsl length_extra.(!index))-1 do incr index done;
    emit_symbol (257+ !index);
    write_bits (length-length_base.(!index)) length_extra.(!index);
    write_bits 0 5 in
  let p=ref 0 in
  while !p<String.length raw do
    let run=ref 1 in
    while !run<258 && !p+ !run<String.length raw && raw.[!p+ !run]=raw.[!p] do incr run done;
    if !p>0 && raw.[!p]=raw.[!p-1] && !run>=3 then (emit_run !run; p:= !p+ !run)
    else (emit_symbol (Char.code raw.[!p]); incr p)
  done;
  emit_symbol 256;
  if !bits>0 then Buffer.add_char compressed (Char.chr !pending);
  let a=adler32 raw in
  let out=Buffer.create 1024 in
  Buffer.add_string out "\137PNG\r\n\026\n";
  let ihdr=Bytes.make 13 '\000' in
  let set32 i v=for j=0 to 3 do Bytes.set ihdr (i+j) (Char.chr ((v lsr (24-8*j)) land 255)) done in
  set32 0 w; set32 4 h; Bytes.set ihdr 8 '\008'; Bytes.set ihdr 9 '\006';
  Buffer.add_string out (png_chunk "IHDR" (Bytes.unsafe_to_string ihdr));
  let z=Buffer.contents compressed in
  let z=z ^ String.init 4 (fun i->Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical a (24-8*i)) 255l))) in
  Buffer.add_string out (png_chunk "IDAT" z); Buffer.add_string out (png_chunk "IEND" ""); Buffer.contents out
let metadata_json m = `Assoc ["app", `String m.app; "platform", `String m.platform;
  "device", `String m.device; "build_sha256", `String m.build_hash;
  "os", `String m.os; "locale", `String m.locale;
  "theme", `String m.theme; "width", `Int m.width; "height", `Int m.height;
  "masks", `List (List.map (fun r -> `Assoc ["x", `Int r.x; "y", `Int r.y; "width", `Int r.width; "height", `Int r.height]) m.masks)]
let metadata_of_json json =
  let field key = Yojson.Basic.Util.member key json in
  let string key = Yojson.Basic.Util.to_string (field key) and integer key = Yojson.Basic.Util.to_int (field key) in
  let masks = Yojson.Basic.Util.to_list (field "masks") |> List.map (fun item ->
    let get key = Yojson.Basic.Util.to_int (Yojson.Basic.Util.member key item) in
    {x=get "x"; y=get "y"; width=get "width"; height=get "height"}) in
  {app=string "app"; platform=string "platform"; device=string "device";
   build_hash=string "build_sha256"; os=string "os"; locale=string "locale";
   theme=string "theme"; width=integer "width"; height=integer "height"; masks}

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
  let temp = Filename.temp_file ~temp_dir:(Filename.dirname path) ".vr02-" ".tmp" in
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
let pixel_changed threshold left right pos =
  let changed=ref false in
  for channel=0 to 3 do
    if abs (Char.code left.rgba.[pos+channel] - Char.code right.rgba.[pos+channel]) > threshold then changed:=true
  done;
  !changed

let compare ~workspace ~name ~threshold ~max_differing_pixels capture =
  if max_differing_pixels < 0 then fail "maximum differing-pixel count must not be negative";
  if threshold < 0 || threshold > 255 then fail "pixel threshold must be between 0 and 255";
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
  let differing_pixels=ref 0 and first_difference=ref None in
  let difference = Bytes.make (width*height*4) '\000' and regions=ref [] in
  let add_region r = if List.length !regions < 128 then regions:=r::!regions in
  for y=0 to height-1 do
    let run=ref None in
    let finish x0 x1 = if x0 < x1 then add_region {x=x0;y;width=x1-x0;height=1} in
    for x=0 to width-1 do
      let pos=(y*width+x)*4 in
      let changed=not (masked stored.masks x y) && pixel_changed threshold previous current pos in
      if changed then begin
        incr differing_pixels;
        if !first_difference=None then first_difference:=Some (x,y);
        if !run=None then run:=Some x;
        Bytes.set difference pos '\255'; Bytes.set difference (pos+1) '\000';
        Bytes.set difference (pos+2) '\000'; Bytes.set difference (pos+3) '\255'
      end else match !run with None->()|Some first->finish first x; run:=None
    done;
    (match !run with None->()|Some first->finish first width)
  done;
  let difference_png=encode_png {width;height;rgba=Bytes.unsafe_to_string difference} in
  if String.length difference_png > max_png_bytes then fail "difference PNG exceeds artifact size limit";
  {comparison={equal = !differing_pixels <= max_differing_pixels;
    differing_pixels= !differing_pixels; first_difference= !first_difference};
   baseline_png;current_png=capture.png;difference_png;regions=List.rev !regions;
   settings_version=version;threshold;max_differing_pixels}
