module Visual = Pave.Workspace_mobile_visual

let expect label condition = if not condition then failwith label
let rejects label action =
  try ignore (action ()); failwith ("accepted " ^ label) with Visual.Error _ -> ()

let () =
  let root = Filename.temp_file "pave-mobile-visual-portable" ".dir" in
  Sys.remove root; Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () ->
    let rec remove path = if Sys.file_exists path then
      if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then begin
        Array.iter (fun entry -> remove (Filename.concat path entry)) (Sys.readdir path);
        Unix.rmdir path
      end else Unix.unlink path in
    remove root) (fun () ->
    let pixels rgba = {Visual.width=2; height=1; rgba} in
    let black = pixels "\000\000\000\255\000\000\000\255" in
    let red = pixels "\000\000\000\255\255\000\000\255" in
    let original_png = Visual.encode_png black in
    let changed_png = Visual.encode_png red in
    let metadata = {Visual.app="dev.example.app"; platform="android"; device="test-device";
      build_hash=String.make 64 'a'; os="Android test";
      locale="en-US"; theme="light"; width=2; height=1; masks=[]} in
    let baseline = {Visual.png=original_png; complete=true; metadata} in
    Visual.save ~workspace:root ~name:"screen" baseline;
    let same = Visual.compare ~workspace:root ~name:"screen" ~threshold:0 ~max_differing_pixels:0 baseline in
    expect "same image compares consistently" same.comparison.equal;
    expect "exact baseline bytes returned" (same.baseline_png = original_png);
    expect "exact current bytes returned" (same.current_png = original_png);
    expect "empty diff artifact decodes" ((Visual.decode_png same.difference_png).width = 2);
    let different = Visual.compare ~workspace:root ~name:"screen" ~threshold:0 ~max_differing_pixels:0
        {baseline with Visual.png=changed_png} in
    expect "different image detected" (not different.comparison.equal && different.comparison.differing_pixels=1);
    expect "difference regions bounded and informative"
      (different.regions = [{Visual.x=1; y=0; width=1; height=1}]);
    expect "difference image has marked pixel"
      ((Visual.decode_png different.difference_png).rgba = "\000\000\000\000\255\000\000\255");
    let tolerated = Visual.compare ~workspace:root ~name:"screen" ~threshold:255 ~max_differing_pixels:1
        {baseline with Visual.png=changed_png} in
    expect "explicit tolerance honored" tolerated.comparison.equal;
    let wide_meta={metadata with Visual.width=257} in
    let wide_black=Bytes.make (257*4) '\000' in
    for x=0 to 256 do Bytes.set wide_black (x*4+3) '\255' done;
    let wide_red=Bytes.copy wide_black in
    for x=0 to 256 do if x mod 2=0 then Bytes.set wide_red (x*4) '\255' done;
    let wide_capture={Visual.png=Visual.encode_png {Visual.width=257; height=1; rgba=Bytes.unsafe_to_string wide_black}; complete=true; metadata=wide_meta} in
    Visual.save ~workspace:root ~name:"wide" wide_capture;
    let wide_diff=Visual.compare ~workspace:root ~name:"wide" ~threshold:0 ~max_differing_pixels:0
        {wide_capture with Visual.png=Visual.encode_png {Visual.width=257; height=1; rgba=Bytes.unsafe_to_string wide_red}} in
    expect "differing-region report is bounded" (wide_diff.comparison.differing_pixels=129 && List.length wide_diff.regions=128);
    let changed_record = Filename.concat root ".pave/mobile-baselines/screen.json" in
    let read path = let ch=open_in_bin path in Fun.protect ~finally:(fun ()->close_in_noerr ch) (fun ()->really_input_string ch (in_channel_length ch)) in
    let baseline_path = Filename.concat root ".pave/mobile-baselines/screen.png" in
    expect "comparison never mutates baseline" (read baseline_path = original_png);
    rejects "dimension mismatch" (fun () -> Visual.compare ~workspace:root ~name:"screen" ~threshold:0 ~max_differing_pixels:0
      {baseline with metadata={metadata with Visual.width=1}});
    rejects "negative tolerance" (fun () -> Visual.compare ~workspace:root ~name:"screen" ~threshold:(-1) ~max_differing_pixels:0 baseline);
    rejects "threshold above channel range" (fun () -> Visual.compare ~workspace:root ~name:"screen" ~threshold:256 ~max_differing_pixels:0 baseline);
    rejects "negative maximum difference count" (fun () -> Visual.compare ~workspace:root ~name:"screen" ~threshold:0 ~max_differing_pixels:(-1) baseline);
    let compressed_png =
      "\137PNG\r\n\026\n\000\000\000\rIHDR\000\000\000\001\000\000\000\001\008\004\000\000\000\181\028\012\002\000\000\000\011IDATx\218c\252\255\031\000\003\003\002\000\239\162\167\091\000\000\000\000IEND\174B`\130" in
    let compressed_image=Visual.decode_png compressed_png in
    expect "normal fixed-Huffman PNG decodes" (compressed_image.width=1 && String.length compressed_image.rgba=4);
    let paeth_png =
      "\137\080\078\071\013\010\026\010\000\000\000\013\073\072\068\082\000\000\000\002\000\000\000\001\008\000\000\000\000\209\073\032\086\000\000\000\011\073\068\065\084\120\156\099\225\226\002\000\000\045\000\025\084\194\022\075\000\000\000\000\073\069\078\068\174\066\096\130" in
    expect "Paeth filter uses the previous row's upper-left sample"
      ((Visual.decode_png paeth_png).rgba =
       "\010\010\010\255\020\020\020\255");
    rejects "corrupt PNG trailer" (fun () -> Visual.decode_png (original_png ^ "extra"));
    let bad_crc=Bytes.of_string original_png in
    let last=Bytes.length bad_crc-1 in Bytes.set bad_crc last (Char.chr (Char.code (Bytes.get bad_crc last) lxor 1));
    rejects "corrupt PNG checksum" (fun () -> Visual.decode_png (Bytes.unsafe_to_string bad_crc));
    let oversized=Bytes.make 41 '\000' in
    Bytes.blit_string "\137PNG\r\n\026\n" 0 oversized 0 8;
    Bytes.blit_string "\000\000\000\rIHDR" 0 oversized 8 8;
    Bytes.blit_string "\127\255\255\255" 0 oversized 16 4;
    Bytes.blit_string "\000\000\000\001" 0 oversized 20 4;
    Bytes.blit_string "IEND" 0 oversized 33 4;
    rejects "oversized PNG dimensions" (fun () -> Visual.decode_png (Bytes.unsafe_to_string oversized));
    let out=open_out_bin changed_record in output_string out "{\"version\":1}"; close_out out;
    rejects "stale baseline version fails closed" (fun () -> Visual.compare ~workspace:root ~name:"screen" ~threshold:0 ~max_differing_pixels:0 baseline);
    let out=open_out_bin changed_record in output_string out "{"; close_out out;
    rejects "corrupt baseline record fails closed" (fun () -> Visual.compare ~workspace:root ~name:"screen" ~threshold:0 ~max_differing_pixels:0 baseline));
  print_endline "portable mobile visual comparisons: ok"
