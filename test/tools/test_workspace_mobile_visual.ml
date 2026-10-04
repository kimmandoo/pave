module Visual = Pave.Workspace_mobile_visual

let expect label condition = if not condition then failwith label
let rejects label action = try ignore (action ()); failwith ("accepted " ^ label) with Visual.Error _ -> ()

let bmp width height pixels =
  let row = ((width * 3 + 3) / 4) * 4 in
  let bytes = Bytes.make (54 + row * height) '\000' in
  let put16 offset value = Bytes.set bytes offset (Char.chr (value land 255)); Bytes.set bytes (offset + 1) (Char.chr ((value lsr 8) land 255)) in
  let put32 offset value = for i = 0 to 3 do Bytes.set bytes (offset + i) (Char.chr ((value lsr (8 * i)) land 255)) done in
  Bytes.blit_string "BM" 0 bytes 0 2; put32 2 (Bytes.length bytes); put32 10 54;
  put32 14 40; put32 18 width; put32 22 height; put16 26 1; put16 28 24;
  put32 34 (row * height);
  for y = 0 to height - 1 do for x = 0 to width - 1 do
    let r, g, b = pixels.(y * width + x) in
    let pos = 54 + (height - y - 1) * row + x * 3 in
    Bytes.set bytes pos (Char.chr b); Bytes.set bytes (pos + 1) (Char.chr g); Bytes.set bytes (pos + 2) (Char.chr r)
  done done;
  Bytes.unsafe_to_string bytes

let () =
  let original = Visual.decode_bmp (bmp 2 1 [|0,0,0; 0,0,0|]) in
  let changed = Visual.decode_bmp (bmp 2 1 [|0,0,0; 255,0,0|]) in
  let same = Visual.compare_pixels ~threshold:0 ~masks:[] original original in
  expect "unchanged pixels compare equal" (same.equal && same.differing_pixels = 0 && same.first_difference = None);
  let diff = Visual.compare_pixels ~threshold:0 ~masks:[] original changed in
  expect "outside-mask pixel change found" (not diff.equal && diff.differing_pixels = 1 && diff.first_difference = Some (1, 0));
  let ignored = Visual.compare_pixels ~threshold:0 ~masks:[{Visual.x=1; y=0; width=1; height=1}] original changed in
  expect "inside-mask pixel change ignored" (ignored.equal && ignored.differing_pixels = 0);
  rejects "truncated BMP" (fun () -> Visual.decode_bmp "BM\000");
  rejects "truncated BMP pixels" (fun () -> Visual.decode_bmp (String.sub (bmp 2 1 [|0,0,0; 0,0,0|]) 0 55));
  rejects "malformed PNG" (fun () -> Visual.validate_png_header "not a png");
  let meta = {Visual.app="dev.example.app"; platform="android"; device="serial-1";
    build_hash=String.make 64 'a'; os="Android 15";
    locale="en-US"; theme="dark"; width=1; height=1; masks=[]} in
  let check_bad_masks masks = rejects "invalid mask" (fun () -> Visual.validate_metadata {meta with masks}) in
  check_bad_masks [{Visual.x=(-1); y=0; width=1; height=1}];
  check_bad_masks [{Visual.x=0; y=0; width=0; height=1}];
  check_bad_masks [{Visual.x=1; y=0; width=1; height=1}];
  rejects "incomplete capture" (fun () -> Visual.save ~workspace:"/tmp" ~name:"baseline" {png=""; complete=false; metadata=meta});
  rejects "path traversal key" (fun () -> Visual.validate_key "../escape");
  rejects "oversized baseline name" (fun () -> Visual.validate_key (String.make 81 'x'));
  rejects "truncated PNG" (fun () ->
    Visual.validate_png_header ("\137PNG\r\n\026\n" ^ String.make 25 '\000'));
  rejects "oversized dimensions" (fun () -> Visual.validate_dimensions 16_385 1);
  let root = Filename.temp_file "pave-vr01-test" ".dir" in
  Sys.remove root; Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () ->
    let rec remove path = if Sys.file_exists path then
      if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (Array.iter (fun entry -> remove (Filename.concat path entry)) (Sys.readdir path); Unix.rmdir path)
      else Unix.unlink path in remove root) (fun () ->
    (* PNG storage uses the fixed macOS ImageIO decoder; pure BMP/pixel tests above are portable. *)
    if Sys.os_type = "Unix" && Sys.file_exists "/usr/bin/swift" then begin
      let png = "\137PNG\r\n\026\n\000\000\000\rIHDR\000\000\000\001\000\000\000\001\008\004\000\000\000\181\028\012\002\000\000\000\011IDATx\218c\252\255\031\000\003\003\002\000\239\162\167\091\000\000\000\000IEND\174B`\130" in
      let capture = {Visual.png; complete=true; metadata=meta} in
      Visual.save ~workspace:root ~name:"home" capture;
      let base = Filename.concat root ".pave/mobile-baselines" in
      let same = Visual.compare ~workspace:root ~name:"home" ~threshold:0 ~max_differing_pixels:0 capture in
      expect "unchanged saved capture equal" same.comparison.equal;
      let altered update = Visual.compare ~workspace:root ~name:"home" ~threshold:0 ~max_differing_pixels:0 {capture with metadata=update meta} in
      List.iter (fun update -> rejects "metadata mismatch" (fun () -> altered update))
        [ (fun m -> {m with Visual.app="other.app"}); (fun m -> {m with Visual.platform="ios"});
          (fun m -> {m with Visual.device="other-device"});
          (fun m -> {m with Visual.build_hash=String.make 64 'b'});
          (fun m -> {m with Visual.os="Android 16"});
          (fun m -> {m with Visual.locale="fr-FR"}); (fun m -> {m with Visual.theme="light"}) ];
      let record = Filename.concat base "home.json" and image = Filename.concat base "home.png" in
      Unix.unlink image;
      let external_image = Filename.concat root "outside.png" in
      let out = open_out_bin external_image in output_string out png; close_out out;
      Unix.symlink external_image image;
      rejects "baseline image symlink" (fun () -> Visual.compare ~workspace:root ~name:"home" ~threshold:0 ~max_differing_pixels:0 capture);
      Unix.unlink image; Unix.unlink external_image;
      let moved = base ^ "-moved" in
      Unix.rename base moved;
      Unix.symlink moved base;
      rejects "baseline directory symlink" (fun () -> Visual.compare ~workspace:root ~name:"home" ~threshold:0 ~max_differing_pixels:0 capture);
      Unix.unlink base;
      Unix.rename moved base;
      let out = open_out record in output_string out "{"; close_out out;
      rejects "corrupt baseline record" (fun () -> Visual.compare ~workspace:root ~name:"home" ~threshold:0 ~max_differing_pixels:0 capture)
    end);
  print_endline "workspace mobile visual baseline core: ok"
