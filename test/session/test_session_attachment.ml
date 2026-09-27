let child = Filename.concat

let rec remove path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter (fun name -> remove (child path name)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let write path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel contents)

let rejected action =
  match action () with
  | exception Invalid_argument _ | exception Pave.Tools.Tool_error _ -> ()
  | _ -> failwith "unsafe or invalid media attachment was accepted"


let () =
  let base = Filename.temp_file "pave-attachment-" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  Fun.protect ~finally:(fun () -> remove base) (fun () ->
    let root = child base "workspace" in
    let outside = child base "outside" in
    Unix.mkdir root 0o700;
    Unix.mkdir outside 0o700;
    let png = "\137PNG\r\n\026\n" in
    write (child root "sample.PNG") png;
    let attachment = Pave.Session_attachment.load ~root "sample.PNG" in
    assert (attachment.Pave.Protocol.name = "sample.PNG");
    assert (attachment.mime_type = "image/png");
    assert (attachment.data = "iVBORw0KGgo=");
    let jpeg = "\255\216\255\224" in
    write (child root "sample.jpg") jpeg;
    let jpeg_attachment = Pave.Session_attachment.load ~root "sample.jpg" in
    assert (jpeg_attachment.mime_type = "image/jpeg");
    let webp = "RIFF\000\000\000\000WEBP" in
    write (child root "sample.webp") webp;
    let webp_attachment = Pave.Session_attachment.load ~root "sample.webp" in
    assert (webp_attachment.mime_type = "image/webp");
    let wav = "RIFF\000\000\000\000WAVE" in
    write (child root "voice.wav") wav;
    let audio = Pave.Session_attachment.load ~root "voice.wav" in
    assert (audio.mime_type = "audio/wav");
    let mp3 = "ID3\004\000\000" in
    write (child root "voice.mp3") mp3;
    let mp3_attachment = Pave.Session_attachment.load ~root "voice.mp3" in
    assert (mp3_attachment.mime_type = "audio/mp3");
    let mp4 = "\000\000\000\012ftypisom" in
    write (child root "clip.mp4") mp4;
    let video = Pave.Session_attachment.load ~root "clip.mp4" in
    assert (video.mime_type = "video/mp4");
    write (child root "clip.webm") ("\026E\223\163" ^ String.make 8 '\000');
    let webm = Pave.Session_attachment.load ~root "clip.webm" in
    assert (webm.mime_type = "video/webm");

    write (child outside "outside.png") png;
    rejected (fun () -> Pave.Session_attachment.load ~root "../outside.png");
    rejected (fun () -> Pave.Session_attachment.load ~root (child root "sample.PNG"));
    Unix.symlink (child outside "outside.png") (child root "escape.png");
    rejected (fun () -> Pave.Session_attachment.load ~root "escape.png");
    write (child root "wrong.jpg") png;
    rejected (fun () -> Pave.Session_attachment.load ~root "wrong.jpg");
    let limit = Pave.Session_attachment.max_file_bytes in
    write (child root "boundary.png")
      (png ^ String.make (limit - String.length png) 'x');
    let boundary = Pave.Session_attachment.load ~root "boundary.png" in
    assert (String.length boundary.data = ((limit + 2) / 3) * 4);
    write (child root "large.png")
      (png ^ String.make (limit - String.length png + 1) 'x');
    rejected (fun () -> Pave.Session_attachment.load ~root "large.png"));
  print_endline "workspace media attachments: ok"
