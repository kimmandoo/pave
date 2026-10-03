module R = Pave.Session_recording

let temp_path name =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "pave-recording-%d-%s" (Unix.getpid ()) name)

let with_temp_file name f =
  let path = temp_path name in
  Fun.protect ~finally:(fun () -> try Unix.unlink path with _ -> ())
    (fun () -> f path)

let write_recording path frames =
  let oc = open_out_bin path in
  let recorder = R.create_recorder oc in
  List.iter (fun (direction, kind, data) -> R.record recorder ~direction ~kind data)
    frames;
  R.close_recorder recorder;
  close_out oc

let read_recording path : R.frame list =
  let ic = open_in_bin path in
  let player = R.open_player ic in
  let rec drain acc =
    match R.next player with
    | None -> close_in ic; List.rev acc
    | Some frame -> drain (frame :: acc) in
  drain []

let round_trip () =
  let inputs = List.init 500 (fun i ->
    ((if i mod 2 = 0 then `Input else `Output),
     "event_" ^ string_of_int (i mod 7),
     `Assoc [("i", `Int i); ("text", `String (Printf.sprintf "frame-%d" i))])) in
  with_temp_file "roundtrip.jsonl" (fun path ->
    write_recording path inputs;
    let frames = read_recording path in
    assert (List.length frames = List.length inputs);
    List.iteri (fun i (frame : R.frame) ->
      let (direction, kind, data) = List.nth inputs i in
      assert (frame.R.seq = i + 1);
      assert (frame.R.direction = direction);
      assert (frame.R.kind = kind);
      assert (frame.R.data = data)) frames;
    let rec nondecreasing = function
      | a :: (b :: _ as rest) ->
          assert (a.R.at_ms <= b.R.at_ms);
          nondecreasing rest
      | _ -> () in
    nondecreasing frames)

let first_frame_timestamps () =
  with_temp_file "first.jsonl" (fun path ->
    write_recording path [(`Input, "kind", `Null); (`Output, "kind", `Int 1)];
    match read_recording path with
    | [a; b] ->
        assert (a.R.seq = 1 && b.R.seq = 2);
        assert (a.R.at_ms >= 0 && a.R.at_ms < 60_000);
        assert (b.R.at_ms >= a.R.at_ms)
    | _ -> failwith "expected two frames")

let contains hay needle =
  let h = String.length hay and n = String.length needle in
  let rec scan i =
    if i + n > h then false
    else if String.sub hay i n = needle then true
    else scan (i + 1) in
  scan 0

let malformed_line_rejected () =
  let good = "{\"seq\":1,\"at_ms\":0,\"direction\":\"input\",\"kind\":\"k\",\"data\":null}" in
  (match R.frames_of_string (good ^ "\nnot json\n") with
   | exception Invalid_argument msg ->
       assert (contains msg "line 2")
   | _ -> failwith "malformed line was accepted");
  (match R.frames_of_string (good ^ "\n{\"seq\":3,\"at_ms\":0,\"direction\":\"input\",\"kind\":\"k\",\"data\":null}\n") with
   | exception Invalid_argument msg -> assert (contains msg "line 2")
   | _ -> failwith "seq gap was accepted");
  (match R.frames_of_string (good ^ "\n{\"seq\":2,\"at_ms\":0,\"direction\":\"bogus\",\"kind\":\"k\",\"data\":null}\n") with
   | exception Invalid_argument _ -> ()
   | _ -> failwith "bad direction was accepted");
  (match R.frames_of_string "{\"seq\":1,\"at_ms\":0,\"direction\":\"input\",\"kind\":\"Bad Kind\",\"data\":null}\n" with
   | exception Invalid_argument _ -> ()
   | _ -> failwith "bad kind charset was accepted");
  (match R.frames_of_string "{\"seq\":1,\"at_ms\":5,\"direction\":\"input\",\"kind\":\"k\",\"data\":null}\n{\"seq\":2,\"at_ms\":3,\"direction\":\"output\",\"kind\":\"k\",\"data\":null}\n" with
   | exception Invalid_argument _ -> ()
   | _ -> failwith "non-monotone at_ms was accepted");
  (match R.frames_of_string "{\"seq\":1,\"at_ms\":0,\"direction\":\"input\",\"kind\":\"k\",\"data\":null,\"extra\":1}\n" with
   | exception Invalid_argument _ -> ()
   | _ -> failwith "extra field was accepted")

let oversize_rejected () =
  with_temp_file "oversize.jsonl" (fun path ->
    let oc = open_out_bin path in
    let recorder = R.create_recorder oc in
    (* serialized `String of max_data_bytes 'x' adds 2 quote bytes *)
    let big = `String (String.make R.max_data_bytes 'x') in
    (match R.record recorder ~direction:`Input ~kind:"k" big with
     | exception Invalid_argument _ -> ()
     | _ -> failwith "oversize frame data was accepted");
    R.close_recorder recorder;
    close_out oc);
  (* oversize line rejected on read *)
  with_temp_file "oversize-line.jsonl" (fun path ->
    let oc = open_out_bin path in
    output_string oc
      ("{\"seq\":1,\"at_ms\":0,\"direction\":\"input\",\"kind\":\"k\",\"data\":\""
       ^ String.make (R.max_data_bytes + 2_000) 'x' ^ "\"}\n");
    close_out oc;
    let ic = open_in_bin path in
    let player = R.open_player ic in
    (match R.next player with
     | exception Invalid_argument _ -> ()
     | Some _ -> failwith "oversize line was accepted"
     | None -> failwith "oversize line silently ended stream");
    close_in ic)

let playback_data_bound () =
  let frame data =
    Yojson.Basic.to_string (`Assoc [
      "seq", `Int 1; "at_ms", `Int 0; "direction", `String "input";
      "kind", `String "k"; "data", data]) ^ "\n" in
  let reject text =
    match R.frames_of_string text with
    | exception Invalid_argument _ -> ()
    | _ -> failwith "playback accepted data larger than the writer's limit" in
  let oversize = frame (`String (String.make (R.max_data_bytes - 1) 'x')) in
  assert (String.length oversize < R.max_line_bytes);
  reject oversize;
  let boundary = frame (`String (String.make (R.max_data_bytes - 2) 'x')) in
  assert (List.length (R.frames_of_string boundary) = 1);
  with_temp_file "playback-data.jsonl" (fun path ->
    let out = open_out_bin path in
    output_string out oversize; close_out out;
    let input = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in input) (fun () ->
      match R.next (R.open_player input) with
      | exception Invalid_argument _ -> ()
      | _ -> failwith "channel playback accepted oversized data"))

let writer_frame_bound () =
  with_temp_file "frame-count.jsonl" (fun path ->
    let recorder = R.create_recorder (open_out_bin path) in
    Fun.protect ~finally:(fun () -> R.close_recorder recorder) (fun () ->
      recorder.R.seq <- R.max_frames;
      (match R.record recorder ~direction:`Input ~kind:"k" `Null with
       | exception Invalid_argument _ -> ()
       | _ -> failwith "writer produced an unplayable over-quota frame");
      assert ((Unix.stat path).Unix.st_size = 0)))

let failed_write_stops_stream () =
  with_temp_file "failed-write.jsonl" (fun path ->
    close_out (open_out_bin path);
    let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
    let recorder = R.create_recorder (Unix.out_channel_of_descr fd) in
    Fun.protect ~finally:(fun () -> R.close_recorder recorder) (fun () ->
      (match R.record recorder ~direction:`Output ~kind:"k" `Null with
       | exception (Sys_error _ | Unix.Unix_error _) -> ()
       | _ -> failwith "writing to a read-only recording channel unexpectedly succeeded");
      (match R.record recorder ~direction:`Output ~kind:"k" `Null with
       | exception Invalid_argument _ -> ()
       | _ -> failwith "recorder reused a stream after a failed frame write")))

let record_after_close_raises () =
  with_temp_file "closed.jsonl" (fun path ->
    let oc = open_out_bin path in
    let recorder = R.create_recorder oc in
    R.close_recorder recorder;
    (match R.record recorder ~direction:`Output ~kind:"k" `Null with
     | exception Invalid_argument _ -> ()
     | _ -> failwith "record after close was accepted");
    close_out oc)

let invalid_kind_rejected () =
  with_temp_file "kind.jsonl" (fun path ->
    let oc = open_out_bin path in
    let recorder = R.create_recorder oc in
    let reject kind =
      match R.record recorder ~direction:`Input ~kind `Null with
      | exception Invalid_argument _ -> ()
      | _ -> failwith ("invalid kind accepted: " ^ kind) in
    reject "";
    reject "Has Upper";
    reject "with space";
    reject "with.dot";
    reject (String.make (R.max_kind_chars + 1) 'a');
    (* boundary: exactly max_kind_chars is accepted *)
    R.record recorder ~direction:`Input ~kind:(String.make R.max_kind_chars 'a') `Null;
    R.close_recorder recorder;
    close_out oc)

let replay_drains_in_order () =
  let inputs = List.init 100 (fun i ->
    ((if i mod 3 = 0 then `Input else `Output), "k", `Int i)) in
  with_temp_file "replay.jsonl" (fun path ->
    write_recording path inputs;
    let ic = open_in_bin path in
    let seen = ref [] in
    R.replay (R.open_player ic)
      ~on_input:(fun f -> seen := (1, f.R.seq) :: !seen)
      ~on_output:(fun f -> seen := (2, f.R.seq) :: !seen);
    close_in ic;
    let expected = List.mapi (fun i (d, _, _) ->
      (if d = `Input then 1 else 2), i + 1) inputs in
    assert (List.rev !seen = expected))

let frames_of_string_round_trip () =
  let text = String.concat "" [
    "{\"seq\":1,\"at_ms\":0,\"direction\":\"input\",\"kind\":\"a\",\"data\":{\"x\":1}}\n";
    "{\"seq\":2,\"at_ms\":2,\"direction\":\"output\",\"kind\":\"b\",\"data\":[1,2,3]}\n";
  ] in
  match (R.frames_of_string text : R.frame list) with
  | [a; b] ->
      assert (a.R.seq = 1 && a.R.direction = `Input && a.R.kind = "a"
              && a.R.data = `Assoc [("x", `Int 1)]);
      assert (b.R.seq = 2 && b.R.at_ms = 2 && b.R.direction = `Output
              && b.R.data = `List [`Int 1; `Int 2; `Int 3])
  | _ -> failwith "frames_of_string returned wrong frame count"

let () =
  round_trip ();
  first_frame_timestamps ();
  malformed_line_rejected ();
  oversize_rejected ();
  playback_data_bound ();
  writer_frame_bound ();
  failed_write_stops_stream ();
  record_after_close_raises ();
  invalid_kind_rejected ();
  replay_drains_in_order ();
  frames_of_string_round_trip ();
  print_endline "test_session_recording: ok"
