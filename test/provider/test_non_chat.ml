module Task = Pave.Non_chat

let assert_true label condition =
  if not condition then failwith ("assertion failed: " ^ label)

let assert_equal label expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "assertion failed: %s (expected %S, got %S)" label expected actual)

let success body request =
  let requests = ref [] in
  let http received = requests := received :: !requests; Ok (200, body) in
  let result = request http in
  result, List.rev !requests

let request_url label expected request =
  assert_equal (label ^ " fixed HTTPS URL") expected request.Task.url;
  assert_equal (label ^ " method") "POST" request.Task.method_;
  assert_true (label ^ " bearer header")
    (List.mem ("Authorization", "Bearer test-secret") request.Task.headers)

let contains text needle =
  try ignore (Str.search_forward (Str.regexp_string needle) text 0); true
  with Not_found -> false

let request_body_contains label needle request =
  assert_true label (contains request.Task.body needle)

let expect_invalid label = function
  | Error (Task.Invalid_input _) | Error (Task.Invalid_credential _) | Error (Task.Missing_credential _) -> ()
  | Error error -> failwith (label ^ ": expected input error, got " ^ Task.message error)
  | Ok _ -> failwith (label ^ ": expected an input error")

let expect_invalid_response label = function
  | Error (Task.Invalid_response _) -> ()
  | Error error -> failwith (label ^ ": expected invalid response, got " ^ Task.message error)
  | Ok _ -> failwith (label ^ ": expected invalid response")

let expect_http_error label code = function
  | Error (Task.Http_error actual) when actual = code -> ()
  | Error error -> failwith (label ^ ": expected HTTP error, got " ^ Task.message error)
  | Ok _ -> failwith (label ^ ": expected HTTP error")

let with_workspace f =
  let root = Filename.temp_file "pave-non-chat-" ".workspace" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () ->
    Array.iter (fun name ->
      let path = Filename.concat root name in
      try
        let stat = Unix.lstat path in
        if stat.Unix.st_kind = Unix.S_DIR then Unix.rmdir path else Sys.remove path
      with _ -> ()) (Sys.readdir root);
    try Unix.rmdir root with _ -> ())
    (fun () -> f root)

let write_file path contents =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output contents)

let read_file path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let bytes_of_hex hex =
  let nibble = function
    | '0'..'9' as c -> Char.code c - Char.code '0'
    | 'a'..'f' as c -> Char.code c - Char.code 'a' + 10
    | _ -> failwith "invalid embedded hex fixture" in
  String.init (String.length hex / 2) (fun index ->
    Char.chr ((nibble hex.[index * 2] lsl 4) lor nibble hex.[index * 2 + 1]))

let png_fixture () =
  bytes_of_hex
    "89504e470d0a1a0a0000000d4948445200000001000000010804000000b51c0c02\
     0000000b4944415478da63fcff1f0003030200efa2a75b0000000049454e44ae426082"


let base64 bytes =
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let output = Buffer.create ((String.length bytes + 2) / 3 * 4) in
  let rec loop offset =
    if offset < String.length bytes then (
      let remaining = String.length bytes - offset in
      let a = Char.code bytes.[offset] in
      let b = if remaining > 1 then Char.code bytes.[offset + 1] else 0 in
      let c = if remaining > 2 then Char.code bytes.[offset + 2] else 0 in
      Buffer.add_char output alphabet.[a lsr 2];
      Buffer.add_char output alphabet.[((a land 3) lsl 4) lor (b lsr 4)];
      Buffer.add_char output (if remaining > 1 then alphabet.[((b land 15) lsl 2) lor (c lsr 6)] else '=');
      Buffer.add_char output (if remaining > 2 then alphabet.[c land 63] else '=');
      loop (offset + 3)) in
  loop 0;
  Buffer.contents output

let le16 number = String.init 2 (fun i -> Char.chr ((number lsr (8 * i)) land 255))
let le32 number = String.init 4 (fun i -> Char.chr ((number lsr (8 * i)) land 255))

let wav_fixture () =
  let fmt = "fmt " ^ le32 16 ^ le16 1 ^ le16 1 ^ le32 8000 ^ le32 8000 ^ le16 1 ^ le16 8 in
  let data = "data" ^ le32 2 ^ "\000\000" in
  let content = "WAVE" ^ fmt ^ data in
  "RIFF" ^ le32 (String.length content) ^ content

let embed_response model =
  Printf.sprintf
    "{\"object\":\"list\",\"data\":[{\"object\":\"embedding\",\"embedding\":[0.25,-0.5],\"index\":0}],\"model\":%s,\"usage\":{\"prompt_tokens\":2,\"total_tokens\":2}}"
    (Yojson.Basic.to_string (`String model))

let rerank_response =
  "{\"results\":[{\"index\":1,\"relevance_score\":0.9},{\"index\":0,\"relevance_score\":0.25}],\"id\":\"r1\"}"

let test_embed () =
  let result, requests = success (embed_response "embed/model-v3") (fun http ->
    Task.embed ~http ~key:"test-secret" ~model:"embed/model-v3" ~input:"exact input" ()) in
  (match result with Ok json ->
     assert_true "embedding JSON output" (String.length json > 10 && json.[0] = '{')
   | Error error -> failwith (Task.message error));
  (match requests with
   | [request] ->
       request_url "embedding" Task.embeddings_url request;
       request_body_contains "exact model ID sent" "embed/model-v3" request;
       request_body_contains "float encoding requested" "float" request;
       request_body_contains "input sent unchanged" "exact input" request
   | _ -> failwith "embedding made an unexpected number of requests");
  let large_vector = String.concat "," (List.init 3072 (fun _ -> "0.25")) in
  let large_response =
    "{\"object\":\"list\",\"model\":\"text-embedding-3-large\",\"usage\":{\"prompt_tokens\":1,\"total_tokens\":1},\"data\":[{\"object\":\"embedding\",\"index\":0,\"embedding\":[" ^
    large_vector ^ "]}]}" in
  let large_embedding = Task.embed ~http:(fun _ -> Ok (200, large_response))
      ~key:"test-secret" ~model:"text-embedding-3-large" ~input:"x" () in
  (match large_embedding with
   | Ok _ -> ()
   | Error error -> failwith ("valid 3072-dimension embedding: " ^ Task.message error));
  let malformed = Task.embed ~http:(fun _ -> Ok (200, "{\"data\":[]}"))
      ~key:"test-secret" ~model:"m" ~input:"text" () in
  expect_invalid_response "embedding missing fields" malformed;
  let invalid_vector = Task.embed ~http:(fun _ -> Ok (200,
      "{\"object\":\"list\",\"model\":\"m\",\"data\":[{\"object\":\"embedding\",\"index\":0,\"embedding\":[\"NaN\"]}],\"usage\":{\"prompt_tokens\":1,\"total_tokens\":1}}"))
      ~key:"test-secret" ~model:"m" ~input:"text" () in
  expect_invalid_response "embedding vector type" invalid_vector;
  let unauthorized = Task.embed ~http:(fun _ -> Ok (403, "test-secret provider body"))
      ~key:"test-secret" ~model:"m" ~input:"text" () in
  expect_http_error "embedding authorization" 403 unauthorized;
  assert_true "provider response body and key redacted"
    (match unauthorized with Error error -> not (contains (Task.message error) "test-secret")
     | Ok _ -> false);
  let empty = Task.embed ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~input:"" () in
  expect_invalid "required embedding input" empty;
  let oversized = Task.embed ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~input:(String.make (256 * 1024 + 1) 'x') () in
  expect_invalid "embedding size boundary" oversized;
  let missing_key = Task.embed ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"" ~model:"m" ~input:"x" () in
  expect_invalid "missing OpenAI credential" missing_key;
  let oversized_response = Task.embed ~http:(fun _ ->
      Ok (200, String.make (Task.max_json_response_bytes + 1) 'x'))
      ~key:"test-secret" ~model:"m" ~input:"text" () in
  expect_invalid_response "embedding response size boundary" oversized_response
let test_embedding_duplicate_keys () =
  let result = Task.embed ~http:(fun _ ->
      Ok (200, "{\"object\":\"list\",\"object\":\"list\"}"))
      ~key:"test-secret" ~model:"m" ~input:"x" () in
  expect_invalid_response "duplicate JSON keys" result

let test_image () = with_workspace (fun root ->
  let image = png_fixture () in
  let response = "{\"created\":1,\"data\":[{\"b64_json\":" ^
    Yojson.Basic.to_string (`String (base64 image)) ^ "}]}" in
  let result, requests = success response (fun http ->
    Task.generate_image ~http ~key:"test-secret" ~model:"image/model"
      ~root ~prompt:"draw a path" ~output:"generated.png" ()) in
  (match result with Ok message -> assert_true "image output message" (String.length message > 0)
   | Error error -> failwith (Task.message error));
  assert_equal "PNG saved byte-for-byte" image (read_file (Filename.concat root "generated.png"));
  let image_stat = Unix.lstat (Filename.concat root "generated.png") in
  assert_true "image output is a regular file" (image_stat.Unix.st_kind = Unix.S_REG);
  (match requests with
   | [request] -> request_url "image" Task.images_url request;
       request_body_contains "exact image model" "image/model" request;
       request_body_contains "one image" "\"n\":1" request;
       request_body_contains "PNG format" "png" request
   | _ -> failwith "image task made an unexpected number of requests");
  let no_request = ref true in
  let escaped = Task.generate_image ~http:(fun _ -> no_request := false; Ok (200, response))
      ~key:"test-secret" ~model:"m" ~root ~prompt:"x" ~output:"../escape.png" () in
  expect_invalid "image traversal" escaped;
  assert_true "path rejected before HTTP" !no_request;
  let malformed = Task.generate_image ~http:(fun _ -> Ok (200, "{\"created\":1,\"data\":[{\"b64_json\":\"not base64\"}]}"))
      ~key:"test-secret" ~model:"m" ~root ~prompt:"x" ~output:"bad.png" () in
  expect_invalid_response "malformed image base64" malformed;
  assert_true "malformed image creates no output" (not (Sys.file_exists (Filename.concat root "bad.png")));
  let invalid_png = Task.generate_image ~http:(fun _ -> Ok (200,
      "{\"created\":1,\"data\":[{\"b64_json\":" ^ Yojson.Basic.to_string (`String (base64 "not a png")) ^ "}]}"))
      ~key:"test-secret" ~model:"m" ~root ~prompt:"x" ~output:"bad.png" () in
  expect_invalid_response "invalid image bytes" invalid_png;
  let bad_crc = Bytes.of_string image in
  Bytes.set bad_crc 52 (Char.chr (Char.code (Bytes.get bad_crc 52) lxor 1));
  let corrupt_png = Task.generate_image ~http:(fun _ -> Ok (200,
      "{\"created\":1,\"data\":[{\"b64_json\":" ^
      Yojson.Basic.to_string (`String (base64 (Bytes.to_string bad_crc))) ^ "}]}"))
      ~key:"test-secret" ~model:"m" ~root ~prompt:"x" ~output:"bad-crc.png" () in
  expect_invalid_response "PNG checksum corruption" corrupt_png;
  assert_true "corrupt PNG creates no output" (not (Sys.file_exists (Filename.concat root "bad-crc.png")));
  let link = Filename.concat root "outside" in
  let outside = Filename.temp_file "pave-outside-" ".dir" in
  Sys.remove outside; Unix.mkdir outside 0o700;
  Fun.protect ~finally:(fun () -> Unix.rmdir outside) (fun () ->
    Unix.symlink outside link;
    let symlink = Task.generate_image ~http:(fun _ -> failwith "HTTP must not run")
        ~key:"test-secret" ~model:"m" ~root ~prompt:"x" ~output:"outside/image.png" () in
    expect_invalid "image symlink parent" symlink);
  let already_exists = Task.generate_image ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~root ~prompt:"x" ~output:"generated.png" () in
  expect_invalid "no output overwrite" already_exists;
  assert_equal "existing image is preserved" image (read_file (Filename.concat root "generated.png"));
  let rejected = Task.generate_image ~http:(fun _ -> Ok (503, "test-secret"))
      ~key:"test-secret" ~model:"m" ~root ~prompt:"x" ~output:"failed.png" () in
  expect_http_error "image HTTP failure" 503 rejected;
  assert_true "image HTTP failure creates no output"
    (not (Sys.file_exists (Filename.concat root "failed.png"))))

let test_speech () = with_workspace (fun root ->
  let wav = wav_fixture () in
  let result, requests = success wav (fun http ->
    Task.speak ~http ~key:"test-secret" ~model:"tts/model" ~root
      ~input:"speak exactly" ~voice:"alloy" ~output:"voice.wav" ()) in
  (match result with Ok _ -> () | Error error -> failwith (Task.message error));
  assert_equal "WAV file saved" wav (read_file (Filename.concat root "voice.wav"));
  let audio_stat = Unix.lstat (Filename.concat root "voice.wav") in
  assert_true "speech output is a regular file" (audio_stat.Unix.st_kind = Unix.S_REG);
  let unicode_limit = Task.speak ~http:(fun _ -> Ok (200, wav))
      ~key:"test-secret" ~model:"m" ~root
      ~input:(String.concat "" (List.init 4096 (fun _ -> "💡")))
      ~voice:"alloy" ~output:"unicode.wav" () in
  (match unicode_limit with
   | Ok _ -> ()
   | Error error -> failwith ("4096-character Unicode boundary: " ^ Task.message error));
  (match requests with
   | [request] -> request_url "speech" Task.speech_url request;
       request_body_contains "speech model exact" "tts/model" request;
       request_body_contains "speech response is WAV" "wav" request;
       request_body_contains "speech text unchanged" "speak exactly" request
   | _ -> failwith "speech task made an unexpected number of requests");
  let malformed = Task.speak ~http:(fun _ -> Ok (200, "not audio"))
      ~key:"test-secret" ~model:"m" ~root ~input:"hello" ~voice:"alloy" ~output:"bad.wav" () in
  expect_invalid_response "malformed WAV" malformed;
  assert_true "malformed speech creates no output" (not (Sys.file_exists (Filename.concat root "bad.wav")));
  let bad_voice = Task.speak ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~root ~input:"hello" ~voice:"invented" ~output:"bad.wav" () in
  expect_invalid "unknown voice" bad_voice;
  let over_limit = Task.speak ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~root ~input:(String.make 4097 'x')
      ~voice:"alloy" ~output:"bad.wav" () in
  expect_invalid "speech text boundary" over_limit;
  let invalid_utf8 = Task.speak ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~root ~input:"\255" ~voice:"alloy" ~output:"bad.wav" () in
  expect_invalid "speech rejects invalid UTF-8" invalid_utf8;
  let rejected = Task.speak ~http:(fun _ -> Ok (401, "test-secret"))
      ~key:"test-secret" ~model:"m" ~root ~input:"hello" ~voice:"alloy" ~output:"failed.wav" () in
  expect_http_error "speech HTTP failure" 401 rejected;
  let existing = Task.speak ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~root ~input:"hello" ~voice:"alloy" ~output:"voice.wav" () in
  expect_invalid "speech overwrite boundary" existing)

let test_transcription () = with_workspace (fun root ->
  write_file (Filename.concat root "recording.wav") (wav_fixture ());
  let result, requests = success "{\"text\":\"transcript text\",\"usage\":{\"type\":\"duration\",\"seconds\":1.5}}"
      (fun http -> Task.transcribe ~http ~key:"test-secret" ~model:"audio/model"
        ~root ~input:"recording.wav" ()) in
  (match result with Ok text -> assert_equal "transcript output" "transcript text\n" text
   | Error error -> failwith (Task.message error));
  (match requests with
   | [request] -> request_url "transcription" Task.transcription_url request;
       request_body_contains "audio file multipart field" "name=\"file\"; filename=\"recording.wav\"" request;
       request_body_contains "audio filename only" "recording.wav" request;
       request_body_contains "transcription model exact" "audio/model" request;
       assert_true "no absolute workspace path in multipart" (not (contains request.Task.body root))
   | _ -> failwith "transcription made an unexpected number of requests");
  let malformed = Task.transcribe ~http:(fun _ -> Ok (200, "{\"duration\":2}"))
      ~key:"test-secret" ~model:"m" ~root ~input:"recording.wav" () in
  expect_invalid_response "transcription missing text" malformed;
  let server_error = Task.transcribe ~http:(fun _ -> Ok (500, "test-secret"))
      ~key:"test-secret" ~model:"m" ~root ~input:"recording.wav" () in
  expect_http_error "transcription HTTP failure" 500 server_error;
  let invalid_usage = Task.transcribe ~http:(fun _ -> Ok (200,
      "{\"text\":\"ok\",\"usage\":{\"type\":\"duration\",\"seconds\":-1}}"))
      ~key:"test-secret" ~model:"m" ~root ~input:"recording.wav" () in
  expect_invalid_response "transcription usage validation" invalid_usage;
  let traversal = Task.transcribe ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~root ~input:"../outside.wav" () in
  expect_invalid "audio traversal" traversal;
  let wrong_type = Task.transcribe ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~root ~input:"recording.txt" () in
  expect_invalid "audio extension boundary" wrong_type;
  let link = Filename.concat root "linked.wav" in
  Unix.symlink (Filename.concat root "recording.wav") link;
  let symlink = Task.transcribe ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~root ~input:"linked.wav" () in
  expect_invalid "audio symlink boundary" symlink;
  let too_large = Filename.concat root "large.wav" in
  let output = open_out_bin too_large in
  output_string output (String.make (Task.max_audio_input_bytes + 1) 'a');
  close_out output;
  let oversize = Task.transcribe ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~root ~input:"large.wav" () in
  expect_invalid "audio size boundary" oversize)

let test_rerank () =
  let result, requests = success rerank_response (fun http ->
    Task.rerank ~http ~key:"test-secret" ~model:"rerank/model-v4" ~query:"capital?"
      ~documents:["first"; "second"] ~top_n:(Some 2) ()) in
  (match result with Ok json -> assert_true "rerank JSON output" (String.length json > 10)
   | Error error -> failwith (Task.message error));
  (match requests with
   | [request] -> request_url "rerank" Task.cohere_rerank_url request;
       request_body_contains "exact Cohere model" "rerank/model-v4" request;
       request_body_contains "top_n requested" "\"top_n\":2" request;
       request_body_contains "document list sent" "second" request
   | _ -> failwith "rerank made an unexpected number of requests");
  let duplicate_index = Task.rerank ~http:(fun _ -> Ok (200,
      "{\"results\":[{\"index\":0,\"relevance_score\":0.9},{\"index\":0,\"relevance_score\":0.8}]}"))
      ~key:"test-secret" ~model:"m" ~query:"q" ~documents:["a"; "b"] ~top_n:None () in
  expect_invalid_response "rerank duplicate index" duplicate_index;
  let invalid_score = Task.rerank ~http:(fun _ -> Ok (200,
      "{\"results\":[{\"index\":0,\"relevance_score\":1.1}]}"))
      ~key:"test-secret" ~model:"m" ~query:"q" ~documents:["a"] ~top_n:None () in
  expect_invalid_response "rerank score bounds" invalid_score;
  let unordered = Task.rerank ~http:(fun _ -> Ok (200,
      "{\"results\":[{\"index\":0,\"relevance_score\":0.2},{\"index\":1,\"relevance_score\":0.9}]}"))
      ~key:"test-secret" ~model:"m" ~query:"q" ~documents:["a"; "b"] ~top_n:None () in
  expect_invalid_response "rerank score ordering" unordered;
  let long_document = Task.rerank ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~query:"q"
      ~documents:[String.make (32 * 1024 + 1) 'x'] ~top_n:None () in
  expect_invalid "rerank document size boundary" long_document;
  let incomplete = Task.rerank ~http:(fun _ -> Ok (200,
      "{\"results\":[{\"index\":0,\"relevance_score\":0.9}]}"))
      ~key:"test-secret" ~model:"m" ~query:"q" ~documents:["a"; "b"] ~top_n:None () in
  expect_invalid_response "rerank partial results" incomplete;
  let http_error = Task.rerank ~http:(fun _ -> Ok (429, "test-secret"))
      ~key:"test-secret" ~model:"m" ~query:"q" ~documents:["a"] ~top_n:None () in
  expect_http_error "rerank rate limit" 429 http_error;
  assert_true "Cohere error body and API key redacted"
    (match http_error with Error error -> not (contains (Task.message error) "test-secret")
     | Ok _ -> false);
  let no_documents = Task.rerank ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~query:"q" ~documents:[] ~top_n:None () in
  expect_invalid "rerank documents required" no_documents;
  let bad_top_n = Task.rerank ~http:(fun _ -> failwith "HTTP must not run")
      ~key:"test-secret" ~model:"m" ~query:"q" ~documents:["a"] ~top_n:(Some 2) () in
  expect_invalid "rerank top_n boundary" bad_top_n

let test_fixed_host_and_cancellation () =
  let restore_env name value =
    Unix.putenv name (Option.value ~default:"" value) in
  with_workspace (fun root ->
    let curl = Filename.concat root "curl" in
    let invoked = Filename.concat root "curl-invoked" in
    write_file curl "#!/bin/sh\n: > \"$PAVE_CURL_SENTINEL\"\nexit 1\n";
    Unix.chmod curl 0o700;
    let old_path = Sys.getenv_opt "PATH" in
    let old_sentinel = Sys.getenv_opt "PAVE_CURL_SENTINEL" in
    Fun.protect
      ~finally:(fun () ->
        restore_env "PATH" old_path;
        restore_env "PAVE_CURL_SENTINEL" old_sentinel)
      (fun () ->
        Unix.putenv "PATH" root;
        Unix.putenv "PAVE_CURL_SENTINEL" invoked;
        let arbitrary = Task.default_http ~response_limit:100 {
          Task.method_ = "POST"; url = "https://example.invalid/v1/embeddings";
          headers = ["Authorization", "Bearer test-secret"]; body = "{}";
        } in
        (match arbitrary with Error Task.Transport_error -> ()
         | _ -> failwith "arbitrary host was not rejected before curl");
        assert_true "arbitrary host never invokes curl" (not (Sys.file_exists invoked))));
  let was_called = ref false in
  let cancelled = Task.embed ~cancel:(fun () -> true)
      ~http:(fun _ -> was_called := true; Ok (200, embed_response "m"))
      ~key:"test-secret" ~model:"m" ~input:"x" () in
  (match cancelled with Error Task.Cancelled -> () | _ -> failwith "pre-cancelled task was not cancelled");
  assert_true "cancelled task sends no request" (not !was_called)

let test_task_cli_required_options () =
  let expect_failure label expected f =
    match f () with
    | () -> failwith (label ^ ": expected CLI error")
    | exception Failure message -> assert_true label (try ignore (Str.search_forward (Str.regexp_string expected) message 0); true with Not_found -> false)
    | exception exn -> failwith (label ^ ": unexpected exception " ^ Printexc.to_string exn) in
  expect_failure "embed input required" "--input is required"
    (fun () -> Task_cli.run [|"embed"; "--model"; "m"|]);
  expect_failure "task rejects chat session option" "unknown task option"
    (fun () -> Task_cli.run [|"embed"; "--model"; "m"; "--input"; "x"; "--session"; "session.jsonl"|]);
  expect_failure "video generation is not a task operation" "unknown task operation"
    (fun () -> Task_cli.run [|"video"; "--model"; "sora-2"|]);
  expect_failure "task model required" "--model is required"
    (fun () -> Task_cli.run [|"rerank"; "--query"; "q"; "--document"; "d"|])

let () =
  test_embed ();
  test_embedding_duplicate_keys ();
  test_image ();
  test_speech ();
  test_transcription ();
  test_rerank ();
  test_fixed_host_and_cancellation ();
  test_task_cli_required_options ()
