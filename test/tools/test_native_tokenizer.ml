module Native_tokenizer = Pave.Native_tokenizer

let fail label = failwith ("native tokenizer: " ^ label)
let expect label condition = if not condition then fail label

let expect_count encoding text expected =
  let result = Native_tokenizer.count_tokens ~encoding text in
  expect (encoding ^ " reports its encoding") (result.encoding = encoding);
  if result.token_count <> expected then
    fail (Printf.sprintf "%s token count for %s: expected %d, got %d"
      encoding (String.escaped text) expected result.token_count)

let expect_error label operation =
  match operation () with
  | _ -> fail (label ^ " was accepted")
  | exception Native_tokenizer.Error _ -> ()
  | exception error ->
      fail (label ^ " raised the wrong exception: " ^ Printexc.to_string error)

let () =
  List.iter (fun (encoding, text, expected) ->
    expect_count encoding text expected) [
    ("cl100k_base", "", 0);
    ("o200k_base", "", 0);
    ("cl100k_base", " ", 1);
    ("o200k_base", " ", 1);
    ("cl100k_base", "\n", 1);
    ("o200k_base", "\n", 1);
    ("cl100k_base", "hello world", 2);
    ("o200k_base", "hello world", 2);
    ("cl100k_base", "Hello, world!", 4);
    ("o200k_base", "Hello, world!", 4);
    ("cl100k_base", "1234567890", 4);
    ("o200k_base", "1234567890", 4);
    ("cl100k_base", "The quick brown fox jumps over the lazy dog.", 10);
    ("o200k_base", "The quick brown fox jumps over the lazy dog.", 10);
    ("cl100k_base", "😀", 2);
    ("o200k_base", "😀", 1);
    ("cl100k_base", "\x65\xcc\x81", 2);
    ("o200k_base", "\x65\xcc\x81", 2);
    ("cl100k_base", "お誕生日おめでとう", 9);
    ("o200k_base", "お誕生日おめでとう", 8);
    ("cl100k_base", "中文测试", 3);
    ("o200k_base", "中文测试", 2);
    ("cl100k_base", "can't won't I'M", 6);
    ("o200k_base", "can't won't I'M", 4);
  ];
  expect_error "unknown encoding" (fun () ->
    Native_tokenizer.count_tokens ~encoding:"estimated" "hello");
  expect_error "known special-token text" (fun () ->
    Native_tokenizer.count_tokens ~encoding:"cl100k_base" "<|endoftext|>");
  expect_error "unknown special-token text" (fun () ->
    Native_tokenizer.count_tokens ~encoding:"o200k_base" "<|unknown_token|>");
  expect_error "invalid UTF-8" (fun () ->
    Native_tokenizer.count_tokens ~encoding:"cl100k_base" "\255");
  expect_error "oversized input" (fun () ->
    Native_tokenizer.count_tokens ~encoding:"o200k_base"
      (String.make (Native_tokenizer.max_input_bytes + 1) 'x'));
  print_endline "native tokenizer exact reference vectors and rejection: ok"
