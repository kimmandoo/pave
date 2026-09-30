open Pave.Write_preview

let decode fragments =
  let decoder = create () in
  List.iter (feed decoder) fragments;
  snapshot decoder
let text preview = String.concat "\n" (List.map snd preview.lines)
let split_bytes string = List.init (String.length string) (fun i -> String.make 1 string.[i])

let () =
  let json = {|{"extra":{"content":"ignore"},"content":"a\n\uD83D\uDE80 café\t\u202eX","path":"src/demo.ml"}|} in
  let preview = decode (split_bytes json) in
  assert (preview.path = Some "src/demo.ml");
  assert (preview.lines = [1, "a"; 2, "🚀 café  X"]);
  assert (preview.total_lines = 2 && preview.omitted_lines = 0);
  let utf = create () in
  feed utf {|{"content":"|};
  feed utf "\xc3";
  assert (text (snapshot utf) = "");
  feed utf "\xa9";
  assert (text (snapshot utf) = "é");
  feed utf {|","path":"a"}|};
  assert ((snapshot utf).path = Some "a");
  List.iter (fun path ->
    let preview = of_values ~path ~content:"safe" in
    assert (preview.path = None)) ["../escape"; "/absolute"; "a/../b";
      "https://host/file"; "a\\b"; "bad\027label"; "a/\226\128\174b"];
  assert ((of_values ~path:(String.make 510 'x' ^ "🚀") ~content:"safe").path = None);
  let content = String.concat "\n" (List.init 1000 string_of_int) in
  let preview = of_values ~path:"a" ~content in
  assert (preview.total_lines = 1000);
  assert (preview.omitted_lines = 984);
  assert (List.hd preview.lines = (985, "984"));
  assert (List.hd (List.rev preview.lines) = (1000, "999"));
  let long = of_values ~path:"a" ~content:(String.make 1_048_576 'x') in
  assert (long.lines = [1, String.make max_line_bytes 'x']);
  assert (long.omitted_bytes = 1_048_576 - max_line_bytes);
  let suffix = of_values ~path:"a"
    ~content:(String.make 1000 'x' ^ String.concat "" (List.init 70 (fun _ -> "🚀"))) in
  assert (suffix.lines = [1, String.concat "" (List.init 64 (fun _ -> "🚀"))]);
  assert (suffix.omitted_bytes = 1024);
  let fragmented = decode (split_bytes {|{"path":"a","content":"\uD83D\uDE80\nfinal"}|}) in
  assert (fragmented.lines = [1, "🚀"; 2, "final"]);
  assert (text (decode [ {|{"content":"\uD800x"}|} ]) = "�x");
  (* Span eviction must drop whole UTF-8 scalars, even when ASCII follows a
     wrapped ring or replaces its entire retained tail. *)
  List.iter (fun (content, expected, omitted) ->
    let json = {|{"path":"src/a.ml","content":"|} ^ content ^ {|"}|} in
    List.iter (fun width ->
      let decoder = create () in
      let rec fragments offset =
        if offset < String.length json then (
          let size = min width (String.length json - offset) in
          feed decoder (String.sub json offset size);
          fragments (offset + size)) in
      fragments 0;
      let preview = snapshot decoder in
      assert (preview.path = Some "src/a.ml");
      assert (preview.lines = [1, expected]);
      assert (preview.omitted_bytes = omitted))
      [1; 3; 17; 255; 256; 257; String.length json])
    [String.make 255 'x' ^ "🚀", String.make 252 'x' ^ "🚀", 3;
     "🚀" ^ String.make 255 'x', String.make 255 'x', 4;
     "🚀" ^ String.make 256 'x', String.make 256 'x', 4;
     String.make 300 'x' ^ "é" ^ String.make 3 'y',
       String.make 251 'x' ^ "éyyy", 49];
  print_endline "incremental bounded write preview: ok"
