let fail label = failwith ("terminal input: " ^ label)

let feed d text = String.iter (Terminal_input.accept d) text

let drain d =
  let rec loop acc =
    if Queue.is_empty d.Terminal_input.events then List.rev acc
    else loop (Queue.take d.Terminal_input.events :: acc)
  in
  loop []

let check label expected actual =
  if actual <> expected then fail label

let ascii c : Notty.Unescape.event = `Key (`ASCII c, [])
let unicode code : Notty.Unescape.event = `Key (`Uchar (Uchar.of_int code), [])

let () =
  let d = Terminal_input.create_decoder () in
  let burst = String.init 150 (fun n -> Char.chr (33 + n mod 80)) in
  feed d (String.sub burst 0 63);
  check "ASCII burst flushed before boundary" [] (drain d);
  if d.size <> 63 then fail "ASCII burst was not buffered";
  feed d (String.sub burst 63 87);
  Terminal_input.flush_ascii d;
  check "ordered ASCII burst across scratch boundaries"
    (List.init (String.length burst) (fun i -> ascii burst.[i])) (drain d);
  if d.size <> 0 then fail "ASCII buffer not emptied";

  let d = Terminal_input.create_decoder () in
  feed d "a\xc3";
  Terminal_input.flush_ascii d;
  check "ASCII preceding fragmented UTF-8" [ ascii 'a' ] (drain d);
  feed d "\xa9b";
  Terminal_input.flush_ascii d;
  check "UTF-8 codepoint not split across reads"
    [ unicode 0xe9; ascii 'b' ] (drain d);
  feed d "\xe0\x80\x80x";
  Terminal_input.flush_ascii d;
  check "overlong UTF-8 replaced before next key"
    [ unicode 0xfffd; ascii 'x' ] (drain d);

  let d = Terminal_input.create_decoder () in
  feed d "q\027";
  Terminal_input.flush_ascii d;
  check "ASCII delivered before pending Escape" [ ascii 'q' ] (drain d);
  if d.mode <> Terminal_input.Escape then fail "standalone Escape was swallowed";
  feed d "[A\003";
  Terminal_input.flush_ascii d;
  check "CSI key and control preserved in order"
    [ `Key (`Arrow `Up, []); `Key (`ASCII 'C', [ `Ctrl ]) ] (drain d);
  feed d "\027x";
  check "Alt ASCII delivered" [ `Key (`ASCII 'x', [ `Meta ]) ] (drain d);

  feed d "\027";
  Terminal_input.flush_ascii d;
  check "standalone Escape awaits timeout" [] (drain d);
  Terminal_input.expire_escape d;
  check "standalone Escape emitted at timeout" [ `Key (`Escape, []) ] (drain d);

  let d = Terminal_input.create_decoder () in
  feed d "x\027[200~ab";
  Terminal_input.flush_ascii d;
  check "paste start precedes pasted text"
    [ ascii 'x'; `Paste `Start; ascii 'a'; ascii 'b' ] (drain d);
  feed d "\xc3\xa9\027[201~z";
  Terminal_input.flush_ascii d;
  check "paste end follows UTF-8 text before next ASCII"
    [ unicode 0xe9; `Paste `End; ascii 'z' ] (drain d);
  print_endline "terminal input decoding: ok"
