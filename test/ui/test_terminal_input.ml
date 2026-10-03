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
  let d = Terminal_input.create_decoder () in
  feed d "\027\xe3";
  Terminal_input.flush_ascii d;
  check "Alt UTF-8 prefix waits for its complete scalar" [] (drain d);
  feed d "\x85";
  Terminal_input.flush_ascii d;
  check "Alt UTF-8 remains pending across terminal reads" [] (drain d);
  feed d "\x90";
  check "Alt Korean shortcut retains Meta rather than cancelling a modal"
    [ `Key (`Uchar (Uchar.of_int 0x3150), [ `Meta ]) ] (drain d);
  feed d "\027\xc3\xa9";
  check "Alt non-Korean Unicode retains its modifier"
    [ `Key (`Uchar (Uchar.of_int 0xe9), [ `Meta ]) ] (drain d);

  let d = Terminal_input.create_decoder () in
  feed d "\xe2\x82";
  Terminal_input.finish_decoder d;
  check "truncated UTF-8 at EOF is replaced"
    [ unicode 0xfffd ] (drain d);

  let d = Terminal_input.create_decoder () in

  feed d "\027\r\027\n";
  check "Alt+Enter control forms are preserved"
    [ `Key (`ASCII 'M', [ `Meta; `Ctrl ]); `Key (`Enter, [ `Meta ]) ]
    (drain d);
  let d = Terminal_input.create_decoder () in
  feed d "\r";
  Terminal_input.flush_ascii d;
  let actual = drain d in
  check "plain carriage return is Enter" [ `Key (`Enter, []) ] actual;
  let d = Terminal_input.create_decoder () in
  feed d "\n";
  Terminal_input.flush_ascii d;
  check "plain line feed is Enter" [ `Key (`Enter, []) ] (drain d);
  feed d "\r\n";
  Terminal_input.flush_ascii d;
  check "CRLF is one Enter" [ `Key (`Enter, []) ] (drain d);

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

  let d = Terminal_input.create_decoder () in
  feed d "\027[20";
  Terminal_input.flush_ascii d;
  check "split CSI prefix stays incomplete" [] (drain d);
  feed d "0~p";
  Terminal_input.flush_ascii d;
  check "split CSI terminator is recognized"
    [ `Paste `Start; ascii 'p' ] (drain d);
  feed d "\027[201";
  Terminal_input.flush_ascii d;
  check "split CSI end stays incomplete" [] (drain d);
  feed d "~q";
  Terminal_input.flush_ascii d;
  check "split CSI end preserves following key"
    [ `Paste `End; ascii 'q' ] (drain d);

  let d = Terminal_input.create_decoder () in
  feed d "\027]0;window title\027";
  Terminal_input.flush_ascii d;
  check "split OSC terminator does not leak title" [] (drain d);
  feed d "\\r";
  Terminal_input.flush_ascii d;
  check "OSC terminator and following key"
    [ ascii 'r' ] (drain d);
  feed d "\027Pdevice data\027";
  feed d "\\s\027_application data\027";
  feed d "\\t";
  Terminal_input.flush_ascii d;
  check "split DCS and APC terminators do not leak payload"
    [ ascii 's'; ascii 't' ] (drain d);

  let d = Terminal_input.create_decoder () in
  feed d "\027[123";
  Terminal_input.expire_escape d;
  feed d "45mX";
  Terminal_input.flush_ascii d;
  check "timed-out CSI payload is discarded through its terminator"
    [ ascii 'X' ] (drain d);

  let d = Terminal_input.create_decoder () in
  feed d "\027]2;unfinished title";
  Terminal_input.expire_escape d;
  feed d "continued title\027\\K";
  Terminal_input.flush_ascii d;
  check "timed-out OSC payload is discarded through split terminator"
    [ ascii 'K' ] (drain d);

  let d = Terminal_input.create_decoder () in
  feed d "\027\x80";
  Terminal_input.expire_escape d;
  feed d "hidden suffix\027x";
  Terminal_input.flush_ascii d;
  check "timed-out malformed Escape prefix cannot leak suffix keys"
    [ `Key (`ASCII 'x', [ `Meta ]) ] (drain d);

  let d = Terminal_input.create_decoder () in
  feed d ("\027[" ^ String.make 80 '1' ^ "mY");
  Terminal_input.flush_ascii d;
  check "oversized CSI payload is discarded through its terminator"
    [ ascii 'Y' ] (drain d);

  let d = Terminal_input.create_decoder () in
  feed d "\x80\xe2\x82Z\xf4\x90\x80\x80W";
  Terminal_input.flush_ascii d;
  check "malformed UTF-8 becomes replacements without swallowing later keys"
    [ unicode 0xfffd; unicode 0xfffd; ascii 'Z';
      unicode 0xfffd; ascii 'W' ] (drain d);

  let d = Terminal_input.create_decoder () in
  feed d "\027[200~\r\027\027[201~z";
  Terminal_input.flush_ascii d;
  check "pasted Enter and Escape remain inside paste framing"
    [ `Paste `Start; `Key (`Enter, []); `Key (`Escape, []);
      `Paste `End; ascii 'z' ] (drain d);
  let d = Terminal_input.create_decoder () in
  feed d "\027[200~x\r\ny\027[201~";
  Terminal_input.flush_ascii d;
  check "pasted CRLF is one newline"
    [ `Paste `Start; ascii 'x'; `Key (`Enter, []); ascii 'y'; `Paste `End ]
    (drain d);

  print_endline "terminal input decoding: ok"
