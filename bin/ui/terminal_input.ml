type mode = Normal | Escape | Utf8 of int

type decoder = {
  filter : Notty.Unescape.t;
  events : Notty.Unescape.event Queue.t;
  scratch : bytes;
  mutable size : int;
  mutable mode : mode;
}

type t = {
  term : Notty_unix.Term.t;
  fd : Unix.file_descr;
  read_fds : Unix.file_descr list;
  mutable wake_fds : (Unix.file_descr * Unix.file_descr list) option;
  decoder : decoder;
  input : bytes;
}

let create_decoder () =
  { filter = Notty.Unescape.create (); events = Queue.create ();
    scratch = Bytes.create 64; size = 0; mode = Normal }

let create term =
  let fd, _ = Notty_unix.Term.fds term in
  { term; fd; read_fds = [ fd ]; wake_fds = None;
    decoder = create_decoder (); input = Bytes.create 1024 }

let flush t =
  if t.size > 0 then (
    Notty.Unescape.input t.filter t.scratch 0 t.size;
    t.size <- 0;
    let rec drain () = match Notty.Unescape.next t.filter with
      | #Notty.Unescape.event as event -> Queue.add event t.events; drain ()
      | `Await | `End -> () in
    drain ())

let flush_ascii t =
  if t.mode = Normal then flush t

let append t c =
  Bytes.set t.scratch t.size c;
  t.size <- t.size + 1

let replacement t =
  t.size <- 0;
  t.mode <- Normal;
  append t '\xef'; append t '\xbf'; append t '\xbd';
  flush t

let rec accept t c =
  let byte = Char.code c in
  match t.mode with
  | Normal ->
      if byte = 27 then (flush t; append t c; t.mode <- Escape)
      else if byte < 128 then (
        append t c;
        if t.size = Bytes.length t.scratch then flush t)
      else if byte >= 0xc2 && byte <= 0xdf then
        (flush t; append t c; t.mode <- Utf8 2)
      else if byte >= 0xe0 && byte <= 0xef then
        (flush t; append t c; t.mode <- Utf8 3)
      else if byte >= 0xf0 && byte <= 0xf4 then
        (flush t; append t c; t.mode <- Utf8 4)
      else (flush t; replacement t)
  | Utf8 expected ->
      if byte land 0xc0 <> 0x80 then (replacement t; accept t c)
      else (
        append t c;
        if t.size = expected then (
          t.mode <- Normal;
          let first = Char.code (Bytes.get t.scratch 0) in
          let second = Char.code (Bytes.get t.scratch 1) in
          if (first = 0xe0 && second < 0xa0)
             || (first = 0xed && second >= 0xa0)
             || (first = 0xf0 && second < 0x90)
             || (first = 0xf4 && second >= 0x90)
          then replacement t else flush t))
  | Escape ->
      if t.size = 1 && byte = 27 then
        (t.mode <- Normal; flush t; accept t c)
      else if t.size = 1 && byte >= 0xc2 then
        (t.mode <- Normal; flush t; accept t c)
      else if t.size = 1 && byte < 0x20 then
        (append t c; t.mode <- Normal; flush t)
      else if t.size = 1 && (c = '[' || c = 'O') then append t c
      else if t.size = 1 && byte >= 0x20 && byte < 0x7f then
        (append t c; t.mode <- Normal; flush t)
      else if t.size >= 2 && byte >= 0x40 && byte <= 0x7e then
        (append t c; t.mode <- Normal; flush t)
      else if t.size >= Bytes.length t.scratch then
        (t.mode <- Normal; t.size <- 0)
      else append t c

let expire_escape t =
  let standalone = t.size = 1 in
  t.mode <- Normal;
  if standalone then flush t else t.size <- 0

let pending t =
  flush_ascii t.decoder;
  if not (Queue.is_empty t.decoder.events) || Notty_unix.Term.pending t.term then true
  else (
    try
      let readable, _, _ = Unix.select t.read_fds [] [] 0. in
      readable <> []
    with Unix.Unix_error (Unix.EINTR, _, _) -> true)

let event ?wake_fd ?timeout t =
  let d = t.decoder in
  let rec next () : [ Notty.Unescape.event | `Resize of int * int | `End | `Wake | `Tick ] =
    flush_ascii d;
    if Notty_unix.Term.pending t.term then
      (match Notty_unix.Term.event t.term with
       | `Resize _ as resized -> resized
       | `End -> `End
       | #Notty.Unescape.event as key -> key)
    else if not (Queue.is_empty d.events) then
      (Queue.take d.events :> [ Notty.Unescape.event | `Resize of int * int | `End | `Wake | `Tick ])
    else (
      let wait = if d.mode = Escape then 0.04 else
        match timeout with Some seconds -> max 0. seconds | None -> -1. in
      let watched = match wake_fd with
        | None -> t.read_fds
        | Some fd ->
            (match t.wake_fds with
             | Some (cached, fds) when cached = fd -> fds
             | _ ->
                 let fds = [ fd; t.fd ] in
                 t.wake_fds <- Some (fd, fds);
                 fds) in
      let readable = try
        let ready, _, _ = Unix.select watched [] [] wait in
        ready
      with Unix.Unix_error (Unix.EINTR, _, _) -> [] in
      if (match wake_fd with None -> false | Some fd -> List.mem fd readable) then
        `Wake
      else if List.mem t.fd readable then (
        let count = try Unix.read t.fd t.input 0 (Bytes.length t.input)
          with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
        if count = 0 then `End
        else (
          if count > 0 then
            for i = 0 to count - 1 do accept d (Bytes.get t.input i) done;
          next ()))
      else if d.mode = Escape then (expire_escape d; next ())
      else (match timeout with Some _ -> `Tick | None -> next ())) in
  next ()
