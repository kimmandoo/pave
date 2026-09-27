type mode =
  | Normal
  | Escape
  | Utf8 of int
  | Csi
  | Discard_csi
  | Discard_escape
  | String of bool * bool
  | Discard_string of bool * bool

type decoder = {
  filter : Notty.Unescape.t;
  events : Notty.Unescape.event Queue.t;
  scratch : bytes;
  mutable size : int;
  mutable mode : mode;
  mutable pasting : bool;
  mutable previous_cr : bool;
}

type t = {
  term : Notty_unix.Term.t;
  fd : Unix.file_descr;
  read_fds : Unix.file_descr list;
  mutable wake_fds : (Unix.file_descr list * Unix.file_descr list) option;
  decoder : decoder;
  input : bytes;
}

let create_decoder () =
  { filter = Notty.Unescape.create (); events = Queue.create ();
    scratch = Bytes.create 64; size = 0; mode = Normal;
    pasting = false; previous_cr = false }

let create term =
  let fd, _ = Notty_unix.Term.fds term in
  { term; fd; read_fds = [ fd ]; wake_fds = None;
    decoder = create_decoder (); input = Bytes.create 1024 }

let flush t =
  if t.size > 0 then (
    Notty.Unescape.input t.filter t.scratch 0 t.size;
    t.size <- 0;
    let rec drain () = match Notty.Unescape.next t.filter with
      | `Paste `Start as event ->
          t.pasting <- true;
          t.previous_cr <- false;
          Queue.add event t.events;
          drain ()
      | `Paste `End as event ->
          t.pasting <- false;
          t.previous_cr <- false;
          Queue.add event t.events;
          drain ()
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
  if t.mode = Normal && c = '\r' then (
    flush t;
    Queue.add (`Key (`Enter, [])) t.events;
    t.previous_cr <- true)
  else if t.mode = Normal && c = '\n' && t.previous_cr then
    t.previous_cr <- false
  else if t.mode = Normal && c = '\n' then (
    flush t;
    Queue.add (`Key (`Enter, [])) t.events;
    t.previous_cr <- false)
  else (
    if t.mode = Normal then t.previous_cr <- false;
    let byte = Char.code c in
  let final = byte >= 0x40 && byte <= 0x7e in
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
      else if t.size = 1 && (c = '[' || c = 'O') then
        (append t c; t.mode <- Csi)
      else if t.size = 1 && (c = ']' || c = 'P' || c = '_' || c = '^' || c = 'X') then
        (t.size <- 0; t.mode <- String (c = ']', false))
      else if t.size = 1 && byte >= 0xc2 && byte <= 0xf4 then
        (t.mode <- Normal; flush t; accept t c)
      else if t.size = 1 && byte >= 0x80 then
        (t.size <- 0; t.mode <- Discard_escape)
      else if t.size = 1 && byte < 0x20 then
        (append t c; t.mode <- Normal; flush t)
      else if t.size = 1 && byte >= 0x20 && byte < 0x7f then
        (append t c; t.mode <- Normal; flush t)
      else if t.size >= 2 && final then
        (append t c; t.mode <- Normal; flush t)
      else if t.size >= Bytes.length t.scratch then
        (t.size <- 0; t.mode <- Discard_escape)
      else append t c
  | Discard_escape ->
      if byte = 27 then (append t c; t.mode <- Escape)
  | Csi ->
      if final then (
        if t.size >= Bytes.length t.scratch then (
          t.size <- 0;
          t.mode <- Normal)
        else (
          append t c;
          t.mode <- Normal;
          flush t))
      else if t.size >= Bytes.length t.scratch then (
        t.size <- 0;
        t.mode <- Discard_csi)
      else append t c
  | Discard_csi ->
      if final then t.mode <- Normal
  | String (osc, escaped) ->
      if (escaped && c = '\\') || (osc && c = '\007')
      then t.mode <- Normal
      else t.mode <- String (osc, c = '\027')
  | Discard_string (osc, escaped) ->
      if (escaped && c = '\\') || (osc && c = '\007')
      then t.mode <- Normal
      else t.mode <- Discard_string (osc, c = '\027')
  )

let expire_escape t =
  match t.mode with
  | Escape ->
      if t.size = 1 then (t.mode <- Normal; flush t)
      else (t.mode <- Discard_escape; t.size <- 0)
  | Csi -> t.mode <- Discard_csi; t.size <- 0
  | String (osc, escaped) -> t.mode <- Discard_string (osc, escaped); t.size <- 0
  | _ -> ()

let pending t =
  flush_ascii t.decoder;
  if not (Queue.is_empty t.decoder.events) || Notty_unix.Term.pending t.term then true
  else (
    try
      let readable, _, _ = Unix.select t.read_fds [] [] 0. in
      readable <> []
    with Unix.Unix_error (Unix.EINTR, _, _) -> true)

let event ?wake_fd ?(wake_fds = []) ?timeout t =
  let wake_fds =
    (match wake_fd with None -> wake_fds | Some fd -> fd :: wake_fds)
    |> List.sort_uniq compare in
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
      let escape_pending = match d.mode with
        | Escape | Csi | String _ -> true
        | _ -> false in
      let wait = if escape_pending then 0.04 else
        match timeout with Some seconds -> max 0. seconds | None -> -1. in
      let watched = if wake_fds = [] then t.read_fds
        else match t.wake_fds with
          | Some (cached, fds) when cached = wake_fds -> fds
          | _ ->
              let fds = wake_fds @ t.read_fds in
              t.wake_fds <- Some (wake_fds, fds);
              fds in
      let readable = try
        let ready, _, _ = Unix.select watched [] [] wait in
        ready
      with Unix.Unix_error (Unix.EINTR, _, _) -> [] in
      if List.exists (fun fd -> List.mem fd readable) wake_fds then
        `Wake
      else if List.mem t.fd readable then (
        let count = try Unix.read t.fd t.input 0 (Bytes.length t.input)
          with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
        if count = 0 then `End
        else (
          if count > 0 then
            for i = 0 to count - 1 do accept d (Bytes.get t.input i) done;
          next ()))
      else if (match d.mode with Escape | Csi | String _ -> true | _ -> false)
      then (expire_escape d; next ())
      else (match timeout with Some _ -> `Tick | None -> next ())) in
  next ()
