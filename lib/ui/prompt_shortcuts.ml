type shortcut = {
  name : string;
  prose : string;
}

let lexicon = [
  { name = "thinkdeep"; prose = "Please reason carefully through this request" };
  { name = "verifyfirst"; prose = "Please verify the relevant details before answering" };
  { name = "planfirst"; prose = "Please outline a concise plan before proceeding" };
]

let names = List.map (fun shortcut -> shortcut.name) lexicon

let is_word_byte c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> Char.code c >= 128

let token_at text start token =
  let text_length = String.length text in
  let token_length = String.length token in
  let stop = start + token_length in
  let rec matches i =
    i = token_length || (text.[start + i] = token.[i] && matches (i + 1))
  in
  if stop > text_length ||
     not (matches 0) ||
     (start > 0 && is_word_byte text.[start - 1]) ||
     (stop < text_length && is_word_byte text.[stop])
  then false
  else
    let left_path = start > 0 &&
      (text.[start - 1] = '/' || text.[start - 1] = '\\' ||
       text.[start - 1] = '.') in
    let right_path = stop < text_length &&
      (text.[stop] = '/' || text.[stop] = '\\' ||
       (text.[stop] = '.' && stop + 1 < text_length &&
        is_word_byte text.[stop + 1])) in
    not (left_path || right_path ||
         (start > 0 && text.[start - 1] = '@'))

let marker_at text line_start line_end =
  let i = ref line_start in
  while !i < line_end && !i - line_start < 4 && text.[!i] = ' ' do
    incr i
  done;
  if !i - line_start > 3 || !i >= line_end ||
     (text.[!i] <> '`' && text.[!i] <> '~')
  then None
  else
    let marker = text.[!i] in
    let first = !i in
    while !i < line_end && text.[!i] = marker do incr i done;
    let count = !i - first in
    if count < 3 then None
    else Some (marker, count, first, !i)

let has_closing_ticks text start line_end count =
  let rec scan i =
    if i >= line_end then false
    else if text.[i] <> '`' then scan (i + 1)
    else
      let j = ref i in
      while !j < line_end && text.[!j] = '`' do incr j done;
      if !j - i = count then true else scan !j
  in
  scan start


let expand ~enabled ~disabled ~paste_ranges text =
  let is_selected shortcut =
    List.mem shortcut.name enabled && not (List.mem shortcut.name disabled)
  in
  if not (List.exists is_selected lexicon) then (text, [])
  else
    let length = String.length text in
    let output = ref None in
    let names = ref [] in
    let copied_until = ref 0 in
    let add_replacement start stop prose name =
      let buffer = match !output with
        | Some buffer -> buffer
        | None ->
            let buffer = Buffer.create (length + 48) in
            output := Some buffer;
            buffer
      in
      Buffer.add_substring buffer text !copied_until (start - !copied_until);
      Buffer.add_string buffer prose;
      copied_until := stop;
      if not (List.mem name !names) then names := name :: !names
    in
    let overlaps_paste start stop =
      List.exists (fun (paste_start, paste_stop) ->
        start < paste_stop && paste_start < stop) paste_ranges
    in
    let process_line start stop =
      let rec scan i inline_ticks =
        if i >= stop then ()
        else if text.[i] = '`' then begin
          let run_end = ref i in
          while !run_end < stop && text.[!run_end] = '`' do incr run_end done;
          let count = !run_end - i in
          if inline_ticks = 0 then begin
            if has_closing_ticks text !run_end stop count then
              let rec find_close j =
                if j >= stop then stop
                else if text.[j] <> '`' then find_close (j + 1)
                else
                  let k = ref j in
                  while !k < stop && text.[!k] = '`' do incr k done;
                  if !k - j = count then !k else find_close !k
              in
              scan (find_close !run_end) 0
            else scan !run_end 0
          end else if count = inline_ticks then scan !run_end 0
          else scan !run_end inline_ticks
        end else if inline_ticks <> 0 then scan (i + 1) inline_ticks
        else
          let rec try_shortcuts = function
            | [] -> scan (i + 1) 0
            | shortcut :: rest ->
                if not (is_selected shortcut) ||
                   not (token_at text i shortcut.name)
                then try_shortcuts rest
                else
                  let token_end = i + String.length shortcut.name in
                  if overlaps_paste i token_end then scan token_end 0
                  else begin
                    add_replacement i token_end shortcut.prose shortcut.name;
                    scan token_end 0
                  end
          in
          try_shortcuts lexicon
      in
      scan start 0
    in
    let rec lines line_start fenced =
      if line_start >= length then ()
      else
        let line_end =
          try String.index_from text line_start '\n' with Not_found -> length
        in
        let marker = marker_at text line_start line_end in
        match fenced, marker with
        | Some (fence_char, fence_count), Some (char, count, _, marker_end)
          when char = fence_char && count >= fence_count ->
            let rec only_space i = i >= line_end ||
              ((text.[i] = ' ' || text.[i] = '\t') && only_space (i + 1)) in
            if only_space marker_end then
              lines (if line_end < length then line_end + 1 else length) None
            else begin
              lines (if line_end < length then line_end + 1 else length) fenced
            end
        | Some _, _ ->
            lines (if line_end < length then line_end + 1 else length) fenced
        | None, Some (char, count, _, _) ->
            lines (if line_end < length then line_end + 1 else length)
              (Some (char, count))
        | None, None ->
            process_line line_start line_end;
            lines (if line_end < length then line_end + 1 else length) None
    in
    lines 0 None;
    match !output with
    | None -> (text, [])
    | Some buffer ->
        Buffer.add_substring buffer text !copied_until (length - !copied_until);
        (Buffer.contents buffer, List.rev !names)
