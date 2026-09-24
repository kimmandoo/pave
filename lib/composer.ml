type t = {
  mutable text : string;
  mutable cursor : int;
  mutable boundaries : int array;
  mutable boundary_count : int;
  mutable history : string list;
  mutable recall : int option;
  mutable draft : string;
}

let create () = { text = ""; cursor = 0; boundaries = [| 0 |];
  boundary_count = 1; history = []; recall = None; draft = "" }
let text t = t.text
let cursor t = t.cursor

let segment text =
  let _, reverse = Uuseg_string.fold_utf_8 `Grapheme_cluster
    (fun (offset, reverse) cluster ->
      let finish = offset + String.length cluster in
      finish, finish :: reverse) (0, []) text in
  Array.of_list (0 :: List.rev reverse)

let at_or_after boundaries count pos =
  let low = ref 0 and high = ref (count - 1) in
  while !low < !high do
    let mid = (!low + !high) / 2 in
    if boundaries.(mid) < pos then low := mid + 1 else high := mid
  done;
  !low

let previous t =
  let index = at_or_after t.boundaries t.boundary_count t.cursor in
  if index = 0 then 0 else t.boundaries.(index - 1)

let next t =
  let index = at_or_after t.boundaries t.boundary_count t.cursor in
  t.boundaries.(min (t.boundary_count - 1) (index + 1))

let set_at t text pos =
  let boundaries = segment text in
  t.text <- text;
  t.boundaries <- boundaries;
  t.boundary_count <- Array.length boundaries;
  t.cursor <- boundaries.(at_or_after boundaries t.boundary_count pos)

let set t text = set_at t text (String.length text)

let clear t =
  set t "";
  t.recall <- None;
  t.draft <- ""

let insert t value =
  if value <> "" && String.length t.text + String.length value <= 16_384 then (
    if t.cursor = String.length t.text then (
      let start = t.boundaries.(max 0 (t.boundary_count - 2)) in
      let suffix = String.sub t.text start (String.length t.text - start) ^ value in
      let tail = segment suffix in
      let prefix_count = max 1 (t.boundary_count - 1) in
      let count = prefix_count + Array.length tail - 1 in
      if Array.length t.boundaries < count then (
        let grown = Array.make (max count (2 * Array.length t.boundaries)) 0 in
        Array.blit t.boundaries 0 grown 0 t.boundary_count;
        t.boundaries <- grown);
      for i = 1 to Array.length tail - 1 do
        t.boundaries.(prefix_count + i - 1) <- start + tail.(i)
      done;
      t.boundary_count <- count;
      t.text <- t.text ^ value;
      t.cursor <- String.length t.text)
    else (
      let pos = t.cursor + String.length value in
      set_at t (String.sub t.text 0 t.cursor ^ value
        ^ String.sub t.text t.cursor (String.length t.text - t.cursor)) pos);
    t.recall <- None)

let erase t =
  let start = previous t in
  if start <> t.cursor then (
    set_at t (String.sub t.text 0 start
      ^ String.sub t.text t.cursor (String.length t.text - t.cursor)) start;
    t.recall <- None)

let delete t =
  let finish = next t in
  if finish <> t.cursor then (
    set_at t (String.sub t.text 0 t.cursor
      ^ String.sub t.text finish (String.length t.text - finish)) t.cursor;
    t.recall <- None)

let left t = t.cursor <- previous t
let right t = t.cursor <- next t
let home t = t.cursor <- 0
let finish t = t.cursor <- String.length t.text

let older t =
  let index = match t.recall with None -> 0 | Some i -> i + 1 in
  match List.nth_opt t.history index with
  | None -> ()
  | Some value ->
      if t.recall = None then t.draft <- t.text;
      t.recall <- Some index;
      set t value

let newer t =
  match t.recall with
  | None -> ()
  | Some 0 -> t.recall <- None; set t t.draft
  | Some i -> t.recall <- Some (i - 1); set t (List.nth t.history (i - 1))

let submit t =
  if String.trim t.text = "" then None
  else (
    let value = t.text in
    if (match t.history with first :: _ -> first <> value | [] -> true) then (
      t.history <- value :: t.history;
      if List.length t.history > 100 then
        t.history <- List.rev (List.tl (List.rev t.history)));
    clear t;
    Some value)
