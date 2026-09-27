type t = {
  mutable secrets : string list;
  mutable placeholders : (string * string) list;
}

(* Longest-first replacement keeps a shorter secret from splitting a longer one. *)
let ordered secrets =
  secrets
  |> List.filter (fun secret -> secret <> "")
  |> List.sort (fun left right ->
       let by_length = compare (String.length right) (String.length left) in
       if by_length <> 0 then by_length else String.compare left right)
  |> List.fold_left (fun unique secret ->
       if List.mem secret unique then unique else unique @ [secret]) []

let create secrets = { secrets = ordered secrets; placeholders = [] }
let add t secrets = t.secrets <- ordered (t.secrets @ secrets)


let matches_at text offset value =
  let value_length = String.length value in
  let rec equal index =
    index = value_length ||
    (text.[offset + index] = value.[index] && equal (index + 1)) in
  offset >= 0 && offset + value_length <= String.length text && equal 0
let contains text needle =
  let text_length = String.length text and needle_length = String.length needle in
  let rec find at =
    at + needle_length <= text_length &&
    (matches_at text at needle || find (at + 1)) in
  needle_length = 0 || find 0
let starts_with = matches_at
let visible_markers = ["⟦"; "⟧"; "◇"; "◆"; "▣"; "▤"; "⌑"; "⌖";
  "⟪"; "⟫"; "⊙"; "⟁"; "⧈"; "⧫"; "⋄"; "⦿"; "❖"; "∎"]

let private_use code =
  String.init 3 (function
    | 0 -> Char.chr (0xe0 lor (code lsr 12))
    | 1 -> Char.chr (0x80 lor ((code lsr 6) land 0x3f))
    | _ -> Char.chr (0x80 lor (code land 0x3f)))

let safe_markers t =
  let safe marker =
    not (List.exists (fun secret -> contains secret marker) t.secrets) in
  let visible = List.filter safe visible_markers in
  let rec take count values = match count, values with
    | 0, _ | _, [] -> []
    | count, value :: rest -> value :: take (count - 1) rest in
  let visible = take 3 visible in
  if List.length visible = 3 then visible else
  let rec private_markers code markers =
    if List.length markers = 3 then List.rev markers
    else if code > 0xf8ff then
      failwith "secret masking could not generate a safe placeholder"
    else
      let marker = private_use code in
      private_markers (code + 1)
        (if safe marker then marker :: markers else markers) in
  private_markers 0xe000 visible

let repeat value count =
  let buffer = Buffer.create (String.length value * count) in
  for _ = 1 to count do Buffer.add_string buffer value done;
  Buffer.contents buffer



let replace_all text needle replacement =
  let text_length = String.length text and needle_length = String.length needle in
  if needle_length = 0 then text else
  let buffer = Buffer.create text_length in
  let rec copy from =
    if from >= text_length then ()
    else
      let rec find at =
        if at + needle_length > text_length then None
        else if matches_at text at needle then Some at
        else find (at + 1) in
      match find from with
      | None -> Buffer.add_substring buffer text from (text_length - from)
      | Some at ->
          Buffer.add_substring buffer text from (at - from);
          Buffer.add_string buffer replacement;
          copy (at + needle_length) in
  copy 0;
  Buffer.contents buffer

let sort_placeholders placeholders =
  List.sort (fun (left, _) (right, _) ->
    compare (String.length right) (String.length left)) placeholders

let mask t text =
  let buffer = Buffer.create (String.length text) in
  let placeholders = ref t.placeholders in
  let known = ref (sort_placeholders !placeholders) in
  let placeholder secret =
    let markers = safe_markers t in
    let make index = match markers with
      | opening :: middle :: closing :: _ ->
          opening ^ repeat middle (index + 1) ^ closing
      | opening :: middle :: _ ->
          opening ^ repeat middle (index + 1) ^ opening
      | [marker] -> repeat marker (index + 1)
      | [] -> assert false in
    let safe candidate =
      not (List.exists (fun active -> contains candidate active) t.secrets) in
    let rec available index =
      let candidate = make index in
      if contains text candidate || not (safe candidate) ||
         (match List.assoc_opt candidate !placeholders with
          | Some existing -> existing <> secret
          | None -> false)
      then available (index + 1)
      else (
        placeholders := (candidate, secret) ::
          List.remove_assoc candidate !placeholders;
        known := sort_placeholders !placeholders;
        candidate) in
    match List.find_opt (fun (candidate, existing) ->
      existing = secret && not (contains text candidate) && safe candidate)
      !placeholders with
    | Some (candidate, _) -> candidate
    | None -> available 0 in
  let rec copy offset =
    if offset >= String.length text then ()
    else
      match List.find_opt (fun (candidate, _) ->
          starts_with text offset candidate &&
          not (List.exists (fun secret -> contains candidate secret) t.secrets))
        !known with
      | Some (candidate, _) ->
          Buffer.add_string buffer candidate;
          copy (offset + String.length candidate)
      | None ->
          (match List.find_opt (starts_with text offset) t.secrets with
           | Some secret ->
               let candidate = placeholder secret in
               Buffer.add_string buffer candidate;
               copy (offset + String.length secret)
           | None ->
               Buffer.add_char buffer text.[offset];
               copy (offset + 1)) in
  copy 0;
  t.placeholders <- !placeholders;
  Buffer.contents buffer
let rec mask_value t = function
  | `String text -> `String (mask t text)
  | `List values -> `List (List.map (mask_value t) values)
  | `Assoc fields -> `Assoc (List.map (fun (key, value) ->
      key, mask_value t value) fields)
  | value -> value

let mask_tool_arguments = mask_value


let redact t text =
  List.fold_left (fun text secret -> replace_all text secret "[redacted]")
    text t.secrets

let rec restore_value t = function
  | `String text ->
      let text = List.fold_left (fun text (placeholder, secret) ->
        replace_all text placeholder secret) text t.placeholders in
      `String text
  | `List values -> `List (List.map (restore_value t) values)
  | `Assoc fields -> `Assoc (List.map (fun (key, value) ->
      key, restore_value t value) fields)
  | value -> value

let restore_tool_arguments t json = restore_value t json
