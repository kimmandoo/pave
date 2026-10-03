(* Read-only pairing of literal Dart Flutter method/event channel names and
   supported literal method/argument shapes with existing native iOS/Android
   handlers. Nothing here executes a command, evaluates project code or invents
   native declarations; anything outside the supported literal shapes stays
   explicitly unresolved. *)

exception Error of string

let fail message = raise (Error message)

let max_walk_entries = 10_000
let max_source_bytes = 262_144
let max_source_files = 256
let max_report_lines = 96
let max_near_distance = 3

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '$' -> true
  | _ -> false

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let skip_ws_forward text i =
  let n = String.length text in
  let rec go i = if i < n && is_space text.[i] then go (i + 1) else i in
  go i

let skip_ws_back text i =
  let rec go i = if i > 0 && is_space text.[i - 1] then go (i - 1) else i in
  go i

let ident_back text i =
  let rec start j = if j > 0 && is_ident_char text.[j - 1] then start (j - 1)
    else j in
  let lo = start i in
  if lo = i then None else Some (lo, i)

let line_of text index =
  let count = ref 1 in
  for i = 0 to min index (String.length text) - 1 do
    if text.[i] = '\n' then incr count
  done;
  !count

(* Single pass that blanks comment bodies (preserving newlines, so positions
   and line numbers stay valid) and records which indices live inside a string
   literal. Tokens inside comments then cannot match and string contents stay
   readable for literal extraction. Raw/triple-quoted strings are outside the
   supported literal shapes and degrade to unresolved, never mis-parsed. *)
let sanitize text =
  let n = String.length text in
  let bytes = Bytes.of_string text in
  let in_string = Array.make n false in
  let rec go i =
    if i >= n then ()
    else match text.[i] with
      | '/' when i + 1 < n && text.[i + 1] = '/' ->
          let rec eol j =
            if j >= n || text.[j] = '\n' then j
            else (Bytes.set bytes j ' '; eol (j + 1)) in
          Bytes.set bytes i ' ';
          go (eol (i + 1))
      | '/' when i + 1 < n && text.[i + 1] = '*' ->
          Bytes.set bytes i ' '; Bytes.set bytes (i + 1) ' ';
          let rec eoc j =
            if j >= n then ()
            else if text.[j] = '*' && j + 1 < n && text.[j + 1] = '/' then (
              Bytes.set bytes j ' '; Bytes.set bytes (j + 1) ' ';
              go (j + 2))
            else (
              if text.[j] <> '\n' then Bytes.set bytes j ' ';
              eoc (j + 1)) in
          eoc (i + 2)
      | '"' | '\'' as quote ->
          let rec content j =
            if j >= n then ()
            else if text.[j] = '\\' then (
              in_string.(j) <- true;
              if j + 1 < n then in_string.(j + 1) <- true;
              content (j + 2))
            else if text.[j] = quote then go (j + 1)
            else (in_string.(j) <- true; content (j + 1)) in
          content (i + 1)
      | _ -> go (i + 1) in
  go 0;
  Bytes.unsafe_to_string bytes, in_string

(* Bounded token occurrences on sanitized text; matches cannot start inside a
   string literal nor continue an identifier on either side. *)
let find_all ?(mask = [||]) text token =
  let n = String.length text and m = String.length token in
  let found = ref [] in
  let rec search i =
    if i + m > n then ()
    else (
      if String.sub text i m = token then (
        let ok_before = i = 0 || not (is_ident_char text.[i - 1]) in
        let ok_after = i + m = n || not (is_ident_char text.[i + m]) in
        let ok_string = mask = [||] || not mask.(i) in
        if ok_before && ok_after && ok_string then found := i :: !found);
      search (i + 1)) in
  if m > 0 then search 0;
  List.rev !found

let contains_token ?(mask = [||]) text token =
  find_all ~mask text token <> []

type string_result = Literal of string | Unsupported

(* [text.[i]] must be a quote. Interpolated or unterminated literals are
   Unsupported; the second result bounds the literal span. *)
let read_string text i =
  let n = String.length text in
  let quote = text.[i] in
  let buffer = Buffer.create 64 in
  let bad = ref false in
  let rec go i =
    if i >= n then (bad := true; n)
    else
      let c = text.[i] in
      if c = '\\' then
        if i + 1 >= n then (bad := true; n)
        else if text.[i + 1] = '(' then (bad := true; go (i + 2)) (* \( *)
        else (
          (match text.[i + 1] with
           | 'n' -> Buffer.add_char buffer '\n'
           | 't' -> Buffer.add_char buffer '\t'
           | 'r' -> Buffer.add_char buffer '\r'
           | other -> Buffer.add_char buffer other);
          go (i + 2))
      else if c = quote then i + 1
      else if c = '$' && i + 1 < n &&
              (text.[i + 1] = '{' || is_ident_char text.[i + 1]) then
        (bad := true; go (i + 1)) (* Dart/Kotlin $ interpolation *)
      else (Buffer.add_char buffer c; go (i + 1)) in
  let stop = go (i + 1) in
  (if !bad then Unsupported else Literal (Buffer.contents buffer)), stop

let skip_string_forward text i = snd (read_string text i)

(* Split a parenthesised argument list; [open_index] points at '('. Returns the
   argument ranges, whether the list was closed, and the index after ')'. *)
let split_args text open_index =
  let n = String.length text in
  let args = ref [] and arg_start = ref (open_index + 1) in
  let rec go i depth =
    if i >= n || List.length !args > 32 then (false, i)
    else match text.[i] with
      | '(' | '[' | '{' -> go (i + 1) (depth + 1)
      | ')' | ']' | '}' ->
          if depth = 1 then (
            if !args <> [] ||
               String.trim (String.sub text !arg_start (i - !arg_start)) <> ""
            then args := (!arg_start, i) :: !args;
            (true, i + 1))
          else if depth <= 0 then (false, i)
          else go (i + 1) (depth - 1)
      | ',' when depth = 1 ->
          args := (!arg_start, i) :: !args;
          arg_start := i + 1;
          go (i + 1) depth
      | '"' | '\'' -> go (skip_string_forward text i) depth
      | '/' when i + 1 < n && text.[i + 1] = '/' ->
          let rec eol j = if j >= n || text.[j] = '\n' then j else eol (j + 1) in
          go (eol (i + 2)) depth
      | '/' when i + 1 < n && text.[i + 1] = '*' ->
          let rec eoc j =
            if j + 1 >= n then n
            else if text.[j] = '*' && text.[j + 1] = '/' then j + 2
            else eoc (j + 1) in
          go (eoc (i + 2)) depth
      | _ -> go (i + 1) depth in
  let closed, after = go (open_index + 1) 1 in
  (List.rev !args, closed, after)

(* The range must contain exactly one string literal. *)
let literal_arg text (lo, hi) =
  let i = skip_ws_forward text lo in
  let i = if i < hi && text.[i] = '@' then skip_ws_forward text (i + 1) else i in
  if i < hi && (text.[i] = '"' || text.[i] = '\'') then
    match read_string text i with
    | Literal value, stop when skip_ws_forward text stop >= hi -> Some value
    | _ -> None
  else None

(* A `name:`/`name =` labelled argument wins; otherwise the positional index. *)
let channel_name_arg positional text args =
  let labelled = List.find_map (fun (lo, hi) ->
    let i = skip_ws_forward text lo in
    if i + 4 <= hi && String.sub text i 4 = "name" &&
       (i + 4 = hi || not (is_ident_char text.[i + 4])) then
      let j = skip_ws_forward text (i + 4) in
      if j < hi && (text.[j] = ':' || text.[j] = '=') &&
         (j + 1 >= hi || text.[j + 1] <> '=') then
        Some (skip_ws_forward text (j + 1), hi)
      else None
    else None) args in
  match labelled with
  | Some range -> literal_arg text range
  | None ->
      (match List.nth_opt args positional with
       | Some range -> literal_arg text range
       | None -> None)

(* `x = MethodChannel(..)`, `this._x = ..`, `new MethodChannel(..)` *)
let binding_before text i =
  let i = match ident_back text (skip_ws_back text i) with
    | Some (lo, hi) when String.sub text lo (hi - lo) = "new" -> lo
    | _ -> i in
  let j = skip_ws_back text i in
  if j > 0 && text.[j - 1] = '=' &&
     (j < 2 || not (List.mem text.[j - 2] ['='; '<'; '>'; '!'])) then
    let k = skip_ws_back text (j - 1) in
    match ident_back text k with
    | Some (lo, hi) -> Some (String.sub text lo (hi - lo))
    | None -> None
  else None

(* Objective-C: `[FlutterMethodChannel methodChannelWithName:..]` binds to the
   receiver variable via the '=' before the enclosing '['. *)
let bracket_binding_before text i =
  let rec scan j =
    if j <= 0 || i - j > 512 then None
    else match text.[j - 1] with
      | '[' -> binding_before text (j - 1)
      | ']' | ';' | '{' | '}' -> None
      | _ -> scan (j - 1) in
  scan i

(* Match the ')' at index [close] back to its '('. Bounded by file size; string
   interiors are not skipped, so a literal ')' could confuse it -- callers only
   use this for inline channel receivers where mis-pairing stays unresolved. *)
let match_paren_back text close =
  let rec go i depth =
    if i < 0 then None
    else match text.[i] with
      | ')' | ']' | '}' -> go (i - 1) (depth + 1)
      | '(' -> if depth = 0 then Some i else go (i - 1) (depth - 1)
      | '[' | '{' -> go (i - 1) (depth - 1)
      | _ -> go (i - 1) depth in
  go (close - 1) 0

type channel_kind = Method | Event

let kind_name = function Method -> "method" | Event -> "event"

type arg_shape =
  | Shape_map | Shape_list | Shape_scalar | Shape_null | Shape_absent
  | Shape_unsupported

let shape_label = function
  | Shape_map -> "map arguments"
  | Shape_list -> "list arguments"
  | Shape_scalar -> "scalar arguments"
  | Shape_null -> "null arguments"
  | Shape_absent -> "no arguments"
  | Shape_unsupported -> "unsupported argument expression"

type dart_decl = {
  dd_kind : channel_kind;
  dd_name : string option;
  dd_file : string;
  dd_line : int;
  dd_implicit : bool;
}

type dart_call = {
  dc_method : string option;
  dc_shape : arg_shape;
  dc_kind : channel_kind option;
  dc_name : string option;
  dc_file : string;
  dc_line : int;
}

type native_expect = Expect_map | Expect_list | Expect_scalar
  | Expect_none | Expect_unknown

let expect_label = function
  | Expect_map -> "expects map arguments"
  | Expect_list -> "expects list arguments"
  | Expect_scalar -> "expects scalar arguments"
  | Expect_none -> "reads no arguments"
  | Expect_unknown -> "argument expectation unresolved"

type native_handler = {
  nh_methods : string list;
  nh_body_unparsed : bool;
  nh_expect : native_expect;
}

type native_decl = {
  nd_kind : channel_kind;
  nd_name : string option;
  nd_file : string;
  nd_line : int;
  nd_binding : string option;
  nd_handler : native_handler option;
  nd_handler_ambiguous : bool;
  nd_in_root : bool;
}

let dart_arg_shape text (lo, hi) =
  let rec head i =
    if i >= hi then `end_
    else if is_space text.[i] then head (i + 1)
    else if i + 5 <= hi && String.sub text i 5 = "const" &&
            (i + 5 = hi || not (is_ident_char text.[i + 5])) then
      head (skip_ws_forward text (i + 5))
    else `at i in
  match head lo with
  | `end_ -> Shape_absent
  | `at i ->
      (match text.[i] with
       | '{' -> Shape_map
       | '[' -> Shape_list
       | '<' ->
           (* `const <K, V>{..}` / `<T>[..]` literal type arguments *)
           let rec fwd j =
             if j >= hi then Shape_unsupported
             else if text.[j] = '{' then Shape_map
             else if text.[j] = '[' then Shape_list
             else fwd (j + 1) in
           fwd (i + 1)
       | '"' | '\'' ->
           (match read_string text i with
            | Literal _, stop when skip_ws_forward text stop >= hi ->
                Shape_scalar
            | _ -> Shape_unsupported)
       | '0' .. '9' | '-' | '+' -> Shape_scalar
       | _ ->
           let word w = i + String.length w <= hi &&
             String.sub text i (String.length w) = w &&
             (i + String.length w = hi ||
              not (is_ident_char text.[i + String.length w])) in
           if word "true" || word "false" then Shape_scalar
           else if word "null" then Shape_null
           else Shape_unsupported)

(* `receiver.invokeMethod`: a declared variable or an inline literal channel. *)
let resolve_dart_receiver bindings text i =
  if i > 0 && text.[i - 1] = '.' then
    let k = i - 2 in
    if k >= 0 && text.[k] = ')' then
      match match_paren_back text k with
      | Some open_index ->
          (match ident_back text (skip_ws_back text open_index) with
           | Some (lo, hi) ->
               let ctor = String.sub text lo (hi - lo) in
               if ctor = "MethodChannel" || ctor = "EventChannel" then
                 let args, closed, _ = split_args text open_index in
                 if not closed then `unresolved
                 else `resolved
                   ((if ctor = "MethodChannel" then Method else Event),
                    channel_name_arg 0 text args)
               else `unresolved
           | None -> `unresolved)
      | None -> `unresolved
    else
      match ident_back text (i - 1) with
      | Some (lo, hi) ->
          (match Hashtbl.find_opt bindings
                   (String.sub text lo (hi - lo)) with
           | Some (kind, name) -> `resolved (kind, name)
           | None -> `unresolved)
      | None -> `unresolved
  else `unresolved

let parse_dart_file path text =
  let clean, mask = sanitize text in
  let decls = ref [] and calls = ref [] and notes = ref [] in
  let bindings = Hashtbl.create 8 in
  let ambiguous = Hashtbl.create 8 in
  let record_binding id entry =
    if Hashtbl.mem bindings id then (
      Hashtbl.remove bindings id;
      Hashtbl.replace ambiguous id ())
    else if not (Hashtbl.mem ambiguous id) then
      Hashtbl.replace bindings id entry in
  List.iter (fun (token, kind) ->
    List.iter (fun i ->
      let after = skip_ws_forward clean (i + String.length token) in
      if after < String.length clean && clean.[after] = '(' then (
        let args, closed, _ = split_args clean after in
        let name = if closed then channel_name_arg 0 clean args else None in
        (match binding_before clean i with
         | Some id -> record_binding id (kind, name)
         | None -> ());
        if name = None then
          notes := Printf.sprintf
            "unresolved: computed or unsupported %s name at %s:%d"
            token path (line_of text i) :: !notes;
        decls := { dd_kind = kind; dd_name = name; dd_file = path;
                   dd_line = line_of text i; dd_implicit = false } :: !decls))
      (find_all ~mask clean token))
    ["MethodChannel", Method; "EventChannel", Event];
  List.iter (fun token ->
    List.iter (fun i ->
      let receiver = resolve_dart_receiver bindings clean i in
      let j = skip_ws_forward clean (i + String.length token) in
      let j =
        if j < String.length clean && clean.[j] = '<' then
          let rec close k depth =
            if k >= String.length clean || depth = 0 then k
            else if clean.[k] = '<' then close (k + 1) (depth + 1)
            else if clean.[k] = '>' then close (k + 1) (depth - 1)
            else close (k + 1) depth in
          skip_ws_forward clean (close (j + 1) 1)
        else j in
      if j < String.length clean && clean.[j] = '(' then (
        let args, closed, _ = split_args clean j in
        let method_name =
          if closed then
            match args with
            | first :: _ -> literal_arg clean first
            | [] -> None
          else None in
        let shape =
          if not closed then Shape_unsupported
          else match args with
            | _ :: second :: _ -> dart_arg_shape clean second
            | _ -> Shape_absent in
        (match receiver with
         | `resolved (kind, name) ->
             calls := { dc_method = method_name; dc_shape = shape;
                        dc_kind = Some kind; dc_name = name;
                        dc_file = path; dc_line = line_of text i } :: !calls
         | `unresolved ->
             calls := { dc_method = method_name; dc_shape = shape;
                        dc_kind = None; dc_name = None;
                        dc_file = path; dc_line = line_of text i } :: !calls;
             notes := Printf.sprintf
               "unresolved: %s receiver at %s:%d is not a literal channel"
               token path (line_of text i) :: !notes);
        if method_name = None then
          notes := Printf.sprintf
            "unresolved: computed %s method name at %s:%d"
            token path (line_of text i) :: !notes))
      (find_all ~mask clean token))
    ["invokeMethod"; "invokeMapMethod"; "invokeListMethod"];
  List.rev !decls, List.rev !calls, List.rev !notes

(* --- native handlers ---------------------------------------------------- *)

type native_lang = Swift | Objc | Kotlin | Java

(* Body region `{ .. }` of a handler argument. Stops at ';', a newline at paren
   depth 0 or an unbalanced ')' so an unrelated later brace cannot be mistaken
   for the handler body. *)
let handler_region text start =
  let n = String.length text in
  let rec find_open i depth =
    if i >= n then None
    else match text.[i] with
      | '{' -> Some i
      | ';' when depth = 0 -> None
      | '\n' when depth = 0 -> None
      | ')' when depth = 0 -> None
      | '(' | '[' -> find_open (i + 1) (depth + 1)
      | ')' | ']' -> find_open (i + 1) (depth - 1)
      | '"' | '\'' -> find_open (skip_string_forward text i) depth
      | _ -> find_open (i + 1) depth in
  match find_open start 0 with
  | None -> None
  | Some open_index ->
      let rec match_close i depth =
        if i >= n then None
        else match text.[i] with
          | '{' -> match_close (i + 1) (depth + 1)
          | '}' ->
              if depth = 1 then Some i else match_close (i + 1) (depth - 1)
          | '"' | '\'' -> match_close (skip_string_forward text i) depth
          | _ -> match_close (i + 1) depth in
      match match_close (open_index + 1) 1 with
      | Some close_index -> Some (open_index, close_index)
      | None -> Some (open_index, n)

(* Whether `switch`/`when` precedes this `call.method` occurrence. *)
let dispatch_before region i =
  let j = skip_ws_back region i in
  let j = if j > 0 && region.[j - 1] = '(' then skip_ws_back region (j - 1)
    else j in
  match ident_back region j with
  | Some (lo, hi) ->
      List.mem (String.sub region lo (hi - lo)) ["switch"; "when"]
  | None -> false

(* Literal method names compared against call.method inside a handler body. *)
let region_methods region mask =
  let methods = ref [] in
  let add i =
    let j = skip_ws_forward region i in
    if j < String.length region &&
       (region.[j] = '"' || region.[j] = '\'') then
      match read_string region j with
      | Literal value, _ -> methods := value :: !methods
      | _ -> () in
  List.iter (fun i ->
    let after = skip_ws_forward region (i + String.length "call.method") in
    if after + 1 < String.length region &&
       region.[after] = '=' && region.[after + 1] = '=' then
      add (after + 2)
    else if after + 7 <= String.length region &&
            String.sub region after 7 = ".equals" then
      let p = skip_ws_forward region (after + 7) in
      if p < String.length region && region.[p] = '(' then (
        let args, closed, _ = split_args region p in
        if closed then
          match args with
          | [range] -> (match literal_arg region range with
              | Some value -> methods := value :: !methods
              | None -> ())
          | _ -> ()))
    (find_all ~mask region "call.method");
  (* Reversed comparisons: "x" == call.method *)
  List.iter (fun i ->
    if not (dispatch_before region i) then
      let before = skip_ws_back region i in
      if before >= 2 && region.[before - 1] = '=' &&
         region.[before - 2] = '=' then
        let s_end = skip_ws_back region (before - 2) in
        if s_end > 0 &&
           (region.[s_end - 1] = '"' || region.[s_end - 1] = '\'') then
          let quote = region.[s_end - 1] in
          let rec find_start j =
            if j <= 0 || s_end - j > 1024 then None
            else if region.[j - 1] = quote then Some (j - 1)
            else find_start (j - 1) in
          match find_start (s_end - 1) with
          | Some start ->
              (match read_string region start with
               | Literal value, stop when stop = s_end ->
                   methods := value :: !methods
               | _ -> ())
          | None -> ())
    (find_all ~mask region "call.method");
  (* `case "m":` / `"m" ->` only inside a switch/when on call.method *)
  if List.exists (dispatch_before region)
       (find_all ~mask region "call.method") then (
    List.iter (fun i -> add (i + 4)) (find_all ~mask region "case");
    String.iteri (fun i c ->
      if c = '"' && not mask.(i) then
        match read_string region i with
        | Literal value, stop ->
            let j = skip_ws_forward region stop in
            if j + 1 < String.length region &&
               region.[j] = '-' && region.[j + 1] = '>' then
              methods := value :: !methods
        | _ -> ()) region);
  List.sort_uniq String.compare !methods

let rec find_sub ?(mask = [||]) haystack needle from =
  let n = String.length haystack and m = String.length needle in
  if from + m > n then None
  else if String.sub haystack from m = needle &&
          (mask = [||] || not mask.(from)) then Some from
  else find_sub ~mask haystack needle (from + 1)

let has ~mask region token = find_sub ~mask region token 0 <> None

(* All substring occurrences (no identifier boundary check); used where a
   shared prefix like call.argument/call.arguments is intentional. *)
let find_all_sub ?(mask = [||]) text token =
  let n = String.length text and m = String.length token in
  let found = ref [] in
  let rec search i =
    if i + m > n then ()
    else (
      if String.sub text i m = token &&
         (mask = [||] || not mask.(i)) then found := i :: !found;
      search (i + 1)) in
  if m > 0 then search 0;
  List.rev !found

(* Classify what a handler body expects from call.arguments/call.argument.
   Only documented literal usage shapes are recognized; anything else stays
   Expect_unknown rather than guessed. *)
let region_expectation region mask =
  let ev_map = ref false and ev_list = ref false
  and ev_scalar = ref false and ev_unknown = ref false in
  let apply = function
    | `map -> ev_map := true
    | `list -> ev_list := true
    | `scalar -> ev_scalar := true
    | `unknown | `none -> ev_unknown := true in
  (* `[K: V]` dictionary casts contain a top-level ':'; list casts do not. *)
  let bracket_is_map i =
    let j = skip_ws_forward region i in
    if j < String.length region && region.[j] = '[' then
      let rec scan k depth colon =
        if k >= String.length region || depth = 0 then colon
        else match region.[k] with
          | '[' | '<' -> scan (k + 1) (depth + 1) colon
          | ']' | '>' -> scan (k + 1) (depth - 1) colon
          | ':' when depth = 1 -> scan (k + 1) depth true
          | _ -> scan (k + 1) depth colon in
      Some (scan (j + 1) 1 false)
    else None in
  let word_kind word =
    match word with
    | "Map" | "Dictionary" | "NSDictionary" | "HashMap"
    | "NSMutableDictionary" -> `map
    | "List" | "ArrayList" | "NSArray" | "Array"
    | "NSMutableArray" -> `list
    | _ -> `scalar in
  (* classify a type expression starting at index i *)
  let cast_target i =
    let j = skip_ws_forward region i in
    if j >= String.length region then `unknown
    else if region.[j] = '[' then
      (match bracket_is_map i with
       | Some true -> `map
       | Some false -> `list
       | None -> `unknown)
    else if is_ident_char region.[j] then
      let rec fwd k =
        if k < String.length region && is_ident_char region.[k]
        then fwd (k + 1) else k in
      let hi = fwd j in
      word_kind (String.sub region j (hi - j))
    else `unknown in
  (* `as`, `as?`, `as!` cast immediately after index i *)
  let as_cast i =
    let j = skip_ws_forward region i in
    if j + 1 < String.length region && String.sub region j 2 = "as" &&
       (j + 2 = String.length region || not (is_ident_char region.[j + 2]))
    then
      let k = skip_ws_forward region (j + 2) in
      let k = if k < String.length region &&
                 (region.[k] = '?' || region.[k] = '!') then k + 1 else k in
      `cast (cast_target k)
    else `not_a_cast in
  (* `(Type) call.arguments` C/Java-style cast ending right before index i;
     the paren contents must be a bare type name, optionally pointer-starred. *)
  let cast_before i =
    let j = skip_ws_back region i in
    if j > 0 && region.[j - 1] = ')' then
      match match_paren_back region (j - 1) with
      | Some open_index ->
          let inner_lo = skip_ws_forward region (open_index + 1) in
          let inner_hi = skip_ws_back region (j - 1) in
          (* strip trailing '*' *)
          let rec strip_star k =
            if k > inner_lo && region.[k - 1] = '*' then
              strip_star (skip_ws_back region (k - 1))
            else k in
          let inner_hi = strip_star inner_hi in
          (match ident_back region inner_hi with
           | Some (lo, hi) when lo = inner_lo ->
               (* `(T)x`: the char before '(' must not be identifier/dot —
                  otherwise it is a call like foo.map(...) *)
               if open_index > 0 &&
                  (is_ident_char region.[open_index - 1] ||
                   region.[open_index - 1] = '.') then `none
               else `cast (word_kind (String.sub region lo (hi - lo)))
           | _ -> `none)
      | None -> `none
    else `none in
  let n = String.length region in
  List.iter (fun i ->
    let after = i + String.length "call.argument" in
    if after < n then
      if region.[after] = 's' &&
         (after + 1 = n || not (is_ident_char region.[after + 1])) then (
        (* call.arguments <usage> *)
        (match cast_before i with
         | `cast kind -> apply kind
         | `none ->
             (match as_cast (after + 1) with
              | `cast kind -> apply kind
              | `not_a_cast ->
                  let j = skip_ws_forward region (after + 1) in
                  if j >= n then ev_unknown := true
                  else match region.[j] with
                    | '(' | '!' | '?' | '.' | '<' -> ev_scalar := true
                    | _ -> ev_unknown := true)))
      else if not (is_ident_char region.[after]) then (
        (* call.argument("key") / call.argument<T>("key") are keyed map
           access; call.argument as? T is a cast. *)
        (match as_cast after with
         | `cast kind -> apply kind
         | `not_a_cast ->
             let j = skip_ws_forward region after in
             if j < n && (region.[j] = '(' || region.[j] = '<')
             then ev_map := true
             else ev_unknown := true)))
    (find_all_sub ~mask region "call.argument");
  if !ev_map then Expect_map
  else if !ev_list then Expect_list
  else if !ev_scalar then Expect_scalar
  else if !ev_unknown then Expect_unknown
  else Expect_none

let parse_native_file ~in_root lang path text =
  let clean, mask = sanitize text in
  let constructors, positional, objc = match lang with
    | Swift -> ["FlutterMethodChannel", Method;
                "FlutterEventChannel", Event], 0, false
    | Kotlin | Java ->
        ["MethodChannel", Method; "EventChannel", Event], 1, false
    | Objc -> ["methodChannelWithName:", Method;
               "eventChannelWithName:", Event], 0, true in
  let bindings = Hashtbl.create 8 in
  let decls = List.concat_map (fun (token, kind) ->
    List.filter_map (fun i ->
      if objc then (
        (* [FlutterMethodChannel methodChannelWithName:@"x" binaryMessenger:m] *)
        let j = skip_ws_forward clean (i + String.length token) in
        let name =
          if j < String.length clean && clean.[j] = '@' then
            let k = skip_ws_forward clean (j + 1) in
            if k < String.length clean &&
               (clean.[k] = '"' || clean.[k] = '\'') then
              match read_string clean k with
              | Literal value, stop ->
                  let t = skip_ws_forward clean stop in
                  if t < String.length clean &&
                     (match clean.[t] with
                      | ' ' | '\t' | ']' -> true | _ -> false)
                  then Some value
                  else None
              | _ -> None
            else None
          else None in
        let binding = bracket_binding_before clean i in
        Option.iter (fun id -> Hashtbl.replace bindings id (kind, name))
          binding;
        Some { nd_kind = kind; nd_name = name; nd_file = path;
               nd_line = line_of text i; nd_binding = binding;
               nd_handler = None; nd_handler_ambiguous = false;
               nd_in_root = in_root })
      else
        let j = skip_ws_forward clean (i + String.length token) in
        if j < String.length clean && clean.[j] = '(' then (
          let args, closed, _ = split_args clean j in
          let name = if closed then channel_name_arg positional clean args
            else None in
          let binding = binding_before clean i in
          Option.iter (fun id -> Hashtbl.replace bindings id (kind, name))
            binding;
          Some { nd_kind = kind; nd_name = name; nd_file = path;
                 nd_line = line_of text i; nd_binding = binding;
                 nd_handler = None; nd_handler_ambiguous = false;
                 nd_in_root = in_root })
        else None)
      (find_all ~mask clean token)) constructors in
  let handlers = List.concat_map (fun (token, kind) ->
    List.map (fun i ->
      let receiver =
        if i > 0 && clean.[i - 1] = '.' then
          match ident_back clean (i - 1) with
          | Some (lo, hi) -> Some (String.sub clean lo (hi - lo))
          | None -> None
        else
          match ident_back clean (skip_ws_back clean i) with
          | Some (lo, hi) -> Some (String.sub clean lo (hi - lo))
          | None -> None in
      let parsed = match handler_region clean (i + String.length token) with
        | Some (lo, hi) ->
            let body = String.sub clean lo (hi - lo) in
            let body_mask = Array.sub mask lo (hi - lo) in
            Some { nh_methods = region_methods body body_mask;
                   nh_body_unparsed = false;
                   nh_expect = region_expectation body body_mask }
        | None ->
            (* `setMethodCallHandler(handler)` -- a handler exists but its body
               is not a supported literal form. *)
            Some { nh_methods = []; nh_body_unparsed = true;
                   nh_expect = Expect_unknown } in
      kind, receiver, parsed, line_of text i)
      (find_all ~mask clean token))
    ["setMethodCallHandler", Method; "setStreamHandler", Event] in
  let attach decl =
    let same_kind = List.filter (fun (kind, _, _, _) ->
      kind = decl.nd_kind) handlers in
    match List.filter (fun (_, receiver, _, _) ->
      receiver <> None && receiver = decl.nd_binding) same_kind with
    | [single] -> Some single, false
    | _ :: _ :: _ -> None, true
    | [] ->
        (match List.filter (fun (_, receiver, _, _) ->
           receiver = None) same_kind with
         | [single] -> Some single, true
         | _ :: _ :: _ -> None, true
         | [] -> None, false) in
  List.map (fun decl ->
    match attach decl with
    | Some (_, _, parsed, _), ambiguous ->
        { decl with nd_handler = parsed; nd_handler_ambiguous = ambiguous }
    | None, ambiguous -> { decl with nd_handler_ambiguous = ambiguous })
    decls

(* --- pairing ------------------------------------------------------------ *)

(* Bounded Levenshtein with an early distance bound for near-name hints. *)
let near_name a b =
  let la = String.length a and lb = String.length b in
  if abs (la - lb) > max_near_distance || la > 128 || lb > 128 then false
  else
    let prev = Array.init (lb + 1) (fun j -> j) in
    let cur = Array.make (lb + 1) 0 in
    let rec rows i =
      if i > la then prev.(lb) <= max_near_distance
      else (
        cur.(0) <- i;
        for j = 1 to lb do
          let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
          cur.(j) <- min (min (prev.(j) + 1) (cur.(j - 1) + 1))
                       (prev.(j - 1) + cost)
        done;
        Array.blit cur 0 prev 0 (lb + 1);
        rows (i + 1)) in
    rows 1

let skip_dir = function
  | ".git" | ".hg" | ".svn" | "_build" | "build" | ".build" | "dist"
  | "node_modules" | "DerivedData" | ".gradle" | ".dart_tool" | "Pods"
  | ".symlinks" | "ephemeral" -> true
  | _ -> false

let native_lang name =
  if Filename.check_suffix name ".swift" then Some Swift
  else if Filename.check_suffix name ".m" ||
          Filename.check_suffix name ".mm" ||
          Filename.check_suffix name ".h" then Some Objc
  else if Filename.check_suffix name ".kt" ||
          Filename.check_suffix name ".kts" then Some Kotlin
  else if Filename.check_suffix name ".java" then Some Java
  else None

(* Bounded workspace walk: lstat only, never follows symlinks, skips generated
   and dependency directories, and stops after [max_walk_entries] entries. *)
let scan_workspace ~root ~subroot =
  let dart_files = ref [] and native_files = ref [] in
  let directories = Hashtbl.create 64 in
  let entries = ref 0 and truncated = ref false in
  let prefix = if subroot = "" then "" else subroot ^ "/" in
  let in_subroot relative =
    prefix = "" ||
    String.length relative > String.length prefix &&
    String.sub relative 0 (String.length prefix) = prefix in
  let in_host relative =
    if not (in_subroot relative) then false
    else
      let rest = if prefix = "" then relative
        else String.sub relative (String.length prefix)
          (String.length relative - String.length prefix) in
      match String.split_on_char '/' rest with
      | ("ios" | "android") :: _ -> true
      | _ -> false in
  let rec visit absolute relative =
    if !truncated then ()
    else
      let dir = Unix.opendir absolute in
      let names = Fun.protect ~finally:(fun () -> Unix.closedir dir)
        (fun () ->
          let rec collect acc =
            if !entries >= max_walk_entries then (truncated := true; acc)
            else match Unix.readdir dir with
              | exception End_of_file -> acc
              | "." | ".." -> collect acc
              | name -> incr entries; collect (name :: acc) in
          collect []) in
      List.iter (fun name ->
        if !truncated then ()
        else
          let child_abs = Filename.concat absolute name in
          let child_rel = if relative = "" then name
            else relative ^ "/" ^ name in
          try
            match (Unix.lstat child_abs).Unix.st_kind with
            | Unix.S_DIR when not (skip_dir name) ->
                Hashtbl.replace directories child_rel ();
                visit child_abs child_rel
            | Unix.S_REG ->
                if Filename.check_suffix name ".dart" && in_subroot child_rel
                then dart_files := child_rel :: !dart_files
                else
                  (match native_lang name with
                   | Some lang ->
                       native_files := (child_rel, lang, in_host child_rel)
                         :: !native_files
                   | None -> ())
            | _ -> ()
          with Unix.Unix_error (Unix.ENOENT, _, _) -> ()) names in
  visit root "";
  List.rev !dart_files, List.rev !native_files, directories, !truncated

let read_source ~root relative =
  try
    let absolute = Workspace_path.checked_path root relative in
    let stat = Unix.lstat absolute in
    if stat.Unix.st_kind <> Unix.S_REG ||
       stat.Unix.st_size > max_source_bytes then `skipped
    else `text (Workspace_path.read_bounded absolute max_source_bytes)
  with Unix.Unix_error _ | Workspace_path.Error _ | Sys_error _ -> `skipped

type pairing =
  | Paired of native_decl
  | Kind_mismatch of native_decl
  | Name_mismatch of native_decl
  | Out_of_root of native_decl
  | Missing_handler

type call_verdict =
  | Consistent
  | Mismatch of string
  | Unresolved of string

let compare_call ~call ~handler =
  let method_part = match call.dc_method, handler.nh_methods with
    | None, _ -> `unresolved "method name is not a literal"
    | Some _, [] when handler.nh_body_unparsed ->
        `unresolved "native handler body is not a supported literal form"
    | Some _, [] ->
        `unresolved "native dispatch has no literal method cases"
    | Some name, methods ->
        if List.mem name methods then `method_ok
        else `mismatch (Printf.sprintf
          "Dart calls method '%s' but no native literal case handles it" name) in
  let shape_part = match call.dc_shape, handler.nh_expect with
    | Shape_unsupported, _ ->
        `unresolved "Dart argument shape is not a literal"
    | _, Expect_unknown ->
        `unresolved "native argument use is not a supported shape"
    | (Shape_absent | Shape_null), (Expect_none | Expect_map) -> `shape_ok
    | Shape_absent, (Expect_list | Expect_scalar) ->
        `mismatch "Dart sends no arguments but the native handler expects values"
    | Shape_null, (Expect_list | Expect_scalar) ->
        `mismatch "Dart sends null but the native handler expects a collection or scalar"
    | (Shape_map | Shape_list | Shape_scalar), Expect_none ->
        `mismatch "Dart sends arguments but the native handler reads none"
    | Shape_map, Expect_map | Shape_list, Expect_list
    | Shape_scalar, Expect_scalar -> `shape_ok
    | Shape_map, (Expect_list | Expect_scalar) ->
        `mismatch "Dart sends a map but the native handler expects a different shape"
    | Shape_list, (Expect_map | Expect_scalar) ->
        `mismatch "Dart sends a list but the native handler expects a different shape"
    | Shape_scalar, (Expect_map | Expect_list) ->
        `mismatch "Dart sends a scalar but the native handler expects a collection" in
  match method_part, shape_part with
  | `method_ok, `shape_ok -> Consistent
  | `method_ok, `unresolved why | `unresolved why, `shape_ok -> Unresolved why
  | `unresolved method_why, `unresolved shape_why ->
      Unresolved (method_why ^ "; " ^ shape_why)
  | `method_ok, `mismatch why | `mismatch why, `shape_ok -> Mismatch why
  | `mismatch method_why, `mismatch shape_why ->
      Mismatch (method_why ^ "; " ^ shape_why)
  | `mismatch why, `unresolved _ | `unresolved _, `mismatch why ->
      Mismatch why

type analysis = {
  a_decl : dart_decl -> pairing list;
  a_calls : dart_call list;
  a_tests : string -> string list -> string list;
}

let analyze ~root ~subroot =
  try
    let root = Workspace_path.root_path root in
    let subroot =
      if subroot = "" || subroot = "." then ""
      else
        let parts = String.split_on_char '/' subroot in
        if String.contains subroot '\000' ||
           not (Filename.is_relative subroot) ||
           List.exists (fun part -> part = "" || part = "." || part = "..")
             parts then
          fail "expected an exact workspace-relative Flutter package root";
        let checked = Workspace_path.checked_path root subroot in
        if (Unix.lstat checked).Unix.st_kind <> Unix.S_DIR then
          fail "selected Flutter package root is not a directory";
        subroot in
    let dart_paths, native_paths, directories, walk_truncated =
      scan_workspace ~root ~subroot in
    let ios_root = Hashtbl.mem directories
      (if subroot = "" then "ios" else subroot ^ "/ios") in
    let android_root = Hashtbl.mem directories
      (if subroot = "" then "android" else subroot ^ "/android") in
    let budget = ref max_source_files and unreadable = ref 0 in
    let read path =
      if !budget <= 0 then (incr unreadable; None)
      else match read_source ~root path with
        | `text text -> decr budget; Some text
        | `skipped -> incr unreadable; None in
    let decls = ref [] and calls = ref [] and notes = ref [] in
    List.iter (fun path ->
      match read path with
      | Some text ->
          let d, c, n = parse_dart_file path text in
          decls := d @ !decls; calls := c @ !calls; notes := n @ !notes
      | None -> ()) dart_paths;
    let natives = ref [] in
    List.iter (fun (path, lang, in_root) ->
      match read path with
      | Some text ->
          natives := parse_native_file ~in_root lang path text @ !natives
      | None -> ()) native_paths;
    let in_root_natives = List.filter (fun d -> d.nd_in_root) !natives in
    let out_root_natives = List.filter (fun d -> not d.nd_in_root) !natives in
    (* Inline literal channel uses without an explicit declaration still pair
       on their literal name. *)
    let known = Hashtbl.create 16 in
    List.iter (fun d ->
      match d.dd_name with
      | Some name -> Hashtbl.replace known (d.dd_kind, name) ()
      | None -> ()) !decls;
    let decls = !decls @ List.filter_map (fun call ->
      match call.dc_kind, call.dc_name with
      | Some kind, Some name when not (Hashtbl.mem known (kind, name)) ->
          Hashtbl.replace known (kind, name) ();
          Some { dd_kind = kind; dd_name = Some name;
                 dd_file = call.dc_file; dd_line = call.dc_line;
                 dd_implicit = true }
      | _ -> None) !calls in
    let pair decl =
      match decl.dd_name with
      | None -> []
      | Some name ->
          let same_name other = other.nd_name = Some name in
          let paired = List.filter (fun n ->
            same_name n && n.nd_kind = decl.dd_kind) in_root_natives in
          if paired <> [] then List.map (fun n -> Paired n) paired
          else
            let kind_hits = List.filter same_name in_root_natives in
            if kind_hits <> [] then
              List.map (fun n -> Kind_mismatch n) kind_hits
            else
              (match List.find_opt (fun n ->
                 n.nd_kind = decl.dd_kind &&
                 (match n.nd_name with
                  | Some other -> near_name name other
                  | None -> false)) in_root_natives with
               | Some native -> [Name_mismatch native]
               | None ->
                   let outside = List.filter same_name out_root_natives in
                   if outside <> [] then
                     List.map (fun n -> Out_of_root n) outside
                   else [Missing_handler]) in
    let test_paths = List.filter (fun path ->
      if subroot = "" then String.starts_with ~prefix:"test/" path
      else
        String.length path > String.length subroot + 5 &&
        String.sub path (String.length subroot + 1) 5 = "test/") dart_paths in
    let suggested_tests name methods =
      let needles = ("'" ^ name ^ "'") :: ("\"" ^ name ^ "\"") ::
        List.concat_map (fun m ->
          ["'" ^ m ^ "'"; "\"" ^ m ^ "\""]) methods in
      List.filter_map (fun path ->
        match read_source ~root path with
        | `text text ->
            let clean, mask = sanitize text in
            if List.exists (fun needle ->
                 contains_token ~mask clean needle) needles then Some path
            else None
        | _ -> None) test_paths
      |> List.sort String.compare in
    decls, List.rev !calls, !notes, in_root_natives, ios_root, android_root,
    walk_truncated, !unreadable,
    { a_decl = pair; a_calls = List.rev !calls; a_tests = suggested_tests },
    subroot
  with
  | Error _ | Workspace_path.Error _ as e -> raise e
  | Unix.Unix_error (error, _, _) ->
      fail ("Flutter channel scan unavailable: " ^ Unix.error_message error)
  | Sys_error message -> fail message

let report_lines ~root ~subroot =
  let (decls, calls, notes, in_root_natives, ios_root, android_root,
       walk_truncated, unreadable, analysis, _) =
    analyze ~root ~subroot in
  ignore calls;
  let lines = ref [] in
  let add line =
    if List.length !lines < max_report_lines then lines := line :: !lines in
  let cite file line = Printf.sprintf "%s:%d" file line in
  let cite_dart decl = cite decl.dd_file decl.dd_line in
  let cite_native decl = cite decl.nd_file decl.nd_line in
  if walk_truncated then
    add "  Channel scan truncated; pairings below may be incomplete.";
  if unreadable > 0 then
    add (Printf.sprintf
      "  %d source file(s) unreadable or oversized; they stay unresolved."
      unreadable);
  if decls = [] && in_root_natives = [] then
    add "  No literal platform channel declarations found.";
  if not ios_root && not android_root then
    add "  No ios/ or android/ host root in this package; native handlers stay unresolved.";
  let details_emitted = Hashtbl.create 8 in
  List.iter (fun decl ->
    let label = Printf.sprintf "%s channel" (kind_name decl.dd_kind) in
    let origin = if decl.dd_implicit then "literal use" else "declaration" in
    let pairings = analysis.a_decl decl in
    let detail_key = (decl.dd_kind, decl.dd_name) in
    List.iter (fun pairing ->
      match pairing with
      | Paired native ->
          let name = Option.get decl.dd_name in
          let handler_text = match native.nd_handler with
            | Some _ when native.nd_handler_ambiguous ->
                "handler registered; receiver binding unverified"
            | Some _ -> "handler registered"
            | None when native.nd_handler_ambiguous ->
                "handler ambiguous; shapes unresolved"
            | None -> "no literal handler registration found" in
          add (Printf.sprintf
            "  Paired %s '%s': Dart %s %s; native %s (%s)."
            label name origin (cite_dart decl) (cite_native native)
            handler_text);
          if decl.dd_kind = Method &&
             not (Hashtbl.mem details_emitted detail_key) then (
            Hashtbl.replace details_emitted detail_key ();
            let channel_calls = List.filter (fun call ->
              call.dc_kind = Some Method && call.dc_name = Some name)
              analysis.a_calls in
            (match native.nd_handler with
             | Some handler ->
                 List.iter (fun call ->
                   let call_site = cite call.dc_file call.dc_line in
                   match compare_call ~call ~handler with
                   | Consistent ->
                       add (Printf.sprintf
                         "    call '%s' (%s) at %s: consistent with native handler (%s)."
                         (Option.value call.dc_method ~default:"?")
                         (shape_label call.dc_shape) call_site
                         (expect_label handler.nh_expect))
                   | Mismatch why ->
                       add (Printf.sprintf
                         "    MISMATCH at %s: %s (Dart %s; native %s)."
                         call_site why (shape_label call.dc_shape)
                         (expect_label handler.nh_expect))
                   | Unresolved why ->
                       add (Printf.sprintf
                         "    call at %s unresolved: %s." call_site why))
                     channel_calls
             | None -> ());
            let methods = List.filter_map (fun c -> c.dc_method)
              channel_calls in
            (match analysis.a_tests name methods with
             | [] ->
                 add (Printf.sprintf
                   "    Focused test: none observed under the package test/ directory referencing '%s'; unknown."
                   name)
             | tests ->
                 List.iter (fun test ->
                   add (Printf.sprintf
                     "    Existing focused test: %s (run only via separately approved mobile_check flutter test)"
                     test)) tests))
      | Kind_mismatch native ->
          add (Printf.sprintf
            "  MISMATCH: Dart %s '%s' at %s is declared as a native %s channel at %s."
            label (Option.get decl.dd_name) (cite_dart decl)
            (kind_name native.nd_kind) (cite_native native))
      | Name_mismatch native ->
          add (Printf.sprintf
            "  MISMATCH: Dart %s '%s' at %s has no native handler; the nearest native %s channel name '%s' at %s differs."
            label (Option.get decl.dd_name) (cite_dart decl)
            (kind_name native.nd_kind)
            (Option.value native.nd_name ~default:"?") (cite_native native))
      | Out_of_root native ->
          add (Printf.sprintf
            "  Unresolved: Dart %s '%s' at %s; matching native declaration at %s is outside this package's ios/android roots and is not verified."
            label (Option.get decl.dd_name) (cite_dart decl)
            (cite_native native))
      | Missing_handler ->
          (match decl.dd_name with
           | Some name ->
               add (Printf.sprintf
                 "  Unresolved: Dart %s '%s' at %s has no verified native handler in this package."
                 label name (cite_dart decl))
           | None -> ())) pairings) decls;
  let referenced = Hashtbl.create 16 in
  List.iter (fun decl ->
    List.iter (function
      | Paired n | Kind_mismatch n | Name_mismatch n | Out_of_root n ->
          Hashtbl.replace referenced (n.nd_file, n.nd_line) ()
      | Missing_handler -> ()) (analysis.a_decl decl)) decls;
  List.iter (fun native ->
    if not (Hashtbl.mem referenced (native.nd_file, native.nd_line)) then
      match native.nd_name with
      | Some name ->
          add (Printf.sprintf
            "  Unresolved: native %s channel '%s' at %s is not referenced by any literal Dart channel."
            (kind_name native.nd_kind) name (cite_native native))
      | None ->
          add (Printf.sprintf
            "  Unresolved: native %s channel at %s uses a computed name."
            (kind_name native.nd_kind) (cite_native native)))
    in_root_natives;
  List.iter (fun note -> add ("  " ^ note))
    (List.sort_uniq String.compare notes);
  lines := "  Channel pairing is read-only: no command was run and no native code was generated."
    :: !lines;
  List.rev !lines
