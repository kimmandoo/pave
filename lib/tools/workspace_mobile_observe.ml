exception Error of string

let fail message = raise (Error message)

let max_screenshot_bytes = Workspace_process.max_output_limit
let max_accessibility_bytes = Workspace_process.max_output_limit
let max_nodes = 10_000

type screenshot = { png : string; width : int; height : int }

type node = {
  index : int;
  parent : int option;
  depth : int;
  role : string;
  text : string;
  description : string;
  identifier : string;
  bounds : string;
  clickable : bool;
  scrollable : bool;
  enabled : bool;
  selected : bool;
}

let command action (session : Workspace_mobile_run.session) =
  let quote = Filename.quote in
  match action, session.platform with
  | "screenshot", Workspace_mobile_run.Android ->
      "adb -s " ^ quote session.device ^ " exec-out screencap -p"
  | "accessibility", Workspace_mobile_run.Android ->
      let path = "/data/local/tmp/pave-accessibility-" ^ session.id ^ ".xml" in
      let remote = Printf.sprintf
        "uiautomator dump %s >/dev/null; status=$?; if [ \"$status\" -eq 0 ]; then cat %s; status=$?; fi; rm -f %s; exit \"$status\""
        path path path in
      "adb -s " ^ quote session.device ^ " shell " ^ quote remote
  | "screenshot", Workspace_mobile_run.Ios ->
      "xcrun simctl io " ^ quote session.device ^
      " screenshot --type=png /dev/stdout 2>/dev/null"
  | "accessibility", Workspace_mobile_run.Ios ->
      fail "iOS Simulator accessibility-tree capture is unavailable through the approved system tools"
  | _ -> fail "mobile observation action must be screenshot or accessibility"

let uint32_be data offset =
  let length = String.length data in
  if offset < 0 || offset > length - 4 then fail "invalid PNG header";
  let value index = Char.code data.[offset + index] in
  (value 0 lsl 24) lor (value 1 lsl 16) lor (value 2 lsl 8) lor value 3

let validate_png png =
  let length = String.length png in
  if length < 8 || length > max_screenshot_bytes ||
     String.sub png 0 8 <> "\137PNG\r\n\026\n" then
    fail "screenshot was not a complete bounded PNG image";
  let rec chunks offset index saw_data =
    if offset > length - 12 then fail "PNG screenshot is truncated before IEND";
    if index >= 1024 then fail "PNG screenshot contains too many chunks";
    let chunk_length = uint32_be png offset in
    if chunk_length < 0 || chunk_length > length - offset - 12 then
      fail "PNG screenshot contains a truncated chunk";
    let kind = String.sub png (offset + 4) 4 in
    if index = 0 && (kind <> "IHDR" || chunk_length <> 13) then
      fail "PNG screenshot has no valid first IHDR chunk";
    if kind = "IEND" then (
      if chunk_length <> 0 || not saw_data || offset + 12 <> length then
        fail "PNG screenshot has an invalid IEND chunk";
      ())
    else if kind = "IDAT" then
      chunks (offset + 12 + chunk_length) (index + 1) true
    else
      chunks (offset + 12 + chunk_length) (index + 1) saw_data in
  if length < 33 then fail "PNG screenshot has a truncated IHDR chunk";
  let width = uint32_be png 16 and height = uint32_be png 20 in
  if width < 1 || height < 1 || width > 16_384 || height > 16_384 ||
     width > 100_000_000 / height then
    fail "screenshot dimensions are outside supported bounds";
  chunks 8 0 false;
  { png; width; height }

let base64_encode data =
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let length = String.length data in
  let output = Buffer.create (((length + 2) / 3) * 4) in
  let byte index = Char.code data.[index] in
  let rec loop index =
    if index < length then (
      let remaining = length - index in
      let a = byte index in
      let b = if remaining > 1 then byte (index + 1) else 0 in
      let c = if remaining > 2 then byte (index + 2) else 0 in
      Buffer.add_char output alphabet.[a lsr 2];
      Buffer.add_char output alphabet.[((a land 3) lsl 4) lor (b lsr 4)];
      Buffer.add_char output (if remaining > 1 then alphabet.[((b land 15) lsl 2) lor (c lsr 6)] else '=');
      Buffer.add_char output (if remaining > 2 then alphabet.[c land 63] else '=');
      loop (index + 3)) in
  loop 0;
  Buffer.contents output

let find_sub text pattern offset =
  let text_length = String.length text and pattern_length = String.length pattern in
  let rec loop index =
    if index + pattern_length > text_length then None
    else if String.sub text index pattern_length = pattern then Some index
    else loop (index + 1) in
  loop offset

let node_start xml offset =
  let rec find index = match find_sub xml "<node" index with
    | None -> None
    | Some start ->
        let after = start + 5 in
        if after >= String.length xml then None
        else if List.mem xml.[after] [' '; '\t'; '\r'; '\n'; '>'; '/'] then Some start
        else find after in
  find offset

let tag_end xml start =
  let rec loop index quote =
    if index >= String.length xml then fail "unterminated accessibility node tag";
    let ch = xml.[index] in
    match quote, ch with
    | Some delimiter, value when value = delimiter -> loop (index + 1) None
    | Some _, _ -> loop (index + 1) quote
    | None, ('\"' | '\'') -> loop (index + 1) (Some ch)
    | None, '>' -> index
    | None, _ -> loop (index + 1) None in
  loop start None

let decode_entities value =
  let output = Buffer.create (String.length value) in
  let add_codepoint entity =
    let number =
      try
        if String.length entity >= 3 && entity.[1] = 'x' then
          int_of_string ("0x" ^ String.sub entity 2 (String.length entity - 2))
        else int_of_string (String.sub entity 1 (String.length entity - 1))
      with Failure _ -> fail "invalid numeric XML entity in accessibility node" in
    if number < 0 || number > 0x10ffff ||
       (number >= 0xd800 && number <= 0xdfff) ||
       not (number = 9 || number = 10 || number = 13 || number >= 0x20) then
      fail "invalid XML character reference in accessibility node";
    if number < 0x80 then Buffer.add_char output (Char.chr number)
    else if number < 0x800 then (
      Buffer.add_char output (Char.chr (0xc0 lor (number lsr 6)));
      Buffer.add_char output (Char.chr (0x80 lor (number land 0x3f)))
    ) else if number < 0x10000 then (
      Buffer.add_char output (Char.chr (0xe0 lor (number lsr 12)));
      Buffer.add_char output (Char.chr (0x80 lor ((number lsr 6) land 0x3f)));
      Buffer.add_char output (Char.chr (0x80 lor (number land 0x3f)))
    ) else (
      Buffer.add_char output (Char.chr (0xf0 lor (number lsr 18)));
      Buffer.add_char output (Char.chr (0x80 lor ((number lsr 12) land 0x3f)));
      Buffer.add_char output (Char.chr (0x80 lor ((number lsr 6) land 0x3f)));
      Buffer.add_char output (Char.chr (0x80 lor (number land 0x3f)))) in
  let rec loop index =
    if index < String.length value then
      if value.[index] <> '&' then (
        Buffer.add_char output value.[index];
        loop (index + 1))
      else
        match String.index_from_opt value (index + 1) ';' with
        | None -> fail "unterminated XML entity in accessibility node"
        | Some ending ->
            let entity = String.sub value (index + 1) (ending - index - 1) in
            (match entity with
             | "amp" -> Buffer.add_char output '&'
             | "quot" -> Buffer.add_char output '"'
             | "apos" -> Buffer.add_char output (Char.chr 39)
             | "lt" -> Buffer.add_char output '<'
             | "gt" -> Buffer.add_char output '>'
             | value when String.length value > 1 && value.[0] = '#' ->
                 add_codepoint value
             | _ -> fail "unsupported XML entity in accessibility node");
            loop (ending + 1) in
  loop 0;
  Buffer.contents output

let parse_attributes tag =
  let length = String.length tag in
  let rec skip index =
    if index < length && List.mem tag.[index] [' '; '\t'; '\r'; '\n'; '/']
    then skip (index + 1) else index in
  let rec parse index attributes =
    let index = skip index in
    if index >= length || tag.[index] = '>' then attributes
    else
      let name_end = ref index in
      while !name_end < length &&
        not (List.mem tag.[!name_end] [' '; '\t'; '\r'; '\n'; '='; '>'; '/'])
      do incr name_end done;
      if !name_end = index then fail "invalid accessibility node attribute";
      let name = String.sub tag index (!name_end - index) in
      let equals = skip !name_end in
      if equals >= length || tag.[equals] <> '=' then
        fail "invalid accessibility node attribute";
      let opening = skip (equals + 1) in
      if opening >= length || not (List.mem tag.[opening] ['\"'; '\'']) then
        fail "invalid accessibility node attribute value";
      let delimiter = tag.[opening] in
      let ending = match String.index_from_opt tag (opening + 1) delimiter with
        | Some value -> value
        | None -> fail "unterminated accessibility node attribute" in
      let value = decode_entities
        (String.sub tag (opening + 1) (ending - opening - 1)) in
      parse (ending + 1) ((name, value) :: attributes) in
  parse 5 []

let attr attributes name = Option.value ~default:"" (List.assoc_opt name attributes)
let bool_attr attributes name = attr attributes name = "true"

let parse_accessibility xml =
  if String.length xml > max_accessibility_bytes then
    fail "accessibility output exceeds its size limit";
  if find_sub xml "<!DOCTYPE" 0 <> None || find_sub xml "<!ENTITY" 0 <> None then
    fail "accessibility output contains a forbidden XML declaration";
  let nodes = ref [] and stack = ref [] in
  let rec loop offset =
    let opening = node_start xml offset
    and closing = find_sub xml "</node" offset in
    match opening, closing with
    | None, None -> ()
    | None, Some start ->
        if !stack = [] then fail "unbalanced accessibility tree";
        stack := List.tl !stack;
        loop (tag_end xml start + 1)
    | Some start, Some ending when ending < start ->
        if !stack = [] then fail "unbalanced accessibility tree";
        stack := List.tl !stack;
        loop (tag_end xml ending + 1)
    | Some start, _ ->
        let ending = tag_end xml start in
        let tag = String.sub xml start (ending - start + 1) in
        let attributes = parse_attributes tag in
        if List.length !nodes >= max_nodes then
          fail "accessibility tree exceeds its node limit";
        let index = List.length !nodes in
        let parent = match !stack with [] -> None | parent :: _ -> Some parent in
        let node = {
          index; parent; depth = List.length !stack;
          role = attr attributes "class";
          text = attr attributes "text";
          description = attr attributes "content-desc";
          identifier = attr attributes "resource-id";
          bounds = attr attributes "bounds";
          clickable = bool_attr attributes "clickable";
          scrollable = bool_attr attributes "scrollable";
          enabled = bool_attr attributes "enabled";
          selected = bool_attr attributes "selected";
        } in
        nodes := node :: !nodes;
        let position = ref (String.length tag - 2) in
        while !position >= 0 &&
          List.mem tag.[!position] [' '; '\t'; '\r'; '\n'] do
          decr position done;
        if !position < 0 || tag.[!position] <> '/' then
          stack := index :: !stack;
        loop (ending + 1) in
  loop 0;
  if !nodes = [] then fail "accessibility tree is empty";
  if !stack <> [] then
    fail (Printf.sprintf "accessibility tree has %d unclosed node(s)"
      (List.length !stack));
  List.rev !nodes

let node_json node = `Assoc [
  "index", `Int node.index;
  "parent", (match node.parent with None -> `Null | Some value -> `Int value);
  "depth", `Int node.depth;
  "role", `String node.role;
  "text", `String node.text;
  "description", `String node.description;
  "identifier", `String node.identifier;
  "bounds", `String node.bounds;
  "clickable", `Bool node.clickable;
  "scrollable", `Bool node.scrollable;
  "enabled", `Bool node.enabled;
  "selected", `Bool node.selected;
]

let accessibility_json nodes = Yojson.Basic.to_string (`Assoc [
  "status", `String "available";
  "node_count", `Int (List.length nodes);
  "nodes", `List (List.map node_json nodes);
])
