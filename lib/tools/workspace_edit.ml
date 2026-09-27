(* Conflict-aware workspace editing. Exact text hunks are all matched against
   one SHA-256-checked snapshot. AST operations use only the OCaml implementation
   grammar parsed by compiler-libs; unsupported languages and invalid syntax
   fail closed, with no text fallback. Identifier rename is syntactic rather
   than scope/type resolved: it covers unqualified value identifiers and
   binders. Expression replacement selects one structurally equal expression.
   AST previews may cover several files; AST mutation is single-file only. *)

exception Error of string

let fail message = raise (Error message)
let ensure_text description text =
  if String.contains text '\000' then fail (description ^ " contains a NUL byte")

let supported_languages = ["ocaml"]
let supported_syntaxes = ["OCaml implementation source (compiler-libs Parse.implementation)"]
let max_file_bytes = Workspace_path.max_write_bytes
let max_ast_files = 32
let max_ast_visits = 100_000
let max_ast_printed_bytes = 4 * max_file_bytes
let max_hunks = 64
let max_hunk_bytes = 1_048_576

type snapshot = { contents : string; sha256 : string }
type hunk = { old_text : string; new_text : string }
type preview = {
  path : string;
  original_sha256 : string;
  result_sha256 : string;
  content : string;
  changed : bool;
}

type ast_operation =
  | Rename_identifier of { old_name : string; new_name : string }
  | Replace_expression of { target : string; replacement : string }

type ast_edit = { path : string; expected_sha256 : string; operation : ast_operation }

type prepared = {
  absolute : string;
  after : string;
  preview : preview;
}

let sha256 contents = Digestif.SHA256.(to_hex (digest_string contents))

let protect_workspace fn =
  try fn () with
  | Workspace_path.Error message -> fail message
  | Unix.Unix_error (error, operation, path) ->
      fail (Printf.sprintf "%s: %s (%s)" operation (Unix.error_message error) path)
  | Sys_error message -> fail message

let read_snapshot ~root ~path = protect_workspace (fun () ->
  let root = Workspace_path.root_path root in
  let absolute = Workspace_path.regular_path root path in
  let contents = Workspace_path.read_bounded absolute max_file_bytes in
  ensure_text "workspace file" contents;
  { contents; sha256 = sha256 contents })

let verify_snapshot expected actual =
  if expected <> actual then fail "workspace file changed since the supplied SHA-256 snapshot"
let verify_current_snapshot absolute expected =
  let contents = Workspace_path.read_bounded absolute max_file_bytes in
  ensure_text "workspace file" contents;
  verify_snapshot expected (sha256 contents)

let occurrences_up_to_two text needle =
  let text_length = String.length text and needle_length = String.length needle in
  if needle_length = 0 then []
  else (
    let prefix = Array.make needle_length 0 in
    let matched = ref 0 in
    for index = 1 to needle_length - 1 do
      while !matched > 0 && needle.[!matched] <> needle.[index] do
        matched := prefix.(!matched - 1)
      done;
      if needle.[!matched] = needle.[index] then incr matched;
      prefix.(index) <- !matched
    done;
    let matches = ref [] in
    let matched = ref 0 in
    let index = ref 0 in
    while !index < text_length && List.length !matches < 2 do
      while !matched > 0 && text.[!index] <> needle.[!matched] do
        matched := prefix.(!matched - 1)
      done;
      if text.[!index] = needle.[!matched] then incr matched;
      if !matched = needle_length then (
        matches := (!index - needle_length + 1) :: !matches;
        matched := prefix.(!matched - 1));
      incr index
    done;
    List.rev !matches)

let replace_ranges original ranges =
  let ranges = List.sort (fun (a, _, _) (b, _, _) -> compare a b) ranges in
  let cursor = ref 0 and output_size = ref (String.length original) in
  List.iter (fun (start, finish, replacement) ->
    if start < !cursor || finish < start || finish > String.length original then
      fail "edit ranges overlap or are outside the original file";
    output_size := !output_size - (finish - start);
    if String.length replacement > max_file_bytes - !output_size then
      fail (Printf.sprintf "edited file exceeds %d-byte limit" max_file_bytes);
    output_size := !output_size + String.length replacement;
    cursor := finish) ranges;
  let buffer = Buffer.create !output_size in
  cursor := 0;
  List.iter (fun (start, finish, replacement) ->
    Buffer.add_substring buffer original !cursor (start - !cursor);
    Buffer.add_string buffer replacement;
    cursor := finish) ranges;
  Buffer.add_substring buffer original !cursor (String.length original - !cursor);
  Buffer.contents buffer

let hunks_result original hunks =
  ensure_text "workspace file" original;
  if hunks = [] then fail "at least one replacement hunk is required";
  if List.length hunks > max_hunks then fail (Printf.sprintf "replacement batch exceeds %d hunks" max_hunks);
  let input_bytes = ref 0 in
  let add_input bytes =
    if bytes > max_hunk_bytes - !input_bytes then
      fail (Printf.sprintf "replacement hunk data exceeds %d-byte limit" max_hunk_bytes);
    input_bytes := !input_bytes + bytes
  in
  let ranges = List.concat_map (fun hunk ->
    if hunk.old_text = "" then fail "replacement hunk old_text must not be empty";
    ensure_text "replacement old_text" hunk.old_text;
    ensure_text "replacement new_text" hunk.new_text;
    add_input (String.length hunk.old_text);
    add_input (String.length hunk.new_text);
    let matches = occurrences_up_to_two original hunk.old_text in
    match matches with
    | [] -> fail "replacement hunk was not found in the original snapshot"
    | [_; _] -> fail "replacement hunk must match exactly once in the original snapshot"
    | [start] -> [start, start + String.length hunk.old_text, hunk.new_text]
    | _ -> assert false) hunks in
  replace_ranges original ranges


let make_preview path before content = {
  path;
  original_sha256 = before.sha256;
  result_sha256 = sha256 content;
  content;
  changed = content <> before.contents;
}

let prepare_hunks ~root ~path ~expected_sha256 ~hunks = protect_workspace (fun () ->
  let root = Workspace_path.root_path root in
  let absolute = Workspace_path.writable_path root path in
  let contents = Workspace_path.read_bounded absolute max_file_bytes in
  ensure_text "workspace file" contents;
  let before = { contents; sha256 = sha256 contents } in
  verify_snapshot expected_sha256 before.sha256;
  let after = hunks_result contents hunks in
  { absolute; after; preview = make_preview path before after })

let preview_hunks ~root ~path ~expected_sha256 ~hunks =
  (prepare_hunks ~root ~path ~expected_sha256 ~hunks).preview

let apply_hunks ~root ~path ~expected_sha256 ~hunks = protect_workspace (fun () ->
  let prepared = prepare_hunks ~root ~path ~expected_sha256 ~hunks in
  if prepared.preview.changed then (
    verify_current_snapshot prepared.absolute expected_sha256;
    Workspace_path.atomic_write prepared.absolute prepared.after);
  prepared.preview)

(* Compatibility operation for the existing edit_file contract: old_text must
   occur exactly once, and no snapshot token is required. *)
let replace_unique ~root ~path ~old_text ~new_text = protect_workspace (fun () ->
  ensure_text "replacement old_text" old_text;
  ensure_text "replacement new_text" new_text;
  if String.length old_text > max_hunk_bytes ||
     String.length new_text > max_hunk_bytes - String.length old_text then
    fail (Printf.sprintf "replacement hunk data exceeds %d-byte limit" max_hunk_bytes);
  let root = Workspace_path.root_path root in
  let absolute = Workspace_path.writable_path root path in
  let contents = Workspace_path.read_bounded absolute max_file_bytes in
  ensure_text "workspace file" contents;
  let before = { contents; sha256 = sha256 contents } in
  let matches = occurrences_up_to_two contents old_text in
  let start = match matches with
    | [] -> fail "old_text was not found; read the file to check the exact text"
    | [_; _] -> fail "old_text matches more than once; provide a longer unique excerpt"
    | [start] -> start
    | _ -> assert false in
  let after = replace_ranges contents
      [start, start + String.length old_text, new_text] in
  if after <> contents then (
    verify_current_snapshot absolute before.sha256;
    Workspace_path.atomic_write absolute after);
  make_preview path before after)

let parse_implementation ~filename source =
  ensure_text "OCaml source" source;
  if String.length source > max_file_bytes then fail "OCaml source exceeds the 1 MiB edit limit";
  try
    let lexbuf = Lexing.from_string source in
    Location.init lexbuf filename;
    Parse.implementation lexbuf
  with
  | Error _ as error -> raise error
  | Stack_overflow -> fail "OCaml source exceeds the bounded parser depth limit"
  | _ -> fail ("invalid OCaml syntax in " ^ filename)

let parse_expression ~filename source =
  ensure_text "OCaml expression" source;
  if source = "" then fail "OCaml expression must not be empty";
  if String.length source > max_file_bytes then fail "OCaml expression exceeds the 1 MiB edit limit";
  try
    let lexbuf = Lexing.from_string source in
    Location.init lexbuf filename;
    let expression = Parse.expression lexbuf in
    (match Lexer.token lexbuf with
     | Parser.EOF -> expression
     | _ -> fail "expected exactly one OCaml expression")
  with
  | Error _ as error -> raise error
  | Stack_overflow -> fail "OCaml expression exceeds the bounded parser depth limit"
  | _ -> fail "invalid OCaml expression syntax"

let expr_string expression = Format.asprintf "%a" Pprintast.expression expression

let simple_value_identifier ~filename name =
  let expression = parse_expression ~filename name in
  match expression.Parsetree.pexp_desc with
  | Parsetree.Pexp_ident { txt = Longident.Lident parsed; _ } when parsed = name -> ()
  | _ -> fail "identifier rename requires an unqualified OCaml value identifier"

let rename_identifier filename source old_name new_name =
  simple_value_identifier ~filename old_name;
  simple_value_identifier ~filename new_name;
  let structure = parse_implementation ~filename source in
  if old_name = new_name then source
  else
    let visits = ref 0 in
    let visit () =
      incr visits;
      if !visits > max_ast_visits then fail "OCaml AST exceeds the bounded edit complexity limit"
    in
    let ranges = ref [] in
    let add loc =
      let start = loc.Location.loc_start.Lexing.pos_cnum in
      let finish = loc.Location.loc_end.Lexing.pos_cnum in
      if start >= 0 && finish > start && finish <= String.length source &&
         String.sub source start (finish - start) = old_name then
        ranges := (start, finish, new_name) :: !ranges
    in
    let mapper = { Ast_mapper.default_mapper with
      structure_item = (fun self item ->
        visit ();
        Ast_mapper.default_mapper.structure_item self item);
      expr = (fun self expression ->
        visit ();
        (match expression.Parsetree.pexp_desc with
         | Parsetree.Pexp_ident { txt = Longident.Lident name; loc } when name = old_name -> add loc
         | _ -> ());
        Ast_mapper.default_mapper.expr self expression);
      pat = (fun self pattern ->
        visit ();
        (match pattern.Parsetree.ppat_desc with
         | Parsetree.Ppat_var { txt; loc } when txt = old_name -> add loc
         | Parsetree.Ppat_alias (_, { txt; loc }) when txt = old_name -> add loc
         | _ -> ());
        Ast_mapper.default_mapper.pat self pattern);
    } in
    ignore (mapper.Ast_mapper.structure mapper structure);
    if !ranges = [] then fail ("OCaml value identifier was not found: " ^ old_name);
    replace_ranges source !ranges

let replace_expression filename source target replacement =
  let wanted = expr_string (parse_expression ~filename target) in
  ignore (parse_expression ~filename replacement);
  let structure = parse_implementation ~filename source in
  let candidates = ref [] in
  let visits = ref 0 and printed_bytes = ref 0 in
  let rendered expression =
    incr visits;
    if !visits > max_ast_visits then fail "OCaml AST exceeds the bounded edit complexity limit";
    let result = expr_string expression in
    if String.length result > max_ast_printed_bytes - !printed_bytes then
      fail "OCaml AST matching exceeds the bounded formatting limit";
    printed_bytes := !printed_bytes + String.length result;
    result
  in
  let mapper = { Ast_mapper.default_mapper with
    structure_item = (fun self item ->
      incr visits;
      if !visits > max_ast_visits then fail "OCaml AST exceeds the bounded edit complexity limit";
      Ast_mapper.default_mapper.structure_item self item);
    expr = (fun self expression ->
      if rendered expression = wanted then (
        let loc = expression.Parsetree.pexp_loc in
        candidates := (loc.Location.loc_start.Lexing.pos_cnum,
                       loc.Location.loc_end.Lexing.pos_cnum) :: !candidates);
      Ast_mapper.default_mapper.expr self expression);
  } in
  ignore (mapper.Ast_mapper.structure mapper structure);
  let start, finish = match !candidates with
    | [] -> fail "no OCaml expression matched the requested AST shape"
    | [range] -> range
    | _ -> fail "OCaml expression shape matched more than once; narrow the target expression" in
  if start < 0 || finish <= start || finish > String.length source then
    fail "OCaml parser returned an invalid expression location";
  replace_ranges source [start, finish, replacement]

let transform_ast ~language ~filename ~source operation =
  if not (List.mem language supported_languages) then
    fail ("unsupported AST language: " ^ language ^ "; supported: " ^ String.concat ", " supported_languages);
  try
    match operation with
    | Rename_identifier { old_name; new_name } -> rename_identifier filename source old_name new_name
    | Replace_expression { target; replacement } -> replace_expression filename source target replacement
  with Stack_overflow -> fail "OCaml AST exceeds the bounded parser/transform depth limit"

let prepare_ast ~root ~language edit = protect_workspace (fun () ->
  if not (List.mem language supported_languages) then
    fail ("unsupported AST language: " ^ language ^ "; supported: " ^ String.concat ", " supported_languages);
  let root = Workspace_path.root_path root in
  let absolute = Workspace_path.writable_path root edit.path in
  let contents = Workspace_path.read_bounded absolute max_file_bytes in
  let before = { contents; sha256 = sha256 contents } in
  verify_snapshot edit.expected_sha256 before.sha256;
  let after = transform_ast ~language ~filename:edit.path ~source:contents edit.operation in
  { absolute; after; preview = make_preview edit.path before after })

let preview_ast ~root ~language ~edits =
  if not (List.mem language supported_languages) then
    fail ("unsupported AST language: " ^ language ^ "; supported: " ^ String.concat ", " supported_languages);
  if List.length edits > max_ast_files then fail (Printf.sprintf "AST batch exceeds %d files" max_ast_files);
  let paths = List.map (fun edit -> edit.path) edits in
  if List.length (List.sort_uniq String.compare paths) <> List.length paths then
    fail "AST batch contains a duplicate path";
  List.map (fun edit -> (prepare_ast ~root ~language edit).preview) edits

(* Applying syntax-aware rewrites is deliberately single-file. A preview can
   cover several files, but mutation has one atomic-write boundary and cannot
   partially publish a multi-file transaction. *)
let apply_ast ~root ~language ~edit = protect_workspace (fun () ->
  let prepared = prepare_ast ~root ~language edit in
  if prepared.preview.changed then (
    verify_current_snapshot prepared.absolute edit.expected_sha256;
    Workspace_path.atomic_write prepared.absolute prepared.after);
  prepared.preview)
