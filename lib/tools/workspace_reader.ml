exception Error of string

let max_output_bytes = 65_536
let max_scan_bytes = 64 * 1024 * 1024
let max_directory_entries = 1_000
let max_sql_rows = 200

let fail message = raise (Error message)

let check_cancel cancel =
  match cancel with
  | Some cancelled when cancelled () -> fail "workspace read cancelled"
  | _ -> ()

let check_output text =
  if String.length text > max_output_bytes then
    fail (Printf.sprintf "workspace read exceeds %d-byte output limit" max_output_bytes);
  text
let check_helper_input path kind =
  if (Unix.stat path).Unix.st_size > max_scan_bytes then
    fail (Printf.sprintf "%s input exceeds the %d-byte limit" kind max_scan_bytes)


let starts_with text prefix =
  String.length text >= String.length prefix &&
  String.sub text 0 (String.length prefix) = prefix

let ends_with text suffix =
  let n = String.length text and m = String.length suffix in
  n >= m && String.sub text (n - m) m = suffix

let lowercase = String.lowercase_ascii

let is_scheme_uri text =
  match String.index_opt text ':' with
  | Some colon when colon + 2 < String.length text &&
                    String.sub text (colon + 1) 2 = "//" ->
      let scheme = String.sub text 0 colon in
      String.length scheme > 0 &&
      let valid = ref true in
      String.iteri (fun i c ->
        if not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
                (i > 0 && ((c >= '0' && c <= '9') || c = '+' || c = '.' || c = '-'))) then
          valid := false) scheme;
      !valid
  | _ -> false

type selector = Raw | Lines of (int * int option) list | Tail of int

let integer text =
  try
    if text = "" || String.exists (fun c -> c < '0' || c > '9') text then None
    else Some (int_of_string text)
  with Failure _ | Invalid_argument _ -> None

let range text =
  match String.index_opt text '-' with
  | None -> Option.bind (integer text) (fun number -> if number > 0 then Some (number, Some number) else None)
  | Some split when split > 0 && split + 1 < String.length text ->
      (match integer (String.sub text 0 split), integer (String.sub text (split + 1) (String.length text - split - 1)) with
       | Some first, Some last when first > 0 && first <= last -> Some (first, Some last)
       | _ -> None)
  | _ -> None

let parse_selector token =
  if token = "raw" then Some Raw
  else if starts_with token "-" then
    (match integer (String.sub token 1 (String.length token - 1)) with
     | Some count when count > 0 -> Some (Tail count)
     | _ -> None)
  else
    match String.split_on_char ',' token with
    | [] -> None
    | [single] -> Option.map (fun selected -> Lines [selected]) (range single)
    | pieces ->
        let selected = List.map (fun piece ->
          if String.contains piece '-' then range piece else None) pieces in
        if List.for_all Option.is_some selected then
          Some (Lines (List.map Option.get selected))
        else None

let selector_suffix text =
  match String.rindex_opt text ':' with
  | None -> None
  | Some colon ->
      let token = String.sub text (colon + 1) (String.length text - colon - 1) in
      if String.length token > 4096 then None
      else
        (match parse_selector token with
         | None -> None
         | Some (Lines ranges) when List.length ranges > 64 -> None
         | Some selector -> Some (String.sub text 0 colon, selector))
let selector_in_url url =
  let authority_start = 8 in
  let path_start =
    let rec find i =
      if i >= String.length url then None
      else if url.[i] = '/' || url.[i] = '?' || url.[i] = '#' then Some i
      else find (i + 1)
    in
    find authority_start
  in
  match path_start with
  | None -> url, None
  | Some start when url.[start] <> '/' -> url, None
  | Some start ->
      let path_end =
        let rec find i =
          if i >= String.length url then String.length url
          else if url.[i] = '?' || url.[i] = '#' then i else find (i + 1)
        in find start
      in
      let path_part = String.sub url start (path_end - start) in
      (match selector_suffix path_part with
       | None -> url, None
       | Some (without, selector) ->
           String.sub url 0 start ^ without ^ String.sub url path_end (String.length url - path_end),
           Some selector)

let suffix_extension path extensions =
  let lower = lowercase path in
  List.find_opt (fun extension -> ends_with lower extension) extensions

let archive_extensions = [".tar.gz"; ".tar.bz2"; ".tar.xz"; ".tar.zst"; ".tgz"; ".tbz2"; ".txz"; ".tar"; ".zip"; ".jar"; ".7z"; ".rar"]

let archive_spec text =
  let lower = lowercase text in
  let rec find = function
    | [] -> None
    | extension :: rest ->
        let length = String.length extension in
        let rec scan at =
          if at + length > String.length text then find rest
          else if String.sub lower at length = extension && at + length < String.length text && text.[at + length] = ':' then
            let archive = String.sub text 0 (at + length) in
            let member = String.sub text (at + length + 1) (String.length text - at - length - 1) in
            Some (archive, member)
          else scan (at + 1)
        in scan 0
  in find archive_extensions

type termination = Workspace_process.termination

type process_output = { output : string; termination : termination; truncated : bool }

let run_process ?cancel ?cwd ?environment ?(timeout_seconds = 20) ?(output_limit = max_output_bytes) ~program ~arguments () =
  check_cancel cancel;
  let result =
    try Workspace_process.run ?cancel ~timeout_seconds
          ~output_limit ?cwd ?environment ~program ~arguments ()
    with
    | Error _ as error -> raise error
    | Workspace_process.Error message -> fail (program ^ " helper failed: " ^ message)
    | Unix.Unix_error (error, operation, argument) ->
        fail (Printf.sprintf "could not run %s (%s %s): %s" program operation argument (Unix.error_message error))
    | Sys_error message -> fail (Printf.sprintf "could not run %s: %s" program message)
  in
  (match result.termination with
   | Workspace_process.Cancelled -> fail "workspace read cancelled"
   | Workspace_process.Timed_out -> fail (program ^ " helper timed out")
   | Workspace_process.Exited 0 -> ()
   | Workspace_process.Exited 127 -> fail (Printf.sprintf "%s helper is unavailable at its fixed system path; install it there to read this format" program)
   | Workspace_process.Exited code ->
       let detail = String.trim result.output in
       fail (if detail = "" then Printf.sprintf "%s helper failed (exit %d)" program code
             else Printf.sprintf "%s helper failed (exit %d): %s" program code detail)
   | Workspace_process.Signaled signal ->
       fail (Printf.sprintf "%s helper terminated by signal %d" program signal));
  if result.truncated || String.length result.output > output_limit then
    fail (Printf.sprintf "%s helper output exceeds %d-byte limit" program output_limit);
  check_cancel cancel;
  { output = result.output; termination = result.termination; truncated = result.truncated }

let read_local_file ?cancel path selector =
  check_cancel cancel;
  if selector = Raw then Workspace_path.read_bounded path max_output_bytes
  else
    let ranges, tail_count = match selector with
      | Lines ranges ->
          List.sort (fun (left, _) (right, _) -> Int.compare left right) ranges, None
      | Tail count -> [], Some (min count (max_output_bytes + 1))
      | Raw -> [], None
    in
    let stat = Unix.stat path in
    let output = Buffer.create (min max_output_bytes (max 0 stat.Unix.st_size)) in
    let add_line text has_newline =
      let extra = String.length text + (if has_newline then 1 else 0) in
      if Buffer.length output + extra > max_output_bytes then
        fail (Printf.sprintf "selected text exceeds %d-byte output limit" max_output_bytes);
      Buffer.add_string output text;
      if has_newline then Buffer.add_char output '\n'
    in
    let active_ranges = ref ranges in
    let selected number =
      match tail_count with
      | Some _ -> true
      | None ->
          let rec advance = function
            | (_, Some ending) :: rest when ending < number ->
                active_ranges := rest;
                advance rest
            | _ -> ()
          in
          advance !active_ranges;
          (match !active_ranges with
           | (first, last) :: _ ->
               number >= first && (match last with None -> true | Some ending -> number <= ending)
           | [] -> false)
    in
    let last_requested = match tail_count with
      | Some _ -> max_int
      | None -> List.fold_left (fun bound (_, last) ->
          match last with None -> max_int | Some ending -> max bound ending) 0 ranges
    in
    let buffer = Buffer.create 256 in
    let total_bytes = ref 0 and line_no = ref 1 in
    let tail_lines = Queue.create () in
    let push_tail text has_newline =
      Queue.add (text, has_newline) tail_lines;
      match tail_count with
      | Some limit when Queue.length tail_lines > limit -> ignore (Queue.take tail_lines)
      | _ -> ()
    in
    let is_tail = Option.is_some tail_count in
    let finish_line has_newline =
      let text = Buffer.contents buffer in
      Buffer.clear buffer;
      if is_tail then push_tail text has_newline
      else if selected !line_no then add_line text has_newline;
      incr line_no
    in
    let fd_buffer = Bytes.create 8192 in
    let stop = ref false in
    Workspace_path.with_fd path [Unix.O_RDONLY] 0 (fun fd ->
      let rec loop () =
        check_cancel cancel;
        if not !stop then (
          let count = Unix.read fd fd_buffer 0 (Bytes.length fd_buffer) in
          if count = 0 then (
            if Buffer.length buffer > 0 then finish_line false
          ) else (
            total_bytes := !total_bytes + count;
            if !total_bytes > max_scan_bytes then
              fail (Printf.sprintf "selected file exceeds the %d-byte scan limit" max_scan_bytes);
            let index = ref 0 in
            while !index < count && not !stop do
              let byte = Bytes.get fd_buffer !index in
              if byte = '\000' && (is_tail || selected !line_no) then
                fail "binary file; workspace_reader supports text only (use :raw for bounded bytes)";
              if byte = '\n' then (
                finish_line true;
                if not is_tail && !line_no > last_requested then stop := true
              ) else if is_tail || selected !line_no then (
                if Buffer.length buffer >= max_output_bytes then
                  fail (Printf.sprintf "selected text exceeds %d-byte output limit" max_output_bytes);
                Buffer.add_char buffer byte
              );
              incr index
            done;
            if not !stop then loop ()))
      in
      loop ());
    (match tail_count with
     | None -> ()
     | Some _ -> Queue.iter (fun (text, newline) -> add_line text newline) tail_lines);
    Buffer.contents output
let read_raw_file path = Workspace_path.read_bounded path max_output_bytes

let directory_listing ?cancel path =
  let directory = Unix.opendir path in
  Fun.protect ~finally:(fun () -> Unix.closedir directory) (fun () ->
    let names = ref [] and count = ref 0 in
    let rec collect () =
      check_cancel cancel;
      match Unix.readdir directory with
      | "." | ".." -> collect ()
      | name ->
          incr count;
          if !count > max_directory_entries then
            fail (Printf.sprintf "directory contains more than %d entries" max_directory_entries);
          let full = Filename.concat path name in
          let kind = try match (Unix.lstat full).Unix.st_kind with
            | Unix.S_DIR -> "directory"
            | Unix.S_REG -> "file"
            | Unix.S_LNK -> "symlink"
            | _ -> "other"
          with Unix.Unix_error _ -> "unavailable" in
          names := (name, kind) :: !names;
          collect ()
      | exception End_of_file -> ()
    in
    collect ();
    let names = List.sort (fun (left, _) (right, _) -> String.compare left right) !names in
    let result = Buffer.create 256 in
    List.iter (fun (name, kind) ->
      let line = Printf.sprintf "%s\t%s\n" kind name in
      if Buffer.length result + String.length line > max_output_bytes then
        fail (Printf.sprintf "directory listing exceeds %d-byte output limit" max_output_bytes);
      Buffer.add_string result line) names;
    Buffer.contents result)

let has_nul text = String.contains text '\000'

let check_text text =
  if has_nul text then fail "binary file; workspace_reader supports text only (use :raw for bounded bytes)";
  check_output text

let json_string_list = function
  | `String text -> [text]
  | `List items -> List.map (function `String text -> text | _ -> fail "notebook cell source must contain only strings") items
  | _ -> fail "notebook cell is missing editable source text"

let notebook_text bytes =
  let notebook = try Yojson.Safe.from_string bytes with Yojson.Json_error message ->
    fail ("invalid Jupyter notebook JSON: " ^ message) in
  let cells = match notebook with
    | `Assoc fields -> (match List.assoc_opt "cells" fields with Some (`List cells) -> cells | _ -> fail "Jupyter notebook is missing its cells array")
    | _ -> fail "Jupyter notebook root must be an object" in
  let output = Buffer.create (min max_output_bytes (String.length bytes)) in
  let index = ref 0 in
  List.iter (fun cell ->
    let fields = match cell with `Assoc fields -> fields | _ -> fail "Jupyter notebook cell must be an object" in
    let kind = match List.assoc_opt "cell_type" fields with Some (`String kind) when List.mem kind ["code"; "markdown"; "raw"] -> kind | _ -> fail "Jupyter notebook has an unsupported cell type" in
    let source = match List.assoc_opt "source" fields with Some value -> String.concat "" (json_string_list value) | None -> fail "notebook cell is missing editable source text" in
    incr index;
    let header = Printf.sprintf "# Cell %d (%s)\n" !index kind in
    if Buffer.length output + String.length header + String.length source + 1 > max_output_bytes then
      fail (Printf.sprintf "converted notebook exceeds %d-byte output limit" max_output_bytes);
    Buffer.add_string output header;
    Buffer.add_string output source;
    if source = "" || source.[String.length source - 1] <> '\n' then Buffer.add_char output '\n';
    Buffer.add_char output '\n') cells;
  Buffer.contents output

let helper_text ?cancel ?cwd ~program ~arguments text_kind =
  let result =
    try run_process ?cancel ?cwd ~program ~arguments () with
    | Error message when starts_with message (program ^ " helper is unavailable") ->
        fail (Printf.sprintf "%s text conversion is unavailable: required helper %s is missing; install it at that fixed system path" text_kind program)
  in result.output

let document_text ?cancel path extension =
  match extension with
  | ".pdf" ->
      check_helper_input path "PDF";
      helper_text ?cancel ~cwd:(Some "/") ~program:"/usr/bin/pdftotext" ~arguments:["-layout"; path; "-"] "PDF"
  | ".docx" | ".odt" | ".rtf" ->
      check_helper_input path "Office";
      helper_text ?cancel ~cwd:(Some "/") ~program:"/usr/bin/pandoc" ~arguments:["--to=plain"; "--wrap=none"; path] "Office"
  | ".doc" -> fail "Office text conversion for .doc is unavailable: no read-only converter is configured; export it to .txt without writing into the workspace"
  | ".ppt" -> fail "Office text conversion for .ppt is unavailable: no trusted read-only converter is installed at a fixed system path; export it to .txt"
  | ".pptx" -> fail "Office text conversion for .pptx is unavailable: no trusted read-only presentation converter is installed at a fixed system path; export it to .txt"
  | ".xls" -> fail "Office text conversion for .xls is unavailable: no trusted read-only spreadsheet converter is installed at a fixed system path; export it to .csv"
  | ".xlsx" | ".ods" -> fail (Printf.sprintf "Office text conversion for %s is unavailable: no trusted read-only spreadsheet converter is installed at a fixed system path; export it to .csv" extension)
  | _ -> fail ("document text conversion is unavailable for " ^ extension)

let process_failure program result =
  match result.termination with
  | Workspace_process.Cancelled -> fail "workspace read cancelled"
  | Workspace_process.Timed_out -> fail (program ^ " helper timed out")
  | Workspace_process.Exited 0 ->
      if result.truncated then fail (program ^ " output exceeded the workspace read limit");
      result.output
  | Workspace_process.Exited 127 -> fail (program ^ " helper is unavailable at its fixed system path; install it there to read this format")
  | Workspace_process.Exited code ->
      let detail = String.trim result.output in
      fail (if detail = "" then Printf.sprintf "%s helper failed (exit %d)" program code
            else Printf.sprintf "%s helper failed (exit %d): %s" program code detail)
  | Workspace_process.Signaled signal -> fail (Printf.sprintf "%s helper terminated by signal %d" program signal)

let archive_program archive =
  match suffix_extension archive archive_extensions with
  | Some ".zip" | Some ".jar" -> "/usr/bin/unzip"
  | Some ".7z" | Some ".rar" -> "/usr/bin/bsdtar"
  | Some _ -> "/usr/bin/tar"
  | None -> ""

let archive_list ?cancel archive =
  check_helper_input archive "archive";
  let program = archive_program archive in
  let arguments = if program = "/usr/bin/unzip" then ["-Z1"; archive]
    else ["-tf"; archive] in
  let result = run_process ?cancel ~timeout_seconds:15 ~program ~arguments () in
  check_text result.output

let archive_member ?cancel archive member =
  check_helper_input archive "archive";
  if member = "" || member.[0] = '/' || member.[0] = '-' || String.contains member '\000' ||
     String.exists (fun c -> List.mem c ['*'; '?'; '['; ']']) member ||
     List.exists (( = ) "..") (String.split_on_char '/' member) then
    fail "archive member must be an exact nonempty relative path without '..' or glob characters";
  let program = archive_program archive in
  let arguments = if program = "/usr/bin/unzip" then ["-p"; archive; member]
    else if program = "/usr/bin/bsdtar" then ["-xOf"; archive; member]
    else ["-xOf"; archive; "--"; member] in
  let result = run_process ?cancel ~timeout_seconds:20 ~program ~arguments () in
  result.output

let parse_ipv4 address =
  let pieces = String.split_on_char '.' address in
  match pieces with
  | [a; b; c; d] ->
      (try
         let numbers = List.map int_of_string [a; b; c; d] in
         if List.exists (fun n -> n < 0 || n > 255) numbers then None
         else match numbers with [a; b; c; d] -> Some (a, b, c, d) | _ -> None
       with Failure _ -> None)
  | _ -> None

let private_ipv4 address =
  match parse_ipv4 address with
  | None -> true
  | Some (a, b, c, _d) ->
      a = 0 || a = 10 || a = 127 || a >= 224 ||
      (a = 100 && b >= 64 && b <= 127) ||
      (a = 169 && b = 254) ||
      (a = 172 && b >= 16 && b <= 31) ||
      (a = 192 && (b = 168 || (b = 0 && c = 0) ||
                   (b = 0 && c = 2) || (b = 88 && c = 99))) ||
      (a = 198 && (b = 18 || b = 19 || (b = 51 && c = 100))) ||
      (a = 203 && b = 0 && c = 113)

let ipv6_words address =
  let address = lowercase address in
  let expand_side text = if text = "" then [] else String.split_on_char ':' text in
  let compression =
    let rec search i =
      if i + 1 < String.length address && address.[i] = ':' && address.[i + 1] = ':' then Some i
      else if i + 1 >= String.length address then None
      else search (i + 1)
    in
    search 0
  in
  let words = match compression with
    | None -> expand_side address
    | Some at ->
        let left = String.sub address 0 at |> expand_side in
        let right = String.sub address (at + 2) (String.length address - at - 2) |> expand_side in
        let missing = 8 - List.length left - List.length right in
        left @ List.init (max 0 missing) (fun _ -> "0") @ right
  in
  if List.length words <> 8 then None
  else
    try Some (Array.of_list (List.map (fun word -> int_of_string ("0x" ^ word)) words))
    with Failure _ -> None
let private_ipv6 address =
  match ipv6_words address with
  | None -> true
  | Some words ->
      let first = words.(0) in
      let all_zero = Array.for_all (( = ) 0) words in
      let loopback = all_zero && words.(7) = 1 in
      let mapped = Array.for_all (( = ) 0) (Array.sub words 0 5) && words.(5) = 0xffff in
      let compatible = Array.for_all (( = ) 0) (Array.sub words 0 6) in
      let ipv4_tail = Printf.sprintf "%d.%d.%d.%d" (words.(6) lsr 8) (words.(6) land 255) (words.(7) lsr 8) (words.(7) land 255) in
      let global_unicast = first land 0xe000 = 0x2000 in
      all_zero || loopback || not global_unicast ||
      (first land 0xfe00 = 0xfc00) ||
      (first land 0xffc0 = 0xfe80) || (first land 0xff00 = 0xff00) ||
      (first = 0x2001 && (words.(1) <= 0x01ff || words.(1) = 0x0020 || words.(1) = 0x0db8)) ||
      first = 0x2002 ||
      ((mapped || compatible) && private_ipv4 ipv4_tail)

let unsafe_ip address =
  match Unix.inet_addr_of_string address with
  | ip ->
      let normalized = Unix.string_of_inet_addr ip in
      if String.contains normalized ':' then private_ipv6 normalized else private_ipv4 normalized
  | exception Failure _ -> true

let parse_url url =
  if String.length url > max_output_bytes || String.exists (fun c -> Char.code c <= 0x20 || Char.code c = 0x7f) url then
    fail "URL is invalid or exceeds the URL length limit";
  if not (starts_with (lowercase url) "https://") then fail "workspace URLs must use HTTPS";
  let authority_start = 8 in
  let authority_end =
    let rec seek i = if i >= String.length url then String.length url else match url.[i] with '/' | '?' | '#' -> i | _ -> seek (i + 1) in
    seek authority_start in
  let authority = String.sub url authority_start (authority_end - authority_start) in
  if authority = "" || String.contains authority '@' || String.contains authority '\\' ||
     String.contains authority '%' then fail "HTTPS URL authority must not contain credentials or escapes";
  let host, port =
    if authority.[0] = '[' then
      (match String.index_opt authority ']' with
       | None -> fail "HTTPS URL has an invalid IPv6 authority"
       | Some close ->
           let host = String.sub authority 1 (close - 1) in
           let rest = String.sub authority (close + 1) (String.length authority - close - 1) in
           let port = if rest = "" then 443 else if starts_with rest ":" then
             (try int_of_string (String.sub rest 1 (String.length rest - 1)) with Failure _ -> fail "HTTPS URL port is invalid")
             else fail "HTTPS URL authority is invalid" in
           host, port)
    else
      match String.split_on_char ':' (String.sub url authority_start (authority_end - authority_start)) with
      | [host] -> host, 443
      | [host; port] ->
          let port = try int_of_string port with Failure _ -> fail "HTTPS URL port is invalid" in
          host, port
      | _ -> fail "IPv6 URL hosts must be bracketed" in
  if host = "" || port <> 443 then fail "HTTPS URL host must be nonempty and use port 443";
  let host_lower = lowercase host in
  if host_lower = "localhost" || ends_with host_lower ".localhost" || ends_with host_lower ".local" then
    fail "HTTPS URLs to local hosts are not allowed";
  (host, host_lower, port, authority_end)

let resolve_host ?cancel host port =
  match (try Some (Unix.inet_addr_of_string host) with Failure _ -> None) with
  | Some ip ->
      let normalized = Unix.string_of_inet_addr ip in
      if unsafe_ip normalized then fail "HTTPS URLs to private, local, or non-public IP addresses are not allowed";
      [normalized]
  | None ->
      let labels_valid =
        String.split_on_char '.' host
        |> List.for_all (fun label ->
          String.length label > 0 && String.length label <= 63 &&
          label.[0] <> '-' && label.[String.length label - 1] <> '-') in
      if String.length host > 253 || host.[0] = '.' || host.[String.length host - 1] = '.' ||
         not labels_valid ||
         String.exists (fun c -> not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
                                      (c >= '0' && c <= '9') || c = '.' || c = '-')) host then
        fail "HTTPS URL host is invalid";
      let program, args = if Sys.file_exists "/usr/bin/dscacheutil" then
          "/usr/bin/dscacheutil", ["-q"; "host"; "-a"; "name"; host]
        else "/usr/bin/getent", ["ahosts"; host] in
      let result = run_process ?cancel ~timeout_seconds:5 ~output_limit:8192 ~program ~arguments:args () in
      let addresses = ref [] in
      String.split_on_char '\n' result.output |> List.iter (fun line ->
        let trimmed = String.trim line in
        let address = match String.split_on_char ' ' trimmed |> List.filter (( <> ) "") with
          | first :: _ when (try ignore (Unix.inet_addr_of_string first); true with Failure _ -> false) -> Some first
          | _ when starts_with (lowercase trimmed) "ip_address:" || starts_with (lowercase trimmed) "ipv6_address:" ->
              (match String.split_on_char ':' trimmed with _label :: rest -> Some (String.trim (String.concat ":" rest)) | [] -> None)
          | _ -> None in
        match address with Some value when value <> "" -> addresses := value :: !addresses | _ -> ());
      let addresses = List.sort_uniq String.compare !addresses in
      if addresses = [] then fail (Printf.sprintf "HTTPS host %s did not resolve to an address" host);
      List.iter (fun address ->
        if unsafe_ip address then fail "HTTPS URL host resolves to a private, local, or non-public IP address") addresses;
      ignore port;
      addresses


let rec read_url ?cancel url =
  let url, selector = selector_in_url url in
  let host, _, port, _ = parse_url url in
  let addresses = resolve_host ?cancel host port in
  let selected = List.hd addresses in
  let host_key = if String.contains host ':' then "[" ^ host ^ "]" else host in
  let pin_address = if String.contains selected ':' then "[" ^ selected ^ "]" else selected in
  let resolve = host_key ^ ":" ^ string_of_int port ^ ":" ^ pin_address in
  let result =
    try Workspace_process.run ?cancel ~timeout_seconds:20 ~output_limit:max_output_bytes
          ~environment:["https_proxy", ""; "HTTPS_PROXY", ""; "http_proxy", ""; "HTTP_PROXY", "";
                        "all_proxy", ""; "ALL_PROXY", ""; "no_proxy", "*"; "NO_PROXY", "*";
                        "CURL_HOME", ""; "SSLKEYLOGFILE", ""]
          ~program:"/usr/bin/curl" ~arguments:["-q"; "--silent"; "--show-error"; "--fail"; "--location";
                   "--max-redirs"; "0"; "--proto"; "=https"; "--proto-redir"; "=https";
                   "--connect-timeout"; "5"; "--max-time"; "15"; "--max-filesize";
                   string_of_int max_output_bytes; "--globoff"; "--noproxy"; "*"; "--resolve"; resolve;
                   "--netrc-file"; "/dev/null"; url] ()
    with
    | Workspace_process.Error message -> fail ("HTTPS request failed: " ^ message)
    | Unix.Unix_error (error, _, _) -> fail ("could not run HTTPS helper curl: " ^ Unix.error_message error)
  in
  let text = process_failure "/usr/bin/curl" { output = result.output; termination = result.termination; truncated = result.truncated } in
  check_cancel cancel;
  (match selector with
   | None -> check_text text
   | Some Raw -> check_output text
   | Some selected -> apply_selector (check_text text) selected)

and apply_selector text selector =
  match selector with
  | Raw -> check_output text
  | Lines ranges -> select_text_lines text ranges
  | Tail count ->
      let lines = split_lines text in
      let start = max 0 (List.length lines - count) in
      let output = Buffer.create (min max_output_bytes (String.length text)) in
      List.iteri (fun index (line, has_newline) ->
        if index >= start then append_selected_line output line has_newline) lines;
      check_output (Buffer.contents output)

and append_selected_line output line has_newline =
  if String.contains line '\000' then
    fail "binary file; workspace_reader supports text only (use :raw for bounded bytes)";
  let extra = String.length line + (if has_newline then 1 else 0) in
  if Buffer.length output + extra > max_output_bytes then
    fail (Printf.sprintf "selected text exceeds %d-byte output limit" max_output_bytes);
  Buffer.add_string output line;
  if has_newline then Buffer.add_char output '\n'

and split_lines text =
  let length = String.length text in
  let rec loop start index accumulated =
    if index = length then
      if start < length then List.rev ((String.sub text start (length - start), false) :: accumulated)
      else List.rev accumulated
    else if text.[index] = '\n' then
      loop (index + 1) (index + 1) ((String.sub text start (index - start), true) :: accumulated)
    else loop start (index + 1) accumulated
  in loop 0 0 []

and select_text_lines text ranges =
  let lines = split_lines text in
  let output = Buffer.create (min max_output_bytes (String.length text)) in
  List.iteri (fun index (line, has_newline) ->
    let number = index + 1 in
    let selected = List.exists (fun (start, ending) ->
      number >= start && (match ending with None -> true | Some last -> number <= last)) ranges in
    if selected then append_selected_line output line has_newline) lines;
  Buffer.contents output


let existing_path root relative =
  try
    let path = Workspace_path.checked_path root relative in
    try Some (path, Unix.stat path) with Unix.Unix_error (Unix.ENOENT, _, _) -> None
  with Unix.Unix_error (Unix.ENOENT, _, _) -> None

let read_directory ?cancel path selector =
  let listing = directory_listing ?cancel path in
  match selector with None -> listing | Some Raw -> listing | Some selected -> apply_selector listing selected

let sqlite_safe_path path =
  check_helper_input path "SQLite database";
  List.iter (fun suffix ->
    let sidecar = Filename.concat (Filename.dirname path) (Filename.basename path ^ suffix) in
    try
      ignore (Unix.lstat sidecar);
      fail ("SQLite read refuses a database with a " ^ suffix ^ " sidecar")
    with Unix.Unix_error (Unix.ENOENT, _, _) -> ()) ["-wal"; "-shm"; "-journal"];
  Workspace_path.with_fd path [Unix.O_RDONLY] 0 (fun fd ->
    let header = Bytes.create 20 in
    let rec fill offset =
      if offset < Bytes.length header then
        let count = Unix.read fd header offset (Bytes.length header - offset) in
        if count = 0 then fail "SQLite database has an incomplete header";
        fill (offset + count)
    in
    fill 0;
    if Bytes.sub_string header 0 16 <> "SQLite format 3\000" then
      fail "file does not contain a SQLite database";
    if Bytes.get header 18 = '\002' || Bytes.get header 19 = '\002' then
      fail "SQLite read refuses WAL-mode databases to avoid creating or reading sidecar files")

let sqlite_cli ?cancel database query =
  sqlite_safe_path database;
  let arguments =
    ["-batch"; "-bail"; "-readonly"; "-nofollow"; "-safe"; "-json";
     "-cmd"; ".limit length 4096"; "-cmd"; ".limit column 64";
     "-cmd"; ".limit sql_length 8192"; "-cmd"; "PRAGMA temp_store=MEMORY";
     "-init"; "/dev/null"; database; query] in
  let result =
    run_process ?cancel ~environment:["HOME", "/dev/null"; "SQLITE_HISTORY", "/dev/null"]
      ~timeout_seconds:15 ~output_limit:max_output_bytes ~program:"/usr/bin/sqlite3" ~arguments () in
  match String.index_opt result.output '[' with
  | None -> result
  | Some start ->
      { result with output = String.sub result.output start (String.length result.output - start) }

let quote_identifier name = "\"" ^ String.concat "\"\"" (String.split_on_char '"' name) ^ "\""

let safe_select sql =
  let sql = String.trim sql in
  if sql = "" then fail "SQLite selector requires one SELECT statement";
  let lower = lowercase sql in
  if not (starts_with lower "select" && (String.length lower = 6 || lower.[6] = ' ' || lower.[6] = '\n' || lower.[6] = '\t' || lower.[6] = '(')) then
    fail "SQLite query must be a single SELECT statement";
  let quote = ref '\000' and bracket = ref false and i = ref 0 in
  let identifier_character = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false in
  let denied_functions = ["readfile"; "writefile"; "load_extension"; "edit"; "shell"; "eval"; "fts3_tokenizer"] in
  while !i < String.length sql do
    let c = sql.[!i] in
    if !bracket then (if c = ']' then bracket := false)
    else if !quote <> '\000' then (
      if c = !quote then
        if !i + 1 < String.length sql && sql.[!i + 1] = !quote then incr i
        else quote := '\000')
    else if c = '\'' || c = '"' || c = '`' then quote := c
    else if c = '[' then bracket := true
    else if identifier_character c then (
      let start = !i in
      while !i + 1 < String.length sql && identifier_character sql.[!i + 1] do incr i done;
      let word = String.sub sql start (!i - start + 1) |> lowercase in
      if List.mem word denied_functions then
        fail ("SQLite query may not call the unsafe " ^ word ^ " function"))
    else if c = ';' then fail "SQLite query must not contain multiple statements or semicolons"
    else if c = '-' && !i + 1 < String.length sql && sql.[!i + 1] = '-' then fail "SQLite comments are not accepted"
    else if c = '/' && !i + 1 < String.length sql && sql.[!i + 1] = '*' then fail "SQLite comments are not accepted";
    incr i
  done;
  if !quote <> '\000' || !bracket then fail "SQLite query contains an unterminated quoted value";
  "SELECT * FROM (" ^ sql ^ ") LIMIT " ^ string_of_int (max_sql_rows + 1)

let sqlite_result ?cancel database directive =
  let query = match lowercase directive with
    | "schema" -> "SELECT type, name, sql FROM sqlite_schema ORDER BY type, name LIMIT " ^ string_of_int (max_sql_rows + 1)
    | _ when starts_with (lowercase directive) "table:" || starts_with (lowercase directive) "rows:" ->
        let offset = if starts_with (lowercase directive) "table:" then 6 else 5 in
        let table = String.sub directive offset (String.length directive - offset) in
        if table = "" || String.contains table '\000' then fail "SQLite table selector requires a table name";
        "SELECT * FROM " ^ quote_identifier table ^ " LIMIT " ^ string_of_int (max_sql_rows + 1)
    | _ when starts_with (lowercase directive) "select" -> safe_select directive
    | _ -> fail "SQLite query must be a single SELECT statement or a supported reader selector" in
  let result = sqlite_cli ?cancel database query in
  let json = try Yojson.Safe.from_string result.output with Yojson.Json_error _ ->
      fail "SQLite helper returned invalid JSON" in
  let rows = match json with
    | `List rows -> rows
    | _ -> fail "SQLite helper returned an invalid row set" in
  let truncated = List.length rows > max_sql_rows in
  let rows = List.filteri (fun index _ -> index < max_sql_rows) rows in
  let response = Yojson.Safe.pretty_to_string (`List rows) in
  check_output (if truncated then response ^ "\n[rows truncated at 200]" else response)

let sqlite_spec text =
  match String.index_opt text ':' with
  | None -> None
  | Some colon ->
      let database = String.sub text 0 colon in
      let directive = String.sub text (colon + 1) (String.length text - colon - 1) in
      if suffix_extension database [".sqlite"; ".sqlite3"; ".db"] <> None then
        Some (database, directive)
      else None
let read_file_kind ?cancel path selector =
  match (Unix.stat path).Unix.st_kind with
  | Unix.S_DIR -> read_directory ?cancel path selector
  | Unix.S_REG ->
      (match selector with
       | Some Raw -> read_raw_file path
       | _ ->
           if suffix_extension path [".sqlite"; ".sqlite3"; ".db"] <> None then
             let result = sqlite_result ?cancel path "schema" in
             (match selector with None | Some Raw -> result | Some selected -> apply_selector result selected)
           else
             match suffix_extension path archive_extensions with
             | Some _ ->
                 let listing = archive_list ?cancel path in
                 (match selector with None -> listing | Some selected -> apply_selector listing selected)
             | None ->
                 let converted =
                   if suffix_extension path [".ipynb"] <> None then
                     Some (notebook_text (Workspace_path.read_bounded path max_output_bytes))
                   else
                     match suffix_extension path [".pdf"; ".docx"; ".odt"; ".rtf"; ".doc"; ".ppt"; ".pptx"; ".xls"; ".xlsx"; ".ods"] with
                     | Some extension -> Some (document_text ?cancel path extension)
                     | None -> None
                 in
                 (match converted with
                  | Some text ->
                      (match selector with None -> check_text text | Some selected -> apply_selector text selected)
                  | None ->
                      read_local_file ?cancel path
                        (Option.value selector ~default:(Lines [(1, None)]))))
  | _ -> fail ("not a regular file or directory: " ^ path)


let read_artifact_content ?cancel reader id selector =
  check_cancel cancel;
  if id = "" || String.exists (fun c -> Char.code c < 0x20 || Char.code c = 0x7f) id then
    fail "artifact URI requires a valid artifact ID";
  let reader = match reader with Some reader -> reader | None -> fail "artifact reads require an owner-scoped artifact callback" in
  let content = match reader id with Some content -> content | None -> fail "artifact is unavailable or is not owned by this session" in
  check_cancel cancel;
  let content = check_output content in
  match selector with
  | Some Raw -> content
  | None -> check_text content
  | Some selector -> apply_selector (check_text content) selector


let read_workspace_path ?cancel root input =
  let root = Workspace_path.root_path root in
  match existing_path root input with
  | Some (path, _) -> read_file_kind ?cancel path None
  | None ->
      let spec_input, selector = match selector_suffix input with
        | Some (base, selector) -> base, Some selector
        | None -> input, None in
      let selected_path = match selector with
        | None -> None
        | Some _ -> Option.map fst (existing_path root spec_input) in
      (match selected_path with
       | Some path -> read_file_kind ?cancel path selector
       | None ->
           (match archive_spec spec_input with
            | Some (archive, member) ->
                let archive_path = match existing_path root archive with Some (path, _) -> path | None -> fail ("workspace path does not exist: " ^ archive) in
                if member = "" then read_file_kind ?cancel archive_path selector
                else
                  let content = archive_member ?cancel archive_path member in
                  (match selector with
                   | Some Raw -> check_output content
                   | None -> check_text content
                   | Some selected -> apply_selector (check_text content) selected)
            | None ->
                (match sqlite_spec spec_input with
                 | Some (database, directive) ->
                     let path = match existing_path root database with Some (path, { Unix.st_kind = Unix.S_REG; _ }) -> path | Some _ -> fail "SQLite path is not a regular file" | None -> fail ("workspace path does not exist: " ^ database) in
                     let result = sqlite_result ?cancel path directive in
                     (match selector with None | Some Raw -> result | Some selected -> apply_selector result selected)
                 | None ->
                     let shown = match selector_suffix input with Some (base, _) -> base | None -> input in
                     fail ("workspace path does not exist: " ^ shown))))

let read ?cancel ?read_artifact ~root ~path () =
  if String.length path > 4096 then fail "workspace read path exceeds the 4096-byte limit";
  let call f =
    check_cancel cancel;
    try check_output (f ()) with
    | Error _ as error -> raise error
    | Workspace_path.Error message -> fail message
    | Workspace_process.Error message -> fail message
    | Unix.Unix_error (Unix.ENOENT, _, _) -> fail ("workspace path does not exist: " ^ path)
    | Unix.Unix_error (Unix.EACCES, _, _) -> fail ("workspace path is not readable: " ^ path)
    | Unix.Unix_error (error, operation, argument) ->
        fail (Printf.sprintf "workspace read failed (%s %s): %s" operation argument (Unix.error_message error))
    | Sys_error message -> fail ("workspace read failed: " ^ message)
    | Yojson.Json_error message -> fail ("invalid JSON: " ^ message)
    | _ -> fail "workspace read failed due to an unexpected error"
  in
  let lower = lowercase path in
  if starts_with lower "local://" then
    let relative = String.sub path 8 (String.length path - 8) in
    if relative = "" then fail "local URI requires a workspace-relative path"
    else call (fun () -> read_workspace_path ?cancel root relative)
  else if starts_with lower "artifact://" then (
    let uri = String.sub path 11 (String.length path - 11) in
    let id, selector = match selector_suffix uri with Some (id, selector) -> id, Some selector | None -> uri, None in
    call (fun () -> read_artifact_content ?cancel read_artifact id selector))
  else if starts_with lower "https://" then call (fun () -> read_url ?cancel path)
  else if starts_with lower "http://" then fail "workspace URLs must use HTTPS"
  else if is_scheme_uri path then fail ("unsupported workspace URI scheme: " ^ String.sub path 0 (String.index path ':'))
  else call (fun () -> read_workspace_path ?cancel root path)
