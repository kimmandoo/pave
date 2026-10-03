type credentials = {
  access_key_id : string;
  secret_access_key : string;
  session_token : string option;
}

type credential_process_policy = Disabled | Allow
type http = method_:string -> url:string -> headers:(string * string) list -> body:string -> int * string
exception Cancelled

let environment name = Sys.getenv_opt name

let credential_process_policy ?(getenv = environment) () =
  match getenv "PAVE_AWS_CREDENTIAL_PROCESS" with
  | None | Some "" | Some "disabled" -> Disabled
  | Some "allow" -> Allow
  | Some _ ->
      invalid_arg "PAVE_AWS_CREDENTIAL_PROCESS must be 'allow' or 'disabled'"

let first_nonempty get names =
  List.find_map (fun name -> match get name with
    | Some value when value <> "" -> Some value
    | _ -> None) names

let region ?(getenv = environment) () =
  match first_nonempty getenv [ "AWS_REGION"; "AWS_DEFAULT_REGION" ] with
  | Some value when String.length value <= 20 &&
      String.for_all (function 'a'..'z' | '0'..'9' | '-' -> true | _ -> false) value &&
      not (String.contains value '/') -> value
  | Some _ -> invalid_arg "invalid AWS region"
  | None -> invalid_arg "AWS_REGION or AWS_DEFAULT_REGION is required for Bedrock"

let validate label value =
  if value = "" || String.exists (fun c -> Char.code c < 33 || Char.code c = 127) value then
    invalid_arg ("invalid AWS " ^ label)

let credentials ~access_key_id ~secret_access_key ~session_token =
  validate "access key ID" access_key_id;
  validate "secret access key" secret_access_key;
  Option.iter (validate "session token") session_token;
  { access_key_id; secret_access_key; session_token }

let check_cancel cancel =
  match cancel with Some check when check () -> raise Cancelled | _ -> ()

let read_file_limited ~limit path =
  try
    let channel = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
      let length = in_channel_length channel in
      if length > limit then invalid_arg "AWS credential file exceeds size limit";
      really_input_string channel length)
  with Sys_error message when not (Sys.file_exists path) ->
    ignore message; ""

let read_file path = read_file_limited ~limit:1_048_576 path

let ini_section ~name contents =
  let section = ref "" and pairs = ref [] and line_count = ref 0 in
  let parse_line start finish =
    incr line_count;
    if !line_count > 10_000 || finish - start > 8192 then
      invalid_arg "AWS profile structure exceeds limits";
    let line = String.sub contents start (finish - start) |> String.trim in
    let size = String.length line in
    if size > 1 && line.[0] = '[' && line.[size - 1] = ']' then
      section := String.trim (String.sub line 1 (size - 2))
    else if !section = name && size > 0 && line.[0] <> '#' && line.[0] <> ';' then
      match String.index_opt line '=' with
      | None -> ()
      | Some index ->
          let key = String.sub line 0 index |> String.trim |> String.lowercase_ascii in
          let value = String.sub line (index + 1) (size - index - 1) |> String.trim in
          pairs := (key, value) :: !pairs in
  let start = ref 0 in
  for index = 0 to String.length contents do
    if index = String.length contents || contents.[index] = '\n' then (
      parse_line !start index;
      start := index + 1)
  done;
  !pairs

let pair pairs name = List.assoc_opt name pairs
let nonempty pairs name = match pair pairs name with Some "" | None -> None | value -> value

let command_arguments command =
  let words = ref [] and word = Buffer.create 32 in
  let quote = ref None and escaped = ref false and started = ref false in
  let flush () =
    if !started then (words := Buffer.contents word :: !words; Buffer.clear word; started := false) in
  String.iter (fun c ->
    if !escaped then (Buffer.add_char word c; escaped := false; started := true)
    else match !quote, c with
      | Some '\'', '\'' -> quote := None
      | Some '"', '"' -> quote := None
      | Some '\'', _ -> Buffer.add_char word c
      | Some '"', '\\' -> escaped := true
      | Some _, _ -> Buffer.add_char word c
      | None, (('\'' | '"') as delimiter) -> quote := Some delimiter; started := true
      | None, '\\' -> escaped := true; started := true
      | None, (' ' | '\t' | '\r' | '\n') -> flush ()
      | None, _ -> Buffer.add_char word c; started := true) command;
  if !escaped || !quote <> None then invalid_arg "invalid AWS credential_process quoting";
  flush ();
  match List.rev !words with
  | [] -> invalid_arg "empty AWS credential_process"
  | program :: arguments -> program, arguments
let curl_path = "/usr/bin/curl"
let curl_environment = [|"LANG=C"; "LC_ALL=C"|]

(* Explicit fixture injection for tests; production requests use curl_path. *)

module Test = struct
  let curl_helper = ref None

  let use_curl_helper executable =
    curl_helper := Some (Unix.realpath executable)
end


let run_child ?exec_path ?child_environment ?cancel
    ~timeout ~output_limit ~stdin program arguments =
  let input_read, input_write = Unix.pipe () in
  let output_read, output_write = Unix.pipe () in
  let null = try Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 with exn ->
    List.iter (fun fd -> try Unix.close fd with _ -> ()) [input_read; input_write; output_read; output_write];
    raise exn in
  let pid =
    try Unix.fork () with exn ->
      List.iter (fun fd -> try Unix.close fd with _ -> ())
        [input_read; input_write; output_read; output_write; null];
      raise exn in
  if pid = 0 then (
    (try
       ignore (Unix.setsid ());
       Unix.dup2 input_read Unix.stdin;
       Unix.dup2 output_write Unix.stdout;
       Unix.dup2 null Unix.stderr;
       List.iter (fun fd -> try Unix.close fd with _ -> ())
         [input_read; input_write; output_read; output_write; null];
       let argv = Array.of_list (program :: arguments) in
       (match exec_path with
        | None -> Unix.execvp program argv
        | Some path ->
            Unix.execve path argv
              (Option.value ~default:(Unix.environment ()) child_environment))
     with _ -> Unix._exit 127));
  Unix.close input_read;
  Unix.close output_write;
  Unix.close null;
  Unix.set_nonblock input_write;
  Unix.set_nonblock output_read;
  let reaped = ref false and input_open = ref true and eof = ref false in
  let output = Buffer.create 1024 and chunk = Bytes.create 4096 in
  let input_bytes = Bytes.unsafe_of_string stdin and input_offset = ref 0 in
  let close fd = try Unix.close fd with _ -> () in
  let stop_child () =
    if not !reaped then (
      (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ ->
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()));
      (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
      reaped := true) in
  Fun.protect ~finally:(fun () ->
    close input_write;
    close output_read;
    stop_child ()) (fun () ->
    let deadline = Unix.gettimeofday () +. timeout in
    let status = ref None in
    while not !eof || !status = None do
      check_cancel cancel;
      let remaining = deadline -. Unix.gettimeofday () in
      if remaining <= 0. then invalid_arg "AWS credential helper timed out";
      let reads = if !eof then [] else [output_read] in
      let writes = if !input_open then [input_write] else [] in
      let readable, writable, _ =
        try Unix.select reads writes [] (min remaining 0.05)
        with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], [] in
      if writable <> [] && !input_open then (
        if !input_offset = Bytes.length input_bytes then (
          close input_write;
          input_open := false)
        else
          try
            let count = Unix.write input_write input_bytes !input_offset
              (Bytes.length input_bytes - !input_offset) in
            if count > 0 then input_offset := !input_offset + count
          with
          | Unix.Unix_error ((Unix.EPIPE | Unix.EBADF), _, _) ->
              close input_write; input_open := false
          | Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN), _, _) -> ());
      if readable <> [] then (
        let count = try Unix.read output_read chunk 0 (Bytes.length chunk)
          with Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN), _, _) -> -1 in
        if count = 0 then eof := true
        else if count > 0 then (
          if Buffer.length output + count > output_limit then
            invalid_arg "AWS credential helper output exceeds size limit";
          Buffer.add_subbytes output chunk 0 count));
      if !status = None then (
        let waited =
          try Unix.waitpid [Unix.WNOHANG] pid
          with Unix.Unix_error (Unix.EINTR, _, _) -> 0, Unix.WEXITED 0 in
        match waited with
        | 0, _ -> ()
        | _, result -> status := Some result; reaped := true;
            if !input_open then (close input_write; input_open := false));
    done;
    match !status with
    | Some (Unix.WEXITED 0) -> Buffer.contents output
    | Some _ -> invalid_arg "AWS credential helper failed"
    | None -> assert false)


let curl_quote value =
  if String.exists (function '\r' | '\n' | '\000' -> true | _ -> false) value then
    invalid_arg "invalid control character in AWS HTTP request";
  let out = Buffer.create (String.length value + 2) in
  Buffer.add_char out '"';
  String.iter (fun c ->
    if c = '"' || c = '\\' then Buffer.add_char out '\\';
    Buffer.add_char out c) value;
  Buffer.add_char out '"';
  Buffer.contents out

let curl_http ?cancel ~method_ ~url ~headers ~body () =
  let scheme = if String.starts_with ~prefix:"https://" url then "https"
    else if String.starts_with ~prefix:"http://" url then "http"
    else invalid_arg "unsupported AWS credential endpoint protocol" in
  let executable, environment = match !Test.curl_helper with
    | Some executable -> executable, Unix.environment ()
    | None ->
        if not (Sys.file_exists curl_path &&
            (try Unix.access curl_path [Unix.X_OK]; true
             with Unix.Unix_error _ -> false)) then
          invalid_arg "trusted AWS credential curl executable is unavailable";
        curl_path, curl_environment in
  let config = Buffer.create 512 in
  let add name value = Buffer.add_string config (name ^ " = " ^ curl_quote value ^ "\n") in
  Buffer.add_string config "silent\nshow-error\n";
  add "url" url;
  add "request" method_;
  add "proto" ("=" ^ scheme);
  add "noproxy" "*";
  add "connect-timeout" "2";
  add "max-time" "4";
  add "max-redirs" "0";
  List.iter (fun (name, value) -> add "header" (name ^ ": " ^ value)) headers;
  if body <> "" then add "data" body;
  add "write-out" "%{http_code}";
  let output = try run_child ~exec_path:executable
      ~child_environment:environment ?cancel ~timeout:5. ~output_limit:1_048_580
      ~stdin:(Buffer.contents config) "curl" ["--disable"; "--config"; "-"]
    with
    | Cancelled as exn -> raise exn
    | Invalid_argument _ as exn -> raise exn
    | Unix.Unix_error _ -> invalid_arg "could not start AWS credential HTTP request" in
  let size = String.length output in
  if size < 3 then invalid_arg "AWS credential HTTP response omitted status";
  let status = try int_of_string (String.sub output (size - 3) 3)
    with Failure _ -> invalid_arg "AWS credential HTTP response has invalid status" in
  status, String.sub output 0 (size - 3)

let form_encode value =
  let out = Buffer.create (String.length value) in
  String.iter (fun c -> match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' -> Buffer.add_char out c
    | _ -> Buffer.add_string out (Printf.sprintf "%%%02X" (Char.code c))) value;
  Buffer.contents out
let uri_authority url scheme =
  let prefix = scheme ^ "://" in
  if not (String.starts_with ~prefix url) then None else
  let start = String.length prefix in
  let rec stop index =
    if index = String.length url || List.mem url.[index] ['/'; '?'; '#'] then index
    else stop (index + 1) in
  let finish = stop start in
  if finish = start then None
  else Some (String.sub url start (finish - start))

let authority_host_port authority =
  let port_ok suffix =
    if suffix = "" then true
    else if String.length suffix <= 1 || suffix.[0] <> ':' then false
    else
      let port = String.sub suffix 1 (String.length suffix - 1) in
      if not (String.for_all (function '0'..'9' -> true | _ -> false) port) then false
      else try let port = int_of_string port in port >= 1 && port <= 65535
        with Failure _ -> false in
  let valid_host ~ipv6 host =
    host <> "" && String.for_all (fun c ->
      match c with
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '.' | '-' -> true
      | ':' when ipv6 -> true
      | _ -> false) host in
  if String.contains authority '@' then None
  else if authority.[0] = '[' then
    (match String.index_opt authority ']' with
     | None -> None
     | Some close ->
         let host = String.sub authority 1 (close - 1) in
         let suffix = String.sub authority (close + 1) (String.length authority - close - 1) in
         if valid_host ~ipv6:true host && port_ok suffix then Some host else None)
  else
    let host, suffix = match String.rindex_opt authority ':' with
      | None -> authority, ""
      | Some colon ->
          String.sub authority 0 colon,
          String.sub authority colon (String.length authority - colon) in
    if valid_host ~ipv6:false host && port_ok suffix then Some host else None

let loopback_http_url url =
  match uri_authority url "http" with
  | None -> false
  | Some authority ->
      (match authority_host_port authority with
       | Some "localhost" | Some "::1" -> true
       | Some host ->
           (match String.split_on_char '.' host with
            | [first; second; third; fourth] ->
                (try int_of_string first = 127 &&
                  List.for_all (fun octet ->
                    let value = int_of_string octet in value >= 0 && value <= 255)
                    [first; second; third; fourth]
                 with Failure _ -> false)
            | _ -> false)
       | None -> false)

let safe_https_url url =
  match uri_authority url "https" with
  | Some authority ->
      authority_host_port authority <> None &&
      not (String.contains url '#' || String.exists
        (fun c -> Char.code c < 33 || Char.code c = 127) url)
  | None -> false


let days_from_civil year month day =
  let year = if month <= 2 then year - 1 else year in
  let era = if year >= 0 then year / 400 else (year - 399) / 400 in
  let year_of_era = year - era * 400 in
  let month_prime = month + (if month > 2 then -3 else 9) in
  let day_of_year = (153 * month_prime + 2) / 5 + day - 1 in
  let day_of_era = year_of_era * 365 + year_of_era / 4 - year_of_era / 100 + day_of_year in
  era * 146097 + day_of_era - 719468

let expiration_epoch value =
  let length = String.length value in
  let digits start count =
    count > 0 && start >= 0 && start + count <= length &&
    String.for_all (function '0'..'9' -> true | _ -> false)
      (String.sub value start count) in
  let digit offset count = int_of_string (String.sub value offset count) in
  try
    if length < 20 || value.[4] <> '-' || value.[7] <> '-' ||
       value.[10] <> 'T' || value.[13] <> ':' || value.[16] <> ':' ||
       not (digits 0 4 && digits 5 2 && digits 8 2 &&
            digits 11 2 && digits 14 2 && digits 17 2) then raise Exit;
    let year = digit 0 4 and month = digit 5 2 and day = digit 8 2 in
    let hour = digit 11 2 and minute = digit 14 2 and second = digit 17 2 in
    let zone_start =
      if value.[19] <> '.' then 19
      else
        let rec end_fraction index =
          if index < length && value.[index] >= '0' && value.[index] <= '9' then
            end_fraction (index + 1) else index in
        let ending = end_fraction 20 in
        if ending = 20 then raise Exit;
        ending in
    let zone_offset = match value.[zone_start] with
      | 'Z' when zone_start + 1 = length -> 0
      | ('+' | '-') as sign when zone_start + 6 = length &&
          value.[zone_start + 3] = ':' && digits (zone_start + 1) 2 &&
          digits (zone_start + 4) 2 ->
          let hours = digit (zone_start + 1) 2 and minutes = digit (zone_start + 4) 2 in
          if hours > 23 || minutes > 59 then raise Exit;
          (if sign = '+' then 1 else -1) * (hours * 3600 + minutes * 60)
      | _ -> raise Exit in
    let leap = year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0) in
    let days = [|31; (if leap then 29 else 28); 31; 30; 31; 30;
      31; 31; 30; 31; 30; 31|] in
    if month < 1 || month > 12 || day < 1 || day > days.(month - 1) ||
       hour > 23 || minute > 59 || second > 59 then raise Exit;
    Some (float_of_int (days_from_civil year month day * 86400 +
      hour * 3600 + minute * 60 + second - zone_offset))
  with _ -> None

let check_expiration_epoch expiry =
  if expiry <= Unix.gettimeofday () then
    invalid_arg "AWS credentials are expired or have invalid expiration"

let check_expiration value =
  match expiration_epoch value with
  | Some expiry -> check_expiration_epoch expiry
  | None -> invalid_arg "AWS credentials are expired or have invalid expiration"

let xml_unescape value =
  let out = Buffer.create (String.length value) in
  let rec loop index =
    if index >= String.length value then ()
    else if value.[index] <> '&' then (Buffer.add_char out value.[index]; loop (index + 1))
    else match String.index_from_opt value (index + 1) ';' with
      | None -> invalid_arg "invalid AWS STS XML entity"
      | Some finish ->
          let entity = String.sub value (index + 1) (finish - index - 1) in
          let decoded = match entity with
            | "amp" -> "&" | "lt" -> "<" | "gt" -> ">"
            | "quot" -> "\"" | "apos" -> "'"
            | _ when String.starts_with ~prefix:"#" entity ->
                let codepoint = try
                    if String.starts_with ~prefix:"#x" entity ||
                       String.starts_with ~prefix:"#X" entity then
                      int_of_string ("0x" ^ String.sub entity 2 (String.length entity - 2))
                    else int_of_string (String.sub entity 1 (String.length entity - 1))
                  with Failure _ -> invalid_arg "invalid AWS STS XML entity" in
                if codepoint <= 0 || codepoint > 0x10ffff ||
                   codepoint >= 0xd800 && codepoint <= 0xdfff then
                  invalid_arg "invalid AWS STS XML codepoint";
                let bytes = Buffer.create 4 in
                if codepoint < 0x80 then Buffer.add_char bytes (Char.chr codepoint)
                else if codepoint < 0x800 then (
                  Buffer.add_char bytes (Char.chr (0xc0 lor (codepoint lsr 6)));
                  Buffer.add_char bytes (Char.chr (0x80 lor (codepoint land 0x3f)))
                ) else if codepoint < 0x10000 then (
                  Buffer.add_char bytes (Char.chr (0xe0 lor (codepoint lsr 12)));
                  Buffer.add_char bytes (Char.chr (0x80 lor ((codepoint lsr 6) land 0x3f)));
                  Buffer.add_char bytes (Char.chr (0x80 lor (codepoint land 0x3f)))
                ) else (
                  Buffer.add_char bytes (Char.chr (0xf0 lor (codepoint lsr 18)));
                  Buffer.add_char bytes (Char.chr (0x80 lor ((codepoint lsr 12) land 0x3f)));
                  Buffer.add_char bytes (Char.chr (0x80 lor ((codepoint lsr 6) land 0x3f)));
                  Buffer.add_char bytes (Char.chr (0x80 lor (codepoint land 0x3f))));
                Buffer.contents bytes
            | _ -> invalid_arg "unsupported AWS STS XML entity" in
          Buffer.add_string out decoded;
          loop (finish + 1) in
  loop 0;
  Buffer.contents out

let xml_tag name xml =
  let open_tag = "<" ^ name ^ ">" and close_tag = "</" ^ name ^ ">" in
  match String.index_from_opt xml 0 '<' with
  | None -> None
  | Some _ ->
      let rec find_from offset =
        match String.index_from_opt xml offset '<' with
        | None -> None
        | Some start when start + String.length open_tag <= String.length xml &&
            String.sub xml start (String.length open_tag) = open_tag ->
              let content = start + String.length open_tag in
              (match String.index_from_opt xml content '<' with
               | Some finish when finish + String.length close_tag <= String.length xml &&
                   String.sub xml finish (String.length close_tag) = close_tag ->
                   Some (xml_unescape (String.sub xml content (finish - content)))
               | _ -> None)
        | Some start -> find_from (start + 1) in
      find_from 0

let sts_credentials xml =
  let tag name = match xml_tag name xml with
    | Some value -> value
    | None -> invalid_arg ("AWS STS response omitted " ^ name) in
  let access_key_id = tag "AccessKeyId" and secret_access_key = tag "SecretAccessKey" in
  let session_token = tag "SessionToken" in
  check_expiration (tag "Expiration");
  credentials ~access_key_id ~secret_access_key ~session_token:(Some session_token)

let validate_json_structure json =
  let visited = ref 0 in
  let rec visit depth value =
    incr visited;
    if depth > 32 || !visited > 10_000 then
      invalid_arg "AWS credential response exceeds structure limits";
    match value with
    | `Assoc fields -> List.iter (fun (_, child) -> visit (depth + 1) child) fields
    | `List values -> List.iter (visit (depth + 1)) values
    | _ -> () in
  visit 0 json

let parse_credentials_json ?(access="AccessKeyId") ?(secret="SecretAccessKey")
    ?(token_names=["SessionToken"; "Token"]) ?(require_token = false)
    ?(expiration_name="Expiration") ?expiration json =
  validate_json_structure json;
  let get name = Protocol.member name json in
  let access_key_id = match get access with `String value -> value | _ ->
    invalid_arg "AWS credential response omitted access key ID" in
  let secret_access_key = match get secret with `String value -> value | _ ->
    invalid_arg "AWS credential response omitted secret access key" in
  List.iter (fun name -> match get name with
    | `Null | `String _ -> ()
    | _ -> invalid_arg "invalid AWS credential session token") token_names;
  let session_token = List.find_map (fun name -> match get name with
    | `String value -> Some value | _ -> None) token_names in
  if require_token && session_token = None then
    invalid_arg "AWS temporary credentials omitted session token";
  (match expiration, get expiration_name with
   | Some true, `String value -> check_expiration value
   | Some true, `Int value when value > 0 ->
       check_expiration_epoch (float_of_int value /. 1000.)
   | Some true, _ -> invalid_arg "AWS credential response omitted or invalid expiration"
   | _ -> ());
  credentials ~access_key_id ~secret_access_key ~session_token

let process_credentials ?cancel command =
  let program, arguments = command_arguments command in
  let output = try run_child ?cancel ~timeout:5. ~output_limit:65_536 ~stdin:""
    program arguments with
    | Cancelled as exn -> raise exn
    | Invalid_argument _ as exn -> raise exn
    | Unix.Unix_error _ -> invalid_arg "could not start AWS credential_process" in
  let json = try Yojson.Basic.from_string output with
    | Yojson.Json_error _ | Stack_overflow ->
        invalid_arg "invalid AWS credential_process JSON" in
  validate_json_structure json;
  if Protocol.member "Version" json <> `Int 1 then
    invalid_arg "unsupported AWS credential_process version";
  (match Protocol.member "Expiration" json with
   | `Null -> ()
   | `String value -> check_expiration value
   | _ -> invalid_arg "invalid AWS credential_process expiration");
  parse_credentials_json ~token_names:["SessionToken"] json

let amz_date () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let sha256 value = Digestif.SHA256.(to_hex (digest_string value))
let hmac key value = Digestif.SHA256.(to_raw_string (hmac_string ~key value))
let hmac_hex key value = Digestif.SHA256.(to_hex (hmac_string ~key value))

(* The HTTP path is already percent-encoded. SigV4's non-S3 canonical URI
   escapes each segment again (including '%' -> '%25'), preserving '/'. *)
let canonical_path path =
  let out = Buffer.create (String.length path) in
  String.iter (fun c -> match c with
    | '/' | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' ->
        Buffer.add_char out c
    | _ -> Buffer.add_string out (Printf.sprintf "%%%02X" (Char.code c))) path;
  Buffer.contents out
let query_component value =
  let output = Buffer.create (String.length value) in
  String.iter (fun character ->
    match character with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' ->
        Buffer.add_char output character
    | _ -> Buffer.add_string output (Printf.sprintf "%%%02X" (Char.code character)))
    value;
  Buffer.contents output

let query_string parameters =
  parameters
  |> List.map (fun (key, value) ->
    if key = "" || String.length key > 4096 || String.length value > 4096 then
      invalid_arg "AWS query parameter exceeds limits";
    query_component key, query_component value)
  |> List.sort compare
  |> List.map (fun (key, value) -> key ^ "=" ^ value)
  |> String.concat "&"

let sign_service ~service ?content_type ?(accept = "application/json")
    ?(extra_headers = []) ?(query = []) ~credentials:keys ~region:aws_region
    ~amz_date ~method_ ~host ~path ~body () =
  if String.length amz_date <> 16 || amz_date.[8] <> 'T' || amz_date.[15] <> 'Z' ||
    not (String.for_all (function '0'..'9' | 'T' | 'Z' -> true | _ -> false) amz_date)
  then invalid_arg "invalid AWS signing date";
  let region = region ~getenv:(fun _ -> Some aws_region) () in
  validate "host" host;
  validate "signing service" service;
  if path = "" || path.[0] <> '/' ||
    String.exists (function ' ' | '\r' | '\n' | '?' | '#' -> true | _ -> false) path
  then invalid_arg "AWS path must be encoded before signing";
  let safe_header (name, value) =
    name <> "" &&
    String.for_all (function 'a'..'z' | '0'..'9' | '-' -> true | _ -> false) name &&
    not (String.exists (fun c -> Char.code c < 32 || Char.code c = 127) value) in
  let short_date = String.sub amz_date 0 8 in
  let payload_hash = sha256 body in
  let fields = [ "accept", accept ] @
    (match content_type with None -> [] | Some value -> ["content-type", value]) @
    [ "host", host; "x-amz-content-sha256", payload_hash; "x-amz-date", amz_date ] @
    (match keys.session_token with
      | None -> [] | Some token -> [ "x-amz-security-token", token ]) @ extra_headers in
  if not (List.for_all safe_header fields) ||
     List.length (List.map fst fields) <>
       List.length (List.sort_uniq String.compare (List.map fst fields)) then
    invalid_arg "invalid or duplicate AWS signing header";
  let fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
  let names = String.concat ";" (List.map fst fields) in
  let canonical_headers = String.concat "" (List.map (fun (name, value) -> name ^ ":" ^ value ^ "\n") fields) in
  let canonical = String.concat "\n"
    [method_; canonical_path path; query_string query; canonical_headers;
     names; payload_hash] in
  let scope = short_date ^ "/" ^ region ^ "/" ^ service ^ "/aws4_request" in
  let to_sign = String.concat "\n" [ "AWS4-HMAC-SHA256"; amz_date; scope; sha256 canonical ] in
  let key = hmac ("AWS4" ^ keys.secret_access_key) short_date in
  let key = hmac key region |> fun key -> hmac key service |> fun key -> hmac key "aws4_request" in
  let authorization = "AWS4-HMAC-SHA256 Credential=" ^ keys.access_key_id ^ "/" ^ scope ^
    ", SignedHeaders=" ^ names ^ ", Signature=" ^ hmac_hex key to_sign in
  ("authorization", authorization) :: fields

let sign ?(content_type = true) ?(accept = "application/json")
    ?(extra_headers = []) ?(query = []) ~credentials:keys ~region:aws_region
    ~amz_date ~method_ ~host ~path ~body () =
  sign_service ~service:"bedrock" ~accept ~extra_headers ~query
    ?content_type:(if content_type then Some "application/json" else None)
    ~credentials:keys ~region:aws_region ~amz_date ~method_ ~host ~path ~body ()

let sign_converse_stream ~credentials ~region ~amz_date ~method_ ~host ~path ~body () =
  sign ~accept:"application/vnd.amazon.eventstream"
    ~extra_headers:["x-amzn-bedrock-accept", "application/json"]
    ~credentials ~region ~amz_date ~method_ ~host ~path ~body ()
(* Resolve documented AWS credential sources with explicit local precedence.
   Running credential_process can execute arbitrary local code, so it is opt-in. *)
let resolve ?(getenv = environment) ?credentials_file ?config_file
    ?(credential_process_policy = Disabled) ?cancel ?http ?sso_cache_dir () =
  let get name = first_nonempty getenv [name] in
  let check () = check_cancel cancel in
  let perform ~method_ ~url ~headers ~body =
    check ();
    let header_bytes = List.fold_left (fun total (name, value) ->
      total + String.length name + String.length value) 0 headers in
    if String.length url > 8192 || String.length body > 1_048_576 ||
       List.length headers > 32 || header_bytes > 65_536 ||
       String.exists (function ' ' | '\r' | '\n' -> true | _ -> false) method_ then
      invalid_arg "AWS credential request exceeds limits";
    let status, response = match http with
      | Some call -> call ~method_ ~url ~headers ~body
      | None -> curl_http ?cancel ~method_ ~url ~headers ~body () in
    check ();
    if String.length response > 1_048_576 then invalid_arg "AWS credential response exceeds 1 MiB";
    if status < 200 || status >= 300 then
      invalid_arg (Printf.sprintf "AWS credential endpoint returned HTTP %d" status);
    status, response in
  let home_file name = match get "HOME" with
    | Some home -> Filename.concat (Filename.concat home ".aws") name
    | None -> "" in
  let path explicit variable fallback = match explicit with
    | Some path -> path
    | None -> (match get variable with Some path -> path | None -> home_file fallback) in
  let credentials_contents = lazy (read_file
    (path credentials_file "AWS_SHARED_CREDENTIALS_FILE" "credentials")) in
  let config_contents = lazy (read_file
    (path config_file "AWS_CONFIG_FILE" "config")) in
  let profile = Option.value (get "AWS_PROFILE") ~default:
    (Option.value (get "AWS_DEFAULT_PROFILE") ~default:"default") in
  let section file name = ini_section ~name file in
  let credentials_profile name = section (Lazy.force credentials_contents) name in
  let config_profile name =
    section (Lazy.force config_contents)
      (if name = "default" then "default" else "profile " ^ name) in
  let source_credentials pairs =
    match nonempty pairs "aws_access_key_id", nonempty pairs "aws_secret_access_key" with
    | Some access_key_id, Some secret_access_key ->
        Some (credentials ~access_key_id ~secret_access_key
          ~session_token:(match nonempty pairs "aws_session_token" with
            | Some _ as value -> value
            | None -> nonempty pairs "aws_security_token"))
    | Some _, None | None, Some _ -> invalid_arg "incomplete AWS profile credentials"
    | None, None -> None in
  let getenv_credentials () =
    match get "AWS_ACCESS_KEY_ID", get "AWS_SECRET_ACCESS_KEY" with
    | Some access_key_id, Some secret_access_key ->
        Some (credentials ~access_key_id ~secret_access_key ~session_token:(get "AWS_SESSION_TOKEN"))
    | Some _, None | None, Some _ -> invalid_arg "incomplete AWS environment credentials"
    | None, None -> None in
  let expected_region () =
    let value = match get "AWS_REGION", get "AWS_DEFAULT_REGION" with
      | Some value, _ | None, Some value -> value
      | None, None -> "us-east-1" in
    ignore (region ~getenv:(fun _ -> Some value) ());
    value in
  let sts_url () = "https://sts." ^ expected_region () ^ ".amazonaws.com/" in
  let session_name fallback = function
    | Some value when value <> "" -> value
    | _ -> fallback in
  let web_identity ~role_arn ~token_file ~role_session_name =
    let token = read_file_limited ~limit:256_000 token_file |> String.trim in
    if token = "" then invalid_arg "AWS web identity token file is empty";
    let session = session_name "pave-web-identity" role_session_name in
    validate "web identity role ARN" role_arn;
    validate "role session name" session;
    let body = String.concat "&" [
      "Action=AssumeRoleWithWebIdentity"; "Version=2011-06-15";
      "RoleArn=" ^ form_encode role_arn;
      "RoleSessionName=" ^ form_encode session;
      "WebIdentityToken=" ^ form_encode token] in
    let _, xml = perform ~method_:"POST" ~url:(sts_url ())
      ~headers:["content-type", "application/x-www-form-urlencoded; charset=utf-8"]
      ~body in
    sts_credentials xml in
  let imds_endpoint () =
    match get "AWS_EC2_METADATA_SERVICE_ENDPOINT" with
    | None ->
        (match get "AWS_EC2_METADATA_SERVICE_ENDPOINT_MODE" with
         | Some "IPv6" -> "http://[fd00:ec2::254]"
         | Some "IPv4" | None -> "http://169.254.169.254"
         | Some _ -> invalid_arg "invalid AWS metadata endpoint mode")
    | Some url ->
        let safe = safe_https_url url || loopback_http_url url in
        let root =
          match uri_authority url "https", uri_authority url "http" with
          | Some authority, _ ->
              let after = String.length "https://" + String.length authority in
              let suffix = String.sub url after (String.length url - after) in
              suffix = "" || suffix = "/"
          | _, Some authority ->
              let after = String.length "http://" + String.length authority in
              let suffix = String.sub url after (String.length url - after) in
              suffix = "" || suffix = "/"
          | _ -> false in
        if not safe || not root then invalid_arg "unsafe AWS metadata endpoint";
        url in
  let imds () =
    if String.lowercase_ascii (Option.value (get "AWS_EC2_METADATA_DISABLED") ~default:"") = "true" then
      invalid_arg "AWS instance metadata is disabled";
    let endpoint = imds_endpoint () in
    let base = String.trim endpoint in
    let base = if String.ends_with ~suffix:"/" base then
        String.sub base 0 (String.length base - 1) else base in
    let _, token = perform ~method_:"PUT"
      ~url:(base ^ "/latest/api/token")
      ~headers:["x-aws-ec2-metadata-token-ttl-seconds", "21600"] ~body:"" in
    let token = String.trim token in
    if token = "" || String.length token > 16_384 then invalid_arg "invalid AWS metadata token";
    let headers = ["x-aws-ec2-metadata-token", token] in
    let _, role_body = perform ~method_:"GET"
      ~url:(base ^ "/latest/meta-data/iam/security-credentials/") ~headers ~body:"" in
    let roles = String.split_on_char '\n' role_body |> List.map String.trim
      |> List.filter ((<>) "") in
    let role = match roles with [role] -> role
      | [] -> invalid_arg "AWS instance role is unavailable"
      | _ -> invalid_arg "multiple AWS instance roles returned" in
    if String.contains role '/' || String.contains role '\r' then
      invalid_arg "invalid AWS instance role name";
    let _, json_body = perform ~method_:"GET"
      ~url:(base ^ "/latest/meta-data/iam/security-credentials/" ^ form_encode role)
      ~headers ~body:"" in
    let json = try Yojson.Basic.from_string json_body with
      | Yojson.Json_error _ | Stack_overflow ->
          invalid_arg "invalid AWS metadata credential response" in
    validate_json_structure json;
    if Protocol.member "Code" json <> `String "Success" then
      invalid_arg "AWS instance metadata returned no active credentials";
    parse_credentials_json ~token_names:["Token"; "SessionToken"]
      ~require_token:true ~expiration:true json in
  let container () =
    let token = match get "AWS_CONTAINER_AUTHORIZATION_TOKEN" with
      | Some value -> Some value
      | None ->
          (match get "AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE" with
           | Some path -> Some (read_file_limited ~limit:16_384 path |> String.trim)
           | None -> None) in
    let headers = match token with None -> []
      | Some value when value <> "" && not (String.exists
          (fun c -> Char.code c < 32 || Char.code c = 127) value) ->
          ["authorization", value]
      | Some _ -> invalid_arg "invalid AWS container authorization token" in
    let endpoint = match get "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI",
      get "AWS_CONTAINER_CREDENTIALS_FULL_URI" with
      | Some relative, _ ->
          if relative = "" || String.length relative > 4096 || relative.[0] <> '/' ||
             String.starts_with ~prefix:"//" relative ||
             String.contains relative '#' || String.contains relative '\\' ||
             String.exists (fun c -> Char.code c < 33 || Char.code c = 127) relative ||
             List.mem ".." (String.split_on_char '/' relative) then
            invalid_arg "unsafe AWS container credential URI";
          "http://169.254.170.2" ^ relative
      | None, Some full ->
          let safe_local = loopback_http_url full &&
            not (String.contains full '#' || String.exists
              (fun c -> Char.code c < 33 || Char.code c = 127) full) in
          if not (safe_https_url full || safe_local) then
            invalid_arg "unsafe AWS container credential endpoint";
          full
      | None, None -> invalid_arg "AWS container credential endpoint unavailable" in
    let _, response = perform ~method_:"GET" ~url:endpoint ~headers ~body:"" in
    let json = try Yojson.Basic.from_string response with
      | Yojson.Json_error _ | Stack_overflow ->
          invalid_arg "invalid AWS container credential response" in
    validate_json_structure json;
    parse_credentials_json ~require_token:true json in
  let assume_role ~role_arn ~session ~external_id source =
    let aws_region = expected_region () in
    let fields = [
      "Action=AssumeRole"; "Version=2011-06-15";
      "RoleArn=" ^ form_encode role_arn;
      "RoleSessionName=" ^ form_encode session] @
      (match external_id with None -> [] | Some id -> ["ExternalId=" ^ form_encode id]) in
    let body = String.concat "&" fields in
    let url = sts_url () in
    let host = "sts." ^ aws_region ^ ".amazonaws.com" in
    let headers = sign_service ~service:"sts"
      ~content_type:"application/x-www-form-urlencoded; charset=utf-8"
      ~credentials:source ~region:aws_region ~amz_date:(amz_date ())
      ~method_:"POST" ~host ~path:"/" ~body () in
    let _, xml = perform ~method_:"POST" ~url ~headers ~body in
    sts_credentials xml in
  let profile_credentials () =
    let rec resolve_profile depth seen name =
      if depth > 8 || List.mem name seen then invalid_arg "cyclic AWS source_profile chain";
      let static = credentials_profile name and configured = config_profile name in
      let get_config key = nonempty configured key in
      let config_value key = get_config key in
      let role_arn = config_value "role_arn" in
      let profile_web_token = config_value "web_identity_token_file" in
      let sso_session = config_value "sso_session" in
      let sso_start = config_value "sso_start_url" in
      let sso_region = config_value "sso_region" in
      let sso_account = config_value "sso_account_id" in
      let sso_role = config_value "sso_role_name" in
      let process = config_value "credential_process" in
      let source_profile = config_value "source_profile" in
      let credential_source = config_value "credential_source" in
      match role_arn, profile_web_token with
      | Some role, Some token_file ->
          Some (web_identity ~role_arn:role ~token_file
            ~role_session_name:(config_value "role_session_name"))
      | Some _, None when source_profile <> None || credential_source <> None ->
          let role = Option.get role_arn in
          let source = match source_profile, credential_source with
            | Some _, Some _ -> invalid_arg "AWS role profile cannot set both source_profile and credential_source"
            | Some source, None ->
                (match resolve_profile (depth + 1) (name :: seen) source with
                 | Some keys -> keys
                 | None -> invalid_arg "AWS source_profile has no credentials")
            | None, Some "Environment" ->
                (match getenv_credentials () with Some keys -> keys
                 | None -> invalid_arg "AWS role source Environment has no credentials")
            | None, Some "EcsContainer" -> container ()
            | None, Some "Ec2InstanceMetadata" -> imds ()
            | None, Some _ -> invalid_arg "unsupported AWS credential_source"
            | None, None -> assert false in
          Some (assume_role ~role_arn:role
            ~session:(session_name "pave-assume-role" (config_value "role_session_name"))
            ~external_id:(config_value "external_id") source)
      | Some _, None -> invalid_arg "AWS role profile requires source_profile or credential_source"
      | None, Some _ -> invalid_arg "AWS web_identity_token_file requires role_arn"
      | None, None when sso_session <> None || sso_start <> None ||
          sso_account <> None || sso_role <> None ->
          let start_url, sso_region, cache_key =
            match sso_session with
            | Some session ->
                let section = section (Lazy.force config_contents) ("sso-session " ^ session) in
                (Option.value (nonempty section "sso_start_url") ~default:"",
                 Option.value (nonempty section "sso_region") ~default:"",
                 session)
            | None ->
                (Option.value sso_start ~default:"",
                 Option.value sso_region ~default:"",
                 Option.value sso_start ~default:"") in
          if start_url = "" || sso_region = "" || sso_account = None || sso_role = None then
            invalid_arg "incomplete AWS IAM Identity Center profile";
          ignore (region ~getenv:(fun _ -> Some sso_region) ());
          let cache_dir = match sso_cache_dir, get "HOME" with
            | Some path, _ -> path
            | None, Some home ->
                Filename.concat (Filename.concat (Filename.concat home ".aws") "sso") "cache"
            | None, None -> invalid_arg "HOME unavailable for AWS SSO token cache" in
          let cache_path = Filename.concat cache_dir
            (Digestif.SHA1.(to_hex (digest_string cache_key)) ^ ".json") in
          let cache_json = try Yojson.Basic.from_string (read_file cache_path) with
            | Yojson.Json_error _ | Stack_overflow -> invalid_arg "invalid AWS SSO token cache" in
          validate_json_structure cache_json;
          let access_token = match Protocol.member "accessToken" cache_json with
            | `String value when value <> "" -> value
            | _ -> invalid_arg "AWS SSO access token unavailable; run aws sso login" in
          (match Protocol.member "expiresAt" cache_json with
           | `String expiry -> check_expiration expiry
           | _ -> invalid_arg "AWS SSO token cache omitted expiration");
          let account = Option.get sso_account and role = Option.get sso_role in
          let url = "https://portal.sso." ^ sso_region
            ^ ".amazonaws.com/federation/credentials?role_name=" ^ form_encode role
            ^ "&account_id=" ^ form_encode account in
          let _, response = perform ~method_:"GET" ~url
            ~headers:["x-amz-sso_bearer_token", access_token] ~body:"" in
          let json = try Yojson.Basic.from_string response with
            | Yojson.Json_error _ | Stack_overflow ->
                invalid_arg "invalid AWS SSO credential response" in
          let role_creds = Protocol.member "roleCredentials" json in
          Some (parse_credentials_json ~access:"accessKeyId" ~secret:"secretAccessKey"
            ~token_names:["sessionToken"] ~require_token:true
            ~expiration_name:"expiration" ~expiration:true role_creds)
      | None, None ->
          (match source_credentials static with
           | Some keys -> Some keys
           | None ->
               (match process with
                | Some command ->
                    (match credential_process_policy with
                     | Disabled -> invalid_arg "AWS credential_process is configured; explicit opt-in is required"
                     | Allow -> Some (process_credentials ?cancel command))
                | None -> source_credentials configured)) in
    resolve_profile 0 [] profile in
  check ();
  match getenv_credentials () with
  | Some keys -> keys
  | None ->
      (match get "AWS_ROLE_ARN", get "AWS_WEB_IDENTITY_TOKEN_FILE" with
       | Some role, Some token_file ->
           web_identity ~role_arn:role ~token_file ~role_session_name:(get "AWS_ROLE_SESSION_NAME")
       | Some _, None | None, Some _ -> invalid_arg "incomplete AWS web identity configuration"
       | None, None ->
           (match profile_credentials () with
            | Some keys -> keys
            | None ->
                if get "AWS_PROFILE" <> None || get "AWS_DEFAULT_PROFILE" <> None then
                  invalid_arg ("AWS credentials unavailable for profile " ^ profile)
                else
                  match get "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI",
                    get "AWS_CONTAINER_CREDENTIALS_FULL_URI" with
                  | Some _, _ | None, Some _ -> container ()
                  | None, None ->
                      (try imds () with
                       | Invalid_argument _ when
                           String.lowercase_ascii (Option.value (get "AWS_EC2_METADATA_DISABLED")
                             ~default:"") = "true" ->
                             invalid_arg ("AWS credentials unavailable for profile " ^ profile)
                       | Invalid_argument reason ->
                           invalid_arg ("AWS credentials unavailable for profile " ^ profile ^
                             " (instance metadata: " ^ reason ^ ")"))))
