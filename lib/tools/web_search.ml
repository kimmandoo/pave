type provider = Brave | Tavily

type request = {
  method_ : string;
  url : string;
  headers : (string * string) list;
  body : string;
  response_limit : int;
}
type http = request -> (int * string, string) result

type result = {
  title : string;
  url : string;
  snippet : string;
  provider : string;
  citation : string;
}
type response = {
  provider : string;
  query : string;
  page : int;
  results : result list;
  citations : string list;
}
type fetched_page = { source_url : string; markdown : string }

exception Error of string
let fail message = raise (Error message)
let brave_endpoint = "https://api.search.brave.com/res/v1/web/search"
let tavily_endpoint = "https://api.tavily.com/search"
let max_query_bytes = 600
let max_results = 20
let max_page = 9
let max_response_bytes = 256 * 1_024
let max_content_bytes = Workspace_reader.max_output_bytes
let timeout_seconds = 20

let starts_with text prefix =
  String.length text >= String.length prefix &&
  String.sub text 0 (String.length prefix) = prefix
let lowercase = String.lowercase_ascii
let contains_control text = String.exists (fun c -> Char.code c < 32 || Char.code c = 127) text
let option name value = name ^ " = " ^ Provider.quote_config value ^ "\n"

let valid_query query =
  if String.trim query = "" then fail "search query must not be empty";
  if String.length query > max_query_bytes then fail "search query exceeds 600 bytes";
  if String.contains query '\000' || contains_control query then fail "search query contains a control character";
  let words = String.split_on_char ' ' query
    |> List.concat_map (String.split_on_char '\t')
    |> List.concat_map (String.split_on_char '\n')
    |> List.concat_map (String.split_on_char '\r')
    |> List.filter (fun word -> String.trim word <> "") in
  if List.length words > 75 then fail "search query exceeds the provider's 75-word limit";
  query

let validate_limits ~page ~count =
  if count < 1 || count > max_results then fail "result count must be between 1 and 20";
  if page < 0 || page > max_page then fail "page must be between 0 and 9"

let percent_encode text =
  let output = Buffer.create (String.length text + 16) in
  String.iter (fun c ->
    let code = Char.code c in
    if (code >= Char.code 'a' && code <= Char.code 'z') ||
       (code >= Char.code 'A' && code <= Char.code 'Z') ||
       (code >= Char.code '0' && code <= Char.code '9') ||
       c = '-' || c = '.' || c = '_' || c = '~' then Buffer.add_char output c
    else Buffer.add_string output (Printf.sprintf "%%%02X" code)) text;
  Buffer.contents output

let provider_name = function Brave -> "brave" | Tavily -> "tavily"
let provider_of_name = function
  | "brave" -> Brave
  | "tavily" -> Tavily
  | name -> fail ("unsupported search provider in PAVE_WEB_SEARCH_PROVIDER_PRIORITY: " ^ name)

let parse_priority value =
  let names = String.split_on_char ',' value |> List.map String.trim in
  if names = [] || List.exists (( = ) "") names then
    fail "PAVE_WEB_SEARCH_PROVIDER_PRIORITY must be a comma-separated list of brave and/or tavily";
  let providers = List.map provider_of_name names in
  let seen = Hashtbl.create 2 in
  List.iter (fun provider ->
    let name = provider_name provider in
    if Hashtbl.mem seen name then fail "PAVE_WEB_SEARCH_PROVIDER_PRIORITY contains a duplicate provider";
    Hashtbl.add seen name ()) providers;
  providers

let credential_name = function
  | Brave -> "BRAVE_SEARCH_API_KEY"
  | Tavily -> "TAVILY_API_KEY"

let valid_credential name value =
  if String.length value = 0 then None
  else if String.length value > 8_192 || contains_control value || String.exists (fun c -> c = ' ' || c = '\t') value then
    fail ("invalid credential value in " ^ name)
  else Some value

let choose_provider ~env priority =
  let rec choose missing = function
    | [] ->
        let variables = List.rev missing |> List.sort_uniq String.compare |> String.concat ", " in
        fail ("no configured search provider has a credential; set " ^ variables)
    | provider :: rest ->
        let name = credential_name provider in
        match Option.bind (env name) (valid_credential name) with
        | Some key -> provider, key
        | None -> choose (name :: missing) rest
  in
  choose [] priority

let field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None
let required name json = match field name json with
  | Some value -> value
  | None -> fail ("malformed search response: missing " ^ name)
let string_field name json = match required name json with
  | `String value -> value
  | _ -> fail ("malformed search response: " ^ name ^ " must be a string")

let rec reject_duplicate_keys = function
  | `Assoc fields ->
      let keys = Hashtbl.create (List.length fields) in
      List.iter (fun (key, value) ->
        if Hashtbl.mem keys key then fail "malformed search response: duplicate JSON object key";
        Hashtbl.add keys key ();
        reject_duplicate_keys value) fields
  | `List values -> List.iter reject_duplicate_keys values
  | _ -> ()

let parse_json body =
  let json = try Yojson.Basic.from_string body
    with Yojson.Json_error _ -> fail "malformed search response JSON" in
  reject_duplicate_keys json;
  json

let normalized_url_key url =
  let host, host_lower, port, authority_end =
    try Workspace_reader.parse_url url
    with Workspace_reader.Error _ -> fail "malformed search response: result URL must be a public HTTPS URL on port 443" in
  if port <> 443 || host_lower = "localhost" ||
     (String.contains host_lower ':' && Workspace_reader.unsafe_ip host_lower) ||
     (not (String.contains host_lower ':') &&
      (try ignore (Unix.inet_addr_of_string host_lower); Workspace_reader.unsafe_ip host_lower
       with Failure _ -> false)) then
    fail "malformed search response: result URL uses a private, local, or non-HTTPS host";
  let suffix = String.sub url authority_end (String.length url - authority_end) in
  let suffix = match String.index_opt suffix '#' with
    | None -> suffix
    | Some index -> String.sub suffix 0 index in
  let host_text = if String.contains host ':' then "[" ^ host_lower ^ "]" else host_lower in
  "https://" ^ host_text ^ suffix

let validate_result_urls urls =
  let seen = Hashtbl.create (List.length urls) in
  List.iter (fun url ->
    let key = normalized_url_key url in
    if Hashtbl.mem seen key then fail "malformed search response: duplicate result URL";
    Hashtbl.add seen key ()) urls

let parse_results provider count json =
  let rows = match provider, json with
    | Brave, `Assoc _ ->
        (match required "web" json with
         | `Assoc _ -> (match required "results" (required "web" json) with
             | `List values -> values
             | _ -> fail "malformed search response: web.results must be an array")
         | _ -> fail "malformed search response: web must be an object")
    | Tavily, `Assoc _ ->
        (match required "results" json with
         | `List values -> values
         | _ -> fail "malformed search response: results must be an array")
    | _ -> fail "malformed search response: expected a JSON object" in
  if List.length rows > count then fail "malformed search response: result count exceeds requested limit";
  let values = List.map (fun row ->
    let title = string_field "title" row in
    let url = string_field "url" row in
    let snippet = string_field (if provider = Brave then "description" else "content") row in
    if title = "" || String.length title > 1_024 || contains_control title then
      fail "malformed search response: invalid result title";
    if String.length url = 0 || String.length url > 4_096 then
      fail "malformed search response: invalid result URL size";
    if String.length snippet > 8_192 || contains_control snippet then
      fail "malformed search response: invalid result snippet";
    title, url, snippet) rows in
  validate_result_urls (List.map (fun (_, url, _) -> url) values);
  List.mapi (fun index (title, url, snippet) -> {
    title; url; snippet; provider = provider_name provider;
    citation = Printf.sprintf "[%d] %s" (index + 1) url;
  }) values

let check_cancel cancel = match cancel with
  | Some is_cancelled when is_cancelled () -> fail "search or fetch cancelled"
  | _ -> ()

let validate_request request =
  if request.response_limit < 1 || request.response_limit > max_response_bytes then
    fail "HTTP response limit is invalid";
  if request.method_ <> "GET" && request.method_ <> "POST" then
    fail "HTTP method is not allowed";
  if String.length request.body > 16_384 then fail "HTTP request body exceeds the size limit"

let is_brave_request request =
  request.method_ = "GET" && starts_with request.url (brave_endpoint ^ "?") &&
  List.exists (fun (name, _) -> name = "X-Subscription-Token") request.headers
let is_tavily_request request =
  request.method_ = "POST" && request.url = tavily_endpoint &&
  List.exists (fun (name, _) -> name = "Authorization") request.headers
let header_allowed_for_search request name =
  (is_brave_request request && (name = "Accept" || name = "X-Subscription-Token")) ||
  (is_tavily_request request && (name = "Accept" || name = "Content-Type" || name = "Authorization"))

let safe_addresses ?cancel host =
  let addresses = try Workspace_reader.resolve_host ?cancel host 443 with
    | Workspace_reader.Error "workspace read cancelled" -> fail "search or fetch cancelled"
    | Workspace_reader.Error _ -> fail "HTTPS host did not resolve to safe public addresses" in
  let addresses = List.sort_uniq String.compare addresses in
  if addresses = [] || List.exists Workspace_reader.unsafe_ip addresses then
    fail "HTTPS host resolves to private, local, or non-public IP addresses";
  addresses

let curl_request ?cancel request =
  validate_request request;
  let is_search = is_brave_request request || is_tavily_request request in
  if not is_search && (request.method_ <> "GET" || request.body <> "" ||
      List.exists (fun (name, _) ->
        (name <> "Accept" && name <> "User-Agent") ||
        name = "Authorization" || name = "X-Subscription-Token") request.headers) then
    fail "URL fetch requests must be unauthenticated GET requests with safe headers";
  if is_search && List.exists (fun (name, _) -> not (header_allowed_for_search request name)) request.headers then
    fail "search request contains an unexpected header";
  let host, host_lower, port, _ =
    try Workspace_reader.parse_url request.url
    with Workspace_reader.Error _ -> fail "HTTPS request URL is invalid" in
  if port <> 443 then fail "HTTPS requests must use port 443";
  if is_search then (
    let expected = if is_brave_request request then "api.search.brave.com" else "api.tavily.com" in
    if host_lower <> expected then fail "search credential is not bound to its fixed provider host");
  let addresses = safe_addresses ?cancel host in
  let address = List.hd addresses in
  let host_key = if String.contains host ':' then "[" ^ host ^ "]" else host in
  let pin_address = if String.contains address ':' then "[" ^ address ^ "]" else address in
  let resolve = host_key ^ ":443:" ^ pin_address in
  let with_body path =
    "silent\nshow-error\n" ^ option "url" request.url ^
    option "request" request.method_ ^ option "write-out" "%{http_code}" ^
    option "connect-timeout" "5" ^ option "max-time" (string_of_int timeout_seconds) ^
    option "max-filesize" (string_of_int request.response_limit) ^
    option "proto" "=https" ^ option "proto-redir" "=https" ^
    option "max-redirs" "0" ^ option "proxy" "" ^ option "noproxy" "*" ^
    option "resolve" resolve ^
    String.concat "" (List.map (fun (name, value) -> option "header" (name ^ ": " ^ value)) request.headers) ^
    (match path with None -> "" | Some path -> option "data-binary" ("@" ^ path))
  in
  let received = Buffer.create (min request.response_limit 4_096) in
  let collect chunk =
    if Buffer.length received + String.length chunk > request.response_limit + 3 then
      fail "HTTP response exceeds the size limit";
    Buffer.add_string received chunk in
  let run path =
    try ignore (Provider.run_curl ?cancel ~on_chunk:collect (with_body path))
    with
    | Provider.Cancelled -> fail "search or fetch cancelled"
    | Provider.Provider_error _ -> fail "HTTPS request failed or timed out"
  in
  (match if request.body = "" then None else Some request.body with
   | None -> run None
   | Some body ->
       Provider.with_temp_file (fun path channel ->
         output_string channel body;
         flush channel;
         run (Some path)));
  check_cancel cancel;
  let combined = Buffer.contents received in
  let length = String.length combined in
  if length < 3 then fail "HTTPS request returned no HTTP status";
  let status = try int_of_string (String.sub combined (length - 3) 3)
    with Failure _ -> fail "HTTPS request returned an invalid HTTP status" in
  let body_length = length - 3 in
  if body_length > request.response_limit then fail "HTTP response exceeds the size limit";
  status, String.sub combined 0 body_length

let default_http ?cancel request =
  try Ok (curl_request ?cancel request) with
  | Error message -> Error message
  | Unix.Unix_error _ | Sys_error _ | Invalid_argument _ -> Error "HTTPS request failed"

let invoke ?http ?cancel request =
  check_cancel cancel;
  let send = match http with
    | Some send -> send
    | None -> default_http ?cancel in
  let status, body = match send request with
    | Ok response -> response
    | Error _ -> fail "HTTPS request failed or timed out" in
  check_cancel cancel;
  if status <> 200 then fail (Printf.sprintf "search/fetch provider returned HTTP %d" status);
  if String.length body > request.response_limit then fail "HTTP response exceeds the size limit";
  body

let search ?http ?cancel ?(env = Sys.getenv_opt) ?(page = 0) ?(count = 5) ~query () =
  let query = valid_query query in
  validate_limits ~page ~count;
  let priority = match env "PAVE_WEB_SEARCH_PROVIDER_PRIORITY" with
    | None -> fail "set PAVE_WEB_SEARCH_PROVIDER_PRIORITY to an explicit provider order (brave,tavily)"
    | Some value -> parse_priority value in
  let provider, key = choose_provider ~env priority in
  if provider = Tavily && page <> 0 then fail "Tavily Search does not support paged requests";
  let request = match provider with
    | Brave -> {
        method_ = "GET";
        url = brave_endpoint ^ "?q=" ^ percent_encode query ^ "&count=" ^ string_of_int count ^
              "&offset=" ^ string_of_int page ^ "&result_filter=web";
        headers = ["Accept", "application/json"; "X-Subscription-Token", key];
        body = ""; response_limit = max_response_bytes;
      }
    | Tavily -> {
        method_ = "POST"; url = tavily_endpoint;
        headers = ["Accept", "application/json"; "Content-Type", "application/json";
                   "Authorization", "Bearer " ^ key];
        body = Yojson.Basic.to_string (`Assoc [
          "query", `String query; "search_depth", `String "basic";
          "max_results", `Int count; "include_answer", `Bool false;
          "include_raw_content", `Bool false]);
        response_limit = max_response_bytes;
      } in
  let body = invoke ?http ?cancel request in
  let results = parse_results provider count (parse_json body) in
  let citations = List.map (fun result -> result.citation) results in
  { provider = provider_name provider; query; page; results; citations }

let utf8_of_codepoint code =
  if code <= 0 || code > 0x10ffff || (code >= 0xd800 && code <= 0xdfff) then "\xef\xbf\xbd"
  else if code < 0x80 then String.make 1 (Char.chr code)
  else if code < 0x800 then String.init 2 (function
      | 0 -> Char.chr (0xc0 lor (code lsr 6))
      | _ -> Char.chr (0x80 lor (code land 0x3f)))
  else if code < 0x10000 then String.init 3 (function
      | 0 -> Char.chr (0xe0 lor (code lsr 12))
      | 1 -> Char.chr (0x80 lor ((code lsr 6) land 0x3f))
      | _ -> Char.chr (0x80 lor (code land 0x3f)))
  else String.init 4 (function
      | 0 -> Char.chr (0xf0 lor (code lsr 18))
      | 1 -> Char.chr (0x80 lor ((code lsr 12) land 0x3f))
      | 2 -> Char.chr (0x80 lor ((code lsr 6) land 0x3f))
      | _ -> Char.chr (0x80 lor (code land 0x3f)))

let decode_entity entity =
  match String.lowercase_ascii entity with
  | "amp" -> Some "&" | "lt" -> Some "<" | "gt" -> Some ">"
  | "quot" -> Some "\"" | "apos" -> Some "'" | "nbsp" -> Some " "
  | value when String.length value > 1 && value.[0] = '#' ->
      let base, digits = if String.length value > 2 && (value.[1] = 'x' || value.[1] = 'X')
        then 16, String.sub value 2 (String.length value - 2)
        else 10, String.sub value 1 (String.length value - 1) in
      (try Some (utf8_of_codepoint (int_of_string ((if base = 16 then "0x" else "") ^ digits)))
       with Failure _ -> None)
  | _ -> None

let append_checked output max_bytes value =
  if Buffer.length output + String.length value > max_bytes then
    fail (Printf.sprintf "converted page exceeds %d-byte content limit" max_bytes);
  Buffer.add_string output value

let append_decoded ?(escape_markdown = false) output max_bytes html start finish =
  let append value =
    if escape_markdown then
      String.iter (function
        | ('\\' | '[' | ']') as c ->
            append_checked output max_bytes (String.make 1 '\\');
            append_checked output max_bytes (String.make 1 c)
        | '\n' | '\r' when escape_markdown ->
            append_checked output max_bytes " "
        | c -> append_checked output max_bytes (String.make 1 c)) value
    else append_checked output max_bytes value in
  let rec loop index =
    if index >= finish then ()
    else if html.[index] = '&' then
      (match String.index_from_opt html (index + 1) ';' with
       | Some ending when ending < finish && ending - index <= 16 ->
           let entity = String.sub html (index + 1) (ending - index - 1) in
           (match decode_entity entity with
            | Some decoded -> append decoded; loop (ending + 1)
            | None -> append "&"; loop (index + 1))
       | _ -> append "&"; loop (index + 1))
    else (append (String.make 1 html.[index]); loop (index + 1))
  in loop start

let find_case_insensitive text needle start =
  let n = String.length text and m = String.length needle in
  let rec seek index =
    if index + m > n then None
    else if String.lowercase_ascii (String.sub text index m) = needle then Some index
    else seek (index + 1) in
  seek start

let find_tag_end html start =
  let length = String.length html in
  let rec seek index quote =
    if index >= length then None
    else
      let c = html.[index] in
      match quote, c with
      | Some delimiter, c when c = delimiter -> seek (index + 1) None
      | Some _, _ -> seek (index + 1) quote
      | None, ('\'' | '"') -> seek (index + 1) (Some c)
      | None, '>' -> Some index
      | _ -> seek (index + 1) None
  in seek start None

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let parse_tag content =
  let length = String.length content in
  let i = ref 0 in
  while !i < length && is_space content.[!i] do incr i done;
  let closing = !i < length && content.[!i] = '/' in
  if closing then incr i;
  let start = !i in
  while !i < length && ((content.[!i] >= 'a' && content.[!i] <= 'z') ||
    (content.[!i] >= 'A' && content.[!i] <= 'Z') ||
    (content.[!i] >= '0' && content.[!i] <= '9')) do incr i done;
  if !i = start then None
  else Some (closing, lowercase (String.sub content start (!i - start)),
             String.sub content !i (length - !i))

let decode_html_attribute value =
  let output = Buffer.create (String.length value) in
  append_decoded output max_content_bytes value 0 (String.length value);
  Buffer.contents output

let parse_href attributes =
  let length = String.length attributes in
  let skip_space index =
    let i = ref index in
    while !i < length && is_space attributes.[!i] do incr i done;
    !i in
  let read_value index =
    let index = skip_space index in
    if index >= length then None, index
    else if attributes.[index] = '=' then (
      let index = skip_space (index + 1) in
      if index >= length then None, index
      else if attributes.[index] = '\'' || attributes.[index] = '"' then
        let quote = attributes.[index] and start = index + 1 in
        let rec find_end at =
          if at >= length then None else
          if attributes.[at] = quote then Some at else find_end (at + 1) in
        (match find_end start with
         | None -> None, length
         | Some ending -> Some (String.sub attributes start (ending - start)), ending + 1)
      else
        let start = index in
        let rec find_end at =
          if at >= length || is_space attributes.[at] then at else find_end (at + 1) in
        let ending = find_end start in
        Some (String.sub attributes start (ending - start)), ending)
    else None, index in
  let href = ref None and seen_href = ref false and duplicate_href = ref false in
  let rec scan index =
    let index = skip_space index in
    if index >= length then ()
    else
      let start = index in
      let rec end_name at =
        if at >= length || is_space attributes.[at] || attributes.[at] = '=' ||
           attributes.[at] = '/' then at
        else end_name (at + 1) in
      let ending = end_name start in
      if ending = start then scan (index + 1)
      else
        let name = lowercase (String.sub attributes start (ending - start)) in
        let value, next = read_value ending in
        if name = "href" then
          if !seen_href then duplicate_href := true
          else (
            seen_href := true;
            href := Option.map decode_html_attribute value);
        scan (max (index + 1) next)
  in
  scan 0;
  if !duplicate_href then None else !href

let markdown_destination url =
  let output = Buffer.create (String.length url) in
  String.iter (function
    | '<' -> Buffer.add_string output "%3C"
    | '>' -> Buffer.add_string output "%3E"
    | '\\' -> Buffer.add_string output "%5C"
    | c -> Buffer.add_char output c) url;
  Buffer.contents output

let safe_anchor_href href =
  if String.length href = 0 || String.length href > 4_096 then None
  else
    try
      let _host, host_lower, port, _ = Workspace_reader.parse_url href in
      let literal_private =
        (String.contains host_lower ':' && Workspace_reader.unsafe_ip host_lower) ||
        (try ignore (Unix.inet_addr_of_string host_lower); Workspace_reader.unsafe_ip host_lower
         with Failure _ -> false) in
      if port <> 443 || literal_private then None
      else Some (markdown_destination href)
    with Workspace_reader.Error _ -> None

let convert_html_to_markdown ?(max_bytes = max_content_bytes) html =
  if max_bytes < 1 || max_bytes > max_content_bytes then fail "page content limit must be between 1 and 65536 bytes";
  if String.length html > max_bytes then fail "HTML page exceeds the content limit";
  let output = Buffer.create (min (String.length html) max_bytes) in
  let length = String.length html in
  let table_active = ref false in
  let table_rows = ref [] in
  let current_row = ref [] in
  let current_cell = ref None in
  let anchors = ref [] in
  let append value =
    match !current_cell, !table_active with
    | Some cell, _ -> append_checked cell max_bytes value
    | None, true -> ()
    | None, false -> append_checked output max_bytes value in
  let newline () =
    match !current_cell, !table_active with
    | Some _, _ -> append " "
    | None, true -> ()
    | None, false ->
        if Buffer.length output > 0 && Buffer.nth output (Buffer.length output - 1) <> '\n' then
          append_checked output max_bytes "\n" in
  let inside_link () = List.exists Option.is_some !anchors in
  let append_text start finish =
    if not !table_active || !current_cell <> None then (
      let target = match !current_cell with Some cell -> cell | None -> output in
      append_decoded ~escape_markdown:(inside_link ()) target max_bytes html start finish) in
  let finish_cell () =
    match !current_cell with
    | None -> ()
    | Some cell ->
        current_row := String.trim (Buffer.contents cell) :: !current_row;
        current_cell := None in
  let finish_row () =
    finish_cell ();
    if !table_active && !current_row <> [] then (
      table_rows := List.rev !current_row :: !table_rows;
      current_row := []) in
  let render_table () =
    let rows = List.rev !table_rows in
    let columns = List.fold_left (fun count row -> max count (List.length row)) 0 rows in
    let render_cell value =
      let output = Buffer.create (String.length value) in
      String.iter (function
        | '|' -> Buffer.add_string output "\\|"
        | '\n' | '\r' -> Buffer.add_char output ' '
        | c -> Buffer.add_char output c) value;
      Buffer.contents output in
    let render_row row =
      append_checked output max_bytes "|";
      let rec render_cells column = function
        | _ when column >= columns -> ()
        | value :: remaining ->
            append_checked output max_bytes (" " ^ render_cell value ^ " |");
            render_cells (column + 1) remaining
        | [] ->
            append_checked output max_bytes "  |";
            render_cells (column + 1) [] in
      render_cells 0 row;
      append_checked output max_bytes "\n" in
    match rows with
    | [] -> ()
    | header :: body ->
        render_row header;
        append_checked output max_bytes "|";
        for _ = 1 to columns do append_checked output max_bytes " --- |" done;
        append_checked output max_bytes "\n";
        List.iter render_row body in
  let open_cell () =
    finish_cell ();
    current_cell := Some (Buffer.create 32) in
  let pop_anchor () =
    match !anchors with
    | [] -> ()
    | anchor :: rest ->
        anchors := rest;
        (match anchor with
         | None -> ()
         | Some destination -> append ("](<" ^ destination ^ ">)")) in
  let close_open_anchors () =
    List.iter (function None -> () | Some destination -> append ("](<" ^ destination ^ ">)")) !anchors in
  let rec scan index ignored =
    if index >= length then ()
    else match ignored with
    | Some tag ->
        (match find_case_insensitive html ("</" ^ tag) index with
         | None -> ()
         | Some closing ->
             (match find_tag_end html (closing + 2 + String.length tag) with
              | None -> ()
              | Some ending -> scan (ending + 1) None))
    | None ->
        if html.[index] <> '<' then (
          let next = match String.index_from_opt html index '<' with Some at -> at | None -> length in
          append_text index next;
          scan next None)
        else if index + 3 < length && String.sub html index 4 = "<!--" then
          (match find_case_insensitive html "-->" (index + 4) with
           | None -> ()
           | Some ending -> scan (ending + 3) None)
        else
          (match find_tag_end html (index + 1) with
           | None -> append "<"; scan (index + 1) None
           | Some ending ->
               let content = String.sub html (index + 1) (ending - index - 1) in
               (match parse_tag content with
                | None -> scan (ending + 1) None
                | Some (closing, tag, attributes) ->
                    if not closing && (tag = "script" || tag = "style") then
                      scan (ending + 1) (Some tag)
                    else (
                      (match tag, closing, !table_active with
                       | "table", false, false ->
                           newline ();
                           table_active := true;
                           table_rows := [];
                           current_row := []
                       | "table", true, true ->
                           finish_row ();
                           render_table ();
                           table_active := false;
                           newline ()
                       | "tr", false, true -> finish_row ()
                       | "tr", true, true -> finish_row ()
                       | ("td" | "th"), false, true -> open_cell ()
                       | ("td" | "th"), true, true -> finish_cell ()
                       | "a", false, _ ->
                           let destination = Option.bind (parse_href attributes) safe_anchor_href in
                           anchors := destination :: !anchors;
                           (match destination with Some _ -> append "[" | None -> ())
                       | "a", true, _ -> pop_anchor ()
                       | (_, _, true) -> ()
                       | ("p" | "div" | "section" | "article" | "header" | "footer" | "main" |
                          "blockquote" | "ul" | "ol"), _, false -> newline ()
                       | ("h1" | "h2" | "h3" | "h4" | "h5" | "h6"), false, false ->
                           newline ();
                           append (String.make (1 + Char.code tag.[1] - Char.code '1') '#');
                           append " "
                       | ("h1" | "h2" | "h3" | "h4" | "h5" | "h6"), true, false -> newline ()
                       | "br", _, false | "hr", _, false -> newline ()
                       | "li", false, false -> newline (); append "- "
                       | "li", true, false -> newline ()
                       | "strong", _, false | "b", _, false -> append "**"
                       | "em", _, false | "i", _, false -> append "*"
                       | "code", _, false -> append "`"
                       | _ -> ());
                      scan (ending + 1) None)))
  in
  scan 0 None;
  close_open_anchors ();
  if !table_active then (
    finish_row ();
    render_table ());
  String.trim (Buffer.contents output)

let fetch_url ?http ?cancel ?(max_bytes = max_content_bytes) url () =
  if max_bytes < 1 || max_bytes > max_content_bytes then fail "page content limit must be between 1 and 65536 bytes";
  let host, host_lower, port, _ =
    try Workspace_reader.parse_url url
    with Workspace_reader.Error _ -> fail "fetch URL must be credential-free public HTTPS on port 443" in
  if (String.contains host_lower ':' && Workspace_reader.unsafe_ip host_lower) ||
     (try ignore (Unix.inet_addr_of_string host_lower); Workspace_reader.unsafe_ip host_lower
      with Failure _ -> false) then
    fail "fetch URL host is a private or local IP address";
  if port <> 443 then fail "fetch URL must use HTTPS port 443";
  check_cancel cancel;
  ignore (safe_addresses ?cancel host);
  let request = { method_ = "GET"; url; headers = ["Accept", "text/html,application/xhtml+xml";
      "User-Agent", "pave-web-fetch/1.0"]; body = ""; response_limit = max_bytes } in
  let html = invoke ?http ?cancel request in
  let markdown = convert_html_to_markdown ~max_bytes html in
  { source_url = url; markdown }
