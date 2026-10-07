(* Credentialed search engines in automatic priority order, followed by the
   credential-free HTML fallback and, when a local Chromium-family browser is
   available, the browser-rendered Ecosia fallback. Explicit configuration
   preserves its order. *)
type provider = Exa | Firecrawl | Brave | Tavily | Kagi | Jina | Duckduckgo | Ecosia

let all_providers = [Exa; Firecrawl; Brave; Tavily; Kagi; Jina; Duckduckgo; Ecosia]

type request = {
  search : provider option;  (* Some engine: a pinned search call; None: URL fetch *)
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
  failed : (string * string) list;  (* earlier engines that failed, with why *)
}
type fetched_page = { source_url : string; markdown : string; truncated : bool }

exception Error of string
exception Download_limit
let fail message = raise (Error message)
let brave_endpoint = "https://api.search.brave.com/res/v1/web/search"
let tavily_endpoint = "https://api.tavily.com/search"
let exa_endpoint = "https://api.exa.ai/search"
let jina_endpoint = "https://s.jina.ai/"
let kagi_endpoint = "https://kagi.com/api/v1/search"
let firecrawl_endpoint = "https://api.firecrawl.dev/v2/search"
let duckduckgo_endpoint = "https://html.duckduckgo.com/html/"
let ecosia_endpoint = "https://www.ecosia.org/search"
let browser_variable = "PAVE_BROWSER"
let priority_variable = "PAVE_WEB_SEARCH_PROVIDER_PRIORITY"
let max_query_bytes = 600
let max_results = 20
let max_page = 9
let max_response_bytes = 256 * 1_024
let max_fetch_response_bytes = 1024 * 1024
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

let provider_name = function
  | Exa -> "exa" | Firecrawl -> "firecrawl" | Brave -> "brave"
  | Tavily -> "tavily" | Kagi -> "kagi" | Jina -> "jina"
  | Duckduckgo -> "duckduckgo" | Ecosia -> "ecosia"

let provider_label = function
  | Exa -> "Exa" | Firecrawl -> "Firecrawl" | Brave -> "Brave Search"
  | Tavily -> "Tavily" | Kagi -> "Kagi" | Jina -> "Jina"
  | Duckduckgo -> "DuckDuckGo" | Ecosia -> "Ecosia"

let provider_of_name name =
  match List.find_opt (fun provider -> provider_name provider = name) all_providers with
  | Some provider -> provider
  | None ->
      fail ("unsupported search provider in " ^ priority_variable ^ ": " ^ name ^
        " (supported: " ^ String.concat ", " (List.map provider_name all_providers) ^ ")")

let parse_priority value =
  let names = String.split_on_char ',' value
    |> List.map (fun name -> String.lowercase_ascii (String.trim name)) in
  if names = [] || List.exists (( = ) "") names then
    fail (priority_variable ^ " must be a comma-separated list of search providers");
  let providers = List.map provider_of_name names in
  let seen = Hashtbl.create 4 in
  List.iter (fun provider ->
    let name = provider_name provider in
    if Hashtbl.mem seen name then fail (priority_variable ^ " contains a duplicate provider");
    Hashtbl.add seen name ()) providers;
  providers

(* DuckDuckGo needs no credential; its HTML endpoint receives only the query. *)
let credential_name = function
  | Brave -> Some "BRAVE_SEARCH_API_KEY"
  | Tavily -> Some "TAVILY_API_KEY"
  | Exa -> Some "EXA_API_KEY"
  | Jina -> Some "JINA_API_KEY"
  | Kagi -> Some "KAGI_API_KEY"
  | Firecrawl -> Some "FIRECRAWL_API_KEY"
  | Duckduckgo | Ecosia -> None

(* Ecosia answers plain HTTP clients with a challenge; it is fetched by a
   local headless browser instead of curl. *)
let requires_browser = function Ecosia -> true | _ -> false

let endpoint = function
  | Brave -> brave_endpoint | Tavily -> tavily_endpoint | Exa -> exa_endpoint
  | Jina -> jina_endpoint | Kagi -> kagi_endpoint | Firecrawl -> firecrawl_endpoint
  | Duckduckgo -> duckduckgo_endpoint | Ecosia -> ecosia_endpoint

let endpoint_host = function
  | Brave -> "api.search.brave.com" | Tavily -> "api.tavily.com"
  | Exa -> "api.exa.ai" | Jina -> "s.jina.ai" | Kagi -> "kagi.com"
  | Firecrawl -> "api.firecrawl.dev" | Duckduckgo -> "html.duckduckgo.com"
  | Ecosia -> "www.ecosia.org"

let endpoint_method = function
  | Brave | Jina | Ecosia -> "GET"
  | Tavily | Exa | Kagi | Firecrawl | Duckduckgo -> "POST"

let credential_header = function
  | Brave -> Some "X-Subscription-Token"
  | Exa -> Some "x-api-key"
  | Tavily | Jina | Kagi | Firecrawl -> Some "Authorization"
  | Duckduckgo | Ecosia -> None

(* Only Brave exposes result paging through its API. *)
let supports_paging = function Brave -> true | _ -> false

let allowed_headers provider =
  let credential = Option.to_list (credential_header provider) in
  match provider with
  | Brave -> "Accept" :: credential
  | Jina -> ["Accept"; "X-Respond-With"; "X-Retain-Images"] @ credential
  | Duckduckgo -> ["Accept"; "Accept-Language"; "Content-Type"; "Referer"; "User-Agent"]
  | Tavily | Exa | Kagi | Firecrawl -> ["Accept"; "Content-Type"] @ credential
  | Ecosia -> []

let valid_credential name value =
  if String.length value = 0 then None
  else if String.length value > 8_192 || contains_control value || String.exists (fun c -> c = ' ' || c = '\t') value then
    fail ("invalid credential value in " ^ name)
  else Some value

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

let array_field label = function
  | `List values -> values
  | _ -> fail ("malformed search response: " ^ label ^ " must be an array")

let optional_string name row = match field name row with
  | None | Some `Null -> None
  | Some (`String value) -> Some value
  | Some _ -> fail ("malformed search response: " ^ name ^ " must be a string")

let first_string names row =
  List.fold_left (fun found name -> match found with
    | Some value when String.trim value <> "" -> found
    | _ -> optional_string name row) None names
  |> Option.value ~default:""

(* Collapse whitespace/control runs and keep at most [limit] bytes on a UTF-8
   boundary; used for engines whose snippets are free text. *)
let clean_text ?(limit = 1_000) text =
  let output = Buffer.create (min limit (String.length text)) in
  let pending_space = ref false in
  String.iter (fun c ->
    if Char.code c < 32 || Char.code c = 127 || c = ' ' then pending_space := true
    else (
      if !pending_space && Buffer.length output > 0 then Buffer.add_char output ' ';
      pending_space := false;
      Buffer.add_char output c)) text;
  let text = Buffer.contents output in
  if String.length text <= limit then text
  else
    let size = ref (max 0 (limit - String.length "…")) in
    while !size > 0 && Char.code text.[!size] land 0xc0 = 0x80 do decr size done;
    String.sub text 0 !size ^ (if limit >= String.length "…" then "…" else "")

(* Each JSON engine's result rows, as (title, url, snippet). *)
let result_rows provider json =
  let object_required label = match json with
    | `Assoc _ -> ()
    | _ -> fail ("malformed search response: expected a JSON object" ^ label) in
  (* Error envelopes never become successful partial answers, regardless of
     whether the provider also included otherwise well-formed result rows. *)
  (match field "error" json with
   | None | Some `Null | Some (`List []) -> ()
   | Some _ -> fail (provider_label provider ^ " reported a search failure"));
  match provider with
  | Brave ->
      object_required "";
      (match required "web" json with
       | `Assoc _ -> array_field "web.results" (required "results" (required "web" json))
       | _ -> fail "malformed search response: web must be an object")
      |> List.map (fun row ->
        string_field "title" row, string_field "url" row, string_field "description" row)
  | Tavily ->
      object_required "";
      array_field "results" (required "results" json)
      |> List.map (fun row ->
        string_field "title" row, string_field "url" row, string_field "content" row)
  | Exa ->
      object_required "";
      array_field "results" (required "results" json)
      |> List.map (fun row ->
        let url = string_field "url" row in
        let highlight = match field "highlights" row with
          | Some (`List (`String text :: _)) -> Some text
          | _ -> None in
        let title = match optional_string "title" row with
          | Some title when String.trim title <> "" -> title
          | _ -> url in
        clean_text ~limit:1_024 title, url, clean_text (match highlight with
          | Some text -> text
          | None -> first_string ["summary"; "text"] row))
  | Jina ->
      let rows = match json with
        | `List rows -> rows
        | `Assoc _ ->
            (match field "code" json with
             | Some (`Int code) when code <> 200 ->
                 fail (Printf.sprintf "Jina reported failure code %d" code)
             | _ -> array_field "data" (required "data" json))
        | _ -> fail "malformed search response: expected a JSON object or array" in
      List.map (fun row ->
        let url = string_field "url" row in
        (match optional_string "title" row with
         | Some title when String.trim title <> "" -> title
         | _ -> url) |> clean_text ~limit:1_024, url,
        clean_text (first_string ["description"; "content"] row)) rows
  | Kagi ->
      object_required "";
      (match required "data" json with
       | `Assoc _ as data ->
           (match field "search" data with
            | None | Some `Null -> []
            | Some rows -> array_field "data.search" rows)
       | _ -> fail "malformed search response: data must be an object")
      |> List.map (fun row ->
        clean_text ~limit:1_024 (string_field "title" row), string_field "url" row,
        clean_text (first_string ["snippet"] row))
  | Firecrawl ->
      object_required "";
      (match field "success" json with
       | Some (`Bool false) ->
           fail "Firecrawl reported a search failure"
       | _ -> ());
      (match required "data" json with
       | `List rows -> rows
       | `Assoc _ as data -> (match field "web" data with
           | None | Some `Null -> []
           | Some rows -> array_field "data.web" rows)
       | _ -> fail "malformed search response: data must be an object or array")
      |> List.map (fun row ->
        let url = string_field "url" row in
        (match optional_string "title" row with
         | Some title when String.trim title <> "" -> title
         | _ -> url) |> clean_text ~limit:1_024, url,
        clean_text (first_string ["description"; "snippet"] row))
  | Duckduckgo | Ecosia -> fail (provider_label provider ^ " returns HTML, not JSON")

(* One unusable row (plain HTTP, private host, duplicate, oversized text)
   is dropped rather than failing the whole search, and engines that ignore
   the requested count are cut to it. The response's structure stays strict. *)
let validate_rows count rows =
  let seen = Hashtbl.create (List.length rows) in
  let usable (title, url, snippet) =
    title <> "" && String.length title <= 1_024 && not (contains_control title) &&
    String.length url > 0 && String.length url <= 4_096 &&
    String.length snippet <= 8_192 && not (contains_control snippet) &&
    match normalized_url_key url with
    | key when Hashtbl.mem seen key -> false
    | key -> Hashtbl.add seen key (); true
    | exception Error _ -> false in
  List.filter usable rows |> List.filteri (fun index _ -> index < count)

let numbered provider rows =
  List.mapi (fun index (title, url, snippet) -> {
    title; url; snippet; provider = provider_name provider;
    citation = Printf.sprintf "[%d] %s" (index + 1) url;
  }) rows

let parse_results provider count json =
  numbered provider (validate_rows count (result_rows provider json))

let check_cancel cancel = match cancel with
  | Some is_cancelled when is_cancelled () -> fail "search or fetch cancelled"
  | _ -> ()

let validate_request request =
  let limit = if request.search = None then max_fetch_response_bytes
    else max_response_bytes in
  if request.response_limit < 1 || request.response_limit > limit then
    fail "HTTP response limit is invalid";
  if request.method_ <> "GET" && request.method_ <> "POST" then
    fail "HTTP method is not allowed";
  if String.length request.body > 16_384 then fail "HTTP request body exceeds the size limit"

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
  let is_search = request.search <> None in
  if not is_search && (request.method_ <> "GET" || request.body <> "" ||
      List.exists (fun (name, _) ->
        (name <> "Accept" && name <> "User-Agent") ||
        name = "Authorization" || name = "X-Subscription-Token") request.headers) then
    fail "URL fetch requests must be unauthenticated GET requests with safe headers";
  Option.iter (fun provider ->
    if request.method_ <> endpoint_method provider ||
       not (starts_with request.url (endpoint provider)) then
      fail "search request does not match its fixed provider endpoint";
    if List.exists (fun (name, _) -> not (List.mem name (allowed_headers provider)))
        request.headers then
      fail "search request contains an unexpected header") request.search;
  let host, host_lower, port, _ =
    try Workspace_reader.parse_url request.url
    with Workspace_reader.Error _ -> fail "HTTPS request URL is invalid" in
  if port <> 443 then fail "HTTPS requests must use port 443";
  Option.iter (fun provider ->
    if host_lower <> endpoint_host provider then
      fail "search credential is not bound to its fixed provider host") request.search;
  let addresses = safe_addresses ?cancel host in
  let address = List.hd addresses in
  let host_key = if String.contains host ':' then "[" ^ host ^ "]" else host in
  let pin_address = if String.contains address ':' then "[" ^ address ^ "]" else address in
  let resolve = host_key ^ ":443:" ^ pin_address in
  let header_path = ref None and capped = ref false in
  let with_body path =
    "silent\nshow-error\n" ^ option "url" request.url ^
    option "request" request.method_ ^ option "write-out" "%{http_code}" ^
    option "connect-timeout" "5" ^ option "max-time" (string_of_int timeout_seconds) ^
    (* A page fetch keeps a bounded prefix past its limit, so curl must not
       abort the transfer; search responses stay hard-limited. *)
    (if is_search then option "max-filesize" (string_of_int request.response_limit) else "") ^
    (match !header_path with None -> "" | Some path -> option "dump-header" path) ^
    option "proto" "=https" ^ option "proto-redir" "=https" ^
    option "max-redirs" "0" ^ option "proxy" "" ^ option "noproxy" "*" ^
    option "resolve" resolve ^
    String.concat "" (List.map (fun (name, value) -> option "header" (name ^ ": " ^ value)) request.headers) ^
    (match path with None -> "" | Some path -> option "data-binary" ("@" ^ path))
  in
  let received = Buffer.create (min request.response_limit 4_096) in
  let collect chunk =
    if Buffer.length received + String.length chunk > request.response_limit + 3 then
      if is_search then fail "HTTP response exceeds the size limit"
      else (
        (* Keep one byte past the limit so the caller can mark truncation. *)
        let room = request.response_limit + 1 - Buffer.length received in
        if room > 0 then Buffer.add_substring received chunk 0 (min room (String.length chunk));
        raise Download_limit)
    else Buffer.add_string received chunk in
  let run path =
    try ignore (Provider.run_curl ?cancel ~on_chunk:collect (with_body path))
    with
    | Download_limit -> capped := true
    | Provider.Cancelled -> fail "search or fetch cancelled"
    | Provider.Provider_error reason ->
        let message = match reason with
          | "Transport error: curl failed (exit status 6)" -> "HTTPS host could not be resolved"
          | "Transport error: curl failed (exit status 7)" -> "HTTPS connection failed"
          | "Transport error: curl failed (exit status 28)" -> "HTTPS request timed out"
          | "Transport error: curl failed (exit status 35)" -> "HTTPS TLS handshake failed"
          | "Transport error: curl failed (exit status 60)" -> "HTTPS certificate verification failed"
          | "Transport error: curl failed (exit status 63)" ->
              Printf.sprintf "HTTP response exceeds the %d-byte download limit" request.response_limit
          | _ -> "HTTPS request failed" in
        fail message
  in
  let transfer () = match if request.body = "" then None else Some request.body with
    | None -> run None
    | Some body ->
        Provider.with_temp_file (fun path channel ->
          output_string channel body;
          flush channel;
          run (Some path)) in
  (* A capped fetch stops before curl's trailing status, so read the status
     from the response headers instead. *)
  let capped_status =
    if is_search then (transfer (); None)
    else Provider.with_temp_file (fun path channel ->
      close_out channel;
      header_path := Some path;
      transfer ();
      if !capped then Provider.status_from_headers (Provider.read_file path) else None) in
  check_cancel cancel;
  if !capped then
    match capped_status with
    | None -> fail "HTTPS request returned no HTTP status"
    | Some status -> status, Buffer.contents received
  else
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
    | Some send -> fun request -> (match send request with
        | Ok response -> Ok response
        | Error _ -> Error "HTTPS request failed")
    | None -> default_http ?cancel in
  let response = send request in
  check_cancel cancel;
  let status, body = match response with
    | Ok response -> response
    | Error message -> fail message in
  if String.length body > request.response_limit && request.search <> None then
    fail "HTTP response exceeds the size limit";
  if request.search = Some Duckduckgo && status = 202 then
    fail "DuckDuckGo blocked or deferred this search (HTTP 202, usually a bot-detection challenge); use another configured search provider or fetch a known source URL directly";
  if status <> 200 then fail (Printf.sprintf "search/fetch provider returned HTTP %d" status);
  body

(* A local Chromium-family browser renders pages that refuse plain HTTP
   clients. PAVE_BROWSER names one explicitly (or "none" disables it);
   otherwise common install locations are checked. *)
let browser_paths = [
  "/usr/bin/google-chrome"; "/usr/bin/google-chrome-stable"; "/usr/bin/chromium";
  "/usr/bin/chromium-browser"; "/snap/bin/chromium"; "/usr/bin/microsoft-edge";
  "/usr/bin/microsoft-edge-stable";
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
  "/Applications/Chromium.app/Contents/MacOS/Chromium";
  "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge" ]

let detect_browser ?(env = Sys.getenv_opt) () =
  match env browser_variable with
  | None | Some "" -> Native_services.first_executable browser_paths
  | Some value when String.lowercase_ascii (String.trim value) = "none" -> None
  | Some path when not (Filename.is_relative path) && Native_services.executable path ->
      Some path
  | Some _ ->
      fail (browser_variable ^
        " must be an absolute path to a Chromium-family browser executable, or none")

let browser_user_agent =
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0 Safari/537.36"

let browser_timeout_seconds = 30.
let max_browser_bytes = 4 * 1024 * 1024

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Array.iter (fun entry -> remove_tree (Filename.concat path entry)) (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error _ -> ()

(* Renders one fixed search URL in a sandboxed headless browser with a fresh
   throwaway profile (no cookies, history or credentials) and returns the DOM. *)
let render_page ?cancel ~program url =
  check_cancel cancel;
  let profile = Filename.temp_dir "pave-browser-" "" in
  Fun.protect ~finally:(fun () -> try remove_tree profile with _ -> ()) (fun () ->
    let result =
      try Native_services.run_native
        ~cancel:(fun () -> match cancel with Some cancelled -> cancelled () | None -> false)
        ~timeout_seconds:browser_timeout_seconds ~output_limit:max_browser_bytes ~program
        ~arguments:[
          "--headless"; "--disable-gpu"; "--no-first-run"; "--no-default-browser-check";
          "--disable-extensions"; "--disable-sync"; "--disable-background-networking";
          "--disable-component-update"; "--mute-audio"; "--hide-scrollbars"; "--lang=en-US";
          "--user-data-dir=" ^ profile; "--user-agent=" ^ browser_user_agent;
          "--virtual-time-budget=8000"; "--dump-dom"; url ]
        ~stdin:""
      with Unix.Unix_error _ -> fail "headless browser could not be started" in
    match result.termination with
    | Workspace_process.Cancelled -> fail "search or fetch cancelled"
    | Workspace_process.Timed_out ->
        fail (Printf.sprintf "headless browser did not finish within %.0f s" browser_timeout_seconds)
    | _ when result.truncated -> fail "rendered page exceeds the size limit"
    | Workspace_process.Exited 0 when String.trim result.output <> "" -> result.output
    | Workspace_process.Exited 0 -> fail "headless browser returned an empty page"
    | Workspace_process.Exited code ->
        fail (Printf.sprintf "headless browser exited with status %d" code)
    | Workspace_process.Signaled _ -> fail "headless browser was terminated")

type candidate = { engine : provider; key : string option; browser : string option }

(* An explicit priority uses exactly the listed engines in order. Otherwise
   every engine with a credential is tried in default order, ending with the
   credential-free DuckDuckGo fallback. Engines without their credential are
   skipped, never sent a request. *)
let plan ?(env = Sys.getenv_opt) ?find_browser ?(page = 0) () =
  let find_browser = match find_browser with
    | Some find -> find
    | None -> detect_browser ~env in
  let browser = lazy (find_browser ()) in
  let credential provider = Option.bind (credential_name provider) (fun name ->
    Option.bind (env name) (valid_credential name)) in
  let explicit, order = match env priority_variable with
    | Some value when String.trim value <> "" -> true, parse_priority value
    | _ -> false, all_providers in
  (* Filter incompatible engines before looking up credentials or probing a
     browser; an excluded Ecosia configuration cannot disable Brave paging. *)
  let order = if page = 0 then order else
    match List.filter supports_paging order with
    | [] ->
        let names = List.map provider_label order in
        fail (String.concat ", " names ^
          (if List.length names = 1 then " does" else " do") ^
          " not support paged requests; only Brave Search pages results")
    | paged -> paged in
  let candidates = List.filter_map (fun engine ->
    match credential_name engine, credential engine with
    | None, _ when requires_browser engine ->
        Option.map (fun path -> { engine; key = None; browser = Some path })
          (Lazy.force browser)
    | None, _ -> Some { engine; key = None; browser = None }
    | Some _, Some key -> Some { engine; key = Some key; browser = None }
    | Some _, None -> None) order in
  if candidates = [] then (
    let variables = List.filter_map credential_name order
      |> List.sort_uniq String.compare |> String.concat ", " in
    let needs_browser = List.exists requires_browser order in
    fail (match variables, needs_browser with
      | "", true -> "no Chromium-family browser was found for " ^
          String.concat ", " (List.map provider_label (List.filter requires_browser order)) ^
          "; install Chrome or Chromium, or set " ^ browser_variable
      | variables, true -> "no configured search provider is available; set " ^ variables ^
          " or install a Chromium-family browser (" ^ browser_variable ^ ")"
      | variables, false ->
          "no configured search provider has a credential; set " ^ variables));
  explicit, candidates

(* Lets callers refuse an unconfigured search before asking for approval. *)
let check_configuration ?(env = Sys.getenv_opt) ?find_browser ?page () =
  ignore (plan ~env ?find_browser ?page ())

(* Engine names in try order, each marked with whether a credential is sent. *)
let plan_summary ?(env = Sys.getenv_opt) ?find_browser ?page () =
  let explicit, candidates = plan ~env ?find_browser ?page () in
  explicit, List.map (fun candidate ->
    provider_name candidate.engine, candidate.key <> None, candidate.browser <> None)
    candidates


let build_request engine key ~query ~page ~count =
  let key = Option.value ~default:"" key in
  let json fields = Yojson.Basic.to_string (`Assoc fields) in
  let post headers body = {
    search = Some engine; method_ = "POST"; url = endpoint engine;
    headers; body; response_limit = max_response_bytes } in
  match engine with
  | Brave -> {
      search = Some Brave; method_ = "GET";
      url = brave_endpoint ^ "?q=" ^ percent_encode query ^ "&count=" ^ string_of_int count ^
            "&offset=" ^ string_of_int page ^ "&result_filter=web";
      headers = ["Accept", "application/json"; "X-Subscription-Token", key];
      body = ""; response_limit = max_response_bytes }
  | Tavily ->
      post ["Accept", "application/json"; "Content-Type", "application/json";
            "Authorization", "Bearer " ^ key]
        (json ["query", `String query; "search_depth", `String "basic";
          "max_results", `Int count; "include_answer", `Bool false;
          "include_raw_content", `Bool false])
  | Exa ->
      post ["Accept", "application/json"; "Content-Type", "application/json";
            "x-api-key", key]
        (json ["query", `String query; "numResults", `Int count; "type", `String "auto";
          "contents", `Assoc ["highlights", `Assoc [
            "numSentences", `Int 2; "highlightsPerUrl", `Int 1]]])
  | Jina -> {
      search = Some Jina; method_ = "GET";
      url = jina_endpoint ^ "?q=" ^ percent_encode query ^ "&count=" ^ string_of_int count;
      headers = ["Accept", "application/json"; "Authorization", "Bearer " ^ key;
                 "X-Respond-With", "no-content"; "X-Retain-Images", "none"];
      body = ""; response_limit = max_response_bytes }
  | Kagi ->
      post ["Accept", "application/json"; "Content-Type", "application/json";
            "Authorization", "Bearer " ^ key]
        (json ["query", `String query; "workflow", `String "search"; "limit", `Int count])
  | Firecrawl ->
      post ["Accept", "application/json"; "Content-Type", "application/json";
            "Authorization", "Bearer " ^ key]
        (json ["query", `String query; "limit", `Int count;
          "sources", `List [`Assoc ["type", `String "web"]]])
  | Duckduckgo ->
      post ["Accept", "text/html"; "Accept-Language", "en-US,en;q=0.9";
            "Content-Type", "application/x-www-form-urlencoded";
            "Referer", "https://html.duckduckgo.com/"; "User-Agent", browser_user_agent]
        ("q=" ^ percent_encode query ^ "&kl=us-en&b=")
  | Ecosia -> {
      search = Some Ecosia; method_ = "GET";
      url = ecosia_endpoint ^ "?q=" ^ percent_encode query;
      headers = []; body = ""; response_limit = max_response_bytes }

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

exception Content_limit

let utf8_prefix_length text limit =
  let rec boundary index =
    if index > 0 && index < String.length text &&
       Char.code text.[index] land 0xc0 = 0x80 then boundary (index - 1)
    else index in
  boundary (min (String.length text) (max 0 limit))

let append_checked output max_bytes value =
  let room = max_bytes - Buffer.length output in
  if String.length value > room then (
    Buffer.add_substring output value 0 (utf8_prefix_length value room);
    raise Content_limit);
  Buffer.add_string output value

let append_decoded ?(escape_markdown = false) output max_bytes html start finish =
  let append value =
    if not escape_markdown then append_checked output max_bytes value
    else
      let rec escaped start index =
        if index = String.length value then
          append_checked output max_bytes (String.sub value start (index - start))
        else match value.[index] with
          | ('\\' | '[' | ']' | '\n' | '\r') as character ->
              append_checked output max_bytes (String.sub value start (index - start));
              append_checked output max_bytes (match character with
                | '\\' -> "\\\\" | '[' -> "\\[" | ']' -> "\\]"
                | _ -> " ");
              escaped (index + 1) (index + 1)
          | _ -> escaped start (index + 1) in
      escaped 0 0 in
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
    else (
      let next = match String.index_from_opt html index '&' with
        | Some at -> min finish at | None -> finish in
      append (String.sub html index (next - index));
      loop next)
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

let parse_attribute wanted attributes =
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
  let found = ref None and seen = ref false and duplicate = ref false in
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
        if name = wanted then
          if !seen then duplicate := true
          else (
            seen := true;
            found := Option.bind value (fun value ->
              try Some (decode_html_attribute value) with Content_limit -> None));
        scan (max (index + 1) next)
  in
  scan 0;
  if !duplicate then None else !found

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
  if String.length html > max_fetch_response_bytes then fail "HTML page exceeds the download limit";
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
                           let destination = Option.bind (parse_attribute "href" attributes) safe_anchor_href in
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
  let truncated =
    try
      scan 0 None;
      close_open_anchors ();
      if !table_active then (finish_row (); render_table ());
      false
    with Content_limit ->
      (* A long table cell may fill before the outer buffer. Preserve its
         bounded prefix rather than returning an empty table preview. *)
      (match !current_cell with
       | Some _ ->
           finish_row ();
           (try render_table () with Content_limit -> ())
       | None -> ());
      true in
  String.trim (Buffer.contents output), truncated

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
  let request = { search = None; method_ = "GET"; url;
    headers = ["Accept", "text/html,application/xhtml+xml,application/json,text/plain;q=0.9";
      "User-Agent", "pave-web-fetch/1.0"];
    body = ""; response_limit = max_fetch_response_bytes } in
  let body = invoke ?http ?cancel request in
  (* Past the download limit, keep the leading part as an explicitly
     truncated preview instead of discarding the whole page. *)
  let partial = String.length body > max_fetch_response_bytes in
  let body = if partial then
      String.sub body 0 (utf8_prefix_length body max_fetch_response_bytes)
    else body in
  let rec first index =
    if index < String.length body && is_space body.[index] then first (index + 1)
    else index in
  let start = first 0 in
  let markdown, truncated =
    if start < String.length body && (body.[start] = '{' || body.[start] = '[') then
      (* Structured documents are data: never strip HTML inside JSON strings.
         A bounded prefix is explicitly marked as incomplete by the tool. *)
      if String.length body <= max_bytes then body, false
      else String.sub body 0 (utf8_prefix_length body max_bytes), true
    else convert_html_to_markdown ~max_bytes body in
  { source_url = url; markdown; truncated = truncated || partial }

(* Visible text of an HTML fragment: tags dropped, entities decoded,
   whitespace collapsed. *)
let html_text fragment =
  let output = Buffer.create (String.length fragment) in
  let length = String.length fragment in
  let rec loop index =
    if index < length then
      if fragment.[index] = '<' then
        (match String.index_from_opt fragment index '>' with
         | Some close -> Buffer.add_char output ' '; loop (close + 1)
         | None -> ())
      else if fragment.[index] = '&' then
        (match String.index_from_opt fragment (index + 1) ';' with
         | Some ending when ending - index <= 16 ->
             (match decode_entity (String.sub fragment (index + 1) (ending - index - 1)) with
              | Some decoded -> Buffer.add_string output decoded; loop (ending + 1)
              | None -> Buffer.add_char output '&'; loop (index + 1))
         | _ -> Buffer.add_char output '&'; loop (index + 1))
      else (Buffer.add_char output fragment.[index]; loop (index + 1)) in
  loop 0;
  clean_text ~limit:1_024 (Buffer.contents output)

let percent_decode text =
  let hex c = match c with
    | '0'..'9' -> Some (Char.code c - 48)
    | 'a'..'f' -> Some (Char.code c - 87)
    | 'A'..'F' -> Some (Char.code c - 55)
    | _ -> None in
  let output = Buffer.create (String.length text) in
  let length = String.length text in
  let rec loop index =
    if index < length then
      match text.[index] with
      | '%' when index + 2 < length ->
          (match hex text.[index + 1], hex text.[index + 2] with
           | Some high, Some low ->
               Buffer.add_char output (Char.chr (high * 16 + low)); loop (index + 3)
           | _ -> Buffer.add_char output '%'; loop (index + 1))
      | '+' -> Buffer.add_char output ' '; loop (index + 1)
      | c -> Buffer.add_char output c; loop (index + 1) in
  loop 0;
  Buffer.contents output

(* HTML class membership is independent of quote style and class order.
   Reuse the same checked attribute scanner as document links. *)
let has_class classes wanted =
  let length = String.length classes and width = String.length wanted in
  let rec equal start offset =
    offset = width ||
    (classes.[start + offset] = wanted.[offset] && equal start (offset + 1)) in
  let rec scan index =
    if index >= length then false
    else if is_space classes.[index] then scan (index + 1)
    else
      let ending = ref index in
      while !ending < length && not (is_space classes.[!ending]) do incr ending done;
      (!ending - index = width && equal index 0) || scan !ending in
  scan 0

(* Next opening tag (optionally of one name) whose attributes satisfy
   [matches]; comments are skipped. Returns its '<' and '>' offsets. *)
let rec find_tag_where html ~tag ~matches index =
  match String.index_from_opt html index '<' with
  | None -> None
  | Some start when start + 4 <= String.length html &&
      String.sub html start 4 = "<!--" ->
      (match find_case_insensitive html "-->" (start + 4) with
       | None -> None
       | Some ending -> find_tag_where html ~tag ~matches (ending + 3))
  | Some start ->
      match find_tag_end html (start + 1) with
      | None -> None
      | Some ending ->
          let content = String.sub html (start + 1) (ending - start - 1) in
          match parse_tag content with
          | Some (false, name, attributes)
              when (match tag with None -> true | Some expected -> name = expected) &&
                matches attributes ->
              Some (start, ending, attributes)
          | _ -> find_tag_where html ~tag ~matches (ending + 1)

let find_class_tag html ~tag wanted index =
  find_tag_where html ~tag index ~matches:(fun attributes ->
    match parse_attribute "class" attributes with
    | Some classes -> has_class classes wanted
    | None -> false)

let find_test_id_tag html ~tag wanted index =
  find_tag_where html ~tag index ~matches:(fun attributes ->
    parse_attribute "data-test-id" attributes = Some wanted)

(* DuckDuckGo routes clicks through //duckduckgo.com/l/?uddg=<target>. *)
let unwrap_duckduckgo_href href =
  let href = String.concat "&" (String.split_on_char '&' href
    |> List.map (fun part -> if starts_with part "amp;" then
      String.sub part 4 (String.length part - 4) else part)) in
  match find_case_insensitive href "uddg=" 0 with
  | Some start ->
      let value = start + 5 in
      let stop = Option.value ~default:(String.length href)
        (String.index_from_opt href value '&') in
      Some (percent_decode (String.sub href value (stop - value)))
  | None when starts_with href "https://" -> Some href
  | None -> None

(* Scraped results are filtered, not trusted: non-HTTPS, private, ad and
   duplicate links are dropped rather than failing the whole page. *)
let parse_duckduckgo count html =
  if find_case_insensitive html "anomaly-modal" 0 <> None ||
     find_case_insensitive html "anomaly.js" 0 <> None then
    fail "DuckDuckGo blocked the request with a bot-detection challenge; configure a credentialed provider such as Brave, Tavily, Exa or Kagi";
  let seen = Hashtbl.create 16 in
  let tag_start_before index =
    let rec back i = if i <= 0 then 0 else if html.[i] = '<' then i else back (i - 1) in
    back index in
  let rec collect index rows =
    if List.length rows >= count then List.rev rows
    else match find_class_tag html ~tag:(Some "a") "result__a" index with
      | None -> List.rev rows
      | Some (_, tag_end, attributes) ->
               let next = match find_class_tag html ~tag:(Some "a") "result__a" (tag_end + 1) with
                 | Some (start, _, _) -> start
                 | None -> String.length html in
               let title_end = match find_case_insensitive html "</a>" (tag_end + 1) with
                 | Some close when close < next -> close
                 | _ -> tag_end + 1 in
               let title = html_text (String.sub html (tag_end + 1) (title_end - tag_end - 1)) in
               let snippet = match find_case_insensitive html "result__snippet" title_end with
                 | Some snippet_at when snippet_at < next ->
                     (* Scan from the tag's '<' so the class attribute's quotes pair up. *)
                     (match find_tag_end html (tag_start_before snippet_at) with
                      | Some open_end ->
                          let close = List.filter_map (fun closing ->
                            find_case_insensitive html closing open_end) ["</a>"; "</div>"; "</span>"]
                            |> List.fold_left min next in
                          html_text (String.sub html (open_end + 1) (max 0 (close - open_end - 1)))
                      | None -> "")
                 | _ -> "" in
               let row = match Option.bind (parse_attribute "href" attributes) unwrap_duckduckgo_href with
                 | Some url when title <> "" && String.length url <= 4_096 ->
                     (match normalized_url_key url with
                      | key when not (Hashtbl.mem seen key) &&
                          not (String.ends_with ~suffix:"duckduckgo.com"
                            (let _, host, _, _ = Workspace_reader.parse_url url in host)) ->
                          Hashtbl.add seen key (); Some (title, url, snippet)
                      | _ -> None
                      | exception Error _ -> None
                      | exception Workspace_reader.Error _ -> None)
                 | _ -> None in
               collect (title_end + 1) (match row with Some row -> row :: rows | None -> rows) in
  match collect 0 [] with
  | [] when find_class_tag html ~tag:None "no-results" 0 <> None ||
            find_class_tag html ~tag:None "result--no-result" 0 <> None ->
      []
  | [] -> fail "DuckDuckGo returned no usable result markup; it may have blocked the request or changed its HTML; use another configured search provider or fetch a known source URL directly"
  | rows -> rows

(* Ecosia's rendered results are <article data-test-id="organic-result">
   blocks titled by aria-label, linked by data-test-id="result-link" and
   described by data-test-id="web-result-description". *)
let parse_ecosia count html =
  let article index = find_test_id_tag html ~tag:(Some "article") "organic-result" index in
  let rec articles index rows =
    match article index with
    | None -> List.rev rows
    | Some (_, ending, attributes) ->
        let next = match article (ending + 1) with
          | Some (start, _, _) -> start
          | None -> String.length html in
        let within = function
          | Some ((start, _, _) as found) when start < next -> Some found
          | _ -> None in
        let rec link index =
          match within (find_test_id_tag html ~tag:(Some "a") "result-link" index) with
          | None -> None
          | Some (_, link_end, link_attributes) ->
              let external_https href =
                starts_with href "https://" &&
                (try let _, host, _, _ = Workspace_reader.parse_url href in
                   host <> "ecosia.org" && not (String.ends_with ~suffix:".ecosia.org" host)
                 with Workspace_reader.Error _ -> false) in
              (match parse_attribute "href" link_attributes with
               | Some href when external_https href -> Some href
               | _ -> link (link_end + 1)) in
        let title = match parse_attribute "aria-label" attributes with
          | Some label -> clean_text ~limit:1_024 label
          | None -> "" in
        let snippet =
          match within (find_test_id_tag html ~tag:None "web-result-description" ending) with
          | Some (_, open_end, _) ->
              let close = Option.value ~default:next
                (find_case_insensitive html "</p>" open_end) in
              html_text (String.sub html (open_end + 1) (max 0 (min close next - open_end - 1)))
          | None -> "" in
        let rows = match link (ending + 1) with
          | Some url when title <> "" -> (title, url, snippet) :: rows
          | _ -> rows in
        articles next rows in
  match validate_rows count (articles 0 []) with
  | [] when List.exists (fun marker -> find_case_insensitive html marker 0 <> None)
      ["_cf_chl_opt"; "/cdn-cgi/challenge-platform/"; "ecosia firewall"; "not a robot"] ->
      fail "Ecosia answered with a bot-detection challenge; use another configured search provider or fetch a known source URL directly"
  | [] -> fail "Ecosia returned no usable result markup; it may have blocked the request or changed its HTML"
  | rows -> rows

let search_engine ?http ?cancel ?render ~query ~page ~count candidate =
  match candidate.engine, candidate.browser with
  | Ecosia, Some program ->
      let request = build_request Ecosia None ~query ~page ~count in
      let render = match render with
        | Some render -> render
        | None -> fun ~program url -> render_page ?cancel ~program url in
      let html = render ~program request.url in
      check_cancel cancel;
      numbered Ecosia (parse_ecosia count html)
  | Ecosia, None -> fail "Ecosia needs a local Chromium-family browser"
  | engine, _ ->
      let request = build_request engine candidate.key ~query ~page ~count in
      let body = invoke ?http ?cancel request in
      match engine with
      | Duckduckgo -> numbered Duckduckgo (parse_duckduckgo count body)
      | engine -> parse_results engine count (parse_json body)

(* Engines are tried in plan order; a failure or an empty answer moves to
   the next engine and failures are reported with the answer, so substitution
   is never silent. If every engine answers empty, the result is empty, not
   an error. Cancellation stops immediately. *)
let search ?http ?cancel ?(env = Sys.getenv_opt) ?find_browser ?render
    ?(page = 0) ?(count = 5) ~query () =
  let query = valid_query query in
  validate_limits ~page ~count;
  let _, candidates = plan ~env ?find_browser ~page () in
  let answer candidate results failed =
    { provider = provider_name candidate.engine; query; page; results;
      citations = List.map (fun result -> result.citation) results;
      failed = List.rev failed } in
  let rec attempt failed empty = function
    | [] ->
        (match empty, List.rev failed with
         | Some candidate, _ -> answer candidate [] failed
         | None, [_, message] -> fail message
         | None, failures ->
             fail ("all web search providers failed: " ^ String.concat "; "
               (List.map (fun (name, message) -> name ^ ": " ^ message) failures)))
    | candidate :: rest ->
        match search_engine ?http ?cancel ?render ~query ~page ~count candidate with
        | [] ->
            check_cancel cancel;
            attempt failed (if empty = None then Some candidate else empty) rest
        | results -> answer candidate results failed
        | exception (Error message) ->
            check_cancel cancel;
            attempt ((provider_name candidate.engine, message) :: failed) empty rest in
  attempt [] None candidates
