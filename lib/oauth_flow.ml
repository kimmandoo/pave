exception OAuth_error of string

type body_format = Json | Form

type policy = {
  client_id : string;
  authorize_url : string;
  token_url : string;
  redirect_uri : string;
  scopes : string list;
  token_body : body_format;
  extra_authorize_params : (string * string) list;
  extra_token_params : (string * string) list;
  extra_token_headers : (string * string) list;
  refresh_url : string option;
  refresh_body : body_format option;
  extra_refresh_params : (string * string) list;
  extra_refresh_headers : (string * string) list;
  account_path : string list;
  metadata_paths : (string * string list) list;
  expiry_skew : float;
}

type authorization = {
  state : string;
  verifier : string;
  challenge : string;
  url : string;
  redirect_uri : string;
  deadline : float;
}

type http = url:string -> headers:(string * string) list -> body:string -> int * string

type uri = { scheme : string; authority : string; path : string; query : string }

let fail message = raise (OAuth_error message)

let has_controls value =
  String.exists (fun c -> Char.code c < 32 || Char.code c = 127 || c = '\\') value

let slice s start stop = String.sub s start (stop - start)

let find_from s start c =
  try String.index_from s start c with Not_found -> String.length s

let parse_uri value =
  if has_controls value || String.contains value '#' || String.contains value ' ' then
    fail "invalid OAuth URL";
  let marker = try String.index value ':' with Not_found -> fail "invalid OAuth URL" in
  if marker + 2 >= String.length value || slice value marker (marker + 3) <> "://" then
    fail "invalid OAuth URL";
  let scheme = String.lowercase_ascii (slice value 0 marker) in
  let start = marker + 3 in
  let stop = min (find_from value start '/') (find_from value start '?') in
  let authority = slice value start stop in
  if authority = "" || String.contains authority '@' || String.contains authority '%'
     || String.contains authority ' ' then fail "invalid OAuth URL";
  let port =
    if authority.[0] = '[' then (
      let close = try String.index authority ']' with Not_found -> fail "invalid OAuth URL" in
      let host = slice authority 1 close in
      if host = "" || not (String.contains host ':') ||
         not (String.for_all (function
           | '0'..'9' | 'a'..'f' | 'A'..'F' | ':' | '.' -> true | _ -> false) host)
      then fail "invalid OAuth URL";
      slice authority (close + 1) (String.length authority))
    else (
      let stop = find_from authority 0 ':' in
      let host = slice authority 0 stop in
      if host = "" || not (String.for_all (function
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' -> true | _ -> false) host)
      then fail "invalid OAuth URL";
      slice authority stop (String.length authority)) in
  if port <> "" then (
    if port.[0] <> ':' || String.length port < 2 ||
       not (String.for_all (function '0'..'9' -> true | _ -> false)
         (slice port 1 (String.length port)))
    then fail "invalid OAuth URL";
    let number = try int_of_string (slice port 1 (String.length port))
      with Failure _ -> fail "invalid OAuth URL" in
    if number < 1 || number > 65535 then fail "invalid OAuth URL");
  let query_start = find_from value stop '?' in
  let path = if stop = String.length value || value.[stop] = '?' then "/"
    else slice value stop query_start in
  let query = if query_start = String.length value then ""
    else slice value (query_start + 1) (String.length value) in
  if path = "" || path.[0] <> '/' then fail "invalid OAuth URL";
  { scheme; authority; path; query }

let loopback_host authority =
  let host = String.lowercase_ascii authority in
  let valid_port prefix =
    let port = slice host (String.length prefix) (String.length host) in
    port <> "" && String.for_all (function '0'..'9' -> true | _ -> false) port
    && (try let number = int_of_string port in number > 0 && number <= 65535
        with Failure _ -> false) in
  List.exists (fun prefix ->
    String.starts_with ~prefix host && valid_port prefix)
    ["localhost:"; "127.0.0.1:"; "[::1]:"]

let validate_url ?(local = false) value =
  let uri = parse_uri value in
  if uri.scheme <> "https" && not (local && uri.scheme = "http" && loopback_host uri.authority)
  then fail "OAuth endpoint must use HTTPS";
  uri

let redirect (policy : policy) =
  let uri = validate_url ~local:true policy.redirect_uri in
  if uri.query <> "" || not (loopback_host uri.authority) then
    fail "OAuth redirect must be a loopback URL without a query";
  uri

let validate_policy ?http policy =
  if policy.client_id = "" || has_controls policy.client_id then fail "invalid OAuth client ID";
  ignore (validate_url ~local:(Option.is_some http) policy.authorize_url);
  ignore (validate_url ~local:(Option.is_some http) policy.token_url);
  Option.iter (fun url -> ignore (validate_url ~local:(Option.is_some http) url)) policy.refresh_url;
  ignore (redirect policy);
  if policy.expiry_skew < 0. || not (Float.is_finite policy.expiry_skew) then
    fail "invalid OAuth expiry skew"

let random_bytes length =
  let fd = Unix.openfile "/dev/urandom" [Unix.O_RDONLY] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let bytes = Bytes.create length in
    let rec fill offset =
      if offset < length then
        let n = try Unix.read fd bytes offset (length - offset)
          with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
        if n = 0 then fail "secure randomness unavailable"
        else if n < 0 then fill offset else fill (offset + n)
    in
    fill 0;
    Bytes.unsafe_to_string bytes)

let base64url input =
  let table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" in
  let size = String.length input in
  let out = Buffer.create ((size * 4 + 2) / 3) in
  let rec go i =
    if i < size then (
      let a = Char.code input.[i] in
      let b = if i + 1 < size then Char.code input.[i + 1] else 0 in
      let c = if i + 2 < size then Char.code input.[i + 2] else 0 in
      Buffer.add_char out table.[a lsr 2];
      Buffer.add_char out table.[((a land 3) lsl 4) lor (b lsr 4)];
      if i + 1 < size then Buffer.add_char out table.[((b land 15) lsl 2) lor (c lsr 6)];
      if i + 2 < size then Buffer.add_char out table.[c land 63];
      go (i + 3))
  in
  go 0;
  Buffer.contents out

let pkce_challenge verifier = base64url (Digestif.SHA256.(to_raw_string (digest_string verifier)))

let url_encode value =
  let out = Buffer.create (String.length value) in
  String.iter (fun c ->
    match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' | '_' | '~' -> Buffer.add_char out c
    | _ -> Printf.bprintf out "%%%02X" (Char.code c)) value;
  Buffer.contents out

let encode_params params =
  String.concat "&" (List.map (fun (key, value) -> url_encode key ^ "=" ^ url_encode value) params)

let unique_params params =
  let seen = Hashtbl.create 16 in
  List.iter (fun (key, value) ->
    if key = "" || has_controls key || has_controls value || Hashtbl.mem seen key then
      fail "invalid or duplicate OAuth parameter";
    Hashtbl.add seen key ()) params

let hex_digit c = match c with
  | '0'..'9' -> Char.code c - 48
  | 'a'..'f' -> Char.code c - 87
  | 'A'..'F' -> Char.code c - 55
  | _ -> fail "malformed OAuth callback query"

let decode value =
  let out = Buffer.create (String.length value) in
  let rec go i =
    if i < String.length value then match value.[i] with
    | '%' ->
        if i + 2 >= String.length value then fail "malformed OAuth callback query";
        Buffer.add_char out (Char.chr (16 * hex_digit value.[i + 1] + hex_digit value.[i + 2]));
        go (i + 3)
    | '+' -> Buffer.add_char out ' '; go (i + 1)
    | c -> Buffer.add_char out c; go (i + 1)
  in
  go 0;
  Buffer.contents out

let query_params query =
  if query = "" then [] else
  let pairs = String.split_on_char '&' query |> List.map (fun item ->
    match String.index_opt item '=' with
    | None -> decode item, ""
    | Some index -> decode (slice item 0 index),
      decode (slice item (index + 1) (String.length item))) in
  unique_params pairs;
  pairs

let start ?http ?(now = Unix.gettimeofday ()) ?(ttl = 300.) policy =
  validate_policy ?http policy;
  if not (Float.is_finite now) || not (Float.is_finite ttl) || ttl <= 0. then
    fail "invalid OAuth authorization deadline";
  let state = base64url (random_bytes 32) in
  let verifier = base64url (random_bytes 32) in
  let challenge = pkce_challenge verifier in
  let params = [
    "response_type", "code"; "client_id", policy.client_id;
    "redirect_uri", policy.redirect_uri; "scope", String.concat " " policy.scopes;
    "state", state; "code_challenge", challenge;
    "code_challenge_method", "S256"
  ] @ policy.extra_authorize_params in
  unique_params (query_params (parse_uri policy.authorize_url).query @ params);
  let separator = if String.contains policy.authorize_url '?' then "&" else "?" in
  { state; verifier; challenge; redirect_uri = policy.redirect_uri;
    url = policy.authorize_url ^ separator ^ encode_params params; deadline = now +. ttl }

let constant_equal a b =
  let differing = ref (String.length a lxor String.length b) in
  for i = 0 to max (String.length a) (String.length b) - 1 do
    let ca = if i < String.length a then Char.code a.[i] else 0 in
    let cb = if i < String.length b then Char.code b.[i] else 0 in
    differing := !differing lor (ca lxor cb)
  done;
  !differing = 0

let required key params =
  match List.assoc_opt key params with
  | Some value when value <> "" -> value
  | _ -> fail "missing OAuth callback parameter"

let check_deadline ?(now = Unix.gettimeofday ()) auth =
  if not (Float.is_finite now) || now >= auth.deadline then fail "OAuth authorization timed out"

let parse_callback ?now auth ~response =
  check_deadline ?now auth;
  let expected = parse_uri auth.redirect_uri in
  let response = String.trim response in
  let params =
    if String.starts_with ~prefix:"http://" response
       || String.starts_with ~prefix:"https://" response then (
      let actual = parse_uri response in
      if actual.scheme <> expected.scheme ||
         String.lowercase_ascii actual.authority <> String.lowercase_ascii expected.authority ||
         actual.path <> expected.path then fail "OAuth callback redirect mismatch";
      query_params actual.query)
    else if String.starts_with ~prefix:"/" response then (
      if String.contains response '#' then fail "OAuth callback redirect mismatch";
      let split = find_from response 0 '?' in
      if slice response 0 split <> expected.path then fail "OAuth callback redirect mismatch";
      query_params (if split = String.length response then "" else
        slice response (split + 1) (String.length response)))
    else (
      (* A pasted code still requires the state sent with the authorization request. *)
      match String.split_on_char '#' response with
      | [code; state] when code <> "" && state <> "" -> ["code", code; "state", state]
      | _ -> fail "paste the redirect URL or code#state") in
  let received = required "state" params in
  if not (constant_equal auth.state received) then fail "OAuth callback state mismatch";
  if List.mem_assoc "error" params then fail "OAuth authorization denied";
  let code = required "code" params in
  if has_controls code then fail "invalid OAuth authorization code";
  code

let quote_config value =
  if has_controls value then fail "invalid OAuth HTTP configuration";
  let buffer = Buffer.create (String.length value + 2) in
  Buffer.add_char buffer '"';
  String.iter (function
    | '"' -> Buffer.add_string buffer "\\\""
    | '\\' -> Buffer.add_string buffer "\\\\"
    | c -> Buffer.add_char buffer c) value;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let with_private_file f =
  let path, oc = Filename.open_temp_file ~mode:[Open_binary] "pave-oauth-" ".tmp" in
  Unix.chmod path 0o600;
  Fun.protect ~finally:(fun () ->
    close_out_noerr oc;
    try Sys.remove path with Sys_error _ -> ()) (fun () -> f path oc)

let read_bounded path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    let size = in_channel_length ic in
    if size > 1_048_576 then fail "OAuth token response too large";
    really_input_string ic size)

let default_http ~url ~headers ~body =
  ignore (validate_url url);
  with_private_file (fun body_path body_file ->
    output_string body_file body; close_out body_file;
    with_private_file (fun output_path output_file ->
      close_out output_file;
      with_private_file (fun config_path config_file ->
        let option key value = output_string config_file (key ^ " = " ^ quote_config value ^ "\n") in
        output_string config_file "silent\n";
        option "url" url;
        option "request" "POST";
        List.iter (fun (key, value) -> option "header" (key ^ ": " ^ value)) headers;
        option "data-binary" ("@" ^ body_path);
        option "output" output_path;
        option "write-out" "%{http_code}";
        option "connect-timeout" "10";
        option "max-time" "30";
        option "max-filesize" "1048576";
        option "proto" "=https";
        close_out config_file;
        let input = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
        let errors = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
        let output_read, output_write = Unix.pipe () in
        let pid = try Unix.create_process "curl" [|"curl"; "--disable"; "--config"; config_path|]
            input output_write errors
          with _ ->
            List.iter Unix.close [input; errors; output_read; output_write];
            fail "could not start OAuth token request" in
        List.iter Unix.close [input; errors; output_write];
        let status = Fun.protect ~finally:(fun () -> Unix.close output_read) (fun () ->
          let bytes = Bytes.create 16 in
          let n = Unix.read output_read bytes 0 16 in
          Bytes.sub_string bytes 0 n) in
        let _, exited = Unix.waitpid [] pid in
        match exited with
        | Unix.WEXITED 0 ->
            let code = try int_of_string status with _ -> fail "invalid OAuth HTTP status" in
            code, read_bounded output_path
        | _ -> fail "OAuth token request failed")))

let post ?http ~url ~headers ~format params =
  ignore (validate_url ~local:(Option.is_some http) url);
  unique_params params;
  let body, content_type = match format with
    | Json -> Yojson.Basic.to_string (`Assoc (List.map (fun (k, v) -> k, `String v) params)),
      "application/json"
    | Form -> encode_params params, "application/x-www-form-urlencoded" in
  if String.length body > 65536 then fail "OAuth token request too large";
  let seen_headers = Hashtbl.create 8 in
  List.iter (fun (key, value) ->
    let key_lower = String.lowercase_ascii key in
    if key = "" || has_controls key || has_controls value || String.contains key ':'
       || key_lower = "content-type" || Hashtbl.mem seen_headers key_lower then
      fail "invalid or duplicate OAuth token header";
    Hashtbl.add seen_headers key_lower ()) headers;
  let send = match http with None -> default_http | Some send -> send in
  let status, response = send ~url ~headers:(("Content-Type", content_type) :: headers) ~body in
  if status < 200 || status >= 300 then fail "OAuth token endpoint rejected the grant";
  if String.length response > 1_048_576 then fail "OAuth token response too large";
  let json = try Yojson.Basic.from_string response with _ -> fail "invalid OAuth token response" in
  (match json with `Assoc _ -> () | _ -> fail "invalid OAuth token response");
  if Yojson.Basic.Util.member "error" json <> `Null then fail "OAuth token endpoint rejected the grant";
  json

let member key = function
  | `Assoc fields -> (match List.assoc_opt key fields with Some value -> value | None -> `Null)
  | _ -> `Null

let string_at path json =
  let value = List.fold_left (fun json key -> member key json) json path in
  match value with `String value when value <> "" -> Some value | _ -> None

let response_field key json = match json with
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let expiry ~now ~skew json =
  let duration = match response_field "expires_in" json with
    | None -> None
    | Some (`Int n) when n > 0 -> Some (float_of_int n)
    | Some (`Float n) when Float.is_finite n && n > 0. -> Some n
    | _ -> fail "invalid OAuth token expiry" in
  Option.map (fun duration ->
    let expires_at = now +. duration -. skew in
    if not (Float.is_finite expires_at) || expires_at <= now then
      fail "OAuth token is already expired";
    expires_at) duration

let credential ?prior ~now policy json : Oauth_store.credential =
  let access = match string_at ["access_token"] json with
    | Some access -> access | None -> fail "OAuth token response is missing an access token" in
  let refresh = match response_field "refresh_token" json with
    | Some (`String token) when token <> "" -> Some token
    | None -> Option.bind prior (fun (row : Oauth_store.credential) -> row.refresh)
    | _ -> fail "invalid OAuth refresh token" in
  let expires_at = expiry ~now ~skew:policy.expiry_skew json in
  let account_id = match string_at policy.account_path json with
    | Some _ as value -> value
    | None -> Option.bind prior (fun (row : Oauth_store.credential) -> row.account_id) in
  let extracted = List.filter_map (fun (name, path) ->
    match string_at path json with
    | Some value -> Some (name, value)
    | None -> None) policy.metadata_paths in
  let metadata = match prior with
    | None -> extracted
    | Some row ->
        (* Refresh responses must not silently move a credential to another org. *)
        row.metadata @ List.filter (fun (name, _) ->
          not (List.mem_assoc name row.metadata)) extracted in
  { access; refresh; expires_at; account_id; metadata }

let substitute_params substitutions params =
  List.map (fun (key, value) ->
    key, (match List.assoc_opt value substitutions with
      | Some resolved -> resolved | None -> value)) params

let exchange ?http ?(now = Unix.gettimeofday ()) policy auth ~response =
  validate_policy ?http policy;
  if auth.redirect_uri <> policy.redirect_uri then fail "OAuth callback redirect mismatch";
  let code = parse_callback ~now auth ~response in
  let params = ["grant_type", "authorization_code"; "client_id", policy.client_id;
    "code", code; "redirect_uri", policy.redirect_uri;
    "code_verifier", auth.verifier] @
    substitute_params ["{state}", auth.state; "{code}", code] policy.extra_token_params in
  let json = post ?http ~url:policy.token_url ~headers:policy.extra_token_headers
      ~format:policy.token_body params in
  credential ~now policy json

let refresh_headers policy =
  let replaced name = List.exists (fun (key, _) ->
    String.lowercase_ascii key = String.lowercase_ascii name) policy.extra_refresh_headers in
  List.filter (fun (name, _) -> not (replaced name)) policy.extra_token_headers
  @ policy.extra_refresh_headers

let refresh ?http ?(now = Unix.gettimeofday ()) policy (prior : Oauth_store.credential) =
  validate_policy ?http policy;
  if not (Float.is_finite now) then fail "invalid OAuth refresh time";
  let token = match prior.refresh with
    | Some token when token <> "" -> token
    | _ -> fail "OAuth credential has no refresh token" in
  let params = ["grant_type", "refresh_token"; "client_id", policy.client_id;
    "refresh_token", token] @ policy.extra_refresh_params in
  let json = post ?http ~url:(Option.value policy.refresh_url ~default:policy.token_url)
      ~headers:(refresh_headers policy)
      ~format:(Option.value policy.refresh_body ~default:policy.token_body) params in
  credential ~prior ~now policy json

let port_of_authority authority =
  let split = try String.rindex authority ':' with Not_found -> fail "loopback callback requires a port" in
  let port = try int_of_string (slice authority (split + 1) (String.length authority))
    with _ -> fail "invalid loopback callback port" in
  if port < 1 || port > 65535 then fail "invalid loopback callback port";
  port

let listen_loopback ?now ?ttl policy =
  validate_policy policy;
  let uri = redirect policy in
  if uri.scheme <> "http" then fail "loopback listener requires an HTTP redirect";
  let host = String.lowercase_ascii uri.authority in
  let address =
    if String.starts_with ~prefix:"localhost:" host || String.starts_with ~prefix:"127.0.0.1:" host
    then Unix.inet_addr_loopback
    else if String.starts_with ~prefix:"[::1]:" host then Unix.inet_addr_of_string "::1"
    else fail "loopback callback requires a port" in
  let domain = if address = Unix.inet_addr_loopback then Unix.PF_INET else Unix.PF_INET6 in
  let socket = Unix.socket domain Unix.SOCK_STREAM 0 in
  try
    Unix.set_close_on_exec socket;
    Unix.setsockopt socket Unix.SO_REUSEADDR true;
    Unix.bind socket (Unix.ADDR_INET (address, port_of_authority uri.authority));
    Unix.listen socket 4;
    let auth = start ?now ?ttl policy in
    auth, socket
  with exn -> Unix.close socket; raise exn

let contains_headers_end input start =
  let length = Buffer.length input in
  let rec scan i =
    i + 3 < length &&
    ((Buffer.nth input i = '\r' && Buffer.nth input (i + 1) = '\n'
      && Buffer.nth input (i + 2) = '\r' && Buffer.nth input (i + 3) = '\n')
     || scan (i + 1)) in
  scan start

let read_callback_request ~now auth client =
  let input = Buffer.create 512 in
  let bytes = Bytes.create 1024 in
  let rec receive () =
    if Buffer.length input >= 8192 then fail "OAuth callback request too large";
    let remaining = auth.deadline -. now () in
    if remaining <= 0. then fail "OAuth authorization timed out";
    let ready, _, _ = Unix.select [client] [] [] remaining in
    if ready = [] then fail "OAuth authorization timed out";
    let n = Unix.read client bytes 0 (min (Bytes.length bytes) (8192 - Buffer.length input)) in
    if n = 0 then fail "invalid OAuth callback request";
    let old_length = Buffer.length input in
    Buffer.add_subbytes input bytes 0 n;
    if contains_headers_end input (max 0 (old_length - 3))
    then Buffer.contents input else receive ()
  in
  receive ()

let await_callback ?(now = Unix.gettimeofday) auth listener =
  Fun.protect ~finally:(fun () -> Unix.close listener) (fun () ->
    let rec loop () =
      let remaining = auth.deadline -. now () in
      if remaining <= 0. then fail "OAuth authorization timed out";
      let ready, _, _ = Unix.select [listener] [] [] remaining in
      if ready = [] then fail "OAuth authorization timed out";
      let client, peer = Unix.accept listener in
      let result = Fun.protect ~finally:(fun () -> Unix.close client) (fun () ->
        Unix.set_close_on_exec client;
        (match peer with
        | Unix.ADDR_INET (ip, _) when Unix.string_of_inet_addr ip = "127.0.0.1"
             || Unix.string_of_inet_addr ip = "::1" -> ()
        | _ -> fail "OAuth callback must originate from loopback");
        let result =
          try
            let request = read_callback_request ~now auth client in
            let lines = String.split_on_char '\n' request in
            let target = match lines with
              | first :: _ -> (match String.split_on_char ' ' (String.trim first) with
                  | ["GET"; target; "HTTP/1.1"] -> target
                  | _ -> fail "invalid OAuth callback request")
              | [] -> fail "invalid OAuth callback request" in
            let hosts = List.filter_map (fun line ->
              let line = String.trim line in
              if String.length line >= 5 && String.lowercase_ascii (String.sub line 0 5) = "host:"
              then Some (String.lowercase_ascii (String.trim (slice line 5 (String.length line))))
              else None) lines in
            let expected_host = String.lowercase_ascii (parse_uri auth.redirect_uri).authority in
            if hosts <> [expected_host] then fail "OAuth callback host mismatch";
            Ok (parse_callback ~now:(now ()) auth ~response:target)
          with
          | OAuth_error message -> Error message
          | Unix.Unix_error _ -> Error "invalid OAuth callback request" in
        let message = match result with Ok _ -> "Authorization received. You may close this page."
          | Error _ -> "Authorization could not be completed." in
        let reply = Printf.sprintf
          "HTTP/1.1 %s\r\nContent-Type: text/plain; charset=utf-8\r\nCache-Control: no-store\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
          (if Result.is_ok result then "200 OK" else "400 Bad Request") (String.length message) message in
        let rec send offset = if offset < String.length reply then
          let count = Unix.write_substring client reply offset (String.length reply - offset) in
          if count > 0 then send (offset + count) in
        (try send 0 with Unix.Unix_error _ -> ());
        result) in
      match result with Ok code -> code | Error _ -> loop ()
    in loop ())

let anthropic ~sdk_version () =
  if sdk_version = "" || has_controls sdk_version then fail "invalid Anthropic SDK version";
  {
    client_id = "9d1c250a-e61b-44d9-88ed-5944d1962f5e";
    authorize_url = "https://claude.ai/oauth/authorize";
    token_url = "https://api.anthropic.com/v1/oauth/token";
    redirect_uri = "http://localhost:54545/callback";
    scopes = ["org:create_api_key"; "user:profile"; "user:inference";
      "user:sessions:claude_code"; "user:mcp_servers"; "user:file_upload"];
    token_body = Json;
    extra_authorize_params = ["code", "true"];
    extra_token_params = ["state", "{state}"];
    extra_token_headers = [];
    refresh_url = None;
    refresh_body = None;
    extra_refresh_params = [];
    extra_refresh_headers = ["anthropic-beta", "oauth-2025-04-20";
      "User-Agent", "anthropic-sdk-typescript/" ^ sdk_version ^ " userOAuthProvider"];
    account_path = ["account"; "uuid"];
    metadata_paths = ["email", ["account"; "email_address"];
      "org_id", ["organization"; "uuid"];
      "org_name", ["organization"; "name"]];
    expiry_skew = 300.;
  }
