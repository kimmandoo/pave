(* RFC 8628 device authorization. The caller supplies a registered public client
   and the provider's declared endpoints; verification URLs come from the device
   response, never from a token or error message. *)
exception OAuth_error of string

type body_format = Oauth_flow.body_format = Json | Form
type http = Oauth_flow.http

type policy = {
  client_id : string;
  device_url : string;
  token_url : string;
  scopes : string list;
  scope_separator : string;
  device_body : body_format;
  token_body : body_format;
  device_headers : (string * string) list;
  token_headers : (string * string) list;
  extra_device_params : (string * string) list;
  extra_token_params : (string * string) list;
  account_path : string list;
  metadata_paths : (string * string list) list;
  expiry_skew : float;
}

type authorization = {
  verification_uri : string;
  verification_uri_complete : string option;
  user_code : string;
}

let fail message = raise (OAuth_error message)

let validate_url ?http url =
  try ignore (Oauth_flow.validate_url ~local:(Option.is_some http) url)
  with Oauth_flow.OAuth_error _ -> fail "OAuth device URL must use HTTPS (or injected loopback HTTP)"

let validate_params params =
  try Oauth_flow.unique_params params
  with Oauth_flow.OAuth_error _ -> fail "invalid OAuth device parameters"

let validate_headers headers =
  List.iter (fun (key, value) ->
    if key = "" || Oauth_flow.has_controls key || Oauth_flow.has_controls value
       || String.contains key ':' || String.lowercase_ascii key = "content-type"
    then fail "invalid OAuth device headers") headers

let validate ?http policy =
  if policy.client_id = "" || Oauth_flow.has_controls policy.client_id
     || policy.scope_separator = "" || Oauth_flow.has_controls policy.scope_separator
     || List.exists (fun scope -> scope = "" || Oauth_flow.has_controls scope) policy.scopes
     || not (Float.is_finite policy.expiry_skew) || policy.expiry_skew < 0.
  then fail "invalid OAuth device policy";
  validate_url ?http policy.device_url;
  validate_url ?http policy.token_url;
  validate_headers policy.device_headers;
  validate_headers policy.token_headers;
  validate_params (("client_id", policy.client_id) :: policy.extra_device_params);
  validate_params (["grant_type", "urn:ietf:params:oauth:grant-type:device_code";
    "client_id", policy.client_id; "device_code", "code"] @ policy.extra_token_params);
  if List.mem_assoc "scope" policy.extra_device_params && policy.scopes <> [] then
    fail "duplicate OAuth device scope"

let member key = function
  | `Assoc entries -> (match List.assoc_opt key entries with Some value -> value | None -> `Null)
  | _ -> `Null

let required key json = match member key json with
  | `String value when value <> "" -> value
  | _ -> fail "OAuth device response missing required field"

let optional key json = match member key json with
  | `Null -> None
  | `String value when value <> "" -> Some value
  | _ -> fail "invalid OAuth device response field"

let positive_number json = match json with
  | `Int value when value > 0 -> float_of_int value
  | `Float value when Float.is_finite value && value > 0. -> value
  | _ -> fail "invalid OAuth device numeric field"

let post ?http ~url ~headers ~format params =
  validate_params params;
  let body, content_type = match format with
    | Json -> Yojson.Basic.to_string (`Assoc (List.map (fun (key, value) -> key, `String value) params)),
      "application/json"
    | Form -> Oauth_flow.encode_params params, "application/x-www-form-urlencoded" in
  if String.length body > 65536 then fail "OAuth device request too large";
  let send = match http with None -> Oauth_flow.default_http | Some send -> send in
  let status, response =
    try send ~url ~headers:(("Content-Type", content_type) :: headers) ~body
    with _ -> fail "OAuth device request failed" in
  if String.length response > 1_048_576 then fail "OAuth device response too large";
  let json = try Yojson.Basic.from_string response with _ -> fail "invalid OAuth device response" in
  (match json with `Assoc _ -> () | _ -> fail "invalid OAuth device response");
  status, json

let path_string path json =
  match List.fold_left (fun json key -> member key json) json path with
  | `String value when value <> "" -> Some value
  | _ -> None

let credential ~now policy json : Oauth_store.credential =
  let access = required "access_token" json in
  let refresh = optional "refresh_token" json in
  let expires_at = match member "expires_in" json with
    | `Null -> None
    | value -> let seconds = positive_number value in
      Some (now +. seconds -. min policy.expiry_skew (seconds /. 10.)) in
  let account_id = path_string policy.account_path json in
  let metadata = List.filter_map (fun (key, path) ->
    Option.map (fun value -> key, value) (path_string path json)) policy.metadata_paths in
  { access; refresh; expires_at; account_id; metadata }

let login ?http ?(now = Unix.gettimeofday) ?(sleep = fun seconds ->
    ignore (Unix.select [] [] [] seconds)) ?(timeout = 600.) policy ~on_authorization =
  validate ?http policy;
  if not (Float.is_finite timeout) || timeout <= 0. || timeout > 3600. then
    fail "invalid OAuth device timeout";
  let start = now () in
  if not (Float.is_finite start) then fail "invalid OAuth device clock";
  let deadline = start +. timeout in
  if not (Float.is_finite deadline) then fail "invalid OAuth device timeout";
  let check_deadline () =
    let current = now () in
    if not (Float.is_finite current) || current >= deadline then
      fail "OAuth device authorization timed out";
    current in
  ignore (check_deadline ());
  let scope = if policy.scopes = [] then []
    else ["scope", String.concat policy.scope_separator policy.scopes] in
  let status, device = post ?http ~url:policy.device_url
    ~headers:policy.device_headers ~format:policy.device_body
    (["client_id", policy.client_id] @ scope @ policy.extra_device_params) in
  ignore (check_deadline ());
  if status < 200 || status >= 300 || member "error" device <> `Null then
    fail "OAuth device authorization rejected";
  let device_code = required "device_code" device in
  let user_code = required "user_code" device in
  let verification_uri = required "verification_uri" device in
  validate_url ?http verification_uri;
  let verification_uri_complete = optional "verification_uri_complete" device in
  Option.iter (validate_url ?http) verification_uri_complete;
  let valid_for = positive_number (member "expires_in" device) in
  let expiry = min deadline (start +. valid_for) in
  let interval = match member "interval" device with
    | `Null -> 5.
    | value -> positive_number value in
  let interval = max 1. interval in
  on_authorization { verification_uri; verification_uri_complete; user_code };
  let rec poll delay polls_left =
    let current = check_deadline () in
    if current >= expiry || polls_left <= 0 then fail "OAuth device authorization timed out";
    let status, json = post ?http ~url:policy.token_url
      ~headers:policy.token_headers ~format:policy.token_body
      (["grant_type", "urn:ietf:params:oauth:grant-type:device_code";
        "client_id", policy.client_id; "device_code", device_code]
        @ policy.extra_token_params) in
    let current = check_deadline () in
    if current >= expiry then fail "OAuth device authorization timed out";
    match member "error" json with
    | `Null when status >= 200 && status < 300 -> credential ~now:current policy json
    | `String ("authorization_pending" | "slow_down" as error)
      when status >= 400 && status < 500 ->
        let delay = if error = "slow_down" then delay +. 5. else delay in
        let remaining = min deadline expiry -. current in
        if remaining <= 0. || polls_left <= 1 then fail "OAuth device authorization timed out";
        sleep (min delay remaining);
        poll delay (polls_left - 1)
    | `String "access_denied" -> fail "OAuth device authorization denied"
    | `String "expired_token" -> fail "OAuth device authorization expired"
    | _ -> fail "OAuth device token request rejected"
  in
  (* Also bound calls if an injected clock does not advance or a provider keeps
     returning pending. The cap assumes a minimum one-second polling cadence. *)
  let max_polls = max 1 (int_of_float (ceil (min timeout valid_for)) + 1) in
  poll interval max_polls
