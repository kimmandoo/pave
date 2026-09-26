type auth = No_auth | Api_key_env of string

type model = {
  id : string;
  display_name : string option;
  tools : bool option;
}

type route = {
  name : string;
  endpoint : string;
  account_id : string option;
  auth : auth;
  models_endpoint : string option;
  models : model list;
}

type t = {
  id : string;
  display_name : string;
  default_route : string;
  routes : route list;
}

type parsed_url = {
  scheme : string;
  host : string;
  port : int option;
  path : string;
}

let fail message = invalid_arg ("custom provider: " ^ message)

let member name fields = List.assoc_opt name fields
let unique label fields =
  let names = List.map fst fields in
  if List.length names <> List.length (List.sort_uniq String.compare names) then
    fail ("duplicate " ^ label)

let object_fields label allowed = function
  | `Assoc fields ->
      unique (label ^ " field") fields;
      List.iter (fun (name, _) ->
        if not (List.mem name allowed) then fail ("unknown " ^ label ^ " field " ^ name)) fields;
      fields
  | _ -> fail (label ^ " must be an object")

let string label max_length = function
  | `String value when value <> "" && String.length value <= max_length &&
      not (String.exists (fun c -> Char.code c < 32 || Char.code c = 127) value) ->
      value
  | _ -> fail (label ^ " must be a nonempty string within its size limit")

let optional_string label max_length fields = match member label fields with
  | None -> None
  | Some value -> Some (string label max_length value)

let valid_identifier value =
  value <> "" && String.length value <= 64 &&
  (match value.[0] with 'a'..'z' | '0'..'9' -> true | _ -> false) &&
  String.for_all (function
    | 'a'..'z' | '0'..'9' | '-' | '_' | '.' -> true
    | _ -> false) value

let identifier label value =
  if valid_identifier value then value else fail (label ^ " must be a lowercase identifier")

let valid_env_name value =
  value <> "" && String.length value <= 128 &&
  (match value.[0] with 'A'..'Z' | '_' -> true | _ -> false) &&
  String.for_all (function 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false) value

let port_number value =
  if value = "" || String.length value > 5 ||
     not (String.for_all (function '0'..'9' -> true | _ -> false) value) then
    fail "endpoint has an invalid port";
  let number = try int_of_string value with Failure _ -> fail "endpoint has an invalid port" in
  if number < 1 || number > 65535 then fail "endpoint port must be between 1 and 65535";
  number

let parse_url ~allow_loopback_http value =
  let length = String.length value in
  if length = 0 || length > 2048 ||
     String.exists (fun c -> Char.code c <= 32 || Char.code c = 127 || c = '\\') value ||
     String.contains value '#' || String.contains value '?' then
    fail "endpoint must be a bounded URL without controls, query, or fragment";
  let scheme, start =
    if String.starts_with ~prefix:"https://" value then "https", 8
    else if allow_loopback_http && String.starts_with ~prefix:"http://" value then "http", 7
    else fail "endpoint must use HTTPS (loopback HTTP is allowed only for inference)" in
  let slash = match String.index_from_opt value start '/' with
    | Some index -> index
    | None -> fail "endpoint must include a path" in
  let authority = String.sub value start (slash - start) in
  let path = String.sub value slash (length - slash) in
  if authority = "" || String.contains authority '@' || String.contains authority '%' then
    fail "endpoint authority must not include user information or escapes";
  let host, port =
    if authority.[0] = '[' then (
      let closing = match String.index_opt authority ']' with
        | Some index -> index
        | None -> fail "endpoint has an invalid IPv6 authority" in
      let literal = String.sub authority 1 (closing - 1) in
      let normalized = try Unix.string_of_inet_addr (Unix.inet_addr_of_string literal)
        with Failure _ | Invalid_argument _ -> fail "endpoint has an invalid IPv6 address" in
      if not (String.contains normalized ':') then
        fail "endpoint bracketed host must be IPv6";
      let suffix = String.sub authority (closing + 1) (String.length authority - closing - 1) in
      let port = if suffix = "" then None
        else if String.starts_with ~prefix:":" suffix then
          Some (port_number (String.sub suffix 1 (String.length suffix - 1)))
        else fail "endpoint has an invalid port" in
      "[" ^ String.lowercase_ascii normalized ^ "]", port
    ) else (
      let colon = String.index_opt authority ':' in
      let host, port_text = match colon with
        | None -> authority, None
        | Some index ->
            String.sub authority 0 index,
            Some (port_number (String.sub authority (index + 1)
              (String.length authority - index - 1))) in
      let labels = String.split_on_char '.' host in
      if host = "" || String.length host > 253 ||
         List.exists (fun label -> label = "" || String.length label > 63 ||
           label.[0] = '-' || label.[String.length label - 1] = '-' ||
           not (String.for_all (function
             | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' -> true
             | _ -> false) label)) labels then
        fail "endpoint host is invalid";
      String.lowercase_ascii host, port_text
    ) in
  let path_segments = String.split_on_char '/' path in
  if not (String.starts_with ~prefix:"/" path) ||
     List.exists (fun segment -> segment = "." || segment = "..") path_segments ||
     String.exists (fun c -> Char.code c <= 32 || Char.code c = 127) path then
    fail "endpoint path is invalid";
  if scheme = "http" &&
     not (host = "127.0.0.1" || host = "[::1]") then
    fail "HTTP is allowed only for numeric loopback inference endpoints";
  { scheme; host; port; path }

let same_origin left right =
  let default_port = function "https" -> 443 | _ -> 80 in
  let port url = Option.value ~default:(default_port url.scheme) url.port in
  left.scheme = right.scheme && left.host = right.host && port left = port right

let validate_chat_endpoint value =
  let parsed = parse_url ~allow_loopback_http:true value in
  if not (String.ends_with ~suffix:"/chat/completions" parsed.path) then
    fail "Chat Completions endpoint path must end in /chat/completions";
  value

let validate_models_endpoint ~completion_endpoint value =
  let completion = parse_url ~allow_loopback_http:true completion_endpoint in
  let listing = parse_url ~allow_loopback_http:false value in
  if not (String.ends_with ~suffix:"/models" listing.path) then
    fail "models_endpoint path must end in /models";
  if not (same_origin completion listing) then
    fail "models_endpoint must use the inference endpoint's origin";
  value

let parse_model json =
  let fields = object_fields "model" ["id"; "display_name"; "tools"] json in
  let id = match member "id" fields with
    | Some (`String value) when value <> "" && String.length value <= 256 &&
        String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) value -> value
    | _ -> fail "model ID must be a bounded printable ASCII string" in
  let display_name = optional_string "display_name" 128 fields in
  let tools = match member "tools" fields with
    | None -> None
    | Some (`Bool value) -> Some value
    | Some _ -> fail "model tools capability must be boolean" in
  { id; display_name; tools }

let parse_route json =
  let fields = object_fields "route"
    ["name"; "api"; "endpoint"; "account_id"; "api_key_env";
     "models_endpoint"; "models"] json in
  let required name = match member name fields with
    | Some value -> value
    | None -> fail ("route requires " ^ name) in
  let name = identifier "route name" (string "route name" 64 (required "name")) in
  (match required "api" with
   | `String "openai-chat" -> ()
   | `String _ -> fail "only the openai-chat route is supported"
   | _ -> fail "route api must be a string");
  let endpoint = validate_chat_endpoint (string "endpoint" 2048 (required "endpoint")) in
  let account_id = optional_string "account_id" 256 fields in
  let api_key_env = match optional_string "api_key_env" 128 fields with
    | None -> No_auth
    | Some name when valid_env_name name -> Api_key_env name
    | Some _ -> fail "api_key_env must be an uppercase environment variable name" in
  let models_endpoint = Option.map
    (validate_models_endpoint ~completion_endpoint:endpoint)
    (optional_string "models_endpoint" 2048 fields) in
  let models = match member "models" fields with
    | None -> []
    | Some (`List entries) when List.length entries <= 4096 ->
        List.map parse_model entries
    | Some (`List _) -> fail "models allows at most 4096 entries"
    | Some _ -> fail "models must be an array" in
  if models <> [] && models_endpoint <> None then
    fail "models and models_endpoint are mutually exclusive";
  let ids = List.map (fun (model : model) -> model.id) models in
  if List.length ids <> List.length (List.sort_uniq String.compare ids) then
    fail "duplicate model ID in route";
  { name; endpoint; account_id; auth = api_key_env; models_endpoint; models }

let parse json =
  let fields = object_fields "provider"
    ["id"; "display_name"; "default_route"; "routes"] json in
  let required name = match member name fields with
    | Some value -> value
    | None -> fail ("provider requires " ^ name) in
  let id = identifier "ID" (string "ID" 64 (required "id")) in
  let display_name = string "display_name" 128 (required "display_name") in
  let default_route = identifier "default_route"
    (string "default_route" 64 (required "default_route")) in
  let routes = match required "routes" with
    | `List values when List.length values > 0 && List.length values <= 16 ->
        List.map parse_route values
    | `List _ -> fail "routes must contain between 1 and 16 entries"
    | _ -> fail "routes must be an array" in
  let names = List.map (fun route -> route.name) routes in
  if List.length names <> List.length (List.sort_uniq String.compare names) then
    fail "duplicate route name";
  let endpoints = List.map (fun route -> route.endpoint) routes in
  if List.length endpoints <> List.length (List.sort_uniq String.compare endpoints) then
    fail "duplicate route endpoint";
  if not (List.mem default_route names) then
    fail "default_route must name a configured route";
  { id; display_name; default_route; routes }

let parse_list = function
  | `List values when List.length values <= 128 ->
      let providers = List.map parse values in
      let ids = List.map (fun provider -> provider.id) providers in
      if List.length ids <> List.length (List.sort_uniq String.compare ids) then
        fail "duplicate provider ID";
      providers
  | `List _ -> fail "custom_providers allows at most 128 entries"
  | _ -> fail "custom_providers must be an array"

let auth_to_json = function
  | No_auth -> []
  | Api_key_env name -> ["api_key_env", `String name]

let model_to_json (model : model) =
  ["id", `String model.id] @
  (match model.display_name with None -> []
   | Some value -> ["display_name", `String value]) @
  (match model.tools with None -> []
   | Some value -> ["tools", `Bool value])

let route_to_json route =
  ["name", `String route.name; "api", `String "openai-chat";
   "endpoint", `String route.endpoint] @
  (match route.account_id with None -> []
   | Some value -> ["account_id", `String value]) @
  auth_to_json route.auth @
  (match route.models_endpoint with None -> []
   | Some value -> ["models_endpoint", `String value]) @
  (if route.models = [] then []
   else ["models", `List (List.map (fun model -> `Assoc (model_to_json model)) route.models)])
let fingerprint route =
  Digestif.SHA256.(to_hex
    (digest_string (Yojson.Basic.to_string (`Assoc (route_to_json route)))))


let to_json provider = `Assoc [
  "id", `String provider.id;
  "display_name", `String provider.display_name;
  "default_route", `String provider.default_route;
  "routes", `List (List.map (fun route -> `Assoc (route_to_json route)) provider.routes)
]
