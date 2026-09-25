(* A deliberately small AWS chain: environment keys, then static shared credentials
   and config profiles. Dynamic sources (SSO, role assumption, container and IMDS)
   are not silently emulated by unrelated environment values. *)
type credentials = {
  access_key_id : string;
  secret_access_key : string;
  session_token : string option;
}

let environment name = Sys.getenv_opt name

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

let read_file path =
  try
    let channel = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
      let length = in_channel_length channel in
      if length > 1_048_576 then invalid_arg "AWS profile file exceeds 1 MiB";
      really_input_string channel length)
  with Sys_error message when not (Sys.file_exists path) ->
    ignore message; ""

let ini_section ~name contents =
  let section = ref "" and pairs = ref [] in
  String.split_on_char '\n' contents |> List.iter (fun line ->
    let line = String.trim line in
    let size = String.length line in
    if size > 1 && line.[0] = '[' && line.[size - 1] = ']' then
      section := String.trim (String.sub line 1 (size - 2))
    else if !section = name && size > 0 && line.[0] <> '#' && line.[0] <> ';' then
      match String.index_opt line '=' with
      | None -> ()
      | Some index ->
          let key = String.sub line 0 index |> String.trim |> String.lowercase_ascii in
          let value = String.sub line (index + 1) (size - index - 1) |> String.trim in
          pairs := (key, value) :: !pairs);
  !pairs

let resolve ?(getenv = environment) ?credentials_file ?config_file () =
  let env_key = first_nonempty getenv [ "AWS_ACCESS_KEY_ID" ] in
  let env_secret = first_nonempty getenv [ "AWS_SECRET_ACCESS_KEY" ] in
  let token = first_nonempty getenv [ "AWS_SESSION_TOKEN" ] in
  match env_key, env_secret with
  | Some access_key_id, Some secret_access_key ->
      credentials ~access_key_id ~secret_access_key ~session_token:token
  | Some _, None | None, Some _ -> invalid_arg "incomplete AWS environment credentials"
  | None, None ->
      let home_file name = match getenv "HOME" with
        | Some home when home <> "" -> Filename.concat (Filename.concat home ".aws") name
        | _ -> invalid_arg "HOME unavailable; configure AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY" in
      let path explicit variable fallback = match explicit with
        | Some path -> path
        | None -> (match first_nonempty getenv [variable] with
          | Some path -> path | None -> home_file fallback) in
      let profile = Option.value (first_nonempty getenv ["AWS_PROFILE"]) ~default:"default" in
      let static = ini_section ~name:profile (read_file (path credentials_file "AWS_SHARED_CREDENTIALS_FILE" "credentials")) in
      let config_section = if profile = "default" then "default" else "profile " ^ profile in
      let configured = ini_section ~name:config_section
        (read_file (path config_file "AWS_CONFIG_FILE" "config")) in
      let from pairs name = match List.assoc_opt name pairs with
        | Some value when value <> "" -> Some value
        | _ -> None in
      let of_source pairs =
        match from pairs "aws_access_key_id", from pairs "aws_secret_access_key" with
        | Some access_key_id, Some secret_access_key ->
            Some (credentials ~access_key_id ~secret_access_key
              ~session_token:(from pairs "aws_session_token"))
        | Some _, None | None, Some _ ->
            invalid_arg "incomplete AWS profile credentials"
        | None, None -> None in
      match of_source static with
      | Some keys -> keys
      | None -> (match of_source configured with
          | Some keys -> keys
          | None -> invalid_arg ("AWS credentials unavailable for profile " ^ profile))

let amz_date () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let sha256 value = Digestif.SHA256.(to_hex (digest_string value))
let hmac key value = Digestif.SHA256.(to_raw_string (hmac_string ~key value))
let hmac_hex key value = Digestif.SHA256.(to_hex (hmac_string ~key value))

(* The HTTP path is already percent-encoded. SigV4's non-S3 canonical URI
   escapes each segment again (including '%' -> '%25'), preserving '/'.
   The Bedrock endpoint constructor encodes the model component on the wire. *)
let canonical_path path =
  let out = Buffer.create (String.length path) in
  String.iter (fun c -> match c with
    | '/' | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' ->
        Buffer.add_char out c
    | _ -> Buffer.add_string out (Printf.sprintf "%%%02X" (Char.code c))) path;
  Buffer.contents out
let sign ?(content_type = true) ~credentials:keys ~region:aws_region ~amz_date ~method_ ~host ~path ~body () =
  if String.length amz_date <> 16 || amz_date.[8] <> 'T' || amz_date.[15] <> 'Z' ||
    not (String.for_all (function '0'..'9' | 'T' | 'Z' -> true | _ -> false) amz_date)
  then invalid_arg "invalid AWS signing date";
  let region = region ~getenv:(fun _ -> Some aws_region) () in
  validate "host" host;
  if path = "" || path.[0] <> '/' ||
    String.exists (function ' ' | '\r' | '\n' | '?' | '#' -> true | _ -> false) path
  then invalid_arg "AWS path must be encoded before signing";
  let short_date = String.sub amz_date 0 8 in
  let payload_hash = sha256 body in
  let fields = [ "accept", "application/json" ] @
    (if content_type then [ "content-type", "application/json" ] else []) @
    [ "host", host;
      "x-amz-content-sha256", payload_hash;
      "x-amz-date", amz_date ] @
    (match keys.session_token with
      | None -> [] | Some token -> [ "x-amz-security-token", token ]) in
  let fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
  let names = String.concat ";" (List.map fst fields) in
  let canonical_headers = String.concat "" (List.map (fun (name, value) -> name ^ ":" ^ value ^ "\n") fields) in
  let canonical = String.concat "\n" [ method_; canonical_path path; ""; canonical_headers; names; payload_hash ] in
  let scope = short_date ^ "/" ^ region ^ "/bedrock/aws4_request" in
  let to_sign = String.concat "\n" [ "AWS4-HMAC-SHA256"; amz_date; scope; sha256 canonical ] in
  let key = hmac ("AWS4" ^ keys.secret_access_key) short_date in
  let key = hmac key region |> fun key -> hmac key "bedrock" |> fun key -> hmac key "aws4_request" in
  let authorization = "AWS4-HMAC-SHA256 Credential=" ^ keys.access_key_id ^ "/" ^ scope ^
    ", SignedHeaders=" ^ names ^ ", Signature=" ^ hmac_hex key to_sign in
  ("authorization", authorization) :: fields
