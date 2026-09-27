exception Authentication_error of string

let fail message = raise (Authentication_error message)

let valid_token token =
  token <> "" && String.length token <= 16384 &&
  String.for_all (fun c -> Char.code c >= 33 && Char.code c <= 126) token

let check_token token =
  if not (valid_token token) then fail "Google ADC returned an invalid access token";
  token

(* ADC authorized-user files contain refresh-token credentials; they are
   exchanged at Google's fixed endpoint using the standard OAuth refresh
   grant (https://developers.google.com/identity/protocols/oauth2/web-server#offline).
   ADC file types and fields: https://cloud.google.com/docs/authentication/application-default-credentials. *)
let oauth_token_endpoint = "https://oauth2.googleapis.com/token"
let oauth_content_type = "application/x-www-form-urlencoded"

(* ADC exchange hosts and credentials are fixed to Google's documented
   OAuth endpoints. Credential bodies use child stdin, never argv or logs. *)
exception Cancelled

let check_cancel = function
  | Some cancel when cancel () -> raise Cancelled
  | _ -> ()

let check_deadline ~cancel ~deadline ~program =
  check_cancel cancel;
  if Unix.gettimeofday () >= deadline then
    fail ("Google ADC " ^ Filename.basename program ^ " command timed out")

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()
let subprocess_unix_error name (error, operation, _) =
  fail (Printf.sprintf "Google ADC %s subprocess failed (%s: %s)"
    name operation (Unix.error_message error))


let execute ?(stdin = "") ?cancel ~timeout program arguments =
  if timeout <= 0. then fail "invalid Google ADC subprocess deadline";
  let deadline = Unix.gettimeofday () +. timeout in
  check_deadline ~cancel ~deadline ~program;
  let input_read, input_write = Unix.pipe () in
  let output_read, output_write = Unix.pipe () in
  let errors =
    try Unix.openfile "/dev/null" [Unix.O_WRONLY] 0
    with exn ->
      List.iter close_fd [input_read; input_write; output_read; output_write];
      raise exn in
  let pid =
    try Unix.fork ()
    with exn ->
      List.iter close_fd [input_read; input_write; output_read; output_write; errors];
      raise exn in
  if pid = 0 then (
    (try
       (try ignore (Unix.setsid ()) with Unix.Unix_error _ -> ());
       Unix.dup2 input_read Unix.stdin;
       Unix.dup2 output_write Unix.stdout;
       Unix.dup2 errors Unix.stderr;
       List.iter close_fd [input_read; input_write; output_read; output_write; errors];
       Unix.execvp program (Array.of_list (program :: arguments))
     with _ -> Unix._exit 127));
  close_fd input_read;
  close_fd output_write;
  close_fd errors;
  let input_open = ref true and eof = ref false and reaped = ref false
  and completed = ref false in
  let input_offset = ref 0 and status = ref None in
  let output = Buffer.create 256 and chunk = Bytes.create 4096 in
  let close_input () =
    if !input_open then (input_open := false; close_fd input_write) in
  let stop_child () =
    if not !completed then (
      (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ ->
         (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()));
      if not !reaped then (
        let rec reap () =
          try ignore (Unix.waitpid [] pid); reaped := true
          with
          | Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
          | Unix.Unix_error (Unix.ECHILD, _, _) -> reaped := true in
        reap ())) in
  Fun.protect ~finally:(fun () ->
    close_input ();
    close_fd output_read;
    stop_child ()) (fun () ->
    Unix.set_nonblock input_write;
    Unix.set_nonblock output_read;
    while not !eof || !status = None do
      check_deadline ~cancel ~deadline ~program;
      if !input_open && !input_offset = String.length stdin then close_input ();
      let reads = if !eof then [] else [output_read] in
      let writes = if !input_open then [input_write] else [] in
      let readable, writable, _ =
        try Unix.select reads writes []
          (min 0.05 (max 0. (deadline -. Unix.gettimeofday ())))
        with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], [] in
      if writable <> [] && !input_open then (
        try
          let count = Unix.write_substring input_write stdin !input_offset
            (String.length stdin - !input_offset) in
          if count > 0 then input_offset := !input_offset + count
        with
        | Unix.Unix_error ((Unix.EPIPE | Unix.EBADF), _, _) -> close_input ()
        | Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN), _, _) -> ());
      if readable <> [] then (
        let count =
          try Unix.read output_read chunk 0 (Bytes.length chunk)
          with Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN), _, _) -> -1 in
        if count = 0 then eof := true
        else if count > 0 then (
          if Buffer.length output + count > 65536 then
            fail "Google ADC token response exceeds 64 KiB";
          Buffer.add_subbytes output chunk 0 count));
      (match try Unix.waitpid [Unix.WNOHANG] pid
         with Unix.Unix_error (Unix.EINTR, _, _) -> 0, Unix.WEXITED 0 with
       | 0, _ -> ()
       | _, child_status ->
           status := Some child_status;
           reaped := true;
           close_input ())
    done;
    match !status with
    | Some (Unix.WEXITED 0) ->
        completed := true;
        Buffer.contents output
    | _ -> fail ("Google ADC " ^ Filename.basename program ^ " command failed"))

let form_component value =
  let buffer = Buffer.create (String.length value) in
  String.iter (fun byte ->
    match byte with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' | '.' | '~' ->
        Buffer.add_char buffer byte
    | _ -> Buffer.add_string buffer (Printf.sprintf "%%%02X" (Char.code byte)))
    value;
  Buffer.contents buffer

let authorized_user_form ~client_id ~client_secret ~refresh_token =
  String.concat "&" [
    "client_id=" ^ form_component client_id;
    "client_secret=" ^ form_component client_secret;
    "refresh_token=" ^ form_component refresh_token;
    "grant_type=refresh_token" ]

let parse_token_response response =
  let json = try Yojson.Basic.from_string response
    with Yojson.Json_error _ -> fail "invalid Google OAuth token response" in
  match Protocol.member "access_token" json,
        Protocol.member "token_type" json,
        Protocol.member "expires_in" json with
  | `String token, `String "Bearer", `Int expires_in
      when expires_in > 0 -> check_token token
  | _ -> fail "invalid or expired Google OAuth token response"

let authorized_user_curl_arguments =
  ["--disable"; "--silent"; "--show-error"; "--fail";
   "--max-time"; "15"; "--connect-timeout"; "5"; "--noproxy"; "*";
   "--proto"; "=https"; "--max-redirs"; "0";
   "--request"; "POST"; "--header"; "Content-Type: " ^ oauth_content_type;
   "--data-binary"; "@-"; oauth_token_endpoint]

let authorized_user_request ~client_id ~client_secret ~refresh_token =
  authorized_user_curl_arguments,
  authorized_user_form ~client_id ~client_secret ~refresh_token

let refresh_authorized_user ?cancel ~client_id ~client_secret ~refresh_token () =
  let arguments, body =
    authorized_user_request ~client_id ~client_secret ~refresh_token in
  let output =
    try execute ~stdin:body ?cancel ~timeout:20. "curl" arguments
    with Unix.Unix_error (error, operation, path) ->
      subprocess_unix_error "curl" (error, operation, path) in
  parse_token_response output
let base64url value =
  let alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" in
  let output = Buffer.create ((String.length value * 4 + 2) / 3) in
  let rec encode index =
    if index < String.length value then (
      let remaining = String.length value - index in
      let first = Char.code value.[index] in
      let second = if remaining > 1 then Char.code value.[index + 1] else 0 in
      let third = if remaining > 2 then Char.code value.[index + 2] else 0 in
      let bits = (first lsl 16) lor (second lsl 8) lor third in
      Buffer.add_char output alphabet.[(bits lsr 18) land 0x3f];
      Buffer.add_char output alphabet.[(bits lsr 12) land 0x3f];
      if remaining > 1 then
        Buffer.add_char output alphabet.[(bits lsr 6) land 0x3f];
      if remaining > 2 then Buffer.add_char output alphabet.[bits land 0x3f];
      encode (index + 3)) in
  encode 0;
  Buffer.contents output

let service_account_form ~assertion =
  String.concat "&" [
    "grant_type=" ^ form_component
      "urn:ietf:params:oauth:grant-type:jwt-bearer";
    "assertion=" ^ form_component assertion ]

let service_account_request ~assertion =
  authorized_user_curl_arguments, service_account_form ~assertion

let service_account_assertion ?cancel ~client_email ~private_key ~now () =
  if client_email = "" || String.length client_email > 1024 ||
     String.exists (fun character -> Char.code character < 33 ||
       Char.code character > 126) client_email ||
     private_key = "" || String.length private_key > 32768 ||
     now <= 0 || now > max_int - 3600 then
    fail "Google service-account credentials are invalid";
  let header = Yojson.Basic.to_string
    (`Assoc ["alg", `String "RS256"; "typ", `String "JWT"]) in
  let claims = Yojson.Basic.to_string (`Assoc [
    "iss", `String client_email;
    "scope", `String "https://www.googleapis.com/auth/cloud-platform";
    "aud", `String oauth_token_endpoint;
    "iat", `Int now;
    "exp", `Int (now + 3600) ]) in
  let signing_input = base64url header ^ "." ^ base64url claims in
  let private_key_path, private_key_output =
    Filename.open_temp_file "pave-google-service-account-" ".pem" in
  Fun.protect ~finally:(fun () ->
    close_out_noerr private_key_output;
    (try Sys.remove private_key_path with Sys_error _ -> ()))
    (fun () ->
      output_string private_key_output private_key;
      close_out private_key_output;
      Unix.chmod private_key_path 0o600;
      let signature =
        try execute ~stdin:signing_input ?cancel ~timeout:10. "openssl"
          ["dgst"; "-sha256"; "-sign"; private_key_path]
        with Unix.Unix_error (error, operation, path) ->
          subprocess_unix_error "openssl" (error, operation, path) in
      if signature = "" then fail "Google service-account JWT signing failed";
      signing_input ^ "." ^ base64url signature)

let refresh_service_account ?cancel ~client_email ~private_key () =
  let assertion = service_account_assertion ?cancel ~client_email ~private_key
    ~now:(int_of_float (Unix.gettimeofday ())) () in
  let arguments, body = service_account_request ~assertion in
  let output =
    try execute ~stdin:body ?cancel ~timeout:20. "curl" arguments
    with Unix.Unix_error (error, operation, path) ->
      subprocess_unix_error "curl" (error, operation, path) in
  parse_token_response output


let json_string name json =
  match Protocol.member name json with
  | `String value when value <> "" -> Some value
  | _ -> None

let read_credential_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    let length = in_channel_length channel in
    if length > 65536 then fail "Google ADC credential file exceeds 64 KiB";
    really_input_string channel length)

let validate_credential_json text =
  let json = try Yojson.Basic.from_string text
    with Yojson.Json_error _ -> fail "invalid Google ADC credential JSON" in
  let require keys =
    List.for_all (fun key -> Option.is_some (json_string key json)) keys in
  match Protocol.member "type" json with
  | `String "authorized_user"
    when require ["client_id"; "client_secret"; "refresh_token"] &&
      Protocol.member "service_account_impersonation_url" json <> `Null ->
      "impersonated_service_account"
  | `String "authorized_user" when require ["client_id"; "client_secret"; "refresh_token"] ->
      "authorized_user"
  | `String "service_account"
    when require ["client_email"; "private_key"; "token_uri"] &&
      json_string "token_uri" json = Some oauth_token_endpoint ->
      "service_account"
  | `String ("authorized_user" | "service_account") ->
      fail "Google ADC credential is missing required fields or uses an unpinned token host"
  | `String _ -> fail "unsupported Google ADC credential type"
  | _ -> fail "Google ADC credential is missing its type"

let validate_credential_file path =
  validate_credential_json (read_credential_file path)

let credential_file () =
  match Sys.getenv_opt "GOOGLE_APPLICATION_CREDENTIALS" with
  | Some path when path <> "" ->
      if not (Sys.file_exists path) || Sys.is_directory path then
        fail "GOOGLE_APPLICATION_CREDENTIALS must point to an ADC JSON file";
      Some path
  | _ ->
      let home = match Sys.getenv_opt "HOME" with Some value -> value | None -> "" in
      let directory = match Sys.getenv_opt "CLOUDSDK_CONFIG" with
        | Some value when value <> "" -> value
        | _ -> Filename.concat home ".config/gcloud" in
      if home = "" && Sys.getenv_opt "CLOUDSDK_CONFIG" = None then None else
      let path = Filename.concat directory "application_default_credentials.json" in
      if Sys.file_exists path && not (Sys.is_directory path) then Some path
      else None

let explicit_access_token () =
  let first name = match Sys.getenv_opt name with
    | Some value when value <> "" -> Some value | _ -> None in
  match first "GOOGLE_CLOUD_ACCESS_TOKEN" with
  | Some token -> Some (check_token token)
  | None -> Option.map check_token (first "CLOUDSDK_AUTH_ACCESS_TOKEN")

let access_token ?cancel () =
  match explicit_access_token () with
  | Some token -> token
  | None ->
      (match credential_file () with
       | Some path ->
          let text = read_credential_file path in
          let json = try Yojson.Basic.from_string text
            with Yojson.Json_error _ -> fail "invalid Google ADC credential JSON" in
          (match validate_credential_json text with
          | "authorized_user" ->
              let required name = match json_string name json with
                | Some value -> value
                | None -> fail "Google ADC credential is missing required fields" in
              refresh_authorized_user ?cancel ~client_id:(required "client_id")
                ~client_secret:(required "client_secret")
                ~refresh_token:(required "refresh_token") ()
          | "service_account" ->
              let required name = match json_string name json with
                | Some value -> value
                | None -> fail "Google ADC credential is missing required fields" in
              refresh_service_account ?cancel ~client_email:(required "client_email")
                ~private_key:(required "private_key") ()
          | "impersonated_service_account" ->
              let output =
                try execute ?cancel ~timeout:30. "gcloud"
                  ["auth"; "application-default"; "print-access-token"]
                with Unix.Unix_error (error, operation, path) ->
                  subprocess_unix_error "gcloud" (error, operation, path) in
              check_token (String.trim output)
          | _ -> fail "unsupported Google ADC credential type")
       | None ->
          let output =
            try execute ?cancel ~timeout:4. "curl" [
              "--disable"; "--silent"; "--show-error"; "--fail";
              "--max-time"; "2"; "--noproxy"; "*";
              "--proto"; "=http"; "--max-redirs"; "0";
              "--header"; "Metadata-Flavor: Google";
              "http://169.254.169.254/computeMetadata/v1/instance/service-accounts/default/token"
            ] with Unix.Unix_error (error, operation, path) ->
              subprocess_unix_error "curl" (error, operation, path) in
          let json = try Yojson.Basic.from_string output
            with Yojson.Json_error _ -> fail "invalid Google metadata token response" in
          (match Protocol.member "access_token" json,
                 Protocol.member "token_type" json with
           | `String token, (`Null | `String "Bearer") -> check_token token
           | _ -> fail "invalid Google metadata token response")
      )
