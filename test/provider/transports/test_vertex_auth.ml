module Vertex = Pave.Vertex_auth

let fail detail = failwith ("Vertex auth fixture: " ^ detail)

let write_file path contents =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output contents)

let read_file path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let contains text needle =
  let rec find index =
    if index + String.length needle > String.length text then false
    else if String.sub text index (String.length needle) = needle then true
    else find (index + 1) in
  needle = "" || find 0

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect ~finally:(fun () ->
    Unix.putenv name (Option.value previous ~default:"")) f

let hex_value = function
  | '0'..'9' as character -> Char.code character - Char.code '0'
  | 'a'..'f' as character -> Char.code character - Char.code 'a' + 10
  | 'A'..'F' as character -> Char.code character - Char.code 'A' + 10
  | _ -> fail "invalid percent encoding"

let form_decode value =
  let output = Buffer.create (String.length value) in
  let rec decode index =
    if index < String.length value then
      if value.[index] = '%' then (
        if index + 2 >= String.length value then fail "truncated form value";
        Buffer.add_char output (Char.chr
          ((hex_value value.[index + 1] lsl 4) lor hex_value value.[index + 2]));
        decode (index + 3))
      else (
        Buffer.add_char output (if value.[index] = '+' then ' ' else value.[index]);
        decode (index + 1)) in
  decode 0;
  Buffer.contents output

let base64url_decode value =
  let digit = function
    | 'A'..'Z' as c -> Char.code c - Char.code 'A'
    | 'a'..'z' as c -> Char.code c - Char.code 'a' + 26
    | '0'..'9' as c -> Char.code c - Char.code '0' + 52
    | '-' -> 62 | '_' -> 63
    | _ -> fail "invalid JWT base64url" in
  let output = Buffer.create (String.length value * 3 / 4) in
  let accumulator = ref 0 and bits = ref 0 in
  String.iter (fun character ->
    accumulator := (!accumulator lsl 6) lor digit character;
    bits := !bits + 6;
    if !bits >= 8 then (
      bits := !bits - 8;
      Buffer.add_char output
        (Char.chr ((!accumulator lsr !bits) land 0xff));
      accumulator := !accumulator land ((1 lsl !bits) - 1))) value;
  Buffer.contents output

let json = function
  | `Assoc _ as value -> value
  | _ -> fail "JWT component is not a JSON object"

let field name value = Pave.Protocol.member name value

let fake_tools directory =
  let openssl = Filename.concat directory "openssl" in
  let curl = Filename.concat directory "curl" in
  write_file openssl {|#!/bin/sh
[ "$#" -eq 4 ] || exit 21
[ "$1" = dgst ] && [ "$2" = -sha256 ] && [ "$3" = -sign ] || exit 22
[ "$(cat "$4")" = "fixture-private-key" ] || exit 23
printf '%s' "$4" > "$PAVE_VERTEX_TEMP_KEY_PATH"
cat > "$PAVE_VERTEX_SIGNING_INPUT"
printf 'fixture-signature'
|};
  write_file curl {|#!/bin/sh
[ "$1" = --disable ] || exit 31
last=
for argument in "$@"; do last=$argument; done
[ "$last" = https://oauth2.googleapis.com/token ] || exit 32
case "$*" in *fixture-private-key*) exit 33 ;; esac
body=$(cat)
printf '%s' "$body" > "$PAVE_VERTEX_TOKEN_BODY"
cat <<'JSON'
{"access_token":"fixture-service-token","token_type":"Bearer","expires_in":3600}
JSON
|};
  Unix.chmod openssl 0o700;
  Unix.chmod curl 0o700

let run_command program arguments =
  let pid = Unix.create_process program
    (Array.of_list (program :: arguments))
    Unix.stdin Unix.stdout Unix.stderr in
  match Unix.waitpid [] pid with
  | _, Unix.WEXITED 0 -> ()
  | _ -> fail (program ^ " failed")


let reaped_leader_pipe () =
  let directory = Filename.temp_file "pave-adc-pipe-" "" in
  Sys.remove directory; Unix.mkdir directory 0o700;
  let leader_file = Filename.concat directory "leader" in
  let release_file = Filename.concat directory "release" in
  Unix.mkfifo release_file 0o600;
  let releaser = Unix.fork () in
  if releaser = 0 then (
    let rec leader attempts =
      if attempts = 0 then Unix._exit 2;
      match int_of_string (String.trim (read_file leader_file)) with
      | pid -> pid
      | exception (Sys_error _ | Failure _) ->
          Unix.sleepf 0.01; leader (attempts - 1) in
    let pid = leader 1000 in
    let rec await_reaped attempts =
      if attempts = 0 then Unix._exit 3;
      match Unix.kill pid 0 with
      | () -> Unix.sleepf 0.01; await_reaped (attempts - 1)
      | exception Unix.Unix_error (Unix.ESRCH, _, _) -> () in
    await_reaped 1000;
    write_file release_file "release\n";
    Unix._exit 0);
  let reaped = ref false in
  Fun.protect ~finally:(fun () ->
    if not !reaped then (
      (try Unix.kill releaser Sys.sigkill with Unix.Unix_error _ -> ());
      (try ignore (Unix.waitpid [] releaser) with Unix.Unix_error _ -> ()));
    List.iter (fun path -> if Sys.file_exists path then Sys.remove path)
      [leader_file; release_file];
    Unix.rmdir directory) (fun () ->
    (* Release inherited stdout only after the direct leader has been reaped. *)
    let output = Vertex.execute ~timeout:10. "/bin/sh"
      ["-c"; "printf ready; (read token < \"$1\"; printf released) & printf '%s\\n' \"$$\" > \"$2\"";
       "adc-pipe"; release_file; leader_file] in
    assert (output = "readyreleased");
    let _, status = Unix.waitpid [] releaser in
    reaped := true;
    match status with
    | Unix.WEXITED 0 -> ()
    | _ -> fail "pipe releaser failed")

let () =
  reaped_leader_pipe ();
  let _, body = Vertex.service_account_request ~assertion:"a.b" in

  assert (body =
    "grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer&assertion=a.b");
  assert (Vertex.validate_credential_json
    {|{"type":"service_account","client_email":"agent@example.iam.gserviceaccount.com","private_key":"fixture-private-key","token_uri":"https://oauth2.googleapis.com/token"}|}
    = "service_account");
  let directory = Filename.temp_file "pave-vertex-service-account-" "" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  let credentials = Filename.concat directory "adc.json" in
  let temp_key = Filename.concat directory "temporary-key-path" in
  let signed_input = Filename.concat directory "signed-input" in
  let token_body = Filename.concat directory "token-body" in
  let cli = Filename.concat directory "gcloud" in
  let real_key = Filename.concat directory "real-key.pem" in
  let public_key = Filename.concat directory "real-public-key.pem" in
  let real_signature = Filename.concat directory "real-signature.bin" in
  Fun.protect ~finally:(fun () ->
    List.iter (fun path -> try Sys.remove path with Sys_error _ -> ())
      [credentials; temp_key; signed_input; token_body; cli; real_key;
       public_key; real_signature;
       Filename.concat directory "openssl"; Filename.concat directory "curl"];
    Unix.rmdir directory) (fun () ->
      fake_tools directory;
      write_file credentials
        {|{"type":"service_account","client_email":"agent@example.iam.gserviceaccount.com","private_key":"fixture-private-key","token_uri":"https://oauth2.googleapis.com/token"}|};
      let previous_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
      let environment = [
        "PATH", directory ^ ":" ^ previous_path;
        "GOOGLE_APPLICATION_CREDENTIALS", credentials;
        "GOOGLE_CLOUD_ACCESS_TOKEN", "";
        "CLOUDSDK_AUTH_ACCESS_TOKEN", "";
        "PAVE_VERTEX_TEMP_KEY_PATH", temp_key;
        "PAVE_VERTEX_SIGNING_INPUT", signed_input;
        "PAVE_VERTEX_TOKEN_BODY", token_body ] in
      let rec with_environment = function
        | [] ->
            Vertex.Test.use_curl_helper (Filename.concat directory "curl");
            assert (Vertex.access_token () = "fixture-service-token")
        | (name, value) :: rest -> with_env name value (fun () -> with_environment rest) in
      with_environment environment;
      assert (not (contains (read_file token_body) "fixture-private-key"));
      let signing_input = read_file signed_input in
      let jwt =
        match String.split_on_char '.' signing_input with
        | [_header; _claims] ->
            let body = read_file token_body in
            let prefix =
              "grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer&assertion=" in
            assert (String.starts_with ~prefix body);
            form_decode
              (String.sub body (String.length prefix)
                (String.length body - String.length prefix))
        | _ -> fail "service-account signer received malformed JWT input" in
      (match String.split_on_char '.' jwt with
       | [header; claims; signature] ->
           let header = json (Yojson.Basic.from_string (base64url_decode header)) in
           let claims = json (Yojson.Basic.from_string (base64url_decode claims)) in
           assert (field "alg" header = `String "RS256");
           assert (field "typ" header = `String "JWT");
           assert (field "iss" claims =
             `String "agent@example.iam.gserviceaccount.com");
           assert (field "aud" claims =
             `String "https://oauth2.googleapis.com/token");
           assert (field "scope" claims =
             `String "https://www.googleapis.com/auth/cloud-platform");
           (match field "iat" claims, field "exp" claims with
            | `Int issued, `Int expires -> assert (expires - issued = 3600)
            | _ -> fail "JWT lifetime claims are missing");
           assert (base64url_decode signature = "fixture-signature")
       | _ -> fail "service-account assertion is not a JWT");
      run_command "openssl" ["genrsa"; "-out"; real_key; "2048"];
      run_command "openssl" ["rsa"; "-in"; real_key; "-pubout"; "-out"; public_key];
      let real_jwt = Vertex.service_account_assertion
        ~client_email:"agent@example.iam.gserviceaccount.com"
        ~private_key:(read_file real_key) ~now:1_700_000_000 () in
      (match String.split_on_char '.' real_jwt with
       | [header; claims; signature] ->
           write_file signed_input (header ^ "." ^ claims);
           write_file real_signature (base64url_decode signature);
           run_command "openssl" ["dgst"; "-sha256"; "-verify"; public_key;
             "-signature"; real_signature; signed_input]
       | _ -> fail "real service-account assertion is not a JWT");
      assert (not (Sys.file_exists (read_file temp_key))));
  print_endline "Vertex service-account ADC JWT exchange: ok"
