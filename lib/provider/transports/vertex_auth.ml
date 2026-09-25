exception Authentication_error of string

let fail message = raise (Authentication_error message)

let valid_token token =
  token <> "" && String.length token <= 16384 &&
  String.for_all (fun c -> Char.code c >= 33 && Char.code c <= 126) token

let check_token token =
  if not (valid_token token) then fail "Google ADC returned an invalid access token";
  token

(* No shell is involved, and neither the destination nor the requested OAuth
   scope can be supplied by a model/provider configuration. A local gcloud ADC
   installation manages all supported ADC JSON formats, including federated
   and impersonated credentials; the metadata source works without gcloud. *)
let execute ~timeout program arguments =
  let input = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
  let errors = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
  let output_read, output_write = Unix.pipe () in
  let pid =
    try
      Unix.set_close_on_exec output_read;
      Unix.create_process program (Array.of_list (program :: arguments))
        input output_write errors
    with exn ->
      List.iter Unix.close [input; errors; output_read; output_write];
      raise exn in
  Unix.close input;
  Unix.close errors;
  Unix.close output_write;
  let reaped = ref false in
  Fun.protect ~finally:(fun () ->
    Unix.close output_read;
    if not !reaped then (
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
      ignore (Unix.waitpid [] pid))) (fun () ->
    let deadline = Unix.gettimeofday () +. timeout in
    let buffer = Buffer.create 256 in
    let chunk = Bytes.create 4096 in
    let rec collect () =
      let remaining = deadline -. Unix.gettimeofday () in
      if remaining <= 0. then fail "Google ADC token command timed out";
      let ready, _, _ = Unix.select [output_read] [] [] remaining in
      if ready = [] then fail "Google ADC token command timed out";
      let count = Unix.read output_read chunk 0 (Bytes.length chunk) in
      if count <> 0 then (
        if Buffer.length buffer + count > 65536 then
          fail "Google ADC token response exceeds 64 KiB";
        Buffer.add_subbytes buffer chunk 0 count;
        collect ()) in
    collect ();
    let _, status = Unix.waitpid [] pid in
    reaped := true;
    match status with
    | Unix.WEXITED 0 -> Buffer.contents buffer
    | _ -> fail "Google ADC token command failed")

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

let access_token () =
  match explicit_access_token () with
  | Some token -> token
  | None ->
      match credential_file () with
      | Some _ ->
          let output =
            try execute ~timeout:30. "gcloud"
              ["auth"; "application-default"; "print-access-token"]
            with Unix.Unix_error _ ->
              fail "Google Cloud SDK is required to read local ADC credentials" in
          check_token (String.trim output)
      | None ->
          let output =
            try execute ~timeout:4. "curl" [
              "--disable"; "--silent"; "--show-error"; "--fail";
              "--max-time"; "2"; "--noproxy"; "*";
              "--proto"; "=http"; "--max-redirs"; "0";
              "--header"; "Metadata-Flavor: Google";
              "http://169.254.169.254/computeMetadata/v1/instance/service-accounts/default/token"
            ] with Unix.Unix_error _ -> fail "Google ADC metadata token unavailable" in
          let json = try Yojson.Basic.from_string output
            with Yojson.Json_error _ -> fail "invalid Google metadata token response" in
          (match Protocol.member "access_token" json,
                 Protocol.member "token_type" json with
           | `String token, (`Null | `String "Bearer") -> check_token token
           | _ -> fail "invalid Google metadata token response")
