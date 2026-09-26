(* Sakana's Models API lists the models accessible to the supplied API key,
   including billing-gated models. Its Responses API supports function tools
   and replayed function-call outputs; do not infer model families from IDs. *)
let sakana_models_url = "https://api.sakana.ai/v1/models"
let sakana_responses_endpoint = "https://api.sakana.ai/v1/responses"

type error =
  | Invalid_credential
  | Transport_error
  | Http_error of int
  | Invalid_response of string

type http = url:string -> headers:(string * string) list ->
  (int * string, error) result

let max_response_bytes = 1_048_576

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let default_http ?cancel ~url ~headers () =
  Provider.with_temp_file (fun path output ->
    close_out output;
    let option name value = name ^ " = " ^ Provider.quote_config value ^ "\n" in
    let config =
      "silent\n"
      ^ option "url" url
      ^ option "request" "GET"
      ^ option "output" path
      ^ option "write-out" "%{http_code}"
      ^ option "connect-timeout" "4"
      ^ option "max-time" "12"
      ^ option "max-filesize" (string_of_int max_response_bytes)
      ^ option "proto" "=https"
      ^ option "proxy" ""
      ^ option "max-redirs" "0"
      ^ String.concat "" (List.map (fun (name, value) ->
          option "header" (name ^ ": " ^ value)) headers) in
    let status = try Ok (Provider.run_curl ?cancel config)
      with Provider.Provider_error _ -> Error Transport_error in
    match status with
    | Error failure -> Error failure
    | Ok code ->
        let code = try int_of_string code with Failure _ -> 0 in
        if code < 100 || code > 599 then Error Transport_error
        else
          let input = open_in_bin path in
          Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
            let length = in_channel_length input in
            if length > max_response_bytes then
              Error (Invalid_response "listing exceeds size limit")
            else Ok (code, really_input_string input length)))

let parse_models body =
  let json = try Some (Yojson.Basic.from_string body)
    with Yojson.Json_error _ -> None in
  match json with
  | Some (`Assoc fields) ->
      (match List.assoc_opt "data" fields with
      | Some (`List rows) ->
          let seen = Hashtbl.create (List.length rows) in
          let models = ref [] in
          let duplicate = ref false in
          let valid = List.for_all (function
            | `Assoc fields -> (match List.assoc_opt "id" fields with
                | Some (`String id) when id <> "" && String.length id <= 256 &&
                    String.for_all (fun c -> Char.code c > 32 &&
                      Char.code c < 127) id ->
                    if Hashtbl.mem seen id then duplicate := true
                    else (
                      Hashtbl.add seen id ();
                      models := id :: !models);
                    true
                | _ -> false)
            | _ -> false) rows in
          if valid && not !duplicate then Ok (List.rev !models)
          else Error (Invalid_response "invalid model ID")
      | _ -> Error (Invalid_response "missing data array"))
  | _ -> Error (Invalid_response "malformed model listing")

let discover_sakana ?http ?cancel ~credential () =
  if not (valid_key credential) then Error Invalid_credential
  else
    let headers = ["Authorization", "Bearer " ^ credential] in
    let http = match http with
      | Some http -> http
      | None -> fun ~url ~headers -> default_http ?cancel ~url ~headers () in
    try
      Provider.check_cancel cancel;
      let response = http ~url:sakana_models_url ~headers in
      Provider.check_cancel cancel;
      match response with
      | Error failure -> Error failure
      | Ok (status, _) when status < 200 || status >= 300 ->
          Error (Http_error status)
      | Ok (_, body) when String.length body > max_response_bytes ->
          Error (Invalid_response "listing exceeds size limit")
      | Ok (_, body) -> parse_models body
    with
    | Provider.Cancelled -> raise Provider.Cancelled
    | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
        Error Transport_error
