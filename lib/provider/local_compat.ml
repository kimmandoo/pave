(* Local OpenAI-compatible servers use Chat Completions, never Responses.
   Model IDs come exclusively from each server's /v1/models response. *)
type engine = Lm_studio | Llama_cpp | Vllm

type error =
  | Invalid_endpoint
  | Invalid_credential
  | Transport_error
  | Http_error of int
  | Invalid_response of string

type http = url:string -> headers:(string * string) list ->
  (int * string, error) result

let engine = function
  | "lm-studio" -> Some Lm_studio
  | "llama.cpp" -> Some Llama_cpp
  | "vllm" -> Some Vllm
  | _ -> None

let default_base = function
  | Lm_studio -> "http://127.0.0.1:1234/v1"
  | Llama_cpp -> "http://127.0.0.1:8080"
  | Vllm -> "http://127.0.0.1:8000/v1"

let base_env = function
  | Lm_studio -> Some "LM_STUDIO_BASE_URL"
  | Llama_cpp -> Some "LLAMA_CPP_BASE_URL"
  | Vllm -> Some "VLLM_BASE_URL"

let configured_base engine =
  match base_env engine with
  | None -> default_base engine
  | Some name -> (match Sys.getenv_opt name with
      | Some value when value <> "" -> value
      | _ -> default_base engine)

let endpoint ?base_url ~provider () =
  let engine = match engine provider with
    | Some engine -> engine
    | None -> invalid_arg "unsupported local Chat Completions provider" in
  let base = Option.value ~default:(configured_base engine) base_url in
  let base = if String.ends_with ~suffix:"/" base then
    String.sub base 0 (String.length base - 1) else base in
  let base = if String.ends_with ~suffix:"/v1" base then base else base ^ "/v1" in
  Provider.local_endpoint (base ^ "/chat/completions")

let listing_url ~endpoint =
  let endpoint = Provider.local_endpoint endpoint in
  let suffix = "chat/completions" in
  String.sub endpoint 0 (String.length endpoint - String.length suffix) ^ "models"

let max_response_bytes = 1_048_576

let valid_key key = String.length key <= 8192 &&
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
      ^ option "proto" (if String.starts_with ~prefix:"http://" url
        then "=http" else "=https")
      ^ option "proxy" ""
      ^ option "noproxy" "*"
      ^ option "max-redirs" "0"
      ^ String.concat "" (List.map (fun (name, value) ->
          option "header" (name ^ ": " ^ value)) headers) in
    let status = try Ok (Provider.run_curl ?cancel config)
      with Provider.Provider_error _ -> Error Transport_error in
    match status with
    | Error _ as failure -> failure
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

let discover ?http ?cancel ~provider ~endpoint ?key () =
  if engine provider = None then Error Invalid_endpoint else
  let url = try Ok (listing_url ~endpoint)
    with Provider.Provider_error _ -> Error Invalid_endpoint in
  match url with
  | Error _ as failure -> failure
  | Ok url ->
      (match key with
      | Some key when not (valid_key key) -> Error Invalid_credential
      | _ ->
          let headers = match key with
            | Some key when key <> "" -> ["Authorization", "Bearer " ^ key]
            | _ -> [] in
          let http = match http with
            | Some http -> http
            | None -> fun ~url ~headers -> default_http ?cancel ~url ~headers () in
          try
            Provider.check_cancel cancel;
            let response = http ~url ~headers in
            Provider.check_cancel cancel;
            match response with
            | Error _ as failure -> failure
            | Ok (status, _) when status < 200 || status >= 300 -> Error (Http_error status)
            | Ok (_, body) ->
                if String.length body > max_response_bytes then
                  Error (Invalid_response "listing exceeds size limit")
                else
                  let json = try Some (Yojson.Basic.from_string body)
                    with Yojson.Json_error _ -> None in
                  (match json with
                  | Some (`Assoc fields) ->
                      (match List.assoc_opt "data" fields with
                      | Some (`List rows) ->
                          let seen = Hashtbl.create (List.length rows) in
                          let models = ref [] and duplicate = ref false in
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
                          if not valid then Error (Invalid_response "invalid model ID")
                          else if !duplicate then
                            Error (Invalid_response "duplicate model ID")
                          else Ok (List.rev !models)
                      | _ -> Error (Invalid_response "missing data array"))
                  | _ -> Error (Invalid_response "malformed model listing"))
          with
          | Provider.Cancelled -> raise Provider.Cancelled
          | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
              Error Transport_error)
