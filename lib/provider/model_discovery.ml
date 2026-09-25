(* Only the registered provider's fixed listing endpoint may receive a credential.
   In particular a GitHub device grant is never sent to a caller-selected host.
   Discovery does not cache: callers decide when to refresh (for example, on
   entering the model selector), never on each change to the search query. *)
type credential =
  | Api_key of string
  | Copilot_oauth of string
  | Codex_oauth of string * string

type error =
  | Unsupported_provider of string
  | Missing_credential
  | Invalid_credential
  | Transport_error of string
  | Http_error of int
  | Invalid_response of string

type http = url:string -> headers:(string * string) list ->
  (int * string, error) result

let message = function
  | Unsupported_provider id -> "Model discovery is unavailable for " ^ id
  | Missing_credential -> "Sign in or configure an API key to list models"
  | Invalid_credential -> "Invalid model discovery credential"
  | Transport_error detail -> "Model discovery connection failed: " ^ detail
  | Http_error 401 | Http_error 403 ->
      "Model discovery access denied; check the account or API key"
  | Http_error 429 -> "Model discovery rate limited; try again later"
  | Http_error code -> Printf.sprintf "Model discovery returned HTTP %d" code
  | Invalid_response detail -> "Invalid model listing response: " ^ detail

let openai_url = "https://api.openai.com/v1/models"
let google_url = "https://generativelanguage.googleapis.com/v1beta/models"
let ollama_url = "http://127.0.0.1:11434/api/tags"
let copilot_url = "https://api.githubcopilot.com/models"
let codex_urls = List.map (fun path ->
  "https://chatgpt.com/backend-api" ^ path ^ "?client_version=" ^
    Codex_wire.client_version) ["/codex/models"; "/models"]
let max_response_bytes = 1_048_576

let default_http ?cancel ~url ~headers () =
  (* This transport is private. The public callback is for isolated fixtures;
     the production callsites below never accept a URL or redirect from JSON. *)
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
      ^ option "proto" (if url = ollama_url then "=http" else "=https")
      ^ option "proxy" ""
      ^ String.concat "" (List.map (fun (name, value) ->
          option "header" (name ^ ": " ^ value)) headers) in
    let status = try Ok (Provider.run_curl ?cancel config)
      with Provider.Provider_error _ -> Error (Transport_error "request failed or timed out") in
    match status with
    | Error _ as failure -> failure
    | Ok code ->
        let code = try int_of_string code with Failure _ -> 0 in
        if code < 100 || code > 599 then Error (Transport_error "invalid HTTP status")
        else
          let input = open_in_bin path in
          Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
            let length = in_channel_length input in
            if length > max_response_bytes then
              Error (Invalid_response "listing exceeds 1 MiB")
            else Ok (code, really_input_string input length)))

let valid_secret secret =
  secret <> "" && String.length secret <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) secret

let extract_field field = function
  | `Assoc fields -> List.assoc_opt field fields
  | _ -> None

let invalid detail = Error (Invalid_response detail)

let listing field json = match extract_field field json with
  | Some (`List rows) -> Ok rows
  | _ -> invalid ("missing " ^ field ^ " array")

let checked_id value =
  if value <> "" && String.length value <= 256 &&
     String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) value
  then Ok value else invalid "invalid model ID"

let id_field field row = match extract_field field row with
  | Some (`String value) -> checked_id value
  | _ -> invalid ("missing " ^ field ^ " model ID")

let rec collect_rows ~id ~include_row rows = match rows with
  | [] -> Ok []
  | row :: rest ->
      (match id_field id row with
      | Error _ as error -> error
      | Ok name ->
          (match include_row row with
          | Error _ as error -> error
          | Ok include_it ->
              match collect_rows ~id ~include_row rest with
              | Error _ as error -> error
              | Ok names -> Ok (if include_it then name :: names else names)))

let include_all _ = Ok true

let include_gemini row =
  match extract_field "supportedGenerationMethods" row with
  | Some (`List methods) ->
      if List.for_all (function `String _ -> true | _ -> false) methods then
        Ok (List.mem (`String "generateContent") methods)
      else invalid "invalid supportedGenerationMethods"
  | _ -> invalid "missing supportedGenerationMethods"

let include_copilot row = match extract_field "capabilities" row with
  | None -> Ok true
  | Some (`Assoc fields) -> (match List.assoc_opt "type" fields with
      | Some (`String "chat") | None -> Ok true
      | Some (`String _) -> Ok false
      | _ -> invalid "invalid Copilot model capability")
  | _ -> invalid "invalid Copilot model capabilities"

let add_unique seen result ids =
  List.iter (fun id -> if not (Hashtbl.mem seen id) then (
    Hashtbl.add seen id (); result := id :: !result)) ids

let discover_codex ?http ?cancel credential =
  let auth = match credential with
    | None -> Error Missing_credential
    | Some (Codex_oauth (access, account)) ->
        if valid_secret access && valid_secret account then Ok (access, account)
        else Error Invalid_credential
    | Some _ -> Error Invalid_credential in
  match auth with
  | Error _ as failure -> failure
  | Ok (access, account) ->
      let headers = [
        "Authorization", "Bearer " ^ access;
        "chatgpt-account-id", account;
        "OpenAI-Beta", "responses=experimental";
        "originator", "pave";
        "version", Codex_wire.client_version;
        "Accept", "application/json" ] in
      let http = match http with
        | Some http -> http
        | None -> fun ~url ~headers -> default_http ?cancel ~url ~headers () in
      let rec request = function
        | [] -> Error (Http_error 404)
        | url :: rest ->
            Provider.check_cancel cancel;
            let response = http ~url ~headers in
            Provider.check_cancel cancel;
            match response with
            | Ok (404, _) when rest <> [] -> request rest
            | Error _ as error -> error
            | Ok (status, _) when status < 200 || status >= 300 ->
                Error (Http_error status)
            | Ok (_, body) ->
                if String.length body > max_response_bytes then
                  invalid "listing exceeds 1 MiB"
                else let json = try Some (Yojson.Basic.from_string body)
                  with Yojson.Json_error _ -> None in
                match json with
                | None -> invalid "malformed or truncated JSON"
                | Some json ->
                    let rows = match extract_field "models" json with
                      | Some (`List rows) -> Ok rows
                      | None -> listing "data" json
                      | _ -> invalid "invalid models array" in
                    (match rows with
                    | Error _ as error -> error
                    | Ok rows ->
                        let seen = Hashtbl.create (List.length rows) in
                        let result = ref [] in
                        let rec collect = function
                          | [] -> Ok (List.rev !result)
                          | row :: tail ->
                              let slug = match extract_field "slug" row with
                                | Some (`String _ as slug) -> Some slug
                                | None -> extract_field "id" row
                                | _ -> None in
                              (match slug with
                              | Some (`String name) ->
                                  (match checked_id name with
                                  | Error _ as error -> error
                                  | Ok name ->
                                      let hidden = match extract_field
                                        "visibility" row with
                                        | Some (`String value) ->
                                            List.mem (String.lowercase_ascii value)
                                              ["hide"; "hidden"]
                                        | _ -> false in
                                      if not hidden && not (Hashtbl.mem seen name)
                                      then (Hashtbl.add seen name ();
                                        result := name :: !result);
                                      collect tail)
                              | _ -> invalid "missing slug or id model ID") in
                        collect rows) in
      try request codex_urls with
      | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
          Error (Transport_error "request failed or timed out")
let discover ?http ?cancel ~provider ?credential () =
  if provider = "openai-codex" then discover_codex ?http ?cancel credential
  else
  let target = match provider with
    | "openai" -> Some (openai_url, "data", "id", include_all)
    | "google" -> Some (google_url, "models", "name", include_gemini)
    | "ollama" -> Some (ollama_url, "models", "name", include_all)
    | "github-copilot" -> Some (copilot_url, "data", "id", include_copilot)
    | _ -> None in
  match target with
  | None -> Error (Unsupported_provider provider)
  | Some (url, field, id, include_row) ->
      let secret = match provider, credential with
        | "ollama", None -> Ok None
        | "ollama", Some _ -> Error Invalid_credential
        | ("openai" | "google"), Some (Api_key key)
        | "github-copilot", Some (Copilot_oauth key) ->
            if valid_secret key then Ok (Some key) else Error Invalid_credential
        | _, None -> Error Missing_credential
        | _ -> Error Invalid_credential in
      (match secret with
      | Error _ as failure -> failure
      | Ok secret ->
          let headers = match provider, secret with
            | "openai", Some key -> ["Authorization", "Bearer " ^ key]
            | "google", Some key -> ["x-goog-api-key", key]
            | "github-copilot", Some key ->
                ["Authorization", "Bearer " ^ key;
                 "User-Agent", "copilot/1.0.82";
                 "Editor-Version", "copilot/1.0.82";
                 "Copilot-Integration-Id", "copilot-chat";
                 "Copilot-Harness-Id", "copilot-sdk"]
            | _ -> [] in
          let seen = Hashtbl.create 32 and result = ref [] in
          let visited = Hashtbl.create 8 in
          let http = match http with
            | Some http -> http
            | None -> fun ~url ~headers -> default_http ?cancel ~url ~headers () in
          let rec pages page token =
            Provider.check_cancel cancel;
            if page >= 50 then invalid "too many model listing pages"
            else let page_url = match token with
              | None -> url
              | Some token -> url ^ "?pageToken=" ^ Oauth_flow.url_encode token in
            let response = http ~url:page_url ~headers in
            Provider.check_cancel cancel;
            match response with
            | Error _ as failure -> failure
            | Ok (status, _) when status < 200 || status >= 300 ->
                Error (Http_error status)
            | Ok (_, body) ->
                if String.length body > max_response_bytes then
                  invalid "listing exceeds 1 MiB"
                else
                  let json = try Some (Yojson.Basic.from_string body)
                    with Yojson.Json_error _ -> None in
                  match json with
                  | None -> invalid "malformed or truncated JSON"
                  | Some json ->
                      (match listing field json with
                      | Error _ as failure -> failure
                      | Ok rows ->
                          match collect_rows ~id ~include_row rows with
                          | Error _ as failure -> failure
                          | Ok ids ->
                              add_unique seen result ids;
                              if provider <> "google" then Ok (List.rev !result)
                              else match extract_field "nextPageToken" json with
                              | None -> Ok (List.rev !result)
                              | Some (`String next) when next <> "" &&
                                  String.length next <= 4096 &&
                                  not (Hashtbl.mem visited next) &&
                                  String.for_all (fun c -> Char.code c >= 32 && Char.code c < 127) next ->
                                  Hashtbl.add visited next ();
                                  pages (page + 1) (Some next)
                              | _ -> invalid "invalid or repeated nextPageToken") in
          try pages 0 None with
          | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
              Error (Transport_error "request failed or timed out"))
