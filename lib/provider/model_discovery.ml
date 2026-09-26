(* Only the registered provider's fixed listing endpoint may receive a credential.
   In particular a GitHub device grant is never sent to a caller-selected host.
   Discovery does not cache: callers decide when to refresh (for example, on
   entering the model selector), never on each change to the search query. *)
type credential =
  | Api_key of string
  | OAuth of { service : string; access : string; account_id : string option }

type error =
  | Unsupported_provider of string
  | Unsupported_route of string * string
  | Missing_credential
  | Invalid_credential
  | Transport_error of string
  | Credential_error of string
  | Http_error of int
  | Invalid_response of string

type raw_model = {
  id : string;
  display_name : string option;
  capabilities : Model_catalog.capabilities;
}

type model = Model_catalog.model
type source = Model_catalog.provenance

type listing = {
  models : model list;
  source : source;
}

let model_ids listing =
  List.map (fun model -> model.Model_catalog.identity.Model_identity.upstream_id)
    listing.models
let model_supports_endpoint ~provider (model : model) ~endpoint =
  if provider <> model.identity.provider then false else
  let route = match Provider_catalog.find provider with
    | None -> None
    | Some descriptor ->
        descriptor.routes
        |> List.find_opt (fun (route : Provider_catalog.route) ->
          route.endpoint = endpoint) in
  match route with
  | None -> false
  | Some route ->
      let advertised_endpoint = match provider, endpoint with
        | "commandcode", value when value = Commandcode_api.chat_url ->
            "/chat/completions"
        | "commandcode", value when value = Commandcode_api.messages_url ->
            "/messages"
        | "commandcode", value when value = Commandcode_api.responses_url ->
            "/responses"
        | _ -> endpoint in
      (match model.capabilities.supported_endpoints with
       | None -> route.name = model.identity.route
       | Some endpoints ->
           List.mem route.name endpoints ||
           List.mem route.endpoint endpoints ||
           List.mem advertised_endpoint endpoints)

type http = url:string -> headers:(string * string) list ->
  (int * string, error) result

let message = function
  | Unsupported_provider id -> "Model discovery is unavailable for " ^ id
  | Unsupported_route (provider, route) ->
      Printf.sprintf "Choose a registered API route for %s (not %s)" provider route
  | Missing_credential -> "Sign in or configure an API key to list models"
  | Invalid_credential -> "Invalid model discovery credential"
  | Credential_error detail ->
      "Model discovery credential error: " ^ detail
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
let openrouter_url = "https://openrouter.ai/api/v1/models/user"
let anthropic_url = "https://api.anthropic.com/v1/models"
let deepseek_url = "https://api.deepseek.com/models"

let groq_url = "https://api.groq.com/openai/v1/models"
let mistral_url = "https://api.mistral.ai/v1/models"
let together_url = "https://api.together.ai/v1/models"
let cerebras_url = "https://api.cerebras.ai/v1/models"
let venice_url = "https://api.venice.ai/api/v1/models?type=text"
let deepinfra_url = "https://api.deepinfra.com/v1/openai/models?filter=with_meta"
let fireworks_url = "https://api.fireworks.ai/v1/accounts/fireworks/models"
let baseten_url = "https://inference.baseten.co/v1/models"
let huggingface_url = "https://router.huggingface.co/v1/models"
let nanogpt_url = "https://api.nano-gpt.com/api/v1/models?detailed=true"
let abliteration_url = "https://api.abliteration.ai/v1/models"
let gmi_cloud_url = "https://api.gmi-serving.com/v1/models"
let moonshot_url = "https://api.moonshot.ai/v1/models"
let codex_urls = List.map (fun path ->
  "https://chatgpt.com/backend-api" ^ path ^ "?client_version=" ^
    Codex_wire.client_version) ["/codex/models"; "/models"]
let max_response_bytes = 1_048_576
let response_limit url =
  if url = openrouter_url || url = huggingface_url || url = nanogpt_url ||
     List.exists (fun (spec : Tool_gateways.spec) ->
       spec.models_url = url && spec.max_response_bytes > max_response_bytes)
       Tool_gateways.all then
    4 * max_response_bytes
  else max_response_bytes

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
      ^ option "max-filesize" (string_of_int (response_limit url))
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
            if length > response_limit url then
              Error (Invalid_response "listing exceeds size limit")
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

let positive_integer_field field row = match extract_field field row with
  | Some (`Int value) when value > 0 -> Some value
  | _ -> None

let capability_supported = function
  | Some (`Assoc fields) ->
      (match List.assoc_opt "supported" fields with
       | Some (`Bool value) -> Some value
       | _ -> None)
  | _ -> None

let anthropic_compaction_supported row =
  match extract_field "capabilities" row with
  | Some capabilities ->
      (match capability_supported (extract_field "compaction" capabilities) with
       | Some false -> Some false
       | Some true ->
           (match extract_field "summarize"
               (Option.value ~default:`Null
                 (extract_field "compaction" capabilities)) with
            | Some (`Assoc _ as summarize) ->
                capability_supported (Some summarize)
            | _ -> None)
       | None -> None)
  | None -> None

let collect_rows ~seen ~provider ~id ~include_row rows =
  let rec collect = function
    | [] -> Ok []
    | row :: rest ->
        (match id_field id row with
        | Error _ as error -> error
        | Ok name ->
            if Hashtbl.mem seen name then invalid "duplicate model ID"
            else (
              Hashtbl.add seen name ();
              match include_row row with
              | Error _ as error -> error
              | Ok include_it ->
                  (match collect rest with
                  | Error _ as error -> error
                  | Ok models ->
                      if not include_it then Ok models
                      else
                        let context_window_tokens =
                          if provider = "google" then
                            positive_integer_field "inputTokenLimit" row
                          else if provider = "anthropic" then
                            positive_integer_field "max_input_tokens" row
                          else None in
                        let native_compaction_supported =
                          if provider = "anthropic" then
                            anthropic_compaction_supported row
                          else None in
                        let display_name = if provider = "anthropic" then
                          match extract_field "display_name" row with
                          | Some (`String value) when value <> "" &&
                              String.length value <= 256 &&
                              not (String.exists (fun c ->
                                Char.code c < 32 || Char.code c = 127) value) ->
                              Some value
                          | _ -> None
                        else None in
                        let tools = match provider with
                          | "venice" ->
                              Option.bind (extract_field "model_spec" row)
                                (fun spec ->
                                  Option.bind (extract_field "capabilities" spec)
                                    (fun capabilities ->
                                      match extract_field "supportsFunctionCalling"
                                          capabilities with
                                      | Some (`Bool value) -> Some value
                                      | _ -> None))
                          | "fireworks" | "baseten" | "huggingface" | "nanogpt" ->
                              Some true
                          | _ -> None in
                        let capabilities = { Model_catalog.empty_capabilities with
                          context_window_tokens; native_compaction_supported; tools } in
                        Ok ({ id = name; display_name; capabilities } :: models))))
  in
  collect rows

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

let include_together row = match extract_field "type" row with
  | Some (`String "chat") -> Ok true
  | Some (`String _) -> Ok false
  | _ -> invalid "missing or invalid Together model type"

let include_venice row = match extract_field "type" row with
  | Some (`String "text") ->
      (match extract_field "model_spec" row with
      | Some spec ->
          (match extract_field "capabilities" spec with
          | Some capabilities ->
              (match extract_field "supportsFunctionCalling" capabilities with
              | Some (`Bool supported) -> Ok supported
              | _ -> invalid "missing Venice function calling capability")
          | _ -> invalid "missing Venice model capabilities")
      | _ -> invalid "missing Venice model specification")
  | Some (`String _) -> Ok false
  | _ -> invalid "missing or invalid Venice model type"

let include_deepinfra row = match extract_field "metadata" row with
  | Some (`Assoc _ as metadata) ->
      (match extract_field "tags" metadata with
      | Some (`List tags) ->
          Ok (List.mem (`String "chat") tags)
      | _ -> invalid "missing DeepInfra model tags")
  | _ -> invalid "missing DeepInfra model metadata"

let include_baseten row = match extract_field "supported_features" row with
  | Some (`List features) -> Ok (List.mem (`String "tools") features)
  | _ -> invalid "missing Baseten supported features"

let include_huggingface row = match extract_field "providers" row with
  | Some (`List providers) ->
      let supported provider =
        extract_field "status" provider = Some (`String "live") &&
        extract_field "supports_tools" provider = Some (`Bool true) in
      Ok (List.exists supported providers)
  | _ -> invalid "missing Hugging Face provider capabilities"

let include_nanogpt row = match extract_field "capabilities" row with
  | Some (`Assoc capabilities) ->
      (match List.assoc_opt "tool_calling" capabilities with
      | Some (`Bool enabled) -> Ok enabled
      | _ -> invalid "missing NanoGPT tool_calling capability")
  | _ -> invalid "missing NanoGPT model capabilities"

let include_fireworks row =
  let available = extract_field "supportsServerless" row = Some (`Bool true)
    && extract_field "supportsTools" row = Some (`Bool true)
    && (match extract_field "state" row with
       | None | Some (`String "READY") -> true
       | _ -> false) in
  match extract_field "name" row with
  | Some (`String name) ->
      Ok (available &&
        String.starts_with ~prefix:"accounts/fireworks/models/" name)
  | _ -> invalid "missing Fireworks model resource name"
let rec add_unique seen result = function
  | [] -> true
  | (model : raw_model) :: rest ->
      if Hashtbl.mem seen model.id then false
      else (
        Hashtbl.add seen model.id ();
        result := model :: !result;
        add_unique seen result rest)

let discover_codex_models ?http ?cancel credential =
  let auth = match credential with
    | None -> Error Missing_credential
    | Some (OAuth { service = "openai-codex"; access; account_id = Some account })
      when valid_secret access && valid_secret account -> Ok (access, account)
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
                                      if Hashtbl.mem seen name then
                                        invalid "duplicate model ID"
                                      else (
                                        Hashtbl.add seen name ();
                                        if hidden then collect tail
                                        else (
                                          let context_window_tokens =
                                            positive_integer_field
                                              "context_window" row in
                                          let capabilities =
                                            { Model_catalog.empty_capabilities with
                                              context_window_tokens } in
                                          result := { id = name; display_name = None;
                                            capabilities } :: !result;
                                          collect tail)))
                              | _ -> invalid "missing slug or id model ID") in
                        collect rows) in
      try request codex_urls with
      | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
          Error (Transport_error "request failed or timed out")
let discover_local ?http ?cancel ~provider credential =
  let key = match credential with
    | None -> Ok None
    | Some (Api_key key) -> Ok (Some key)
    | Some _ -> Error Invalid_credential in
  match key with
  | Error _ as failure -> failure
  | Ok key ->
      let endpoint = try Ok (Local_compat.endpoint ~provider ())
        with Provider.Provider_error _ -> Error
          (Invalid_response "invalid local provider endpoint") in
      (match endpoint with
      | Error _ as failure -> failure
      | Ok endpoint ->
          let http = Option.map (fun http ~url ~headers ->
            match http ~url ~headers with
            | Ok _ as response -> response
            | Error failure -> Error (match failure with
                | Invalid_credential -> Local_compat.Invalid_credential
                | Http_error code -> Local_compat.Http_error code
                | Invalid_response detail -> Local_compat.Invalid_response detail
                | _ -> Local_compat.Transport_error)) http in
          match Local_compat.discover ?http ?cancel ~provider ~endpoint ?key () with
          | Ok _ as models -> models
          | Error failure -> Error (match failure with
              | Local_compat.Invalid_endpoint ->
                  Invalid_response "invalid local provider endpoint"
              | Local_compat.Invalid_credential -> Invalid_credential
              | Local_compat.Transport_error -> Transport_error
                  "request failed or timed out"
              | Local_compat.Http_error code -> Http_error code
              | Local_compat.Invalid_response detail -> Invalid_response detail))
let discover_bedrock ?http ?cancel credential =
  match credential with
  | Some _ -> Error Invalid_credential
  | None ->
      (try
        Provider.check_cancel cancel;
        let region = Aws_auth.region () in
        let keys = Aws_auth.resolve () in
        let target = Bedrock_wire.discovery_endpoint ~region () in
        let signed = Aws_auth.sign ~content_type:false ~credentials:keys
          ~region ~amz_date:(Aws_auth.amz_date ()) ~method_:"GET"
          ~host:target.host ~path:target.path ~body:"" () in
        let http = match http with
          | Some http -> http
          | None -> fun ~url ~headers ->
              default_http ?cancel ~url ~headers () in
        let response = http ~url:target.url ~headers:signed in
        Provider.check_cancel cancel;
        match response with
        | Error failure -> Error failure
        | Ok (status, _) when status < 200 || status >= 300 ->
            Error (Http_error status)
        | Ok (_, body) when String.length body > max_response_bytes ->
            Error (Invalid_response "listing exceeds size limit")
        | Ok (_, body) ->
            let json = Yojson.Basic.from_string body in
            (try Ok (Bedrock_wire.parse_models json)
             with Protocol.Invalid_response reason ->
               Error (Invalid_response reason))
       with
       | Invalid_argument reason -> Error (Invalid_response reason)
       | Yojson.Json_error _ -> Error (Invalid_response "malformed model listing JSON")
       | Provider.Cancelled -> raise Provider.Cancelled
       | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
           Error (Transport_error "request failed or timed out"))

let discover_mantle ?http ?cancel credential =
  let key = match credential with
    | Some (Api_key key) when valid_secret key -> Ok key
    | Some _ -> Error Invalid_credential
    | None -> Error Missing_credential in
  match key with
  | Error _ as failure -> failure
  | Ok key ->
      (try
        let target = Bedrock_mantle.discovery_endpoint
          ~region:(Bedrock_mantle.region ()) () in
        let http = match http with
          | Some http -> http
          | None -> fun ~url ~headers ->
              default_http ?cancel ~url ~headers () in
        Provider.check_cancel cancel;
        let response = http ~url:target.url
          ~headers:["Authorization", "Bearer " ^ key] in
        Provider.check_cancel cancel;
        match response with
        | Error failure -> Error failure
        | Ok (status, _) when status < 200 || status >= 300 ->
            Error (Http_error status)
        | Ok (_, body) when String.length body > max_response_bytes ->
            Error (Invalid_response "listing exceeds size limit")
        | Ok (_, body) ->
            (try Ok (Bedrock_mantle.parse_models (Yojson.Basic.from_string body))
             with Yojson.Json_error _ ->
               Error (Invalid_response "malformed model listing JSON")
                | Protocol.Invalid_response reason ->
               Error (Invalid_response reason))
       with
       | Invalid_argument reason -> Error (Invalid_response reason)
       | Provider.Cancelled -> raise Provider.Cancelled
       | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
           Error (Transport_error "request failed or timed out"))

let discover_ollama_cloud ?http ?cancel credential =
  match credential with
  | None -> Error Missing_credential
  | Some (Api_key key) ->
      let http = Option.map (fun http ~url ~headers ->
        match http ~url ~headers with
        | Ok response -> Ok response
        | Error Invalid_credential -> Error Ollama_cloud.Invalid_credential
        | Error (Http_error status) -> Error (Ollama_cloud.Http_error status)
        | Error (Invalid_response reason) ->
            Error (Ollama_cloud.Invalid_response reason)
        | Error _ -> Error Ollama_cloud.Transport_error) http in
      (match Ollama_cloud.discover ?http ?cancel ~api_key:key () with
      | Ok ids -> Ok ids
      | Error Ollama_cloud.Invalid_credential -> Error Invalid_credential
      | Error Ollama_cloud.Transport_error ->
          Error (Transport_error "request failed or timed out")
      | Error (Ollama_cloud.Http_error status) -> Error (Http_error status)
      | Error (Ollama_cloud.Invalid_response reason) ->
          Error (Invalid_response reason))
  | Some _ -> Error Invalid_credential

let discover_xai ?http ?cancel credential =
  match credential with
  | None -> Error Missing_credential
  | Some (Api_key key) ->
      let source = match http with
        | Some callback -> callback
        | None -> fun ~url ~headers -> default_http ?cancel ~url ~headers () in
      let http ~url ~headers = match source ~url ~headers with
        | Ok response -> Ok response
        | Error Invalid_credential -> Error Xai_api.Invalid_credential
        | Error (Http_error status) -> Error (Xai_api.Http_error status)
        | Error (Invalid_response reason) ->
            Error (Xai_api.Invalid_response reason)
        | Error _ -> Error Xai_api.Transport_error in
      (try
        Provider.check_cancel cancel;
        let result = Xai_api.discover ~http ~api_key:key () in
        Provider.check_cancel cancel;
        match result with
        | Ok ids -> Ok ids
        | Error Xai_api.Invalid_credential -> Error Invalid_credential
        | Error Xai_api.Transport_error ->
            Error (Transport_error "request failed or timed out")
        | Error (Xai_api.Http_error status) -> Error (Http_error status)
        | Error (Xai_api.Invalid_response reason) ->
            Error (Invalid_response reason)
       with
       | Provider.Cancelled -> raise Provider.Cancelled
       | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
           Error (Transport_error "request failed or timed out"))
  | Some _ -> Error Invalid_credential

let discover_nvidia ?http ?cancel credential =
  match credential with
  | None -> Error Missing_credential
  | Some (Api_key key) ->
      let source = match http with
        | Some callback -> callback
        | None -> fun ~url ~headers -> default_http ?cancel ~url ~headers () in
      let http ~url ~headers = match source ~url ~headers with
        | Ok response -> Ok response
        | Error Invalid_credential -> Error Nvidia_api.Invalid_credential
        | Error (Http_error status) -> Error (Nvidia_api.Http_error status)
        | Error (Invalid_response reason) ->
            Error (Nvidia_api.Invalid_response reason)
        | Error _ -> Error Nvidia_api.Transport_error in
      (try
        Provider.check_cancel cancel;
        let result = Nvidia_api.discover ~http ~api_key:key () in
        Provider.check_cancel cancel;
        match result with
        | Ok ids -> Ok ids
        | Error Nvidia_api.Invalid_credential -> Error Invalid_credential
        | Error Nvidia_api.Transport_error ->
            Error (Transport_error "request failed or timed out")
        | Error (Nvidia_api.Http_error status) -> Error (Http_error status)
        | Error (Nvidia_api.Invalid_response reason) ->
            Error (Invalid_response reason)
       with
       | Provider.Cancelled -> raise Provider.Cancelled
       | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
           Error (Transport_error "request failed or timed out"))
  | Some _ -> Error Invalid_credential

let discover_stepfun ?http ?cancel credential =
  match credential with
  | None -> Error Missing_credential
  | Some (Api_key key) ->
      let source = match http with
        | Some callback -> callback
        | None -> fun ~url ~headers -> default_http ?cancel ~url ~headers () in
      let http ~url ~headers = match source ~url ~headers with
        | Ok response -> Ok response
        | Error Invalid_credential -> Error Stepfun_api.Invalid_credential
        | Error (Http_error status) -> Error (Stepfun_api.Http_error status)
        | Error (Invalid_response reason) ->
            Error (Stepfun_api.Invalid_response reason)
        | Error _ -> Error Stepfun_api.Transport_error in
      (try
        Provider.check_cancel cancel;
        let result = Stepfun_api.discover ~http ~api_key:key () in
        Provider.check_cancel cancel;
        match result with
        | Ok ids -> Ok ids
        | Error Stepfun_api.Invalid_credential -> Error Invalid_credential
        | Error Stepfun_api.Transport_error ->
            Error (Transport_error "request failed or timed out")
        | Error (Stepfun_api.Http_error status) -> Error (Http_error status)
        | Error (Stepfun_api.Invalid_response reason) ->
            Error (Invalid_response reason)
       with
       | Provider.Cancelled -> raise Provider.Cancelled
       | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
           Error (Transport_error "request failed or timed out"))
  | Some _ -> Error Invalid_credential

let discover_synthetic ?http ?cancel credential =
  match credential with
  | None -> Error Missing_credential
  | Some (Api_key key) ->
      let source = match http with
        | Some callback -> callback
        | None -> fun ~url ~headers -> default_http ?cancel ~url ~headers () in
      let http ~url ~headers = match source ~url ~headers with
        | Ok response -> Ok response
        | Error Invalid_credential -> Error Synthetic_api.Invalid_credential
        | Error (Http_error status) -> Error (Synthetic_api.Http_error status)
        | Error (Invalid_response reason) ->
            Error (Synthetic_api.Invalid_response reason)
        | Error _ -> Error Synthetic_api.Transport_error in
      (try
        Provider.check_cancel cancel;
        let result = Synthetic_api.discover ~http ~api_key:key () in
        Provider.check_cancel cancel;
        match result with
        | Ok ids -> Ok ids
        | Error Synthetic_api.Invalid_credential -> Error Invalid_credential
        | Error Synthetic_api.Transport_error ->
            Error (Transport_error "request failed or timed out")
        | Error (Synthetic_api.Http_error status) -> Error (Http_error status)
        | Error (Synthetic_api.Invalid_response reason) ->
            Error (Invalid_response reason)
       with
       | Provider.Cancelled -> raise Provider.Cancelled
       | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
           Error (Transport_error "request failed or timed out"))
  | Some _ -> Error Invalid_credential

(* Vendor adapters own their pinned URL, key policy, response schema and
   model-kind filter. This bridge translates shared HTTP/cancel errors while
   preserving the adapter's model payload. *)
let run_native_discovery ?http ?cancel ?(public = false) ?oauth_service credential
    ~discover ~map_http_error ~map_api_error =
  let api_key = match credential with
    | None when public -> Ok ""
    | None -> Error Missing_credential
    | Some (Api_key key) -> Ok key
    | Some (OAuth { service; access; _ })
      when Some service = oauth_service -> Ok access
    | Some _ -> Error Invalid_credential in
  match api_key with
  | Error _ as failure -> failure
  | Ok key ->
      let source = match http with
        | Some callback -> callback
        | None -> fun ~url ~headers -> default_http ?cancel ~url ~headers () in
      let http ~url ~headers = match source ~url ~headers with
        | Ok response -> Ok response
        | Error failure -> Error (map_http_error failure) in
      (try
         Provider.check_cancel cancel;
         let result = discover ~http ~api_key:key () in
         Provider.check_cancel cancel;
         match result with
         | Ok models -> Ok models
         | Error failure -> Error (map_api_error failure)
       with
       | Provider.Cancelled -> raise Provider.Cancelled
       | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
           Error (Transport_error "request failed or timed out"))

module Native_discovery (M : sig
  type error =
    | Invalid_credential
    | Transport_error
    | Http_error of int
    | Invalid_response of string
  val discover :
    http:(url:string -> headers:(string * string) list ->
      (int * string, error) result) ->
    api_key:string -> unit -> (string list, error) result
end) = struct
  let map_http_error = function
    | Invalid_credential -> M.Invalid_credential
    | Http_error status -> M.Http_error status
    | Invalid_response reason -> M.Invalid_response reason
    | Unsupported_provider _ | Unsupported_route _ | Missing_credential
    | Credential_error _ | Transport_error _ -> M.Transport_error

  let map_api_error = function
    | M.Invalid_credential -> Invalid_credential
    | M.Transport_error -> Transport_error "request failed or timed out"
    | M.Http_error status -> Http_error status
    | M.Invalid_response reason -> Invalid_response reason

  let discover ?http ?cancel ?(public = false) ?oauth_service credential =
    run_native_discovery ?http ?cancel ~public ?oauth_service credential
      ~discover:M.discover ~map_http_error ~map_api_error
end

module Zenmux_discovery = Native_discovery (Zenmux_api)
module Wafer_discovery = Native_discovery (Wafer_api)
module Qianfan_discovery = Native_discovery (Qianfan_api)
module Xiaomi_discovery = Native_discovery (Xiaomi_api)
module Kilo_discovery = Native_discovery (Kilo_api)
module Singularity_dev_discovery = Native_discovery (Singularity_dev_api)
module Opencode_zen_discovery = Native_discovery (Opencode_zen_api)
module Opencode_go_discovery = Native_discovery (Opencode_go_api)
module Yolo_auto_discovery = Native_discovery (Yolo_auto_api)
module Meta_discovery = Native_discovery (Meta_api)
module Vercel_ai_gateway_discovery = Native_discovery (Vercel_ai_gateway_api)

module Commandcode_listing_discovery = struct
  let map_http_error = function
    | Invalid_credential -> Commandcode_api.Invalid_credential
    | Http_error status -> Commandcode_api.Http_error status
    | Invalid_response reason -> Commandcode_api.Invalid_response reason
    | Unsupported_provider _ | Unsupported_route _ | Missing_credential
    | Credential_error _ | Transport_error _ -> Commandcode_api.Transport_error

  let map_api_error = function
    | Commandcode_api.Invalid_credential -> Invalid_credential
    | Commandcode_api.Transport_error ->
        Transport_error "request failed or timed out"
    | Commandcode_api.Http_error status -> Http_error status
    | Commandcode_api.Invalid_response reason -> Invalid_response reason

  let discover ?http ?cancel credential =
    run_native_discovery ?http ?cancel credential
      ~discover:Commandcode_api.discover ~map_http_error ~map_api_error
end

let discover_charm ?http ?cancel credential =
  match credential with
  | Some (OAuth _) -> Error Invalid_credential
  | None | Some (Api_key _) ->
      let source = match http with
        | Some callback -> callback
        | None -> fun ~url ~headers -> default_http ?cancel ~url ~headers () in
      let http ~url ~headers = match source ~url ~headers with
        | Ok response -> Ok response
        | Error (Http_error status) -> Error (Charm_hyper_api.Http_error status)
        | Error (Invalid_response reason) ->
            Error (Charm_hyper_api.Invalid_response reason)
        | Error _ -> Error Charm_hyper_api.Transport_error in
      (try
         Provider.check_cancel cancel;
         let result = Charm_hyper_api.discover ~http ~api_key:"" () in
         Provider.check_cancel cancel;
         match result with
         | Ok ids -> Ok ids
         | Error Charm_hyper_api.Transport_error ->
             Error (Transport_error "request failed or timed out")
         | Error (Charm_hyper_api.Http_error status) -> Error (Http_error status)
         | Error (Charm_hyper_api.Invalid_response reason) ->
             Error (Invalid_response reason)
       with
       | Provider.Cancelled -> raise Provider.Cancelled
       | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
           Error (Transport_error "request failed or timed out"))


let discover_devin_models ?http ?cancel credential =
  match credential with
  | None -> Error Missing_credential
  | Some (OAuth { service = "devin"; access; _ }) ->
      if http <> None then
        Error (Invalid_response "Devin model discovery uses its native protobuf transport")
      else (match (try Devin_api.discover ?cancel ~api_key:access ()
        with Devin_binary_http.Cancelled -> raise Provider.Cancelled) with
       | Ok rows -> Ok rows
       | Error Devin_api.Invalid_credential -> Error Invalid_credential
       | Error Devin_api.Transport_error ->
           Error (Transport_error "Devin Connect request failed")
       | Error (Devin_api.Http_error status) -> Error (Http_error status)
       | Error (Devin_api.Invalid_response reason) ->
           Error (Invalid_response reason))
  | Some _ -> Error Invalid_credential

let discover_generic_models ?http ?cancel ~provider ?credential () =
  let target = match provider with
    | "openai" -> Some (openai_url, "data", "id", include_all)
    | "google" -> Some (google_url, "models", "name", include_gemini)
    | "ollama" -> Some (ollama_url, "models", "name", include_all)
    | "github-copilot" -> Some (copilot_url, "data", "id", include_copilot)
    | "openrouter" -> Some (openrouter_url, "data", "id", include_all)
    | "anthropic" -> Some (anthropic_url, "data", "id", include_all)
    | "deepseek" -> Some (deepseek_url, "data", "id", include_all)
    | "groq" -> Some (groq_url, "data", "id", include_all)
    | "mistral" -> Some (mistral_url, "data", "id", include_all)
    | "together" -> Some (together_url, "", "id", include_together)
    | "cerebras" -> Some (cerebras_url, "data", "id", include_all)
    | "venice" -> Some (venice_url, "data", "id", include_venice)
    | "deepinfra" -> Some (deepinfra_url, "data", "id", include_deepinfra)
    | "fireworks" -> Some (fireworks_url, "models", "name", include_fireworks)
    | "baseten" -> Some (baseten_url, "data", "id", include_baseten)
    | "huggingface" -> Some (huggingface_url, "data", "id", include_huggingface)
    | "nanogpt" -> Some (nanogpt_url, "data", "id", include_nanogpt)
    | "abliteration" -> Some (abliteration_url, "data", "id", include_all)
    | "gmi-cloud" -> Some (gmi_cloud_url, "data", "id", include_all)
    | "moonshot" -> Some (moonshot_url, "data", "id", include_all)
    | "novita" -> Some (Novita_api.models_url, "data", "id", include_all)
    | "siliconflow" ->
        Some (Siliconflow_api.models_url, "data", "id", include_all)
    | "siliconflow-cn" ->
        Some (Siliconflow_api.cn_models_url, "data", "id", include_all)
    | "coreweave" ->
        Some (Coreweave_api.models_url, "data", "id", include_all)
    | _ -> None in
  match target with
  | None -> Error (Unsupported_provider provider)
  | Some (url, field, id, include_row) ->
      let secret = match provider, credential with
        | "ollama", None -> Ok None
        | "ollama", Some _ -> Error Invalid_credential
        | ("openai" | "google" | "openrouter" | "anthropic" |
           "deepseek" | "groq" | "mistral" | "together" |
           "cerebras" | "venice" | "deepinfra" | "fireworks" |
           "baseten" | "huggingface" | "nanogpt" | "abliteration" |
           "gmi-cloud" | "moonshot" | "novita" | "siliconflow" |
           "siliconflow-cn" | "coreweave"),
          Some (Api_key key) ->
            if valid_secret key then Ok (Some key) else Error Invalid_credential
        | "github-copilot",
          Some (OAuth { service = "github-copilot"; access = key; _ }) ->
            if valid_secret key then Ok (Some key) else Error Invalid_credential
        | _, None -> Error Missing_credential
        | _ -> Error Invalid_credential in
      (match secret with
      | Error _ as failure -> failure
      | Ok secret ->
          let headers = match provider, secret with
            | "openai", Some key -> ["Authorization", "Bearer " ^ key]
            | "openrouter", Some key -> ["Authorization", "Bearer " ^ key]
            | "google", Some key -> ["x-goog-api-key", key]
            | ("novita" | "siliconflow" | "siliconflow-cn" | "coreweave"),
              Some key ->
                ["Authorization", "Bearer " ^ key;
                 "Accept", "application/json"]
            | ("deepseek" | "groq" | "mistral" | "together" |
               "cerebras" | "venice" | "deepinfra" | "fireworks" |
               "baseten" | "huggingface" | "nanogpt" | "abliteration" |
               "gmi-cloud" | "moonshot"), Some key ->
                ["Authorization", "Bearer " ^ key]
            | "anthropic", Some key ->
                ["x-api-key", key; "anthropic-version", "2023-06-01";
                 "anthropic-beta", "compact-2026-09-04"]
            | "github-copilot", Some key ->
                ["Authorization", "Bearer " ^ key;
                 "User-Agent", "copilot/1.0.82";
                 "Editor-Version", "copilot/1.0.82";
                 "Copilot-Integration-Id", "copilot-chat";
                 "Copilot-Harness-Id", "copilot-sdk"]
            | _ -> [] in
          let seen = Hashtbl.create 32 and seen_models = Hashtbl.create 32
          and result = ref [] in
          let visited = Hashtbl.create 8 in
          let http = match http with
            | Some http -> http
            | None -> fun ~url ~headers -> default_http ?cancel ~url ~headers () in
          let rec pages page token =
            Provider.check_cancel cancel;
            if page >= 50 then invalid "too many model listing pages"
            else let page_url = match provider, token with
              | "fireworks", token ->
                  url ^ "?pageSize=200&filter=supports_serverless%3Dtrue" ^
                  (match token with
                   | None -> ""
                   | Some next -> "&pageToken=" ^ Oauth_flow.url_encode next)
              | "anthropic", None -> url ^ "?limit=100"
              | "anthropic", Some token ->
                  url ^ "?limit=100&after_id=" ^ Oauth_flow.url_encode token
              | _, None -> url
              | _, Some token ->
                  url ^ "?pageToken=" ^ Oauth_flow.url_encode token in
            let response = http ~url:page_url ~headers in
            Provider.check_cancel cancel;
            match response with
            | Error _ as failure -> failure
            | Ok (status, _) when status < 200 || status >= 300 ->
                Error (Http_error status)
            | Ok (_, body) ->
                if String.length body > response_limit url then
                  invalid "listing exceeds size limit"
                else
                  let json = try Some (Yojson.Basic.from_string body)
                    with Yojson.Json_error _ -> None in
                  match json with
                  | None -> invalid "malformed or truncated JSON"
                  | Some json ->
                      (match (if provider = "together" then
                        match json with
                        | `List rows -> Ok rows
                        | _ -> invalid "missing Together model array"
                      else listing field json) with
                      | Error _ as failure -> failure
                      | Ok rows ->
                          match collect_rows ~seen ~provider ~id ~include_row rows with
                          | Error _ as failure -> failure
                          | Ok models ->
                              let ids = List.map
                                (fun (model : raw_model) -> model.id) models in
                              if not (add_unique seen_models result models) then
                                invalid "duplicate model ID"
                              else if provider = "anthropic" then
                                (match extract_field "has_more" json with
                                | Some (`Bool false) -> Ok (List.rev !result)
                                | Some (`Bool true) ->
                                    (match extract_field "last_id" json with
                                    | Some (`String next) ->
                                        (match checked_id next with
                                        | Ok next when ids <> [] &&
                                            List.hd (List.rev ids) = next &&
                                            not (Hashtbl.mem visited next) ->
                                            Hashtbl.add visited next ();
                                            pages (page + 1) (Some next)
                                        | _ -> invalid "invalid or repeated last_id")
                                    | _ -> invalid "missing last_id")
                                | _ -> invalid "invalid or missing has_more")
                              else if provider = "fireworks" then
                                (match extract_field "nextPageToken" json with
                                | None | Some (`String "") -> Ok (List.rev !result)
                                | Some (`String next) when next <> "" &&
                                    String.length next <= 4096 &&
                                    not (Hashtbl.mem visited next) &&
                                    String.for_all
                                      (fun c -> Char.code c >= 32 && Char.code c < 127)
                                      next ->
                                    Hashtbl.add visited next ();
                                    pages (page + 1) (Some next)
                                | _ -> invalid "invalid or repeated nextPageToken")
                              else if provider <> "google" then Ok (List.rev !result)
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
let discover_ids ?http ?cancel ~provider ?credential () =
  if Local_compat.engine provider <> None then
    discover_local ?http ?cancel ~provider credential
  else if provider = "amazon-bedrock" then
    discover_bedrock ?http ?cancel credential
  else if provider = "bedrock-mantle" then
    discover_mantle ?http ?cancel credential
  else if provider = "ollama-cloud" then
    discover_ollama_cloud ?http ?cancel credential
  else if provider = "xai" then
    discover_xai ?http ?cancel credential
  else if provider = "nvidia" then
    discover_nvidia ?http ?cancel credential
  else if provider = "stepfun" then
    discover_stepfun ?http ?cancel credential
  else if provider = "synthetic" then
    discover_synthetic ?http ?cancel credential
  else if provider = "zenmux" then
    Zenmux_discovery.discover ?http ?cancel credential
  else if provider = "wafer-serverless" then
    Wafer_discovery.discover ?http ?cancel credential
  else if provider = "qianfan" then
    Qianfan_discovery.discover ?http ?cancel credential
  else if provider = "xiaomi" then
    Xiaomi_discovery.discover ?http ?cancel credential
  else if provider = "kilo" then
    Kilo_discovery.discover ?http ?cancel ~public:true
      ~oauth_service:"kilo" credential
  else if provider = "singularityapi-dev" then
    Singularity_dev_discovery.discover ?http ?cancel credential
  else if provider = "opencode-zen" then
    Opencode_zen_discovery.discover ?http ?cancel ~public:true credential
  else if provider = "opencode-go" then
    Opencode_go_discovery.discover ?http ?cancel ~public:true credential
  else if provider = "charm-hyper" then
    discover_charm ?http ?cancel credential
  else if provider = "yolo-auto" then
    Yolo_auto_discovery.discover ?http ?cancel credential
  else if provider = "meta" then
    Meta_discovery.discover ?http ?cancel credential
  else if provider = "vercel-ai-gateway" then
    Vercel_ai_gateway_discovery.discover ?http ?cancel credential
  else if provider = "devin" then
    Result.map (List.map (fun (model : Devin_api.model) -> model.id))
      (discover_devin_models ?http ?cancel credential)
  else if provider = "openai-codex" then
    Result.map (List.map (fun (model : raw_model) -> model.id))
      (discover_codex_models ?http ?cancel credential)
  else if provider = "sakana" then (
    let credential = match credential with
      | None -> Error Missing_credential
      | Some (Api_key key) -> Ok key
      | Some _ -> Error Invalid_credential in
    match credential with
    | Error _ as failure -> failure
    | Ok key ->
        let http = Option.map (fun http ~url ~headers ->
          match http ~url ~headers with
          | Ok value -> Ok value
          | Error Invalid_credential -> Error Sakana_api.Invalid_credential
          | Error (Http_error status) -> Error (Sakana_api.Http_error status)
          | Error (Invalid_response reason) ->
              Error (Sakana_api.Invalid_response reason)
          | Error _ -> Error Sakana_api.Transport_error) http in
        (match Sakana_api.discover_sakana ?http ?cancel ~credential:key () with
        | Ok ids -> Ok ids
        | Error Sakana_api.Invalid_credential -> Error Invalid_credential
        | Error Sakana_api.Transport_error ->
            Error (Transport_error "request failed or timed out")
        | Error (Sakana_api.Http_error status) -> Error (Http_error status)
        | Error (Sakana_api.Invalid_response reason) ->
            Error (Invalid_response reason)))
  else if Tool_gateways.find provider <> None then (
    let spec = Option.get (Tool_gateways.find provider) in
    match credential with
    | None -> Error Missing_credential
    | Some (Api_key key) when valid_secret key ->
        let headers = ["Authorization", "Bearer " ^ key] in
        let http = match http with
          | Some http -> http
          | None -> fun ~url ~headers -> default_http ?cancel ~url ~headers () in
        (try
          Provider.check_cancel cancel;
          let result = http ~url:spec.models_url ~headers in
          Provider.check_cancel cancel;
          match result with
          | Error failure -> Error failure
          | Ok (status, _) when status < 200 || status >= 300 ->
              Error (Http_error status)
          | Ok (_, body) ->
              (match Tool_gateways.parse_models ~provider body with
              | Ok ids -> Ok ids
              | Error reason -> Error (Invalid_response reason))
         with
         | Provider.Cancelled -> raise Provider.Cancelled
         | Provider.Provider_error _ | Unix.Unix_error _ | Sys_error _ ->
             Error (Transport_error "request failed or timed out"))
    | Some _ -> Error Invalid_credential)
  else
    Result.map (List.map (fun (model : raw_model) -> model.id))
      (discover_generic_models ?http ?cancel ~provider ?credential ())
type credential_policy =
  | Anonymous
  | Ambient_credentials
  | Optional_api_key
  | Required_api_key
  | Stored_api_key of string
  | OAuth_account of string

type adapter = {
  provider : string;
  endpoint : string;
  credential_policy : credential_policy;
}

let pinned_endpoint ~provider ~(route : Provider_catalog.route) =
  let target_url target = target.Bedrock_wire.url in
  let dynamic f = try Some (f ()) with
    | Invalid_argument _ | Provider.Provider_error _ -> None in
  if Local_compat.engine provider <> None then
    dynamic (fun () -> Local_compat.listing_url ~endpoint:route.endpoint)
  else
    match provider with
    | "amazon-bedrock" ->
        dynamic (fun () ->
          target_url (Bedrock_wire.discovery_endpoint
            ~region:(Aws_auth.region ()) ()))
    | "bedrock-mantle" ->
        dynamic (fun () ->
          (Bedrock_mantle.discovery_endpoint
            ~region:(Bedrock_mantle.region ()) ()).Bedrock_mantle.url)
    | "openai" -> Some openai_url
    | "google" -> Some google_url
    | "ollama" -> Some ollama_url
    | "github-copilot" -> Some copilot_url
    | "openrouter" -> Some openrouter_url
    | "anthropic" -> Some anthropic_url
    | "deepseek" -> Some deepseek_url
    | "groq" -> Some groq_url
    | "mistral" -> Some mistral_url
    | "together" -> Some together_url
    | "cerebras" -> Some cerebras_url
    | "venice" -> Some venice_url
    | "deepinfra" -> Some deepinfra_url
    | "fireworks" -> Some fireworks_url
    | "baseten" -> Some baseten_url
    | "huggingface" -> Some huggingface_url
    | "nanogpt" -> Some nanogpt_url
    | "abliteration" -> Some abliteration_url
    | "gmi-cloud" -> Some gmi_cloud_url
    | "moonshot" -> Some moonshot_url
    | "novita" -> Some Novita_api.models_url
    | "siliconflow" -> Some Siliconflow_api.models_url
    | "siliconflow-cn" -> Some Siliconflow_api.cn_models_url
    | "coreweave" -> Some Coreweave_api.models_url
    | "ollama-cloud" -> Some Ollama_cloud.tags_url
    | "xai" -> Some Xai_api.models_url
    | "nvidia" -> Some Nvidia_api.models_url
    | "stepfun" -> Some Stepfun_api.models_url
    | "synthetic" -> Some Synthetic_api.models_url
    | "zenmux" -> Some Zenmux_api.models_url
    | "wafer-serverless" -> Some Wafer_api.models_url
    | "qianfan" -> Some Qianfan_api.models_url
    | "xiaomi" -> Some Xiaomi_api.models_url
    | "kilo" -> Some Kilo_api.models_url
    | "singularityapi-dev" -> Some Singularity_dev_api.models_url
    | "opencode-zen" -> Some Opencode_zen_api.models_url
    | "opencode-go" -> Some Opencode_go_api.models_url
    | "charm-hyper" -> Some Charm_hyper_api.models_url
    | "yolo-auto" -> Some Yolo_auto_api.models_url
    | "meta" -> Some Meta_api.models_url
    | "vercel-ai-gateway" -> Some Vercel_ai_gateway_api.models_url
    | "devin" -> Some Devin_api.models_url
    | "openai-codex" -> List.nth_opt codex_urls 0
    | "commandcode" -> Some Commandcode_api.models_url
    | "sakana" -> Some Sakana_api.sakana_models_url
    | id -> Option.map (fun (spec : Tool_gateways.spec) -> spec.models_url)
        (Tool_gateways.find id)

let credential_policy provider =
  if Local_compat.engine provider <> None then Some Optional_api_key
  else match provider with
  | "ollama" | "charm-hyper" | "kilo" | "opencode-zen" | "opencode-go" ->
      Some Anonymous
  | "amazon-bedrock" -> Some Ambient_credentials
  | "github-copilot" | "devin" | "openai-codex" ->
      Some (OAuth_account provider)
  | "openrouter" -> Some (Stored_api_key provider)
  | _ when provider = "bedrock-mantle" || provider = "ollama-cloud" ||
      provider = "xai" || provider = "nvidia" || provider = "stepfun" ||
      provider = "synthetic" || provider = "zenmux" ||
      provider = "wafer-serverless" || provider = "qianfan" ||
      provider = "xiaomi" || provider = "singularityapi-dev" ||
      provider = "yolo-auto" || provider = "meta" ||
      provider = "vercel-ai-gateway" || provider = "commandcode" ||
      provider = "sakana" || Tool_gateways.find provider <> None ||
      List.mem provider [
        "openai"; "google"; "anthropic"; "deepseek"; "groq"; "mistral";
        "together"; "cerebras"; "venice"; "deepinfra"; "fireworks";
        "baseten"; "huggingface"; "nanogpt"; "abliteration"; "gmi-cloud";
        "moonshot"; "novita"; "siliconflow"; "siliconflow-cn"; "coreweave"
      ] -> Some Required_api_key
  | _ -> None

let adapter_for ~provider ~(route : Provider_catalog.route) =
  match pinned_endpoint ~provider ~route, credential_policy provider with
  | Some endpoint, Some credential_policy ->
      Some { provider; endpoint; credential_policy }
  | _ -> None

let check_credential policy credential =
  let required () = match credential with
    | None -> Error Missing_credential
    | Some _ -> Error Invalid_credential in
  match policy, credential with
  | Anonymous, None | Ambient_credentials, None | Optional_api_key, None ->
      Ok None
  | Anonymous, Some (Api_key key) when valid_secret key -> Ok None
  | (Optional_api_key | Required_api_key | Stored_api_key _),
      Some (Api_key key) when valid_secret key ->
      Ok None
  | Required_api_key, None | Required_api_key, Some (Api_key _)
  | Stored_api_key _, None -> required ()
  | OAuth_account service, Some (OAuth { service = actual; access; account_id })
      when service = actual && valid_secret access &&
        Option.fold ~none:true ~some:valid_secret account_id ->
      Ok account_id
  | OAuth_account _, None -> Error Missing_credential
  | _ -> Error Invalid_credential

let discover_raw ?http ?cancel ~provider ?credential () =
  if provider = "commandcode" then
    Result.map (List.map (fun (model : Commandcode_api.model) ->
      { id = model.id; display_name = Some model.name;
        capabilities = { Model_catalog.empty_capabilities with
          context_window_tokens = model.context_length;
          supported_endpoints = Some model.supported_endpoints } }))
      (Commandcode_listing_discovery.discover ?http ?cancel credential)
  else if provider = "devin" then
    Result.map (List.map (fun (model : Devin_api.model) ->
      { id = model.id; display_name = Some model.name;
        capabilities = { Model_catalog.empty_capabilities with
          context_window_tokens = model.context_window_tokens;
          max_output_tokens = model.max_tokens;
          tools = model.supports_tools;
          provider_tokenizer = model.tokenizer_type } }))
      (discover_devin_models ?http ?cancel credential)
  else if provider = "openai-codex" then
    discover_codex_models ?http ?cancel credential
  else if provider = "google" || provider = "anthropic" then
    discover_generic_models ?http ?cancel ~provider ?credential ()
  else
    Result.map (List.map (fun id ->
      { id; display_name = None; capabilities = Model_catalog.empty_capabilities }))
      (discover_ids ?http ?cancel ~provider ?credential ())

let discover ?http ?cancel ?route_name ?account_id ~provider ?credential () =
  match Provider_catalog.find provider with
  | None -> Error (Unsupported_provider provider)
  | Some descriptor ->
      let route_name = Option.value ~default:descriptor.default_route route_name in
      (match Provider_catalog.route descriptor route_name with
       | None -> Error (Unsupported_route (provider, route_name))
       | Some route ->
           (match adapter_for ~provider ~route with
            | None -> Error (Unsupported_provider provider)
            | Some adapter ->
                (match check_credential adapter.credential_policy credential with
                 | Error _ as error -> error
                 | Ok credential_account ->
                     let account_mismatch = match account_id, credential_account with
                       | Some expected, Some actual -> expected <> actual
                       | _ -> false in
                     if account_mismatch then Error Invalid_credential else
                     let account_id = match account_id, credential_account with
                       | Some expected, _ -> Some expected
                       | None, Some actual -> Some actual
                       | None, None -> None in
                     (match discover_raw ?http ?cancel ~provider ?credential () with
                      | Error _ as error -> error
                      | Ok raw_models ->
                          let seen = Hashtbl.create (List.length raw_models) in
                          let unique = List.for_all (fun (raw : raw_model) ->
                            if Hashtbl.mem seen raw.id then false
                            else (Hashtbl.add seen raw.id (); true)) raw_models in
                          if not unique then invalid "duplicate model ID" else
                          let retrieved_at = Unix.gettimeofday () in
                          let id_source = match adapter.credential_policy,
                              credential with
                            | Anonymous, _ | _, None ->
                                Model_catalog.Provider_listing
                            | _, Some _ ->
                                Model_catalog.Pinned_account_listing in
                          let source = {
                            Model_catalog.id_source;
                            capability_source = None;
                            endpoint = Some adapter.endpoint;
                            retrieved_at = Some retrieved_at;
                          } in
                          let models = List.map (fun raw ->
                            let identity = Model_identity.make ~provider
                              ?account_id ~route:route.name
                              ~upstream_id:raw.id () in
                            let capability_source =
                              if Model_catalog.has_reported_capabilities
                                  raw.capabilities then
                                Some Model_catalog.Capability_response
                              else None in
                            let provenance = { source with capability_source } in
                            { Model_catalog.identity;
                              display_name = raw.display_name;
                              capabilities = raw.capabilities; provenance })
                            raw_models in
                          Ok { models; source }))))
