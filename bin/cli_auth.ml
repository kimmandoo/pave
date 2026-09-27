let oauth_policy service = match service with
  | "anthropic" -> Pave.Oauth_flow.anthropic ~sdk_version:"0.112.1" ()
  | "openai-codex" -> Pave.Codex_oauth.policy ()
  | "gitlab-duo" -> Pave.Gitlab_duo_oauth.policy ()
  | _ -> failwith ("unsupported OAuth login: " ^ service)

let oauth_exchange service policy authorization response =
  if service = "openai-codex" then
    Pave.Codex_oauth.exchange authorization ~response
  else if service = "gitlab-duo" then
    Pave.Gitlab_duo_oauth.exchange authorization ~response
  else Pave.Oauth_flow.exchange policy authorization ~response

let oauth_refresh service policy credential =
  if service = "openai-codex" then Pave.Codex_oauth.refresh credential
  else if service = "gitlab-duo" then Pave.Gitlab_duo_oauth.refresh credential
  else Pave.Oauth_flow.refresh policy credential

let grant_type service = match service with
  | "github-copilot" | "kilo" -> Pave.Oauth_store.Device_approval
  | "devin" -> Pave.Oauth_store.Provider_session
  | _ -> Pave.Oauth_store.Authorization_code

let binding (descriptor : Pave.Provider_catalog.descriptor) grant_type =
  ({ Pave.Oauth_store.provider = descriptor.id;
     grant_type;
     routes = List.map
       (fun (route : Pave.Provider_catalog.route) ->
         route.name, route.endpoint) descriptor.routes }
   : Pave.Oauth_store.binding)

let grant_types service =
  if service = "openai-codex" then
    [Pave.Oauth_store.Authorization_code; Pave.Oauth_store.Device_approval]
  else [grant_type service]

let bindings descriptor service =
  List.map (binding descriptor) (grant_types service)

let handle_action ?account_id ?(login_device = "")
    ~login ~login_manual ~logout () =
    let actions = List.filter ((<>) "") [ login; login_manual; login_device; logout ] in
    if List.length actions > 1 then failwith "choose only one OAuth action";
    (match actions with
     | [] -> false
     | [ id ] ->
         let descriptor = match Pave.Provider_catalog.find id with
           | Some entry -> entry
           | None -> failwith ("unsupported provider: " ^ id) in
         let service = match descriptor.oauth with
           | Some service -> service
           | None -> failwith ("OAuth is not available for " ^ id) in
         let path = Pave.Oauth_store.default_path () in
         if logout <> "" then (
           (match account_id with
            | Some account_id ->
                Pave.Oauth_store.remove_account ~path ~provider:id
                  ~account_id:(Some account_id);
                Printf.printf "Local OAuth credential removed for %s account %s.\n"
                  id account_id
            | None ->
                Pave.Oauth_store.remove_provider ~path ~provider:id;
                Printf.printf "All local OAuth credentials removed for %s.\n" id);
           true)
         else (
           let credential =
             if login_device <> "" then (
               if service <> "openai-codex" then
                 failwith "--login-device is only available for openai-codex";
               Pave.Codex_oauth.device_login
                 ~on_authorization:(fun (auth : Pave.Oauth_device.authorization) ->
                   Printf.printf "Open this verification URL:\n%s\nEnter code: %s\nWaiting for authorization...\n%!"
                     auth.verification_uri auth.user_code) ())
             else if service = "openrouter" then (
               if login_manual <> "" then (
                 let authorization = Pave.Openrouter_oauth.start () in
                 Printf.printf "Open this authorization URL:\n%s\nPaste the full redirect URL: %!"
                   authorization.url;
                 Pave.Openrouter_oauth.exchange authorization ~response:(read_line ()))
               else (
                 let authorization, listener = Pave.Openrouter_oauth.listen_loopback () in
                 Printf.printf "Open this authorization URL:\n%s\nWaiting for browser callback...\n%!"
                   authorization.url;
                 let code = Pave.Openrouter_oauth.await_callback authorization listener in
                 Pave.Openrouter_oauth.exchange authorization ~response:code))
             else if service = "github-copilot" then (
               if login_manual <> "" then
                 failwith "GitHub Copilot uses a device code; run --login instead of --login-manual";
               Pave.Github_copilot_oauth.login
                 ~on_authorization:(fun (auth : Pave.Oauth_device.authorization) ->
                   Printf.printf "Open this verification URL:\n%s\nEnter code: %s\nWaiting for authorization...\n%!"
                     auth.verification_uri auth.user_code) ())
             else if service = "devin" then (
               let on_authorization (auth : Pave.Oauth_flow.authorization) =
                 Printf.printf "Open this authorization URL:\n%s\n%!" auth.url in
               if login_manual <> "" then
                 Pave.Devin_oauth.login_manual ~on_authorization
                   ~on_code:(fun () ->
                     print_string "Paste the full callback URL: "; flush stdout;
                     read_line ()) ()
               else Pave.Devin_oauth.login
                 ~on_authorization:(fun auth ->
                   on_authorization auth;
                   print_endline "Waiting for browser callback...") ())
             else if service = "kilo" then (
               if login_manual <> "" then
                 failwith "Kilo uses a device code; run --login instead of --login-manual";
               Pave.Kilo_oauth.login
                 ~on_authorization:(fun (auth : Pave.Kilo_oauth.authorization) ->
                   Printf.printf "Open this verification URL:\n%s\nEnter code: %s\nWaiting for authorization...\n%!"
                     auth.verification_url auth.code) ())
             else (
               let policy = oauth_policy service in
               if login_manual <> "" then (
                 let authorization = Pave.Oauth_flow.start policy in
                 Printf.printf "Open this authorization URL:\n%s\nPaste the full redirect URL: %!"
                   authorization.url;
                 oauth_exchange service policy authorization (read_line ()))
               else (
                 let authorization, listener = Pave.Oauth_flow.listen_loopback policy in
                 Printf.printf "Open this authorization URL:\n%s\nWaiting for browser callback...\n%!"
                   authorization.url;
                 let code = Pave.Oauth_flow.await_callback authorization listener in
                 oauth_exchange service policy authorization
                   (code ^ "#" ^ authorization.state))) in
           Option.iter (fun requested ->
             if credential.Pave.Oauth_store.account_id <> Some requested then
               failwith "granted account does not match --account; credential was not stored")
             account_id;
           let selection_id =
             Pave.Oauth_store.put_account_with_selection ~path ~provider:id
               ~binding:(binding descriptor
                 (if login_device <> "" then Pave.Oauth_store.Device_approval
                  else grant_type service)) credential in
           Printf.printf "OAuth credential stored for %s%s.\n" id
             (match credential.Pave.Oauth_store.account_id with
              | Some value -> " account " ^ value
              | None -> " local sign-in ID " ^ selection_id);
           true)
     | _ -> assert false)


let api_key (descriptor : Pave.Provider_catalog.descriptor) =
  let configured = Option.bind descriptor.api_key_env (fun name ->
    match Sys.getenv_opt name with
    | Some key when key <> "" -> Some key
    | _ -> None) in
  match configured with
  | Some _ -> configured
  | None when descriptor.id = "sakana" ->
      (match Sys.getenv_opt "FUGU_API_KEY" with
      | Some key when key <> "" -> Some key
      | _ -> None)
  | None when descriptor.id = "abliteration" ->
      (match Sys.getenv_opt "ABLIT_KEY" with
      | Some key when key <> "" -> Some key
      | _ -> None)
  | None when descriptor.id = "moonshot" ->
      (match Sys.getenv_opt "KIMI_API_KEY" with
      | Some key when key <> "" -> Some key
      | _ -> None)
  | None when descriptor.id = "ollama-cloud" ->
      Pave.Ollama_cloud.env_api_key ()
  | None when descriptor.id = "coreweave" ->
      Pave.Coreweave_api.env_api_key ()
  | None when descriptor.id = "charm-hyper" ->
      Pave.Charm_hyper_api.env_api_key ()
  | None when descriptor.id = "meta" ->
      Pave.Meta_api.env_api_key ()
  | None when descriptor.id = "vercel-ai-gateway" ->
      Pave.Vercel_ai_gateway_api.env_api_key ()
  | None when descriptor.id = "commandcode" ->
      Pave.Commandcode_api.env_api_key ()
  | None -> None
let resolve_builtin_authentication ?account_id
    ~(descriptor : Pave.Provider_catalog.descriptor)
    ~(route : Pave.Provider_catalog.route) ~endpoint () =
    Pave.Provider.validate_endpoint_override ~api:route.wire
      ~pinned_endpoint:route.endpoint ~requested:endpoint;
    if route.wire = Pave.Provider.Apple_foundation_models then (
      if descriptor.id <> "apple" || endpoint <> "" then
        failwith "Apple Foundation Models requires its registered local route";
      Pave.Provider.Api_key, "", None)
    else
    if route.wire = Pave.Provider.Local_chat then (
      if not (List.mem descriptor.id ["lm-studio"; "llama.cpp"; "vllm"]) ||
         descriptor.oauth <> None then
        failwith "unsupported local provider authentication";
      ignore (Pave.Provider.local_endpoint
        (if endpoint = "" then route.endpoint else endpoint));
      let key = api_key descriptor in
      Pave.Provider.Api_key, Option.value ~default:"" key, None)
    else if route.wire = Pave.Provider.Vertex_generate ||
            route.wire = Pave.Provider.Vertex_anthropic ||
            route.wire = Pave.Provider.Bedrock_converse ||
            route.wire = Pave.Provider.Bedrock_converse_stream then (
      if endpoint <> "" then
        failwith "cloud credentials require the provider's derived endpoint";
      Pave.Provider.Cloud_identity, "", None)
    else if route.wire = Pave.Provider.Azure_responses ||
            route.wire = Pave.Provider.Azure_chat then
      (match api_key descriptor with
       | Some key -> Pave.Provider.Api_key, key, None
       | None -> Pave.Provider.Cloud_identity, "", None)
    else if route.wire = Pave.Provider.Bedrock_mantle_responses then (
      if endpoint <> "" then
        failwith "Mantle bearer token requires the derived regional endpoint";
      let key = Option.value ~default:"" (api_key descriptor) in
      let target = Pave.Bedrock_mantle.endpoint
        ~region:(Pave.Bedrock_mantle.region ()) () in
      let _, headers = Pave.Bedrock_mantle.resolve
        ~endpoint:target.url ~api_key:key () in
      match headers with
      | [ _ ] -> Pave.Provider.Api_key, key, None
      | _ -> assert false)
    else if route.wire = Pave.Provider.Cloudflare_ai_gateway_chat then (
      if endpoint <> "" then
        failwith "Cloudflare gateway credentials require the configured account/gateway endpoint";
      let url = match Pave.Cloudflare_ai_gateway_api.env_chat_url () with
        | Some url -> url
        | None -> failwith "set CLOUDFLARE_ACCOUNT_ID and CLOUDFLARE_GATEWAY_ID" in
      let key = Option.value ~default:"" (api_key descriptor) in
      ignore (Pave.Cloudflare_ai_gateway_api.chat_headers
        ~endpoint:url ~api_key:key);
      Pave.Provider.Api_key, key, None)
    else
    let env_key = api_key descriptor in
    let authentication, api_key, resolve_credential =
      match env_key, descriptor.oauth with
      | Some key, _ -> Pave.Provider.Api_key, key, None
      | None, None ->
          (match descriptor.api_key_env with
           | None -> Pave.Provider.Api_key, "", None
           | Some name -> failwith ("set " ^ name ^ " to use the configured provider"))
      | None, Some service ->
          if endpoint <> "" && endpoint <> route.endpoint then
            failwith "OAuth credentials cannot be sent to a custom endpoint";
          let path = Pave.Oauth_store.default_path () in
          let provider_id = descriptor.id in
          let expected_bindings = bindings descriptor service in
          let expected_binding = binding descriptor (grant_type service) in
          let stored_accounts =
            Pave.Oauth_store.accounts ~path ~provider:provider_id in
          let stored_account = match account_id, stored_accounts with
            | Some requested, _ ->
                Pave.Oauth_store.account ~path ~provider:provider_id
                  ~account_id:(Some requested)
            | None, [ account ] -> Some account
            | None, [] -> None
            | None, _ ->
                failwith ("multiple saved accounts for " ^ provider_id ^
                  "; select one with --account or an account-scoped model selector") in
          let stored_account = match stored_account with
            | Some account -> account
            | None -> failwith ("run pave --login " ^ provider_id ^
                (match descriptor.api_key_env with
                 | Some name -> " or set " ^ name | None -> "")) in
          let selected_account_id = Some stored_account.selection_id in
          let validate_binding (actual : Pave.Oauth_store.binding) =
            if actual.provider <> expected_binding.provider ||
               not (List.exists (fun expected ->
                 actual.grant_type = expected.Pave.Oauth_store.grant_type)
                 expected_bindings) ||
               not (List.mem (route.name, route.endpoint) actual.routes) then
              failwith "saved credential is not authorized for this provider, grant, and API route" in
          (match stored_account.binding with
           | None ->
               Pave.Oauth_store.put_account ~path ~provider:provider_id
                 ~binding:expected_binding
                 ~selection_id:stored_account.selection_id stored_account.credential
           | Some actual -> validate_binding actual);
          let policy = if List.mem service
            ["openrouter"; "github-copilot"; "devin"; "kilo"]
            then None else Some (oauth_policy service) in
          let resolve_credential () = Pave.Oauth_store.with_lock ~path (fun () ->
            let account = match Pave.Oauth_store.account ~path
                ~provider:provider_id ~account_id:selected_account_id with
              | Some account -> account
              | None -> failwith ("OAuth account removed; run pave --login " ^
                  provider_id) in
            let credential = account.Pave.Oauth_store.credential in
            let account_binding = match account.binding with
              | Some actual -> validate_binding actual; actual
              | None ->
                  failwith "saved credential route binding is missing; sign in again" in
            let credential = match credential.expires_at with
              | Some expires when Unix.gettimeofday () >= expires -. 60. ->
                  let policy = match policy with
                    | Some policy -> policy
                    | None -> failwith "nonrefreshable provider credential unexpectedly has an expiry" in
                  let updated = oauth_refresh service policy credential in
                  if updated.account_id <> credential.account_id then
                    failwith "OAuth refresh changed the provider account; credential was not updated";
                  Pave.Oauth_store.put_account ~path ~provider:provider_id
                    ~binding:account_binding ~selection_id:account.selection_id
                    updated;
                  updated
              | _ -> credential in
            if service = "openrouter" &&
               (credential.refresh <> None || credential.expires_at <> None ||
                not (String.starts_with ~prefix:"sk-or-" credential.access)) then
              failwith "OpenRouter stored API key is invalid";
            if service = "github-copilot" &&
               (credential.access = "" || credential.refresh <> None ||
                credential.expires_at <> None) then
              failwith "GitHub Copilot stored credential is invalid";
            if service = "devin" &&
               (not (Pave.Devin_api.valid_text credential.access) ||
                credential.access = "devin-session-token$" ||
                credential.refresh <> None || credential.expires_at <> None) then
              failwith "Devin session credential is invalid; run pave --login devin";
            if service = "kilo" &&
               (not (Pave.Kilo_api.valid_key credential.access) ||
                credential.refresh <> None || credential.expires_at <> None) then
              failwith "Kilo gateway credential is invalid; run pave --login kilo";
            let account_id, residency =
              if service = "openai-codex" then
                let id, residency = Pave.Codex_oauth.identity credential in
                Some id, residency
              else credential.account_id, None in
            ({ access = credential.access; account_id; residency }
              : Pave.Provider.credentials)) in
          (if service = "openrouter" then Pave.Provider.Api_key
           else Pave.Provider.OAuth), "", Some resolve_credential in
    authentication, api_key, resolve_credential
let resolve_authentication ?account_id ?custom_route
    ~(descriptor : Pave.Provider_catalog.descriptor)
    ~(route : Pave.Provider_catalog.route) ~endpoint () =
  match custom_route with
  | None -> resolve_builtin_authentication ?account_id ~descriptor ~route ~endpoint ()
  | Some custom ->
      if route.name <> custom.Pave.Custom_provider.name ||
         route.wire <> Pave.Provider.Openai_completions ||
         route.endpoint <> custom.endpoint then
        failwith "custom provider route does not match its validated configuration";
      Pave.Provider.validate_endpoint_override ~api:route.wire
        ~pinned_endpoint:custom.endpoint ~requested:endpoint;
      (match custom.auth with
       | Pave.Custom_provider.No_auth -> Pave.Provider.Api_key, "", None
       | Pave.Custom_provider.Api_key_env name ->
           (match Sys.getenv_opt name with
            | Some key when key <> "" && String.length key <= 8192 &&
                String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key ->
                Pave.Provider.Api_key, key, None
            | Some key when key <> "" ->
                failwith "custom provider API key contains invalid characters"
            | _ -> failwith ("set " ^ name ^ " to use the configured custom provider")))
