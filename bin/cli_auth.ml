let oauth_policy service = match service with
  | "anthropic" -> Pave.Oauth_flow.anthropic ~sdk_version:"0.112.1" ()
  | "openai-codex" -> Pave.Codex_oauth.policy ()
  | _ -> failwith ("unsupported OAuth login: " ^ service)

let oauth_exchange service policy authorization response =
  if service = "openai-codex" then
    Pave.Codex_oauth.exchange authorization ~response
  else Pave.Oauth_flow.exchange policy authorization ~response

let oauth_refresh service policy credential =
  if service = "openai-codex" then Pave.Codex_oauth.refresh credential
  else Pave.Oauth_flow.refresh policy credential

let handle_action ~login ~login_manual ~logout =
    let actions = List.filter ((<>) "") [ login; login_manual; logout ] in
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
           Pave.Oauth_store.remove ~path ~provider:id;
           Printf.printf "Local OAuth credential removed for %s.\n" id)
         else (
           let credential =
             if service = "openrouter" then (
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
           Pave.Oauth_store.put ~path ~provider:id credential;
           Printf.printf "OAuth credential stored for %s.\n" id);
         true
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
let resolve_authentication ~(descriptor : Pave.Provider_catalog.descriptor)
    ~(route : Pave.Provider_catalog.route) ~endpoint =
    if route.wire = Pave.Provider.Local_chat then (
      if not (List.mem descriptor.id ["lm-studio"; "llama.cpp"; "vllm"]) ||
         descriptor.oauth <> None then
        failwith "unsupported local provider authentication";
      ignore (Pave.Provider.local_endpoint
        (if endpoint = "" then route.endpoint else endpoint));
      let key = api_key descriptor in
      Pave.Provider.Api_key, Option.value ~default:"" key, None)
    else if route.wire = Pave.Provider.Vertex_generate ||
            route.wire = Pave.Provider.Bedrock_converse then (
      if endpoint <> "" then
        failwith "cloud credentials require the provider's derived regional endpoint";
      Pave.Provider.Api_key, "", None)
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
          if Pave.Oauth_store.get ~path ~provider:provider_id = None then
            failwith ("run pave --login " ^ provider_id ^
              (match descriptor.api_key_env with
               | Some name -> " or set " ^ name | None -> ""));
          let policy = if service = "openrouter" || service = "github-copilot"
            then None else Some (oauth_policy service) in
          let resolve_credential () = Pave.Oauth_store.with_lock ~path (fun () ->
            let credential = match Pave.Oauth_store.get ~path ~provider:provider_id with
              | Some credential -> credential
              | None -> failwith ("OAuth credential removed; run pave --login " ^ provider_id) in
            let credential = match credential.expires_at with
              | Some expires when Unix.gettimeofday () >= expires -. 60. ->
                  let policy = match policy with
                    | Some policy -> policy
                    | None -> failwith "nonrefreshable provider credential unexpectedly has an expiry" in
                  let updated = oauth_refresh service policy credential in
                  Pave.Oauth_store.put ~path ~provider:provider_id updated;
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
