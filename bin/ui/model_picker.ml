(* The UI owns chooser state; discovery runs on a cancellable worker and only
   delivers verified IDs through the chooser's wake pipe. *)
let credential (descriptor : Pave.Provider_catalog.descriptor) =
  match descriptor.id with
  | "openai" | "google" | "anthropic" | "deepseek" | "groq" | "mistral"
  | "together" | "cerebras" | "venice" | "deepinfra" | "fireworks"
  | "baseten" | "huggingface" | "nanogpt" | "aimlapi" | "aiand"
  | "sakana" | "abliteration" | "gmi-cloud" | "moonshot" | "xai" | "nvidia"
  | "ollama-cloud" | "bedrock-mantle" | "lm-studio" | "llama.cpp" | "vllm" ->
      Option.map (fun key -> Pave.Model_discovery.Api_key key)
        (Cli_auth.api_key descriptor)
  | "openrouter" ->
      (match descriptor.routes with
      | [] -> None
      | route :: _ ->
          let _, key, resolve = Cli_auth.resolve_authentication
            ~descriptor ~route ~endpoint:route.endpoint in
          if key <> "" then Some (Pave.Model_discovery.Api_key key)
          else Option.map (fun resolve ->
            let (credential : Pave.Provider.credentials) = resolve () in
            Pave.Model_discovery.Api_key credential.access) resolve)
  | "github-copilot" ->
      Option.map (fun (stored : Pave.Oauth_store.credential) ->
        Pave.Model_discovery.Copilot_oauth stored.access)
        (Pave.Oauth_store.get ~path:(Pave.Oauth_store.default_path ())
          ~provider:descriptor.id)
  | "openai-codex" ->
      (match descriptor.routes with
      | [] -> None
      | route :: _ ->
          let _, _, resolve = Cli_auth.resolve_authentication
            ~descriptor ~route ~endpoint:route.endpoint in
          Option.map (fun resolve ->
            let (credential : Pave.Provider.credentials) = resolve () in
            let account = match credential.account_id with
              | Some id -> id
              | None -> failwith "Codex OAuth account ID unavailable" in
            Pave.Model_discovery.Codex_oauth
              (credential.access, account)) resolve)
  | _ -> None

let choose screen ~(descriptor : Pave.Provider_catalog.descriptor) ?(intro = [])
    ?(plain = []) ~title ~choices () =
  let read_fd, write_fd = Unix.pipe () in
  Unix.set_close_on_exec read_fd;
  Unix.set_close_on_exec write_fd;
  let cancelled = Atomic.make false in
  let lock = Mutex.create () and outcome = ref None in
  let worker = Thread.create (fun () ->
    let answer =
      try
        let credential = credential descriptor in
        `Listing (Pave.Model_discovery.discover
          ~cancel:(fun () -> Atomic.get cancelled)
          ~provider:descriptor.id ?credential ())
      with Pave.Provider.Cancelled -> `Cancelled
         | _ -> `Unavailable in
    Mutex.lock lock;
    outcome := Some answer;
    Mutex.unlock lock;
    (try ignore (Unix.write_substring write_fd "x" 0 1)
     with Unix.Unix_error _ -> ())) () in
  Fun.protect ~finally:(fun () ->
    Atomic.set cancelled true;
    Thread.join worker;
    Unix.close write_fd;
    Unix.close read_fd) (fun () ->
    let on_wake () =
      let marker = Bytes.create 1 in
      ignore (Unix.read read_fd marker 0 1);
      Mutex.lock lock;
      let answer = !outcome in
      Mutex.unlock lock;
      match answer with
      | Some (`Listing (Ok ids)) ->
          let verified = match Pave.Provider_catalog.route descriptor "" with
            | None -> []
            | Some _ -> List.map (fun id -> descriptor.id ^ "/" ^ id) ids in
          let status = Printf.sprintf "%s: %d live routable model%s"
            descriptor.display_name (List.length verified)
            (if List.length verified = 1 then "" else "s") in
          Tui.update_choices screen ~verified ~status ()
      | Some (`Listing (Error error)) ->
          Tui.update_choices screen ~verified:[]
            ~status:(Pave.Model_discovery.message error) ()
      | Some `Unavailable ->
          Tui.update_choices screen ~verified:[]
            ~status:"Model listing unavailable; type an ID or retry" ()
      | Some `Cancelled | None -> () in
    Tui.choose screen ~allow_custom:true ~intro ~plain
      ~initial_status:"Loading available models…" ~wake_fd:read_fd
      ~on_wake ~title ~choices)
