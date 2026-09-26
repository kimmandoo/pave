(* Discovery runs on a cancellable worker; unclassified listings never become
   verified model suggestions in the chooser. *)
let credential (descriptor : Pave.Provider_catalog.descriptor) =
  match descriptor.id with
  | "openai" | "google" | "anthropic" | "deepseek" | "groq" | "mistral"
  | "together" | "cerebras" | "venice" | "deepinfra" | "fireworks"
  | "baseten" | "huggingface" | "nanogpt" | "aimlapi" | "aiand"
  | "sakana" | "abliteration" | "gmi-cloud" | "moonshot" | "xai" | "nvidia"
  | "novita" | "siliconflow" | "siliconflow-cn" | "ollama-cloud"
  | "bedrock-mantle" | "stepfun" | "coreweave" | "synthetic"
  | "zenmux" | "wafer-serverless" | "qianfan" | "xiaomi" | "kilo"
  | "singularityapi-dev" | "opencode-zen" | "opencode-go"
  | "yolo-auto"
  | "charm-hyper" | "meta" | "vercel-ai-gateway" | "commandcode"
  | "lm-studio" | "llama.cpp" | "vllm" ->
      Option.map (fun key -> Pave.Model_discovery.Api_key key)
        (Cli_auth.api_key descriptor)
  | "devin" ->
      (match descriptor.routes with
      | [] -> None
      | route :: _ ->
          let _, key, resolve = Cli_auth.resolve_authentication
            ~descriptor ~route ~endpoint:route.endpoint in
          if key <> "" then Some (Pave.Model_discovery.Api_key key)
          else Option.map (fun resolve ->
            let (credential : Pave.Provider.credentials) = resolve () in
            Pave.Model_discovery.Api_key credential.access) resolve)
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
let supports_route (descriptor : Pave.Provider_catalog.descriptor)
    (model : Pave.Model_discovery.model)
    (route : Pave.Provider_catalog.route) =
  Pave.Model_discovery.model_supports_endpoint ~provider:descriptor.id model
    ~endpoint:route.endpoint

let model_detail (descriptor : Pave.Provider_catalog.descriptor)
    (model : Pave.Model_discovery.model) =
  let display_name = Option.map
    (fun value -> "display name " ^ value) model.name in
  let context = Option.map
    (fun tokens -> Printf.sprintf "context %d tokens" tokens)
    model.context_window_tokens in
  let endpoints = match model.supported_endpoints with
    | None -> None
    | Some _ ->
        let names = List.filter_map
          (fun (route : Pave.Provider_catalog.route) ->
            if Pave.Model_discovery.model_supports_endpoint
                ~provider:descriptor.id model ~endpoint:route.endpoint
            then Some route.name else None) descriptor.routes in
        if names = [] then None
        else Some ("APIs " ^ String.concat "/" names) in
  let tokenizer = Option.map
    (Printf.sprintf "provider tokenizer type %s; metadata only")
    model.provider_tokenizer in
  let compaction = match model.native_compaction_supported with
    | Some true -> Some "native compaction supported (provider-reported)"
    | Some false -> Some "native compaction not supported (provider-reported)"
    | None -> None in
  match List.filter_map Fun.id
    [context; compaction; display_name; endpoints; tokenizer] with
  | [] -> None
  | parts -> Some (String.concat " · " parts)

let model_details (descriptor : Pave.Provider_catalog.descriptor) models =
  List.filter_map (fun (model : Pave.Model_discovery.model) ->
    Option.map (fun detail -> descriptor.id ^ "/" ^ model.id, detail)
      (model_detail descriptor model)) models


let choose screen ~(descriptor : Pave.Provider_catalog.descriptor) ?(intro = [])
    ?(plain = []) ?route_name ~title ~choices () =
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
      | Some (`Listing (Ok listing)) ->
          let selected = Option.value ~default:descriptor.default_route route_name in
          let route = Pave.Provider_catalog.route descriptor selected in
          let routed_models = match route with
            | None -> []
            | Some route -> List.filter
                (fun model -> supports_route descriptor model route) listing.models in
          let routed = List.map
            (fun (model : Pave.Model_discovery.model) ->
              descriptor.id ^ "/" ^ model.id) routed_models in
          let details = model_details descriptor routed_models in
          if Pave.Provider_catalog.unclassified_models descriptor.id then
            Tui.update_choices screen ~verified:[] ~listed:routed ~details
              ~status:(if routed = [] && listing.models <> [] then
                descriptor.id ^ ": choose an explicit API before selecting a model"
              else Printf.sprintf
                "%s: %d listed IDs · Chat/tool compatibility unverified"
                descriptor.id (List.length routed)) ()
          else
            let excluded = List.length listing.models - List.length routed_models in
            Tui.update_choices screen ~verified:routed ~details
              ~status:(Printf.sprintf "%s: %d live routable model%s%s"
                descriptor.display_name (List.length routed)
                (if List.length routed = 1 then "" else "s")
                (if excluded = 0 then "" else
                   Printf.sprintf " · %d don't advertise %s"
                     excluded selected))
              ()
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

(* The conversation picker aggregates account-bound listings. It never tests a
   provider by sending credentials to a guessed host: each discovery adapter
   owns its pinned endpoint. Subscription logins without a documented listing
   remain manual rather than presenting fabricated model IDs. *)
let connected (descriptor : Pave.Provider_catalog.descriptor) =
  descriptor.id = "ollama" ||
  Option.is_some (Cli_auth.api_key descriptor) ||
  (descriptor.oauth <> None &&
   try Pave.Oauth_store.get ~path:(Pave.Oauth_store.default_path ())
     ~provider:descriptor.id <> None
   with Pave.Oauth_store.Storage_error _ -> false)

let choose_all screen ~(active : Pave.Provider_catalog.descriptor)
    ~current_route () =
  let providers = Pave.Provider_catalog.all () in
  let providers = active :: List.filter
    (fun (entry : Pave.Provider_catalog.descriptor) -> entry.id <> active.id)
    providers in
  let sources = List.filter connected providers in
  let read_fd, write_fd = Unix.pipe () in
  Unix.set_close_on_exec read_fd;
  Unix.set_close_on_exec write_fd;
  let cancelled = Atomic.make false in
  let lock = Mutex.create () in
  let pending = ref sources and outcomes = ref [] in
  let take () =
    Mutex.lock lock;
    let next = match !pending with
      | [] -> None
      | descriptor :: rest ->
          pending := rest;
          Some descriptor in
    Mutex.unlock lock;
    next in
  let rec work () =
    if not (Atomic.get cancelled) then
      match take () with
      | None -> ()
      | Some descriptor ->
          let result = try
            let access = credential descriptor in
            `Listing (Pave.Model_discovery.discover
              ~cancel:(fun () -> Atomic.get cancelled)
              ~provider:descriptor.id ?credential:access ())
          with Pave.Provider.Cancelled -> `Cancelled
             | _ -> `Unavailable in
          Mutex.lock lock;
          outcomes := (descriptor, result) :: !outcomes;
          Mutex.unlock lock;
          (try ignore (Unix.write_substring write_fd "x" 0 1)
           with Unix.Unix_error _ -> ());
          work () in
  let workers = List.init (min 4 (List.length sources))
    (fun _ -> Thread.create work ()) in
  let completed = Hashtbl.create (List.length sources) in
  Fun.protect ~finally:(fun () ->
    Atomic.set cancelled true;
    List.iter Thread.join workers;
    Unix.close write_fd;
    Unix.close read_fd) (fun () ->
    let on_wake () =
      let markers = Bytes.create 256 in
      ignore (Unix.read read_fd markers 0 (Bytes.length markers));
      Mutex.lock lock;
      let ready = !outcomes in
      outcomes := [];
      Mutex.unlock lock;
      List.iter (fun ((descriptor : Pave.Provider_catalog.descriptor), result) ->
        Hashtbl.replace completed descriptor.id result) ready;
      let verified, listed, details = List.fold_left
        (fun (verified, listed, details)
            (descriptor : Pave.Provider_catalog.descriptor) ->
          match Hashtbl.find_opt completed descriptor.id with
          | Some (`Listing (Ok listing)) ->
              let selected_route = if descriptor.id = active.id
                then current_route else descriptor.default_route in
              (match Pave.Provider_catalog.route descriptor selected_route with
               | None -> verified, listed, details
               | Some route ->
                   let models = List.filter
                     (fun model -> supports_route descriptor model route) listing.models in
                   let items = List.map
                     (fun (model : Pave.Model_discovery.model) ->
                       descriptor.id ^ "/" ^ model.id) models in
                   let details = model_details descriptor models :: details in
                   if Pave.Provider_catalog.unclassified_models descriptor.id then
                     verified, items :: listed, details
                   else items :: verified, listed, details)
          | _ -> verified, listed, details) ([], [], []) sources in
      let verified = List.concat (List.rev verified) in
      let listed = List.concat (List.rev listed) in
      let details = List.concat (List.rev details) in
      let checked = Hashtbl.length completed in
      let notes = List.filter_map (fun (descriptor : Pave.Provider_catalog.descriptor) ->
        match Hashtbl.find_opt completed descriptor.id with
        | Some (`Listing (Error Pave.Model_discovery.Missing_credential))
          when descriptor.id = "anthropic" && Cli_auth.api_key descriptor = None ->
            Some "Anthropic OAuth: listing needs ANTHROPIC_API_KEY; type an ID"
        | Some (`Listing (Error error)) ->
            Some (descriptor.id ^ ": " ^ Pave.Model_discovery.message error)
        | Some (`Listing (Ok listing)) when listing.models <> [] ->
            (match Pave.Provider_catalog.route descriptor
              (if descriptor.id = active.id then current_route
               else descriptor.default_route) with
             | None ->
                 Some (descriptor.id ^ ": choose an explicit API with /model provider@API/ID")
             | Some route ->
                 let compatible = List.filter
                   (fun model -> supports_route descriptor model route) listing.models in
                 if compatible = [] then
                   Some (descriptor.id ^ ": no listed model advertises " ^ route.name)
                 else if List.length compatible < List.length listing.models then
                   Some (Printf.sprintf "%s: %d listed model%s not available on %s"
                     descriptor.id
                     (List.length listing.models - List.length compatible)
                     (if List.length listing.models - List.length compatible = 1 then "" else "s")
                     route.name)
                 else None)
        | _ -> None) sources in
      let note = match notes with
        | [] -> ""
        | first :: rest ->
            " · " ^ first ^ (if rest = [] then "" else
              Printf.sprintf " (+%d more listing notices)" (List.length rest)) in
      let status = Printf.sprintf "%d models · %d/%d providers checked%s"
        (List.length verified + List.length listed) checked (List.length sources)
        note in
      Tui.update_choices screen ~verified ~listed ~details ~status () in
    Tui.choose screen ~allow_custom:true
      ~intro:["Account models appear together as provider/model IDs.";
        "A signed-in provider without a public listing needs a typed ID.";
        "Switching here changes this conversation, not saved defaults."]
      ~initial_status:(Printf.sprintf "Discovering %d connected provider%s…"
        (List.length sources) (if List.length sources = 1 then "" else "s"))
      ~wake_fd:read_fd ~on_wake
      ~title:"Models · connected providers" ~choices:[])
