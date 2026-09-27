module Workspace_lsp = Pave.Workspace_lsp
module Json = Yojson.Basic

let fail message = failwith message
let expect condition message = if not condition then fail message

let expect_error fn =
  match fn () with
  | _ -> fail "expected Workspace_lsp.Error"
  | exception Workspace_lsp.Error _ -> ()

let member name = function
  | `Assoc fields -> (try List.assoc name fields with Not_found -> `Null)
  | _ -> `Null

let write path content =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel content)

let read path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let frame message =
  let payload = Json.to_string message in
  Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length payload) payload

let decode_frame frame =
  let separator = "\r\n\r\n" in
  let index = Str.search_forward (Str.regexp_string separator) frame 0 in
  let header = String.sub frame 0 index in
  let payload_start = index + String.length separator in
  let length =
    match String.split_on_char '\n' header with
    | line :: _ ->
        let colon = String.index line ':' in
        int_of_string (String.trim (String.sub line (colon + 1) (String.length line - colon - 1)))
    | [] -> fail "missing client frame header" in
  expect (String.length frame - payload_start = length) "client Content-Length must match body bytes";
  Json.from_string (String.sub frame payload_start length)

let uri path =
  let buffer = Buffer.create (String.length path + 8) in
  Buffer.add_string buffer "file://";
  String.iter (fun ch ->
    match ch with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '/' | '-' | '_' | '.' | '~' | ':' -> Buffer.add_char buffer ch
    | _ -> Buffer.add_string buffer (Printf.sprintf "%%%02X" (Char.code ch))) path;
  Buffer.contents buffer

type fake_server = {
  lock : Mutex.t;
  ready : Condition.t;
  incoming : string Queue.t;
  mutable current : string option;
  mutable offset : int;
  mutable closed : bool;
  mutable terminated : bool;
  mutable client_messages : Json.t list;
  mutable rename_version : int;
  mutable rename_extra_uri : string option;
  fragment : int;
  file_uri : string;
}

let create_fake_server ~file_uri ~fragment = {
  lock = Mutex.create (); ready = Condition.create ();
  incoming = Queue.create (); current = None; offset = 0; closed = false; terminated = false;
  client_messages = []; rename_version = 1; rename_extra_uri = None;
  fragment; file_uri;
}

let send_wire server wire =
  Mutex.lock server.lock;
  Queue.add wire server.incoming;
  Condition.signal server.ready;
  Mutex.unlock server.lock

let send server message = send_wire server (frame message)

let response id result = `Assoc ["jsonrpc", `String "2.0"; "id", id; "result", result]

let method_name message = match member "method" message with
  | `String method_ -> Some method_
  | _ -> None

let handle_client_message server message =
  Mutex.lock server.lock;
  server.client_messages <- message :: server.client_messages;
  Mutex.unlock server.lock;
  match method_name message with
  | Some "initialize" ->
      let id = member "id" message in
      let server_request = `Assoc [
        "jsonrpc", `String "2.0"; "id", `String "server-request-9";
        "method", `String "workspace/applyEdit";
        "params", `Assoc ["edit", `Assoc []]] in
      let capabilities = `Assoc [
        "positionEncoding", `String "utf-16";
        "definitionProvider", `Bool true;
        "referencesProvider", `Bool true;
        "hoverProvider", `Bool true;
        "renameProvider", `Bool true;
        "codeActionProvider", `Bool true] in
      send_wire server (frame server_request ^ frame (response id (`Assoc ["capabilities", capabilities])))
  | Some "textDocument/didOpen" ->
      send server (`Assoc ["jsonrpc", `String "2.0";
        "method", `String "textDocument/publishDiagnostics";
        "params", `Assoc ["uri", `String server.file_uri; "version", `Int 1;
          "diagnostics", `List [`Assoc [
            "range", `Assoc ["start", `Assoc ["line", `Int 0; "character", `Int 0];
              "end", `Assoc ["line", `Int 0; "character", `Int 1]];
          "message", `String "diagnostic from fake server"; "severity", `Int 2]]]])
  | Some "textDocument/definition" ->
      let location = `Assoc ["uri", `String server.file_uri;
        "range", `Assoc ["start", `Assoc ["line", `Int 0; "character", `Int 0];
          "end", `Assoc ["line", `Int 0; "character", `Int 1]]] in
      send server (response (member "id" message) location)
  | Some "textDocument/references" ->
      send server (response (member "id" message) (`List []))
  | Some "textDocument/hover" -> ()
  | Some "textDocument/rename" ->
      let document = member "textDocument" (member "params" message) in
      let edits = `List [`Assoc [
        "range", `Assoc ["start", `Assoc ["line", `Int 0; "character", `Int 0];
          "end", `Assoc ["line", `Int 0; "character", `Int 1]];
        "newText", `String "Z"]] in
      let main_change = `Assoc [
        "textDocument", `Assoc ["uri", member "uri" document;
          "version", `Int server.rename_version]; "edits", edits] in
      let changes = main_change :: (match server.rename_extra_uri with
        | None -> []
        | Some uri -> [`Assoc [
            "textDocument", `Assoc ["uri", `String uri;
              "version", `Int server.rename_version]; "edits", edits]]) in
      let result = `Assoc ["documentChanges", `List changes] in
      send server (response (member "id" message) result)
  | Some "textDocument/codeAction" ->
      let document = member "textDocument" (member "params" message) in
      let edits = `List [`Assoc [
        "range", `Assoc ["start", `Assoc ["line", `Int 0; "character", `Int 0];
          "end", `Assoc ["line", `Int 0; "character", `Int 1]];
        "newText", `String "C"]] in
      let changes = `Assoc [
        "textDocument", `Assoc ["uri", member "uri" document;
          "version", `Int server.rename_version];
        "edits", edits] in
      let action = `Assoc ["title", `String "Use C";
        "kind", `String "quickfix";
        "edit", `Assoc ["documentChanges", `List [changes]]] in
      send server (response (member "id" message) (`List [action]))
  | Some "shutdown" -> send server (response (member "id" message) `Null)
  | Some _ | None -> ()

let fake_io server = {
  Workspace_lsp.read = (fun bytes offset length ->
    Mutex.lock server.lock;
    let rec available () =
      match server.current with
      | Some text when server.offset < String.length text -> ()
      | _ ->
          server.current <- None; server.offset <- 0;
          if Queue.is_empty server.incoming && not server.closed then Condition.wait server.ready server.lock;
          if Queue.is_empty server.incoming then ()
          else server.current <- Some (Queue.take server.incoming);
          if server.current = None && not server.closed then available ()
    in
    available ();
    let result = match server.current with
      | None -> 0
      | Some text ->
          let count = min length (min server.fragment (String.length text - server.offset)) in
          Bytes.blit_string text server.offset bytes offset count;
          server.offset <- server.offset + count;
          count in
    Mutex.unlock server.lock;
    result);
  write = (fun wire -> handle_client_message server (decode_frame wire));
  close = (fun () ->
    Mutex.lock server.lock; server.closed <- true; Condition.broadcast server.ready; Mutex.unlock server.lock);
  terminate = (fun () -> server.terminated <- true);
}

let messages server =
  Mutex.lock server.lock;
  let result = List.rev server.client_messages in
  Mutex.unlock server.lock;
  result

let has_method server expected =
  List.exists (fun message -> method_name message = Some expected) (messages server)

let with_root fn =
  let root = Filename.temp_file "pave-workspace-lsp-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let root = Unix.realpath root in
  let file = Filename.concat root "sample.ml" in
  write file "abc\n";
  Fun.protect ~finally:(fun () ->
    (try Sys.remove (Filename.concat root "other/second.ml") with _ -> ());
    (try Sys.remove file with _ -> ());
    (try Unix.rmdir (Filename.concat root "other") with _ -> ());
    Unix.rmdir root)
    (fun () -> fn root file)

let arguments ?(action = "definition") ?(language_id = "ocaml") ?(line = 0)
    ?(character = 0) ?(path = "sample.ml") extra =
  `Assoc (["action", `String action; "path", `String path;
    "language_id", `String language_id;
    "position", `Assoc ["line", `Int line; "character", `Int character]] @ extra)

let execute manager ~owner ~root ?(args = []) ?apply_approved ?cancel
    ?on_file_change arguments =
  Workspace_lsp.execute manager ~owner ~root ~program:"fake-lsp" ~args
    ?apply_approved ?cancel ?on_file_change arguments

let start manager ~owner ~root ?(args = []) ?(execution_approved = true) () =
  Workspace_lsp.start manager ~owner ~root ~program:"fake-lsp" ~args ~execution_approved

let () =
  with_root (fun root file ->
    Unix.mkdir (Filename.concat root "other") 0o700;
    let second_file = Filename.concat root "other/second.ml" in
    write second_file "xyz\n";
    let fake = create_fake_server ~file_uri:(uri file) ~fragment:1 in
    fake.rename_extra_uri <- Some (uri second_file);
    let launches = ref 0 and captured_environment = ref [] in
    let manager = Workspace_lsp.create_manager ~launcher:(fun ~program:_ ~arguments:_ ~cwd:_ ~environment ->
      incr launches;
      captured_environment := Array.to_list environment;
      fake_io fake) () in
    Fun.protect ~finally:(fun () -> Workspace_lsp.close_manager manager)
      (fun () ->
        expect_error (fun () -> execute manager ~owner:"session-a" ~root (arguments []));
        expect (!launches = 0) "document actions cannot start a server implicitly";
        expect_error (fun () -> start manager ~owner:"session-a" ~root ~execution_approved:false ());
        expect (!launches = 0) "server startup is blocked without explicit execution approval";
        start manager ~owner:"session-a" ~root ();
        expect (!launches = 1) "explicit start initializes exactly one persistent server";
        expect (!captured_environment =
          ["PATH=/usr/bin:/bin:/usr/sbin:/sbin"; "LANG=C"; "LC_ALL=C"; "TMPDIR=/tmp"])
          "the launcher receives only the minimal safe environment";
        expect_error (fun () -> start manager ~owner:"different-session" ~root ());
        expect_error (fun () -> execute manager ~owner:"different-session" ~root (arguments []));
        expect_error (fun () -> start manager ~owner:"session-a" ~root ~args:["--different"] ());
        expect_error (fun () -> execute manager ~owner:"session-a" ~root ~args:["--different"]
          (arguments []));
        start manager ~owner:"session-a" ~root ();
        expect (!launches = 1) "restarting the exact identity reuses its initialized server";
        let definition = execute manager ~owner:"session-a" ~root (arguments []) in
        expect (member "uri" definition = `String (uri file)) "definition must preserve the LSP location";
        expect (!launches = 1) "navigation reuses the explicitly started server";
        expect (has_method fake "initialized") "initialize must be followed by initialized";
        expect (has_method fake "textDocument/didOpen") "first document use sends didOpen";
        expect (has_method fake "textDocument/definition") "definition is sent to the semantic server";
        expect (List.exists (fun message ->
          member "id" message = `String "server-request-9" &&
          member "error" message <> `Null) (messages fake))
          "server requests receive an error correlated to their exact id";
        let diagnostics = execute manager ~owner:"session-a" ~root
          (arguments ~action:"diagnostics" []) in
        expect (member "diagnostics" diagnostics = `List [`Assoc [
          "range", `Assoc ["start", `Assoc ["line", `Int 0; "character", `Int 0];
            "end", `Assoc ["line", `Int 0; "character", `Int 1]];
          "message", `String "diagnostic from fake server"; "severity", `Int 2]])
          "interleaved publishDiagnostics is retained for the opened document";
        ignore (execute manager ~owner:"session-a" ~root
          (arguments ~action:"diagnostics" ~path:"other/second.ml" []));
        expect_error (fun () -> execute manager ~owner:"different-owner" ~root
          (arguments []));
        expect_error (fun () -> execute manager ~owner:"session-a" ~root ~args:["--other"]
          (arguments []));
        expect_error (fun () -> execute manager ~owner:"session-a"
          ~root:(Filename.concat root "other") (arguments []));
        expect (!launches = 1) "identity mismatches cannot start another server";

        let file_changes = ref [] in
        let on_file_change ~path ~before ~after =
          file_changes := (path, before, after) :: !file_changes in
        let code_actions = execute manager ~owner:"session-a" ~root
          (arguments ~action:"code_actions" []) in
        let code_action = match code_actions with
          | `List [action] -> action
          | _ -> fail "code actions should return the previewable action" in
        let code_previews = match member "previews" code_action with
          | `List [preview] -> preview
          | _ -> fail "code action should expose its proposed file contents" in
        expect (member "title" code_action = `String "Use C" &&
                member "content" code_previews = `String "Cbc\n" &&
                member "preview_id" code_action <> `Null)
          "code action returns an unapplied exact-content preview ID";
        expect (read file = "abc\n" && !file_changes = [])
          "code-action preview never writes or records files";

        let preview = execute manager ~owner:"session-a" ~root ~on_file_change
          (arguments ~action:"rename" ["new_name", `String "changed"]) in
        let preview_item, second_preview = match preview with
          | `List [first; second] -> first, second
          | _ -> fail "rename should return previews for both files" in
        let preview_id = match member "preview_id" preview_item with
          | `String preview_id -> preview_id
          | _ -> fail "rename preview must have an approval-bound ID" in
        expect (member "path" preview_item = `String "sample.ml" &&
                member "version" preview_item = `Int 1 &&
                member "content" preview_item = `String "Zbc\n" &&
                member "changed" preview_item = `Bool true)
          "unapproved rename returns a versioned preview";
        expect (member "path" second_preview = `String "other/second.ml" &&
                member "content" second_preview = `String "Zyz\n" &&
                member "preview_id" second_preview = `String preview_id)
          "multi-file rename previews share one approval-bound ID in server order";
        expect (!file_changes = []) "edit previews never notify file changes";
        expect (match member "original_sha256" preview_item,
                     member "result_sha256" preview_item with
          | `String before, `String after ->
              String.length before = 64 && String.length after = 64 && before <> after
          | _ -> false) "edit previews carry before and after snapshot hashes";
        expect (read file = "abc\n" && read second_file = "xyz\n")
          "unapproved edits never write workspace files";
        let title, details = Workspace_lsp.preview_details manager
          ~owner:"session-a" ~root ~program:"fake-lsp" ~arguments:[] preview_id in
        let details_match = match details, preview with
          | `List cached, `List displayed
            when List.length cached = List.length displayed ->
              List.for_all2 (fun cached displayed ->
                match cached, displayed with
                | `Assoc cached_fields, `Assoc displayed_fields ->
                    cached_fields = List.filter
                      (fun (name, _) -> name <> "preview_id") displayed_fields
                | _ -> false) cached displayed
          | _ -> false in
        expect (title = "Rename to changed" && details_match)
          "cached approval details preserve exact preview contents";

        expect_error (fun () -> execute manager ~owner:"session-a" ~root
          ~apply_approved:true ~on_file_change
          (arguments ~action:"rename" ["new_name", `String "changed"]));
        expect (read file = "abc\n" && read second_file = "xyz\n")
          "approval cannot authorize a fresh, unpreviewed rename request";
        expect_error (fun () -> execute manager ~owner:"session-a" ~root
          ~on_file_change (arguments ~action:"apply_preview"
            ["preview_id", `String preview_id]));
        expect (read file = "abc\n" && read second_file = "xyz\n")
          "preview application without explicit approval has no effect";

        write second_file "concurrent user edit\n";
        expect_error (fun () -> execute manager ~owner:"session-a" ~root
          ~apply_approved:true ~on_file_change
          (arguments ~action:"apply_preview" ["preview_id", `String preview_id]));
        expect (!file_changes = [] && read file = "abc\n" &&
                read second_file = "concurrent user edit\n")
          "stale multi-file preview fails before writing any target";
        write second_file "xyz\n";
        fake.rename_version <- 99;
        ignore (execute manager ~owner:"session-a" ~root ~apply_approved:true ~on_file_change
          (arguments ~action:"apply_preview" ["preview_id", `String preview_id]));
        expect (read file = "Zbc\n" && read second_file = "Zyz\n")
          "approved cached contents apply without rerunning the language server";
        expect (List.rev !file_changes =
          ["sample.ml", "abc\n", "Zbc\n"; "other/second.ml", "xyz\n", "Zyz\n"])
          "each changed atomic write notifies once in edit order with full contents";
        expect_error (fun () -> execute manager ~owner:"session-a" ~root
          ~apply_approved:true
          (arguments ~action:"apply_preview" ["preview_id", `String preview_id]));
        expect (read file = "Zbc\n" && read second_file = "Zyz\n")
          "a cached edit preview can be applied only once";
        ignore (execute manager ~owner:"session-a" ~root (arguments []));
        expect (List.exists (fun message ->
          method_name message = Some "textDocument/didChange" &&
          member "version" (member "textDocument" (member "params" message)) = `Int 2)
          (messages fake)) "external document changes advance the LSP version";
        fake.rename_version <- 1;
        expect_error (fun () -> execute manager ~owner:"session-a" ~root
          (arguments ~action:"rename" ["new_name", `String "changed"]));
        expect (read file = "Zbc\n") "a response for a stale document version is rejected";

        ignore (execute manager ~owner:"session-a" ~root
          (arguments ~action:"shutdown" []));
        expect (has_method fake "shutdown") "shutdown is sent before disposal";
        expect (has_method fake "exit") "exit follows the shutdown response";
        expect (has_method fake "textDocument/didClose")
          "disposing the session closes every opened document";
        expect fake.terminated "disposing the manager terminates the injected process";
        expect_error (fun () -> execute manager ~owner:"session-a" ~root
          (arguments []))));

  with_root (fun root _file ->
    let fake = create_fake_server ~file_uri:(uri (Filename.concat root "sample.ml")) ~fragment:3 in
    let manager = Workspace_lsp.create_manager ~launcher:(fun ~program:_ ~arguments:_ ~cwd:_ ~environment:_ -> fake_io fake) () in
    Fun.protect ~finally:(fun () -> Workspace_lsp.close_manager manager)
      (fun () ->
        start manager ~owner:"dispose-session" ~root ();
        ignore (execute manager ~owner:"dispose-session" ~root
          (arguments ~action:"diagnostics" []));
        Workspace_lsp.close_manager manager;
        expect (has_method fake "shutdown" && has_method fake "exit")
          "explicit manager disposal performs shutdown and exit";
        expect fake.terminated "explicit disposal closes the process transport"));

  with_root (fun root _file ->
    let fake = create_fake_server ~file_uri:(uri (Filename.concat root "sample.ml")) ~fragment:7 in
    let manager = Workspace_lsp.create_manager ~launcher:(fun ~program:_ ~arguments:_ ~cwd:_ ~environment:_ -> fake_io fake) () in
    let cancelled = ref 0 in
    start manager ~owner:"cancel-session" ~root ();
    Fun.protect ~finally:(fun () -> Workspace_lsp.close_manager manager)
      (fun () ->
        expect_error (fun () -> execute manager ~owner:"cancel-session" ~root
          ~cancel:(fun () -> incr cancelled; !cancelled >= 4)
          (arguments ~action:"hover" []));
        expect (has_method fake "$/cancelRequest")
          "cancelling a pending request sends LSP $/cancelRequest"));
  print_endline "workspace LSP: ok"
