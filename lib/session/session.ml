type kind =
  | Message of Protocol.message
  | Compaction of { summary : string; first_kept_id : string }
  | Model of { provider : string; model : string; api : string option }
  | Usage of { provider : string; model : string; tokens : Protocol.usage }
  | Branch
type entry = { id : string; parent_id : string option; timestamp : string; kind : kind }

type t = {
  path : string;
  header : Yojson.Basic.t;
  mutable records_rev : entry list;
  by_id : (string, entry) Hashtbl.t;
  mutable leaf : string option;
  mutable disk_size : int;
}

let invalid text = raise (Protocol.Invalid_response ("invalid session journal: " ^ text))

let fresh_id () =
  let bytes = Bytes.create 16 in
  let fd = Unix.openfile "/dev/urandom" [ Unix.O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let rec fill offset =
      if offset < Bytes.length bytes then (
        let n = Unix.read fd bytes offset (Bytes.length bytes - offset) in
        if n = 0 then failwith "could not generate session ID";
        fill (offset + n)) in
    fill 0);
  let hex = "0123456789abcdef" in
  let result = Bytes.create 32 in
  for i = 0 to 15 do
    let byte = Char.code (Bytes.get bytes i) in
    Bytes.set result (2 * i) hex.[byte lsr 4];
    Bytes.set result (2 * i + 1) hex.[byte land 15]
  done;
  Bytes.unsafe_to_string result

let timestamp () =
  let time = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (time.Unix.tm_year + 1900) (time.Unix.tm_mon + 1) time.Unix.tm_mday
    time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec

let option_json = function None -> `Null | Some text -> `String text

let new_header cwd =
  `Assoc [ "type", `String "session"; "version", `Int 1;
           "id", `String (fresh_id ()); "timestamp", `String (timestamp ());
           "cwd", `String cwd ]

let entry_json entry =
  let fields = [ "type", `String (match entry.kind with
    | Message _ -> "message" | Compaction _ -> "compaction"
    | Model _ -> "model" | Usage _ -> "usage" | Branch -> "branch");
    "id", `String entry.id; "parentId", option_json entry.parent_id;
    "timestamp", `String entry.timestamp ] in
  match entry.kind with
  | Message message -> `Assoc (fields @ [
      "message", Protocol.message_to_json ~stored:true message ])
  | Compaction { summary; first_kept_id } ->
      `Assoc (fields @ [ "summary", `String summary;
                         "firstKeptEntryId", `String first_kept_id ])
  | Model { provider; model; api } ->
      `Assoc (fields @ ["provider", `String provider; "model", `String model] @
        (match api with None -> [] | Some api -> ["api", `String api]))
  | Usage { provider; model; tokens } ->
      `Assoc (fields @ ["provider", `String provider; "model", `String model;
        "inputTokens", `Int tokens.input_tokens;
        "outputTokens", `Int tokens.output_tokens])
  | Branch -> `Assoc fields

let write_all fd text =
  let rec loop offset =
    if offset < String.length text then (
      let n = Unix.write_substring fd text offset (String.length text - offset) in
      if n = 0 then failwith "could not write session journal";
      loop (offset + n)) in
  loop 0

let line json = Yojson.Basic.to_string json ^ "\n"

let write_new_file path content =
  let temp = Filename.temp_file ~temp_dir:(Filename.dirname path) ".pave-journal-" ".tmp" in
  Fun.protect ~finally:(fun () -> try Unix.unlink temp with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
    (fun () ->
      let fd = Unix.openfile temp [ Unix.O_WRONLY; Unix.O_TRUNC ] 0 in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
        Unix.fchmod fd 0o600;
        write_all fd content;
        Unix.fsync fd);
      Unix.link temp path)

let append_line t json =
  let fd = Unix.openfile t.path [ Unix.O_WRONLY; Unix.O_APPEND ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    Unix.lockf fd Unix.F_LOCK 0;
    Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) (fun () ->
      if (Unix.fstat fd).Unix.st_size <> t.disk_size then
        failwith "session changed on disk; reopen before writing";
      write_all fd (line json);
      Unix.fsync fd;
      t.disk_size <- (Unix.fstat fd).Unix.st_size))

let valid_model_field text =
  text <> "" && not (String.exists (fun char ->
    Char.code char <= 32 || Char.code char = 127) text)

let parse_entry json =
  let get key = Protocol.member key json in
  let id = match get "id" with `String value when value <> "" -> value
    | _ -> invalid "entry ID missing" in
  let parent_id = match get "parentId" with `String value -> Some value
    | `Null -> None | _ -> invalid "invalid parent ID" in
  let timestamp = match get "timestamp" with `String value -> value
    | _ -> invalid "entry timestamp missing" in
  let kind = match get "type" with
    | `String "message" -> Message (Protocol.message_from_json (get "message"))
    | `String "compaction" ->
        (match get "summary", get "firstKeptEntryId" with
         | `String summary, `String first_kept_id
           when String.trim summary <> "" && first_kept_id <> "" ->
             Compaction { summary; first_kept_id }
         | _ -> invalid "invalid compaction")
    | `String "model" ->
        (match get "provider", get "model", get "api" with
         | `String provider, `String model, (`Null | `String _ as api)
           when valid_model_field provider && valid_model_field model &&
             (match api with
              | `Null -> true
              | `String name -> valid_model_field name
              | _ -> false) ->
             Model { provider; model;
               api = (match api with `String name -> Some name | _ -> None) }
         | _ -> invalid "invalid model selection")
    | `String "usage" ->
        (match get "provider", get "model",
          get "inputTokens", get "outputTokens" with
         | `String provider, `String model, `Int input_tokens, `Int output_tokens
           when valid_model_field provider && valid_model_field model &&
             input_tokens >= 0 && output_tokens >= 0 ->
             Usage { provider; model; tokens = { input_tokens; output_tokens } }
         | _ -> invalid "invalid provider token usage")
    | `String "branch" -> Branch
    | _ -> invalid "unsupported journal entry type" in
  { id; parent_id; timestamp; kind }

let branch_entries t =
  let rec walk id items =
    match id with
    | None -> items
    | Some id ->
        let entry = try Hashtbl.find t.by_id id
          with Not_found -> invalid ("missing parent entry: " ^ id) in
        walk entry.parent_id (entry :: items) in
  walk t.leaf []

let entries t = List.rev t.records_rev
let leaf_id t = t.leaf
let model_at t leaf =
  let rec find = function
    | None -> None
    | Some id ->
        let entry = try Hashtbl.find t.by_id id
          with Not_found -> invalid ("missing parent entry: " ^ id) in
        match entry.kind with
        | Model { provider; model; _ } -> Some (provider, model)
        | Message _ | Compaction _ | Usage _ | Branch -> find entry.parent_id in
  find leaf
let model t = model_at t t.leaf
let api_at t leaf =
  let rec find = function
    | None -> None
    | Some id ->
        let entry = try Hashtbl.find t.by_id id
          with Not_found -> invalid ("missing parent entry: " ^ id) in
        match entry.kind with
        | Model { api; _ } -> api
        | Message _ | Compaction _ | Usage _ | Branch -> find entry.parent_id in
  find leaf
let api t = api_at t t.leaf
let usage t =
  List.fold_left (fun total entry -> match entry.kind with
    | Usage { tokens; _ } ->
        (match total with
         | None -> Some tokens
         | Some previous -> Some (Protocol.add_usage previous tokens))
    | Message _ | Compaction _ | Model _ | Branch -> total)
    None (branch_entries t)
module Usage_models = Map.Make (struct
  type t = string * string
  let compare = Stdlib.compare
end)

let usage_by_model t =
  let models = List.fold_left (fun models entry -> match entry.kind with
    | Usage { provider; model; tokens } ->
        let key = provider, model in
        Usage_models.update key (function
          | None -> Some tokens
          | Some previous -> Some (Protocol.add_usage previous tokens)) models
    | Message _ | Compaction _ | Model _ | Branch -> models)
    Usage_models.empty (branch_entries t) in
  Usage_models.bindings models
let messages entries =
  List.filter_map (fun entry -> match entry.kind with
    | Message message -> Some message
    | Compaction _ | Model _ | Usage _ | Branch -> None) entries

let history t = messages (branch_entries t)
let retryable_history history =
  let rec find safe = function
    | [] -> None
    | (message : Protocol.message) :: earlier ->
        (match message.role, message.content, message.tool_calls with
         | "user", Some text, [] when safe && String.trim text <> "" ->
             Some (List.rev earlier, text)
         | "user", _, _ -> None
         | "assistant", _, [] -> find safe earlier
         | _ -> find false earlier) in
  find true (List.rev history)

let retry_candidate t =
  let rec find = function
    | [] -> None
    | { kind = Message { role = "user"; content = Some text; _ };
        parent_id = Some parent; _ } :: _ when String.trim text <> "" ->
        Some (parent, text)
    | { kind = Message { role = "assistant"; tool_calls = []; _ }; _ } :: rest
    | { kind = Usage _; _ } :: rest -> find rest
    | _ -> None in
  find (List.rev (branch_entries t))

let context t =
  let path = branch_entries t in
  let latest = List.fold_left (fun found entry -> match entry.kind with
    | Compaction { summary; first_kept_id } -> Some (entry.id, summary, first_kept_id)
    | Message _ | Model _ | Usage _ | Branch -> found) None path in
  match latest with
  | None -> messages path
  | Some (marker_id, summary, first_kept_id) ->
      let rec split before = function
        | [] -> invalid "compaction marker missing from branch"
        | entry :: after when entry.id = marker_id -> List.rev before, after
        | entry :: rest -> split (entry :: before) rest in
      let before, after = split [] path in
      let rec kept = function
        | [] -> invalid "compaction boundary missing from branch"
        | entry :: rest when entry.id = first_kept_id -> entry :: rest
        | _ :: rest -> kept rest in
      Protocol.user summary :: messages (kept before @ after)

let compaction_plan t =
  let path = branch_entries t in
  let rec last_user candidate = function
    | [] -> candidate
    | { id; kind = Message { role = "user"; _ }; _ } :: rest ->
        last_user (Some id) rest
    | _ :: rest -> last_user candidate rest in
  match last_user None path with
  | None -> invalid "nothing to compact"
  | Some first_kept_id ->
      let rec before_last_user = function
        | [] -> invalid "compaction boundary missing from context"
        | message :: prefix when message.Protocol.role = "user" -> List.rev prefix
        | _ :: rest -> before_last_user rest in
      let prefix = before_last_user (List.rev (context t)) in
      if List.length prefix < 2 then invalid "nothing to compact";
      first_kept_id, prefix

let missing_results messages =
  let pending = List.fold_left (fun pending (msg : Protocol.message) ->
    match msg.role with
    | "assistant" ->
        if pending <> [] then invalid "assistant before outstanding tool results";
        List.map (fun (call : Protocol.tool_call) -> call.id) msg.tool_calls
    | "tool" ->
        (match msg.tool_call_id with
        | Some id when List.mem id pending -> List.filter ((<>) id) pending
        | _ -> invalid "orphan tool result")
    | "user" ->
        if pending <> [] then invalid "user before outstanding tool results";
        []
    | _ -> invalid "unsupported transcript role") [] messages in
  List.map (fun id -> Protocol.tool_result id
    "Error: previous process stopped before this tool result; do not assume it executed") pending

let compact t ~summary ~first_kept_id =
  if String.trim summary = "" then invalid "empty compaction summary";
  let planned_id, _ = compaction_plan t in
  if planned_id <> first_kept_id then invalid "compaction must retain the latest user turn";
  if missing_results (history t) <> [] then invalid "unresolved tool results";
  let entry = { id = fresh_id (); parent_id = t.leaf; timestamp = timestamp ();
                kind = Compaction { summary; first_kept_id } } in
  append_line t (entry_json entry);
  t.records_rev <- entry :: t.records_rev;
  Hashtbl.add t.by_id entry.id entry;
  t.leaf <- Some entry.id;
  entry.id

let append t message =
  (match message.Protocol.role, message.content, message.tool_calls, message.tool_call_id with
   | "user", Some _, [], None | "assistant", _, _, None
   | "tool", Some _, [], Some _ -> ()
   | _ -> invalid "unsupported message");
  let entry = { id = fresh_id (); parent_id = t.leaf; timestamp = timestamp ();
                kind = Message message } in
  append_line t (entry_json entry);
  t.records_rev <- entry :: t.records_rev;
  Hashtbl.add t.by_id entry.id entry;
  t.leaf <- Some entry.id;
  entry.id

let set_model ?api t ~provider ~model:selected =
  if not (valid_model_field provider && valid_model_field selected) ||
     not (Option.fold ~none:true ~some:valid_model_field api) then
    invalid "invalid model selection";
  let selection = Model { provider; model = selected; api } in
  if model t <> Some (provider, selected) || api_at t t.leaf <> api then (
    let entry = { id = fresh_id (); parent_id = t.leaf;
      timestamp = timestamp (); kind = selection } in
    append_line t (entry_json entry);
    t.records_rev <- entry :: t.records_rev;
    Hashtbl.add t.by_id entry.id entry;
    t.leaf <- Some entry.id)

let append_usage t ~provider ~model (tokens : Protocol.usage) =
  if not (valid_model_field provider && valid_model_field model) ||
    tokens.input_tokens < 0 || tokens.output_tokens < 0 then
    invalid "invalid provider token usage";
  let entry = { id = fresh_id (); parent_id = t.leaf;
    timestamp = timestamp (); kind = Usage { provider; model; tokens } } in
  append_line t (entry_json entry);
  t.records_rev <- entry :: t.records_rev;
  Hashtbl.add t.by_id entry.id entry;
  t.leaf <- Some entry.id

let branch t id =
  if not (Hashtbl.mem t.by_id id) then invalid ("entry not found: " ^ id);
  let marker = { id = fresh_id (); parent_id = Some id; timestamp = timestamp ();
                 kind = Branch } in
  append_line t (entry_json marker);
  t.records_rev <- marker :: t.records_rev;
  Hashtbl.add t.by_id marker.id marker;
  t.leaf <- Some id;
  List.iter (fun message -> ignore (append t message))
    (missing_results (history t))

let load_journal path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    let size = in_channel_length ic in
    if size = 0 then invalid "empty file";
    seek_in ic (size - 1);
    if input_char ic <> '\n' then invalid "truncated last entry";
    seek_in ic 0;
    let read_json () =
      try Yojson.Basic.from_string (input_line ic)
      with Yojson.Json_error _ -> invalid "invalid JSONL entry" in
    let header = read_json () in
    if Protocol.member "type" header <> `String "session"
      || Protocol.member "version" header <> `Int 1 then invalid "unsupported session header";
    (match Protocol.member "id" header, Protocol.member "timestamp" header,
      Protocol.member "cwd" header with
     | `String id, `String _, `String _ when id <> "" -> ()
     | _ -> invalid "incomplete session header");
    let records = ref [] in
    let by_id = Hashtbl.create 32 in
    let seen_ids = Hashtbl.create 32 in
    let leaf = ref None in
    (try while true do
      let entry = parse_entry (read_json ()) in
      if Hashtbl.mem seen_ids entry.id then invalid "duplicate entry ID";
      Hashtbl.add seen_ids entry.id ();
      (match entry.parent_id with
       | Some parent when not (Hashtbl.mem by_id parent) ->
           invalid ("entry has missing parent: " ^ parent)
       | _ -> ());
      (match entry.kind with
       | Message _ | Model _ | Usage _ ->
           Hashtbl.add by_id entry.id entry; leaf := Some entry.id
       | Compaction { first_kept_id; _ } ->
           let rec ancestor = function
             | None -> false
             | Some id when id = first_kept_id ->
                 (match (Hashtbl.find by_id id).kind with
                  | Message { role = "user"; _ } -> true | _ -> false)
             | Some id -> ancestor (Hashtbl.find by_id id).parent_id in
           if not (ancestor entry.parent_id) then
             invalid "compaction boundary is not an ancestor user entry";
           Hashtbl.add by_id entry.id entry; leaf := Some entry.id
       | Branch ->
           Hashtbl.add by_id entry.id entry;
           leaf := entry.parent_id);
      records := entry :: !records
    done with End_of_file -> ());
    { path; header; records_rev = !records; by_id; leaf = !leaf;
      disk_size = size })

let migrate_legacy ~cwd path =
  let fd = Unix.openfile path [ Unix.O_RDWR ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    Unix.lockf fd Unix.F_LOCK 0;
    Fun.protect ~finally:(fun () -> Unix.lockf fd Unix.F_ULOCK 0) (fun () ->
      let opened = Unix.fstat fd and current = Unix.stat path in
      if opened.Unix.st_ino = current.Unix.st_ino
        && opened.Unix.st_dev = current.Unix.st_dev then (
        let ic = Unix.in_channel_of_descr (Unix.dup fd) in
        let legacy = Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
          if input_char ic = '[' then (
            seek_in ic 0;
            Some (Yojson.Basic.from_channel ic))
          else None) in
        match legacy with
        | None -> ()
        | Some legacy ->
            let messages = match legacy with
              | `List values -> List.map Protocol.message_from_json values
              | _ -> invalid "legacy session is not a JSON array" in
            let header = new_header cwd in
            let parent = ref None in
            let records = List.map (fun message ->
              let entry = { id = fresh_id (); parent_id = !parent;
                            timestamp = timestamp (); kind = Message message } in
              parent := Some entry.id;
              entry) messages in
            let body = line header ^ String.concat ""
              (List.map (fun entry -> line (entry_json entry)) records) in
            let temp = Filename.temp_file ~temp_dir:(Filename.dirname path)
              ".pave-migrate-" ".tmp" in
            Fun.protect ~finally:(fun () ->
              try Unix.unlink temp with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
              (fun () ->
                let dest = Unix.openfile temp [ Unix.O_WRONLY; Unix.O_TRUNC ] 0 in
                Fun.protect ~finally:(fun () -> Unix.close dest) (fun () ->
                  Unix.fchmod dest 0o600;
                  write_all dest body;
                  Unix.fsync dest);
                Unix.rename temp path))))

let rec open_file ?(cwd = Unix.getcwd ()) path =
  let exists = try
    let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
    Unix.close fd; true
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false in
  if not exists then (
    let header = new_header cwd in
    (try write_new_file path (line header)
     with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    open_file ~cwd path)
  else (
    let ic = open_in_bin path in
    let first = Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
      try Some (input_char ic) with End_of_file -> None) in
    if first = Some '[' then (
      migrate_legacy ~cwd path;
      open_file ~cwd path)
    else (
      let session = load_journal path in
      List.iter (fun message -> ignore (append session message))
        (missing_results (history session));
      session))

let fork session path =
  let header = new_header (match Protocol.member "cwd" session.header with
    | `String cwd -> cwd | _ -> Unix.getcwd ()) in
  let body = line header ^ String.concat "" (List.map (fun entry -> line (entry_json entry))
    (branch_entries session)) in
  write_new_file path body;
  open_file path
