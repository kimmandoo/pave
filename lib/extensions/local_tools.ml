(* User-installed executable tools are never project instructions. A registry is an
   immutable, validated snapshot; a session owns its callbacks and cancellation. *)
type source = User_manifest of string

type tool = {
  name : string;
  description : string;
  parameters : Yojson.Basic.t;
  program : string;
  arguments : string list;
  timeout_seconds : int;
  source : source;
}

type registry = tool list

type error =
  | Invalid of string
  | Unavailable of string
  | Approval_required
  | Denied
  | Cancelled
  | Timed_out
  | Exit of int * string
  | Signaled of int * string
  | Runner_failed of string

type invocation = {
  program : string;
  arguments : string list;
  cwd : string;
  stdin : string;
  timeout_seconds : int;
  output_limit : int;
}

type runner = cancel:(unit -> bool) -> invocation ->
  (string, error) result

type approval = {
  name : string;
  source : source;
  workspace : string;
  program : string;
  arguments : string list;
  timeout_seconds : int;
  environment : (string * string) list;
  parameters : Yojson.Basic.t;
  (* The arguments are shown verbatim: approval MUST be an informed, per-call
     interactive decision, even under permissive ordinary-tool policy. *)
  input : Yojson.Basic.t;
}

type hook_event =
  | Session_started
  | Turn_started
  | Before_tool of string
  | After_tool of string * (string, error) result
  | Turn_finished

type session = {
  owner : string;
  root : string;
  registry : registry;
  opt_in : bool;
  lock : Mutex.t;
  mutable stopped : bool;
  mutable hooks : (source * (hook_event -> unit)) list;
}
let child_environment = ["PATH", "/usr/bin:/bin"; "LANG", "C"]

exception Bad of string
let bad message = raise (Bad message)
let assoc = function `Assoc fields -> fields | _ -> bad "expected JSON object"
let required fields key = match List.assoc_opt key fields with
  | Some value -> value | None -> bad ("missing " ^ key)
let string = function `String value -> value | _ -> bad "expected JSON string"
let integer = function `Int value -> value | _ -> bad "expected JSON integer"
let unique fields =
  let keys = List.map fst fields in
  if List.length keys <> List.length (List.sort_uniq String.compare keys) then
    bad "duplicate JSON object field"
let keys_only fields allowed =
  unique fields;
  List.iter (fun (key, _) -> if not (List.mem key allowed) then
    bad ("unsupported field: " ^ key)) fields
let bounded_string ~label ~max value =
  if value = "" || String.length value > max ||
     String.exists (fun c -> Char.code c < 32 || Char.code c = 127) value then
    bad ("invalid " ^ label)
let identifier value =
  bounded_string ~label:"identifier" ~max:64 value;
  if not (match value.[0] with 'a'..'z' -> true | _ -> false) ||
     not (String.for_all (function
       | 'a'..'z' | '0'..'9' | '_' -> true | _ -> false) value) then
    bad "invalid identifier"
let within ~root path =
  path = root || String.starts_with ~prefix:(root ^ "/") path

(* Reject unsupported schema keywords rather than advertise constraints that
   the local validator cannot enforce. This intentionally accepts a small,
   boring and fully validated subset of JSON Schema. *)
let validate_schema schema =
  let count = ref 0 in
  let rec check depth schema =
    incr count;
    if depth > 8 || !count > 256 then bad "parameter schema is too complex";
    let fields = assoc schema in
    keys_only fields ["type"; "description"; "properties"; "required";
      "additionalProperties"; "items"; "maxItems"; "minItems";
      "maxLength"; "minLength"; "minimum"; "maximum"; "enum"];
    (match List.assoc_opt "description" fields with
     | None -> () | Some value ->
         bounded_string ~label:"schema description" ~max:512 (string value));
    let kind = string (required fields "type") in
    let allowed = match kind with
      | "object" -> ["properties"; "required"; "additionalProperties"]
      | "array" -> ["items"; "maxItems"; "minItems"]
      | "string" -> ["maxLength"; "minLength"]
      | "integer" | "number" -> ["minimum"; "maximum"]
      | "boolean" | "null" -> []
      | _ -> bad "unsupported parameter type" in
    List.iter (fun (key, _) -> if not (List.mem key ("type" :: "description" :: "enum" :: allowed)) then
      bad ("inapplicable schema field: " ^ key)) fields;
    (match kind with
     | "object" ->
         let props = assoc (required fields "properties") in
         unique props;
         if List.length props > 64 then bad "too many schema properties";
         List.iter (fun (name, sub) -> identifier name; check (depth + 1) sub) props;
         let req = match required fields "required" with
           | `List values -> List.map string values
           | _ -> bad "required must be an array" in
         if List.length req <> List.length (List.sort_uniq String.compare req) ||
            List.exists (fun name -> not (List.mem_assoc name props)) req then
           bad "invalid required properties";
         (match required fields "additionalProperties" with
          | `Bool false -> () | _ -> bad "additionalProperties must be false")
     | "array" ->
         check (depth + 1) (required fields "items");
         let max_items = integer (required fields "maxItems") in
         let min_items = match List.assoc_opt "minItems" fields with
           | None -> 0 | Some value -> integer value in
         if max_items < 0 || max_items > 64 || min_items < 0 || min_items > max_items then
           bad "invalid array bounds"
     | "string" ->
         let max_length = integer (required fields "maxLength") in
         let min_length = match List.assoc_opt "minLength" fields with
           | None -> 0 | Some value -> integer value in
         if max_length < 0 || max_length > 8192 || min_length < 0 || min_length > max_length then
           bad "invalid string bounds"
     | "integer" | "number" ->
         let min_value = integer (required fields "minimum") in
         let max_value = integer (required fields "maximum") in
         if min_value < -1_000_000_000 || max_value > 1_000_000_000 || min_value > max_value then
           bad "invalid numeric bounds"
     | _ -> ());
    (match List.assoc_opt "enum" fields with
     | None -> ()
     | Some (`List values) when values <> [] && List.length values <= 64 ->
         if List.length values <> List.length (List.sort_uniq compare values) then
           bad "duplicate enum value";
         List.iter (fun value ->
           match kind, value with
           | "string", `String text ->
               let maximum = integer (required fields "maxLength") in
               let minimum = match List.assoc_opt "minLength" fields with
                 | None -> 0 | Some value -> integer value in
               if String.length text < minimum || String.length text > maximum then
                 bad "enum exceeds string bound"
           | "integer", `Int number ->
               if number < integer (required fields "minimum") ||
                  number > integer (required fields "maximum") then bad "enum exceeds numeric bound"
           | "number", (`Int _ | `Float _) ->
               let number = match value with
                 | `Int n -> float_of_int n | `Float n -> n | _ -> assert false in
               if not (Float.is_finite number) ||
                  number < float_of_int (integer (required fields "minimum")) ||
                  number > float_of_int (integer (required fields "maximum")) then
                 bad "enum exceeds numeric bound"
           | "boolean", `Bool _ | "null", `Null -> ()
           | _ -> bad "enum has incompatible type") values
     | Some _ -> bad "invalid enum") in
  check 0 schema;
  if List.assoc "type" (assoc schema) <> `String "object" then
    bad "tool parameters must be an object"

let validate_input schema input =
  let rec check schema value =
    let fields = assoc schema in
    let kind = string (required fields "type") in
    (match List.assoc_opt "enum" fields with
     | Some (`List values) when not (List.mem value values) -> bad "argument is not an allowed enum value"
     | _ -> ());
    match kind, value with
    | "object", `Assoc values ->
        unique values;
        let props = assoc (required fields "properties") in
        let required_names = match required fields "required" with
          | `List values -> List.map string values | _ -> assert false in
        List.iter (fun key -> if not (List.mem_assoc key values) then
          bad ("missing argument: " ^ key)) required_names;
        List.iter (fun (key, value) -> match List.assoc_opt key props with
          | None -> bad ("unexpected argument: " ^ key)
          | Some sub -> check sub value) values
    | "array", `List values ->
        let max_items = integer (required fields "maxItems") in
        let min_items = match List.assoc_opt "minItems" fields with
          | None -> 0 | Some v -> integer v in
        if List.length values < min_items || List.length values > max_items then
          bad "array argument is outside its bounds";
        List.iter (check (required fields "items")) values
    | "string", `String text ->
        let max_length = integer (required fields "maxLength") in
        let min_length = match List.assoc_opt "minLength" fields with
          | None -> 0 | Some v -> integer v in
        if String.length text < min_length || String.length text > max_length then
          bad "string argument is outside its bounds"
    | "integer", `Int number ->
        if number < integer (required fields "minimum") ||
           number > integer (required fields "maximum") then
          bad "integer argument is outside its bounds"
    | "number", (`Int _ | `Float _) ->
        let number = match value with `Int n -> float_of_int n | `Float n -> n | _ -> assert false in
        if not (Float.is_finite number) ||
           number < float_of_int (integer (required fields "minimum")) ||
           number > float_of_int (integer (required fields "maximum")) then
          bad "number argument is outside its bounds"
    | "boolean", `Bool _ | "null", `Null -> ()
    | _ -> bad ("argument has wrong JSON type: " ^ kind) in
  check schema input

let trusted_file ~user_dir ~root path =
  let user_dir = Unix.realpath user_dir and root = Unix.realpath root in
  let path = Unix.realpath path in
  if not (within ~root:user_dir path) || within ~root path ||
     within ~root user_dir then bad "manifest must be outside the workspace and under user config";
  let rec directories dir =
    if not (within ~root:user_dir dir) then () else (
      let stat = Unix.lstat dir in
      if stat.Unix.st_uid <> Unix.getuid () || stat.Unix.st_kind <> Unix.S_DIR ||
         stat.Unix.st_perm land 0o077 <> 0 then
        bad "manifest directories must be private and user owned";
      if dir <> user_dir then directories (Filename.dirname dir)) in
  directories (Filename.dirname path);
  let stat = Unix.lstat path in
  if stat.Unix.st_uid <> Unix.getuid () || stat.Unix.st_kind <> Unix.S_REG ||
     stat.Unix.st_perm land 0o077 <> 0 || stat.Unix.st_size > 65_536 then
    bad "manifest must be a private user-owned regular file (at most 64 KiB)";
  path

let load ~user_dir ~root ~builtins path =
  try
    let path = trusted_file ~user_dir ~root path in
    let channel = open_in_bin path in
    let json = Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
      let size = in_channel_length channel in
      if size > 65_536 then bad "manifest exceeds 64 KiB";
      Yojson.Basic.from_string (really_input_string channel size)) in
    let fields = assoc json in
    keys_only fields ["version"; "tools"];
    if required fields "version" <> `Int 1 then bad "unsupported tool manifest version";
    let rows = match required fields "tools" with
      | `List rows when List.length rows <= 32 -> rows
      | _ -> bad "tool manifest exceeds 32 tools" in
    let names = ref builtins in
    Ok (List.map (fun row ->
      let fields = assoc row in
      keys_only fields ["name"; "description"; "parameters";
        "program"; "arguments"; "timeoutSeconds"];
      let name = string (required fields "name") in
      identifier name;
      if List.mem name !names then bad ("duplicate or built-in tool name: " ^ name);
      names := name :: !names;
      let description = string (required fields "description") in
      bounded_string ~label:"description" ~max:1024 description;
      let parameters = required fields "parameters" in
      validate_schema parameters;
      let program = string (required fields "program") in
      if String.length program > 4096 || Filename.is_relative program ||
         String.exists (fun c -> Char.code c < 32 || Char.code c = 127) program then
        bad "tool program must be an absolute path";
      let program = Unix.realpath program in
      if within ~root:(Unix.realpath root) program then
        bad "project executables cannot be registered as user tools";
      let stat = Unix.stat program in
      if stat.Unix.st_kind <> Unix.S_REG || stat.Unix.st_perm land 0o111 = 0 then
        bad "tool program must be executable regular file";
      let arguments = match required fields "arguments" with
        | `List args when List.length args <= 64 -> List.map (fun arg ->
            let arg = string arg in
            if String.length arg > 4096 ||
               String.exists (fun c -> Char.code c < 32 || Char.code c = 127) arg then
              bad "invalid tool program argument";
            arg) args
        | _ -> bad "tool arguments must be at most 64 strings" in
      let timeout_seconds = match List.assoc_opt "timeoutSeconds" fields with
        | None -> 30 | Some value -> integer value in
      if timeout_seconds < 1 || timeout_seconds > 120 then bad "invalid tool timeout";
      { name; description; parameters; program; arguments;
        timeout_seconds; source = User_manifest path }) rows)
  with
  | Bad reason -> Error (Invalid reason)
  | Yojson.Json_error _ -> Error (Invalid "malformed tool manifest JSON")
  | Stack_overflow | Invalid_argument _ ->
      Error (Invalid "tool manifest is malformed or too deeply nested")
  | Unix.Unix_error _ | Sys_error _ | End_of_file ->
      Error (Invalid "tool manifest or executable is unavailable")

let definitions (registry : registry) = List.map (fun (tool : tool) ->
  `Assoc ["type", `String "function";
    "function", `Assoc ["name", `String tool.name;
      "description", `String tool.description;
      "parameters", tool.parameters]]) registry
let find (registry : registry) name =
  List.find_opt (fun (tool : tool) -> tool.name = name) registry
let tool_name (tool : tool) = tool.name
let tool_source (tool : tool) = tool.source
let tool_invocation (tool : tool) =
  tool.program, tool.arguments, tool.timeout_seconds

let create_session ~owner ~root ~registry ~opt_in =
  if owner = "" || String.length owner > 128 then invalid_arg "invalid tool session owner";
  { owner; root = Unix.realpath root; opt_in;
    registry = (if opt_in then registry else []);
    lock = Mutex.create (); stopped = false; hooks = [] }
let session_owner session = session.owner
let synchronized session callback =
  Mutex.lock session.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock session.lock) callback
let cancelled session = synchronized session (fun () -> session.stopped)
let cancel session = synchronized session (fun () ->
  session.stopped <- true; session.hooks <- [])
let dispose = cancel
(* A hook must be attributed to a manifest already present in this private
   session's trusted snapshot; project-sourced data cannot subscribe. The
   callback is application-owned code, never code loaded out of a manifest. *)
let subscribe session ~source callback = synchronized session (fun () ->
  if session.stopped || not session.opt_in || List.length session.hooks >= 32 ||
     not (List.exists (fun (tool : tool) -> tool.source = source) session.registry)
  then false else (session.hooks <- (source, callback) :: session.hooks; true))
let emit session event = synchronized session (fun () ->
  if session.opt_in && not session.stopped then
    List.iter (fun (_, callback) -> try callback event with _ -> ())
      (List.rev session.hooks))

let real_runner ~cancel (invocation : invocation) =
  try
    if cancel () then Error Cancelled else
    let result = Workspace_process.run ~cancel
      ~program:invocation.program ~arguments:invocation.arguments
      ~cwd:(Some invocation.cwd) ~stdin:invocation.stdin
      ~timeout_seconds:invocation.timeout_seconds
      ~output_limit:invocation.output_limit
      ~inherit_environment:false
      ~environment:child_environment () in
    if cancel () then Error Cancelled else
    match result.Workspace_process.termination with
    | Workspace_process.Cancelled -> Error Cancelled
    | Workspace_process.Timed_out -> Error Timed_out
    | Workspace_process.Exited 0 when result.truncated ->
        Error (Runner_failed "tool output exceeds its limit")
    | Workspace_process.Exited 0 -> Ok result.output
    | Workspace_process.Exited code -> Error (Exit (code, result.output))
    | Workspace_process.Signaled signal -> Error (Signaled (signal, result.output))
  with
  | Workspace_process.Error reason -> Error (Runner_failed reason)
  | Unix.Unix_error _ | Sys_error _ -> Error (Runner_failed "tool executable unavailable")

(* One invocation has exactly one terminal result. The caller, not the runner,
   owns provider call IDs and ordered settlement. [interactive] is supplied by
   the UI; a headless caller cannot supply an approval callback to bypass it. *)
let invoke ?(runner = real_runner) session ~name ~input ~interactive ~approve =
  if cancelled session then Error Cancelled else
  match find session.registry name with
  | None -> Error (Unavailable name)
  | Some tool ->
      (try
         validate_input tool.parameters input;
         let stdin = Yojson.Basic.to_string input in
         if String.length stdin > 65_536 then bad "tool input exceeds 64 KiB";
         if not interactive then Error Approval_required
         else
           let request = { name; source = tool.source;
             workspace = session.root; program = tool.program;
             arguments = tool.arguments; parameters = tool.parameters;
             timeout_seconds = tool.timeout_seconds;
             environment = child_environment; input } in
           if cancelled session then Error Cancelled
           else if not (approve request) then Error Denied
           else if cancelled session then Error Cancelled
           else
             let invocation = { program = tool.program;
               arguments = tool.arguments; cwd = session.root;
               stdin; timeout_seconds = tool.timeout_seconds;
               output_limit = 65_536 } in
             let result = runner ~cancel:(fun () -> cancelled session) invocation in
             if cancelled session then Error Cancelled else
             (match result with
              | Ok output when String.length output > invocation.output_limit ->
                  Error (Runner_failed "tool result exceeds output limit")
              | result -> result)
       with
       | Bad reason -> Error (Invalid reason)
       | _ -> Error (Runner_failed "tool callback failed"))
