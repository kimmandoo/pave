(* Configuration is data, never an instruction to start a process. A snapshot is
   complete or unavailable: a malformed entry cannot be hidden by precedence. *)
exception Error of string
let fail text = raise (Error text)

type source = User | Project
type transport =
  | Stdio of { program : string; arguments : string list;
               environment : (string * string) list }
  | Http of { endpoint : string; bearer_secret_ref : string option;
              allow_loopback_http : bool }
type server = {
  name : string; source : source; root : string; owner : string;
  transport : transport;
}
type snapshot = { root : string; owner : string; servers : server list }
type entry = Deny of string | Server of server

let field name fields = List.assoc_opt name fields
let unique label fields =
  let names = List.map fst fields in
  if List.length names <> List.length (List.sort_uniq String.compare names)
  then fail ("duplicate " ^ label)
let object_ label = function
  | `Assoc fields -> unique label fields; fields
  | _ -> fail (label ^ " must be an object")
let only label keys fields = List.iter (fun (key, _) ->
  if not (List.mem key keys) then fail ("unknown " ^ label ^ " field " ^ key)) fields
let bounded label max text =
  if text = "" || String.length text > max ||
     String.exists (fun c -> Char.code c < 32 || Char.code c = 127) text
  then fail (label ^ " must be bounded, nonempty, and free of control characters");
  text
let string label = function `String s -> bounded label 4096 s | _ -> fail (label ^ " must be a string")
let required name fields = match field name fields with
  | Some value -> value | None -> fail ("missing " ^ name)
let identifier label text =
  ignore (bounded label 64 text);
  if not (String.for_all (function
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' | '.' -> true
      | _ -> false) text) || text = "." || text = ".."
  then fail (label ^ " must be an identifier"); text
let normal_name name = identifier "server name" name
let same_file a b = a.Unix.st_dev = b.Unix.st_dev && a.Unix.st_ino = b.Unix.st_ino
let read_file ~private_file path =
  let before = Unix.lstat path in
  if before.Unix.st_kind <> Unix.S_REG || before.Unix.st_size > 65_536 ||
     (private_file && (before.Unix.st_uid <> Unix.geteuid () ||
                       before.Unix.st_perm land 0o077 <> 0 || before.Unix.st_nlink <> 1))
  then fail ("unsafe MCP configuration file " ^ path);
  let fd = Unix.openfile path [Unix.O_RDONLY; Unix.O_NONBLOCK] 0 in
  let input = Unix.in_channel_of_descr fd in
  Fun.protect ~finally:(fun () -> close_in input) (fun () ->
    let current = Unix.fstat fd and after = Unix.lstat path in
    if current.Unix.st_kind <> Unix.S_REG || current.Unix.st_size > 65_536 ||
       not (same_file before current && same_file current after) then
      fail ("MCP configuration file changed " ^ path);
    really_input_string input current.Unix.st_size)
let optional_read ~private_file path =
  try Some (read_file ~private_file path) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> None
let parse_json label text =
  try Yojson.Basic.from_string text with Yojson.Json_error _ -> fail (label ^ " is not valid JSON")
let private_directory directory =
  let stat = Unix.lstat directory in
  if stat.Unix.st_kind <> Unix.S_DIR || stat.Unix.st_uid <> Unix.geteuid () ||
     stat.Unix.st_perm land 0o077 <> 0 then fail "MCP private directory must be owned and mode 0700"
let secrets path =
  match optional_read ~private_file:true path with
  | None -> []
  | Some text ->
      let fields = object_ "MCP secrets" (parse_json "MCP secrets" text) in
      if List.length fields > 64 then fail "too many MCP secrets";
      List.map (fun (key, value) ->
        let key = identifier "secret reference" key in
        let value = string "secret value" value in
        if String.length value > 4096 then fail "MCP secret exceeds 4096 bytes";
        key, value) fields
let env_key text =
  if text = "" || String.length text > 64 ||
     not (String.for_all (function
       | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> true | _ -> false) text) ||
     (match text.[0] with '0'..'9' -> true | _ -> false) ||
     List.mem text ["PATH"; "HOME"; "LD_PRELOAD"; "LD_LIBRARY_PATH"; "DYLD_INSERT_LIBRARIES"]
  then fail "unsafe MCP environment variable";
  text
let program_path path =
  ignore (bounded "MCP executable" 4096 path);
  if Filename.is_relative path || List.mem ".." (String.split_on_char '/' path) then
    fail "MCP executable must be an absolute path without traversal";
  let stat = Unix.lstat path in
  if stat.Unix.st_kind <> Unix.S_REG then fail "MCP executable must be a regular file";
  Unix.access path [Unix.X_OK]; path
let http_endpoint ~allow_loopback_http endpoint =
  ignore (bounded "MCP HTTP URL" 2048 endpoint);
  let scheme, rest =
    if String.starts_with ~prefix:"https://" endpoint then
      `Https, String.sub endpoint 8 (String.length endpoint - 8)
    else if String.starts_with ~prefix:"http://" endpoint then
      `Http, String.sub endpoint 7 (String.length endpoint - 7)
    else fail "MCP HTTP URL must use HTTPS" in
  let authority = match String.index_opt rest '/' with
    | None -> rest
    | Some pos -> String.sub rest 0 pos in
  if authority = "" || String.contains authority '@' ||
     String.contains endpoint '?' || String.contains endpoint '#' ||
     String.contains endpoint '\\' then fail "unsafe MCP HTTP URL";
  let host = match String.index_opt authority ':' with
    | None -> authority
    | Some pos ->
        let port = String.sub authority (pos + 1) (String.length authority - pos - 1) in
        if port = "" || not (String.for_all (function '0'..'9' -> true | _ -> false) port) ||
           (try let n = int_of_string port in n < 1 || n > 65_535 with _ -> true)
        then fail "invalid MCP HTTP port";
        String.sub authority 0 pos in
  if host = "" || not (String.for_all (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '-' -> true
    | _ -> false) host) then fail "invalid MCP HTTP host";
  if scheme = `Http && (not allow_loopback_http ||
      not (List.mem (String.lowercase_ascii host) ["localhost"; "127.0.0.1"]))
  then fail "plaintext MCP HTTP requires explicit loopback approval";
  endpoint
let parse_entries ~root ~owner ~source ~secrets text =
  let fields = object_ "MCP config" (parse_json "MCP config" text) in
  only "MCP config" ["servers"] fields;
  let list = match required "servers" fields with
    | `List list when List.length list <= 32 -> list
    | _ -> fail "MCP servers must be an array of at most 32 entries" in
  let entries = List.map (fun json ->
    let fields = object_ "MCP server" json in
    only "MCP server" ["name"; "deny"; "transport"; "command"; "args"; "env";
      "url"; "bearerSecretRef"; "allowLoopbackHttp"] fields;
    let name = normal_name (string "server name" (required "name" fields)) in
    match field "deny" fields with
    | Some (`Bool true) ->
        if List.exists (fun (key, _) -> key <> "name" && key <> "deny") fields
        then fail "denied MCP server cannot specify a transport";
        Deny name
    | Some (`Bool false) | None ->
        let kind = match field "transport" fields with
          | None | Some (`String "stdio") -> `Stdio
          | Some (`String "http") -> `Http
          | _ -> fail "MCP transport must be stdio or http" in
        let transport = match kind with
        | `Http ->
            if List.exists (fun key -> field key fields <> None) ["command"; "args"; "env"]
            then fail "HTTP MCP server cannot specify stdio fields";
            let allow_loopback_http = match field "allowLoopbackHttp" fields with
              | None -> false
              | Some (`Bool value) -> value
              | _ -> fail "allowLoopbackHttp must be a boolean" in
            let endpoint = http_endpoint ~allow_loopback_http
                (string "url" (required "url" fields)) in
            let bearer_secret_ref = match field "bearerSecretRef" fields with
              | None -> None
              | Some json ->
                  let reference = identifier "secret reference"
                      (string "bearerSecretRef" json) in
                  if not (List.mem_assoc reference secrets) then
                    fail ("missing private MCP secret reference " ^ reference);
                  Some reference in
            Http { endpoint; bearer_secret_ref; allow_loopback_http }
        | `Stdio ->
            if List.exists (fun key -> field key fields <> None)
                ["url"; "bearerSecretRef"; "allowLoopbackHttp"]
            then fail "stdio MCP server cannot specify HTTP fields";
            let program = program_path (string "command" (required "command" fields)) in
            let arguments = match field "args" fields with
              | None -> []
              | Some (`List items) when List.length items <= 128 ->
                  List.map (fun value -> string "argument" value) items
              | _ -> fail "MCP args must be an array of at most 128 strings" in
            let total = List.fold_left (fun n s -> n + String.length s) 0 arguments in
            if total > 32_768 then fail "MCP arguments exceed 32 KiB";
            let environment = match field "env" fields with
              | None -> []
              | Some json ->
                  let env = object_ "MCP environment" json in
                  if List.length env > 32 then fail "too many MCP environment variables";
                  List.map (fun (key, json) ->
                    let key = env_key key in
                    let value = object_ "MCP secret binding" json in
                    only "MCP secret binding" ["secretRef"] value;
                    let reference = identifier "secret reference"
                        (string "secretRef" (required "secretRef" value)) in
                    if not (List.mem_assoc reference secrets) then
                      fail ("missing private MCP secret reference " ^ reference);
                    key, reference) env in
            Stdio {program; arguments; environment} in
        Server { name; source; transport; root; owner }
    | Some _ -> fail "MCP deny must be boolean") list in
  let names = List.map (function Deny n -> n | Server s -> s.name) entries in
  if List.length names <> List.length (List.sort_uniq String.compare names)
  then fail "duplicate MCP server name";
  entries
let config_home () =
  let base = match Sys.getenv_opt "XDG_CONFIG_HOME" with
    | Some path when path <> "" && not (Filename.is_relative path) -> path
    | _ -> Filename.concat (Sys.getenv "HOME") ".config" in
  Filename.concat base "pave"
let load ~root ~owner =
  ignore (bounded "MCP session owner" 128 owner);
  let root = try Unix.realpath root with Unix.Unix_error _ -> fail "MCP workspace root is unavailable" in
  if (Unix.stat root).Unix.st_kind <> Unix.S_DIR then fail "MCP workspace root must be a directory";
  let user_dir = config_home () in
  let user_path = Filename.concat user_dir "mcp.json"
  and secret_path = Filename.concat user_dir "mcp-secrets.json" in
  let project_dir = Filename.concat root ".pave" in
  let project_path = Filename.concat project_dir "mcp.json" in
  let user_dir_exists = try private_directory user_dir; true with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> false in
  let secret_values = if user_dir_exists then secrets secret_path else [] in
  let user = if user_dir_exists then optional_read ~private_file:true user_path else None in
  let project = try
    let stat = Unix.lstat project_dir in
    if stat.Unix.st_kind <> Unix.S_DIR then fail "project .pave must be a real directory";
    optional_read ~private_file:false project_path
  with Unix.Unix_error (Unix.ENOENT, _, _) -> None in
  let parse source = function
    | None -> []
    | Some text -> parse_entries ~root ~owner ~source ~secrets:secret_values text in
  let user = parse User user and project = parse Project project in
  let denied = List.filter_map (function Deny name -> Some name | _ -> None) (user @ project) in
  let selected = List.filter_map (function Server value -> Some value | _ -> None) project in
  let project_names = List.map (function Deny n -> n | Server s -> s.name) project in
  let selected = selected @ List.filter_map (function
    | Server s when not (List.mem s.name project_names) -> Some s
    | _ -> None) user in
  { root; owner; servers = List.filter (fun s -> not (List.mem s.name denied)) selected }
let find snapshot name = List.find_opt (fun s -> s.name = name) snapshot.servers
let names snapshot = List.map (fun s -> s.name) snapshot.servers
let resolve_secret reference =
  ignore (identifier "secret reference" reference);
  let directory = config_home () in
  private_directory directory;
  match List.assoc_opt reference
      (secrets (Filename.concat directory "mcp-secrets.json")) with
  | Some value -> value
  | None -> fail ("missing private MCP secret reference " ^ reference)
let resolve_environment server =
  match server.transport with
  | Http _ -> fail "HTTP MCP server has no process environment"
  | Stdio {environment = []; _} -> []
  | Stdio {environment; _} ->
      let directory = config_home () in
      private_directory directory;
      let values = secrets (Filename.concat directory "mcp-secrets.json") in
      List.map (fun (key, reference) ->
        let value = match List.assoc_opt reference values with
          | Some value -> value
          | None -> fail ("missing private MCP secret reference " ^ reference) in
        key, value) environment
