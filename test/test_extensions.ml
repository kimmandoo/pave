open Pave

let check label value = if not value then failwith label
let write ?(mode = 0o600) path text =
  let out = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out out) (fun () -> output_string out text);
  Unix.chmod path mode
let rec remove path =
  match Unix.lstat path with
  | stat when stat.Unix.st_kind = Unix.S_DIR ->
      Array.iter (fun entry -> remove (Filename.concat path entry))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
let available : Plugin_registry.capabilities =
  { skills = ["review"]; commands = ["explain"]; tools = ["local_review"] }
let empty : Plugin_registry.capabilities =
  { skills = []; commands = []; tools = [] }

let () =
  let base = Filename.temp_file "pave-extensions-" "" in
  Unix.unlink base;
  Unix.mkdir base 0o700;
  let config = Filename.concat base "config" in
  let user = Filename.concat config "pave" in
  let root = Filename.concat base "workspace" in
  Unix.mkdir config 0o700;
  Unix.mkdir user 0o700;
  Unix.mkdir root 0o700;
  let alias = Filename.concat base "user-alias" in
  Unix.symlink user alias;
  let previous = Sys.getenv_opt "XDG_CONFIG_HOME" in
  Fun.protect ~finally:(fun () ->
    (match previous with Some value -> Unix.putenv "XDG_CONFIG_HOME" value
     | None -> Unix.putenv "XDG_CONFIG_HOME" "");
    remove base) (fun () ->
    Unix.putenv "XDG_CONFIG_HOME" config;
    check "user-owned symlink cannot redirect plugin storage"
      (match Plugin_registry.load ~user_dir:alias ~available ~builtins:empty with
       | Error _ -> true | Ok _ -> false);
    let registry = match Plugin_registry.load ~user_dir:user
        ~available ~builtins:empty with
      | Ok value -> value | Error message -> failwith message in
    let plugins = Filename.concat user "plugins" in
    let manifest = Filename.concat plugins "review-pack.json" in
    write manifest
      {|{"schemaVersion":1,"name":"review-pack","version":"1.0.0","skills":["review"],"commands":["explain"],"tools":["local_review"]}|};
    let snapshot = match Plugin_registry.reload registry with
      | Ok value -> value | Error message -> failwith message in
    check "plugin references are inert until enabled"
      (snapshot.active = empty);
    let snapshot = match Plugin_registry.enable registry "review-pack" with
      | Ok value -> value | Error message -> failwith message in
    check "all capabilities become available together"
      (snapshot.active = available);
    let snapshot = match Plugin_registry.disable registry "review-pack" with
      | Ok value -> value | Error message -> failwith message in
    check "disable withdraws skill, command and tool"
      (snapshot.active = empty);
    let user_config = Filename.concat user "mcp.json" in
    let project_dir = Filename.concat root ".pave" in
    Unix.mkdir project_dir 0o700;
    write user_config
      {|{"servers":[{"name":"blocked","command":"/bin/cat"},{"name":"kept","command":"/bin/cat"}]}|};
    write (Filename.concat project_dir "mcp.json")
      {|{"servers":[{"name":"blocked","deny":true}]}|};
    let snapshot = Mcp_config.load ~root ~owner:"test-session" in
    check "project deny overrides owned user server without starting it"
      (Mcp_config.names snapshot = ["kept"]);
    write (Filename.concat project_dir "mcp.json")
      {|{"servers":[{"name":"kept","transport":"http","url":"https://different.example/mcp","bearerSecretRef":"absent"}]}|};
    check "missing private secret fails entire config before connection"
      (match Mcp_config.load ~root ~owner:"test-session" with
       | exception Mcp_config.Error _ -> true | _ -> false);
    print_endline "private plugin lifecycle and MCP config boundaries: ok")
