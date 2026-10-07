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
  let result = `Assoc ["ok", `Bool true] in
  let response = Yojson.Basic.to_string (`Assoc [
    "jsonrpc", `String "2.0"; "id", `Int 1; "result", result]) in
  List.iter (fun newline ->
    let line = "data: " ^ response ^ newline in
    check "MCP SSE accepts every standard line ending"
      (Mcp_http.sse_response 1 (line ^ newline) = Some result);
    check "MCP SSE does not accept an unfinished event"
      (Mcp_http.sse_response 1 line = None))
    ["\n"; "\r"; "\r\n"];
  check "MCP SSE accepts its initial UTF-8 BOM"
    (Mcp_http.sse_response 1 ("\239\187\191data: " ^ response ^ "\r\r") =
      Some result);
  check "CR is a line boundary rather than discarded data"
    (Mcp_http.sse_response 1 ("da\rta: " ^ response ^ "\n\n") = None);
  let notification = {|{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}|} in
  check "MCP SSE preserves notification ordering and multiline data"
    (Mcp_http.sse_response 1 ("data: " ^ notification ^ "\r\n\r\n" ^
       "data: {\"jsonrpc\":\"2.0\",\rdata: \"id\":1,\"result\":{\"ok\":true}}\r\r") =
      Some result);
  let schema = `Assoc [
    "type", `String "object";
    "properties", `Assoc ["text", `Assoc [
      "type", `String "string"; "maxLength", `Int 1]];
    "required", `List [`String "text"]; "additionalProperties", `Bool false] in
  List.iter (fun text ->
    Mcp_client.validate_arguments ~schema (`Assoc ["text", `String text]))
    ["한"; "😀"; ""];
  List.iter (fun text ->
    check "MCP string bounds count Unicode scalars and reject malformed text"
      (match Mcp_client.validate_arguments ~schema (`Assoc ["text", `String text]) with
       | exception Mcp_client.Error _ -> true | _ -> false))
    ["한글"; "e\204\129"; "\255"];
  let transport = Mcp_http.create "https://example.test/mcp" in
  Mutex.lock transport.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock transport.lock) (fun () ->
    check "MCP HTTP cancellation covers lock acquisition"
      (match Mcp_http.request transport ~method_:"initialize" ~params:(`Assoc [])
        ~timeout_seconds:1. ~cancelled:(fun () -> true) with
       | exception Mcp_http.Cancelled -> true | _ -> false);
    check "MCP HTTP deadline covers lock acquisition"
      (match Mcp_http.request transport ~method_:"initialize" ~params:(`Assoc [])
        ~timeout_seconds:0.01 ~cancelled:(fun () -> false) with
       | exception Mcp_http.Error _ -> true | _ -> false);
    check "cancelled and expired waiters never allocate request IDs"
      (transport.next_id = 1))

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

let http_response_fixture ~extra_bytes callback =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 1;
  let port = match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    let code = try
      let peer, _ = Unix.accept socket in
      Unix.close socket;
      let input = Unix.in_channel_of_descr peer in
      ignore (input_line input);
      let rec request_headers length =
        let line = input_line input in
        if line = "\r" || line = "" then length
        else
          let length = match String.index_opt line ':' with
            | Some colon when String.lowercase_ascii (String.sub line 0 colon) =
                "content-length" ->
                int_of_string (String.trim (String.sub line (colon + 1)
                  (String.length line - colon - 1)))
            | _ -> length in
          request_headers length in
      let length = request_headers 0 in
      ignore (really_input_string input length);
      let body = {|{"jsonrpc":"2.0","id":1,"result":{"ok":true}}|} in
      let output = Unix.out_channel_of_descr peer in
      output_string output (Printf.sprintf
        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
        (String.length body + extra_bytes) body);
      flush output;
      Unix.shutdown peer Unix.SHUTDOWN_SEND;
      close_out_noerr output;
      close_in_noerr input;
      0
    with _ -> 1 in
    Unix._exit code)
  else (
    Unix.close socket;
    Fun.protect ~finally:(fun () ->
      (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
      let rec reap () = try ignore (Unix.waitpid [] child) with
        | Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
        | Unix.Unix_error (Unix.ECHILD, _, _) -> () in
      reap ()) (fun () ->
      let transport = Mcp_http.create ~allow_loopback_http:true
        (Printf.sprintf "http://127.0.0.1:%d/mcp" port) in
      callback transport))

let () =
  http_response_fixture ~extra_bytes:0 (fun transport ->
    check "MCP HTTP accepts a complete successful JSON transfer"
      (Mcp_http.request transport ~method_:"initialize" ~params:(`Assoc [])
         ~timeout_seconds:2. ~cancelled:(fun () -> false) =
       `Assoc ["ok", `Bool true]));
  http_response_fixture ~extra_bytes:20 (fun transport ->
    check "MCP HTTP rejects valid JSON received through a truncated transfer"
      (match Mcp_http.request transport ~method_:"initialize" ~params:(`Assoc [])
        ~timeout_seconds:2. ~cancelled:(fun () -> false) with
       | exception Mcp_http.Error _ -> not transport.initialized
       | _ -> false))
