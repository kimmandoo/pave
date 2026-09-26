module Cloud = Pave.Ollama_cloud
module Protocol = Pave.Protocol
let field = Protocol.member

let expect_models expected = function
  | Ok models when models = expected -> ()
  | Ok models -> failwith ("wrong cloud models: " ^ String.concat "," models)
  | Error _ -> failwith "cloud model discovery failed"
let expect_error check = function
  | Error failure when check failure -> ()
  | _ -> failwith "unsafe cloud model listing accepted"
let invalid = function Cloud.Invalid_response _ -> true | _ -> false
let http_error = function Cloud.Http_error _ -> true | _ -> false

let string_schema = `Assoc ["type", `String "string"]
let parameters = `Assoc ["type", `String "object";
  "properties", `Assoc ["item", string_schema]]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "lookup";
    "description", `String "Look up a value";
    "parameters", parameters]]
let args = `Assoc ["item", `String "current"]
let native_call = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "lookup"; "arguments", args]]
let key = "private-cloud-fixture"
let model = "next-cloud-model:77b"

(* This executable impersonates curl when launched by Provider.run_curl. It
   inspects the actual curl config and JSON sent through the production HTTP
   path, not a mock of the wire serializer or Provider.complete. *)
let fake_curl () =
  let configs = ref [] in
  (try while true do configs := input_line stdin :: !configs done
   with End_of_file -> ());
  let config = List.rev !configs in
  let values name = List.filter_map (fun line ->
    let prefix = name ^ " = " in
    if String.starts_with ~prefix line then
      match Yojson.Basic.from_string
        (String.sub line (String.length prefix)
          (String.length line - String.length prefix)) with
      | `String value -> Some value | _ -> failwith "invalid curl option"
    else None) config in
  let one name = match values name with [value] -> value | _ -> failwith ("missing curl " ^ name) in
  let has name value = assert (one name = value) in
  has "proto" "=https";
  assert (not (List.mem "location" config));
  assert (not (List.mem "location-trusted" config));
  assert (values "header" |> List.mem ("Authorization: Bearer " ^ key));
  let state_path = Sys.getenv "PAVE_CLOUD_FIXTURE_STATE" in
  let read_step () =
    if not (Sys.file_exists state_path) then 0 else
      let input = open_in state_path in
      Fun.protect ~finally:(fun () -> close_in input)
        (fun () -> int_of_string (input_line input)) in
  let step = read_step () in
  let output = open_out state_path in
  output_string output (string_of_int (step + 1)); close_out output;
  let response = match step with
    | 0 ->
        has "url" Cloud.tags_url;
        has "request" "GET";
        assert (List.mem "Accept: application/json" (values "header"));
        has "max-filesize" (string_of_int Cloud.max_response_bytes);
        {|{"models":[{"name":"next-cloud-model:cloud","model":"next-cloud-model:77b"}]}|}
    | 1 | 2 ->
        has "url" Cloud.chat_url;
        has "request" "POST";
        assert (List.mem "Content-Type: application/json" (values "header"));
        let path = one "data-binary" in
        assert (String.starts_with ~prefix:"@" path);
        let input = open_in_bin (String.sub path 1 (String.length path - 1)) in
        let request = Fun.protect ~finally:(fun () -> close_in input)
          (fun () -> Yojson.Basic.from_string (really_input_string input (in_channel_length input))) in
        assert (field "model" request = `String model);
        assert (field "stream" request = `Bool false);
        assert (field "tools" request = `List [tool]);
        let user = `Assoc ["role", `String "user"; "content", `String "Look this up"] in
        if step = 1 then (
          assert (field "messages" request = `List [user]);
          {|{"done":true,"done_reason":"tool_calls","message":{"role":"assistant","content":"","tool_calls":[{"type":"function","function":{"name":"lookup","arguments":{"item":"current"}}}]}}|})
        else (
          assert (field "messages" request = `List [user;
            `Assoc ["role", `String "assistant"; "content", `String "";
              "tool_calls", `List [native_call]];
            `Assoc ["role", `String "tool"; "content", `String "found";
              "tool_name", `String "lookup"]]);
          {|{"done":true,"done_reason":"stop","message":{"role":"assistant","content":"The answer is found."}}|})
    | _ -> failwith "unexpected cloud network request" in
  let path = one "output" in
  let file = open_out_bin path in
  output_string file response; close_out file;
  print_string "200"; flush stdout

let () =
  if Array.length Sys.argv >= 3 && Sys.argv.(1) = "--disable" && Sys.argv.(2) = "--config" then
    (try fake_curl () with exn ->
       prerr_endline (Printexc.to_string exn); exit 2)
  else (
    let calls = ref 0 in
    let http ~url ~headers =
      incr calls;
      assert (url = Cloud.tags_url);
      assert (headers = ["Authorization", "Bearer " ^ key;
                         "Accept", "application/json"]);
      Ok (200, {|{"models":[{"name":"local-alias:cloud","model":"actual-cloud-model:9b"},{"name":"standalone-cloud-model"},{"model":"distinct-cloud-model:4b"}]}|}) in
    expect_models ["actual-cloud-model:9b"; "standalone-cloud-model"; "distinct-cloud-model:4b"]
      (Cloud.discover ~http ~api_key:key ());
    assert (!calls = 1);
    expect_error ((=) Cloud.Invalid_credential)
      (Cloud.discover ~http ~api_key:"" ());
    expect_error ((=) Cloud.Invalid_credential)
      (Cloud.discover ~http ~api_key:"bad\r\nAuthorization: Bearer stolen" ());
    assert (!calls = 1);
    List.iter (fun payload ->
      expect_error invalid (Cloud.discover
        ~http:(fun ~url:_ ~headers:_ -> Ok (200, payload)) ~api_key:key ())) [
      {|{"models":[{"model":"duplicate-cloud-model"},{"name":"duplicate-cloud-model"}]}|};
      {|{"models":[{"model":"illegal:cloud"}]}|};
      {|{"models":[{"model":"bad\nname"}]}|};
      {|{"models":[{"model":null,"name":"fallback"}]}|};
      {|{"models":[{"name":"ok"},{}]}|};
      {|{"data":[]}|}; "not json" ];
    expect_error invalid (Cloud.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (200,
        String.make (Cloud.max_response_bytes + 1) 'x')) ~api_key:key ());
    expect_error http_error (Cloud.discover
      ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect")) ~api_key:key ());
    let child = Unix.fork () in
    if child = 0 then (
      Unix.putenv "OLLAMA_API_KEY" "official-cloud-key";
      Unix.putenv "OLLAMA_CLOUD_API_KEY" "";
      assert (Cloud.env_api_key () = Some "official-cloud-key");
      Unix.putenv "OLLAMA_CLOUD_API_KEY" "specific-cloud-key";
      assert (Cloud.env_api_key () = Some "specific-cloud-key");
      exit 0);
    (match Unix.waitpid [] child with
     | _, Unix.WEXITED 0 -> ()
     | _ -> failwith "Ollama Cloud environment key resolution failed");
    let directory = Filename.temp_file "pave-ollama-cloud-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_CLOUD_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_CLOUD_FIXTURE_STATE" state;
      let discovered = match Cloud.discover ~api_key:key () with
        | Ok [id] -> id | _ -> failwith "default HTTP cloud discovery failed" in
      assert (discovered = model);
      let config : Pave.Provider.config = {
        endpoint = Cloud.chat_url; api_key = key; model = discovered;
        api = Pave.Provider.Ollama_chat } in
      (match Pave.Provider.complete
        { config with endpoint = "https://evil.example/api/chat" }
        [Protocol.user "Do not leak credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> failwith "cloud credential sent to an untrusted endpoint");
      (match Pave.Provider.complete
        { config with api_key = String.make 8193 'x' }
        [Protocol.user "Do not send oversized credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> failwith "oversized cloud credential was sent");
      let before = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in before)
        (fun () -> input_line before) = "1");
      let first = Pave.Provider.complete config [Protocol.user "Look this up"] [tool] in
      let call = match first.tool_calls with
        | [call] when call.name = "lookup" && call.arguments = args -> call
        | _ -> failwith "Ollama Cloud did not return its native tool call" in
      let final = Pave.Provider.complete config [Protocol.user "Look this up";
        first; Protocol.tool_result call.id "found"] [tool] in
      assert (final.content = Some "The answer is found.");
      let input = open_in state in
      let count = Fun.protect ~finally:(fun () -> close_in input)
        (fun () -> int_of_string (input_line input)) in
      assert (count = 3));
    print_endline "Ollama Cloud native authenticated discovery and tool round-trip: ok")
