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


let key = "private-cloud-fixture"
let model = "next-cloud-model:77b"
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "lookup";
    "description", `String "Look up a value";
    "parameters", `Assoc ["type", `String "object"]]]
let args = `Assoc ["item", `String "current"]
let native_call = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "lookup"; "arguments", args]]

let fake_curl () =
  let lines = ref [] in
  (try while true do lines := input_line stdin :: !lines done
   with End_of_file -> ());
  let config = List.rev !lines in
  let values name =
    let prefix = name ^ " = " in
    List.filter_map (fun line ->
      if String.starts_with ~prefix line then
        match Yojson.Basic.from_string
          (String.sub line (String.length prefix)
            (String.length line - String.length prefix)) with
        | `String value -> Some value
        | _ -> failwith "invalid curl config value"
      else None) config in
  let one name = match values name with
    | [value] -> value
    | _ -> failwith ("missing curl " ^ name) in
  assert (List.mem ("Authorization: Bearer " ^ key) (values "header"));
  let state = Sys.getenv "PAVE_CLOUD_FIXTURE_STATE" in
  let step = if Sys.file_exists state then
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))
    else 0 in
  let response =
    match step with
    | 0 ->
        assert (one "url" = Cloud.tags_url && one "request" = "GET");
        {|{"models":[{"name":"next-cloud-model:cloud","model":"next-cloud-model:77b"}]}|}
    | 1 | 2 ->
        assert (one "url" = Cloud.chat_url && one "request" = "POST");
        let body_path = one "data-binary" in
        assert (String.starts_with ~prefix:"@" body_path);
        let input = open_in_bin (String.sub body_path 1 (String.length body_path - 1)) in
        let body = Fun.protect ~finally:(fun () -> close_in input)
          (fun () -> Yojson.Basic.from_string
            (really_input_string input (in_channel_length input))) in
        assert (field "model" body = `String model);
        assert (field "stream" body = `Bool false);
        if step = 1 then
          assert (field "messages" body =
            `List [`Assoc ["role", `String "user";
              "content", `String "Look this up"]])
        else
          (match field "messages" body with
           | `List [_user; assistant; result] ->
               assert (field "tool_calls" assistant = `List [native_call]);
               assert (field "role" result = `String "tool");
               assert (field "content" result = `String "found")
           | _ -> failwith "cloud tool result turn was not serialized");
        assert (field "tools" body = `List [tool]);
        if step = 1 then
          {|{"done":true,"done_reason":"tool_calls","message":{"role":"assistant","content":"","tool_calls":[{"type":"function","function":{"name":"lookup","arguments":{"item":"current"}}}]}}|}
        else
          {|{"done":true,"done_reason":"stop","message":{"role":"assistant","content":"The answer is found."}}|}
    | _ -> failwith "unexpected Ollama Cloud request" in
  let output = open_out_bin (one "output") in
  output_string output response;
  close_out output;
  let record = open_out state in
  output_string record (string_of_int (step + 1));
  close_out record;
  print_string "200"; flush stdout

let () = Pave.Provider.Test.use_curl_helper Sys.executable_name

let () =
  if Array.length Sys.argv >= 3 && Sys.argv.(1) = "--disable" &&
     Sys.argv.(2) = "--config" then fake_curl ()
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
    Sys.remove directory;
    Unix.mkdir directory 0o700;
    let state = Filename.concat directory "state" in
    let previous_state = Sys.getenv_opt "PAVE_CLOUD_FIXTURE_STATE" in
    Fun.protect
      ~finally:(fun () ->
        Unix.putenv "PAVE_CLOUD_FIXTURE_STATE"
          (Option.value ~default:"" previous_state);
        if Sys.file_exists state then Sys.remove state;
        Unix.rmdir directory)
      (fun () ->
        Unix.putenv "PAVE_CLOUD_FIXTURE_STATE" state;
        let discovered = match Cloud.discover ~api_key:key () with
          | Ok [id] -> id
          | _ -> failwith "default HTTP cloud discovery failed" in
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
        let first = Pave.Provider.complete config
          [Protocol.user "Look this up"] [tool] in
        let call = match first.tool_calls with
          | [call] when call.name = "lookup" && call.arguments = args -> call
          | _ -> failwith "Ollama Cloud did not return its native tool call" in
        let final = Pave.Provider.complete config
          [Protocol.user "Look this up"; first;
           Protocol.tool_result call.id "found"] [tool] in
        assert (final.content = Some "The answer is found.");
        let input = open_in state in
        let count = Fun.protect ~finally:(fun () -> close_in input)
          (fun () -> int_of_string (input_line input)) in
        assert (count = 3));
    print_endline "Ollama Cloud authenticated discovery and native tool round-trip: ok")
