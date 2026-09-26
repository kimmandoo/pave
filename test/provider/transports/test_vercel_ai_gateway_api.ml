module Gateway = Pave.Vercel_ai_gateway_api
module Protocol = Pave.Protocol
let field = Protocol.member
let key = "vercel-private-fixture-key"
let model = "future-vercel-provider/future-model"
let call_id = "call_vercel_tool_01"
let arguments = `Assoc ["number", `Int 6]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let reasoning = "Use a calculator first."
let user = `Assoc ["role", `String "user";
  "content", `String "What is six times seven?"]
let fail reason = failwith ("Vercel gateway fixture: " ^ reason)

(* Stand-in curl enforces the actual HTTPS request configuration, including
   the credential destination and both sides of a native Chat tool exchange. *)
let fake_curl () =
  let config = ref [] in
  (try while true do config := input_line stdin :: !config done
   with End_of_file -> ());
  let config = List.rev !config in
  let values name = List.filter_map (fun line ->
    let prefix = name ^ " = " in
    if String.starts_with ~prefix line then
      match Yojson.Basic.from_string
        (String.sub line (String.length prefix)
          (String.length line - String.length prefix)) with
      | `String value -> Some value
      | _ -> fail "invalid curl config"
    else None) config in
  let one name = match values name with
    | [value] -> value | _ -> fail ("missing curl " ^ name) in
  let has name value = assert (one name = value) in
  has "proto" "=https";
  assert (not (List.mem "location" config));
  assert (not (List.mem "location-trusted" config));
  let state = Sys.getenv "PAVE_VERCEL_GATEWAY_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 ->
        has "url" Gateway.models_url;
        has "request" "GET";
        has "max-filesize" (string_of_int Gateway.max_response_bytes);
        assert (List.mem ("Authorization: Bearer " ^ key) (values "header"));
        {|{"object":"list","data":[{"id":"future-vercel-provider/future-model","object":"model"}]}|}
    | 1 | 2 ->
        has "url" Gateway.chat_url;
        has "request" "POST";
        assert (List.mem ("Authorization: Bearer " ^ key) (values "header"));
        let body_path = one "data-binary" in
        assert (String.starts_with ~prefix:"@" body_path);
        let input = open_in_bin
          (String.sub body_path 1 (String.length body_path - 1)) in
        let body = Fun.protect ~finally:(fun () -> close_in input)
          (fun () -> Yojson.Basic.from_string
            (really_input_string input (in_channel_length input))) in
        assert (field "model" body = `String model);
        assert (field "stream" body = `Bool false);
        assert (field "tools" body = `List [tool]);
        if step = 1 then (
          assert (field "messages" body = `List [user]);
          {|{"id":"vercel-1","object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"Use a calculator first.","tool_calls":[{"id":"call_vercel_tool_01","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|})
        else (
          (match field "messages" body with
           | `List [first; assistant; result] ->
               assert (first = user);
               assert (field "role" assistant = `String "assistant");
               assert (field "content" assistant = `Null);
               assert (field "reasoning_content" assistant = `String reasoning);
               assert (field "tool_calls" assistant = `List [Protocol.call_to_json
                 { id = call_id; name = "multiply_seven"; arguments }]);
               assert (result = `Assoc ["role", `String "tool";
                 "content", `String {|{"product":42}|};
                 "tool_call_id", `String call_id])
           | _ -> fail "missing native tool result in continuation");
          {|{"id":"vercel-2","object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Six times seven is 42."}}]}|})
    | _ -> fail "unexpected network request" in
  let count = open_out state in
  output_string count (string_of_int (step + 1)); close_out count;
  let output = open_out_bin (one "output") in
  output_string output response; close_out output;
  print_string "200"; flush stdout

let () =
  if Array.length Sys.argv >= 3 && Sys.argv.(1) = "--disable" && Sys.argv.(2) = "--config" then
    (try fake_curl () with exn ->
       prerr_endline (Printexc.to_string exn); exit 2)
  else (
    let http ~url ~headers =
      assert (url = Gateway.models_url);
      assert (List.mem ("Authorization", "Bearer " ^ key) headers);
      Ok (200, {|{"data":[{"id":"future-vercel-provider/future-model"},{"id":"future-vercel-provider/future-model"}]}|}) in
    assert (Gateway.discover ~http ~api_key:key () = Ok [model]);
    assert (Gateway.discover ~http ~api_key:"invalid\nkey" () = Error Gateway.Invalid_credential);
    assert (Gateway.discover ~http:(fun ~url:_ ~headers:_ -> Ok (302, "redirect"))
      ~api_key:key () = Error (Gateway.Http_error 302));
    assert (Gateway.chat_headers ~endpoint:Gateway.chat_url ~api_key:key =
      ["Authorization: Bearer " ^ key]);
    (match Gateway.chat_headers
      ~endpoint:"https://ai-gateway.vercel.sh.evil.example/v1/chat/completions"
      ~api_key:key with
     | exception Invalid_argument _ -> ()
     | _ -> fail "credential escaped pinned gateway");
    let directory = Filename.temp_file "pave-vercel-gateway-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_VERCEL_GATEWAY_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_VERCEL_GATEWAY_FIXTURE_STATE" state;
      let discovered = match Result.map Pave.Model_discovery.model_ids
        (Pave.Model_discovery.discover
          ~provider:"vercel-ai-gateway" ~credential:(Pave.Model_discovery.Api_key key) ()) with
        | Ok [id] -> id | _ -> fail "production model discovery failed" in
      assert (discovered = model);
      let config : Pave.Provider.config = {
        endpoint = Gateway.chat_url; api_key = key; model = discovered;
        api = Pave.Provider.Vercel_ai_gateway_chat } in
      (match Pave.Provider.complete
        { config with endpoint = "https://evil.example/v1/chat/completions" }
        [Protocol.user "Never send this token elsewhere"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> fail "gateway credential sent to untrusted endpoint");
      let first = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"] [tool] in
      let call = match first.tool_calls with
        | [call] when call.id = call_id && call.name = "multiply_seven" &&
          call.arguments = arguments -> call
        | _ -> fail "gateway tool call not decoded" in
      assert (first.provider_state = Some (`Assoc [
        "reasoning_content", `String reasoning]));
      let result = match field "number" call.arguments with
        | `Int number -> Yojson.Basic.to_string
            (`Assoc ["product", `Int (number * 7)])
        | _ -> fail "invalid tool argument" in
      let final = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"; first;
          Protocol.tool_result call.id result] [tool] in
      assert (final.content = Some "Six times seven is 42.");
      assert (final.provider_state = None);
      let input = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in input)
        (fun () -> int_of_string (input_line input)) = 3));
    print_endline "Vercel discovery and gateway tool continuation: ok")
