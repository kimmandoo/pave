module Coding = Pave.Alibaba_coding_api
module Protocol = Pave.Protocol
let field = Protocol.member

let key = "sk-sp-private-fixture"
let model = "qwen3.7-plus"
let call_id = "call_coding_14"
let parameters = `Assoc ["type", `String "object";
  "properties", `Assoc ["left", `Assoc ["type", `String "integer"];
    "right", `Assoc ["type", `String "integer"]];
  "required", `List [`String "left"; `String "right"]]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "add";
    "description", `String "Add two integers";
    "parameters", parameters]]
let native_call = `Assoc ["id", `String call_id;
  "type", `String "function";
  "function", `Assoc ["name", `String "add";
    "arguments", `String {|{"left":8,"right":13}|}]]
let user = `Assoc ["role", `String "user";
  "content", `String "What is eight plus thirteen?"]
let fail reason = failwith ("Coding Plan fixture: " ^ reason)

(* A local replacement for curl inspects the production HTTPS POST config
   before issuing either simulated Chat completion. No vendor key is used. *)
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
  let endpoint = Sys.getenv "PAVE_ALIBABA_CODING_FIXTURE_ENDPOINT" in
  assert (endpoint = "https://coding.dashscope.aliyuncs.com/v1/chat/completions" ||
    endpoint = "https://coding-intl.dashscope.aliyuncs.com/v1/chat/completions");
  has "url" endpoint;
  has "proto" "=https";
  has "request" "POST";
  assert (not (List.mem "location" config));
  assert (not (List.mem "location-trusted" config));
  assert (List.mem ("Authorization: Bearer " ^ key) (values "header"));
  assert (List.mem "Content-Type: application/json" (values "header"));
  let state_path = Sys.getenv "PAVE_ALIBABA_CODING_FIXTURE_STATE" in
  let step = if Sys.file_exists state_path then (
    let input = open_in state_path in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
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
  (match body with
  | `Assoc fields ->
      assert (not (List.mem_assoc "enable_thinking" fields));
      assert (not (List.mem_assoc "tool_stream" fields))
  | _ -> fail "Chat request must be an object");
  let response = match step with
    | 0 ->
        assert (field "messages" body = `List [user]);
        {|{"id":"coding-1","object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"Add the two numbers.","tool_calls":[{"id":"call_coding_14","type":"function","function":{"name":"add","arguments":"{\"left\":8,\"right\":13}"}}]}}]}|}
    | 1 ->
        assert (field "messages" body = `List [user;
          `Assoc ["role", `String "assistant";
            "tool_calls", `List [native_call];
            "content", `String "";
            "reasoning_content", `String "Add the two numbers."];
          `Assoc ["role", `String "tool";
            "content", `String "21";
            "tool_call_id", `String call_id]]);
        {|{"id":"coding-2","object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Eight plus thirteen is 21."}}]}|}
    | _ -> fail "unexpected Coding Plan network request" in
  let state = open_out state_path in
  output_string state (string_of_int (step + 1)); close_out state;
  let output = open_out_bin (one "output") in
  output_string output response; close_out output;
  print_string "200"; flush stdout

let expect_rejected f = match f () with
  | exception Invalid_argument _ -> ()
  | _ -> fail "unsafe key/endpoint accepted"

let () =
  if Array.length Sys.argv >= 3 && Sys.argv.(1) = "--disable" && Sys.argv.(2) = "--config" then
    (try fake_curl () with exn ->
       prerr_endline (Printexc.to_string exn); exit 2)
  else (
    assert (Coding.china_chat_url =
      "https://coding.dashscope.aliyuncs.com/v1/chat/completions");
    assert (Coding.intl_chat_url =
      "https://coding-intl.dashscope.aliyuncs.com/v1/chat/completions");
    List.iter (fun endpoint ->
      assert (Coding.chat_headers ~endpoint ~api_key:key =
        ["Authorization: Bearer " ^ key]);
      List.iter (fun api_key ->
        expect_rejected (fun () -> Coding.chat_headers ~endpoint ~api_key))
        [""; "sk-payg-key"; "sk-sp-"; "sk-sp-injected\nheader"])
      [Coding.china_chat_url; Coding.intl_chat_url];
    List.iter (fun endpoint ->
      expect_rejected (fun () -> Coding.chat_headers ~endpoint ~api_key:key))
      ["https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions";
       "https://dashscope-intl.aliyuncs.com/compatible-mode/v1/chat/completions";
       "https://coding.dashscope.aliyuncs.com/v1/models";
       "https://attacker.example/v1/chat/completions"];
    let child = Unix.fork () in
    if child = 0 then (
      Unix.putenv "ALIBABA_CODING_PLAN_API_KEY" key;
      assert (Coding.env_api_key () = Some key);
      Unix.putenv "ALIBABA_CODING_PLAN_API_KEY" "sk-payg-key";
      assert (Coding.env_api_key () = None);
      exit 0);
    (match Unix.waitpid [] child with
    | _, Unix.WEXITED 0 -> ()
    | _ -> fail "Coding Plan environment key resolution failed");
    let directory = Filename.temp_file "pave-alibaba-coding-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_ALIBABA_CODING_FIXTURE_STATE" "";
      Unix.putenv "PAVE_ALIBABA_CODING_FIXTURE_ENDPOINT" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_ALIBABA_CODING_FIXTURE_STATE" state;
      List.iter (fun endpoint ->
        Unix.putenv "PAVE_ALIBABA_CODING_FIXTURE_ENDPOINT" endpoint;
        let config : Pave.Provider.config = {
          endpoint; api_key = key; model;
          api = Pave.Provider.Alibaba_coding_chat } in
        (match Pave.Provider.complete
          { config with endpoint = "https://attacker.example/v1/chat/completions" }
          [Protocol.user "Do not leak my key"] [] with
        | exception Pave.Provider.Provider_error _ -> ()
        | _ -> fail "Coding Plan key escaped pinned Chat endpoint");
        assert (not (Sys.file_exists state));
        let first = Pave.Provider.complete config
          [Protocol.user "What is eight plus thirteen?"] [tool] in
        let call = match first.tool_calls with
          | [call] when call.id = call_id && call.name = "add" -> call
          | _ -> fail "native Coding Plan tool call was lost" in
        let argument name = match field name call.arguments with
          | `Int n -> n | _ -> fail "non-integer function argument" in
        let result = string_of_int (argument "left" + argument "right") in
        assert (first.provider_state = Some (`Assoc
          ["reasoning_content", `String "Add the two numbers."]));
        let final = Pave.Provider.complete config
          [Protocol.user "What is eight plus thirteen?"; first;
           Protocol.tool_result call.id result] [tool] in
        assert (final.content = Some "Eight plus thirteen is 21.");
        let input = open_in state in
        assert (Fun.protect ~finally:(fun () -> close_in input)
          (fun () -> int_of_string (input_line input)) = 2);
        Sys.remove state)
        [Coding.china_chat_url; Coding.intl_chat_url]);
    print_endline "Coding Plan region-pinned native Chat function round-trip: ok")
