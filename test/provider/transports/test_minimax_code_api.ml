module Coding = Pave.Minimax_code_api
module Protocol = Pave.Protocol
let field = Protocol.member

let key = "sk-cp-private-fixture"
let model = "fixture-chat-model"
let call_id = "call_minimax_14"
let reasoning_details = `List [`Assoc [
  "type", `String "reasoning.text";
  "id", `String "reasoning-text-1";
  "format", `String "MiniMax-response-v1";
  "index", `Int 0;
  "text", `String "Add both values."]]
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
let fail reason = failwith ("MiniMax Token Plan fixture: " ^ reason)

(* This binary impersonates curl, inspecting the real HTTPS transport config
   and the two submitted requests; it never reaches MiniMax or uses a live key. *)
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
  let endpoint = Sys.getenv "PAVE_MINIMAX_CODE_FIXTURE_ENDPOINT" in
  assert (endpoint = Coding.intl_chat_url || endpoint = Coding.china_chat_url);
  assert (one "url" = endpoint);
  assert (one "proto" = "=https");
  assert (one "request" = "POST");
  assert (not (List.mem "location" config));
  assert (not (List.mem "location-trusted" config));
  assert (List.mem ("Authorization: Bearer " ^ key) (values "header"));
  assert (List.mem "Content-Type: application/json" (values "header"));
  let state_path = Sys.getenv "PAVE_MINIMAX_CODE_FIXTURE_STATE" in
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
  assert (field "reasoning_split" body = `Null);
  assert (field "tools" body = `List [tool]);
  (match body with
  | `Assoc fields ->
      assert (not (List.mem_assoc "thinking" fields));
      assert (not (List.mem_assoc "enable_thinking" fields))
  | _ -> fail "Chat request must be an object");
  let response = match step with
    | 0 ->
        assert (field "messages" body = `List [user]);
        {|{"id":"minimax-1","object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":"<think>Add both values.</think>","tool_calls":[{"id":"call_minimax_14","type":"function","function":{"name":"add","arguments":"{\"left\":8,\"right\":13}"}}]}}]}|}
    | 1 ->
        assert (field "messages" body = `List [user;
          `Assoc ["role", `String "assistant";
            "content", `String "<think>Add both values.</think>";
            "tool_calls", `List [native_call]];
          `Assoc ["role", `String "tool";
            "content", `String "21";
            "tool_call_id", `String call_id]]);
        {|{"id":"minimax-2","object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Eight plus thirteen is 21."}}]}|}
    | _ -> fail "unexpected MiniMax network request" in
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
    assert (Coding.intl_chat_url = "https://api.minimax.io/v1/chat/completions");
    assert (Coding.china_chat_url = "https://api.minimax.cn/v1/chat/completions");
    List.iter (fun endpoint ->
      assert (Coding.chat_headers ~endpoint ~api_key:key =
        ["Authorization: Bearer " ^ key]);
      List.iter (fun api_key ->
        expect_rejected (fun () -> Coding.chat_headers ~endpoint ~api_key))
        [""; "sk-payg-key"; "sk-cp-"; "sk-cp-injected\nheader"])
      [Coding.intl_chat_url; Coding.china_chat_url];
    List.iter (fun endpoint ->
      expect_rejected (fun () -> Coding.chat_headers ~endpoint ~api_key:key))
      ["https://api.minimaxi.com/v1/chat/completions";
       "https://api.minimax.io/v1/models";
       "https://api.minimax.cn/v1/models";
       "https://api.minimax.io/anthropic/v1/messages";
       "https://attacker.example/v1/chat/completions"];
    let child = Unix.fork () in
    if child = 0 then (
      Unix.putenv "MINIMAX_CODE_API_KEY" key;
      Unix.putenv "MINIMAX_CODE_CN_API_KEY" "sk-payg-key";
      assert (Coding.env_intl_api_key () = Some key);
      assert (Coding.env_china_api_key () = None);
      Unix.putenv "MINIMAX_CODE_CN_API_KEY" key;
      Unix.putenv "MINIMAX_CODE_API_KEY" "sk-payg-key";
      assert (Coding.env_china_api_key () = Some key);
      assert (Coding.env_intl_api_key () = None);
      exit 0);
    (match Unix.waitpid [] child with
    | _, Unix.WEXITED 0 -> ()
    | _ -> fail "MiniMax regional key resolution failed");
    let directory = Filename.temp_file "pave-minimax-code-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_MINIMAX_CODE_FIXTURE_STATE" "";
      Unix.putenv "PAVE_MINIMAX_CODE_FIXTURE_ENDPOINT" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_MINIMAX_CODE_FIXTURE_STATE" state;
      List.iter (fun endpoint ->
        Unix.putenv "PAVE_MINIMAX_CODE_FIXTURE_ENDPOINT" endpoint;
        let config : Pave.Provider.config = {
          endpoint; api_key = key; model;
          api = (if endpoint = Coding.intl_chat_url then
            Pave.Provider.Minimax_code_chat else
            Pave.Provider.Minimax_code_cn_chat) } in
        (match Pave.Provider.complete
          { config with endpoint = "https://attacker.example/v1/chat/completions" }
          [Protocol.user "Do not leak my key"] [] with
        | exception Pave.Provider.Provider_error _ -> ()
        | _ -> fail "MiniMax key escaped pinned Chat endpoint");
        (match Pave.Provider.complete
          { config with endpoint =
            (if endpoint = Coding.intl_chat_url then
               Coding.china_chat_url else Coding.intl_chat_url) }
          [Protocol.user "Do not cross plan regions"] [] with
        | exception Pave.Provider.Provider_error _ -> ()
        | _ -> fail "MiniMax regional credential crossed hosts");
        assert (not (Sys.file_exists state));
        let first = Pave.Provider.complete config
          [Protocol.user "What is eight plus thirteen?"] [tool] in
        let call = match first.tool_calls with
          | [call] when call.id = call_id && call.name = "add" -> call
          | _ -> fail "native MiniMax tool call was lost" in
        let argument name = match field name call.arguments with
          | `Int n -> n | _ -> fail "non-integer function argument" in
        let result = string_of_int (argument "left" + argument "right") in
        assert (first.provider_state = None);
        assert (first.content = Some "<think>Add both values.</think>");
        let final = Pave.Provider.complete config
          [Protocol.user "What is eight plus thirteen?"; first;
           Protocol.tool_result call.id result] [tool] in
        assert (final.content = Some "Eight plus thirteen is 21.");
        let input = open_in state in
        assert (Fun.protect ~finally:(fun () -> close_in input)
          (fun () -> int_of_string (input_line input)) = 2);
        Sys.remove state)
        [Coding.intl_chat_url; Coding.china_chat_url]);
    (* Split-thinking responses are optional: if returned, retain both native
       fields verbatim on the assistant turn without model-name dispatch. *)
    let split = Coding.parse_completion (Yojson.Basic.from_string
      {|{"choices":[{"finish_reason":"tool_calls","message":{"content":null,"reasoning_content":"Add both values.","reasoning_details":[{"type":"reasoning.text","id":"reasoning-text-1","format":"MiniMax-response-v1","index":0,"text":"Add both values."}],"tool_calls":[{"id":"call_minimax_14","type":"function","function":{"name":"add","arguments":"{\"left\":8,\"right\":13}"}}]}}]}|}) in
    assert (split.provider_state = Some (`Assoc
      ["reasoning_details", reasoning_details;
       "reasoning_content", `String "Add both values."]));
    assert (field "messages" (Coding.request ~model
      [Protocol.user "What is eight plus thirteen?"; split;
       Protocol.tool_result call_id "21"] [tool]) = `List [user;
        `Assoc ["role", `String "assistant";
          "tool_calls", `List [native_call];
          "content", `String "";
          "reasoning_details", reasoning_details;
          "reasoning_content", `String "Add both values."];
        `Assoc ["role", `String "tool";
          "content", `String "21";
          "tool_call_id", `String call_id]]);
    print_endline "MiniMax Token Plan region-pinned native Chat round-trip: ok")
