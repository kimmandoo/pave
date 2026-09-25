module Gateway = Pave.Cloudflare_ai_gateway_api
module Protocol = Pave.Protocol
let field = Protocol.member
let key = "cf-aig-private-fixture-token"
let account_id = "0123456789abcdef0123456789abcdef"
let gateway_id = "workspace-gateway"
let model = "dynamic/my-account-route"
let call_id = "call_cloudflare_tool_01"
let arguments = `Assoc ["number", `Int 6]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let reasoning = "Need calculator output."
let user = `Assoc ["role", `String "user";
  "content", `String "What is six times seven?"]
let fail reason = failwith ("Cloudflare gateway fixture: " ^ reason)

(* Self-executing curl replacement checks the generated, account-scoped HTTPS
   route and keeps the gateway token out of upstream Authorization entirely. *)
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
  assert (one "url" = Gateway.chat_url ~account_id ~gateway_id);
  assert (one "proto" = "=https");
  assert (one "request" = "POST");
  assert (not (List.mem "location" config));
  assert (not (List.mem "location-trusted" config));
  assert (List.mem ("cf-aig-authorization: Bearer " ^ key) (values "header"));
  assert (not (List.exists (fun header ->
    String.starts_with ~prefix:"Authorization:" header ||
    String.starts_with ~prefix:"x-api-key:" header) (values "header")));
  let state = Sys.getenv "PAVE_CLOUDFLARE_GATEWAY_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
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
  let response = match step with
    | 0 ->
        assert (field "messages" body = `List [user]);
        {|{"id":"cf-aig-1","object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"Need calculator output.","tool_calls":[{"id":"call_cloudflare_tool_01","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|}
    | 1 ->
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
        {|{"id":"cf-aig-2","object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Six times seven is 42."}}]}|}
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
    let old_account = Sys.getenv_opt "CLOUDFLARE_ACCOUNT_ID" in
    let old_gateway = Sys.getenv_opt "CLOUDFLARE_GATEWAY_ID" in
    let old_key = Sys.getenv_opt "CLOUDFLARE_AI_GATEWAY_API_KEY" in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "CLOUDFLARE_ACCOUNT_ID" (Option.value ~default:"" old_account);
      Unix.putenv "CLOUDFLARE_GATEWAY_ID" (Option.value ~default:"" old_gateway);
      Unix.putenv "CLOUDFLARE_AI_GATEWAY_API_KEY" (Option.value ~default:"" old_key))
      (fun () ->
        Unix.putenv "CLOUDFLARE_ACCOUNT_ID" account_id;
        Unix.putenv "CLOUDFLARE_GATEWAY_ID" gateway_id;
        Unix.putenv "CLOUDFLARE_AI_GATEWAY_API_KEY" key;
        let endpoint = Gateway.chat_url ~account_id ~gateway_id in
        assert (Gateway.env_chat_url () = Some endpoint);
        assert (Gateway.env_api_key () = Some key);
        assert (Gateway.chat_headers ~endpoint ~api_key:key =
          ["cf-aig-authorization: Bearer " ^ key]);
        List.iter (fun url -> match Gateway.chat_headers ~endpoint:url ~api_key:key with
          | exception Invalid_argument _ -> ()
          | _ -> fail "credential escaped configured gateway") [
            "https://gateway.ai.cloudflare.com.evil.example/v1/" ^ account_id ^
              "/" ^ gateway_id ^ "/compat/chat/completions";
            Gateway.chat_url ~account_id ~gateway_id:"other-gateway";
            endpoint ^ "/../external" ];
        (match Gateway.chat_headers ~endpoint ~api_key:"bad\nheader" with
         | exception Invalid_argument _ -> ()
         | _ -> fail "header injection accepted");
        (match Gateway.chat_url ~account_id:(account_id ^ "/attack") ~gateway_id with
         | exception Invalid_argument _ -> ()
         | _ -> fail "untrusted Cloudflare account ID accepted");
        (match Gateway.chat_url ~account_id ~gateway_id:"evil/../gateway" with
         | exception Invalid_argument _ -> ()
         | _ -> fail "untrusted Cloudflare gateway ID accepted");
        let directory = Filename.temp_file "pave-cloudflare-gateway-" "" in
        Sys.remove directory; Unix.mkdir directory 0o700;
        let binary = Filename.concat directory "curl" in
        let state = Filename.concat directory "state" in
        Unix.symlink (Unix.realpath Sys.executable_name) binary;
        let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
        Fun.protect ~finally:(fun () ->
          Unix.putenv "PATH" old_path;
          Unix.putenv "PAVE_CLOUDFLARE_GATEWAY_FIXTURE_STATE" "";
          List.iter (fun path -> if Sys.file_exists path then Sys.remove path)
            [binary; state];
          Unix.rmdir directory) (fun () ->
          Unix.putenv "PATH" (directory ^ ":" ^ old_path);
          Unix.putenv "PAVE_CLOUDFLARE_GATEWAY_FIXTURE_STATE" state;
          let config : Pave.Provider.config = {
            endpoint; api_key = key; model;
            api = Pave.Provider.Cloudflare_ai_gateway_chat } in
          (match Pave.Provider.complete
            { config with endpoint = "https://evil.example/v1/chat/completions" }
            [Protocol.user "Never send this token elsewhere"] [] with
           | exception Pave.Provider.Provider_error _ -> ()
           | _ -> fail "gateway credential sent to untrusted endpoint");
          assert (not (Sys.file_exists state));
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
            (fun () -> int_of_string (input_line input)) = 2)));
    print_endline "Cloudflare account-scoped dynamic route tool continuation: ok")
