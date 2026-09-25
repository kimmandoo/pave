module Token = Pave.Xiaomi_token_api
module Protocol = Pave.Protocol
let field = Protocol.member

let model = "my-plan-chat-model"
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let arguments = `Assoc ["number", `Int 6]
let call_id = "call_plan_42"
let reasoning = "I need to compute six times seven using the function."
let user = `Assoc ["role", `String "user";
  "content", `String "What is six times seven?"]
let fail reason = failwith ("Xiaomi Token Plan fixture: " ^ reason)

(* Fake HTTPS executable observes the *production* curl request, with the
   pinned region, scoped key and authentic two-turn native function payload. *)
let fake_curl () =
  let lines = ref [] in
  (try while true do lines := input_line stdin :: !lines done
   with End_of_file -> ());
  let lines = List.rev !lines in
  let values name = List.filter_map (fun line ->
    let prefix = name ^ " = " in
    if String.starts_with ~prefix line then
      match Yojson.Basic.from_string
        (String.sub line (String.length prefix)
          (String.length line - String.length prefix)) with
      | `String value -> Some value
      | _ -> fail "invalid curl config"
    else None) lines in
  let one name = match values name with
    | [value] -> value | _ -> fail ("missing curl " ^ name) in
  assert (one "proto" = "=https");
  assert (not (List.mem "location" lines || List.mem "location-trusted" lines));
  assert (one "request" = "POST");
  assert (one "url" = Sys.getenv "PAVE_XIAOMI_TOKEN_FIXTURE_URL");
  let headers = values "header" in
  assert (List.filter (String.starts_with ~prefix:"api-key: ") headers =
    ["api-key: " ^ Sys.getenv "PAVE_XIAOMI_TOKEN_FIXTURE_KEY"]);
  assert (List.mem "Content-Type: application/json" headers);
  let state = Sys.getenv "PAVE_XIAOMI_TOKEN_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let body_path = one "data-binary" in
  assert (String.starts_with ~prefix:"@" body_path);
  let input = open_in_bin (String.sub body_path 1 (String.length body_path - 1)) in
  let body = Fun.protect ~finally:(fun () -> close_in input)
    (fun () -> Yojson.Basic.from_string
      (really_input_string input (in_channel_length input))) in
  assert (field "model" body = `String model);
  assert (field "stream" body = `Bool false);
  assert (field "tools" body = `List [tool]);
  let response = match step, field "messages" body with
    | 0, `List [first] when first = user ->
        {|{"object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"I need to compute six times seven using the function.","tool_calls":[{"id":"call_plan_42","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|}
    | 1, `List [first; assistant; result] when first = user ->
        assert (field "role" assistant = `String "assistant");
        assert (field "content" assistant = `Null);
        assert (field "reasoning_content" assistant = `String reasoning);
        assert (field "tool_calls" assistant = `List [Protocol.call_to_json
          { id = call_id; name = "multiply_seven"; arguments }]);
        assert (result = `Assoc ["role", `String "tool";
          "content", `String {|{"product":42}|};
          "tool_call_id", `String call_id]);
        {|{"object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Six times seven is 42."}}]}|}
    | _ -> fail "unexpected step or missing computed tool continuation" in
  let out = open_out state in
  output_string out (string_of_int (step + 1)); close_out out;
  let out = open_out_bin (one "output") in
  output_string out response; close_out out;
  print_string "200"; flush stdout

let regions : (Token.region * string * Pave.Provider.api) list = [
  Token.Ams, "tp-ams-fixture", Pave.Provider.Xiaomi_token_ams_chat;
  Token.Cn, "ttp-cn-fixture", Pave.Provider.Xiaomi_token_cn_chat;
  Token.Sgp, "tp-sgp-fixture", Pave.Provider.Xiaomi_token_sgp_chat]

let reject_headers ~region ~endpoint ~api_key =
  match Token.chat_headers ~region ~endpoint ~api_key with
  | exception Invalid_argument _ -> ()
  | _ -> fail "key escaped pinned region or invalid key was accepted"

let () =
  if Array.length Sys.argv >= 3 && Sys.argv.(1) = "--disable" &&
     Sys.argv.(2) = "--config" then
    (try fake_curl () with exn ->
       prerr_endline (Printexc.to_string exn); exit 2)
  else (
    let env_names = List.map (fun (region, _, _) -> Token.env_name region) regions in
    let previous = List.map (fun name -> name, Sys.getenv_opt name) env_names in
    Fun.protect ~finally:(fun () -> List.iter (fun (name, value) ->
      Unix.putenv name (Option.value ~default:"" value)) previous) (fun () ->
      List.iter (fun (region, key, _) ->
        Unix.putenv (Token.env_name region) key) regions;
      List.iter (fun (region, key, _) ->
        assert (Token.env_api_key region = Some key);
        assert (Token.chat_headers ~region ~endpoint:(Token.chat_url region)
          ~api_key:key = ["api-key: " ^ key]);
        List.iter (fun (other, other_key, _) ->
          if other <> region then (
            reject_headers ~region ~endpoint:(Token.chat_url other)
              ~api_key:key;
            reject_headers ~region ~endpoint:(Token.chat_url region)
              ~api_key:other_key)) regions;
        List.iter (fun endpoint -> reject_headers ~region ~endpoint ~api_key:key)
          [Pave.Xiaomi_api.chat_url;
           "https://attacker.example/v1/chat/completions"];
        List.iter (fun api_key -> reject_headers ~region
          ~endpoint:(Token.chat_url region) ~api_key)
          [""; "tp-"; "ttp-"; "sk-payg-key"; "tp-with\nInjected: yes";
           "ttp-with space"]) regions;
      let missing = Token.env_name Token.Sgp in
      Unix.putenv missing "";
      assert (Token.env_api_key Token.Sgp = None);
      assert (Token.env_api_key Token.Ams = Some "tp-ams-fixture");
      Unix.putenv missing "tp-sgp-fixture";
      let unsourced : Protocol.message = {
        (Protocol.user "hello") with role = "assistant"; content = None;
        tool_calls = [{id = call_id; name = "multiply_seven"; arguments}] } in
      (match Token.request ~model [unsourced] [tool] with
       | exception Protocol.Invalid_response _ -> ()
       | _ -> fail "tool continuation fabricated reasoning_content");
      (match Token.parse_completion (Yojson.Basic.from_string
        {|{"choices":[{"finish_reason":"tool_calls","message":{"content":null,"tool_calls":[{"id":"call_plan_42","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|}) with
       | exception Protocol.Invalid_response _ -> ()
       | _ -> fail "tool call without authentic reasoning_content accepted");
      let directory = Filename.temp_file "pave-xiaomi-token-" "" in
      Sys.remove directory; Unix.mkdir directory 0o700;
      let binary = Filename.concat directory "curl" in
      Unix.symlink (Unix.realpath Sys.executable_name) binary;
      let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
      Fun.protect ~finally:(fun () ->
        Unix.putenv "PATH" old_path;
        List.iter (fun name -> Unix.putenv name "")
          ["PAVE_XIAOMI_TOKEN_FIXTURE_URL"; "PAVE_XIAOMI_TOKEN_FIXTURE_KEY";
           "PAVE_XIAOMI_TOKEN_FIXTURE_STATE"];
        Sys.remove binary;
        List.iteri (fun index _ ->
          let state = Filename.concat directory (string_of_int index) in
          if Sys.file_exists state then Sys.remove state) regions;
        Unix.rmdir directory) (fun () ->
        Unix.putenv "PATH" (directory ^ ":" ^ old_path);
        List.iteri (fun index (region, key, api) ->
          let state = Filename.concat directory (string_of_int index) in
          let endpoint = Token.chat_url region in
          Unix.putenv "PAVE_XIAOMI_TOKEN_FIXTURE_URL" endpoint;
          Unix.putenv "PAVE_XIAOMI_TOKEN_FIXTURE_KEY" key;
          Unix.putenv "PAVE_XIAOMI_TOKEN_FIXTURE_STATE" state;
          let config : Pave.Provider.config = { endpoint; api_key = key;
            model; api } in
          List.iter (fun hostile ->
            match Pave.Provider.complete { config with endpoint = hostile }
              [Protocol.user "Do not leak credentials"] [] with
            | exception Pave.Provider.Provider_error _ -> ()
            | _ -> fail "cross-region or hostile endpoint accepted")
            (Pave.Xiaomi_api.chat_url ::
              "https://attacker.example/v1/chat/completions" ::
              List.filter_map (fun (other, _, _) ->
                if other = region then None else Some (Token.chat_url other))
                regions);
          List.iter (fun (other, other_key, _) ->
            if other <> region then
              match Pave.Provider.complete
                { config with api_key = other_key }
                [Protocol.user "Do not leak credentials"] [] with
              | exception Pave.Provider.Provider_error _ -> ()
              | _ -> fail "other region's configured key was accepted")
            regions;
          assert (not (Sys.file_exists state));
          let first = Pave.Provider.complete config
            [Protocol.user "What is six times seven?"] [tool] in
          let call = match first.tool_calls with
            | [call] when call.id = call_id &&
                call.name = "multiply_seven" && call.arguments = arguments -> call
            | _ -> fail "native plan function call not decoded" in
          assert (first.provider_state = Some (`Assoc [
            "reasoning_content", `String reasoning]));
          let computed = match field "number" call.arguments with
            | `Int number -> Yojson.Basic.to_string
                (`Assoc ["product", `Int (number * 7)])
            | _ -> fail "tool argument is not numeric" in
          let final = Pave.Provider.complete config
            [Protocol.user "What is six times seven?"; first;
              Protocol.tool_result call.id computed] [tool] in
          assert (final.content = Some "Six times seven is 42.");
          let input = open_in state in
          assert (Fun.protect ~finally:(fun () -> close_in input)
            (fun () -> input_line input) = "2")) regions));
    print_endline "Xiaomi Token Plan AMS/CN/SGP pinned HTTPS and native tool turns: ok")
