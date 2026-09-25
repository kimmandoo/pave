module Lane = Pave.Singularity_tech_api
module Protocol = Pave.Protocol
let field = Protocol.member

let key = "sk-lane-fixture-only"
let model = "example/explicit-fixture-only-model"
let call_id = "call_lane_284"
let arguments = `Assoc ["number", `Int 6]
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply a number by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]];
      "required", `List [`String "number"]]]]
let user = `Assoc ["role", `String "user";
  "content", `String "What is six times seven?"]
let fail reason = failwith ("SingularityAPI lane fixture: " ^ reason)

(* An executable named curl receives the real production HTTP config. It
   refuses non-HTTPS and redirects, and checks each request's destination. *)
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
  assert (List.mem ("Authorization: Bearer " ^ key) (values "header"));
  let state = Sys.getenv "PAVE_SINGULARITY_TECH_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 | 1 ->
        has "url" Lane.chat_url;
        has "request" "POST";
        assert (List.mem "Content-Type: application/json" (values "header"));
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
             assert (not (List.mem_assoc "reasoning_effort" fields));
             assert (not (List.mem_assoc "max_tokens" fields))
         | _ -> fail "Chat request is not an object");
        if step = 0 then (
          assert (field "messages" body = `List [user]);
          {|{"choices":[{"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"I need the product.","tool_calls":[{"id":"call_lane_284","type":"function","function":{"name":"multiply_seven","arguments":"{\"number\":6}"}}]}}]}|})
        else (
          (match field "messages" body with
           | `List [first; assistant; result] ->
               assert (first = user);
               assert (field "role" assistant = `String "assistant");
               assert (field "content" assistant = `Null);
               assert (field "reasoning_content" assistant =
                 `String "I need the product.");
               assert (field "tool_calls" assistant = `List [Protocol.call_to_json
                 { id = call_id; name = "multiply_seven"; arguments }]);
               assert (result = `Assoc ["role", `String "tool";
                 "content", `String {|{"product":42}|};
                 "tool_call_id", `String call_id])
           | _ -> fail "tool result missing from continuation");
          {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"Six times seven is 42."}}]}|})
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
    assert (Lane.chat_headers ~endpoint:Lane.chat_url ~api_key:key =
      ["Authorization: Bearer " ^ key]);
    List.iter (fun endpoint ->
      match Lane.chat_headers ~endpoint ~api_key:key with
      | exception Invalid_argument _ -> ()
      | _ -> fail "reserved key escaped pinned Chat host") [
        "https://api.singularityapi.dev/v1/chat/completions";
        "https://evil.example/v1/chat/completions"];
    (match Lane.chat_headers ~endpoint:Lane.chat_url ~api_key:"bad\nvalue" with
     | exception Invalid_argument _ -> ()
     | _ -> fail "header injection accepted");
    let directory = Filename.temp_file "pave-singularity-tech-" "" in
    Sys.remove directory; Unix.mkdir directory 0o700;
    let binary = Filename.concat directory "curl" in
    let state = Filename.concat directory "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let old_path = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PAVE_SINGULARITY_TECH_FIXTURE_STATE" "";
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) [binary; state];
      Unix.rmdir directory) (fun () ->
      Unix.putenv "PATH" (directory ^ ":" ^ old_path);
      Unix.putenv "PAVE_SINGULARITY_TECH_FIXTURE_STATE" state;
      let config : Pave.Provider.config = {
        endpoint = Lane.chat_url; api_key = key; model;
        api = Pave.Provider.Singularity_tech_chat } in
      (match Pave.Provider.complete
        { config with endpoint = "https://api.singularityapi.dev/v1/chat/completions" }
        [Protocol.user "Do not leak reserved credentials"] [] with
       | exception Pave.Provider.Provider_error _ -> ()
       | _ -> fail "reserved key sent to .dev gateway");
      assert (not (Sys.file_exists state));
      let first = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"] [tool] in
      let call = match first.tool_calls with
        | [call] when call.id = call_id && call.name = "multiply_seven" &&
          call.arguments = arguments -> call
        | _ -> fail "tool call ID or arguments lost" in
      assert (first.provider_state = Some (`Assoc [
        "reasoning_content", `String "I need the product."]));
      let result = match field "number" call.arguments with
        | `Int number -> Yojson.Basic.to_string
            (`Assoc ["product", `Int (number * 7)])
        | _ -> fail "unexpected tool argument" in
      let final = Pave.Provider.complete config
        [Protocol.user "What is six times seven?"; first;
          Protocol.tool_result call.id result] [tool] in
      assert (final.content = Some "Six times seven is 42.");
      let input = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in input)
        (fun () -> int_of_string (input_line input)) = 2));
    print_endline "Reserved lane pinned Chat tool continuation: ok")
