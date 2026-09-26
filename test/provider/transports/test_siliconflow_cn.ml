module Wire = Pave.Siliconflow_api
module Protocol = Pave.Protocol
let field = Protocol.member
let key = "private-cn-key"
let model = "account-future-cn-model"
let call_id = "cn-call-1"
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["factor", `Assoc ["type", `String "integer"]]]]]

let fake_curl () =
  let lines = ref [] in
  (try while true do lines := input_line stdin :: !lines done with End_of_file -> ());
  let entries name = List.filter_map (fun line ->
    let prefix = name ^ " = " in
    if String.starts_with ~prefix line then
      Some (Yojson.Basic.from_string (String.sub line (String.length prefix)
        (String.length line - String.length prefix)))
    else None) !lines in
  let one name = match entries name with [`String value] -> value | _ -> assert false in
  assert (one "proto" = "=https");
  assert (List.mem (`String ("Authorization: Bearer " ^ key)) (entries "header"));
  let state = Sys.getenv "PAVE_CN_FIXTURE_STATE" in
  let step = if Sys.file_exists state then (
    let input = open_in state in
    Fun.protect ~finally:(fun () -> close_in input)
      (fun () -> int_of_string (input_line input))) else 0 in
  let response = match step with
    | 0 ->
        assert (one "url" = Wire.cn_models_url);
        assert (one "request" = "GET");
        assert (one "max-filesize" = "1048576");
        {|{"data":[{"id":"account-future-cn-model"}]}|}
    | 1 | 2 ->
        assert (one "url" = Wire.cn_chat_url);
        assert (one "request" = "POST");
        let binary = one "data-binary" in
        assert (String.starts_with ~prefix:"@" binary);
        let input = open_in_bin (String.sub binary 1 (String.length binary - 1)) in
        let request = Fun.protect ~finally:(fun () -> close_in input)
          (fun () -> Yojson.Basic.from_string
            (really_input_string input (in_channel_length input))) in
        assert (field "model" request = `String model);
        assert (field "tools" request = `List [tool]);
        if step = 1 then
          {|{"choices":[{"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"reasoning_content":"Calculate the product.","tool_calls":[{"id":"cn-call-1","type":"function","function":{"name":"multiply","arguments":"{\"factor\":7}"}}]}}]}|}
        else (
          let messages = match field "messages" request with `List rows -> rows | _ -> assert false in
          assert (List.exists (fun message -> field "reasoning_content" message =
            `String "Calculate the product.") messages);
          assert (List.exists (fun message -> field "role" message = `String "tool" &&
            field "tool_call_id" message = `String call_id &&
            field "content" message = `String "21") messages);
          {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"The answer is 21."}}]}|})
    | _ -> failwith "unexpected China-region network request" in
  let output = open_out_bin (one "output") in
  output_string output response; close_out output;
  let record = open_out state in
  output_string record (string_of_int (step + 1)); close_out record;
  print_string "200"; flush stdout

let () =
  if Array.length Sys.argv >= 3 && Sys.argv.(1) = "--disable" && Sys.argv.(2) = "--config" then
    (try fake_curl () with exn -> prerr_endline (Printexc.to_string exn); exit 2)
  else (
    let dir = Filename.temp_file "pave-siliconflow-cn-" "" in
    Sys.remove dir; Unix.mkdir dir 0o700;
    let binary = Filename.concat dir "curl" in
    let state = Filename.concat dir "state" in
    Unix.symlink (Unix.realpath Sys.executable_name) binary;
    let prior = Option.value ~default:"/usr/bin:/bin" (Sys.getenv_opt "PATH") in
    Fun.protect ~finally:(fun () ->
      Unix.putenv "PATH" prior;
      Unix.putenv "PAVE_CN_FIXTURE_STATE" "";
      Sys.remove binary;
      if Sys.file_exists state then Sys.remove state;
      Unix.rmdir dir) (fun () ->
      Unix.putenv "PATH" (dir ^ ":" ^ prior);
      Unix.putenv "PAVE_CN_FIXTURE_STATE" state;
      let models = Result.map Pave.Model_discovery.model_ids
        (Pave.Model_discovery.discover ~provider:"siliconflow-cn"
          ~credential:(Pave.Model_discovery.Api_key key) ()) in
      assert (models = Ok [model]);
      let config : Pave.Provider.config = { endpoint = Wire.cn_chat_url;
        api_key = key; model; api = Pave.Provider.Siliconflow_cn_chat } in
      let reject config = match Pave.Provider.complete config [Protocol.user "do not leak"] [] with
        | exception Pave.Provider.Provider_error _ -> ()
        | _ -> failwith "regional API key escaped its pinned host" in
      reject {config with endpoint = Wire.chat_url};
      reject {config with endpoint = "https://evil.example/v1/chat/completions"};
      reject {config with api = Pave.Provider.Siliconflow_chat};
      let original = [Protocol.user "Three times the factor"] in
      let first = Pave.Provider.complete config original [tool] in
      let call = match first.tool_calls with [call] -> call | _ -> assert false in
      let factor = match field "factor" call.arguments with `Int factor -> factor | _ -> assert false in
      let second = Pave.Provider.complete config
        (original @ [first; Protocol.tool_result call.id (string_of_int (factor * 3))])
        [tool] in
      assert (second.content = Some "The answer is 21.");
      let record = open_in state in
      assert (Fun.protect ~finally:(fun () -> close_in record)
        (fun () -> int_of_string (input_line record)) = 3));
    print_endline "SiliconFlow China isolated credential and native tool round-trip: ok")
