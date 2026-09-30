open Pave

let field = Protocol.member
let fail message = failwith ("Codex HTTPS fixture: " ^ message)
let item kind fields = `Assoc (("type", `String kind) :: fields)
let model = "gpt-6-luna"
let low_model = "model-with-low-only"
let key = "private-codex-token"
let account = "account-a"
let is_fake_curl =
  Array.length Sys.argv >= 3 && Sys.argv.(1) = "--disable" &&
  Sys.argv.(2) = "--config"
let state = if is_fake_curl then
  Sys.getenv "PAVE_CODEX_FIXTURE_STATE"
else Filename.temp_file "pave-codex-https-" ".step"

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))
let write_file path text =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc text)
let contains needle text =
  let n = String.length needle in
  let rec search i = i + n <= String.length text &&
    (String.sub text i n = needle || search (i + 1)) in
  search 0

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
      | _ -> fail "invalid curl configuration"
    else None) config in
  let one name = match values name with
    | [value] -> value | _ -> fail ("missing curl " ^ name) in
  let headers = values "header" in
  let step = int_of_string (read_file state) in
  assert (one "proto" = "=https");
  assert (not (List.mem "location" config));
  assert (not (List.mem "location-trusted" config));
  let scoped_account = if step = 6 || step = 16 then "account-b" else account in
  let scoped_key = if step = 6 || step = 16 then "other-codex-token" else key in
  assert (List.mem ("Authorization: Bearer " ^ scoped_key) headers);
  assert (List.mem ("chatgpt-account-id: " ^ scoped_account) headers);
  assert (List.mem "OpenAI-Beta: responses=experimental" headers);
  assert (List.mem ("version: " ^ Codex_wire.client_version) headers);
  let listing = if step <= 6 then step mod 2 = 0
    else List.mem step [7; 9; 11; 13; 14; 15; 16; 17; 18; 19; 21; 23] in
  if listing then (
    assert (one "url" = List.hd Codex_wire.models_urls);
    assert (one "request" = "GET");
    assert (one "max-filesize" = "1048576");
    assert (one "proxy" = "");
    assert (List.mem "Accept: application/json" headers))
  else (
    assert (one "url" = "https://chatgpt.com/backend-api/codex/responses");
    assert (one "request" = "POST");
    assert (List.mem "Accept: text/event-stream" headers);
    assert (List.mem "x-openai-internal-codex-responses-lite: true" headers =
      (step <> 8 && step <> 22));
    let body_path = one "data-binary" in
    assert (String.starts_with ~prefix:"@" body_path);
    let body = Yojson.Basic.from_string
      (read_file (String.sub body_path 1 (String.length body_path - 1))) in
    assert (field "model" body = `String (if step = 20 then low_model else model));
    assert (field "store" body = `Bool false);
    assert (field "stream" body = `Bool true);
    let expected_effort = match step with
      | 3 | 8 | 22 -> "high" | 10 -> "ultra" | 12 | 20 -> "low" | _ -> "medium" in
    assert (field "effort" (field "reasoning" body) = `String expected_effort);
    if step = 8 || step = 22 then (
      assert (field "context" (field "reasoning" body) = `Null);
      assert (field "instructions" body = `String "Be exact");
      match field "tools" body with
      | `List [tool] -> assert (field "type" tool = `String "function")
      | _ -> fail "Standard tools missing")
    else (
    assert (field "tools" body = `Null);
    assert (field "context" (field "reasoning" body) = `String "all_turns");
    assert (field "parallel_tool_calls" body = `Bool false);
    let inputs = match field "input" body with
      | `List inputs -> inputs | _ -> fail "missing Responses input" in
    (match inputs with
    | first :: instructions :: _ ->
        assert (field "type" first = `String "additional_tools");
        assert (field "role" first = `String "developer");
        assert (field "tools" first = `List [item "namespace" [
          "name", `String "functions"; "description", `String "";
          "tools", `List [item "function" [
            "name", `String "read_file";
            "parameters", `Assoc ["type", `String "object";
              "properties", `Assoc []]; "description", `String "Read a file"]]]]);
        assert (field "role" instructions = `String "developer")
    | _ -> fail "missing Lite tool namespace or developer instructions");
    if step = 3 then
      (match inputs with
      | [_; _; _; call; output; user] ->
          assert (field "namespace" call = `String "functions");
          assert (field "call_id" call = `String "call_1");
          assert (field "call_id" output = `String "call_1");
          assert (field "output" output = `String "file contents");
          assert (field "role" user = `String "user")
      | _ -> fail "Lite response or tool result was not replayed")));
  let levels values = "supported_reasoning_levels", `List (List.map (fun level ->
    `Assoc ["effort", `String level; "description", `String "Reported effort"]) values) in
  let listing_response ~lite metadata = 200,
    Yojson.Basic.to_string (`Assoc ["models", `List [
      `Assoc (["slug", `String model; "supported_in_api", `Bool true;
        "use_responses_lite", `Bool lite; "default_reasoning_level", `String "medium"] @ metadata)]]) in
  let status, response = match step with
    | 0 | 2 | 4 | 9 -> listing_response ~lite:true [levels ["medium"; "high"; "ultra"]]
    | 7 -> listing_response ~lite:false [levels ["medium"; "high"]]
    | 11 | 17 -> listing_response ~lite:true []
    | 13 -> listing_response ~lite:true [levels []]
    | 14 | 16 -> listing_response ~lite:true [levels ["low"]]
    | 15 -> listing_response ~lite:true ["supported_reasoning_levels", `List [`String "high"]]
    | 18 | 19 | 21 | 23 ->
        200, Yojson.Basic.to_string (`Assoc ["models", `List [
          `Assoc ["slug", `String model; "supported_in_api", `Bool true;
            "use_responses_lite", `Bool false; levels ["high"]];
          `Assoc ["slug", `String low_model; "supported_in_api", `Bool true;
            "use_responses_lite", `Bool true;
            "default_reasoning_level", `String "low"; levels ["low"]]]])
    | 1 ->
        let output = item "function_call" ["id", `String "fc_1";
          "status", `String "completed"; "call_id", `String "call_1";
          "namespace", `String "functions"; "name", `String "read_file";
          "arguments", `String {|{"path":"README.md"}|}] in
        200, "data: " ^ Yojson.Basic.to_string (item "response.output_item.added" [
          "output_index", `Int 0; "item", item "function_call" [
            "id", `String "fc_1"; "call_id", `String "call_1";
            "name", `String "read_file"; "arguments", `String ""]]) ^ "\n\n" ^
        "data: " ^ Yojson.Basic.to_string (item "response.completed" [
          "response", `Assoc ["id", `String "resp_1";
            "status", `String "completed"; "output", `List [output]]]) ^ "\n\n"
    | 3 | 8 | 10 | 12 | 20 | 22 ->
        let output = item "message" ["id", `String "msg_2";
          "role", `String "assistant"; "status", `String "completed";
          "content", `List [item "output_text" ["text", `String "Read."]]] in
        200, "data: " ^ Yojson.Basic.to_string (item "response.completed" [
          "response", `Assoc ["id", `String "resp_2";
            "status", `String "completed"; "output", `List [output]]]) ^ "\n\n"
    | 5 -> 400,
        {|{"error":{"code":"unsupported_value","message":"This model is not enabled for private-codex-token on account-a"}}|}
    | 6 -> 200, {|{"models":[{"slug":"other-model","use_responses_lite":false}]}|}
    | _ -> fail "unexpected request" in
  write_file state (string_of_int (step + 1));
  (if listing then write_file (one "output") response else (
    write_file (one "dump-header")
      (Printf.sprintf "HTTP/1.1 %d Mock\r\nContent-Type: application/json\r\n\r\n" status);
    print_string response));
  print_string (if listing then string_of_int status else "");
  flush stdout

let () = Provider.Test.use_curl_helper Sys.executable_name
let () =
  if is_fake_curl then
    (try fake_curl () with exn -> prerr_endline (Printexc.to_string exn); exit 2)
  else
    Fun.protect ~finally:(fun () -> Sys.remove state) (fun () ->
      Unix.putenv "PAVE_CODEX_FIXTURE_STATE" state;
      write_file state "0";
      let config : Provider.config = {
        api = Provider.Codex_responses;
        endpoint = "https://chatgpt.com/backend-api/codex/responses";
        model; api_key = "" } in
      let credential access account_id () : Provider.credentials =
        { access; account_id = Some account_id; residency = None } in
      let tool = item "function" ["function", `Assoc [
        "name", `String "read_file"; "description", `String "Read a file";
        "parameters", `Assoc ["type", `String "object"]]] in
      let system : Protocol.message = { role = "system";
        content = Some "Be exact"; tool_calls = []; tool_call_id = None;
        tool_result_content = None; provider_state = None; attachments = [] } in
      let first = Provider.complete ~authentication:Provider.OAuth
        ~resolve_credential:(credential key account) config
        [system; Protocol.user "Read the file"] [tool] in
      assert (first.tool_calls = [{ Protocol.id = "call_1";
        name = "read_file"; arguments = `Assoc ["path", `String "README.md"] }]);
      let second = Provider.complete ~authentication:Provider.OAuth ~thinking:"high"
        ~resolve_credential:(credential key account) config
        [system; Protocol.user "Read the file"; first;
         Protocol.tool_result "call_1" "file contents";
         Protocol.user "Summarize"] [tool] in
      assert (second.content = Some "Read.");
      (match Provider.complete ~authentication:Provider.OAuth
        ~resolve_credential:(credential key account) config
        [system; Protocol.user "Does this model work?"] [tool] with
      | exception Provider.Provider_error reason ->
          assert (String.starts_with
            ~prefix:"Request error: invalid provider request (HTTP 400)" reason);
          assert (contains "This model is not enabled" reason);
          assert (contains "HTTP 400" reason);
          assert (not (contains key reason));
          assert (not (contains account reason));
          assert (contains "[redacted]" reason)
      | _ -> fail "HTTP 400 was silently accepted");
      (match Provider.complete ~authentication:Provider.OAuth
        ~resolve_credential:(credential "other-codex-token" "account-b") config
        [system; Protocol.user "Read the file"] [tool] with
      | exception Provider.Provider_error _ -> ()
      | _ -> fail "model entitlement leaked across accounts");
      assert (read_file state = "7");
      let run ?thinking ?(access = key) ?(account_id = account)
          ?(selected_model = model) () =
        Provider.complete ~authentication:Provider.OAuth ?thinking
          ~resolve_credential:(credential access account_id)
          { config with model = selected_model }
          [system; Protocol.user "Describe the effort"] [tool] in
      assert ((run ~thinking:"high" ()).content = Some "Read.");
      assert ((run ~thinking:"ultra" ()).content = Some "Read.");
      assert ((run ~thinking:"low" ()).content = Some "Read.");
      let rejects ?access ?account_id ?selected_model effort expected_step =
        (match run ~thinking:effort ?access ?account_id ?selected_model () with
        | exception Provider.Provider_error _ -> ()
        | _ -> fail "unsupported effort reached inference");
        assert (read_file state = string_of_int expected_step) in
      rejects "high" 14;
      rejects "high" 15;
      rejects "high" 16;
      rejects ~access:"other-codex-token" ~account_id:"account-b" "high" 17;
      rejects "invented-effort" 18;
      rejects ~selected_model:low_model "high" 19;
      ignore (run ~selected_model:low_model ~thinking:"low" ());
      assert (read_file state = "21");
      ignore (run ~thinking:"high" ());
      assert (read_file state = "23");
      rejects "low" 24;
      print_endline "Codex account/model-specific Standard/Lite HTTPS effort isolation: ok")
