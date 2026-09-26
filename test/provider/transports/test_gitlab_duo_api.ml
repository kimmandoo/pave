module Duo = Pave.Gitlab_duo_api
module P = Pave.Protocol
let field = P.member
let account_token = "gitlab-pat-or-oauth-bearer"
let direct_token = "account-scoped-direct-access"
let model = "selected-upstream-model"
let tool_id = "server-call-17"
let tool = `Assoc ["type", `String "function";
  "function", `Assoc ["name", `String "multiply_seven";
    "description", `String "Multiply by seven";
    "parameters", `Assoc ["type", `String "object";
      "properties", `Assoc ["number", `Assoc ["type", `String "integer"]]]]]
let fail text = failwith ("GitLab Duo fixture: " ^ text)
let expect_invalid f = match f () with
  | exception Invalid_argument _ -> ()
  | _ -> fail "expected endpoint or credential guard"
let expect_protocol_invalid f = match f () with
  | exception P.Invalid_response _ -> ()
  | _ -> fail "expected malformed signed replay rejection"
let expect_error predicate = function
  | Error error when predicate error -> ()
  | _ -> fail "expected GitLab error"
let json = Yojson.Basic.to_string
let signed_thinking = `Assoc ["type", `String "thinking";
  "thinking", `String "Need a calculation.";
  "signature", `String "server-signed-anthropic-state"]
let redacted_thinking = `Assoc ["type", `String "redacted_thinking";
  "data", `String "server-redacted-signed-state"]
let signed_reasoning = `Assoc ["type", `String "reasoning";
  "id", `String "rs_gitlab_17"; "status", `String "completed";
  "summary", `List [];
  "encrypted_content", `String "server-encrypted-reasoning"]
let signed_call = `Assoc ["type", `String "function_call";
  "call_id", `String tool_id; "name", `String "multiply_seven";
  "arguments", `String {|{"number":6}|}]
let assert_initial route body =
  assert (field "model" body = `String model);
  (match route with
   | Duo.Anthropic ->
       assert (field "max_tokens" body = `Int 512);
       assert (field "messages" body = `List [`Assoc [
         "role", `String "user"; "content", `String "What is six times seven?"]]);
       assert (field "tools" body = `List [`Assoc [
         "name", `String "multiply_seven";
         "input_schema", field "parameters" (field "function" tool);
         "description", `String "Multiply by seven"]])
   | Duo.Openai_responses ->
       assert (field "input" body = `List [`Assoc [
         "role", `String "user"; "content", `List [`Assoc [
           "type", `String "input_text"; "text", `String "What is six times seven?"]]]]);
       assert (field "tools" body <> `Null);
       assert (field "store" body = `Bool false);
       assert (field "include" body = `List [`String "reasoning.encrypted_content"])
   | Duo.Openai_completions ->
       assert (field "messages" body = `List [`Assoc [
         "role", `String "user"; "content", `String "What is six times seven?"]]);
       assert (field "tools" body = `List [tool]))
let tool_completion route = match route with
  | Duo.Anthropic -> `Assoc ["type", `String "message";
      "role", `String "assistant"; "stop_reason", `String "tool_use";
      "content", `List [signed_thinking; redacted_thinking;
        `Assoc ["type", `String "tool_use";
          "id", `String tool_id; "name", `String "multiply_seven";
          "input", `Assoc ["number", `Int 6]]]]
  | Duo.Openai_responses -> `Assoc ["status", `String "completed";
      "output", `List [signed_reasoning; signed_call]]
  | Duo.Openai_completions -> `Assoc ["choices", `List [`Assoc [
      "finish_reason", `String "tool_calls";
      "message", `Assoc ["role", `String "assistant"; "content", `Null;
        "tool_calls", `List [`Assoc ["id", `String tool_id;
          "type", `String "function";
          "function", `Assoc ["name", `String "multiply_seven";
            "arguments", `String {|{"number":6}|}]]]]]]]
let answer_completion route number =
  let text = Printf.sprintf "Six times seven is %d." number in
  match route with
  | Duo.Anthropic -> `Assoc ["type", `String "message";
      "role", `String "assistant"; "stop_reason", `String "end_turn";
      "content", `List [`Assoc ["type", `String "text"; "text", `String text]]]
  | Duo.Openai_responses -> `Assoc ["status", `String "completed";
      "output", `List [`Assoc ["type", `String "message";
        "role", `String "assistant";
        "content", `List [`Assoc ["type", `String "output_text";
          "text", `String text]]]]]
  | Duo.Openai_completions -> `Assoc ["choices", `List [`Assoc [
      "finish_reason", `String "stop";
      "message", `Assoc ["role", `String "assistant"; "content", `String text]]]]
let answer_from_transcript route body =
  let history = match route with
    | Duo.Openai_responses -> field "input" body
    | _ -> field "messages" body in
  let output = match route, history with
    | Duo.Openai_responses, `List [_; reasoning; call; result] ->
        assert (reasoning = signed_reasoning);
        assert (call = signed_call);
        assert (field "type" result = `String "function_call_output");
        assert (field "call_id" result = `String tool_id);
        field "output" result
    | Duo.Anthropic, `List [_; assistant; result] ->
        assert (field "role" assistant = `String "assistant");
        (match field "content" assistant with
         | `List [thinking; redacted; call] ->
             assert (thinking = signed_thinking);
             assert (redacted = redacted_thinking);
             assert (field "type" call = `String "tool_use");
             assert (field "id" call = `String tool_id)
         | _ -> fail "lost signed Anthropic thinking or assistant tool turn");
        (match field "content" result with
         | `List [`Assoc _ as answer] ->
             assert (field "tool_use_id" answer = `String tool_id);
             field "content" answer
         | _ -> fail "lost Anthropic tool result")
    | Duo.Openai_completions, `List [_; assistant; result] ->
        assert (field "role" assistant = `String "assistant");
        assert (field "tool_calls" assistant <> `Null);
        assert (field "role" result = `String "tool");
        assert (field "tool_call_id" result = `String tool_id);
        field "content" result
    | _ -> fail "second request must replay exact signed assistant call and tool result" in
  match output with
  | `String text ->
      (match field "product" (Yojson.Basic.from_string text) with
       | `Int number -> number
       | _ -> fail "tool output was not passed to model")
  | _ -> fail "tool output is not a string"
let test_route route =
  let exchange_count = ref 0 and turns = ref 0 in
  let http ~url ~headers ~body =
    if url = Duo.direct_access_url then (
      incr exchange_count;
      assert (headers = ["Authorization", "Bearer " ^ account_token;
        "Content-Type", "application/json"]);
      assert (body = `Assoc ["feature_flags", `Assoc ["DuoAgentPlatformNext", `Bool true]]);
      let response = `Assoc ["token", `String direct_token;
        "headers", `Assoc ["X-Gitlab-Feature", `String "duo"]] in
      Ok (200, json response))
    else if url = Duo.endpoint route then (
      incr turns;
      assert (headers = (["Content-Type", "application/json";
        "Authorization", "Bearer " ^ direct_token;
        "X-Gitlab-Feature", "duo"] @
        if route = Duo.Anthropic then ["anthropic-version", "2023-06-01"] else []));
      if !turns = 1 then (assert_initial route body;
        Ok (200, json (tool_completion route)))
      else if !turns = 2 then
        Ok (200, json (answer_completion route (answer_from_transcript route body)))
      else fail "unexpected third turn")
    else fail ("credential escaped pinned GitLab origin: " ^ url) in
  let user = P.user "What is six times seven?" in
  let first = match Duo.complete ~http ~bearer:account_token ~route ~model
      ~max_tokens:512 [user] [tool] with
    | Ok answer -> answer
    | Error _ -> fail "first turn failed" in
  let call = match first.tool_calls with
    | [call] -> call
    | _ -> fail "model call missing" in
  assert (call.id = tool_id);
  assert (call.name = "multiply_seven");
  assert (call.arguments = `Assoc ["number", `Int 6]);
  let number = match field "number" call.arguments with `Int n -> n | _ -> fail "argument" in
  let tool_result = P.tool_result call.id
    (json (`Assoc ["product", `Int (number * 7)])) in
  (match first.provider_state with
   | None when route = Duo.Openai_completions -> ()
   | Some (`Assoc fields) when route <> Duo.Openai_completions ->
       let wrong_model = { first with provider_state = Some (`Assoc
         (List.map (fun (key, value) ->
           key, if key = "model" then `String "other-upstream-model" else value)
           fields)) } in
       expect_protocol_invalid (fun () -> Duo.request ~route ~model
         ~max_tokens:512 [user; wrong_model; tool_result] [tool]);
       let forged_arguments = { first with tool_calls = [
         { call with arguments = `Assoc ["number", `Int 8] }] } in
       expect_protocol_invalid (fun () -> Duo.request ~route ~model
         ~max_tokens:512 [user; forged_arguments; tool_result] [tool]);
       let strip key = function `Assoc fields ->
         `Assoc (List.remove_assoc key fields) | _ -> assert false in
       let malformed = match route with
         | Duo.Anthropic -> `Assoc ["type", `String "message";
             "role", `String "assistant"; "stop_reason", `String "tool_use";
             "content", `List [strip "signature" signed_thinking;
               redacted_thinking; `Assoc ["type", `String "tool_use";
                 "id", `String tool_id; "name", `String "multiply_seven";
                 "input", call.arguments]]]
         | Duo.Openai_responses -> `Assoc ["status", `String "completed";
             "output", `List [strip "encrypted_content" signed_reasoning;
               signed_call]]
         | Duo.Openai_completions -> assert false in
       expect_protocol_invalid (fun () ->
         Duo.parse_completion ~route ~model malformed)
   | _ -> fail "missing signed state on tool call");
  let second = match Duo.complete ~http ~bearer:account_token ~route ~model
      ~max_tokens:512 [user; first; tool_result] [tool] with
    | Ok answer -> answer
    | Error _ -> fail "continuation failed" in
  assert (second.content = Some "Six times seven is 42.");
  assert (second.tool_calls = []);
  assert (!exchange_count = 2 && !turns = 2)
let () =
  let old = Sys.getenv_opt "GITLAB_TOKEN" in
  Fun.protect ~finally:(fun () -> Unix.putenv "GITLAB_TOKEN"
      (Option.value ~default:"" old)) (fun () ->
    Unix.putenv "GITLAB_TOKEN" account_token;
    assert (Duo.credential () = Some account_token);
    Unix.putenv "GITLAB_TOKEN" "bad\nheader";
    assert (Duo.credential () = None));
  let get ~url ~headers =
    assert (url = Duo.account_url);
    assert (headers = ["Authorization", "Bearer " ^ account_token;
      "Accept", "application/json"]);
    Ok (200, {|{"id":917,"username":"authorized-user"}|}) in
  (match Duo.discover_account ~get ~bearer:account_token () with
   | Ok account -> assert (account.id = 917 && account.username = "authorized-user")
   | Error _ -> fail "account discovery failed");
  let count = ref 0 in
  expect_error ((=) Duo.Invalid_credential)
    (Duo.discover_account ~get:(fun ~url:_ ~headers:_ -> incr count; assert false)
      ~bearer:"bad\nheader" ());
  assert (!count = 0);
  expect_error ((=) (Duo.Http_error 403))
    (Duo.discover_account ~get:(fun ~url:_ ~headers:_ -> Ok (403, "denied"))
      ~bearer:account_token ());
  expect_error (function Duo.Invalid_response _ -> true | _ -> false)
    (Duo.discover_account ~get:(fun ~url:_ ~headers:_ -> Ok (200, {|{"id":5}|}))
      ~bearer:account_token ());
  let http ~url:_ ~headers:_ ~body:_ = incr count; assert false in
  expect_error ((=) Duo.Invalid_credential)
    (Duo.direct_access ~http ~bearer:"bad\r\nBearer: attacker" ());
  assert (!count = 0);
  let forged value =
    Duo.direct_access ~http:(fun ~url:_ ~headers:_ ~body:_ ->
      Ok (200, json (`Assoc ["token", `String direct_token;
        "headers", value]))) ~bearer:account_token () in
  List.iter (fun headers ->
    expect_error (function Duo.Invalid_response _ -> true | _ -> false)
      (forged headers)) [
        `Assoc ["Authorization", `String "Bearer attacker"];
        `Assoc ["X-Injected\r\nHost", `String "attacker"];
        `Assoc ["x-dupe", `String "a"; "X-Dupe", `String "b"];
        `Assoc ["X-Test", `String "ok\r\nHost: attacker"]];
  let access : Duo.access = { token = direct_token; headers = [] } in
  expect_invalid (fun () -> Duo.gateway_headers
    ~endpoint:"https://gitlab.com/v1/messages" access);
  assert (Duo.gateway_headers ~endpoint:Duo.anthropic_url access =
    ["Authorization: Bearer " ^ direct_token;
      "anthropic-version: 2023-06-01"]);
  List.iter test_route [Duo.Anthropic; Duo.Openai_responses; Duo.Openai_completions];
  print_endline "GitLab Direct Access account isolation and 3 native two-turn routes: ok"
