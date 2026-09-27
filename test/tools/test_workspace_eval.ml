module Eval = Pave.Workspace_eval

let contains text fragment =
  let text_length = String.length text and fragment_length = String.length fragment in
  let rec find index =
    index + fragment_length <= text_length &&
    (String.sub text index fragment_length = fragment || find (index + 1))
  in
  find 0

let expect_error ?(contains_text = "") fn =
  match fn () with
  | _ -> failwith "expected Workspace_eval.Error"
  | exception Eval.Error message ->
      if contains_text <> "" && not (contains message contains_text) then
        failwith ("unexpected Workspace_eval.Error: " ^ message)

let output result =
  match result.Eval.error with
  | None -> result.Eval.output
  | Some message -> failwith ("evaluation failed: " ^ message)

let with_kernel owner language fn =
  let kernel = Eval.create ~owner language in
  Fun.protect ~finally:(fun () -> Eval.close kernel) (fun () -> fn kernel)

let response output error truncated =
  Yojson.Basic.to_string (`Assoc [
    "type", `String "result";
    "output", `String output;
    "error", (match error with None -> `Null | Some text -> `String text);
    "truncated", `Bool truncated;
  ])

let () =
  with_kernel "workspace-eval-state" Eval.Python (fun python ->
    let same = Eval.create ~owner:"workspace-eval-state" Eval.Python in
    assert (same == python);
    assert (output (Eval.evaluate python "counter = 40\nprint(counter + 2)") = "42\n");
    let continued = output (Eval.evaluate same "counter += 1\nprint(counter)") in
    if continued <> "41\n" then
      failwith (Printf.sprintf "Python kernel did not preserve state between calls: %S" continued);
    let python_bridge_calls = ref 0 in
    let bridged = Eval.evaluate
        ~tool_bridge:(fun ~name ~arguments ->
          assert (name = "workspace_read");
          assert (arguments = `Assoc ["path", `String "README"]);
          incr python_bridge_calls;
          "local file result")
        python
        ("bridge_count = globals().get('bridge_count', 0) + 1" ^ "\n" ^
         "first = pave.tool('workspace_read', {'path': 'README'})" ^ "\n" ^
         "second = pave.tool('workspace_read', {'path': 'README'})" ^ "\n" ^
         "print(first, second)") in
    assert (output bridged = "local file result local file result" ^ "\n");
    assert (!python_bridge_calls = 2);
    assert (output (Eval.evaluate python "print(bridge_count)") = "1" ^ "\n");
    let disabled_bridge = Eval.evaluate python "pave.tool('workspace_read', {})" in
    assert (Option.fold ~none:false
      ~some:(fun message -> contains message "bridge is not enabled") disabled_bridge.Eval.error);

    let other_session = Eval.create ~owner:"workspace-eval-isolated" Eval.Python in
    Fun.protect ~finally:(fun () -> Eval.close other_session) (fun () ->
      assert (output (Eval.evaluate other_session "print('counter' in globals())") = "False\n");
      assert (output (Eval.evaluate python "print(counter)") = "41\n"));

    let javascript = Eval.create ~owner:"workspace-eval-state" Eval.JavaScript in
    Fun.protect ~finally:(fun () -> Eval.close javascript) (fun () ->
      assert (output (Eval.evaluate javascript "console.log(typeof counter)") = "undefined\n");
      assert (output (Eval.evaluate javascript "let total = 6; console.log(total)") = "6\n");
      assert (output (Eval.evaluate javascript "total += 1; console.log(total)") = "7\n");
      let javascript_bridge_calls = ref 0 in
      let bridged = Eval.evaluate
          ~tool_bridge:(fun ~name ~arguments ->
            assert (name = "workspace_read");
            assert (arguments = `Assoc ["path", `String "src"]);
            incr javascript_bridge_calls;
            "javascript local result")
          javascript
          "let bridgeResult = pave.tool('workspace_read', {path: 'src'}); console.log(bridgeResult)" in
      assert (output bridged = "javascript local result" ^ "\n");
      assert (!javascript_bridge_calls = 1);
      assert (output (Eval.evaluate javascript "console.log(bridgeResult, total)") =
        "javascript local result 7" ^ "\n");
      Eval.reset javascript;
      assert (output (Eval.evaluate javascript "console.log(typeof total)") = "undefined\n"));

    Eval.reset python;
    assert (output (Eval.evaluate python "print('counter' in globals())") = "False\n");
    let bridge_called = ref false in
    let invalid_name = Eval.evaluate
        ~tool_bridge:(fun ~name:_ ~arguments:_ -> bridge_called := true; "unexpected")
        python "pave.tool('bad/name', {})" in
    assert (Option.fold ~none:false
      ~some:(fun message -> contains message "tool name or arguments are invalid") invalid_name.Eval.error);
    let oversized_arguments = Eval.evaluate
        ~tool_bridge:(fun ~name:_ ~arguments:_ -> bridge_called := true; "unexpected")
        python "pave.tool('workspace_read', {'payload': 'x' * 17000})" in
    assert (Option.fold ~none:false
      ~some:(fun message -> contains message "pave.tool argument") oversized_arguments.Eval.error);
    assert (not !bridge_called);
    let oversized_result = Eval.evaluate
        ~tool_bridge:(fun ~name:_ ~arguments:_ -> String.make 32_769 'r')
        python "pave.tool('workspace_read', {})" in
    assert (Option.fold ~none:false
      ~some:(fun message -> contains message "result exceeds its size limit") oversized_result.Eval.error);

    let bridge_cancelled = ref false in
    expect_error ~contains_text:"cancelled" (fun () ->
      Eval.evaluate
        ~cancel:(fun () -> !bridge_cancelled)
        ~tool_bridge:(fun ~name:_ ~arguments:_ -> bridge_cancelled := true; "not delivered")
        python
        ("bridge_before_cancel = True" ^ "\n" ^
         "print(pave.tool('workspace_read', {}))" ^ "\n" ^
         "print('BRIDGE_CANCEL_LEAK')"));
    let after_bridge_cancel = Eval.evaluate python "print('bridge_before_cancel' in globals())" in
    assert (output after_bridge_cancel = "False" ^ "\n");
    assert (not (contains after_bridge_cancel.Eval.output "BRIDGE_CANCEL_LEAK"));

    let denied = Eval.evaluate python "import os" in
    assert (Option.fold ~none:false
      ~some:(fun message -> contains message "package is not permitted") denied.Eval.error);
    let denied_install = Eval.evaluate python "import pip" in
    assert (Option.fold ~none:false
      ~some:(fun message -> contains message "package is not permitted") denied_install.Eval.error);
    let allowed = Eval.evaluate python "import math\nprint(math.sqrt(9))" in
    assert (output allowed = "3.0\n");
    expect_error ~contains_text:"source exceeds" (fun () ->
      Eval.evaluate python (String.make (Eval.max_source_bytes + 1) ' '));

    let exactly_full = Eval.evaluate python "print('x' * 65536, end='')" in
    assert (String.length exactly_full.Eval.output = Eval.max_output_bytes);
    assert (not exactly_full.Eval.truncated);
    let too_large = Eval.evaluate python "print('y' * 70000, end='')" in
    assert (String.length too_large.Eval.output = Eval.max_output_bytes);
    assert too_large.Eval.truncated;

    expect_error ~contains_text:"timed out" (fun () ->
      Eval.evaluate ~timeout_seconds:1 python
        "print('TIMEOUT_LEAK');\nwhile True: pass");
    let after_timeout = Eval.evaluate python "print('fresh kernel')" in
    assert (output after_timeout = "fresh kernel\n");
    assert (not (contains after_timeout.Eval.output "TIMEOUT_LEAK"));

    let cancel_started = Unix.gettimeofday () in
    expect_error ~contains_text:"cancelled" (fun () ->
      Eval.evaluate ~timeout_seconds:5
        ~cancel:(fun () -> Unix.gettimeofday () -. cancel_started > 0.1)
        python "print('CANCEL_LEAK');\nwhile True: pass");
    let after_cancel = Eval.evaluate python "print('cancel reset')" in
    assert (output after_cancel = "cancel reset\n");
    assert (not (contains after_cancel.Eval.output "CANCEL_LEAK")));

  with_kernel "workspace-eval-js-output" Eval.JavaScript (fun javascript ->
    let exactly_full = Eval.evaluate javascript "console.log('x'.repeat(65535))" in
    assert (String.length exactly_full.Eval.output = Eval.max_output_bytes);
    assert (not exactly_full.Eval.truncated);
    let too_large = Eval.evaluate javascript "console.log('y'.repeat(70000))" in
    assert (String.length too_large.Eval.output = Eval.max_output_bytes);
    assert too_large.Eval.truncated;
    let denied_import = Eval.evaluate javascript "require('node:fs')" in
    assert (Option.fold ~none:false
      ~some:(fun message -> contains message "package imports are disabled") denied_import.Eval.error);
    let denied_install = Eval.evaluate javascript "require('npm')" in
    assert (Option.fold ~none:false
      ~some:(fun message -> contains message "package imports are disabled") denied_install.Eval.error));

  let missing_runtime = fun _language -> {
    Eval.start = (fun () -> raise (Eval.Error "node runtime is unavailable (fixed launcher not found)"));
    exchange = (fun ~request:_ ~timeout_seconds:_ ~cancel:_ ~tool_bridge:_ ->
      response "" None false);
    close = (fun () -> ());
  } in
  expect_error ~contains_text:"runtime is unavailable" (fun () ->
    Eval.create ~launcher:missing_runtime ~owner:"workspace-eval-missing-runtime" Eval.JavaScript);

  let close_count = ref 0 in
  let cancellable_transport = fun _language -> {
    Eval.start = (fun () -> ());
    exchange = (fun ~request:_ ~timeout_seconds:_ ~cancel ~tool_bridge:_ ->
      while not (cancel ()) do Thread.delay 0.01 done;
      raise (Eval.Error "workspace evaluation cancelled"));
    close = (fun () -> incr close_count);
  } in
  let fake = Eval.create ~launcher:cancellable_transport
      ~owner:"workspace-eval-transport-cleanup" Eval.Python in
  let cancel_started = Unix.gettimeofday () in
  expect_error ~contains_text:"cancelled" (fun () ->
    Eval.evaluate ~cancel:(fun () -> Unix.gettimeofday () -. cancel_started > 0.05)
      fake "print('not returned')");
  assert (!close_count = 1);
  Eval.close fake;

  let disposable = Eval.create ~owner:"workspace-eval-disposal" Eval.Python in
  Eval.close disposable;
  expect_error ~contains_text:"closed" (fun () -> Eval.evaluate disposable "print('closed')");
  with_kernel "workspace-eval-disposal" Eval.Python (fun fresh ->
    assert (output (Eval.evaluate fresh "print('new session')") = "new session\n");
    assert (fresh != disposable));
  print_endline "workspace eval: ok"
