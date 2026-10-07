open Pave

open Local_tools

let check label condition = if not condition then failwith label
let assoc values = `Assoc values
let list values = `List values
let str value = `String value
let schema = assoc [
  "type", str "object";
  "properties", assoc [
    "text", assoc ["type", str "string"; "maxLength", `Int 64];
    "count", assoc ["type", str "integer"; "minimum", `Int 0;
      "maximum", `Int 3]];
  "required", list [str "text"];
  "additionalProperties", `Bool false]

let with_fixture callback =
  let base = Filename.temp_file "pave-local-tools-" "" in
  Unix.unlink base;
  Unix.mkdir base 0o700;
  let user_dir = Filename.concat base "user" in
  let root = Filename.concat base "workspace" in
  Unix.mkdir user_dir 0o700;
  Unix.mkdir root 0o700;
  let manifest = Filename.concat user_dir "tools.json" in
  Fun.protect ~finally:(fun () ->
    (try Unix.unlink manifest with Unix.Unix_error _ -> ());
    Unix.rmdir user_dir; Unix.rmdir root; Unix.rmdir base) (fun () ->
    callback ~user_dir ~root ~manifest)

let tool ?(parameters = schema) ?(name = "local_echo")
    ?(program = "/bin/cat") () = assoc [
  "name", str name;
  "description", str "Approved local stdin echo";
  "parameters", parameters;
  "program", str program;
  "arguments", list [];
  "timeoutSeconds", `Int 2]
let save manifest tools =
  let out = open_out_bin manifest in
  Fun.protect ~finally:(fun () -> close_out_noerr out) (fun () ->
    output_string out (Yojson.Basic.to_string (assoc [
      "version", `Int 1; "tools", list tools])));
  Unix.chmod manifest 0o600
let load_registry ~user_dir ~root manifest =
  match load ~user_dir ~root ~builtins:["run_command"] manifest with
  | Ok registry -> registry
  | Error _ -> failwith "expected valid trusted manifest"
let is_invalid = function Error (Invalid _) -> true | _ -> false

let () = with_fixture (fun ~user_dir ~root ~manifest ->
  save manifest [tool ()];
  let registry = load_registry ~user_dir ~root manifest in
  let session = create_session ~owner:"private-test" ~root ~registry ~opt_in:true in
  let input = assoc ["text", str "hello"; "count", `Int 2] in
  let calls = ref 0 and approvals = ref 0 in
  let fake ~cancel:_ _ = incr calls; Ok "fake" in
  let approve _ = incr approvals; true in
  check "headless must deny before approval and runner"
    (invoke ~runner:fake session ~name:"local_echo" ~input
       ~interactive:false ~approve = Error Approval_required &&
     !approvals = 0 && !calls = 0);
  check "duplicate argument fields denied before approval"
    (is_invalid (invoke ~runner:fake session ~name:"local_echo"
       ~input:(assoc ["text", str "x"; "text", str "y"])
       ~interactive:true ~approve) && !approvals = 0);
  check "typed integer refuses a floating-point argument"
    (is_invalid (invoke ~runner:fake session ~name:"local_echo"
       ~input:(assoc ["text", str "x"; "count", `Float 2.0])
       ~interactive:true ~approve) && !approvals = 0);
  check "approved isolated executable echoes typed JSON stdin"
    (invoke session ~name:"local_echo" ~input ~interactive:true ~approve =
     Ok (Yojson.Basic.to_string input));
  let events = ref [] in
  let source = tool_source (Option.get (find registry "local_echo")) in
  check "opt-in hook subscribes" (subscribe session ~source (fun event ->
    events := event :: !events));
  emit session Session_started;
  check "hook receives event" (!events = [Session_started]);
  let cancelled_runner ~cancel:_ _ = cancel session; incr calls; Ok "too late" in
  check "cancellation settles failure even after runner returns success"
    (invoke ~runner:cancelled_runner session ~name:"local_echo"
       ~input ~interactive:true ~approve = Error Cancelled && !calls = 1);
  emit session Turn_finished;
  check "cancel discards hooks" (!events = [Session_started]);
  check "cancel prevents further callbacks and execution"
    (invoke ~runner:fake session ~name:"local_echo" ~input
       ~interactive:true ~approve = Error Cancelled && !calls = 1);
  save manifest [tool ~name:"local_environment" ~program:"/usr/bin/env" ()];
  let environment_registry = load_registry ~user_dir ~root manifest in
  let environment_session = create_session ~owner:"env-test" ~root
    ~registry:environment_registry ~opt_in:true in
  Unix.putenv "PAVE_EXTENSION_SECRET_TEST" "never-pass-this-value";
  check "ambient credentials cannot reach approved executable"
    (match invoke environment_session ~name:"local_environment" ~input
       ~interactive:true ~approve with
     | Ok output ->
         not (List.exists
           (String.starts_with ~prefix:"PAVE_EXTENSION_SECRET_TEST=")
           (String.split_on_char '\n' output))
     | Error _ -> false);
  save manifest [tool ~name:"run_command" ()];
  check "built-in collision rejects entire manifest"
    (is_invalid (load ~user_dir ~root ~builtins:["run_command"] manifest));
  save manifest [tool (); tool ()];
  check "duplicate custom name rejects entire manifest"
    (is_invalid (load ~user_dir ~root ~builtins:[] manifest));
  save manifest [tool ~parameters:(assoc ["type", str "object";
    "properties", assoc []; "required", list []]) ()];
  check "open object schema rejected before registration"
    (is_invalid (load ~user_dir ~root ~builtins:[] manifest));
  save manifest [tool ()];
  Unix.chmod manifest 0o644;
  check "nonprivate manifest refused"
    (is_invalid (load ~user_dir ~root ~builtins:[] manifest));
  let disabled = create_session ~owner:"disabled" ~root ~registry ~opt_in:false in
  check "disabled hooks cannot subscribe"
    (not (subscribe disabled ~source (fun _ -> ())));
  check "disabled tools cannot execute"
    (match invoke ~runner:fake disabled ~name:"local_echo" ~input
       ~interactive:true ~approve with Error (Unavailable _) -> true | _ -> false))

let () = with_fixture (fun ~user_dir ~root ~manifest ->
  let parameters = assoc [
    "type", str "object";
    "properties", assoc ["text", assoc [
      "type", str "string"; "minLength", `Int 1; "maxLength", `Int 1;
      "enum", list [str "한"; str "😀"]]];
    "required", list [str "text"]; "additionalProperties", `Bool false] in
  save manifest [tool ~parameters ()];
  let registry = load_registry ~user_dir ~root manifest in
  let session = create_session ~owner:"unicode-test" ~root ~registry ~opt_in:true in
  let approvals = ref 0 and runs = ref 0 in
  let approve _ = incr approvals; true in
  let runner ~cancel:_ _ = incr runs; Ok "accepted" in
  List.iter (fun text ->
    check "local schema and enum bounds count Unicode scalars"
      (invoke ~runner session ~name:"local_echo" ~input:(assoc ["text", str text])
         ~interactive:true ~approve = Ok "accepted")) ["한"; "😀"];
  List.iter (fun text ->
    check "invalid Unicode and out-of-bound arguments never reach approval"
      (is_invalid (invoke ~runner session ~name:"local_echo"
        ~input:(assoc ["text", str text]) ~interactive:true ~approve)))
    [""; "한글"; "\255"];
  check "only valid Unicode calls reach approval and runner"
    (!approvals = 2 && !runs = 2))
