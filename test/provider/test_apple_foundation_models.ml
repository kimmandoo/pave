module Provider = Pave.Provider
module Protocol = Pave.Protocol
module Apple = Pave.Apple_foundation_models

let member = Protocol.member
let temp_root = Filename.concat (Filename.get_temp_dir_name ())
  ("pave-apple-model-test-" ^ string_of_int (Unix.getpid ()))
let helper_path = Filename.concat temp_root "apple-helper"
let capture_path = Filename.concat temp_root "request.json"
let test_executable =
  if Filename.is_relative Sys.executable_name then
    Filename.concat (Sys.getcwd ()) Sys.executable_name
  else Sys.executable_name

let write_file path contents mode =
  let channel = open_out_bin path in
  output_string channel contents;
  close_out channel;
  Unix.chmod path mode

let read_all_stdin () =
  let buffer = Buffer.create 256 in
  (try while true do
     Buffer.add_channel buffer stdin 4096
   done with End_of_file -> ());
  Buffer.contents buffer

let helper_main () =
  let request = read_all_stdin () in
  (match Sys.getenv_opt "PAVE_APPLE_FIXTURE_CAPTURE" with
   | Some path -> write_file path request 0o600
   | None -> ());
  match Sys.getenv_opt "PAVE_APPLE_FIXTURE_MODE" with
  | Some "valid" ->
      print_endline {|{"type":"text","text":"Local "}|};
      print_endline {|{"type":"text","text":"answer."}|};
      print_endline {|{"type":"done","message":"complete"}|};
      flush stdout
  | Some "malformed" -> print_endline "{"; flush stdout
  | Some "truncated" -> print_endline {|{"type":"text","text":"partial"}|}; flush stdout
  | Some "error" ->
      print_endline {|{"type":"error","message":"Apple Intelligence is disabled"}|};
      flush stdout;
      exit 1
  | Some "large" ->
      let payload = String.make 65_536 'x' in
      let event = Yojson.Basic.to_string (`Assoc [
        "type", `String "text"; "text", `String payload]) in
      for _ = 1 to 65 do print_endline event done;
      print_endline {|{"type":"done","message":"complete"}|};
      flush stdout
  | Some "hang" ->
      ignore request;
      let rec wait () = ignore (Unix.select [] [] [] 1.); wait () in
      wait ()
  | _ -> failwith "unknown Apple helper fixture mode"

let with_helper f =
  Unix.mkdir temp_root 0o700;
  Fun.protect ~finally:(fun () ->
    List.iter (fun path -> try Sys.remove path with Sys_error _ -> ())
      [helper_path; capture_path];
    try Unix.rmdir temp_root with Unix.Unix_error _ -> ()) (fun () ->
      write_file helper_path
        ("#!/bin/sh\nexec \"$PAVE_APPLE_FIXTURE_EXE\" --helper\n") 0o700;
      Unix.putenv "PAVE_APPLE_FIXTURE_EXE" test_executable;
      Unix.putenv "PAVE_APPLE_FIXTURE_CAPTURE" capture_path;
      f ())

let configuration ?(model = "default") ?(api_key = "") () =
  { Provider.endpoint = ""; api_key; model;
    api = Provider.Apple_foundation_models }

let contains value fragment =
  let value_length = String.length value and fragment_length = String.length fragment in
  let rec search index =
    index + fragment_length <= value_length &&
    (String.sub value index fragment_length = fragment || search (index + 1))
  in
  search 0
let expect_error ?(authentication = Provider.Api_key) ?(tools = [])
    ?(messages = [Protocol.user "hello"]) config expected =
  try
    ignore (Provider.complete ~authentication ~apple_helper_path:helper_path
      config messages tools);
    failwith ("expected Apple model error containing " ^ expected)
  with Provider.Provider_error message ->
    if not (contains message expected) then
      failwith ("unexpected Apple model error: " ^ message)

let tests () =
  with_helper (fun () ->
    Unix.putenv "PAVE_APPLE_FIXTURE_MODE" "valid";
    let streamed = Buffer.create 32 in
    let user = Protocol.user "please inspect a workspace file" in
    let tool = `Assoc ["type", `String "function";
      "function", `Assoc ["name", `String "read_file";
        "description", `String "Read a workspace file";
        "parameters", `Assoc ["type", `String "object"]]] in
    let reply = Provider.complete ~apple_helper_path:helper_path
      ~on_text:(Buffer.add_string streamed) (configuration ()) [user] [tool] in
    assert (reply.role = "assistant");
    assert (reply.content = Some "Local answer.");
    assert (reply.tool_calls = []);
    assert (Buffer.contents streamed = "Local answer.");
    let request = Yojson.Basic.from_file capture_path in
    assert (member "action" request = `String "complete");
    assert (member "model" request = `String "default");
    assert (member "messages" request = Protocol.chat_messages_to_json [user]);
    assert (member "tools" request = `Null);

    Unix.putenv "PAVE_APPLE_FIXTURE_MODE" "error";
    expect_error (configuration ()) "Apple Intelligence is disabled";
    Unix.putenv "PAVE_APPLE_FIXTURE_MODE" "malformed";
    expect_error (configuration ()) "invalid JSON";
    Unix.putenv "PAVE_APPLE_FIXTURE_MODE" "truncated";
    expect_error (configuration ()) "ended before completion";
    Unix.putenv "PAVE_APPLE_FIXTURE_MODE" "large";
    expect_error (configuration ()) "response exceeds the 4 MiB limit";

    expect_error (configuration ~model:"other-model" ()) "OS-managed model ID";
    expect_error (configuration ~api_key:"not-a-local-model-key" ()) "does not accept an API key";
    expect_error ~authentication:Provider.OAuth (configuration ()) "does not accept provider credentials";
    expect_error ~messages:[Protocol.user ~attachments:[
      { Protocol.name = "photo.png"; mime_type = "image/png"; data = "aGVsbG8=" }]
      "look"] (configuration ()) "does not support user media attachments";
    expect_error ~messages:[Protocol.user "continue";
      Protocol.tool_result_blocks "call-1" [Protocol.Image {
        mime_type = "image/png"; data = "aGVsbG8=" }]]
      (configuration ()) "does not support image tool results";

    Unix.putenv "PAVE_APPLE_FIXTURE_MODE" "hang";
    let started = Unix.gettimeofday () in
    (try
      ignore (Provider.complete ~apple_helper_path:helper_path
        ~cancel:(fun () -> Unix.gettimeofday () -. started >= 0.2)
        (configuration ()) [Protocol.user "cancel this"] []);
      failwith "cancelled Apple helper completed"
     with Provider.Cancelled -> ()));

  let launch = Filename.concat temp_root "pave" in
  let sibling_helper = Filename.concat temp_root Apple.helper_name in
  Unix.mkdir temp_root 0o700;
  Fun.protect ~finally:(fun () ->
    List.iter (fun path -> try Sys.remove path with Sys_error _ -> ())
      [launch; sibling_helper];
    try Unix.rmdir temp_root with Unix.Unix_error _ -> ()) (fun () ->
      write_file launch "binary" 0o700;
      write_file sibling_helper "" 0o700;
      assert (Apple.executable_path ~executable:launch () = Some sibling_helper);
      let helper_bytes = "embedded native helper" in
      let output = open_out_gen [Open_append; Open_binary] 0 launch in
      output_string output helper_bytes;
      output_string output (Printf.sprintf "%016Lx%s"
        (Int64.of_int (String.length helper_bytes)) Apple.embedded_helper_magic);
      close_out output;
      assert (Apple.embedded_helper_range ~executable:launch () =
        Some (launch, String.length "binary", String.length helper_bytes)));
  print_endline "Apple Foundation Models route, bounded streaming, cancellation, and embedded helper footer: ok"

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--helper" then helper_main ()
  else tests ()
