type scenario = Success | Denied | Invalid | Cancel | Truncated | Masked | Buffered | Error
let event json = "data: " ^ Yojson.Basic.to_string json ^ "\n\n"
let chunk ?(id = `Null) ?(name = "") fragment = event (`Assoc [
  "choices", `List [`Assoc ["index", `Int 0; "delta", `Assoc [
    "tool_calls", `List [`Assoc ["index", `Int 0; "id", id;
      "function", `Assoc ["name", `String name; "arguments", `String fragment]]]];
    "finish_reason", `Null]]])
let finish = event (`Assoc ["choices", `List [`Assoc ["index", `Int 0;
  "delta", `Assoc []; "finish_reason", `String "tool_calls"]]]) ^ "data: [DONE]\n\n"
let answer = event (`Assoc ["choices", `List [`Assoc ["index", `Int 0;
  "delta", `Assoc ["content", `String "done"]; "finish_reason", `String "stop"]]]) ^ "data: [DONE]\n\n"
let serve socket body =
  let client, _ = Unix.accept socket in
  let ic = Unix.in_channel_of_descr client and oc = Unix.out_channel_of_descr client in
  let length = ref 0 in
  let rec headers () =
    let line = input_line ic in
    if line <> "\r" && line <> "" then (
      let lower = String.lowercase_ascii line in
      if String.starts_with ~prefix:"content-length:" lower then
        length := int_of_string (String.trim (String.sub line 15 (String.length line - 15)));
      headers ()) in
  headers (); ignore (really_input_string ic !length);
  Printf.fprintf oc "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s" (String.length body) body;
  flush oc; close_in_noerr ic; close_out_noerr oc
let run scenario =
  let root = Filename.temp_file "pave-write-preview-" "" in
  Sys.remove root; Unix.mkdir root 0o700;
  let path = if scenario = Error then "missing/proof.md" else "proof.md" in
  let file = Filename.concat root "proof.md" in
  let content = "LIVE\n한글 🚀 split-secret" in
  let arguments = `Assoc (["path", `String path; "content", `String content] @
    if scenario = Invalid then ["unexpected", `Bool true] else []) in
  let raw = Yojson.Basic.to_string arguments in
  let first = chunk ~id:(`String "write-call") ~name:"write_file" "" in
  let fragments = List.init (String.length raw) (fun index ->
    chunk (String.make 1 raw.[index])) |> String.concat "" in
  let body = first ^ fragments ^ (if scenario = Truncated then "" else finish) in
  let body, final = if scenario = Buffered then
    Yojson.Basic.to_string (`Assoc ["choices", `List [`Assoc ["finish_reason", `String "tool_calls"; "message", `Assoc [
      "role", `String "assistant"; "content", `Null; "tool_calls", `List [`Assoc [
        "id", `String "write-call"; "type", `String "function";
        "function", `Assoc ["name", `String "write_file";
          "arguments", `String raw]]]]]]]),
    {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"done"}}]}|}
    else body, answer in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)); Unix.listen socket 2;
  let port = match Unix.getsockname socket with Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let child = Unix.fork () in
  if child = 0 then (
    (try serve socket body;
      if scenario <> Cancel && scenario <> Truncated then serve socket final;
      exit 0 with _ -> exit 2));
  Unix.close socket;
  Fun.protect ~finally:(fun () ->
    (try Unix.kill child Sys.sigkill with Unix.Unix_error _ -> ());
    ignore (Unix.waitpid [] child);
    if Sys.file_exists file then Sys.remove file;
    Unix.rmdir root) (fun () ->
    let events = ref [] and cancel = ref false and before_finish = ref false in
    let decoder = Pave.Write_preview.create () in
    let mask = if scenario = Masked then Some (Pave.Secret_mask.create ["split-secret"]) else None in
    let provider : Pave.Provider.config = { api = Pave.Provider.Openai_completions;
      endpoint = Printf.sprintf "http://127.0.0.1:%d/chat/completions" port;
      api_key = "mock"; model = "mock" } in
    let agent = Pave.Agent.create ~provider ~root ~system:"preview"
      ~stream:(scenario <> Buffered) ~preview_tools:true ?secret_mask:mask
      ~approval_mode:Pave.Approval.Ask_writes
      ~approve_tool:(fun request ->
        assert (not (Sys.file_exists file));
        if scenario = Masked then
          assert (List.for_all (fun detail ->
            let rec search index =
              if index + 12 > String.length detail then false
              else if String.sub detail index 12 = "split-secret" then true
              else search (index + 1) in
            not (search 0)) request.Pave.Approval.details);
        scenario <> Denied)
      ~on_tool_event:(fun event ->
        events := event :: !events;
        match event with
        | Pave.Agent.Tool_draft delta ->
            assert (not (Sys.file_exists file));
            Pave.Write_preview.feed decoder delta.fragment;
            let preview = Pave.Write_preview.snapshot decoder in
            if preview.path = Some path && List.exists (fun (_, line) -> line = "LIVE") preview.lines then (
              before_finish := true;
              if scenario = Cancel then cancel := true)
        | Pave.Agent.Tool_started { target; write_content; _ } ->
            assert (target = Some path);
            if scenario <> Invalid then assert (write_content = Some
              (if scenario = Masked then "LIVE\n한글 🚀 [redacted]" else content))
        | _ -> ()) ~on_event:ignore () in
    (match Pave.Agent.run ~cancel:(fun () -> !cancel) agent "write it" with
     | _ -> assert (scenario <> Cancel && scenario <> Truncated)
     | exception Pave.Provider.Cancelled -> assert (scenario = Cancel)
     | exception Pave.Provider.Provider_error reason ->
         if scenario <> Truncated then failwith ("unexpected preview provider error: " ^ reason));
    let drafts = List.filter (function Pave.Agent.Tool_draft _ -> true | _ -> false) !events in
    let endings = List.filter_map (function
      | Pave.Agent.Tool_draft_ended { key; call_id; valid } -> Some (key, call_id, valid)
      | _ -> None) !events in
    if scenario = Masked || scenario = Buffered then assert (drafts = [] && endings = [])
    else (
      assert (!before_finish);
      match endings with
      | [key, call_id, valid] ->
          assert (List.for_all (function Pave.Agent.Tool_draft delta ->
            delta.Pave.Protocol.key = key | _ -> true) drafts);
          assert (valid = (scenario <> Invalid && scenario <> Cancel && scenario <> Truncated));
          if valid then assert (call_id = Some "write-call")
      | _ -> failwith "draft not finalized exactly once");
    let written = scenario = Success || scenario = Buffered || scenario = Masked in
    if Sys.file_exists file <> written then
      failwith "write preview scenario produced an unexpected filesystem side effect";
    if written then (
      let ic = open_in_bin file in
      let actual = really_input_string ic (in_channel_length ic) in close_in ic;
      assert (actual = content));
    if scenario = Cancel || scenario = Truncated then
      assert (List.for_all (fun (message : Pave.Protocol.message) ->
        message.tool_calls = [] && message.role <> "tool") (Pave.Agent.messages agent)))
let () =
  List.iter run [Success; Denied; Invalid; Cancel; Truncated; Masked; Buffered; Error];
  print_endline "write draft lifecycle, approval, masking and no false write: ok"
