module P = Pave.Protocol
module Context = Pave.Project_context

let child = Filename.concat
let write path text =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc text)

let contains text fragment =
  let n = String.length fragment in
  let rec find i = i + n <= String.length text &&
    (String.sub text i n = fragment || find (i + 1)) in
  find 0

let tool id name path content =
  let arguments = if name = "read_file" then ["path", `String path]
    else ["path", `String path; "content", `String content] in
  let fn = `Assoc ["name", `String name;
    "arguments", `String (Yojson.Basic.to_string (`Assoc arguments))] in
  `Assoc ["id", `String id; "type", `String "function"; "function", fn]

let response calls =
  let message = `Assoc ["role", `String "assistant"; "content", `Null;
    "tool_calls", `List calls] in
  let choice = `Assoc ["finish_reason", `String "tool_calls";
    "message", message] in
  Yojson.Basic.to_string (`Assoc ["choices", `List [choice]])
let answer = {|{"choices":[{"finish_reason":"stop","message":{"role":"assistant","content":"done"}}]}|}

let request client =
  let ic = Unix.in_channel_of_descr client in
  let oc = Unix.out_channel_of_descr client in
  let length = ref 0 in
  let rec headers () =
    let line = input_line ic in
    if line <> "\r" && line <> "" then (
      let lower = String.lowercase_ascii line in
      if String.starts_with ~prefix:"content-length:" lower then
        length := int_of_string (String.trim (String.sub line 15 (String.length line - 15)));
      headers ()) in
  headers ();
  (Yojson.Basic.from_string (really_input_string ic !length), ic, oc)

let serve socket count check =
  try
    for step = 0 to count - 1 do
      let client, _ = Unix.accept socket in
      let req, ic, oc = request client in
      let body = check step req in
      Printf.fprintf oc
        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
        (String.length body) body;
      flush oc; close_in_noerr ic; close_out_noerr oc
    done;
    exit 0
  with exn -> prerr_endline (Printexc.to_string exn); exit 2

let with_server count check run =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 4;
  let port = match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let pid = Unix.fork () in
  if pid = 0 then serve socket count check;
  Unix.close socket;
  let reaped = ref false in
  Fun.protect ~finally:(fun () ->
    if not !reaped then (
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      ignore (Unix.waitpid [] pid)))
    (fun () ->
      let provider : Pave.Provider.config = {
        api = Pave.Provider.Openai_completions;
        endpoint = Printf.sprintf "http://127.0.0.1:%d/chat/completions" port;
        api_key = "mock"; model = "mock" } in
      run provider;
      let _, status = Unix.waitpid [] pid in
      reaped := true;
      assert (status = Unix.WEXITED 0))

let messages request = match P.member "messages" request with
  | `List messages -> messages | _ -> failwith "missing messages"
let system request = match messages request with
  | first :: _ -> P.member "content" first |> P.string
  | _ -> failwith "missing system"

let check_pairs journal =
  let rec walk = function
    | [] -> ()
    | (msg : P.message) :: rest when msg.role = "assistant" ->
      let rec consume calls rest = match calls, rest with
        | [], rest -> walk rest
        | call :: calls, result :: rest when result.role = "tool" &&
          result.tool_call_id = Some call.P.id -> consume calls rest
        | _ -> failwith "assistant tool calls lack adjacent results" in
      consume msg.tool_calls rest
    | msg :: _ when msg.role = "tool" -> failwith "orphan tool result"
    | _ :: rest -> walk rest in
  walk journal

let () =
  let root = Filename.temp_file "pave-scoped-" "" in
  Sys.remove root; Unix.mkdir root 0o700;
  let pave = child root ".pave" in
  let rules = child pave "rules" in
  Unix.mkdir pave 0o700; Unix.mkdir rules 0o700;
  let src = child root "src" and docs = child root "docs" in
  Unix.mkdir src 0o700; Unix.mkdir docs 0o700;
  let a = child src "A.swift" and b = child docs "B.md" in
  let pre = child src "Pre.swift" and secret = child root "secrets.txt" in
  write pre "struct Pre {}\n";
  write secret "Authorization: Bearer PRIVATE_TOKEN\n";
  write (child rules "a.md") "---\npaths: src/**\n---\nA_ONLY\n";
  write (child rules "b.md") "---\npaths: docs/**\n---\nB_ONLY\n";
  Fun.protect ~finally:(fun () ->
    List.iter (fun path -> if Sys.file_exists path then Sys.remove path)
      [a; b; pre; secret; child root "alias"; child rules "a.md";
        child rules "aa.md"; child rules "b.md"];
    Unix.rmdir src; Unix.rmdir docs; Unix.rmdir rules; Unix.rmdir pave; Unix.rmdir root)
    (fun () ->
      let check step req =
        let scope = system req in
        (match step with
         | 0 -> assert (not (contains scope "A_ONLY"));
           assert (not (Sys.file_exists a))
         | 1 -> assert (contains scope "A_ONLY");
           assert (not (contains scope "B_ONLY"));
           assert (not (contains scope "Bearer PRIVATE_TOKEN"));
           assert (not (Sys.file_exists a));
           let journal = messages req in
           assert (List.exists (fun msg ->
             P.member "tool_call_id" msg = `String "read-pre" &&
             (match P.member "content" msg with
              | `String text -> String.starts_with ~prefix:"struct Pre {}\n" text
              | _ -> false)) journal);
           assert (List.exists (fun msg ->
             P.member "tool_call_id" msg = `String "first-write" &&
             (match P.member "content" msg with
              | `String text -> contains text "withheld" | _ -> false)) journal)
         | 2 -> assert (Sys.file_exists a);
           assert (not (contains scope "A_ONLY"));
           assert (not (contains scope "B_ONLY"))
         | 3 -> assert (contains scope "B_ONLY");
           assert (not (contains scope "A_ONLY"));
           assert (not (Sys.file_exists b))
         | 4 -> assert (Sys.file_exists b)
         | _ -> assert false);
        match step with
        | 0 -> response [tool "read-secret" "read_file" "secrets.txt" "";
            tool "read-pre" "read_file" "src/Pre.swift" "";
            tool "first-write" "write_file" "src/A.swift" "A done"]
        | 1 -> response [tool "second-write" "write_file" "src/A.swift" "A done"]
        | 2 -> response [tool "first-b" "write_file" "docs/B.md" "B done"]
        | 3 -> response [tool "second-b" "write_file" "docs/B.md" "B done"]
        | _ -> answer in
      with_server 5 check (fun provider ->
        let agent = Pave.Agent.create ~provider ~root ~system:"mobile safety" ~on_event:ignore () in
        assert (Pave.Agent.run agent "Update both files" = "done");
        check_pairs (Pave.Agent.messages agent);
        assert (Sys.file_exists a && Sys.file_exists b));
      let cancelled = ref false in
      with_server 2 (fun step req ->
        assert (not (contains (system req) "A_ONLY"));
        if step = 0 then
          response [tool "withheld" "write_file" "src/A.swift" "changed";
            tool "skipped" "write_file" "docs/B.md" "changed"]
        else answer)
        (fun provider ->
          let agent = Pave.Agent.create ~provider ~root ~system:"mobile safety"
            ~on_event:(fun event -> if contains event "mutation withheld" then cancelled := true) () in
          (match Pave.Agent.run ~cancel:(fun () -> !cancelled) agent "Update files" with
           | exception Pave.Provider.Cancelled -> ()
           | _ -> failwith "expected cancellation");
          check_pairs (Pave.Agent.messages agent);
          assert (List.length (Pave.Agent.messages agent) = 4);
          assert (match List.nth (Pave.Agent.messages agent) 3 with
            | { P.content = Some text; _ } -> contains text "cancelled"
            | _ -> false);
          assert (not (contains (let ic = open_in a in
            Fun.protect ~finally:(fun () -> close_in ic) (fun () -> input_line ic)) "changed"));
          cancelled := false;
          assert (Pave.Agent.run agent "Unrelated follow-up" = "done"));
      write (child rules "a.md") "---\npaths: src/**\n---\n@../private-token.md";
      let failed = Context.resolve_scoped ~root ~path:"src/A.swift" () in
      assert (not failed.safe && failed.text = "");
      assert (List.exists (fun (d : Context.diagnostic) -> d.code = "unsafe_import") failed.diagnostics);
      with_server 2 (fun step req ->
        if step = 0 then (
          assert (not (contains (system req) "private-token"));
          response [tool "failed-import" "write_file" "src/A.swift" "changed"])
        else (
          assert (not (contains (system req) "private-token"));
          assert (List.exists (fun msg ->
            P.member "tool_call_id" msg = `String "failed-import" &&
            (match P.member "content" msg with
             | `String text -> contains text "unsafe_import" &&
               not (contains text "private-token")
             | _ -> false)) (messages req));
          answer))
        (fun provider ->
          let agent = Pave.Agent.create ~provider ~root ~system:"mobile safety"
            ~on_event:ignore () in
          assert (Pave.Agent.run agent "Update A" = "done");
          check_pairs (Pave.Agent.messages agent);
          assert (let ic = open_in a in
            Fun.protect ~finally:(fun () -> close_in ic)
              (fun () -> input_line ic = "A done")));
      let escaped = Context.resolve_scoped ~root ~path:"../private-token.md" () in
      assert (not escaped.safe && escaped.text = "");
      write (child rules "a.md") "---\npaths: src/**\n---\nA_ONLY";
      write (child rules "aa.md") "---\npaths: src/*.swift\n---\nSECOND_ONLY";
      let overlapping = Context.resolve_scoped ~root ~path:"src/A.swift" () in
      assert (overlapping.safe && contains overlapping.text "A_ONLY\n\nSECOND_ONLY");
      assert (List.exists (fun (d : Context.diagnostic) ->
        d.code = "rule_conflict") overlapping.diagnostics);
      Unix.symlink src (child root "alias");
      let linked = Context.resolve_scoped ~root ~path:"alias/A.swift" () in
      assert (not linked.safe && linked.text = "");
      assert (List.exists (fun (d : Context.diagnostic) ->
        d.code = "unsafe_target") linked.diagnostics));
  print_endline "scoped rules: ok"
