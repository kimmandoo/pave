module B = Pave.Workspace_browser
let j = Yojson.Basic.to_string
let env name =
  if name = "PAVE_BROWSER" then Sys.getenv_opt "CHROME_BIN" else Sys.getenv_opt name

(* Real end-to-end drive of one isolated headless session. Gated on CHROME_BIN
   because no Chromium is installed on every host; SMOKE_URL is the page to
   open (a reachable HTTPS URL). Fun.protect guarantees teardown after a
   mid-flow failure too. *)
let () =
  match Sys.getenv_opt "CHROME_BIN", Sys.getenv_opt "SMOKE_URL" with
  | Some _, Some url ->
      let m = B.create_manager ~owner:"live" in
      Fun.protect ~finally:(fun () -> B.close_manager m) (fun () ->
        let id = B.open_session ~env m ~id:"s1" in
        Printf.printf "open: %s\n%!" id;
        let nav = B.navigate m ~id ~url ~timeout_seconds:15. in
        Printf.printf "nav: %s\n%!" (j nav);
        let obs = B.observe m ~id in
        Printf.printf "observe: %s\n%!" (j obs);
        let ev = B.evaluate m ~id ~expression:"document.title" ~timeout_seconds:5. in
        Printf.printf "eval: %s\n%!" (j ev);
        let ev2 = B.evaluate m ~id
          ~expression:"(navigator.modelContext && navigator.modelContext.registerTool({name:'echo',description:'echoes',execute:async a=>({seen:a.text})}) && 'registered') || 'no-modelContext'"
          ~timeout_seconds:5. in
        Printf.printf "register: %s\n%!" (j ev2);
        let shot = B.screenshot m ~id in
        let data = match shot with
          | `Assoc f -> (match List.assoc_opt "data" f with
              | Some (`String s) -> s | _ -> "")
          | _ -> "" in
        Printf.printf "screenshot: %d b64 bytes\n%!" (String.length data);
        let tools = B.list_tools m ~id in
        Printf.printf "tools: %s\n%!" (j tools);
        let call = B.call_tool m ~id ~name:"echo"
          ~arguments:(`Assoc ["text", `String "hi"]) ~timeout_seconds:5. in
        Printf.printf "call: %s\n%!" (j call));
      print_endline "live done"
  | _ -> print_endline "test_browser_live: skipped (set CHROME_BIN and SMOKE_URL)"
