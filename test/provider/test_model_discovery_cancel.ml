let close fd = try Unix.close fd with Unix.Unix_error _ -> ()

let wait_byte fd seconds label =
  let ready, _, _ = Unix.select [fd] [] [] seconds in
  if ready = [] then failwith (label ^ " timed out");
  let byte = Bytes.create 1 in
  if Unix.read fd byte 0 1 <> 1 then failwith (label ^ " closed early")

let () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 1;
  let port = match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let connected_read, connected_write = Unix.pipe () in
  let closed_read, closed_write = Unix.pipe () in
  let child = Unix.fork () in
  if child = 0 then (
    close connected_read;
    close closed_read;
    let client, _ = Unix.accept socket in
    close socket;
    ignore (Unix.write_substring connected_write "c" 0 1);
    let buffer = Bytes.create 512 in
    let rec until_close () =
      let readable, _, _ = Unix.select [client] [] [] 5. in
      if readable = [] then exit 2;
      match Unix.read client buffer 0 (Bytes.length buffer) with
      | 0 -> ignore (Unix.write_substring closed_write "x" 0 1)
      | _ -> until_close () in
    until_close ();
    close client;
    close connected_write;
    close closed_write;
    exit 0);
  close socket;
  close connected_write;
  close closed_write;
  Fun.protect ~finally:(fun () ->
    (try Unix.kill child Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
    ignore (Unix.waitpid [] child);
    close connected_read;
    close closed_read) (fun () ->
    let cancelled = Atomic.make false in
    let completed_read, completed_write = Unix.pipe () in
    Fun.protect ~finally:(fun () ->
      close completed_read; close completed_write) (fun () ->
      let outcome = ref "worker did not finish" in
      let worker = Thread.create (fun () ->
        let http ~url ~headers =
          assert (url = Pave.Model_discovery.ollama_url && headers = []);
          let config = Printf.sprintf
            "silent\nurl = \"http://127.0.0.1:%d/models\"\nrequest = \"GET\"\nproto = \"=http\"\nproxy = \"\"\nconnect-timeout = \"4\"\nmax-time = \"12\"\n" port in
          ignore (Pave.Provider.run_curl ~cancel:(fun () -> Atomic.get cancelled) config);
          failwith "hanging HTTP response unexpectedly completed" in
        outcome := (match Pave.Model_discovery.discover ~http
          ~cancel:(fun () -> Atomic.get cancelled) ~provider:"ollama" () with
          | exception Pave.Provider.Cancelled -> "cancelled"
          | Error reason -> Pave.Model_discovery.message reason
          | Ok _ -> "unexpected models");
        ignore (Unix.write_substring completed_write "d" 0 1)) () in
      wait_byte connected_read 3. "hanging local HTTP connection";
      let started = Unix.gettimeofday () in
      Atomic.set cancelled true;
      wait_byte completed_read 2. "cancelled model listing";
      Thread.join worker;
      if !outcome <> "cancelled" then failwith !outcome;
      if Unix.gettimeofday () -. started >= 2. then
        failwith "curl cancellation waited for the request timeout";
      wait_byte closed_read 2. "curl socket shutdown"));
  print_endline "model discovery curl cancellation: ok"
