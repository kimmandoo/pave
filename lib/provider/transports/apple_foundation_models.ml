exception Error of string
exception Cancelled

let helper_name = "pave-apple-foundation-models"
let max_request_bytes = 4 * 1024 * 1024
let max_response_bytes = 4 * 1024 * 1024
let max_output_bytes = 32 * 1024 * 1024
let request_timeout_seconds = 300.

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

let executable_candidates executable =
  if String.contains executable '/' then [executable]
  else
    match Sys.getenv_opt "PATH" with
    | None -> []
    | Some path ->
        String.split_on_char ':' path
        |> List.map (fun directory ->
          Filename.concat (if directory = "" then "." else directory) executable)

let find_executable ?(executable = Sys.executable_name) () =
  executable_candidates executable
  |> List.find_opt (fun path ->
    try
      (Unix.stat path).Unix.st_kind = Unix.S_REG &&
      Unix.access path [Unix.X_OK] = ()
    with Unix.Unix_error _ -> false)

let executable_path ?(executable = Sys.executable_name) () =
  Option.map (fun path -> Filename.concat (Filename.dirname path) helper_name)
    (find_executable ~executable ())

let embedded_helper_magic = "PAVEAFM1"
let embedded_helper_footer_bytes = 24
let max_embedded_helper_bytes = 100 * 1024 * 1024

let read_exact fd length =
  let bytes = Bytes.create length in
  let rec loop offset =
    if offset < length then
      let count = try Unix.read fd bytes offset (length - offset)
        with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
      if count > 0 then loop (offset + count)
      else if count < 0 then loop offset
      else raise End_of_file
  in
  loop 0;
  Bytes.unsafe_to_string bytes

let embedded_helper_range ?executable () =
  Option.bind (find_executable ?executable ()) (fun executable ->
    try
      let fd = Unix.openfile executable [Unix.O_RDONLY] 0 in
      Fun.protect ~finally:(fun () -> close_fd fd) (fun () ->
        let size = (Unix.fstat fd).Unix.st_size in
        if size < embedded_helper_footer_bytes then None
        else (
          ignore (Unix.lseek fd (size - embedded_helper_footer_bytes) Unix.SEEK_SET);
          let footer = read_exact fd embedded_helper_footer_bytes in
          let length_hex = String.sub footer 0 16 in
          if String.sub footer 16 8 <> embedded_helper_magic ||
             not (String.for_all (function
               | '0'..'9' | 'a'..'f' | 'A'..'F' -> true | _ -> false) length_hex)
          then None
          else
            let length = Int64.of_string ("0x" ^ length_hex) in
            if length <= 0L ||
               length > Int64.of_int max_embedded_helper_bytes ||
               length > Int64.of_int (size - embedded_helper_footer_bytes)
            then None
            else
              let length = Int64.to_int length in
              Some (executable, size - embedded_helper_footer_bytes - length, length)))
    with Unix.Unix_error _ | End_of_file | Failure _ -> None)

let helper_is_executable path =
  try
    (Unix.stat path).Unix.st_kind = Unix.S_REG &&
    Unix.access path [Unix.X_OK] = ()
  with Unix.Unix_error _ -> false

let helper_available () =
  let adjacent = Option.fold ~none:false ~some:helper_is_executable
    (executable_path ()) in
  adjacent || Option.is_some (embedded_helper_range ())

let extracted_helper = ref None

let materialize_embedded_helper () =
  match !extracted_helper with
  | Some path when helper_is_executable path -> Some path
  | _ ->
      (match embedded_helper_range () with
       | None -> None
       | Some (executable, offset, length) ->
           let source = Unix.openfile executable [Unix.O_RDONLY] 0 in
           let bytes = Fun.protect ~finally:(fun () -> close_fd source)
             (fun () -> ignore (Unix.lseek source offset Unix.SEEK_SET);
               read_exact source length) in
           let path, output = Filename.open_temp_file ~mode:[Open_binary]
             "pave-apple-models-" ".helper" in
           (try
              output_string output bytes;
              close_out output;
              Unix.chmod path 0o700;
              extracted_helper := Some path;
              at_exit (fun () ->
                try Sys.remove path with Sys_error _ -> ());
              Some path
            with exn ->
              close_out_noerr output;
              (try Sys.remove path with Sys_error _ -> ());
              raise exn))

let check_cancel = function
  | Some cancel when cancel () -> raise Cancelled
  | _ -> ()

let write_all fd value =
  let bytes = Bytes.unsafe_of_string value in
  let rec loop offset =
    if offset < Bytes.length bytes then
      let written =
        try Unix.write fd bytes offset (Bytes.length bytes - offset)
        with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
      if written > 0 then loop (offset + written)
      else if written < 0 then loop offset
      else raise (Error "Apple model helper stopped reading its request")
  in
  loop 0

let wait_for ?cancel ~deadline pid =
  let rec loop () =
    check_cancel cancel;
    if Unix.gettimeofday () >= deadline then
      raise (Error "Apple Foundation Models request timed out");
    try
      match Unix.waitpid [Unix.WNOHANG] pid with
      | 0, _ ->
          ignore (Unix.select [] [] [] 0.1);
          loop ()
      | _, status -> status
    with Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()

type stream_state = {
  on_text : (string -> unit) option;
  text : Buffer.t;
  line : Buffer.t;
  mutable bytes : int;
  mutable done_ : bool;
  mutable error : string option;
}

let consume_event state line =
  if line <> "" then (
    let json = try Yojson.Basic.from_string line with
      | Yojson.Json_error _ -> raise (Error "Apple model helper returned invalid JSON") in
    let field name = Protocol.member name json in
    if state.done_ then raise (Error "Apple model helper wrote data after completion");
    match field "type" with
    | `String "text" ->
        (match field "text" with
         | `String value when value <> "" ->
             if String.length value > max_response_bytes - Buffer.length state.text then
               raise (Error "Apple model response exceeds the 4 MiB limit");
             Buffer.add_string state.text value;
             Option.iter (fun emit -> emit value) state.on_text
         | `String _ -> ()
         | _ -> raise (Error "Apple model helper returned invalid text"))
    | `String "done" -> state.done_ <- true
    | `String "error" ->
        (match field "message" with
         | `String message when message <> "" && String.length message <= 4096 ->
             state.error <- Some message
         | _ -> raise (Error "Apple model helper returned an invalid error"))
    | _ -> raise (Error "Apple model helper returned an unknown event"))

let consume_chunk state chunk =
  if String.length chunk > max_output_bytes - state.bytes then
    raise (Error "Apple model helper output exceeds the 32 MiB limit");
  state.bytes <- state.bytes + String.length chunk;
  Buffer.add_string state.line chunk;
  if Buffer.length state.line > 1_048_576 then
    raise (Error "Apple model helper event exceeds the 1 MiB limit");
  let rec lines () =
    match String.index_opt (Buffer.contents state.line) '\n' with
    | None -> ()
    | Some index ->
        let buffered = Buffer.contents state.line in
        let line = String.sub buffered 0 index in
        let remaining = String.sub buffered (index + 1)
          (String.length buffered - index - 1) in
        Buffer.clear state.line;
        Buffer.add_string state.line remaining;
        consume_event state (if String.ends_with ~suffix:"\r" line then
          String.sub line 0 (String.length line - 1) else line);
        lines ()
  in
  lines ()

let run_helper ?cancel ?on_text helper request =
  check_cancel cancel;
  if String.length request > max_request_bytes then
    raise (Error "Apple model request exceeds the 4 MiB limit");
  let input_read, input_write = Unix.pipe () in
  let output_read, output_write =
    try Unix.pipe () with exn ->
      close_fd input_read;
      close_fd input_write;
      raise exn in
  let error_fd =
    try Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 with exn ->
      List.iter close_fd [input_read; input_write; output_read; output_write];
      raise exn in
  let pid =
    try
      Unix.set_close_on_exec input_write;
      Unix.set_close_on_exec output_read;
      Unix.create_process helper [|helper|] input_read output_write error_fd
    with exn ->
      List.iter close_fd [input_read; input_write; output_read; output_write; error_fd];
      raise (Error ("could not start Apple model helper: " ^ Printexc.to_string exn)) in
  close_fd input_read;
  close_fd output_write;
  close_fd error_fd;
  let waited = ref false in
  Fun.protect
    ~finally:(fun () ->
      close_fd input_write;
      close_fd output_read;
      if not !waited then (
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
        (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())))
    (fun () ->
      let write_failed =
        try write_all input_write request; false
        with Unix.Unix_error (Unix.EPIPE, _, _) -> true in
      close_fd input_write;
      let state = { on_text; text = Buffer.create 256; line = Buffer.create 256;
        bytes = 0; done_ = false; error = None } in
      let deadline = Unix.gettimeofday () +. request_timeout_seconds in
      let chunk = Bytes.create 8192 in
      let rec read_output () =
        check_cancel cancel;
        if Unix.gettimeofday () >= deadline then
          raise (Error "Apple Foundation Models request timed out");
        let readable, _, _ = try Unix.select [output_read] [] [] 0.1
          with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], [] in
        if readable = [] then read_output ()
        else
          let count = try Unix.read output_read chunk 0 (Bytes.length chunk)
            with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
          if count > 0 then (
            consume_chunk state (Bytes.sub_string chunk 0 count);
            read_output ())
          else if count < 0 then read_output ()
      in
      read_output ();
      if Buffer.length state.line <> 0 then
        raise (Error "Apple model helper returned a truncated event");
      close_fd output_read;
      let status = wait_for ?cancel ~deadline pid in
      waited := true;
      check_cancel cancel;
      match status, state.error, state.done_, write_failed with
      | Unix.WEXITED 0, None, true, false -> Buffer.contents state.text
      | _, Some message, _, _ -> raise (Error message)
      | Unix.WEXITED 0, None, false, _ ->
          raise (Error "Apple model helper ended before completion")
      | _, None, _, true ->
          raise (Error "Apple model helper stopped reading its request")
      | Unix.WEXITED code, None, _, _ ->
          raise (Error (Printf.sprintf "Apple model helper failed (exit status %d)" code))
      | Unix.WSIGNALED signal, None, _, _
      | Unix.WSTOPPED signal, None, _, _ ->
          raise (Error (Printf.sprintf "Apple model helper terminated (signal %d)" signal)))

let validate_messages messages =
  List.iter (fun (message : Protocol.message) ->
    if message.attachments <> [] then
      raise (Error "Apple Foundation Models does not support media attachments");
    (match message.tool_result_content with
     | Some blocks when List.exists (function Protocol.Image _ -> true | _ -> false) blocks ->
         raise (Error "Apple Foundation Models does not support image tool results")
     | _ -> ());
    if not (List.mem message.role ["system"; "developer"; "user"; "assistant"; "tool"]) then
      raise (Error "Apple Foundation Models received an unsupported message role")) messages

let complete ?helper_path ?on_text ?cancel ~endpoint ~model ~api_key messages =
  if endpoint <> "" then raise (Error "Apple Foundation Models does not use an API endpoint");
  if api_key <> "" then raise (Error "Apple Foundation Models does not accept an API key");
  if model <> "default" then
    raise (Error "Apple Foundation Models uses the OS-managed model ID 'default'");
  validate_messages messages;
  let helper = match helper_path with
    | Some path when path <> "" -> path
    | Some _ -> raise (Error "Apple model helper path is empty")
    | None ->
        let adjacent = Option.bind (executable_path ()) (fun path ->
          if helper_is_executable path then Some path else None) in
        (match adjacent with
         | Some path -> path
         | None ->
             (match materialize_embedded_helper () with
              | Some path -> path
              | None ->
                  raise (Error "Apple Foundation Models helper is unavailable; use a macOS arm64 build"))) in
  let request = Yojson.Basic.to_string (`Assoc [
    "action", `String "complete";
    "model", `String model;
    "messages", Protocol.chat_messages_to_json messages]) in
  let text = run_helper ?cancel ?on_text helper request in
  if text = "" then raise (Error "Apple Foundation Models returned an empty response");
  let response = `Assoc ["choices", `List [`Assoc [
    "index", `Int 0;
    "finish_reason", `String "stop";
    "message", `Assoc ["role", `String "assistant"; "content", `String text]]]] in
  try Protocol.parse_completion response with
  | Protocol.Invalid_response message -> raise (Error message)
