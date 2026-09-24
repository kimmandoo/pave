type api = Openai_completions | Anthropic_messages | Openai_responses
  | Ollama_chat | Gemini_direct
type authentication = Api_key | OAuth
type config = { endpoint : string; api_key : string; model : string; api : api }

exception Provider_error of string

let reject_controls label value =
  String.iter
    (fun c ->
      let code = Char.code c in
      if code < 32 || code = 127 then
        raise (Provider_error ("invalid control character in " ^ label)))
    value

let quote_config value =
  reject_controls "curl configuration value" value;
  let escaped = Buffer.create (String.length value + 2) in
  Buffer.add_char escaped '"';
  String.iter
    (function
      | '"' -> Buffer.add_string escaped "\\\""
      | '\\' -> Buffer.add_string escaped "\\\\"
      | c -> Buffer.add_char escaped c)
    value;
  Buffer.add_char escaped '"';
  Buffer.contents escaped

let redact secret text =
  let secret_length = String.length secret in
  if secret_length = 0 then text
  else
    let text_length = String.length text in
    let result = Buffer.create text_length in
    let rec matches offset index =
      index = secret_length
      || (text.[offset + index] = secret.[index] && matches offset (index + 1))
    in
    let rec loop pos =
      if pos = text_length then Buffer.contents result
      else if pos + secret_length <= text_length && matches pos 0 then (
        Buffer.add_string result "[redacted]";
        loop (pos + secret_length))
      else (
        Buffer.add_char result text.[pos];
        loop (pos + 1))
    in
    loop 0

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()
let close_output oc = try close_out oc with Sys_error _ -> ()
let close_input ic = try close_in ic with Sys_error _ -> ()

let with_temp_file f =
  let path, oc = Filename.open_temp_file ~mode:[ Open_binary ] "pave-provider-" ".json" in
  Fun.protect
    ~finally:(fun () ->
      close_output oc;
      try Sys.remove path with Sys_error _ -> ())
    (fun () -> f path oc)

let rec write_all fd data offset =
  if offset < String.length data then (
    let rec write_chunk () =
      try Unix.write_substring fd data offset (String.length data - offset)
      with Unix.Unix_error (Unix.EINTR, _, _) -> write_chunk ()
    in
    let count = write_chunk () in
    if count = 0 then raise (Provider_error "could not send curl configuration");
    write_all fd data (offset + count))

exception Stream_complete

let read_all ?on_chunk ?is_done ?is_finished fd =
  let buffer = Buffer.create 128 in
  let chunk = Bytes.create 8192 in
  let finished_at = ref None in
  let rec loop () =
    (match is_done with
     | Some done_now when done_now () -> raise Stream_complete
     | _ -> ());
    let timeout = match is_finished with
      | None -> None
      | Some finished ->
          if finished () && !finished_at = None then
            finished_at := Some (Unix.gettimeofday ());
          (match !finished_at with
           | None -> None
           | Some start ->
               let remaining = 1. -. (Unix.gettimeofday () -. start) in
               if remaining <= 0. then raise Stream_complete;
               Some remaining) in
    let ready = match timeout with
      | None -> true
      | Some remaining ->
          let readable, _, _ = Unix.select [fd] [] [] remaining in
          readable <> [] in
    if ready then (
      let count =
        try Unix.read fd chunk 0 (Bytes.length chunk)
        with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
      if count <> 0 then (
        if count > 0 then (
          match on_chunk with
          | None -> Buffer.add_subbytes buffer chunk 0 count
          | Some consume ->
              consume (Bytes.sub_string chunk 0 count);
              Buffer.add_subbytes buffer chunk 0 count;
              if Buffer.length buffer > 256 then (
                let tail = Buffer.sub buffer (Buffer.length buffer - 128) 128 in
                Buffer.clear buffer;
                Buffer.add_string buffer tail));
        loop ()))
    else loop () in
  loop ();
  Buffer.contents buffer

let rec wait_for pid =
  try snd (Unix.waitpid [] pid) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> wait_for pid

let run_curl ?on_chunk ?is_done ?is_finished configuration =
  let input_read, input_write = Unix.pipe () in
  let output_read, output_write =
    try Unix.pipe ()
    with exn ->
      close_fd input_read;
      close_fd input_write;
      raise exn
  in
  let errors =
    try Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0
    with exn ->
      List.iter close_fd [ input_read; input_write; output_read; output_write ];
      raise exn
  in
  let pid =
    try
      Unix.set_close_on_exec input_write;
      Unix.set_close_on_exec output_read;
      Unix.create_process "curl" [| "curl"; "--disable"; "--config"; "-" |]
        input_read output_write errors
    with exn ->
      List.iter close_fd [ input_read; input_write; output_read; output_write; errors ];
      raise (Provider_error ("could not start curl: " ^ Printexc.to_string exn))
  in
  close_fd input_read;
  close_fd output_write;
  close_fd errors;
  let waited = ref false in
  Fun.protect
    ~finally:(fun () ->
      close_fd input_write;
      close_fd output_read;
      if not !waited then (
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
        ignore (wait_for pid)))
    (fun () ->
      let write_failed =
        try
          write_all input_write configuration 0;
          false
        with Unix.Unix_error (Unix.EPIPE, _, _) -> true
      in
      close_fd input_write;
      let status_code = read_all ?on_chunk ?is_done ?is_finished output_read in
      close_fd output_read;
      let status = wait_for pid in
      waited := true;
      match status with
      | Unix.WEXITED 0 when not write_failed -> status_code
      | Unix.WEXITED code ->
          raise (Provider_error (Printf.sprintf "curl failed (exit status %d)" code))
      | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
          raise (Provider_error (Printf.sprintf "curl terminated (signal %d)" signal)))

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_input ic) (fun () ->
    let length = in_channel_length ic in
    if length > 16_777_216 then
      raise (Provider_error "completion response exceeds 16 MiB");
    really_input_string ic length)

let error_message key json =
  let error = Protocol.member "error" json in
  let message = match error with
    | `String message -> Some message
    | `Assoc _ -> (match Protocol.member "message" error with
        | `String message -> Some message
        | _ -> None)
    | _ -> None
  in
  match message with
  | Some message when message <> "" -> Some (redact key message)
  | _ -> None

let gemini_model_path model =
  let model = if String.starts_with ~prefix:"models/" model then
    String.sub model 7 (String.length model - 7) else model in
  if model = "" || String.length model > 256 then
    raise (Provider_error "invalid Gemini model ID");
  let result = Buffer.create (String.length model) in
  String.iter (fun c ->
    let code = Char.code c in
    if (code >= 0x41 && code <= 0x5a)
       || (code >= 0x61 && code <= 0x7a)
       || (code >= 0x30 && code <= 0x39)
       || c = '-' || c = '_' || c = '.' || c = '~'
    then Buffer.add_char result c
    else Printf.bprintf result "%%%02X" code) model;
  Buffer.contents result

let request_body ~endpoint ~headers body_json =
  if not (String.starts_with ~prefix:"https://" endpoint
          || String.starts_with ~prefix:"http://" endpoint) then
    raise (Provider_error "endpoint must use HTTP or HTTPS");
  reject_controls "endpoint" endpoint;
  List.iter (reject_controls "header") headers;
  let body = Yojson.Basic.to_string body_json in
  if String.length body > 8_388_608 then
    raise (Provider_error "conversation request exceeds 8 MiB; start a new session");
  body

let curl_options ~endpoint ~headers ~body_path =
  let option name value = name ^ " = " ^ quote_config value ^ "\n" in
  "silent\n"
  ^ option "url" endpoint
  ^ option "request" "POST"
  ^ option "header" "Content-Type: application/json"
  ^ String.concat "" (List.map (option "header") headers)
  ^ option "data-binary" ("@" ^ body_path)
  ^ option "connect-timeout" "10"
  ^ option "max-time" "120"
  ^ option "proto" "=http,https"

let post_json ~endpoint ~headers ~secret body_json =
  let body = request_body ~endpoint ~headers body_json in
  with_temp_file (fun body_path body_output ->
    output_string body_output body;
    close_out body_output;
    with_temp_file (fun response_path response_output ->
      close_out response_output;
      let option name value = name ^ " = " ^ quote_config value ^ "\n" in
      let configuration =
        curl_options ~endpoint ~headers ~body_path
        ^ option "output" response_path
        ^ option "write-out" "%{http_code}" in
      let status = run_curl configuration in
      let response = read_file response_path in
      let json =
        try Some (Yojson.Basic.from_string response)
        with Yojson.Json_error _ -> None in
      let http_status =
        try int_of_string status
        with Failure _ -> raise (Provider_error "curl returned an invalid HTTP status") in
      if http_status < 200 || http_status >= 300 then (
        let detail = match json with
          | Some value -> error_message secret value
          | None -> None in
        let suffix = match detail with None -> "" | Some message -> ": " ^ message in
        raise (Provider_error (Printf.sprintf "HTTP %d%s" http_status suffix)));
      match json with
      | None -> raise (Provider_error "invalid JSON in completion response")
      | Some json ->
          (match error_message secret json with
          | Some message -> raise (Provider_error ("provider error: " ^ message))
          | None -> ());
          json))

let status_from_headers headers =
  List.fold_left (fun current line ->
    if String.starts_with ~prefix:"HTTP/" line then
      match String.split_on_char ' ' line with
      | _version :: status :: _ ->
          (try Some (int_of_string status) with Failure _ -> current)
      | _ -> current
    else current) None (String.split_on_char '\n' headers)

let post_stream ~endpoint ~headers ~secret body_json ~on_chunk ~is_done ~is_finished =
  let body = request_body ~endpoint ~headers body_json in
  with_temp_file (fun body_path body_output ->
    output_string body_output body;
    close_out body_output;
    with_temp_file (fun header_path header_output ->
      close_out header_output;
      let option name value = name ^ " = " ^ quote_config value ^ "\n" in
      let configuration =
        curl_options ~endpoint ~headers ~body_path
        ^ "no-buffer\n"
        ^ option "dump-header" header_path
        ^ option "speed-time" "30"
        ^ option "speed-limit" "1" in
      let status = ref None in
      let pending = Buffer.create 256 in
      let consume chunk =
        if !status = None then status := status_from_headers (read_file header_path);
        match !status with
        | Some code when code >= 200 && code < 300 ->
            if Buffer.length pending <> 0 then (
              on_chunk (Buffer.contents pending);
              Buffer.clear pending);
            on_chunk chunk
        | _ ->
            if Buffer.length pending + String.length chunk > 16_384 then
              raise (Provider_error "HTTP error or missing response headers exceeded 16 KiB");
            Buffer.add_string pending chunk in
      (try ignore (run_curl ~on_chunk:consume ~is_done ~is_finished configuration)
       with Stream_complete -> ());
      let code = match status_from_headers (read_file header_path) with
        | Some code -> code
        | None -> raise (Provider_error "missing HTTP response status") in
      if code < 200 || code >= 300 then (
        let detail = try
          error_message secret (Yojson.Basic.from_string (Buffer.contents pending))
          with Yojson.Json_error _ -> None in
        let suffix = match detail with None -> "" | Some message -> ": " ^ message in
        raise (Provider_error (Printf.sprintf "HTTP %d%s" code suffix)));
      if Buffer.length pending <> 0 then on_chunk (Buffer.contents pending)))

let complete ?(authentication = Api_key) ?resolve_key ?on_text config messages tools =
  if authentication = OAuth &&
     (config.api <> Anthropic_messages ||
      config.endpoint <> "https://api.anthropic.com/v1/messages") then
    raise (Provider_error "OAuth inference requires the registered Anthropic endpoint");
  let api_key = match resolve_key with
    | Some get -> get ()
    | None -> config.api_key in
  reject_controls "API key" api_key;
  if authentication = OAuth && api_key = "" then
    raise (Provider_error "OAuth access token unavailable");
  let parse f =
    try f () with
    | Protocol.Invalid_response message ->
        raise (Provider_error ("invalid completion response: " ^ redact api_key message)) in
  match config.api with
  | Openai_completions ->
      let fields = [ "model", `String config.model;
                     "messages", `List (List.map Protocol.message_to_json messages) ] in
      let fields = if tools = [] then fields else fields @ [ "tools", `List tools ] in
      let headers = if api_key = "" then [] else
        [ "Authorization: Bearer " ^ api_key ] in
      (match on_text with
      | None ->
          let json = post_json ~endpoint:config.endpoint ~headers ~secret:api_key
            (`Assoc fields) in
          parse (fun () -> Protocol.parse_completion json)
      | Some emit ->
          let stream = Openai_stream.create ~on_text:emit in
          let body = `Assoc (fields @ [ "stream", `Bool true ]) in
          parse (fun () ->
            post_stream ~endpoint:config.endpoint ~headers ~secret:api_key
              body ~on_chunk:(Openai_stream.feed stream)
              ~is_done:(fun () -> Openai_stream.is_done stream)
              ~is_finished:(fun () -> Openai_stream.is_finished stream);
            Openai_stream.finish stream))
  | Anthropic_messages ->
      let body = parse (fun () ->
        Anthropic_wire.request ~model:config.model ~max_tokens:4096 messages tools) in
      let headers = [ "anthropic-version: 2023-06-01" ] @
        (match authentication with
         | Api_key ->
             if api_key = "" then [] else [ "x-api-key: " ^ api_key ]
         | OAuth ->
             [ "Authorization: Bearer " ^ api_key;
               "anthropic-beta: oauth-2025-04-20,claude-code-20250219";
               "anthropic-dangerous-direct-browser-access: true";
               "User-Agent: pave/0.1.1"; "x-app: cli" ]) in
      (match on_text with
      | None ->
          let json = post_json ~endpoint:config.endpoint ~headers ~secret:api_key body in
          parse (fun () -> Anthropic_wire.parse_response json)
      | Some emit ->
          let stream = Anthropic_stream.create ~on_text:emit in
          let body = match body with
            | `Assoc fields -> `Assoc (fields @ [ "stream", `Bool true ])
            | _ -> assert false in
          parse (fun () ->
            post_stream ~endpoint:config.endpoint ~headers ~secret:api_key
              body ~on_chunk:(Anthropic_stream.feed stream)
              ~is_done:(fun () -> Anthropic_stream.is_done stream)
              ~is_finished:(fun () -> Anthropic_stream.is_finished stream);
            Anthropic_stream.finish stream))
  | Openai_responses ->
      let body = parse (fun () ->
        Openai_responses_wire.request ~model:config.model messages tools) in
      let headers = if api_key = "" then [] else
        [ "Authorization: Bearer " ^ api_key ] in
      (match on_text with
      | None ->
          let json = post_json ~endpoint:config.endpoint ~headers ~secret:api_key body in
          parse (fun () -> Openai_responses_wire.parse_completion json)
      | Some emit ->
          let stream = Openai_responses_stream.create ~on_text:emit in
          let body = parse (fun () ->
            Openai_responses_wire.request ~stream:true
              ~model:config.model messages tools) in
          parse (fun () ->
            post_stream ~endpoint:config.endpoint ~headers ~secret:api_key
              body ~on_chunk:(Openai_responses_stream.feed stream)
              ~is_done:(fun () -> Openai_responses_stream.is_done stream)
              ~is_finished:(fun () -> Openai_responses_stream.is_finished stream);
            Openai_responses_stream.finish stream))
  | Ollama_chat ->
      let body = parse (fun () ->
        Ollama_wire.request ~model:config.model messages tools) in
      (match on_text with
      | None ->
          let json = post_json ~endpoint:config.endpoint ~headers:[] ~secret:"" body in
          parse (fun () -> Ollama_wire.parse_completion json)
      | Some emit ->
          let stream = Ollama_stream.create ~on_text:emit in
          let body = match body with
            | `Assoc fields ->
                `Assoc (("stream", `Bool true) :: List.remove_assoc "stream" fields)
            | _ -> assert false in
          parse (fun () ->
            post_stream ~endpoint:config.endpoint ~headers:[] ~secret:""
              body ~on_chunk:(Ollama_stream.feed stream)
              ~is_done:(fun () -> Ollama_stream.is_done stream)
              ~is_finished:(fun () -> Ollama_stream.is_finished stream);
            Ollama_stream.finish stream))
  | Gemini_direct ->
      let body = parse (fun () ->
        Gemini_wire.request ~model:config.model messages tools) in
      let headers = [ "x-goog-api-key: " ^ api_key ] in
      let base = if String.ends_with ~suffix:"/" config.endpoint then
        String.sub config.endpoint 0 (String.length config.endpoint - 1)
        else config.endpoint in
      let model_path = gemini_model_path config.model in
      (match on_text with
      | None ->
          let endpoint = base ^ "/" ^ model_path ^ ":generateContent" in
          let json = post_json ~endpoint ~headers ~secret:api_key body in
          parse (fun () -> Gemini_wire.parse_completion json)
      | Some emit ->
          let stream = Gemini_stream.create ~on_text:emit in
          let endpoint = base ^ "/" ^ model_path ^ ":streamGenerateContent?alt=sse" in
          parse (fun () ->
            post_stream ~endpoint ~headers ~secret:api_key
              body ~on_chunk:(Gemini_stream.feed stream)
              ~is_done:(fun () -> Gemini_stream.is_done stream)
              ~is_finished:(fun () -> Gemini_stream.is_finished stream);
            Gemini_stream.finish stream))
