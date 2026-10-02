type api = Openai_completions | Local_chat | Anthropic_messages | Openai_responses
  | Azure_responses | Azure_chat | Bedrock_mantle_responses | Ollama_chat | Gemini_direct
  | Vertex_generate | Vertex_anthropic | Bedrock_converse | Bedrock_converse_stream
  | Apple_foundation_models
  | Xai_chat | Nvidia_chat
  | Novita_chat | Siliconflow_chat | Siliconflow_cn_chat
  | Stepfun_chat | Coreweave_chat | Synthetic_chat | Zai_chat
  | Zenmux_chat | Wafer_chat | Qianfan_chat | Xiaomi_chat
  | Kilo_chat | Alibaba_coding_chat | Singularity_dev_chat
  | Opencode_go_chat | Charm_hyper_chat | Opencode_zen_responses
  | Singularity_tech_chat | Firepass_chat | Yolo_auto_chat
  | Xiaomi_token_ams_chat | Xiaomi_token_cn_chat | Xiaomi_token_sgp_chat
  | Minimax_code_chat | Minimax_code_cn_chat | Meta_responses
  | Vercel_ai_gateway_chat | Cloudflare_ai_gateway_chat
  | Commandcode_chat | Commandcode_messages | Commandcode_responses
  | Devin_connect | Gitlab_duo_messages | Gitlab_duo_responses
  | Gitlab_duo_chat
  | Codex_responses | Copilot_chat
  | Fireworks_chat
  | Minimax_chat | Deepseek_chat | Mistral_chat | Openrouter_chat
  | Umans_chat | Umans_messages | Cline_pass_chat | Alibaba_token_plan_chat
  | Kimi_code_chat | Kimi_code_cn_chat
  | Kimi_code_messages | Kimi_code_cn_messages
type authentication = Api_key | OAuth | Cloud_identity
type config = { endpoint : string; api_key : string; model : string; api : api }
type credentials = {
  access : string;
  account_id : string option;
  residency : string option;
}
let effort_choices api reported =
  let valid = match api with
    | Codex_responses -> Codex_wire.valid_effort
    | Umans_chat | Umans_messages -> Umans_api.valid_effort
    | _ -> fun _ -> false in
  match reported with
  | None -> []
  | Some levels -> List.filter valid levels


exception Provider_error of string
exception Cancelled

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

type stream_timeouts = {
  first_byte_seconds : float;
  idle_seconds : float;
}

exception Stream_timeout of [ `First_byte | `Idle ]

let check_cancel = function
  | Some cancel when cancel () -> raise Cancelled
  | _ -> ()

let read_all ?on_chunk ?is_done ?is_finished ?cancel ?stream_timeouts ?progress fd =
  let buffer = Buffer.create 128 in
  let chunk = Bytes.create 8192 in
  let finished_at = ref None in
  let started_at = Unix.gettimeofday () in
  let last_received_at = ref None in
  let rec loop () =
    check_cancel cancel;
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
    let deadline = Option.map (fun limits ->
      match !last_received_at with
      | None -> started_at +. limits.first_byte_seconds, `First_byte
      | Some received -> received +. limits.idle_seconds, `Idle)
        stream_timeouts in
    let timeout = match timeout, deadline with
      | timeout, None -> timeout
      | timeout, Some (deadline, _) ->
          let remaining = max 0. (deadline -. Unix.gettimeofday ()) in
          Some (match timeout with
            | None -> remaining
            | Some timeout -> min timeout remaining) in
    let timeout = match cancel, timeout with
      | None, None -> None
      | Some _, None -> Some 0.1
      | None, Some duration -> Some duration
      | Some _, Some duration -> Some (min 0.1 duration) in
    let ready = match timeout with
      | None -> true
      | Some duration ->
          (try
            let readable, _, _ = Unix.select [fd] [] [] duration in
            readable <> []
          with Unix.Unix_error (Unix.EINTR, _, _) -> false) in
    check_cancel cancel;
    if ready then (
      let count =
        try Unix.read fd chunk 0 (Bytes.length chunk)
        with Unix.Unix_error (Unix.EINTR, _, _) -> -1 in
      if count <> 0 then (
        check_cancel cancel;
        if count > 0 then (
          last_received_at := Some (Unix.gettimeofday ());
          match on_chunk with
          | None -> Buffer.add_subbytes buffer chunk 0 count
          | Some consume ->
              consume (Bytes.sub_string chunk 0 count);
              Buffer.add_subbytes buffer chunk 0 count;
              if Buffer.length buffer > 256 then (
                let tail = Buffer.sub buffer (Buffer.length buffer - 128) 128 in
                Buffer.clear buffer;
                Buffer.add_string buffer tail));
          (* A keep-alive comment still yields bytes but no parsed event; only
             a delivered event counts as real stream progress for the idle
             watchdog. *)
          (match progress with
           | Some advanced when advanced () ->
               last_received_at := Some (Unix.gettimeofday ())
           | _ -> ());
          loop ()))
    else (
      (match deadline with
       | Some (deadline, phase) when Unix.gettimeofday () >= deadline ->
           raise (Stream_timeout phase)
       | _ -> ());
      loop ()) in
  loop ();
  Buffer.contents buffer

let rec wait_for ?cancel pid =
  check_cancel cancel;
  let flags = match cancel with None -> [] | Some _ -> [ Unix.WNOHANG ] in
  try
    let waited, status = Unix.waitpid flags pid in
    if waited <> 0 then status
    else (
      ignore (Unix.select [] [] [] 0.1);
      wait_for ?cancel pid)
  with Unix.Unix_error (Unix.EINTR, _, _) -> wait_for ?cancel pid


let curl_path = "/usr/bin/curl"

let curl_environment = [|
  "LANG=C";
  "LC_ALL=C"
|]
(* Explicit fixture injection for tests; production requests use curl_path. *)

module Test = struct
  let curl_helper = ref None

  let use_curl_helper executable =
    curl_helper := Some (Unix.realpath executable)
end


let run_curl ?on_chunk ?is_done ?is_finished ?cancel ?stream_timeouts ?progress configuration =
  check_cancel cancel;
  let executable = match !Test.curl_helper with
    | Some executable -> executable
    | None -> curl_path in
  let environment = match !Test.curl_helper with
    | Some _ -> Unix.environment ()
    | None -> curl_environment in
  (match !Test.curl_helper with
   | Some _ -> ()
   | None ->
       if not (Sys.file_exists curl_path &&
           (try Unix.access curl_path [Unix.X_OK]; true
            with Unix.Unix_error _ -> false)) then
         raise (Provider_error
           "Transport error: trusted curl executable is unavailable"));
  let input_read, input_write = Unix.pipe () in
  let output_read, output_write =
    try Unix.pipe ()
    with exn ->
      close_fd input_read;
      close_fd input_write;
      raise exn
  in
  let errors =
    (* Fixture helpers exit non-zero on a violated assertion; their stderr is
       normally discarded, but PAVE_CURL_ERRORS names a capture file. *)
    let target = match Sys.getenv_opt "PAVE_CURL_ERRORS" with
      | Some path when path <> "" -> path
      | _ -> "/dev/null" in
    try Unix.openfile target [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ] 0o600
    with exn ->
      List.iter close_fd [ input_read; input_write; output_read; output_write ];
      raise exn
  in
  let pid =
    try
      Unix.set_close_on_exec input_write;
      Unix.set_close_on_exec output_read;
      let arguments = [| "curl"; "--disable"; "--config"; "-" |] in
      Unix.create_process_env executable arguments environment
        input_read output_write errors
    with exn ->
      List.iter close_fd [ input_read; input_write; output_read; output_write; errors ];
      raise (Provider_error ("Transport error: could not start trusted curl: " ^
        Printexc.to_string exn))
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
      let status_code = read_all ?on_chunk ?is_done ?is_finished ?cancel
        ?stream_timeouts ?progress output_read in
      close_fd output_read;
      let status = wait_for ?cancel pid in
      waited := true;
      check_cancel cancel;
      match status with
      | Unix.WEXITED 0 when not write_failed -> status_code
      | Unix.WEXITED code ->
          raise (Provider_error (Printf.sprintf
            "Transport error: curl failed (exit status %d)" code))
      | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
          raise (Provider_error (Printf.sprintf
            "Transport error: curl terminated (signal %d)" signal)))
(* Upload and model prefill share the buffered request budget until the first
   response body byte. Only then does the response inactivity deadline apply.
   curl's low-speed guard is not an idle timer: it also runs before a response
   and averages transfer speed, including upload. Keep its total/connect bounds,
   but enforce response phase deadlines in the cancellable reader instead. *)
let buffered_max_seconds = 600
let stream_idle_seconds = 120
let stream_max_seconds = 3600

let curl_timeout_message ~streaming ~response_body_seen =
  let phase = if response_body_seen then "after response data"
    else "before response data" in
  Printf.sprintf
    "Transport error: provider %s timed out %s (connection setup or %d s total request limit)"
    (if streaming then "stream" else "request") phase
    (if streaming then stream_max_seconds else buffered_max_seconds)


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
let context_limit_error json =
  let error = Protocol.member "error" json in
  let candidates =
    List.map (fun key -> Protocol.member key error) ["code"; "type"; "status"] @
    [Protocol.member "code" json; Protocol.member "status" json] in
  List.exists (function
    | `String code ->
        List.mem (String.lowercase_ascii code) [
          "context_length_exceeded"; "context_window_exceeded";
          "context_limit_exceeded"; "prompt_too_long"; "prompt_is_too_long";
          "input_too_long"; "max_input_tokens_exceeded";
          "token_limit_exceeded" ]
    | _ -> false) candidates

let http_error_reason secret status json =
  let detail = match error_message secret json with
    | Some detail -> ": " ^ detail
    | None -> "" in
  let classification, reason = match status with
    | 400 when context_limit_error json ->
        "Request error", "provider context limit exceeded"
    | 400 -> "Request error", "invalid provider request"
    | 401 -> "Authentication error", "provider authentication failed"
    | 403 -> "Authentication error", "provider permission denied"
    | 404 -> "Route error", "provider model or route not found"
    | 408 -> "Transport error", "provider request timed out"
    | 413 -> "Request error", "provider request exceeds its context or size limit"
    | 429 -> "Transport error", "provider rate limited"
    | code when code >= 500 && code <= 599 ->
        "Transport error", "provider unavailable"
    | code -> "Provider error", "HTTP " ^ string_of_int code in
  let reason = match status with
    | 400 | 401 | 403 | 404 | 408 | 413 | 429 ->
        Printf.sprintf "%s (HTTP %d)" reason status
    | _ when status >= 500 && status <= 599 ->
        reason ^ " (HTTP " ^ string_of_int status ^ ")"
    | _ -> reason in
  classification ^ ": " ^ reason ^ detail

let devin_http_error stage status code =
  let reason = match status with
    | 400 -> "invalid request"
    | 401 -> "session credential rejected"
    | 403 -> "account access denied"
    | 404 -> "model or route not found"
    | 408 -> "request timed out (remote acceptance unknown)"
    | 413 -> "request exceeds size limit"
    | 429 -> "rate limited"
    | status when status >= 500 && status <= 599 -> "provider unavailable"
    | _ -> "request failed" in
  let detail = match code with None -> "" | Some code -> " (Connect " ^ code ^ ")" in
  Printf.sprintf "%s HTTP %d: %s%s" stage status reason detail

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

(* Local Chat Completions never sends credentials to a DNS name other than
   localhost, which is replaced with a numeric loopback address before curl.
   Numeric LAN addresses are accepted only when explicitly configured. *)
let local_endpoint endpoint =
  let fail () = raise (Provider_error "invalid local Chat Completions endpoint") in
  let length = String.length endpoint in
  if length > 2048 then fail ();
  reject_controls "local endpoint" endpoint;
  let scheme, start =
    if String.starts_with ~prefix:"http://" endpoint then "http://", 7
    else if String.starts_with ~prefix:"https://" endpoint then "https://", 8
    else fail () in
  let slash = match String.index_from_opt endpoint start '/' with
    | Some index -> index | None -> fail () in
  let authority = String.sub endpoint start (slash - start) in
  let path = String.sub endpoint slash (length - slash) in
  if not (String.ends_with ~suffix:"/chat/completions" path) ||
     String.length path < String.length "/chat/completions" ||
     String.exists (fun c ->
       not (match c with
         | 'a'..'z' | 'A'..'Z' | '0'..'9' | '/' | '-' | '_' | '.' -> true
         | _ -> false)) path ||
     List.exists (fun segment -> segment = "." || segment = "..")
       (String.split_on_char '/' path)
  then fail ();
  let host, port =
    if String.starts_with ~prefix:"[" authority then
      let closing = match String.index_opt authority ']' with
        | Some index -> index | None -> fail () in
      let host = String.sub authority 1 (closing - 1) in
      let port = String.sub authority (closing + 1)
        (String.length authority - closing - 1) in
      if port <> "" && port.[0] <> ':' then fail ();
      "[" ^ host ^ "]", port
    else
      match String.index_opt authority ':' with
      | Some index ->
          String.sub authority 0 index,
          String.sub authority index (String.length authority - index)
      | None -> authority, "" in
  if port <> "" then (
    let digits = String.sub port 1 (String.length port - 1) in
    if digits = "" || String.length digits > 5 ||
       not (String.for_all (function '0'..'9' -> true | _ -> false) digits)
    then fail ();
    let number = int_of_string digits in
    if number < 1 || number > 65535 then fail ());
  let canonical_host =
    if String.lowercase_ascii host = "localhost" then "127.0.0.1"
    else if String.starts_with ~prefix:"[" host then (
      let numeric = String.sub host 1 (String.length host - 2) in
      if not (String.contains numeric ':') ||
         not (String.for_all (function
           | '0'..'9' | 'a'..'f' | 'A'..'F' | ':' | '.' -> true
           | _ -> false) numeric)
      then fail ();
      let normalized = try Unix.string_of_inet_addr
        (Unix.inet_addr_of_string numeric)
        with Failure _ | Invalid_argument _ -> fail () in
      let normalized = String.lowercase_ascii normalized in
      if normalized <> "::1" &&
         not (String.starts_with ~prefix:"fc" normalized ||
              String.starts_with ~prefix:"fd" normalized)
      then fail ();
      "[" ^ normalized ^ "]")
    else (
      let octets = String.split_on_char '.' host in
      let number part =
        if part = "" || String.length part > 3 ||
           (String.length part > 1 && part.[0] = '0') ||
           not (String.for_all (function '0'..'9' -> true | _ -> false) part)
        then fail ();
        let value = int_of_string part in
        if value > 255 then fail ();
        value in
      match List.map number octets with
      | [127; _; _; _] | [10; _; _; _]
      | [192; 168; _; _] -> host
      | [172; second; _; _] when second >= 16 && second <= 31 -> host
      | _ -> fail ()) in
  scheme ^ canonical_host ^ port ^ path

let loopback_http endpoint =
  let prefix = "http://" in
  if not (String.starts_with ~prefix endpoint) then false
  else
    let offset = String.length prefix in
    let slash = match String.index_from_opt endpoint offset '/' with
      | Some index -> index
      | None -> String.length endpoint in
    let authority = String.sub endpoint offset (slash - offset) in
    let host, port =
      if String.starts_with ~prefix:"127.0.0.1" authority then
        "127.0.0.1", String.sub authority 9 (String.length authority - 9)
      else if String.starts_with ~prefix:"[::1]" authority then
        "[::1]", String.sub authority 5 (String.length authority - 5)
      else "", "" in
    host <> "" && (port = "" ||
      (String.length port > 1 && port.[0] = ':' &&
        (let digits = String.sub port 1 (String.length port - 1) in
         String.length digits <= 5 &&
         String.for_all (fun c -> c >= '0' && c <= '9') digits &&
         let number = int_of_string digits in number > 0 && number <= 65535)))
let validate_endpoint_override ~api ~pinned_endpoint ~requested =
  if requested = "" || requested = pinned_endpoint then ()
  else if api = Local_chat then ignore (local_endpoint requested)
  else raise (Provider_error
    "remote endpoint overrides are disabled; define a custom provider in user settings")


let gemini_max_request_bytes = 19 * 1024 * 1024

let request_body ?max_request_bytes ~local ~endpoint ~headers body_json =
  if not (String.starts_with ~prefix:"https://" endpoint ||
    (String.starts_with ~prefix:"http://" endpoint &&
      (local || loopback_http endpoint))) then
    raise (Provider_error "completion endpoint must use HTTPS or validated local HTTP");
  reject_controls "endpoint" endpoint;
  List.iter (reject_controls "header") headers;
  let body = Yojson.Basic.to_string body_json in
  let max_request_bytes = Option.value ~default:8_388_608 max_request_bytes in
  if String.length body > max_request_bytes then
    raise (Provider_error (Printf.sprintf
      "conversation request exceeds %d MiB; start a new session"
      (max 1 (max_request_bytes / 1_048_576))));
  body


let curl_options ~local ~max_seconds ~endpoint ~headers ~body_path =
  let option name value = name ^ " = " ^ quote_config value ^ "\n" in
  "silent\n"
  ^ option "url" endpoint
  ^ option "request" "POST"
  ^ option "header" "Content-Type: application/json"
  ^ String.concat "" (List.map (option "header") headers)
  ^ option "data-binary" ("@" ^ body_path)
  ^ option "connect-timeout" "10"
  ^ option "max-time" (string_of_int max_seconds)
  ^ option "proto" (if String.starts_with ~prefix:"https://" endpoint
    then "=https" else "=http")
  ^ (if local then option "proxy" "" ^ option "noproxy" "*" ^
      option "max-redirs" "0" else "")

let post_json ?max_request_bytes ?(local = false) ?cancel
    ~endpoint ~headers ~secret body_json =
  let body = request_body ?max_request_bytes ~local ~endpoint ~headers body_json in

  with_temp_file (fun body_path body_output ->
    output_string body_output body;
    close_out body_output;
    let option name value = name ^ " = " ^ quote_config value ^ "\n" in
    let max_response_bytes = 16_777_216 in
    let received = Buffer.create 8192 in
    let consume chunk =
      (* curl appends a three-byte HTTP status after the body. *)
      if String.length chunk > max_response_bytes + 3 - Buffer.length received then
        raise (Provider_error "completion response exceeds 16 MiB");
      Buffer.add_string received chunk in
    let configuration =
      curl_options ~local ~max_seconds:buffered_max_seconds
        ~endpoint ~headers ~body_path
      ^ option "output" "/dev/stdout"
      ^ option "max-filesize" (string_of_int max_response_bytes)
      ^ option "write-out" "%{http_code}" in
    (try ignore (run_curl ?cancel ~on_chunk:consume configuration) with
      | Provider_error "Transport error: curl failed (exit status 28)" ->
          raise (Provider_error (curl_timeout_message ~streaming:false
            ~response_body_seen:false))
      | Provider_error "Transport error: curl failed (exit status 63)" ->
          raise (Provider_error "completion response exceeds 16 MiB"));
    check_cancel cancel;
    let length = Buffer.length received in
    if length < 3 then raise (Provider_error "curl returned an invalid HTTP status");
    let http_status =
      try int_of_string (Buffer.sub received (length - 3) 3)
      with Failure _ -> raise (Provider_error "curl returned an invalid HTTP status") in
    let response = Buffer.sub received 0 (length - 3) in
    let json =
      try Some (Yojson.Basic.from_string response)
      with Yojson.Json_error _ -> None in
    if http_status < 200 || http_status >= 300 then
      raise (Provider_error (http_error_reason secret http_status
        (Option.value ~default:`Null json)));
    match json with
    | None -> raise (Provider_error "invalid JSON in completion response")
    | Some json ->
        (match error_message secret json with
        | Some message -> raise (Provider_error ("provider error: " ^ message))
        | None -> ());
        json)

let status_from_headers headers =
  List.fold_left (fun current line ->
    if String.starts_with ~prefix:"HTTP/" line then
      match String.split_on_char ' ' line with
      | _version :: status :: _ ->
          (try Some (int_of_string status) with Failure _ -> current)
      | _ -> current
    else current) None (String.split_on_char '\n' headers)

let post_stream ?max_request_bytes ?(local = false) ?cancel ~endpoint ~headers
    ~secret ?progress body_json ~on_chunk ~is_done ~is_finished =
  (* SSE endpoints content-negotiate on Accept; add it unless the caller (or a
     signed request) already supplied one. *)
  let headers =
    if List.exists (fun header ->
        String.lowercase_ascii
          (match String.index_opt header ':' with
           | Some index -> String.sub header 0 index
           | None -> header)
          |> String.trim = "accept") headers
    then headers
    else headers @ [ "Accept: text/event-stream" ] in
  let body = request_body ?max_request_bytes ~local ~endpoint ~headers body_json in
  with_temp_file (fun body_path body_output ->
    output_string body_output body;
    close_out body_output;
    with_temp_file (fun header_path header_output ->
      close_out header_output;
      let option name value = name ^ " = " ^ quote_config value ^ "\n" in
      let configuration =
        curl_options ~local ~max_seconds:stream_max_seconds
          ~endpoint ~headers ~body_path
        ^ "no-buffer\n"
        ^ option "dump-header" header_path in
      let status = ref None in
      let response_body_seen = ref false in
      let pending = Buffer.create 256 in
      let consume chunk =
        if chunk <> "" then response_body_seen := true;
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
      (try ignore (run_curl ~on_chunk:consume ~is_done ~is_finished ?cancel
         ?progress
         ~stream_timeouts:{
           first_byte_seconds = float_of_int buffered_max_seconds;
           idle_seconds = float_of_int stream_idle_seconds;
         } configuration)
       with
       | Stream_complete -> ()
       | Stream_timeout `First_byte ->
           raise (Provider_error (Printf.sprintf
             "Transport error: provider stream timed out before the first response data byte (upload and response wait exceeded %d s)"
             buffered_max_seconds))
       | Stream_timeout `Idle ->
           raise (Provider_error (Printf.sprintf
             "Transport error: provider stream stalled after response data (no data for %d s)"
             stream_idle_seconds))
       | Provider_error "Transport error: curl failed (exit status 28)" ->
           raise (Provider_error (curl_timeout_message ~streaming:true
             ~response_body_seen:!response_body_seen)));
      check_cancel cancel;
      let code = match status_from_headers (read_file header_path) with
        | Some code -> code
        | None -> raise (Provider_error "missing HTTP response status") in
      if code < 200 || code >= 300 then (
        let error = try Yojson.Basic.from_string (Buffer.contents pending)
          with Yojson.Json_error _ -> `Null in
        raise (Provider_error (http_error_reason secret code error)));
      if Buffer.length pending <> 0 then on_chunk (Buffer.contents pending)))

let supports_user_media = function
  | Openai_completions | Local_chat | Anthropic_messages | Openai_responses
  | Azure_responses | Azure_chat | Bedrock_mantle_responses | Ollama_chat
  | Gemini_direct | Vertex_generate | Bedrock_converse
  | Bedrock_converse_stream | Xai_chat | Nvidia_chat
  | Siliconflow_chat | Siliconflow_cn_chat | Stepfun_chat | Novita_chat
  | Coreweave_chat | Synthetic_chat | Zai_chat | Zenmux_chat | Wafer_chat
  | Qianfan_chat | Xiaomi_chat | Kilo_chat | Alibaba_coding_chat
  | Singularity_dev_chat | Opencode_go_chat | Charm_hyper_chat
  | Singularity_tech_chat | Firepass_chat | Yolo_auto_chat
  | Xiaomi_token_ams_chat | Xiaomi_token_cn_chat | Xiaomi_token_sgp_chat
  | Minimax_code_chat | Minimax_code_cn_chat | Vercel_ai_gateway_chat
  | Cloudflare_ai_gateway_chat | Commandcode_chat | Commandcode_messages
  | Commandcode_responses | Gitlab_duo_messages | Gitlab_duo_responses
  | Gitlab_duo_chat | Codex_responses | Copilot_chat | Opencode_zen_responses
  | Meta_responses | Minimax_chat | Deepseek_chat | Mistral_chat
  | Openrouter_chat | Umans_chat | Umans_messages | Cline_pass_chat
  | Alibaba_token_plan_chat | Kimi_code_chat | Kimi_code_cn_chat
  | Kimi_code_messages | Kimi_code_cn_messages | Fireworks_chat -> true
  | Vertex_anthropic | Devin_connect | Apple_foundation_models -> false

let complete ?(authentication = Api_key) ?resolve_credential ?on_text ?on_usage
    ?on_tool_arguments ?thinking ?max_output_tokens ?cancel ?apple_helper_path
    config messages tools =
  check_cancel cancel;
  let on_text = match on_text, on_tool_arguments with
    | None, Some _ -> Some (fun _ -> ())
    | _ -> on_text in

  let has_attachments = ref false in
  List.iter (fun (message : Protocol.message) ->
    if message.attachments <> [] then (
      has_attachments := true;
      if message.role <> "user" then
        raise (Provider_error "media attachments are supported only on user messages");
      try Protocol.validate_attachments message.attachments
      with Protocol.Invalid_response reason -> raise (Provider_error reason)
    )) messages;
  if !has_attachments && not (supports_user_media config.api) then
    raise (Provider_error "this provider route does not support user media attachments");
  let has_audio_video = List.exists (fun (message : Protocol.message) ->
    List.exists (fun (attachment : Protocol.attachment) ->
      Protocol.attachment_kind attachment.mime_type <>
        Some Protocol.Image_attachment) message.attachments) messages in
  if has_audio_video &&
     config.api <> Gemini_direct && config.api <> Vertex_generate then
    raise (Provider_error
      "audio/video attachments require a Gemini generateContent route");
  let config = if config.api = Local_chat then
    { config with endpoint = local_endpoint config.endpoint } else config in
  if config.api = Apple_foundation_models && authentication <> Api_key then
    raise (Provider_error "Apple Foundation Models does not accept provider credentials");
  if authentication = Cloud_identity &&
     config.api <> Vertex_generate && config.api <> Vertex_anthropic &&
     config.api <> Bedrock_converse && config.api <> Bedrock_converse_stream &&
     config.api <> Azure_responses && config.api <> Azure_chat then
    raise (Provider_error
      "cloud identity authentication requires an AWS Bedrock, Google Vertex, or Azure route");
  if authentication = OAuth &&
     not (match config.api, config.endpoint with
       | Anthropic_messages, "https://api.anthropic.com/v1/messages"
       | Codex_responses, "https://chatgpt.com/backend-api/codex/responses" -> true
       | Copilot_chat, endpoint when endpoint = Github_copilot_wire.endpoint -> true
       | Devin_connect, endpoint when endpoint = Devin_api.chat_url -> true
       | (Gitlab_duo_messages | Gitlab_duo_responses | Gitlab_duo_chat), endpoint
         when endpoint = Gitlab_duo_api.anthropic_url ||
           endpoint = Gitlab_duo_api.responses_url ||
           endpoint = Gitlab_duo_api.completions_url -> true
       | Kilo_chat, endpoint when endpoint = Kilo_api.chat_url -> true
       | _ -> false) then
    raise (Provider_error "OAuth inference requires a registered provider endpoint");
  if config.api = Codex_responses && authentication <> OAuth then
    raise (Provider_error "Codex subscription inference requires OAuth");
  if config.api = Copilot_chat && authentication <> OAuth then
    raise (Provider_error "GitHub Copilot inference requires a device grant");
  let credential = match resolve_credential with
    | Some get -> get ()
    | None -> { access = config.api_key; account_id = None; residency = None } in
  let api_key = credential.access in
  reject_controls "API key" api_key;
  if authentication = OAuth && api_key = "" then
    raise (Provider_error "OAuth access token unavailable");
  let parse_with_secret secret f =
    try f () with
    | Protocol.Invalid_response message
      when String.starts_with ~prefix:Protocol.truncated_prefix message ->
        raise (Provider_error message)
    | Protocol.Invalid_response message ->
        raise (Provider_error ("invalid completion response: " ^
          redact secret message)) in
  let parse f = parse_with_secret api_key f in
  let azure_resolve route =
    let resolve authentication =
      try Azure_wire.resolve ~route ~endpoint:config.endpoint
        ~deployment:config.model ~authentication
      with Invalid_argument message -> raise (Provider_error message) in
    ignore (resolve (Azure_wire.Api_key "pave-preflight"));
    let authentication, secret = match authentication with
      | Api_key -> Azure_wire.Api_key api_key, api_key
      | Cloud_identity ->
          if api_key <> "" then
            raise (Provider_error
              "Azure Entra authentication cannot also carry an API key");
          let token = try Azure_auth.access_token ~endpoint:config.endpoint
              ?cancel ()
            with
            | Azure_auth.Cancelled -> raise Cancelled
            | Azure_auth.Authentication_error reason ->
                raise (Provider_error reason) in
          Azure_wire.Entra_token token, token
      | OAuth ->
          raise (Provider_error
            "Azure routes do not accept subscription OAuth credentials") in
    let endpoint, headers = resolve authentication in
    endpoint, headers, secret in
  (* Per-request output cap: prefer the model's provider-reported ceiling;
     otherwise keep the conservative default. *)
  let output_tokens = Option.value ~default:4096 max_output_tokens in
  let result = match config.api with
  | Apple_foundation_models ->
      if authentication <> Api_key then
        raise (Provider_error "Apple Foundation Models does not accept provider credentials");
      (try
        Apple_foundation_models.complete ?helper_path:apple_helper_path ?on_text
          ?cancel ~endpoint:config.endpoint ~model:config.model
          ~api_key messages
       with Apple_foundation_models.Cancelled -> raise Cancelled
          | Apple_foundation_models.Error message -> raise (Provider_error message))
  | Openai_completions | Local_chat | Copilot_chat | Azure_chat ->
      let fields = [ "model", `String config.model;
                     "messages", Protocol.chat_messages_to_json messages ] in
      let fields = if tools = [] then fields else fields @ [ "tools", `List tools ] in
      if config.api = Local_chat &&
         (String.length api_key > 8192 ||
          not (String.for_all (fun c -> Char.code c > 32 &&
            Char.code c < 127) api_key))
      then raise (Provider_error "invalid local API key");
      let endpoint, headers, secret = match config.api with
        | Azure_chat -> azure_resolve Azure_wire.Chat_completions
        | Copilot_chat ->
            config.endpoint,
            (try Github_copilot_wire.headers ~endpoint:config.endpoint
               ~model:config.model ~token:api_key ~messages
             with Invalid_argument message -> raise (Provider_error message)),
            api_key
        | _ ->
            config.endpoint,
            (if api_key = "" then [] else
              [ "Authorization: Bearer " ^ api_key ]),
            api_key in
      (match on_text with
      | None ->
          let json = post_json ~local:(config.api = Local_chat) ?cancel
            ~endpoint ~headers ~secret (`Assoc fields) in
          let reply = parse_with_secret secret
            (fun () -> Protocol.parse_completion json) in
          (match on_usage with
           | None -> ()
           | Some report ->
               check_cancel cancel;
               Option.iter report (Protocol.completion_usage json));
          reply
      | Some emit ->
          let stream = Openai_stream.create ?on_tool_arguments ~on_text:emit () in
          let fields = fields @ [ "stream", `Bool true ] in
          (* Usage arrives on a trailing usage-only chunk only when asked.
             Official and local endpoints accept stream_options; strict compat
             hosts may 400 on the unknown field, so they keep the omission. *)
          let fields = if config.api = Local_chat ||
            (config.api = Openai_completions &&
             config.endpoint = "https://api.openai.com/v1/chat/completions") then
              fields @ [ "stream_options", `Assoc [ "include_usage", `Bool true ] ]
            else fields in
          let fields = match thinking, config.api with
            | Some effort, (Openai_completions | Local_chat) ->
                fields @ [ "reasoning_effort", `String effort ]
            | _ -> fields in
          let body = `Assoc fields in
          parse_with_secret secret (fun () ->
            let sse_events = ref (Openai_stream.events stream) in
            post_stream ~local:(config.api = Local_chat) ?cancel
              ~endpoint ~headers ~secret body
              ~on_chunk:(Openai_stream.feed stream)
              ~is_done:(fun () -> Openai_stream.is_done stream)
              ~is_finished:(fun () -> Openai_stream.is_finished stream)
              ~progress:(fun () ->
                let count = Openai_stream.events stream in
                count > !sse_events && (sse_events := count; true));
            let reply = Openai_stream.finish stream in
            (match on_usage with
             | None -> ()
             | Some report ->
                 check_cancel cancel;
                 Option.iter report (Openai_stream.usage stream));
            reply))
  | Xai_chat | Nvidia_chat | Novita_chat | Siliconflow_chat
  | Siliconflow_cn_chat | Stepfun_chat | Coreweave_chat | Synthetic_chat
  | Zai_chat | Zenmux_chat | Wafer_chat | Qianfan_chat | Xiaomi_chat
  | Kilo_chat | Alibaba_coding_chat | Singularity_dev_chat
  | Opencode_go_chat | Charm_hyper_chat | Singularity_tech_chat
  | Firepass_chat | Yolo_auto_chat | Xiaomi_token_ams_chat
  | Xiaomi_token_cn_chat | Xiaomi_token_sgp_chat
  | Minimax_code_chat | Minimax_code_cn_chat
  | Vercel_ai_gateway_chat | Cloudflare_ai_gateway_chat
  | Commandcode_chat | Minimax_chat | Deepseek_chat | Mistral_chat
  | Openrouter_chat | Umans_chat | Cline_pass_chat | Alibaba_token_plan_chat
  | Kimi_code_chat | Kimi_code_cn_chat | Fireworks_chat ->
      let openai_request () =
        let fields = [ "model", `String config.model;
          "messages", Protocol.chat_messages_to_json messages;
          "stream", `Bool false ] in
        `Assoc (if tools = [] then fields else fields @ ["tools", `List tools]) in
      let headers, body, parse_reply =
        (try match config.api with
        | Xai_chat ->
            Xai_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Xai_api.request ~model:config.model messages tools,
            Xai_api.parse_completion
        | Nvidia_chat ->
            Nvidia_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Nvidia_api.request ~model:config.model messages tools,
            Protocol.parse_completion
        | Novita_chat ->
            Novita_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Novita_api.request ~model:config.model messages tools,
            Novita_api.parse_completion
        | Siliconflow_chat ->
            Siliconflow_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Siliconflow_api.request ~model:config.model messages tools,
            Siliconflow_api.parse_completion
        | Siliconflow_cn_chat ->
            Siliconflow_api.cn_chat_headers ~endpoint:config.endpoint ~api_key,
            Siliconflow_api.request ~model:config.model messages tools,
            Siliconflow_api.parse_completion
        | Stepfun_chat ->
            Stepfun_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Stepfun_api.request ~model:config.model messages tools,
            Stepfun_api.parse_completion
        | Coreweave_chat ->
            Coreweave_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Coreweave_api.request ~model:config.model messages tools,
            Coreweave_api.parse_completion
        | Synthetic_chat ->
            Synthetic_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Synthetic_api.request ~model:config.model messages tools,
            Synthetic_api.parse_completion
        | Zai_chat ->
            Zai_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Zai_api.request ~model:config.model messages tools,
            Zai_api.parse_completion
        | Zenmux_chat ->
            Zenmux_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Zenmux_api.request ~model:config.model messages tools,
            Zenmux_api.parse_completion
        | Wafer_chat ->
            Wafer_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Wafer_api.request ~model:config.model messages tools,
            Wafer_api.parse_completion
        | Qianfan_chat ->
            Qianfan_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Qianfan_api.request ~model:config.model messages tools,
            Qianfan_api.parse_completion
        | Xiaomi_chat ->
            Xiaomi_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Xiaomi_api.request ~model:config.model messages tools,
            Xiaomi_api.parse_completion
        | Kilo_chat ->
            Kilo_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Kilo_api.request ~model:config.model messages tools,
            Kilo_api.parse_completion
        | Alibaba_coding_chat ->
            Alibaba_coding_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Alibaba_coding_api.request ~endpoint:config.endpoint
              ~model:config.model ?thinking messages tools,
            Alibaba_coding_api.parse_completion ~endpoint:config.endpoint
              ~model:config.model
        | Singularity_dev_chat ->
            Singularity_dev_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Singularity_dev_api.request ~model:config.model messages tools,
            Singularity_dev_api.parse_completion
        | Opencode_go_chat ->
            Opencode_go_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Opencode_go_api.request ~model:config.model messages tools,
            Opencode_go_api.parse_completion
        | Charm_hyper_chat ->
            Charm_hyper_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Charm_hyper_api.request ~model:config.model messages tools,
            Charm_hyper_api.parse_completion
        | Singularity_tech_chat ->
            Singularity_tech_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Singularity_tech_api.request ~model:config.model messages tools,
            Singularity_tech_api.parse_completion
        | Firepass_chat ->
            Firepass_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Firepass_api.request ~model:config.model messages tools,
            Firepass_api.parse_completion
        | Yolo_auto_chat ->
            Yolo_auto_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Yolo_auto_api.request ~model:config.model messages tools,
            Yolo_auto_api.parse_completion
        | Xiaomi_token_ams_chat ->
            Xiaomi_token_api.chat_headers ~region:Xiaomi_token_api.Ams
              ~endpoint:config.endpoint ~api_key,
            Xiaomi_token_api.request ~model:config.model messages tools,
            Xiaomi_token_api.parse_completion
        | Xiaomi_token_cn_chat ->
            Xiaomi_token_api.chat_headers ~region:Xiaomi_token_api.Cn
              ~endpoint:config.endpoint ~api_key,
            Xiaomi_token_api.request ~model:config.model messages tools,
            Xiaomi_token_api.parse_completion
        | Xiaomi_token_sgp_chat ->
            Xiaomi_token_api.chat_headers ~region:Xiaomi_token_api.Sgp
              ~endpoint:config.endpoint ~api_key,
            Xiaomi_token_api.request ~model:config.model messages tools,
            Xiaomi_token_api.parse_completion
        | Minimax_code_chat ->
            if config.endpoint <> Minimax_code_api.intl_chat_url then
              raise (Provider_error "MiniMax global plan credential requires its pinned international endpoint");
            Minimax_code_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Minimax_code_api.request ~model:config.model messages tools,
            Minimax_code_api.parse_completion
        | Minimax_code_cn_chat ->
            if config.endpoint <> Minimax_code_api.china_chat_url then
              raise (Provider_error "MiniMax China plan credential requires its pinned China endpoint");
            Minimax_code_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Minimax_code_api.request ~model:config.model messages tools,
            Minimax_code_api.parse_completion
        | Vercel_ai_gateway_chat ->
            Vercel_ai_gateway_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Vercel_ai_gateway_api.request ~model:config.model messages tools,
            Vercel_ai_gateway_api.parse_completion
        | Cloudflare_ai_gateway_chat ->
            Cloudflare_ai_gateway_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Cloudflare_ai_gateway_api.request ~model:config.model messages tools,
            Cloudflare_ai_gateway_api.parse_completion
        | Commandcode_chat ->
            Commandcode_api.chat_headers ~endpoint:config.endpoint ~api_key,
            parse (fun () ->
              Commandcode_api.chat_request ~model:config.model messages tools),
            Commandcode_api.parse_chat_completion ~model:config.model
        | Minimax_chat ->
            Minimax_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Minimax_api.request ~model:config.model messages tools,
            Minimax_api.parse_completion
        | Deepseek_chat ->
            Deepseek_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Deepseek_api.request ~model:config.model ?thinking messages tools,
            Deepseek_api.parse_completion
        | Mistral_chat ->
            Mistral_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Mistral_api.request ~model:config.model ?thinking messages tools,
            Mistral_api.parse_completion
        | Openrouter_chat ->
            Openrouter_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Openrouter_api.request ~model:config.model ?thinking messages tools,
            Openrouter_api.parse_completion
        | Umans_chat ->
            Umans_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Umans_api.request ~model:config.model ?thinking messages tools,
            Umans_api.parse_completion
        | Fireworks_chat ->
            Fireworks_api.chat_headers ~endpoint:config.endpoint ~api_key,
            Fireworks_api.request ~model:config.model ?thinking messages tools,
            Fireworks_api.parse_completion ~model:config.model
        | Cline_pass_chat ->
            Cline_pass_api.chat_headers ~endpoint:config.endpoint ~api_key,
            openai_request (), Protocol.parse_completion
        | Alibaba_token_plan_chat ->
            Alibaba_token_plan_api.chat_headers
              ~endpoint:config.endpoint ~api_key,
            Alibaba_token_plan_api.request ~model:config.model ?thinking
              messages tools,
            Alibaba_token_plan_api.parse_completion ~model:config.model
        | Kimi_code_chat ->
            if config.endpoint <> Kimi_code_api.intl_openai_chat_url then
              raise (Provider_error "Kimi Code international credentials require the international Chat endpoint");
            Kimi_code_api.chat_headers ~endpoint:config.endpoint ~api_key,
            openai_request (), Protocol.parse_completion
        | Kimi_code_cn_chat ->
            if config.endpoint <> Kimi_code_api.china_openai_chat_url then
              raise (Provider_error "Kimi Code China credentials require the China Chat endpoint");
            Kimi_code_api.chat_headers ~endpoint:config.endpoint ~api_key,
            openai_request (), Protocol.parse_completion
        | _ -> assert false
        with Invalid_argument reason -> raise (Provider_error reason)) in
      let json = post_json ?cancel ~endpoint:config.endpoint
        ~headers ~secret:api_key body in
      let reply = parse (fun () -> parse_reply json) in
      (match on_usage with
      | None -> ()
      | Some report ->
          check_cancel cancel;
          Option.iter report (Protocol.completion_usage json));
      (match on_text, reply.content with
      | Some emit, Some text -> check_cancel cancel; emit text
      | _ -> ());
      reply
  | Opencode_zen_responses ->
      let headers = try
        Opencode_zen_api.responses_headers ~endpoint:config.endpoint ~api_key
      with Invalid_argument reason -> raise (Provider_error reason) in
      let body = parse (fun () ->
        Opencode_zen_api.request ~model:config.model messages tools) in
      let json = post_json ?cancel ~endpoint:config.endpoint
        ~headers ~secret:api_key body in
      let reply = parse (fun () ->
        Opencode_zen_api.parse_completion ~model:config.model json) in
      (match on_usage with
       | None -> ()
       | Some report ->
           check_cancel cancel;
           Option.iter report (Openai_responses_wire.usage json));
      (match on_text, reply.content with
       | Some emit, Some text -> check_cancel cancel; emit text
       | _ -> ());
      reply
  | Meta_responses ->
      let headers = try
        Meta_api.responses_headers ~endpoint:config.endpoint ~api_key
      with Invalid_argument reason -> raise (Provider_error reason) in
      let body = parse (fun () ->
        Meta_api.request ~model:config.model messages tools) in
      let json = post_json ?cancel ~endpoint:config.endpoint
        ~headers ~secret:api_key body in
      let reply = parse (fun () ->
        Meta_api.parse_completion ~model:config.model json) in
      (match on_usage with
       | None -> ()
       | Some report ->
           check_cancel cancel;
           Option.iter report (Openai_responses_wire.usage json));
      (match on_text, reply.content with
       | Some emit, Some text -> check_cancel cancel; emit text
       | _ -> ());
      reply
  | Commandcode_responses ->
      let headers = try
        Commandcode_api.responses_headers ~endpoint:config.endpoint ~api_key
      with Invalid_argument reason -> raise (Provider_error reason) in
      let body = parse (fun () ->
        Commandcode_api.responses_request ~model:config.model messages tools) in
      let json = post_json ?cancel ~endpoint:config.endpoint
        ~headers ~secret:api_key body in
      let reply = parse (fun () ->
        Commandcode_api.parse_responses_completion ~model:config.model json) in
      (match on_usage with
       | None -> ()
       | Some report ->
           check_cancel cancel;
           Option.iter report (Openai_responses_wire.usage json));
      (match on_text, reply.content with
       | Some emit, Some text -> check_cancel cancel; emit text
       | _ -> ());
      reply
  | Commandcode_messages ->
      let headers = try
        Commandcode_api.messages_headers ~endpoint:config.endpoint ~api_key
      with Invalid_argument reason -> raise (Provider_error reason) in
      let body = parse (fun () ->
        Commandcode_api.messages_request ~model:config.model ~max_tokens:output_tokens
          messages tools) in
      let json = post_json ?cancel ~endpoint:config.endpoint
        ~headers ~secret:api_key body in
      let reply = parse (fun () ->
        Commandcode_api.parse_messages_completion ~model:config.model json) in
      (match on_usage with
       | None -> ()
       | Some report ->
           check_cancel cancel;
           Option.iter report (Anthropic_wire.usage json));
      (match on_text, reply.content with
       | Some emit, Some text -> check_cancel cancel; emit text
       | _ -> ());
      reply
  | Devin_connect ->
      if config.endpoint <> Devin_api.chat_url then
        raise (Provider_error "Devin session credential requires the pinned Connect endpoint");
      let models = try (match Devin_api.discover ?cancel ~api_key () with
        | Ok models -> models
        | Error Devin_api.Invalid_credential ->
            raise (Provider_error "invalid Devin session credential")
        | Error (Devin_api.Transport_error reason) ->
            raise (Provider_error ("Devin model discovery transport failed: " ^ reason))
        | Error (Devin_api.Http_error (status, code)) ->
            raise (Provider_error (devin_http_error
              "Devin model discovery" status code))
        | Error (Devin_api.Invalid_response reason) ->
            raise (Provider_error reason))
        with Devin_binary_http.Cancelled -> raise Cancelled in
      let selected = match List.find_opt
        (fun (entry : Devin_api.model) -> entry.id = config.model) models with
        | Some model -> model
        | None -> raise (Provider_error "Devin model is not in this account's live roster") in
      if tools <> [] && selected.supports_tools <> Some true then
        raise (Provider_error
          "selected Devin model does not report tool support");
      let cascade_id = try Devin_api.cascade_id messages
        with Devin_api.Bad_wire reason -> raise (Provider_error reason) in
      let reply, usage = try (match Devin_api.complete ?cancel ~api_key
        ~model:selected.id ~cascade_id ~router:selected.router
        ~max_tokens:(Option.value ~default:64000 selected.max_tokens)
        ~supports_parallel_tool_calls:(Option.value ~default:false
          selected.supports_parallel_tool_calls)
        messages tools with
        | Ok completion -> completion
        | Error Devin_api.Invalid_credential ->
            raise (Provider_error "invalid Devin session credential")
        | Error (Devin_api.Transport_error reason) ->
            raise (Provider_error ("Devin Connect transport failed: " ^ reason))
        | Error (Devin_api.Http_error (status, code)) ->
            raise (Provider_error (devin_http_error
              "Devin Connect" status code))
        | Error (Devin_api.Invalid_response reason) ->
            raise (Provider_error reason))
        with Devin_binary_http.Cancelled -> raise Cancelled in
      (match on_usage, usage with
       | Some report, Some (input_tokens, output_tokens) ->
           check_cancel cancel;
           report { Protocol.input_tokens = input_tokens; output_tokens;
             cached_input_tokens = None; cache_creation_input_tokens = None;
             reasoning_output_tokens = None;
             input_modality_tokens = None; cached_input_modality_tokens = None;
             output_modality_tokens = None }
       | _ -> ());
      (match on_text, reply.content with
       | Some emit, Some text -> check_cancel cancel; emit text
       | _ -> ());
      reply
  | Gitlab_duo_messages | Gitlab_duo_responses | Gitlab_duo_chat ->
      let module Duo = Gitlab_duo_api in
      let route = match config.api with
        | Gitlab_duo_messages -> Duo.Anthropic
        | Gitlab_duo_responses -> Duo.Openai_responses
        | Gitlab_duo_chat -> Duo.Openai_completions
        | _ -> assert false in
      if config.endpoint <> Duo.endpoint route then
        raise (Provider_error "GitLab account credentials require their pinned Duo route");
      let exchange = post_json ?cancel ~endpoint:Duo.direct_access_url
        ~headers:["Authorization: Bearer " ^ api_key]
        ~secret:api_key
        (`Assoc ["feature_flags", `Assoc ["DuoAgentPlatformNext", `Bool true]]) in
      let access = match Duo.parse_access exchange with
        | Ok access -> access
        | Error Duo.Invalid_credential ->
            raise (Provider_error "invalid GitLab account credential")
        | Error (Duo.Http_error status) ->
            raise (Provider_error (Printf.sprintf "GitLab Direct Access HTTP %d" status))
        | Error Duo.Transport_error ->
            raise (Provider_error "GitLab Direct Access transport failed")
        | Error (Duo.Invalid_response reason) ->
            raise (Provider_error reason) in
      let headers = try Duo.gateway_headers ~endpoint:config.endpoint access
        with Invalid_argument reason -> raise (Provider_error reason) in
      let body = parse (fun () -> Duo.request ~route ~model:config.model
        ~max_tokens:output_tokens messages tools) in
      let json = post_json ?cancel ~endpoint:config.endpoint
        ~headers ~secret:access.token body in
      let reply = parse (fun () ->
        Duo.parse_completion ~route ~model:config.model json) in
      (match on_usage with
       | None -> ()
       | Some report ->
           check_cancel cancel;
           let usage = match route with
             | Duo.Anthropic -> Anthropic_wire.usage json
             | Duo.Openai_responses -> Openai_responses_wire.usage json
             | Duo.Openai_completions -> Protocol.completion_usage json in
           Option.iter report usage);
      (match on_text, reply.content with
       | Some emit, Some text -> check_cancel cancel; emit text
       | _ -> ());
      reply
  | Umans_messages | Kimi_code_messages | Kimi_code_cn_messages ->
      if authentication <> Api_key then
        raise (Provider_error "this Messages route requires its API key");
      let headers = try
        match config.api with
        | Umans_messages ->
            Umans_api.messages_headers ~endpoint:config.endpoint ~api_key
            |> List.map (fun (name, value) -> name ^ ": " ^ value)
        | Kimi_code_messages ->
            if config.endpoint <> Kimi_code_api.intl_messages_url then
              raise (Provider_error "Kimi Code international credentials require the international Messages endpoint");
            Kimi_code_api.messages_headers ~endpoint:config.endpoint ~api_key
            |> List.map (fun (name, value) -> name ^ ": " ^ value)
        | Kimi_code_cn_messages ->
            if config.endpoint <> Kimi_code_api.china_messages_url then
              raise (Provider_error "Kimi Code China credentials require the China Messages endpoint");
            Kimi_code_api.messages_headers ~endpoint:config.endpoint ~api_key
            |> List.map (fun (name, value) -> name ^ ": " ^ value)
        | _ -> assert false
        with Invalid_argument reason -> raise (Provider_error reason) in
      let native_provider = match config.api with
        | Umans_messages -> "umans"
        | Kimi_code_messages -> "kimi-code"
        | Kimi_code_cn_messages -> "kimi-code-cn"
        | _ -> assert false in
      let replay_assistant_content =
        Anthropic_wire.replay_native_content ~provider:native_provider
          ~model:config.model in
      let body = parse (fun () ->
        Anthropic_wire.request ~allow_compaction:false
          ~allow_prompt_caching:false
          ~replay_assistant_content ~model:config.model ~max_tokens:output_tokens
          messages tools) in
      let body = if config.api = Umans_messages then
        (try Umans_api.add_messages_reasoning_effort ~thinking body
         with Invalid_argument reason -> raise (Provider_error reason))
        else body in
      let json = post_json ?cancel ~endpoint:config.endpoint
        ~headers ~secret:api_key body in
      let reply = parse (fun () ->
        Anthropic_wire.parse_native_completion ~provider:native_provider
          ~model:config.model json) in
      (match on_usage with
       | None -> ()
       | Some report ->
           check_cancel cancel;
           Option.iter report (Anthropic_wire.usage json));
      (match on_text, reply.content with
       | Some emit, Some text -> check_cancel cancel; emit text
       | _ -> ());
      reply
  | Anthropic_messages ->
      let allow_direct_api_key_features = authentication = Api_key &&
        config.endpoint = "https://api.anthropic.com/v1/messages" in
      let allow_compaction = allow_direct_api_key_features in
      let allow_prompt_caching = allow_direct_api_key_features in
      let compaction_beta = allow_compaction &&
        Anthropic_wire.requires_compaction_beta ~model:config.model messages in
      let replay_assistant_content =
        Anthropic_wire.replay_native_content ~provider:"anthropic"
          ~model:config.model in
      let body = parse (fun () ->
        Anthropic_wire.request ~allow_compaction ~allow_prompt_caching
          ~replay_assistant_content ?thinking
          ~model:config.model ~max_tokens:output_tokens messages tools) in
      let headers = [ "anthropic-version: 2023-06-01" ] @
        (if compaction_beta then
          ["anthropic-beta: " ^ Anthropic_wire.compaction_beta] else []) @
        (match authentication with
         | Api_key ->
             if api_key = "" then [] else [ "x-api-key: " ^ api_key ]
         | OAuth ->
             [ "Authorization: Bearer " ^ api_key;
               "anthropic-beta: oauth-2025-04-20,claude-code-20250219";
               "anthropic-dangerous-direct-browser-access: true";
               "User-Agent: claude-cli/2.0.0 (external, cli)"; "x-app: cli" ]
         | Cloud_identity ->
             raise (Provider_error
               "Anthropic Messages does not accept cloud identity credentials")) in
      (match on_text with
      | None ->
          let json = post_json ?cancel ~endpoint:config.endpoint ~headers ~secret:api_key body in
          let reply = parse (fun () ->
            Anthropic_wire.parse_native_completion ~provider:"anthropic"
              ~model:config.model json) in
          (match on_usage with
           | None -> ()
           | Some report ->
               check_cancel cancel;
               Option.iter report (Anthropic_wire.usage json));
          reply
      | Some emit ->
          let stream = Anthropic_stream.create ?on_tool_arguments
            ~provider:"anthropic" ~model:config.model ~on_text:emit () in
          let body = match body with
            | `Assoc fields -> `Assoc (fields @ [ "stream", `Bool true ])
            | _ -> assert false in
          parse (fun () ->
            let sse_events = ref (Anthropic_stream.events stream) in
            post_stream ?cancel ~endpoint:config.endpoint ~headers ~secret:api_key
              body ~on_chunk:(Anthropic_stream.feed stream)
              ~is_done:(fun () -> Anthropic_stream.is_done stream)
              ~is_finished:(fun () -> Anthropic_stream.is_finished stream)
              ~progress:(fun () ->
                let count = Anthropic_stream.events stream in
                count > !sse_events && (sse_events := count; true));
            let reply = Anthropic_stream.finish stream in
            (match on_usage with
             | None -> ()
             | Some report ->
                 check_cancel cancel;
                 Option.iter report (Anthropic_stream.usage stream));
            reply))
  | Openai_responses | Azure_responses | Bedrock_mantle_responses ->
      let endpoint, headers, secret =
        if config.api = Azure_responses then
          azure_resolve Azure_wire.Responses
        else if config.api = Bedrock_mantle_responses then
          (try
             let requested = if config.endpoint = "" then
               (Bedrock_mantle.endpoint ~region:(Bedrock_mantle.region ()) ()).url
               else config.endpoint in
             let endpoint, headers =
               Bedrock_mantle.resolve ~endpoint:requested ~api_key () in
             endpoint, headers, api_key
           with Invalid_argument message -> raise (Provider_error message))
        else config.endpoint,
          (if api_key = "" then [] else [ "Authorization: Bearer " ^ api_key ]),
          api_key in
      let body = parse (fun () ->
        Openai_responses_wire.request ~model:config.model ?thinking
          ~max_output_tokens:output_tokens messages tools) in
      (match on_text with
      | None ->
          let json = post_json ?cancel ~endpoint ~headers ~secret body in
          let reply = parse_with_secret secret
            (fun () -> Openai_responses_wire.parse_completion json) in
          (match on_usage with
           | None -> ()
           | Some report ->
               check_cancel cancel;
               Option.iter report (Openai_responses_wire.usage json));
          reply
      | Some emit ->
          let stream = Openai_responses_stream.create ?on_tool_arguments ~on_text:emit () in
          let body = parse (fun () ->
            Openai_responses_wire.request ~stream:true ?thinking
              ~max_output_tokens:output_tokens
              ~model:config.model messages tools) in
          parse_with_secret secret (fun () ->
            let sse_events = ref (Openai_responses_stream.events stream) in
            post_stream ?cancel ~endpoint ~headers ~secret
              body ~on_chunk:(Openai_responses_stream.feed stream)
              ~is_done:(fun () -> Openai_responses_stream.is_done stream)
              ~is_finished:(fun () -> Openai_responses_stream.is_finished stream)
              ~progress:(fun () ->
                let count = Openai_responses_stream.events stream in
                count > !sse_events && (sse_events := count; true));
            let reply = Openai_responses_stream.finish stream in
            (match on_usage with
             | None -> ()
             | Some report ->
                 check_cancel cancel;
                 Option.iter report (Openai_responses_stream.usage stream));
            reply))
  | Bedrock_converse | Bedrock_converse_stream ->
      if authentication <> Cloud_identity || api_key <> "" then
        raise (Provider_error
          "Bedrock Converse requires AWS cloud identity, not an API key");
      let streaming = config.api = Bedrock_converse_stream in
      let region, keys =
        try Aws_auth.region (),
          Aws_auth.resolve
            ~credential_process_policy:(Aws_auth.credential_process_policy ())
            ?cancel ()
        with
        | Aws_auth.Cancelled -> raise Cancelled
        | Invalid_argument reason -> raise (Provider_error reason)
        | Unix.Unix_error _ | Sys_error _ ->
            raise (Provider_error "AWS credentials are unavailable") in
      let target = try
        if streaming then
          Bedrock_wire.converse_stream_endpoint ~region ~model:config.model
            ?base_url:(if config.endpoint = "" then None else Some config.endpoint) ()
        else
          Bedrock_wire.endpoint ~region ~model:config.model
            ?base_url:(if config.endpoint = "" then None else Some config.endpoint) ()
        with Invalid_argument reason -> raise (Provider_error reason) in
      let body = parse (fun () -> Bedrock_wire.request messages tools) in
      let serialized = Yojson.Basic.to_string body in
      let signed = try
        if streaming then
          Aws_auth.sign_converse_stream ~credentials:keys ~region
            ~amz_date:(Aws_auth.amz_date ()) ~method_:"POST"
            ~host:target.host ~path:target.path ~body:serialized ()
        else
          Aws_auth.sign ~credentials:keys ~region
            ~amz_date:(Aws_auth.amz_date ()) ~method_:"POST"
            ~host:target.host ~path:target.path ~body:serialized ()
        with Invalid_argument reason -> raise (Provider_error reason) in
      let headers = List.filter_map (fun (name, value) ->
        if name = "content-type" then None else Some (name ^ ": " ^ value)) signed in
      if not streaming then (
        let json = post_json ~local:(config.endpoint <> "") ?cancel
          ~endpoint:target.url ~headers ~secret:keys.access_key_id body in
        let reply = parse (fun () -> Bedrock_wire.parse_response json) in
        (match on_usage with
         | None -> ()
         | Some report ->
             check_cancel cancel;
             Option.iter report (Bedrock_wire.usage json));
        (match on_text, reply.content with
         | Some emit, Some text -> check_cancel cancel; emit text
         | _ -> ());
        reply
      ) else
        let stream = Bedrock_wire.create_converse_stream ?on_tool_arguments () in
        let content = Buffer.create 256 and calls = ref [] and usage = ref None in
        parse_with_secret keys.access_key_id (fun () ->
          post_stream ~local:(config.endpoint <> "") ?cancel
            ~endpoint:target.url ~headers ~secret:keys.access_key_id body
            ~on_chunk:(fun chunk ->
              Bedrock_wire.feed_converse_stream stream chunk
              |> List.iter (function
                | Bedrock_wire.Text_delta text ->
                    Buffer.add_string content text;
                    (match on_text with
                     | None -> ()
                     | Some emit -> check_cancel cancel; emit text)
                | Bedrock_wire.Tool_call call -> calls := call :: !calls
                | Bedrock_wire.Message_stop _ -> ()
                | Bedrock_wire.Usage reported -> usage := Some reported))
            ~is_done:(fun () -> false) ~is_finished:(fun () -> false);
          Bedrock_wire.finish_converse_stream stream);
        (match on_usage with
         | None -> ()
         | Some report ->
             check_cancel cancel;
             Option.iter report !usage);
        { Protocol.role = "assistant";
          content = if Buffer.length content = 0 then None
            else Some (Buffer.contents content);
          tool_calls = List.rev !calls;
          tool_call_id = None; tool_result_content = None;
          provider_state = None; attachments = [] }
  | Ollama_chat ->
      let cloud = api_key <> "" in
      if cloud && config.endpoint <> "https://ollama.com/api/chat" then
        raise (Provider_error "Ollama Cloud token requires the official cloud endpoint");
      if cloud && (String.length api_key > 8192 ||
        not (String.for_all (fun c -> Char.code c > 32 &&
          Char.code c < 127) api_key)) then
        raise (Provider_error "invalid Ollama Cloud API key");
      let headers = if cloud then [ "Authorization: Bearer " ^ api_key ] else [] in
      let body = parse (fun () ->
        Ollama_wire.request ~model:config.model ?thinking messages tools) in
      (match on_text with
      | None ->
          let json = post_json ?cancel ~endpoint:config.endpoint ~headers
            ~secret:api_key body in
          let reply = parse (fun () -> Ollama_wire.parse_completion json) in
          (match on_usage with
           | None -> ()
           | Some report ->
               check_cancel cancel;
               Option.iter report (Ollama_wire.usage json));
          reply
      | Some emit ->
          let stream = Ollama_stream.create ?on_tool_arguments ~on_text:emit () in
          let body = match body with
            | `Assoc fields ->
                `Assoc (("stream", `Bool true) :: List.remove_assoc "stream" fields)
            | _ -> assert false in
          parse (fun () ->
            post_stream ?cancel ~endpoint:config.endpoint ~headers ~secret:api_key
              body ~on_chunk:(Ollama_stream.feed stream)
              ~is_done:(fun () -> Ollama_stream.is_done stream)
              ~is_finished:(fun () -> Ollama_stream.is_finished stream);
            let reply = Ollama_stream.finish stream in
            (match on_usage with
             | None -> ()
             | Some report ->
                 check_cancel cancel;
                 Option.iter report (Ollama_stream.usage stream));
            reply))
  | Gemini_direct ->
      let body = parse (fun () ->
        Gemini_wire.request ~model:config.model ?thinking messages tools) in
      let headers = [ "x-goog-api-key: " ^ api_key ] in
      let base = if String.ends_with ~suffix:"/" config.endpoint then
        String.sub config.endpoint 0 (String.length config.endpoint - 1)
        else config.endpoint in
      let model_path = gemini_model_path config.model in
      (match on_text with
      | None ->
          let endpoint = base ^ "/" ^ model_path ^ ":generateContent" in
          let json = post_json ~max_request_bytes:gemini_max_request_bytes
            ?cancel ~endpoint ~headers ~secret:api_key body in

          let reply = parse (fun () -> Gemini_wire.parse_completion ~model:config.model json) in
          (match on_usage with
           | None -> ()
           | Some report ->
               check_cancel cancel;
               Option.iter report (Gemini_wire.usage json));
          reply
      | Some emit ->
          let stream = Gemini_stream.create ?on_tool_arguments ~model:config.model ~on_text:emit () in
          let endpoint = base ^ "/" ^ model_path ^ ":streamGenerateContent?alt=sse" in
          parse (fun () ->
            let sse_events = ref (Gemini_stream.events stream) in
            post_stream ~max_request_bytes:gemini_max_request_bytes ?cancel
              ~endpoint ~headers ~secret:api_key body
              ~on_chunk:(Gemini_stream.feed stream)
              ~is_done:(fun () -> Gemini_stream.is_done stream)
              ~is_finished:(fun () -> Gemini_stream.is_finished stream)
              ~progress:(fun () ->
                let count = Gemini_stream.events stream in
                count > !sse_events && (sse_events := count; true));

            let reply = Gemini_stream.finish stream in
            (match on_usage with
             | None -> ()
             | Some report ->
                 check_cancel cancel;
                 Option.iter report (Gemini_stream.usage stream));
            reply))
  | Vertex_generate ->
      if authentication <> Cloud_identity || config.endpoint <> "" || api_key <> "" then
        raise (Provider_error "Vertex requires scoped Google cloud identity and a derived Google endpoint");
      let project, location, access =
        try
          let project, location = Vertex_wire.resolve_environment () in
          project, location, Vertex_auth.access_token ?cancel ()
        with
        | Invalid_argument reason -> raise (Provider_error reason)
        | Vertex_auth.Cancelled -> raise Cancelled
        | Vertex_auth.Authentication_error reason -> raise (Provider_error reason) in
      let endpoint =
        try Vertex_wire.endpoint ~project ~location ~model:config.model
        with Invalid_argument reason -> raise (Provider_error reason) in
      let body = parse (fun () -> Vertex_wire.request ~model:config.model
        ?thinking messages tools) in
      let emit = Option.value ~default:(fun _ -> ()) on_text in
      let stream = Gemini_stream.create ?on_tool_arguments ~model:config.model ~on_text:emit () in
      parse (fun () ->
        let sse_events = ref (Gemini_stream.events stream) in
        post_stream ~max_request_bytes:gemini_max_request_bytes ?cancel
          ~endpoint ~headers:["Authorization: Bearer " ^ access] ~secret:access
          body ~on_chunk:(Gemini_stream.feed stream)
          ~is_done:(fun () -> Gemini_stream.is_done stream)
          ~is_finished:(fun () -> Gemini_stream.is_finished stream)
          ~progress:(fun () ->
            let count = Gemini_stream.events stream in
            count > !sse_events && (sse_events := count; true));
        let reply = Vertex_wire.finish_stream ~model:config.model stream in
        (match on_usage with
        | None -> ()
        | Some report ->
            check_cancel cancel;
            Option.iter report (Gemini_stream.usage stream));
        reply)
  | Vertex_anthropic ->
      if authentication <> Cloud_identity || config.endpoint <> "" || api_key <> "" then
        raise (Provider_error
          "Vertex Claude requires Google cloud identity and a derived Google endpoint");
      let project, location, access =
        try
          let project, location = Vertex_wire.resolve_environment () in
          project, location, Vertex_auth.access_token ?cancel ()
        with
        | Invalid_argument reason -> raise (Provider_error reason)
        | Vertex_auth.Cancelled -> raise Cancelled
        | Vertex_auth.Authentication_error reason -> raise (Provider_error reason) in
      let streaming = Option.is_some on_text in
      let endpoint = try Vertex_anthropic_wire.endpoint
        ~project ~location ~model:config.model ~streaming
        with Invalid_argument reason -> raise (Provider_error reason) in
      let body = parse (fun () ->
        Vertex_anthropic_wire.request ~model:config.model ~max_tokens:output_tokens
          ~streaming ?thinking messages tools) in
      let headers = ["Authorization: Bearer " ^ access] in
      if streaming then (
        let emit = Option.value ~default:(fun _ -> ()) on_text in
        let stream = Vertex_anthropic_wire.create_stream ?on_tool_arguments
          ~model:config.model ~on_text:emit () in
        parse (fun () ->
          let sse_events = ref (Anthropic_stream.events stream) in
          post_stream ?cancel ~endpoint ~headers ~secret:access body
            ~on_chunk:(Vertex_anthropic_wire.feed_stream stream)
            ~is_done:(fun () -> Anthropic_stream.is_done stream)
            ~is_finished:(fun () ->
              Vertex_anthropic_wire.stream_is_finished stream)
            ~progress:(fun () ->
              let count = Anthropic_stream.events stream in
              count > !sse_events && (sse_events := count; true));
          let reply = Vertex_anthropic_wire.finish_stream
            ~model:config.model stream in
          (match on_usage with
           | None -> ()
           | Some report ->
               check_cancel cancel;
               Option.iter report (Vertex_anthropic_wire.stream_usage stream));
          reply))
      else
        let json = post_json ?cancel ~endpoint ~headers ~secret:access body in
        let reply = parse (fun () ->
          Vertex_anthropic_wire.parse_completion ~model:config.model json) in
        (match on_usage with
         | None -> ()
         | Some report ->
             check_cancel cancel;
             Option.iter report (Anthropic_wire.usage json));
        reply
  | Codex_responses ->
      let account_id = match credential.account_id with
        | Some id when id <> "" -> id
        | _ -> raise (Provider_error "Codex OAuth account ID unavailable") in
      reject_controls "account ID" account_id;
      reject_controls "model" config.model;
      let model_format =
        let rec from_listing = function
          | [] -> raise (Provider_error
              "Codex account model listing unavailable; cannot determine request format")
          | url :: rest ->
              check_cancel cancel;
              with_temp_file (fun response_path output ->
                close_out output;
                let option name value = name ^ " = " ^ quote_config value ^ "\n" in
                let configuration = "silent\n" ^
                  option "url" url ^ option "request" "GET" ^
                  option "output" response_path ^
                  option "write-out" "%{http_code}" ^
                  option "connect-timeout" "10" ^
                  option "max-time" "30" ^
                  option "max-filesize" "1048576" ^
                  option "proto" "=https" ^
                  option "proxy" "" ^
                  option "header" ("Authorization: Bearer " ^ api_key) ^
                  option "header" ("chatgpt-account-id: " ^ account_id) ^
                  option "header" "OpenAI-Beta: responses=experimental" ^
                  option "header" "originator: pave" ^
                  option "header" ("version: " ^ Codex_wire.client_version) ^
                  option "header" "Accept: application/json" in
                let status = run_curl ?cancel configuration in
                check_cancel cancel;
                let code = try int_of_string status with Failure _ ->
                  raise (Provider_error "invalid Codex model listing HTTP status") in
                if code = 404 && rest <> [] then from_listing rest
                else if code < 200 || code >= 300 then
                  raise (Provider_error
                    ("Codex account model listing: " ^ http_error_reason api_key code `Null))
                else
                  let listing = try Yojson.Basic.from_string (read_file response_path)
                    with Yojson.Json_error _ ->
                      raise (Provider_error "invalid Codex account model listing JSON") in
                  parse (fun () ->
                    Codex_wire.model_format ?thinking ~model:config.model listing)) in
        from_listing Codex_wire.models_urls in
      let headers = [
        "Authorization: Bearer " ^ api_key;
        "chatgpt-account-id: " ^ account_id;
        "OpenAI-Beta: responses=experimental";
        "originator: pave";
        "version: " ^ Codex_wire.client_version;
        "x-codex-routing-hint: model=" ^ config.model;
        "Accept: text/event-stream" ] @
        (match model_format with
         | Codex_wire.Standard -> []
         | Codex_wire.Responses_lite _ ->
             ["x-openai-internal-codex-responses-lite: true"]) @
        (match credential.residency with
         | None -> []
         | Some residency ->
             reject_controls "Codex residency" residency;
             [ "x-openai-internal-codex-residency: " ^ residency ]) in
      let body = parse (fun () ->
        Codex_wire.request ~format:model_format ?thinking ~model:config.model messages tools) in
      let emit = match on_text with Some emit -> emit | None -> fun _ -> () in
      let stream = Codex_stream.create ?on_tool_arguments ~model:config.model ~on_text:emit () in
      (try parse (fun () ->
        let sse_events = ref (Codex_stream.events stream) in
        post_stream ?cancel ~endpoint:config.endpoint ~headers ~secret:api_key
          body ~on_chunk:(Codex_stream.feed stream)
          ~is_done:(fun () -> Codex_stream.is_done stream)
          ~is_finished:(fun () -> Codex_stream.is_finished stream)
          ~progress:(fun () ->
            let count = Codex_stream.events stream in
            count > !sse_events && (sse_events := count; true));
        let reply = Codex_stream.finish stream in
        (match on_usage with
         | None -> ()
         | Some report ->
             check_cancel cancel;
             Option.iter report (Codex_stream.usage stream));
        reply)
       with Provider_error reason ->
         raise (Provider_error (redact account_id reason)))
  in
  check_cancel cancel;
  result

type native_compaction = {
  summary : string;
  provider_state : Yojson.Basic.t;
}
let compact_anthropic_messages ?(authentication = Api_key) ?resolve_credential
    ?max_output_tokens ?cancel ?on_usage config ~instructions ~messages ~tools =
  if config.api <> Anthropic_messages || authentication <> Api_key then
    raise (Provider_error "native compaction requires the Anthropic Messages API-key route");
  if config.endpoint <> "https://api.anthropic.com/v1/messages" then
    raise (Provider_error
      "native compaction requires the official Anthropic Messages endpoint");
  if config.model = "" then raise (Provider_error "empty Anthropic model");
  reject_controls "model" config.model;
  let credential = match resolve_credential with
    | Some resolve -> resolve ()
    | None -> { access = config.api_key; account_id = None; residency = None } in
  let api_key = credential.access in
  reject_controls "API key" api_key;
  if api_key = "" then raise (Provider_error "missing Anthropic API key");
  check_cancel cancel;
  let body = Anthropic_wire.compaction_request ~allow_prompt_caching:true
    ~model:config.model
    ~max_tokens:(Option.value ~default:4096 max_output_tokens)
    ~instructions messages tools in
  let headers = [
    "anthropic-version: 2023-06-01";
    "anthropic-beta: " ^ Anthropic_wire.compaction_beta;
    "x-api-key: " ^ api_key ] in
  let json = post_json ?cancel ~endpoint:config.endpoint ~headers
    ~secret:api_key body in
  let content, signature =
    Anthropic_wire.parse_compaction_response json in
  check_cancel cancel;
  (match on_usage, Anthropic_wire.usage json with
   | Some report, Some usage -> report usage
   | _ -> ());
  { summary = content;
    provider_state = Anthropic_wire.compaction_state ~model:config.model
      ~content ~signature }


let compact_openai_responses ?(authentication = Api_key) ?resolve_credential
    ?cancel ?on_usage config ~instructions messages =
  if config.api <> Openai_responses || authentication <> Api_key then
    raise (Provider_error "native compaction requires the OpenAI Responses API-key route");
  if config.model = "" then raise (Provider_error "empty Responses model");
  reject_controls "model" config.model;
  let endpoint =
    if String.ends_with ~suffix:"/responses" config.endpoint &&
       not (String.contains config.endpoint '?' ||
            String.contains config.endpoint '#')
    then config.endpoint ^ "/compact"
    else raise (Provider_error
      "native compaction requires a Responses endpoint ending in /responses") in
  let credential = match resolve_credential with
    | Some resolve -> resolve ()
    | None -> { access = config.api_key; account_id = None; residency = None } in
  let api_key = credential.access in
  reject_controls "API key" api_key;
  if api_key = "" then raise (Provider_error "missing OpenAI API key");
  check_cancel cancel;
  let wire = Openai_responses_wire.request ~model:config.model messages [] in
  let input = match Protocol.member "input" wire with
    | `List input -> input
    | _ -> raise (Provider_error "Responses compaction input is missing") in
  let body = `Assoc [
    "model", `String config.model;
    "input", `List input;
    "instructions", `String instructions ] in
  let json = post_json ?cancel ~endpoint
    ~headers:["Authorization: Bearer " ^ api_key] ~secret:api_key body in
  let raw_output = match Protocol.member "output" json with
    | `List items -> items
    | _ -> raise (Protocol.Invalid_response
        "invalid OpenAI compaction response: missing output items") in
  let kept = List.filter (fun item ->
    match Protocol.member "type" item with
    | `String "compaction" ->
        (match Protocol.member "encrypted_content" item with
         | `String value -> value <> ""
         | _ -> false)
    | `String "compaction_summary" ->
        (match Protocol.member "summary" item with
         | `String value -> String.trim value <> ""
         | _ -> false)
    | `String "message" ->
        List.mem (Protocol.member "role" item)
          [`String "assistant"; `String "user"]
    | _ -> false) raw_output in
  if not (List.exists (fun item ->
    match Protocol.member "type" item with
    | `String ("compaction" | "compaction_summary") -> true
    | _ -> false) kept) then
    raise (Protocol.Invalid_response
      "invalid OpenAI compaction response: missing native compaction item");
  check_cancel cancel;
  (match on_usage, Openai_responses_wire.usage json with
   | Some report, Some usage -> report usage
   | _ -> ());
  let summary = match List.find_opt (fun item ->
    Protocol.member "type" item = `String "compaction_summary") kept with
    | Some item ->
        (match Protocol.member "summary" item with
         | `String text when String.trim text <> "" -> String.trim text
         | _ -> assert false)
    | None -> "OpenAI Responses compacted context" in
  { summary;
    provider_state = `Assoc [
      "provider", `String "openai";
      "route", `String "responses";
      "model", `String config.model;
      "items", `List kept ] }
