type api = Openai_completions | Local_chat | Anthropic_messages | Openai_responses
  | Azure_responses | Bedrock_mantle_responses | Ollama_chat | Gemini_direct
  | Vertex_generate | Bedrock_converse | Xai_chat | Nvidia_chat
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
type authentication = Api_key | OAuth
type config = { endpoint : string; api_key : string; model : string; api : api }
type credentials = {
  access : string;
  account_id : string option;
  residency : string option;
}

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

let check_cancel = function
  | Some cancel when cancel () -> raise Cancelled
  | _ -> ()

let read_all ?on_chunk ?is_done ?is_finished ?cancel fd =
  let buffer = Buffer.create 128 in
  let chunk = Bytes.create 8192 in
  let finished_at = ref None in
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

let run_curl ?on_chunk ?is_done ?is_finished ?cancel configuration =
  check_cancel cancel;
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
      let status_code = read_all ?on_chunk ?is_done ?is_finished ?cancel output_read in
      close_fd output_read;
      let status = wait_for ?cancel pid in
      waited := true;
      check_cancel cancel;
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

let request_body ~local ~endpoint ~headers body_json =
  if not (String.starts_with ~prefix:"https://" endpoint ||
    (String.starts_with ~prefix:"http://" endpoint &&
      (local || loopback_http endpoint))) then
    raise (Provider_error "completion endpoint must use HTTPS or validated local HTTP");
  reject_controls "endpoint" endpoint;
  List.iter (reject_controls "header") headers;
  let body = Yojson.Basic.to_string body_json in
  if String.length body > 8_388_608 then
    raise (Provider_error "conversation request exceeds 8 MiB; start a new session");
  body

let curl_options ~local ~endpoint ~headers ~body_path =
  let option name value = name ^ " = " ^ quote_config value ^ "\n" in
  "silent\n"
  ^ option "url" endpoint
  ^ option "request" "POST"
  ^ option "header" "Content-Type: application/json"
  ^ String.concat "" (List.map (option "header") headers)
  ^ option "data-binary" ("@" ^ body_path)
  ^ option "connect-timeout" "10"
  ^ option "max-time" "120"
  ^ option "proto" (if String.starts_with ~prefix:"https://" endpoint
    then "=https" else "=http")
  ^ (if local then option "proxy" "" ^ option "noproxy" "*" ^
      option "max-redirs" "0" else "")

let post_json ?(local = false) ?cancel ~endpoint ~headers ~secret body_json =
  let body = request_body ~local ~endpoint ~headers body_json in
  with_temp_file (fun body_path body_output ->
    output_string body_output body;
    close_out body_output;
    with_temp_file (fun response_path response_output ->
      close_out response_output;
      let option name value = name ^ " = " ^ quote_config value ^ "\n" in
      let configuration =
        curl_options ~local ~endpoint ~headers ~body_path
        ^ option "output" response_path
        ^ option "write-out" "%{http_code}" in
      let status = run_curl ?cancel configuration in
      check_cancel cancel;
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

let post_stream ?(local = false) ?cancel ~endpoint ~headers ~secret body_json ~on_chunk ~is_done ~is_finished =
  let body = request_body ~local ~endpoint ~headers body_json in
  with_temp_file (fun body_path body_output ->
    output_string body_output body;
    close_out body_output;
    with_temp_file (fun header_path header_output ->
      close_out header_output;
      let option name value = name ^ " = " ^ quote_config value ^ "\n" in
      let configuration =
        curl_options ~local ~endpoint ~headers ~body_path
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
      (try ignore (run_curl ~on_chunk:consume ~is_done ~is_finished ?cancel configuration)
       with Stream_complete -> ());
      check_cancel cancel;
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

let complete ?(authentication = Api_key) ?resolve_credential ?on_text ?on_usage ?cancel
    config messages tools =
  check_cancel cancel;
  let config = if config.api = Local_chat then
    { config with endpoint = local_endpoint config.endpoint } else config in
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
  let parse f =
    try f () with
    | Protocol.Invalid_response message ->
        raise (Provider_error ("invalid completion response: " ^ redact api_key message)) in
  let result = match config.api with
  | Openai_completions | Local_chat | Copilot_chat ->
      let fields = [ "model", `String config.model;
                     "messages", Protocol.chat_messages_to_json messages ] in
      let fields = if tools = [] then fields else fields @ [ "tools", `List tools ] in
      if config.api = Local_chat &&
         (String.length api_key > 8192 ||
          not (String.for_all (fun c -> Char.code c > 32 &&
            Char.code c < 127) api_key))
      then raise (Provider_error "invalid local API key");
      let headers = match config.api with
        | Copilot_chat ->
            (try Github_copilot_wire.headers ~endpoint:config.endpoint
               ~model:config.model ~token:api_key ~messages
             with Invalid_argument message -> raise (Provider_error message))
        | _ -> if api_key = "" then [] else
            [ "Authorization: Bearer " ^ api_key ] in
      (match on_text with
      | None ->
          let json = post_json ~local:(config.api = Local_chat) ?cancel
            ~endpoint:config.endpoint ~headers ~secret:api_key
            (`Assoc fields) in
          let reply = parse (fun () -> Protocol.parse_completion json) in
          (match on_usage with
           | None -> ()
           | Some report ->
               check_cancel cancel;
               Option.iter report (Protocol.completion_usage json));
          reply
      | Some emit ->
          let stream = Openai_stream.create ~on_text:emit in
          let fields = fields @ [ "stream", `Bool true ] in
          let fields = if config.api = Openai_completions &&
            config.endpoint = "https://api.openai.com/v1/chat/completions" then
              fields @ [ "stream_options", `Assoc [ "include_usage", `Bool true ] ]
            else fields in
          let body = `Assoc fields in
          parse (fun () ->
            post_stream ~local:(config.api = Local_chat) ?cancel
              ~endpoint:config.endpoint ~headers ~secret:api_key
              body ~on_chunk:(Openai_stream.feed stream)
              ~is_done:(fun () -> Openai_stream.is_done stream)
              ~is_finished:(fun () -> Openai_stream.is_finished stream);
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
  | Commandcode_chat ->
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
            Alibaba_coding_api.request ~model:config.model messages tools,
            Alibaba_coding_api.parse_completion
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
        Commandcode_api.messages_request ~model:config.model ~max_tokens:4096
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
        | Error Devin_api.Transport_error ->
            raise (Provider_error "Devin model discovery transport failed")
        | Error (Devin_api.Http_error status) ->
            raise (Provider_error (Printf.sprintf "Devin model discovery HTTP %d" status))
        | Error (Devin_api.Invalid_response reason) ->
            raise (Provider_error reason))
        with Devin_binary_http.Cancelled -> raise Cancelled in
      let selected = match List.find_opt
        (fun (entry : Devin_api.model) -> entry.id = config.model) models with
        | Some model -> model
        | None -> raise (Provider_error "Devin model is not in this account's live roster") in
      if tools <> [] && not selected.supports_tools then
        raise (Provider_error "selected Devin model does not support tools");
      let cascade_id = try Devin_api.cascade_id messages
        with Devin_api.Bad_wire reason -> raise (Provider_error reason) in
      let reply, usage = try (match Devin_api.complete ?cancel ~api_key
        ~model:selected.id ~cascade_id ~router:selected.router
        ~max_tokens:selected.max_tokens
        ~supports_parallel_tool_calls:selected.supports_parallel_tool_calls
        messages tools with
        | Ok completion -> completion
        | Error Devin_api.Invalid_credential ->
            raise (Provider_error "invalid Devin session credential")
        | Error Devin_api.Transport_error ->
            raise (Provider_error "Devin Connect transport failed")
        | Error (Devin_api.Http_error status) ->
            raise (Provider_error (Printf.sprintf "Devin Connect HTTP %d" status))
        | Error (Devin_api.Invalid_response reason) ->
            raise (Provider_error reason))
        with Devin_binary_http.Cancelled -> raise Cancelled in
      (match on_usage, usage with
       | Some report, Some (input_tokens, output_tokens) ->
           check_cancel cancel;
           report { Protocol.input_tokens = input_tokens; output_tokens }
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
        ~max_tokens:4096 messages tools) in
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
          let json = post_json ?cancel ~endpoint:config.endpoint ~headers ~secret:api_key body in
          let reply = parse (fun () -> Anthropic_wire.parse_response json) in
          (match on_usage with
           | None -> ()
           | Some report ->
               check_cancel cancel;
               Option.iter report (Anthropic_wire.usage json));
          reply
      | Some emit ->
          let stream = Anthropic_stream.create ~on_text:emit in
          let body = match body with
            | `Assoc fields -> `Assoc (fields @ [ "stream", `Bool true ])
            | _ -> assert false in
          parse (fun () ->
            post_stream ?cancel ~endpoint:config.endpoint ~headers ~secret:api_key
              body ~on_chunk:(Anthropic_stream.feed stream)
              ~is_done:(fun () -> Anthropic_stream.is_done stream)
              ~is_finished:(fun () -> Anthropic_stream.is_finished stream);
            let reply = Anthropic_stream.finish stream in
            (match on_usage with
             | None -> ()
             | Some report ->
                 check_cancel cancel;
                 Option.iter report (Anthropic_stream.usage stream));
            reply))
  | Openai_responses | Azure_responses | Bedrock_mantle_responses ->
      let endpoint, headers =
        if config.api = Azure_responses then
          (try Azure_wire.resolve ~endpoint:config.endpoint
             ~deployment:config.model ~api_key
           with Invalid_argument message -> raise (Provider_error message))
        else if config.api = Bedrock_mantle_responses then
          (try
             let requested = if config.endpoint = "" then
               (Bedrock_mantle.endpoint ~region:(Bedrock_mantle.region ()) ()).url
               else config.endpoint in
             Bedrock_mantle.resolve ~endpoint:requested ~api_key ()
           with Invalid_argument message -> raise (Provider_error message))
        else config.endpoint,
          (if api_key = "" then [] else [ "Authorization: Bearer " ^ api_key ]) in
      let body = parse (fun () ->
        Openai_responses_wire.request ~model:config.model messages tools) in
      (match on_text with
      | None ->
          let json = post_json ?cancel ~endpoint ~headers ~secret:api_key body in
          let reply = parse (fun () -> Openai_responses_wire.parse_completion json) in
          (match on_usage with
           | None -> ()
           | Some report ->
               check_cancel cancel;
               Option.iter report (Openai_responses_wire.usage json));
          reply
      | Some emit ->
          let stream = Openai_responses_stream.create ~on_text:emit in
          let body = parse (fun () ->
            Openai_responses_wire.request ~stream:true
              ~model:config.model messages tools) in
          parse (fun () ->
            post_stream ?cancel ~endpoint ~headers ~secret:api_key
              body ~on_chunk:(Openai_responses_stream.feed stream)
              ~is_done:(fun () -> Openai_responses_stream.is_done stream)
              ~is_finished:(fun () -> Openai_responses_stream.is_finished stream);
            let reply = Openai_responses_stream.finish stream in
            (match on_usage with
             | None -> ()
             | Some report ->
                 check_cancel cancel;
                 Option.iter report (Openai_responses_stream.usage stream));
            reply))
  | Bedrock_converse ->
      if authentication <> Api_key || api_key <> "" then
        raise (Provider_error "Bedrock Converse uses AWS credentials, not an API key");
      let region, keys =
        try Aws_auth.region (), Aws_auth.resolve ()
        with Invalid_argument reason -> raise (Provider_error reason) in
      let target = try
        Bedrock_wire.endpoint ~region ~model:config.model
          ?base_url:(if config.endpoint = "" then None else Some config.endpoint) ()
        with Invalid_argument reason -> raise (Provider_error reason) in
      let body = parse (fun () -> Bedrock_wire.request messages tools) in
      let serialized = Yojson.Basic.to_string body in
      let signed = try
        Aws_auth.sign ~credentials:keys ~region
          ~amz_date:(Aws_auth.amz_date ()) ~method_:"POST"
          ~host:target.host ~path:target.path ~body:serialized ()
        with Invalid_argument reason -> raise (Provider_error reason) in
      let headers = List.filter_map (fun (name, value) ->
        if name = "content-type" then None else Some (name ^ ": " ^ value)) signed in
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
        Ollama_wire.request ~model:config.model messages tools) in
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
          let stream = Ollama_stream.create ~on_text:emit in
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
        Gemini_wire.request ~model:config.model messages tools) in
      let headers = [ "x-goog-api-key: " ^ api_key ] in
      let base = if String.ends_with ~suffix:"/" config.endpoint then
        String.sub config.endpoint 0 (String.length config.endpoint - 1)
        else config.endpoint in
      let model_path = gemini_model_path config.model in
      (match on_text with
      | None ->
          let endpoint = base ^ "/" ^ model_path ^ ":generateContent" in
          let json = post_json ?cancel ~endpoint ~headers ~secret:api_key body in
          let reply = parse (fun () -> Gemini_wire.parse_completion ~model:config.model json) in
          (match on_usage with
           | None -> ()
           | Some report ->
               check_cancel cancel;
               Option.iter report (Gemini_wire.usage json));
          reply
      | Some emit ->
          let stream = Gemini_stream.create ~model:config.model ~on_text:emit in
          let endpoint = base ^ "/" ^ model_path ^ ":streamGenerateContent?alt=sse" in
          parse (fun () ->
            post_stream ?cancel ~endpoint ~headers ~secret:api_key
              body ~on_chunk:(Gemini_stream.feed stream)
              ~is_done:(fun () -> Gemini_stream.is_done stream)
              ~is_finished:(fun () -> Gemini_stream.is_finished stream);
            let reply = Gemini_stream.finish stream in
            (match on_usage with
             | None -> ()
             | Some report ->
                 check_cancel cancel;
                 Option.iter report (Gemini_stream.usage stream));
            reply))
  | Vertex_generate ->
      if authentication <> Api_key || config.endpoint <> "" || api_key <> "" then
        raise (Provider_error "Vertex uses scoped Google ADC and a derived Google endpoint");
      let project, location, access =
        try
          let project, location = Vertex_wire.resolve_environment () in
          project, location, Vertex_auth.access_token ()
        with
        | Invalid_argument reason -> raise (Provider_error reason)
        | Vertex_auth.Authentication_error reason -> raise (Provider_error reason) in
      let endpoint =
        try Vertex_wire.endpoint ~project ~location ~model:config.model
        with Invalid_argument reason -> raise (Provider_error reason) in
      let body = parse (fun () -> Vertex_wire.request ~model:config.model messages tools) in
      let emit = Option.value ~default:(fun _ -> ()) on_text in
      let stream = Gemini_stream.create ~model:config.model ~on_text:emit in
      parse (fun () ->
        post_stream ?cancel ~endpoint
          ~headers:["Authorization: Bearer " ^ access] ~secret:access
          body ~on_chunk:(Gemini_stream.feed stream)
          ~is_done:(fun () -> Gemini_stream.is_done stream)
          ~is_finished:(fun () -> Gemini_stream.is_finished stream);
        let reply = Vertex_wire.finish_stream ~model:config.model stream in
        (match on_usage with
        | None -> ()
        | Some report ->
            check_cancel cancel;
            Option.iter report (Gemini_stream.usage stream));
        reply)
  | Codex_responses ->
      let account_id = match credential.account_id with
        | Some id when id <> "" -> id
        | _ -> raise (Provider_error "Codex OAuth account ID unavailable") in
      reject_controls "account ID" account_id;
      reject_controls "model" config.model;
      let headers = [
        "Authorization: Bearer " ^ api_key;
        "chatgpt-account-id: " ^ account_id;
        "OpenAI-Beta: responses=experimental";
        "originator: pave";
        "version: " ^ Codex_wire.client_version;
        "x-codex-routing-hint: model=" ^ config.model;
        "Accept: text/event-stream" ] @
        (match credential.residency with
         | None -> []
         | Some residency ->
             reject_controls "Codex residency" residency;
             [ "x-openai-internal-codex-residency: " ^ residency ]) in
      let body = parse (fun () ->
        Codex_wire.request ~model:config.model messages tools) in
      let emit = match on_text with Some emit -> emit | None -> fun _ -> () in
      let stream = Codex_stream.create ~model:config.model ~on_text:emit in
      parse (fun () ->
        post_stream ?cancel ~endpoint:config.endpoint ~headers ~secret:api_key
          body ~on_chunk:(Codex_stream.feed stream)
          ~is_done:(fun () -> Codex_stream.is_done stream)
          ~is_finished:(fun () -> Codex_stream.is_finished stream);
        let reply = Codex_stream.finish stream in
        (match on_usage with
         | None -> ()
         | Some report ->
             check_cancel cancel;
             Option.iter report (Codex_stream.usage stream));
        reply)
  in
  check_cancel cancel;
  result
