type error =
  | Invalid_input of string
  | Missing_credential of string
  | Invalid_credential of string
  | Transport_error
  | Http_error of int
  | Invalid_response of string
  | Cancelled

type request = {
  method_ : string;
  url : string;
  headers : (string * string) list;
  body : string;
}
type http = request -> (int * string, error) result

let openai_base = "https://api.openai.com/v1"
let cohere_rerank_url = "https://api.cohere.com/v2/rerank"
let embeddings_url = openai_base ^ "/embeddings"
let images_url = openai_base ^ "/images/generations"
let speech_url = openai_base ^ "/audio/speech"
let transcription_url = openai_base ^ "/audio/transcriptions"
let max_request_bytes = 1_048_576
let max_json_response_bytes = 4 * 1_048_576
let max_image_response_bytes = 24 * 1_048_576
let max_output_bytes = 16 * 1_048_576
let max_audio_input_bytes = 25_000_000
let max_audio_response_bytes = 20 * 1_048_576

let message = function
  | Invalid_input text -> text
  | Missing_credential name -> "Set " ^ name ^ " to use this task"
  | Invalid_credential name -> "Invalid " ^ name ^ " value"
  | Transport_error -> "Task request failed to connect to the pinned provider endpoint"
  | Http_error 401 -> "Provider rejected the API key (HTTP 401)"
  | Http_error 403 -> "Provider denied this operation or model entitlement (HTTP 403)"
  | Http_error 429 -> "Provider rate limited this task (HTTP 429)"
  | Http_error status -> Printf.sprintf "Provider returned HTTP %d" status
  | Invalid_response text -> "Invalid provider response: " ^ text
  | Cancelled -> "Task cancelled"

exception Task_error of error
let fail error = raise (Task_error error)
let invalid text = fail (Invalid_input text)
let invalid_response text = fail (Invalid_response text)

let protect f =
  try Ok (f ()) with
  | Task_error error -> Error error
  | Provider.Cancelled -> Error Cancelled
  | Unix.Unix_error _ | Sys_error _ -> Error (Invalid_input "workspace file operation failed")
  | Yojson.Json_error _ -> Error (Invalid_response "malformed JSON")
  | Failure _ | Invalid_argument _ -> Error (Invalid_response "invalid task data")

let check_cancel cancel =
  match cancel with
  | Some is_cancelled when is_cancelled () -> raise Provider.Cancelled
  | _ -> ()

let valid_text_field ~label ~max_bytes value =
  if value = "" then invalid (label ^ " must not be empty");
  if String.length value > max_bytes then
    invalid (Printf.sprintf "%s exceeds the %d-byte limit" label max_bytes);
  if String.exists (fun c -> Char.code c = 0) value then
    invalid (label ^ " contains a NUL byte");
  value

let valid_model model =
  if model = "" || String.length model > 256 ||
     String.exists (fun c -> Char.code c < 32 || Char.code c = 127) model then
    invalid "--model must be a non-empty exact model ID (at most 256 bytes)";
  model

let valid_key ~name key =
  if key = "" then fail (Missing_credential name);
  if String.length key > 8192 ||
     String.exists (fun c -> Char.code c <= 32 || Char.code c >= 127) key then
    fail (Invalid_credential name);
  key

let field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let required name json = match field name json with
  | Some value -> value
  | None -> invalid_response ("missing " ^ name)

let string_field name json = match required name json with
  | `String value -> value
  | _ -> invalid_response (name ^ " must be a string")

let int_value : Yojson.Basic.t -> int option = function
  | `Int value when value >= 0 -> Some value
  | _ -> None

let nonnegative_int name json = match required name json with
  | value -> (match int_value value with
      | Some n -> n
      | None -> invalid_response (name ^ " must be a nonnegative integer"))

let finite_float : Yojson.Basic.t -> float option = function
  | `Int n -> Some (float_of_int n)
  | `Float f when classify_float f <> FP_nan && classify_float f <> FP_infinite -> Some f
  | _ -> None

let finite_number name value = match finite_float value with
  | Some number -> number
  | None -> invalid_response (name ^ " must be a finite number")

let rec reject_duplicate_keys = function
  | `Assoc fields ->
      let keys = Hashtbl.create (List.length fields) in
      List.iter (fun (key, value) ->
        if Hashtbl.mem keys key then invalid_response "duplicate JSON object key";
        Hashtbl.add keys key ();
        reject_duplicate_keys value) fields
  | `List values -> List.iter reject_duplicate_keys values
  | _ -> ()

let parse_json body =
  let json = Yojson.Basic.from_string body in
  reject_duplicate_keys json;
  json


let with_temp_body body f =
  let path, oc = Filename.open_temp_file ~mode:[Open_binary] "pave-task-" ".request" in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc; try Sys.remove path with Sys_error _ -> ())
    (fun () -> output_string oc body; flush oc; f path)

let default_http ?cancel ~response_limit request =
  if request.url <> embeddings_url && request.url <> images_url &&
     request.url <> speech_url && request.url <> transcription_url &&
     request.url <> cohere_rerank_url then Error Transport_error
  else if request.method_ <> "POST" then Error Transport_error
  else if String.length request.body > max_request_bytes + max_audio_input_bytes + 65536 then
    Error (Invalid_input "request exceeds the size limit")
  else
    try
      check_cancel cancel;
      with_temp_body request.body (fun body_path ->
        let option name value = name ^ " = " ^ Provider.quote_config value ^ "\n" in
        let config =
          "silent\n"
          ^ option "url" request.url
          ^ option "request" "POST"
          ^ option "write-out" "%{http_code}"
          ^ option "connect-timeout" "5"
          ^ option "max-time" "90"
          ^ option "max-filesize" (string_of_int response_limit)
          ^ option "proto" "=https"
          ^ option "proxy" ""
          ^ option "max-redirs" "0"
          ^ option "data-binary" ("@" ^ body_path)
          ^ String.concat "" (List.map (fun (name, value) ->
              option "header" (name ^ ": " ^ value)) request.headers) in
        let received = Buffer.create (min response_limit 65536) in
        let consume chunk =
          Buffer.add_string received chunk;
          if Buffer.length received > response_limit + 3 then
            invalid_response "response exceeds size limit" in
        (try ignore (Provider.run_curl ?cancel ~on_chunk:consume config)
         with Provider.Cancelled -> raise Provider.Cancelled
            | Provider.Provider_error _ -> raise (Task_error Transport_error));
        let output = Buffer.contents received in
        let length = String.length output in
        if length < 3 then Error Transport_error
        else
          let status = String.sub output (length - 3) 3 in
          let code = try int_of_string status with Failure _ -> 0 in
          let body_length = length - 3 in
          if code < 100 || code > 599 then Error Transport_error
          else if body_length > response_limit then
            Error (Invalid_response "response exceeds size limit")
          else Ok (code, String.sub output 0 body_length))
    with
    | Task_error error -> Error error
    | Provider.Cancelled -> Error Cancelled
    | Unix.Unix_error _ | Sys_error _ -> Error Transport_error

let perform ?http ?cancel ~key ~key_name ~url ~content_type ~body ~response_limit () =
  let key = valid_key ~name:key_name key in
  if String.length body > max_request_bytes + max_audio_input_bytes + 65536 then
    invalid "request exceeds the size limit";
  let request = {
    method_ = "POST"; url;
    headers = ["Authorization", "Bearer " ^ key; "Content-Type", content_type];
    body;
  } in
  check_cancel cancel;
  let send = match http with
    | Some send -> send
    | None -> fun req -> default_http ?cancel ~response_limit req in
  match send request with
  | Error Cancelled -> raise Provider.Cancelled
  | Error error -> fail error
  | Ok (status, _) when status < 200 || status >= 300 -> fail (Http_error status)
  | Ok (_, body) when String.length body > response_limit ->
      invalid_response "response exceeds size limit"
  | Ok (_, body) -> body

let json_request ?http ?cancel ?(response_limit = max_json_response_bytes)
    ~key ~key_name ~url json =
  let body = Yojson.Basic.to_string json in
  let body = perform ?http ?cancel ~key ~key_name ~url
      ~content_type:"application/json" ~body ~response_limit () in
  parse_json body

let workspace root =
  let root = try Unix.realpath root with Unix.Unix_error _ -> invalid "workspace root is unavailable" in
  let stat = Unix.stat root in
  if stat.Unix.st_kind <> Unix.S_DIR then invalid "workspace root must be a directory";
  root

let relative_components path =
  if path = "" || not (Filename.is_relative path) || String.contains path '\000' then
    invalid "file paths must be workspace-relative";
  let components = String.split_on_char '/' path in
  if List.exists (fun part -> part = "" || part = "." || part = "..") components then
    invalid "file path must not contain empty, dot, or parent components";
  if String.contains path '\\' then invalid "file paths must use workspace-relative '/' separators";
  if String.length path > 4096 || List.exists (fun part -> String.length part > 255) components then
    invalid "file path exceeds the path-length limit";
  components

let check_parent_components root components =
  let rec walk current = function
    | [] -> current
    | part :: rest ->
        let next = Filename.concat current part in
        let stat = try Unix.lstat next with Unix.Unix_error (Unix.ENOENT, _, _) ->
          invalid "workspace path parent does not exist" in
        if stat.Unix.st_kind <> Unix.S_DIR then
          invalid "workspace path parents must be real directories, not symlinks";
        walk next rest in
  walk root components

let read_workspace_file ~root ~path ~max_bytes =
  let root = workspace root in
  let components = relative_components path in
  let basename = List.hd (List.rev components) in
  let parent = check_parent_components root (List.rev (List.tl (List.rev components))) in
  let full_path = Filename.concat parent basename in
  let stat = try Unix.lstat full_path with Unix.Unix_error _ -> invalid "input file does not exist" in
  if stat.Unix.st_kind <> Unix.S_REG then invalid "input path must be a regular file, not a symlink";
  if stat.Unix.st_size <= 0 || stat.Unix.st_size > max_bytes then
    invalid (Printf.sprintf "input file must be 1..%d bytes" max_bytes);
  let ic = open_in_bin full_path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    let opened = Unix.fstat (Unix.descr_of_in_channel ic) in
    if opened.Unix.st_kind <> Unix.S_REG || opened.Unix.st_dev <> stat.Unix.st_dev ||
       opened.Unix.st_ino <> stat.Unix.st_ino then
      invalid "input file changed while it was being opened";
    let actual = in_channel_length ic in
    if actual <= 0 || actual > max_bytes then
      invalid (Printf.sprintf "input file must be 1..%d bytes" max_bytes);
    (basename, really_input_string ic actual))
let check_output_path ~root ~path ~extension =
  let root = workspace root in
  let components = relative_components path in
  let basename = List.hd (List.rev components) in
  if Filename.extension basename <> extension then
    invalid ("output path must end in " ^ extension);
  let parent = check_parent_components root (List.rev (List.tl (List.rev components))) in
  let destination = Filename.concat parent basename in
  (try ignore (Unix.lstat destination); invalid "output file already exists; choose a new path"
   with Unix.Unix_error (Unix.ENOENT, _, _) -> ())

let atomic_output ~root ~path ~extension ~bytes ~max_bytes =
  let root = workspace root in
  let components = relative_components path in
  let basename = List.hd (List.rev components) in
  if Filename.extension basename <> extension then
    invalid ("output path must end in " ^ extension);
  let parent = check_parent_components root (List.rev (List.tl (List.rev components))) in
  let destination = Filename.concat parent basename in
  (try ignore (Unix.lstat destination); invalid "output file already exists; choose a new path"
   with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  if String.length bytes <= 0 || String.length bytes > max_bytes then
    invalid "media output has an invalid size";
  let temp, oc = Filename.open_temp_file ~mode:[Open_binary] ~perms:0o600
      ~temp_dir:parent "pave-task-output-" ".tmp" in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc; try Sys.remove temp with Sys_error _ -> ())
    (fun () ->
      output_string oc bytes;
      flush oc;
      Unix.fsync (Unix.descr_of_out_channel oc);
      close_out oc;
      (try Unix.link temp destination
       with Unix.Unix_error (Unix.EEXIST, _, _) -> invalid "output file already exists; choose a new path");
      Unix.unlink temp)

let ensure_json_result json = Yojson.Basic.to_string json ^ "\n"

let embed ?http ?cancel ~key ~model ~input () = protect (fun () ->
  let model = valid_model model in
  let input = valid_text_field ~label:"--input" ~max_bytes:(256 * 1024) input in
  let json = json_request ?http ?cancel ~key ~key_name:"OPENAI_API_KEY"
      ~url:embeddings_url (`Assoc [
        "model", `String model; "input", `String input; "encoding_format", `String "float"] ) in
  if string_field "object" json <> "list" then invalid_response "object must be list";
  if string_field "model" json <> model then invalid_response "response model does not match requested model";
  let usage = required "usage" json in
  let prompt_tokens = nonnegative_int "prompt_tokens" usage in
  let total_tokens = nonnegative_int "total_tokens" usage in
  if total_tokens < prompt_tokens then invalid_response "usage totals are inconsistent";
  let data = match required "data" json with `List rows -> rows | _ -> invalid_response "data must be an array" in
  if List.length data <> 1 then invalid_response "expected exactly one embedding";
  let row = List.hd data in
  if string_field "object" row <> "embedding" then invalid_response "embedding object type is invalid";
  if nonnegative_int "index" row <> 0 then invalid_response "embedding index is invalid";
  let vector = match required "embedding" row with `List values -> values | _ -> invalid_response "embedding must be an array" in
  if vector = [] || List.length vector > 16_384 then invalid_response "embedding dimensions are invalid";
  List.iter (fun value -> ignore (finite_number "embedding value" value)) vector;
  ensure_json_result json)

let base64_value = function
  | 'A'..'Z' as c -> Char.code c - Char.code 'A'
  | 'a'..'z' as c -> Char.code c - Char.code 'a' + 26
  | '0'..'9' as c -> Char.code c - Char.code '0' + 52
  | '+' -> 62 | '/' -> 63 | _ -> -1

let decode_base64 ~max_bytes encoded =
  let length = String.length encoded in
  if length = 0 || length mod 4 <> 0 || length > ((max_bytes + 2) / 3 * 4) then
    invalid_response "image base64 is empty, malformed, or exceeds the media limit";
  let output = Buffer.create (min max_bytes (length / 4 * 3)) in
  let groups = length / 4 in
  for group = 0 to groups - 1 do
    let offset = group * 4 in
    let last = group = groups - 1 in
    let a = base64_value encoded.[offset] and b = base64_value encoded.[offset + 1] in
    let c = if encoded.[offset + 2] = '=' then -2 else base64_value encoded.[offset + 2] in
    let d = if encoded.[offset + 3] = '=' then -2 else base64_value encoded.[offset + 3] in
    if a < 0 || b < 0 || c = -1 || d = -1 || (not last && (c = -2 || d = -2)) ||
       (c = -2 && d <> -2) || (c = -2 && b land 15 <> 0) ||
       (d = -2 && c >= 0 && c land 3 <> 0) then
      invalid_response "image base64 is malformed";
    let n = (a lsl 18) lor (b lsl 12) lor ((max 0 c) lsl 6) lor max 0 d in
    Buffer.add_char output (Char.chr ((n lsr 16) land 255));
    if c <> -2 then Buffer.add_char output (Char.chr ((n lsr 8) land 255));
    if d <> -2 then Buffer.add_char output (Char.chr (n land 255))
  done;
  if Buffer.length output = 0 || Buffer.length output > max_bytes then
    invalid_response "image media exceeds the size limit";
  Buffer.contents output

let png_u32 bytes offset =
  (Char.code bytes.[offset] lsl 24) lor
  (Char.code bytes.[offset + 1] lsl 16) lor
  (Char.code bytes.[offset + 2] lsl 8) lor
  Char.code bytes.[offset + 3]

let png_crc32 bytes offset length =
  let crc = ref 0xffffffffl in
  for index = offset to offset + length - 1 do
    crc := Int32.logxor !crc (Int32.of_int (Char.code bytes.[index]));
    for _ = 0 to 7 do
      crc := if Int32.logand !crc 1l <> 0l then
        Int32.logxor (Int32.shift_right_logical !crc 1) 0xedb88320l
      else Int32.shift_right_logical !crc 1
    done
  done;
  Int32.lognot !crc

let validate_png bytes =
  let length = String.length bytes in
  if length < 8 || String.sub bytes 0 8 <> "\137PNG\r\n\026\n" then
    invalid_response "image is not a PNG";
  let rec chunks offset seen_header seen_data seen_palette idat_ended =
    if offset + 12 > length then false
    else
      let chunk_length = png_u32 bytes offset in
      if chunk_length > length - offset - 12 then false
      else
        let kind = String.sub bytes (offset + 4) 4 in
        let crc_offset = offset + 8 + chunk_length in
        let crc_valid =
          png_crc32 bytes (offset + 4) (4 + chunk_length) =
          Int32.of_int (png_u32 bytes crc_offset) in
        if not crc_valid then false
        else
          let next = crc_offset + 4 in
          match kind with
          | "IHDR" ->
              if offset <> 8 || seen_header || chunk_length <> 13 then false
              else
                let width = png_u32 bytes (offset + 8) in
                let height = png_u32 bytes (offset + 12) in
                let depth = Char.code bytes.[offset + 16] in
                let color_type = Char.code bytes.[offset + 17] in
                let depth_valid = match color_type with
                  | 0 -> List.mem depth [1; 2; 4; 8; 16]
                  | 2 -> depth = 8 || depth = 16
                  | 3 -> List.mem depth [1; 2; 4; 8]
                  | 4 | 6 -> depth = 8 || depth = 16
                  | _ -> false in
                let methods_valid =
                  bytes.[offset + 18] = '\000' && bytes.[offset + 19] = '\000' &&
                  (bytes.[offset + 20] = '\000' || bytes.[offset + 20] = '\001') in
                if width <= 0 || height <= 0 || width > 4096 || height > 4096 ||
                   width * height > 4_194_304 || not depth_valid || not methods_valid
                then false
                else chunks next true seen_data seen_palette idat_ended
          | "PLTE" ->
              if not seen_header || seen_data || seen_palette ||
                 chunk_length < 3 || chunk_length > 768 || chunk_length mod 3 <> 0
              then false
              else chunks next seen_header seen_data true idat_ended
          | "IDAT" ->
              if not seen_header || idat_ended then false
              else chunks next seen_header (seen_data || chunk_length > 0) seen_palette false
          | "IEND" -> chunk_length = 0 && seen_header && seen_data && next = length
          | _ ->
              let critical = kind.[0] >= 'A' && kind.[0] <= 'Z' in
              if not seen_header || critical then false
              else chunks next seen_header seen_data seen_palette (idat_ended || seen_data) in
  if not (chunks 8 false false false false) then
    invalid_response "image is not a complete, well-formed PNG"

let generate_image ?http ?cancel ~key ~model ~root ~prompt ~output () = protect (fun () ->
  let model = valid_model model in
  let prompt = valid_text_field ~label:"--prompt" ~max_bytes:(16 * 1024) prompt in
  check_output_path ~root ~path:output ~extension:".png";
  let json = json_request ?http ?cancel ~response_limit:max_image_response_bytes
      ~key ~key_name:"OPENAI_API_KEY" ~url:images_url
      (`Assoc ["model", `String model; "prompt", `String prompt;
        "n", `Int 1; "size", `String "1024x1024"; "output_format", `String "png"]) in
  ignore (nonnegative_int "created" json);
  let rows = match required "data" json with `List rows -> rows | _ -> invalid_response "data must be an array" in
  if List.length rows <> 1 then invalid_response "expected exactly one generated image";
  let image = decode_base64 ~max_bytes:max_output_bytes (string_field "b64_json" (List.hd rows)) in
  validate_png image;
  atomic_output ~root ~path:output ~extension:".png" ~bytes:image ~max_bytes:max_output_bytes;
  "Wrote " ^ output ^ "\n")

let valid_wav bytes =
  let length = String.length bytes in
  if length < 44 || String.sub bytes 0 4 <> "RIFF" || String.sub bytes 8 4 <> "WAVE" then false
  else
    let u32 offset =
      Char.code bytes.[offset] lor (Char.code bytes.[offset + 1] lsl 8) lor
      (Char.code bytes.[offset + 2] lsl 16) lor (Char.code bytes.[offset + 3] lsl 24) in
    let u16 offset = Char.code bytes.[offset] lor (Char.code bytes.[offset + 1] lsl 8) in
    let declared = u32 4 in
    if declared < 36 || declared + 8 <> length then false
    else
      let rec chunks offset has_format has_data =
        if offset = length then has_format && has_data
        else if offset + 8 > length then false
        else
          let size = u32 (offset + 4) in
          let next = offset + 8 + size + (size land 1) in
          if next > length then false
          else
            let tag = String.sub bytes offset 4 in
            let format_valid = tag = "fmt " && size >= 16 &&
              u16 (offset + 8 + 2) > 0 && u32 (offset + 8 + 4) > 0 &&
              u32 (offset + 8 + 8) > 0 && u16 (offset + 8 + 14) > 0 in
            if (tag = "fmt " && (has_format || not format_valid)) ||
               (tag = "data" && not has_format) then false
            else
              let has_format = has_format || format_valid in
              let has_data = has_data || (tag = "data" && size > 0) in
              chunks next has_format has_data in
      chunks 12 false false

let utf8_character_count text =
  let length = String.length text in
  let byte index = Char.code text.[index] in
  let continuation index = index < length && (byte index land 0xc0) = 0x80 in
  let rec count index total =
    if total > 4096 then Some total
    else if index = length then Some total
    else
      let first = byte index in
      if first <= 0x7f then count (index + 1) (total + 1)
      else if first >= 0xc2 && first <= 0xdf && continuation (index + 1) then
        count (index + 2) (total + 1)
      else if first >= 0xe0 && first <= 0xef &&
              continuation (index + 1) && continuation (index + 2) &&
              (let second = byte (index + 1) in
               (first <> 0xe0 || second >= 0xa0) &&
               (first <> 0xed || second <= 0x9f)) then
        count (index + 3) (total + 1)
      else if first >= 0xf0 && first <= 0xf4 &&
              continuation (index + 1) && continuation (index + 2) &&
              continuation (index + 3) &&
              (let second = byte (index + 1) in
               (first <> 0xf0 || second >= 0x90) &&
               (first <> 0xf4 || second <= 0x8f)) then
        count (index + 4) (total + 1)
      else None in
  count 0 0

let valid_speech_input input =
  let input = valid_text_field ~label:"--input" ~max_bytes:(4 * 4096) input in
  match utf8_character_count input with
  | Some count when count <= 4096 -> input
  | Some _ -> invalid "--input exceeds the 4096-character limit"
  | None -> invalid "--input must be valid UTF-8 text"

let speak ?http ?cancel ~key ~model ~root ~input ~voice ~output () = protect (fun () ->
  let model = valid_model model in
  let input = valid_speech_input input in
  let voice = match voice with
    | "alloy" | "ash" | "ballad" | "coral" | "echo" | "fable" | "onyx" |
      "nova" | "sage" | "shimmer" | "verse" | "marin" | "cedar" -> voice
    | _ -> invalid "--voice must be a documented OpenAI built-in voice" in
  check_output_path ~root ~path:output ~extension:".wav";
  let body = Yojson.Basic.to_string (`Assoc ["model", `String model;
    "input", `String input; "voice", `String voice; "response_format", `String "wav"]) in
  let audio = perform ?http ?cancel ~key ~key_name:"OPENAI_API_KEY" ~url:speech_url
      ~content_type:"application/json" ~body ~response_limit:max_audio_response_bytes () in
  if not (valid_wav audio) then invalid_response "speech response is not a complete WAV file";
  atomic_output ~root ~path:output ~extension:".wav" ~bytes:audio ~max_bytes:max_audio_response_bytes;
  "Wrote " ^ output ^ "\n")

let contains_substring text substring =
  let text_length = String.length text and substring_length = String.length substring in
  let rec search index =
    index + substring_length <= text_length &&
    (String.sub text index substring_length = substring || search (index + 1)) in
  search 0

let choose_multipart_boundary audio model =
  let rec choose index =
    let boundary = "pave-task-4F936E2A-" ^ string_of_int index in
    if contains_substring audio boundary || contains_substring model boundary then
      if index < 4096 then choose (index + 1)
      else invalid "could not select a safe multipart boundary"
    else boundary in
  choose 0

let multipart_field boundary name value =
  "--" ^ boundary ^ "\r\nContent-Disposition: form-data; name=\"" ^ name ^ "\"\r\n\r\n" ^
  value ^ "\r\n"

let audio_type basename =
  match String.lowercase_ascii (Filename.extension basename) with
  | ".wav" -> Some "audio/wav"
  | ".mp3" | ".mpga" | ".mpeg" -> Some "audio/mpeg"
  | ".m4a" | ".mp4" -> Some "audio/mp4"
  | ".webm" -> Some "audio/webm"
  | ".flac" -> Some "audio/flac"
  | ".ogg" -> Some "audio/ogg"
  | _ -> None
let valid_audio_input basename bytes =
  let length = String.length bytes in
  match String.lowercase_ascii (Filename.extension basename) with
  | ".wav" -> valid_wav bytes
  | ".mp3" | ".mpga" | ".mpeg" ->
      length >= 3 && (String.sub bytes 0 3 = "ID3" ||
        (length >= 2 && Char.code bytes.[0] = 255 && (Char.code bytes.[1] land 224) = 224))
  | ".m4a" | ".mp4" -> length >= 12 && String.sub bytes 4 4 = "ftyp"
  | ".webm" -> length >= 4 && String.sub bytes 0 4 = "\026E\223\163"
  | ".flac" -> length >= 4 && String.sub bytes 0 4 = "fLaC"
  | ".ogg" -> length >= 4 && String.sub bytes 0 4 = "OggS"
  | _ -> false

let safe_multipart_filename filename =
  if filename = "" || String.exists (fun c -> c = '\r' || c = '\n' || c = '"' || Char.code c < 32) filename then
    invalid "audio filename contains unsupported characters";
  filename

let transcribe ?http ?cancel ~key ~model ~root ~input () = protect (fun () ->
  let model = valid_model model in
  let basename, audio = read_workspace_file ~root ~path:input ~max_bytes:max_audio_input_bytes in
  let mime = match audio_type basename with
    | Some mime -> mime
    | None -> invalid "--file must use a documented audio extension: wav, mp3, mpga, mpeg, m4a, mp4, webm, flac, or ogg" in
  if not (valid_audio_input basename audio) then invalid "audio file bytes do not match the file type";
  let filename = safe_multipart_filename basename in
  let boundary = choose_multipart_boundary audio model in
  let prefix = "--" ^ boundary ^ "\r\nContent-Disposition: form-data; name=\"file\"; filename=\"" ^ filename ^ "\"\r\nContent-Type: " ^ mime ^ "\r\n\r\n" in
  let body = prefix ^ audio ^ "\r\n" ^ multipart_field boundary "model" model ^
      "--" ^ boundary ^ "--\r\n" in
  if String.length body > max_request_bytes + max_audio_input_bytes + 65536 then
    invalid "multipart audio request exceeds the size limit";
  let response = perform ?http ?cancel ~key ~key_name:"OPENAI_API_KEY" ~url:transcription_url
      ~content_type:("multipart/form-data; boundary=" ^ boundary)
      ~body ~response_limit:max_json_response_bytes () in
  let json = parse_json response in
  let text = string_field "text" json in
  (match field "usage" json with
   | None | Some `Null -> ()
   | Some (`Assoc usage) ->
       (match List.assoc_opt "type" usage with
        | Some (`String "tokens") ->
            List.iter (fun key -> match List.assoc_opt key usage with
              | None -> ()
              | Some value -> ignore (nonnegative_int ("usage." ^ key) (`Assoc [key, value])))
              ["input_tokens"; "output_tokens"; "total_tokens"]
        | Some (`String "duration") ->
            (match List.assoc_opt "seconds" usage with
             | Some value -> let seconds = finite_number "usage.seconds" value in
                 if seconds < 0. then invalid_response "usage.seconds must be nonnegative"
             | None -> invalid_response "usage.seconds is missing")
        | Some (`String _) -> invalid_response "usage.type is unsupported"
        | _ -> invalid_response "usage.type is missing")
   | Some _ -> invalid_response "usage must be an object");
  text ^ "\n")

let rerank ?http ?cancel ~key ~model ~query ~documents ~top_n () = protect (fun () ->
  let model = valid_model model in
  let query = valid_text_field ~label:"--query" ~max_bytes:(16 * 1024) query in
  if documents = [] then invalid "at least one --document is required";
  if List.length documents > 1000 then invalid "at most 1000 --document values are supported";
  let documents = List.map (valid_text_field ~label:"--document" ~max_bytes:(32 * 1024)) documents in
  let total = List.fold_left (fun bytes doc -> bytes + String.length doc) (String.length query) documents in
  if total > max_request_bytes / 2 then invalid "combined query and documents exceed the 512-KiB limit";
  let top_n = match top_n with
    | None -> None
    | Some n when n > 0 && n <= List.length documents -> Some n
    | Some _ -> invalid "--top-n must be from 1 through the number of documents" in
  let fields = ["model", `String model; "query", `String query;
    "documents", `List (List.map (fun doc -> `String doc) documents)] in
  let fields = match top_n with None -> fields | Some n -> fields @ ["top_n", `Int n] in
  let json = json_request ?http ?cancel ~key ~key_name:"COHERE_API_KEY"
      ~url:cohere_rerank_url (`Assoc fields) in
  let results = match required "results" json with `List rows -> rows | _ -> invalid_response "results must be an array" in
  let expected = Option.value ~default:(List.length documents) top_n in
  if List.length results <> expected then invalid_response "rerank result count is incomplete";
  let seen = Hashtbl.create (List.length results) in
  let previous = ref infinity in
  List.iter (fun row ->
    let index = nonnegative_int "index" row in
    if index >= List.length documents || Hashtbl.mem seen index then
      invalid_response "rerank index is out of range or duplicated";
    Hashtbl.add seen index ();
    let score = finite_number "relevance_score" (required "relevance_score" row) in
    if score < 0. || score > 1. then invalid_response "relevance_score must be in [0, 1]";
    if score > !previous then invalid_response "rerank scores are not in descending order";
    previous := score) results;
  ensure_json_result json)
