(* Devin CLI's Codeium Cascade protocol. Credentials live in protobuf metadata,
   never in an Authorization header or a client-selected endpoint. The fixed
   Connect paths and metadata fields follow oh-my-pi's Devin wire declarations. *)
let base_url = "https://server.codeium.com"
let auth_url = base_url ^ "/exa.auth_pb.AuthService/GetUserJwt"
let models_url = base_url ^ "/exa.api_server_pb.ApiServerService/GetCliModelConfigs"
let assign_url = base_url ^ "/exa.api_server_pb.ApiServerService/AssignModel"
let chat_url = base_url ^ "/exa.api_server_pb.ApiServerService/GetChatMessage"
let max_frame = 16 * 1024 * 1024
let max_body = 32 * 1024 * 1024
let max_models = 4096

type error = Invalid_credential | Transport_error | Http_error of int
  | Invalid_response of string
type model = { id : string; name : string; router : bool;
  context_window_tokens : int option; tokenizer_type : string option;
  max_tokens : int; supports_tools : bool; supports_parallel_tool_calls : bool }
type http = url:string -> headers:(string * string) list -> body:string ->
  on_chunk:(string -> unit) -> (int, error) result

exception Bad_wire of string
let bad why = raise (Bad_wire why)
let valid_text text = text <> "" && String.length text <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) text
let valid_id text = text <> "" && String.length text <= 256 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) text
let env_api_key () = match Sys.getenv_opt "DEVIN_API_KEY" with
  | Some value when valid_text value -> Some value | _ -> None
let session_key key =
  if not (valid_text key) then invalid_arg "invalid Devin session token";
  if String.starts_with ~prefix:"devin-session-token$" key then key
  else "devin-session-token$" ^ key

(* Minimal protobuf wire primitives: unknown fields are skipped, lengths and
   varints are checked before allocation. No generated dependency is needed. *)
let varint b n =
  if n < 0 then bad "negative protobuf varint";
  let rec emit n =
    if n >= 128 then (Buffer.add_char b (Char.chr ((n land 127) lor 128)); emit (n lsr 7))
    else Buffer.add_char b (Char.chr n) in
  emit n
let key b number wire = varint b ((number lsl 3) lor wire)
let number b field value = if value <> 0 then (key b field 0; varint b value)
let boolean b field value = if value then number b field 1
let string b field value = if value <> "" then (
  key b field 2; varint b (String.length value); Buffer.add_string b value)
let bytes b field value = key b field 2; varint b (String.length value); Buffer.add_string b value
let double b field value =
  key b field 1;
  let bits = Int64.bits_of_float value in
  for i = 0 to 7 do
    let byte = Int64.(to_int (logand (shift_right_logical bits (i * 8)) 255L)) in
    Buffer.add_char b (Char.chr byte)
  done
let buf f = let b = Buffer.create 256 in f b; Buffer.contents b
let read_varint data pos =
  let size = String.length data in
  let rec go shift value =
    if !pos >= size || shift >= 63 then bad "truncated or oversized varint";
    let ch = Char.code data.[!pos] in incr pos;
    let value = value lor ((ch land 127) lsl shift) in
    if ch land 128 = 0 then value else go (shift + 7) value in
  go 0 0
let fields data =
  let pos = ref 0 and size = String.length data and result = ref [] in
  let count = ref 0 in
  while !pos < size do
    incr count;
    if !count > 65536 then bad "too many protobuf fields";
    let tag = read_varint data pos in
    let field = tag lsr 3 and wire = tag land 7 in
    if field = 0 then bad "zero protobuf tag";
    let value = match wire with
      | 0 -> `Number (read_varint data pos)
      | 1 -> if size - !pos < 8 then bad "truncated fixed64";
          pos := !pos + 8; `Other
      | 2 -> let length = read_varint data pos in
          if length < 0 || length > size - !pos then bad "truncated protobuf field";
          let value = String.sub data !pos length in pos := !pos + length; `Bytes value
      | 5 -> if size - !pos < 4 then bad "truncated fixed32";
          pos := !pos + 4; `Other
      | _ -> bad "unsupported protobuf wire type" in
    result := (field, value) :: !result
  done;
  List.rev !result
let entries number fields = List.filter_map (fun (field, data) ->
  if field = number then Some data else None) fields
let text number fs = match entries number fs with
  | `Bytes text :: _ -> text | _ -> ""
let integer number fs = match entries number fs with
  | `Number value :: _ -> value | _ -> 0
let flag number fs = integer number fs <> 0
let submessages number fs = List.filter_map (function
  | `Bytes text -> Some (fields text) | _ -> None) (entries number fs)
let submessage number fs = match submessages number fs with
  | first :: _ -> first | [] -> []

let metadata ?(jwt="") ?(discovery=false) api_key = buf (fun b ->
  let client = if discovery then "chisel" else "devin-cli" in
  let version = if discovery then "0.0.0-dev" else "3000.6.2" in
  string b 1 client; string b 2 version;
  string b 3 (session_key api_key); string b 4 "en";
  string b 5 (if Sys.os_type = "Win32" then "windows" else
    if Sys.os_type = "Unix" && Sys.file_exists "/System/Library" then "darwin" else "linux");
  string b 7 version; string b 12 "chisel";
  if not discovery then string b 28 "chisel";
  string b 21 jwt;
  if discovery then List.iter (fun display -> number b 30 display)
    [3; 4; 6; 7; 8])
let unary_request meta = buf (fun b -> bytes b 1 meta)
let unary_headers = ["Content-Type", "application/proto";
  "Connect-Protocol-Version", "1"; "Accept", "*/*"]
let stream_headers = ["Content-Type", "application/connect+proto";
  "Connect-Protocol-Version", "1"; "Connect-Content-Encoding", "gzip";
  "Connect-Accept-Encoding", "gzip"; "Accept-Encoding", "identity";
  "User-Agent", "connect-go/1.18.1 (go1.26.3)"]

(* gzip(1) is invoked without a shell, with controlled arguments and 0600
   input. Pipe output is capped while reading, so a gzip bomb cannot fill disk. *)
let gzip ?(decode=false) input =
  if String.length input > max_body then bad "compressed input exceeds limit";
  Devin_binary_http.with_temp_file (fun input_path output ->
    output_string output input; close_out output;
    let source = Unix.openfile input_path [Unix.O_RDONLY] 0 in
    let reader,writer = Unix.pipe () in
    let null = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
    let args = if decode then [|"gzip"; "-d"; "-c"|]
      else [|"gzip"; "-n"; "-c"|] in
    let pid = try Unix.create_process "/usr/bin/gzip" args source writer null
      with exn ->
        Unix.close source; Unix.close reader; Unix.close writer;
        Unix.close null; raise exn in
    Unix.close source; Unix.close writer; Unix.close null;
    let output = Buffer.create (min (String.length input) 4096) in
    let chunk = Bytes.create 8192 in
    let status = try
      let rec consume () =
        let count = Unix.read reader chunk 0 (Bytes.length chunk) in
        if count <> 0 then (
          if count > max_frame - Buffer.length output then
            bad "decompressed protobuf exceeds frame cap";
          Buffer.add_subbytes output chunk 0 count;
          consume ()) in
      consume ();
      Unix.close reader;
      snd (Unix.waitpid [] pid)
    with exn ->
      Devin_binary_http.close_fd reader;
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
      raise exn in
    match status with
    | Unix.WEXITED 0 -> Buffer.contents output
    | _ -> bad "gzip failed")
let decode_unary payload =
  if String.length payload >= 2 && payload.[0] = '\x1f' && payload.[1] = '\x8b'
  then gzip ~decode:true payload else payload

let allowed_url url =
  url = auth_url || url = models_url || url = assign_url || url = chat_url

(* The injected HTTP callback has the same pinning obligation as the default
   executor; the module itself NEVER accepts caller-defined URLs. *)
let default_http ?cancel ~url ~headers ~body ~on_chunk () =
  if not (allowed_url url) then Error (Invalid_response "untrusted Devin API endpoint")
  else try
    let status, response = Devin_binary_http.post ?cancel
      ~url ~headers ~body ~max_bytes:max_body () in
    on_chunk response;
    Ok status
  with Devin_binary_http.Failed -> Error Transport_error
let rpc ?http ?cancel ~url ~headers ~body () =
  if not (allowed_url url) then Error (Invalid_response "untrusted Devin API endpoint")
  else let received = Buffer.create 4096 in
  let on_chunk chunk =
    if Buffer.length received + String.length chunk > max_body then
      bad "Devin response exceeds size limit";
    Buffer.add_string received chunk in
  let call = match http with
    | Some http -> http
    | None -> fun ~url ~headers ~body ~on_chunk ->
        default_http ?cancel ~url ~headers ~body ~on_chunk () in
  match call ~url ~headers ~body ~on_chunk with
  | Error reason -> Error reason
  | Ok status when status < 200 || status >= 300 -> Error (Http_error status)
  | Ok _ -> Ok (Buffer.contents received)
let protect f = try f () with
  | Bad_wire text -> Error (Invalid_response text)
  | Invalid_argument text -> Error (Invalid_response text)
  | Unix.Unix_error _ | Sys_error _ -> Error Transport_error
  | Protocol.Invalid_response text -> Error (Invalid_response text)

let authenticate ?http ?cancel ~api_key () = protect (fun () ->
  if not (valid_text api_key) then Error Invalid_credential else
  match rpc ?http ?cancel ~url:auth_url ~headers:unary_headers
    ~body:(unary_request (metadata api_key)) () with
  | Error _ as error -> error
  | Ok payload ->
      let response = fields (decode_unary payload) in
      let jwt = text 1 response and custom = text 2 response in
      if jwt = "" then Error (Invalid_response "empty Devin user JWT")
      else if custom <> "" && custom <> base_url && custom <> base_url ^ "/" then
        Error (Invalid_response "Devin auth redirected to untrusted API host")
      else Ok jwt)

let model_of_config fs =
  let id = text 22 fs and info = submessage 23 fs in
  if flag 4 fs || not (valid_id id) then None else
  let display = integer 22 info in
  if display = 4 || display = 6 then None else
  let harness = entries 20 info in
  let router = (display = 3 || flag 25 info) && harness = [] in
  let features = submessage 6 info in
  let context_window = integer 4 info in
  let tokenizer = text 5 info in
  let tokenizer_type =
    if tokenizer <> "" && String.length tokenizer <= 128 &&
        String.for_all (fun c -> Char.code c > 32 && Char.code c < 127)
          tokenizer
    then Some tokenizer else None in
  let max_tokens = integer 13 info in
  Some { id; name = (let label = String.trim (text 1 fs) in
    if label = "" then id else label);
    router;
    context_window_tokens = (if context_window > 0 then Some context_window else None);
    tokenizer_type;
    max_tokens = (if max_tokens > 0 then max_tokens else 64000);
    supports_tools = features = [] || flag 12 features;
    supports_parallel_tool_calls = flag 21 features }
let discover ?http ?cancel ~api_key () = protect (fun () ->
  if not (valid_text api_key) then Error Invalid_credential else
  let discover_with meta = match rpc ?http ?cancel ~url:models_url
    ~headers:unary_headers ~body:(unary_request meta) () with
  | Error _ as error -> error
  | Ok payload ->
      let records = submessages 1 (fields (decode_unary payload)) in
      if List.length records > max_models then bad "Devin catalog exceeds size limit";
      let seen = Hashtbl.create (List.length records) in
      Ok (List.filter_map (fun record -> match model_of_config record with
        | Some model when not (Hashtbl.mem seen model.id) ->
            Hashtbl.add seen model.id (); Some model
        | _ -> None) records) in
  let native = discover_with (metadata ~discovery:true api_key) in
  (* Enterprise seats can expose more models to the legacy identity than the
     CLI identity. Prefer the larger credential-scoped roster, not seed IDs. *)
  let legacy = buf (fun b -> string b 1 "windsurf";
    string b 2 "1.48.2"; string b 3 api_key; string b 4 "en";
    string b 7 "3.2.23"; string b 12 "windsurf") in
  let previous = match native with Ok rows -> List.length rows | Error _ -> 0 in
  match discover_with legacy, native with
  | Ok rows, _ when List.length rows > previous -> Ok rows
  | _, Ok (_ :: _ as rows) -> Ok rows
  | Ok _, Error error -> Error error
  | Error error, _ -> Error error
  | _, Ok [] -> Error (Invalid_response "empty Devin account model catalog"))

(* The Devin Prompt wire message carries text (field 3), call references and
   metadata, but has no image/content-block field. Reject typed images before
   any RPC rather than silently dropping them or stringifying base64 as text. *)
let reject_image_tool_results messages =
  if List.exists (fun (message : Protocol.message) ->
    match message.tool_result_content with
    | Some blocks -> List.exists (function Protocol.Image _ -> true | _ -> false) blocks
    | None -> false) messages then
    bad "Devin protobuf transport does not support image tool results"

let prompt ?(message_id="") ?(source=1) ?(call_id="") ?(is_error=false)
    ?(thinking="") ?(signature="") ?(calls=[]) value =
  buf (fun b -> string b 1 message_id; number b 2 source; string b 3 value;
    List.iter (bytes b 6) calls; string b 7 call_id;
    boolean b 9 is_error; string b 11 thinking; string b 12 signature)
let tool_call (call : Protocol.tool_call) = buf (fun b ->
  string b 1 call.id; string b 2 call.name;
  string b 3 (Yojson.Basic.to_string call.arguments))
let format_uuid hex =
  String.sub hex 0 8 ^ "-" ^ String.sub hex 8 4 ^ "-" ^
  String.sub hex 12 4 ^ "-" ^ String.sub hex 16 4 ^ "-" ^
  String.sub hex 20 12
let uuid seed =
  let hex = Bytes.of_string (Digest.to_hex (Digest.string seed)) in
  Bytes.set hex 12 '3'; Bytes.set hex 16 '8';
  format_uuid (Bytes.unsafe_to_string hex)
let execution_id () =
  let ic = open_in_bin "/dev/urandom" in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
    let random = really_input_string ic 16 in
    let hex = Bytes.create 32 in
    let alphabet = "0123456789abcdef" in
    for index = 0 to 15 do
      let code = Char.code random.[index] in
      Bytes.set hex (index * 2) alphabet.[code lsr 4];
      Bytes.set hex (index * 2 + 1) alphabet.[code land 15]
    done;
    Bytes.set hex 12 '4';
    Bytes.set hex 16 "89ab".[Char.code random.[8] land 3];
    format_uuid (Bytes.unsafe_to_string hex))
let valid_uuid value =
  let hex = function
    | '0'..'9' | 'a'..'f' | 'A'..'F' -> true | _ -> false in
  if String.length value <> 36 then false else
  let valid = ref true in
  for index = 0 to 35 do
    if index = 8 || index = 13 || index = 18 || index = 23 then (
      if value.[index] <> '-' then valid := false)
    else if not (hex value.[index]) then valid := false
  done;
  !valid
(* Pave does not put a conversation ID in Provider.config. Persist the
   cryptographically random Cascade thread in the assistant state and recover
   it from the most recent valid native Devin response on a later turn. *)
let cascade_id messages =
  let rec previous = function
    | [] -> execution_id ()
    | (message : Protocol.message) :: remaining ->
        (match message.role, message.provider_state with
        | "assistant", Some state
          when Protocol.member "provider" state = `String "devin" ->
            (match Protocol.member "cascade_id" state,
              Protocol.member "model" state,
              Protocol.member "message_id" state,
              Protocol.member "thinking" state,
              Protocol.member "signature" state with
            | `String cascade, `String model, `String message_id,
                `String _, `String _
                when valid_uuid cascade && valid_id model &&
                  (message_id = "" || valid_id message_id) -> cascade
            | _ -> previous remaining)
        | _ -> previous remaining) in
  previous (List.rev messages)
let prompts ~cascade_id ~selected_model messages =
  List.mapi (fun index (message : Protocol.message) ->
    let id = uuid (cascade_id ^ "\000" ^ string_of_int index ^
      "\000" ^ message.role) in
    match message.role with
    | "user" | "developer" ->
        prompt ~message_id:id (Option.value ~default:"" message.content)
    | "assistant" ->
        let state_id,thinking,signature = match message.provider_state with
          | Some state when Protocol.member "provider" state = `String "devin"
              && Protocol.member "model" state = `String selected_model ->
              (match Protocol.member "message_id" state,
                Protocol.member "thinking" state,
                Protocol.member "signature" state with
              | `String native_id, `String t, `String s
                  when native_id = "" || valid_id native_id ->
                  (if native_id = "" then "bot-" ^ id else native_id),t,s
              | _ -> bad "malformed Devin assistant state")
          | Some _ -> bad "cannot replay another provider's signed state"
          | None -> "bot-" ^ id,"","" in
        prompt ~message_id:state_id ~source:2 ~thinking ~signature
          ~calls:(List.map tool_call message.tool_calls)
          (Option.value ~default:"" message.content)
    | "tool" ->
        let call_id = match message.tool_call_id with
          | Some id when valid_id id -> id | _ -> bad "missing tool call ID" in
        prompt ~message_id:id ~source:4 ~call_id
          (Option.value ~default:"" message.content)
    | _ -> bad "unsupported Devin transcript role") messages
let tool_definition json =
  let member = Protocol.member in
  let fn = member "function" json in
  let value field = match member field fn with `String value -> value
    | _ -> bad ("invalid Devin tool " ^ field) in
  let name = value "name" in
  if not (valid_id name) then bad "invalid Devin tool name";
  let schema = match member "parameters" fn with
    | `Null -> `Assoc ["type", `String "object"; "properties", `Assoc []]
    | schema -> schema in
  buf (fun b -> string b 1 name; string b 2 (value "description");
    string b 3 (Yojson.Basic.to_string schema);
    boolean b 12 (member "strict" fn = `Bool true))
let router_prompt messages =
  let rec last = function
    | [] -> None
    | (message : Protocol.message) :: tail ->
        if message.role = "user" || message.role = "developer" then
          Some (prompt (Option.value ~default:"" message.content))
        else last tail in
  last (List.rev messages)
let assign ?http ?cancel ~api_key ~model ~cascade_id messages =
  let body = buf (fun b ->
    bytes b 1 (metadata api_key); string b 2 model; string b 3 cascade_id;
    Option.iter (bytes b 5) (router_prompt messages)) in
  match rpc ?http ?cancel ~url:assign_url ~headers:unary_headers ~body () with
  | Error error -> Error error
  | Ok body -> let assignment = submessage 1 (fields (decode_unary body)) in
      let jwt = text 1 assignment and actual = text 2 assignment in
      if not (valid_text jwt && valid_id actual) then
        Error (Invalid_response "Devin router returned no assignment JWT/model UID")
      else Ok (actual,jwt)
let request ?(max_tokens=64000) ?(supports_parallel_tool_calls=false)
    ~api_key ~jwt ~model ~selected_model ~cascade_id ?assignment messages tools =
  reject_image_tool_results messages;
  if not (valid_id model && valid_uuid cascade_id) then bad "invalid Devin model or cascade ID";
  if max_tokens < 1 || max_tokens > 1_000_000 then bad "invalid Devin max tokens";
  buf (fun b ->
    bytes b 1 (metadata ~jwt api_key);
    let system = List.filter_map (fun (m : Protocol.message) ->
      if m.role = "system" then m.content else None) messages in
    string b 2 (String.concat "\n\n" system);
    List.iter (bytes b 3) (prompts ~cascade_id ~selected_model
      (List.filter (fun (m : Protocol.message) -> m.role <> "system") messages));
    string b 21 model; number b 7 5;
    bytes b 8 (buf (fun c ->
      number c 1 1; number c 2 max_tokens; number c 3 200;
      double c 5 0.4; double c 6 0.4; number c 7 50;
      double c 8 1.; double c 11 1.;
      List.iter (string c 9)
        ["<|user|>"; "<|bot|>"; "<|context_request|>";
          "<|endoftext|>"; "<|end_of_turn|>"]));
    boolean b 11 (not supports_parallel_tool_calls);
    List.iter (fun tool -> bytes b 10 (tool_definition tool)) tools;
    bytes b 12 (buf (fun c -> string c 1 "auto"));
    bytes b 13 (buf (fun c -> number c 1 1));
    string b 16 cascade_id; number b 20 1;
    string b 22 (execution_id ());
    Option.iter (string b 26) assignment)
let frame flag payload =
  let n = String.length payload in
  if n > max_frame then bad "Connect frame exceeds cap";
  let b = Bytes.create (5 + n) in
  Bytes.set b 0 (Char.chr flag);
  for i = 0 to 3 do Bytes.set b (i+1)
    (Char.chr ((n lsr ((3-i)*8)) land 255)) done;
  Bytes.blit_string payload 0 b 5 n; Bytes.unsafe_to_string b
let parse_stream ?(assigned_model="") ?(selected_model="") ?(cascade_id="") body =
  let offset = ref 0 and output_text = Buffer.create 256 and thinking = Buffer.create 256 in
  let calls = Hashtbl.create 4 and order = ref [] in
  let active_call = ref "" and decoded_bytes = ref 0 in
  let message_id = ref "" and signature = ref "" and actual_model = ref assigned_model in
  let trailer = ref false and stop = ref 0 and usage = ref None in
  let rec parse () =
    let size = String.length body - !offset in
    if size >= 5 then (
      let flags = Char.code body.[!offset] in
      if flags land (lnot 3) <> 0 then bad "invalid Connect frame flags";
      let len = ref 0 in
      for i = 1 to 4 do len := (!len lsl 8) lor Char.code body.[!offset+i] done;
      if !len > max_frame then bad "Connect frame exceeds cap";
      if size >= !len + 5 then (
        let data = String.sub body (!offset+5) !len in
        offset := !offset + !len + 5;
        if !trailer then bad "Connect data after end-of-stream";
        let data = if flags land 1 <> 0 then gzip ~decode:true data else data in
        if String.length data > max_body - !decoded_bytes then
          bad "decoded Devin stream exceeds size limit";
        decoded_bytes := !decoded_bytes + String.length data;
        if flags land 2 <> 0 then (
          trailer := true;
          let json = try Yojson.Basic.from_string data
            with Yojson.Json_error _ -> bad "invalid Connect trailer" in
          match Protocol.member "error" json with
          | `Null -> ()
          | error ->
              let code = Protocol.member "code" error in
              let message = Protocol.member "message" error in
              let text = (match code with `String s -> s | _ -> "unknown") ^
                ": " ^ (match message with `String s -> s | _ -> "unknown") in
              bad ("Devin Connect error " ^ String.sub text 0 (min 1024 (String.length text))))
        else (
          let fs = fields data in
          if text 1 fs <> "" then message_id := text 1 fs;
          if text 23 fs <> "" then actual_model := text 23 fs;
          Buffer.add_string output_text (text 3 fs);
          Buffer.add_string thinking (text 9 fs);
          if text 10 fs <> "" then signature := text 10 fs;
          if integer 5 fs <> 0 then stop := integer 5 fs;
          List.iter (fun stats -> usage := Some (integer 2 stats,integer 3 stats))
            (submessages 7 fs);
          List.iter (fun tc ->
            let id = let wire_id = text 1 tc in
              if wire_id = "" then !active_call else wire_id in
            if id <> "" then (
              if not (valid_id id) then bad "invalid tool call ID";
              active_call := id;
              let name = text 2 tc and delta = text 3 tc in
              let old_name,old_args = match Hashtbl.find_opt calls id with
                | Some pair -> pair
                | None -> order := id :: !order; "","" in
              let args = if String.starts_with ~prefix:old_args delta then delta
                else old_args ^ delta in
              if String.length args > max_frame then bad "tool arguments exceed size limit";
              Hashtbl.replace calls id ((if name = "" then old_name else name),args)))
            (submessages 6 fs));
        parse ())) in
  parse ();
  if !offset <> String.length body then bad "truncated Connect frame";
  if not !trailer then bad "missing Connect end-of-stream trailer";
  let tool_calls = List.map (fun id ->
    let name,args = Hashtbl.find calls id in
    if not (valid_id name) then bad "invalid streamed tool name";
    let arguments = try Yojson.Basic.from_string args
      with Yojson.Json_error _ -> bad "invalid streamed tool arguments" in
    ({ id; name; arguments } : Protocol.tool_call)) (List.rev !order) in
  if !stop = 3 && tool_calls = [] then bad "Devin response stopped at token limit";
  let content = Buffer.contents output_text in
  let provider_state = Some (`Assoc ["provider", `String "devin";
    "model", `String selected_model;
    "cascade_id", `String cascade_id;
    "message_id", `String !message_id;
    "thinking", `String (Buffer.contents thinking);
    "signature", `String !signature;
    "actual_model", `String !actual_model]) in
  ({ role = "assistant"; content = (if content = "" then None else Some content);
    tool_result_content = None; tool_calls; tool_call_id = None; provider_state;
    attachments = [] } : Protocol.message), !usage

let complete ?http ?cancel ?(max_tokens=64000)
    ?(supports_parallel_tool_calls=false) ~api_key ~model ~cascade_id ~router
    messages tools =
  protect (fun () ->
    reject_image_tool_results messages;
    if not (valid_text api_key) then Error Invalid_credential
    else if not (valid_id model && valid_uuid cascade_id) then
      Error (Invalid_response "invalid Devin model or cascade ID")
    else match authenticate ?http ?cancel ~api_key () with
    | Error _ as error -> error
    | Ok jwt ->
        let assigned = if router then
          assign ?http ?cancel ~api_key ~model ~cascade_id messages
          else Ok (model, "") in
        (match assigned with
        | Error _ as error -> error
        | Ok (actual, assignment) ->
            let body = request ~max_tokens ~supports_parallel_tool_calls
              ~api_key ~jwt ~model:actual ~selected_model:model ~cascade_id
              ?assignment:(if assignment = "" then None else Some assignment)
              messages tools in
            let body = frame 1 (gzip body) in
            match rpc ?http ?cancel ~url:chat_url ~headers:stream_headers ~body () with
            | Error _ as error -> error
            | Ok response -> Ok (parse_stream ~assigned_model:actual
                ~selected_model:model ~cascade_id response)))
