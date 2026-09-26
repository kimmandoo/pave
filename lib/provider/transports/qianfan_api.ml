(* Qianfan ModelBuilder V2 API (not the legacy OAuth access_token API).
   https://cloud.baidu.com/doc/qianfan-api/s/ym9chdsy5
   https://cloud.baidu.com/doc/qianfan-api/s/Dmba8k71y
   https://cloud.baidu.com/doc/qianfan-api/s/3m7of64lb
   The account listing contains non-Chat models; only explicitly type=chat
   entries are offered for Chat Completions. Missing type remains unclassified
   and is omitted, not guessed from the model name. *)
let models_url = "https://qianfan.baidubce.com/v2/models"
let chat_url = "https://qianfan.baidubce.com/v2/chat/completions"
let max_response_bytes = 1_048_576
let max_models = 4096

type error =
  | Invalid_credential
  | Transport_error
  | Http_error of int
  | Invalid_response of string

type http = url:string -> headers:(string * string) list ->
  (int * string, error) result

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let env_api_key () = match Sys.getenv_opt "QIANFAN_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Qianfan credential requires the official V2 Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Qianfan API key";
  ["Authorization: Bearer " ^ api_key]

let valid_id id = id <> "" && String.length id <= 256 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) id

let parse_models body =
  let json = try Some (Yojson.Basic.from_string body)
    with Yojson.Json_error _ -> None in
  match json with
  | Some (`Assoc fields) ->
      (match List.assoc_opt "data" fields with
      | Some (`List rows) when List.length rows <= max_models ->
          let seen = Hashtbl.create (List.length rows) in
          let duplicate = ref false in
          let ids = ref [] in
          let valid = List.for_all (function
            | `Assoc model ->
                (match List.assoc_opt "id" model,
                  List.assoc_opt "object" model,
                  List.assoc_opt "type" model with
                | Some (`String id), Some (`String "model"),
                    (Some (`String _) | None) when valid_id id ->
                    if Hashtbl.mem seen id then duplicate := true
                    else (
                      Hashtbl.add seen id ();
                      if List.assoc_opt "type" model = Some (`String "chat") then
                        ids := id :: !ids);
                    true
                | _ -> false)
            | _ -> false) rows in
          if valid && not !duplicate then Ok (List.rev !ids)
          else Error (Invalid_response "invalid Qianfan model object")
      | Some (`List _) -> Error (Invalid_response "too many Qianfan models")
      | _ -> Error (Invalid_response "missing data array"))
  | _ -> Error (Invalid_response "malformed Qianfan model listing")

(* Production's HTTPS executor must bound downloads and reject redirects;
   this layer also rejects oversized replies from injected HTTP clients. *)
let discover ~http ~api_key () =
  if not (valid_key api_key) then Error Invalid_credential
  else
    let headers = ["Authorization", "Bearer " ^ api_key;
      "Accept", "application/json"] in
    match http ~url:models_url ~headers with
    | Error failure -> Error failure
    | Ok (status, _) when status < 200 || status >= 300 ->
        Error (Http_error status)
    | Ok (_, body) when String.length body > max_response_bytes ->
        Error (Invalid_response "listing exceeds size limit")
    | Ok (_, body) -> parse_models body

(* The response may report reasoning_content, but the official Chat request
   schema and its function-call continuation do not accept/replay this field.
   Preserve it for the conversation state without sending an undocumented
   field back in messages. *)
let reported_reasoning json = match Protocol.member "reasoning_content" json with
  | `Null -> None
  | `String content -> Some (`Assoc ["reasoning_content", `String content])
  | _ -> raise (Protocol.Invalid_response "invalid Qianfan reasoning_content")

let request ~model messages tools =
  let message (msg : Protocol.message) =
    let json = Protocol.message_to_json msg in
    match json with
    | `Assoc fields when msg.role = "assistant" && msg.content = None ->
        `Assoc (fields @ ["content", `String ""])
    | _ -> json in
  let fields = ["model", `String model;
    "messages", Protocol.chat_messages_to_json ~serialize:message messages;
    "stream", `Bool false] in
  `Assoc (if tools = [] then fields
    else fields @ ["tools", `List tools; "tool_choice", `String "auto"])

let parse_completion json =
  let choice = match Protocol.member "choices" json with
    | `List (choice :: _) -> choice
    | _ -> raise (Protocol.Invalid_response "missing Qianfan choices") in
  let message = Protocol.member "message" choice in
  (match Protocol.member "tool_calls" message with
  | `List calls -> List.iter (fun call ->
      let fn = Protocol.member "function" call in
      match Protocol.member "type" call,
        Protocol.member "arguments" fn with
      | `String "function", `String arguments ->
          (match (try Some (Yojson.Basic.from_string arguments)
            with Yojson.Json_error _ -> None) with
          | Some (`Assoc _) -> ()
          | _ -> raise (Protocol.Invalid_response
              "invalid Qianfan function arguments"))
      | _ -> raise (Protocol.Invalid_response
          "invalid Qianfan function call type or arguments")) calls
  | _ -> ());
  let result = Protocol.parse_completion json in
  match reported_reasoning message with
  | None -> result
  | Some provider_state -> { result with provider_state = Some provider_state }
