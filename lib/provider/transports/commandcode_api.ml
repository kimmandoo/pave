(* Command Code Provider API, not the BYOK endpoint of the Command Code CLI.
   https://commandcode.ai/docs/provider
   https://commandcode.ai/docs/studio#api-keys
   Studio keys authenticate with Bearer on one pinned host; GO-plan credentials
   cannot call Provider API. The public model listing is not a key-validation
   endpoint: even a bogus key may receive HTTP 200. Each row's
   supported_endpoints, not its model ID, determines valid inference routes.
   Only subscribed accounts with a usable Studio key can perform inference. *)
let models_url = "https://api.commandcode.ai/provider/v1/models"
let chat_url = "https://api.commandcode.ai/provider/v1/chat/completions"
let responses_url = "https://api.commandcode.ai/provider/v1/responses"
let messages_url = "https://api.commandcode.ai/provider/v1/messages"
let max_response_bytes = 1_048_576
let max_models = 4096

type model = {
  id : string;
  name : string;
  context_length : int option;
  supported_endpoints : string list;
}
type error =
  | Invalid_credential
  | Transport_error
  | Http_error of int
  | Invalid_response of string

type http = url:string -> headers:(string * string) list ->
  (int * string, error) result

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let env_api_key () =
  let find name = match Sys.getenv_opt name with
    | Some key when valid_key key -> Some key
    | _ -> None in
  match find "COMMAND_CODE_API_KEY" with
  | Some _ as key -> key
  | None -> find "COMMANDCODE_API_KEY"

let headers ~route ~endpoint ~api_key =
  if (route <> chat_url && route <> responses_url && route <> messages_url) ||
     endpoint <> route then
    invalid_arg "Command Code API key requires its pinned Provider API route";
  if not (valid_key api_key) then invalid_arg "invalid Command Code Studio key";
  ["Authorization: Bearer " ^ api_key]

let chat_headers ~endpoint ~api_key = headers ~route:chat_url ~endpoint ~api_key
let responses_headers ~endpoint ~api_key =
  headers ~route:responses_url ~endpoint ~api_key
let messages_headers ~endpoint ~api_key =
  headers ~route:messages_url ~endpoint ~api_key @
  ["anthropic-version: 2023-06-01"]

let valid_id id = id <> "" && String.length id <= 256 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) id

let parse_models body =
  let json = try Some (Yojson.Basic.from_string body)
    with Yojson.Json_error _ -> None in
  match json with
  | Some (`Assoc fields) ->
      (match List.assoc_opt "object" fields, List.assoc_opt "data" fields with
      | Some (`String "list"), Some (`List rows) when List.length rows <= max_models ->
          let seen = Hashtbl.create (List.length rows) in
          let models = ref [] in
          let duplicate = ref false in
          let valid = List.for_all (function
            | `Assoc row ->
                (match List.assoc_opt "id" row, List.assoc_opt "name" row,
                  List.assoc_opt "context_length" row,
                  List.assoc_opt "supported_endpoints" row with
                | Some (`String id), Some (`String name), length,
                  Some (`List endpoints) when valid_id id && name <> "" &&
                    String.length name <= 512 ->
                    if Hashtbl.mem seen id then duplicate := true
                    else Hashtbl.add seen id ();
                    let context_length = match length with
                      | None | Some `Null -> Some None
                      | Some (`Int value) when value > 0 -> Some (Some value)
                      | _ -> None in
                    let supported_endpoints = List.fold_left (fun acc value ->
                      match acc, value with
                      | Some endpoints, `String
                          ("/messages" | "/chat/completions" | "/responses" as endpoint) ->
                          Some (if List.mem endpoint endpoints then endpoints
                            else endpoint :: endpoints)
                      | Some endpoints, `String _ -> Some endpoints
                      | _ -> None) (Some []) endpoints in
                    (match context_length, supported_endpoints with
                    | Some context_length, Some endpoints ->
                        let endpoints = List.rev endpoints in
                        if endpoints <> [] then
                          models := { id; name; context_length;
                            supported_endpoints = endpoints } :: !models;
                        true
                    | _ -> false)
                | _ -> false)
            | _ -> false) rows in
          if valid && not !duplicate then Ok (List.rev !models)
          else Error (Invalid_response "invalid Command Code model row")
      | Some (`String "list"), Some (`List _) ->
          Error (Invalid_response "too many Command Code models")
      | _ -> Error (Invalid_response "invalid Command Code model listing"))
  | _ -> Error (Invalid_response "malformed Command Code model listing")

(* The supplied HTTP executor must use HTTPS, disable redirects, and cap downloads.
   The catalog can be fetched anonymously, but passing the Studio key mirrors
   inference entitlements. An authenticated GET does NOT prove that key works. *)
let discover ~http ~api_key () =
  if not (valid_key api_key) then Error Invalid_credential
  else match http ~url:models_url
    ~headers:["Authorization", "Bearer " ^ api_key;
      "Accept", "application/json"] with
  | Error failure -> Error failure
  | Ok (status, _) when status < 200 || status >= 300 -> Error (Http_error status)
  | Ok (_, body) when String.length body > max_response_bytes ->
      Error (Invalid_response "listing exceeds size limit")
  | Ok (_, body) -> parse_models body

let invalid detail =
  raise (Protocol.Invalid_response ("invalid Command Code response: " ^ detail))
let member = Protocol.member

let state ~route ~model fields =
  `Assoc (["provider", `String "commandcode";
    "route", `String route; "model", `String model] @ fields)

let state_fields ~route ~model = function
  | `Assoc (("provider", `String "commandcode") ::
      ("route", `String saved_route) ::
      ("model", `String saved_model) :: fields)
    when saved_route = route && saved_model = model -> fields
  | _ -> invalid "assistant state belongs to another model or route"

let chat_reasoning = ["reasoning_content"; "reasoning_details"; "reasoning"]
let chat_fields fields =
  List.map (fun (key, value) ->
    let valid = match key, value with
      | ("reasoning_content" | "reasoning"), `String _ -> true
      | "reasoning_details", `List _ -> true
      | _ -> false in
    if not valid || not (List.mem key chat_reasoning) then
      invalid "invalid Chat reasoning state";
    key, value) fields

(* A tool result must follow the complete assistant tool-call turn. Preserve
   provider-specific reasoning verbatim when returned; do not synthesize it. *)
let chat_request ~model messages tools =
  let message (msg : Protocol.message) =
    let fields = match Protocol.message_to_json msg with
      | `Assoc fields -> fields
      | _ -> assert false in
    match msg.role, msg.provider_state with
    | "assistant", Some stored ->
        let extras = chat_fields (state_fields ~route:"chat" ~model stored) in
        let fields = if msg.content = None then fields @ ["content", `Null]
          else fields in
        `Assoc (fields @ extras)
    | "assistant", None when msg.content = None && msg.tool_calls <> [] ->
        `Assoc (fields @ ["content", `Null])
    | _, None -> `Assoc fields
    | _ -> invalid "non-assistant Chat reasoning state" in
  let fields = ["model", `String model;
    "messages", Protocol.chat_messages_to_json ~serialize:message messages;
    "stream", `Bool false] in
  `Assoc (if tools = [] then fields else fields @ ["tools", `List tools])

let parse_chat_completion ~model json =
  let reply = Protocol.parse_completion json in
  let message = match member "choices" json with
    | `List (choice :: _) -> member "message" choice
    | _ -> assert false in
  let fields = List.filter_map (fun name -> match member name message with
    | `Null -> None
    | value -> Some (name, value)) chat_reasoning |> chat_fields in
  if fields = [] then reply else
    { reply with provider_state = Some (state ~route:"chat" ~model fields) }

let validate_messages_blocks blocks =
  List.iter (fun block -> match member "type" block with
    | `String "thinking" ->
        (match member "thinking" block, member "signature" block with
        | `String _, `String signature when signature <> "" -> ()
        | _ -> invalid "unsigned Messages thinking block")
    | `String "redacted_thinking" ->
        (match member "data" block with
        | `String data when data <> "" -> ()
        | _ -> invalid "empty Messages redacted thinking block")
    | _ -> ()) blocks

let messages_request ~model ~max_tokens messages tools =
  let base = Anthropic_wire.request ~model ~max_tokens messages tools in
  let stored = List.filter_map (fun (msg : Protocol.message) ->
    match msg.role, msg.provider_state with
    | "assistant", provider_state -> Some (msg, provider_state)
    | _, None -> None
    | _ -> invalid "non-assistant Messages state") messages in
  let rec replay wire stored = match wire, stored with
    | [], [] -> []
    | (`Assoc _ as turn) :: tail, _
      when member "role" turn <> `String "assistant" ->
        turn :: replay tail stored
    | (`Assoc fields) :: tail, ((msg : Protocol.message), provider_state) :: rest ->
        let content = match provider_state with
          | None -> member "content" (`Assoc fields)
          | Some saved ->
              let blocks = match state_fields ~route:"messages" ~model saved with
                | ["content", `List blocks] -> blocks
                | _ -> invalid "malformed Messages assistant state" in
              validate_messages_blocks blocks;
              let decoded = Anthropic_wire.parse_response (`Assoc [
                "content", `List blocks;
                "stop_reason", `String
                  (if msg.tool_calls = [] then "end_turn" else "tool_use")]) in
              if decoded.content <> msg.content || decoded.tool_calls <> msg.tool_calls then
                invalid "Messages assistant state differs from stored turn";
              `List blocks in
        `Assoc (List.map (fun (key, value) ->
          if key = "content" then key, content else key, value) fields) :: replay tail rest
    | _ -> invalid "Messages transcript differs from serialized turns" in
  match base with
  | `Assoc fields ->
      `Assoc (List.map (fun (key, value) ->
        if key = "messages" then
          match value with `List wire -> key, `List (replay wire stored)
          | _ -> assert false
        else key, value) fields)
  | _ -> assert false

let parse_messages_completion ~model json =
  let reply = Anthropic_wire.parse_response json in
  let blocks = match member "content" json with `List blocks -> blocks | _ -> assert false in
  validate_messages_blocks blocks;
  if List.exists (fun block ->
    match member "type" block with
    | `String ("thinking" | "redacted_thinking") -> true
    | _ -> false) blocks then
    { reply with provider_state = Some (state ~route:"messages" ~model
        ["content", `List blocks]) }
  else reply

(* Responses stateless continuation carries complete original output, not just
   the public text and tool calls. Include encrypted reasoning so the upstream
   can verify the prior thought turn without server-side conversation storage. *)
let responses_request ~model messages tools =
  let base = Openai_responses_wire.request ~model messages tools in
  let input = match member "input" base with `List input -> input | _ -> assert false in
  let rec replace wire acc = function
    | [] -> if wire <> [] then invalid "unconsumed Responses transcript items"
        else List.rev acc
    | (msg : Protocol.message) :: rest ->
        let count = match msg.role with
          | "system" | "developer" -> 0
          | "assistant" -> (if msg.content = None then 0 else 1) +
              List.length msg.tool_calls
          | "user" | "tool" -> 1
          | _ -> invalid "unsupported Responses transcript role" in
        let rec take n consumed remaining =
          if n = 0 then List.rev consumed, remaining else
          match remaining with
          | item :: tail -> take (n - 1) (item :: consumed) tail
          | [] -> invalid "missing Responses transcript item" in
        let original, remaining = take count [] wire in
        let items = match msg.role, msg.provider_state with
          | "assistant", Some saved ->
              let output = match state_fields ~route:"responses" ~model saved with
                | ["output", `List output] -> output
                | _ -> invalid "malformed Responses assistant state" in
              let decoded = Openai_responses_wire.parse_completion
                (`Assoc ["status", `String "completed"; "output", `List output]) in
              if decoded.content <> msg.content || decoded.tool_calls <> msg.tool_calls then
                invalid "Responses assistant state differs from stored turn";
              List.iter (fun item -> if member "type" item = `String "reasoning" then
                match member "encrypted_content" item with
                | `String value when value <> "" -> ()
                | _ -> invalid "Responses reasoning lacks encrypted content") output;
              output
          | "assistant", None -> original
          | _, Some _ -> invalid "non-assistant Responses state"
          | _ -> original in
        replace remaining (List.rev_append items acc) rest in
  let input = replace input [] messages in
  match base with
  | `Assoc fields -> `Assoc (List.map (fun (key, value) ->
      if key = "input" then key, `List input else key, value) fields @ [
        "store", `Bool false;
        "include", `List [`String "reasoning.encrypted_content"]])
  | _ -> assert false

let parse_responses_completion ~model json =
  let reply = Openai_responses_wire.parse_completion json in
  (match member "model" json with
  | `String value when value = model -> ()
  | _ -> invalid "Responses model mismatch");
  let output = match member "output" json with `List output -> output | _ -> assert false in
  List.iter (fun item -> if member "type" item = `String "reasoning" then
    match member "encrypted_content" item with
    | `String value when value <> "" -> ()
    | _ -> invalid "Responses reasoning lacks encrypted content") output;
  { reply with provider_state = Some (state ~route:"responses" ~model
      ["output", `List output]) }
