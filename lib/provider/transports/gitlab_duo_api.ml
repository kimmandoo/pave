(* GitLab Duo Non-Agentic: first exchange a GitLab OAuth/PAT bearer at the
   GitLab REST endpoint, then send the resulting account-scoped Direct Access
   token to GitLab's AI Gateway. Model routing is explicit: the account/model
   catalog selects an upstream model ID and one of three native wire protocols.
   GitLab Duo Agent uses a separate stateful WebSocket protocol and MUST NOT be
   wired to this HTTP transport. *)
let gitlab_url = "https://gitlab.com"
let direct_access_url = gitlab_url ^ "/api/v4/ai/third_party_agents/direct_access"
let account_url = gitlab_url ^ "/api/v4/user"
let anthropic_url = "https://cloud.gitlab.com/ai/v1/proxy/anthropic/v1/messages"
let responses_url = "https://cloud.gitlab.com/ai/v1/proxy/openai/v1/responses"
let completions_url = "https://cloud.gitlab.com/ai/v1/proxy/openai/v1/chat/completions"
let max_response_bytes = 1_048_576

type error =
  | Invalid_credential
  | Http_error of int
  | Invalid_response of string
  | Transport_error

type http = url:string -> headers:(string * string) list -> body:Yojson.Basic.t ->
  (int * string, error) result
type get = url:string -> headers:(string * string) list ->
  (int * string, error) result

let member = Protocol.member
let string = function `String s -> s | _ -> ""
let safe_text ?(max_length=8192) s = s <> "" && String.length s <= max_length &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) s
let credential () = match Sys.getenv_opt "GITLAB_TOKEN" with
  | Some token when safe_text token -> Some token
  | _ -> None
let json_body body =
  if String.length body > max_response_bytes then Error (Invalid_response "oversized GitLab response")
  else try Ok (Yojson.Basic.from_string body)
    with Yojson.Json_error _ -> Error (Invalid_response "malformed GitLab JSON")
let post ~http ~url ~bearer body =
  if not (safe_text bearer) then Error Invalid_credential else
  match http ~url ~headers:["Authorization", "Bearer " ^ bearer;
    "Content-Type", "application/json"] ~body with
  | Error error -> Error error
  | Ok (status, _) when status < 200 || status >= 300 -> Error (Http_error status)
  | Ok (_, text) -> json_body text
type account = { id : int; username : string }
let discover_account ~get ~bearer () =
  if not (safe_text bearer) then Error Invalid_credential
  else match get ~url:account_url
    ~headers:["Authorization", "Bearer " ^ bearer;
      "Accept", "application/json"] with
  | Error error -> Error error
  | Ok (status, _) when status < 200 || status >= 300 -> Error (Http_error status)
  | Ok (_, body) ->
      (match json_body body with
       | Error error -> Error error
       | Ok json ->
           (match member "id" json, member "username" json with
            | `Int id, `String username
              when id > 0 && safe_text ~max_length:256 username ->
                Ok { id; username }
            | _ -> Error (Invalid_response "missing GitLab account identity")))

(* The Direct Access response's headers are account-bound. They must only be
   attached to the pinned Gateway endpoints; they must never replace the token
   exchange's Authorization, inject new HTTP headers, or leak to another host. *)
type access = { token : string; headers : (string * string) list }
let valid_header_name name = name <> "" && String.length name <= 128 &&
  String.for_all (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' -> true
    | _ -> false) name
let parse_headers = function
  | `Assoc pairs when List.length pairs <= 64 ->
      let names = Hashtbl.create (List.length pairs) in
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | (name, `String value) :: rest
          when valid_header_name name && String.length value <= 8192 &&
            String.for_all (fun c -> Char.code c >= 32 && Char.code c < 127) value &&
            not (Hashtbl.mem names (String.lowercase_ascii name)) &&
            not (List.mem (String.lowercase_ascii name)
              ["authorization"; "proxy-authorization"; "cookie"; "host";
                "content-type"; "content-length"; "transfer-encoding"; "connection"]) ->
            Hashtbl.add names (String.lowercase_ascii name) ();
            loop ((name, value) :: acc) rest
        | _ -> Error (Invalid_response "invalid Direct Access header") in
      loop [] pairs
  | _ -> Error (Invalid_response "missing Direct Access headers")
let parse_access json =
  let token = string (member "token" json) in
  if not (safe_text token) then Error (Invalid_response "missing Direct Access token")
  else match parse_headers (member "headers" json) with
    | Error error -> Error error
    | Ok headers -> Ok { token; headers }
let direct_access ~http ~bearer () =
  match post ~http ~url:direct_access_url ~bearer
    (`Assoc ["feature_flags", `Assoc ["DuoAgentPlatformNext", `Bool true]]) with
  | Error error -> Error error
  | Ok json -> parse_access json

type route = Anthropic | Openai_responses | Openai_completions
let endpoint = function
  | Anthropic -> anthropic_url
  | Openai_responses -> responses_url
  | Openai_completions -> completions_url
let gateway_header_pairs ~endpoint:url access =
  if not (List.mem url [anthropic_url; responses_url; completions_url]) then
    invalid_arg "GitLab Direct Access token restricted to the GitLab AI Gateway";
  if not (safe_text access.token) then invalid_arg "invalid GitLab Direct Access token";
  let headers = match parse_headers (`Assoc (List.map (fun (k,v) -> k, `String v) access.headers)) with
    | Ok headers -> headers
    | Error _ -> invalid_arg "invalid GitLab Direct Access headers" in
  let version = if url = anthropic_url &&
      not (List.exists (fun (key, _) ->
        String.lowercase_ascii key = "anthropic-version") headers)
    then ["anthropic-version", "2023-06-01"] else [] in
  ("Authorization", "Bearer " ^ access.token) :: headers @ version
let gateway_headers ~endpoint:url access =
  List.map (fun (key, value) -> key ^ ": " ^ value)
    (gateway_header_pairs ~endpoint:url access)
let invalid text =
  raise (Protocol.Invalid_response ("invalid GitLab Duo signed output: " ^ text))
let validate_thinking content =
  List.iter (fun block ->
    match member "type" block with
    | `String "thinking" ->
        (match member "thinking" block, member "signature" block with
         | `String _, `String signature when signature <> "" -> ()
         | _ -> invalid "missing Anthropic thinking signature")
    | `String "redacted_thinking" ->
        (match member "data" block with
         | `String data when data <> "" -> ()
         | _ -> invalid "missing Anthropic redacted thinking data")
    | _ -> ()) content
let validate_reasoning output =
  List.iter (fun item ->
    if member "type" item = `String "reasoning" then
      match member "id" item, member "summary" item,
        member "encrypted_content" item with
      | `String id, `List _, `String ciphertext
        when id <> "" && ciphertext <> "" -> ()
      | _ -> invalid "missing encrypted Responses reasoning") output
let replay_anthropic (msg : Protocol.message) content =
  validate_thinking content;
  let decoded = Anthropic_wire.parse_response (`Assoc [
    "stop_reason", `String
      (if msg.tool_calls = [] then "end_turn" else "tool_use");
    "content", `List content]) in
  if decoded.content <> msg.content || decoded.tool_calls <> msg.tool_calls then
    invalid "Anthropic assistant differs from signed blocks";
  `List content
let replay_responses (msg : Protocol.message) output =
  validate_reasoning output;
  let decoded = Openai_responses_wire.parse_completion (`Assoc [
    "status", `String "completed"; "output", `List output]) in
  if decoded.content <> msg.content || decoded.tool_calls <> msg.tool_calls then
    invalid "Responses assistant differs from signed items";
  output
let replay_anthropic_messages ~model messages base =
  let assistants = List.filter (fun (msg : Protocol.message) ->
    msg.role = "assistant") messages in
  let wire = match member "messages" base with
    | `List wire -> wire | _ -> assert false in
  let rec replace assistants = function
    | [] when assistants = [] -> []
    | [] -> invalid "missing Anthropic assistant"
    | item :: rest when member "role" item = `String "assistant" ->
        (match assistants with
         | [] -> invalid "unexpected Anthropic assistant"
         | (msg : Protocol.message) :: remaining ->
             let item = match msg.provider_state, item with
               | None, _ -> item
               | Some (`Assoc ["provider", `String "gitlab-duo";
                   "route", `String "anthropic"; "model", `String saved;
                   "content", `List content]), `Assoc fields
                   when saved = model ->
                     let raw = replay_anthropic msg content in
                     `Assoc (List.map (fun (key, value) ->
                       key, if key = "content" then raw else value) fields)
               | _ -> invalid "wrong or malformed Anthropic signed state" in
             item :: replace remaining rest)
    | item :: rest -> item :: replace assistants rest in
  let wire = replace assistants wire in
  match base with
  | `Assoc fields -> `Assoc (List.map (fun (key, value) ->
      key, if key = "messages" then `List wire else value) fields)
  | _ -> assert false
let replay_responses_input ~model messages base =
  let wire = match member "input" base with
    | `List wire -> wire | _ -> assert false in
  let rec take n acc = function
    | rest when n = 0 -> List.rev acc, rest
    | item :: rest -> take (n-1) (item :: acc) rest
    | [] -> invalid "missing Responses transcript item" in
  let rec replace wire acc = function
    | [] -> if wire <> [] then invalid "unconsumed Responses input" else List.rev acc
    | (msg : Protocol.message) :: remaining ->
        let count = match msg.role with
          | "system" | "developer" -> 0
          | "assistant" -> (if msg.content = None then 0 else 1) +
              List.length msg.tool_calls
          | "user" | "tool" -> 1
          | _ -> invalid "unsupported Responses role" in
        let original, wire = take count [] wire in
        let items = match msg.role, msg.provider_state with
          | "assistant", Some (`Assoc ["provider", `String "gitlab-duo";
              "route", `String "responses"; "model", `String saved;
              "output", `List output]) when saved = model ->
                replay_responses msg output
          | "assistant", Some _ -> invalid "wrong or malformed Responses signed state"
          | _, Some _ -> invalid "signed state on non-assistant message"
          | _ -> original in
        replace wire (List.rev_append items acc) remaining in
  let input = replace wire [] messages in
  match base with
  | `Assoc fields -> `Assoc (List.map (fun (key, value) ->
      key, if key = "input" then `List input else value) fields @ [
        "store", `Bool false;
        "include", `List [`String "reasoning.encrypted_content"]])
  | _ -> assert false

let request ?(stream=false) ~route ~model ~max_tokens messages tools =
  if not (safe_text ~max_length:256 model) then invalid_arg "invalid GitLab upstream model";
  List.iter (fun (msg : Protocol.message) ->
    if msg.role <> "assistant" && msg.provider_state <> None then
      invalid "signed state on non-assistant message") messages;
  let base = match route with
    | Anthropic -> Anthropic_wire.request ~model ~max_tokens messages tools
    | Openai_responses ->
        Openai_responses_wire.request ~stream ~model messages tools
    | Openai_completions ->
        if List.exists (fun (msg : Protocol.message) ->
          msg.provider_state <> None) messages then
          invalid "signed state on Chat Completions route";
        let fields = ["model", `String model;
          "messages", `List (List.map Protocol.message_to_json messages)] in
        let fields = if tools = [] then fields else fields @ ["tools", `List tools] in
        `Assoc (if stream then fields @ ["stream", `Bool true] else fields) in
  match route with
  | Anthropic ->
      let replayed = replay_anthropic_messages ~model messages base in
      if not stream then replayed else
      (match replayed with `Assoc fields -> `Assoc (fields @ ["stream", `Bool true])
      | _ -> assert false)
  | Openai_responses -> replay_responses_input ~model messages base
  | Openai_completions -> base
let parse_completion ~route ~model json =
  if not (safe_text ~max_length:256 model) then invalid_arg "invalid GitLab upstream model";
  (match member "model" json with
   | `String actual when actual <> model -> invalid "response model mismatch"
   | _ -> ());
  match route with
  | Anthropic ->
      let answer = Anthropic_wire.parse_response json in
      let content = match member "content" json with
        | `List blocks -> blocks | _ -> assert false in
      if List.exists (fun block -> List.mem (member "type" block)
        [`String "thinking"; `String "redacted_thinking"]) content then (
        validate_thinking content;
        { answer with provider_state = Some (`Assoc [
          "provider", `String "gitlab-duo";
          "route", `String "anthropic";
          "model", `String model;
          "content", `List content]) })
      else answer
  | Openai_responses ->
      let answer = Openai_responses_wire.parse_completion json in
      let output = match member "output" json with
        | `List output -> output | _ -> assert false in
      if answer.tool_calls <> [] ||
        List.exists (fun item -> member "type" item = `String "reasoning") output then (
        validate_reasoning output;
        { answer with provider_state = Some (`Assoc [
          "provider", `String "gitlab-duo";
          "route", `String "responses";
          "model", `String model;
          "output", `List output]) })
      else answer
  | Openai_completions -> Protocol.parse_completion json

(* A complete non-streaming turn, including the account-bound token exchange.
   The supplied HTTP executor must use HTTPS only, disable redirects, and bound
   response downloads. Streaming callers use endpoint/gateway_headers/request
   with the corresponding Anthropic or OpenAI SSE decoder instead. *)
let complete ~http ~bearer ~route ~model ~max_tokens messages tools =
  match direct_access ~http ~bearer () with
  | Error error -> Error error
  | Ok access ->
      let url = endpoint route in
      let body = request ~route ~model ~max_tokens messages tools in
      let headers = ("Content-Type", "application/json") ::
        gateway_header_pairs ~endpoint:url access in
      (match http ~url ~headers ~body with
       | Error error -> Error error
       | Ok (status, _) when status < 200 || status >= 300 -> Error (Http_error status)
       | Ok (_, text) ->
           (match json_body text with
            | Error error -> Error error
            | Ok json ->
                try Ok (parse_completion ~route ~model json)
                with Protocol.Invalid_response reason -> Error (Invalid_response reason)))

