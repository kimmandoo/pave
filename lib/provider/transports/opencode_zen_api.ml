(* Zen API keys and endpoints are distinct from the Zen Go subscription.
   https://opencode.ai/docs/zen
   https://developers.openai.com/api/docs/guides/function-calling
   https://developers.openai.com/api/docs/guides/conversation-state
   The Zen catalog has no route/tool capability field: discovered IDs are
   unclassified. This module supports only the explicitly selected Responses
   route, never routing by an inferred model-name prefix. *)
let models_url = "https://opencode.ai/zen/v1/models"
let responses_url = "https://opencode.ai/zen/v1/responses"
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

let env_api_key () = match Sys.getenv_opt "OPENCODE_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> None

let responses_headers ~endpoint ~api_key =
  if endpoint <> responses_url then
    invalid_arg "OpenCode Zen key requires its pinned Responses endpoint";
  if not (valid_key api_key) then invalid_arg "invalid OpenCode Zen API key";
  ["Authorization: Bearer " ^ api_key]

let valid_id id = id <> "" && String.length id <= 256 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) id

(* Verified against the published GET endpoint: OpenAI list of model objects.
   It is publicly readable; availability on an individual account and tool
   support cannot be inferred from its rows. *)
let parse_models body =
  let json = try Some (Yojson.Basic.from_string body)
    with Yojson.Json_error _ -> None in
  match json with
  | Some (`Assoc fields) ->
      (match List.assoc_opt "object" fields, List.assoc_opt "data" fields with
      | Some (`String "list"), Some (`List rows) when List.length rows <= max_models ->
          let seen = Hashtbl.create (List.length rows) in
          let duplicate = ref false in
          let ids = ref [] in
          let valid = List.for_all (function
            | `Assoc fields ->
                (match List.assoc_opt "id" fields, List.assoc_opt "object" fields with
                | Some (`String id), Some (`String "model") when valid_id id ->
                    if Hashtbl.mem seen id then duplicate := true
                    else (
                      Hashtbl.add seen id ();
                      ids := id :: !ids);
                    true
                | _ -> false)
            | _ -> false) rows in
          if valid && not !duplicate then Ok (List.rev !ids)
          else Error (Invalid_response "invalid Zen model object")
      | Some (`String "list"), Some (`List _) ->
          Error (Invalid_response "too many Zen models")
      | _ -> Error (Invalid_response "invalid Zen model listing"))
  | _ -> Error (Invalid_response "malformed Zen model listing")

(* This GET is public, not account-scoped: never send a user's Zen key in a
   catalog request. The supplied HTTPS executor must cap responses and must
   not follow redirects. ~api_key retains the discovery callback signature. *)
let discover ~http ~api_key:_ () =
  match http ~url:models_url ~headers:["Accept", "application/json"] with
  | Error error -> Error error
  | Ok (status, _) when status < 200 || status >= 300 -> Error (Http_error status)
  | Ok (_, body) when String.length body > max_response_bytes ->
      Error (Invalid_response "listing exceeds size limit")
  | Ok (_, body) -> parse_models body

let invalid text = raise (Protocol.Invalid_response ("invalid Zen Responses: " ^ text))
let member = Protocol.member

(* The stateless Responses continuation must replay the ORIGINAL output items:
   in particular the opaque encrypted reasoning, in order, before function
   results. Reconstructing reasoning from text or the tool-call record loses
   the provider signature. Never silently continue with incomplete state. *)
let replay_items (msg : Protocol.message) output =
  let calls = List.filter_map (fun item -> match member "type" item with
    | `String "function_call" ->
        let arguments = match member "arguments" item with
          | `String json -> (try Yojson.Basic.from_string json
              with Yojson.Json_error _ -> invalid "malformed signed tool arguments")
          | _ -> invalid "missing signed tool arguments" in
        Some (member "call_id" item, member "name" item, arguments)
    | `String "reasoning" ->
        (match member "encrypted_content" item with
        | `String value when value <> "" -> ()
        | _ -> invalid "missing encrypted reasoning for stateless replay");
        None
    | `String "message" -> None
    | _ -> invalid "unsupported replay output item") output in
  let expected = List.map (fun (call : Protocol.tool_call) ->
    (`String call.id, `String call.name, call.arguments)) msg.tool_calls in
  if calls <> expected then invalid "tool calls differ from signed output";
  output

let request ~model messages tools =
  (* This canonical wire builder validates the full transcript, pending tool
     results and schema, and supplies native Responses function-call items. *)
  let base = Openai_responses_wire.request ~model messages tools in
  let input = match member "input" base with
    | `List input -> input | _ -> assert false in
  let rec replace wire acc = function
    | [] -> if wire <> [] then invalid "unconsumed transcript items" else List.rev acc
    | (msg : Protocol.message) :: rest ->
        let count = match msg.role with
          | "system" | "developer" -> 0
          | "assistant" -> (if msg.content = None then 0 else 1) + List.length msg.tool_calls
          | "user" | "tool" -> 1
          | _ -> invalid "unsupported transcript role" in
        let rec take n consumed remaining =
          if n = 0 then List.rev consumed, remaining else
          match remaining with
          | item :: rest -> take (n - 1) (item :: consumed) rest
          | [] -> invalid "missing transcript item" in
        let original, remaining = take count [] wire in
        let items = match msg.role, msg.provider_state with
          | "assistant", Some (`Assoc [
              "provider", `String "opencode-zen";
              "model", `String saved_model;
              "output", `List output]) when saved_model = model ->
              replay_items msg output
          | "assistant", Some _ -> invalid "malformed signed output"
          | _, Some _ -> invalid "signed output on non-assistant message"
          | _ -> original in
        replace remaining (List.rev_append items acc) rest in
  let input = replace input [] messages in
  match base with
  | `Assoc fields -> `Assoc (List.map (fun (key, value) ->
      if key = "input" then key, `List input else key, value) fields @ [
        "store", `Bool false; "include", `List [`String "reasoning.encrypted_content"]])
  | _ -> assert false

let parse_completion ~model json =
  let reply = Openai_responses_wire.parse_completion json in
  let output = match member "output" json with
    | `List output -> output | _ -> assert false in
  if reply.tool_calls <> [] ||
    List.exists (fun item -> member "type" item = `String "reasoning") output then (
    ignore (replay_items reply output);
    { reply with provider_state = Some (`Assoc [
      "provider", `String "opencode-zen"; "model", `String model;
      "output", `List output]) })
  else reply
