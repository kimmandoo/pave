(* First-party, pay-as-you-go Meta Model API (not a Meta AI consumer login).
   https://dev.meta.ai/docs/authentication
   https://dev.meta.ai/docs/pricing-rate-limits
   https://dev.meta.ai/docs/api-reference/models/list-models
   https://dev.meta.ai/docs/api-reference/responses/create-response
   https://dev.meta.ai/docs/protocols/responses#reasoning-items
   https://dev.meta.ai/docs/tool-calling#tool-calling-with-the-responses-api
   The model catalog includes models for other endpoints; choosing this explicitly
   pinned Responses route does not infer compatibility from an ID. *)
let models_url = "https://api.meta.ai/v1/models"
let responses_url = "https://api.meta.ai/v1/responses"
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

(* MODEL_API_KEY is the official SDK name; META_API_KEY is the Pave-specific
   alternative. Prefer the official name when both are valid. *)
let env_api_key () = match Sys.getenv_opt "MODEL_API_KEY" with
  | Some key when valid_key key -> Some key
  | _ -> (match Sys.getenv_opt "META_API_KEY" with
      | Some key when valid_key key -> Some key
      | _ -> None)

let responses_headers ~endpoint ~api_key =
  if endpoint <> responses_url then
    invalid_arg "Meta Model API key requires the pinned Responses endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Meta Model API key";
  ["Authorization: Bearer " ^ api_key]

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
          let ids = ref [] in
          let valid = List.for_all (function
            | `Assoc row ->
                (match List.assoc_opt "id" row, List.assoc_opt "object" row,
                  List.assoc_opt "created" row, List.assoc_opt "owned_by" row with
                | Some (`String id), Some (`String "model"),
                  Some (`Int created), Some (`String owner)
                  when valid_id id && created >= 0 && owner <> "" ->
                    if not (Hashtbl.mem seen id) then (
                      Hashtbl.add seen id ();
                      ids := id :: !ids);
                    true
                | _ -> false)
            | _ -> false) rows in
          if valid then Ok (List.rev !ids)
          else Error (Invalid_response "invalid Meta model object")
      | Some (`String "list"), Some (`List _) ->
          Error (Invalid_response "too many Meta models")
      | _ -> Error (Invalid_response "invalid Meta model listing"))
  | _ -> Error (Invalid_response "malformed Meta model listing")

(* The documented GET requires a team key. The supplied executor is pinned by
   the caller to HTTPS with redirects disabled and a bounded response body. *)
let discover ~http ~api_key () =
  if not (valid_key api_key) then Error Invalid_credential
  else match http ~url:models_url
    ~headers:["Authorization", "Bearer " ^ api_key;
      "Accept", "application/json"] with
  | Error error -> Error error
  | Ok (status, _) when status < 200 || status >= 300 -> Error (Http_error status)
  | Ok (_, body) when String.length body > max_response_bytes ->
      Error (Invalid_response "listing exceeds size limit")
  | Ok (_, body) -> parse_models body

let invalid text =
  raise (Protocol.Invalid_response ("invalid Meta Responses: " ^ text))
let member = Protocol.member

(* The signed output must be stored and replayed as received, in order. In
   particular, never reconstruct encrypted reasoning, tool-call arguments,
   output_text blocks, or the commentary phase from the lossy public message. *)
let validate_reasoning output =
  List.iter (fun item -> if member "type" item = `String "reasoning" then (
    match member "id" item, member "summary" item,
      member "encrypted_content" item with
    | `String id, `List _, `String ciphertext
      when id <> "" && ciphertext <> "" -> ()
    | _ -> invalid "missing encrypted reasoning for stateless replay")) output

let replay_items (msg : Protocol.message) output =
  let decoded = Openai_responses_wire.parse_completion
    (`Assoc ["status", `String "completed"; "output", `List output]) in
  if decoded.content <> msg.content || decoded.tool_calls <> msg.tool_calls then
    invalid "stored assistant differs from signed output";
  validate_reasoning output;
  output

let request ~model messages tools =
  let base = Openai_responses_wire.request ~model messages tools in
  let input = match member "input" base with `List input -> input | _ -> assert false in
  let rec replace wire acc = function
    | [] -> if wire <> [] then invalid "unconsumed transcript items" else List.rev acc
    | (msg : Protocol.message) :: rest ->
        let count = match msg.role with
          | "system" | "developer" -> 0
          | "assistant" -> (if msg.content = None then 0 else 1) +
              List.length msg.tool_calls
          | "user" | "tool" -> 1
          | _ -> invalid "unsupported transcript role" in
        let rec take n consumed remaining =
          if n = 0 then List.rev consumed, remaining else
          match remaining with
          | item :: tail -> take (n - 1) (item :: consumed) tail
          | [] -> invalid "missing transcript item" in
        let original, remaining = take count [] wire in
        let items = match msg.role, msg.provider_state with
          | "assistant", Some (`Assoc ["provider", `String "meta";
              "model", `String saved_model; "output", `List output])
              when saved_model = model -> replay_items msg output
          | "assistant", Some _ -> invalid "wrong or malformed signed output"
          | _, Some _ -> invalid "signed output on non-assistant message"
          | _ -> original in
        replace remaining (List.rev_append items acc) rest in
  let input = replace input [] messages in
  match base with
  | `Assoc fields -> `Assoc (List.map (fun (key, value) ->
      if key = "input" then key, `List input else key, value) fields @ [
        "store", `Bool false;
        "include", `List [`String "reasoning.encrypted_content"]])
  | _ -> assert false

let parse_completion ~model json =
  let reply = Openai_responses_wire.parse_completion json in
  (match member "model" json with
  | `String actual when actual = model -> ()
  | `String _ -> invalid "response model mismatch"
  | _ -> invalid "missing response model");
  let output = match member "output" json with
    | `List output -> output | _ -> assert false in
  if reply.tool_calls <> [] ||
    List.exists (fun item -> member "type" item = `String "reasoning") output then (
    validate_reasoning output;
    { reply with provider_state = Some (`Assoc [
        "provider", `String "meta"; "model", `String model;
        "output", `List output]) })
  else reply
