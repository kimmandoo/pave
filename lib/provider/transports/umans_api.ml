let chat_url = "https://api.code.umans.ai/v1/chat/completions"
let messages_url = "https://api.code.umans.ai/v1/messages"
let models_url = "https://api.code.umans.ai/v1/models/info"
let max_models_bytes = 1_048_576

let valid_key key = key <> "" && String.length key <= 8192 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) key

let chat_headers ~endpoint ~api_key =
  if endpoint <> chat_url then
    invalid_arg "Umans API key requires its pinned Chat endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Umans API key";
  ["Authorization: Bearer " ^ api_key]

let messages_headers ~endpoint ~api_key =
  if endpoint <> messages_url then
    invalid_arg "Umans API key requires its pinned Messages endpoint";
  if not (valid_key api_key) then invalid_arg "invalid Umans API key";
  ["x-api-key", api_key; "anthropic-version", "2023-06-01"]

let state_tag model = [
  "provider", `String "umans";
  "route", `String "chat";
  "model", `String model;
]

let matching_state model = function
  | Some (`Assoc fields)
    when List.for_all (fun (key, value) -> List.assoc_opt key fields = Some value)
      (state_tag model) -> Some fields
  | _ -> None

let valid_effort = function
  | "none" | "minimal" | "low" | "medium" | "high" | "max" -> true
  | _ -> false

let add_messages_reasoning_effort ~thinking body =
  match thinking, body with
  | None, _ -> body
  | Some level, `Assoc fields when valid_effort level ->
      `Assoc (fields @ ["reasoning_effort", `String level])
  | Some _, _ -> invalid_arg
      "Umans thinking level must be none, minimal, low, medium, high, or max"

let request ~model ?thinking messages tools =
  let serialize (message : Protocol.message) =
    match Protocol.message_to_json message with
    | `Assoc fields when message.role = "assistant" ->
        let fields = if message.content = None &&
            not (List.mem_assoc "content" fields) then
          fields @ ["content", `String ""] else fields in
        let fields = match matching_state model message.provider_state with
          | None -> fields
          | Some state ->
              (match List.assoc_opt "reasoning_content" state with
               | Some value -> fields @ ["reasoning_content", value]
               | None -> fields) in
        `Assoc fields
    | json -> json in
  let fields = ["model", `String model;
    "messages", Protocol.chat_messages_to_json ~serialize messages;
    "stream", `Bool false] in
  let fields = match thinking with
    | None -> fields
    | Some level when valid_effort level ->
        fields @ ["reasoning_effort", `String level]
    | Some _ -> invalid_arg
        "Umans thinking level must be none, minimal, low, medium, high, or max" in
  `Assoc (if tools = [] then fields else fields @ ["tools", `List tools])

let parse_completion json =
  let message = match Protocol.member "choices" json with
    | `List [choice] -> Protocol.member "message" choice
    | _ -> raise (Protocol.Invalid_response "missing Umans Chat message") in
  let fields = match Protocol.member "reasoning_content" message with
    | `Null -> []
    | (`String _ as value) -> ["reasoning_content", value]
    | _ -> raise (Protocol.Invalid_response "invalid Umans reasoning_content") in
  let reply = Protocol.parse_completion json in
  if fields = [] then reply
  else { reply with provider_state = Some (`Assoc (state_tag
    (match Protocol.member "model" json with
     | `String model when model <> "" -> model
     | _ -> raise (Protocol.Invalid_response "missing Umans response model")) @ fields)) }

type model = {
  id : string;
  display_name : string option;
  capabilities : Model_catalog.capabilities;
}

let invalid reason = Error reason
let field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None
let json_value = function
  | Some value -> value
  | None -> `Null
let unique_fields fields =
  let names = List.map fst fields in
  List.length names = List.length (List.sort_uniq String.compare names)
let valid_id id = id <> "" && String.length id <= 256 &&
  String.for_all (fun c -> Char.code c > 32 && Char.code c < 127) id
let text_value value = match value with
  | `String text when text <> "" && String.length text <= 256 &&
      not (String.exists (fun c -> Char.code c < 32 || Char.code c = 127) text) ->
      Some text
  | _ -> None
let optional_positive name json = match field name json with
  | None | Some `Null -> Ok None
  | Some (`Int value) when value > 0 -> Ok (Some value)
  | _ -> invalid ("invalid Umans " ^ name)
let optional_boolean name json = match field name json with
  | None | Some `Null -> Ok None
  | Some (`Bool value) -> Ok (Some value)
  | _ -> invalid ("invalid Umans " ^ name)

let parse_reasoning capabilities = match field "reasoning" capabilities with
  | None | Some `Null -> Ok None
  | Some (`Assoc fields as reasoning) when unique_fields fields ->
      (match field "supported" reasoning with
       | Some (`Bool false) | None -> Ok None
       | Some (`Bool true) ->
           (match field "levels" reasoning with
            | None | Some `Null -> Ok None
            | Some (`List levels) when List.length levels <= 16 ->
                let rec collect seen reversed = function
                  | [] -> Ok (Some (List.rev reversed))
                  | `String level :: rest when level <> "" &&
                      String.length level <= 32 &&
                      String.for_all (fun c ->
                        Char.code c > 32 && Char.code c < 127) level &&
                      not (List.mem level seen) ->
                      collect (level :: seen) (level :: reversed) rest
                  | _ -> invalid "invalid Umans reasoning levels" in
                collect [] [] levels
            | _ -> invalid "invalid Umans reasoning levels")
       | Some _ -> invalid "invalid Umans reasoning support flag")
  | Some _ -> invalid "invalid Umans reasoning metadata"
let parse_capabilities json = match json with
  | `Assoc fields when unique_fields fields ->
      (match optional_positive "context_window" json,
        optional_positive "max_completion_tokens" json,
        optional_boolean "supports_tools" json,
        optional_boolean "supports_vision" json,
        parse_reasoning json with
       | Ok context_window_tokens, Ok max_output_tokens, Ok tools,
           Ok vision, Ok effort_levels ->
           let input_modalities = Option.map (function
             | true -> [Model_catalog.Text; Model_catalog.Image]
             | false -> [Model_catalog.Text]) vision in
           Ok { Model_catalog.empty_capabilities with input_modalities; tools;
             context_window_tokens; max_output_tokens; effort_levels }
       | Error reason, _, _, _, _ | _, Error reason, _, _, _
       | _, _, Error reason, _, _ | _, _, _, Error reason, _
       | _, _, _, _, Error reason -> Error reason)
  | _ -> invalid "missing Umans capabilities object"

let parse_models body =
  if String.length body > max_models_bytes then invalid "Umans model info exceeds size limit"
  else
    let json = try Ok (Yojson.Basic.from_string body)
      with Yojson.Json_error _ -> invalid "malformed Umans model info JSON" in
    match json with
    | Error _ as error -> error
    | Ok (`Assoc fields) when unique_fields fields && List.length fields <= 4096 ->
        let rec collect seen reversed = function
          | [] -> Ok (List.rev reversed)
          | (id, (`Assoc model_fields as item)) :: rest
            when unique_fields model_fields && valid_id id ->
              if List.mem id seen then invalid "duplicate Umans model ID"
              else
                (match field "name" item, field "display_name" item,
                    field "capabilities" item with
                 | Some (`String name), display_name, Some capabilities
                   when name = id ->
                     (match parse_capabilities capabilities with
                      | Error _ as error -> error
                      | Ok capabilities ->
                          let display_name = match display_name with
                            | None | Some `Null -> None
                            | Some (`String value) -> text_value (`String value)
                            | _ -> None in
                          (match display_name, field "display_name" item with
                           | None, Some (`String _) ->
                               invalid "invalid Umans display_name"
                           | _ -> collect (id :: seen)
                               ({ id; display_name; capabilities } :: reversed) rest))
                 | _ -> invalid "invalid Umans model info entry")
          | _ :: _ -> invalid "invalid Umans model info entry" in
        collect [] [] fields
    | Ok (`Assoc _) -> invalid "duplicate or excessive Umans model entries"
    | Ok _ -> invalid "Umans model info must be an object"
