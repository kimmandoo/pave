open Protocol

let invalid detail = raise (Invalid_response ("invalid Gemini response: " ^ detail))
let field = member
let required_string key json = match field key json with
  | `String value -> value
  | _ -> invalid ("missing or invalid " ^ key)
let usage json =
  let reported = field "usageMetadata" json in
  let thoughts = match field "thoughtsTokenCount" reported with
    | `Null -> Some 0
    | `Int count when count >= 0 -> Some count
    | _ -> None in
  match field "promptTokenCount" reported,
    field "candidatesTokenCount" reported, thoughts with
  | `Int input_tokens, `Int candidates, Some thoughts
    when input_tokens >= 0 && candidates >= 0 ->
      if thoughts > max_int - candidates then
        invalid "output token total exceeds host integer";
      Some { input_tokens; output_tokens = candidates + thoughts }
  | _ -> None

let call_sequence = ref 0
let next_call_id () =
  incr call_sequence;
  "gemini_call_" ^ string_of_int !call_sequence

let gemini_three model =
  let model = if String.starts_with ~prefix:"models/" model then
    String.sub model 7 (String.length model - 7) else model in
  String.starts_with ~prefix:"gemini-3" model
let part text = `Assoc [ "text", `String text ]
let content role parts = `Assoc [ "role", `String role; "parts", `List parts ]
let native_state ~model parts = `Assoc [
  "provider", `String "google"; "model", `String model; "parts", `List parts ]

let unique_fields fields =
  let names = List.map fst fields in
  if List.length names <> List.length (List.sort_uniq String.compare names) then
    invalid "duplicate native part field"

let has_signature parts =
  List.exists (function
    | `Assoc fields -> (match List.assoc_opt "thoughtSignature" fields with
        | Some (`String signature) -> signature <> ""
        | _ -> false)
    | _ -> false) parts

(* Google's function declarations reject several ordinary JSON Schema keywords.
   Local tool validation still applies the original schema; preserve numeric
   bounds as descriptions rather than sending unsupported protobuf fields. *)
let rec google_schema = function
  | `Assoc fields ->
      let bounds = ref [] in
      let fields = List.filter_map (fun (key, value) ->
        match key, value with
        | "additionalProperties", `Bool false -> None
        | ("minimum" | "maximum"), (`Int _ | `Float _) ->
            bounds := (key ^ ": " ^ Yojson.Basic.to_string value) :: !bounds;
            None
        | "properties", `Assoc properties ->
            Some (key, `Assoc (List.map (fun (name, value) ->
              name, google_schema value) properties))
        | "items", `Assoc _ -> Some (key, google_schema value)
        | ("type" | "description"), `String _ -> Some (key, value)
        | "required", `List values when List.for_all (function `String _ -> true | _ -> false) values ->
            Some (key, value)
        | "enum", `List values when List.for_all (function `String _ -> true | _ -> false) values ->
            Some (key, value)
        | "nullable", `Bool _ -> Some (key, value)
        | _ -> invalid ("unsupported function schema field: " ^ key)) fields in
      let fields = match List.rev !bounds with
        | [] -> fields
        | bounds ->
            let existing = match List.assoc_opt "description" fields with
              | None -> ""
              | Some (`String description) -> description
              | _ -> invalid "invalid function schema description" in
            let description = String.concat "; " (bounds @
              (if existing = "" then [] else [ existing ])) in
            ("description", `String description) :: List.remove_assoc "description" fields in
      `Assoc fields
  | _ -> invalid "function schema must be an object"

let tool_schema json =
  match field "type" json, field "function" json with
  | `String "function", (`Assoc _ as fn) ->
      let name = required_string "name" fn in
      if name = "" then invalid "empty function name";
      let schema = field "parameters" fn in
      (match field "type" schema with
       | `String "object" -> ()
       | _ -> invalid "function parameters must be an object schema");
      let description = match field "description" fn with
        | `String text -> text
        | `Null -> ""
        | _ -> invalid "invalid function description" in
      `Assoc [ "name", `String name; "description", `String description;
               "parametersJsonSchema", google_schema schema ]
  | _ -> invalid "unsupported tool definition"

let parse_parts parts =
  let texts = ref [] and calls = ref [] and ids = Hashtbl.create 4 in
  List.iter (fun json ->
    match json with
    | `Assoc fields ->
        unique_fields fields;
        List.iter (fun (name, _) ->
          if not (List.mem name [ "text"; "functionCall"; "thought"; "thoughtSignature" ]) then
            invalid ("unsupported content part field: " ^ name)) fields;
        if List.mem_assoc "text" fields && List.mem_assoc "functionCall" fields then
          invalid "ambiguous content part";
        (match field "thoughtSignature" json with
         | `Null | `String _ -> ()
         | _ -> invalid "invalid thought signature");
        if List.mem_assoc "thought" fields && field "thought" json <> `Bool false &&
           field "thought" json <> `Bool true then invalid "invalid thought flag";
        let thought = field "thought" json = `Bool true in
        (match List.assoc_opt "text" fields with
         | Some (`String text) when not thought -> texts := text :: !texts
         | Some (`String _) when thought -> ()
         | None -> ()
         | _ -> invalid "invalid text part");
        (match List.assoc_opt "functionCall" fields with
         | None -> ()
         | Some (`Assoc _ as fn) when not thought ->
             let name = required_string "name" fn in
             if name = "" then invalid "empty function name";
             (match fn with
              | `Assoc fields ->
                  unique_fields fields;
                  List.iter (fun (key, _) ->
                    if not (List.mem key [ "name"; "args"; "id" ]) then
                      invalid ("unsupported function call field: " ^ key)) fields
              | _ -> ());
             let arguments = match field "args" fn with
               | `Assoc _ as args -> args
               | `Null -> `Assoc []
               | _ -> invalid "function arguments must be an object" in
             let id = match field "id" fn with
               | `Null -> next_call_id ()
               | `String id when id <> "" -> id
               | _ -> invalid "invalid function call id" in
             if Hashtbl.mem ids id then invalid "duplicate function call id";
             Hashtbl.add ids id ();
             calls := { id; name; arguments } :: !calls
         | _ -> invalid "invalid function call part");
        if not (List.mem_assoc "text" fields || List.mem_assoc "functionCall" fields) then
          invalid "unsupported or empty content part"
    | _ -> invalid "invalid content part") parts;
  let content = match List.rev !texts with [] -> None | texts -> Some (String.concat "" texts) in
  content, List.rev !calls

let replay_parts ~model (msg : message) state =
  let parts = match state with
    | `Assoc fields ->
        unique_fields fields;
        if List.length fields <> 3 ||
           List.assoc_opt "provider" fields <> Some (`String "google") ||
           List.assoc_opt "model" fields <> Some (`String model) then
          invalid "foreign or malformed native state";
        (match List.assoc_opt "parts" fields with
         | Some (`List parts) -> parts
         | _ -> invalid "missing native parts")
    | _ -> invalid "invalid native state" in
  if not (has_signature parts) then invalid "native state lacks thought signature";
  let text, parsed_calls = parse_parts parts in
  if text <> msg.content then invalid "native text differs from assistant message";
  let native_calls = List.filter_map (fun part ->
    match field "functionCall" part with
    | `Assoc _ as fn ->
        Some (match field "id" fn with `String id -> Some id | _ -> None)
    | _ -> None) parts in
  if List.length parsed_calls <> List.length msg.tool_calls then
    invalid "native function calls differ from assistant message";
  List.iter2 (fun ((parsed : tool_call), native_id) (call : tool_call) ->
    if parsed.name <> call.name || parsed.arguments <> call.arguments ||
       (match native_id with Some id -> id <> call.id | None -> false) then
      invalid "native function calls differ from assistant message")
    (List.combine parsed_calls native_calls) msg.tool_calls;
  parts

let request ~model messages tools =
  if model = "" then invalid_arg "empty Gemini model";
  let systems = ref [] and contents = ref [] and pending = ref [] in
  let add message = contents := message :: !contents in
  let rec convert = function
    | [] -> if !pending <> [] then invalid "missing tool results"
    | (msg : message) :: rest ->
        (match msg.role with
         | "system" ->
             if !pending <> [] || msg.tool_calls <> [] || msg.tool_call_id <> None then
               invalid "system message during tool results";
             (match msg.content with
              | Some text -> systems := text :: !systems
              | None -> invalid "system message without content");
             convert rest
         | "user" ->
             if !pending <> [] || msg.tool_calls <> [] || msg.tool_call_id <> None then
               invalid "user message during tool results";
             (match msg.content with
              | Some text when text <> "" -> add (content "user" [ part text ])
              | _ -> invalid "empty user message");
             convert rest
         | "assistant" ->
             if !pending <> [] || msg.tool_call_id <> None then
               invalid "assistant message during tool results";
             let native_parts = match msg.provider_state with
               | Some state -> Some (replay_parts ~model msg state)
               | None -> None in
             if gemini_three model && msg.tool_calls <> [] && native_parts = None then
               invalid "Gemini 3 tool turns require native thought signatures";
             let ids = List.map (fun (call : tool_call) ->
               if call.id = "" || call.name = "" then invalid "empty function call id or name";
               (match call.arguments with `Assoc _ -> () | _ -> invalid "function arguments must be an object");
               call.id) msg.tool_calls in
             if List.length ids <> List.length (List.sort_uniq String.compare ids) then
               invalid "duplicate function call id";
             let text_parts = match msg.content with
               | Some text when text <> "" -> [ part text ]
               | _ -> [] in
             let calls = List.map (fun (call : tool_call) ->
               `Assoc [ "functionCall", `Assoc [
                 "name", `String call.name; "args", call.arguments ] ]) msg.tool_calls in
             let parts = match native_parts with
               | Some parts -> parts
               | None -> text_parts @ calls in
             if parts = [] then invalid "empty assistant message";
             add (content "model" parts);
             pending := List.map (fun (call : tool_call) -> call.id, call.name) msg.tool_calls;
             convert rest
         | "tool" ->
             (* Gemini 2 omits function IDs: response parts must follow call order,
                even if independent tools completed in a different order. *)
             let ordered_ids = List.map fst !pending in
             let rec collect parts = function
               | ({ role = "tool"; content = Some text; tool_call_id = Some id;
                    tool_calls = []; _ } : message) :: remaining ->
                   let name = match List.assoc_opt id !pending with
                     | Some name -> name
                     | None -> invalid "unexpected or duplicate tool result" in
                   pending := List.remove_assoc id !pending;
                   let response = [ "name", `String name;
                     "response", `Assoc [ "output", `String text ] ] in
                   collect ((id, `Assoc [ "functionResponse", `Assoc response ]) :: parts) remaining
               | ({ role = "tool"; _ } : message) :: _ -> invalid "malformed tool result"
               | remaining ->
                   if !pending <> [] then invalid "missing tool results";
                   add (content "user" (List.map (fun id -> List.assoc id parts) ordered_ids));
                   convert remaining
             in
             collect [] (msg :: rest)
         | _ -> invalid "unsupported transcript role")
  in
  convert messages;
  if !contents = [] then invalid "missing contents";
  let fields = [ "contents", `List (List.rev !contents) ] in
  let fields = match List.rev !systems with
    | [] -> fields
    | texts -> fields @ [ "systemInstruction", `Assoc [ "parts", `List (List.map part texts) ] ] in
  let fields = match tools with
    | [] -> fields
    | definitions -> fields @ [ "tools", `List [ `Assoc [
        "functionDeclarations", `List (List.map tool_schema definitions) ] ] ] in
  `Assoc fields

let parse_candidate ~model candidate =
  let finish = required_string "finishReason" candidate in
  if finish <> "STOP" then invalid ("generation finished with " ^ finish);
  (match field "index" candidate with
   | `Null | `Int 0 -> ()
   | _ -> invalid "unexpected candidate index");
  let parts = match field "content" candidate with
    | `Assoc _ as message ->
        (match field "role" message with
         | `Null | `String "model" -> ()
         | _ -> invalid "unexpected candidate role");
        (match field "parts" message with
         | `List parts -> parts
         | _ -> invalid "missing candidate parts")
    | _ -> invalid "missing candidate content" in
  let content, tool_calls = parse_parts parts in
  if (content = None || content = Some "") && tool_calls = [] then invalid "empty candidate";
  let signed = has_signature parts in
  if gemini_three model && tool_calls <> [] && not signed then
    invalid "Gemini 3 tool turn lacks native thought signature";
  { role = "assistant"; content; tool_calls; tool_call_id = None;
    provider_state = (if signed then Some (native_state ~model parts) else None) }

let parse_completion ~model json =
  (match field "error" json with
   | `Null -> ()
   | error ->
       let text = match field "message" error with `String text -> text | _ -> "API error" in
       invalid text);
  (match field "promptFeedback" json with
   | `Assoc _ as feedback when field "blockReason" feedback <> `Null -> invalid "prompt blocked"
   | _ -> ());
  match field "candidates" json with
  | `List [ candidate ] -> parse_candidate ~model candidate
  | _ -> invalid "missing or ambiguous candidates"
