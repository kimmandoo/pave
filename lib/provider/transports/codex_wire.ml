open Protocol

let invalid detail = raise (Invalid_response ("invalid Codex response: " ^ detail))

let required_string key json = match member key json with
  | `String text when text <> "" -> text
  | _ -> invalid ("missing or invalid " ^ key)

let assoc key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let valid_call_id id =
  let n = String.length id in
  n > 0 && n <= 64 && String.for_all (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' -> true
    | _ -> false) id

let check_call_id id =
  if not (valid_call_id id) then invalid "Codex call_id must be 1–64 ASCII letters, digits, '-' or '_'"
let wire_call_id id =
  if id = "" then invalid "empty tool call ID";
  let base = match String.index_opt id '|' , String.index_opt id '\n' with
    | None, None -> id
    | Some a, None | None, Some a -> String.sub id 0 a
    | Some a, Some b -> String.sub id 0 (min a b) in
  let base = if base = "" then id else base in
  if valid_call_id base then base
  else (
    let sanitized = String.map (function
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' as c -> c
      | _ -> '_') base in
    let rec trim_end n =
      if n > 0 && sanitized.[n - 1] = '_' then trim_end (n - 1) else n in
    let n = trim_end (String.length sanitized) in
    let stem = if n = 0 then "call" else String.sub sanitized 0 n in
    let digest = Digest.to_hex (Digest.string base) in
    let prefix = String.sub stem 0 (min (63 - String.length digest) (String.length stem)) in
    prefix ^ "_" ^ digest)


let tool_schema json =
  let fn = member "function" json in
  if member "type" json <> `String "function" then invalid "only function tools are supported";
  let name = required_string "name" fn in
  let parameters = member "parameters" fn in
  if member "type" parameters <> `String "object" then invalid "tool parameters must be an object schema";
  let parameters = match parameters with
    | `Assoc fields when not (List.mem_assoc "properties" fields) ->
        `Assoc (fields @ ["properties", `Assoc []])
    | `Assoc _ -> parameters
    | _ -> assert false in
  let fields = ["type", `String "function"; "name", `String name;
    "parameters", parameters] in
  let fields = match assoc "description" fn with
    | None -> fields @ ["description", `String ""]
    | Some (`String text) -> fields @ ["description", `String text]
    | _ -> invalid "invalid tool description" in
  let fields = match assoc "strict" fn with
    | None -> fields
    | Some (`Bool _ as value) -> fields @ ["strict", value]
    | _ -> invalid "invalid strict tool setting" in
  `Assoc fields


let parse_completion ~model json =
  if model = "" then invalid_arg "empty Codex model";
  (match member "model" json with
  | `Null -> ()
  | `String actual when actual = model -> ()
  | _ -> invalid "completion model mismatch");
  (match member "status" json with
  | `String "completed" -> ()
  | `String "incomplete" -> invalid "incomplete response (possibly truncated)"
  | `String "failed" -> invalid "failed response"
  | _ -> invalid "response not completed");
  (match member "error" json with `Null -> () | _ -> invalid "response error");
  (match member "incomplete_details" json with `Null -> () | _ -> invalid "incomplete response details");
  let outputs = match member "output" json with
    | `List outputs -> outputs
    | _ -> invalid "missing output items" in
  let texts = ref [] and calls = ref [] and ids = Hashtbl.create 4 in
  List.iter (fun item ->
    (match member "status" item with
    | `Null | `String "completed" -> ()
    | _ -> invalid "incomplete output item");
    match member "type" item with
    | `String "message" ->
        if member "role" item <> `String "assistant" then invalid "unexpected output role";
        (match member "phase" item with
        | `Null | `String "final_answer" | `String "commentary" -> ()
        | _ -> invalid "unsupported output phase");
        let parts = match member "content" item with
          | `List parts -> parts | _ -> invalid "missing message content" in
        List.iter (fun part -> match member "type" part with
          | `String "output_text" ->
              (match member "annotations" part with
              | `Null | `List [] -> () | _ -> invalid "unsupported output annotations");
              (match member "text" part with
              | `String text -> texts := text :: !texts
              | _ -> invalid "invalid output text")
          | `String "refusal" -> invalid "refusal"
          | _ -> invalid "unsupported message content") parts
    | `String "function_call" ->
        let id = required_string "call_id" item in
        check_call_id id;
        let name = required_string "name" item in
        if Hashtbl.mem ids id then invalid "duplicate tool call id";
        Hashtbl.add ids id ();
        let raw = required_string "arguments" item in
        let arguments = try Yojson.Basic.from_string raw
          with Yojson.Json_error _ -> invalid "invalid function arguments JSON" in
        (match arguments with `Assoc _ -> () | _ -> invalid "tool arguments must be an object");
        calls := { id; name; arguments } :: !calls
    | `String "reasoning" ->
        (match member "summary" item with
        | `Null -> ()
        | `List parts -> List.iter (fun part ->
            if member "type" part <> `String "summary_text" then
              invalid "unsupported reasoning summary part";
            match member "text" part with
            | `String _ -> ()
            | _ -> invalid "invalid reasoning summary text") parts
        | _ -> invalid "invalid reasoning summary");
        (match member "encrypted_content" item with
        | `Null | `String _ -> ()
        | _ -> invalid "invalid encrypted reasoning")
    | _ -> invalid "unsupported Codex output item") outputs;
  if !texts = [] && !calls = [] then invalid "completion without assistant output";
  { role = "assistant";
    content = (match List.rev !texts with [] -> None | texts -> Some (String.concat "" texts));
    tool_calls = List.rev !calls; tool_call_id = None;
    provider_state = Some (`Assoc ["provider", `String "openai-codex";
      "model", `String model; "output", `List outputs]) }

let replay_items ~model (msg : message) state =
  if member "provider" state <> `String "openai-codex" ||
     member "model" state <> `String model then
    invalid "opaque Codex output belongs to a different provider or model";
  let items = match member "output" state with
    | `List items -> items
    | _ -> invalid "invalid opaque Codex output" in
  let reconstructed = parse_completion ~model (`Assoc [
    "status", `String "completed"; "output", `List items ]) in
  if reconstructed.content <> msg.content ||
     reconstructed.tool_calls <> msg.tool_calls then
    invalid "opaque Codex output differs from assistant message";
  List.map (function
    | `Assoc fields -> `Assoc (List.remove_assoc "id" fields)
    | _ -> invalid "invalid opaque Codex output item") items

let request ~model messages tools =
  if model = "" then invalid_arg "empty Codex model";
  let input = ref [] and pending = ref [] and instructions = ref None
  and seen_input = ref false in
  let emit item = input := item :: !input; seen_input := true in
  let text_item role text = `Assoc ["role", `String role;
    "content", `List [`Assoc ["type", `String "input_text"; "text", `String text]]] in
  List.iter (fun (msg : message) ->
    match msg.role with
    | "system" ->
        if !seen_input || !instructions <> None || msg.tool_calls <> [] ||
           msg.tool_call_id <> None || msg.provider_state <> None
        then invalid "system instructions must precede input";
        (match msg.content with
        | Some text when text <> "" -> instructions := Some text
        | _ -> invalid "empty system instructions")
    | "developer" ->
        if !pending <> [] || msg.tool_calls <> [] || msg.tool_call_id <> None ||
           msg.provider_state <> None then
          invalid "developer message during tool results";
        (match msg.content with
        | Some text when text <> "" -> emit (text_item "developer" text)
        | _ -> invalid "empty developer message")
    | "user" ->
        if !pending <> [] || msg.tool_calls <> [] || msg.tool_call_id <> None ||
           msg.provider_state <> None then
          invalid "user message during tool results";
        (match msg.content with
        | Some text -> emit (text_item "user" text)
        | None -> invalid "user message without content")
    | "assistant" ->
        if !pending <> [] || msg.tool_call_id <> None then invalid "assistant during tool results";
        let native = match msg.provider_state with
          | None -> None
          | Some state -> Some (replay_items ~model msg state) in
        let calls = List.map (fun (call : tool_call) ->
          let id = match native with
            | None -> wire_call_id call.id
            | Some _ -> check_call_id call.id; call.id in
          if call.name = "" ||
             List.exists (fun (original, wire) -> original = call.id || wire = id) !pending then
            invalid "empty function name or duplicate tool call id";
          (match call.arguments with `Assoc _ -> () | _ -> invalid "tool arguments must be an object");
          call, id) msg.tool_calls in
        if List.length (List.sort_uniq String.compare (List.map snd calls)) <>
           List.length calls then invalid "duplicate normalized Codex call_id";
        (match native with
        | Some items -> List.iter emit items
        | None ->
            (match msg.content with
            | Some text -> emit (`Assoc ["role", `String "assistant"; "content", `String text])
            | None when calls = [] -> invalid "empty assistant message"
            | None -> ());
            List.iter (fun (call, id) ->
              emit (`Assoc ["type", `String "function_call"; "call_id", `String id;
                "name", `String call.name;
                "arguments", `String (Yojson.Basic.to_string call.arguments)])) calls);
        pending := List.map (fun (call, id) -> call.id, id) calls
    | "tool" ->
        (match msg.content, msg.tool_call_id, msg.tool_calls with
        | Some text, Some id, [] ->
            (match List.assoc_opt id !pending with
            | Some wire_id ->
                pending := List.filter (fun (original, _) -> original <> id) !pending;
                emit (`Assoc ["type", `String "function_call_output";
                  "call_id", `String wire_id; "output", `String text])
            | None -> invalid "unpaired or malformed tool result")
        | _ -> invalid "unpaired or malformed tool result")
    | _ -> invalid "unsupported transcript role") messages;
  if !pending <> [] then invalid "missing tool results";
  let fields = ["model", `String model; "input", `List (List.rev !input);
    "store", `Bool false; "stream", `Bool true;
    "include", `List [`String "reasoning.encrypted_content"]] in
  let fields = match !instructions with
    | None -> fields | Some text -> fields @ ["instructions", `String text] in
  let fields = if tools = [] then fields else
    fields @ ["tools", `List (List.map tool_schema tools)] in
  `Assoc fields
