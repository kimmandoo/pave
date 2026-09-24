open Protocol

let invalid detail = raise (Invalid_response ("invalid Codex stream: " ^ detail))
let field = member
let max_response_bytes = 16_777_216
let max_output_items = 128

let required name json = match field name json with
  | `String value when value <> "" -> value
  | _ -> invalid ("missing or invalid " ^ name)
let string name json = match field name json with
  | `String value -> value
  | _ -> invalid ("missing or invalid " ^ name)
let optional_index name json = match field name json with
  | `Null -> None
  | `Int n when n >= 0 && n < max_output_items -> Some n
  | _ -> invalid ("invalid " ^ name)
let optional_id name json = match field name json with
  | `Null -> None
  | `String id when id <> "" -> Some id
  | _ -> invalid ("invalid " ^ name)
let parse_json data = try Yojson.Basic.from_string data
  with Yojson.Json_error _ -> invalid "invalid SSE JSON"

type item = {
  id : string option;
  kind : string;
  call_id : string option;
  name : string option;
  text : Buffer.t;
  arguments : Buffer.t;
  mutable text_seen : bool;
  mutable args_seen : bool;
  mutable args_done : string option;
  mutable last_content_index : int;
  parts : (int, Buffer.t) Hashtbl.t;
  mutable done_output : Yojson.Basic.t option;
}

type t = {
  model : string;
  on_text : string -> unit;
  items : (int, item) Hashtbl.t;
  ids : (string, int) Hashtbl.t;
  mutable bytes : int;
  mutable response_id : string option;
  mutable completed : Protocol.message option;
  mutable usage : Protocol.usage option;
  mutable done_seen : bool;
  mutable parser : Sse.t option;
}

let check_id t id = match t.response_id, id with
  | None, Some id -> t.response_id <- Some id
  | Some first, Some id when first <> id -> invalid "response ID changed"
  | _ -> ()

let find t ?(kind = "") json =
  let index = optional_index "output_index" json in
  let id = optional_id "item_id" json in
  let by_id = match id with
    | None -> None
    | Some id -> (match Hashtbl.find_opt t.ids id with
      | Some n -> Some n | None -> invalid "unknown output item ID") in
  (match index, by_id with
  | Some a, Some b when a <> b -> invalid "output index / item ID mismatch"
  | _ -> ());
  let selected = match index, by_id with
    | Some n, _ | _, Some n -> n
    | None, None ->
        let matches = Hashtbl.fold (fun n item found ->
          if item.done_output <> None || (kind <> "" && item.kind <> kind) then found
          else n :: found) t.items [] in
        (match matches with
        | [n] -> n
        | [] -> invalid "output event without an open item"
        | _ -> invalid "ambiguous unkeyed output event") in
  let item = match Hashtbl.find_opt t.items selected with
    | Some item -> item | None -> invalid "output event before item added" in
  (match id, item.id with
  | Some a, Some b when a <> b -> invalid "output item ID mismatch"
  | Some _, None -> invalid "ID for an idless output item"
  | _ -> ());
  if kind <> "" && item.kind <> kind then invalid "output event for wrong item type";
  if item.done_output <> None then invalid "event after output item done";
  item

let handle_added t json =
  let index = match optional_index "output_index" json with
    | Some n -> n | None -> Hashtbl.length t.items in
  if Hashtbl.length t.items >= max_output_items then invalid "too many output items";
  if Hashtbl.mem t.items index then invalid "duplicate output index";
  let output = field "item" json in
  let id = optional_id "id" output in
  (match id with Some id when Hashtbl.mem t.ids id -> invalid "duplicate output item ID" | _ -> ());
  let kind = required "type" output in
  let call_id, name = match kind with
    | "message" ->
        if field "role" output <> `String "assistant" then invalid "unexpected message role";
        None, None
    | "function_call" ->
        let call_id = required "call_id" output in
        Codex_wire.check_call_id call_id;
        Some call_id, Some (required "name" output)
    | "reasoning" -> None, None
    | _ -> invalid "unsupported Codex output item" in
  let item = { id; kind; call_id; name; text = Buffer.create 128;
    arguments = Buffer.create 128; text_seen = false; args_seen = false;
    args_done = None; last_content_index = -1; parts = Hashtbl.create 2;
    done_output = None } in
  (match kind, field "arguments" output with
  | "function_call", `String args when args <> "" ->
      Buffer.add_string item.arguments args;
      item.args_seen <- true
  | "function_call", (`String "" | `Null) -> ()
  | "function_call", _ -> invalid "invalid function arguments"
  | _ -> ());
  Hashtbl.add t.items index item;
  (match id with Some id -> Hashtbl.add t.ids id index | None -> ())

let find_done t json =
  let output = field "item" json in
  let id = optional_id "id" output in
  let index = optional_index "output_index" json in
  (match id, index with
  | Some id, Some index ->
      (match Hashtbl.find_opt t.ids id with
      | Some n when n = index -> () | _ -> invalid "completed output ID / index mismatch")
  | Some id, None when not (Hashtbl.mem t.ids id) -> invalid "unknown completed output ID"
  | _ -> ());
  let kind = required "type" output in
  let item = match index, id with
    | Some n, _ -> find t (`Assoc ["output_index", `Int n])
    | None, Some id -> find t (`Assoc ["item_id", `String id])
    | None, None ->
        (match kind with
        | "function_call" ->
            let id = required "call_id" output in
            let matches = Hashtbl.fold (fun _ item found ->
              if item.done_output = None && item.call_id = Some id then item :: found
              else found) t.items [] in
            (match matches with [item] -> item | _ -> find t ~kind json)
        | _ -> find t ~kind json) in
  if item.kind <> kind || item.id <> id then invalid "completed output identity mismatch";
  item, output

let message_text output =
  if field "role" output <> `String "assistant" then invalid "unexpected message role";
  let parts = match field "content" output with
    | `List parts -> parts | _ -> invalid "missing message content" in
  String.concat "" (List.map (fun part -> match field "type" part with
    | `String "output_text" -> string "text" part
    | `String "refusal" -> invalid "refusal"
    | _ -> invalid "unsupported message content") parts)

let handle_done t json =
  let item, output = find_done t json in
  (match field "status" output with
  | `Null | `String "completed" -> ()
  | _ -> invalid "incomplete output item");
  (match item.kind with
  | "message" ->
      let text = message_text output in
      if item.text_seen then (
        if Buffer.contents item.text <> text then invalid "text delta / final item mismatch")
      else if text <> "" then t.on_text text
  | "function_call" ->
      if optional_id "call_id" output <> item.call_id ||
         optional_id "name" output <> item.name then
        invalid "function call changed between events";
      let args = string "arguments" output in
      if item.args_seen && Buffer.contents item.arguments <> args then
        invalid "arguments delta / final item mismatch";
      (match item.args_done with
      | Some prior when prior <> args -> invalid "arguments done / final item mismatch"
      | _ -> ());
      (match (try Yojson.Basic.from_string args with Yojson.Json_error _ ->
        invalid "invalid function arguments JSON") with
      | `Assoc _ -> () | _ -> invalid "tool arguments must be an object")
  | "reasoning" -> ()
  | _ -> assert false);
  item.done_output <- Some output

let handle_content t kind json =
  let item = find t ~kind:"message" json in
  let n = match optional_index "content_index" json with
    | None -> 0 | Some n -> n in
  (match kind with
  | "response.content_part.added" ->
      if Hashtbl.mem item.parts n then invalid "duplicate content part";
      let part = field "part" json in
      if field "type" part <> `String "output_text" then invalid "unsupported content part";
      let initial = string "text" part in
      let buffer = Buffer.create 128 in
      Buffer.add_string buffer initial;
      Hashtbl.add item.parts n buffer;
      if initial <> "" then (
        item.text_seen <- true;
        Buffer.add_string item.text initial;
        t.on_text initial)
  | "response.output_text.delta" ->
      if n < item.last_content_index then invalid "out-of-order content delta";
      item.last_content_index <- n;
      let delta = string "delta" json in
      (match Hashtbl.find_opt item.parts n with
      | Some part -> Buffer.add_string part delta
      | None when n = 0 ->
          let part = Buffer.create 128 in
          Buffer.add_string part delta;
          Hashtbl.add item.parts n part
      | None -> invalid "text delta without content part");
      item.text_seen <- true;
      Buffer.add_string item.text delta;
      if delta <> "" then t.on_text delta
  | "response.output_text.done" | "response.content_part.done" ->
      let text = if kind = "response.content_part.done" then (
        let part = field "part" json in
        if field "type" part <> `String "output_text" then invalid "unsupported content part";
        string "text" part) else string "text" json in
      (match Hashtbl.find_opt item.parts n with
      | Some prior when Buffer.contents prior <> text ->
          invalid "completed content part mismatch"
      | Some _ -> ()
      | None when n = 0 ->
          let part = Buffer.create (String.length text) in
          Buffer.add_string part text;
          Hashtbl.add item.parts n part;
          if text <> "" then (item.text_seen <- true; Buffer.add_string item.text text; t.on_text text)
      | None -> invalid "completed text without content part")
  | _ -> assert false)

let handle_arguments t ~done_event json =
  let item = find t ~kind:"function_call" json in
  if done_event then (
    if item.args_done <> None then invalid "duplicate arguments done";
    let args = string "arguments" json in
    (match field "name" json with
    | `Null -> () | `String name when Some name = item.name -> ()
    | _ -> invalid "function name mismatch");
    if item.args_seen && Buffer.contents item.arguments <> args then
      invalid "arguments delta / arguments done mismatch";
    item.args_done <- Some args)
  else (
    if item.args_done <> None then invalid "arguments after arguments done";
    item.args_seen <- true;
    Buffer.add_string item.arguments (string "delta" json))

let handle_completed t json =
  if t.completed <> None then invalid "duplicate response completion";
  let response = field "response" json in
  check_id t (optional_id "id" response);
  if field "status" response <> `String "completed" then invalid "response incomplete or failed";
  if field "error" response <> `Null || field "incomplete_details" response <> `Null then
    invalid "response error or truncation";
  let outputs = match field "output" response with
    | `Null ->
        let count = Hashtbl.length t.items in
        if count = 0 then invalid "completion without output items";
        List.init count (fun n -> match Hashtbl.find_opt t.items n with
          | Some { done_output = Some output; _ } -> output
          | Some _ -> invalid "completion with unfinished output item"
          | None -> invalid "non-contiguous output indices")
    | `List outputs when Hashtbl.length t.items = 0 ->
        if outputs = [] then invalid "completion without output items";
        outputs
    | `List outputs ->
        Hashtbl.iter (fun n item ->
          let candidate, expected = match List.nth_opt outputs n, item.done_output with
            | Some candidate, Some expected -> candidate, expected
            | _ -> invalid "missing completed output item" in
          if field "id" candidate <> field "id" expected ||
             field "type" candidate <> field "type" expected ||
             (match item.kind with
              | "message" -> message_text candidate <> message_text expected
              | "function_call" ->
                  field "call_id" candidate <> field "call_id" expected ||
                  field "name" candidate <> field "name" expected ||
                  field "arguments" candidate <> field "arguments" expected
              | _ ->
                  field "encrypted_content" candidate <> field "encrypted_content" expected ||
                  field "summary" candidate <> field "summary" expected)
          then invalid "completion output differs from item.done") t.items;
        if Hashtbl.length t.items <> List.length outputs then
          invalid "completion output count mismatch";
        outputs
    | _ -> invalid "invalid completion output" in
  let response = match response with
    | `Assoc fields -> `Assoc (("output", `List outputs) :: List.remove_assoc "output" fields)
    | _ -> invalid "missing response" in
  let result = Codex_wire.parse_completion ~model:t.model response in
  if Hashtbl.length t.items = 0 then
    List.iter (fun output ->
      if field "type" output = `String "message" then
        let text = message_text output in
        if text <> "" then t.on_text text) outputs;
  t.completed <- Some result;
  t.usage <- Openai_responses_wire.usage response;
  t.done_seen <- true

let handle_event t event data =
  if data = "[DONE]" then (
    if not t.done_seen then invalid "[DONE] before response completion")
  else (
    if t.done_seen then invalid "event after response completion";
    if String.length data > max_response_bytes - t.bytes then
      invalid "response exceeds 16 MiB";
    t.bytes <- t.bytes + String.length data;
    let json = parse_json data in
    let kind = match field "type" json, event with
    | `String kind, Some label when label <> "message" && label <> kind ->
        invalid "SSE event type mismatch"
    | `String kind, _ -> kind
    | _, Some "error" -> "error"
    | _ -> invalid "missing event type" in
    check_id t (optional_id "response_id" json);
    (match kind with
    | "response.created" | "response.in_progress" | "response.queued" ->
        check_id t (optional_id "id" (field "response" json))
    | "response.output_item.added" -> handle_added t json
    | "response.output_item.done" -> handle_done t json
    | "response.content_part.added" | "response.content_part.done" |
      "response.output_text.delta" | "response.output_text.done" -> handle_content t kind json
    | "response.refusal.delta" | "response.refusal.done" -> invalid "refusal"
    | "response.function_call_arguments.delta" -> handle_arguments t ~done_event:false json
    | "response.function_call_arguments.done" -> handle_arguments t ~done_event:true json
    | "response.reasoning_summary_part.added" | "response.reasoning_summary_part.done" |
      "response.reasoning_summary_text.delta" | "response.reasoning_summary_text.done" |
      "response.reasoning_text.delta" | "response.reasoning_text.done" ->
        ignore (find t ~kind:"reasoning" json)
    | "response.completed" | "response.done" -> handle_completed t json
    | "error" | "response.failed" | "response.incomplete" -> invalid "response failed or incomplete"
    | "response.metadata" -> ()
    | "response.output_text.annotation.added" ->
        invalid "unsupported Codex output annotation"
    | _ -> invalid "unknown Codex event"))

let create ~model ~on_text =
  if model = "" then invalid_arg "empty Codex model";
  let t = { model; on_text; items = Hashtbl.create 4; ids = Hashtbl.create 4;
    bytes = 0; response_id = None; completed = None; usage = None;
    done_seen = false; parser = None } in
  t.parser <- Some (Sse.create ~on_event:(handle_event t));
  t

let parser t = match t.parser with
  | Some parser -> parser
  | None -> invalid "SSE parser not initialized"
let feed t bytes = Sse.feed (parser t) bytes
let is_done t = t.done_seen
let is_finished t = t.completed <> None
let usage t = if t.completed <> None then t.usage else None
let finish t =
  Sse.finish (parser t);
  match t.completed with
  | Some response -> response
  | None -> invalid "missing response completion"
