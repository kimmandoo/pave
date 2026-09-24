type t = {
  model : string;
  on_text : string -> unit;
  content : Buffer.t;
  mutable text_seen : bool;
  mutable calls : Protocol.tool_call list;
  mutable call_count : int;
  mutable parts : Yojson.Basic.t list;
  mutable signature_seen : bool;
  signatures : (string, unit) Hashtbl.t;
  native_calls : (Yojson.Basic.t, unit) Hashtbl.t;
  ids : (string, unit) Hashtbl.t;
  mutable finished : bool;
  mutable usage : Protocol.usage option;
  mutable wire_bytes : int;
  mutable response_bytes : int;
  mutable parser : Sse.t option;
}

let invalid text = raise (Protocol.Invalid_response ("invalid Gemini stream: " ^ text))
let field = Protocol.member
let max_response_bytes = 16_777_216
let max_tool_calls = 128

let parse_json data = try Yojson.Basic.from_string data
  with Yojson.Json_error _ -> invalid "invalid SSE JSON"

let append t text =
  let length = String.length text in
  if length > max_response_bytes - t.response_bytes then invalid "response exceeds 16 MiB";
  t.response_bytes <- t.response_bytes + length;
  t.text_seen <- true;
  Buffer.add_string t.content text;
  if text <> "" then t.on_text text

let handle_chunk t json =
  (match field "error" json with
   | `Null -> ()
   | error ->
       let detail = match field "message" error with
         | `String text -> text
         | _ -> "API error" in
       invalid detail);
  (match field "promptFeedback" json with
   | `Assoc _ as feedback when field "blockReason" feedback <> `Null -> invalid "prompt blocked"
   | _ -> ());
  match field "candidates" json with
  | `List [] -> () (* A usage-only event has no candidate. *)
  | `List [ candidate ] ->
      (match field "index" candidate with
       | `Null | `Int 0 -> ()
       | _ -> invalid "unexpected candidate index");
      (match field "content" candidate with
       | `Null -> ()
       | `Assoc _ as content ->
           (match field "role" content with
            | `Null | `String "model" -> ()
            | _ -> invalid "unexpected candidate role");
           (match field "parts" content with
            | `List parts ->
                List.iter (fun part ->
                  let text, calls = Gemini_wire.parse_parts [ part ] in
                  (match text with Some text -> append t text | None -> ());
                  (match field "thoughtSignature" part with
                   | `String signature when signature <> "" ->
                       if Hashtbl.mem t.signatures signature then
                         invalid "repeated native thought signature";
                       Hashtbl.add t.signatures signature ();
                       t.signature_seen <- true
                   | _ -> ());
                  (match field "functionCall" part with
                   | `Assoc _ as fn when Gemini_wire.gemini_three t.model ->
                       if Hashtbl.mem t.native_calls fn then
                         invalid "repeated native function call";
                       Hashtbl.add t.native_calls fn ()
                   | _ -> ());
                  List.iter (fun (call : Protocol.tool_call) ->
                    if t.call_count >= max_tool_calls then invalid "too many tool calls";
                    if Hashtbl.mem t.ids call.id then invalid "duplicate function call id";
                    Hashtbl.add t.ids call.id ();
                    t.call_count <- t.call_count + 1;
                    t.calls <- call :: t.calls) calls;
                  t.parts <- part :: t.parts) parts
            | _ -> invalid "missing candidate parts")
       | _ -> invalid "invalid candidate content");
      (match field "finishReason" candidate with
       | `Null -> ()
       | `String "STOP" -> t.finished <- true
       | `String reason -> invalid ("generation finished with " ^ reason)
       | _ -> invalid "invalid finish reason")
  | _ -> invalid "missing or ambiguous candidates"

let handle_event t event data =
  (match event with
   | None | Some "message" -> ()
   | Some "error" ->
       let json = parse_json data in
       let detail = match field "message" json with
         | `String text -> text
         | _ -> (match field "error" json with
             | `Assoc _ as error -> (match field "message" error with
                 | `String text -> text | _ -> "API error")
             | _ -> "API error") in
       invalid detail
   | Some kind -> invalid ("unsupported SSE event: " ^ kind));
  let length = String.length data in
  if length > max_response_bytes - t.wire_bytes then
    invalid "response exceeds 16 MiB";
  t.wire_bytes <- t.wire_bytes + length;
  let json = parse_json data in
  if t.finished then (
    (match field "candidates" json with
     | `Null | `List [] -> ()
     | _ -> invalid "event after finish reason");
    if field "usageMetadata" json = `Null ||
       field "error" json <> `Null || t.usage <> None then
      invalid "event after finish reason";
    t.usage <- Gemini_wire.usage json)
  else (
    handle_chunk t json;
    if t.finished then t.usage <- Gemini_wire.usage json)

let create ~model ~on_text =
  if model = "" then invalid_arg "empty Gemini model";
  let t = { model; on_text; content = Buffer.create 256; text_seen = false;
    signature_seen = false; parts = []; native_calls = Hashtbl.create 4;
    signatures = Hashtbl.create 4;
    calls = []; call_count = 0; ids = Hashtbl.create 4;
    finished = false; usage = None;
    response_bytes = 0; wire_bytes = 0; parser = None } in
  t.parser <- Some (Sse.create ~on_event:(handle_event t));
  t

let feed t bytes =
  match t.parser with
  | Some parser -> Sse.feed parser bytes
  | None -> invalid "SSE parser was not initialized"

let is_done t = t.finished && t.usage <> None
let is_finished t = t.finished

let usage t = if t.finished then t.usage else None
let finish t =
  (match t.parser with
   | Some parser -> Sse.finish parser
   | None -> invalid "SSE parser was not initialized");
  if not t.finished then invalid "missing finish reason";
  if (not t.text_seen || Buffer.length t.content = 0) && t.calls = [] then
    invalid "empty response";
  if Gemini_wire.gemini_three t.model && t.calls <> [] && not t.signature_seen then
    invalid "Gemini 3 tool turn lacks native thought signature";
  { Protocol.role = "assistant";
    content = (if t.text_seen then Some (Buffer.contents t.content) else None);
    tool_calls = List.rev t.calls; tool_call_id = None;
    provider_state = (if t.signature_seen then
      Some (Gemini_wire.native_state ~model:t.model (List.rev t.parts))
      else None) }
