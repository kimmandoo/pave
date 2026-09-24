type t = {
  on_text : string -> unit;
  content : Buffer.t;
  mutable text_seen : bool;
  mutable calls : Protocol.tool_call list;
  mutable signature_seen : bool;
  ids : (string, unit) Hashtbl.t;
  mutable finished : bool;
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
                  (match field "thoughtSignature" part with
                   | `String signature when signature <> "" ->
                       if t.calls <> [] then invalid "tool turn requires retained thought signature";
                       t.signature_seen <- true
                   | _ -> ());
                  if t.signature_seen && field "functionCall" part <> `Null then
                    invalid "tool turn requires retained thought signature") parts;
                List.iter (fun part ->
                  let text, calls = Gemini_wire.parse_parts [ part ] in
                  (match text with Some text -> append t text | None -> ());
                  List.iter (fun (call : Protocol.tool_call) ->
                    if List.length t.calls >= max_tool_calls then invalid "too many tool calls";
                    if Hashtbl.mem t.ids call.id then invalid "duplicate function call id";
                    Hashtbl.add t.ids call.id ();
                    t.calls <- call :: t.calls) calls) parts
            | _ -> invalid "missing candidate parts")
       | _ -> invalid "invalid candidate content");
      (match field "finishReason" candidate with
       | `Null -> ()
       | `String "STOP" -> t.finished <- true
       | `String reason -> invalid ("generation finished with " ^ reason)
       | _ -> invalid "invalid finish reason")
  | _ -> invalid "missing or ambiguous candidates"

let handle_event t event data =
  if t.finished then invalid "event after finish reason";
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
  handle_chunk t (parse_json data)

let create ~on_text =
  let t = { on_text; content = Buffer.create 256; text_seen = false;
    signature_seen = false; calls = []; ids = Hashtbl.create 4;
    finished = false; response_bytes = 0; parser = None } in
  t.parser <- Some (Sse.create ~on_event:(handle_event t));
  t

let feed t bytes =
  match t.parser with
  | Some parser -> Sse.feed parser bytes
  | None -> invalid "SSE parser was not initialized"

let is_done t = t.finished
let is_finished t = t.finished

let finish t =
  (match t.parser with
   | Some parser -> Sse.finish parser
   | None -> invalid "SSE parser was not initialized");
  if not t.finished then invalid "missing finish reason";
  if (not t.text_seen || Buffer.length t.content = 0) && t.calls = [] then
    invalid "empty response";
  { Protocol.role = "assistant";
    content = (if t.text_seen then Some (Buffer.contents t.content) else None);
    tool_calls = List.rev t.calls; tool_call_id = None; provider_state = None }
