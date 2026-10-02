module P = Pave.Protocol

let assistant ?(content = "") ?(state = None) calls =
  { P.role = "assistant";
    content = (if content = "" then None else Some content);
    tool_result_content = None; tool_calls = calls;
    tool_call_id = None; provider_state = state; attachments = [] }

let call ?(id = "call-1") ?(name = "lookup") () : P.tool_call =
  { P.id; name; arguments = `Assoc ["q", `String "x"] }

let result ?(id = "call-1") text =
  P.tool_result_blocks id [P.Text text]

let user text = P.user text

let ids msgs =
  List.filter_map (fun (m : P.message) -> m.tool_call_id) msgs

let call_ids msgs =
  List.concat_map (fun (m : P.message) ->
    List.map (fun (c : P.tool_call) -> c.id) m.tool_calls) msgs

let texts msgs =
  List.filter_map (fun (m : P.message) -> m.content) msgs

(* 1. Clean history is unchanged. *)
let () =
  let msgs = [ user "a"; assistant [call ()]; result "found"; user "b" ] in
  assert (P.sanitize_messages msgs = msgs)

(* 2. Malformed call (empty id) drops the call and its result; a contentless
      assistant turn disappears entirely. *)
let () =
  let msgs = [ user "a";
    assistant [call ~id:"" ~name:"lookup" ()];
    result ~id:"" "oops" ] in
  assert (P.sanitize_messages msgs = [ user "a" ])

(* 3. Malformed name drops just the call; sibling content keeps the message. *)
let () =
  let msgs = [ user "a";
    assistant ~content:"partial" [call ~id:"x" ~name:"" ()];
    result ~id:"x" "oops" ] in
  let out = P.sanitize_messages msgs in
  assert (call_ids out = [] && ids out = [] && texts out = ["a"; "partial"])

(* 4. Missing result gets a synthetic conservative one. *)
let () =
  let msgs = [ user "a"; assistant [call ()]; user "b" ] in
  let out = P.sanitize_messages msgs in
  assert (ids out = [ "call-1" ]);
  assert (texts out |> List.exists (fun t -> t = "No result provided"))

(* 5. Duplicate call ids get _dup names; results map in order. *)
let () =
  let msgs = [ user "a";
    assistant [call ~id:"call-9" ()];
    result ~id:"call-9" "first";
    user "b";
    assistant [call ~id:"call-9" ()];
    result ~id:"call-9" "second" ] in
  let out = P.sanitize_messages msgs in
  assert (call_ids out = ["call-9"; "call-9_dup1"]);
  assert (ids out = ["call-9"; "call-9_dup1"]);
  assert (texts out = ["a"; "first"; "b"; "second"])

(* 6. Orphan result (no matching call) is dropped. *)
let () =
  let msgs = [ user "a"; result ~id:"ghost" "stale"; user "b" ] in
  assert (P.sanitize_messages msgs = [ user "a"; user "b" ])

(* 7. A result that arrives before a boundary still reaches its call
      (pulled forward). *)
let () =
  let msgs = [ user "a";
    assistant [call ~id:"one" (); call ~id:"two" ()];
    result ~id:"one" "r1"; result ~id:"two" "r2"; user "b" ] in
  let out = P.sanitize_messages msgs in
  assert (ids out = ["one"; "two"])

(* 8. A straggler result after the boundary is pulled into position. *)
let () =
  let msgs = [ user "a"; assistant [call ~id:"one" ()];
    user "interjected"; result ~id:"one" "r1"; user "b" ] in
  let out = P.sanitize_messages msgs in
  (* result pulled forward to sit right after its call *)
  let positions = List.mapi (fun i (m : P.message) -> i, m.role) out in
  assert (List.map snd positions =
    ["user"; "assistant"; "tool"; "user"; "user"])

(* 9. Result for an already-paired id (duplicate result) is dropped. *)
let () =
  let msgs = [ user "a"; assistant [call ~id:"one" ()];
    result ~id:"one" "r1"; result ~id:"one" "r1-again"; user "b" ] in
  let out = P.sanitize_messages msgs in
  assert (ids out = ["one"]);
  assert (texts out = ["a"; "r1"; "b"])

let () = print_endline "sanitize_messages: ok"
