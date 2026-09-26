type estimate = { estimated_bytes : int; unmeasured_images : int }

type status = Within_budget | Over_budget | Images_unmeasured

let add left right =
  if right > max_int - left then max_int else left + right

let add_text size = function
  | None -> size
  | Some text -> add size (String.length text)

let message_estimate (message : Protocol.message) =
  let size = ref 128 and images = ref 0 in
  let add_image mime_type data =
    incr images;
    size := add !size (add 64
      (add (String.length mime_type) (String.length data))) in
  List.iter (fun (attachment : Protocol.attachment) ->
    add_image attachment.mime_type attachment.data) message.attachments;
  size := add_text !size message.tool_call_id;
  List.iter (fun (call : Protocol.tool_call) ->
    size := add !size (String.length call.id);
    size := add !size (String.length call.name);
    size := add !size (String.length (Yojson.Basic.to_string call.arguments));
    size := add !size 64) message.tool_calls;
  (match message.tool_result_content with
   | None -> size := add_text !size message.content
   | Some blocks -> List.iter (function
       | Protocol.Text text -> size := add !size (String.length text)
       | Protocol.Image image -> add_image image.mime_type image.data) blocks);
  Option.iter (fun state ->
    size := add !size (String.length (Yojson.Basic.to_string state)))
    message.provider_state;
  !size, !images

let request ~system ~messages ~tools =
  let size = ref (add 256 (String.length system)) in
  let images = ref 0 in
  List.iter (fun message ->
    let bytes, count = message_estimate message in
    size := add !size bytes;
    images := add !images count) messages;
  List.iter (fun schema ->
    size := add !size (add 64 (String.length (Yojson.Basic.to_string schema)))) tools;
  { estimated_bytes = !size; unmeasured_images = !images }

let request_text_bytes ~system ~text_bytes =
  if text_bytes < 0 then invalid_arg "request text size must be nonnegative";
  { estimated_bytes = add 384 (add (String.length system) text_bytes);
    unmeasured_images = 0 }

let output_reserve window_tokens =
  if window_tokens < 8192 then
    invalid_arg "automatic compaction requires a context window of at least 8192 tokens";
  max 4096 (window_tokens / 5)

let status ~window_tokens ~reserve_tokens estimate =
  if window_tokens <= 0 || reserve_tokens < 0 then
    invalid_arg "context window must be positive and reserve must be nonnegative";
  let prompt_budget = max 0 (window_tokens - reserve_tokens) in
  if estimate.estimated_bytes > prompt_budget then Over_budget
  else if estimate.unmeasured_images > 0 then Images_unmeasured
  else Within_budget

let turn_groups messages =
  let groups_rev, current_rev = List.fold_left (fun (groups, current) message ->
    if message.Protocol.role = "user" && current <> [] then
      List.rev current :: groups, [message]
    else groups, message :: current) ([], []) messages in
  List.rev (if current_rev = [] then groups_rev
    else List.rev current_rev :: groups_rev)

let truncation_note =
  "\n[tool output truncated for context; full output remains in the journal]\n"

let utf8_floor text index =
  let index = min (String.length text) (max 0 index) in
  let rec back index =
    if index < String.length text && index > 0 &&
       (Char.code text.[index] land 0xc0) = 0x80 then back (index - 1)
    else index in
  back index

let utf8_ceil text index =
  let index = min (String.length text) (max 0 index) in
  let rec forward index =
    if index < String.length text &&
       (Char.code text.[index] land 0xc0) = 0x80 then forward (index + 1)
    else index in
  forward index

let truncate_text ~max_bytes text =
  if String.length text <= max_bytes then text
  else
    let remaining = max_bytes - String.length truncation_note in
    let prefix_limit = remaining / 2 in
    let suffix_limit = remaining - prefix_limit in
    let prefix_end = utf8_floor text prefix_limit in
    let suffix_start = utf8_ceil text (String.length text - suffix_limit) in
    String.sub text 0 prefix_end ^ truncation_note ^
      String.sub text suffix_start (String.length text - suffix_start)

let trim_tool_results ~max_bytes messages =
  if max_bytes <= String.length truncation_note then
    invalid_arg "tool result context limit is too small";
  let trimmed = ref 0 in
  let messages = List.map (fun (message : Protocol.message) ->
    if message.role <> "tool" then message
    else
      let text = match message.tool_result_content with
        | None -> message.content
        | Some blocks when List.for_all (function
            | Protocol.Text _ -> true | Protocol.Image _ -> false) blocks ->
            Some (Protocol.text_of_content_blocks blocks)
        | Some _ -> None in
      match text with
      | Some text when String.length text > max_bytes ->
          incr trimmed;
          let text = truncate_text ~max_bytes text in
          { message with content = Some text;
            tool_result_content = Option.map (fun _ -> [Protocol.Text text])
              message.tool_result_content }
      | _ -> message) messages in
  messages, !trimmed
