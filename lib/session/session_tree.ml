type choice = { label : string; id : string }

let max_choices = 1_024

let first_line text =
  let limit = min 96 (String.length text) in
  let stop = ref 0 in
  while !stop < limit && text.[!stop] <> '\n' do incr stop done;
  let bytes = !stop in
  while !stop > 0 && !stop < String.length text &&
    Char.code text.[!stop] land 0xc0 = 0x80 do decr stop done;
  let buffer = Buffer.create (!stop + 4) in
  ignore (Uutf.String.fold_utf_8 (fun () _ -> function
    | `Malformed _ -> Buffer.add_utf_8_uchar buffer Uutf.u_rep
    | `Uchar uchar ->
        let code = Uchar.to_int uchar in
        if code < 32 || (code >= 127 && code <= 159) ||
          code = 0x61c || code = 0x200e || code = 0x200f ||
          (code >= 0x202a && code <= 0x202e) ||
          (code >= 0x2066 && code <= 0x2069) then
          Buffer.add_char buffer ' '
        else Buffer.add_utf_8_uchar buffer uchar) ()
    (String.sub text 0 !stop));
  String.trim (Buffer.contents buffer) ^
    (if bytes = limit && limit < String.length text then "…" else "")

let summary (entry : Session.entry) =
  match entry.kind with
  | Session.Branch -> "branch point"
  | Session.Compaction _ -> "compacted context"
  | Session.Model { provider; model; api } ->
      "model · " ^ first_line (provider ^
        (match api with None -> "" | Some api -> "@" ^ api) ^ "/" ^ model)
  | Session.Thinking level ->
      "thinking · " ^ Option.value ~default:"default" level
  | Session.Tool_selection disabled ->
      "tools · disabled " ^ String.concat ", " (List.map first_line disabled)
  | Session.Mode_change mode ->
      "approval · " ^ (match mode with
        | None -> "inherit" | Some mode -> Approval.string_of_mode mode)
  | Session.Title title -> "title · " ^ first_line title
  | Session.Label { target_id; label } ->
      "label · " ^ (match label with None -> "cleared" | Some value -> first_line value) ^
      " · " ^ target_id
  | Session.Pin pinned ->
      "pin · " ^ (if pinned then "pinned" else "unpinned")
  | Session.Reset_boundary -> "cleared model context"
  | Session.Usage { provider; model; tokens } ->
      Printf.sprintf "usage · %s · %d in / %d out"
        (first_line (provider ^ "/" ^ model))
        tokens.input_tokens tokens.output_tokens
  | Session.Tool_lifecycle { name; state; _ } ->
      let state = match state with
        | Session.Tool_started -> "started"
        | Session.Tool_settled { is_error = false } -> "settled"
        | Session.Tool_settled { is_error = true } -> "failed"
        | Session.Tool_aborted { side_effects_may_have_occurred = false } ->
            "aborted"
        | Session.Tool_aborted { side_effects_may_have_occurred = true } ->
            "aborted · side effects possible" in
      "tool · " ^ first_line name ^ " · " ^ state
  | Session.Session_exit { kind; pending_tool_calls } ->
      let kind = match kind with
        | Session.Normal -> "normal" | Session.Signal -> "signal"
        | Session.Fatal -> "fatal" | Session.Process_exit -> "process exit" in
      let count = List.length pending_tool_calls in
      Printf.sprintf "session exit · %s · %d pending tool%s"
        kind count (if count = 1 then "" else "s")
  | Session.Message message ->
      let content = match message.tool_result_content with
        | Some blocks -> Some (Protocol.display_content_blocks blocks)
        | None -> message.content in
      let content = match content with
        | None | Some "" when message.tool_calls <> [] -> "tool calls"
        | None -> ""
        | Some text -> first_line text in
      let attachments = match message.attachments with
        | [] -> ""
        | items -> " · " ^ String.concat ", "
            (List.map (fun (item : Protocol.attachment) ->
              first_line item.name) items) in
      message.role ^ (if content = "" then "" else " · " ^ content) ^ attachments
let choices ?(labels = []) ~leaf entries =
  let total = List.length entries in
  let depth_by_id = Hashtbl.create (min 2048 total) in
  let first = max 0 (total - max_choices) in
  let index = ref 0 and selected = ref [] in
  List.iter (fun (entry : Session.entry) ->
    let depth = match entry.parent_id with
      | None -> 0
      | Some parent -> 1 + Option.value (Hashtbl.find_opt depth_by_id parent) ~default:0 in
    Hashtbl.replace depth_by_id entry.id depth;
    if !index >= first then (
      let indent = String.make (2 * min depth 4) ' ' in
      let marker = if leaf = Some entry.id then "◆" else " " in
      let label = match List.assoc_opt entry.id labels with
        | None -> ""
        | Some text -> " · " ^ first_line text in
      let label = Printf.sprintf "%s %s%s%s · %s%s" marker indent
        (if depth = 0 then "• " else if depth > 4 then "… " else "↳ ")
        (summary entry) entry.id label in
      selected := { label; id = entry.id } :: !selected);
    incr index) entries;
  List.rev !selected, first > 0
