exception Error of string

let fail message = raise (Error message)

let max_nodes = Workspace_mobile_observe.max_nodes
let max_findings = 100
let min_target_dp = 48

type severity = Warning | Serious

type finding = {
  source_index : int;
  rule_id : string;
  severity : severity;
  evidence : string;
}

type density = { dpi : int; observation_id : string }
type capture = {
  observation_id : string;
  nodes : Workspace_mobile_observe.node list;
  density : density option;
}
type report = {
  observation_id : string;
  findings : finding list;
  truncated : bool;
  density_available : bool;
}

type bounds = { left : int; top : int; right : int; bottom : int }

let actionable (node : Workspace_mobile_observe.node) =
  node.clickable || node.scrollable

let has_label (node : Workspace_mobile_observe.node) =
  String.trim node.text <> "" || String.trim node.description <> ""

let parse_bounds value =
  try
    Scanf.sscanf value "[%d,%d][%d,%d]%!" (fun left top right bottom ->
      if left < 0 || top < 0 || right <= left || bottom <= top then None
      else Some { left; top; right; bottom })
  with _ -> None

let dimensions bounds =
  bounds.right - bounds.left, bounds.bottom - bounds.top

let within_source_limit nodes =
  if List.length nodes > max_nodes then fail "accessibility audit tree exceeds its node limit";
  List.iteri (fun position (node : Workspace_mobile_observe.node) ->
    if node.index < 0 || node.index >= max_nodes || node.index <> position then
      fail "accessibility audit tree has invalid source indices") nodes

let analyze ~observation_id ?density (nodes : Workspace_mobile_observe.node list) =
  if observation_id = "" || String.length observation_id > 128 then
    fail "accessibility audit observation identity is invalid";
  within_source_limit nodes;
  (* The caller must construct density only from this exact current approved
     observation; the identity match prevents accidentally reusing stale DPI. *)
  let density_dpi = match density with
    | Some { dpi; observation_id = source }
      when source <> "" && source = observation_id && dpi > 0 && dpi <= 2000 -> Some dpi
    | _ -> None in
  let identifiers = Hashtbl.create (min (List.length nodes) 128) in
  List.iter (fun (node : Workspace_mobile_observe.node) ->
    if actionable node && String.trim node.identifier <> "" then
      let key = String.trim node.identifier in
      Hashtbl.replace identifiers key (1 + Option.value ~default:0 (Hashtbl.find_opt identifiers key)))
    nodes;
  let findings = ref [] and count = ref 0 and truncated = ref false in
  let add (finding : finding) =
    if !count < max_findings then begin
      incr count;
      findings := finding :: !findings
    end else truncated := true in
  List.iter (fun (node : Workspace_mobile_observe.node) ->
    if actionable node && not (has_label node) then
      add { source_index = node.index; rule_id = "android.accessible_label_missing";
        severity = Serious; evidence = "actionable control has no visible text or content description" };
    if actionable node && String.trim node.identifier <> "" &&
       Option.value ~default:0 (Hashtbl.find_opt identifiers (String.trim node.identifier)) > 1 then
      add { source_index = node.index; rule_id = "android.duplicate_identifier";
        severity = Warning; evidence = "identifier is shared by multiple actionable controls" };
    match density_dpi, actionable node, parse_bounds node.bounds with
    | Some dpi, true, Some bounds ->
        let width, height = dimensions bounds in
        let min_pixels = (min_target_dp * dpi + 159) / 160 in
        if width < min_pixels || height < min_pixels then
          add { source_index = node.index; rule_id = "android.touch_target_small";
            severity = Warning; evidence = "actionable control is smaller than 48 dp in at least one dimension" }
    | _ -> ()) nodes;
  { observation_id; findings = List.rev !findings; truncated = !truncated;
    density_available = Option.is_some density_dpi }

let starts_with text prefix =
  String.length text >= String.length prefix &&
  String.sub text 0 (String.length prefix) = prefix

let parse_density output =
  let lines = List.filter (( <> ) "") (List.map String.trim
    (String.split_on_char '\n' output)) in
  if lines = ["PAVE_MOBILE_DENSITY_UNAVAILABLE"] then None
  else
    let physical = ref None and override = ref None and invalid = ref false in
    let parse_value prefix line =
      if not (starts_with line prefix) then None
      else
        let value = String.trim
          (String.sub line (String.length prefix) (String.length line - String.length prefix)) in
        try
          if value = "" || String.length value > 4 then None
          else
            let dpi = int_of_string value in
            if dpi > 0 && dpi <= 2000 then Some dpi else None
        with Failure _ -> None in
    List.iter (fun line ->
      if starts_with line "Physical density:" then
        (match parse_value "Physical density:" line, !physical with
         | Some dpi, None -> physical := Some dpi
         | _ -> invalid := true)
      else if starts_with line "Override density:" then
        (match parse_value "Override density:" line, !override with
         | Some dpi, None -> override := Some dpi
         | _ -> invalid := true)
      else invalid := true) lines;
    if !invalid then None
    else match !physical with
      | None -> None
      | Some dpi -> Some (Option.value ~default:dpi !override)

let parse_capture raw =
  let marker = "\nPAVE_MOBILE_DENSITY_BEGIN\n" in
  let marker_length = String.length marker and raw_length = String.length raw in
  if raw_length > Workspace_mobile_observe.max_accessibility_bytes then
    fail "accessibility audit capture exceeds its size limit";
  let matches_at index =
    index + marker_length <= raw_length &&
    let rec equal offset =
      offset = marker_length ||
      (raw.[index + offset] = marker.[offset] && equal (offset + 1)) in
    equal 0 in
  let rec last_marker index found =
    if index > raw_length - marker_length then found
    else last_marker (index + 1)
      (if matches_at index then Some index else found) in
  let marker_index = match last_marker 0 None with
    | Some index -> index
    | None -> fail "accessibility audit capture is missing its density boundary" in
  let xml = String.sub raw 0 marker_index in
  let density_output = String.sub raw (marker_index + marker_length)
    (raw_length - marker_index - marker_length) in
  let nodes = try Workspace_mobile_observe.parse_accessibility xml
    with Workspace_mobile_observe.Error message -> fail message in
  let observation_id = Digestif.SHA256.(to_hex (digest_string raw)) in
  let density = Option.map (fun dpi -> { dpi; observation_id })
    (parse_density density_output) in
  { observation_id; nodes; density }

let severity_string = function Warning -> "warning" | Serious -> "serious"

let finding_json finding = `Assoc [
  "source_index", `Int finding.source_index;
  "rule_id", `String finding.rule_id;
  "severity", `String (severity_string finding.severity);
  "evidence", `String finding.evidence;
]

let report_json report = Yojson.Basic.to_string (`Assoc [
  "observation_id", `String report.observation_id;
  "density_available", `Bool report.density_available;
  "findings", `List (List.map finding_json report.findings);
  "truncated", `Bool report.truncated;
])
