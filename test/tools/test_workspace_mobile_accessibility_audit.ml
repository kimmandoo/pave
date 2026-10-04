module Observe = Pave.Workspace_mobile_observe
module Audit = Pave.Workspace_mobile_accessibility_audit

let expect label condition = if not condition then failwith label
let contains haystack needle =
  let hlen = String.length haystack and nlen = String.length needle in
  let rec loop index =
    index + nlen <= hlen &&
    (String.sub haystack index nlen = needle || loop (index + 1)) in
  loop 0

let node ?(parent=None) ?(depth=0) ?(role="android.widget.Button")
    ?(package="dev.example") ?(text="") ?(description="") ?(identifier="")
    ?(bounds="[0,0][48,48]") ?(clickable=true) ?(scrollable=false)
    ?(enabled=true) ?(selected=false) index : Observe.node =
  { index; parent; depth; role; package; text; description; identifier; bounds;
    clickable; scrollable; enabled; selected }

let () =
  let observed = [
    node ~identifier:"app:id/first" ~bounds:"[0,0][30,40]" 0;
    node ~identifier:"app:id/first" ~text:"Continue" ~bounds:"[40,0][88,48]" 1;
  ] in
  let density = { Audit.dpi = 160; observation_id = "current-approved-capture" } in
  let report = Audit.analyze ~observation_id:"current-approved-capture" ~density observed in
  let has rule = List.exists (fun finding -> finding.Audit.rule_id = rule) report.findings in
  expect "missing accessible label detected" (has "android.accessible_label_missing");
  expect "duplicate actionable identifier detected" (has "android.duplicate_identifier");
  expect "small density-scaled target detected" (has "android.touch_target_small");
  expect "source indexes retained" (List.exists (fun finding -> finding.Audit.source_index = 0) report.findings);
  expect "finding evidence does not echo labels or identifiers"
    (not (contains (Audit.report_json report) "Continue") &&
     not (contains (Audit.report_json report) "app:id/first"));
  let fixed = [
    node ~text:"Open" ~identifier:"app:id/open" 0;
    node ~text:"Close" ~identifier:"app:id/close" 1;
  ] in
  expect "fixed controls clear all rules"
    ((Audit.analyze ~observation_id:"fixed" ~density:{ density with observation_id = "fixed" } fixed).findings = []);
  let unknown_density = Audit.analyze ~observation_id:"unknown" [node ~bounds:"[0,0][1,1]" 0] in
  expect "unknown density is reported unavailable and does not infer target size"
    (not unknown_density.Audit.density_available &&
     not (List.exists (fun f -> f.Audit.rule_id = "android.touch_target_small") unknown_density.findings));
  let stale_node = [node ~text:"Open" ~bounds:"[0,0][1,1]" 0] in
  let stale_density = Audit.analyze ~observation_id:"new-capture"
      ~density:{ density with observation_id = "old-capture" } stale_node in
  expect "density from a different observation is unavailable"
    (not stale_density.Audit.density_available && stale_density.findings = []);
  let invalid_density = Audit.analyze ~observation_id:"invalid"
      ~density:{ Audit.dpi = 0; observation_id = "invalid" } stale_node in
  expect "invalid density is unavailable"
    (not invalid_density.Audit.density_available && invalid_density.findings = []);
  let invalid_bounds = [node ~text:"Open" ~bounds:"[8,0][2,40]" 0] in
  expect "invalid bounds do not infer target size"
    ((Audit.analyze ~observation_id:"bounds" ~density:{ density with observation_id = "bounds" } invalid_bounds).findings = []);
  let decorative = [node ~clickable:false ~scrollable:false ~bounds:"[0,0][1,1]" 0] in
  expect "decorative nodes are not actionable findings"
    ((Audit.analyze ~observation_id:"decorative" ~density:{ density with observation_id = "decorative" } decorative).findings = []);
  let raw_capture =
    "<hierarchy rotation=\"0\"><node class=\"android.widget.Button\" text=\"Continue\" content-desc=\"\" resource-id=\"app:id/continue\" bounds=\"[0,0][80,90]\" clickable=\"true\" scrollable=\"false\" enabled=\"true\" selected=\"false\"/></hierarchy>\nPAVE_MOBILE_DENSITY_BEGIN\nPhysical density: 320\n" in
  let capture = Audit.parse_capture raw_capture in
  let measured = Audit.analyze ~observation_id:capture.observation_id
      ?density:capture.density capture.nodes in
  expect "same-capture Android density reports undersized dp target"
    (measured.density_available &&
     List.exists (fun finding -> finding.Audit.rule_id = "android.touch_target_small")
       measured.findings);
  expect "report retains the exact observation identity"
    (contains (Audit.report_json measured) capture.observation_id);
  let stale_density = Option.map
      (fun (density : Audit.density) ->
        { density with observation_id = "older-capture" })
      capture.density in
  let stale = Audit.analyze ~observation_id:capture.observation_id
      ?density:stale_density capture.nodes in
  expect "density from a prior tree cannot infer target size"
    (not stale.density_available &&
     not (List.exists (fun finding -> finding.Audit.rule_id = "android.touch_target_small")
       stale.findings));
  let malformed = Audit.parse_capture
      (String.sub raw_capture 0 (String.length raw_capture -
        String.length "Physical density: 320\n") ^ "Physical density: 320\nOverride density: 3000\n") in
  let unknown = Audit.analyze ~observation_id:malformed.observation_id
      ?density:malformed.density malformed.nodes in
  expect "malformed device density is unknown, not a touch-target pass"
    (not unknown.density_available &&
     not (List.exists (fun finding -> finding.Audit.rule_id = "android.touch_target_small")
       unknown.findings));
  expect "missing density boundary is rejected"
    (try ignore (Audit.parse_capture "<hierarchy/>"); false
     with Audit.Error _ -> true);
  print_endline "workspace mobile accessibility audit: ok"
