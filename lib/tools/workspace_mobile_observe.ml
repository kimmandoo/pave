exception Error of string

let fail message = raise (Error message)

let max_screenshot_bytes = Workspace_process.max_output_limit
let max_accessibility_bytes = Workspace_process.max_output_limit
let max_nodes = 10_000

type screenshot = { png : string; width : int; height : int }

type node = {
  index : int;
  parent : int option;
  depth : int;
  role : string;
  package : string;
  text : string;
  description : string;
  identifier : string;
  bounds : string;
  clickable : bool;
  scrollable : bool;
  enabled : bool;
  selected : bool;
}

let command action (session : Workspace_mobile_run.session) =
  let quote = Filename.quote in
  match action, session.platform with
  | "screenshot", Workspace_mobile_run.Android ->
      "adb -s " ^ quote session.device ^ " exec-out screencap -p"
  | "accessibility", Workspace_mobile_run.Android ->
      let path = "/data/local/tmp/pave-accessibility-" ^ session.id ^ ".xml" in
      let remote = Printf.sprintf
        "uiautomator dump %s >/dev/null; status=$?; if [ \"$status\" -eq 0 ]; then cat %s; status=$?; fi; rm -f %s; exit \"$status\""
        path path path in
      "adb -s " ^ quote session.device ^ " shell " ^ quote remote
  | "screenshot", Workspace_mobile_run.Ios ->
      "xcrun simctl io " ^ quote session.device ^
      " screenshot --type=png /dev/stdout 2>/dev/null"
  | "accessibility", Workspace_mobile_run.Ios ->
      fail "iOS Simulator accessibility-tree capture is unavailable through the approved system tools"
  | _ -> fail "mobile observation action must be screenshot or accessibility"

let accessibility_audit_command (session : Workspace_mobile_run.session) =
  match session.platform with
  | Workspace_mobile_run.Android ->
      let quote = Filename.quote in
      let path = "/data/local/tmp/pave-accessibility-" ^ session.id ^ ".xml" in
      let remote = Printf.sprintf
        "uiautomator dump %s >/dev/null; status=$?; if [ \"$status\" -eq 0 ]; then cat %s; status=$?; fi; if [ \"$status\" -eq 0 ]; then printf '\\nPAVE_MOBILE_DENSITY_BEGIN\\n'; if wm density 2>/dev/null; then :; else printf 'PAVE_MOBILE_DENSITY_UNAVAILABLE\\n'; fi; fi; rm -f %s; exit \"$status\""
        (quote path) (quote path) (quote path) in
      "adb -s " ^ quote session.device ^ " shell " ^ quote remote
  | Workspace_mobile_run.Ios ->
      fail "rule-based mobile accessibility audit is currently Android-only"

let uint32_be data offset =
  let length = String.length data in
  if offset < 0 || offset > length - 4 then fail "invalid PNG header";
  let value index = Char.code data.[offset + index] in
  (value 0 lsl 24) lor (value 1 lsl 16) lor (value 2 lsl 8) lor value 3

let validate_png png =
  let length = String.length png in
  if length < 8 || length > max_screenshot_bytes ||
     String.sub png 0 8 <> "\137PNG\r\n\026\n" then
    fail "screenshot was not a complete bounded PNG image";
  let rec chunks offset index saw_data =
    if offset > length - 12 then fail "PNG screenshot is truncated before IEND";
    if index >= 1024 then fail "PNG screenshot contains too many chunks";
    let chunk_length = uint32_be png offset in
    if chunk_length < 0 || chunk_length > length - offset - 12 then
      fail "PNG screenshot contains a truncated chunk";
    let kind = String.sub png (offset + 4) 4 in
    if index = 0 && (kind <> "IHDR" || chunk_length <> 13) then
      fail "PNG screenshot has no valid first IHDR chunk";
    if kind = "IEND" then (
      if chunk_length <> 0 || not saw_data || offset + 12 <> length then
        fail "PNG screenshot has an invalid IEND chunk";
      ())
    else if kind = "IDAT" then
      chunks (offset + 12 + chunk_length) (index + 1) true
    else
      chunks (offset + 12 + chunk_length) (index + 1) saw_data in
  if length < 33 then fail "PNG screenshot has a truncated IHDR chunk";
  let width = uint32_be png 16 and height = uint32_be png 20 in
  if width < 1 || height < 1 || width > 16_384 || height > 16_384 ||
     width > 100_000_000 / height then
    fail "screenshot dimensions are outside supported bounds";
  chunks 8 0 false;
  { png; width; height }

let base64_encode data =
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let length = String.length data in
  let output = Buffer.create (((length + 2) / 3) * 4) in
  let byte index = Char.code data.[index] in
  let rec loop index =
    if index < length then (
      let remaining = length - index in
      let a = byte index in
      let b = if remaining > 1 then byte (index + 1) else 0 in
      let c = if remaining > 2 then byte (index + 2) else 0 in
      Buffer.add_char output alphabet.[a lsr 2];
      Buffer.add_char output alphabet.[((a land 3) lsl 4) lor (b lsr 4)];
      Buffer.add_char output (if remaining > 1 then alphabet.[((b land 15) lsl 2) lor (c lsr 6)] else '=');
      Buffer.add_char output (if remaining > 2 then alphabet.[c land 63] else '=');
      loop (index + 3)) in
  loop 0;
  Buffer.contents output

let find_sub text pattern offset =
  let text_length = String.length text and pattern_length = String.length pattern in
  let rec loop index =
    if index + pattern_length > text_length then None
    else if String.sub text index pattern_length = pattern then Some index
    else loop (index + 1) in
  loop offset

let node_start xml offset =
  let rec find index = match find_sub xml "<node" index with
    | None -> None
    | Some start ->
        let after = start + 5 in
        if after >= String.length xml then None
        else if List.mem xml.[after] [' '; '\t'; '\r'; '\n'; '>'; '/'] then Some start
        else find after in
  find offset

let tag_end xml start =
  let rec loop index quote =
    if index >= String.length xml then fail "unterminated accessibility node tag";
    let ch = xml.[index] in
    match quote, ch with
    | Some delimiter, value when value = delimiter -> loop (index + 1) None
    | Some _, _ -> loop (index + 1) quote
    | None, ('\"' | '\'') -> loop (index + 1) (Some ch)
    | None, '>' -> index
    | None, _ -> loop (index + 1) None in
  loop start None

let decode_entities value =
  let output = Buffer.create (String.length value) in
  let add_codepoint entity =
    let number =
      try
        if String.length entity >= 3 && entity.[1] = 'x' then
          int_of_string ("0x" ^ String.sub entity 2 (String.length entity - 2))
        else int_of_string (String.sub entity 1 (String.length entity - 1))
      with Failure _ -> fail "invalid numeric XML entity in accessibility node" in
    if number < 0 || number > 0x10ffff ||
       (number >= 0xd800 && number <= 0xdfff) ||
       not (number = 9 || number = 10 || number = 13 || number >= 0x20) then
      fail "invalid XML character reference in accessibility node";
    if number < 0x80 then Buffer.add_char output (Char.chr number)
    else if number < 0x800 then (
      Buffer.add_char output (Char.chr (0xc0 lor (number lsr 6)));
      Buffer.add_char output (Char.chr (0x80 lor (number land 0x3f)))
    ) else if number < 0x10000 then (
      Buffer.add_char output (Char.chr (0xe0 lor (number lsr 12)));
      Buffer.add_char output (Char.chr (0x80 lor ((number lsr 6) land 0x3f)));
      Buffer.add_char output (Char.chr (0x80 lor (number land 0x3f)))
    ) else (
      Buffer.add_char output (Char.chr (0xf0 lor (number lsr 18)));
      Buffer.add_char output (Char.chr (0x80 lor ((number lsr 12) land 0x3f)));
      Buffer.add_char output (Char.chr (0x80 lor ((number lsr 6) land 0x3f)));
      Buffer.add_char output (Char.chr (0x80 lor (number land 0x3f)))) in
  let rec loop index =
    if index < String.length value then
      if value.[index] <> '&' then (
        Buffer.add_char output value.[index];
        loop (index + 1))
      else
        match String.index_from_opt value (index + 1) ';' with
        | None -> fail "unterminated XML entity in accessibility node"
        | Some ending ->
            let entity = String.sub value (index + 1) (ending - index - 1) in
            (match entity with
             | "amp" -> Buffer.add_char output '&'
             | "quot" -> Buffer.add_char output '"'
             | "apos" -> Buffer.add_char output (Char.chr 39)
             | "lt" -> Buffer.add_char output '<'
             | "gt" -> Buffer.add_char output '>'
             | value when String.length value > 1 && value.[0] = '#' ->
                 add_codepoint value
             | _ -> fail "unsupported XML entity in accessibility node");
            loop (ending + 1) in
  loop 0;
  Buffer.contents output

let parse_attributes tag =
  let length = String.length tag in
  let rec skip index =
    if index < length && List.mem tag.[index] [' '; '\t'; '\r'; '\n'; '/']
    then skip (index + 1) else index in
  let rec parse index attributes =
    let index = skip index in
    if index >= length || tag.[index] = '>' then attributes
    else
      let name_end = ref index in
      while !name_end < length &&
        not (List.mem tag.[!name_end] [' '; '\t'; '\r'; '\n'; '='; '>'; '/'])
      do incr name_end done;
      if !name_end = index then fail "invalid accessibility node attribute";
      let name = String.sub tag index (!name_end - index) in
      let equals = skip !name_end in
      if equals >= length || tag.[equals] <> '=' then
        fail "invalid accessibility node attribute";
      let opening = skip (equals + 1) in
      if opening >= length || not (List.mem tag.[opening] ['\"'; '\'']) then
        fail "invalid accessibility node attribute value";
      let delimiter = tag.[opening] in
      let ending = match String.index_from_opt tag (opening + 1) delimiter with
        | Some value -> value
        | None -> fail "unterminated accessibility node attribute" in
      let value = decode_entities
        (String.sub tag (opening + 1) (ending - opening - 1)) in
      parse (ending + 1) ((name, value) :: attributes) in
  parse 5 []

let attr attributes name = Option.value ~default:"" (List.assoc_opt name attributes)
let bool_attr attributes name = attr attributes name = "true"

let parse_accessibility xml =
  if String.length xml > max_accessibility_bytes then
    fail "accessibility output exceeds its size limit";
  if find_sub xml "<!DOCTYPE" 0 <> None || find_sub xml "<!ENTITY" 0 <> None then
    fail "accessibility output contains a forbidden XML declaration";
  let nodes = ref [] and stack = ref [] in
  let rec loop offset =
    let opening = node_start xml offset
    and closing = find_sub xml "</node" offset in
    match opening, closing with
    | None, None -> ()
    | None, Some start ->
        if !stack = [] then fail "unbalanced accessibility tree";
        stack := List.tl !stack;
        loop (tag_end xml start + 1)
    | Some start, Some ending when ending < start ->
        if !stack = [] then fail "unbalanced accessibility tree";
        stack := List.tl !stack;
        loop (tag_end xml ending + 1)
    | Some start, _ ->
        let ending = tag_end xml start in
        let tag = String.sub xml start (ending - start + 1) in
        let attributes = parse_attributes tag in
        if List.length !nodes >= max_nodes then
          fail "accessibility tree exceeds its node limit";
        let index = List.length !nodes in
        let parent = match !stack with [] -> None | parent :: _ -> Some parent in
        let node = {
          index; parent; depth = List.length !stack;
          role = attr attributes "class";
          package = attr attributes "package";
          text = attr attributes "text";
          description = attr attributes "content-desc";
          identifier = attr attributes "resource-id";
          bounds = attr attributes "bounds";
          clickable = bool_attr attributes "clickable";
          scrollable = bool_attr attributes "scrollable";
          enabled = bool_attr attributes "enabled";
          selected = bool_attr attributes "selected";
        } in
        nodes := node :: !nodes;
        let position = ref (String.length tag - 2) in
        while !position >= 0 &&
          List.mem tag.[!position] [' '; '\t'; '\r'; '\n'] do
          decr position done;
        if !position < 0 || tag.[!position] <> '/' then
          stack := index :: !stack;
        loop (ending + 1) in
  loop 0;
  if !nodes = [] then fail "accessibility tree is empty";
  if !stack <> [] then
    fail (Printf.sprintf "accessibility tree has %d unclosed node(s)"
      (List.length !stack));
  List.rev !nodes

let node_json node = `Assoc [
  "index", `Int node.index;
  "parent", (match node.parent with None -> `Null | Some value -> `Int value);
  "depth", `Int node.depth;
  "role", `String node.role;
  "package", `String node.package;
  "text", `String node.text;
  "description", `String node.description;
  "identifier", `String node.identifier;
  "bounds", `String node.bounds;
  "clickable", `Bool node.clickable;
  "scrollable", `Bool node.scrollable;
  "enabled", `Bool node.enabled;
  "selected", `Bool node.selected;
]

let accessibility_json nodes = Yojson.Basic.to_string (`Assoc [
  "status", `String "available";
  "node_count", `Int (List.length nodes);
  "nodes", `List (List.map node_json nodes);
])
let xctest_operation = "accessibility"
let xctest_version = "pave-native-xctest-ui-1"
let max_xctest_request_bytes = 8_192
let max_xctest_response_bytes = 1_048_576
let max_xctest_manifest_bytes = 65_536
let xctest_attachment_name = "PaveXCTestResponse"

let xctest_request ~bundle_id =
  if bundle_id = "" || String.length bundle_id > 256 then
    fail "selected iOS bundle identifier is invalid";
  let json = Yojson.Basic.to_string (`Assoc [
    "version", `Int 1;
    "operation", `String xctest_operation;
    "bundle_id", `String bundle_id]) in
  if String.length json > max_xctest_request_bytes then
    fail "XCTest request exceeds its byte limit";
  json

let xctest_runner_source ~bundle_id =
  let request = xctest_request ~bundle_id |> base64_encode in
  let template = {|
import Foundation
import CoreFoundation
import XCTest

final class PaveXCTestRunnerTests: XCTestCase {
    private let maximumRequestBytes = 8192
    private let maximumResponseBytes = 1048576
    private let maximumNodes = 10000
    private let maximumDepth = 128
    private let maximumFieldBytes = 4096

    func testPaveRequest() throws {
        let requestBytes = Data(base64Encoded: "__PAVE_REQUEST_BASE64__")!
        guard requestBytes.count <= maximumRequestBytes else {
            throw NSError(domain: "PaveXCTest", code: 1,
                          userInfo: [NSLocalizedDescriptionKey: "request exceeds byte limit"])
        }
        let decoded = try JSONSerialization.jsonObject(with: requestBytes)
        guard let request = decoded as? [String: Any],
              let bundleID = request["bundle_id"] as? String,
              let operation = request["operation"] as? String,
              operation == "accessibility",
              request["version"] as? Int == 1 else {
            throw NSError(domain: "PaveXCTest", code: 2,
                          userInfo: [NSLocalizedDescriptionKey: "malformed request"])
        }
        var response: [String: Any]
        do {
            response = try execute(bundleID: bundleID, operation: operation)
        } catch {
            response = ["version": 1, "status": "error",
                        "operation": operation, "bundle_id": bundleID,
                        "error": String(describing: error)]
        }
        let data = try JSONSerialization.data(withJSONObject: response,
                                               options: [.sortedKeys])
        guard data.count <= maximumResponseBytes else {
            throw NSError(domain: "PaveXCTest", code: 3,
                          userInfo: [NSLocalizedDescriptionKey: "response exceeds byte limit"])
        }
        let attachment = XCTAttachment(data: data, uniformTypeIdentifier: "public.json")
        attachment.name = "PaveXCTestResponse"
        attachment.lifetime = .keepAlways
        add(attachment)
        if response["status"] as? String == "error" {
            XCTFail("Pave XCTest helper failed: \(response["error"] ?? "unknown error")")
        }
    }

    private func checkedString(_ value: String, _ label: String) throws -> String {
        guard value.utf8.count <= maximumFieldBytes else {
            throw NSError(domain: "PaveXCTest", code: 4,
                          userInfo: [NSLocalizedDescriptionKey: "\(label) exceeds byte limit"])
        }
        return value
    }

    private func jsonValue(_ value: Any?, _ label: String) throws -> Any {
        guard let value = value else { return NSNull() }
        if let string = value as? String { return try checkedString(string, label) }
        if let number = value as? NSNumber {
            if CFGetTypeID(number) != CFBooleanGetTypeID() &&
                !number.doubleValue.isFinite {
                throw NSError(domain: "PaveXCTest", code: 5,
                              userInfo: [NSLocalizedDescriptionKey: "\(label) is not finite"])
            }
            return number
        }
        if value is NSNull { return NSNull() }
        throw NSError(domain: "PaveXCTest", code: 6,
                      userInfo: [NSLocalizedDescriptionKey: "\(label) has an unsupported type"])
    }

    private func elementNode(_ element: XCUIElement, index: Int,
                             parent: Int?, depth: Int) throws -> [String: Any] {
        if depth > maximumDepth {
            throw NSError(domain: "PaveXCTest", code: 7,
                          userInfo: [NSLocalizedDescriptionKey: "accessibility depth exceeds limit"])
        }
        let label = try checkedString(element.label, "label")
        let identifier = try checkedString(element.identifier, "identifier")
        let type = String(describing: element.elementType)
        let frame = element.frame
        let bounds = [frame.origin.x, frame.origin.y,
                      frame.size.width, frame.size.height]
        guard bounds.allSatisfy({ $0.isFinite }) else {
            throw NSError(domain: "PaveXCTest", code: 8,
                          userInfo: [NSLocalizedDescriptionKey: "element bounds are invalid"])
        }
        return [
            "index": index,
            "parent": parent.map { $0 as Any } ?? NSNull(),
            "depth": depth,
            "role": type,
            "type": type,
            "identifier": identifier,
            "label": label,
            "text": label,
            "description": label,
            "value": try jsonValue(element.value, "value"),
            "enabled": element.isEnabled,
            "hittable": element.isHittable,
            "bounds": ["x": bounds[0], "y": bounds[1],
                       "width": bounds[2], "height": bounds[3]]
        ]
    }

    private func accessibilityTree(_ app: XCUIApplication) throws -> [[String: Any]] {
        var nodes: [[String: Any]] = []
        func visit(_ element: XCUIElement, _ parent: Int?, _ depth: Int) throws {
            if nodes.count >= maximumNodes {
                throw NSError(domain: "PaveXCTest", code: 9,
                              userInfo: [NSLocalizedDescriptionKey: "accessibility node limit exceeded"])
            }
            let index = nodes.count
            nodes.append(try elementNode(element, index: index,
                                         parent: parent, depth: depth))
            let query = element.children(matching: .any)
            let childCount = query.count
            if childCount > maximumNodes - nodes.count {
                throw NSError(domain: "PaveXCTest", code: 10,
                              userInfo: [NSLocalizedDescriptionKey: "accessibility node limit exceeded"])
            }
            for child in query.allElementsBoundByIndex {
                try visit(child, index, depth + 1)
            }
        }
        try visit(app, nil, 0)
        guard !nodes.isEmpty else {
            throw NSError(domain: "PaveXCTest", code: 11,
                          userInfo: [NSLocalizedDescriptionKey: "accessibility tree is empty"])
        }
        return nodes
    }

    private func execute(bundleID: String,
                         operation: String) throws -> [String: Any] {
        let app = XCUIApplication(bundleIdentifier: bundleID)
        guard app.state == .runningForeground ||
              app.state == .runningBackground ||
              app.state == .runningBackgroundSuspended else {
            throw NSError(domain: "PaveXCTest", code: 12,
                          userInfo: [NSLocalizedDescriptionKey: "selected app is not already running"])
        }
        app.activate()
        let nodes = try accessibilityTree(app)
        let result: [String: Any] = [
            "version": 1, "status": "available", "operation": operation,
            "bundle_id": bundleID, "node_count": nodes.count, "nodes": nodes
        ]
        let data = try JSONSerialization.data(withJSONObject: result)
        guard data.count <= maximumResponseBytes else {
            throw NSError(domain: "PaveXCTest", code: 13,
                          userInfo: [NSLocalizedDescriptionKey: "accessibility response exceeds byte limit"])
        }
        return result
    }
}
|} in
  let marker = "__PAVE_REQUEST_BASE64__" in
  let replace start =
    match find_sub template marker start with
    | None -> template
    | Some offset ->
        let before = String.sub template 0 offset in
        let after = String.sub template (offset + String.length marker)
            (String.length template - offset - String.length marker) in
        before ^ request ^ after in
  replace 0

let xctest_host_source = {|
import UIKit

@main
final class PaveXCTestHostAppDelegate: UIResponder, UIApplicationDelegate {
    var window: UIWindow?
    func application(_ application: UIApplication,
                     didFinishLaunchingWithOptions launchOptions:
                        [UIApplication.LaunchOptionsKey: Any]? = nil) -> Bool {
        true
    }
}
|}

let xctest_project_source = {|
// !$*UTF8*$!
{
  archiveVersion = 1;
  classes = {};
  objectVersion = 56;
  objects = {
    000000000000000000000001 = {
      isa = PBXProject;
      attributes = {
        LastUpgradeCheck = 1600;
        TargetAttributes = {
          000000000000000000000005 = { CreatedOnToolsVersion = 16.0; };
          000000000000000000000006 = {
            CreatedOnToolsVersion = 16.0;
            TestTargetID = 000000000000000000000005;
          };
        };
      };
      buildConfigurationList = 000000000000000000000019;
      compatibilityVersion = "Xcode 14.0";
      developmentRegion = en;
      hasScannedForEncodings = 0;
      knownRegions = (en, Base);
      mainGroup = 000000000000000000000003;
      productRefGroup = 000000000000000000000004;
      projectDirPath = "";
      projectRoot = "";
      targets = (
        000000000000000000000005,
        000000000000000000000006,
      );
    };
    000000000000000000000003 = {
      isa = PBXGroup;
      children = (
        000000000000000000000007,
        000000000000000000000008,
        000000000000000000000004,
      );
      sourceTree = "<group>";
    };
    000000000000000000000004 = {
      isa = PBXGroup;
      children = (
        000000000000000000000009,
        00000000000000000000000A,
      );
      name = Products;
      sourceTree = "<group>";
    };
    000000000000000000000007 = {
      isa = PBXGroup;
      children = (00000000000000000000000B,);
      path = Host;
      sourceTree = "<group>";
    };
    000000000000000000000008 = {
      isa = PBXGroup;
      children = (00000000000000000000000C,);
      path = Tests;
      sourceTree = "<group>";
    };
    00000000000000000000000B = {
      isa = PBXFileReference;
      lastKnownFileType = sourcecode.swift;
      path = PaveXCTestHost.swift;
      sourceTree = "<group>";
    };
    00000000000000000000000C = {
      isa = PBXFileReference;
      lastKnownFileType = sourcecode.swift;
      path = PaveXCTestRunner.swift;
      sourceTree = "<group>";
    };
    000000000000000000000009 = {
      isa = PBXFileReference;
      explicitFileType = wrapper.application;
      includeInIndex = 0;
      path = PaveXCTestHost.app;
      sourceTree = BUILT_PRODUCTS_DIR;
    };
    00000000000000000000000A = {
      isa = PBXFileReference;
      explicitFileType = wrapper.cfbundle;
      includeInIndex = 0;
      path = PaveXCTestRunner.xctest;
      sourceTree = BUILT_PRODUCTS_DIR;
    };
    00000000000000000000000D = {
      isa = PBXBuildFile;
      fileRef = 00000000000000000000000B;
    };
    00000000000000000000000E = {
      isa = PBXBuildFile;
      fileRef = 00000000000000000000000C;
    };
    00000000000000000000000F = {
      isa = PBXSourcesBuildPhase;
      buildActionMask = 2147483647;
      files = (00000000000000000000000D,);
      runOnlyForDeploymentPostprocessing = 0;
    };
    000000000000000000000010 = {
      isa = PBXSourcesBuildPhase;
      buildActionMask = 2147483647;
      files = (00000000000000000000000E,);
      runOnlyForDeploymentPostprocessing = 0;
    };
    000000000000000000000011 = {
      isa = PBXFrameworksBuildPhase;
      buildActionMask = 2147483647;
      files = ();
      runOnlyForDeploymentPostprocessing = 0;
    };
    000000000000000000000012 = {
      isa = PBXFrameworksBuildPhase;
      buildActionMask = 2147483647;
      files = ();
      runOnlyForDeploymentPostprocessing = 0;
    };
    000000000000000000000013 = {
      isa = PBXResourcesBuildPhase;
      buildActionMask = 2147483647;
      files = ();
      runOnlyForDeploymentPostprocessing = 0;
    };
    000000000000000014 = {
      isa = PBXResourcesBuildPhase;
      buildActionMask = 2147483647;
      files = ();
      runOnlyForDeploymentPostprocessing = 0;
    };
    000000000000000015 = {
      isa = PBXContainerItemProxy;
      containerPortal = 000000000000000001;
      proxyType = 1;
      remoteGlobalIDString = 000000000000000000000005;
      remoteInfo = PaveXCTestHost;
    };
    000000000000000016 = {
      isa = PBXTargetDependency;
      target = 000000000000000000000005;
      targetProxy = 000000000000000000000015;
    };
    000000000000000000000005 = {
      isa = PBXNativeTarget;
      buildConfigurationList = 000000000000000000000017;
      buildPhases = (
        00000000000000000000000F,
        000000000000000000000011,
        000000000000000000000013,
      );
      buildRules = ();
      dependencies = ();
      name = PaveXCTestHost;
      productName = PaveXCTestHost;
      productReference = 000000000000000000000009;
      productType = "com.apple.product-type.application";
    };
    000000000000000000000006 = {
      isa = PBXNativeTarget;
      buildConfigurationList = 000000000000000000000018;
      buildPhases = (
        000000000000000000000010,
        000000000000000000000012,
        000000000000000000000014,
      );
      buildRules = ();
      dependencies = (000000000000000000000016,);
      name = PaveXCTestRunner;
      productName = PaveXCTestRunner;
      productReference = 00000000000000000000000A;
      productType = "com.apple.product-type.bundle.ui-testing";
    };
    000000000000000000000019 = {
      isa = XCConfigurationList;
      buildConfigurations = (
        00000000000000000000001A,
        00000000000000000000001B,
      );
      defaultConfigurationIsVisible = 0;
      defaultConfigurationName = Release;
    };
    00000000000000000000001A = {
      isa = XCBuildConfiguration;
      buildSettings = {
        CODE_SIGNING_ALLOWED = NO;
        IPHONEOS_DEPLOYMENT_TARGET = 15.0;
        SDKROOT = iphoneos;
        SUPPORTED_PLATFORMS = "iphoneos iphonesimulator";
        TARGETED_DEVICE_FAMILY = "1,2";
      };
      name = Debug;
    };
    00000000000000000000001B = {
      isa = XCBuildConfiguration;
      buildSettings = {
        CODE_SIGNING_ALLOWED = NO;
        IPHONEOS_DEPLOYMENT_TARGET = 15.0;
        SDKROOT = iphoneos;
        SUPPORTED_PLATFORMS = "iphoneos iphonesimulator";
        TARGETED_DEVICE_FAMILY = "1,2";
      };
      name = Release;
    };
    000000000000000000000017 = {
      isa = XCConfigurationList;
      buildConfigurations = (
        00000000000000000000001C,
        00000000000000000000001D,
      );
      defaultConfigurationIsVisible = 0;
      defaultConfigurationName = Release;
    };
    00000000000000000000001C = {
      isa = XCBuildConfiguration;
      buildSettings = {
        CODE_SIGNING_ALLOWED = NO;
        GENERATE_INFOPLIST_FILE = YES;
        INFOPLIST_KEY_CFBundleDisplayName = PaveXCTestHost;
        IPHONEOS_DEPLOYMENT_TARGET = 15.0;
        PRODUCT_BUNDLE_IDENTIFIER = dev.pave.native-xctest.host;
        PRODUCT_NAME = "$(TARGET_NAME)";
        SDKROOT = iphoneos;
        SUPPORTED_PLATFORMS = "iphoneos iphonesimulator";
        SWIFT_VERSION = 5.0;
        TARGETED_DEVICE_FAMILY = "1,2";
      };
      name = Debug;
    };
    00000000000000000000001D = {
      isa = XCBuildConfiguration;
      buildSettings = {
        CODE_SIGNING_ALLOWED = NO;
        GENERATE_INFOPLIST_FILE = YES;
        INFOPLIST_KEY_CFBundleDisplayName = PaveXCTestHost;
        IPHONEOS_DEPLOYMENT_TARGET = 15.0;
        PRODUCT_BUNDLE_IDENTIFIER = dev.pave.native-xctest.host;
        PRODUCT_NAME = "$(TARGET_NAME)";
        SDKROOT = iphoneos;
        SUPPORTED_PLATFORMS = "iphoneos iphonesimulator";
        SWIFT_VERSION = 5.0;
        TARGETED_DEVICE_FAMILY = "1,2";
      };
      name = Release;
    };
    000000000000000000000018 = {
      isa = XCConfigurationList;
      buildConfigurations = (
        00000000000000000000001E,
        00000000000000000000001F,
      );
      defaultConfigurationIsVisible = 0;
      defaultConfigurationName = Release;
    };
    00000000000000000000001E = {
      isa = XCBuildConfiguration;
      buildSettings = {
        CODE_SIGNING_ALLOWED = NO;
        GENERATE_INFOPLIST_FILE = YES;
        IPHONEOS_DEPLOYMENT_TARGET = 15.0;
        PRODUCT_BUNDLE_IDENTIFIER = dev.pave.native-xctest.tests;
        PRODUCT_NAME = "$(TARGET_NAME)";
        SDKROOT = iphoneos;
        SUPPORTED_PLATFORMS = "iphoneos iphonesimulator";
        SWIFT_VERSION = 5.0;
        TARGETED_DEVICE_FAMILY = "1,2";
        TEST_TARGET_NAME = PaveXCTestHost;
      };
      name = Debug;
    };
    00000000000000000000001F = {
      isa = XCBuildConfiguration;
      buildSettings = {
        CODE_SIGNING_ALLOWED = NO;
        GENERATE_INFOPLIST_FILE = YES;
        IPHONEOS_DEPLOYMENT_TARGET = 15.0;
        PRODUCT_BUNDLE_IDENTIFIER = dev.pave.native-xctest.tests;
        PRODUCT_NAME = "$(TARGET_NAME)";
        SDKROOT = iphoneos;
        SUPPORTED_PLATFORMS = "iphoneos iphonesimulator";
        SWIFT_VERSION = 5.0;
        TARGETED_DEVICE_FAMILY = "1,2";
        TEST_TARGET_NAME = PaveXCTestHost;
      };
      name = Release;
    };
  };
  rootObject = 000000000000000000000001;
}
|}

let xctest_scheme_source = {|
<?xml version="1.0" encoding="UTF-8"?>
<Scheme LastUpgradeVersion="1600" version="1.7">
  <BuildAction parallelizeBuildables="NO" buildImplicitDependencies="YES">
    <BuildActionEntries>
      <BuildActionEntry buildForTesting="YES" buildForRunning="YES" buildForProfiling="NO" buildForArchiving="NO" buildForAnalyzing="YES">
        <BuildableReference BuildableIdentifier="primary" BlueprintIdentifier="000000000000000000000005" BuildableName="PaveXCTestHost.app" BlueprintName="PaveXCTestHost" ReferencedContainer="container:PaveXCTestRunner.xcodeproj"/>
      </BuildActionEntry>
      <BuildActionEntry buildForTesting="YES" buildForRunning="NO" buildForProfiling="NO" buildForArchiving="NO" buildForAnalyzing="YES">
        <BuildableReference BuildableIdentifier="primary" BlueprintIdentifier="000000000000000000000006" BuildableName="PaveXCTestRunner.xctest" BlueprintName="PaveXCTestRunner" ReferencedContainer="container:PaveXCTestRunner.xcodeproj"/>
      </BuildActionEntry>
    </BuildActionEntries>
  </BuildAction>
  <TestAction buildConfiguration="Debug" selectedDebuggerIdentifier="Xcode.DebuggerFoundation.Debugger.LLDB" selectedLauncherIdentifier="Xcode.DebuggerFoundation.Launcher.LLDB" shouldUseLaunchSchemeArgsEnv="YES">
    <Testables>
      <TestableReference skipped="NO" parallelizable="NO">
        <BuildableReference BuildableIdentifier="primary" BlueprintIdentifier="000000000000000000000006" BuildableName="PaveXCTestRunner.xctest" BlueprintName="PaveXCTestRunner" ReferencedContainer="container:PaveXCTestRunner.xcodeproj"/>
      </TestableReference>
    </Testables>
  </TestAction>
  <LaunchAction buildConfiguration="Debug" selectedDebuggerIdentifier="Xcode.DebuggerFoundation.Debugger.LLDB" selectedLauncherIdentifier="Xcode.DebuggerFoundation.Launcher.LLDB" launchStyle="0" useCustomWorkingDirectory="NO" ignoresPersistentStateOnLaunch="NO" debugDocumentVersioning="YES" debugServiceExtension="internal" allowLocationSimulation="YES">
    <BuildableProductRunnable runnableDebuggingMode="0">
      <BuildableReference BuildableIdentifier="primary" BlueprintIdentifier="000000000000000000000005" BuildableName="PaveXCTestHost.app" BlueprintName="PaveXCTestHost" ReferencedContainer="container:PaveXCTestRunner.xcodeproj"/>
    </BuildableProductRunnable>
  </LaunchAction>
</Scheme>
|}

let xctest_project_fingerprint ~bundle_id =
  let test_source = xctest_runner_source ~bundle_id in
  Workspace_edit.sha256 (String.concat "\000" [
    xctest_version; test_source; xctest_host_source;
    xctest_project_source; xctest_scheme_source])

let write_private_file path content =
  let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600 in
  let channel = Unix.out_channel_of_descr fd in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel content)

let create_xctest_project ~directory ~bundle_id =
  ignore (xctest_request ~bundle_id);
  Unix.mkdir directory 0o700;
  let project = Filename.concat directory "PaveXCTestRunner.xcodeproj" in
  Unix.mkdir project 0o700;
  let schemes = Filename.concat project "xcshareddata" in
  Unix.mkdir schemes 0o700;
  let schemes = Filename.concat schemes "xcschemes" in
  Unix.mkdir schemes 0o700;
  let host = Filename.concat directory "Host" and tests = Filename.concat directory "Tests" in
  Unix.mkdir host 0o700;
  Unix.mkdir tests 0o700;
  write_private_file (Filename.concat host "PaveXCTestHost.swift") xctest_host_source;
  let test_source = xctest_runner_source ~bundle_id in
  write_private_file (Filename.concat tests "PaveXCTestRunner.swift") test_source;
  write_private_file (Filename.concat project "project.pbxproj") xctest_project_source;
  write_private_file (Filename.concat schemes "PaveXCTestRunner.xcscheme")
    xctest_scheme_source;
  Workspace_edit.sha256 (String.concat "\000" [
    xctest_version; test_source; xctest_host_source;
    xctest_project_source; xctest_scheme_source])

let xctest_command ~directory ~simulator_id =
  if not (Workspace_mobile_device_lifecycle.valid_uuid simulator_id) then
    fail "XCTest requires the exact selected Simulator UUID";
  let quote = Filename.quote in
  let project = Filename.concat directory "PaveXCTestRunner.xcodeproj" in
  let derived = Filename.concat directory "DerivedData" in
  let result = Filename.concat directory "Result.xcresult" in
  let export = Filename.concat directory "Attachments" in
  let booted_log = Filename.concat directory "booted-simulators.json" in
  let log = Filename.concat directory "xcodebuild.log" in
  let export_log = Filename.concat directory "xcresulttool.log" in
  let booted_preflight = String.concat " " [
    "xcrun"; "simctl"; "list"; "devices"; "booted"; "-j";
    ">" ^ quote booted_log; "2>&1"] in
  let xcodebuild = String.concat " " [
    "xcodebuild"; "test"; "-project"; quote project;
    "-scheme"; quote "PaveXCTestRunner";
    "-destination"; quote ("platform=iOS Simulator,id=" ^ simulator_id);
    "-parallel-testing-enabled"; "NO";
    "-derivedDataPath"; quote derived;
    "-resultBundlePath"; quote result;
    "CODE_SIGNING_ALLOWED=NO";
    ">" ^ quote log; "2>&1"] in
  let export_attachments = String.concat " " [
    "xcrun"; "xcresulttool"; "export"; "attachments";
    "--path"; quote result; "--output-path"; quote export;
    ">" ^ quote export_log; "2>&1"] in
  let report_status =
    "printf 'PAVE_XCTEST_PREFLIGHT_STATUS=%s\\nPAVE_XCTEST_BUILD_STATUS=%s\\nPAVE_XCTEST_EXPORT_STATUS=%s\\n' " ^
    "\"$preflight_status\" \"$build_status\" \"$export_status\"" in
  let report_preflight_failure =
    "if [ \"$preflight_status\" -eq 0 ]; then preflight_status=1; fi; " ^
    "printf 'PAVE_XCTEST_PREFLIGHT_STATUS=%s\\nPAVE_XCTEST_BUILD_STATUS=0\\nPAVE_XCTEST_EXPORT_STATUS=0\\n' \"$preflight_status\"" in
  let shell = "umask 077; " ^ booted_preflight ^
    "; preflight_status=$?; if [ \"$preflight_status\" -eq 0 ] && " ^
    "/usr/bin/grep -Fq " ^ quote simulator_id ^ " " ^ quote booted_log ^
    "; then " ^ xcodebuild ^
    "; build_status=$?; " ^ export_attachments ^
    "; export_status=$?; " ^ report_status ^
    "; else " ^ report_preflight_failure ^ "; fi" in
  shell, project, derived, result, export, booted_log, log, export_log

let xctest_manifest_attachment manifest =
  if String.length manifest > max_xctest_manifest_bytes then
    fail "XCTest attachment manifest exceeds its byte limit";
  let json = try Yojson.Basic.from_string manifest
    with Yojson.Json_error _ -> fail "XCTest attachment manifest is malformed JSON" in
  let attachments = ref [] in
  let rec visit = function
    | `Assoc fields ->
        let names = List.map fst fields in
        if List.length names <> List.length (List.sort_uniq String.compare names) then
          fail "XCTest attachment manifest contains duplicate object keys";
        (match List.assoc_opt "name" fields with
         | Some (`String name) when name = xctest_attachment_name ->
             let filename = match List.assoc_opt "exportedFileName" fields with
               | Some (`String filename) when filename <> "" &&
                   String.length filename <= 256 &&
                   String.for_all (function
                     | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '_' | '-' -> true
                     | _ -> false) filename -> filename
               | _ -> fail "XCTest result attachment has no safe exported filename" in
             let mime = match List.assoc_opt "uniformTypeIdentifier" fields with
               | Some (`String value) -> value
               | _ -> fail "XCTest result attachment has no content type" in
             if mime <> "public.json" then
               fail "XCTest result attachment has an unexpected content type";
             attachments := filename :: !attachments
         | _ -> ());
        List.iter (fun (_, value) -> visit value) fields
    | `List values -> List.iter visit values
    | _ -> () in
  visit json;
  match !attachments with
  | [filename] -> filename
  | [] -> fail "XCTest result bundle is missing the kept structured response attachment"
  | _ -> fail "XCTest result bundle contains ambiguous structured response attachments"

let xctest_export_payload ~manifest ~files =
  let filename = xctest_manifest_attachment manifest in
  match List.filter (fun (name, _) -> name = filename) files with
  | [_, payload] when String.length payload <= max_xctest_response_bytes -> payload
  | [_, _] -> fail "XCTest response attachment exceeds its byte limit"
  | [] -> fail "XCTest response attachment file is missing from the export"
  | _ -> fail "XCTest response attachment filename is ambiguous"

let xctest_response_field name = function
  | `Assoc fields ->
      let values = List.filter (fun (key, _) -> key = name) fields in
      (match values with
       | [_, value] -> value
       | [] -> fail ("XCTest response is missing " ^ name)
       | _ -> fail ("XCTest response contains duplicate " ^ name))
  | _ -> fail "XCTest response must be a JSON object"

let xctest_response_string name json = match xctest_response_field name json with
  | `String value -> value
  | _ -> fail ("XCTest response " ^ name ^ " must be a string")

let xctest_response_integer name json = match xctest_response_field name json with
  | `Int value when value >= 0 -> value
  | _ -> fail ("XCTest response " ^ name ^ " must be a nonnegative integer")

let xctest_response_bool name json = match xctest_response_field name json with
  | `Bool value -> value
  | _ -> fail ("XCTest response " ^ name ^ " must be a boolean")

let rec reject_duplicate_json_keys = function
  | `Assoc fields ->
      let keys = List.map fst fields in
      if List.length keys <> List.length (List.sort_uniq String.compare keys) then
        fail "XCTest response contains duplicate JSON object keys";
      List.iter (fun (_, value) -> reject_duplicate_json_keys value) fields
  | `List values -> List.iter reject_duplicate_json_keys values
  | _ -> ()

let parse_xctest_response ~bundle_id response =
  if String.length response > max_xctest_response_bytes then
    fail "XCTest response exceeds its byte limit";
  let json = try Yojson.Basic.from_string response
    with Yojson.Json_error _ -> fail "XCTest response attachment is malformed JSON" in
  reject_duplicate_json_keys json;
  let version = xctest_response_integer "version" json in
  if version <> 1 ||
     xctest_response_string "bundle_id" json <> bundle_id ||
     xctest_response_string "operation" json <> xctest_operation then
    fail "XCTest response identity does not match the approved request";
  let status = xctest_response_string "status" json in
  if status = "error" then
    let message = xctest_response_string "error" json in
    fail ("XCTest helper failed: " ^ message)
  else if status <> "available" then
    fail "XCTest response status is unknown";
  let nodes = match xctest_response_field "nodes" json with
    | `List nodes when List.length nodes >= 1 && List.length nodes <= max_nodes -> nodes
    | `List _ -> fail "XCTest accessibility response has an invalid node count"
    | _ -> fail "XCTest accessibility nodes must be an array" in
  let node_array = Array.of_list nodes in
  if xctest_response_integer "node_count" json <> Array.length node_array then
    fail "XCTest accessibility node count does not match the response";
  let seen = Hashtbl.create (Array.length node_array) in
  Array.iteri (fun index node ->
    if xctest_response_integer "index" node <> index then
      fail "XCTest accessibility node indexes are not contiguous";
    let depth = xctest_response_integer "depth" node in
    if depth > 128 then fail "XCTest accessibility depth exceeds its limit";
    let parent = xctest_response_field "parent" node in
    (match index, parent with
     | 0, `Null when depth = 0 -> ()
     | 0, _ -> fail "XCTest accessibility root has an invalid parent or depth"
     | _, `Int parent_index when parent_index >= 0 && parent_index < index &&
         Hashtbl.mem seen parent_index &&
         xctest_response_integer "depth" node_array.(parent_index) + 1 = depth -> ()
     | _ -> fail "XCTest accessibility parent/depth relationships are inconsistent");
    Hashtbl.add seen index ();
    List.iter (fun field ->
      let value = xctest_response_string field node in
      if String.length value > 4096 then
        fail ("XCTest accessibility " ^ field ^ " exceeds its byte limit"))
      ["role"; "type"; "identifier"; "label"; "text"; "description"];
    (match xctest_response_field "value" node with
     | `Null | `String _ | `Bool _ | `Int _ | `Float _ -> ()
     | _ -> fail "XCTest accessibility value has an unsupported JSON type");
    ignore (xctest_response_bool "enabled" node);
    ignore (xctest_response_bool "hittable" node);
    (match xctest_response_field "bounds" node with
     | `Assoc fields ->
         List.iter (fun key -> match List.assoc_opt key fields with
           | Some (`Float value) when Float.is_finite value &&
               (key = "x" || key = "y" || value >= 0.) -> ()
           | Some (`Int value) when key = "x" || key = "y" || value >= 0 -> ()
           | _ -> fail ("XCTest accessibility bounds has invalid " ^ key))
           ["x"; "y"; "width"; "height"]
     | _ -> fail "XCTest accessibility bounds must be an object"))
    node_array;
  `Assoc [
    "status", `String "available";
    "content_trust", `String "untrusted";
    "labels_are_untrusted", `Bool true;
    "node_count", `Int (Array.length node_array);
    "nodes", `List nodes]
