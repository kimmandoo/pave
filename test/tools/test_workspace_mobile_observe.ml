module Observe = Pave.Workspace_mobile_observe

let expect label condition = if not condition then failwith label
let rejects label action =
  try ignore (action ()); failwith ("mobile observation accepted " ^ label)
  with Observe.Error _ -> ()

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Sys.readdir path |> Array.iter (fun name ->
        remove_tree (Filename.concat path name));
      Unix.rmdir path
  | Unix.S_REG | Unix.S_LNK -> Unix.unlink path
  | _ -> failwith "unexpected XCTest temporary artifact type"

let png width height =
  let buffer = Buffer.create 64 in
  Buffer.add_string buffer "\137PNG\r\n\026\n";
  let add32 value =
    Buffer.add_char buffer (Char.chr ((value lsr 24) land 255));
    Buffer.add_char buffer (Char.chr ((value lsr 16) land 255));
    Buffer.add_char buffer (Char.chr ((value lsr 8) land 255));
    Buffer.add_char buffer (Char.chr (value land 255)) in
  let chunk kind content =
    add32 (String.length content);
    Buffer.add_string buffer kind;
    Buffer.add_string buffer content;
    add32 (Int32.to_int (Pave.Workspace_mobile_visual.crc32 (kind ^ content)
      0 (4 + String.length content))) in
  let dimension value = String.init 4 (fun index ->
    Char.chr ((value lsr (24 - index * 8)) land 255)) in
  chunk "IHDR" (dimension width ^ dimension height ^ "\008\006\000\000\000");
  chunk "IDAT" "\120\156\099\096\000\002\000\000\005\000\001";
  chunk "IEND" "";
  Buffer.contents buffer

let () =
  let root = Observe.validate_png (png 1080 2400) in
  expect "PNG dimensions retained" (root.width = 1080 && root.height = 2400);
  expect "base64 one-byte padding" (Observe.base64_encode "f" = "Zg==");
  expect "base64 two-byte padding" (Observe.base64_encode "fo" = "Zm8=");
  expect "base64 complete group" (Observe.base64_encode "foo" = "Zm9v");
  rejects "non-PNG bytes" (fun () -> Observe.validate_png "not an image");
  rejects "zero PNG width" (fun () -> Observe.validate_png (png 0 3));
  rejects "oversized PNG dimensions" (fun () -> Observe.validate_png (png 16_385 1));
  let valid = png 2 3 in
  List.iter (fun offset ->
    let damaged = Bytes.of_string valid in
    Bytes.set damaged offset (Char.chr (Char.code (Bytes.get damaged offset) lxor 1));
    rejects "PNG checksum corruption" (fun () ->
      Observe.validate_png (Bytes.unsafe_to_string damaged)))
    [16; 29; 41; String.length valid - 1];
  let duplicate_header = String.sub valid 0 33 ^ String.sub valid 8 25 ^
    String.sub valid 33 (String.length valid - 33) in
  rejects "duplicate PNG header" (fun () -> Observe.validate_png duplicate_header);
  let xml = "<?xml version='1.0'?><hierarchy rotation='0'><node class='android.widget.FrameLayout' package='dev.example' text='' bounds='[0,0][100,100]' enabled='true'><node class=\"android.widget.Button\" package=\"dev.example\" text=\"Go &amp; now&#10;green\" resource-id=\"dev.example:id/go\" content-desc=\"Continue\" bounds=\"[2,3][40,20]\" clickable=\"true\" enabled=\"true\" selected=\"false\" /></node></hierarchy>" in
  let nodes = Observe.parse_accessibility xml in
  expect "accessibility parent and depth"
    (List.length nodes = 2 && (List.nth nodes 0).parent = None &&
     (List.nth nodes 1).parent = Some 0 && (List.nth nodes 1).depth = 1);
  let button = List.nth nodes 1 in
  expect "accessibility labels, package and role decoded"
    (button.role = "android.widget.Button" && button.package = "dev.example" &&
     button.text = "Go & now\ngreen" &&
     button.description = "Continue" && button.identifier = "dev.example:id/go" &&
     button.bounds = "[2,3][40,20]" && button.clickable && button.enabled &&
     not button.selected);
  let json = Observe.accessibility_json nodes in
  expect "structured accessibility result" (String.starts_with ~prefix:"{\"status\":\"available\"" json);
  rejects "XML external declaration"
    (fun () -> Observe.parse_accessibility "<!DOCTYPE x [<!ENTITY e SYSTEM 'file:///etc/passwd'>]><hierarchy/>");
  rejects "unclosed node" (fun () -> Observe.parse_accessibility "<hierarchy><node class='Broken'>");
  rejects "empty tree" (fun () -> Observe.parse_accessibility "<hierarchy/>");
  rejects "unsupported entity" (fun () -> Observe.parse_accessibility "<hierarchy><node text='&boom;' /></hierarchy>");
  rejects "ambiguous duplicate XML attributes" (fun () ->
    Observe.parse_accessibility "<hierarchy><node text='failed' text='passed' /></hierarchy>");
  let node = `Assoc [
    "index", `Int 0; "parent", `Null; "depth", `Int 0;
    "role", `String "Application"; "type", `String "XCUIApplication";
    "identifier", `String "sample.app"; "label", `String "Home";
    "text", `String "Home"; "description", `String "Home screen";
    "value", `Null; "enabled", `Bool true; "hittable", `Bool true;
    "bounds", `Assoc ["x", `Int 0; "y", `Int 0;
      "width", `Int 100; "height", `Int 200]] in
  let response ?(operation = Observe.xctest_operation) ?(status = "available")
      ?(node_count = 1) ?(nodes = [node]) extra =
    Yojson.Basic.to_string (`Assoc ([
      "version", `Int 1; "bundle_id", `String "sample.app";
      "operation", `String operation; "status", `String status;
      "node_count", `Int node_count; "nodes", `List nodes] @ extra)) in
  let payload = response [] in
  let parsed = Observe.parse_xctest_response ~bundle_id:"sample.app" payload in
  expect "XCTest accessibility response is marked untrusted"
    (Yojson.Basic.Util.member "content_trust" parsed = `String "untrusted");
  rejects "XCTest response bundle mismatch" (fun () ->
    Observe.parse_xctest_response ~bundle_id:"other.app" payload);
  rejects "XCTest response rejects out-of-scope semantic operations" (fun () ->
    Observe.parse_xctest_response ~bundle_id:"sample.app"
      (response ~operation:"tap" []));
  rejects "XCTest response rejects non-available status" (fun () ->
    Observe.parse_xctest_response ~bundle_id:"sample.app"
      (response ~status:"performed" []));
  rejects "XCTest duplicate response keys" (fun () ->
    Observe.parse_xctest_response ~bundle_id:"sample.app"
      "{\"version\":1,\"version\":1}");
  rejects "oversized XCTest response" (fun () ->
    Observe.parse_xctest_response ~bundle_id:"sample.app"
      (String.make (Observe.max_xctest_response_bytes + 1) 'x'));
  rejects "XCTest request rejects an unbound bundle identifier" (fun () ->
    Observe.xctest_request ~bundle_id:"");
  rejects "XCTest response rejects inconsistent node count" (fun () ->
    Observe.parse_xctest_response ~bundle_id:"sample.app"
      (response ~node_count:2 []));
  rejects "XCTest response rejects negative element sizes" (fun () ->
    let invalid = match node with
      | `Assoc fields -> `Assoc (List.map (function
          | "bounds", _ -> "bounds", `Assoc [
              "x", `Int 0; "y", `Int 0; "width", `Int (-1); "height", `Int 20]
          | field -> field) fields)
      | _ -> assert false in
    Observe.parse_xctest_response ~bundle_id:"sample.app"
      (response ~nodes:[invalid] []));
  let manifest = "{\"tests\":[{\"attachments\":[{\"name\":\"PaveXCTestResponse\",\"uniformTypeIdentifier\":\"public.json\",\"exportedFileName\":\"response.json\"}]}]}" in
  expect "kept XCTest attachment filename is selected"
    (Observe.xctest_manifest_attachment manifest = "response.json");
  expect "XCTest exported payload follows manifest identity"
    (Observe.xctest_export_payload ~manifest
      ~files:["other.json", "wrong"; "response.json", payload] = payload);
  rejects "missing XCTest response attachment" (fun () ->
    Observe.xctest_manifest_attachment "{\"tests\":[]}");
  rejects "ambiguous XCTest response attachments" (fun () ->
    Observe.xctest_manifest_attachment
      "{\"a\":{\"name\":\"PaveXCTestResponse\",\"uniformTypeIdentifier\":\"public.json\",\"exportedFileName\":\"a.json\"},\"b\":{\"name\":\"PaveXCTestResponse\",\"uniformTypeIdentifier\":\"public.json\",\"exportedFileName\":\"b.json\"}}");
  rejects "oversized XCTest attachment manifest" (fun () ->
    Observe.xctest_manifest_attachment
      (String.make (Observe.max_xctest_manifest_bytes + 1) ' '));
  let directory = Filename.temp_file "pave-native-xctest-test-" "" in
  Unix.unlink directory;
  Fun.protect ~finally:(fun () -> remove_tree directory) (fun () ->
    ignore (Observe.create_xctest_project ~directory ~bundle_id:"sample.app");
    let project_file = Filename.concat directory
      "PaveXCTestRunner.xcodeproj/project.pbxproj" in
    if Sys.file_exists "/usr/bin/plutil" then (
      let null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0 in
      Fun.protect ~finally:(fun () -> Unix.close null) (fun () ->
        let pid = Unix.create_process "/usr/bin/plutil"
          [|"/usr/bin/plutil"; "-lint"; project_file|] null null null in
        let _, status = Unix.waitpid [] pid in
        expect "generated Xcode project parses as a property list"
          (status = Unix.WEXITED 0)));
    let private_directory path =
      (Unix.stat path).Unix.st_perm land 0o777 = 0o700 in
    let private_file path =
      (Unix.stat path).Unix.st_perm land 0o777 = 0o600 in
    expect "XCTest project and source directories are private"
      (private_directory directory &&
       private_directory (Filename.concat directory "PaveXCTestRunner.xcodeproj") &&
       private_directory (Filename.concat directory
         "PaveXCTestRunner.xcodeproj/xcshareddata") &&
       private_directory (Filename.concat directory
         "PaveXCTestRunner.xcodeproj/xcshareddata/xcschemes") &&
       private_directory (Filename.concat directory "Host") &&
       private_directory (Filename.concat directory "Tests"));
    expect "XCTest helper project and source files are private"
      (List.for_all private_file [
        Filename.concat directory "Host/PaveXCTestHost.swift";
        Filename.concat directory "Tests/PaveXCTestRunner.swift";
        Filename.concat directory "PaveXCTestRunner.xcodeproj/project.pbxproj";
        Filename.concat directory
          "PaveXCTestRunner.xcodeproj/xcshareddata/xcschemes/PaveXCTestRunner.xcscheme"]));
  print_endline "workspace mobile screen observation: ok"
