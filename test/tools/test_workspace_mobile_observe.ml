module Observe = Pave.Workspace_mobile_observe

let expect label condition = if not condition then failwith label
let rejects label action =
  try ignore (action ()); failwith ("mobile observation accepted " ^ label)
  with Observe.Error _ -> ()

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
    add32 0 in
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
  let xml = "<?xml version='1.0'?><hierarchy rotation='0'><node class='android.widget.FrameLayout' text='' bounds='[0,0][100,100]' enabled='true'><node class=\"android.widget.Button\" text=\"Go &amp; now&#10;green\" resource-id=\"dev.example:id/go\" content-desc=\"Continue\" bounds=\"[2,3][40,20]\" clickable=\"true\" enabled=\"true\" selected=\"false\" /></node></hierarchy>" in
  let nodes = Observe.parse_accessibility xml in
  expect "accessibility parent and depth"
    (List.length nodes = 2 && (List.nth nodes 0).parent = None &&
     (List.nth nodes 1).parent = Some 0 && (List.nth nodes 1).depth = 1);
  let button = List.nth nodes 1 in
  expect "accessibility labels and role decoded"
    (button.role = "android.widget.Button" && button.text = "Go & now\ngreen" &&
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
  print_endline "workspace mobile screen observation: ok"
