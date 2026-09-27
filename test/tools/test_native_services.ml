module Services = Pave.Native_services
module Process = Pave.Workspace_process

let fail label = failwith ("native services: " ^ label)
let expect label condition = if not condition then fail label
let contains text fragment =
  let n = String.length text and m = String.length fragment in
  let rec search i = i + m <= n &&
    (String.sub text i m = fragment || search (i + 1)) in
  search 0

let result ?(termination = Process.Exited 0) ?(output = "") ?(bytes_received = 0)
    ?(truncated = false) () =
  { Process.termination = termination; output; bytes_received; truncated }

let runner ?(termination = Process.Exited 0) ?(output = "") ?(bytes_received = 0)
    ?(truncated = false) capture =
  { Services.run = (fun ~cancel ~timeout_seconds ~output_limit ~program ~arguments ~stdin ->
      capture := Some (program, arguments, stdin, output_limit, timeout_seconds);
      if cancel () then result ~termination:Process.Cancelled ()
      else result ~termination ~output ~bytes_received ~truncated ()) }

let expect_error label fragment fn =
  match fn () with
  | _ -> fail (label ^ " was accepted")
  | exception Services.Error message ->
      expect (label ^ " error detail")
        (let n = String.length message and m = String.length fragment in
         let rec search i = i + m <= n &&
           (String.sub message i m = fragment || search (i + 1)) in
         search 0)

let getenv values name = List.assoc_opt name values

let () =
  let previous_auth = Sys.getenv_opt "OPENAI_API_KEY" in
  let auth_sentinel = "pave-native-helper-auth-secret" in
  Unix.putenv "OPENAI_API_KEY" auth_sentinel;
  Fun.protect ~finally:(fun () ->
    Unix.putenv "OPENAI_API_KEY" (Option.value ~default:"" previous_auth))
    (fun () ->
      let isolated = Services.run_native ~cancel:(fun () -> false)
        ~timeout_seconds:1. ~output_limit:4096 ~program:"/usr/bin/env"
        ~arguments:[] ~stdin:"" in
      expect "fixed local helper does not inherit provider credentials"
        (isolated.termination = Process.Exited 0 &&
         not (String.contains isolated.output '\000') &&
         not (contains isolated.output auth_sentinel)));
  let capture = ref None in
  let png = "\x89PNG\r\n\x1a\nimage-data" in
  let recognized = Services.recognize ~runner:(runner ~output:"recognized" ~bytes_received:10 capture)
      ~find_tesseract:(fun () -> Some "/fixed/tesseract") ~mime:"image/png" png in
  expect "OCR returns nonempty recognized text" (recognized = Services.Available "recognized");
  expect "OCR uses fixed Tesseract args and raw stdin"
    (!capture = Some ("/fixed/tesseract", ["stdin"; "stdout"], png,
                      Services.max_ocr_output_bytes, Services.operation_timeout_seconds));
  expect_error "MIME mismatch" "do not match" (fun () ->
    Services.recognize ~runner:(runner capture) ~find_tesseract:(fun () -> Some "/fixed/tesseract")
      ~mime:"image/jpeg" png);
  expect_error "unsupported MIME" "MIME type" (fun () ->
    Services.recognize ~runner:(runner capture) ~mime:"image/webp" png);
  expect_error "image bound" "10 MiB" (fun () ->
    Services.recognize ~runner:(runner capture) ~mime:"image/png"
      ("\x89PNG\r\n\x1a\n" ^ String.make Services.max_image_bytes 'x'));
  expect_error "OCR cancellation" "cancelled" (fun () ->
    Services.recognize ~runner:(runner ~termination:Process.Cancelled capture)
      ~find_tesseract:(fun () -> Some "/fixed/tesseract") ~mime:"image/png" png);
  expect_error "OCR output bound" "size limit" (fun () ->
    Services.recognize ~runner:(runner ~output:"too long"
      ~bytes_received:(Services.max_ocr_output_bytes + 1) capture)
      ~find_tesseract:(fun () -> Some "/fixed/tesseract") ~mime:"image/png" png);
  expect "missing Tesseract is unavailable"
    (Services.recognize ~find_tesseract:(fun () -> None) ~mime:"image/png" png =
       Services.Unavailable "OCR is unavailable: Tesseract is not installed at a supported fixed path");

  let no_env _ = None in
  expect "Wayland read helper selection"
    (Services.clipboard_helper ~getenv:(getenv ["WAYLAND_DISPLAY", "wayland-0"])
       ~os_type:"Unix" ~system_library_exists:(fun () -> false) ~write:false () =
       Some ("/usr/bin/wl-paste", ["--no-newline"]));
  expect "Wayland write helper selection"
    (Services.clipboard_helper ~getenv:(getenv ["WAYLAND_DISPLAY", "wayland-0"])
       ~os_type:"Unix" ~system_library_exists:(fun () -> false) ~write:true () =
       Some ("/usr/bin/wl-copy", []));
  expect "X11 read helper selection"
    (Services.clipboard_helper ~getenv:(getenv ["DISPLAY", ":0"])
       ~os_type:"Unix" ~system_library_exists:(fun () -> false) ~write:false () =
       Some ("/usr/bin/xclip", ["-selection"; "clipboard"; "-out"]));
  expect "X11 write helper selection"
    (Services.clipboard_helper ~getenv:(getenv ["DISPLAY", ":0"])
       ~os_type:"Unix" ~system_library_exists:(fun () -> false) ~write:true () =
       Some ("/usr/bin/xclip", ["-selection"; "clipboard"; "-in"]));
  expect "macOS helper selection"
    (Services.clipboard_helper ~getenv:no_env ~os_type:"Unix"
       ~system_library_exists:(fun () -> true) ~write:false () =
       Some ("/usr/bin/pbpaste", []));
  expect "unsupported platform selection"
    (Services.clipboard_helper ~getenv:no_env ~os_type:"Win32"
       ~system_library_exists:(fun () -> false) ~write:false () = None);

  let wayland = getenv ["WAYLAND_DISPLAY", "wayland-0"] in
  let read_runner = runner ~output:"" ~bytes_received:0 capture in
  expect "empty clipboard is successful, not unavailable"
    (Services.clipboard ~runner:read_runner ~helper_exists:(fun _ -> true) ~getenv:wayland
       ~os_type:"Unix" ~system_library_exists:(fun () -> false) () = Services.Available "");
  expect "clipboard read command and no-newline args"
    (!capture = Some ("/usr/bin/wl-paste", ["--no-newline"], "",
                      Services.max_clipboard_bytes, Services.operation_timeout_seconds));
  let written = Services.clipboard_write ~runner:(runner capture) ~helper_exists:(fun _ -> true)
      ~getenv:wayland ~os_type:"Unix" ~system_library_exists:(fun () -> false) "secret" in
  expect "clipboard write succeeds" (written = Services.Available ());
  expect "clipboard write command, args, and stdin"
    (!capture = Some ("/usr/bin/wl-copy", [], "secret", 0, Services.operation_timeout_seconds));
  expect_error "clipboard input bound" "1 MiB" (fun () ->
    Services.clipboard_write ~runner:(runner capture) ~helper_exists:(fun _ -> true)
      ~getenv:wayland ~os_type:"Unix" ~system_library_exists:(fun () -> false)
      (String.make (Services.max_clipboard_bytes + 1) 'x'));
  expect_error "nonzero helper status" "exit 9" (fun () ->
    Services.clipboard ~runner:(runner ~termination:(Process.Exited 9) capture)
      ~helper_exists:(fun _ -> true) ~getenv:wayland ~os_type:"Unix"
      ~system_library_exists:(fun () -> false) ());
  expect_error "cancelled helper" "cancelled" (fun () ->
    Services.clipboard ~runner:(runner ~termination:Process.Cancelled capture)
      ~helper_exists:(fun _ -> true) ~getenv:wayland ~os_type:"Unix"
      ~system_library_exists:(fun () -> false) ());
  expect_error "clipboard output bound" "size limit" (fun () ->
    Services.clipboard ~runner:(runner ~output:"oversized" ~bytes_received:(Services.max_clipboard_bytes + 1)
      capture) ~helper_exists:(fun _ -> true) ~getenv:wayland ~os_type:"Unix"
      ~system_library_exists:(fun () -> false) ());
  expect "unsupported clipboard platform returns unavailable"
    (Services.clipboard ~getenv:no_env ~os_type:"Win32"
       ~system_library_exists:(fun () -> false) () |> function Services.Unavailable _ -> true | _ -> false);
  print_endline "native OCR and clipboard boundaries: ok"
