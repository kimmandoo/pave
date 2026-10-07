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

let test_stdin_descriptor_ownership () =
  let calls = ref 0 and reserved = ref None and borrowed = ref None in
  Fun.protect ~finally:(fun () ->
    List.iter (Option.iter (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ()))
      [!reserved; !borrowed])
    (fun () ->
      let cancel () =
        incr calls;
        if !calls = 1 then
          reserved := Some (Unix.openfile "/dev/null" [Unix.O_RDONLY] 0);
        if !calls = 2 then
          borrowed := Some (Unix.openfile "/dev/null" [Unix.O_RDONLY] 0);
        false in
      let result = Services.run_native ~cancel ~timeout_seconds:1. ~output_limit:0
        ~program:"/bin/sh" ~arguments:["-c"; "sleep 0.05"] ~stdin:"" in
      expect "helper completes after closing its empty stdin" (result.termination = Process.Exited 0);
      match !borrowed with
      | None -> fail "descriptor reuse callback was not reached"
      | Some fd ->
          expect "helper cleanup preserves a descriptor reused after stdin EOF"
            (try ignore (Unix.fstat fd); true with Unix.Unix_error _ -> false))

let test_exceptional_helper_cleanup () =
  let calls = ref 0 in
  let started = Unix.gettimeofday () in
  let raised =
    try
      ignore (Services.run_native
        ~cancel:(fun () -> incr calls; if !calls = 2 then failwith "callback failure"; false)
        ~timeout_seconds:20. ~output_limit:0 ~program:"/bin/sh"
        ~arguments:["-c"; "exec sleep 10"] ~stdin:"");
      false
    with Failure message when message = "callback failure" -> true in
  expect "callback exceptions propagate after helper cleanup" raised;
  expect "exceptional cleanup does not wait for a live helper"
    (Unix.gettimeofday () -. started < 2.)

let test_cancelled_helper_descendants () =
  let ready = Filename.temp_file "pave-native-ready-" "" in
  let survived = Filename.temp_file "pave-native-survived-" "" in
  Unix.unlink ready; Unix.unlink survived;
  Fun.protect ~finally:(fun () ->
    List.iter (fun path -> try Unix.unlink path with Unix.Unix_error _ -> ()) [ready; survived])
    (fun () ->
      let result = Services.run_native ~cancel:(fun () -> Sys.file_exists ready)
        ~timeout_seconds:2. ~output_limit:0 ~program:"/bin/sh"
        ~arguments:["-c";
          "(/bin/sleep 0.3; printf survived > \"$2\") & printf ready > \"$1\"; wait";
          "fixture"; ready; survived] ~stdin:"" in
      expect "cancellation returns the cancelled helper outcome"
        (result.termination = Process.Cancelled);
      Thread.delay 0.4;
      expect "cancelled helper descendants cannot finish their effect"
        (not (Sys.file_exists survived)))

let () =
  test_stdin_descriptor_ownership ();
  test_exceptional_helper_cleanup ();
  test_cancelled_helper_descendants ();
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
