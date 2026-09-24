let () =
  if Array.length Sys.argv <> 3 then failwith "expected installer input and generated output";
  let input = open_in_bin Sys.argv.(1) in
  let script = Fun.protect ~finally:(fun () -> close_in input) (fun () ->
    let length = in_channel_length input in
    if length > 1_048_576 then failwith "installer script is unexpectedly large";
    really_input_string input length) in
  let version = match Sys.getenv_opt "PAVE_RELEASE_VERSION" with
    | None -> "source"
    | Some tag ->
        let valid = String.length tag >= 6 && tag.[0] = 'v' &&
          match String.split_on_char '.'
            (String.sub tag 1 (String.length tag - 1)) with
          | [ major; minor; patch ] ->
              List.for_all (fun part -> part <> "" &&
                String.for_all (function '0'..'9' -> true | _ -> false) part)
                [ major; minor; patch ]
          | _ -> false in
        if not valid then failwith "PAVE_RELEASE_VERSION must be a release tag such as v0.1.6";
        tag in
  let output = open_out_bin Sys.argv.(2) in
  Fun.protect ~finally:(fun () -> close_out output) (fun () ->
    Printf.fprintf output
      "(* Generated from install.sh; do not edit. *)\nlet script = %S\nlet version = %S\n"
      script version)
