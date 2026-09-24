let () =
  if Array.length Sys.argv <> 3 then failwith "expected installer input and generated output";
  let input = open_in_bin Sys.argv.(1) in
  let script = Fun.protect ~finally:(fun () -> close_in input) (fun () ->
    let length = in_channel_length input in
    if length > 1_048_576 then failwith "installer script is unexpectedly large";
    really_input_string input length) in
  let output = open_out_bin Sys.argv.(2) in
  Fun.protect ~finally:(fun () -> close_out output) (fun () ->
    Printf.fprintf output "(* Generated from install.sh; do not edit. *)\nlet script = %S\n" script)
