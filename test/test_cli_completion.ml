let fail label = failwith ("cli completion: " ^ label)
let expect label condition = if not condition then fail label

let () =
  let candidates = [
    "openai@responses/MODEL";
    "openai@chat#team/MODEL";
    "session with spaces";
    "Other"
  ] in
  expect "prefix selects exact local selectors"
    (Cli_completion.filter_candidates ~prefix:"openai@chat"
      candidates = ["openai@chat#team/MODEL"]);
  expect "empty prefix preserves exact local candidate order"
    (Cli_completion.filter_candidates ~prefix:"" candidates = candidates);
  expect "filter is case sensitive"
    (Cli_completion.filter_candidates ~prefix:"other" candidates = []);
  expect "no candidate is synthesized"
    (Cli_completion.filter_candidates ~prefix:"provider/" candidates = []);
  print_endline "cli completion: ok"
