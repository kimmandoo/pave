let expect label condition =
  if not condition then failwith ("approval policy: " ^ label)

let decision tier = {
  Pave.Approval.tier = tier;
  policy = None;
  override = false;
  reason = None;
}

let () =
  let module A = Pave.Approval in
  expect "always-ask gates writes and execution"
    (A.mode_approves A.Ask_writes A.Read &&
     not (A.mode_approves A.Ask_writes A.Write) &&
     not (A.mode_approves A.Ask_writes A.Exec));
  expect "write mode gates execution"
    (A.mode_approves A.Ask_exec A.Read &&
     A.mode_approves A.Ask_exec A.Write &&
     not (A.mode_approves A.Ask_exec A.Exec));
  let denied = A.resolve ~mode:A.Auto_all
    ~decision:{ (decision A.Write) with policy = Some A.Deny }
    ~user_policy:(Some A.Allow) in
  expect "argument deny overrides allow and yolo"
    (match denied with A.Denied _ -> true | _ -> false);
  expect "tool prompt overrides global yolo"
    (A.resolve ~mode:A.Auto_all ~decision:(decision A.Write)
      ~user_policy:(Some A.Prompt) = A.Requires_prompt None);
  expect "tool allow overrides always-ask"
    (A.resolve ~mode:A.Ask_writes ~decision:(decision A.Write)
      ~user_policy:(Some A.Allow) = A.Allowed);
  let deny_rules = [
    { A.match_text = "rm -rf *"; policy = A.Deny };
    { A.match_text = "echo *"; policy = A.Allow }
  ] in
  let compound_deny = A.command_decision deny_rules
    "echo safe && rm -rf /tmp/pave-marker" in
  expect "deny detects a destructive subcommand despite allow"
    (compound_deny.policy = Some A.Deny);
  let allow_rule = [{ A.match_text = "git status*"; policy = A.Allow }] in
  expect "allow recognizes one simple command"
    ((A.command_decision allow_rule "git status --short").policy = Some A.Allow);
  let compound_allow = A.command_decision allow_rule
    "git status --short && touch marker" in
  expect "allow cannot promote a compound command"
    (compound_allow.policy <> Some A.Allow &&
     compound_allow.tier = A.Exec);
  let prompt_rule = [{ A.match_text = "rm *"; policy = A.Prompt }] in
  expect "prompt pattern detects a later compound segment"
    ((A.command_decision prompt_rule "echo safe; rm file").policy = Some A.Prompt);
  expect "wildcards match without regex semantics"
    (A.glob_matches "rm *" "rm -rf /tmp" &&
     not (A.glob_matches "rm *" "echo rm -rf /tmp"));
  print_endline "approval tiers, precedence and compound command policy: ok"
