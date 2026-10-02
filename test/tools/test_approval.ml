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
    { A.match_text = "rm -rf *"; policy = A.Deny; exact = false };
    { A.match_text = "echo *"; policy = A.Allow; exact = false }
  ] in
  let compound_deny = A.command_decision deny_rules
    "echo safe && rm -rf /tmp/pave-marker" in
  expect "deny detects a destructive subcommand despite allow"
    (compound_deny.policy = Some A.Deny);
  let allow_rule = [{ A.match_text = "git status*"; policy = A.Allow;
                      exact = false }] in
  expect "allow recognizes one simple command"
    ((A.command_decision allow_rule "git status --short").policy = Some A.Allow);
  let compound_allow = A.command_decision allow_rule
    "git status --short && touch marker" in
  expect "allow cannot promote a compound command"
    (compound_allow.policy <> Some A.Allow &&
     compound_allow.tier = A.Exec);
  let prompt_rule = [{ A.match_text = "rm *"; policy = A.Prompt;
                       exact = false }] in
  (* Exact rules (persisted w/Always grants) match the literal normalized
     command; `*` inside them is data, not a wildcard. *)
  let exact_rule = [{ A.match_text = "ls *.log"; policy = A.Allow;
                      exact = true }] in
  expect "exact allow matches the literal command"
    ((A.command_decision exact_rule "ls *.log").policy = Some A.Allow);
  expect "exact allow does not widen with *"
    ((A.command_decision exact_rule "ls keep.log").policy <> Some A.Allow);
  expect "prompt pattern detects a later compound segment"
    ((A.command_decision prompt_rule "echo safe; rm file").policy = Some A.Prompt);
  expect "wildcards match without regex semantics"
    (A.glob_matches "rm *" "rm -rf /tmp" &&
     not (A.glob_matches "rm *" "echo rm -rf /tmp"));
  expect "line prompts accept y, yes and the Korean y key, and deny the rest"
    (A.confirmed_answer (Some " Y ") && A.confirmed_answer (Some "yes") &&
     A.confirmed_answer (Some "\xe3\x85\x9b") &&
     not (A.confirmed_answer (Some "")) && not (A.confirmed_answer (Some "n")) &&
     not (A.confirmed_answer (Some "yes please")) && not (A.confirmed_answer None));
  expect "allow-all is offered only for reviewable low-impact tools"
    (A.session_grantable "web_search" && A.session_grantable "write_file" &&
     not (A.session_grantable "run_command") && not (A.session_grantable "start_shell") &&
     not (A.session_grantable "task") && not (A.session_grantable "mcp:server") &&
     not (A.session_grantable "workspace_rewind"));
  expect "line answers map a/all/ㅁ to a session grant only where offered"
    (A.answer_of_line ~session:true (Some " A ") = A.Allow_for_session &&
     A.answer_of_line ~session:true (Some "\xe3\x85\x81") = A.Allow_for_session &&
     A.answer_of_line ~session:false (Some "a") = A.Deny_once &&
     A.answer_of_line ~session:true (Some "y") = A.Allow_once &&
     A.answer_of_line ~session:true None = A.Deny_once);
  print_endline "approval tiers, precedence and compound command policy: ok"
