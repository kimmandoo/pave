(* Sensitive workspace-mutation review (M21b/M22b).

   This module owns the narrow contract between the sensitive-diff classifier
   (M21a/M22a) and the write/edit/apply/AST/LSP mutation paths in [Tools].

   Contract for the classifier:
   - [classify ~root change] inspects one proposed single-file change.
     [change.before] is [None] for a new file or when the file's current bytes
     could not be confirmed against the reviewed snapshot; the classifier must
     not guess content it was not given.
   - [Ordinary] keeps the existing file-write approval policy unchanged.
   - [Sensitive effects] attaches exact per-file effects to a distinct
     exact-content authorization; effect text is bounded here.
   - [Unresolved reason] is a fail-closed outcome for mobile-relevant changes
     the classifier cannot decide (for example dynamic signing configuration);
     it receives the same distinct review as [Sensitive], labeled unresolved.
   - The classifier must be pure and bounded: no process execution, no
     keychain/keystore access, no writes. Exceptions are caught and reported
     as [Unresolved].
   - Until the classifier slice installs a classifier, [classifier_ref] is
     [None] and every change reviews as [Ordinary]: mutation paths behave
     exactly as before and no file is read for classification. *)

type change = {
  path : string;
  before : string option;
  after : string;
}

type classification =
  | Ordinary
  | Sensitive of Approval.sensitive_effect list
  | Unresolved of string

type classifier = root:string -> change -> classification

(* A proposed write of one file. [original_sha256] is the file hash the
   review was computed against, or a non-hex marker such as "new-file" or
   "unreadable" when no confirmed original exists. [before] carries the
   confirmed original bytes when available so the classifier sees the real
   removed content. *)
type proposed = {
  path : string;
  original_sha256 : string;
  before : string option;
  after : string;
}

let new_file_sha256 = "new-file"
let unreadable_sha256 = "unreadable"

let classifier_ref : classifier option ref = ref None

let set_classifier classifier = classifier_ref := classifier
let installed () = !classifier_ref <> None

let max_effects = 32
let max_text_bytes = 256

let bound_text text =
  if String.length text <= max_text_bytes then text
  else
    let rec boundary index =
      if index > 0 && index < String.length text &&
         (Char.code text.[index] land 0xc0) = 0x80 then boundary (index - 1)
      else index in
    let length = boundary max_text_bytes in
    String.sub text 0 length ^ "…"

let bound_effect (item : Approval.sensitive_effect) =
  { Approval.effect_path = bound_text item.effect_path;
    effect_summary = bound_text item.effect_summary }

let classify ~root (change : change) =
  match !classifier_ref with
  | None -> Ordinary
  | Some classify ->
      (try
         match classify ~root change with
         | Ordinary -> Ordinary
         | Sensitive effects -> Sensitive (List.map bound_effect effects)
         | Unresolved reason -> Unresolved (bound_text reason)
       with _ -> Unresolved "sensitive-change classification failed")

let sha256 = Workspace_edit.sha256

let target_of_proposed (proposed : proposed) : Approval.sensitive_target =
  { Approval.target_path = proposed.path;
    original_sha256 = proposed.original_sha256;
    result_sha256 = sha256 proposed.after }

(* Fold per-file classifications of every file the call will write. Returns
   [None] when nothing is sensitive or unresolved, or no classifier is
   installed. An oversized or invalid effect set degrades to [Unresolved]
   rather than silently dropping findings. *)
let review ~root proposals : Approval.sensitive_review option =
  if not (installed ()) then None
  else
    let effects, unresolved = List.fold_left
      (fun (effects, unresolved) (proposed : proposed) ->
        match classify ~root
          { path = proposed.path; before = proposed.before;
            after = proposed.after } with
        | Ordinary -> effects, unresolved
        | Sensitive found -> found @ effects, unresolved
        | Unresolved reason -> effects, reason :: unresolved)
      ([], []) proposals in
    if List.length effects > max_effects ||
       List.length unresolved > max_effects then
      Some { Approval.effects = [];
             unresolved = ["too many sensitive findings to review exactly"];
             targets = List.map target_of_proposed proposals }
    else if effects = [] && unresolved = [] then None
    else
      Some { Approval.effects = List.rev effects;
             unresolved = List.rev unresolved;
             targets = List.map target_of_proposed proposals }

let sort_targets (targets : Approval.sensitive_target list) =
  List.sort (fun (a : Approval.sensitive_target) (b : Approval.sensitive_target) ->
    match String.compare a.target_path b.target_path with
    | 0 ->
        (match String.compare a.original_sha256 b.original_sha256 with
         | 0 -> String.compare a.result_sha256 b.result_sha256
         | order -> order)
    | order -> order) targets

(* The authorization is bound to the exact target set: same paths, same
   confirmed original hashes and same proposed result hashes. Any concurrent
   edit or retargeting changes the set and fails the match. *)
let targets_match (approved : Approval.sensitive_target list)
    (computed : Approval.sensitive_target list) =
  sort_targets approved = sort_targets computed
