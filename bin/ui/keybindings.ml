type focus = Paste | Approval | Chooser | Search | Hints | Composer

type action =
  | Ignore | Cancel | Accept | Reject | Approve | Next_status
  | Move_up | Move_down | Page_up | Page_down | First | Last | Backspace
  | Cancel_search | Accept_search | Search_older | Search_erase
  | Dismiss_hint | Insert_hint | Accept_hint | Interrupt
  | Submit | Newline | Complete
  | Scroll_up | Scroll_down | Toggle_details | Scroll_to_start | Scroll_to_end
  | Open_queue | Restore_or_history | History_older | History_newer
  | Vertical_up | Vertical_down | Select_up | Select_down
  | Move_left | Move_right | Select_left | Select_right | Word_left | Word_right
  | Erase_word | Erase | Delete | Undo | Redo | Kill_end | Kill_before | Yank
  | Home | End | Beginning_of_line | End_of_line | Select_home | Select_end
  | End_of_input
  | Insert_ascii of char | Insert_uchar of Uchar.t
  | Filter_ascii of char | Filter_uchar of Uchar.t
  | Search_ascii of char | Search_uchar of Uchar.t
  | Paste_newline | Paste_space | Paste_ascii of char | Paste_uchar of Uchar.t

type binding = {
  id : string;
  focus : focus;
  event : Notty.Unescape.event;
  any_modifiers : bool;
  action : action;
  help : string option;
}

type override = {
  target : string;
  event : Notty.Unescape.event;
  any_modifiers : bool;
}

let bind ?(any_modifiers = false) ?help id focus event action =
  { id; focus; event; any_modifiers; action; help }

let exact ?help id focus key modifiers action =
  bind ?help id focus (`Key (key, modifiers)) action

let any ?help id focus key action =
  bind ~any_modifiers:true ?help id focus (`Key (key, [])) action

let event_key = function `Key (key, _) -> Some key | _ -> None

let overlaps left right =
  left.focus = right.focus &&
  if left.any_modifiers || right.any_modifiers then
    match event_key left.event, event_key right.event with
    | Some left, Some right -> left = right
    | _ -> left.event = right.event
  else left.event = right.event

let conflicts bindings =
  let rec collect found = function
    | [] -> List.rev found
    | current :: rest ->
        let pairs = List.filter_map (fun other ->
          if overlaps current other then Some (current.id, other.id) else None)
          rest in
        collect (List.rev_append pairs found) rest in
  collect [] bindings

let apply_overrides bindings overrides =
  let rec duplicate_target seen = function
    | [] -> None
    | override :: rest ->
        if List.mem override.target seen then Some override.target
        else duplicate_target (override.target :: seen) rest in
  match duplicate_target [] overrides with
  | Some target -> Error ("duplicate keybinding override: " ^ target)
  | None ->
      let unknown = List.find_opt (fun override ->
        not (List.exists (fun binding -> binding.id = override.target) bindings))
        overrides in
      (match unknown with
      | Some override -> Error ("unknown keybinding: " ^ override.target)
      | None ->
          let updated = List.map (fun binding ->
            match List.find_opt (fun override ->
              override.target = binding.id) overrides with
            | None -> binding
            | Some override ->
                { binding with event = override.event;
                  any_modifiers = override.any_modifiers }) bindings in
          match conflicts updated with
          | [] -> Ok updated
          | (left, right) :: _ ->
              Error (Printf.sprintf "keybinding conflict: %s and %s" left right))

let focus ~paste ~overlay ~search ~hints =
  if paste then Paste
  else match overlay with
  | Some owner -> owner
  | None when search -> Search
  | None when hints -> Hints
  | None -> Composer

let submit_help =
  (if Sys.os_type = "Unix" && Sys.file_exists "/System/Library" then "Return" else "Enter") ^
  " sends when idle; queues while working · /steer MESSAGE interrupts and sends next"

let slash_help enter_key =
  "/ · live commands; ↑/↓ select · Tab/" ^ enter_key ^ " insert · Esc close"

let bindings =
  let enter_key =
    if Sys.os_type = "Unix" && Sys.file_exists "/System/Library" then "Return"
    else "Enter" in
  let meta_key =
    if Sys.os_type = "Unix" && Sys.file_exists "/System/Library" then "Option"
    else "Alt" in
  let search_help = "Ctrl+R reverse search · Esc cancel search" in
  let history_help = "Ctrl+P/N or " ^ meta_key ^
    "+↓ prompt history · ↑/↓ move in the draft" in
  let queue_help = meta_key ^
    "+↑ restores a queued prompt when available; otherwise prompt history" in
  let edit_help = "Ctrl+A/E line ends · " ^ meta_key ^
    "+B/F move by word · Ctrl+W erase word" in
  let undo_help = "Ctrl+Z/Y undo/redo · Ctrl+K/U kill line · " ^ meta_key ^ "+Y yank" in
  let transcript_help = meta_key ^
    "+O tool details · PgUp/Dn scroll · Ctrl+Home/End transcript" in
  let interrupt_help =
    "Ctrl+C closes a picker; in the composer it interrupts without losing the draft; idle clears it" in
  let paste_help = "Bracketed paste inserts atomically; pasted " ^ enter_key ^ " does not send" in
  [
    any "chooser.escape" Chooser `Escape Cancel;
    exact "chooser.ctrl-c" Chooser (`ASCII 'C') [`Ctrl] Cancel;
    any "chooser.accept" Chooser `Enter Accept;
    any "chooser.status" Chooser `Tab Next_status;
    any "chooser.up" Chooser (`Arrow `Up) Move_up;
    any "chooser.down" Chooser (`Arrow `Down) Move_down;
    any "chooser.page-up" Chooser (`Page `Up) Page_up;
    any "chooser.page-down" Chooser (`Page `Down) Page_down;
    any "chooser.home" Chooser `Home First;
    any "chooser.end" Chooser `End Last;
    any "chooser.backspace" Chooser `Backspace Backspace;

    exact "approval.y" Approval (`ASCII 'y') [] Approve;
    exact "approval.upper-y" Approval (`ASCII 'Y') [] Approve;
    (* The y key under a Korean two-set input method sends ㅛ. *)
    exact "approval.hangul-y" Approval (`Uchar (Uchar.of_int 0x315B)) [] Approve;
    exact "approval.n" Approval (`ASCII 'n') [] Reject;
    exact "approval.upper-n" Approval (`ASCII 'N') [] Reject;
    exact "approval.escape" Approval `Escape [] Reject;
    exact "approval.ctrl-c" Approval (`ASCII 'C') [`Ctrl] Reject;
    exact "approval.enter" Approval `Enter [] Accept;
    exact "approval.tab" Approval `Tab [] Next_status;
    exact "approval.up" Approval (`Arrow `Up) [] Move_up;
    exact "approval.down" Approval (`Arrow `Down) [] Move_down;
    exact "approval.left" Approval (`Arrow `Left) [] Move_up;
    exact "approval.right" Approval (`Arrow `Right) [] Move_down;

    any ~help:search_help "search.escape" Search `Escape Cancel_search;
    exact "search.ctrl-g" Search (`ASCII 'G') [`Ctrl] Cancel_search;
    any "search.enter" Search `Enter Accept_search;
    exact "search.ctrl-r" Search (`ASCII 'R') [`Ctrl] Search_older;
    any "search.backspace" Search `Backspace Search_erase;
    exact ~help:interrupt_help "search.ctrl-c" Search (`ASCII 'C') [`Ctrl] Interrupt;

    any ~help:(slash_help enter_key) "hints.escape" Hints `Escape Dismiss_hint;
    any "hints.tab" Hints `Tab Insert_hint;
    any "hints.enter" Hints `Enter Accept_hint;
    any "hints.up" Hints (`Arrow `Up) Move_up;
    any "hints.down" Hints (`Arrow `Down) Move_down;
    exact "hints.ctrl-r" Hints (`ASCII 'R') [`Ctrl] Search_older;
    exact ~help:interrupt_help "hints.ctrl-c" Hints (`ASCII 'C') [`Ctrl] Interrupt;

    exact ~help:interrupt_help "composer.ctrl-c" Composer (`ASCII 'C') [`Ctrl] Interrupt;
    exact ~help:submit_help "composer.enter" Composer `Enter [] Submit;
    exact ~help:submit_help "composer.meta-enter" Composer `Enter [`Meta] Submit;
    exact "composer.meta-ctrl-m" Composer (`ASCII 'M') [`Meta; `Ctrl] Submit;
    exact "composer.ctrl-enter" Composer `Enter [`Ctrl] Submit;
    exact "composer.shift-enter" Composer `Enter [`Shift] Newline;
    any "composer.tab" Composer `Tab Complete;
    any ~help:transcript_help "composer.page-up" Composer (`Page `Up) Scroll_up;
    any "composer.page-down" Composer (`Page `Down) Scroll_down;
    exact "composer.meta-o" Composer (`ASCII 'o') [`Meta] Toggle_details;
    exact "composer.ctrl-home" Composer `Home [`Ctrl] Scroll_to_start;
    exact "composer.ctrl-end" Composer `End [`Ctrl] Scroll_to_end;
    exact ~help:queue_help "composer.meta-up" Composer (`Arrow `Up) [`Meta] Restore_or_history;
    exact ~help:(meta_key ^ "+Q manages queued prompts without clearing the draft · /queue")
      "composer.meta-q" Composer (`ASCII 'q') [`Meta] Open_queue;
    exact ~help:history_help "composer.ctrl-p" Composer (`ASCII 'P') [`Ctrl] History_older;
    exact ~help:history_help "composer.ctrl-n" Composer (`ASCII 'N') [`Ctrl] History_newer;
    exact "composer.meta-down" Composer (`Arrow `Down) [`Meta] History_newer;
    exact "composer.up" Composer (`Arrow `Up) [] Vertical_up;
    exact "composer.down" Composer (`Arrow `Down) [] Vertical_down;
    exact "composer.ctrl-up" Composer (`Arrow `Up) [`Ctrl] Vertical_up;
    exact "composer.ctrl-down" Composer (`Arrow `Down) [`Ctrl] Vertical_down;
    exact "composer.shift-up" Composer (`Arrow `Up) [`Shift] Select_up;
    exact "composer.shift-down" Composer (`Arrow `Down) [`Shift] Select_down;
    exact "composer.meta-shift-up" Composer (`Arrow `Up) [`Meta; `Shift] Select_up;
    exact "composer.meta-shift-down" Composer (`Arrow `Down) [`Meta; `Shift] Select_down;
    exact "composer.left" Composer (`Arrow `Left) [] Move_left;
    exact "composer.right" Composer (`Arrow `Right) [] Move_right;
    exact "composer.shift-left" Composer (`Arrow `Left) [`Shift] Select_left;
    exact "composer.shift-right" Composer (`Arrow `Right) [`Shift] Select_right;
    exact "composer.ctrl-left" Composer (`Arrow `Left) [`Ctrl] Word_left;
    exact "composer.ctrl-right" Composer (`Arrow `Right) [`Ctrl] Word_right;
    exact "composer.meta-left" Composer (`Arrow `Left) [`Meta] Word_left;
    exact "composer.meta-right" Composer (`Arrow `Right) [`Meta] Word_right;
    exact "composer.meta-shift-left" Composer (`Arrow `Left) [`Meta; `Shift] Select_left;
    exact "composer.meta-shift-right" Composer (`Arrow `Right) [`Meta; `Shift] Select_right;
    exact ~help:edit_help "composer.meta-b" Composer (`ASCII 'b') [`Meta] Word_left;
    exact ~help:edit_help "composer.meta-f" Composer (`ASCII 'f') [`Meta] Word_right;
    exact ~help:edit_help "composer.ctrl-w" Composer (`ASCII 'W') [`Ctrl] Erase_word;
    exact "composer.meta-backspace" Composer `Backspace [`Meta] Erase_word;
    exact "composer.ctrl-backspace" Composer `Backspace [`Ctrl] Erase;
    exact "composer.backspace" Composer `Backspace [] Erase;
    any "composer.delete" Composer `Delete Delete;
    exact ~help:undo_help "composer.ctrl-z" Composer (`ASCII 'Z') [`Ctrl] Undo;
    exact ~help:undo_help "composer.ctrl-y" Composer (`ASCII 'Y') [`Ctrl] Redo;
    exact ~help:undo_help "composer.ctrl-k" Composer (`ASCII 'K') [`Ctrl] Kill_end;
    exact ~help:undo_help "composer.ctrl-u" Composer (`ASCII 'U') [`Ctrl] Kill_before;
    exact ~help:undo_help "composer.meta-y" Composer (`ASCII 'y') [`Meta] Yank;
    exact "composer.home" Composer `Home [] Home;
    exact "composer.shift-home" Composer `Home [`Shift] Select_home;
    exact "composer.end" Composer `End [] End;
    exact "composer.shift-end" Composer `End [`Shift] Select_end;
    exact ~help:edit_help "composer.ctrl-a" Composer (`ASCII 'A') [`Ctrl] Beginning_of_line;
    exact ~help:edit_help "composer.ctrl-e" Composer (`ASCII 'E') [`Ctrl] End_of_line;
    exact ~help:search_help "composer.ctrl-r" Composer (`ASCII 'R') [`Ctrl] Search_older;
    exact "composer.ctrl-d" Composer (`ASCII 'D') [`Ctrl] End_of_input;
    bind ~help:paste_help "bracketed-paste" Paste (`Paste `Start) Ignore;
  ]
let same_key left right =
  match left, right with
  | `Key (left, _), `Key (right, _) -> left = right
  | _ -> false

let fallback focus event =
  match focus, event with
  | Paste, `Key (`Enter, _) -> Some Paste_newline
  | Paste, `Key (`Tab, _) -> Some Paste_space
  | Paste, `Key (`ASCII char, []) when Char.code char >= 32 ->
      Some (Paste_ascii char)
  | Paste, `Key (`Uchar uchar, []) -> Some (Paste_uchar uchar)
  | Paste, _ -> Some Ignore
  | Chooser, `Key (`ASCII char, []) when Char.code char >= 32 ->
      Some (Filter_ascii char)
  | Chooser, `Key (`Uchar uchar, []) -> Some (Filter_uchar uchar)
  | Search, `Key (`ASCII char, []) when Char.code char >= 32 ->
      Some (Search_ascii char)
  | Search, `Key (`Uchar uchar, []) -> Some (Search_uchar uchar)
  | (Hints | Composer), `Key (`ASCII char, []) when Char.code char >= 32 ->
      Some (Insert_ascii char)
  | (Hints | Composer), `Key (`Uchar uchar, []) -> Some (Insert_uchar uchar)
  (* Only explicit choices decide an approval; typing and unknown keys are inert. *)
  | Approval, `Key _ -> Some Ignore
  | _ -> None

(* Korean two-set layout: compatibility jamo sent for each Latin key. *)
let hangul_keys = [
  0x3142, 'q'; 0x3148, 'w'; 0x3137, 'e'; 0x3131, 'r'; 0x3145, 't';
  0x315B, 'y'; 0x3155, 'u'; 0x3151, 'i'; 0x3150, 'o'; 0x3154, 'p';
  0x3141, 'a'; 0x3134, 's'; 0x3147, 'd'; 0x3139, 'f'; 0x314E, 'g';
  0x3157, 'h'; 0x3153, 'j'; 0x314F, 'k'; 0x3163, 'l'; 0x314B, 'z';
  0x314C, 'x'; 0x314A, 'c'; 0x314D, 'v'; 0x3160, 'b'; 0x315C, 'n';
  0x3161, 'm'; 0x3143, 'Q'; 0x3149, 'W'; 0x3138, 'E'; 0x3132, 'R';
  0x3146, 'T'; 0x3152, 'O'; 0x3156, 'P'
]

let rec resolve bindings focus event =
  match event with
  | `Key (`Uchar uchar, (_ :: _ as modifiers)) ->
      (* A shortcut typed with a Korean input method still means its Latin
         key; plain jamo stay text. *)
      (match List.assoc_opt (Uchar.to_int uchar) hangul_keys with
       | Some key -> resolve bindings focus (`Key (`ASCII key, modifiers))
       | None -> resolve_exact bindings focus event)
  | _ -> resolve_exact bindings focus event

and resolve_exact bindings focus event =
  let rec find owner = function
    | [] -> None
    | binding :: rest when binding.focus <> owner -> find owner rest
    | binding :: _ when binding.any_modifiers &&
        same_key binding.event event -> Some binding.action
    | binding :: _ when binding.event = event -> Some binding.action
    | _ :: rest -> find owner rest in
  match find focus bindings with
  | Some _ as action -> action
  | None when focus = Hints ->
      (match find Composer bindings with
       | Some _ as action -> action
       | None -> fallback Hints event)
  | None -> fallback focus event

let override ~target ~event ?(any_modifiers = false) () =
  { target; event; any_modifiers }


let hotkeys bindings =
  List.fold_left (fun help binding -> match binding.help with
    | Some line when not (List.mem line help) -> line :: help
    | _ -> help) [] bindings
  |> List.rev


let () = match conflicts bindings with
  | [] -> ()
  | (left, right) :: _ ->
      invalid_arg (Printf.sprintf "default keybinding conflict: %s and %s"
        left right)
