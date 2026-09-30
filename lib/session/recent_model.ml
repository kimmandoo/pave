(* Last used model (or explicit interactive selection) is private,
   workspace-scoped UI state, not a journal entry or a configured default.
   Reuse the journal's exact identity format and the session store's owned,
   atomic metadata operations. *)
let filename = "last-model.json"
let max_bytes = 16_384

let load ~root =
  let dir = Session_store.directory ~root in
  let base = Session_store.base_dir () in
  let private_directory =
    try
      Session_store.safe_directory (Filename.dirname base);
      Session_store.safe_directory base;
      Session_store.safe_directory dir;
      true
    with Unix.Unix_error _ | Invalid_argument _ -> false in
  if not private_directory then None
  else match Session_store.read_private_file
      (Filename.concat dir filename) max_bytes with
  | None -> None
  | Some text ->
      (try Some (Session.parse_model_identity
        (Yojson.Basic.from_string text)) with
       | Yojson.Json_error _ | Invalid_argument _
       | Protocol.Invalid_response _ -> None)

let save ~root (identity : Model_identity.t) =
  let text = Yojson.Basic.to_string (Session.model_identity_json identity) ^ "\n" in
  if String.length text > max_bytes then
    invalid_arg "recent model identity exceeds storage limit";
  let dir = Session_store.ensure ~root in
  Session_store.with_file_lock (Filename.concat dir "last-model.lock")
    (fun () ->
      (* Check disk while holding the lock, rather than caching in the process:
         another CLI instance may have used a different model since our turn. *)
      if load ~root <> Some identity then
        Session_store.write_atomic ~dir ~prefix:".last-model-"
          (Filename.concat dir filename) text)
