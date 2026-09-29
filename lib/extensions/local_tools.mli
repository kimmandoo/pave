type source = User_manifest of string
type tool
type registry
type session

type error =
  | Invalid of string
  | Unavailable of string
  | Approval_required
  | Denied
  | Cancelled
  | Timed_out
  | Exit of int * string
  | Signaled of int * string
  | Runner_failed of string

type invocation = {
  program : string;
  arguments : string list;
  cwd : string;
  stdin : string;
  timeout_seconds : int;
  output_limit : int;
}

type runner = cancel:(unit -> bool) -> invocation -> (string, error) result

type approval = {
  name : string;
  source : source;
  workspace : string;
  program : string;
  arguments : string list;
  timeout_seconds : int;
  environment : (string * string) list;
  parameters : Yojson.Basic.t;
  input : Yojson.Basic.t;
}

type hook_event =
  | Session_started
  | Turn_started
  | Before_tool of string
  | After_tool of string * (string, error) result
  | Turn_finished

(* The manifest is an explicitly chosen private user-owned JSON file under
   user_dir, outside root. No workspace/plugin discovery or executable loading
   is implicit. Built-in tool names must include every currently advertised
   reserved name, even if disabled for this request. The result is atomic. *)
val load : user_dir:string -> root:string -> builtins:string list -> string ->
  (registry, error) result
val definitions : registry -> Yojson.Basic.t list
val find : registry -> string -> tool option
val tool_name : tool -> string
val tool_source : tool -> source
val tool_invocation : tool -> string * string list * int
val validate_input : Yojson.Basic.t -> Yojson.Basic.t -> unit

val create_session : owner:string -> root:string -> registry:registry ->
  opt_in:bool -> session
val session_owner : session -> string
val cancelled : session -> bool
val cancel : session -> unit
val dispose : session -> unit
(* Only application-owned callback code attributed to a trusted user manifest
   present in the session registry may subscribe. Callbacks must not reenter
   this session and must return promptly; cancellation waits for an active
   callback to finish and prevents all later callbacks. *)
val subscribe : session -> source:source -> (hook_event -> unit) -> bool
val emit : session -> hook_event -> unit

(* Headless denies before calling approve or runner. Approve must ask the user
   interactively for each invocation, independently of normal tool policy.
   The caller settles the returned result once against its provider call ID. *)
val invoke : ?runner:runner -> session -> name:string ->
  input:Yojson.Basic.t -> interactive:bool ->
  approve:(approval -> bool) -> (string, error) result
val real_runner : runner
