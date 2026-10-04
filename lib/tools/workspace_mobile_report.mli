exception Error of string

type identity = {
  platform : string; device : string; app_id : string; app_path : string;
  scheme : string option; variant : string option; build_hash : string;
}
type artifact_kind = Visual | Scenario | Diagnostic
type artifact = { kind : artifact_kind; path : string; sha256 : string; identity : identity }
type verified_check
type report

val hash_build : string -> string -> string
val build_identity : string -> Workspace_mobile_run.session -> identity
val process_check : name:string -> Workspace_process.result -> verified_check
val not_run_check : name:string -> verified_check
val create : root:string -> session:Workspace_mobile_run.session -> sources:Workspace_edit.preview list -> checks:verified_check list -> artifacts:artifact list -> report
val projection : report -> string
val write : root:string -> path:string -> cancelled:(unit -> bool) -> report -> unit
