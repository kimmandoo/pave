type action =
  | Build | Install | Launch | Stop | Observe | Control | Replay | Diagnostics
  | Verify | Save_baseline | Compare_baseline | Accessibility_audit | Environment
  | Deep_link | Lifecycle | Ios_tree | Android_performance | Flutter_integration
  | Ios_performance | Permission | Network

type item = {
  action : action;
  description : string;
  unavailable_reason : string option;
}

val label : action -> string
val key : action -> string
val proposed : action -> bool
val recommended : state:Workspace_mobile_run.state -> artifact_available:bool -> action

val entries :
  platform:Workspace_mobile_run.platform ->
  state:Workspace_mobile_run.state ->
  artifact_available:bool ->
  ios_accessibility_reason:string option ->
  flutter_integration_reason:string option ->
  verification_available:bool ->
  item list
