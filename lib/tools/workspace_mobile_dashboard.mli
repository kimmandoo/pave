type item = { action : string; unavailable_reason : string option }

val entries :
  platform:Workspace_mobile_run.platform ->
  state:Workspace_mobile_run.state ->
  ios_accessibility_reason:string option ->
  flutter_integration_reason:string option ->
  verification_available:bool ->
  item list
