exception Error of string
val max_output_bytes : int

type action = Boot | Readiness | Shutdown

type approval
val approval : marker:string -> action:action -> session_id:string ->
  inventory_id:string -> target_id:string -> approval

type target =
  | Android_avd of { name : string; port : int }
  | Ios_simulator of { id : string }
val target_id : target -> string
val target_serial : target -> string

val parse_avd_name : string -> string
val avd_name_command : string -> string
val configured_simulator_ids : string -> string list


(** The inventory ID is a parent-owned binding for one selected mobile session
    and its approved configured/compatible-device inventory. A changed ID
    intentionally invalidates pending or managed lifecycle state. *)
type inventory = {
  session_id : string;
  inventory_id : string;
  configured_avds : string list;
  android_devices : Workspace_android_devices.device list;
  avd_bindings : (string * string) list;
  android_bindings_complete : bool;
  configured_simulator_ids : string list;
  ios_destinations : string list;
  compatible_simulators : Workspace_xcode.simulator list;
}
val create_inventory : session_id:string -> inventory_id:string ->
  configured_avds:string list ->
  android_devices:Workspace_android_devices.device list ->
  avd_bindings:(string * string) list -> android_bindings_complete:bool ->
  configured_simulator_ids:string list -> ios_destinations:string list ->
  compatible_simulators:Workspace_xcode.simulator list -> inventory
val inventory_session_id : inventory -> string
val inventory_id : inventory -> string

type ownership = Preexisting | Owned of {
  owner_session_id : string;
  ownership_id : string;
  launcher_id : string;
}
type managed
val ownership : managed -> ownership
val managed_target : managed -> target

type pending
type booting
val launcher_identity : booting -> string

type boot_plan = Already_booted of managed | Start of {
  command : string;
  pending : pending;
}
val boot_command : inventory:inventory -> approval:approval -> target:target ->
  ownership_id:string -> boot_plan
(* Call [Started] only after the owner has created a managed launcher/process
   record. It is not a device-readiness assertion. The owner must retain and
   reap [launcher_identity] on cancellation. *)
val settle_launch : pending ->
  [ `Failed | `Cancelled | `Started of string ] -> booting option
val readiness_command : booting:booting -> approval:approval -> string

(* Readiness parsers only accept bounded successful command output. A caller
   must not invoke them for failed, cancelled, or truncated commands. *)

type readiness = Waiting | Ready of managed
val android_readiness : booting:booting -> inventory:inventory ->
  approval:approval -> avd_name_output:string -> devices_output:string ->
  boot_status_output:string -> readiness
val ios_readiness : booting:booting -> inventory:inventory ->
  approval:approval -> devices_json:string -> readiness

val shutdown_command : inventory:inventory -> approval:approval ->
  managed:managed -> string option
val complete_shutdown : managed ->
  [ `Succeeded | `Failed | `Cancelled ] -> managed option
val abort_command : inventory:inventory -> approval:approval ->
  booting:booting -> string option
val complete_abort : booting ->
  [ `Succeeded | `Failed | `Cancelled ] -> booting option
