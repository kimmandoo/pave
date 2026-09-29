exception Error of string
exception Cancelled

type approval = Start of Mcp_config.server
              | Effect of Mcp_config.server * string * Yojson.Basic.t
type client
type session
type sourced = {
  server : string;
  source : Mcp_config.source;
  value : Yojson.Basic.t;
}
type request = method_name:string -> params:Yojson.Basic.t ->
  timeout_seconds:float -> cancelled:(unit -> bool) -> Yojson.Basic.t

val create : snapshot:Mcp_config.snapshot -> authorize:(approval -> bool) -> session
val dispose : session -> unit
val connect : session -> server:string -> timeout_seconds:float ->
  cancelled:(unit -> bool) -> client
val list_tools : session -> server:string -> timeout_seconds:float ->
  cancelled:(unit -> bool) -> sourced list
val list_resources : session -> server:string -> timeout_seconds:float ->
  cancelled:(unit -> bool) -> sourced list
val list_prompts : session -> server:string -> timeout_seconds:float ->
  cancelled:(unit -> bool) -> sourced list
val call_tool : session -> server:string -> name:string ->
  arguments:Yojson.Basic.t -> timeout_seconds:float ->
  cancelled:(unit -> bool) -> sourced
val read_resource : session -> server:string -> uri:string ->
  timeout_seconds:float -> cancelled:(unit -> bool) -> sourced
val get_prompt : session -> server:string -> name:string ->
  arguments:Yojson.Basic.t -> timeout_seconds:float ->
  cancelled:(unit -> bool) -> sourced

(* A separate transport owns its own framing, IDs, origin and session state.
   These functions validate its result values without trusting server text. *)
val list_tools_with : source:Mcp_config.server -> request:request ->
  timeout_seconds:float -> cancelled:(unit -> bool) -> sourced list
val list_resources_with : source:Mcp_config.server -> request:request ->
  timeout_seconds:float -> cancelled:(unit -> bool) -> sourced list
val list_prompts_with : source:Mcp_config.server -> request:request ->
  timeout_seconds:float -> cancelled:(unit -> bool) -> sourced list
val validate_tool : Yojson.Basic.t -> unit
val validate_resource : Yojson.Basic.t -> unit
val validate_prompt : Yojson.Basic.t -> unit
val validate_arguments : schema:Yojson.Basic.t -> Yojson.Basic.t -> unit
val validate_tool_result : Yojson.Basic.t -> unit
val validate_resource_result : uri:string -> Yojson.Basic.t -> unit
val validate_prompt_result : Yojson.Basic.t -> unit
