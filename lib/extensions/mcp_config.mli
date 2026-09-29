exception Error of string

type source = User | Project
type transport =
  | Stdio of { program : string; arguments : string list;
               environment : (string * string) list }
  | Http of { endpoint : string; bearer_secret_ref : string option;
              allow_loopback_http : bool }
type server = {
  name : string; source : source; root : string; owner : string;
  transport : transport;
}
type snapshot = { root : string; owner : string; servers : server list }

val load : root:string -> owner:string -> snapshot
val find : snapshot -> string -> server option
val names : snapshot -> string list
val resolve_secret : string -> string
val resolve_environment : server -> (string * string) list
val program_path : string -> string
val identifier : string -> string -> string
