(** Local plugin manifests are data-only capability references. No package loading,
    installation, execution, or network access occurs here. [user_dir] must be an
    existing, private, user-owned configuration directory. Manifests live in its
    private [plugins/] child; the registry creates that child if absent.

    Example [plugins/review-pack.json] (private, owner-only ordinary file):
    {"schemaVersion":1,"name":"review-pack","version":"1.0.0",
     "skills":["review-code"],"commands":["review-code"],"tools":["local_review"]}

    Names in those arrays refer only to capabilities already supplied by the caller.
    The caller must separately reject built-in collisions when loading capabilities.
    No project/workspace paths or remote plugin sources are accepted. *)

type capabilities = {
  skills : string list;
  commands : string list;
  tools : string list;
}

type diagnostic = { path : string; code : string; message : string }
type plugin = {
  name : string;
  version : string;
  source : string;
  digest : string;  (** SHA-256 of the manifest bytes, lowercase hex. *)
  enabled : bool;
  references : capabilities;
  active : capabilities;
  installed_from : string;
    (** Manifest `installedFrom` provenance label; `"local"` when absent. *)
  is_builtin : bool;
    (** Reserved for curated registry plugins; always false for directory manifests. *)
  discovery_category : string;
    (** Manifest `category` label; `"installed"` when absent. *)
}
type snapshot = { plugins : plugin list; active : capabilities; diagnostics : diagnostic list }
type registry

(** [builtins] includes all reserved names, including temporarily unavailable
    built-ins. Duplicate names across manifests are rejected for both plugins.
    Invalid manifests are diagnosed and never activated. A malformed/unsafe state
    file or unsafe registry directory fails closed. *)
val load : user_dir:string -> available:capabilities -> builtins:capabilities ->
  (registry, string) result
val snapshot : registry -> snapshot
(* Changes take effect immediately in the returned snapshot; caller must query
    [snapshot] on every turn, rather than cache its previous [active] field.
    State is persisted atomically before the in-memory snapshot changes. *)
val enable : registry -> string -> (snapshot, string) result
val disable : registry -> string -> (snapshot, string) result
(* Re-scan manifests and persisted enabled state, replacing the old snapshot
    only after a complete scan. This never adds capabilities from invalid files. *)
val reload : registry -> (snapshot, string) result
