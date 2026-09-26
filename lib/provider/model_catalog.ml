type modality = Text | Image | Audio | Video | Embedding | Reranking

type source_kind =
  | Pinned_account_listing
  | Provider_listing
  | Capability_response
  | Explicit_user_input

type provenance = {
  id_source : source_kind;
  capability_source : source_kind option;
  endpoint : string option;
  retrieved_at : float option;
}

type capabilities = {
  input_modalities : modality list option;
  output_modalities : modality list option;
  tools : bool option;
  context_window_tokens : int option;
  max_output_tokens : int option;
  effort_levels : string list option;
  supported_endpoints : string list option;
  provider_tokenizer : string option;
  native_compaction_supported : bool option;
}

type model = {
  identity : Model_identity.t;
  display_name : string option;
  capabilities : capabilities;
  provenance : provenance;
}

let empty_capabilities = {
  input_modalities = None;
  output_modalities = None;
  tools = None;
  context_window_tokens = None;
  max_output_tokens = None;
  effort_levels = None;
  supported_endpoints = None;
  provider_tokenizer = None;
  native_compaction_supported = None;
}

let has_reported_capabilities capabilities =
  Option.is_some capabilities.input_modalities ||
  Option.is_some capabilities.output_modalities ||
  Option.is_some capabilities.tools ||
  Option.is_some capabilities.context_window_tokens ||
  Option.is_some capabilities.max_output_tokens ||
  Option.is_some capabilities.effort_levels ||
  Option.is_some capabilities.supported_endpoints ||
  Option.is_some capabilities.provider_tokenizer ||
  Option.is_some capabilities.native_compaction_supported
