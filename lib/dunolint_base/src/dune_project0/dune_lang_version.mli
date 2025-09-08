type t = Dunolint.Dune_project.Dune_lang_version.t

include module type of Dunolint.Dune_project.Dune_lang_version with type t := t
include Comparable.S with type t := t
