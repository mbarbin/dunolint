type t = Dunolint.Dune.Compilation_mode.t

include module type of Dunolint.Dune.Compilation_mode with type t := t
include Comparable.S with type t := t
