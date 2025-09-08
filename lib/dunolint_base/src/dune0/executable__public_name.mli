type t = Dunolint.Dune.Executable.Public_name.t

include module type of Dunolint.Dune.Executable.Public_name with type t := t
include Comparable.S with type t := t
