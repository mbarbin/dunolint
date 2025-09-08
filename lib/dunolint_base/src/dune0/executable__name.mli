type t = Dunolint.Dune.Executable.Name.t

include module type of Dunolint.Dune.Executable.Name with type t := t
include Comparable.S with type t := t
