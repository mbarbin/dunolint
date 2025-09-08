type t = Dunolint.Dune.Pp.Name.t

include module type of Dunolint.Dune.Pp.Name with type t := t
include Comparable.S with type t := t
