type t = Dunolint.Dune.Library.Name.t

include module type of Dunolint.Dune.Library.Name with type t := t
include Comparable.S with type t := t
