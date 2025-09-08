type t = Dunolint.Dune.Library.Public_name.t

include module type of Dunolint.Dune.Library.Public_name with type t := t
include Comparable.S with type t := t
