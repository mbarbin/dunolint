type t = Dunolint.Dune.Package.Name.t

include module type of Dunolint.Dune.Package.Name with type t := t
include Comparable.S with type t := t
