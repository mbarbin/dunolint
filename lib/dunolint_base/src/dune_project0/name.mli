type t = Dunolint.Dune_project.Name.t

include module type of Dunolint.Dune_project.Name with type t := t
include Comparable.S with type t := t
