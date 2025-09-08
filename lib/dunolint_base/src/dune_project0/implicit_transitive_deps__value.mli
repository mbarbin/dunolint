type t = Dunolint.Dune_project.Implicit_transitive_deps.Value.t

include
  module type of Dunolint.Dune_project.Implicit_transitive_deps.Value with type t := t

include Comparable.S with type t := t
