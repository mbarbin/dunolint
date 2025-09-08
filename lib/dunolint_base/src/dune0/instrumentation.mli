module Backend : sig
  module Name : sig
    type t = Dunolint.Dune.Instrumentation.Backend.Name.t

    include module type of Dunolint.Dune.Instrumentation.Backend.Name with type t := t
    include Comparable.S with type t := t
  end
end

module Predicate = Dunolint.Dune.Instrumentation.Predicate
