module Backend = struct
  module Name = struct
    module T = Dunolint.Dune.Instrumentation.Backend.Name
    include T
    include Comparable.Make (T)
  end
end

module Predicate = Dunolint.Dune.Instrumentation.Predicate
