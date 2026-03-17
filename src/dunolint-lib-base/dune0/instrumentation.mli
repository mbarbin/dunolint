(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module Backend : sig
  module Name : sig
    type t = Dunolint.Dune.Instrumentation.Backend.Name.t

    include module type of Dunolint.Dune.Instrumentation.Backend.Name with type t := t
    include Comparable.S with type t := t
  end

  module Flag : sig
    type t = Dunolint.Dune.Instrumentation.Backend.Flag.t

    include module type of Dunolint.Dune.Instrumentation.Backend.Flag with type t := t
  end

  type t = Dunolint.Dune.Instrumentation.Backend.t

  include
    module type of Dunolint.Dune.Instrumentation.Backend
    with type t := t
    with module Name := Name
    with module Flag := Flag
end

module Predicate = Dunolint.Dune.Instrumentation.Predicate
