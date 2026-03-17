(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module Backend = struct
  module Name = struct
    module T = Dunolint.Dune.Instrumentation.Backend.Name
    include T
    include Comparable.Make (T)
  end

  module Flag = Dunolint.Dune.Instrumentation.Backend.Flag

  include (
    Dunolint.Dune.Instrumentation.Backend :
      module type of Dunolint.Dune.Instrumentation.Backend
      with module Name := Name
      with module Flag := Flag)
end

module Predicate = Dunolint.Dune.Instrumentation.Predicate
