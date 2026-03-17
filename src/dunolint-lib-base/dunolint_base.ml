(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module Dunolint = struct
  module Condition = Dunolint.Condition
  module Config = Dunolint.Config
  module Dune = Dune
  module Dune_project = Dune_project
  module Glob = Dunolint.Glob
  module Linted_file_kind = Linted_file_kind
  module Path = Dunolint.Path
  module Predicate = Dunolint.Predicate
  module Rule = Dunolint.Rule
  module Trilang = Dunolint.Trilang

  module Std = struct
    module Blang = Dunolint.Blang
    module Dune = Dune
    module Dune_project = Dune_project
  end
end
