(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module Blang = Blang
module Condition = Condition
module Config = Config
module Dune = Dune
module Dune_project = Dune_project
module Dune_workspace = Dune_workspace
module Dunolint0 = Dunolint0
module Glob = Glob
module Linted_file_kind = Linted_file_kind
module Path = Path
module Predicate = Predicate
module Rule = Rule
module Trilang = Trilang

module Std : sig
  (** [Std] is meant to be open to access common modules from the root path. *)

  module Blang = Blang
  module Dune = Dune
  module Dune_project = Dune_project
  module Dune_workspace = Dune_workspace
  module Dunolint0 = Dunolint0
end

module Private : sig
  module Sexp_helpers = Sexp_helpers
end
