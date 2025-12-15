(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*_                                                                               *)
(*_  This file is part of Dunolint.                                               *)
(*_                                                                               *)
(*_  Dunolint is free software; you can redistribute it and/or modify it          *)
(*_  under the terms of the GNU Lesser General Public License as published by     *)
(*_  the Free Software Foundation either version 3 of the License, or any later   *)
(*_  version, with the LGPL-3.0 Linking Exception.                                *)
(*_                                                                               *)
(*_  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*_  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*_  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*_  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*_                                                                               *)
(*_  You should have received a copy of the GNU Lesser General Public License     *)
(*_  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*_  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*_********************************************************************************)

(** An extension of the Dunolint library for use with Base.

    [Dunolint_base.Dunolint] is a module that extends the [Dunolint] module with
    additional modules and functionalities, aimed to improve the compatibility
    of [Dunolint] for programs using [Base].

    For example, it adds [Comparable.S] to all container keys modules so that
    they can be used with Base-style containers.

    The library is designed to be used as a drop-in replacement for [Dunolint].
    For this, it includes a single module named [Dunolint] which must be setup to
    shadow the regular [Dunolint] module.

    You may do so by defining the following module alias in a place that's
    available to your scope:

    {[
      module Dunolint = Dunolint_base.Dunolint
    ]}

    Another way to achieve this is to open [Dunolint_base] via dune flags.
    When doing that, all the files in your library will use
    [Dunolint_base.Dunolint] consistently.

    {v
      (library
        (name my_library)
        (flags (:standard -open Dunolint_base))
        (libraries dunolint-lib-base))
    v} *)

module Dunolint : sig
  (** {1 Extended Dunolint API} *)

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

  module Std : sig
    module Blang = Dunolint.Blang
    module Dune = Dune
    module Dune_project = Dune_project
  end
end
