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

module Name = Executable__name
module Public_name = Executable__public_name

type t

val create
  :  ?name:Dune.Executable.Name.t
  -> ?public_name:Dune.Executable.Public_name.t
  -> ?flags:Sexp.t list
  -> ?libraries:Dune.Library.Name.t list
  -> ?instrumentation:Instrumentation.t
  -> ?lint:Lint.t
  -> ?preprocess:Preprocess.t
  -> unit
  -> t

include
  Dunolinter.Stanza_linter.S
  with type t := t
   and type predicate = Dune.Executable.Predicate.t

module Linter : Dunolinter.Linter.S with type t = t and type predicate = Dune.Predicate.t

module Private : sig
  (** At this time we export [rewrite] with an altered type, to add the
      extra flag [load_existing_libraries], which defaults to [false].
      Using [true] causes existing libraries to be read and added to the
      in-memory value prior to rewriting. This is used internally by a
      private tool but shall disappear after some refactoring has
      happened. *)
  val rewrite
    :  ?load_existing_libraries:bool
    -> t
    -> sexps_rewriter:Sexps_rewriter.t
    -> field:Sexp.t
    -> unit
end
