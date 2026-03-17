(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t

val create : mode:Dune.Include_subdirs.Mode.t -> t

include
  Dunolinter.Stanza_linter.S
  with type t := t
   and type predicate = Dune.Include_subdirs.Predicate.t

module Linter : Dunolinter.Linter.S with type t = t and type predicate = Dune.Predicate.t

(** {1 Getters} *)

val mode : t -> Dune.Include_subdirs.Mode.t

(** {1 Setters} *)

val set_mode : t -> mode:Dune.Include_subdirs.Mode.t -> unit
