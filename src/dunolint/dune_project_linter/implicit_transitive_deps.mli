(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module Value = Dune_project.Implicit_transitive_deps.Value

type t

val create : implicit_transitive_deps:Value.t -> t

include
  Dunolinter.Stanza_linter.S
  with type t := t
   and type predicate = Dune_project.Implicit_transitive_deps.Predicate.t

module Linter :
  Dunolinter.Linter.S with type t = t and type predicate = Dune_project.Predicate.t

(** {1 Getters} *)

val value : t -> Value.t

(** {1 Setters} *)

val set_value : t -> value:Value.t -> unit
