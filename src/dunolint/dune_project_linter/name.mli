(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t

val create : name:Dune_project.Name.t -> t

include
  Dunolinter.Stanza_linter.S
  with type t := t
   and type predicate = Dune_project.Name.Predicate.t

module Linter :
  Dunolinter.Linter.S with type t = t and type predicate = Dune_project.Predicate.t

(** {1 Getters} *)

val name : t -> Dune_project.Name.t

(** {1 Setters} *)

val set_name : t -> name:Dune_project.Name.t -> unit
