(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module Ordered_set : sig
  type t = Dune.Compilation_mode.t Dunolinter.Ordered_set.t [@@deriving sexp_of]
end

type t

val create : modes:Ordered_set.t -> t

include
  Dunolinter.Stanza_linter.S
  with type t := t
   and type predicate := Dune.Library.Modes.Predicate.t

(** {1 Getters} *)

val modes : t -> Ordered_set.t

(** {1 Setters} *)

val set_modes : t -> modes:Ordered_set.t -> unit
