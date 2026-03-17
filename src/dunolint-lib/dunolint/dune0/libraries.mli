(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** Predicate for the ["libraries"] field of library/executable stanzas.

    The predicates are syntactic - they refer to what is written in the dune
    file, literally. *)

module Predicate : sig
  (** Predicates to check library dependencies.

      Example sexp syntax:
      {v
        (libraries (mem ordering yojson))
      v} *)

  type t = [ `mem of Library__name.t list ]

  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end
