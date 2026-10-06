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
      {v   (libraries (present ordering yojson)) v} *)

  (** These names are deprecated and will be removed by a future upgrade. Do not
      use in new code and migrate at your earliest convenience. Use [`present]
      instead, or [`absent] instead of the negation of [`mem]. *)
  type deprecated_names = [ `mem of Library__name.t list ]

  type t =
    [ `present of Library__name.t Nonempty_list.t
    | `absent of Library__name.t Nonempty_list.t
    | deprecated_names
    ]

  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end
