(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** The format of the syntax used in a project to express dune stanzas.

    This is specified in the [dune-project] file, as the first stanza. For
    example:

    {v   (lang dune 3.20) v} *)

(** Representing the two integers that are separated by the dot. For example,
    the representation of ["3.20"] is [(3, 20)]. *)
type t

val equal : t -> t -> bool
val compare : t -> t -> int

include Sexpable.S with type t := t

val create : int * int -> t

(** Returns the string that can be used as atom in the dune-project
    configuration file. *)
val to_string : t -> string

module Predicate : sig
  type version := t

  (** These longer names are deprecated and will be removed by a future upgrade.
      Do not use in new code and migrate at your earliest convenience. *)
  type deprecated_names =
    [ `equals of version
    | `greater_than_or_equal_to of version
    | `less_than_or_equal_to of version
    ]

  type t =
    [ `eq of version
    | `gt of version
    | `gte of version
    | `lt of version
    | `lte of version
    | `neq of version
    | deprecated_names
    ]

  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end
