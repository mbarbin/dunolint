(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** The format of the dune syntax used in a dune-workspace file.

    This is specified as the first stanza. For example:

    {v
      (lang dune 3.17)
    v} *)

(** Representing the two integers that are separated by the dot. For example,
    the representation of ["3.17"] is [(3, 17)]. *)
type t

val equal : t -> t -> bool
val compare : t -> t -> int

include Sexpable.S with type t := t

val create : int * int -> t

(** Returns the string that can be used as atom in the dune-workspace file. *)
val to_string : t -> string

module Predicate : sig
  type version := t

  type t =
    [ `eq of version
    | `gt of version
    | `gte of version
    | `lt of version
    | `lte of version
    | `neq of version
    ]

  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end
