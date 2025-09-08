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

(** The format of the syntax used in a project to express dune stanzas.

    This is specified in the [dune-project] file, as the first stanza. For
    example:

    {v
      (lang dune 3.20)
    v} *)

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

  type t =
    [ `equals of version
    | `greater_than_or_equal_to of version
    | `less_than_or_equal_to of version
    ]

  val equal : t -> t -> bool
  val compare : t -> t -> int

  include Sexpable.S with type t := t
end
