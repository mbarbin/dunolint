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

(** Configuration for the ["modes"] field of library.

    Some examples as found in dune files:

    {v
      (modes byte)
      (modes byte native)
      (modes best)
      (modes :standard melange)
    v} *)

module Predicate : sig
  (** A very crucial design points here is that the predicates are syntactic.
      They do not talk about the evaluation of the ordered set, but refer to
      what is written in the dune file, literally.

      So, for example even if the evaluation of the [:standard] mode includes
      [byte], evaluating: [`has_mode `byte] on the input [(:standard)] returns
      [false].

      The reason is that dunolint focuses on linting what the user writes in the
      dune files, as opposed to how dune interprets it. *)

  type t =
    [ `has_mode of Compilation_mode.t
    | `has_modes of Compilation_mode.t list
    ]

  val equal : t -> t -> bool
  val compare : t -> t -> int

  include Sexpable.S with type t := t
end
