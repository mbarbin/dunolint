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

type t

val equal : t -> t -> bool
val compare : t -> t -> int
val sexp_of_t : t -> Sexp.t
val of_stanzas : Sexp.t list -> t
val to_stanzas : t -> Sexp.t list

(** {1 Getters} *)

module Rule : sig
  type t = (Predicate.t, Condition.t) Rule.t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  include Sexpable.S with type t := t
end

val skip_paths : t -> Glob.t list list
val rules : t -> Rule.t list

(** {1 Creating configs} *)

val create : [ `skip_paths of Glob.t list | `rule of Rule.t ] list -> t

module Std = Edsl_std
