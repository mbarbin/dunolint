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

type t =
  | True
  | False
  | Undefined
[@@deriving equal, compare, enumerate, sexp_of]

val const : bool -> t
val eval : 'a Blang.t -> f:('a -> t) -> t

(** If there exists a [True] value, that is the returned value for the
    list. Otherwise, this requires all values to be [False] to return
    false, and is [Undefined] in the remaining cases. This returns
    [false] on an empty list. *)
val disjunction : t list -> t

(** Same as [disjunction] but applies a [f] function to the elements of the
    input list as the values are needed. If there exists a [True] value, that is
    the returned value for the list. Otherwise, this requires all values to be
    [False] to return false, and is [Undefined] in the remaining cases.

    The function [f] is called on the elements of the input list from left to
    right. If the result of the evaluation is determined before visiting the end
    of the input, [f] won't be called on the remaining values. Such case happens
    when [f] returns [True] for one of the elements. *)
val exists : 'a list -> f:('a -> t) -> t

(** If there exists a [False] value, that is the returned value for
    the list. Otherwise, this requires all values to be [True] to
    return [true], and is [Undefined] in the remaining cases. This
    returns [true] on an empty list. *)
val conjunction : t list -> t

(** Same as [conjunction] but builds the input by applying a [f] function on a
    list of input.

    The function [f] is called on the elements of the input list from left to
    right. If the result of the evaluation is determined before visiting the end
    of the input, [f] won't be called on the remaining values. Such case happens
    when [f] returns [False] for one of the elements. *)
val for_all : 'a list -> f:('a -> t) -> t
