(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t =
  | True
  | False
  | Undefined

val equal : t -> t -> bool
val compare : t -> t -> int
val sexp_of_t : t -> Sexp.t
val all : t list
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

module Private : sig
  val and_ : t -> t -> t
  val or_ : t -> t -> t
end
