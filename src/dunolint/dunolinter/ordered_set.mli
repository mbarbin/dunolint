(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type 'a t =
  | Element of 'a
  | Standard
  | Union of 'a t list
  | Diff of 'a t * 'a t
  | Include of string
[@@deriving sexp_of]

val read
  :  read_element:(sexps_rewriter:Sexps_rewriter.t -> Sexp.t -> 'a)
  -> sexps_rewriter:Sexps_rewriter.t
  -> Sexp.t list
  -> 'a t

val write : write_a:('a -> Sexp.t) -> 'a t -> Sexp.t list

module Evaluation_result : sig
  type 'a t =
    | Known of 'a
    | Unknown
  [@@deriving sexp_of]
end

module Evaluator : sig
  type 'a t =
    { standard : unit -> 'a list Evaluation_result.t
    ; include_ : string -> 'a list Evaluation_result.t
    }

  (** A static evaluator, meaning it evaluates [:standard] and file inclusion
      to [Unknown]. *)
  val static : _ t
end

val of_set : ('a, _) Set.t -> 'a t
val of_list : 'a list -> 'a t
val empty : 'a t

(** Return the elements of [t] as a set, using the specified static evaluator. *)
val as_set
  :  ('a, 'cmp) Comparator.Module.t
  -> 'a t
  -> evaluator:'a Evaluator.t
  -> ('a, 'cmp) Set.t Evaluation_result.t

(** Whether it is possible to determine statically if a value belongs to the
    set. This is meant to cover more cases in which, even though it is not
    possible to know the set completely statically, it is still possible to
    determine whether an element belongs to it. *)
val mem
  :  (module Comparator.S with type t = 'a)
  -> 'a t
  -> 'a
  -> evaluator:'a Evaluator.t
  -> bool Evaluation_result.t

val insert : (module Comparator.S with type t = 'a) -> 'a t -> 'a -> 'a t
val remove : (module Comparator.S with type t = 'a) -> 'a t -> 'a -> 'a t

(** A canonical sort defined by dunolint when the order has no particular
    meaning. *)
val canonical_sort : (module Comparator.S with type t = 'a) -> 'a t -> 'a t
