(*_**************************************************************************************)
(*_  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*_  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(*_**************************************************************************************)

include module type of struct
  include Stdlib.ListLabels
end

val bind : 'a t -> f:('a -> 'b t) -> 'b t
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
val count : 'a t -> f:('a -> bool) -> int
val dedup_and_sort : 'a list -> compare:('a -> 'a -> int) -> 'a list
val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val exists : 'a t -> f:('a -> bool) -> bool
val filter : 'a t -> f:('a -> bool) -> 'a t
val filter_opt : 'a option list -> 'a list
val find : 'a list -> f:('a -> bool) -> 'a option
val find_map : 'a list -> f:('a -> 'b option) -> 'b option
val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t
val fold_mapi : 'a t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc * 'b) -> 'acc * 'b t
val for_all : 'a t -> f:('a -> bool) -> bool
val group : 'a t -> break:('a -> 'a -> bool) -> 'a t t
val iter : 'a t -> f:('a -> unit) -> unit
val iter2 : 'a list -> 'b list -> f:('a -> 'b -> unit) -> unit
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
val last : 'a list -> 'a option
val last_exn : 'a list -> 'a
val map : 'a t -> f:('a -> 'b) -> 'b t
val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option
val mem : 'a list -> 'a -> equal:('a -> 'a -> bool) -> bool
val rev_filter_map : 'a t -> f:('a -> 'b option) -> 'b t
val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
val take : 'a t -> int -> 'a t

module type Summable = sig
  type t

  val zero : t
  val ( + ) : t -> t -> t
end

val sum : (module Summable with type t = 'sum) -> 'a t -> f:('a -> 'sum) -> 'sum
