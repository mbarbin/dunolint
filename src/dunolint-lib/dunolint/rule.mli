(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type ('predicate, 'invariant) t =
  [ `enforce of 'invariant
  | `return
    (** [return] stops the evaluation of the rule without trying to enforce any
        invariant. *)
  | `cond of ('predicate Blang.t * ('predicate, 'invariant) t) list
  ]

val equal : ('p -> 'p -> bool) -> ('i -> 'i -> bool) -> ('p, 'i) t -> ('p, 'i) t -> bool

val eval
  :  ('predicate, 'invariant) t
  -> f:('predicate -> Trilang.t)
  -> [ `enforce of 'invariant | `return ]

module Stable : sig
  module V1 : sig
    type nonrec ('a, 'b) t = ('a, 'b) t

    val equal
      :  ('p -> 'p -> bool)
      -> ('i -> 'i -> bool)
      -> ('p, 'i) t
      -> ('p, 'i) t
      -> bool

    include Sexpable.S2 with type ('p, 'i) t := ('p, 'i) t
  end
end
