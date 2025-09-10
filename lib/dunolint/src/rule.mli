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

type ('predicate, 'invariant) t =
  [ `enforce of 'invariant
  | `return
    (** [return] stops the evaluation of the rule without trying to enforce any
        invariant. *)
  | `skip_subtree
    (** This causes the linter to finish the linting of the current rule,
        however any remaining rule will be skipped, and the entire subtree will
        not be linted. *)
  | `cond of ('predicate Blang.t * ('predicate, 'invariant) t) list
  ]

val compare : ('p -> 'p -> int) -> ('i -> 'i -> int) -> ('p, 'i) t -> ('p, 'i) t -> int
val equal : ('p -> 'p -> bool) -> ('i -> 'i -> bool) -> ('p, 'i) t -> ('p, 'i) t -> bool

val eval
  :  ('predicate, 'invariant) t
  -> f:('predicate -> Trilang.t)
  -> [ `enforce of 'invariant | `return | `skip_subtree ]

module Stable : sig
  module V1 : sig
    type nonrec ('a, 'b) t = ('a, 'b) t

    val compare
      :  ('p -> 'p -> int)
      -> ('i -> 'i -> int)
      -> ('p, 'i) t
      -> ('p, 'i) t
      -> int

    val equal
      :  ('p -> 'p -> bool)
      -> ('i -> 'i -> bool)
      -> ('p, 'i) t
      -> ('p, 'i) t
      -> bool

    include Sexpable.S2 with type ('p, 'i) t := ('p, 'i) t
  end

  module V0 : sig
    type nonrec ('a, 'b) t = ('a, 'b) t

    val compare
      :  ('p -> 'p -> int)
      -> ('i -> 'i -> int)
      -> ('p, 'i) t
      -> ('p, 'i) t
      -> int

    val equal
      :  ('p -> 'p -> bool)
      -> ('i -> 'i -> bool)
      -> ('p, 'i) t
      -> ('p, 'i) t
      -> bool

    include Sexpable.S2 with type ('p, 'i) t := ('p, 'i) t
  end
end
