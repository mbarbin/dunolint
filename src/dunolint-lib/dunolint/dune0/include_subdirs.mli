(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module Mode : sig
  type t =
    [ `no
    | `unqualified
    | `qualified
    ]

  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end

module Predicate : sig
  type t = [ `equals of Mode.t ]

  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end
