(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** Package field for library stanzas. *)

module Predicate : sig
  type t =
    [ `equals of Package.Name.t
    | `is_prefix of string
    | `is_suffix of string
    ]

  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end
