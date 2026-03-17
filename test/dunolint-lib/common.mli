(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module type Roundtripable = sig
  type t [@@deriving equal, sexp]
end

val test_roundtrip : (module Roundtripable with type t = 'a) -> 'a -> unit

module type Predicate = sig
  type t [@@deriving equal, sexp]
end

val test_predicate : (module Predicate with type t = 'a) -> 'a Blang.t -> unit
