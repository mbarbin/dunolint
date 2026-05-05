(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

module type Roundtripable = sig
  type t

  val equal : t -> t -> bool
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

val test_roundtrip : (module Roundtripable with type t = 'a) -> 'a -> unit

module type Predicate = sig
  type t

  val equal : t -> t -> bool
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

val test_predicate : (module Predicate with type t = 'a) -> 'a Blang.t -> unit
