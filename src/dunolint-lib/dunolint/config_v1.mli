(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t

val equal : t -> t -> bool
val sexp_of_t : t -> Sexp.t
val of_stanzas : Sexp.t list -> t
val to_stanzas : t -> Sexp.t list

(** {1 Getters} *)

module Rule : sig
  type t = (Predicate.t, Condition.t) Rule.t

  val equal : t -> t -> bool

  include Sexpable.S with type t := t
end

val skip_paths : t -> Glob.t list list
val rules : t -> Rule.t list

(** {1 Creating configs} *)

val create : [ `skip_paths of Glob.t list | `rule of Rule.t ] list -> t

module Std = Edsl_std
