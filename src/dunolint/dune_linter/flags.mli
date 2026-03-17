(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** The "flags" to be passed to OCaml. *)

type t

val create : flags:Sexp.t list -> t

(** At the moment there is not predicate nor enforceable conditions on flags.
    TBD. *)
include Dunolinter.Stanza_linter.S with type t := t and type predicate := Nothing.t

(** {1 Getters} *)

val is_empty : t -> bool
val flags : t -> Sexp.t list

(** {1 Setters} *)

val set_flags : t -> flags:Sexp.t list -> unit
