(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

type t

val equal : t -> t -> bool
val compare : t -> t -> int

include Sexpable.S with type t := t

(** Tests if string matches the glob. *)
val test : t -> string -> bool

(** Returns textual representation of a glob. *)
val to_string : t -> string

(** Converts string to glob. Throws [Invalid_argument] exception if string is
    not a valid glob. *)
val v : string -> t
