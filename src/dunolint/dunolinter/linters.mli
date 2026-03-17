(*_********************************************************************************)
(*_  Dunolint - A tool to lint and help manage files in dune projects             *)
(*_  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*_********************************************************************************)

(** A data structure to hold linters with a binary search lookup based
    on field name. *)

type 'a t

val create : 'a list -> field_name:('a -> string) -> 'a t
val lookup : 'a t -> field_name:string -> 'a option
