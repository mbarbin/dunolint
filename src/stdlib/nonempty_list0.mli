(*_**************************************************************************************)
(*_  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*_  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(*_**************************************************************************************)

type 'a t = 'a Dunolint.Nonempty_list.t = ( :: ) of 'a * 'a list

val to_list : 'a t -> 'a list
val iter : 'a t -> f:('a -> unit) -> unit
val for_all : 'a t -> f:('a -> bool) -> bool
