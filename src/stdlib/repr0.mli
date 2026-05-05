(*_**************************************************************************************)
(*_  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*_  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(*_**************************************************************************************)

(** This is to mirror OCaml [Repr] module from the stdlib. That module is available
    from OCaml [5.4] but at this time our lower bound is below that. Once we
    pass [5.4] we'll change the code and start including [Repr] directly. *)

external phys_equal : 'a -> 'a -> bool = "%eq"
external equal : 'a -> 'a -> bool = "%equal"
external compare : 'a -> 'a -> int = "%compare"
