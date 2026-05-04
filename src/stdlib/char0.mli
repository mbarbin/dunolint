(*_**************************************************************************************)
(*_  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*_  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(*_**************************************************************************************)

include module type of Stdlib.Char

val is_alphanum : char -> bool
val is_whitespace : char -> bool
val is_uppercase : char -> bool
val to_string : t -> string
val lowercase : t -> t
val uppercase : t -> t
