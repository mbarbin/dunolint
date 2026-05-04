(*_**************************************************************************************)
(*_  Dunolint_stdlib - Extending OCaml's Stdlib for Dunolint                            *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>        *)
(*_  SPDX-License-Identifier: MIT OR LGPL-3.0-or-later WITH LGPL-3.0-linking-exception  *)
(*_**************************************************************************************)

include module type of struct
  include StringLabels
end

val sexp_of_t : t -> Sexplib0.Sexp.t
val t_of_sexp : Sexplib0.Sexp.t -> t
val chop_prefix : t -> prefix:string -> t option
val chop_suffix : t -> suffix:string -> t option
val is_empty : t -> bool
val is_prefix : t -> prefix:string -> bool
val is_suffix : t -> suffix:string -> bool
val lsplit2 : t -> on:char -> (t * t) option
val lstrip : ?drop:(char -> bool) -> t -> t
val rstrip : ?drop:(char -> bool) -> t -> t
val of_char_list : char list -> t
val split : t -> on:char -> t list
val strip : ?drop:(char -> bool) -> t -> t
val to_string : t -> t
