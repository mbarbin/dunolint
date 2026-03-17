(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

include Stdlib.Int

let zero = 0
let ( + ) = Stdlib.( + )
let max_value = max_int
let of_string = int_of_string
let to_dyn = Dyn0.int
let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_int
let t_of_sexp = Sexplib0.Sexp_conv.int_of_sexp
