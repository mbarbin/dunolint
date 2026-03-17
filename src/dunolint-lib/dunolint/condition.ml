(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

type t = Predicate.t Blang.t

let equal (a : t) (b : t) = Blang.equal Predicate.equal a b
let t_of_sexp sexp = Blang.t_of_sexp Predicate.t_of_sexp sexp
let sexp_of_t t = Blang.sexp_of_t Predicate.sexp_of_t t
