(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  Copyright (C) 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>            *)
(*                                                                               *)
(*  This file is part of Dunolint.                                               *)
(*                                                                               *)
(*  Dunolint is free software; you can redistribute it and/or modify it          *)
(*  under the terms of the GNU Lesser General Public License as published by     *)
(*  the Free Software Foundation either version 3 of the License, or any later   *)
(*  version, with the LGPL-3.0 Linking Exception.                                *)
(*                                                                               *)
(*  Dunolint is distributed in the hope that it will be useful, but WITHOUT      *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or        *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License  *)
(*  and the file `NOTICE.md` at the root of this repository for more details.    *)
(*                                                                               *)
(*  You should have received a copy of the GNU Lesser General Public License     *)
(*  and the LGPL-3.0 Linking Exception along with this library. If not, see      *)
(*  <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.         *)
(*********************************************************************************)

let error_source = "predicate.t"

type t = [ `dunolint_lang_version of Dunolint_lang_version.Predicate.t Blang.t ]

let equal (a : t) (b : t) =
  if Stdlib.( == ) a b
  then true
  else (
    match a, b with
    | `dunolint_lang_version va, `dunolint_lang_version vb ->
      Blang.equal Dunolint_lang_version.Predicate.equal va vb)
;;

let predicates : t Sexp_helpers.Predicate_spec.t =
  [ { atom = "dunolint_lang_version"
    ; conv =
        (fun sexp ->
          `dunolint_lang_version
            (Blang.t_of_sexp Dunolint_lang_version.Predicate.t_of_sexp sexp))
    }
  ]
;;

let t_of_sexp (sexp : Sexp.t) : t =
  Sexp_helpers.parse_poly_variant_predicate predicates ~error_source sexp
;;

let sexp_of_t (t : t) : Sexp.t =
  match t with
  | `dunolint_lang_version v ->
    List
      [ Atom "dunolint_lang_version"
      ; Blang.sexp_of_t Dunolint_lang_version.Predicate.sexp_of_t v
      ]
;;
