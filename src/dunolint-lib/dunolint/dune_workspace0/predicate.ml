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

open! Import

let error_source = "predicate.t"

type t = [ `dune_lang_version of Dune_lang_version.Predicate.t Blang.t ]

let equal (a : t) (b : t) =
  if phys_equal a b
  then true
  else (
    match a, b with
    | `dune_lang_version va, `dune_lang_version vb ->
      Blang.equal Dune_lang_version.Predicate.equal va vb)
;;

let variant_spec : t Sexp_helpers.Variant_spec.t =
  [ { atom = "dune_lang_version"
    ; conv =
        Unary
          (fun sexp ->
            `dune_lang_version
              (Blang.t_of_sexp Dune_lang_version.Predicate.t_of_sexp sexp))
    }
  ]
;;

let t_of_sexp (sexp : Sexp.t) : t =
  Sexp_helpers.parse_variant variant_spec ~error_source sexp
;;

let sexp_of_t (t : t) : Sexp.t =
  match t with
  | `dune_lang_version v ->
    List
      [ Atom "dune_lang_version"
      ; Blang.sexp_of_t Dune_lang_version.Predicate.sexp_of_t v
      ]
;;
