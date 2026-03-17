(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
