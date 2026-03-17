(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open! Import

let error_source = "predicate.t"

type t = [ `dunolint_lang_version of Dunolint_lang_version.Predicate.t Blang.t ]

let equal (a : t) (b : t) =
  if phys_equal a b
  then true
  else (
    match a, b with
    | `dunolint_lang_version va, `dunolint_lang_version vb ->
      Blang.equal Dunolint_lang_version.Predicate.equal va vb)
;;

let variant_spec : t Sexp_helpers.Variant_spec.t =
  [ { atom = "dunolint_lang_version"
    ; conv =
        Unary
          (fun sexp ->
            `dunolint_lang_version
              (Blang.t_of_sexp Dunolint_lang_version.Predicate.t_of_sexp sexp))
    }
  ]
;;

let t_of_sexp (sexp : Sexp.t) : t =
  Sexp_helpers.parse_variant variant_spec ~error_source sexp
;;

let sexp_of_t (t : t) : Sexp.t =
  match t with
  | `dunolint_lang_version v ->
    List
      [ Atom "dunolint_lang_version"
      ; Blang.sexp_of_t Dunolint_lang_version.Predicate.sexp_of_t v
      ]
;;
