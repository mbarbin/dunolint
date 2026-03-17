(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open! Import

module Predicate = struct
  let error_source = "lint.t"

  type t = [ `pps of Pps.Predicate.t Blang.t ]

  let equal (a : t) (b : t) =
    if phys_equal a b
    then true
    else (
      match a, b with
      | `pps va, `pps vb -> Blang.equal Pps.Predicate.equal va vb)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "pps"
      ; conv = Unary (fun sexp -> `pps (Blang.t_of_sexp Pps.Predicate.t_of_sexp sexp))
      }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `pps v -> List [ Atom "pps"; Blang.sexp_of_t Pps.Predicate.sexp_of_t v ]
  ;;
end
