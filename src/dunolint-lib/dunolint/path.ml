(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open! Import

module Predicate = struct
  let error_source = "path.t"

  type t = [ `glob of Glob.t ]

  let equal (a : t) (b : t) =
    if phys_equal a b
    then true
    else (
      match a, b with
      | `glob va, `glob vb -> Glob.equal va vb)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "glob"; conv = Unary (fun sexp -> `glob (Glob.t_of_sexp sexp)) }
    ; { atom = "equals"
      ; conv =
          Unary_with_context
            (fun ~context ~arg:_ ->
              raise
                (Sexp.Of_sexp_error
                   (Failure "The [path.equals] construct is no longer supported.", context)))
      }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `glob v -> List [ Atom "glob"; Glob.sexp_of_t v ]
  ;;
end
