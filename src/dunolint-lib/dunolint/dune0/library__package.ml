(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open! Import

module Predicate = struct
  let error_source = "library.package.t"

  type t =
    [ `equals of Package.Name.t
    | `is_prefix of string
    | `is_suffix of string
    ]

  let equal (a : t) (b : t) =
    if phys_equal a b
    then true
    else (
      match a, b with
      | `equals va, `equals vb -> Package.Name.equal va vb
      | `is_prefix va, `is_prefix vb -> String.equal va vb
      | `is_suffix va, `is_suffix vb -> String.equal va vb
      | (`equals _ | `is_prefix _ | `is_suffix _), _ -> false)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "equals"
      ; conv = Unary (fun sexp -> `equals (Package.Name.t_of_sexp sexp))
      }
    ; { atom = "is_prefix"
      ; conv = Unary (fun sexp -> `is_prefix (Sexplib0.Sexp_conv.string_of_sexp sexp))
      }
    ; { atom = "is_suffix"
      ; conv = Unary (fun sexp -> `is_suffix (Sexplib0.Sexp_conv.string_of_sexp sexp))
      }
    ]
  ;;

  let t_of_sexp (sexp : Sexplib0.Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexplib0.Sexp.t =
    match t with
    | `equals v -> List [ Atom "equals"; Package.Name.sexp_of_t v ]
    | `is_prefix v -> List [ Atom "is_prefix"; Sexplib0.Sexp_conv.sexp_of_string v ]
    | `is_suffix v -> List [ Atom "is_suffix"; Sexplib0.Sexp_conv.sexp_of_string v ]
  ;;
end
