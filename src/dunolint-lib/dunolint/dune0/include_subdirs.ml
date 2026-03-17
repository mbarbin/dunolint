(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open! Import

module Mode = struct
  let error_source = "include_subdirs.mode.t"

  type t =
    [ `no
    | `unqualified
    | `qualified
    ]

  let equal : t -> t -> bool = Stdlib.( = )

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "no"; conv = Nullary `no }
    ; { atom = "unqualified"; conv = Nullary `unqualified }
    ; { atom = "qualified"; conv = Nullary `qualified }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `no -> Atom "no"
    | `unqualified -> Atom "unqualified"
    | `qualified -> Atom "qualified"
  ;;
end

module Predicate = struct
  let error_source = "include_subdirs.t"

  type t = [ `equals of Mode.t ]

  let equal (a : t) (b : t) =
    if phys_equal a b
    then true
    else (
      match a, b with
      | `equals va, `equals vb -> Mode.equal va vb)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "equals"; conv = Unary (fun sexp -> `equals (Mode.t_of_sexp sexp)) } ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `equals v -> List [ Atom "equals"; Mode.sexp_of_t v ]
  ;;
end
