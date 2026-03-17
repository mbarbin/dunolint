(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open! Import

module Predicate = struct
  let error_source = "libraries.predicate.t"

  type t = [ `mem of Library__name.t list ]

  let equal (a : t) (b : t) =
    if phys_equal a b
    then true
    else (
      match a, b with
      | `mem va, `mem vb -> equal_list Library__name.equal va vb)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "mem"
      ; conv =
          Variadic
            (fun ~context:_ ~fields -> `mem (List.map fields ~f:Library__name.t_of_sexp))
      }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `mem v -> List (Atom "mem" :: List.map v ~f:Library__name.sexp_of_t)
  ;;
end
