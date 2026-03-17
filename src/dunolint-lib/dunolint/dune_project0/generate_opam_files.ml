(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

module Predicate = struct
  let error_source = "generate_opam_files.t"

  type t = [ `is_present ]

  let equal : t -> t -> bool = Stdlib.( = )

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "is_present"; conv = Nullary `is_present } ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `is_present -> Atom "is_present"
  ;;
end
