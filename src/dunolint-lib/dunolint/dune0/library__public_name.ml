(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open! Import
include String_container_key

let invariant t =
  (not (String.is_empty t))
  && String.for_all t ~f:(fun c ->
    Char.is_alphanum c || Char.equal c '_' || Char.equal c '-' || Char.equal c '.')
;;

include Validated_string.Make (struct
    let module_name = "Dunolint.Library.Public_name"
    let invariant = invariant
  end)

module Predicate = struct
  type name = t

  let equal_name = (equal : name -> name -> bool)
  let name_of_sexp = (t_of_sexp : Sexplib0.Sexp.t -> name)
  let sexp_of_name = (sexp_of_t : name -> Sexplib0.Sexp.t)
  let error_source = "library.public_name.t"

  type t =
    [ `equals of name
    | `is_prefix of string
    | `is_suffix of string
    ]

  let equal (a : t) (b : t) =
    if phys_equal a b
    then true
    else (
      match a, b with
      | `equals va, `equals vb -> equal_name va vb
      | `is_prefix va, `is_prefix vb -> equal_string va vb
      | `is_suffix va, `is_suffix vb -> equal_string va vb
      | (`equals _ | `is_prefix _ | `is_suffix _), _ -> false)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "equals"; conv = Unary (fun sexp -> `equals (name_of_sexp sexp)) }
    ; { atom = "is_prefix"; conv = Unary (fun sexp -> `is_prefix (string_of_sexp sexp)) }
    ; { atom = "is_suffix"; conv = Unary (fun sexp -> `is_suffix (string_of_sexp sexp)) }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `equals v -> List [ Atom "equals"; sexp_of_name v ]
    | `is_prefix v -> List [ Atom "is_prefix"; sexp_of_string v ]
    | `is_suffix v -> List [ Atom "is_suffix"; sexp_of_string v ]
  ;;
end
