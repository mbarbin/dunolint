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
