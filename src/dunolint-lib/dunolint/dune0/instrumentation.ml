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

module Backend = struct
  module Name = struct
    include Container_key.String_impl

    let invariant t =
      (not (String.is_empty t))
      && String.for_all t ~f:(fun c ->
        Char.is_alphanum c || Char.equal c '_' || Char.equal c '.')
    ;;

    include Validated_string.Make (struct
        let module_name = "Dunolint.Instrumentation.Backend.Name"
        let invariant = invariant
      end)
  end
end

module Predicate = struct
  let error_source = "instrumentation.t"

  type t = [ `backend of Backend.Name.t ]

  let equal (a : t) (b : t) =
    if Stdlib.( == ) a b
    then true
    else (
      match a, b with
      | `backend va, `backend vb -> Backend.Name.equal va vb)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "backend"
      ; conv = Unary (fun sexp -> `backend (Backend.Name.t_of_sexp sexp))
      }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `backend v -> List [ Atom "backend"; Backend.Name.sexp_of_t v ]
  ;;
end
