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

module Predicate = struct
  let error_source = "library.modes.t"

  type deprecated_names =
    [ `has_mode of Compilation_mode.t
    | `has_modes of Compilation_mode.t list
    ]

  type t =
    [ `mem of Compilation_mode.t list
    | deprecated_names
    ]

  let equal (a : t) (b : t) =
    if Stdlib.( == ) a b
    then true
    else (
      match a, b with
      | `mem va, `mem vb -> equal_list Compilation_mode.equal va vb
      | `has_mode va, `has_mode vb -> Compilation_mode.equal va vb
      | `has_modes va, `has_modes vb -> equal_list Compilation_mode.equal va vb
      | (`mem _ | `has_mode _ | `has_modes _), _ -> false)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "mem"
      ; conv =
          Variadic
            (fun ~context:_ ~fields ->
              `mem (List.map fields ~f:Compilation_mode.t_of_sexp))
      }
      (* Deprecated - parsed and normalized to [mem]. *)
    ; { atom = "has_mode"
      ; conv = Unary (fun sexp -> `mem [ Compilation_mode.t_of_sexp sexp ])
      }
    ; { atom = "has_modes"
      ; conv = Unary (fun sexp -> `mem (list_of_sexp Compilation_mode.t_of_sexp sexp))
      }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `mem v -> List (Atom "mem" :: List.map v ~f:Compilation_mode.sexp_of_t)
    | `has_mode v -> List [ Atom "has_mode"; Compilation_mode.sexp_of_t v ]
    | `has_modes v -> List [ Atom "has_modes"; sexp_of_list Compilation_mode.sexp_of_t v ]
  ;;
end
