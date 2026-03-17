(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
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
    if phys_equal a b
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
