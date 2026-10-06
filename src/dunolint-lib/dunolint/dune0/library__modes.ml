(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open! Import

module Predicate = struct
  let error_source = "library.modes.t"

  type deprecated_names =
    [ `mem of Compilation_mode.t list
    | `has_mode of Compilation_mode.t
    | `has_modes of Compilation_mode.t list
    ]

  type t =
    [ `present of Compilation_mode.t Nonempty_list.t
    | `absent of Compilation_mode.t Nonempty_list.t
    | deprecated_names
    ]

  let equal (a : t) (b : t) =
    if phys_equal a b
    then true
    else (
      match a, b with
      | `present (a :: va), `present (b :: vb) | `absent (a :: va), `absent (b :: vb) ->
        equal_list Compilation_mode.equal (a :: va) (b :: vb)
      | `mem va, `mem vb -> equal_list Compilation_mode.equal va vb
      | `has_mode va, `has_mode vb -> Compilation_mode.equal va vb
      | `has_modes va, `has_modes vb -> equal_list Compilation_mode.equal va vb
      | (`present _ | `absent _ | `mem _ | `has_mode _ | `has_modes _), _ -> false)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    let modes (f : Compilation_mode.t Nonempty_list.t -> t) =
      Sexp_helpers.Variant_spec.Nonempty
        (fun ~context:_ ~fields:(hd :: tl) ->
          f (Compilation_mode.t_of_sexp hd :: List.map tl ~f:Compilation_mode.t_of_sexp))
    in
    [ { atom = "present"; conv = modes (fun v -> `present v) }
    ; { atom = "absent"; conv = modes (fun v -> `absent v) }
    ; { atom = "mem"
      ; conv =
          Variadic
            (fun ~context:_ ~fields ->
              `mem (List.map fields ~f:Compilation_mode.t_of_sexp))
      }
      (* Deprecated - parsed and normalized to [present] when that preserves their
         behavior, otherwise to [mem]. *)
    ; { atom = "has_mode"
      ; conv = Unary (fun sexp -> `present [ Compilation_mode.t_of_sexp sexp ])
      }
    ; { atom = "has_modes"
      ; conv =
          Unary
            (fun sexp ->
              match list_of_sexp Compilation_mode.t_of_sexp sexp with
              | [ mode ] -> `present [ mode ]
              | ([] | _ :: _ :: _) as modes -> `mem modes)
      }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `present (hd :: tl) ->
      List (Atom "present" :: List.map (hd :: tl) ~f:Compilation_mode.sexp_of_t)
    | `absent (hd :: tl) ->
      List (Atom "absent" :: List.map (hd :: tl) ~f:Compilation_mode.sexp_of_t)
    | `mem v -> List (Atom "mem" :: List.map v ~f:Compilation_mode.sexp_of_t)
    | `has_mode v -> List [ Atom "has_mode"; Compilation_mode.sexp_of_t v ]
    | `has_modes v -> List [ Atom "has_modes"; sexp_of_list Compilation_mode.sexp_of_t v ]
  ;;
end
