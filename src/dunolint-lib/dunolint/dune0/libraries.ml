(*********************************************************************************)
(*  Dunolint - A tool to lint and help manage files in dune projects             *)
(*  SPDX-FileCopyrightText: 2024-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: LGPL-3.0-or-later WITH LGPL-3.0-linking-exception   *)
(*********************************************************************************)

open! Import

module Predicate = struct
  let error_source = "libraries.predicate.t"

  type deprecated_names = [ `mem of Library__name.t list ]

  type t =
    [ `present of Library__name.t Nonempty_list.t
    | `absent of Library__name.t Nonempty_list.t
    | deprecated_names
    ]

  let equal (a : t) (b : t) =
    if phys_equal a b
    then true
    else (
      match a, b with
      | `present (a :: va), `present (b :: vb) | `absent (a :: va), `absent (b :: vb) ->
        equal_list Library__name.equal (a :: va) (b :: vb)
      | `mem va, `mem vb -> equal_list Library__name.equal va vb
      | (`present _ | `absent _ | `mem _), _ -> false)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    let names (f : Library__name.t Nonempty_list.t -> t) =
      Sexp_helpers.Variant_spec.Nonempty
        (fun ~context:_ ~fields:(hd :: tl) ->
          f (Library__name.t_of_sexp hd :: List.map tl ~f:Library__name.t_of_sexp))
    in
    [ { atom = "present"; conv = names (fun v -> `present v) }
    ; { atom = "absent"; conv = names (fun v -> `absent v) }
    ; { atom = "mem"
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
    | `present (hd :: tl) ->
      List (Atom "present" :: List.map (hd :: tl) ~f:Library__name.sexp_of_t)
    | `absent (hd :: tl) ->
      List (Atom "absent" :: List.map (hd :: tl) ~f:Library__name.sexp_of_t)
    | `mem v -> List (Atom "mem" :: List.map v ~f:Library__name.sexp_of_t)
  ;;
end
