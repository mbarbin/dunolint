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

type ('predicate, 'invariant) t =
  [ `enforce of 'invariant
  | `return
  | `cond of ('predicate Blang.t * ('predicate, 'invariant) t) list
  ]

let rec equal
  :  'predicate 'invariant.
     ('predicate -> 'predicate -> bool)
  -> ('invariant -> 'invariant -> bool)
  -> ('predicate, 'invariant) t
  -> ('predicate, 'invariant) t
  -> bool
  =
  fun equal_predicate equal_invariant (a : _ t) (b : _ t) ->
  match a, b with
  | `enforce va, `enforce vb -> phys_equal a b || equal_invariant va vb
  | `return, `return -> true
  | `cond va, `cond vb ->
    phys_equal a b
    || equal_list
         (fun (cond_a, ta) (cond_b, tb) ->
            Blang.equal equal_predicate cond_a cond_b
            && equal equal_predicate equal_invariant ta tb)
         va
         vb
  | (`enforce _ | `return | `cond _), _ -> false
;;

let rec eval t ~f =
  match t with
  | (`enforce _ | `return) as invariant -> invariant
  | `cond clauses ->
    (match
       List.find clauses ~f:(fun (condition, _) ->
         match Trilang.eval condition ~f with
         | True -> true
         | False | Undefined -> false)
     with
     | None -> `return
     | Some (_, t) -> eval t ~f)
;;

module Stable = struct
  module V1 = struct
    let error_source = "rule.v1.t"

    type ('predicate, 'invariant) t =
      [ `enforce of 'invariant
      | `return
      | `cond of ('predicate Blang.t * ('predicate, 'invariant) t) list
      ]

    let rec t_of_sexp
      :  'predicate 'invariant.
         (Sexp.t -> 'predicate)
      -> (Sexp.t -> 'invariant)
      -> Sexp.t
      -> ('predicate, 'invariant) t
      =
      fun predicate_of_sexp invariant_of_sexp sexp ->
      match sexp with
      | (Atom "skip_subtree" as located_sexp)
      | List ((Atom "skip_subtree" as located_sexp) :: _) ->
        Sexplib0.Sexp_conv.of_sexp_error
          "The [skip_subtree] construct is no longer supported."
          located_sexp
      | _ ->
        Sexp_helpers.parse_variant
          (variant_spec predicate_of_sexp invariant_of_sexp)
          ~error_source
          sexp

    and variant_spec
      :  'predicate 'invariant.
         (Sexp.t -> 'predicate)
      -> (Sexp.t -> 'invariant)
      -> ('predicate, 'invariant) t Sexp_helpers.Variant_spec.t
      =
      fun predicate_of_sexp invariant_of_sexp ->
      [ { atom = "enforce"; conv = Unary (fun sexp -> `enforce (invariant_of_sexp sexp)) }
      ; { atom = "return"; conv = Nullary `return }
      ; { atom = "cond"
        ; conv =
            Variadic
              (fun ~context:_ ~fields ->
                `cond
                  (List.map fields ~f:(function
                     | Sexp.List [ predicate_sexp; rule_sexp ] ->
                       let predicate = Blang.t_of_sexp predicate_of_sexp predicate_sexp
                       and rule =
                         t_of_sexp predicate_of_sexp invariant_of_sexp rule_sexp
                       in
                       predicate, rule
                     | sexp ->
                       Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                         error_source
                         2
                         sexp)))
        }
      ]
    ;;

    let rec sexp_of_t
      :  'predicate 'invariant.
         ('predicate -> Sexp.t)
      -> ('invariant -> Sexp.t)
      -> ('predicate, 'invariant) t
      -> Sexp.t
      =
      fun sexp_of_predicate sexp_of_invariant -> function
      | `enforce v -> Sexp.List [ Atom "enforce"; sexp_of_invariant v ]
      | `return -> Atom "return"
      | `cond clauses ->
        Sexp.List
          (Atom "cond"
           :: List.map clauses ~f:(fun (predicate, rule) ->
             Sexp.List
               [ Blang.sexp_of_t sexp_of_predicate predicate
               ; sexp_of_t sexp_of_predicate sexp_of_invariant rule
               ]))
    ;;

    let equal = equal
  end
end
