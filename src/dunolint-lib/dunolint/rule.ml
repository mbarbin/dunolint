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

module T = struct
  [@@@coverage off]

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
    if Stdlib.( == ) a b
    then true
    else (
      match a, b with
      | `enforce va, `enforce vb -> equal_invariant va vb
      | `return, `return -> true
      | `cond va, `cond vb ->
        equal_list
          (fun (conda, ta) (condb, tb) ->
             Blang.equal equal_predicate conda condb
             && equal equal_predicate equal_invariant ta tb)
          va
          vb
      | (`enforce _ | `return | `cond _), _ -> false)
  ;;
end

include T

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
    [@@@coverage off]

    let error_source = "rule.v1.t"

    type ('predicate, 'invariant) t =
      [ `enforce of 'invariant
      | `return
      | `cond of ('predicate Blang.t * ('predicate, 'invariant) t) list
      ]

    let rec __t_of_sexp__
      :  'predicate 'invariant.
         (Sexplib0.Sexp.t -> 'predicate)
      -> (Sexplib0.Sexp.t -> 'invariant)
      -> Sexplib0.Sexp.t
      -> ('predicate, 'invariant) t
      =
      fun _of_predicate__073_ ->
      fun _of_invariant__074_ -> function
        | Sexplib0.Sexp.Atom atom__076_ as _sexp__078_ ->
          (match atom__076_ with
           | "return" -> `return
           | "skip_subtree" ->
             Sexplib0.Sexp_conv.of_sexp_error
               "The [skip_subtree] construct is no longer supported."
               _sexp__078_
           | "enforce" ->
             Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__078_
           | "cond" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__078_
           | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
        | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__076_ :: sexp_args__079_) as
          _sexp__078_ ->
          (match atom__076_ with
           | "enforce" as _tag__089_ ->
             (match sexp_args__079_ with
              | arg0__090_ :: [] ->
                let res0__091_ = _of_invariant__074_ arg0__090_ in
                `enforce res0__091_
              | _ ->
                Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                  error_source
                  _tag__089_
                  _sexp__078_)
           | "cond" as _tag__081_ ->
             let res0__088_ =
               List.map sexp_args__079_ ~f:(function
                 | Sexplib0.Sexp.List [ arg0__082_; arg1__083_ ] ->
                   let res0__084_ = Blang.t_of_sexp _of_predicate__073_ arg0__082_
                   and res1__085_ =
                     t_of_sexp _of_predicate__073_ _of_invariant__074_ arg1__083_
                   in
                   res0__084_, res1__085_
                 | sexp__086_ ->
                   Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                     error_source
                     2
                     sexp__086_)
             in
             `cond res0__088_
           | "return" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__078_
           | "skip_subtree" ->
             Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__078_
           | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
        | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__077_ ->
          Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source sexp__077_
        | Sexplib0.Sexp.List [] as sexp__077_ ->
          Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source sexp__077_

    and t_of_sexp
      :  'predicate 'invariant.
         (Sexplib0.Sexp.t -> 'predicate)
      -> (Sexplib0.Sexp.t -> 'invariant)
      -> Sexplib0.Sexp.t
      -> ('predicate, 'invariant) t
      =
      fun _of_predicate__073_ ->
      fun _of_invariant__074_ ->
      fun sexp__092_ ->
      try __t_of_sexp__ _of_predicate__073_ _of_invariant__074_ sexp__092_ with
      | Sexplib0.Sexp_conv_error.No_variant_match ->
        Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp__092_
    ;;

    let rec sexp_of_t
      :  'predicate 'invariant.
         ('predicate -> Sexplib0.Sexp.t)
      -> ('invariant -> Sexplib0.Sexp.t)
      -> ('predicate, 'invariant) t
      -> Sexplib0.Sexp.t
      =
      fun _of_predicate__094_ ->
      fun _of_invariant__095_ -> function
        | `enforce v__096_ ->
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "enforce"; _of_invariant__095_ v__096_ ]
        | `return -> Sexplib0.Sexp.Atom "return"
        | `cond v__097_ ->
          Sexplib0.Sexp.List
            (Sexplib0.Sexp.Atom "cond"
             :: List.map v__097_ ~f:(fun (arg0__098_, arg1__099_) ->
               let res0__100_ = Blang.sexp_of_t _of_predicate__094_ arg0__098_
               and res1__101_ =
                 sexp_of_t _of_predicate__094_ _of_invariant__095_ arg1__099_
               in
               Sexplib0.Sexp.List [ res0__100_; res1__101_ ]))
    ;;

    let equal = equal
  end
end
