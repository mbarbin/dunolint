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
    | `skip_subtree
    | `cond of ('predicate Blang.t * ('predicate, 'invariant) t) list
    ]

  let rec compare
    :  'predicate 'invariant.
       ('predicate -> 'predicate -> int)
    -> ('invariant -> 'invariant -> int)
    -> ('predicate, 'invariant) t
    -> ('predicate, 'invariant) t
    -> int
    =
    fun _cmp__predicate ->
    fun _cmp__invariant ->
    fun a__001_ ->
    fun b__002_ ->
    if Stdlib.( == ) a__001_ b__002_
    then 0
    else (
      match a__001_, b__002_ with
      | `enforce _left__003_, `enforce _right__004_ ->
        _cmp__invariant _left__003_ _right__004_
      | `return, `return -> 0
      | `skip_subtree, `skip_subtree -> 0
      | `cond _left__005_, `cond _right__006_ ->
        compare_list
          (fun a__007_ ->
             fun b__008_ ->
             let t__009_, t__010_ = a__007_ in
             let t__011_, t__012_ = b__008_ in
             match Blang.compare _cmp__predicate t__009_ t__011_ with
             | 0 -> compare _cmp__predicate _cmp__invariant t__010_ t__012_
             | n -> n)
          _left__005_
          _right__006_
      | x, y -> Stdlib.compare x y)
  ;;

  let rec equal
    :  'predicate 'invariant.
       ('predicate -> 'predicate -> bool)
    -> ('invariant -> 'invariant -> bool)
    -> ('predicate, 'invariant) t
    -> ('predicate, 'invariant) t
    -> bool
    =
    fun _cmp__predicate ->
    fun _cmp__invariant ->
    fun a__019_ ->
    fun b__020_ ->
    if Stdlib.( == ) a__019_ b__020_
    then true
    else (
      match a__019_, b__020_ with
      | `enforce _left__021_, `enforce _right__022_ ->
        _cmp__invariant _left__021_ _right__022_
      | `return, `return -> true
      | `skip_subtree, `skip_subtree -> true
      | `cond _left__023_, `cond _right__024_ ->
        equal_list
          (fun a__025_ ->
             fun b__026_ ->
             let t__027_, t__028_ = a__025_ in
             let t__029_, t__030_ = b__026_ in
             Stdlib.( && )
               (Blang.equal _cmp__predicate t__027_ t__029_)
               (equal _cmp__predicate _cmp__invariant t__028_ t__030_))
          _left__023_
          _right__024_
      | x, y -> Stdlib.( = ) x y)
  ;;

  let rec __t_of_sexp__
    :  'predicate 'invariant.
       (Sexplib0.Sexp.t -> 'predicate)
    -> (Sexplib0.Sexp.t -> 'invariant)
    -> Sexplib0.Sexp.t
    -> ('predicate, 'invariant) t
    =
    let error_source__044_ = "lib/dunolint/src/rule.ml.T.t" in
    fun _of_predicate__037_ ->
      fun _of_invariant__038_ -> function
        | Sexplib0.Sexp.Atom atom__040_ as _sexp__042_ ->
          (match atom__040_ with
           | "return" -> `return
           | "skip_subtree" -> `skip_subtree
           | "enforce" ->
             Sexplib0.Sexp_conv_error.ptag_takes_args error_source__044_ _sexp__042_
           | "cond" ->
             Sexplib0.Sexp_conv_error.ptag_takes_args error_source__044_ _sexp__042_
           | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
        | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__040_ :: sexp_args__043_) as
          _sexp__042_ ->
          (match atom__040_ with
           | "enforce" as _tag__053_ ->
             (match sexp_args__043_ with
              | arg0__054_ :: [] ->
                let res0__055_ = _of_invariant__038_ arg0__054_ in
                `enforce res0__055_
              | _ ->
                Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                  error_source__044_
                  _tag__053_
                  _sexp__042_)
           | "cond" as _tag__045_ ->
             (match sexp_args__043_ with
              | arg0__051_ :: [] ->
                let res0__052_ =
                  list_of_sexp
                    (function
                      | Sexplib0.Sexp.List [ arg0__046_; arg1__047_ ] ->
                        let res0__048_ = Blang.t_of_sexp _of_predicate__037_ arg0__046_
                        and res1__049_ =
                          t_of_sexp _of_predicate__037_ _of_invariant__038_ arg1__047_
                        in
                        res0__048_, res1__049_
                      | sexp__050_ ->
                        Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                          error_source__044_
                          2
                          sexp__050_)
                    arg0__051_
                in
                `cond res0__052_
              | _ ->
                Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                  error_source__044_
                  _tag__045_
                  _sexp__042_)
           | "return" ->
             Sexplib0.Sexp_conv_error.ptag_no_args error_source__044_ _sexp__042_
           | "skip_subtree" ->
             Sexplib0.Sexp_conv_error.ptag_no_args error_source__044_ _sexp__042_
           | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
        | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__041_ ->
          Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
            error_source__044_
            sexp__041_
        | Sexplib0.Sexp.List [] as sexp__041_ ->
          Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
            error_source__044_
            sexp__041_

  and t_of_sexp
    :  'predicate 'invariant.
       (Sexplib0.Sexp.t -> 'predicate)
    -> (Sexplib0.Sexp.t -> 'invariant)
    -> Sexplib0.Sexp.t
    -> ('predicate, 'invariant) t
    =
    let error_source__057_ = "lib/dunolint/src/rule.ml.T.t" in
    fun _of_predicate__037_ ->
      fun _of_invariant__038_ ->
      fun sexp__056_ ->
      try __t_of_sexp__ _of_predicate__037_ _of_invariant__038_ sexp__056_ with
      | Sexplib0.Sexp_conv_error.No_variant_match ->
        Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__057_ sexp__056_
  ;;

  let rec sexp_of_t
    :  'predicate 'invariant.
       ('predicate -> Sexplib0.Sexp.t)
    -> ('invariant -> Sexplib0.Sexp.t)
    -> ('predicate, 'invariant) t
    -> Sexplib0.Sexp.t
    =
    fun _of_predicate__058_ ->
    fun _of_invariant__059_ -> function
      | `enforce v__060_ ->
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "enforce"; _of_invariant__059_ v__060_ ]
      | `return -> Sexplib0.Sexp.Atom "return"
      | `skip_subtree -> Sexplib0.Sexp.Atom "skip_subtree"
      | `cond v__061_ ->
        Sexplib0.Sexp.List
          [ Sexplib0.Sexp.Atom "cond"
          ; sexp_of_list
              (fun (arg0__062_, arg1__063_) ->
                 let res0__064_ = Blang.sexp_of_t _of_predicate__058_ arg0__062_
                 and res1__065_ =
                   sexp_of_t _of_predicate__058_ _of_invariant__059_ arg1__063_
                 in
                 Sexplib0.Sexp.List [ res0__064_; res1__065_ ])
              v__061_
          ]
  ;;
end

include T

let eval t ~f =
  let rec aux_t t =
    match t with
    | (`enforce _ | `return | `skip_subtree) as invariant -> invariant
    | `cond clauses ->
      (match
         List.find clauses ~f:(fun (condition, _) ->
           match Trilang.eval condition ~f with
           | True -> true
           | False | Undefined -> false)
       with
       | None -> `return
       | Some (_, t) -> aux_t t)
  in
  aux_t t
;;
