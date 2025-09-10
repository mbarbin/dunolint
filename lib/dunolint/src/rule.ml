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

module Stable = struct
  module V1 = struct
    [@@@coverage off]

    let error_source = "rule.v1.t"

    type ('predicate, 'invariant) t =
      [ `enforce of 'invariant
      | `return
      | `skip_subtree
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
             if Sexp_helpers.parsing_config_version_0.contents
             then `skip_subtree [@coverage off]
             else
               (* We'll drop this constructor entirely when we remove support
                  for version 0. *)
               Sexplib0.Sexp_conv.of_sexp_error
                 "The [skip_subtree] construct is not allowed in version 1 of dunolint \
                  config."
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
        | `skip_subtree -> Sexplib0.Sexp.Atom "skip_subtree"
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

    let compare = compare
    let equal = equal
  end

  module V0 = struct
    [@@@coverage off]

    let error_source = "rule.v0.t"

    type ('predicate, 'invariant) t =
      [ `enforce of 'invariant
      | `return
      | `skip_subtree
      | `cond of ('predicate Blang.t * ('predicate, 'invariant) t) list
      ]

    let rec __t_of_sexp__
      :  'predicate 'invariant.
         (Sexplib0.Sexp.t -> 'predicate)
      -> (Sexplib0.Sexp.t -> 'invariant)
      -> Sexplib0.Sexp.t
      -> ('predicate, 'invariant) t
      =
      fun _of_predicate__001_ ->
      fun _of_invariant__002_ -> function
        | Sexplib0.Sexp.Atom atom__004_ as _sexp__006_ ->
          (match atom__004_ with
           | "return" -> `return
           | "skip_subtree" -> `skip_subtree
           | "enforce" ->
             Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__006_
           | "cond" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__006_
           | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
        | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__004_ :: sexp_args__007_) as
          _sexp__006_ ->
          (match atom__004_ with
           | "enforce" as _tag__017_ ->
             (match sexp_args__007_ with
              | arg0__018_ :: [] ->
                let res0__019_ = _of_invariant__002_ arg0__018_ in
                `enforce res0__019_
              | _ ->
                Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                  error_source
                  _tag__017_
                  _sexp__006_)
           | "cond" as _tag__009_ ->
             (match sexp_args__007_ with
              | arg0__015_ :: [] ->
                let res0__016_ =
                  list_of_sexp
                    (function
                      | Sexplib0.Sexp.List [ arg0__010_; arg1__011_ ] ->
                        let res0__012_ = Blang.t_of_sexp _of_predicate__001_ arg0__010_
                        and res1__013_ =
                          t_of_sexp _of_predicate__001_ _of_invariant__002_ arg1__011_
                        in
                        res0__012_, res1__013_
                      | sexp__014_ ->
                        Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                          error_source
                          2
                          sexp__014_)
                    arg0__015_
                in
                `cond res0__016_
              | _ ->
                Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                  error_source
                  _tag__009_
                  _sexp__006_)
           | "return" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__006_
           | "skip_subtree" ->
             Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__006_
           | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
        | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__005_ ->
          Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source sexp__005_
        | Sexplib0.Sexp.List [] as sexp__005_ ->
          Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source sexp__005_

    and t_of_sexp
      :  'predicate 'invariant.
         (Sexplib0.Sexp.t -> 'predicate)
      -> (Sexplib0.Sexp.t -> 'invariant)
      -> Sexplib0.Sexp.t
      -> ('predicate, 'invariant) t
      =
      fun _of_predicate__001_ ->
      fun _of_invariant__002_ ->
      fun sexp__020_ ->
      try __t_of_sexp__ _of_predicate__001_ _of_invariant__002_ sexp__020_ with
      | Sexplib0.Sexp_conv_error.No_variant_match ->
        Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp__020_
    ;;

    let rec sexp_of_t
      :  'predicate 'invariant.
         ('predicate -> Sexplib0.Sexp.t)
      -> ('invariant -> Sexplib0.Sexp.t)
      -> ('predicate, 'invariant) t
      -> Sexplib0.Sexp.t
      =
      fun _of_predicate__022_ ->
      fun _of_invariant__023_ -> function
        | `enforce v__024_ ->
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "enforce"; _of_invariant__023_ v__024_ ]
        | `return -> Sexplib0.Sexp.Atom "return"
        | `skip_subtree -> Sexplib0.Sexp.Atom "skip_subtree"
        | `cond v__025_ ->
          Sexplib0.Sexp.List
            [ Sexplib0.Sexp.Atom "cond"
            ; sexp_of_list
                (fun (arg0__026_, arg1__027_) ->
                   let res0__028_ = Blang.sexp_of_t _of_predicate__022_ arg0__026_
                   and res1__029_ =
                     sexp_of_t _of_predicate__022_ _of_invariant__023_ arg1__027_
                   in
                   Sexplib0.Sexp.List [ res0__028_; res1__029_ ])
                v__025_
            ]
    ;;

    let compare = compare
    let equal = equal
  end
end
