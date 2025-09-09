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

module T0 = struct
  [@@@coverage off]

  type t = int * int

  let equal =
    (fun a__001_ ->
       fun b__002_ ->
       let t__003_, t__004_ = a__001_ in
       let t__005_, t__006_ = b__002_ in
       Stdlib.( && ) (equal_int t__003_ t__005_) (equal_int t__004_ t__006_)
     : t -> t -> bool)
  ;;

  let compare =
    (fun a__007_ ->
       fun b__008_ ->
       let t__009_, t__010_ = a__007_ in
       let t__011_, t__012_ = b__008_ in
       match compare_int t__009_ t__011_ with
       | 0 -> compare_int t__010_ t__012_
       | n -> n
     : t -> t -> int)
  ;;

  let t_of_sexp =
    (let error_source__019_ =
       "lib/dunolint/src/dune_project0/dune_lang_version.ml.T0.t"
     in
     function
     | Sexplib0.Sexp.List [ arg0__014_; arg1__015_ ] ->
       let res0__016_ = int_of_sexp arg0__014_
       and res1__017_ = int_of_sexp arg1__015_ in
       res0__016_, res1__017_
     | sexp__018_ ->
       Sexplib0.Sexp_conv_error.tuple_of_size_n_expected error_source__019_ 2 sexp__018_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (fun (arg0__020_, arg1__021_) ->
       let res0__022_ = sexp_of_int arg0__020_
       and res1__023_ = sexp_of_int arg1__021_ in
       Sexplib0.Sexp.List [ res0__022_; res1__023_ ]
     : t -> Sexplib0.Sexp.t)
  ;;
end

include T0

let create t = t
let to_string (a, b) = Printf.sprintf "%d.%d" a b

module Predicate = struct
  [@@@coverage off]

  type nonrec t =
    [ `equals of t
    | `greater_than_or_equal_to of t
    | `less_than_or_equal_to of t
    ]

  let compare =
    (fun a__024_ ->
       fun b__025_ ->
       if Stdlib.( == ) a__024_ b__025_
       then 0
       else (
         match a__024_, b__025_ with
         | `equals _left__026_, `equals _right__027_ -> compare _left__026_ _right__027_
         | `greater_than_or_equal_to _left__028_, `greater_than_or_equal_to _right__029_
           -> compare _left__028_ _right__029_
         | `less_than_or_equal_to _left__030_, `less_than_or_equal_to _right__031_ ->
           compare _left__030_ _right__031_
         | x, y -> Stdlib.compare x y)
     : t -> t -> int)
  ;;

  let equal =
    (fun a__032_ ->
       fun b__033_ ->
       if Stdlib.( == ) a__032_ b__033_
       then true
       else (
         match a__032_, b__033_ with
         | `equals _left__034_, `equals _right__035_ -> equal _left__034_ _right__035_
         | `greater_than_or_equal_to _left__036_, `greater_than_or_equal_to _right__037_
           -> equal _left__036_ _right__037_
         | `less_than_or_equal_to _left__038_, `less_than_or_equal_to _right__039_ ->
           equal _left__038_ _right__039_
         | x, y -> Stdlib.( = ) x y)
     : t -> t -> bool)
  ;;

  let __t_of_sexp__ =
    (let error_source__048_ =
       "lib/dunolint/src/dune_project0/dune_lang_version.ml.Predicate.t"
     in
     function
     | Sexplib0.Sexp.Atom atom__041_ as _sexp__043_ ->
       (match atom__041_ with
        | "equals" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source__048_ _sexp__043_
        | "greater_than_or_equal_to" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source__048_ _sexp__043_
        | "less_than_or_equal_to" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source__048_ _sexp__043_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__041_ :: sexp_args__044_) as
       _sexp__043_ ->
       (match atom__041_ with
        | "equals" as _tag__052_ ->
          (match sexp_args__044_ with
           | arg0__053_ :: [] ->
             let res0__054_ = t_of_sexp arg0__053_ in
             `equals res0__054_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__048_
               _tag__052_
               _sexp__043_)
        | "greater_than_or_equal_to" as _tag__049_ ->
          (match sexp_args__044_ with
           | arg0__050_ :: [] ->
             let res0__051_ = t_of_sexp arg0__050_ in
             `greater_than_or_equal_to res0__051_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__048_
               _tag__049_
               _sexp__043_)
        | "less_than_or_equal_to" as _tag__045_ ->
          (match sexp_args__044_ with
           | arg0__046_ :: [] ->
             let res0__047_ = t_of_sexp arg0__046_ in
             `less_than_or_equal_to res0__047_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__048_
               _tag__045_
               _sexp__043_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__042_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__048_ sexp__042_
     | Sexplib0.Sexp.List [] as sexp__042_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__048_ sexp__042_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (let error_source__056_ =
       "lib/dunolint/src/dune_project0/dune_lang_version.ml.Predicate.t"
     in
     fun sexp__055_ ->
       try __t_of_sexp__ sexp__055_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__056_ sexp__055_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (function
     | `equals v__057_ ->
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "equals"; sexp_of_t v__057_ ]
     | `greater_than_or_equal_to v__058_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "greater_than_or_equal_to"; sexp_of_t v__058_ ]
     | `less_than_or_equal_to v__059_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "less_than_or_equal_to"; sexp_of_t v__059_ ]
     : t -> Sexplib0.Sexp.t)
  ;;
end
