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

module Predicate = struct
  [@@@coverage off]

  type t =
    [ `no_preprocessing
    | `pps of Pps.Predicate.t Blang.t
    ]
  [@@deriving_inline compare, equal, sexp]

  let compare =
    (fun a__001_ ->
       fun b__002_ ->
       if Stdlib.( == ) a__001_ b__002_
       then 0
       else (
         match a__001_, b__002_ with
         | `no_preprocessing, `no_preprocessing -> 0
         | `pps _left__003_, `pps _right__004_ ->
           Blang.compare Pps.Predicate.compare _left__003_ _right__004_
         | x, y -> Stdlib.compare x y)
     : t -> t -> int)
  ;;

  let equal =
    (fun a__007_ ->
       fun b__008_ ->
       if Stdlib.( == ) a__007_ b__008_
       then true
       else (
         match a__007_, b__008_ with
         | `no_preprocessing, `no_preprocessing -> true
         | `pps _left__009_, `pps _right__010_ ->
           Blang.equal Pps.Predicate.equal _left__009_ _right__010_
         | x, y -> Stdlib.( = ) x y)
     : t -> t -> bool)
  ;;

  let __t_of_sexp__ =
    (let error_source__018_ = "lib/dunolint/src/dune0/preprocess.ml.Predicate.t" in
     function
     | Sexplib0.Sexp.Atom atom__014_ as _sexp__016_ ->
       (match atom__014_ with
        | "no_preprocessing" -> `no_preprocessing
        | "pps" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__018_ _sexp__016_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__014_ :: sexp_args__017_) as
       _sexp__016_ ->
       (match atom__014_ with
        | "pps" as _tag__019_ ->
          (match sexp_args__017_ with
           | arg0__020_ :: [] ->
             let res0__021_ = Blang.t_of_sexp Pps.Predicate.t_of_sexp arg0__020_ in
             `pps res0__021_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__018_
               _tag__019_
               _sexp__016_)
        | "no_preprocessing" ->
          Sexplib0.Sexp_conv_error.ptag_no_args error_source__018_ _sexp__016_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__015_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__018_ sexp__015_
     | Sexplib0.Sexp.List [] as sexp__015_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__018_ sexp__015_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (let error_source__023_ = "lib/dunolint/src/dune0/preprocess.ml.Predicate.t" in
     fun sexp__022_ ->
       try __t_of_sexp__ sexp__022_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__023_ sexp__022_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (function
     | `no_preprocessing -> Sexplib0.Sexp.Atom "no_preprocessing"
     | `pps v__024_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "pps"; Blang.sexp_of_t Pps.Predicate.sexp_of_t v__024_ ]
     : t -> Sexplib0.Sexp.t)
  ;;

  [@@@deriving.end]
end
