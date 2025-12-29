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

  let error_source = "lint.t"

  type t = [ `pps of Pps.Predicate.t Blang.t ]

  let equal =
    (fun a__007_ ->
       fun b__008_ ->
       if Stdlib.( == ) a__007_ b__008_
       then true
       else (
         match a__007_, b__008_ with
         | `pps _left__009_, `pps _right__010_ ->
           Blang.equal Pps.Predicate.equal _left__009_ _right__010_)
     : t -> t -> bool)
  ;;

  let __t_of_sexp__ =
    (function
     | Sexplib0.Sexp.Atom atom__014_ as _sexp__016_ ->
       (match atom__014_ with
        | "pps" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__016_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__014_ :: sexp_args__017_) as
       _sexp__016_ ->
       (match atom__014_ with
        | "pps" as _tag__018_ ->
          (match sexp_args__017_ with
           | arg0__019_ :: [] ->
             let res0__020_ = Blang.t_of_sexp Pps.Predicate.t_of_sexp arg0__019_ in
             `pps res0__020_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__018_
               _sexp__016_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__015_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source sexp__015_
     | Sexplib0.Sexp.List [] as sexp__015_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source sexp__015_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (fun sexp__022_ ->
       try __t_of_sexp__ sexp__022_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp__022_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (fun (`pps v__024_) ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "pps"; Blang.sexp_of_t Pps.Predicate.sexp_of_t v__024_ ]
     : t -> Sexplib0.Sexp.t)
  ;;
end
