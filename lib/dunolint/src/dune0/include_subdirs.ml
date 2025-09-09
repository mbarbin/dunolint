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

module Mode = struct
  [@@@coverage off]

  type t =
    [ `no
    | `unqualified
    | `qualified
    ]

  let compare =
    (fun a__001_ ->
       fun b__002_ ->
       if Stdlib.( == ) a__001_ b__002_
       then 0
       else (
         match a__001_, b__002_ with
         | `no, `no -> 0
         | `unqualified, `unqualified -> 0
         | `qualified, `qualified -> 0
         | x, y -> Stdlib.compare x y)
     : t -> t -> int)
  ;;

  let equal =
    (fun a__003_ ->
       fun b__004_ ->
       if Stdlib.( == ) a__003_ b__004_
       then true
       else (
         match a__003_, b__004_ with
         | `no, `no -> true
         | `unqualified, `unqualified -> true
         | `qualified, `qualified -> true
         | x, y -> Stdlib.( = ) x y)
     : t -> t -> bool)
  ;;

  let __t_of_sexp__ =
    (let error_source__010_ = "lib/dunolint/src/dune0/include_subdirs.ml.Mode.t" in
     function
     | Sexplib0.Sexp.Atom atom__006_ as _sexp__008_ ->
       (match atom__006_ with
        | "no" -> `no
        | "unqualified" -> `unqualified
        | "qualified" -> `qualified
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__006_ :: _) as _sexp__008_ ->
       (match atom__006_ with
        | "no" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source__010_ _sexp__008_
        | "unqualified" ->
          Sexplib0.Sexp_conv_error.ptag_no_args error_source__010_ _sexp__008_
        | "qualified" ->
          Sexplib0.Sexp_conv_error.ptag_no_args error_source__010_ _sexp__008_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__007_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__010_ sexp__007_
     | Sexplib0.Sexp.List [] as sexp__007_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__010_ sexp__007_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (let error_source__012_ = "lib/dunolint/src/dune0/include_subdirs.ml.Mode.t" in
     fun sexp__011_ ->
       try __t_of_sexp__ sexp__011_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__012_ sexp__011_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (function
     | `no -> Sexplib0.Sexp.Atom "no"
     | `unqualified -> Sexplib0.Sexp.Atom "unqualified"
     | `qualified -> Sexplib0.Sexp.Atom "qualified"
     : t -> Sexplib0.Sexp.t)
  ;;
end

module Predicate = struct
  [@@@coverage off]

  type t = [ `equals of Mode.t ]

  let compare =
    (fun a__013_ ->
       fun b__014_ ->
       if Stdlib.( == ) a__013_ b__014_
       then 0
       else (
         match a__013_, b__014_ with
         | `equals _left__015_, `equals _right__016_ ->
           Mode.compare _left__015_ _right__016_)
     : t -> t -> int)
  ;;

  let equal =
    (fun a__017_ ->
       fun b__018_ ->
       if Stdlib.( == ) a__017_ b__018_
       then true
       else (
         match a__017_, b__018_ with
         | `equals _left__019_, `equals _right__020_ ->
           Mode.equal _left__019_ _right__020_)
     : t -> t -> bool)
  ;;

  let __t_of_sexp__ =
    (let error_source__029_ = "lib/dunolint/src/dune0/include_subdirs.ml.Predicate.t" in
     function
     | Sexplib0.Sexp.Atom atom__022_ as _sexp__024_ ->
       (match atom__022_ with
        | "equals" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source__029_ _sexp__024_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__022_ :: sexp_args__025_) as
       _sexp__024_ ->
       (match atom__022_ with
        | "equals" as _tag__026_ ->
          (match sexp_args__025_ with
           | arg0__027_ :: [] ->
             let res0__028_ = Mode.t_of_sexp arg0__027_ in
             `equals res0__028_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__029_
               _tag__026_
               _sexp__024_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__023_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__029_ sexp__023_
     | Sexplib0.Sexp.List [] as sexp__023_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__029_ sexp__023_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (let error_source__031_ = "lib/dunolint/src/dune0/include_subdirs.ml.Predicate.t" in
     fun sexp__030_ ->
       try __t_of_sexp__ sexp__030_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__031_ sexp__030_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (fun (`equals v__032_) ->
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "equals"; Mode.sexp_of_t v__032_ ]
     : t -> Sexplib0.Sexp.t)
  ;;
end
