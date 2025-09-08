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

module T = struct
  [@@@coverage off]

  type t =
    [ `path of Path.Predicate.t Blang.t
    | `dune of Dune.Predicate.t Blang.t
    | `dune_project of Dune_project.Predicate.t Blang.t
    ]
  [@@deriving_inline compare, equal, sexp]

  let compare =
    (fun a__001_ ->
       fun b__002_ ->
       if Stdlib.( == ) a__001_ b__002_
       then 0
       else (
         match a__001_, b__002_ with
         | `path _left__003_, `path _right__004_ ->
           Blang.compare Path.Predicate.compare _left__003_ _right__004_
         | `dune _left__007_, `dune _right__008_ ->
           Blang.compare Dune.Predicate.compare _left__007_ _right__008_
         | `dune_project _left__011_, `dune_project _right__012_ ->
           Blang.compare Dune_project.Predicate.compare _left__011_ _right__012_
         | x, y -> Stdlib.compare x y)
     : t -> t -> int)
  ;;

  let equal =
    (fun a__015_ ->
       fun b__016_ ->
       if Stdlib.( == ) a__015_ b__016_
       then true
       else (
         match a__015_, b__016_ with
         | `path _left__017_, `path _right__018_ ->
           Blang.equal Path.Predicate.equal _left__017_ _right__018_
         | `dune _left__021_, `dune _right__022_ ->
           Blang.equal Dune.Predicate.equal _left__021_ _right__022_
         | `dune_project _left__025_, `dune_project _right__026_ ->
           Blang.equal Dune_project.Predicate.equal _left__025_ _right__026_
         | x, y -> Stdlib.( = ) x y)
     : t -> t -> bool)
  ;;

  let __t_of_sexp__ =
    (let error_source__037_ = "lib/dunolint/src/predicate.ml.t" in
     function
     | Sexplib0.Sexp.Atom atom__030_ as _sexp__032_ ->
       (match atom__030_ with
        | "path" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source__037_ _sexp__032_
        | "dune" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source__037_ _sexp__032_
        | "dune_project" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source__037_ _sexp__032_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__030_ :: sexp_args__033_) as
       _sexp__032_ ->
       (match atom__030_ with
        | "path" as _tag__041_ ->
          (match sexp_args__033_ with
           | arg0__042_ :: [] ->
             let res0__043_ = Blang.t_of_sexp Path.Predicate.t_of_sexp arg0__042_ in
             `path res0__043_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__037_
               _tag__041_
               _sexp__032_)
        | "dune" as _tag__038_ ->
          (match sexp_args__033_ with
           | arg0__039_ :: [] ->
             let res0__040_ = Blang.t_of_sexp Dune.Predicate.t_of_sexp arg0__039_ in
             `dune res0__040_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__037_
               _tag__038_
               _sexp__032_)
        | "dune_project" as _tag__034_ ->
          (match sexp_args__033_ with
           | arg0__035_ :: [] ->
             let res0__036_ =
               Blang.t_of_sexp Dune_project.Predicate.t_of_sexp arg0__035_
             in
             `dune_project res0__036_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__037_
               _tag__034_
               _sexp__032_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__031_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__037_ sexp__031_
     | Sexplib0.Sexp.List [] as sexp__031_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__037_ sexp__031_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (let error_source__045_ = "lib/dunolint/src/predicate.ml.t" in
     fun sexp__044_ ->
       try __t_of_sexp__ sexp__044_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__045_ sexp__044_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (function
     | `path v__046_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "path"; Blang.sexp_of_t Path.Predicate.sexp_of_t v__046_ ]
     | `dune v__047_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "dune"; Blang.sexp_of_t Dune.Predicate.sexp_of_t v__047_ ]
     | `dune_project v__048_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "dune_project"
         ; Blang.sexp_of_t Dune_project.Predicate.sexp_of_t v__048_
         ]
     : t -> Sexplib0.Sexp.t)
  ;;

  [@@@deriving.end]
end

include T
