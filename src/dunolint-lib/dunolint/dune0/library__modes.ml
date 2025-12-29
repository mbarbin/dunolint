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

module Predicate = struct
  [@@@coverage off]

  let error_source = "library.modes.t"

  type t =
    [ `has_mode of Compilation_mode.t
    | `has_modes of Compilation_mode.t list
    ]

  let equal (a : t) (b : t) =
    if Stdlib.( == ) a b
    then true
    else (
      match a, b with
      | `has_mode va, `has_mode vb -> Compilation_mode.equal va vb
      | `has_modes va, `has_modes vb -> equal_list Compilation_mode.equal va vb
      | (`has_mode _ | `has_modes _), _ -> false)
  ;;

  let __t_of_sexp__ =
    (function
     | Sexplib0.Sexp.Atom atom__018_ as _sexp__020_ ->
       (match atom__018_ with
        | "has_mode" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__020_
        | "has_modes" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__020_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__018_ :: sexp_args__021_) as
       _sexp__020_ ->
       (match atom__018_ with
        | "has_mode" as _tag__026_ ->
          (match sexp_args__021_ with
           | arg0__027_ :: [] ->
             let res0__028_ = Compilation_mode.t_of_sexp arg0__027_ in
             `has_mode res0__028_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__026_
               _sexp__020_)
        | "has_modes" as _tag__022_ ->
          (match sexp_args__021_ with
           | arg0__023_ :: [] ->
             let res0__024_ = list_of_sexp Compilation_mode.t_of_sexp arg0__023_ in
             `has_modes res0__024_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__022_
               _sexp__020_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__019_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source sexp__019_
     | Sexplib0.Sexp.List [] as sexp__019_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source sexp__019_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (fun sexp__029_ ->
       try __t_of_sexp__ sexp__029_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp__029_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (function
     | `has_mode v__031_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "has_mode"; Compilation_mode.sexp_of_t v__031_ ]
     | `has_modes v__032_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "has_modes"
         ; sexp_of_list Compilation_mode.sexp_of_t v__032_
         ]
     : t -> Sexplib0.Sexp.t)
  ;;
end
