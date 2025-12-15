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

  let error_source = "path.t"

  type t = [ `glob of Glob.t ]

  let compare =
    (fun a__001_ ->
       fun b__002_ ->
       if Stdlib.( == ) a__001_ b__002_
       then 0
       else (
         match a__001_, b__002_ with
         | `glob _left__005_, `glob _right__006_ -> Glob.compare _left__005_ _right__006_)
     : t -> t -> int)
  ;;

  let equal =
    (fun a__007_ ->
       fun b__008_ ->
       if Stdlib.( == ) a__007_ b__008_
       then true
       else (
         match a__007_, b__008_ with
         | `glob _left__011_, `glob _right__012_ -> Glob.equal _left__011_ _right__012_)
     : t -> t -> bool)
  ;;

  let __t_of_sexp__ =
    (function
     | Sexplib0.Sexp.Atom atom__014_ as _sexp__016_ ->
       (match atom__014_ with
        | "equals" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__016_
        | "glob" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__016_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__014_ :: sexp_args__017_) as
       _sexp__016_ ->
       (match atom__014_ with
        | "equals" as _tag__022_ ->
          (match sexp_args__017_ with
           | _ :: [] ->
             Sexplib0.Sexp_conv.of_sexp_error
               "The [path.equals] construct is no longer supported."
               _sexp__016_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__022_
               _sexp__016_)
        | "glob" as _tag__018_ ->
          (match sexp_args__017_ with
           | arg0__019_ :: [] ->
             let res0__020_ = Glob.t_of_sexp arg0__019_ in
             `glob res0__020_
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
    (fun sexp__025_ ->
       try __t_of_sexp__ sexp__025_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp__025_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (function
     | `glob v__028_ ->
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "glob"; Glob.sexp_of_t v__028_ ]
     : t -> Sexplib0.Sexp.t)
  ;;
end
