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
include String_container_key

let invariant t =
  (not (String.is_empty t))
  && String.for_all t ~f:(fun c ->
    Char.is_alphanum c || Char.equal c '_' || Char.equal c '-' || Char.equal c '.')
;;

include Validated_string.Make (struct
    let module_name = "Dunolint.Library.Public_name"
    let invariant = invariant
  end)

module Predicate = struct
  [@@@coverage off]

  type name = t

  let equal_name = (equal : name -> name -> bool)
  let name_of_sexp = (t_of_sexp : Sexplib0.Sexp.t -> name)
  let sexp_of_name = (sexp_of_t : name -> Sexplib0.Sexp.t)
  let error_source = "library.public_name.t"

  type t =
    [ `equals of name
    | `is_prefix of string
    | `is_suffix of string
    ]

  let equal =
    (fun a__014_ ->
       fun b__015_ ->
       if Stdlib.( == ) a__014_ b__015_
       then true
       else (
         match a__014_, b__015_ with
         | `equals _left__016_, `equals _right__017_ ->
           equal_name _left__016_ _right__017_
         | `is_prefix _left__018_, `is_prefix _right__019_ ->
           equal_string _left__018_ _right__019_
         | `is_suffix _left__020_, `is_suffix _right__021_ ->
           equal_string _left__020_ _right__021_
         | x, y -> Stdlib.( = ) x y)
     : t -> t -> bool)
  ;;

  let __t_of_sexp__ =
    (function
     | Sexplib0.Sexp.Atom atom__023_ as _sexp__025_ ->
       (match atom__023_ with
        | "equals" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__025_
        | "is_prefix" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__025_
        | "is_suffix" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__025_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__023_ :: sexp_args__026_) as
       _sexp__025_ ->
       (match atom__023_ with
        | "equals" as _tag__034_ ->
          (match sexp_args__026_ with
           | arg0__035_ :: [] ->
             let res0__036_ = name_of_sexp arg0__035_ in
             `equals res0__036_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__034_
               _sexp__025_)
        | "is_prefix" as _tag__031_ ->
          (match sexp_args__026_ with
           | arg0__032_ :: [] ->
             let res0__033_ = string_of_sexp arg0__032_ in
             `is_prefix res0__033_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__031_
               _sexp__025_)
        | "is_suffix" as _tag__027_ ->
          (match sexp_args__026_ with
           | arg0__028_ :: [] ->
             let res0__029_ = string_of_sexp arg0__028_ in
             `is_suffix res0__029_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__027_
               _sexp__025_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__024_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source sexp__024_
     | Sexplib0.Sexp.List [] as sexp__024_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source sexp__024_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (fun sexp__037_ ->
       try __t_of_sexp__ sexp__037_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp__037_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (function
     | `equals v__039_ ->
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "equals"; sexp_of_name v__039_ ]
     | `is_prefix v__040_ ->
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "is_prefix"; sexp_of_string v__040_ ]
     | `is_suffix v__041_ ->
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "is_suffix"; sexp_of_string v__041_ ]
     : t -> Sexplib0.Sexp.t)
  ;;
end
