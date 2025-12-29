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

module Modes = Library__modes
module Name = Library__name
module Public_name = Library__public_name

module Predicate = struct
  [@@@coverage off]

  let error_source = "library.t"

  type t =
    [ `has_field of
        [ `instrumentation | `lint | `modes | `name | `preprocess | `public_name ]
    | `instrumentation of Instrumentation.Predicate.t Blang.t
    | `lint of Lint.Predicate.t Blang.t
    | `modes of Modes.Predicate.t Blang.t
    | `name of Name.Predicate.t Blang.t
    | `preprocess of Preprocess.Predicate.t Blang.t
    | `public_name of Public_name.Predicate.t Blang.t
    ]

  let equal_has_field
        (va : [ `instrumentation | `lint | `modes | `name | `preprocess | `public_name ])
        (vb : [ `instrumentation | `lint | `modes | `name | `preprocess | `public_name ])
    =
    if Stdlib.( == ) va vb
    then true
    else (
      match va, vb with
      | `instrumentation, `instrumentation -> true
      | `lint, `lint -> true
      | `modes, `modes -> true
      | `name, `name -> true
      | `preprocess, `preprocess -> true
      | `public_name, `public_name -> true
      | (`instrumentation | `lint | `modes | `name | `preprocess | `public_name), _ ->
        false)
  ;;

  let equal (a : t) (b : t) =
    if Stdlib.( == ) a b
    then true
    else (
      match a, b with
      | `has_field va, `has_field vb -> equal_has_field va vb
      | `instrumentation va, `instrumentation vb ->
        Blang.equal Instrumentation.Predicate.equal va vb
      | `lint va, `lint vb -> Blang.equal Lint.Predicate.equal va vb
      | `modes va, `modes vb -> Blang.equal Modes.Predicate.equal va vb
      | `name va, `name vb -> Blang.equal Name.Predicate.equal va vb
      | `preprocess va, `preprocess vb -> Blang.equal Preprocess.Predicate.equal va vb
      | `public_name va, `public_name vb -> Blang.equal Public_name.Predicate.equal va vb
      | ( ( `has_field _
          | `instrumentation _
          | `lint _
          | `modes _
          | `name _
          | `preprocess _
          | `public_name _ )
        , _ ) -> false)
  ;;

  let __t_of_sexp__ =
    (function
     | Sexplib0.Sexp.Atom atom__058_ as _sexp__060_ ->
       (match atom__058_ with
        | "has_field" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__060_
        | "instrumentation" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__060_
        | "lint" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__060_
        | "modes" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__060_
        | "name" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__060_
        | "preprocess" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__060_
        | "public_name" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__060_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__058_ :: sexp_args__061_) as
       _sexp__060_ ->
       (match atom__058_ with
        | "has_field" as _tag__081_ ->
          (match sexp_args__061_ with
           | arg0__088_ :: [] ->
             let res0__089_ =
               let sexp__087_ = arg0__088_ in
               try
                 match sexp__087_ with
                 | Sexplib0.Sexp.Atom atom__083_ as _sexp__085_ ->
                   (match atom__083_ with
                    | "instrumentation" -> `instrumentation
                    | "lint" -> `lint
                    | "modes" -> `modes
                    | "name" -> `name
                    | "preprocess" -> `preprocess
                    | "public_name" -> `public_name
                    | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                 | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__083_ :: _) as _sexp__085_
                   ->
                   (match atom__083_ with
                    | "instrumentation" ->
                      Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__085_
                    | "lint" ->
                      Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__085_
                    | "modes" ->
                      Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__085_
                    | "name" ->
                      Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__085_
                    | "preprocess" ->
                      Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__085_
                    | "public_name" ->
                      Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__085_
                    | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                 | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__084_ ->
                   Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                     error_source
                     sexp__084_
                 | Sexplib0.Sexp.List [] as sexp__084_ ->
                   Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                     error_source
                     sexp__084_
               with
               | Sexplib0.Sexp_conv_error.No_variant_match ->
                 Sexplib0.Sexp_conv_error.no_matching_variant_found
                   error_source
                   sexp__087_
             in
             `has_field res0__089_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__081_
               _sexp__060_)
        | "instrumentation" as _tag__078_ ->
          (match sexp_args__061_ with
           | arg0__079_ :: [] ->
             let res0__080_ =
               Blang.t_of_sexp Instrumentation.Predicate.t_of_sexp arg0__079_
             in
             `instrumentation res0__080_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__078_
               _sexp__060_)
        | "lint" as _tag__075_ ->
          (match sexp_args__061_ with
           | arg0__076_ :: [] ->
             let res0__077_ = Blang.t_of_sexp Lint.Predicate.t_of_sexp arg0__076_ in
             `lint res0__077_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__075_
               _sexp__060_)
        | "modes" as _tag__072_ ->
          (match sexp_args__061_ with
           | arg0__073_ :: [] ->
             let res0__074_ = Blang.t_of_sexp Modes.Predicate.t_of_sexp arg0__073_ in
             `modes res0__074_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__072_
               _sexp__060_)
        | "name" as _tag__069_ ->
          (match sexp_args__061_ with
           | arg0__070_ :: [] ->
             let res0__071_ = Blang.t_of_sexp Name.Predicate.t_of_sexp arg0__070_ in
             `name res0__071_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__069_
               _sexp__060_)
        | "preprocess" as _tag__066_ ->
          (match sexp_args__061_ with
           | arg0__067_ :: [] ->
             let res0__068_ = Blang.t_of_sexp Preprocess.Predicate.t_of_sexp arg0__067_ in
             `preprocess res0__068_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__066_
               _sexp__060_)
        | "public_name" as _tag__062_ ->
          (match sexp_args__061_ with
           | arg0__063_ :: [] ->
             let res0__064_ =
               Blang.t_of_sexp Public_name.Predicate.t_of_sexp arg0__063_
             in
             `public_name res0__064_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__062_
               _sexp__060_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__059_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source sexp__059_
     | Sexplib0.Sexp.List [] as sexp__059_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source sexp__059_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (fun sexp__090_ ->
       try __t_of_sexp__ sexp__090_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp__090_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (function
     | `has_field v__092_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "has_field"
         ; (match v__092_ with
            | `instrumentation -> Sexplib0.Sexp.Atom "instrumentation"
            | `lint -> Sexplib0.Sexp.Atom "lint"
            | `modes -> Sexplib0.Sexp.Atom "modes"
            | `name -> Sexplib0.Sexp.Atom "name"
            | `preprocess -> Sexplib0.Sexp.Atom "preprocess"
            | `public_name -> Sexplib0.Sexp.Atom "public_name")
         ]
     | `instrumentation v__093_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "instrumentation"
         ; Blang.sexp_of_t Instrumentation.Predicate.sexp_of_t v__093_
         ]
     | `lint v__094_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "lint"; Blang.sexp_of_t Lint.Predicate.sexp_of_t v__094_ ]
     | `modes v__095_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "modes"; Blang.sexp_of_t Modes.Predicate.sexp_of_t v__095_ ]
     | `name v__096_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "name"; Blang.sexp_of_t Name.Predicate.sexp_of_t v__096_ ]
     | `preprocess v__097_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "preprocess"
         ; Blang.sexp_of_t Preprocess.Predicate.sexp_of_t v__097_
         ]
     | `public_name v__098_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "public_name"
         ; Blang.sexp_of_t Public_name.Predicate.sexp_of_t v__098_
         ]
     : t -> Sexplib0.Sexp.t)
  ;;
end
