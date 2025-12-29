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

module Name = Executable__name
module Public_name = Executable__public_name

module Predicate = struct
  [@@@coverage off]

  let error_source = "executable.t"

  type t =
    [ `has_field of [ `instrumentation | `lint | `name | `preprocess | `public_name ]
    | `instrumentation of Instrumentation.Predicate.t Blang.t
    | `lint of Lint.Predicate.t Blang.t
    | `name of Name.Predicate.t Blang.t
    | `preprocess of Preprocess.Predicate.t Blang.t
    | `public_name of Public_name.Predicate.t Blang.t
    ]

  let equal_has_field
        (va : [ `instrumentation | `lint | `name | `preprocess | `public_name ])
        (vb : [ `instrumentation | `lint | `name | `preprocess | `public_name ])
    =
    if Stdlib.( == ) va vb
    then true
    else (
      match va, vb with
      | `instrumentation, `instrumentation -> true
      | `lint, `lint -> true
      | `name, `name -> true
      | `preprocess, `preprocess -> true
      | `public_name, `public_name -> true
      | (`instrumentation | `lint | `name | `preprocess | `public_name), _ -> false)
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
      | `name va, `name vb -> Blang.equal Name.Predicate.equal va vb
      | `preprocess va, `preprocess vb -> Blang.equal Preprocess.Predicate.equal va vb
      | `public_name va, `public_name vb -> Blang.equal Public_name.Predicate.equal va vb
      | ( ( `has_field _
          | `instrumentation _
          | `lint _
          | `name _
          | `preprocess _
          | `public_name _ )
        , _ ) -> false)
  ;;

  let __t_of_sexp__ =
    (function
     | Sexplib0.Sexp.Atom atom__050_ as _sexp__052_ ->
       (match atom__050_ with
        | "has_field" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__052_
        | "instrumentation" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__052_
        | "lint" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__052_
        | "name" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__052_
        | "preprocess" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__052_
        | "public_name" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__052_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__050_ :: sexp_args__053_) as
       _sexp__052_ ->
       (match atom__050_ with
        | "has_field" as _tag__070_ ->
          (match sexp_args__053_ with
           | arg0__077_ :: [] ->
             let res0__078_ =
               let sexp__076_ = arg0__077_ in
               try
                 match sexp__076_ with
                 | Sexplib0.Sexp.Atom atom__072_ as _sexp__074_ ->
                   (match atom__072_ with
                    | "instrumentation" -> `instrumentation
                    | "lint" -> `lint
                    | "name" -> `name
                    | "preprocess" -> `preprocess
                    | "public_name" -> `public_name
                    | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                 | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__072_ :: _) as _sexp__074_
                   ->
                   (match atom__072_ with
                    | "instrumentation" ->
                      Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__074_
                    | "lint" ->
                      Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__074_
                    | "name" ->
                      Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__074_
                    | "preprocess" ->
                      Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__074_
                    | "public_name" ->
                      Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__074_
                    | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
                 | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__073_ ->
                   Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                     error_source
                     sexp__073_
                 | Sexplib0.Sexp.List [] as sexp__073_ ->
                   Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                     error_source
                     sexp__073_
               with
               | Sexplib0.Sexp_conv_error.No_variant_match ->
                 Sexplib0.Sexp_conv_error.no_matching_variant_found
                   error_source
                   sexp__076_
             in
             `has_field res0__078_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__070_
               _sexp__052_)
        | "instrumentation" as _tag__067_ ->
          (match sexp_args__053_ with
           | arg0__068_ :: [] ->
             let res0__069_ =
               Blang.t_of_sexp Instrumentation.Predicate.t_of_sexp arg0__068_
             in
             `instrumentation res0__069_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__067_
               _sexp__052_)
        | "lint" as _tag__064_ ->
          (match sexp_args__053_ with
           | arg0__065_ :: [] ->
             let res0__066_ = Blang.t_of_sexp Lint.Predicate.t_of_sexp arg0__065_ in
             `lint res0__066_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__064_
               _sexp__052_)
        | "name" as _tag__061_ ->
          (match sexp_args__053_ with
           | arg0__062_ :: [] ->
             let res0__063_ = Blang.t_of_sexp Name.Predicate.t_of_sexp arg0__062_ in
             `name res0__063_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__061_
               _sexp__052_)
        | "preprocess" as _tag__058_ ->
          (match sexp_args__053_ with
           | arg0__059_ :: [] ->
             let res0__060_ = Blang.t_of_sexp Preprocess.Predicate.t_of_sexp arg0__059_ in
             `preprocess res0__060_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__058_
               _sexp__052_)
        | "public_name" as _tag__054_ ->
          (match sexp_args__053_ with
           | arg0__055_ :: [] ->
             let res0__056_ =
               Blang.t_of_sexp Public_name.Predicate.t_of_sexp arg0__055_
             in
             `public_name res0__056_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source
               _tag__054_
               _sexp__052_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__051_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source sexp__051_
     | Sexplib0.Sexp.List [] as sexp__051_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source sexp__051_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (fun sexp__079_ ->
       try __t_of_sexp__ sexp__079_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp__079_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (function
     | `has_field v__081_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "has_field"
         ; (match v__081_ with
            | `instrumentation -> Sexplib0.Sexp.Atom "instrumentation"
            | `lint -> Sexplib0.Sexp.Atom "lint"
            | `name -> Sexplib0.Sexp.Atom "name"
            | `preprocess -> Sexplib0.Sexp.Atom "preprocess"
            | `public_name -> Sexplib0.Sexp.Atom "public_name")
         ]
     | `instrumentation v__082_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "instrumentation"
         ; Blang.sexp_of_t Instrumentation.Predicate.sexp_of_t v__082_
         ]
     | `lint v__083_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "lint"; Blang.sexp_of_t Lint.Predicate.sexp_of_t v__083_ ]
     | `name v__084_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "name"; Blang.sexp_of_t Name.Predicate.sexp_of_t v__084_ ]
     | `preprocess v__085_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "preprocess"
         ; Blang.sexp_of_t Preprocess.Predicate.sexp_of_t v__085_
         ]
     | `public_name v__086_ ->
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "public_name"
         ; Blang.sexp_of_t Public_name.Predicate.sexp_of_t v__086_
         ]
     : t -> Sexplib0.Sexp.t)
  ;;
end
