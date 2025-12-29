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

[@@@coverage off]

let error_source = "predicate.t"

type t =
  [ `executable of Executable.Predicate.t Blang.t
  | `has_field of [ `instrumentation | `lint | `name | `preprocess | `public_name ]
  | `include_subdirs of Include_subdirs.Predicate.t Blang.t
  | `instrumentation of Instrumentation.Predicate.t Blang.t
  | `library of Library.Predicate.t Blang.t
  | `lint of Lint.Predicate.t Blang.t
  | `preprocess of Preprocess.Predicate.t Blang.t
  | `stanza of Stanza.Predicate.t Blang.t
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
    | `executable va, `executable vb -> Blang.equal Executable.Predicate.equal va vb
    | `has_field va, `has_field vb -> equal_has_field va vb
    | `include_subdirs va, `include_subdirs vb ->
      Blang.equal Include_subdirs.Predicate.equal va vb
    | `instrumentation va, `instrumentation vb ->
      Blang.equal Instrumentation.Predicate.equal va vb
    | `library va, `library vb -> Blang.equal Library.Predicate.equal va vb
    | `lint va, `lint vb -> Blang.equal Lint.Predicate.equal va vb
    | `preprocess va, `preprocess vb -> Blang.equal Preprocess.Predicate.equal va vb
    | `stanza va, `stanza vb -> Blang.equal Stanza.Predicate.equal va vb
    | ( ( `executable _
        | `has_field _
        | `include_subdirs _
        | `instrumentation _
        | `library _
        | `lint _
        | `preprocess _
        | `stanza _ )
      , _ ) -> false)
;;

let __t_of_sexp__ =
  (function
   | Sexplib0.Sexp.Atom atom__066_ as _sexp__068_ ->
     (match atom__066_ with
      | "executable" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__068_
      | "has_field" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__068_
      | "include_subdirs" ->
        Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__068_
      | "instrumentation" ->
        Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__068_
      | "library" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__068_
      | "lint" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__068_
      | "preprocess" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__068_
      | "stanza" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__068_
      | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
   | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__066_ :: sexp_args__069_) as _sexp__068_
     ->
     (match atom__066_ with
      | "executable" as _tag__098_ ->
        (match sexp_args__069_ with
         | arg0__099_ :: [] ->
           let res0__100_ = Blang.t_of_sexp Executable.Predicate.t_of_sexp arg0__099_ in
           `executable res0__100_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source
             _tag__098_
             _sexp__068_)
      | "has_field" as _tag__089_ ->
        (match sexp_args__069_ with
         | arg0__096_ :: [] ->
           let res0__097_ =
             let sexp__095_ = arg0__096_ in
             try
               match sexp__095_ with
               | Sexplib0.Sexp.Atom atom__091_ as _sexp__093_ ->
                 (match atom__091_ with
                  | "instrumentation" -> `instrumentation
                  | "lint" -> `lint
                  | "name" -> `name
                  | "preprocess" -> `preprocess
                  | "public_name" -> `public_name
                  | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
               | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__091_ :: _) as _sexp__093_ ->
                 (match atom__091_ with
                  | "instrumentation" ->
                    Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__093_
                  | "lint" ->
                    Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__093_
                  | "name" ->
                    Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__093_
                  | "preprocess" ->
                    Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__093_
                  | "public_name" ->
                    Sexplib0.Sexp_conv_error.ptag_no_args error_source _sexp__093_
                  | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
               | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__092_ ->
                 Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                   error_source
                   sexp__092_
               | Sexplib0.Sexp.List [] as sexp__092_ ->
                 Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                   error_source
                   sexp__092_
             with
             | Sexplib0.Sexp_conv_error.No_variant_match ->
               Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp__095_
           in
           `has_field res0__097_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source
             _tag__089_
             _sexp__068_)
      | "include_subdirs" as _tag__086_ ->
        (match sexp_args__069_ with
         | arg0__087_ :: [] ->
           let res0__088_ =
             Blang.t_of_sexp Include_subdirs.Predicate.t_of_sexp arg0__087_
           in
           `include_subdirs res0__088_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source
             _tag__086_
             _sexp__068_)
      | "instrumentation" as _tag__083_ ->
        (match sexp_args__069_ with
         | arg0__084_ :: [] ->
           let res0__085_ =
             Blang.t_of_sexp Instrumentation.Predicate.t_of_sexp arg0__084_
           in
           `instrumentation res0__085_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source
             _tag__083_
             _sexp__068_)
      | "library" as _tag__080_ ->
        (match sexp_args__069_ with
         | arg0__081_ :: [] ->
           let res0__082_ = Blang.t_of_sexp Library.Predicate.t_of_sexp arg0__081_ in
           `library res0__082_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source
             _tag__080_
             _sexp__068_)
      | "lint" as _tag__077_ ->
        (match sexp_args__069_ with
         | arg0__078_ :: [] ->
           let res0__079_ = Blang.t_of_sexp Lint.Predicate.t_of_sexp arg0__078_ in
           `lint res0__079_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source
             _tag__077_
             _sexp__068_)
      | "preprocess" as _tag__074_ ->
        (match sexp_args__069_ with
         | arg0__075_ :: [] ->
           let res0__076_ = Blang.t_of_sexp Preprocess.Predicate.t_of_sexp arg0__075_ in
           `preprocess res0__076_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source
             _tag__074_
             _sexp__068_)
      | "stanza" as _tag__070_ ->
        (match sexp_args__069_ with
         | arg0__071_ :: [] ->
           let res0__072_ = Blang.t_of_sexp Stanza.Predicate.t_of_sexp arg0__071_ in
           `stanza res0__072_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source
             _tag__070_
             _sexp__068_)
      | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
   | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__067_ ->
     Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source sexp__067_
   | Sexplib0.Sexp.List [] as sexp__067_ ->
     Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source sexp__067_
   : Sexplib0.Sexp.t -> t)
;;

let t_of_sexp =
  (fun sexp__101_ ->
     try __t_of_sexp__ sexp__101_ with
     | Sexplib0.Sexp_conv_error.No_variant_match ->
       Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp__101_
   : Sexplib0.Sexp.t -> t)
;;

let sexp_of_t =
  (function
   | `executable v__103_ ->
     Sexplib0.Sexp.List
       [ Sexplib0.Sexp.Atom "executable"
       ; Blang.sexp_of_t Executable.Predicate.sexp_of_t v__103_
       ]
   | `has_field v__104_ ->
     Sexplib0.Sexp.List
       [ Sexplib0.Sexp.Atom "has_field"
       ; (match v__104_ with
          | `instrumentation -> Sexplib0.Sexp.Atom "instrumentation"
          | `lint -> Sexplib0.Sexp.Atom "lint"
          | `name -> Sexplib0.Sexp.Atom "name"
          | `preprocess -> Sexplib0.Sexp.Atom "preprocess"
          | `public_name -> Sexplib0.Sexp.Atom "public_name")
       ]
   | `include_subdirs v__105_ ->
     Sexplib0.Sexp.List
       [ Sexplib0.Sexp.Atom "include_subdirs"
       ; Blang.sexp_of_t Include_subdirs.Predicate.sexp_of_t v__105_
       ]
   | `instrumentation v__106_ ->
     Sexplib0.Sexp.List
       [ Sexplib0.Sexp.Atom "instrumentation"
       ; Blang.sexp_of_t Instrumentation.Predicate.sexp_of_t v__106_
       ]
   | `library v__107_ ->
     Sexplib0.Sexp.List
       [ Sexplib0.Sexp.Atom "library"
       ; Blang.sexp_of_t Library.Predicate.sexp_of_t v__107_
       ]
   | `lint v__108_ ->
     Sexplib0.Sexp.List
       [ Sexplib0.Sexp.Atom "lint"; Blang.sexp_of_t Lint.Predicate.sexp_of_t v__108_ ]
   | `preprocess v__109_ ->
     Sexplib0.Sexp.List
       [ Sexplib0.Sexp.Atom "preprocess"
       ; Blang.sexp_of_t Preprocess.Predicate.sexp_of_t v__109_
       ]
   | `stanza v__110_ ->
     Sexplib0.Sexp.List
       [ Sexplib0.Sexp.Atom "stanza"; Blang.sexp_of_t Stanza.Predicate.sexp_of_t v__110_ ]
   : t -> Sexplib0.Sexp.t)
;;
