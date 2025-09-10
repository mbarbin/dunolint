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
[@@deriving_inline compare, equal, sexp]

let compare =
  (fun a__001_ ->
     fun b__002_ ->
     if Stdlib.( == ) a__001_ b__002_
     then 0
     else (
       match a__001_, b__002_ with
       | `executable _left__003_, `executable _right__004_ ->
         Blang.compare Executable.Predicate.compare _left__003_ _right__004_
       | `has_field _left__007_, `has_field _right__008_ ->
         if Stdlib.( == ) _left__007_ _right__008_
         then 0
         else (
           match _left__007_, _right__008_ with
           | `instrumentation, `instrumentation -> 0
           | `lint, `lint -> 0
           | `name, `name -> 0
           | `preprocess, `preprocess -> 0
           | `public_name, `public_name -> 0
           | x, y -> Stdlib.compare x y)
       | `include_subdirs _left__009_, `include_subdirs _right__010_ ->
         Blang.compare Include_subdirs.Predicate.compare _left__009_ _right__010_
       | `instrumentation _left__013_, `instrumentation _right__014_ ->
         Blang.compare Instrumentation.Predicate.compare _left__013_ _right__014_
       | `library _left__017_, `library _right__018_ ->
         Blang.compare Library.Predicate.compare _left__017_ _right__018_
       | `lint _left__021_, `lint _right__022_ ->
         Blang.compare Lint.Predicate.compare _left__021_ _right__022_
       | `preprocess _left__025_, `preprocess _right__026_ ->
         Blang.compare Preprocess.Predicate.compare _left__025_ _right__026_
       | `stanza _left__029_, `stanza _right__030_ ->
         Blang.compare Stanza.Predicate.compare _left__029_ _right__030_
       | x, y -> Stdlib.compare x y)
   : t -> t -> int)
;;

let equal =
  (fun a__033_ ->
     fun b__034_ ->
     if Stdlib.( == ) a__033_ b__034_
     then true
     else (
       match a__033_, b__034_ with
       | `executable _left__035_, `executable _right__036_ ->
         Blang.equal Executable.Predicate.equal _left__035_ _right__036_
       | `has_field _left__039_, `has_field _right__040_ ->
         if Stdlib.( == ) _left__039_ _right__040_
         then true
         else (
           match _left__039_, _right__040_ with
           | `instrumentation, `instrumentation -> true
           | `lint, `lint -> true
           | `name, `name -> true
           | `preprocess, `preprocess -> true
           | `public_name, `public_name -> true
           | x, y -> Stdlib.( = ) x y)
       | `include_subdirs _left__041_, `include_subdirs _right__042_ ->
         Blang.equal Include_subdirs.Predicate.equal _left__041_ _right__042_
       | `instrumentation _left__045_, `instrumentation _right__046_ ->
         Blang.equal Instrumentation.Predicate.equal _left__045_ _right__046_
       | `library _left__049_, `library _right__050_ ->
         Blang.equal Library.Predicate.equal _left__049_ _right__050_
       | `lint _left__053_, `lint _right__054_ ->
         Blang.equal Lint.Predicate.equal _left__053_ _right__054_
       | `preprocess _left__057_, `preprocess _right__058_ ->
         Blang.equal Preprocess.Predicate.equal _left__057_ _right__058_
       | `stanza _left__061_, `stanza _right__062_ ->
         Blang.equal Stanza.Predicate.equal _left__061_ _right__062_
       | x, y -> Stdlib.( = ) x y)
   : t -> t -> bool)
;;

let __t_of_sexp__ =
  (let error_source__073_ = "lib/dunolint/src/dune0/predicate.ml.t" in
   function
   | Sexplib0.Sexp.Atom atom__066_ as _sexp__068_ ->
     (match atom__066_ with
      | "executable" ->
        Sexplib0.Sexp_conv_error.ptag_takes_args error_source__073_ _sexp__068_
      | "has_field" ->
        Sexplib0.Sexp_conv_error.ptag_takes_args error_source__073_ _sexp__068_
      | "include_subdirs" ->
        Sexplib0.Sexp_conv_error.ptag_takes_args error_source__073_ _sexp__068_
      | "instrumentation" ->
        Sexplib0.Sexp_conv_error.ptag_takes_args error_source__073_ _sexp__068_
      | "library" ->
        Sexplib0.Sexp_conv_error.ptag_takes_args error_source__073_ _sexp__068_
      | "lint" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source__073_ _sexp__068_
      | "preprocess" ->
        Sexplib0.Sexp_conv_error.ptag_takes_args error_source__073_ _sexp__068_
      | "stanza" ->
        Sexplib0.Sexp_conv_error.ptag_takes_args error_source__073_ _sexp__068_
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
             error_source__073_
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
                    Sexplib0.Sexp_conv_error.ptag_no_args error_source__073_ _sexp__093_
                  | "lint" ->
                    Sexplib0.Sexp_conv_error.ptag_no_args error_source__073_ _sexp__093_
                  | "name" ->
                    Sexplib0.Sexp_conv_error.ptag_no_args error_source__073_ _sexp__093_
                  | "preprocess" ->
                    Sexplib0.Sexp_conv_error.ptag_no_args error_source__073_ _sexp__093_
                  | "public_name" ->
                    Sexplib0.Sexp_conv_error.ptag_no_args error_source__073_ _sexp__093_
                  | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
               | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__092_ ->
                 Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
                   error_source__073_
                   sexp__092_
               | Sexplib0.Sexp.List [] as sexp__092_ ->
                 Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
                   error_source__073_
                   sexp__092_
             with
             | Sexplib0.Sexp_conv_error.No_variant_match ->
               Sexplib0.Sexp_conv_error.no_matching_variant_found
                 error_source__073_
                 sexp__095_
           in
           `has_field res0__097_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source__073_
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
             error_source__073_
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
             error_source__073_
             _tag__083_
             _sexp__068_)
      | "library" as _tag__080_ ->
        (match sexp_args__069_ with
         | arg0__081_ :: [] ->
           let res0__082_ = Blang.t_of_sexp Library.Predicate.t_of_sexp arg0__081_ in
           `library res0__082_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source__073_
             _tag__080_
             _sexp__068_)
      | "lint" as _tag__077_ ->
        (match sexp_args__069_ with
         | arg0__078_ :: [] ->
           let res0__079_ = Blang.t_of_sexp Lint.Predicate.t_of_sexp arg0__078_ in
           `lint res0__079_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source__073_
             _tag__077_
             _sexp__068_)
      | "preprocess" as _tag__074_ ->
        (match sexp_args__069_ with
         | arg0__075_ :: [] ->
           let res0__076_ = Blang.t_of_sexp Preprocess.Predicate.t_of_sexp arg0__075_ in
           `preprocess res0__076_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source__073_
             _tag__074_
             _sexp__068_)
      | "stanza" as _tag__070_ ->
        (match sexp_args__069_ with
         | arg0__071_ :: [] ->
           let res0__072_ = Blang.t_of_sexp Stanza.Predicate.t_of_sexp arg0__071_ in
           `stanza res0__072_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source__073_
             _tag__070_
             _sexp__068_)
      | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
   | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__067_ ->
     Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__073_ sexp__067_
   | Sexplib0.Sexp.List [] as sexp__067_ ->
     Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__073_ sexp__067_
   : Sexplib0.Sexp.t -> t)
;;

let t_of_sexp =
  (let error_source__102_ = "lib/dunolint/src/dune0/predicate.ml.t" in
   fun sexp__101_ ->
     try __t_of_sexp__ sexp__101_ with
     | Sexplib0.Sexp_conv_error.No_variant_match ->
       Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__102_ sexp__101_
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

[@@@deriving.end]
