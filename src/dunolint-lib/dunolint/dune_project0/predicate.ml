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
  [ `dune_lang_version of Dune_lang_version.Predicate.t Blang.t
  | `generate_opam_files of Generate_opam_files.Predicate.t Blang.t
  | `implicit_transitive_deps of Implicit_transitive_deps.Predicate.t Blang.t
  | `name of Name.Predicate.t Blang.t
  ]

let equal (a : t) (b : t) =
  if Stdlib.( == ) a b
  then true
  else (
    match a, b with
    | `dune_lang_version va, `dune_lang_version vb ->
      Blang.equal Dune_lang_version.Predicate.equal va vb
    | `generate_opam_files va, `generate_opam_files vb ->
      Blang.equal Generate_opam_files.Predicate.equal va vb
    | `implicit_transitive_deps va, `implicit_transitive_deps vb ->
      Blang.equal Implicit_transitive_deps.Predicate.equal va vb
    | `name va, `name vb -> Blang.equal Name.Predicate.equal va vb
    | ( ( `dune_lang_version _
        | `generate_opam_files _
        | `implicit_transitive_deps _
        | `name _ )
      , _ ) -> false)
;;

let __t_of_sexp__ =
  (function
   | Sexplib0.Sexp.Atom atom__038_ as _sexp__040_ ->
     (match atom__038_ with
      | "dune_lang_version" ->
        Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__040_
      | "generate_opam_files" ->
        Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__040_
      | "implicit_transitive_deps" ->
        Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__040_
      | "name" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__040_
      | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
   | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__038_ :: sexp_args__041_) as _sexp__040_
     ->
     (match atom__038_ with
      | "dune_lang_version" as _tag__052_ ->
        (match sexp_args__041_ with
         | arg0__053_ :: [] ->
           let res0__054_ =
             Blang.t_of_sexp Dune_lang_version.Predicate.t_of_sexp arg0__053_
           in
           `dune_lang_version res0__054_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source
             _tag__052_
             _sexp__040_)
      | "generate_opam_files" as _tag__049_ ->
        (match sexp_args__041_ with
         | arg0__050_ :: [] ->
           let res0__051_ =
             Blang.t_of_sexp Generate_opam_files.Predicate.t_of_sexp arg0__050_
           in
           `generate_opam_files res0__051_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source
             _tag__049_
             _sexp__040_)
      | "implicit_transitive_deps" as _tag__046_ ->
        (match sexp_args__041_ with
         | arg0__047_ :: [] ->
           let res0__048_ =
             Blang.t_of_sexp Implicit_transitive_deps.Predicate.t_of_sexp arg0__047_
           in
           `implicit_transitive_deps res0__048_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source
             _tag__046_
             _sexp__040_)
      | "name" as _tag__042_ ->
        (match sexp_args__041_ with
         | arg0__043_ :: [] ->
           let res0__044_ = Blang.t_of_sexp Name.Predicate.t_of_sexp arg0__043_ in
           `name res0__044_
         | _ ->
           Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
             error_source
             _tag__042_
             _sexp__040_)
      | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
   | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__039_ ->
     Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source sexp__039_
   | Sexplib0.Sexp.List [] as sexp__039_ ->
     Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source sexp__039_
   : Sexplib0.Sexp.t -> t)
;;

let t_of_sexp =
  (fun sexp__055_ ->
     try __t_of_sexp__ sexp__055_ with
     | Sexplib0.Sexp_conv_error.No_variant_match ->
       Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp__055_
   : Sexplib0.Sexp.t -> t)
;;

let sexp_of_t =
  (function
   | `dune_lang_version v__057_ ->
     Sexplib0.Sexp.List
       [ Sexplib0.Sexp.Atom "dune_lang_version"
       ; Blang.sexp_of_t Dune_lang_version.Predicate.sexp_of_t v__057_
       ]
   | `generate_opam_files v__058_ ->
     Sexplib0.Sexp.List
       [ Sexplib0.Sexp.Atom "generate_opam_files"
       ; Blang.sexp_of_t Generate_opam_files.Predicate.sexp_of_t v__058_
       ]
   | `implicit_transitive_deps v__059_ ->
     Sexplib0.Sexp.List
       [ Sexplib0.Sexp.Atom "implicit_transitive_deps"
       ; Blang.sexp_of_t Implicit_transitive_deps.Predicate.sexp_of_t v__059_
       ]
   | `name v__060_ ->
     Sexplib0.Sexp.List
       [ Sexplib0.Sexp.Atom "name"; Blang.sexp_of_t Name.Predicate.sexp_of_t v__060_ ]
   : t -> Sexplib0.Sexp.t)
;;
