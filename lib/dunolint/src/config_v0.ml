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

module Skip_subtree = struct
  [@@@coverage off]

  module Predicate = struct
    type t = [ `path of Path.Predicate.t Blang.t ]
    [@@deriving_inline compare, equal, sexp]

    let compare =
      (fun a__001_ ->
         fun b__002_ ->
         if Stdlib.( == ) a__001_ b__002_
         then 0
         else (
           match a__001_, b__002_ with
           | `path _left__003_, `path _right__004_ ->
             Blang.compare Path.Predicate.compare _left__003_ _right__004_)
       : t -> t -> int)
    ;;

    let equal =
      (fun a__007_ ->
         fun b__008_ ->
         if Stdlib.( == ) a__007_ b__008_
         then true
         else (
           match a__007_, b__008_ with
           | `path _left__009_, `path _right__010_ ->
             Blang.equal Path.Predicate.equal _left__009_ _right__010_)
       : t -> t -> bool)
    ;;

    let __t_of_sexp__ =
      (let error_source__021_ =
         "lib/dunolint/src/config_v0.ml.Skip_subtree.Predicate.t"
       in
       function
       | Sexplib0.Sexp.Atom atom__014_ as _sexp__016_ ->
         (match atom__014_ with
          | "path" ->
            Sexplib0.Sexp_conv_error.ptag_takes_args error_source__021_ _sexp__016_
          | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
       | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__014_ :: sexp_args__017_) as
         _sexp__016_ ->
         (match atom__014_ with
          | "path" as _tag__018_ ->
            (match sexp_args__017_ with
             | arg0__019_ :: [] ->
               let res0__020_ = Blang.t_of_sexp Path.Predicate.t_of_sexp arg0__019_ in
               `path res0__020_
             | _ ->
               Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                 error_source__021_
                 _tag__018_
                 _sexp__016_)
          | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__015_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
           error_source__021_
           sexp__015_
       | Sexplib0.Sexp.List [] as sexp__015_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
           error_source__021_
           sexp__015_
       : Sexplib0.Sexp.t -> t)
    ;;

    let t_of_sexp =
      (let error_source__023_ =
         "lib/dunolint/src/config_v0.ml.Skip_subtree.Predicate.t"
       in
       fun sexp__022_ ->
         try __t_of_sexp__ sexp__022_ with
         | Sexplib0.Sexp_conv_error.No_variant_match ->
           Sexplib0.Sexp_conv_error.no_matching_variant_found
             error_source__023_
             sexp__022_
       : Sexplib0.Sexp.t -> t)
    ;;

    let sexp_of_t =
      (fun (`path v__024_) ->
         Sexplib0.Sexp.List
           [ Sexplib0.Sexp.Atom "path"; Blang.sexp_of_t Path.Predicate.sexp_of_t v__024_ ]
       : t -> Sexplib0.Sexp.t)
    ;;

    [@@@deriving.end]
  end

  module Result = struct
    type t = | [@@deriving_inline compare, equal, sexp]

    let compare = (Stdlib.compare : t -> t -> int)
    let equal = (Stdlib.( = ) : t -> t -> bool)

    let t_of_sexp =
      (let error_source__031_ = "lib/dunolint/src/config_v0.ml.Skip_subtree.Result.t" in
       function
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__030_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__031_ sexp__030_
       | Sexplib0.Sexp.List [] as sexp__030_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__031_ sexp__030_
       | sexp__030_ ->
         Sexplib0.Sexp_conv_error.unexpected_stag error_source__031_ sexp__030_
       : Sexplib0.Sexp.t -> t)
    ;;

    let sexp_of_t =
      (function
       | _ -> .
       : t -> Sexplib0.Sexp.t)
    ;;

    [@@@deriving.end]
  end

  type t = (Predicate.t, Result.t) Rule.t [@@deriving_inline compare, equal, sexp]

  let compare =
    (fun a__033_ ->
       fun b__034_ -> Rule.compare Predicate.compare Result.compare a__033_ b__034_
     : t -> t -> int)
  ;;

  let equal =
    (fun a__039_ -> fun b__040_ -> Rule.equal Predicate.equal Result.equal a__039_ b__040_
     : t -> t -> bool)
  ;;

  let t_of_sexp =
    (fun x__046_ -> Rule.t_of_sexp Predicate.t_of_sexp Result.t_of_sexp x__046_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (fun x__047_ -> Rule.sexp_of_t Predicate.sexp_of_t Result.sexp_of_t x__047_
     : t -> Sexplib0.Sexp.t)
  ;;

  [@@@deriving.end]
end

module Rule = struct
  [@@@coverage off]

  type t = (Predicate.t, Condition.t) Rule.t [@@deriving_inline compare, equal, sexp]

  let compare =
    (fun a__048_ ->
       fun b__049_ -> Rule.compare Predicate.compare Condition.compare a__048_ b__049_
     : t -> t -> int)
  ;;

  let equal =
    (fun a__054_ ->
       fun b__055_ -> Rule.equal Predicate.equal Condition.equal a__054_ b__055_
     : t -> t -> bool)
  ;;

  let t_of_sexp =
    (fun x__061_ -> Rule.t_of_sexp Predicate.t_of_sexp Condition.t_of_sexp x__061_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (fun x__062_ -> Rule.sexp_of_t Predicate.sexp_of_t Condition.sexp_of_t x__062_
     : t -> Sexplib0.Sexp.t)
  ;;

  [@@@deriving.end]
end

module T = struct
  [@@@coverage off]

  type t =
    { skip_subtree : Skip_subtree.t option [@sexp.option]
    ; rules : Rule.t list
    }
  [@@deriving_inline compare, equal, sexp]

  let compare =
    (fun a__063_ ->
       fun b__064_ ->
       if Stdlib.( == ) a__063_ b__064_
       then 0
       else (
         match
           compare_option Skip_subtree.compare a__063_.skip_subtree b__064_.skip_subtree
         with
         | 0 -> compare_list Rule.compare a__063_.rules b__064_.rules
         | n -> n)
     : t -> t -> int)
  ;;

  let equal =
    (fun a__069_ ->
       fun b__070_ ->
       if Stdlib.( == ) a__069_ b__070_
       then true
       else
         Stdlib.( && )
           (equal_option Skip_subtree.equal a__069_.skip_subtree b__070_.skip_subtree)
           (equal_list Rule.equal a__069_.rules b__070_.rules)
     : t -> t -> bool)
  ;;

  let t_of_sexp =
    (let error_source__076_ = "lib/dunolint/src/config_v0.ml.T.t" in
     fun x__077_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source__076_
         ~fields:
           (Field
              { name = "skip_subtree"
              ; kind = Sexp_option
              ; conv = Skip_subtree.t_of_sexp
              ; rest =
                  Field
                    { name = "rules"
                    ; kind = Required
                    ; conv = list_of_sexp Rule.t_of_sexp
                    ; rest = Empty
                    }
              })
         ~index_of_field:(function
           | "skip_subtree" -> 0
           | "rules" -> 1
           | _ -> -1)
         ~allow_extra_fields:false
         ~create:(fun (skip_subtree, (rules, ())) -> ({ skip_subtree; rules } : t))
         x__077_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (fun { skip_subtree = skip_subtree__079_; rules = rules__083_ } ->
       let bnds__078_ = ([] : _ Stdlib.List.t) in
       let bnds__078_ =
         let arg__084_ = sexp_of_list Rule.sexp_of_t rules__083_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "rules"; arg__084_ ] :: bnds__078_
          : _ Stdlib.List.t)
       in
       let bnds__078_ =
         match skip_subtree__079_ with
         | Stdlib.Option.None -> bnds__078_
         | Stdlib.Option.Some v__080_ ->
           let arg__082_ = Skip_subtree.sexp_of_t v__080_ in
           let bnd__081_ =
             Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "skip_subtree"; arg__082_ ]
           in
           (bnd__081_ :: bnds__078_ : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__078_
     : t -> Sexplib0.Sexp.t)
  ;;

  [@@@deriving.end]
end

include T

let skip_subtree t = t.skip_subtree
let rules t = t.rules
let create ?skip_subtree ?(rules = []) () = { skip_subtree; rules }

module Std = struct
  module Blang = Blang
  module Dune = Dune
  module Dune_project = Dune_project
  include Blang.O

  let backend p = Blang.base (`backend p)
  let cond clauses = `cond clauses
  let dune p = Blang.base (`dune p)
  let dune_lang_version p = Blang.base (`dune_lang_version p)
  let dune_project p = Blang.base (`dune_project p)
  let enforce p = `enforce p
  let equals p = Blang.base (`equals p)
  let executable p = Blang.base (`executable p)
  let flag p = Blang.base (`flag p)
  let generate_opam_files p = Blang.base (`generate_opam_files p)
  let glob p = Blang.base (`glob (Glob.v p))
  let greater_than_or_equal_to p = Blang.base (`greater_than_or_equal_to p)
  let has_field p = Blang.base (`has_field p)
  let has_mode p = Blang.base (`has_mode p)
  let has_modes p = Blang.base (`has_modes p)
  let implicit_transitive_deps p = Blang.base (`implicit_transitive_deps p)
  let include_subdirs p = Blang.base (`include_subdirs p)
  let instrumentation p = Blang.base (`instrumentation p)
  let is_prefix p = Blang.base (`is_prefix p)
  let is_present = Blang.base `is_present
  let is_suffix p = Blang.base (`is_suffix p)
  let less_than_or_equal_to p = Blang.base (`less_than_or_equal_to p)
  let library p = Blang.base (`library p)
  let lint p = Blang.base (`lint p)
  let modes p = Blang.base (`modes p)
  let name p = Blang.base (`name p)
  let no_preprocessing = Blang.base `no_preprocessing
  let path p = Blang.base (`path p)
  let pp p = Blang.base (`pp p)
  let pps p = Blang.base (`pps p)
  let pp_with_flag p = Blang.base (`pp_with_flag p)
  let preprocess p = Blang.base (`preprocess p)
  let public_name p = Blang.base (`public_name p)
  let return = `return
  let skip_subtree = `skip_subtree
  let stanza p = Blang.base (`stanza p)
end
