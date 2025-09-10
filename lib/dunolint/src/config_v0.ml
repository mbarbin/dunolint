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
    let error_source = "config.v0.skip_subtree.t"

    type t = [ `path of Path.Predicate.t Blang.t ]

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
      (function
       | Sexplib0.Sexp.Atom atom__014_ as _sexp__016_ ->
         (match atom__014_ with
          | "path" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__016_
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
      (fun sexp__022_ ->
         try __t_of_sexp__ sexp__022_ with
         | Sexplib0.Sexp_conv_error.No_variant_match ->
           Sexplib0.Sexp_conv_error.no_matching_variant_found error_source sexp__022_
       : Sexplib0.Sexp.t -> t)
    ;;

    let sexp_of_t =
      (fun (`path v__024_) ->
         Sexplib0.Sexp.List
           [ Sexplib0.Sexp.Atom "path"; Blang.sexp_of_t Path.Predicate.sexp_of_t v__024_ ]
       : t -> Sexplib0.Sexp.t)
    ;;
  end

  module Result = struct
    let error_source = "config.v0.skip_subtree.result.t"

    type t = |

    let compare = (Stdlib.compare : t -> t -> int)
    let equal = (Stdlib.( = ) : t -> t -> bool)

    let t_of_sexp =
      (function
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__030_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source sexp__030_
       | Sexplib0.Sexp.List [] as sexp__030_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source sexp__030_
       | sexp__030_ -> Sexplib0.Sexp_conv_error.unexpected_stag error_source sexp__030_
       : Sexplib0.Sexp.t -> t)
    ;;

    let sexp_of_t =
      (function
       | _ -> .
       : t -> Sexplib0.Sexp.t)
    ;;
  end

  type t = (Predicate.t, Result.t) Rule.Stable.V0.t

  let compare =
    (fun a__033_ ->
       fun b__034_ ->
       Rule.Stable.V0.compare Predicate.compare Result.compare a__033_ b__034_
     : t -> t -> int)
  ;;

  let equal =
    (fun a__039_ ->
       fun b__040_ -> Rule.Stable.V0.equal Predicate.equal Result.equal a__039_ b__040_
     : t -> t -> bool)
  ;;

  let t_of_sexp =
    (fun x__046_ -> Rule.Stable.V0.t_of_sexp Predicate.t_of_sexp Result.t_of_sexp x__046_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (fun x__047_ -> Rule.Stable.V0.sexp_of_t Predicate.sexp_of_t Result.sexp_of_t x__047_
     : t -> Sexplib0.Sexp.t)
  ;;
end

module Rule = struct
  [@@@coverage off]

  type t = (Predicate.t, Condition.t) Rule.Stable.V0.t

  let compare =
    (fun a__048_ ->
       fun b__049_ ->
       Rule.Stable.V0.compare Predicate.compare Condition.compare a__048_ b__049_
     : t -> t -> int)
  ;;

  let equal =
    (fun a__054_ ->
       fun b__055_ -> Rule.Stable.V0.equal Predicate.equal Condition.equal a__054_ b__055_
     : t -> t -> bool)
  ;;

  let t_of_sexp =
    (fun x__061_ ->
       Rule.Stable.V0.t_of_sexp Predicate.t_of_sexp Condition.t_of_sexp x__061_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (fun x__062_ ->
       Rule.Stable.V0.sexp_of_t Predicate.sexp_of_t Condition.sexp_of_t x__062_
     : t -> Sexplib0.Sexp.t)
  ;;
end

module T = struct
  [@@@coverage off]

  let error_source = "config.v0.t"

  type t =
    { skip_subtree : Skip_subtree.t option [@sexp.option]
    ; rules : Rule.t list
    }

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
    (fun x__077_ ->
       Sexplib0.Sexp_conv_record.record_of_sexp
         ~caller:error_source
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
end

include T

let t_of_sexp sexp =
  Sexp_helpers.when_parsing_config_version_0 ~f:(fun () -> t_of_sexp sexp)
;;

let skip_subtree t = t.skip_subtree
let rules t = t.rules
let create ?skip_subtree ?(rules = []) () = { skip_subtree; rules }

module Std = Edsl_std
