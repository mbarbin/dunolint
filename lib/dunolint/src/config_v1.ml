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

module Rule = struct
  [@@@coverage off]

  type t = (Predicate.t, Condition.t) Rule.Stable.V1.t

  let compare =
    (fun a__001_ ->
       fun b__002_ ->
       Rule.Stable.V1.compare Predicate.compare Condition.compare a__001_ b__002_
     : t -> t -> int)
  ;;

  let equal =
    (fun a__007_ ->
       fun b__008_ -> Rule.Stable.V1.equal Predicate.equal Condition.equal a__007_ b__008_
     : t -> t -> bool)
  ;;

  let t_of_sexp =
    (fun x__014_ ->
       Rule.Stable.V1.t_of_sexp Predicate.t_of_sexp Condition.t_of_sexp x__014_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (fun x__015_ ->
       Rule.Stable.V1.sexp_of_t Predicate.sexp_of_t Condition.sexp_of_t x__015_
     : t -> Sexplib0.Sexp.t)
  ;;
end

module Stanza = struct
  [@@@coverage off]

  let error_source = "config.v1.stanza.t"

  type t =
    [ `skip_paths of Glob.t list
    | `rule of Rule.t
    ]

  let compare =
    (fun a__001_ ->
       fun b__002_ ->
       if Stdlib.( == ) a__001_ b__002_
       then 0
       else (
         match a__001_, b__002_ with
         | `skip_paths _left__003_, `skip_paths _right__004_ ->
           compare_list Glob.compare _left__003_ _right__004_
         | `rule _left__007_, `rule _right__008_ -> Rule.compare _left__007_ _right__008_
         | x, y -> Stdlib.compare x y)
     : t -> t -> int)
  ;;

  let equal =
    (fun a__009_ ->
       fun b__010_ ->
       if Stdlib.( == ) a__009_ b__010_
       then true
       else (
         match a__009_, b__010_ with
         | `skip_paths _left__011_, `skip_paths _right__012_ ->
           equal_list Glob.equal _left__011_ _right__012_
         | `rule _left__015_, `rule _right__016_ -> Rule.equal _left__015_ _right__016_
         | x, y -> Stdlib.( = ) x y)
     : t -> t -> bool)
  ;;

  let __t_of_sexp__ =
    (function
     | Sexplib0.Sexp.Atom atom__018_ as _sexp__020_ ->
       (match atom__018_ with
        | "skip_paths" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__020_
        | "rule" -> Sexplib0.Sexp_conv_error.ptag_takes_args error_source _sexp__020_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__018_ :: sexp_args__021_) as
       _sexp__020_ ->
       (match atom__018_ with
        | "skip_paths" -> `skip_paths (List.map sexp_args__021_ ~f:Glob.t_of_sexp)
        | "rule" as _tag__022_ ->
          (match sexp_args__021_ with
           | arg0__023_ :: [] ->
             let res0__024_ = Rule.t_of_sexp arg0__023_ in
             `rule res0__024_
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
     | `skip_paths v__031_ ->
       Sexplib0.Sexp.List
         (Sexplib0.Sexp.Atom "skip_paths" :: List.map ~f:Glob.sexp_of_t v__031_)
     | `rule v__032_ ->
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "rule"; Rule.sexp_of_t v__032_ ]
     : t -> Sexplib0.Sexp.t)
  ;;
end

module T0 = struct
  [@@@coverage off]

  type t = { stanzas : Stanza.t list }

  let compare =
    (fun a__001_ ->
       fun b__002_ ->
       if Stdlib.( == ) a__001_ b__002_
       then 0
       else compare_list Stanza.compare a__001_.stanzas b__002_.stanzas
     : t -> t -> int)
  ;;

  let equal =
    (fun a__005_ ->
       fun b__006_ ->
       if Stdlib.( == ) a__005_ b__006_
       then true
       else equal_list Stanza.equal a__005_.stanzas b__006_.stanzas
     : t -> t -> bool)
  ;;

  let sexp_of_t =
    (fun { stanzas = stanzas__010_ } ->
       let bnds__009_ = ([] : _ Stdlib.List.t) in
       let bnds__009_ =
         let arg__011_ = sexp_of_list Stanza.sexp_of_t stanzas__010_ in
         (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "stanzas"; arg__011_ ] :: bnds__009_
          : _ Stdlib.List.t)
       in
       Sexplib0.Sexp.List bnds__009_
     : t -> Sexplib0.Sexp.t)
  ;;
end

include T0

let of_stanzas sexps =
  let stanzas = List.map sexps ~f:Stanza.t_of_sexp in
  { stanzas }
;;

let to_stanzas { stanzas } = List.map stanzas ~f:Stanza.sexp_of_t

let skip_paths t =
  List.filter_map t.stanzas ~f:(function
    | `skip_paths globs -> Some globs
    | `rule _ -> None)
;;

let rules t =
  List.filter_map t.stanzas ~f:(function
    | `rule rule -> Some rule
    | `skip_paths _ -> None)
;;

let create stanzas = { stanzas }

module Std = Edsl_std
