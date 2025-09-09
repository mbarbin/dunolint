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

module Backend = struct
  module Name = struct
    include Container_key.String_impl

    let invariant t =
      (not (String.is_empty t))
      && String.for_all t ~f:(fun c ->
        Char.is_alphanum c || Char.equal c '_' || Char.equal c '.')
    ;;

    include Validated_string.Make (struct
        let module_name = "Dunolint.Instrumentation.Backend.Name"
        let invariant = invariant
      end)
  end
end

module Predicate = struct
  [@@@coverage off]

  type t = [ `backend of Backend.Name.t ]

  let compare =
    (fun a__001_ ->
       fun b__002_ ->
       if Stdlib.( == ) a__001_ b__002_
       then 0
       else (
         match a__001_, b__002_ with
         | `backend _left__003_, `backend _right__004_ ->
           Backend.Name.compare _left__003_ _right__004_)
     : t -> t -> int)
  ;;

  let equal =
    (fun a__005_ ->
       fun b__006_ ->
       if Stdlib.( == ) a__005_ b__006_
       then true
       else (
         match a__005_, b__006_ with
         | `backend _left__007_, `backend _right__008_ ->
           Backend.Name.equal _left__007_ _right__008_)
     : t -> t -> bool)
  ;;

  let __t_of_sexp__ =
    (let error_source__017_ = "lib/dunolint/src/dune0/instrumentation.ml.Predicate.t" in
     function
     | Sexplib0.Sexp.Atom atom__010_ as _sexp__012_ ->
       (match atom__010_ with
        | "backend" ->
          Sexplib0.Sexp_conv_error.ptag_takes_args error_source__017_ _sexp__012_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__010_ :: sexp_args__013_) as
       _sexp__012_ ->
       (match atom__010_ with
        | "backend" as _tag__014_ ->
          (match sexp_args__013_ with
           | arg0__015_ :: [] ->
             let res0__016_ = Backend.Name.t_of_sexp arg0__015_ in
             `backend res0__016_
           | _ ->
             Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
               error_source__017_
               _tag__014_
               _sexp__012_)
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__011_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__017_ sexp__011_
     | Sexplib0.Sexp.List [] as sexp__011_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__017_ sexp__011_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (let error_source__019_ = "lib/dunolint/src/dune0/instrumentation.ml.Predicate.t" in
     fun sexp__018_ ->
       try __t_of_sexp__ sexp__018_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__019_ sexp__018_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (fun (`backend v__020_) ->
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "backend"; Backend.Name.sexp_of_t v__020_ ]
     : t -> Sexplib0.Sexp.t)
  ;;
end
