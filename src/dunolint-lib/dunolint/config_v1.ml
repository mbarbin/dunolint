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
  type t = (Predicate.t, Condition.t) Rule.Stable.V1.t

  let equal (a : t) (b : t) = Rule.Stable.V1.equal Predicate.equal Condition.equal a b

  let t_of_sexp sexp =
    Rule.Stable.V1.t_of_sexp Predicate.t_of_sexp Condition.t_of_sexp sexp
  ;;

  let sexp_of_t t = Rule.Stable.V1.sexp_of_t Predicate.sexp_of_t Condition.sexp_of_t t
end

module Stanza = struct
  let error_source = "config.v1.stanza.t"

  type t =
    [ `skip_paths of Glob.t list
    | `rule of Rule.t
    ]

  let equal (a : t) (b : t) =
    if Stdlib.( == ) a b
    then true
    else (
      match a, b with
      | `skip_paths va, `skip_paths vb -> equal_list Glob.equal va vb
      | `rule va, `rule vb -> Rule.equal va vb
      | (`skip_paths _ | `rule _), _ -> false)
  ;;

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    [ { atom = "skip_paths"
      ; conv =
          Variadic
            (fun ~context:_ ~fields -> `skip_paths (List.map fields ~f:Glob.t_of_sexp))
      }
    ; { atom = "rule"; conv = Unary (fun sexp -> `rule (Rule.t_of_sexp sexp)) }
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    match t with
    | `skip_paths v -> List (Atom "skip_paths" :: List.map v ~f:Glob.sexp_of_t)
    | `rule v -> List [ Atom "rule"; Rule.sexp_of_t v ]
  ;;
end

type t = { stanzas : Stanza.t list }

let equal (a : t) (b : t) =
  if Stdlib.( == ) a b
  then true
  else (
    let { stanzas } = b in
    equal_list Stanza.equal a.stanzas stanzas)
;;

let sexp_of_t { stanzas } =
  Sexp.List [ List [ Atom "stanzas"; sexp_of_list Stanza.sexp_of_t stanzas ] ]
;;

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
