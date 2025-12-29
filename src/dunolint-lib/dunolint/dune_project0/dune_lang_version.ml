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

type t = int * int

(* Given the actual shape and representation of the values inhabiting this type,
   polymorphic comparison is adequate here. *)

let compare : t -> t -> int = Stdlib.compare
let equal : t -> t -> bool = Stdlib.( = )
let to_string (a, b) = Printf.sprintf "%d.%d" a b
let sexp_of_t t = Sexp.Atom (to_string t)

let t_of_sexp sexp =
  let of_two_atoms a b =
    match Int.of_string_opt a, Int.of_string_opt b with
    | Some a, Some b -> Some (a, b)
    | _ -> None
  in
  match
    match (sexp : Sexp.t) with
    | List _ -> None
    | Atom str ->
      (match String.split str ~on:'.' with
       | [ a; b ] -> of_two_atoms a b
       | _ -> None)
  with
  | Some t -> t
  | None ->
    raise (Sexp.Of_sexp_error (Failure "Invalid version - expected [MAJOR.MINOR].", sexp))
;;

let create t = t

module Predicate = struct
  [@@@coverage off]

  let error_source = "dune_lang_version.t"

  type version = t

  type deprecated_names =
    [ `equals of version
    | `greater_than_or_equal_to of version
    | `less_than_or_equal_to of version
    ]

  type t =
    [ `eq of version
    | `gt of version
    | `gte of version
    | `lt of version
    | `lte of version
    | `neq of version
    | deprecated_names
    ]

  (* Given the actual shape and representation of the values inhabiting this
     type, polymorphic comparison is adequate here. *)

  let equal : t -> t -> bool = Stdlib.( = )

  let variant_spec : t Sexp_helpers.Variant_spec.t =
    let pred atom cons : t Sexp_helpers.Variant_spec.case =
      { atom; conv = Unary (fun sexp -> cons (t_of_sexp sexp)) }
    in
    [ pred "=" (fun v -> `eq v)
    ; pred ">" (fun v -> `gt v)
    ; pred ">=" (fun v -> `gte v)
    ; pred "<" (fun v -> `lt v)
    ; pred "<=" (fun v -> `lte v)
    ; pred "!=" (fun v -> `neq v)
    ; pred "equals" (fun v -> `eq v)
    ; pred "greater_than_or_equal_to" (fun v -> `gte v)
    ; pred "less_than_or_equal_to" (fun v -> `lte v)
    ]
  ;;

  let t_of_sexp (sexp : Sexp.t) : t =
    Sexp_helpers.parse_variant variant_spec ~error_source sexp
  ;;

  let sexp_of_t (t : t) : Sexp.t =
    let op name v = Sexp.List [ Atom name; sexp_of_t v ] in
    match t with
    | `eq v -> op "=" v
    | `gt v -> op ">" v
    | `gte v -> op ">=" v
    | `lt v -> op "<" v
    | `lte v -> op "<=" v
    | `neq v -> op "!=" v
    | `equals v -> op "equals" v
    | `greater_than_or_equal_to v -> op "greater_than_or_equal_to" v
    | `less_than_or_equal_to v -> op "less_than_or_equal_to" v
  ;;
end
