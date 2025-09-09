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

module T = struct
  [@@@coverage off]

  type t =
    [ `byte
    | `native
    | `best
    | `melange
    ]

  let all = ([ `byte; `native; `best; `melange ] : t list)

  let __t_of_sexp__ =
    (let error_source__006_ = "compilation_mode.t" in
     function
     | Sexplib0.Sexp.Atom atom__002_ as _sexp__004_ ->
       (match atom__002_ with
        | "byte" -> `byte
        | "native" -> `native
        | "best" -> `best
        | "melange" -> `melange
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__002_ :: _) as _sexp__004_ ->
       (match atom__002_ with
        | "byte" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source__006_ _sexp__004_
        | "native" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source__006_ _sexp__004_
        | "best" -> Sexplib0.Sexp_conv_error.ptag_no_args error_source__006_ _sexp__004_
        | "melange" ->
          Sexplib0.Sexp_conv_error.ptag_no_args error_source__006_ _sexp__004_
        | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
     | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__003_ ->
       Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var error_source__006_ sexp__003_
     | Sexplib0.Sexp.List [] as sexp__003_ ->
       Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var error_source__006_ sexp__003_
     : Sexplib0.Sexp.t -> t)
  ;;

  let t_of_sexp =
    (let error_source__008_ = "compilation_mode.t" in
     fun sexp__007_ ->
       try __t_of_sexp__ sexp__007_ with
       | Sexplib0.Sexp_conv_error.No_variant_match ->
         Sexplib0.Sexp_conv_error.no_matching_variant_found error_source__008_ sexp__007_
     : Sexplib0.Sexp.t -> t)
  ;;

  let sexp_of_t =
    (function
     | `byte -> Sexplib0.Sexp.Atom "byte"
     | `native -> Sexplib0.Sexp.Atom "native"
     | `best -> Sexplib0.Sexp.Atom "best"
     | `melange -> Sexplib0.Sexp.Atom "melange"
     : t -> Sexplib0.Sexp.t)
  ;;
end

include T

let to_comparable_int = function
  | `byte -> 0
  | `native -> 1
  | `best -> 2
  | `melange -> 3
;;

let compare a b = Int.compare (to_comparable_int a) (to_comparable_int b)
let equal a b = Int.equal (to_comparable_int a) (to_comparable_int b)
let hash : t -> int = Stdlib.Hashtbl.hash
let seeded_hash : int -> t -> int = Stdlib.Hashtbl.seeded_hash
