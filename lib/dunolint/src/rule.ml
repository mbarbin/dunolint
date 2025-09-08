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

module T = struct
  [@@@coverage off]

  type ('predicate, 'invariant) t =
    [ `enforce of 'invariant
    | `return
    | `skip_subtree
    | `cond of ('predicate Blang.t * ('predicate, 'invariant) t) list
    ]
  [@@deriving compare, equal, sexp]
end

include T

let eval t ~f =
  let rec aux_t t =
    match t with
    | (`enforce _ | `return | `skip_subtree) as invariant -> invariant
    | `cond clauses ->
      (match
         List.find clauses ~f:(fun (condition, _) ->
           match Trilang.eval condition ~f with
           | True -> true
           | False | Undefined -> false)
       with
       | None -> `return
       | Some (_, t) -> aux_t t)
  in
  aux_t t
;;
